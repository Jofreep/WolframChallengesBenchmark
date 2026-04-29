# Methodology and limitations

This document describes how WolframChallengesBenchmark grades a model
and, more importantly, what it does **not** establish about that
model. The audience is anyone trying to decide whether to trust a
number on the leaderboard. Limitations are stated in concrete terms
so a future contributor knows exactly which corners are open.

## What the benchmark measures

WolframChallengesBenchmark measures the one-shot pass rate of an LLM,
under a fixed prompt template, on a fixed bank of programming problems
drawn from the public Wolfram Challenges site. For each challenge the
model sees a single English prompt, returns Wolfram Language source
code, and that code is run inside an isolated kernel against a curated
test bank. A test passes if its output matches the canonical answer.

That is the entire scope of the metric. The headline pass rate is the
fraction of tests that produced the expected output, full stop. It is
not a measure of code quality, idiomatic style, latency, cost,
reasoning depth, or how the model would perform in an iterative
coding session with a human in the loop. The only signal it carries
is "given one English prompt, can the model emit Wolfram code that
runs and produces the right answer."

## Test bank

The bank contains 166 challenges drawn from
[wolframchallenges.com](https://wolframchallenges.com), with
approximately 700 individual tests across them (a challenge can have
multiple input/output pairs). Bank composition is captured in
`challenges.jsonl` and is the same for every model. A separate
`bank-self-test` job runs every challenge's canonical solution
against its own tests so we can detect bank-quality drift, but those
results are not currently published.

The tracks (Algorithms, Geometry, Words, etc.) are inherited from the
upstream Wolfram Challenges classification with a small set of
synthesized track tags assigned in `scripts/AddTagsToBank.wls`. The
ModelStrengths dashboard groups results by these tracks, so any error
in the upstream tag assignment will show up there before it shows up
in the headline number.

## Generation procedure

For every (model, challenge) pair the runner builds a prompt by
substituting the challenge's English description into a fixed prompt
template (`$defaultPromptTemplate` in
`WolframChallengesLLMBenchmarkPaclet/Kernel/Generator.wl`). The
template asks the model to "write a correct, efficient, and idiomatic
Wolfram Language solution," to "output only Wolfram Language code,"
and to "define the requested function exactly as specified in the
challenge."

The prompt is sent to OpenRouter via `HTTPRequest`/`URLRead` with the
model identifier, max-tokens, and temperature carried in the per-model
config file. Each call has a per-call timeout (default 120s, larger
for slower models) and up to `--retries` attempts with exponential
backoff. Retries are restricted to transport-level failures
(connection aborted, timeout, malformed JSON); a successful HTTP
response that contains audit-rejecting code is not retried.

The model's response is run through `extractCode` (which strips
`” ```wl ... ``` ” `` markdown fences if present), then through an
audit gate. The audit gate inspects the extracted source via
`HoldComplete`-safe inspection and refuses to save the solution if
the set of top-level defined symbols does not intersect the set of
function names expected by the test bank. This is meant to catch
LLM responses that solve a different problem or use a wrong function
name, but it is also a partial proxy: it cannot validate that the
function actually computes the right thing, only that it has roughly
the right shape.

If the audit gate refuses, the run is recorded as `audit-rejected`
and the extracted source is dumped to
`solutions/<model>/raw/<name>.audit-rejected.raw.txt` for forensic
inspection. If the LLM call itself failed, the un-extracted body is
dumped to `<name>.failed.raw.txt`. On success, the canonical save
path is `solutions/<model>/<name>.wl` with a sibling `.meta.json`
carrying token usage, generation id, finish reason, latency, prompt
hash, and response hash. The full raw response is preserved at
`solutions/<model>/raw/<name>.raw.txt` for reproducibility audits.

## Evaluation procedure

`scripts/RunBenchmark.wls` loads the saved solutions, the
challenges, and the test bank, then evaluates every test in an
isolated kernel. The default isolation mode is `PerTestKernel`,
which boots a fresh `WolframKernel` subprocess per test so that
shared state (definitions, side effects, package loads) cannot leak
between tests. A `Sandbox` option sets `$Path = {}` and unsets
network primitives inside the test kernel to reduce the surface
area of an adversarial solution.

Each test has a wall-clock `TimeConstraint` (default 60s) and a
`MemoryConstraint` (default 2 GB). Outcomes are bucketed as
`Evaluated`, `TimedOut`, `MemoryExceeded`, `EvaluationError`,
`ParseError`, `KernelDied`, or `NoSolution`. A test is counted as
*passed* only if it both `Evaluated` and produced output equal to
the canonical answer; every other outcome counts as a failure with
its specific reason recorded.

Results are written as `runs/run-*/results.wxf`,
`runs/run-*/run.json`, and `runs/run-*/progress.jsonl`. The runner
calls `scripts/PublishResults.wls` at the tail end to extract a
safe summary (per-challenge pass counts, per-test pass/fail with no
source code) into `data/results/<modelSlug>.json`. The dashboards
read from there.

## Reporting and aggregation

The leaderboard ranks models by their headline pass rate
(passed_tests / total_tests). The Trend dashboard plots that pass
rate over time per model. ModelStrengths bins results by track and
reports a per-(model, track) pass rate, plus each model's three
strongest and three weakest tracks. None of these dashboards
currently report confidence intervals or uncertainty.

When a model has incomplete coverage (some challenges produced no
`.wl` because the LLM kept timing out or audit-rejecting), the
"challenges attempted" denominator shrinks accordingly. The
leaderboard prints both `passed/total` and `challengesFullyPassing/
challengesAttempted` so partial coverage is visible, but the
headline rank is still based on pass rate and a model with worse
coverage can sort higher than one with better coverage if its
remaining solutions are stronger. This is currently called out in
the table but not adjusted for; a more rigorous version would
penalize coverage gaps explicitly.

## Limitations

### Statistical power

Each model is evaluated against ~700 tests. Treating those as
independent Bernoulli trials, a 95% Wilson confidence interval on
a 70% pass rate spans roughly ±3.4 percentage points. Differences
in headline pass rate of less than 5 points should not be read as
evidence that one model is better than another. The current
leaderboard does not show these intervals, so a casual reader can
overinterpret a 71% vs 68% gap as a real ranking when it is not.

### Single trial per model

Every published number is from a single benchmark run. We do not
average across trials, do not lock in an OpenRouter provider, and
do not report seed sensitivity. OpenRouter's routing layer can
direct the same model identifier to different upstream providers on
different days; provider-side variance (cold starts, queue depth,
quantization choices, fallback behavior) can produce run-to-run
differences that exceed real differences between two models.
Production-grade evaluation would average 3–5 trials per (model,
challenge) and lock the provider via OpenRouter's
`provider.order` parameter.

### Single-shot pass@1 only

Models get exactly one attempt per challenge. There is no chain
of feedback, no error message visibility, no multi-turn debugging.
This is a defensible choice — pass@1 is interpretable and cheap —
but it does not reflect how LLMs are typically used to write code
in practice, where a developer pastes back error output and the
model iterates. A model that is bad at one-shot but good at
iterative loops will look worse here than it really is. Future
work should add `pass@k` (re-sample k candidates, count any pass)
and an interactive variant.

### Audit gate as a partial proxy

The audit gate refuses to save a solution if the top-level defined
symbols don't intersect the expected names from the test bank.
This catches the obvious failure mode where the LLM solves a
different problem entirely, but it has two weaknesses. First, it
penalizes naming mismatches more harshly than capability gaps:
the LLM might define `MultTable` instead of `MultiplicationTable`
and fail audit despite computing the right thing — a test of the
benchmark's prompt design as much as the model. Second, the gate
cannot tell whether the function *works*, only whether it has the
right shape; a solution that audits cleanly but produces wrong
output runs through the test bank and fails there, which is the
correct outcome but means audit and tests are testing different
things.

### Prompt does not include canonical function name

The current prompt template asks the model to "define the
requested function exactly as specified in the challenge," but
the challenge body itself is the only place the function name
appears. When the model picks a plausible synonym
(`MultTable` for `MultiplicationTable`, `QueenGraph` for
`NDimensionalQueenGraph`) the audit gate refuses and the
challenge counts as a failure even though the implementation
might be correct. This is a benchmark artifact, not a model
failing. The fix is to inject the canonical name into the prompt
explicitly; that has not been done, partly because doing so
changes the historical numbers.

### Wolfram-Language-specific signal

Wolfram Language is a small slice of the LLM coding eval surface.
A model that excels here is fluent in functional, expression-tree-
oriented programming with a large built-in stdlib; that
correlates with general code competence but is not the same.
Anyone using this benchmark should treat it as a track-specific
signal, not a substitute for HumanEval, SWE-Bench, or general
code-eval suites.

### Coverage gaps across models bias comparisons

When a model fails to generate code for some challenges (typically
because of recurrent timeouts or audit rejections), those
challenges are silently absent from its denominator, while a more
reliably-completing model has them in its denominator. The two
are not directly comparable. The leaderboard does report
`challengesAttempted/166` so the gap is visible, but the headline
pass rate doesn't penalize coverage. A more honest aggregator
would either treat missing challenges as failures (no-credit
denominator) or report two numbers: pass rate among attempted, and
overall pass rate vs total bank.

### Network and timeout failures aren't model failures

`failed.raw.txt` and "no solution" outcomes can stem from
provider-side latency, queue saturation, or transient network
errors rather than from the model's actual capability. The
generator retries with exponential backoff and a generous
`--timeout`, but at some point the wrapper gives up and marks the
challenge missing. Slower or more rate-limited routes look worse
than they should.

## Reproducibility

Generated `.wl` solutions are intentionally not tracked in git:
some providers' terms of service constrain redistribution of model
output, and we do not want the public repo to be a vector for that.
What is tracked, under `data/results/<modelSlug>.json`, is the
strict metric output: per-run summary, per-challenge pass counts,
and per-test pass/fail with status — no source code, no LLM text.
This is enough to rebuild the dashboards on a fresh checkout but
not enough to re-grade independently.

A third party who wants to verify these numbers needs:

an OpenRouter account and API key, the same model identifier,
roughly 30–60 minutes per model for the generation pass plus a few
minutes for the benchmark run. Costs vary by model — opus-class
models run several dollars per full bank pass; cheap models are
under a dollar. Re-runs will not be byte-identical because OpenRouter
routing and model non-determinism (even at temperature 0) produce
different completions, but pass rates should be within the
confidence interval discussed above.

## Roadmap toward production-grade

In rough priority order:

Multi-trial averaging with a fixed seed schedule and provider
locking, so a single model number reflects 3–5 runs rather than
one. Confidence intervals on every pass rate displayed in the
dashboards. The canonical function name baked into the prompt so
naming mismatches stop showing up as model failures. A `pass@k`
metric alongside `pass@1`. Bank expansion toward several hundred
problems with tags audited by a second pass. A bank-quality story
in the published site (currently bank-quality is local-only)
documenting which challenges are flaky, ambiguous, or have
canonical answers that drift between Wolfram versions. An
aggregator that handles incomplete coverage explicitly rather
than silently shrinking the denominator.

Until those are in place, the right way to read this leaderboard
is: this is a one-run, one-shot, niche-language pass rate on a
small bank, useful for spotting large gaps between models and
useless for resolving small ones. The infrastructure is mature;
the methodology is still personal-project-grade, and we say so
here so no one is misled by the polish of the dashboards into
thinking the underlying study is more rigorous than it is.
