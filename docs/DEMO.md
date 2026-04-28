# 10-minute demo

A guided tour of the harness for a new collaborator.  Every block is
copy-pasteable; the whole thing runs in ~5 minutes wall clock if
you skip the live LLM call (Section 6).

Assumes:
- You've cloned the repo and `cd`ed into it.
- `wolframscript` is on your PATH (Wolfram 14+).
- `gh` is set up if you want to peek at CI (Section 8) — optional.

---

## 1. Smoke-test the harness (15 s)

Confirm the paclet loads and the unit suite is green.

```
wolframscript -file WolframChallengesLLMBenchmarkPaclet/Tests/RunTests.wls 2>&1 | tail -n 12
```

You should see `failed: 0`, `errored: 0`, and 99/99 passed across 10
test files.  This grades the harness itself \[LongDash] not any
particular model.

---

## 2. Inspect the bank (10 s)

```
wolframscript -code '
  PacletDirectoryLoad["WolframChallengesLLMBenchmarkPaclet"];
  Needs["JofreEspigulePons`WolframChallengesBenchmark`"];
  data = LoadChallengesJSONL["challenges.jsonl"];
  Print["Total challenges: ", Length[data["challenges"]]];
  Print["Total tests:      ", Total[Length /@ Values[data["testBank"]]]];
  Print["Sample challenge: ", Keys[data["challenges"]][[42]]];
  Print["First 200 chars of its prompt:"];
  Print[StringTake[data["challenges", Keys[data["challenges"]][[42]], "prompt"], UpTo[200]]];
'
```

Demonstrates: 166 challenges + 724 tests load from a single
HumanEval-shaped JSONL file in well under a second.

---

## 3. Run the benchmark on a filtered subset (~30 s)

Run two known-easy challenges (`FizzBuzz`, `Palindromes`) against
gemini-2.5-flash's existing solutions, sandboxed, in-process so it
finishes in seconds.

```
wolframscript -file scripts/RunBenchmark.wls \
  --solutions solutions/google_gemini-2.5-flash \
  --model     google/gemini-2.5-flash \
  --filter    FizzBuzz,Palindromes \
  --isolation InProcess \
  --out       /tmp/wclb-demo
```

The output shows pass/fail per test, p50/p90/p99 timing, and a
pointer to the rendered HTML report.

```
open /tmp/wclb-demo/run-*/report.html
```

Demonstrates: filterable runs, typed per-test results, per-run
HTML/Markdown/JSON/JUnit reports.

---

## 4. Run under per-test kernel isolation (~30 s)

Same call, but with each test in its own subkernel \[LongDash] the
production grading mode for untrusted LLM output.  An infinite loop,
`Abort[]`, `Quit[]`, OOM, or stack overflow in the candidate cannot
take the driver down.

```
wolframscript -file scripts/RunBenchmark.wls \
  --solutions solutions/google_gemini-2.5-flash \
  --model     google/gemini-2.5-flash \
  --filter    FizzBuzz,Palindromes \
  --isolation PerTestKernel \
  --parallel  4 \
  --out       /tmp/wclb-demo-perkernel
```

Demonstrates: process-level fault containment with per-test wall-clock
+ memory enforcement.

---

## 5. Pre-flight audit a solutions directory (5 s)

`AuditSolutions` parses every `.wl` (held \[LongDash] never
evaluates) and checks each candidate defines the function the test
bank expects.  Catches mislabeled solutions before a long run.

```
wolframscript -code '
  PacletDirectoryLoad["WolframChallengesLLMBenchmarkPaclet"];
  Needs["JofreEspigulePons`WolframChallengesBenchmark`"];
  data = LoadChallengesJSONL["challenges.jsonl"];
  audit = AuditSolutions["solutions/google_gemini-2.5-flash", data["testBank"]];
  Print["ok:          ", audit["okCount"]];
  Print["mismatches:  ", Length[audit["mismatches"]]];
  Print["missing:     ", Length[audit["missing"]]];
  Print["unparseable: ", Length[audit["unparseable"]]];
'
```

Demonstrates: write-time audit gate keeps the on-disk solution set
self-consistent with the test bank.

---

## 6. Generate one solution from an LLM (~30 s, optional)

Skip if no OpenRouter key.  Generates a fresh solution for `FizzBuzz`
against gemini, with full retry/backoff/timeout logic and the
write-time audit gate.

```
export OPENROUTER_API_KEY=...    # your key here
wolframscript -file scripts/GenerateSolutions.wls \
  --filter    FizzBuzz \
  --model     google/gemini-2.5-flash \
  --out       /tmp/wclb-gen
ls /tmp/wclb-gen/google_gemini-2.5-flash/
```

Demonstrates: end-to-end LLM \[Rule] code-extraction \[Rule] audit
\[Rule] disk write, with rich JSONL audit log, exponential backoff,
and HoldComplete-safe parsing.

---

## 7. Compare two model runs (~10 s)

```
wolframscript -file scripts/CompareModels.wls \
  --out runs/demo-compare \
  /tmp/wclb-demo/run-* \
  /tmp/wclb-demo-perkernel/run-*
open runs/demo-compare/compare.html
```

Demonstrates: per-challenge pass-rate matrix across models,
uniquely-passed / uniquely-failed buckets, headline summary.

---

## 8. Leaderboard (optional, ~2 s)

Walks `runs/**/run.json`, takes the latest benchmark run per model,
and renders a podium-style leaderboard ranked by pass rate.

```
wolframscript -file scripts/Leaderboard.wls
open runs/leaderboard/report.html
```

Demonstrates: at-a-glance ranking across all models the harness has
graded, with gold/silver/bronze podium for the top 3 + a full ranked
table.  Naturally populates as new model runs land.

---

## 9. Model strengths by topic tag (optional, ~3 s)

Reads each model's most recent benchmark run, groups results by the
topic tags attached to each challenge in `challenges.jsonl`, and
renders a per-tag pass-rate matrix (rows = tags hardest-first,
cols = models best-first) plus per-model top-3 strongest /
weakest tags.

```
wolframscript -file scripts/ModelStrengths.wls
open runs/model-strengths/report.html
```

Demonstrates: where each LLM is strong (e.g. "this model is great at
algorithmic challenges but weak on geography lookups") at a glance.
Most useful with 2+ models on the leaderboard.

If you've recently edited the bank, refresh the tags first:

```
wolframscript -file scripts/AddTagsToBank.wls
```

---

## 10. Trend dashboard (optional, ~2 s)

Walks `runs/**/run.json` (every benchmark run + bank-self-test run on
disk), groups by model, and renders an HTML + Markdown timeline of
pass-rate-over-time per model with inline SVG sparklines.

```
wolframscript -file scripts/TrendReport.wls
open runs/trend/report.html
```

Demonstrates: how a model drifts run-over-run and how multiple models
compare on the same bank.  Most useful once you've accumulated several
runs.

---

## 11. Bank-quality dashboard (optional, ~5 s)

After running the bank-self-test (`scripts/BankSelfTest.wls`),
classify every challenge as fully_passing / empty_canonical /
parse_error / value_drift / timeout_or_other and render an
actionable HTML + Markdown worklist:

```
wolframscript -file scripts/BankQualityReport.wls
open runs/bank-quality/report.html
```

Demonstrates: per-challenge bank-quality classification with sample
failing tests inline, sorted with the most-actionable categories
first. Drives bank-curation work by producing a concrete TODO list.

---

## 12. CI (optional)

```
gh run list --workflow=tests.yml --limit 3
gh run view --log <id>   # any green run
```

Demonstrates: every push runs the 99 VerificationTests and the
bank-self-test (canonical solutions vs. their own bank).  See
`.github/workflows/tests.yml` and `docs/ci-setup.md`.

---

## What to read next

- `README.md` \[LongDash] full reference
- `docs/CHALLENGES-JSONL-FORMAT.md` \[LongDash] schema spec for the bank
- `docs/Tutorial.nb` \[LongDash] interactive walk-through (open in Mathematica)
- `docs/ci-setup.md` \[LongDash] runner + secrets setup
- `WolframChallengesLLMBenchmarkPaclet/Documentation/English/ReferencePages/Symbols/`
  \[LongDash] per-symbol reference docs
