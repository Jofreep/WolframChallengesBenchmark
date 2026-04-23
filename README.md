# WolframChallengesBenchmark

[![Tests](https://github.com/Jofreep/WolframChallengesBenchmark/actions/workflows/tests.yml/badge.svg?branch=main)](https://github.com/Jofreep/WolframChallengesBenchmark/actions/workflows/tests.yml)

A production-ready harness for grading LLM-generated Wolfram Language
solutions against the [Wolfram
Challenges](https://challenges.wolframcloud.com/) test bank.

The harness:

- Runs each candidate test in its own isolated subkernel so that an
  `Abort[]`, `Quit[]`, infinite loop, or out-of-memory in the candidate
  cannot take the driver down.
- Enforces per-test wall-clock and memory limits, reporting breaches as
  structured outcomes rather than swallowing them.
- Streams progress to a JSONL file so a `tail -f` consumer or live
  dashboard sees results as soon as they complete.
- Writes a typed result schema (`results.wxf`), a run header
  (`run.json`), three rendered reports (`report.html`, `report.md`,
  `report.json`), and a `junit.xml` file consumable by any CI system
  that ingests the JUnit XML format (GitHub Actions, Jenkins xUnit,
  GitLab, Buildkite, etc.) into a per-run directory.
- Diffs runs against a baseline so a CI step can fail on any regression.
- Accepts a plain-text `.wlchallenge` authoring format (one file per
  challenge) that compiles losslessly to the legacy JSON + WXF pair.
  See [`docs/WLCHALLENGE-FORMAT.md`](docs/WLCHALLENGE-FORMAT.md).

## Repository layout

    WolframChallengesLLMBenchmarkPaclet/
        PacletInfo.wl
        Kernel/
            WolframChallengesBenchmark.wl  public symbols + dispatchers
            Utilities.wl             logging, code extraction, JSONL writer
            OpenRouter.wl            OpenRouter HTTP client
            Loader.wl                JSON/WXF input validation
            Solutions.wl             on-disk solution storage + audit
            Generator.wl             LLM solution generator (retry/timeout/JSONL)
            Results.wl               summarization + run diffs
            Runner.wl                sandboxed runner (3 isolation modes)
            Report.wl                HTML / Markdown / JSON / JUnit rendering
            Compare.wl               cross-model comparison
            TestBankBuilder.wl       .wlchallenge parser / emitter / builder
        Tests/
            *.wlt                    harness self-tests (VerificationTest)
            RunTests.wls             test runner (TestReport, exits non-zero)
        Documentation/English/       Reference pages and guides
    scripts/
        RunBenchmark.wls         CLI entry point
        BuildTestBank.wls        .wlchallenge → JSON + WXF compiler
        GenerateSolutions.wls    LLM-backed solution generator
        RunOneChallenge.wls      single-challenge runner
        MigrateFromNotebook.wls  legacy-notebook → on-disk migration
        VerifyAgainstLegacy.wls  parity check with the pre-refactor notebook
        CompareModels.wls        cross-model report generator
        TestOpenRouter.wls       OpenRouter smoke test
    config/
        <model>.json             LLM provider/model/timeout/retry config
    solutions/<model>/<name>.wl  per-model candidate solutions
    runs/<runId>/                outputs of one benchmark run
        run.json                 run header + summary
        results.wxf              typed per-test results
        progress.jsonl           append-only event stream
        report.{html,md,json}    rendered reports
        junit.xml                JUnit XML for CI ingestion
    ChallengesTestDataV1.json    challenge prompts
    ChallengesTests.wxf          expected-output test bank
    docs/WLCHALLENGE-FORMAT.md   .wlchallenge authoring format spec
    ALTERNATIVE_TESTS.md         design notes on test primitives & monitoring
    AUDIT.md                     pre-refactor architecture audit

## Quick start

Run the bundled mini self-test suite to confirm the harness boots:

    wolframscript -file tests/RunTests.wls

Run the full benchmark for one model:

    wolframscript -file scripts/RunBenchmark.wls \
        --challenges ChallengesTestDataV1.json \
        --testbank   ChallengesTests.wxf \
        --solutions  solutions/claude-opus-4.6 \
        --model      claude-opus-4.6 \
        --out        runs

Add `--filter Aliquot,FizzBuzz` to focus on a subset, `--parallel 8` to
override the default of `$ProcessorCount - 1`, `--timeout 30` to cap
each test at 30 seconds, `--memory 1000000000` to cap each test at 1
GB, and `--baseline runs/<previous-runId>` plus `--fail-under 0.65`
to make the script exit non-zero on regressions or below-threshold
pass rates.

CLI exit codes:

| code | meaning                                              |
|------|------------------------------------------------------|
| 0    | run completed and any thresholds were met            |
| 1    | invalid arguments or missing input files             |
| 2    | pass rate below `--fail-under`                       |
| 3    | regression(s) introduced vs. `--baseline`            |
| 4    | unexpected fatal error in the runner                 |

## Authoring tests (`.wlchallenge` format)

Tests can be edited as plain-text `.wlchallenge` files, one per
challenge, and compiled to the JSON + WXF pair the runtime consumes:

    # build compiled bank from the authoring directory
    wolframscript -file scripts/BuildTestBank.wls \
        --in   .challenges \
        --json ChallengesTestDataV1.json \
        --wxf  ChallengesTests.wxf

    # seed the authoring directory from an existing compiled bank
    wolframscript -file scripts/BuildTestBank.wls --reverse \
        --json ChallengesTestDataV1.json \
        --wxf  ChallengesTests.wxf \
        --out  .challenges

The builder parses test-input expressions *held* so nothing the
candidate is asked to compute is ever evaluated at build time; only
the expected RHS is evaluated (it's the same data already shipping in
the compiled WXF). See
[`docs/WLCHALLENGE-FORMAT.md`](docs/WLCHALLENGE-FORMAT.md) for the
grammar.

## Generating solutions (`GenerateSolutions.wls`)

Production-ready replacement for the legacy notebook's
`callLLMOnChallenge`. Drives an LLM over every challenge and writes
`solutions/<model>/<name>.wl` plus a rich `<name>.meta.json` sidecar.
Wraps each call in a `TimeConstrained` envelope with bounded
exponential-backoff retry, strips the reply with `ExtractCode`, and
routes the write through `SaveSolution`'s test-bank audit so code that
does not define the function expected by the test bank is rejected
before it lands on disk.

    wolframscript -file scripts/GenerateSolutions.wls \
        --challenges   ChallengesTestDataV1.json \
        --testbank     ChallengesTests.wxf \
        --model-config config/claude-opus-4.6.json \
        --out          solutions

Provider choice, model id, temperature, timeout, and retry budget live
in a JSON config file so the same harness drives any LLMSynthesize
provider:

    {
      "model": "claude-opus-4.6",
      "llmEvaluator": {
        "Service": "Anthropic",
        "Model":   "claude-opus-4-6",
        "Temperature": 0.0,
        "MaxTokens":   4000
      },
      "timeoutSec":    120,
      "maxAttempts":   3,
      "retryBaseDelay": 2.0
    }

Every attempt is appended to a per-run JSONL audit log
(`solutions/<model>/generate-<runId>.jsonl`) with `prompt`, `saved`,
`failed`, `auditRejected`, and summary events. `tail -f` on this file
during a run gives a live progress feed. Add `--save-raw` to also
write `<name>.raw.txt` with the unextracted response next to each
solution for forensic replay.

Other flags:

| flag                      | effect                                                 |
|---------------------------|--------------------------------------------------------|
| `--filter A,B`            | generate only these challenges                         |
| `--timeout N`             | per-call seconds (overrides config)                    |
| `--retries N`             | max attempts per challenge (overrides config)          |
| `--retry-base-delay N`    | base seconds between retries, doubled each attempt     |
| `--overwrite`             | regenerate even if `<name>.wl` already exists          |
| `--dry-run`               | write a placeholder stub, no LLM call made             |
| `--log PATH`              | override JSONL log location                            |
| `--service S --model-id M`| override provider without a config file                |

CLI exit codes:

| code | meaning                                                 |
|------|---------------------------------------------------------|
| 0    | every processed challenge produced an audited solution  |
| 1    | invalid arguments or missing input files                |
| 2    | generator finished but some challenges failed/rejected  |
| 4    | unexpected fatal error                                  |

The meta.json sidecar carries, in addition to `model`, `challengeName`,
`sourceHash`, `generatedAt`, and `extractor`:

    "generator"       : "GenerateSolutions/v1"
    "promptHash"      : "sha256:..."
    "rawResponseHash" : "sha256:..."
    "attempts"        : 1
    "llm"             : { "service": "...", "modelId": "...", ... }

so a downstream run is fully reproducible from the sidecar + log.

Programmatic form:

    run = JofreEspigulePons`WolframChallengesBenchmark`GenerateSolutions[challenges, testBank,
      <|
        "Model"           -> "claude-opus-4.6",
        "OutputDirectory" -> "solutions/claude-opus-4.6",
        "LLMEvaluator"    -> <|"Service" -> "Anthropic",
                                "Model"   -> "claude-opus-4-6",
                                "Temperature" -> 0|>,
        "TimeConstraint"  -> 120,
        "MaxAttempts"     -> 3,
        "RetryBaseDelay"  -> 2.0
      |>];

Pass `"Generator" -> myFn` to swap out `LLMSynthesize` for a custom
closure (useful for testing, caching, or calling providers that aren't
yet wired into `LLMEvaluator`).

## CI integration (JUnit XML)

Every run now writes a `junit.xml` file to the run directory, mapping:

| Outcome                           | JUnit element                 |
|-----------------------------------|-------------------------------|
| `Evaluated` & `passed=True`       | bare `<testcase/>`            |
| `Evaluated` & `passed=False`      | `<failure type="AssertionError">` |
| `TimedOut` / `MemoryExceeded`     | `<error type="...">`          |
| `EvaluationError` / `ParseError`  | `<error type="...">`          |
| `KernelDied` / `RunnerError`      | `<error type="...">`          |
| `NoSolution`                      | `<skipped>`                   |

Pass `--junit /path/to/junit.xml` to `scripts/RunBenchmark.wls` to
also write the file to a stable, run-id-independent location (useful
for CI steps that publish by glob).

## Migrating from the legacy notebook

The previous version of this project lived in
`Wolfram Challenges Benchmark 2026-04-19.nb` with all candidate
solutions held in an in-notebook `solutionsAssoc` association. To
extract that into the on-disk layout the new harness expects:

    wolframscript -file scripts/MigrateFromNotebook.wls \
        --notebook "Wolfram Challenges Benchmark 2026-04-19.nb" \
        --model    claude-opus-4.6 \
        --out      solutions

Pass `--dry-run` first to preview the file list. The script writes
`solutions/<model>/<challenge>.wl` plus a `<challenge>.meta.json`
sidecar carrying a SHA-256 source hash for cache-friendliness.

## Programmatic use

    PacletDirectoryLoad["WolframChallengesLLMBenchmarkPaclet"];
    Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

    challenges = LoadChallenges["ChallengesTestDataV1.json"];
    testBank   = LoadTestBank["ChallengesTests.wxf"];
    solutions  = LoadSolutions["solutions/claude-opus-4.6"];

    run = RunBenchmark[challenges, testBank, solutions,
      "Model"            -> "claude-opus-4.6",
      "TimeConstraint"   -> 60,
      "MemoryConstraint" -> 2*^9,
      "Parallel"         -> 8,
      "IsolationMode"    -> "PerTestKernel"
    ];

    paths = WriteReport[run, run["runDir"]];
    SystemOpen[paths["html"]];

`RunBenchmark` returns an Association with keys `runId`, `runDir`,
`meta`, and `results`. `meta["summary"]` carries a one-line scoreboard
(passed / total, pass rate, per-status counts, fastest/slowest tests).

## Result schema

Every entry in `results` has the same shape:

    <|
      "challengeName" -> "Aliquot",
      "testIndex"     -> 1,
      "testId"        -> "Aliquot/1",
      "model"         -> "claude-opus-4.6",
      "status"        -> "Evaluated" | "TimedOut" | "MemoryExceeded"
                       | "EvaluationError" | "ParseError" | "NoSolution"
                       | "KernelDied" | "RunnerError",
      "passed"        -> True | False,
      "expected"      -> <expected value>,
      "actualOutput"  -> <candidate's return value or sentinel Missing[...]>,
      "messageCount"  -> <number of WL messages emitted>,
      "error"         -> <human-readable diagnostic or None>,
      "durationSec"   -> <wall-clock seconds>,
      "memoryBytes"   -> 0
    |>

Only `status -> "Evaluated"` results can be `passed -> True`; all
other statuses imply `passed -> False`. The per-test comparison is
`SameQ` by default; per-challenge overrides may be supplied via the
test bank's `metadata["sameTest"]` key, or globally via the
`"SameTestFunction"` option to `RunBenchmark`.

## Isolation modes

| Mode             | Speed  | Safety | When to use                              |
|------------------|--------|--------|------------------------------------------|
| `PerTestKernel`  | slower | high   | Default. Untrusted candidate code.       |
| `PooledKernels`  | faster | medium | Trusted candidates, tight test budgets.  |
| `InProcess`      | fastest| none   | Harness self-tests only.                 |

`PerTestKernel` launches a fresh subkernel per test via `LocalSubmit`,
then captures the result from a `TaskFinished` handler that pushes
into an `Internal\`Bag`. The driver loop keeps `parallel` tasks in
flight; on kernel death we deduplicate by `TaskUUID` and optionally
retry (`--retryOnKernelDeath`).

See `ALTERNATIVE_TESTS.md` for the design rationale behind the
runner and the alternative monitoring patterns that can layer on top.

## Continuous integration

The pieces you need are already in place:

    # One-shot smoke test of the harness itself
    wolframscript -file tests/RunTests.wls

    # Nightly regression run, fails the build on any regression
    wolframscript -file scripts/RunBenchmark.wls \
        --challenges ChallengesTestDataV1.json \
        --testbank   ChallengesTests.wxf \
        --solutions  solutions/claude-opus-4.6 \
        --model      claude-opus-4.6 \
        --out        runs \
        --baseline   runs/baseline-claude-opus-4.6 \
        --fail-under 0.60

The exit code carries the failure mode; the per-run directory
contains the detailed report.
