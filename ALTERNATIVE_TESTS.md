# Alternative test creation and monitoring approaches

The refactored harness drives the test bank with a custom runner
(`LocalSubmit` + `HandlerFunctions` + `Internal\`Bag`) because the
benchmark has three demands that no single built-in primitive covers:

1. Process-level isolation so a candidate's `Quit[]` or `$Aborted`
   cannot take the driver down.
2. Both time *and* memory constraints, enforced in the worker, with the
   limit breach reported as a structured status rather than swallowed.
3. A streaming result pipeline so the UI can render progress as tests
   complete instead of only at the end.

This document surveys the Wolfram primitives that were considered, why
each was rejected or adopted only in part, and sketches a few
alternative monitoring patterns that can layer on top of the current
design without rewriting the runner.

## Built-in test primitives

### `VerificationTest`

`VerificationTest[expr, expected]` evaluates `expr` and compares it to
`expected`. It records wall time, memory used, and any messages
emitted, returning a `TestResultObject`. It takes a `SameTest` option
and a `MemoryConstraint` option (default `Infinity`); `TimeConstraint`
is honored too.

It is the natural unit in a Wolfram-native test suite, and the harness
uses it for its **own** self-tests (`tests/BenchmarkTests.wlt`). It is
not a good fit for candidate code because:

- It runs in the calling kernel. A `Quit[]` or runaway loop in a
  candidate takes the whole harness down.
- Messages cause the test to be marked failed even when the return
  value is correct; LLM code is noisy and this produces spurious
  failures.
- `expected` must be a literal expression available at test-creation
  time. The test bank stores inputs as `HoldComplete[...]` held
  expressions, so the candidate's definitions must be installed
  *before* `expected` is evaluated for comparison — which
  `VerificationTest` has no hook for.

### `TestCreate` / `TestEvaluate` / `TestReport`

`TestCreate` builds a `TestObject` without evaluating it; `TestEvaluate`
runs one; `TestReport` runs a collection. Together they give you a
lightweight test-case registry with a `TestResultObject` per case.

We use this pair in `tests/RunTests.wls` to render the harness
self-tests with a pass/fail summary. It is deliberately kept out of
the candidate-evaluation path for the reasons above.

### `WriteUnitTest` (Function Repository)

`ResourceFunction["WriteUnitTest"]` writes a `VerificationTest` literal
to disk, capturing *current* behavior as the oracle. It is useful for
**golden-output** testing — "whatever this function returns today is
what we expect tomorrow" — and could be a way to seed a reference run
from a known-good model:

    ResourceFunction["WriteUnitTest"][
      myHarness[challengeName],
      FileNameJoin[{"tests", "golden", challengeName <> ".wlt"}]
    ]

This is out of scope for the benchmark itself (the test bank is the
oracle, not any particular model's output), but it is a good
pattern for locking down the harness's own output format across
refactors. A future exercise: run the harness on a small, stable
model, snapshot the result with `WriteUnitTest`, and assert byte
equality in CI.

## Isolation options

### `TimeConstrained` / `MemoryConstrained`

Both honor their third argument — a sentinel expression returned when
the constraint is breached. This is the cleanest failure signaling in
the language and the harness relies on it:

    TimeConstrained[
      MemoryConstrained[body, memBytes, $MemSentinel],
      timeSec, $TimeSentinel
    ]

Sentinels are `Unique[]`-generated symbols so they cannot collide with
any legitimate candidate return value.

Limitation: these primitives interrupt the evaluator cooperatively.
A tight, non-interruptible C loop inside a compiled function can
defeat `TimeConstrained`. In practice the test bank's inputs don't
trigger this, but it is why we still want process isolation on top.

### `LocalSubmit`

`LocalSubmit[expr]` launches a fresh subkernel, returns a
`TaskObject`, and removes the subkernel when the task finishes (the
default `AutoRemove -> True`). This gives us real process-level
isolation: `Quit[]`, `Abort[]`, out-of-memory, or undefined-symbol
warnings in the candidate cannot leak into the driver. Wall-clock
parallelism is a side effect of having N tasks in flight at once.

Pitfalls we hit:

- `TaskObject["EvaluationResult"]` returns unevaluated after
  completion when `AutoRemove -> True` — the subkernel is gone by the
  time you ask. Fix: use `HandlerFunctions` with a `TaskFinished`
  callback and push results into a buffer.
- `WaitNext` does not apply to `TaskObject`s (it's for
  `EvaluationObject`s produced by `Parallel`-family functions).
- On kernel death, the handler fires twice. We dedupe by `TaskUUID`.

### `ParallelSubmit` on a `LaunchKernels` pool

Uses a fixed pool of subkernels (`LaunchKernels[n]`) with
`ParallelSubmit` / `ParallelMap`. Exposed as `"PooledKernels"`
isolation mode. Faster on large test sets because the subkernels
don't pay startup cost per test, but a misbehaving candidate can
poison a worker for subsequent tests on the same kernel. We call
`Remove["Global\`*"]` between tests as a soft reset, which is
best-effort only.

### In-process `Block`

Exposed as `"InProcess"` isolation mode. Used only for the harness
self-tests, where the candidate is trusted and we want the test suite
to run in a single kernel for speed. `Block[{$Context=...}]` does
**not** help against untrusted code — see the comment in
`Runner.wl::evaluateOneTest`.

## Monitoring

### Streaming JSONL

The runner emits one record per `run.start` / `test.submit` /
`test.complete` / `run.end` event to `progress.jsonl`, flushed via
open-append-close on each write. This is deliberately boring so that:

- `tail -f runs/<runId>/progress.jsonl | jq .` gives you a live log.
- A notebook `DynamicModule` can poll the file for the dashboard.
- Crashes leave the history on disk.

### `LiveDashboard[runDir]`

Provided in `Report.wl`. Builds a `DynamicModule` that tails the
JSONL file and re-renders a KPI + per-challenge view on each tick.
Works unmodified inside a notebook; no Wolfram Cloud required.

### External monitoring

Because the progress is JSONL with ISO-8601 timestamps, anything that
consumes newline-delimited JSON works: `jq`, `vector`, Grafana-Loki,
OpenTelemetry via a tail-shipper. If we add an HTTP sink later, the
simplest path is a `ProgressHandler` option already wired into
`RunBenchmark`:

    RunBenchmark[challenges, testBank, solutions,
      "Model" -> "claude-opus-4.6",
      "ProgressHandler" -> Function[ev,
        URLSubmit[HTTPRequest[endpoint,
          <|"Method" -> "POST",
            "Body" -> ExportString[ev, "RawJSON"]|>]]
      ]
    ]

The handler is called synchronously on the driver kernel from inside
the drain loop, so it should avoid blocking for more than a few tens
of milliseconds; the recommended pattern is `URLSubmit` (fire and
forget) rather than `URLExecute`.

### Per-run HTML report

`WriteReport` renders three static artifacts into the run directory
(`report.html`, `report.md`, `report.json`) that can be served as-is
from any static file server or checked into git for review. The
structure is:

- KPI header (pass rate, counts, duration).
- Status breakdown.
- Per-challenge pass/fail grid.
- Collapsible `<details>` blocks for each failing test showing
  expected vs. actual, messages, status, duration.

## Extensions

Ideas worth prototyping later:

- **Property-based testing.** Use `ResourceFunction["RandomExpression"]`
  to fuzz candidate functions that are supposed to be total functions
  of a known signature, comparing the candidate to an oracle
  implementation.
- **Parameterized test cases.** The current test bank is a fixed list.
  Annotating each entry with a `"generators"` key that produces extra
  input/expected pairs on the fly would broaden coverage without
  inflating the WXF file.
- **Test matrix over models.** `RunBenchmark` takes one model per
  invocation. A small driver script that sweeps
  `models/<model>/*` directories and emits a cross-model comparison
  report would make regression tracking across the model zoo
  one-command.
- **Differential testing.** `DiffRuns` already identifies
  regressions/fixes between two runs. A scheduled nightly CI could
  fail the build if any regressions are introduced, using the
  `--baseline` / `--fail-under` flags already supported by
  `scripts/RunBenchmark.wls`.
- **Golden-output lock for the harness itself.** Use `WriteUnitTest`
  against a small deterministic candidate (e.g. `Identity`) to
  generate `tests/golden.wlt`, then assert equality on every refactor
  to catch accidental output-schema changes.
