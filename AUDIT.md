# Wolfram Challenges Benchmark — Architecture Audit

Source files reviewed:

- `Wolfram Challenges Benchmark 2026-04-19.nb` (15,795 lines)
- `ChallengesTests.wxf` (~1 MB held-expression test data, 166 challenges)
- `ChallengesTestDataV1.json` (~315 KB challenge prompts)

## 1. Current architecture, in one paragraph

The benchmark lives entirely inside a single `.nb` notebook. It loads the
challenge prompts from `ChallengesTestDataV1.json`, calls an LLM
(`LLMSynthesize`) once per challenge to generate a Wolfram Language solution,
strips the markdown fences with a `codeExtract` helper, stores the resulting
strings in a global `solutionsAssoc` association keyed by challenge name,
loads `ChallengesTests.wxf` into `allTestsDataset`, and finally builds a list
of `TestCreate[...]` objects via `makeTest[challengeName, testNumber]`. Tests
are executed by `Map[TestReport, allTests]` and a pass count is summarized
with `Counts[#["ReportSucceeded"] & /@ reportsOpusV3]`. A handful of
`BarChart`/`Infographics` cells render the final headline number.

## 2. Data flow

```
ChallengesTestDataV1.json ──► challengesDataset ──► callLLMOnChallenge ──► results{Opus|GPT...}
                                                                                  │
                                                                                  ▼
                                                                            codeExtract
                                                                                  │
                                                                                  ▼
ChallengesTests.wxf ───────► allTestsDataset                            solutionsAssoc
                                          │                                       │
                                          └────────────► makeTest ◄───────────────┘
                                                            │
                                                            ▼
                                                        allTests
                                                            │
                                                            ▼
                                                  Map[TestReport, allTests]
                                                            │
                                                            ▼
                                                       reportsOpusV3
                                                            │
                                                            ▼
                                                Counts[#["ReportSucceeded"] &]
```

## 3. Key code surfaces

| Symbol               | Role                                                | Lines (approx) |
|----------------------|-----------------------------------------------------|----------------|
| `challengesDataset`  | Imported challenge prompts                          | 45             |
| `challengesNames`    | Hard-coded list of 166 challenge IDs                | 67             |
| `allTestsDataset`    | Imported `.wxf` of held expected I/O pairs          | 226            |
| `callLLMOnChallenge` | Builds prompt, calls `LLMSynthesize`                | 271            |
| `codeExtract`        | Regex-strips ```` ```wl ... ``` ```` fences        | 3992           |
| `solutionsAssoc`     | `<|challengeName -> codeString, ...|>`              | 1161, 6680     |
| `makeTest`           | Builds a `TestCreate` from solution + held test     | 7913           |
| `allTests`           | `MapThread` of all `makeTest` results               | 8150           |
| `reportsOpusV3`      | `Map[TestReport, allTests]` per-challenge results   | 8198           |
| `Infographics`       | `BarChart` of pass counts                           | 8267           |

## 4. What's wrong with it (ranked by risk)

### 4.1 Critical — safety of evaluating untrusted LLM output

`makeTest` does:

```mathematica
ImportString[ solutionsAssoc[[challengeName]], {"WL", "HeldExpressions"} ]
```

…then injects the held definition into a `TestCreate[(def; input), output, TimeConstraint -> 60]`.

- The expression is **only `TimeConstraint`-bounded — there is no `MemoryConstraint`**, so an LLM can produce a one-liner that allocates until the kernel OOMs.
- It runs in the **main interactive kernel**. A single test that calls
  `Quit[]`, `SetDirectory["/"]`, `DeleteFile`, `URLExecute`,
  `ExternalEvaluate`, or that mutates `$ContextPath`/global symbols, will
  contaminate every subsequent test in the run. There is no per-test
  sandbox or fresh context.
- The held expression is `ReleaseHold`-equivalent on evaluation. If the LLM
  produces a top-level `SetDelayed` whose RHS calls `Get` on a
  network-mounted package, that side effect is trusted.

### 4.2 Critical — silent test contamination

All 166 challenges are evaluated in the same kernel session, so symbol
collisions (`f`, `next`, `seen`, `solve`, etc., used as locals in `Module`
but also as globals in many LLM solutions) leak. The first test that
defines `f[x_] := ...` outside a `Module` poisons the next challenge that
relies on `f`. Because `TestReport` records only `Outcome -> "Success"|"Failure"`,
this is invisible.

### 4.3 High — no per-run identity, no persistence

`reportsOpusV3` is a kernel-local symbol. There is no run id, no model
version pinning, no timestamp, no Wolfram version, no system info.
Re-running the notebook overwrites `reportsOpus`, `reportsOpusV3`, etc.,
and historical comparisons rely on the variable name suffixes the author
added by hand (`V3`).

### 4.4 High — no error model

Anywhere `LLMSynthesize`, `Import`, or `ImportString` can fail, the failure
becomes a `$Failed`/`Failure[...]` that propagates silently into
`solutionsAssoc` and then becomes a `TestCreate` whose evaluation throws.
There is no `Check` / `CheckAbort` boundary, no retry, and no structured
error record.

### 4.5 High — sequential execution

`Map[TestReport, allTests]` runs all 166 reports on a single kernel.
With a 60-second `TimeConstraint` and ~3.6 tests per challenge on average,
worst-case wall time is ~10 hours. There is no `ParallelMap`, no
`LocalSubmit`, and no progress indicator beyond Mathematica's busy
indicator.

### 4.6 Medium — `codeExtract` is brittle

The regex matches ```` ```wl ````, ```` ```wolfram ````, ```` ```mathematica ````,
and (sic) ```` ```wollframlanguage ````. Any model that emits ```` ``` ```` with
no language hint, or `~~~` fences, or a leading prose paragraph followed by an
unfenced `Module[...]`, falls through to "return the whole text", which then
fails to parse as WL.

### 4.7 Medium — schema-free inputs

`Import["ChallengesTestDataV1.json"]` and `Import["ChallengesTests.wxf"]` are
trusted to have the expected shape. There is no validation that
`allTestsDataset[challengeName]` exists, or that each entry is
`{HoldComplete[input], output}`. A malformed file produces a cryptic
`Part::partw` error mid-run.

### 4.8 Medium — duplicated prompt + harness logic per model

The "Generate All Solutions with GPT-5.4", "Claude Opus 4.6", and
"Increasing the time constraint to 60 seconds" sections each redefine
`callLLMOnChallenge`, re-import `allTestsDataset`, and re-run a copy of
`makeTest`. Adding a new model means copy-pasting an entire section.

### 4.9 Low — dead and one-off cells

Several `Length @ allTestsDataset` cells, exploratory
`testsPerChallenge` printouts, and `Infographics` blocks with hard-coded
numbers (`{23, 37, 105}`) live alongside the real benchmark code with no
visible separation.

## 5. What to keep

- The **held-expression test format** (`{HoldComplete[input], expectedOutput}`)
  is a good design — it preserves arbitrary WL inputs without re-parsing.
- The **two-file split** (challenge prompts in JSON, expected I/O in WXF)
  is right; WXF is faster and round-trips Mathematica expressions losslessly.
- The **`MetaInformation -> <|"ChallengeName" -> ...|>`** annotation on
  `TestCreate` is the right hook for grouping results downstream.
- `ImportString[..., {"WL", "HeldExpressions"}]` is the correct way to parse
  candidate code without evaluating it — it just needs to be paired with a
  sandbox.

## 6. Recommended target architecture

```
ChallengesTestDataV1.json ─┐
ChallengesTests.wxf      ─┤  ChallengesBenchmark`Loader`        (validates schema)
                         ─┘
                                         │
                                         ▼
                          ChallengesBenchmark`Solutions`        (per-model solution store, on disk, content-addressed)
                                         │
                                         ▼
                          ChallengesBenchmark`Runner`           (one LocalSubmit per test, fresh `Begin["BenchTest`xxx`"]` context, TimeConstraint + MemoryConstraint, captured messages)
                                         │
                                         ▼
                          ChallengesBenchmark`Results`          (typed Association: <|runId, model, challenge, testIndex, outcome, expected, actual, messages, timeUsed, memoryUsed, kernelExitCode|>)
                                         │
                  ┌──────────────────────┼──────────────────────┐
                  ▼                      ▼                      ▼
       JSONL progress stream     HTML/Markdown report     baseline diff (regressions)
```

## 7. Notes on the Wolfram primitives we should standardize on

(Wolfram reference docs were not reachable from this environment, so the
following is from the language as I know it; please cross-check before
locking in.)

### `VerificationTest` / `TestCreate`

- `VerificationTest[input, expected]` runs immediately and returns a
  `TestResultObject` with properties including `"Outcome"` (`"Success"`,
  `"Failure"`, `"MessagesFailure"`, `"Error"`), `"Input"`,
  `"ExpectedOutput"`, `"ActualOutput"`, `"ExpectedMessages"`,
  `"ActualMessages"`, `"AbsoluteTimeUsed"`, `"MemoryUsed"`, `"TestID"`.
- `TestCreate[input, expected, opts...]` builds the test object **without
  evaluating it**, which is what `makeTest` already uses. Pair with
  `TestEvaluate[testObj]` to run on demand.
- Useful options to standardize: `SameTest -> (...)` (custom equality, e.g.
  `Equal` for numerics with tolerance), `TestID -> {challenge, i}`,
  `TimeConstraint -> 60`, `MemoryConstraint -> 2*^9` (2 GB),
  `MetaInformation -> <|...|>`.

### `TestReport`

- Aggregates a list of `VerificationTest`/`TestCreate` objects (or a
  `.wlt` file) into a `TestReportObject` with properties like
  `"TestsSucceededCount"`, `"TestsFailedCount"`,
  `"TestsFailedWithWrongResultsCount"`, `"TestsFailedWithMessagesCount"`,
  `"TestsFailedWithErrorsCount"`, `"TestResults"`, `"TimeElapsed"`,
  `"ReportSucceeded"`.
- Today the notebook uses just `"ReportSucceeded"`. We should be storing
  the entire `TestReportObject` per challenge and serializing the
  `TestResults` association so failures are diagnosable after the fact.

### `WriteUnitTest` (Function Repository)

- `ResourceFunction["WriteUnitTest"][expr]` evaluates `expr` once and
  emits the corresponding `VerificationTest[...]` source code with the
  observed output baked in.
- Right tool for **growing the test bank** from new challenge examples
  without hand-typing expected outputs. Good fit for a future
  `regenerate-goldens.wls` script.

### `LocalSubmit` (the user's specific request)

- `LocalSubmit[expr]` enqueues `expr` to run in a **separate local kernel
  process**, returning a `LocalEvaluationObject`. Unlike `ParallelSubmit`
  (which targets the parallel-kernel pool), `LocalSubmit` spawns an
  ordinary background subkernel — well-suited to running untrusted
  candidate code because:
  1. Crashes / `Quit[]` in the candidate kill the worker, not the driver.
  2. Symbols defined by the candidate never touch the driver's contexts.
  3. We can apply `TimeConstrained` *and* an outer wall-clock kill
     (`AbortKernels[]` / `KillProcess[]`) if the candidate ignores it.
- Wait primitives:
  - `WaitAll[{job1, job2, ...}]` — block until all complete, returns
    results in order.
  - `WaitNext[{...}]` — block until *one* completes; returns
    `{result, remaining}`. This is the right primitive for a streaming
    progress UI.
- Worth standardizing in the runner:

  ```mathematica
  job = LocalSubmit[
    TimeConstrained[
      MemoryConstrained[
        Block[{$ContextPath = $ContextPath, $Context = "BenchTest`" <> id <> "`"},
          ReleaseHold[def]; input
        ],
        2*^9, $MemoryFailed],
      60, $TimeFailed],
    HandlerFunctions -> <|
      "EvaluationCompleted" -> recordResult,
      "EvaluationCanceled" -> recordCancel
    |>
  ];
  ```

- Pool sizing: cap concurrent `LocalSubmit` jobs at `$ProcessorCount - 1`
  via a small semaphore (`Internal\`Bag` of pending `LocalEvaluationObject`s)
  and use `WaitNext` to drain.

### Adjacent functions worth a look

- **`RunScheduledTask` / `SessionSubmit`** — for periodic re-runs.
- **`HandlerFunctions` on `LocalSubmit`** — push events into a JSONL stream
  for live monitoring instead of polling.
- **`Catch` / `Throw`, `CheckAbort`, `Internal\`HandlerBlock`** — the
  inner ring of error containment around `ReleaseHold[def]; input`.
- **`Begin` / `End`** with a per-test private context to fully isolate
  symbol bindings even when running in-process (fallback if `LocalSubmit`
  is unavailable, e.g. constrained CI).
- **`TestReportObject[...]["TestResults"]`** is keyed by `TestID`. Standardizing
  `TestID -> {challengeName, testIndex}` makes downstream diffs trivial.

## 8. Concrete next steps (mapped to the existing task list)

1. Task #1 — Pull `callLLMOnChallenge`, `codeExtract`, `makeTest`, and the
   `Map[TestReport, ...]` driver into `ChallengesBenchmark.wl` with a
   public API (`LoadChallenges`, `LoadTestBank`, `RunBenchmark`,
   `WriteReport`).
2. Task #3 — Replace the in-kernel `TestReport` call with a
   `LocalSubmit`-based runner enforcing `TimeConstrained` +
   `MemoryConstrained` + per-test private context. This is the biggest
   single quality win.
3. Task #4 — Add a schema validator for `ChallengesTests.wxf` and
   `ChallengesTestDataV1.json` at load time.
4. Task #5 — Define the typed result association above and persist runs
   to `runs/<runId>.wxf` + `runs/<runId>.jsonl`.
5. Task #8 — Cover the runner with `VerificationTest`s of its own,
   including a candidate that throws, one that times out, one that OOMs,
   and one that calls `Quit[]`.

## 9. Open questions for the project owner

1. Is there a non-OpenRouter path to Anthropic that should be preferred
   for the Claude row? The current `{"OpenRouter", "anthropic/claude-opus-4.6"}`
   tuple makes the benchmark dependent on a third-party gateway.
2. Should the benchmark grade exact equality only, or do we want a
   per-challenge `SameTest` (e.g. `Sort` for set-valued answers,
   `Equal` with tolerance for floats)? Several current "failures" are
   likely formatting-only mismatches.
3. What's the canonical place for solutions to live across runs — keep
   them inside the notebook, or commit them as
   `solutions/<model>/<challenge>.wl` so they're diffable in git?
