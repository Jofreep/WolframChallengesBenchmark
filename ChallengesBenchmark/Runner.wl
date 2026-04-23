(* ::Package:: *)

(* :Context: ChallengesBenchmark`Private` *)
(* :Summary: Sandboxed test runner.

   Three isolation modes are supported:

     "PerTestKernel"  Default. Each test runs in its own subkernel via
                      LocalSubmit. Crashes / Quit[] / global mutation in
                      the candidate cannot affect the driver or other
                      tests. Handler-based result capture plus TaskUUID
                      dedupe keeps backpressure at the requested
                      parallelism and tolerates double-delivery on
                      kernel death.

     "PooledKernels"  Maintains a fixed pool launched with LaunchKernels[n]
                      and ParallelSubmits tasks to it. Resets each kernel
                      between tests with Remove["Global`*"]. Faster on
                      large test sets at the cost of weaker isolation —
                      a misbehaving candidate can poison a worker for
                      subsequent tests on the same kernel.

     "InProcess"      Runs in the driver kernel inside a fresh private
                      context. Used only for the harness self-tests.
                      Not safe for untrusted candidate code.

   Result capture design (PerTestKernel):

       bag = Internal`Bag[]
       LocalSubmit[ evaluateOneTest[...],
         HandlerFunctions -> <|"TaskFinished" -> (StuffBag[bag, #]&)|>,
         HandlerFunctionsKeys -> {"EvaluationResult","Failure","TaskUUID"} ]

   The handler receives an Association and appends it to a bag. The
   main loop polls the bag with a short Pause to yield to the task
   event loop. TaskUUIDs are deduped so a kernel death that fires the
   handler twice does not produce a duplicate result.
*)

Begin["ChallengesBenchmark`Private`"];

(* ----------------------------------------------------------------------- *)
(* Public-facing benchmark entry                                           *)
(* ----------------------------------------------------------------------- *)

runBenchmarkImpl[challenges_Association, testBank_Association, solutions_Association,
  opts___?OptionQ] := Module[
  {
    o, runId, outDir, runDir, jsonlPath, resultsWxfPath, runJsonPath,
    filter, parallel, timeC, memC, sameTest, model, seed,
    isolationMode, retryCount, pollInterval, sandbox,
    workItems, totalTests, fingerprint, results, ts0, durationS,
    progressHandler, runMeta
  },

  o = Association[Flatten[{opts}]];
  runId         = Replace[Lookup[o, "RunId", Lookup[$BenchmarkDefaults, "RunId"]],
                          Automatic :> newRunId[]];
  outDir        = Replace[Lookup[o, "OutputDirectory", Lookup[$BenchmarkDefaults, "OutputDirectory"]],
                          Automatic :> FileNameJoin[{Directory[], "runs"}]];
  filter        = Lookup[o, "Filter", Lookup[$BenchmarkDefaults, "Filter"]];
  parallel      = Replace[Lookup[o, "Parallel", Lookup[$BenchmarkDefaults, "Parallel"]],
                          Automatic :> Max[1, $ProcessorCount - 1]];
  timeC         = Lookup[o, "TimeConstraint", Lookup[$BenchmarkDefaults, "TimeConstraint"]];
  memC          = Lookup[o, "MemoryConstraint", Lookup[$BenchmarkDefaults, "MemoryConstraint"]];
  sameTest      = Lookup[o, "SameTestFunction", Lookup[$BenchmarkDefaults, "SameTestFunction"]];
  model         = Lookup[o, "Model", Lookup[$BenchmarkDefaults, "Model"]];
  seed          = Replace[Lookup[o, "Seed", Lookup[$BenchmarkDefaults, "Seed"]],
                          Automatic :> Hash[runId, "SHA256"]];
  isolationMode = Lookup[o, "IsolationMode", Lookup[$BenchmarkDefaults, "IsolationMode"]];
  retryCount    = Lookup[o, "RetryOnKernelDeath", Lookup[$BenchmarkDefaults, "RetryOnKernelDeath"]];
  pollInterval  = Lookup[o, "PollInterval", Lookup[$BenchmarkDefaults, "PollInterval"]];
  progressHandler = Lookup[o, "ProgressHandler", Lookup[$BenchmarkDefaults, "ProgressHandler"]];
  sandbox       = TrueQ @ Lookup[o, "Sandbox", Lookup[$BenchmarkDefaults, "Sandbox"]];

  If[! IntegerQ[parallel] || parallel < 1,
    Message[RunBenchmark::badparallel, parallel]; Return[$Failed]
  ];
  If[model === None,
    Message[RunBenchmark::nomodel]; Return[$Failed]
  ];

  runDir = FileNameJoin[{outDir, runId}];
  If[! DirectoryQ[runDir],
    CreateDirectory[runDir, CreateIntermediateDirectories -> True]];

  jsonlPath      = FileNameJoin[{runDir, "progress.jsonl"}];
  resultsWxfPath = FileNameJoin[{runDir, "results.wxf"}];
  runJsonPath    = FileNameJoin[{runDir, "run.json"}];

  fingerprint = runtimeFingerprint[];
  fingerprint["randomSeedUsed"] = ToString[seed];

  workItems = buildWorkItems[challenges, testBank, solutions, filter, model, sameTest];
  totalTests = Length[workItems];

  If[totalTests === 0,
    logWarn["RunBenchmark: no work items matched the filter — nothing to run."]
  ];

  logInfo[StringTemplate["RunBenchmark: model=`` runId=`` tests=`` parallel=`` mode=``"][
    model, runId, totalTests, parallel, isolationMode]];

  (* Persist the run header up front so a crash leaves something behind. *)
  runMeta = <|
    "runId"            -> runId,
    "model"            -> model,
    "createdAt"        -> DateString["ISODateTime"],
    "runtime"          -> fingerprint,
    "totalTests"       -> totalTests,
    "options"          -> <|
      "timeConstraint"   -> timeC,
      "memoryConstraint" -> memC,
      "parallel"         -> parallel,
      "isolationMode"    -> isolationMode,
      "filter"           -> If[filter === All, "All", filter],
      "seed"             -> ToString[seed],
      "pollInterval"     -> pollInterval,
      "retryOnKernelDeath" -> retryCount,
      "sandbox"          -> sandbox
    |>,
    "status"           -> "running"
  |>;
  Export[runJsonPath, runMeta, "RawJSON"];
  appendJSONL[jsonlPath, <|"event" -> "run.start", "runId" -> runId,
    "totalTests" -> totalTests, "timestamp" -> DateString["ISODateTime"]|>];

  ts0 = AbsoluteTime[];

  results = Switch[isolationMode,
    "PerTestKernel", runWithLocalSubmit[workItems, parallel, timeC, memC,
                       retryCount, pollInterval, jsonlPath, progressHandler, sandbox],
    "PooledKernels", runWithKernelPool[workItems, parallel, timeC, memC,
                       jsonlPath, progressHandler, sandbox],
    "InProcess",     runInProcess[workItems, timeC, memC,
                       jsonlPath, progressHandler, sandbox],
    _, (Message[RunBenchmark::badmode, isolationMode]; Return[$Failed])
  ];

  If[results === $Failed, Return[$Failed]];

  durationS = AbsoluteTime[] - ts0;

  Quiet @ Check[Export[resultsWxfPath, results, "WXF"],
    logError["Failed to write results.wxf to " <> resultsWxfPath]];

  runMeta["status"]      = "completed";
  runMeta["finishedAt"]  = DateString["ISODateTime"];
  runMeta["durationSec"] = durationS;
  runMeta["summary"]     = summarizeResults[results];
  Export[runJsonPath, runMeta, "RawJSON"];

  appendJSONL[jsonlPath, <|"event" -> "run.end", "runId" -> runId,
    "durationSec" -> durationS, "summary" -> runMeta["summary"]|>];

  logInfo[StringTemplate["RunBenchmark complete: `` (passed `` / ``)"][
    runId, runMeta["summary", "passed"], totalTests]];

  <|
    "runId"   -> runId,
    "runDir"  -> runDir,
    "meta"    -> runMeta,
    "results" -> results
  |>
];

RunBenchmark::nomodel     = "RunBenchmark requires the Model option to be set.";
RunBenchmark::badmode     = "Unknown IsolationMode: `1`.";
RunBenchmark::badparallel = "Parallel must be a positive integer; got `1`.";

(* ----------------------------------------------------------------------- *)
(* Build the flat work list                                                *)
(* ----------------------------------------------------------------------- *)

buildWorkItems[challenges_, testBank_, solutions_, filter_, model_, sameTest_] := Module[
  {names},
  names = Keys[testBank];
  If[filter =!= All, names = Intersection[names, filter]];
  Flatten @ Map[
    Function[name,
      Module[{tests, sol},
        sol  = Lookup[solutions, name, Missing["NoSolution"]];
        tests = testBank[name];
        MapIndexed[
          <|
            "challengeName" -> name,
            "testIndex"     -> First[#2],
            "testId"        -> name <> "/" <> IntegerString[First[#2]],
            "model"         -> model,
            "input"         -> #1["input"],              (* HoldComplete *)
            "expected"      -> #1["expected"],
            "metadata"      -> Lookup[#1, "metadata", <||>],
            "solutionCode"  -> If[AssociationQ[sol], sol["code"], Missing["NoSolution"]],
            "sameTest"      -> sameTest
          |> &,
          tests
        ]
      ]
    ],
    names
  ]
];

(* ----------------------------------------------------------------------- *)
(* PerTestKernel: LocalSubmit with handler-based result capture            *)
(* ----------------------------------------------------------------------- *)

(* Default poll interval — short enough to feel live, long enough to avoid
   busy-waiting. The main kernel sleeps this long between bag drains. *)
$DefaultPollInterval = 0.05;

runWithLocalSubmit[workItems_List, parallel_Integer, timeC_, memC_,
  retryCount_Integer, pollInterval_, jsonlPath_String, progressHandler_,
  sandbox_:True] :=
  Module[
    {pending = workItems, inFlight = <||>, completed = {},
     bag, seenUUIDs = <||>, totalCount, doneCount = 0,
     submit, drain, normalized, sleepFor},

    totalCount = Length[workItems];
    bag = Internal`Bag[];
    sleepFor = If[NumericQ[pollInterval] && pollInterval > 0,
                  pollInterval, $DefaultPollInterval];

    (* --- submit: launch one LocalSubmit task and register it ---------- *)
    submit[wi_Association, attemptNum_Integer] := Module[{task, uuid},
      task = Quiet @ Check[
        LocalSubmit[
          ChallengesBenchmark`Private`evaluateOneTest[wi, timeC, memC, sandbox],
          HandlerFunctions -> <|
            "TaskFinished" -> Function[a, Internal`StuffBag[bag, a]]
          |>,
          HandlerFunctionsKeys -> {"EvaluationResult", "Failure", "TaskUUID"}
        ],
        $Failed
      ];
      If[task === $Failed || ! MatchQ[task, _TaskObject],
        (* LocalSubmit itself failed — emit a synthetic runner-error result. *)
        logError["LocalSubmit failed for " <> wi["testId"]];
        AppendTo[completed,
          normalizeResult[wi,
            <|"status" -> "RunnerError",
              "error" -> "LocalSubmit refused the job",
              "actualOutput" -> Missing["RunnerError"],
              "messageCount" -> 0, "durationSec" -> 0., "memoryBytes" -> 0|>,
            0.]];
        doneCount += 1;
        Return[]
      ];
      uuid = task["TaskUUID"];
      inFlight[uuid] = <|
        "task" -> task, "workItem" -> wi, "attempt" -> attemptNum,
        "submittedAt" -> AbsoluteTime[]
      |>;
      appendJSONL[jsonlPath, <|
        "event" -> "test.submit", "testId" -> wi["testId"],
        "attempt" -> attemptNum, "timestamp" -> DateString["ISODateTime"]|>];
    ];

    (* --- drain: pull finished tasks from the bag, emit results ------- *)
    drain[] := Module[{items, ev, uuid, rec, raw, dt, normResult},
      If[Internal`BagLength[bag] === 0, Return[]];
      items = Internal`BagPart[bag, All];
      bag   = Internal`Bag[];   (* fresh bag for new arrivals *)
      Do[
        ev   = items[[i]];
        uuid = Lookup[ev, "TaskUUID", None];
        Which[
          uuid === None, logWarn["Handler event missing TaskUUID; dropping."],
          KeyExistsQ[seenUUIDs, uuid],
            (* Kernel death can fire TaskFinished twice; second delivery
               is a no-op. *)
            Null,
          ! KeyExistsQ[inFlight, uuid],
            (* Unknown UUID — most likely a late-arriving event for a
               task we already retired. *)
            Null,
          True,
            seenUUIDs[uuid] = True;
            rec = inFlight[uuid];
            KeyDropFrom[inFlight, uuid];
            dt  = AbsoluteTime[] - rec["submittedAt"];

            (* On success: Failure -> Missing["NotAvailable"], EvaluationResult
               -> the sandbox Association. On kernel death: Failure ->
               Failure["RemoteKernelFailure", ...], EvaluationResult ->
               Missing["NotAvailable"]. Only the _Failure case means the
               task itself did not run to completion. *)
            raw = Which[
              MatchQ[Lookup[ev, "Failure", None], _Failure],
                ev["Failure"],
              True,
                Lookup[ev, "EvaluationResult", $Failed]
            ];

            normResult = normalizeResult[rec["workItem"], raw, dt];

            If[normResult["status"] === "KernelDied" && rec["attempt"] <= retryCount,
              logWarn["Kernel died on " <> rec["workItem", "testId"] <>
                      " (attempt " <> IntegerString[rec["attempt"]] <> "); retrying."];
              submit[rec["workItem"], rec["attempt"] + 1],
              (* else record the result *)
              AppendTo[completed, normResult];
              doneCount += 1;
              appendJSONL[jsonlPath, <|
                "event"       -> "test.complete",
                "testId"      -> normResult["testId"],
                "status"      -> normResult["status"],
                "passed"      -> normResult["passed"],
                "durationSec" -> normResult["durationSec"],
                "doneCount"   -> doneCount,
                "totalCount"  -> totalCount,
                "timestamp"   -> DateString["ISODateTime"]
              |>];
              If[progressHandler =!= None,
                Quiet @ Check[
                  progressHandler[<|
                    "doneCount"  -> doneCount,
                    "totalCount" -> totalCount,
                    "result"     -> normResult|>],
                  Null
                ]
              ]
            ]
        ],
        {i, Length[items]}
      ]
    ];

    (* --- main loop --------------------------------------------------- *)
    While[Length[pending] > 0 || Length[inFlight] > 0,

      (* Fill the in-flight slots up to `parallel`. *)
      While[Length[pending] > 0 && Length[inFlight] < parallel,
        submit[First[pending], 1];
        pending = Rest[pending];
      ];

      (* Yield to let TaskFinished handlers fire, then drain. *)
      If[Length[inFlight] > 0,
        Pause[sleepFor];
        drain[];
      ];
    ];

    (* One final drain in case of straggler deliveries. *)
    drain[];

    SortBy[completed, {#["challengeName"], #["testIndex"]} &]
  ];

(* ----------------------------------------------------------------------- *)
(* PooledKernels: reuse LaunchKernels[n] subkernels                        *)
(* ----------------------------------------------------------------------- *)

runWithKernelPool[workItems_List, parallel_Integer, timeC_, memC_,
  jsonlPath_String, progressHandler_, sandbox_:True] :=
  Module[{kernels, rawResults, results, totalCount},
    totalCount = Length[workItems];
    kernels = LaunchKernels[parallel];

    (* Make evaluateOneTest and the sandbox helpers visible on all workers. *)
    DistributeDefinitions[
      ChallengesBenchmark`Private`evaluateOneTest,
      ChallengesBenchmark`Private`parseHeldWL,
      ChallengesBenchmark`Private`$TimeSentinel,
      ChallengesBenchmark`Private`$MemSentinel,
      ChallengesBenchmark`Private`$EvaluationError,
      ChallengesBenchmark`Private`sandboxTrap,
      ChallengesBenchmark`Private`applySandbox,
      ChallengesBenchmark`Private`$SandboxDenylist
    ];

    rawResults = ParallelMap[
      Function[wi,
        Module[{r},
          r = ChallengesBenchmark`Private`evaluateOneTest[wi, timeC, memC, sandbox];
          (* Best-effort per-test reset on the worker. *)
          Quiet @ Check[Remove["Global`*"], Null];
          {wi, r}
        ]
      ],
      workItems,
      DistributedContexts -> Automatic,
      Method -> "FinestGrained"
    ];

    Quiet @ CloseKernels[kernels];

    results = MapIndexed[
      Function[{pair, i},
        Module[{wi = pair[[1]], r = pair[[2]], normResult},
          normResult = normalizeResult[wi, r, 0.];
          appendJSONL[jsonlPath, <|
            "event"      -> "test.complete",
            "testId"     -> normResult["testId"],
            "status"     -> normResult["status"],
            "passed"     -> normResult["passed"],
            "doneCount"  -> First[i],
            "totalCount" -> totalCount,
            "timestamp"  -> DateString["ISODateTime"]|>];
          If[progressHandler =!= None,
            Quiet @ Check[progressHandler[<|
              "doneCount"  -> First[i],
              "totalCount" -> totalCount,
              "result"     -> normResult|>], Null]
          ];
          normResult
        ]
      ],
      rawResults
    ];

    SortBy[results, {#["challengeName"], #["testIndex"]} &]
  ];

(* ----------------------------------------------------------------------- *)
(* InProcess: only for self-tests of the harness                           *)
(* ----------------------------------------------------------------------- *)

runInProcess[workItems_List, timeC_, memC_, jsonlPath_String,
  progressHandler_, sandbox_:True] :=
  Module[{totalCount, doneCount = 0},
    totalCount = Length[workItems];
    Map[
      Function[wi,
        Module[{r, normResult},
          (* The sandbox message is informational — it signals a blocked
             candidate call, not a runner failure. Quiet disables the
             message, which in turn means Check does not react to it,
             so legitimate runner errors still fall through to Check's
             fallback association. *)
          r = Check[
                Quiet[
                  ChallengesBenchmark`Private`evaluateOneTest[wi, timeC, memC, sandbox],
                  {ChallengesBenchmark`RunBenchmark::sandbox}
                ],
                <|"status" -> "RunnerError", "error" -> "sandbox threw",
                  "actualOutput" -> Missing["RunnerError"],
                  "messageCount" -> 0, "durationSec" -> 0., "memoryBytes" -> 0|>
              ];
          normResult = normalizeResult[wi, r, 0.];
          doneCount += 1;
          appendJSONL[jsonlPath, <|
            "event"      -> "test.complete",
            "testId"     -> normResult["testId"],
            "status"     -> normResult["status"],
            "passed"     -> normResult["passed"],
            "doneCount"  -> doneCount,
            "totalCount" -> totalCount,
            "timestamp"  -> DateString["ISODateTime"]|>];
          If[progressHandler =!= None,
            Quiet @ Check[progressHandler[<|
              "doneCount"  -> doneCount,
              "totalCount" -> totalCount,
              "result"     -> normResult|>], Null]
          ];
          normResult
        ]
      ],
      workItems
    ]
  ];

(* ----------------------------------------------------------------------- *)
(* Capability sandbox for untrusted candidate code                         *)
(*                                                                         *)
(* LLM-generated solutions occasionally reach for things no benchmark      *)
(* answer should need: DeleteFile, Run, URLFetch, WriteString to random    *)
(* paths, etc. Per-kernel isolation already contains crashes, but a        *)
(* malicious or buggy call like DeleteFile["/"] affects the *host*, not    *)
(* the kernel. The TimeConstraint / MemoryConstraint layer also cannot     *)
(* catch side effects that complete instantly.                             *)
(*                                                                         *)
(* Strategy:                                                               *)
(*   Wrap each test evaluation in Block[{ deniedSym = sandboxTrap, ... }]. *)
(*   Block does not call Set, so it can rebind Protected System symbols    *)
(*   without Unprotect. Inside the Block, a call like DeleteFile["x"]      *)
(*   evaluates the head (DeleteFile -> sandboxTrap), then                  *)
(*   sandboxTrap["x"] emits a message and returns $Failed. Outside the     *)
(*   Block, DeleteFile is restored to its System` definition.              *)
(*                                                                         *)
(* Denylist covers: filesystem mutation, process spawn, network I/O, and   *)
(* file handle openers. Pure-reader primitives like Import and ReadString  *)
(* are permitted because many legitimate solutions need them (text-file    *)
(* lookups, WordList, etc.).                                               *)
(* ----------------------------------------------------------------------- *)

SetAttributes[ChallengesBenchmark`Private`sandboxTrap, HoldAllComplete];
ChallengesBenchmark`Private`sandboxTrap[args___] := (
  Message[ChallengesBenchmark`RunBenchmark::sandbox];
  $Failed
);

ChallengesBenchmark`RunBenchmark::sandbox =
  "Sandbox policy blocked a side-effecting call by the candidate.";

ChallengesBenchmark`Private`$SandboxDenylist = {
  (* filesystem mutation *)
  DeleteFile, CopyFile, RenameFile,
  CreateDirectory, DeleteDirectory, RenameDirectory, CopyDirectory,
  SetFileDate, SetDirectory, ResetDirectory,
  (* writing primitives *)
  Put, PutAppend, WriteString, Write,
  OpenWrite, OpenAppend,
  (* process spawn *)
  Run, RunProcess, StartProcess, KillProcess,
  SystemOpen, Install, LinkLaunch,
  (* network egress *)
  URLExecute, URLFetch, URLRead, URLSubmit, URLDownload,
  URLSave, HTTPRequest, SocketConnect, SocketListen,
  SendMail,
  (* cloud *)
  CloudDeploy, CloudPublish, CloudSubmit, CloudPut,
  (* evaluation escape hatches *)
  ToExpression (* string -> code; denied to block eval-from-string attacks *)
};

(* applySandbox[sandbox, body] evaluates body inside a Block that rebinds
   every denylisted symbol to sandboxTrap. When sandbox is False we just
   evaluate body directly, preserving the legacy unguarded behavior for
   harness self-tests.

   Attribute choice: HoldRest (not HoldAllComplete). The first argument
   must evaluate so a Module-local variable like sandbox$1234 can reduce
   to True/False and hit the right downvalue. The body must stay held
   so Block sets up its scope before the candidate code runs.

   Block expects bindings of the form {sym = val, ...} (Set), NOT
   {sym -> val, ...} (Rule). We therefore enumerate the bindings
   inline. The list must stay in lockstep with $SandboxDenylist — that
   constant exists so AuditSolutions and tests can introspect coverage
   without re-parsing this Block. *)

SetAttributes[ChallengesBenchmark`Private`applySandbox, HoldRest];

ChallengesBenchmark`Private`applySandbox[False, body_] := body;

ChallengesBenchmark`Private`applySandbox[True, body_] :=
  Block[{
    (* filesystem mutation *)
    DeleteFile      = ChallengesBenchmark`Private`sandboxTrap,
    CopyFile        = ChallengesBenchmark`Private`sandboxTrap,
    RenameFile      = ChallengesBenchmark`Private`sandboxTrap,
    CreateDirectory = ChallengesBenchmark`Private`sandboxTrap,
    DeleteDirectory = ChallengesBenchmark`Private`sandboxTrap,
    RenameDirectory = ChallengesBenchmark`Private`sandboxTrap,
    CopyDirectory   = ChallengesBenchmark`Private`sandboxTrap,
    SetFileDate     = ChallengesBenchmark`Private`sandboxTrap,
    SetDirectory    = ChallengesBenchmark`Private`sandboxTrap,
    ResetDirectory  = ChallengesBenchmark`Private`sandboxTrap,
    (* writing primitives *)
    Put             = ChallengesBenchmark`Private`sandboxTrap,
    PutAppend       = ChallengesBenchmark`Private`sandboxTrap,
    WriteString     = ChallengesBenchmark`Private`sandboxTrap,
    Write           = ChallengesBenchmark`Private`sandboxTrap,
    OpenWrite       = ChallengesBenchmark`Private`sandboxTrap,
    OpenAppend      = ChallengesBenchmark`Private`sandboxTrap,
    (* process spawn *)
    Run             = ChallengesBenchmark`Private`sandboxTrap,
    RunProcess      = ChallengesBenchmark`Private`sandboxTrap,
    StartProcess    = ChallengesBenchmark`Private`sandboxTrap,
    KillProcess     = ChallengesBenchmark`Private`sandboxTrap,
    SystemOpen      = ChallengesBenchmark`Private`sandboxTrap,
    Install         = ChallengesBenchmark`Private`sandboxTrap,
    LinkLaunch      = ChallengesBenchmark`Private`sandboxTrap,
    (* network egress *)
    URLExecute      = ChallengesBenchmark`Private`sandboxTrap,
    URLFetch        = ChallengesBenchmark`Private`sandboxTrap,
    URLRead         = ChallengesBenchmark`Private`sandboxTrap,
    URLSubmit       = ChallengesBenchmark`Private`sandboxTrap,
    URLDownload     = ChallengesBenchmark`Private`sandboxTrap,
    URLSave         = ChallengesBenchmark`Private`sandboxTrap,
    HTTPRequest     = ChallengesBenchmark`Private`sandboxTrap,
    SocketConnect   = ChallengesBenchmark`Private`sandboxTrap,
    SocketListen    = ChallengesBenchmark`Private`sandboxTrap,
    SendMail        = ChallengesBenchmark`Private`sandboxTrap,
    (* cloud *)
    CloudDeploy     = ChallengesBenchmark`Private`sandboxTrap,
    CloudPublish    = ChallengesBenchmark`Private`sandboxTrap,
    CloudSubmit     = ChallengesBenchmark`Private`sandboxTrap,
    CloudPut        = ChallengesBenchmark`Private`sandboxTrap,
    (* evaluation escape hatches *)
    ToExpression    = ChallengesBenchmark`Private`sandboxTrap
  },
    body
  ];

(* ----------------------------------------------------------------------- *)
(* The sandboxed evaluator                                                 *)
(* ----------------------------------------------------------------------- *)

(* evaluateOneTest is a public-private symbol so that ParallelMap and
   LocalSubmit bodies can call it across kernel boundaries. It returns
   an Association (never throws) describing the outcome.

   Sentinels:
     $TimeSentinel      — returned by TimeConstrained when time ran out
     $MemSentinel       — returned by MemoryConstrained when memory ran out
     $EvaluationError   — returned by CheckAbort/Catch when the candidate
                          aborted or threw uncaught

   Status values:
     "Evaluated"        — the candidate returned a value (pass/fail
                          decided later by resolveSameTest)
     "TimedOut"         — time-constraint breached
     "MemoryExceeded"   — memory-constraint breached
     "EvaluationError"  — Abort, uncaught Throw, or a Failure[]
     "ParseError"       — the candidate code would not parse as WL
     "NoSolution"       — no candidate was supplied for this challenge

   Context isolation:
     In PerTestKernel mode, each subkernel already has a clean Global`
     so we do not need a private context. In the PooledKernels and
     InProcess modes, the caller is expected to reset Global` between
     tests. Using Block[{$Context=...}] here would not help: ImportString
     with "HeldExpressions" resolves symbols in the current $Context at
     PARSE time, so a private context would only isolate the definition
     from the driver's Global`, not from any symbols referenced by the
     pre-parsed test-bank input. Cleanup is therefore a kernel-pool
     concern, not a per-test concern. *)

ChallengesBenchmark`Private`evaluateOneTest[workItem_Association, timeC_, memC_] :=
  ChallengesBenchmark`Private`evaluateOneTest[workItem, timeC, memC, True];

ChallengesBenchmark`Private`evaluateOneTest[workItem_Association, timeC_, memC_, sandbox_] :=
  Module[
    {solutionCode, heldDef, heldInput, actual, msgCount, t0, dt, status, errorTag},

    solutionCode = workItem["solutionCode"];
    If[MissingQ[solutionCode],
      Return[<|
        "status"        -> "NoSolution",
        "error"         -> "no solution was supplied for this challenge",
        "actualOutput"  -> Missing["NoSolution"],
        "messageCount"  -> 0,
        "durationSec"   -> 0.,
        "memoryBytes"   -> 0
      |>]
    ];

    heldDef = ChallengesBenchmark`Private`parseHeldWL[solutionCode];
    If[heldDef === $Failed,
      Return[<|
        "status"        -> "ParseError",
        "error"         -> "could not parse candidate code as Wolfram Language",
        "actualOutput"  -> Missing["ParseError"],
        "messageCount"  -> 0,
        "durationSec"   -> 0.,
        "memoryBytes"   -> 0
      |>]
    ];

    heldInput = workItem["input"];   (* HoldComplete[...] *)
    t0 = AbsoluteTime[];
    actual = ChallengesBenchmark`Private`$EvaluationError;
    msgCount = 0;

    (* Run the candidate. Message list is captured but not suppressed so
       the output can cite how noisy the candidate was. TimeConstrained
       and MemoryConstrained wrap the release+call; CheckAbort and
       Catch translate escapes (Abort/Throw) into a sentinel.

       Throw containment is two-layered because Catch[expr, form, f] does
       NOT catch untagged Throw[value] — form only matches tagged throws.
       LLM-generated solutions frequently contain bugs of the shape
       "Throw[x] outside any matching Catch" (observed: gemini-2.5-flash's
       PairingCompatibleIntegers uses Throw[result] inside a
       Catch[..., "PairingFound"] that only catches the "PairingFound"
       tag). An uncaught Throw bubbles to the Runner and, in InProcess
       mode, kills the driver kernel with Throw::nocatch.

       Strategy:
         * Inner Catch[expr, _, h] catches tagged throws -> sentinel.
         * Outer Catch[...] catches any remaining untagged throw.
         * We detect untagged escape with a local flag that only flips
           to False after the inner Catch completes normally; any
           untagged Throw inside skips the assignment.

       The whole release-and-call chain runs inside applySandbox, which
       (when sandbox -> True) Block-rebinds the denylisted side-effecting
       primitives so any DeleteFile / Run / URLFetch call from the
       candidate returns $Failed with a sandbox message instead of
       touching the host. *)
    Block[{$MessageList = {}},
      actual = ChallengesBenchmark`Private`applySandbox[sandbox,
        CheckAbort[
          Module[{untaggedThrowEscaped = True, innerResult},
            Catch[
              innerResult = Catch[
                TimeConstrained[
                  MemoryConstrained[
                    ReplaceAll[
                      heldInput,
                      HoldComplete[expr_] :> (
                        ReleaseHold[heldDef];
                        expr
                      )
                    ],
                    memC,
                    ChallengesBenchmark`Private`$MemSentinel
                  ],
                  timeC,
                  ChallengesBenchmark`Private`$TimeSentinel
                ],
                _,
                ChallengesBenchmark`Private`$EvaluationError &
              ];
              untaggedThrowEscaped = False;
            ];
            If[untaggedThrowEscaped,
              ChallengesBenchmark`Private`$EvaluationError,
              innerResult]
          ],
          ChallengesBenchmark`Private`$EvaluationError
        ]
      ];
      msgCount = Length[$MessageList];
    ];

    dt = AbsoluteTime[] - t0;

    Which[
      actual === ChallengesBenchmark`Private`$TimeSentinel,
        status = "TimedOut"; errorTag = "time-constraint exceeded",
      actual === ChallengesBenchmark`Private`$MemSentinel,
        status = "MemoryExceeded"; errorTag = "memory-constraint exceeded",
      actual === ChallengesBenchmark`Private`$EvaluationError,
        status = "EvaluationError"; errorTag = "evaluation aborted or threw",
      MatchQ[actual, _Failure],
        status = "EvaluationError"; errorTag = ToString[actual, InputForm],
      True,
        status = "Evaluated"; errorTag = None
    ];

    <|
      "status"       -> status,
      "actualOutput" -> actual,
      "messageCount" -> msgCount,
      "durationSec"  -> dt,
      "memoryBytes"  -> 0,          (* MemoryConstrained does not report usage *)
      "error"        -> errorTag
    |>
  ];

(* Sentinel singletons. These are symbols (not strings) so they cannot
   possibly match a legitimate candidate return value. *)
ChallengesBenchmark`Private`$TimeSentinel     = Unique["$TimeSentinel$"];
ChallengesBenchmark`Private`$MemSentinel      = Unique["$MemSentinel$"];
ChallengesBenchmark`Private`$EvaluationError  = Unique["$EvalError$"];

(* ----------------------------------------------------------------------- *)
(* Result normalization                                                    *)
(* ----------------------------------------------------------------------- *)

normalizeResult[workItem_Association, raw_, fallbackDt_] := Module[
  {sub, status, passed, sameTestFn, expected, actual, dt, mem, msgCount, err},

  sub = Which[
    AssociationQ[raw], raw,
    raw === $Failed,
      <|"status" -> "KernelDied",
        "error"  -> "subkernel returned $Failed (process died or crashed)",
        "actualOutput" -> Missing["KernelDied"],
        "messageCount" -> 0, "durationSec" -> fallbackDt, "memoryBytes" -> 0|>,
    MatchQ[raw, _Failure],
      <|"status" -> "KernelDied",
        "error"  -> ToString[raw, InputForm],
        "actualOutput" -> Missing["KernelDied"],
        "messageCount" -> 0, "durationSec" -> fallbackDt, "memoryBytes" -> 0|>,
    True,
      <|"status" -> "RunnerError",
        "error"  -> "non-association result from sandbox: " <>
                    ToString[Short[raw, 5], InputForm],
        "actualOutput" -> raw,
        "messageCount" -> 0, "durationSec" -> fallbackDt, "memoryBytes" -> 0|>
  ];

  status   = Lookup[sub, "status", "RunnerError"];
  expected = workItem["expected"];
  actual   = Lookup[sub, "actualOutput", Missing["Unknown"]];
  dt       = Lookup[sub, "durationSec", fallbackDt];
  mem      = Lookup[sub, "memoryBytes", 0];
  msgCount = Lookup[sub, "messageCount", 0];
  err      = Lookup[sub, "error", None];

  sameTestFn = resolveSameTest[workItem, workItem["sameTest"]];

  passed = If[status === "Evaluated",
    TrueQ @ Quiet @ Check[sameTestFn[actual, expected], False],
    False
  ];

  <|
    "challengeName" -> workItem["challengeName"],
    "testIndex"     -> workItem["testIndex"],
    "testId"        -> workItem["testId"],
    "model"         -> workItem["model"],
    "status"        -> status,
    "passed"        -> passed,
    "expected"      -> expected,
    "actualOutput"  -> actual,
    "messageCount"  -> msgCount,
    "error"         -> err,
    "durationSec"   -> dt,
    "memoryBytes"   -> mem
  |>
];

(* resolveSameTest — pick a comparison function:
     1. Per-test metadata "sameTest" key (a symbol or pure function)
     2. The workItem-wide override (from RunBenchmark options)
     3. Default: SameQ
   Anything that is not a Symbol or Function is ignored. *)

(* A valid comparator is a callable Symbol or Function. Crucially it must
   not be one of the sentinel symbols (Automatic, None, Null) that callers
   pass to mean "pick a default for me" — those have Head Symbol but are
   not comparators, and letting them through collapses every test's
   SameQ[actual,expected] to an unevaluated head that TrueQ maps to False. *)

validComparatorQ[Automatic | None | Null | Missing[___]] := False;
validComparatorQ[f_] := MatchQ[Head[f], Symbol | Function];

resolveSameTest[workItem_Association, override_] := Module[{md, fromMd},
  md = Lookup[workItem, "metadata", <||>];
  fromMd = If[AssociationQ[md], Lookup[md, "sameTest", None], None];
  Which[
    validComparatorQ[fromMd],    fromMd,
    validComparatorQ[override],  override,
    True,                        SameQ
  ]
];

End[];
