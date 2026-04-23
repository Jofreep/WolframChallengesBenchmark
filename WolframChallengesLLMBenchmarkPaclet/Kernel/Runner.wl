(* ::Package:: *)

(* :Context: JofreEspigulePons`WolframChallengesBenchmark`Private`          *)
(* :Summary:
     Sandboxed test runner.  Three isolation modes:

       "PerTestKernel"  Default.  Each test in its own subkernel via
                        LocalSubmit.  Crashes, Quit[], and global
                        mutation in the candidate cannot affect the
                        driver or other tests.  TaskUUID de-duplication
                        plus a bounded retry tolerates kernel death.

       "PooledKernels"  Fixed pool launched with LaunchKernels[n] and
                        ParallelSubmit.  Each kernel reset between
                        tests with Remove["Global`*"].  Weaker
                        isolation, faster on large test sets.

       "InProcess"      Runs in the driver kernel.  Only used for the
                        harness self-tests; not safe for untrusted
                        candidate code.

     Every test evaluation is wrapped in applySandbox, which Block-
     rebinds filesystem / process / network primitives to a sentinel
     trap.  Candidate code that tries DeleteFile, Run, URLFetch, etc.
     gets a ::sandbox message and a $Failed value \[LongDash] the host is
     unaffected.
*)

Begin["JofreEspigulePons`WolframChallengesBenchmark`Private`"];


(* ------------------------------------------------------------------ *)
(* Messages + defaults (public-symbol-tagged)                         *)
(* ------------------------------------------------------------------ *)

JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark::nomodel =
  "RunBenchmark requires the Model option to be set.";
JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark::badmode =
  "Unknown IsolationMode: `1`.";
JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark::badparallel =
  "Parallel must be a positive integer; got `1`.";
JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark::sandbox =
  "Sandbox policy blocked a side-effecting call by the candidate.";


$BenchmarkDefaults = <|
  "TimeConstraint"     -> 60,
  "MemoryConstraint"   -> 2*^9,
  "Parallel"           -> Automatic,
  "RunId"              -> Automatic,
  "OutputDirectory"    -> Automatic,
  "Filter"             -> All,
  "ProgressHandler"    -> None,
  "SameTestFunction"   -> Automatic,
  "Model"              -> None,
  "Seed"               -> Automatic,
  "IsolationMode"      -> "PerTestKernel",
  "RetryOnKernelDeath" -> 1,
  "PollInterval"       -> 0.05,
  "Sandbox"            -> True
|>;

$DefaultPollInterval = 0.05;


(* ------------------------------------------------------------------ *)
(* Sentinel singletons                                                *)
(*                                                                     *)
(* These are Unique[...]-generated symbols (not strings) so they       *)
(* cannot collide with a legitimate candidate return value.            *)
(* ------------------------------------------------------------------ *)

$TimeSentinel    = Unique["$TimeSentinel$"];
$MemSentinel     = Unique["$MemSentinel$"];
$EvaluationError = Unique["$EvalError$"];


(* ------------------------------------------------------------------ *)
(* Capability sandbox                                                  *)
(*                                                                     *)
(* Block-rebinds a denylist of side-effecting primitives to a trap.    *)
(* Block does not call Set, so Protected System symbols can be         *)
(* rebound without Unprotect.  Outside the Block, the originals are   *)
(* restored.                                                            *)
(* ------------------------------------------------------------------ *)

SetAttributes[sandboxTrap, HoldAllComplete];
sandboxTrap[___] := (
  Message[JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark::sandbox];
  $Failed
);

$SandboxDenylist = {
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
  ToExpression
};

SetAttributes[applySandbox, HoldRest];

applySandbox[False, body_] := body;

applySandbox[True, body_] :=
  Block[{
    (* filesystem mutation *)
    DeleteFile = sandboxTrap, CopyFile = sandboxTrap, RenameFile = sandboxTrap,
    CreateDirectory = sandboxTrap, DeleteDirectory = sandboxTrap,
    RenameDirectory = sandboxTrap, CopyDirectory = sandboxTrap,
    SetFileDate = sandboxTrap, SetDirectory = sandboxTrap,
    ResetDirectory = sandboxTrap,
    (* writing primitives *)
    Put = sandboxTrap, PutAppend = sandboxTrap,
    WriteString = sandboxTrap, Write = sandboxTrap,
    OpenWrite = sandboxTrap, OpenAppend = sandboxTrap,
    (* process spawn *)
    Run = sandboxTrap, RunProcess = sandboxTrap, StartProcess = sandboxTrap,
    KillProcess = sandboxTrap, SystemOpen = sandboxTrap,
    Install = sandboxTrap, LinkLaunch = sandboxTrap,
    (* network egress *)
    URLExecute = sandboxTrap, URLFetch = sandboxTrap, URLRead = sandboxTrap,
    URLSubmit = sandboxTrap, URLDownload = sandboxTrap, URLSave = sandboxTrap,
    HTTPRequest = sandboxTrap, SocketConnect = sandboxTrap,
    SocketListen = sandboxTrap, SendMail = sandboxTrap,
    (* cloud *)
    CloudDeploy = sandboxTrap, CloudPublish = sandboxTrap,
    CloudSubmit = sandboxTrap, CloudPut = sandboxTrap,
    (* evaluation escape hatches *)
    ToExpression = sandboxTrap
  },
    body
  ];


(* ------------------------------------------------------------------ *)
(* evaluateOneTest                                                     *)
(*                                                                     *)
(* Core per-test evaluator.  Public-private so ParallelMap and         *)
(* LocalSubmit bodies can call it across kernel boundaries.            *)
(* ------------------------------------------------------------------ *)

evaluateOneTest[workItem_Association, timeC_, memC_] :=
  evaluateOneTest[workItem, timeC, memC, True];

evaluateOneTest[workItem_Association, timeC_, memC_, sandbox_] :=
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

    heldDef = parseHeldWL[solutionCode];
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

    heldInput = workItem["input"];
    t0 = AbsoluteTime[];
    actual = $EvaluationError;
    msgCount = 0;

    (* Throw containment is two-layered because Catch[expr, form, f] does
       NOT catch untagged Throw[value] — form only matches tagged throws.
       LLM-generated solutions frequently contain bugs of the shape
       "Throw[x] outside any matching Catch" (observed: gemini-2.5-flash's
       PairingCompatibleIntegers uses Throw[result] inside a
       Catch[..., "PairingFound"] that only catches the "PairingFound"
       tag). An uncaught Throw bubbles to the Runner and, in InProcess
       mode, kills the driver kernel with Throw::nocatch.

       Inner Catch[expr, _, h] catches tagged throws -> sentinel. Outer
       Catch[...] catches any remaining untagged throw. We detect
       untagged escape with a local flag that only flips to False after
       the inner Catch completes normally. *)
    Block[{$MessageList = {}},
      actual = applySandbox[sandbox,
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
                    $MemSentinel
                  ],
                  timeC,
                  $TimeSentinel
                ],
                _,
                $EvaluationError &
              ];
              untaggedThrowEscaped = False;
            ];
            If[untaggedThrowEscaped, $EvaluationError, innerResult]
          ],
          $EvaluationError
        ]
      ];
      msgCount = Length[$MessageList];
    ];

    dt = AbsoluteTime[] - t0;

    Which[
      actual === $TimeSentinel,
        status = "TimedOut"; errorTag = "time-constraint exceeded",
      actual === $MemSentinel,
        status = "MemoryExceeded"; errorTag = "memory-constraint exceeded",
      actual === $EvaluationError,
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
      "memoryBytes"  -> 0,
      "error"        -> errorTag
    |>
  ];


(* ------------------------------------------------------------------ *)
(* Work-item builder                                                   *)
(* ------------------------------------------------------------------ *)

buildWorkItems[challenges_, testBank_, solutions_, filter_, model_, sameTest_] :=
  Module[{names},
    names = Keys[testBank];
    If[filter =!= All, names = Intersection[names, filter]];
    Flatten @ Map[
      Function[name,
        Module[{tests, sol},
          sol   = Lookup[solutions, name, Missing["NoSolution"]];
          tests = testBank[name];
          MapIndexed[
            <|
              "challengeName" -> name,
              "testIndex"     -> First[#2],
              "testId"        -> name <> "/" <> IntegerString[First[#2]],
              "model"         -> model,
              "input"         -> #1["input"],
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


(* ------------------------------------------------------------------ *)
(* Comparator resolution                                               *)
(*                                                                     *)
(* A valid comparator is a callable Symbol or Function, but NOT one    *)
(* of the sentinel defaults (Automatic, None, Null, Missing[]).        *)
(* ------------------------------------------------------------------ *)

validComparatorQ[Automatic | None | Null | Missing[___]] := False;
validComparatorQ[f_] := MatchQ[Head[f], Symbol | Function];

resolveSameTest[workItem_Association, override_] := Module[{md, fromMd},
  md     = Lookup[workItem, "metadata", <||>];
  fromMd = If[AssociationQ[md], Lookup[md, "sameTest", None], None];
  Which[
    validComparatorQ[fromMd],    fromMd,
    validComparatorQ[override],  override,
    True,                        SameQ
  ]
];


(* ------------------------------------------------------------------ *)
(* Result normalization                                                *)
(* ------------------------------------------------------------------ *)

normalizeResult[workItem_Association, raw_, fallbackDt_] := Module[
  {sub, status, passed, sameTestFn, expected, actual, dt, mem, msgCount, err},

  sub = Which[
    AssociationQ[raw], raw,
    raw === $Failed,
      <|"status"       -> "KernelDied",
        "error"        -> "subkernel returned $Failed (process died or crashed)",
        "actualOutput" -> Missing["KernelDied"],
        "messageCount" -> 0, "durationSec" -> fallbackDt, "memoryBytes" -> 0|>,
    MatchQ[raw, _Failure],
      <|"status"       -> "KernelDied",
        "error"        -> ToString[raw, InputForm],
        "actualOutput" -> Missing["KernelDied"],
        "messageCount" -> 0, "durationSec" -> fallbackDt, "memoryBytes" -> 0|>,
    True,
      <|"status"       -> "RunnerError",
        "error"        -> "non-association result from sandbox: " <>
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


(* ------------------------------------------------------------------ *)
(* Isolation mode implementations                                      *)
(* ------------------------------------------------------------------ *)

runInProcess[workItems_List, timeC_, memC_, jsonlPath_String,
    progressHandler_, sandbox_:True] :=
  Module[{totalCount, doneCount = 0},
    totalCount = Length[workItems];
    Map[
      Function[wi,
        Module[{r, normResult},
          (* IMPORTANT: do NOT wrap evaluateOneTest in Check[].  Candidate
             code routinely fires messages during evaluation (First::nofirst
             on an empty-list access, Divide::indet on 0/0, General::stop
             after iteration quotas, etc.) and those messages must grade
             as Evaluated/passed=False rather than poisoning the whole
             row as a harness-level RunnerError.  Check triggers on ANY
             message regardless of Quiet (Quiet only suppresses display),
             so any Check wrapper at this layer mis-classifies legitimate
             candidate failures.  evaluateOneTest already captures the
             per-test message count via its own Block[{$MessageList = {}}]
             so no information is lost.  The Quiet here only hides the
             sandbox tripwire message from the console; the test outcome
             is determined entirely by what evaluateOneTest returns.      *)
          r = Quiet[
            evaluateOneTest[wi, timeC, memC, sandbox],
            {JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark::sandbox}
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
            "timestamp"  -> isoUTC[]
          |>];
          If[progressHandler =!= None,
            Quiet @ Check[
              progressHandler[<|
                "doneCount"  -> doneCount,
                "totalCount" -> totalCount,
                "result"     -> normResult|>],
              Null
            ]
          ];
          normResult
        ]
      ],
      workItems
    ]
  ];


runWithLocalSubmit[workItems_List, parallel_Integer, timeC_, memC_,
    retryCount_Integer, pollInterval_, jsonlPath_String, progressHandler_,
    sandbox_:True] :=
  Module[
    {pending = workItems, inFlight = <||>, completed = {},
     bag, seenUUIDs = <||>, totalCount, doneCount = 0,
     submit, drain, sleepFor},

    totalCount = Length[workItems];
    bag = Internal`Bag[];
    sleepFor = If[NumericQ[pollInterval] && pollInterval > 0,
                  pollInterval, $DefaultPollInterval];

    submit[wi_Association, attemptNum_Integer] := Module[{task, uuid},
      task = Quiet @ Check[
        LocalSubmit[
          JofreEspigulePons`WolframChallengesBenchmark`Private`evaluateOneTest[
            wi, timeC, memC, sandbox
          ],
          HandlerFunctions -> <|
            "TaskFinished" -> Function[a, Internal`StuffBag[bag, a]]
          |>,
          HandlerFunctionsKeys -> {"EvaluationResult", "Failure", "TaskUUID"}
        ],
        $Failed
      ];
      If[task === $Failed || ! MatchQ[task, _TaskObject],
        logError["LocalSubmit failed for " <> wi["testId"]];
        AppendTo[completed,
          normalizeResult[wi,
            <|"status"        -> "RunnerError",
              "error"         -> "LocalSubmit refused the job",
              "actualOutput"  -> Missing["RunnerError"],
              "messageCount"  -> 0, "durationSec" -> 0., "memoryBytes" -> 0|>,
            0.]
        ];
        doneCount += 1;
        Return[]
      ];
      uuid = task["TaskUUID"];
      inFlight[uuid] = <|
        "task"        -> task,
        "workItem"    -> wi,
        "attempt"     -> attemptNum,
        "submittedAt" -> AbsoluteTime[]
      |>;
      appendJSONL[jsonlPath, <|
        "event"     -> "test.submit",
        "testId"    -> wi["testId"],
        "attempt"   -> attemptNum,
        "timestamp" -> isoUTC[]
      |>];
    ];

    drain[] := Module[{items, ev, uuid, rec, raw, dt, normResult},
      If[Internal`BagLength[bag] === 0, Return[]];
      items = Internal`BagPart[bag, All];
      bag   = Internal`Bag[];
      Do[
        ev   = items[[i]];
        uuid = Lookup[ev, "TaskUUID", None];
        Which[
          uuid === None, logWarn["Handler event missing TaskUUID; dropping."],
          KeyExistsQ[seenUUIDs, uuid],    Null,
          ! KeyExistsQ[inFlight, uuid],   Null,
          True,
            seenUUIDs[uuid] = True;
            rec = inFlight[uuid];
            KeyDropFrom[inFlight, uuid];
            dt  = AbsoluteTime[] - rec["submittedAt"];

            raw = Which[
              MatchQ[Lookup[ev, "Failure", None], _Failure],
                ev["Failure"],
              True,
                Lookup[ev, "EvaluationResult", $Failed]
            ];

            normResult = normalizeResult[rec["workItem"], raw, dt];

            If[normResult["status"] === "KernelDied" &&
                 rec["attempt"] <= retryCount,
              logWarn["Kernel died on " <> rec["workItem", "testId"] <>
                      " (attempt " <> IntegerString[rec["attempt"]] <>
                      "); retrying."];
              submit[rec["workItem"], rec["attempt"] + 1],
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
                "timestamp"   -> isoUTC[]
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

    While[Length[pending] > 0 || Length[inFlight] > 0,
      While[Length[pending] > 0 && Length[inFlight] < parallel,
        submit[First[pending], 1];
        pending = Rest[pending];
      ];
      If[Length[inFlight] > 0,
        Pause[sleepFor];
        drain[];
      ];
    ];
    drain[];

    SortBy[completed, {#["challengeName"], #["testIndex"]} &]
  ];


runWithKernelPool[workItems_List, parallel_Integer, timeC_, memC_,
    jsonlPath_String, progressHandler_, sandbox_:True] :=
  Module[{kernels, rawResults, results, totalCount},
    totalCount = Length[workItems];
    kernels = LaunchKernels[parallel];

    DistributeDefinitions[
      JofreEspigulePons`WolframChallengesBenchmark`Private`evaluateOneTest,
      JofreEspigulePons`WolframChallengesBenchmark`Private`parseHeldWL,
      JofreEspigulePons`WolframChallengesBenchmark`Private`$TimeSentinel,
      JofreEspigulePons`WolframChallengesBenchmark`Private`$MemSentinel,
      JofreEspigulePons`WolframChallengesBenchmark`Private`$EvaluationError,
      JofreEspigulePons`WolframChallengesBenchmark`Private`sandboxTrap,
      JofreEspigulePons`WolframChallengesBenchmark`Private`applySandbox,
      JofreEspigulePons`WolframChallengesBenchmark`Private`$SandboxDenylist
    ];

    rawResults = ParallelMap[
      Function[wi,
        Module[{r},
          r = JofreEspigulePons`WolframChallengesBenchmark`Private`evaluateOneTest[
            wi, timeC, memC, sandbox
          ];
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
            "timestamp"  -> isoUTC[]
          |>];
          If[progressHandler =!= None,
            Quiet @ Check[
              progressHandler[<|
                "doneCount"  -> First[i],
                "totalCount" -> totalCount,
                "result"     -> normResult|>],
              Null
            ]
          ];
          normResult
        ]
      ],
      rawResults
    ];

    SortBy[results, {#["challengeName"], #["testIndex"]} &]
  ];


(* ------------------------------------------------------------------ *)
(* runBenchmarkImpl \[Dash] top-level driver                            *)
(* ------------------------------------------------------------------ *)

runBenchmarkImpl[challenges_Association, testBank_Association,
    solutions_Association, opts___?OptionQ] := Module[
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
  outDir        = Replace[Lookup[o, "OutputDirectory",
                            Lookup[$BenchmarkDefaults, "OutputDirectory"]],
                          Automatic :> FileNameJoin[{Directory[], "runs"}]];
  filter        = Lookup[o, "Filter", Lookup[$BenchmarkDefaults, "Filter"]];
  parallel      = Replace[Lookup[o, "Parallel",
                            Lookup[$BenchmarkDefaults, "Parallel"]],
                          Automatic :> Max[1, $ProcessorCount - 1]];
  timeC         = Lookup[o, "TimeConstraint",
                    Lookup[$BenchmarkDefaults, "TimeConstraint"]];
  memC          = Lookup[o, "MemoryConstraint",
                    Lookup[$BenchmarkDefaults, "MemoryConstraint"]];
  sameTest      = Lookup[o, "SameTestFunction",
                    Lookup[$BenchmarkDefaults, "SameTestFunction"]];
  model         = Lookup[o, "Model", Lookup[$BenchmarkDefaults, "Model"]];
  seed          = Replace[Lookup[o, "Seed",
                            Lookup[$BenchmarkDefaults, "Seed"]],
                          Automatic :> Hash[runId, "SHA256"]];
  isolationMode = Lookup[o, "IsolationMode",
                    Lookup[$BenchmarkDefaults, "IsolationMode"]];
  retryCount    = Lookup[o, "RetryOnKernelDeath",
                    Lookup[$BenchmarkDefaults, "RetryOnKernelDeath"]];
  pollInterval  = Lookup[o, "PollInterval",
                    Lookup[$BenchmarkDefaults, "PollInterval"]];
  progressHandler = Lookup[o, "ProgressHandler",
                      Lookup[$BenchmarkDefaults, "ProgressHandler"]];
  sandbox       = TrueQ @ Lookup[o, "Sandbox",
                    Lookup[$BenchmarkDefaults, "Sandbox"]];

  If[! IntegerQ[parallel] || parallel < 1,
    Message[
      JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark::badparallel,
      parallel];
    Return[$Failed]
  ];
  If[model === None,
    Message[
      JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark::nomodel];
    Return[$Failed]
  ];

  runDir = FileNameJoin[{outDir, runId}];
  If[! DirectoryQ[runDir],
    CreateDirectory[runDir, CreateIntermediateDirectories -> True]
  ];

  jsonlPath      = FileNameJoin[{runDir, "progress.jsonl"}];
  resultsWxfPath = FileNameJoin[{runDir, "results.wxf"}];
  runJsonPath    = FileNameJoin[{runDir, "run.json"}];

  fingerprint = runtimeFingerprint[];
  fingerprint["randomSeedUsed"] = ToString[seed];

  workItems  = buildWorkItems[challenges, testBank, solutions, filter,
                 model, sameTest];
  totalTests = Length[workItems];

  If[totalTests === 0,
    logWarn["RunBenchmark: no work items matched the filter \[LongDash] nothing to run."]
  ];

  logInfo[StringTemplate[
    "RunBenchmark: model=`` runId=`` tests=`` parallel=`` mode=``"][
    model, runId, totalTests, parallel, isolationMode]];

  runMeta = <|
    "runId"      -> runId,
    "model"      -> model,
    "createdAt"  -> isoUTC[],
    "runtime"    -> fingerprint,
    "totalTests" -> totalTests,
    "options"    -> <|
      "timeConstraint"     -> timeC,
      "memoryConstraint"   -> memC,
      "parallel"           -> parallel,
      "isolationMode"      -> isolationMode,
      "filter"             -> If[filter === All, "All", filter],
      "seed"               -> ToString[seed],
      "pollInterval"       -> pollInterval,
      "retryOnKernelDeath" -> retryCount,
      "sandbox"            -> sandbox
    |>,
    "status"     -> "running"
  |>;
  Export[runJsonPath, runMeta, "RawJSON"];
  appendJSONL[jsonlPath, <|
    "event"      -> "run.start",
    "runId"      -> runId,
    "totalTests" -> totalTests,
    "timestamp"  -> isoUTC[]
  |>];

  ts0 = AbsoluteTime[];

  results = Switch[isolationMode,
    "PerTestKernel",
      runWithLocalSubmit[workItems, parallel, timeC, memC, retryCount,
        pollInterval, jsonlPath, progressHandler, sandbox],
    "PooledKernels",
      runWithKernelPool[workItems, parallel, timeC, memC,
        jsonlPath, progressHandler, sandbox],
    "InProcess",
      runInProcess[workItems, timeC, memC, jsonlPath, progressHandler, sandbox],
    _,
      Message[
        JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark::badmode,
        isolationMode];
      Return[$Failed]
  ];

  If[results === $Failed, Return[$Failed]];

  durationS = AbsoluteTime[] - ts0;

  Quiet @ Check[
    Export[resultsWxfPath, results, "WXF"],
    logError["Failed to write results.wxf to " <> resultsWxfPath]
  ];

  runMeta["status"]      = "completed";
  runMeta["finishedAt"]  = isoUTC[];
  runMeta["durationSec"] = durationS;
  runMeta["summary"]     = summarizeResults[results];
  Export[runJsonPath, runMeta, "RawJSON"];

  appendJSONL[jsonlPath, <|
    "event"       -> "run.end",
    "runId"       -> runId,
    "durationSec" -> durationS,
    "summary"     -> runMeta["summary"]
  |>];

  logInfo[StringTemplate["RunBenchmark complete: `` (passed `` / ``)"][
    runId, runMeta["summary", "passed"], totalTests]];

  <|
    "runId"   -> runId,
    "runDir"  -> runDir,
    "meta"    -> runMeta,
    "results" -> results
  |>
];

runBenchmarkImpl[___] := $Failed;


End[];  (* `Private` *)
