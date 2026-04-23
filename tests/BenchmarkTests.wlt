(* ::Package:: *)

(* Self-tests for the ChallengesBenchmark harness.

   Run with:
     wolframscript -file tests/RunTests.wls

   Or interactively:
     TestReport[FileNameJoin[{NotebookDirectory[], "BenchmarkTests.wlt"}]]

   These tests cover the harness itself, not the LLM-generated solutions.
*)

(* ----------------------------------------------------------------------- *)
(* Bootstrap                                                               *)
(* ----------------------------------------------------------------------- *)

PrependTo[$Path, FileNameJoin[{DirectoryName[$InputFileName, 2]}]];
Get["ChallengesBenchmark`ChallengesBenchmark`"];

(* Use InProcess isolation so the test suite is fast and self-contained. *)

makeMiniSolutions[] := <|
  "PlusOne"     -> <|"code" -> "PlusOne[x_] := x + 1", "metadata" -> <||>|>,
  "Slow"        -> <|"code" -> "Slow[] := (Pause[5]; \"done\")", "metadata" -> <||>|>,
  "Hungry"      -> <|"code" -> "Hungry[n_] := ConstantArray[1, n]", "metadata" -> <||>|>,
  "Throws"      -> <|"code" -> "Throws[] := (1/0; \"unreachable\")", "metadata" -> <||>|>,
  "BadParse"    -> <|"code" -> "this is ((( not WL", "metadata" -> <||>|>,
  "QuitsKernel" -> <|"code" -> "QuitsKernel[] := \"ok\"", "metadata" -> <||>|>
|>;

makeMiniTestBank[] := <|
  "PlusOne" -> {
    <|"challengeName" -> "PlusOne", "testIndex" -> 1,
      "input" -> HoldComplete[PlusOne[1]], "expected" -> 2, "metadata" -> <||>|>,
    <|"challengeName" -> "PlusOne", "testIndex" -> 2,
      "input" -> HoldComplete[PlusOne[41]], "expected" -> 42, "metadata" -> <||>|>
  },
  "Slow" -> {
    <|"challengeName" -> "Slow", "testIndex" -> 1,
      "input" -> HoldComplete[Slow[]], "expected" -> "done", "metadata" -> <||>|>
  },
  "Hungry" -> {
    <|"challengeName" -> "Hungry", "testIndex" -> 1,
      "input" -> HoldComplete[Hungry[10^10]], "expected" -> Null, "metadata" -> <||>|>
  },
  "Throws" -> {
    <|"challengeName" -> "Throws", "testIndex" -> 1,
      "input" -> HoldComplete[Throws[]], "expected" -> "anything", "metadata" -> <||>|>
  },
  "BadParse" -> {
    <|"challengeName" -> "BadParse", "testIndex" -> 1,
      "input" -> HoldComplete[BadParse[]], "expected" -> 0, "metadata" -> <||>|>
  },
  "Missing" -> {
    <|"challengeName" -> "Missing", "testIndex" -> 1,
      "input" -> HoldComplete[Missing[]], "expected" -> 0, "metadata" -> <||>|>
  }
|>;

(* ----------------------------------------------------------------------- *)
(* ExtractCode                                                             *)
(* ----------------------------------------------------------------------- *)

VerificationTest[
  ChallengesBenchmark`ExtractCode["```wl\nf[x_] := x + 1\n```"],
  "f[x_] := x + 1",
  TestID -> "ExtractCode/labeled-fence"
]

VerificationTest[
  ChallengesBenchmark`ExtractCode[
    "Some prose\n```wolfram\nold code\n```\nWait, let me reconsider:\n```wolfram\ngood code\n```"
  ],
  "good code",
  TestID -> "ExtractCode/picks-last-block"
]

VerificationTest[
  ChallengesBenchmark`ExtractCode["```\nbare fence\n```"],
  "bare fence",
  TestID -> "ExtractCode/unlabeled-fence"
]

VerificationTest[
  ChallengesBenchmark`ExtractCode["just raw code with no fences"],
  "just raw code with no fences",
  TestID -> "ExtractCode/no-fence-passthrough"
]

(* ----------------------------------------------------------------------- *)
(* Loader / schema validation                                              *)
(* ----------------------------------------------------------------------- *)

VerificationTest[
  ChallengesBenchmark`LoadChallenges["/no/such/file.json"],
  $Failed,
  {LoadChallenges::notfound},
  TestID -> "LoadChallenges/missing-file"
]

VerificationTest[
  ChallengesBenchmark`LoadTestBank["/no/such/file.wxf"],
  $Failed,
  {LoadTestBank::notfound},
  TestID -> "LoadTestBank/missing-file"
]

(* ----------------------------------------------------------------------- *)
(* Runner — full smoke test                                                *)
(* ----------------------------------------------------------------------- *)

Module[{run, results, byId, tmpRunsDir = CreateDirectory[]},

  run = ChallengesBenchmark`RunBenchmark[
    <||>,                          (* challenges not used by runner directly *)
    makeMiniTestBank[],
    makeMiniSolutions[],
    "Model"            -> "harness-self-test",
    "OutputDirectory"  -> tmpRunsDir,
    "TimeConstraint"   -> 1,       (* short, so Slow times out *)
    "MemoryConstraint" -> 100*^6,  (* 100 MB, so Hungry OOMs *)
    "IsolationMode"    -> "InProcess",
    "Parallel"         -> 1
  ];
  results = run["results"];
  byId = Association[(#["testId"] -> #) & /@ results];

  VerificationTest[
    byId["PlusOne/1"]["passed"], True,
    TestID -> "Runner/passing-test"
  ];

  VerificationTest[
    byId["PlusOne/2"]["passed"], True,
    TestID -> "Runner/passing-test-2"
  ];

  VerificationTest[
    byId["Slow/1"]["status"], "TimedOut",
    TestID -> "Runner/timeout-detected"
  ];

  VerificationTest[
    MemberQ[{"MemoryExceeded", "EvaluationError"},
            byId["Hungry/1"]["status"]],
    True,
    TestID -> "Runner/memory-or-error-detected"
  ];

  VerificationTest[
    byId["Throws/1"]["passed"], False,
    TestID -> "Runner/throwing-test-not-passing"
  ];

  VerificationTest[
    byId["BadParse/1"]["status"], "ParseError",
    TestID -> "Runner/parse-error-classified"
  ];

  VerificationTest[
    byId["Missing/1"]["status"], "NoSolution",
    TestID -> "Runner/missing-solution-classified"
  ];

  VerificationTest[
    AllTrue[results, KeyExistsQ[#, "durationSec"] &],
    True,
    TestID -> "Runner/result-shape-has-duration"
  ];

  VerificationTest[
    FileExistsQ[FileNameJoin[{run["runDir"], "progress.jsonl"}]],
    True,
    TestID -> "Runner/jsonl-progress-written"
  ];

  VerificationTest[
    FileExistsQ[FileNameJoin[{run["runDir"], "results.wxf"}]],
    True,
    TestID -> "Runner/results-wxf-written"
  ];

  VerificationTest[
    FileExistsQ[FileNameJoin[{run["runDir"], "run.json"}]],
    True,
    TestID -> "Runner/run-json-written"
  ];

  (* ------------------------------------------------------------------- *)
  (* Report                                                              *)
  (* ------------------------------------------------------------------- *)

  Module[{paths},
    paths = ChallengesBenchmark`WriteReport[run, run["runDir"]];
    VerificationTest[
      And @@ FileExistsQ /@ Values[paths],
      True,
      TestID -> "Report/all-three-formats-written"
    ];
  ];

  (* ------------------------------------------------------------------- *)
  (* Diff                                                                *)
  (* ------------------------------------------------------------------- *)

  Module[{run2, diff},
    run2 = ChallengesBenchmark`RunBenchmark[
      <||>,
      makeMiniTestBank[],
      <|
        "PlusOne" -> <|"code" -> "PlusOne[x_] := x + 999", "metadata" -> <||>|> (* now wrong *)
      |>,
      "Model"            -> "harness-self-test-v2",
      "OutputDirectory"  -> tmpRunsDir,
      "TimeConstraint"   -> 1,
      "IsolationMode"    -> "InProcess",
      "Parallel"         -> 1
    ];
    diff = ChallengesBenchmark`DiffRuns[run, run2];
    VerificationTest[
      MemberQ[diff["regressions"], "PlusOne/1"],
      True,
      TestID -> "Diff/regression-detected"
    ];
  ];
];

(* ----------------------------------------------------------------------- *)
(* Regression: SameTestFunction sentinels must fall through to SameQ.      *)
(*                                                                         *)
(* Prior to the validComparatorQ fix, passing Automatic / None / Null —    *)
(* which all have Head Symbol — satisfied the "looks callable" check, and  *)
(* the runner then applied e.g. Automatic[actual, expected] which stayed   *)
(* unevaluated, TrueQ'd to False, and flipped every test to passed:False  *)
(* regardless of actual correctness. The following tests pin the intended  *)
(* behavior so that regression cannot silently return.                     *)
(* ----------------------------------------------------------------------- *)

Module[{tmpRunsDir2 = CreateDirectory[], runWithSentinel},

  runWithSentinel[sentinel_] := ChallengesBenchmark`RunBenchmark[
    <||>,
    <|"PlusOne" -> {
      <|"challengeName" -> "PlusOne", "testIndex" -> 1,
        "input" -> HoldComplete[PlusOne[1]], "expected" -> 2,
        "metadata" -> <||>|>
    }|>,
    <|"PlusOne" -> <|"code" -> "PlusOne[x_] := x + 1", "metadata" -> <||>|>|>,
    "Model"            -> "harness-same-test-regression",
    "OutputDirectory"  -> tmpRunsDir2,
    "TimeConstraint"   -> 2,
    "IsolationMode"    -> "InProcess",
    "Parallel"         -> 1,
    "SameTestFunction" -> sentinel
  ];

  VerificationTest[
    First[runWithSentinel[Automatic]["results"]]["passed"],
    True,
    TestID -> "Runner/SameTestFunction-Automatic-falls-through-to-SameQ"
  ];

  VerificationTest[
    First[runWithSentinel[None]["results"]]["passed"],
    True,
    TestID -> "Runner/SameTestFunction-None-falls-through-to-SameQ"
  ];

  VerificationTest[
    First[runWithSentinel[Null]["results"]]["passed"],
    True,
    TestID -> "Runner/SameTestFunction-Null-falls-through-to-SameQ"
  ];

  (* An explicitly-supplied comparator must still be honored. *)
  VerificationTest[
    First[runWithSentinel[Equal]["results"]]["passed"],
    True,
    TestID -> "Runner/SameTestFunction-Equal-honored"
  ];

  (* And a custom Function must still be honored. *)
  VerificationTest[
    First[runWithSentinel[Function[{a, b}, a - b == 0]]["results"]]["passed"],
    True,
    TestID -> "Runner/SameTestFunction-pure-function-honored"
  ];
];

(* ----------------------------------------------------------------------- *)
(* Capability sandbox: denylisted symbols return $Failed with a sandbox    *)
(* message instead of touching the host. These tests pin the behavior so   *)
(* a regression that silently opens a hole (e.g. forgetting to call        *)
(* applySandbox in evaluateOneTest) gets caught.                           *)
(*                                                                         *)
(* InProcess isolation is used so the tests run in the driver kernel — if  *)
(* the sandbox actually failed, DeleteFile / Run / URLFetch would touch    *)
(* the host filesystem or network from the test harness itself, so this   *)
(* doubles as a smoke check that the denylist is wired up at all.          *)
(* ----------------------------------------------------------------------- *)

Module[{tmpSandboxDir = CreateDirectory[], runSandboxed, runUnsandboxed,
        sentinelPath, sandboxBank},

  (* A file we do NOT want the candidate to delete. If the sandbox leaks,
     DeleteFile would remove it and FileExistsQ would flip to False. *)
  sentinelPath = FileNameJoin[{tmpSandboxDir, "DO_NOT_DELETE.txt"}];
  Export[sentinelPath, "sentinel", "Text"];

  sandboxBank[name_, input_HoldComplete] := <|
    name -> {<|"challengeName" -> name, "testIndex" -> 1,
               "input" -> input, "expected" -> "ignored",
               "metadata" -> <||>|>}
  |>;

  runSandboxed[code_String, input_HoldComplete] :=
    ChallengesBenchmark`RunBenchmark[
      <||>,
      sandboxBank["Cand", input],
      <|"Cand" -> <|"code" -> code, "metadata" -> <||>|>|>,
      "Model"            -> "sandbox-tests",
      "OutputDirectory"  -> tmpSandboxDir,
      "TimeConstraint"   -> 5,
      "IsolationMode"    -> "InProcess",
      "Parallel"         -> 1,
      "Sandbox"          -> True
    ];

  runUnsandboxed[code_String, input_HoldComplete] :=
    ChallengesBenchmark`RunBenchmark[
      <||>,
      sandboxBank["Cand", input],
      <|"Cand" -> <|"code" -> code, "metadata" -> <||>|>|>,
      "Model"            -> "sandbox-tests-off",
      "OutputDirectory"  -> tmpSandboxDir,
      "TimeConstraint"   -> 5,
      "IsolationMode"    -> "InProcess",
      "Parallel"         -> 1,
      "Sandbox"          -> False
    ];

  (* 1. DeleteFile must be blocked and sentinel must survive. *)
  VerificationTest[
    Module[{r},
      r = First[runSandboxed[
        "Cand[p_] := DeleteFile[p]",
        HoldComplete[Cand[sentinelPath]]
      ]["results"]];
      {r["actualOutput"], FileExistsQ[sentinelPath]}
    ],
    {$Failed, True},
    TestID -> "Sandbox/DeleteFile-blocked"
  ];

  (* 2. Run (process spawn) must be blocked. *)
  VerificationTest[
    First[runSandboxed[
      "Cand[] := Run[\"touch /tmp/CHALLENGESBENCHMARK_SHOULD_NOT_EXIST\"]",
      HoldComplete[Cand[]]
    ]["results"]]["actualOutput"],
    $Failed,
    TestID -> "Sandbox/Run-blocked"
  ];

  (* 3. URLFetch (network egress) must be blocked. *)
  VerificationTest[
    First[runSandboxed[
      "Cand[] := URLFetch[\"http://example.com/\"]",
      HoldComplete[Cand[]]
    ]["results"]]["actualOutput"],
    $Failed,
    TestID -> "Sandbox/URLFetch-blocked"
  ];

  (* 4. ToExpression (eval-from-string escape hatch) must be blocked. *)
  VerificationTest[
    First[runSandboxed[
      "Cand[] := ToExpression[\"1+1\"]",
      HoldComplete[Cand[]]
    ]["results"]]["actualOutput"],
    $Failed,
    TestID -> "Sandbox/ToExpression-blocked"
  ];

  (* 5. Legitimate pure computation must still work inside the sandbox. *)
  VerificationTest[
    First[runSandboxed[
      "Cand[x_] := x^2 + 1",
      HoldComplete[Cand[6]]
    ]["results"]]["actualOutput"],
    37,
    TestID -> "Sandbox/pure-computation-unaffected"
  ];

  (* 6. Import (pure reader) must still work inside the sandbox. *)
  VerificationTest[
    First[runSandboxed[
      "Cand[p_] := Import[p, \"Text\"]",
      HoldComplete[Cand[sentinelPath]]
    ]["results"]]["actualOutput"],
    "sentinel",
    TestID -> "Sandbox/Import-allowed"
  ];

  (* 7. With Sandbox -> False the denylist is NOT installed, so the real
        System`DeleteFile runs and actually deletes the file. This is the
        clean complement to test 1: same code, opposite effect depending
        on the sandbox flag. *)
  VerificationTest[
    Module[{r, disposable},
      disposable = FileNameJoin[{tmpSandboxDir, "disposable.txt"}];
      Export[disposable, "toast", "Text"];
      r = First[runUnsandboxed[
        "Cand[p_] := DeleteFile[p]",
        HoldComplete[Cand[disposable]]
      ]["results"]];
      (* Sandbox off: real DeleteFile runs, returns Null, file is gone. *)
      {r["status"], FileExistsQ[disposable]}
    ],
    {"Evaluated", False},
    TestID -> "Sandbox/off-bypasses-denylist"
  ];

  (* Cleanup *)
  Quiet @ DeleteFile[sentinelPath];
  Quiet @ DeleteDirectory[tmpSandboxDir, DeleteContents -> True];
];

(* ----------------------------------------------------------------------- *)
(* LiveDashboard snapshot math                                             *)
(*                                                                         *)
(* The original dashboard read doneCount / totalCount off the last event   *)
(* of progress.jsonl. That silently broke whenever the last event wasn't a *)
(* test.complete (i.e. run.end for finished runs, test.submit between      *)
(* completes for in-flight runs), displaying 0/N. These tests pin the      *)
(* derived-by-counting behavior.                                           *)
(* ----------------------------------------------------------------------- *)

Module[{startEv, subEv, compEv, endEv, snap},

  startEv = <|"event" -> "run.start", "runId" -> "r",
              "totalTests" -> 3, "timestamp" -> "2026-04-19T00:00:00"|>;
  subEv[id_]  := <|"event" -> "test.submit", "testId" -> id,
                   "attempt" -> 1|>;
  compEv[id_, passed_] := <|"event" -> "test.complete", "testId" -> id,
                            "status" -> "Evaluated", "passed" -> passed,
                            "durationSec" -> 0.01|>;
  endEv       = <|"event" -> "run.end", "runId" -> "r",
                  "durationSec" -> 1.0,
                  "summary" -> <|"total" -> 3, "passed" -> 2, "failed" -> 1,
                                 "challengesAttempted" -> 1,
                                 "challengesFullyPassing" -> 0|>|>;

  (* State: nothing written yet. *)
  snap = ChallengesBenchmark`Private`progressSnapshot[{}];
  VerificationTest[snap["state"], "not-started",
    TestID -> "LiveDashboard/snapshot-not-started"];
  VerificationTest[snap["total"], 0,
    TestID -> "LiveDashboard/snapshot-not-started-total-zero"];

  (* State: header only, no completes. *)
  snap = ChallengesBenchmark`Private`progressSnapshot[{startEv, subEv["a"]}];
  VerificationTest[snap["state"], "running",
    TestID -> "LiveDashboard/snapshot-just-started-running"];
  VerificationTest[{snap["total"], snap["done"]}, {3, 0},
    TestID -> "LiveDashboard/snapshot-just-started-counts"];

  (* State: in progress — 2 of 3 complete, 1 passed. *)
  snap = ChallengesBenchmark`Private`progressSnapshot[{
    startEv, subEv["a"], compEv["a", True],
    subEv["b"], compEv["b", False], subEv["c"]
  }];
  VerificationTest[snap["state"], "running",
    TestID -> "LiveDashboard/snapshot-mid-run-state"];
  VerificationTest[
    {snap["total"], snap["done"], snap["passed"], snap["failed"]},
    {3, 2, 1, 1},
    TestID -> "LiveDashboard/snapshot-mid-run-counts"];
  VerificationTest[snap["lastTestId"], "b",
    TestID -> "LiveDashboard/snapshot-mid-run-last-test"];

  (* State: finished — last event is run.end (no doneCount key!). *)
  snap = ChallengesBenchmark`Private`progressSnapshot[{
    startEv,
    subEv["a"], compEv["a", True],
    subEv["b"], compEv["b", True],
    subEv["c"], compEv["c", False],
    endEv
  }];
  VerificationTest[snap["state"], "finished",
    TestID -> "LiveDashboard/snapshot-finished-state"];
  VerificationTest[
    {snap["total"], snap["done"], snap["passed"], snap["failed"]},
    {3, 3, 2, 1},
    TestID -> "LiveDashboard/snapshot-finished-counts"];
  VerificationTest[snap["rate"], 1.0,
    TestID -> "LiveDashboard/snapshot-finished-rate-is-one"];

  (* The public entry point must return a DynamicModule, whether or not
     the path exists. Missing file must not raise. *)
  VerificationTest[
    Head[ChallengesBenchmark`LiveDashboard["/no/such/run/dir"]],
    DynamicModule,
    TestID -> "LiveDashboard/handles-missing-dir"
  ];
];

(* ----------------------------------------------------------------------- *)
(* AuditSolutions — pre-flight consistency check                            *)
(*                                                                         *)
(* The audit must (a) catch LHS/challenge-name mismatches; (b) never       *)
(* evaluate the .wl files — a file defining a protected System` symbol    *)
(* must not trigger SetDelayed::write; (c) not report Module/With/Block   *)
(* locals as top-level definitions. *)

Module[{tmp, writeSol, auditOf, bank},
  tmp = CreateDirectory[];

  writeSol[name_String, code_String] :=
    Export[FileNameJoin[{tmp, name <> ".wl"}], code, "Text",
      CharacterEncoding -> "UTF-8"];

  bank = <|
    "Foo" -> {<|"input" -> HoldComplete[Foo[1]], "expected" -> 2|>},
    "Bar" -> {<|"input" -> HoldComplete[Bar[1, 2]], "expected" -> 3|>},
    "WithCondition" -> {<|"input" -> HoldComplete[WithCondition[3]],
                          "expected" -> 9|>},
    "SafetyCheck" -> {<|"input" -> HoldComplete[SafetyCheck[]],
                        "expected" -> 0|>},
    "Missing" -> {<|"input" -> HoldComplete[Missing[]], "expected" -> Null|>}
  |>;

  auditOf[] := ChallengesBenchmark`AuditSolutions[tmp, bank];

  (* --- Happy path: file name matches defined function --- *)
  writeSol["Foo", "Foo[x_] := x + 1"];
  VerificationTest[
    auditOf[]["byChallenge", "Foo", "status"],
    "ok",
    TestID -> "AuditSolutions/matching-filename"
  ];

  (* --- Mismatch: Foo.wl defines Bar — the bug we saw in production --- *)
  writeSol["Foo", "Bar[x_, y_] := x + y"];
  VerificationTest[
    auditOf[]["byChallenge", "Foo", "status"],
    "mismatch",
    TestID -> "AuditSolutions/detects-mislabeled-file"
  ];

  (* --- CompoundExpression — harvest multiple top-level defs in one file --- *)
  writeSol["Bar", "Bar[x_, y_] := x + y; helper[z_] := z*2"];
  VerificationTest[
    Sort @ auditOf[]["byChallenge", "Bar", "defined"],
    {"Bar", "helper"},
    TestID -> "AuditSolutions/compound-expression-defs"
  ];

  (* --- Condition LHS: f[x_] /; x > 0 := ... must resolve to "f" --- *)
  writeSol["WithCondition", "WithCondition[x_] /; x > 0 := x^2"];
  VerificationTest[
    auditOf[]["byChallenge", "WithCondition", "status"],
    "ok",
    TestID -> "AuditSolutions/condition-on-lhs"
  ];

  (* --- Module-locals must NOT be reported as defs --- *)
  writeSol["Bar",
    "Bar[x_, y_] := Module[{a = 1, b = helper[x]}, a + b]"];
  VerificationTest[
    auditOf[]["byChallenge", "Bar", "defined"],
    {"Bar"},
    TestID -> "AuditSolutions/ignores-module-locals"
  ];

  (* --- Safety: a file whose LHS is a protected System` symbol must NOT
         raise SetDelayed::write. MaxColorDistance is a real System`
         symbol; this is the exact case we hit in the wild. --- *)
  writeSol["SafetyCheck",
    "MaxColorDistance[img_Image] := Total[ImageData[img]]"];
  VerificationTest[
    ChallengesBenchmark`Private`definedFunctionNames[
      "MaxColorDistance[img_Image] := Total[ImageData[img]]"],
    {"MaxColorDistance"},
    {},
    TestID -> "AuditSolutions/no-eval-of-protected-lhs"
  ];

  (* --- Missing solution file --- *)
  Quiet @ DeleteFile @ FileNameJoin[{tmp, "Missing.wl"}];
  VerificationTest[
    MemberQ[auditOf[]["missing"], "Missing"],
    True,
    TestID -> "AuditSolutions/missing-file-detected"
  ];

  (* --- Unexpected file (not in bank) --- *)
  writeSol["Orphan", "Orphan[] := 0"];
  VerificationTest[
    MemberQ[auditOf[]["unexpected"], "Orphan"],
    True,
    TestID -> "AuditSolutions/unexpected-file-detected"
  ];

  (* --- Unparseable code falls into "unparseable", not "mismatch" --- *)
  writeSol["Foo", "this is not (((("];
  With[{r = auditOf[]},
    VerificationTest[
      MemberQ[r["unparseable"], "Foo"] || MemberQ[r["emptyCode"], "Foo"],
      True,
      TestID -> "AuditSolutions/unparseable-not-mismatch"
    ]
  ];

  (* --- Tearing down the temp dir at end --- *)
  Quiet @ DeleteDirectory[tmp, DeleteContents -> True];
];

(* ----------------------------------------------------------------------- *)
(* SaveSolution — write-time audit                                          *)
(* ----------------------------------------------------------------------- *)

Module[{tmp, bank, rGood, rBad, rNoBank, rEmpty, expectedBadFile},
  tmp = CreateDirectory[];
  bank = <|
    "Foo" -> {<|"input" -> HoldComplete[Foo[1]], "expected" -> 2|>}
  |>;

  (* Happy path: code defines Foo, write succeeds, file exists. *)
  rGood = ChallengesBenchmark`SaveSolution[tmp, "Foo", "Foo[x_] := x + 1", bank];
  VerificationTest[
    StringQ[rGood] && FileExistsQ[rGood],
    True,
    TestID -> "SaveSolution/audit-passes-writes-file"
  ];

  (* Mismatch: code defines Bar, audit refuses, returns $Failed, no file. *)
  expectedBadFile = FileNameJoin[{tmp, "BadCode.wl"}];
  Quiet @ DeleteFile @ expectedBadFile;
  rBad = Quiet @ ChallengesBenchmark`SaveSolution[
    tmp, "Foo", "Bar[x_, y_] := x + y", bank
  ];
  VerificationTest[
    {rBad === $Failed, FileExistsQ[expectedBadFile]},
    {True, False},
    TestID -> "SaveSolution/audit-blocks-mislabeled-write"
  ];

  (* Bypass: 3-arg form skips audit entirely. *)
  rNoBank = ChallengesBenchmark`SaveSolution[tmp, "Foo", "Bar[x_, y_] := x + y"];
  VerificationTest[
    StringQ[rNoBank] && FileExistsQ[rNoBank],
    True,
    TestID -> "SaveSolution/no-bank-bypasses-audit"
  ];

  (* Empty / definition-free code is also blocked when bank is provided. *)
  rEmpty = Quiet @ ChallengesBenchmark`SaveSolution[
    tmp, "Foo", "(* just a comment *)", bank
  ];
  VerificationTest[
    rEmpty,
    $Failed,
    TestID -> "SaveSolution/empty-code-blocked"
  ];

  (* testBank -> None explicitly bypasses. *)
  VerificationTest[
    StringQ @ ChallengesBenchmark`SaveSolution[tmp, "Foo", "Bar[x_]:=x", None],
    True,
    TestID -> "SaveSolution/none-bypasses-audit"
  ];

  Quiet @ DeleteDirectory[tmp, DeleteContents -> True];
];

(* ----------------------------------------------------------------------- *)
(* percentilesOf — tail-latency aggregator                                  *)

VerificationTest[
  ChallengesBenchmark`Private`percentilesOf[{}],
  <|"count" -> 0, "mean" -> 0., "p50" -> 0., "p75" -> 0., "p90" -> 0.,
    "p95" -> 0., "p99" -> 0., "max" -> 0., "total" -> 0.|>,
  TestID -> "percentiles/empty-input"
];

Module[{stats},
  stats = ChallengesBenchmark`Private`percentilesOf[Range[100]];
  VerificationTest[stats["count"], 100,
    TestID -> "percentiles/count"];
  VerificationTest[stats["max"], 100.,
    TestID -> "percentiles/max"];
  VerificationTest[stats["mean"], 50.5,
    TestID -> "percentiles/mean"];
  VerificationTest[stats["p50"] >= 50 && stats["p50"] <= 51, True,
    TestID -> "percentiles/p50-near-median"];
  VerificationTest[stats["p99"] >= 99., True,
    TestID -> "percentiles/p99-tail"];
];

(* Negative durations get clipped to 0 (LocalSubmit can occasionally
   report tiny negative timings due to clock skew). *)
VerificationTest[
  ChallengesBenchmark`Private`percentilesOf[{-1.0*^-6, 0.5, 1.0}]["max"],
  1.0,
  TestID -> "percentiles/clips-negative"
];

(* Non-numeric entries are filtered out, not coerced. *)
VerificationTest[
  ChallengesBenchmark`Private`percentilesOf[{1, 2, "bogus", Null, 3}]["count"],
  3,
  TestID -> "percentiles/skips-non-numeric"
];

(* Summarised results expose duration percentiles when there's at least
   one Evaluated row. *)
Module[{rs, summary},
  rs = Table[
    <|"testId" -> "t/" <> ToString[i],
      "challengeName" -> "Foo",
      "status" -> "Evaluated", "passed" -> True,
      "expected" -> i, "actualOutput" -> i,
      "durationSec" -> i*0.01, "memoryBytes" -> 1000 i|>,
    {i, 1, 100}
  ];
  summary = ChallengesBenchmark`Private`summarizeResults[rs];
  VerificationTest[
    KeyExistsQ[summary, "duration"] &&
    KeyExistsQ[summary["duration"], "p99"],
    True,
    TestID -> "summarize/exposes-duration-percentiles"
  ];
  VerificationTest[summary["duration", "count"], 100,
    TestID -> "summarize/duration-count"];
];

(* ----------------------------------------------------------------------- *)
(* CompareModels — cross-model comparison                                   *)

Module[{tmp, writeRun, cmp, reportPaths},
  tmp = CreateDirectory[];

  (* Helper that writes a fake run directory with a run.json + results.wxf. *)
  writeRun[name_String, model_String, perTest_List] :=
    Module[{dir, summary},
      dir = FileNameJoin[{tmp, name}];
      CreateDirectory[dir];
      summary = <|
        "total"    -> Length[perTest],
        "passed"   -> Count[perTest, _?(#["passed"] === True &)],
        "passRate" ->
          N[Count[perTest, _?(#["passed"] === True &)] / Length[perTest]],
        "duration" -> <|"count" -> Length[perTest],
                        "p50" -> 0.1, "p90" -> 0.3, "p99" -> 0.5,
                        "max" -> 0.5|>
      |>;
      Export[FileNameJoin[{dir, "run.json"}],
        <|"runId" -> name, "model" -> model, "summary" -> summary|>,
        "RawJSON"];
      Export[FileNameJoin[{dir, "results.wxf"}], perTest, "WXF"];
      dir
    ];

  (* Model A passes Foo only; Model B passes Bar only. *)
  writeRun["rA", "model-a", {
    <|"challengeName" -> "Foo", "testId" -> "Foo/1", "passed" -> True,  "status" -> "Evaluated"|>,
    <|"challengeName" -> "Foo", "testId" -> "Foo/2", "passed" -> True,  "status" -> "Evaluated"|>,
    <|"challengeName" -> "Bar", "testId" -> "Bar/1", "passed" -> False, "status" -> "Evaluated"|>
  }];
  writeRun["rB", "model-b", {
    <|"challengeName" -> "Foo", "testId" -> "Foo/1", "passed" -> False, "status" -> "Evaluated"|>,
    <|"challengeName" -> "Foo", "testId" -> "Foo/2", "passed" -> False, "status" -> "Evaluated"|>,
    <|"challengeName" -> "Bar", "testId" -> "Bar/1", "passed" -> True,  "status" -> "Evaluated"|>
  }];

  cmp = ChallengesBenchmark`CompareModels[{
    FileNameJoin[{tmp, "rA"}],
    FileNameJoin[{tmp, "rB"}]
  }];

  VerificationTest[Sort[cmp["models"]], {"model-a", "model-b"},
    TestID -> "CompareModels/picks-model-labels"];
  VerificationTest[cmp["allChallenges"], {"Bar", "Foo"},
    TestID -> "CompareModels/union-of-challenges"];
  VerificationTest[cmp["uniquelyPassed", "model-a"], {"Foo"},
    TestID -> "CompareModels/uniquelyPassed-A"];
  VerificationTest[cmp["uniquelyPassed", "model-b"], {"Bar"},
    TestID -> "CompareModels/uniquelyPassed-B"];
  VerificationTest[cmp["uniquelyFailed", "model-a"], {"Bar"},
    TestID -> "CompareModels/uniquelyFailed-A"];

  (* Association form overrides the model labels. *)
  cmp = ChallengesBenchmark`CompareModels[<|
    "first"  -> FileNameJoin[{tmp, "rA"}],
    "second" -> FileNameJoin[{tmp, "rB"}]
  |>];
  VerificationTest[Sort[cmp["models"]], {"first", "second"},
    TestID -> "CompareModels/honors-association-labels"];

  (* Report generation lands two files and closes them. *)
  reportPaths = ChallengesBenchmark`WriteCompareReport[cmp,
    FileNameJoin[{tmp, "report"}]];
  VerificationTest[FileExistsQ[reportPaths["markdown"]], True,
    TestID -> "CompareModels/markdown-written"];
  VerificationTest[FileExistsQ[reportPaths["html"]], True,
    TestID -> "CompareModels/html-written"];
  VerificationTest[
    StringContainsQ[Import[reportPaths["markdown"], "Text"], "Model comparison"],
    True,
    TestID -> "CompareModels/markdown-has-header"];

  Quiet @ DeleteDirectory[tmp, DeleteContents -> True];
];

(* ----------------------------------------------------------------------- *)
(* JUnit XML export                                                        *)
(*                                                                         *)
(* The JUnit rendering is a contract with external CI systems, so we pin  *)
(* (a) well-formed XML output, (b) correct status mapping (pass vs       *)
(* <failure> vs <error> vs <skipped>), (c) XML-entity escaping of       *)
(* payload characters, (d) the "]]>" split inside CDATA, and (e) that    *)
(* WriteReport writes junit.xml alongside the other formats.             *)
(* ----------------------------------------------------------------------- *)

Module[{junitRun, xml, parsed},

  junitRun = <|
    "meta" -> <|
      "runId" -> "run-junit-test",
      "model" -> "test-model",
      "createdAt"  -> "2026-04-20T10:00:00",
      "finishedAt" -> "2026-04-20T10:00:03",
      "durationSec" -> 3.14,
      "status" -> "finished",
      "summary" -> <|"total" -> 5, "passed" -> 1, "failed" -> 1, "passRate" -> 0.2|>,
      "runtime" -> <||>,
      "options" -> <|"seed" -> 1|>
    |>,
    "results" -> {
      <|"testId" -> "Foo/1", "challengeName" -> "Foo",
        "status" -> "Evaluated", "passed" -> True,
        "expected" -> 1, "actualOutput" -> 1,
        "durationSec" -> 0.01, "error" -> None|>,
      <|"testId" -> "Foo/2", "challengeName" -> "Foo",
        "status" -> "Evaluated", "passed" -> False,
        "expected" -> 2, "actualOutput" -> 99,
        "durationSec" -> 0.02, "error" -> None|>,
      <|"testId" -> "Bar/1", "challengeName" -> "Bar",
        "status" -> "TimedOut", "passed" -> False,
        "expected" -> 0, "actualOutput" -> Missing["TimedOut"],
        "durationSec" -> 1.0, "error" -> "time-constraint exceeded"|>,
      <|"testId" -> "Baz/1", "challengeName" -> "Baz",
        "status" -> "NoSolution", "passed" -> False,
        "expected" -> 0, "actualOutput" -> Missing["NoSolution"],
        "durationSec" -> 0., "error" -> None|>,
      (* XML-escape stress test: <, >, &, quotes, and a raw CDATA terminator. *)
      <|"testId" -> "Html/1", "challengeName" -> "Html",
        "status" -> "Evaluated", "passed" -> False,
        "expected" -> "<a href=\"x\">y & z</a>",
        "actualOutput" -> "text with ]]> embedded",
        "durationSec" -> 0.002, "error" -> None|>
    }
  |>;

  xml = ChallengesBenchmark`Private`renderJUnit[junitRun];

  VerificationTest[
    StringQ[xml] && StringStartsQ[xml, "<?xml"],
    True,
    TestID -> "JUnit/well-formed-prolog"
  ];

  (* ImportString as XML must parse cleanly — proves well-formedness. *)
  parsed = Quiet @ ImportString[xml, "XML"];
  VerificationTest[
    Head[parsed] === XMLObject["Document"],
    True,
    TestID -> "JUnit/parses-as-xml"
  ];

  (* Suite-level attribute counts must match what the runner produced. *)
  VerificationTest[
    StringContainsQ[xml, "tests=\"5\""] &&
    StringContainsQ[xml, "failures=\"2\""] &&   (* Foo/2 + Html/1 *)
    StringContainsQ[xml, "errors=\"1\""]   &&   (* Bar/1 *)
    StringContainsQ[xml, "skipped=\"1\""],      (* Baz/1 *)
    True,
    TestID -> "JUnit/testsuites-attrs-correct"
  ];

  (* Passing test gets a bare self-closed <testcase/> — no failure/error.
     Check the specific line containing Foo/1 rather than pattern-matching
     across the whole document, because `___` would greedily match over
     subsequent failing tests' <failure> elements. *)
  Module[{fooLine},
    fooLine = First @ Select[
      StringSplit[xml, "\n"],
      StringContainsQ[#, "name=\"Foo/1\""] &
    ];
    VerificationTest[
      StringEndsQ[StringTrim[fooLine], "/>"] &&
        ! StringContainsQ[fooLine, "<failure"] &&
        ! StringContainsQ[fooLine, "<error"],
      True,
      TestID -> "JUnit/pass-emits-bare-testcase"
    ]
  ];

  (* Failing assertion gets a <failure> tag. *)
  VerificationTest[
    StringContainsQ[xml, "<failure type=\"AssertionError\""],
    True,
    TestID -> "JUnit/failure-mapped-for-wrong-answer"
  ];

  (* Timeout gets <error type="TimedOut">. *)
  VerificationTest[
    StringContainsQ[xml, "<error type=\"TimedOut\""],
    True,
    TestID -> "JUnit/error-mapped-for-timeout"
  ];

  (* NoSolution gets <skipped>. *)
  VerificationTest[
    StringContainsQ[xml, "<skipped message=\"no solution provided\""],
    True,
    TestID -> "JUnit/skipped-mapped-for-no-solution"
  ];

  (* XML entity escaping of <, >, & in attribute payload. *)
  VerificationTest[
    StringContainsQ[xml, "&lt;a href"] &&
    StringContainsQ[xml, "&amp;"] &&
    StringContainsQ[xml, "&gt;"],
    True,
    TestID -> "JUnit/escapes-xml-entities-in-message"
  ];

  (* "]]>" inside a CDATA must be split using the canonical XML idiom. *)
  VerificationTest[
    StringContainsQ[xml, "]]]]><![CDATA[>"],
    True,
    TestID -> "JUnit/splits-CDATA-terminator"
  ];

  (* WriteReport must emit junit.xml alongside the other formats. *)
  Module[{tmp, paths},
    tmp = CreateDirectory[];
    paths = ChallengesBenchmark`WriteReport[junitRun, tmp];
    VerificationTest[
      KeyExistsQ[paths, "junit"] && FileExistsQ[paths["junit"]],
      True,
      TestID -> "JUnit/WriteReport-emits-junit-xml"
    ];
    VerificationTest[
      Head @ Quiet @ Import[paths["junit"], "XML"],
      XMLObject["Document"],
      TestID -> "JUnit/WriteReport-output-parses"
    ];
    Quiet @ DeleteDirectory[tmp, DeleteContents -> True];
  ];

  (* WriteJUnitReport writes a standalone file at a custom path. *)
  Module[{tmp, path},
    tmp = CreateDirectory[];
    path = FileNameJoin[{tmp, "deep", "nested", "junit.xml"}];
    ChallengesBenchmark`WriteJUnitReport[junitRun, path];
    VerificationTest[
      FileExistsQ[path],
      True,
      TestID -> "JUnit/WriteJUnitReport-creates-intermediate-dirs"
    ];
    Quiet @ DeleteDirectory[tmp, DeleteContents -> True];
  ];
];

(* ----------------------------------------------------------------------- *)
(* TestBankBuilder — .wlchallenge parse, emit, and round-trip                *)
(*                                                                         *)
(* The authoring format is user-facing: we pin (a) scalar and block header *)
(* parsing; (b) that test inputs stay held during parse; (c) that bad     *)
(* inputs produce $Failed with a message rather than crashing; (d) that   *)
(* a full write → read cycle preserves challenge count, test count, and   *)
(* held inputs.                                                           *)
(* ----------------------------------------------------------------------- *)

Module[{tmp, writeFile, readOneChallenge, aPath},
  tmp = CreateDirectory[];

  writeFile[name_String, body_String] := Module[{p},
    p = FileNameJoin[{tmp, name <> ".wlchallenge"}];
    Export[p, body, "Text", CharacterEncoding -> "UTF-8"];
    p
  ];

  aPath = writeFile["aliquot", StringJoin[
    "(* :Name: Aliquot *)\n",
    "(* :Index: 7 *)\n",
    "(* :Instruction: Define Aliquot. *)\n",
    "\n",
    "(* :Prompt: *)\n",
    "Define the aliquot function.\n",
    "Second line of prose.\n",
    "\n",
    "(* :Tests: *)\n",
    "{Aliquot[1], {1}}\n",
    "{Aliquot[2], {2, 1}}\n"
  ]];

  readOneChallenge = ChallengesBenchmark`LoadWLChallenge[aPath];

  VerificationTest[
    readOneChallenge["name"],
    "Aliquot",
    TestID -> "WLChallenge/parses-name"
  ];

  VerificationTest[
    readOneChallenge["index"],
    7,
    TestID -> "WLChallenge/parses-index"
  ];

  VerificationTest[
    StringContainsQ[readOneChallenge["prompt"], "aliquot"],
    True,
    TestID -> "WLChallenge/parses-block-prompt"
  ];

  VerificationTest[
    Length[readOneChallenge["tests"]],
    2,
    TestID -> "WLChallenge/parses-both-tests"
  ];

  (* Inputs must stay held: the test harness never evaluates
     Aliquot[1] during the parse. *)
  VerificationTest[
    Head[readOneChallenge["tests"][[1, 1]]],
    HoldComplete,
    TestID -> "WLChallenge/input-stays-held"
  ];

  VerificationTest[
    readOneChallenge["tests"][[1, 2]],
    {1},
    TestID -> "WLChallenge/expected-is-evaluated"
  ];

  (* Test-line comments and blanks inside :Tests: must be skipped. *)
  Module[{p, ch},
    p = writeFile["withcomments", StringJoin[
      "(* :Name: WithComments *)\n",
      "(* :Prompt: *)\n",
      "p\n",
      "(* :Tests: *)\n",
      "(* edge cases *)\n",
      "{Foo[1], 1}\n",
      "\n",
      "(* another group *)\n",
      "{Foo[2], 2}\n"
    ]];
    ch = ChallengesBenchmark`LoadWLChallenge[p];
    VerificationTest[
      Length[ch["tests"]],
      2,
      TestID -> "WLChallenge/skips-comments-and-blanks-in-tests"
    ];
  ];

  (* Missing :Name: must yield $Failed. *)
  Module[{p, r},
    p = writeFile["noname",
      "(* :Prompt: *)\np\n(* :Tests: *)\n{Foo[], 0}\n"];
    r = Quiet @ ChallengesBenchmark`LoadWLChallenge[p];
    VerificationTest[
      r,
      $Failed,
      TestID -> "WLChallenge/missing-name-fails"
    ];
  ];

  (* Missing :Tests: must yield $Failed. *)
  Module[{p, r},
    p = writeFile["notests",
      "(* :Name: X *)\n(* :Prompt: *)\np\n"];
    r = Quiet @ ChallengesBenchmark`LoadWLChallenge[p];
    VerificationTest[
      r,
      $Failed,
      TestID -> "WLChallenge/missing-tests-fails"
    ];
  ];

  (* Unparseable test line must yield $Failed. *)
  Module[{p, r},
    p = writeFile["badtest", StringJoin[
      "(* :Name: BadTest *)\n",
      "(* :Prompt: *)\np\n",
      "(* :Tests: *)\n",
      "{Foo[1, 1}\n"   (* mismatched brackets *)
    ]];
    r = Quiet @ ChallengesBenchmark`LoadWLChallenge[p];
    VerificationTest[
      r,
      $Failed,
      TestID -> "WLChallenge/bad-test-line-fails"
    ];
  ];

  (* Directory load: all files come back as one Association,
     sorted by :Index:. *)
  Module[{bundleDir, ch1, ch2, loaded, keys},
    bundleDir = CreateDirectory[];
    ch1 = FileNameJoin[{bundleDir, "a.wlchallenge"}];
    ch2 = FileNameJoin[{bundleDir, "b.wlchallenge"}];
    Export[ch1,
      "(* :Name: Alpha *)\n(* :Index: 2 *)\n(* :Prompt: *)\np\n(* :Tests: *)\n{Alpha[1], 1}\n",
      "Text"];
    Export[ch2,
      "(* :Name: Beta *)\n(* :Index: 1 *)\n(* :Prompt: *)\np\n(* :Tests: *)\n{Beta[1], 2}\n",
      "Text"];
    loaded = ChallengesBenchmark`LoadChallengesDir[bundleDir];
    keys   = Keys[loaded];
    VerificationTest[
      keys,
      {"Beta", "Alpha"},
      TestID -> "WLChallenge/dir-sorts-by-index"
    ];
    VerificationTest[
      Length[loaded],
      2,
      TestID -> "WLChallenge/dir-loads-all-files"
    ];
    Quiet @ DeleteDirectory[bundleDir, DeleteContents -> True];
  ];

  (* Full round-trip: parse .wlchallenge dir -> emit WXF+JSON -> reload -> compare *)
  Module[{build, challenges, bank, outDir, reloadedC, reloadedB,
          sampleTests, heldA, heldB},
    build = ChallengesBenchmark`BuildTestBank[tmp];
    {challenges, bank} = build;

    outDir = CreateDirectory[];
    ChallengesBenchmark`WriteTestBankFiles[tmp,
      FileNameJoin[{outDir, "c.json"}],
      FileNameJoin[{outDir, "b.wxf"}]];

    reloadedC = ChallengesBenchmark`LoadChallenges[
      FileNameJoin[{outDir, "c.json"}]];
    reloadedB = ChallengesBenchmark`LoadTestBank[
      FileNameJoin[{outDir, "b.wxf"}]];

    VerificationTest[
      Length[reloadedC],
      Length[challenges],
      TestID -> "WLChallenge/roundtrip-challenge-count"
    ];

    VerificationTest[
      Length[reloadedB],
      Length[bank],
      TestID -> "WLChallenge/roundtrip-bank-count"
    ];

    VerificationTest[
      Total[Length /@ Values[reloadedB]],
      Total[Length /@ Values[bank]],
      TestID -> "WLChallenge/roundtrip-test-count"
    ];

    (* Held inputs must survive the build step structurally: compare
       FullForm strings to rule out silent evaluation. *)
    sampleTests = bank["Aliquot"];
    heldA = ToString[FullForm[sampleTests[[1]]["input"]]];
    heldB = ToString[FullForm[reloadedB["Aliquot"][[1]]["input"]]];
    VerificationTest[
      heldA,
      heldB,
      TestID -> "WLChallenge/roundtrip-held-input-preserved"
    ];

    Quiet @ DeleteDirectory[outDir, DeleteContents -> True];
  ];

  Quiet @ DeleteDirectory[tmp, DeleteContents -> True];
];

(* ----------------------------------------------------------------------- *)
(* Generator                                                               *)
(* ----------------------------------------------------------------------- *)
(* All generator tests use an injected "Generator" function so none of     *)
(* them depend on LLMSynthesize or network access.                         *)

Module[{genChallenges, genBank, tmp},

  genChallenges = <|
    "Foo" -> <|"index" -> 1, "name" -> "Foo", "instruction" -> "",
      "prompt" -> "Write Foo[x_] that returns x+1."|>,
    "Bar" -> <|"index" -> 2, "name" -> "Bar", "instruction" -> "",
      "prompt" -> "Write Bar[x_] that returns 2 x."|>
  |>;

  genBank = <|
    "Foo" -> {<|"challengeName" -> "Foo", "testIndex" -> 1,
      "input" -> HoldComplete[Foo[1]], "expected" -> 2, "metadata" -> <||>|>},
    "Bar" -> {<|"challengeName" -> "Bar", "testIndex" -> 1,
      "input" -> HoldComplete[Bar[3]], "expected" -> 6, "metadata" -> <||>|>}
  |>;

  (* ------- buildPrompt ------- *)

  VerificationTest[
    StringContainsQ[
      ChallengesBenchmark`Private`buildPrompt[
        "Challenge:\n{{CHALLENGE}}",
        <|"prompt" -> "Compute X."|>
      ],
      "Compute X."
    ],
    True,
    TestID -> "Generator/buildPrompt-substitutes-marker"
  ];

  VerificationTest[
    ChallengesBenchmark`Private`buildPrompt[
      "Challenge:\n{{CHALLENGE}}",
      <|"prompt" -> ""|>
    ],
    $Failed,
    TestID -> "Generator/buildPrompt-empty-body-fails"
  ];

  (* ------- callLLMWithRetry ------- *)

  VerificationTest[
    Module[{stub, r},
      stub = Function[{prompt}, "```wl\nFoo[x_] := x + 1\n```"];
      r = ChallengesBenchmark`Private`callLLMWithRetry[stub, "hi",
        <|"MaxAttempts" -> 3, "TimeConstraint" -> 10, "RetryBaseDelay" -> 0|>];
      {r["status"], Length[r["attempts"]], StringQ[r["response"]]}
    ],
    {"ok", 1, True},
    TestID -> "Generator/callLLMWithRetry/success-first-attempt"
  ];

  VerificationTest[
    Module[{stub, r},
      stub = Function[{prompt}, $Failed];
      r = ChallengesBenchmark`Private`callLLMWithRetry[stub, "hi",
        <|"MaxAttempts" -> 3, "TimeConstraint" -> 5, "RetryBaseDelay" -> 0|>];
      {r["status"], Length[r["attempts"]], r["response"]}
    ],
    {"failed", 3, None},
    TestID -> "Generator/callLLMWithRetry/exhausts-retries-on-failure"
  ];

  VerificationTest[
    Module[{stub, r},
      (* retry-until-success: first two calls fail, third succeeds. *)
      Module[{ctr = 0},
        stub = Function[{prompt},
          ctr++;
          If[ctr < 3, $Failed, "```wl\nOk[x_] := x\n```"]
        ]
      ];
      r = ChallengesBenchmark`Private`callLLMWithRetry[stub, "hi",
        <|"MaxAttempts" -> 5, "TimeConstraint" -> 5, "RetryBaseDelay" -> 0|>];
      {r["status"], Length[r["attempts"]]}
    ],
    {"ok", 3},
    TestID -> "Generator/callLLMWithRetry/retries-then-succeeds"
  ];

  VerificationTest[
    Module[{stub, r},
      stub = Function[{prompt}, Pause[3]; "never gets here"];
      r = ChallengesBenchmark`Private`callLLMWithRetry[stub, "hi",
        <|"MaxAttempts" -> 2, "TimeConstraint" -> 1, "RetryBaseDelay" -> 0|>];
      r["status"]
    ],
    "timeout",
    TestID -> "Generator/callLLMWithRetry/timeout-classified"
  ];

  (* ------- processOneChallenge happy path ------- *)

  tmp = CreateDirectory[];

  VerificationTest[
    Module[{stub, opts, r, wlPath, metaPath, meta},
      stub = Function[{prompt}, "```wl\nFoo[x_] := x + 1\n```"];
      opts = <|
        "OutputDirectory" -> tmp,
        "Generator"       -> stub,
        "MaxAttempts"     -> 1,
        "TimeConstraint"  -> 5,
        "RetryBaseDelay"  -> 0,
        "LLMInfo"         -> <|"service" -> "StubCo", "modelId" -> "stub-v1"|>
      |>;
      r = ChallengesBenchmark`Private`processOneChallenge[
        "Foo", genChallenges["Foo"], genBank, opts];
      wlPath   = FileNameJoin[{tmp, "Foo.wl"}];
      metaPath = FileNameJoin[{tmp, "Foo.meta.json"}];
      meta = Import[metaPath, "RawJSON"];
      {
        r["status"],
        FileExistsQ[wlPath],
        StringContainsQ[Import[wlPath, "Text"], "Foo[x_]"],
        Lookup[meta, "generator", None],
        AssociationQ[Lookup[meta, "llm", None]] &&
          Lookup[meta["llm"], "service"] === "StubCo"
      }
    ],
    {"ok", True, True, "GenerateSolutions/v1", True},
    TestID -> "Generator/processOneChallenge/writes-solution-and-enriched-meta"
  ];

  VerificationTest[
    Module[{stub, opts, r},
      (* The generator returns code that defines the wrong function.
         The SaveSolution audit must refuse and we must surface the
         audit-rejected status on the outcome. *)
      stub = Function[{prompt}, "```wl\nWrongName[x_] := x + 1\n```"];
      opts = <|
        "OutputDirectory" -> tmp,
        "Generator"       -> stub,
        "MaxAttempts"     -> 1,
        "TimeConstraint"  -> 5,
        "RetryBaseDelay"  -> 0
      |>;
      r = Quiet @ ChallengesBenchmark`Private`processOneChallenge[
        "Bar", genChallenges["Bar"], genBank, opts];
      r["status"]
    ],
    "audit-rejected",
    TestID -> "Generator/processOneChallenge/audit-rejects-wrong-name"
  ];

  VerificationTest[
    Module[{stub, opts, r},
      stub = Function[{prompt}, $Failed];
      opts = <|
        "OutputDirectory" -> tmp,
        "Generator"       -> stub,
        "MaxAttempts"     -> 2,
        "TimeConstraint"  -> 5,
        "RetryBaseDelay"  -> 0
      |>;
      r = ChallengesBenchmark`Private`processOneChallenge[
        "Foo", genChallenges["Foo"], genBank, opts];
      r["status"]
    ],
    "llm-failed",
    TestID -> "Generator/processOneChallenge/surfaces-llm-failure"
  ];

  VerificationTest[
    Module[{stub, opts, r},
      stub = Function[{prompt}, "```wl\n\n```"];
      opts = <|
        "OutputDirectory" -> tmp,
        "Generator"       -> stub,
        "MaxAttempts"     -> 1,
        "TimeConstraint"  -> 5,
        "RetryBaseDelay"  -> 0
      |>;
      r = ChallengesBenchmark`Private`processOneChallenge[
        "Foo", genChallenges["Foo"], genBank, opts];
      r["status"]
    ],
    "empty-extracted",
    TestID -> "Generator/processOneChallenge/empty-response-flagged"
  ];

  Quiet @ DeleteDirectory[tmp, DeleteContents -> True];

  (* ------- generateSolutionsImpl top-level ------- *)

  tmp = CreateDirectory[];

  VerificationTest[
    Module[{stub, r, logPath, lines, events},
      stub = Function[{prompt}, "```wl\nFoo[x_] := x + 1\n```"];
      r = ChallengesBenchmark`GenerateSolutions[
        KeyTake[genChallenges, {"Foo"}], genBank,
        <|
          "Model"           -> "test-model",
          "OutputDirectory" -> tmp,
          "Generator"       -> stub,
          "MaxAttempts"     -> 1,
          "TimeConstraint"  -> 5,
          "RetryBaseDelay"  -> 0,
          "LLMInfo"         -> <|"service" -> "StubCo"|>
        |>
      ];
      logPath = r["logPath"];
      lines   = StringSplit[Import[logPath, "Text"], "\n"];
      events  = Map[
        Lookup[ImportString[#, "RawJSON"], "event"] &,
        Select[lines, StringLength[#] > 0 &]
      ];
      {
        r["counts"]["ok"],
        r["counts"]["failed"],
        FileExistsQ[FileNameJoin[{tmp, "Foo.wl"}]],
        MemberQ[events, "generate.start"],
        MemberQ[events, "challenge.saved"],
        MemberQ[events, "generate.finished"]
      }
    ],
    {1, 0, True, True, True, True},
    TestID -> "Generator/generateSolutions/writes-and-logs-jsonl"
  ];

  VerificationTest[
    Module[{stub, callCount = 0, r},
      (* With Foo.wl already on disk from the previous test, a fresh run
         without --overwrite must skip the challenge entirely: the stub
         should never be called. *)
      stub = Function[{prompt}, callCount++; "```wl\nFoo[x_] := x\n```"];
      r = ChallengesBenchmark`GenerateSolutions[
        KeyTake[genChallenges, {"Foo"}], genBank,
        <|
          "Model"           -> "test-model",
          "OutputDirectory" -> tmp,
          "Generator"       -> stub,
          "Overwrite"       -> False,
          "MaxAttempts"     -> 1,
          "TimeConstraint"  -> 5,
          "RetryBaseDelay"  -> 0
        |>
      ];
      {callCount, Length[Keys[r["results"]]]}
    ],
    {0, 0},
    TestID -> "Generator/generateSolutions/skips-existing-by-default"
  ];

  VerificationTest[
    Module[{stub, callCount = 0, r},
      stub = Function[{prompt}, callCount++;
        "```wl\nFoo[x_] := x + 1\n```"];
      r = ChallengesBenchmark`GenerateSolutions[
        KeyTake[genChallenges, {"Foo"}], genBank,
        <|
          "Model"           -> "test-model",
          "OutputDirectory" -> tmp,
          "Generator"       -> stub,
          "Overwrite"       -> True,
          "MaxAttempts"     -> 1,
          "TimeConstraint"  -> 5,
          "RetryBaseDelay"  -> 0
        |>
      ];
      {callCount, r["counts"]["ok"]}
    ],
    {1, 1},
    TestID -> "Generator/generateSolutions/overwrite-true-regenerates"
  ];

  VerificationTest[
    Module[{r},
      r = ChallengesBenchmark`GenerateSolutions[
        genChallenges, genBank,
        <|
          "Model"           -> "test-model",
          "OutputDirectory" -> FileNameJoin[{tmp, "dry"}],
          "DryRun"          -> True,
          "Overwrite"       -> True,
          "MaxAttempts"     -> 1,
          "TimeConstraint"  -> 5,
          "RetryBaseDelay"  -> 0
        |>
      ];
      (* Dry-run stubs emit a "DryRunSolution" definition, which
         audit-rejects against Foo/Bar — both should land in
         auditRejected, neither in ok. *)
      {r["counts"]["ok"], r["counts"]["auditRejected"], r["dryRun"]}
    ],
    {0, 2, True},
    TestID -> "Generator/generateSolutions/dry-run-uses-stub"
  ];

  VerificationTest[
    Module[{stub, r},
      (* Filter to just "Bar": "Foo" must be absent from results. *)
      stub = Function[{prompt}, "```wl\nBar[x_] := 2 x\n```"];
      r = ChallengesBenchmark`GenerateSolutions[
        genChallenges, genBank,
        <|
          "Model"           -> "test-model",
          "OutputDirectory" -> FileNameJoin[{tmp, "filt"}],
          "Generator"       -> stub,
          "Filter"          -> {"Bar"},
          "Overwrite"       -> True,
          "MaxAttempts"     -> 1,
          "TimeConstraint"  -> 5,
          "RetryBaseDelay"  -> 0
        |>
      ];
      {Keys[r["results"]], r["counts"]["ok"]}
    ],
    {{"Bar"}, 1},
    TestID -> "Generator/generateSolutions/filter-narrows-names"
  ];

  Quiet @ DeleteDirectory[tmp, DeleteContents -> True];
];

(* ----------------------------------------------------------------------- *)
(* SaveSolution 5-arg overload                                             *)
(* ----------------------------------------------------------------------- *)

Module[{tmp, bank, path, meta},
  tmp = CreateDirectory[];
  bank = <|"Foo" -> {<|"challengeName" -> "Foo", "testIndex" -> 1,
    "input" -> HoldComplete[Foo[1]], "expected" -> 2, "metadata" -> <||>|>}|>;

  path = ChallengesBenchmark`SaveSolution[tmp, "Foo",
    "Foo[x_] := x + 1", bank,
    <|"promptHash" -> "sha256:abc", "attempts" -> 2|>];
  meta = Import[
    StringReplace[path, ".wl" ~~ EndOfString -> ".meta.json"],
    "RawJSON"];

  VerificationTest[
    {
      Lookup[meta, "promptHash", None],
      Lookup[meta, "attempts",   None],
      Lookup[meta, "challengeName", None],
      StringStartsQ[Lookup[meta, "sourceHash", ""], "sha256:"]
    },
    {"sha256:abc", 2, "Foo", True},
    TestID -> "SaveSolution/5arg-merges-extra-meta"
  ];

  Quiet @ DeleteDirectory[tmp, DeleteContents -> True];
];

