#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ChallengesBenchmark CLI runner.

   Usage:
     wolframscript -file scripts/RunBenchmark.wls \
        --challenges ChallengesTestDataV1.json \
        --testbank   ChallengesTests.wxf \
        --solutions  solutions/claude-opus-4.6 \
        --model      claude-opus-4.6 \
        --out        runs \
        [--filter Aliquot,FizzBuzz] \
        [--parallel 8] \
        [--timeout 60] \
        [--memory 2000000000] \
        [--isolation PerTestKernel] \
        [--retry-on-death 1] \
        [--sandbox true] \
        [--verify-solutions] \
        [--strict-verify] \
        [--junit path/to/junit.xml] \
        [--baseline runs/run-2026-04-01-...] \
        [--fail-under 0.65]

   Exits:
     0  benchmark completed and (if --fail-under is given) pass rate >= threshold
        and (if --baseline is given) no regressions
     1  invalid arguments / missing files
     2  benchmark completed but pass rate < --fail-under threshold
     3  benchmark completed but introduced regressions vs baseline
     4  unexpected error
     5  --strict-verify failed (mismatches / unparseable solutions found)
*)

(* ----------------------------------------------------------------------- *)
(* Argv parsing                                                            *)
(* ----------------------------------------------------------------------- *)

parseArgs[args_List] := Module[{i = 1, n = Length[args], out = <||>, k, v},
  While[i <= n,
    k = args[[i]];
    Which[
      StringStartsQ[k, "--"],
        v = If[i < n && ! StringStartsQ[args[[i + 1]], "--"], args[[i + 1]], "true"];
        out[StringDrop[k, 2]] = v;
        i += If[v === "true", 1, 2],
      True,
        i++
    ]
  ];
  out
];

withDefault[args_, key_, default_] := Lookup[args, key, default];

requireKey[args_, key_] := If[KeyExistsQ[args, key],
  args[key],
  WriteString[$Output, "ERROR: missing required --", key, "\n"]; Exit[1]
];

(* ----------------------------------------------------------------------- *)
(* Boot the paclet                                                         *)
(* ----------------------------------------------------------------------- *)

$scriptDir = DirectoryName[$InputFileName];
$repoRoot  = DirectoryName[$scriptDir];
$pacletDir = FileNameJoin[{$repoRoot, "WolframChallengesLLMBenchmarkPaclet"}];

Quiet @ PacletDirectoryLoad[$pacletDir];
Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

(* ----------------------------------------------------------------------- *)
(* Main                                                                    *)
(* ----------------------------------------------------------------------- *)

main[] := Module[
  {args, challengesPath, testBankPath, solutionsDir, model, outDir,
   filter, parallel, timeout, memory, isolation, retryOnDeath, sandbox,
   verifySolutionsFlag, strictVerify, baselinePath, failUnder,
   junitPath,
   challenges, testBank, solutions, verifyReport, run, reportPaths,
   baseline, diff, exitCode = 0},

  args = parseArgs[Drop[$ScriptCommandLine, 1]];   (* drop script name *)

  challengesPath = requireKey[args, "challenges"];
  testBankPath   = requireKey[args, "testbank"];
  solutionsDir   = requireKey[args, "solutions"];
  model          = requireKey[args, "model"];

  outDir       = withDefault[args, "out", FileNameJoin[{Directory[], "runs"}]];
  parallel     = ToExpression @ withDefault[args, "parallel", "Automatic"];
  timeout      = ToExpression @ withDefault[args, "timeout",  "60"];
  memory       = ToExpression @ withDefault[args, "memory",   "2000000000"];
  isolation    = withDefault[args, "isolation", "PerTestKernel"];
  retryOnDeath = ToExpression @ withDefault[args, "retry-on-death", "1"];
  sandbox      = ! MemberQ[{"false", "False", "0", "off", "no"},
                           withDefault[args, "sandbox", "true"]];
  baselinePath = Lookup[args, "baseline", None];
  failUnder    = ToExpression @ Lookup[args, "fail-under", "0"];
  junitPath    = Lookup[args, "junit", None];

  (* --verify-solutions and --strict-verify are presence flags. parseArgs
     turns a bare flag (no value) into the literal string "true". *)
  verifySolutionsFlag = KeyExistsQ[args, "verify-solutions"];
  strictVerify        = KeyExistsQ[args, "strict-verify"];
  If[strictVerify, verifySolutionsFlag = True];  (* strict implies verify *)

  filter = If[KeyExistsQ[args, "filter"],
    StringSplit[args["filter"], ","],
    All
  ];

  WriteString[$Output, "Loading challenges from ", challengesPath, "...\n"];
  challenges = JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges[challengesPath];
  If[challenges === $Failed, Exit[1]];

  WriteString[$Output, "Loading test bank from ", testBankPath, "...\n"];
  testBank = JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank[testBankPath];
  If[testBank === $Failed, Exit[1]];

  WriteString[$Output, "Loading solutions from ", solutionsDir, "...\n"];
  solutions = JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions[solutionsDir];

  If[verifySolutionsFlag,
    WriteString[$Output, "Auditing solutions directory ", solutionsDir, "...\n"];
    verifyReport = JofreEspigulePons`WolframChallengesBenchmark`AuditSolutions[solutionsDir, testBank];
    WriteString[$Output,
      "  ok:          ", verifyReport["okCount"], "\n",
      "  missing:     ", Length[verifyReport["missing"]],     "\n",
      "  unexpected:  ", Length[verifyReport["unexpected"]],  "\n",
      "  mismatches:  ", Length[verifyReport["mismatches"]],  "\n",
      "  unparseable: ", Length[verifyReport["unparseable"]], "\n",
      "  emptyCode:   ", Length[verifyReport["emptyCode"]],   "\n"
    ];
    If[Length[verifyReport["mismatches"]] > 0,
      WriteString[$Output, "  First mismatches:\n"];
      Scan[
        WriteString[$Output,
          "    ", #["name"], ": expected any of ", ToString[#["expected"]],
          ", defined ", ToString[#["defined"]], "\n"] &,
        Take[verifyReport["mismatches"], UpTo[10]]
      ]
    ];
    If[strictVerify && ! verifyReport["ok"],
      WriteString[$Output,
        "FAIL: --strict-verify: solutions directory has ",
        verifyReport["issueCount"], " issue(s).\n"];
      Exit[5]
    ]
  ];

  WriteString[$Output, "Running benchmark (model=", model, ", parallel=",
    ToString[parallel], ", isolation=", isolation,
    ", retryOnDeath=", retryOnDeath,
    ", sandbox=", If[sandbox, "on", "OFF"], ")...\n"];

  run = JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
    challenges, testBank, solutions,
    "Model"              -> model,
    "OutputDirectory"    -> outDir,
    "Parallel"           -> parallel,
    "TimeConstraint"     -> timeout,
    "MemoryConstraint"   -> memory,
    "IsolationMode"      -> isolation,
    "RetryOnKernelDeath" -> retryOnDeath,
    "Sandbox"            -> sandbox,
    "Filter"             -> filter
  ];

  If[run === $Failed, Exit[4]];

  WriteString[$Output, "Writing report to ", run["runDir"], "...\n"];
  reportPaths = JofreEspigulePons`WolframChallengesBenchmark`WriteReport[run, run["runDir"]];

  (* If --junit was given, also write the JUnit file to that override
     path (e.g. so CI can publish from a stable, run-id-independent
     location). The default junit.xml inside the run dir is always
     produced by WriteReport above. *)
  If[junitPath =!= None,
    WriteString[$Output, "Writing JUnit XML to ", junitPath, "...\n"];
    Quiet @ Check[
      JofreEspigulePons`WolframChallengesBenchmark`WriteJUnitReport[run, junitPath],
      WriteString[$Output, "WARN: failed to write JUnit XML to ", junitPath, "\n"]
    ]
  ];

  Module[{summary, ds},
    summary = run["meta", "summary"];
    ds      = Lookup[summary, "duration", <||>];
    WriteString[$Output, "\n=== ", model, " ===\n"];
    WriteString[$Output, "  passed:    ", summary["passed"], " / ", summary["total"], "\n"];
    WriteString[$Output, "  pass rate: ", NumberForm[100. summary["passRate"], {4, 2}], "%\n"];
    WriteString[$Output, "  duration:  ", run["meta", "durationSec"], "s\n"];
    If[KeyExistsQ[ds, "p50"] && ds["count"] > 0,
      WriteString[$Output,
        "  per-test:  p50=", NumberForm[1.0 ds["p50"], {5, 2}],
        "s  p90=",            NumberForm[1.0 ds["p90"], {5, 2}],
        "s  p99=",            NumberForm[1.0 ds["p99"], {5, 2}],
        "s  max=",            NumberForm[1.0 ds["max"], {5, 2}], "s\n"]
    ];
    WriteString[$Output, "\nReports: ", reportPaths["html"], "\n"];

    If[summary["passRate"] < failUnder,
      WriteString[$Output, "FAIL: pass rate ", NumberForm[100. summary["passRate"], {4, 2}],
        "% < required ", NumberForm[100. failUnder, {4, 2}], "%\n"];
      exitCode = 2
    ]
  ];

  If[baselinePath =!= None,
    Module[{bMeta, bResultsPath, bResults, baselineRun},
      bMeta = Quiet @ Check[Import[FileNameJoin[{baselinePath, "run.json"}], "RawJSON"], $Failed];
      bResultsPath = FileNameJoin[{baselinePath, "results.wxf"}];
      bResults = If[FileExistsQ[bResultsPath], Import[bResultsPath, "WXF"], {}];
      baselineRun = <|"runId" -> Lookup[bMeta, "runId", "?"], "results" -> bResults|>;
      diff = JofreEspigulePons`WolframChallengesBenchmark`DiffRuns[baselineRun, run];
      WriteString[$Output, "\n=== diff vs baseline ", baselineRun["runId"], " ===\n"];
      WriteString[$Output, "  regressions: ", diff["summary", "regressions"], "\n"];
      WriteString[$Output, "  fixes:       ", diff["summary", "fixes"], "\n"];
      If[diff["summary", "regressions"] > 0,
        WriteString[$Output, "FAIL: ", diff["summary", "regressions"],
          " regression(s) vs baseline:\n  ",
          StringRiffle[Take[diff["regressions"], UpTo[20]], "\n  "], "\n"];
        exitCode = Max[exitCode, 3]
      ]
    ]
  ];

  Exit[exitCode]
];

(* Top-level safety net *)
Module[{},
  Quiet @ Check[main[],
    WriteString[$Output, "FATAL: ", $MessageList, "\n"];
    Exit[4]
  ]
];
