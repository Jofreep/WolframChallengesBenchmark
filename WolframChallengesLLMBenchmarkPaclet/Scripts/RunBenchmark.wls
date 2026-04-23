#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ----------------------------------------------------------------------- *)
(*  RunBenchmark.wls                                                       *)
(*                                                                         *)
(*  CLI front-end for JofreEspigulePons`WolframChallengesBenchmark`        *)
(*  RunBenchmark[].  Loads a challenge bank, a test bank, and a directory  *)
(*  of candidate solutions, then evaluates every test in a sandboxed      *)
(*  per-test sub-kernel and writes a run directory containing             *)
(*    run.json          aggregate run metadata + summary                   *)
(*    progress.jsonl    one event per test (submit / complete)             *)
(*    results.wxf       full per-test result list (WXF)                    *)
(*                                                                         *)
(*  Usage                                                                  *)
(*  -----                                                                  *)
(*    wolframscript -file Scripts/RunBenchmark.wls \                       *)
(*       --challenges path/to/challenges.json \                            *)
(*       --testbank   path/to/tests.wxf       \                            *)
(*       --solutions  solutions/<model>       \                            *)
(*       --model      <model-id>              \                            *)
(*       [--out runs]                                                      *)
(*       [--filter Aliquot,FizzBuzz]                                       *)
(*       [--parallel 8]                                                    *)
(*       [--timeout 60]                                                    *)
(*       [--memory 2000000000]                                             *)
(*       [--isolation PerTestKernel|PooledKernels|InProcess]               *)
(*       [--retry-on-death 1]                                              *)
(*       [--sandbox true|false]                                            *)
(*       [--baseline runs/<previous-run-id>]                               *)
(*       [--fail-under 0.65]                                               *)
(*                                                                         *)
(*  Exit codes                                                             *)
(*  ----------                                                             *)
(*    0  benchmark completed; pass rate >= --fail-under and (if --baseline *)
(*       was given) zero regressions                                       *)
(*    1  invalid arguments / missing input files                           *)
(*    2  pass rate < --fail-under                                          *)
(*    3  one or more regressions vs --baseline                             *)
(*    4  unexpected fatal error                                            *)
(* ----------------------------------------------------------------------- *)

(* ----------------------------------------------------------------------- *)
(* Boot the paclet                                                         *)
(*                                                                         *)
(* The script lives inside the paclet directory, so we register that      *)
(* directory with PacletDirectoryLoad and then Needs the public context.  *)
(* No global Wolfram installation of the paclet is required.              *)
(* ----------------------------------------------------------------------- *)

$scriptDir  = DirectoryName[$InputFileName];
$pacletRoot = DirectoryName[$scriptDir];

PacletDirectoryLoad[$pacletRoot];
Needs["JofreEspigulePons`WolframChallengesBenchmark`"];


(* ----------------------------------------------------------------------- *)
(* Argument parsing                                                        *)
(* ----------------------------------------------------------------------- *)

parseArgs[args_List] := Module[{i = 1, n = Length[args], out = <||>, k, v},
  While[i <= n,
    k = args[[i]];
    Which[
      StringStartsQ[k, "--"],
        v = If[i < n && ! StringStartsQ[args[[i + 1]], "--"],
               args[[i + 1]], "true"];
        out[StringDrop[k, 2]] = v;
        i += If[v === "true", 1, 2],
      True,
        i++
    ]
  ];
  out
];

withDefault[args_, key_, default_] := Lookup[args, key, default];

requireKey[args_, key_] :=
  If[KeyExistsQ[args, key],
    args[key],
    WriteString[$Output, "ERROR: missing required --", key, "\n"];
    Exit[1]
  ];

truthy[v_] := ! MemberQ[{"false", "False", "0", "off", "no"}, v];


(* ----------------------------------------------------------------------- *)
(* Solutions directory loader                                              *)
(*                                                                         *)
(* Reads <dir>/<challenge>.wl + <challenge>.meta.json into the shape       *)
(* RunBenchmark expects:                                                   *)
(*   <| challengeName -> <| "code" -> <wl source>, "meta" -> <meta> |> |> *)
(* ----------------------------------------------------------------------- *)

loadSolutionsDir[dir_String] := Module[{wls, recs},
  If[! DirectoryQ[dir],
    WriteString[$Output, "ERROR: solutions directory not found: ", dir, "\n"];
    Exit[1]
  ];
  wls = FileNames["*.wl", dir];
  recs = Map[
    Function[wl,
      Module[{name, code, metaPath, meta},
        name     = FileBaseName[wl];
        code     = Quiet @ Check[Import[wl, "Text"], ""];
        metaPath = StringReplace[wl, ".wl" ~~ EndOfString -> ".meta.json"];
        meta     = If[FileExistsQ[metaPath],
          Quiet @ Check[Import[metaPath, "RawJSON"], <||>],
          <||>];
        name -> <|"code" -> code, "meta" -> meta|>
      ]
    ],
    wls
  ];
  Association @ recs
];


(* ----------------------------------------------------------------------- *)
(* Main                                                                    *)
(* ----------------------------------------------------------------------- *)

main[] := Module[
  {args, challengesPath, testBankPath, solutionsDir, model, outDir,
   filter, parallel, timeout, memory, isolation, retryOnDeath, sandbox,
   baselinePath, failUnder, challenges, testBank, solutions, run,
   summary, exitCode = 0, ds},

  args = parseArgs[Drop[$ScriptCommandLine, 1]];

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
  sandbox      = truthy @ withDefault[args, "sandbox", "true"];
  baselinePath = Lookup[args, "baseline", None];
  failUnder    = ToExpression @ Lookup[args, "fail-under", "0"];

  filter = If[KeyExistsQ[args, "filter"],
    StringSplit[args["filter"], ","],
    All
  ];

  WriteString[$Output, "Loading challenges from ", challengesPath, "...\n"];
  challenges =
    JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges[challengesPath];
  If[challenges === $Failed, Exit[1]];

  WriteString[$Output, "Loading test bank from ", testBankPath, "...\n"];
  testBank =
    JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank[testBankPath];
  If[testBank === $Failed, Exit[1]];

  WriteString[$Output, "Loading solutions from ", solutionsDir, "...\n"];
  solutions = loadSolutionsDir[solutionsDir];

  WriteString[$Output,
    "Running benchmark (model=", model,
    ", parallel=",      ToString[parallel],
    ", isolation=",     isolation,
    ", retryOnDeath=",  retryOnDeath,
    ", sandbox=",       If[sandbox, "on", "OFF"], ")...\n"
  ];

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

  summary = run["meta", "summary"];
  ds      = Lookup[summary, "duration", <||>];

  WriteString[$Output, "\n=== ", model, " ===\n"];
  WriteString[$Output, "  passed:    ", summary["passed"], " / ",
                       summary["total"], "\n"];
  WriteString[$Output, "  pass rate: ",
              NumberForm[100. summary["passRate"], {4, 2}], "%\n"];
  WriteString[$Output, "  duration:  ", run["meta", "durationSec"], "s\n"];
  If[KeyExistsQ[ds, "p50"] && ds["count"] > 0,
    WriteString[$Output,
      "  per-test:  p50=", NumberForm[1.0 ds["p50"], {5, 2}],
      "s  p90=",            NumberForm[1.0 ds["p90"], {5, 2}],
      "s  p99=",            NumberForm[1.0 ds["p99"], {5, 2}],
      "s  max=",            NumberForm[1.0 ds["max"], {5, 2}], "s\n"]
  ];
  WriteString[$Output, "\nRun directory: ", run["runDir"], "\n"];

  If[summary["passRate"] < failUnder,
    WriteString[$Output, "FAIL: pass rate ",
      NumberForm[100. summary["passRate"], {4, 2}],
      "% < required ",
      NumberForm[100. failUnder, {4, 2}], "%\n"];
    exitCode = 2
  ];

  If[baselinePath =!= None,
    Module[{bMeta, bResultsPath, bResults, baselineRun, diff},
      bMeta = Quiet @ Check[
        Import[FileNameJoin[{baselinePath, "run.json"}], "RawJSON"], $Failed];
      bResultsPath = FileNameJoin[{baselinePath, "results.wxf"}];
      bResults = If[FileExistsQ[bResultsPath],
        Import[bResultsPath, "WXF"], {}];
      baselineRun = <|
        "runId"   -> Lookup[bMeta, "runId", "?"],
        "results" -> bResults
      |>;
      diff = JofreEspigulePons`WolframChallengesBenchmark`DiffRuns[
        baselineRun, run];
      WriteString[$Output, "\n=== diff vs baseline ",
        baselineRun["runId"], " ===\n"];
      WriteString[$Output, "  regressions: ",
        diff["summary", "regressions"], "\n"];
      WriteString[$Output, "  fixes:       ",
        diff["summary", "fixes"], "\n"];
      If[diff["summary", "regressions"] > 0,
        WriteString[$Output,
          "FAIL: ", diff["summary", "regressions"],
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
