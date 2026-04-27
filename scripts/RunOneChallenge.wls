#!/usr/bin/env wolframscript
(* ::Package:: *)

(* scripts/RunOneChallenge.wls

   One-shot benchmark runner against a single challenge for a single
   model.  Loads the paclet, reconciles names, loads solutions, runs
   RunBenchmark with the given filter, and prints a compact verdict.

   Usage:
     wolframscript -file scripts/RunOneChallenge.wls \
       --model minimax/minimax-m2.7 \
       --challenge FivePointConic
*)

parseArgs[args_List] := Module[{i = 1, n = Length[args], out = <||>, k, v},
  While[i <= n,
    k = args[[i]];
    If[StringStartsQ[k, "--"],
      v = If[i < n && ! StringStartsQ[args[[i + 1]], "--"],
             args[[i + 1]], "true"];
      out[StringDrop[k, 2]] = v;
      i += If[v === "true", 1, 2],
      i++
    ]
  ];
  out
];

modelSlug[m_String] := StringReplace[m,
  RegularExpression["[^A-Za-z0-9_.-]"] -> "_"];

args         = parseArgs[Drop[$ScriptCommandLine, 1]];
modelArg     = Lookup[args, "model",     "minimax/minimax-m2.7"];
challengeArg = Lookup[args, "challenge", "FivePointConic"];

here     = DirectoryName[$InputFileName];
root     = DirectoryName[here];
pacletDir = FileNameJoin[{root, "WolframChallengesLLMBenchmarkPaclet"}];
PacletDirectoryLoad[pacletDir];
Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

data = JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL[
  FileNameJoin[{root, "challenges.jsonl"}]];
challenges = data["challenges"];
testBank   = data["testBank"];

(* Use the public LoadSolutions paclet symbol instead of walking files
   inline.  It returns the same shape RunBenchmark consumes, plus
   optional "meta" / "wlPath" / "metaPath" fields we don't need here
   but cost nothing to carry through.                                 *)
solutionsDir = FileNameJoin[{root, "solutions", modelSlug[modelArg]}];
solutions = JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions[
  solutionsDir];
If[solutions === $Failed,
  WriteString[$Output, "ERROR: solutions directory not found: ",
    solutionsDir, "\n"];
  Exit[1]];

If[! KeyExistsQ[solutions, challengeArg],
  WriteString[$Output, "ERROR: no saved solution for ", challengeArg,
    " under ", solutionsDir, "\n"];
  WriteString[$Output, "  available: ",
    ToString[Take[Sort @ Keys @ solutions, UpTo[10]]], "\n"];
  Exit[1]
];

run = JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
  challenges, testBank, solutions,
  "Model"           -> modelArg,
  "IsolationMode"   -> "InProcess",
  "OutputDirectory" -> FileNameJoin[{root, "runs"}],
  "Filter"          -> {challengeArg},
  "TimeConstraint"  -> 30];

If[run === $Failed,
  WriteString[$Output, "RunBenchmark returned $Failed\n"];
  Exit[2]];

passed = run["meta", "summary", "passed"];
total  = run["meta", "summary", "total"];
statuses = Counts[#["status"] & /@ run["results"]];

WriteString[$Output, "\n=== ", modelArg, " / ", challengeArg, " ===\n"];
WriteString[$Output, "  passed:    ", passed, " / ", total, "\n"];
WriteString[$Output, "  statuses:  ", ToString[Normal @ statuses], "\n"];
WriteString[$Output, "  runDir:    ", run["runDir"], "\n"];

(* Per-test breakdown so we see which sub-case failed and how. *)
WriteString[$Output, "\n  per-test:\n"];
Scan[
  Function[r,
    WriteString[$Output,
      "    ", r["testId"],
      " -> ", r["status"],
      If[r["status"] === "Evaluated",
        " (passed=" <> ToString[r["passed"]] <>
        ", expected=" <> ToString[r["expected"], InputForm] <>
        ", actual="   <> ToString[r["actualOutput"], InputForm] <> ")",
        ""],
      "\n"]
  ],
  run["results"]
];

Exit[If[passed === total, 0, 2]]
