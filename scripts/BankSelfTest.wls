#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Bank self-test: run the canonical solutions against their own tests.

   Loads private/canonical_solutions.jsonl, materializes one
   <task_id>.wl file per record into a temp solutions directory,
   then calls RunBenchmark.  Fails (exit 2) if any canonical
   reference impl does not pass its own tests \[LongDash] which
   means the bank's expected_wl disagrees with what the canonical
   actually returns.  Catches bank-edit mistakes (e.g. wrong
   expected output) before they leak into a real model run.

   Usage:
     wolframscript -file scripts/BankSelfTest.wls \
       [--jsonl       challenges.jsonl] \
       [--private     private/canonical_solutions.jsonl] \
       [--out         runs/bank-self-test] \
       [--timeout     30] \
       [--isolation   PerTestKernel] \
       [--fail-under  0.35]

   --fail-under sets a regression gate.  CI fails (exit 2) only when
   the canonical pass rate drops below this threshold.  This lets us
   detect bank-quality REGRESSIONS without making CI permanently red
   on the existing noise.  The current baseline is ~0.39 (60 / 166
   challenges have empty canonicals + a long tail of parse errors and
   value drift in the non-empty ones).  Bank curation to lift this
   number is tracked separately.

   Exit codes:
     0  pass rate >= --fail-under threshold
     1  invalid arguments / missing files
     2  pass rate < --fail-under threshold (regression vs baseline)
     4  unexpected fatal error
*)

$scriptDir = DirectoryName[$InputFileName];
$repoRoot  = DirectoryName[$scriptDir];
$pacletDir = FileNameJoin[{$repoRoot, "WolframChallengesLLMBenchmarkPaclet"}];

Quiet @ PacletDirectoryLoad[$pacletDir];
Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

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

abort[code_, msg_] := (WriteString[$Output, "ERROR: ", msg, "\n"]; Exit[code]);

main[] := Module[
  {args, jsonlPath, privatePath, outDir, timeoutSec, isolation,
   failUnder, data, challenges, testBank, canon, solDir, run, summary,
   failed, passRate},

  args        = parseArgs[Drop[$ScriptCommandLine, 1]];
  jsonlPath   = Lookup[args, "jsonl",
                  FileNameJoin[{$repoRoot, "challenges.jsonl"}]];
  privatePath = Lookup[args, "private",
                  FileNameJoin[{$repoRoot, "private",
                                "canonical_solutions.jsonl"}]];
  outDir      = Lookup[args, "out",
                  FileNameJoin[{$repoRoot, "runs", "bank-self-test"}]];
  timeoutSec  = ToExpression @ Lookup[args, "timeout", "30"];
  isolation   = Lookup[args, "isolation", "PerTestKernel"];
  failUnder   = ToExpression @ Lookup[args, "fail-under", "0.35"];

  If[! FileExistsQ[jsonlPath],
    abort[1, "challenges.jsonl not found: " <> jsonlPath]];
  If[! FileExistsQ[privatePath],
    abort[1, "canonical_solutions.jsonl not found: " <> privatePath <>
            " (this script expects the gitignored private file; run on a host that has it)"]];

  WriteString[$Output, "Loading bank from ",   jsonlPath,   " ...\n"];
  WriteString[$Output, "Loading canonicals from ", privatePath, " ...\n"];
  data = JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL[
    jsonlPath, privatePath];
  If[! AssociationQ[data] || ! KeyExistsQ[data, "canonicalSolutions"],
    abort[4, "LoadChallengesJSONL did not return canonicalSolutions"]];

  challenges = data["challenges"];
  testBank   = data["testBank"];
  canon      = data["canonicalSolutions"];

  WriteString[$Output, "Canonical solutions: ", Length[canon], "\n"];

  (* Materialize each canonical to a temp solutions dir, one .wl file
     per task_id, in the same shape solutions/<model>/<task>.wl uses. *)
  solDir = FileNameJoin[{$TemporaryDirectory,
    "wclb-bank-selftest-" <> ToString[RandomInteger[10^9]]}];
  CreateDirectory[solDir, CreateIntermediateDirectories -> True];
  KeyValueMap[
    Function[{taskId, srcCode},
      Export[FileNameJoin[{solDir, taskId <> ".wl"}],
        srcCode, "Text", CharacterEncoding -> "UTF-8"]
    ],
    canon
  ];

  WriteString[$Output, "Materialized canonicals into ", solDir, "\n"];
  WriteString[$Output, "Running bench: isolation=", isolation,
    " timeout=", timeoutSec, "s\n"];

  (* Convert solDir into the shape RunBenchmark expects (the LoadSolutions
     wrapper). *)
  Module[{solutions},
    solutions = JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions[
      solDir];
    run = JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
      challenges, testBank, solutions,
      "Model"           -> "bank-self-test",
      "OutputDirectory" -> outDir,
      "TimeConstraint"  -> timeoutSec,
      "IsolationMode"   -> isolation,
      (* Canonicals are trusted reference code (we wrote them).  The
         sandbox exists to contain untrusted LLM output \[LongDash]
         applying it here gives false negatives whenever a canonical
         legitimately uses GraphData, Import, etc.  Reference run is
         intentionally unsandboxed. *)
      "Sandbox"         -> False];
  ];

  If[! AssociationQ[run], abort[4, "RunBenchmark did not return an Association"]];

  summary  = run["meta", "summary"];
  failed   = summary["total"] - summary["passed"];
  passRate = summary["passRate"];

  WriteString[$Output, "\n=== Bank self-test ===\n"];
  WriteString[$Output, "  total:      ", summary["total"], "\n"];
  WriteString[$Output, "  passed:     ", summary["passed"], "\n"];
  WriteString[$Output, "  failed:     ", failed, "\n"];
  WriteString[$Output, "  pass rate:  ",
    NumberForm[100. passRate, {4, 2}], "%\n"];
  WriteString[$Output, "  threshold:  ",
    NumberForm[100. failUnder, {4, 2}], "% (--fail-under)\n"];
  WriteString[$Output, "  run dir:    ", run["runDir"], "\n"];

  If[passRate < failUnder,
    WriteString[$Output, "\nFAIL: pass rate ",
      NumberForm[100. passRate, {4, 2}], "% < threshold ",
      NumberForm[100. failUnder, {4, 2}], "%\n"];
    WriteString[$Output,
      "      Bank quality regressed vs. baseline.  Inspect ",
      run["runDir"], "/results.wxf\n"];
    Exit[2]
  ];

  WriteString[$Output, "\nOK \[LongDash] pass rate ",
    NumberForm[100. passRate, {4, 2}], "% \[GreaterEqual] threshold ",
    NumberForm[100. failUnder, {4, 2}], "%.\n"];
  Exit[0]
];

main[]
