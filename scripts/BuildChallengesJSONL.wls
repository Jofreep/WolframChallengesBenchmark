#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Build challenges.jsonl from a directory of .wlchallenge authoring
   files in one step. Replaces the legacy two-step
   BuildTestBank \[Rule] MigrateToJSONL pipeline.

   Usage:
     wolframscript -file scripts/BuildChallengesJSONL.wls \
        --in    .challenges \
        [--out  challenges.jsonl] \
        [--quiet]

   Defaults:
     --in   .challenges
     --out  challenges.jsonl

   Exit codes:
     0  build succeeded
     1  invalid arguments / missing inputs
     2  parse / validation failure (at least one .wlchallenge unreadable)
     4  unexpected error
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

main[] := Module[{args, inDir, outPath, quiet, summary},
  args    = parseArgs[Drop[$ScriptCommandLine, 1]];
  inDir   = Lookup[args, "in",  ".challenges"];
  outPath = Lookup[args, "out", "challenges.jsonl"];
  quiet   = KeyExistsQ[args, "quiet"];

  If[! DirectoryQ[inDir],
    abort[1, "input directory not found: " <> inDir]];

  If[! quiet,
    WriteString[$Output, "Reading .wlchallenge files from ", inDir, " ...\n"]];

  summary = JofreEspigulePons`WolframChallengesBenchmark`BuildChallengesJSONL[
    inDir, outPath];

  If[summary === $Failed,
    abort[2, "BuildChallengesJSONL returned $Failed (no .wlchallenge files, or parse error)"]];

  WriteString[$Output,
    "Wrote ", summary["jsonl"], "\n",
    "  challenges: ", summary["challenges"], "\n",
    "  tests:      ", summary["tests"],      "\n"];
  Exit[0]
];

main[]
