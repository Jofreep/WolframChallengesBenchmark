#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ChallengesBenchmark CompareModels CLI.

   Usage (positional run directories — model label taken from each run.json):
     wolframscript -file scripts/CompareModels.wls \
        --out runs/compare \
        runs/run-2026-04-18-abc/run-opus \
        runs/run-2026-04-18-def/run-sonnet

   Usage (Automatic scan of a parent directory, picking up run-* children):
     wolframscript -file scripts/CompareModels.wls \
        --out runs/compare \
        --all runs

   Exits:
     0  compare produced, reports written
     1  invalid arguments
     4  unexpected error
*)

$scriptDir = DirectoryName[$InputFileName];
$repoRoot  = DirectoryName[$scriptDir];
$pacletDir = FileNameJoin[{$repoRoot, "WolframChallengesLLMBenchmarkPaclet"}];

Quiet @ PacletDirectoryLoad[$pacletDir];
Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

main[] := Module[{argv, runDirs, outDir, allDir, cmp, paths},
  argv = Drop[$ScriptCommandLine, 1];

  (* Parse --out outDir and --all parentDir; the rest are positional run dirs. *)
  outDir = "runs/compare";
  allDir = None;
  runDirs = {};

  Module[{i = 1, n = Length[argv], arg},
    While[i <= n,
      arg = argv[[i]];
      Which[
        arg === "--out" && i < n,
          outDir = argv[[i + 1]]; i += 2,
        arg === "--all" && i < n,
          allDir = argv[[i + 1]]; i += 2,
        StringStartsQ[arg, "--"],
          WriteString[$Output, "Unknown flag: ", arg, "\n"]; Exit[1],
        True,
          AppendTo[runDirs, arg]; i++
      ]
    ]
  ];

  cmp = Which[
    allDir =!= None,
      JofreEspigulePons`WolframChallengesBenchmark`CompareModels[allDir, Automatic],
    Length[runDirs] >= 2,
      JofreEspigulePons`WolframChallengesBenchmark`CompareModels[runDirs],
    True,
      WriteString[$Output,
        "ERROR: supply either --all <parentDir> or two or more run directories.\n"];
      Exit[1]
  ];

  If[cmp === $Failed, Exit[4]];

  paths = JofreEspigulePons`WolframChallengesBenchmark`WriteCompareReport[cmp, outDir];
  WriteString[$Output, "Wrote:\n  ", paths["markdown"], "\n  ", paths["html"], "\n"];
  WriteString[$Output, "Models: ", StringRiffle[cmp["models"], ", "], "\n"];
  Scan[
    Function[m,
      Module[{s = cmp["runsByModel", m, "summary"]},
        WriteString[$Output, "  ", m,
          ": ", Lookup[s, "passed", 0], "/", Lookup[s, "total", 0],
          " (", NumberForm[100. Lookup[s, "passRate", 0.], {4, 2}], "%)\n"]
      ]
    ],
    cmp["models"]
  ];
  Exit[0]
];

Module[{},
  Quiet @ Check[main[],
    WriteString[$Output, "FATAL: ", $MessageList, "\n"];
    Exit[4]
  ]
];
