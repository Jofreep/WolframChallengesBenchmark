#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Migrate a legacy notebook's `solutionsAssoc` into the
   solutions/<model>/<challenge>.wl on-disk layout used by the refactored
   benchmark.

   Usage:
     wolframscript -file scripts/MigrateFromNotebook.wls \
        --notebook "Wolfram Challenges Benchmark 2026-04-19.nb" \
        --model    claude-opus-4.6 \
        --out      solutions \
        [--variable solutionsAssoc] \
        [--dry-run]

   The script opens the notebook, evaluates every Input cell in order so
   that the target variable is populated, then writes each entry to disk
   as `<out>/<model>/<challengeName>.wl` with a `.meta.json` sidecar.

   Exits:
     0  migration succeeded
     1  invalid arguments or missing notebook
     2  target variable was not populated by the notebook
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
      v = If[i < n && ! StringStartsQ[args[[i + 1]], "--"], args[[i + 1]], "true"];
      out[StringDrop[k, 2]] = v;
      i += If[v === "true", 1, 2],
      i++
    ]
  ];
  out
];

abort[code_Integer, msg_String] := (
  WriteString[$Output, "ERROR: ", msg, "\n"]; Exit[code]
);

main[] := Module[
  {args, nbPath, model, outDir, varName, dryRun,
   nb, cells, count, expr, assoc, payload, perChallenge, written = 0},

  args = parseArgs[Drop[$ScriptCommandLine, 1]];

  nbPath  = Lookup[args, "notebook",  abort[1, "missing --notebook"]];
  model   = Lookup[args, "model",     abort[1, "missing --model"]];
  outDir  = Lookup[args, "out",       FileNameJoin[{Directory[], "solutions"}]];
  varName = Lookup[args, "variable",  "solutionsAssoc"];
  dryRun  = TrueQ @ ToExpression @ Lookup[args, "dry-run", "False"];

  If[! FileExistsQ[nbPath], abort[1, "notebook not found: " <> nbPath]];

  WriteString[$Output, "Opening notebook: ", nbPath, "\n"];
  nb = Quiet @ Check[Import[nbPath, "NB"], $Failed];
  If[nb === $Failed, abort[1, "could not import notebook"]];

  (* Extract all Input cells, evaluate them in a scratch kernel-local
     context so the target variable ends up populated. We deliberately
     do not use NotebookEvaluate — it triggers file-local side effects
     and is fragile in scripts. Instead we parse cell contents as WL. *)

  cells = Cases[nb, Cell[CellGroupData[_, _] | _, "Input", ___] :> #, Infinity];
  count = Length[cells];
  WriteString[$Output, "Found ", count, " Input cells. Evaluating...\n"];

  Block[{$Context = "MigrationScratch`",
         $ContextPath = Prepend[$ContextPath, "MigrationScratch`"]},
    Scan[
      Function[cell,
        Module[{src = ToString[cell[[1]], InputForm]},
          Quiet @ Check[ToExpression[src], Null]
        ]
      ],
      cells
    ];

    expr = Symbol["MigrationScratch`" <> varName];
    If[! AssociationQ[expr],
      abort[2, "variable " <> varName <> " was not populated as an Association " <>
              "after evaluating the notebook"]
    ];
    assoc = expr;
  ];

  WriteString[$Output, "Extracted ", Length[assoc], " solutions for model ", model, "\n"];

  If[dryRun,
    WriteString[$Output, "Dry run — not writing files.\n"];
    WriteString[$Output, "Would write:\n"];
    Scan[WriteString[$Output, "  ", outDir, "/", model, "/", #, ".wl\n"] &,
      Keys[assoc]];
    Exit[0]
  ];

  (* Each value in solutionsAssoc may be either:
       - a bare String (the raw WL source)
       - an Association with a "code" key
       - the full LLM response text (to be run through ExtractCode first) *)

  Do[
    Module[{name = Keys[assoc][[i]], val = Values[assoc][[i]], code},
      code = Which[
        StringQ[val],                    JofreEspigulePons`WolframChallengesBenchmark`ExtractCode[val],
        AssociationQ[val] && StringQ[val["code"]], val["code"],
        AssociationQ[val] && StringQ[val["response"]],
                                          JofreEspigulePons`WolframChallengesBenchmark`ExtractCode[val["response"]],
        True, ""
      ];
      If[code =!= "",
        JofreEspigulePons`WolframChallengesBenchmark`SaveSolution[
          FileNameJoin[{outDir, model}], name, code];
        written++
      ]
    ],
    {i, Length[assoc]}
  ];

  WriteString[$Output, "Wrote ", written, " / ", Length[assoc],
    " solutions under ", FileNameJoin[{outDir, model}], "\n"];

  Exit[0]
];

main[];
