#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Verify the refactored harness against the legacy notebook's results.

   Usage:
     wolframscript -file scripts/VerifyAgainstLegacy.wls \
        --notebook "Wolfram Challenges Benchmark 2026-04-19.nb" \
        --new-run  runs/<runId> \
        [--variable resultsAssoc] \
        [--tolerance 0]

   What it does:
     1. Opens the legacy notebook and extracts its results variable.
     2. Loads the refactored run's results.wxf.
     3. Joins the two on (challengeName, testIndex) and reports:
          - count of tests that agree on passed
          - count that flipped (legacy passed -> new failed, etc.)
          - first N disagreements in full detail

   Exits 0 if the two runs agree to within --tolerance flips, else 2.
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
      i++]
  ]; out];

abort[c_, m_] := (WriteString[$Output, "ERROR: ", m, "\n"]; Exit[c]);

main[] := Module[
  {args, nbPath, runDir, varName, tolerance,
   nb, cells, legacy, newResults, newById, common, flips,
   legacyPassed, newPassed, agree, disagree},

  args      = parseArgs[Drop[$ScriptCommandLine, 1]];
  nbPath    = Lookup[args, "notebook", abort[1, "missing --notebook"]];
  runDir    = Lookup[args, "new-run",  abort[1, "missing --new-run"]];
  varName   = Lookup[args, "variable", "resultsAssoc"];
  tolerance = ToExpression @ Lookup[args, "tolerance", "0"];

  If[! FileExistsQ[nbPath],
    abort[1, "notebook not found: " <> nbPath]];
  If[! FileExistsQ[FileNameJoin[{runDir, "results.wxf"}]],
    abort[1, "new run missing results.wxf: " <> runDir]];

  (* --- load legacy ----------------------------------------------------- *)
  WriteString[$Output, "Loading legacy notebook...\n"];
  nb = Quiet @ Check[Import[nbPath, "NB"], $Failed];
  If[nb === $Failed, abort[1, "could not import notebook"]];
  cells = Cases[nb, Cell[CellGroupData[_, _] | _, "Input", ___] :> #, Infinity];

  Block[{$Context = "VerifyScratch`",
         $ContextPath = Prepend[$ContextPath, "VerifyScratch`"]},
    Scan[Function[cell,
      Module[{src = ToString[cell[[1]], InputForm]},
        Quiet @ Check[ToExpression[src], Null]]], cells];
    legacy = Symbol["VerifyScratch`" <> varName];
  ];
  If[! AssociationQ[legacy],
    abort[2, "legacy variable " <> varName <> " was not populated"]];

  WriteString[$Output, "Legacy has ", Length[legacy], " entries\n"];

  (* --- load new -------------------------------------------------------- *)
  WriteString[$Output, "Loading new run from ", runDir, "...\n"];
  newResults = Import[FileNameJoin[{runDir, "results.wxf"}], "WXF"];
  newById = Association @ Map[
    (#["challengeName"] <> "/" <> IntegerString[#["testIndex"]]) -> # &,
    newResults];

  WriteString[$Output, "New run has ", Length[newResults], " entries\n"];

  (* --- join ------------------------------------------------------------ *)
  (* Legacy is assumed to be keyed by challengeName with a list-of-booleans
     value; adapt here if the actual shape differs. *)

  legacyPassed = Flatten @ KeyValueMap[
    Function[{chName, val},
      Which[
        (* shape: list of Booleans *)
        VectorQ[val, BooleanQ],
          MapIndexed[{chName <> "/" <> IntegerString[First[#2]], #1} &, val],
        (* shape: Association with testIndex -> Boolean *)
        AssociationQ[val] && AllTrue[Values[val], BooleanQ],
          KeyValueMap[{chName <> "/" <> ToString[#1], #2} &, val],
        True, {}
      ]
    ],
    legacy
  ];

  legacyPassed = Association @ Map[First[#] -> Last[#] &, legacyPassed];
  common = Intersection[Keys[legacyPassed], Keys[newById]];

  agree = disagree = 0;
  flips = Reap[
    Scan[
      Function[id,
        Module[{lp = legacyPassed[id], np = newById[id]["passed"]},
          If[lp === np, agree++,
            disagree++;
            Sow[<|"testId" -> id, "legacyPassed" -> lp,
              "newPassed" -> np, "newStatus" -> newById[id]["status"],
              "newError" -> newById[id]["error"]|>]
          ]
        ]
      ],
      common
    ]
  ][[2]];
  flips = If[Length[flips] > 0, First[flips], {}];

  WriteString[$Output, "\n=== comparison ===\n"];
  WriteString[$Output, "  common testIds : ", Length[common], "\n"];
  WriteString[$Output, "  agree          : ", agree, "\n"];
  WriteString[$Output, "  disagree       : ", disagree, "\n"];

  If[disagree > 0,
    WriteString[$Output, "\nFirst 10 disagreements:\n"];
    Scan[
      WriteString[$Output, "  ", #["testId"],
        ": legacy=", #["legacyPassed"], " new=", #["newPassed"],
        " (status=", #["newStatus"], ")\n"] &,
      Take[flips, UpTo[10]]
    ]
  ];

  If[disagree > tolerance,
    WriteString[$Output, "\nFAIL: ", disagree, " disagreement(s) > tolerance ",
      tolerance, "\n"];
    Exit[2]
  ];

  WriteString[$Output, "\nOK: runs agree within tolerance\n"];
  Exit[0]
];

main[];
