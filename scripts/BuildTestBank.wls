#!/usr/bin/env wolframscript
(* ::Package:: *)

(* BuildTestBank CLI — compile a directory of plain-text .wlchallenge
   authoring files into the legacy ChallengesTestDataV1.json /
   ChallengesTests.wxf pair consumed by the runtime, or reverse the
   process to seed a .wlchallenge directory from an existing bank.

   See docs/WLCHALLENGE-FORMAT.md for the file format.

   Forward (build) mode — read .wlchallenge files, emit JSON + WXF:
     wolframscript -file scripts/BuildTestBank.wls \
        --in   .challenges \
        --json ChallengesTestDataV1.json \
        --wxf  ChallengesTests.wxf

   Reverse (seed) mode — read an existing bank, emit .wlchallenge files:
     wolframscript -file scripts/BuildTestBank.wls \
        --reverse \
        --json ChallengesTestDataV1.json \
        --wxf  ChallengesTests.wxf \
        --out  .challenges

   Exit codes:
     0  success
     1  invalid arguments / missing inputs
     2  parse / validation failure (at least one .wlchallenge is unreadable,
        or at least one round-trip sanity check failed)
     4  unexpected error
*)

(* ----------------------------------------------------------------------- *)
(* Argv parsing                                                            *)
(* ----------------------------------------------------------------------- *)

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

withDefault[args_, key_, default_] := Lookup[args, key, default];

abort[code_Integer, msg_String] := (
  WriteString[$Output, "ERROR: ", msg, "\n"];
  Exit[code]
);

requireKey[args_, key_] := If[KeyExistsQ[args, key],
  args[key], abort[1, "missing required --" <> key]
];

truthy[s_String] := ! MemberQ[{"false", "False", "0", "off", "no"}, s];
truthy[_]        := False;

(* ----------------------------------------------------------------------- *)
(* Boot the paclet                                                         *)
(* ----------------------------------------------------------------------- *)

$scriptDir = DirectoryName[$InputFileName];
$repoRoot  = DirectoryName[$scriptDir];
$pacletDir = FileNameJoin[{$repoRoot, "WolframChallengesLLMBenchmarkPaclet"}];

Quiet @ PacletDirectoryLoad[$pacletDir];
Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

(* ----------------------------------------------------------------------- *)
(* Forward mode                                                            *)
(* ----------------------------------------------------------------------- *)

runForward[args_Association] := Module[
  {inDir, jsonOut, wxfOut, quiet, verify, res, built, challenges, bank,
   perChallenge, names, reloaded, reloadedBank, parity},

  inDir   = requireKey[args, "in"];
  jsonOut = requireKey[args, "json"];
  wxfOut  = requireKey[args, "wxf"];
  quiet   = KeyExistsQ[args, "quiet"] && truthy[args["quiet"]];
  verify  = ! (KeyExistsQ[args, "no-verify"] && truthy[args["no-verify"]]);

  If[! DirectoryQ[inDir], abort[1, "input directory not found: " <> inDir]];

  If[! quiet,
    WriteString[$Output,
      "Building test bank from ", inDir, "\n",
      "  json: ", jsonOut, "\n",
      "  wxf:  ", wxfOut, "\n"]
  ];

  (* Pre-parse so we can report per-challenge counts and catch failures
     before we touch the output files. *)
  built = Quiet @ Check[
    JofreEspigulePons`WolframChallengesBenchmark`BuildTestBank[inDir],
    $Failed
  ];
  If[built === $Failed || ! MatchQ[built, {_Association, _Association}],
    abort[2, "BuildTestBank failed parsing " <> inDir]
  ];
  {challenges, bank} = built;

  If[Length[challenges] === 0,
    abort[2, "no .wlchallenge files parsed successfully from " <> inDir]
  ];

  names = Keys[bank];
  If[! quiet,
    WriteString[$Output, "Parsed ", Length[names], " challenges:\n"];
    Scan[
      Function[name,
        perChallenge = Length @ Lookup[bank, name, {}];
        WriteString[$Output,
          "  ", StringPadRight[name, 32], " ",
          ToString[perChallenge], " test(s)\n"]
      ],
      names
    ]
  ];

  (* Now emit. WriteTestBankFiles re-reads the directory; we pass through
     its return so the summary below reflects what actually hit disk. *)
  res = Quiet @ Check[
    JofreEspigulePons`WolframChallengesBenchmark`WriteTestBankFiles[inDir, jsonOut, wxfOut],
    $Failed
  ];
  If[res === $Failed || ! AssociationQ[res],
    abort[2, "WriteTestBankFiles failed writing " <> jsonOut <> " / " <> wxfOut]
  ];

  If[! quiet,
    WriteString[$Output,
      "\nWrote:\n",
      "  ", res["json"], " (", FileByteCount[res["json"]], " bytes)\n",
      "  ", res["wxf"],  " (", FileByteCount[res["wxf"]],  " bytes)\n",
      "Totals: ", res["challenges"], " challenges, ",
      res["tests"], " tests.\n"]
  ];

  If[verify,
    (* Sanity check: re-import the emitted files and confirm the runtime
       shape matches what we just built. This is cheap and catches
       exporter regressions immediately. *)
    reloaded     = Quiet @ JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges[jsonOut];
    reloadedBank = Quiet @ JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank[wxfOut];
    parity = <|
      "challengeCountMatch" ->
        reloaded =!= $Failed && Length[reloaded]     === Length[challenges],
      "bankCountMatch"      ->
        reloadedBank =!= $Failed && Length[reloadedBank] === Length[bank],
      "testTotalMatch"      ->
        reloadedBank =!= $Failed &&
        Total[Length /@ Values[reloadedBank]] ===
          Total[Length /@ Values[bank]]
    |>;
    If[! AllTrue[Values[parity], TrueQ],
      WriteString[$Output,
        "FAIL: post-write verification mismatch:\n",
        "  challengeCountMatch: ", parity["challengeCountMatch"], "\n",
        "  bankCountMatch:      ", parity["bankCountMatch"],      "\n",
        "  testTotalMatch:      ", parity["testTotalMatch"],      "\n"];
      Exit[2]
    ];
    If[! quiet,
      WriteString[$Output, "Post-write verification OK.\n"]
    ]
  ];

  Exit[0]
];

(* ----------------------------------------------------------------------- *)
(* Reverse mode                                                            *)
(* ----------------------------------------------------------------------- *)

runReverse[args_Association] := Module[
  {jsonIn, wxfIn, outDir, quiet, challenges, bank, paths},

  jsonIn = requireKey[args, "json"];
  wxfIn  = requireKey[args, "wxf"];
  outDir = requireKey[args, "out"];
  quiet  = KeyExistsQ[args, "quiet"] && truthy[args["quiet"]];

  If[! FileExistsQ[jsonIn], abort[1, "challenges JSON not found: " <> jsonIn]];
  If[! FileExistsQ[wxfIn],  abort[1, "test bank WXF not found: "   <> wxfIn]];

  If[! quiet,
    WriteString[$Output,
      "Seeding .wlchallenge directory from:\n",
      "  json: ", jsonIn, "\n",
      "  wxf:  ", wxfIn,  "\n",
      "  out:  ", outDir, "\n"]
  ];

  challenges = JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges[jsonIn];
  If[challenges === $Failed, abort[2, "could not load " <> jsonIn]];

  bank = JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank[wxfIn];
  If[bank === $Failed, abort[2, "could not load " <> wxfIn]];

  paths = Quiet @ Check[
    JofreEspigulePons`WolframChallengesBenchmark`WriteWLChallengeDir[challenges, bank, outDir],
    $Failed
  ];
  If[paths === $Failed || ! ListQ[paths],
    abort[2, "WriteWLChallengeDir failed"]
  ];

  If[! quiet,
    WriteString[$Output, "Wrote ", Length[paths], " file(s) to ", outDir, "\n"]
  ];

  Exit[0]
];

(* ----------------------------------------------------------------------- *)
(* Main                                                                    *)
(* ----------------------------------------------------------------------- *)

main[] := Module[{args},
  args = parseArgs[Drop[$ScriptCommandLine, 1]];

  If[KeyExistsQ[args, "help"] || KeyExistsQ[args, "h"],
    WriteString[$Output,
      "Usage:\n",
      "  build:   --in DIR --json OUT.json --wxf OUT.wxf [--no-verify] [--quiet]\n",
      "  reverse: --reverse --json IN.json --wxf IN.wxf --out DIR [--quiet]\n"];
    Exit[0]
  ];

  If[KeyExistsQ[args, "reverse"] && truthy[args["reverse"]],
    runReverse[args],
    runForward[args]
  ]
];

(* Top-level safety net *)
Module[{},
  Quiet @ Check[main[],
    WriteString[$Output, "FATAL: ", $MessageList, "\n"];
    Exit[4]
  ]
];
