(* ::Package:: *)

(* Tests for TestBankBuilder.wl: LoadWLChallenge / LoadChallengesDir /
   BuildTestBank / WriteTestBankFiles / WriteWLChallengeDir.

   The .wlchallenge format is line-oriented and held-parsed; these tests
   exercise the round-trip from authoring directory -> challenges +
   testBank Associations -> serialized JSON/WXF -> back through
   LoadChallenges/LoadTestBank. *)

Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

$tmp = FileNameJoin[{$TemporaryDirectory,
  "wclb-tests-tbb-" <> ToString[RandomInteger[10^9]]}];
If[! DirectoryQ[$tmp],
  CreateDirectory[$tmp, CreateIntermediateDirectories -> True]];

(* A small authoring directory.  Two challenges; Sum has metadata on its
   second test to exercise the 3-tuple branch.  Order on disk is
   deliberately reversed against :Index: so the SortBy-by-index path is
   exercised. *)

$authorDir = FileNameJoin[{$tmp, "authoring"}];
CreateDirectory[$authorDir, CreateIntermediateDirectories -> True];

(* Square: index 2, two simple tests. *)
Export[FileNameJoin[{$authorDir, "square.wlchallenge"}],
  StringJoin[
    "(* :Name: Square *)\n",
    "(* :Index: 2 *)\n",
    "(* :Instruction: Squaring *)\n",
    "(* :Prompt: *)\n",
    "Define square that returns x^2.\n",
    "\n",
    "(* :Tests: *)\n",
    "{square[3], 9}\n",
    "{square[5], 25}\n"
  ],
  "Text", CharacterEncoding -> "UTF-8"
];

(* Sum: index 1, three tests; second test has a metadata Association. *)
Export[FileNameJoin[{$authorDir, "sum.wlchallenge"}],
  StringJoin[
    "(* :Name: Sum *)\n",
    "(* :Index: 1 *)\n",
    "(* :Instruction: Adding *)\n",
    "(* :Prompt: *)\n",
    "Define addTwo[a,b] returning a+b.\n",
    "\n",
    "(* :Tests: *)\n",
    "{addTwo[1, 2], 3}\n",
    "{addTwo[10, 20], 30, <|\"tag\" -> \"big\"|>}\n",
    "{addTwo[0, 0], 0}\n"
  ],
  "Text", CharacterEncoding -> "UTF-8"
];


(* ---------- LoadWLChallenge: parses a single file ---------- *)

VerificationTest[
  Module[{e},
    e = JofreEspigulePons`WolframChallengesBenchmark`LoadWLChallenge[
      FileNameJoin[{$authorDir, "sum.wlchallenge"}]];
    {e["name"], e["index"], e["instruction"],
     StringContainsQ[e["prompt"], "addTwo"],
     Length[e["tests"]],
     (* First held input must remain HoldComplete[addTwo[1,2]] *)
     Head[e["tests"][[1, 1]]],
     e["tests"][[1, 2]],
     (* Metadata on the second test should round-trip as an Association. *)
     Length[e["tests"][[2]]],
     e["tests"][[2, 3]]}
  ],
  {"Sum", 1, "Adding", True, 3, HoldComplete, 3, 3, <|"tag" -> "big"|>},
  TestID -> "LoadWLChallenge/parses-single-file"
]


(* ---------- LoadWLChallenge: missing file errors cleanly ---------- *)

VerificationTest[
  JofreEspigulePons`WolframChallengesBenchmark`LoadWLChallenge[
    FileNameJoin[{$tmp, "definitely-not-here.wlchallenge"}]],
  $Failed,
  {JofreEspigulePons`WolframChallengesBenchmark`LoadWLChallenge::notfound},
  TestID -> "LoadWLChallenge/missing-file"
]


(* ---------- LoadWLChallenge: missing required section errors ---------- *)

VerificationTest[
  Module[{p},
    p = FileNameJoin[{$tmp, "no-prompt.wlchallenge"}];
    Export[p, "(* :Name: NoPrompt *)\n(* :Tests: *)\n{1, 1}\n",
      "Text", CharacterEncoding -> "UTF-8"];
    JofreEspigulePons`WolframChallengesBenchmark`LoadWLChallenge[p]
  ],
  $Failed,
  {JofreEspigulePons`WolframChallengesBenchmark`LoadWLChallenge::missing},
  TestID -> "LoadWLChallenge/missing-required-section"
]


(* ---------- LoadChallengesDir: returns Association sorted by :Index: -- *)

VerificationTest[
  Module[{loaded},
    loaded = JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesDir[
      $authorDir];
    {AssociationQ[loaded], Keys[loaded]}
  ],
  {True, {"Sum", "Square"}},   (* :Index: 1 then 2 *)
  TestID -> "LoadChallengesDir/sorted-by-index"
]


(* ---------- LoadChallengesDir: missing dir returns empty <||> --------- *)

VerificationTest[
  Quiet @ JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesDir[
    FileNameJoin[{$tmp, "no-such-author-dir"}]],
  <||>,
  TestID -> "LoadChallengesDir/missing-dir-empty"
]


(* ---------- BuildTestBank: returns {challengesAssoc, testBankAssoc} --- *)

VerificationTest[
  Module[{built, ch, tb},
    built = JofreEspigulePons`WolframChallengesBenchmark`BuildTestBank[
      $authorDir];
    {ch, tb} = built;
    {Length[built],
     Sort @ Keys[ch],
     ch["Sum", "index"],
     ch["Sum", "prompt"] // (StringContainsQ[#, "addTwo"] &),
     Length[tb["Sum"]],
     tb["Sum"][[1, "challengeName"]],
     tb["Sum"][[1, "expected"]],
     tb["Sum"][[2, "metadata"]]}
  ],
  {2, {"Square", "Sum"}, 1, True, 3, "Sum", 3, <|"tag" -> "big"|>},
  TestID -> "BuildTestBank/full-shape"
]


(* ---------- BuildTestBank: empty dir -> {<||>, <||>} ---------------- *)

VerificationTest[
  Module[{empty},
    empty = FileNameJoin[{$tmp, "empty-author-dir"}];
    CreateDirectory[empty, CreateIntermediateDirectories -> True];
    JofreEspigulePons`WolframChallengesBenchmark`BuildTestBank[empty]
  ],
  {<||>, <||>},
  TestID -> "BuildTestBank/empty-dir"
]


(* ---------- WriteTestBankFiles: emits json+wxf, summary correct ----- *)
(* And the emitted files round-trip through the public LoadChallenges /
   LoadTestBank loaders, which is the contract downstream consumers
   depend on. *)

VerificationTest[
  Module[{outDir, jsonOut, wxfOut, summary, ch, tb},
    outDir  = FileNameJoin[{$tmp, "build-out"}];
    CreateDirectory[outDir, CreateIntermediateDirectories -> True];
    jsonOut = FileNameJoin[{outDir, "challenges.json"}];
    wxfOut  = FileNameJoin[{outDir, "testbank.wxf"}];
    summary = JofreEspigulePons`WolframChallengesBenchmark`WriteTestBankFiles[
      $authorDir, jsonOut, wxfOut];
    ch = JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges[jsonOut];
    tb = JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank[wxfOut];
    {summary["challenges"],
     summary["tests"],
     FileExistsQ[summary["json"]],
     FileExistsQ[summary["wxf"]],
     AssociationQ[ch],
     AssociationQ[tb],
     Length[tb["Sum"]],
     Length[tb["Square"]]}
  ],
  {2, 5, True, True, True, True, 3, 2},
  TestID -> "WriteTestBankFiles/round-trips-via-public-loaders"
]


(* ---------- WriteWLChallengeDir: bank + challenges -> .wlchallenge --- *)
(* Round-trip: take the just-built bank, emit .wlchallenge files into a
   fresh directory, then re-load them and confirm everything survives.   *)

VerificationTest[
  Module[{built, ch, tb, seedDir, paths, reBuilt, reCh, reTb},
    built = JofreEspigulePons`WolframChallengesBenchmark`BuildTestBank[
      $authorDir];
    {ch, tb} = built;
    seedDir = FileNameJoin[{$tmp, "seeded-from-bank"}];
    paths = JofreEspigulePons`WolframChallengesBenchmark`WriteWLChallengeDir[
      ch, tb, seedDir];
    (* Each emitted file should re-parse via LoadChallengesDir into the
       same set of challenge keys. *)
    reBuilt = JofreEspigulePons`WolframChallengesBenchmark`BuildTestBank[
      seedDir];
    {reCh, reTb} = reBuilt;
    {Length[paths],
     AllTrue[paths, FileExistsQ],
     Sort @ Keys[reCh],
     Sort @ Keys[reTb],
     Length[reTb["Sum"]],
     Length[reTb["Square"]],
     reTb["Sum"][[2, "metadata"]]}
  ],
  {2, True, {"Square", "Sum"}, {"Square", "Sum"}, 3, 2, <|"tag" -> "big"|>},
  TestID -> "WriteWLChallengeDir/round-trip"
]


(* ---------- BuildChallengesJSONL: full pipeline -------------------- *)
(* End-to-end: read .wlchallenge files from $authorDir -> emit
   challenges.jsonl -> re-load via LoadChallengesJSONL -> verify the
   bank shape and contents survive the JSONL round-trip.  This is the
   one-step replacement for the legacy BuildTestBank \[Rule]
   MigrateToJSONL pipeline. *)

VerificationTest[
  Module[{outPath, summary, data, sumTests},
    outPath = FileNameJoin[{$tmp, "build-jsonl.jsonl"}];
    summary = JofreEspigulePons`WolframChallengesBenchmark`BuildChallengesJSONL[
      $authorDir, outPath];
    {summary["challenges"],
     summary["tests"],
     FileExistsQ[outPath],
     summary["jsonl"] === outPath}
  ],
  {2, 5, True, True},
  TestID -> "BuildChallengesJSONL/writes-jsonl"
]


(* ---------- BuildChallengesJSONL: round-trip via LoadChallengesJSONL --- *)

VerificationTest[
  Module[{outPath, data, sumTests},
    outPath = FileNameJoin[{$tmp, "build-jsonl-roundtrip.jsonl"}];
    JofreEspigulePons`WolframChallengesBenchmark`BuildChallengesJSONL[
      $authorDir, outPath];
    data = JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL[
      outPath];
    sumTests = data["testBank", "Sum"];
    {Sort @ Keys[data["challenges"]],
     data["challenges", "Sum", "entry_point"],
     data["challenges", "Square", "entry_point"],
     (* The held input must round-trip back as HoldComplete[expr]. *)
     Head[sumTests[[1, "input"]]],
     sumTests[[1, "expected"]],
     (* Metadata on Sum's second test should survive the round-trip. *)
     sumTests[[2, "metadata"]]}
  ],
  {{"Square", "Sum"}, "addTwo", "square",
   HoldComplete, 3, <|"tag" -> "big"|>},
  TestID -> "BuildChallengesJSONL/round-trip-via-LoadChallengesJSONL"
]


(* ---------- BuildChallengesJSONL: empty dir returns $Failed ---------- *)

VerificationTest[
  Module[{emptyDir, outPath},
    emptyDir = FileNameJoin[{$tmp, "empty-author-dir-jsonl"}];
    CreateDirectory[emptyDir, CreateIntermediateDirectories -> True];
    outPath = FileNameJoin[{$tmp, "should-not-be-written.jsonl"}];
    Quiet @ JofreEspigulePons`WolframChallengesBenchmark`BuildChallengesJSONL[
      emptyDir, outPath]
  ],
  $Failed,
  TestID -> "BuildChallengesJSONL/empty-dir-fails"
]
