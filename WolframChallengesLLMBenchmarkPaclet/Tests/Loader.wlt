(* ::Package:: *)

(* Tests for LoadChallenges + LoadTestBank.

   Wolfram 15.0's VerificationTest no longer populates ActualMessages when
   Messages fire via the regular Message[] pipeline (it silently drops them),
   so this suite verifies message emission manually via $MessageList under
   Internal`InheritedBlock and quiets expected noise with Quiet. *)

Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

(* Wolfram 15.0's VerificationTest silently drops ActualMessages, so
   we capture $MessageList ourselves. $MessageList is Protected. *)
ClearAll[captureMsgs, hasMessageQ];
SetAttributes[captureMsgs, HoldFirst];
captureMsgs[expr_] := Module[{v, msgs},
  Unprotect[$MessageList];
  Block[{$MessageList = {}}, v = expr; msgs = $MessageList];
  Protect[$MessageList];
  {v, msgs}];
SetAttributes[hasMessageQ, HoldRest];
hasMessageQ[msgs_List, msgNameExpr_] := MemberQ[msgs, HoldForm[msgNameExpr]];

(* Per-file temp dir isolates concurrent runs. *)
$tmp = FileNameJoin[{$TemporaryDirectory,
  "wclb-tests-loader-" <> ToString[RandomInteger[10^9]]}];
If[! DirectoryQ[$tmp],
  CreateDirectory[$tmp, CreateIntermediateDirectories -> True]];


(* ---------- LoadChallenges ---------- *)

VerificationTest[
  Module[{p = FileNameJoin[{$tmp, "ok-list.json"}], r},
    Export[p,
      {<|"name" -> "A", "prompt" -> "solve a"|>,
       <|"name" -> "B", "prompt" -> "solve b"|>}, "RawJSON"];
    r = JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges[p];
    {AssociationQ[r], Sort @ Keys[r], r["A"]["prompt"]}
  ],
  {True, {"A", "B"}, "solve a"},
  TestID -> "LoadChallenges/list-of-assoc"
]

VerificationTest[
  Module[{p = FileNameJoin[{$tmp, "ok-assoc.json"}], r},
    Export[p,
      <|"A" -> <|"prompt" -> "solve a"|>,
        "B" -> <|"prompt" -> "solve b"|>|>, "RawJSON"];
    r = JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges[p];
    {AssociationQ[r], Sort @ Keys[r]}
  ],
  {True, {"A", "B"}},
  TestID -> "LoadChallenges/assoc-of-assoc"
]

VerificationTest[
  Module[{v, msgs},
    {v, msgs} = captureMsgs @
      JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges[
        FileNameJoin[{$tmp, "nope-does-not-exist.json"}]];
    {v, hasMessageQ[msgs,
      JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges::notfound]}
  ],
  {$Failed, True},
  {JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges::notfound},
  TestID -> "LoadChallenges/missing-file"
]

VerificationTest[
  Module[{p = FileNameJoin[{$tmp, "bad-shape.json"}], v, msgs},
    Export[p, 42, "RawJSON"];
    {v, msgs} = captureMsgs @
      JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges[p];
    {v, hasMessageQ[msgs,
      JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges::badshape]}
  ],
  {$Failed, True},
  {JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges::badshape},
  TestID -> "LoadChallenges/bad-shape"
]

VerificationTest[
  Module[{v, msgs},
    {v, msgs} = captureMsgs @
      JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges[42];
    {v, hasMessageQ[msgs,
      JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges::badarg]}
  ],
  {$Failed, True},
  {JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges::badarg},
  TestID -> "LoadChallenges/bad-arg"
]


(* ---------- LoadTestBank ---------- *)

VerificationTest[
  Module[{p = FileNameJoin[{$tmp, "ok-tb.wxf"}], r},
    Export[p,
      <|"A" -> {{HoldComplete[f[1]], 1},
                {HoldComplete[f[2]], 4}},
        "B" -> {{HoldComplete[g[]],  "x", <|"sameTest" -> SameQ|>}}|>,
      "WXF"];
    r = JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank[p];
    {AssociationQ[r],
     Length[r["A"]],
     Length[r["B"]],
     r["A"][[1]]["expected"],
     AssociationQ[r["B"][[1]]["metadata"]]}
  ],
  {True, 2, 1, 1, True},
  TestID -> "LoadTestBank/basic-wxf"
]

VerificationTest[
  Module[{v, msgs},
    {v, msgs} = captureMsgs @
      JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank[
        FileNameJoin[{$tmp, "nope.wxf"}]];
    {v, hasMessageQ[msgs,
      JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank::notfound]}
  ],
  {$Failed, True},
  {JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank::notfound},
  TestID -> "LoadTestBank/missing-file"
]

VerificationTest[
  Module[{p = FileNameJoin[{$tmp, "bad-tb-top.wxf"}], v, msgs},
    Export[p, {1, 2, 3}, "WXF"];
    {v, msgs} = captureMsgs @
      JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank[p];
    {v, hasMessageQ[msgs,
      JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank::badshape]}
  ],
  {$Failed, True},
  {JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank::badshape},
  TestID -> "LoadTestBank/bad-shape-top"
]

VerificationTest[
  Module[{v, msgs},
    {v, msgs} = captureMsgs @
      JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank[42];
    {v, hasMessageQ[msgs,
      JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank::badarg]}
  ],
  {$Failed, True},
  {JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank::badarg},
  TestID -> "LoadTestBank/bad-arg"
]


(* ---------- ReconcileNames ---------- *)

(* Happy path: case-, punctuation-, and diacritic-only diffs are bridged
   back to the test bank's canonical keys. *)
VerificationTest[
  Module[{ch, tb, r},
    ch = <|
      "Five-PointConic"     -> <|"name" -> "Five-PointConic", "prompt" -> "p1"|>,
      "VigenèreCipher"      -> <|"name" -> "VigenèreCipher",  "prompt" -> "p2"|>,
      "HowRoundIsaCountry?" -> <|"name" -> "HowRoundIsaCountry?", "prompt" -> "p3"|>,
      "Unmatched"           -> <|"name" -> "Unmatched", "prompt" -> "p4"|>
    |>;
    tb = <|
      "FivePointConic"    -> {<|"challengeName" -> "FivePointConic",
                                "input" -> HoldComplete[f[]],
                                "expected" -> 1, "metadata" -> <||>|>},
      "VigenereCipher"    -> {<|"challengeName" -> "VigenereCipher",
                                "input" -> HoldComplete[f[]],
                                "expected" -> 1, "metadata" -> <||>|>},
      "HowRoundIsACountry"-> {<|"challengeName" -> "HowRoundIsACountry",
                                "input" -> HoldComplete[f[]],
                                "expected" -> 1, "metadata" -> <||>|>},
      "OrphanTbName"      -> {<|"challengeName" -> "OrphanTbName",
                                "input" -> HoldComplete[f[]],
                                "expected" -> 1, "metadata" -> <||>|>}
    |>;
    r = JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames[ch, tb];
    {AssociationQ[r],
     r["summary", "matched"],
     r["summary", "renamed"],
     r["summary", "unmatchedChallenges"],
     r["summary", "unmatchedTestBank"],
     Sort[#["old"] & /@ r["renamed"]],
     r["challenges", "FivePointConic", "prompt"],
     MemberQ[Keys[r["challenges"]], "Unmatched"],
     MemberQ[r["unmatchedInTestBank"], "OrphanTbName"]}
  ],
  {True, 3, 3, 1, 1,
    Sort[{"Five-PointConic", "VigenèreCipher", "HowRoundIsaCountry?"}],
    "p1", True, True},
  TestID -> "ReconcileNames/happy-path"
]

(* Collision: two challenges canonicalize to the same key. Message fires;
   first-wins survives, the duplicate is dropped. *)
VerificationTest[
  Module[{ch, tb, v, msgs},
    ch = <|
      "FooBar" -> <|"name" -> "FooBar", "prompt" -> "first"|>,
      "foobar" -> <|"name" -> "foobar", "prompt" -> "second"|>
    |>;
    tb = <|"FooBar" -> {<|"challengeName" -> "FooBar",
                          "input" -> HoldComplete[f[]],
                          "expected" -> 1, "metadata" -> <||>|>}|>;
    {v, msgs} = captureMsgs @
      JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames[ch, tb];
    {v["summary", "matched"],
     v["challenges", "FooBar", "prompt"],
     Length[Keys[v["challenges"]]],
     hasMessageQ[msgs,
       JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames::collision]}
  ],
  {1, "first", 1, True},
  {JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames::collision},
  TestID -> "ReconcileNames/collision-first-wins"
]

(* Bad-arg: non-assoc inputs emit a tagged message and return $Failed. *)
VerificationTest[
  Module[{v, msgs},
    {v, msgs} = captureMsgs @
      JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames[
        "not-an-assoc", <||>];
    {v, hasMessageQ[msgs,
      JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames::badarg]}
  ],
  {$Failed, True},
  {JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames::badarg},
  TestID -> "ReconcileNames/bad-arg"
]
