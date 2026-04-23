(* ::Package:: *)

(* :Context: ChallengesBenchmark`Private` *)
(* :Summary: Schema-validating loaders for the challenge prompts and test bank. *)

Begin["ChallengesBenchmark`Private`"];

(* ----------------------------------------------------------------------- *)
(* Schema                                                                  *)
(* ----------------------------------------------------------------------- *)

(* The challenge prompts file is a JSON list of objects with at least:
     "instruction" -> String
     "input"       -> String (the challenge text)
   Some entries carry an extra tuple in position 2 (the notebook's
   challengesDataset[[All, 2, 2]] slicing); we normalise that out here. *)

(* The test bank WXF is an Association:
     <| challengeName -> { {HoldComplete[input], expectedOutput}, ... }, ... |>
*)

LoadChallenges::notfound = "Challenges file not found: `1`.";
LoadChallenges::badshape = "Challenges file has an unexpected shape.";
LoadChallenges::parseerr = "Could not parse challenges JSON: `1`.";

loadChallengesImpl[path_String] := Module[{raw, items, normalized},
  If[! FileExistsQ[path],
    Message[LoadChallenges::notfound, path]; Return[$Failed]
  ];
  raw = Quiet @ Check[Import[path, "RawJSON"], $Failed];
  If[raw === $Failed,
    Message[LoadChallenges::parseerr, path]; Return[$Failed]
  ];

  items = Which[
    (* already list of records *)
    ListQ[raw] && AllTrue[raw, AssociationQ], raw,
    (* association-of-records *)
    AssociationQ[raw], Values[raw],
    True, $Failed
  ];

  If[items === $Failed,
    Message[LoadChallenges::badshape]; Return[$Failed]
  ];

  normalized = MapIndexed[
    Module[{rec = #1, idx = First[#2], name, prompt},
      prompt = Lookup[rec, "input",
        Lookup[rec, "prompt",
          Lookup[rec, "challenge", Missing["NoPrompt"]]]];
      (* Heuristic: derive challenge name from the first line of the prompt
         ("Challenge Name\n<body>") if no explicit name field is present. *)
      name = Lookup[rec, "name",
        Lookup[rec, "challengeName",
          If[StringQ[prompt],
            StringReplace[First @ StringSplit[prompt, "\n"],
              RegularExpression["\\s+"] -> ""],
            "Challenge" <> IntegerString[idx, 10, 4]
          ]
        ]];
      name -> <|
        "index"       -> idx,
        "name"        -> name,
        "instruction" -> Lookup[rec, "instruction", ""],
        "prompt"      -> If[StringQ[prompt], prompt, ""]
      |>
    ] &, items
  ];

  Association[normalized]
];

(* ----------------------------------------------------------------------- *)
(* Test bank                                                               *)
(* ----------------------------------------------------------------------- *)

LoadTestBank::notfound = "Test bank file not found: `1`.";
LoadTestBank::badshape = "Test bank entry for `1` has an unexpected shape.";
LoadTestBank::parseerr = "Could not import test bank: `1`.";

SetAttributes[loadTestBankImpl, HoldAll];
(* NB: expected outputs in the WXF may contain unevaluated expressions
   that we must not evaluate here — Import with the "WXF" format returns
   them as plain data; there is nothing to Hold on this entry point. *)

loadTestBankImpl[path_String] := Module[{raw, validated},
  If[! FileExistsQ[path],
    Message[LoadTestBank::notfound, path]; Return[$Failed]
  ];
  raw = Quiet @ Check[Import[path, "WXF"], $Failed];
  If[raw === $Failed,
    Message[LoadTestBank::parseerr, path]; Return[$Failed]
  ];
  If[! AssociationQ[raw],
    Message[LoadTestBank::badshape, "<top>"]; Return[$Failed]
  ];

  validated = KeyValueMap[
    Function[{name, entries},
      If[! ListQ[entries] || ! AllTrue[entries, validTestEntryQ],
        Message[LoadTestBank::badshape, name];
        Nothing,
        name -> MapIndexed[
          <|
            "challengeName" -> name,
            "testIndex"     -> First[#2],
            "input"         -> First[#1],   (* HoldComplete[input] *)
            "expected"      -> Last[#1],
            "metadata"      -> Replace[#1, {_, _, m_Association} :> m, {0}]
          |> &,
          entries
        ]
      ]
    ],
    raw
  ];

  Association[validated]
];

(* A valid entry is a pair {HoldComplete[input], expected} or a triple
   with an Association of per-test metadata in position 3. *)
validTestEntryQ[entry_List] :=
  MatchQ[entry,
    {_HoldComplete, _} |
    {_HoldComplete, _, _Association}
  ];
validTestEntryQ[_] := False;

End[];
