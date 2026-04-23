(* ::Package:: *)

(* :Context: JofreEspigulePons`WolframChallengesBenchmark`Private`          *)
(* :Summary:
     Schema-validating loaders for the challenge prompts and the
     expected-output test bank.  All loaders return $Failed and emit
     a public-symbol-tagged Message on any shape or parse error; they
     never throw.
*)

Begin["JofreEspigulePons`WolframChallengesBenchmark`Private`"];


(* ------------------------------------------------------------------ *)
(* Messages (attached to the PUBLIC symbols)                          *)
(* ------------------------------------------------------------------ *)

JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges::notfound =
  "Challenges file not found: `1`.";
JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges::badshape =
  "Challenges file has an unexpected shape.";
JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges::parseerr =
  "Could not parse challenges JSON: `1`.";
JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges::badarg =
  "LoadChallenges expects a file path string; got `1`.";

JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank::notfound =
  "Test bank file not found: `1`.";
JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank::badshape =
  "Test bank entry for `1` has an unexpected shape.";
JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank::parseerr =
  "Could not import test bank: `1`.";
JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank::badarg =
  "LoadTestBank expects a file path string; got `1`.";

JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames::badarg =
  "ReconcileNames expects two Associations; got `1`, `2`.";
JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames::collision =
  "Multiple challenge names canonicalize to `1` (e.g. `2`); keeping the \
first one and dropping the rest.";
JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames::tbcollision =
  "Multiple test-bank names canonicalize to `1` (e.g. `2`); keeping the \
first one and dropping the rest.";


(* ------------------------------------------------------------------ *)
(* Name canonicalization                                               *)
(*                                                                     *)
(* canonicalNameKey strips diacritics (NFKD), drops every non-ASCII-   *)
(* alphanumeric character, and lowercases.  Two names are considered  *)
(* "the same challenge" iff their canonical forms are equal.          *)
(*                                                                     *)
(* Examples:                                                           *)
(*   "Five-PointConic"        -> "fivepointconic"                     *)
(*   "FivePointConic"         -> "fivepointconic"                     *)
(*   "VigenèreCipher"         -> "vigenerecipher"                     *)
(*   "VigenereCipher"         -> "vigenerecipher"                     *)
(*   "HowRoundIsaCountry?"    -> "howroundisacountry"                 *)
(*   "HowRoundIsACountry"     -> "howroundisacountry"                 *)
(* ------------------------------------------------------------------ *)

canonicalNameKey[s_String] := ToLowerCase @ StringDelete[
  CharacterNormalize[s, "NFKD"],
  Except[LetterCharacter | DigitCharacter]
];
canonicalNameKey[_] := $Failed;


(* ------------------------------------------------------------------ *)
(* Challenge prompts                                                   *)
(*                                                                     *)
(* Accepted JSON shapes:                                               *)
(*   List of Associations (one per challenge), OR                      *)
(*   Association of name -> Association.                               *)
(*                                                                     *)
(* Each record must carry (at minimum) a prompt string under one of   *)
(* "input" / "prompt" / "challenge".  The challenge name is taken     *)
(* from "name" / "challengeName", or (heuristically) the first line   *)
(* of the prompt with whitespace stripped.                            *)
(* ------------------------------------------------------------------ *)

loadChallengesImpl[path_String] := Module[{raw, items, normalized},
  If[! FileExistsQ[path],
    Message[JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges::notfound,
            path];
    Return[$Failed]
  ];
  raw = Quiet @ Check[Import[path, "RawJSON"], $Failed];
  If[raw === $Failed,
    Message[JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges::parseerr,
            path];
    Return[$Failed]
  ];

  items = Which[
    ListQ[raw] && AllTrue[raw, AssociationQ], raw,
    AssociationQ[raw] && AllTrue[Values[raw], AssociationQ],
      (* Propagate the outer key as the challenge name so it isn't
         reinvented by the prompt-first-line heuristic. *)
      KeyValueMap[
        Function[{k, v},
          Association[<|"name" -> k|>, v]
        ], raw],
    True, $Failed
  ];
  If[items === $Failed,
    Message[JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges::badshape];
    Return[$Failed]
  ];

  normalized = MapIndexed[
    Module[{rec = #1, idx = First[#2], name, prompt},
      prompt = Lookup[rec, "input",
        Lookup[rec, "prompt",
          Lookup[rec, "challenge", Missing["NoPrompt"]]]];
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

loadChallengesImpl[other_] := (
  Message[
    JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges::badarg,
    other
  ];
  $Failed
);


(* ------------------------------------------------------------------ *)
(* Test bank                                                           *)
(*                                                                     *)
(* WXF Association:                                                    *)
(*   <| name -> { {HoldComplete[input], expected},                     *)
(*                {HoldComplete[input], expected, <| metadata |>} },  *)
(*      ... |>                                                         *)
(* ------------------------------------------------------------------ *)

validTestEntryQ[entry_List] :=
  MatchQ[entry,
    {_HoldComplete, _} |
    {_HoldComplete, _, _Association}
  ];
validTestEntryQ[_] := False;

loadTestBankImpl[path_String] := Module[{raw, validated},
  If[! FileExistsQ[path],
    Message[JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank::notfound,
            path];
    Return[$Failed]
  ];
  raw = Quiet @ Check[Import[path, "WXF"], $Failed];
  If[raw === $Failed,
    Message[JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank::parseerr,
            path];
    Return[$Failed]
  ];
  If[! AssociationQ[raw],
    Message[JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank::badshape,
            "<top>"];
    Return[$Failed]
  ];

  validated = KeyValueMap[
    Function[{name, entries},
      If[! ListQ[entries] || ! AllTrue[entries, validTestEntryQ],
        Message[JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank::badshape,
                name];
        Nothing,
        name -> MapIndexed[
          <|
            "challengeName" -> name,
            "testIndex"     -> First[#2],
            "input"         -> First[#1],
            "expected"      -> #1[[2]],
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

loadTestBankImpl[other_] := (
  Message[
    JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank::badarg,
    other
  ];
  $Failed
);


(* ------------------------------------------------------------------ *)
(* reconcileNamesImpl                                                  *)
(*                                                                     *)
(* Aligns the keys of `challenges` and `testBank` so that both         *)
(* dictionaries use the same canonical name for each challenge.        *)
(* The test bank's key is treated as the canonical form (it's the      *)
(* file the runner already grades against and the legacy solutions     *)
(* directory matches).                                                  *)
(*                                                                     *)
(* Returns:                                                             *)
(*   <| "challenges" -> aligned challenges Assoc,                       *)
(*      "testBank"   -> testBank (returned unchanged for symmetry),    *)
(*      "renamed"    -> {<|"old"->..,"new"->..|>, ...},                *)
(*      "unmatchedInChallenges" -> {names without a test-bank twin},   *)
(*      "unmatchedInTestBank"   -> {names without a challenge twin},   *)
(*      "summary"    -> <|"renamed"->Int, "matched"->Int,              *)
(*                         "unmatchedChallenges"->Int,                 *)
(*                         "unmatchedTestBank"->Int|> |>                *)
(*                                                                     *)
(* Collisions (two challenge names that canonicalize to the same key) *)
(* surface as a tagged Message and the second-and-onwards entries are *)
(* dropped from the aligned output.  This is loud-by-design so a       *)
(* badly-authored bank can't silently lose data.                        *)
(* ------------------------------------------------------------------ *)

reconcileNamesImpl[challenges_Association, testBank_Association] := Module[
  {tbCanonGroups, tbCanon, chCanonGroups, alignedChallenges = <||>,
   renamed = {}, unmatchedC = {}, matched = 0, k, canon, tbName,
   newEntry, seen = <||>},

  (* Build canon -> tbName map.  Detect collisions on the test-bank
     side and warn (the runner would also get confused by them).      *)
  tbCanonGroups = GroupBy[Keys[testBank], canonicalNameKey];
  KeyValueMap[
    Function[{c, names},
      If[Length[names] > 1,
        Message[
          JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames::tbcollision,
          c, names
        ]
      ]
    ],
    tbCanonGroups
  ];
  tbCanon = AssociationMap[First @ tbCanonGroups[#] &, Keys[tbCanonGroups]];

  (* Walk challenges in input order; remap key when the canonical form
     hits the test bank, keep the original key otherwise.              *)
  chCanonGroups = GroupBy[Keys[challenges], canonicalNameKey];
  Do[
    canon = canonicalNameKey[k];
    If[KeyExistsQ[seen, canon],
      Message[
        JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames::collision,
        canon, {seen[canon], k}
      ];
      Continue[]
    ];
    seen[canon] = k;

    tbName = Lookup[tbCanon, canon, Missing["NoMatch"]];
    If[MissingQ[tbName],
      (* Keep original key but flag as unmatched. *)
      alignedChallenges[k] = challenges[k];
      AppendTo[unmatchedC, k],
      (* Remap to the test bank's canonical name. *)
      newEntry = Append[challenges[k], "name" -> tbName];
      alignedChallenges[tbName] = newEntry;
      matched++;
      If[k =!= tbName,
        AppendTo[renamed, <|"old" -> k, "new" -> tbName|>]
      ]
    ],
    {k, Keys[challenges]}
  ];

  <|
    "challenges"             -> alignedChallenges,
    "testBank"               -> testBank,
    "renamed"                -> renamed,
    "unmatchedInChallenges"  -> unmatchedC,
    "unmatchedInTestBank"    -> Complement[
                                  Keys[testBank],
                                  Lookup[tbCanon, canonicalNameKey /@ Keys[challenges], Nothing]
                                ],
    "summary" -> <|
      "matched"             -> matched,
      "renamed"             -> Length[renamed],
      "unmatchedChallenges" -> Length[unmatchedC],
      "unmatchedTestBank"   -> Length[Complement[
                                  Keys[testBank],
                                  Lookup[tbCanon, canonicalNameKey /@ Keys[challenges], Nothing]
                                ]]
    |>
  |>
];

reconcileNamesImpl[a_, b_] := (
  Message[
    JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames::badarg,
    Head[a], Head[b]
  ];
  $Failed
);


End[];  (* `Private` *)
