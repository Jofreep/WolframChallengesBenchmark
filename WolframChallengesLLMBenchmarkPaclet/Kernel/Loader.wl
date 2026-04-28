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


(* ------------------------------------------------------------------ *)
(* LoadChallengesJSONL                                                 *)
(*                                                                     *)
(* Reads the single-file benchmark format (challenges.jsonl), one      *)
(* JSON record per line, and returns                                   *)
(*                                                                     *)
(*   <|"challenges" -> Association of task_id -> challenge metadata,  *)
(*     "testBank"   -> Association of task_id -> list of test entries|> *)
(*                                                                     *)
(* Same downstream shape as combining LoadChallenges + LoadTestBank,  *)
(* so callers can swap loaders without touching the runner.            *)
(*                                                                     *)
(* Optional second argument: path to a private canonical-solutions    *)
(* JSONL file.  When supplied (and file exists), the returned         *)
(* Association also has a "canonicalSolutions" key mapping            *)
(* task_id -> source string.  Used by the bank-self-test path.        *)
(*                                                                     *)
(* Format reference: docs/CHALLENGES-JSONL-FORMAT.md                  *)
(* ------------------------------------------------------------------ *)

JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL::notfound =
  "JSONL file not found: `1`.";
JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL::parseerr =
  "Could not parse JSONL line `1` of `2`: `3`.";
JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL::badrecord =
  "Record `1` of `2` missing required field: `3`.";
JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL::heldparse =
  "Could not parse held WL source string in `1` test #`2`: `3`.";
JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL::badarg =
  "LoadChallengesJSONL expects a file path string; got `1`.";

(* parseHeldWL: turn an InputForm WL source string into HoldComplete[expr].
   Wraps ImportString[..., {"WL", "HeldExpressions"}] which returns a list
   of HoldComplete'd top-level expressions; we take the first.  Returns
   $Failed on parse error; the caller emits the message with context. *)
parseHeldWLString[src_String] := Module[{held},
  held = Quiet @ ImportString[src, {"WL", "HeldExpressions"}];
  If[! ListQ[held] || held === {} || ! MatchQ[First[held], _HoldComplete],
    Return[$Failed]
  ];
  First[held]
];

(* parseExpectedString: parse the expected_wl as a value (released).
   ToExpression handles complex literals like Graph[...], Image[...]. *)
parseExpectedString[src_String] :=
  Quiet @ Check[ToExpression[src], $Failed];

loadChallengesJSONLImpl[path_String] :=
  loadChallengesJSONLImpl[path, None];

loadChallengesJSONLImpl[path_String, privatePath_] := Module[
  {lines, records, challenges, testBank, canonicalSolutions = None,
   privLines, privRecords},

  If[! FileExistsQ[path],
    Message[JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL::notfound,
      path];
    Return[$Failed]
  ];

  lines = Quiet @ Check[ReadList[path, "String"], $Failed];
  If[lines === $Failed, Return[$Failed]];

  records = MapIndexed[
    Function[{line, idxList},
      Module[{idx = First[idxList], rec},
        rec = Quiet @ Check[ImportString[line, "RawJSON"], $Failed];
        If[! AssociationQ[rec],
          Message[JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL::parseerr,
            idx, path, StringTake[line, UpTo[80]]];
          Return[$Failed, Module]
        ];
        Scan[
          If[! KeyExistsQ[rec, #],
            Message[JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL::badrecord,
              idx, path, #];
            Return[$Failed, Module]
          ] &,
          {"task_id", "prompt", "tests"}
        ];
        rec
      ]
    ],
    DeleteCases[lines, ""]
  ];
  If[MemberQ[records, $Failed], Return[$Failed]];

  (* Build challenges Association: task_id -> metadata (no tests).
     NB: parameter must be a plain Symbol \[LongDash] `task_id` would
     parse as Pattern[task, Blank[id]] and break Function. *)
  challenges = AssociationMap[
    Function[taskId,
      Module[{rec = SelectFirst[records, #["task_id"] === taskId &]},
        <|
          "name"        -> Lookup[rec, "name", taskId],
          "index"       -> Lookup[rec, "index", 0],
          "instruction" -> Lookup[rec, "instruction", ""],
          "prompt"      -> rec["prompt"],
          "entry_point" -> Lookup[rec, "entry_point", ""],
          (* Topic tags from the Wolfram Challenges site.  Optional:
             records that don't have a "tags" field get an empty list.
             Tag-aware dashboards (ModelStrengths, BankQualityReport)
             read from here. *)
          "tags"        -> Lookup[rec, "tags", {}]
        |>
      ]
    ],
    #["task_id"] & /@ records
  ];

  (* Build testBank Association: task_id -> list of <|input, expected, ...|>.
     Same shape LoadTestBank produces. *)
  testBank = Association @ Map[
    Function[rec,
      Module[{task = rec["task_id"], rawTests, parsedTests, parseFailures},
        rawTests = Lookup[rec, "tests", {}];
        parseFailures = {};
        parsedTests = MapIndexed[
          Function[{t, ti},
            Module[{held, exp},
              held = parseHeldWLString[t["input_wl"]];
              If[held === $Failed,
                AppendTo[parseFailures, First[ti]];
                Message[JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL::heldparse,
                  task, First[ti], StringTake[t["input_wl"], UpTo[60]]];
                Return[Nothing, Module]
              ];
              exp = parseExpectedString[t["expected_wl"]];
              <|
                "challengeName" -> task,
                "testIndex"     -> First[ti],
                "input"         -> held,
                "expected"      -> exp,
                "metadata"      -> Lookup[t, "metadata", <||>]
              |>
            ]
          ],
          rawTests
        ];
        task -> parsedTests
      ]
    ],
    records
  ];

  (* Optionally load the private canonical-solutions JSONL. *)
  If[StringQ[privatePath] && FileExistsQ[privatePath],
    privLines = Quiet @ Check[ReadList[privatePath, "String"], {}];
    privLines = DeleteCases[privLines, ""];
    privRecords = DeleteCases[
      Quiet @ Check[ImportString[#, "RawJSON"], $Failed] & /@ privLines,
      $Failed];
    canonicalSolutions = Association @ Map[
      Function[r, r["task_id"] -> Lookup[r, "canonical_solution", ""]],
      Select[privRecords, AssociationQ]
    ];
  ];

  If[canonicalSolutions =!= None,
    <|"challenges"         -> challenges,
      "testBank"           -> testBank,
      "canonicalSolutions" -> canonicalSolutions|>,
    <|"challenges" -> challenges,
      "testBank"   -> testBank|>
  ]
];

loadChallengesJSONLImpl[other_, ___] := (
  Message[
    JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL::badarg,
    other];
  $Failed
);


End[];  (* `Private` *)
