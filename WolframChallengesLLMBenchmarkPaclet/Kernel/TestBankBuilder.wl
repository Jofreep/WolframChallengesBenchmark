(* ::Package:: *)

(* :Context: JofreEspigulePons`WolframChallengesBenchmark`Private` *)
(* :Summary:
     Plain-text .wlchallenge authoring format and round-trippable build
     pipeline.  See docs/WLCHALLENGE-FORMAT.md for the user-facing
     specification (kept out of this source to avoid nested-comment
     parsing hazards).

     Public impls (dispatched from the root module):
       loadWLChallengeImpl[path]        -> Association name -> entry
       loadChallengesDirImpl[dir]       -> Association name -> entry
       buildTestBankImpl[dir]           -> {challengesAssoc, testBankAssoc}
       writeTestBankFilesImpl[dir, jsonOut, wxfOut]
                                        -> summary Association
       writeWLChallengeDirImpl[challengesAssoc, testBankAssoc, dir]
                                        -> {paths...} \[LongDash] used to
                                           seed an authoring directory
                                           from an existing WXF bank

     Safety notes:
       - Parsing uses ImportString with the HeldExpressions format so
         test inputs are NEVER evaluated during the build.  The build
         step does evaluate the expected-value RHS by design: it is
         the same data that ships in the compiled WXF bank.

     Cross-file dependencies (all in the paclet's Private context):
       safeSlug, logWarn -- Utilities.wl
*)

Begin["JofreEspigulePons`WolframChallengesBenchmark`Private`"];

$wlcExt = ".wlchallenge";

(* ----------------------------------------------------------------------- *)
(* Header / section parser                                                 *)
(* ----------------------------------------------------------------------- *)

(* A header line.  The captured key and (possibly empty) inline value are
   returned as {key, value}.  We require the line to be exactly the header
   so an inline metadata-style comment mid-prose does not get picked up. *)

$wlcHeaderPattern =
  StartOfString ~~ Whitespace... ~~ "(*" ~~ Whitespace... ~~ ":" ~~
    key : LetterCharacter .. ~~ ":" ~~ Whitespace... ~~
    value : Shortest[___] ~~ Whitespace... ~~ "*)" ~~
    Whitespace... ~~ EndOfString;

wlcHeaderMatch[line_String] :=
  Replace[
    StringCases[line, $wlcHeaderPattern :> {key, value}],
    {{{k_, v_}} :> {k, StringTrim[v]}, _ :> None}
  ];

(* parseWLCSections \[LongDash] walk the file line-by-line and produce an
   ordered list of {key, "scalar"|"block", value} entries.  Block values
   are joined with newlines and trimmed.  Inlined (no nested closure) so
   Module locals cannot leak across invocations. *)

parseWLCSections[text_String] := Module[
  {lines, n, i, sections = {}, currentKey = None, currentBuf = {}, m, line},
  lines = StringSplit[text, "\n", All];
  n = Length[lines];
  Do[
    line = lines[[i]];
    m = wlcHeaderMatch[line];
    If[m =!= None,
      (* flush prior open block *)
      If[currentKey =!= None,
        AppendTo[sections, {currentKey, "block",
          StringTrim @ StringRiffle[currentBuf, "\n"]}];
        currentKey = None; currentBuf = {}
      ];
      Which[
        m[[2]] =!= "",
          AppendTo[sections, {m[[1]], "scalar", m[[2]]}],
        True,
          currentKey = m[[1]]; currentBuf = {}
      ],
      If[currentKey =!= None, AppendTo[currentBuf, line]]
    ],
    {i, n}
  ];
  (* flush trailing open block *)
  If[currentKey =!= None,
    AppendTo[sections, {currentKey, "block",
      StringTrim @ StringRiffle[currentBuf, "\n"]}]
  ];
  sections
];

(* ----------------------------------------------------------------------- *)
(* Test-line parser                                                        *)
(*                                                                         *)
(* A single test is parsed by importing the line as held.  The result is a *)
(* HoldComplete[{input, expected[, metadata]}] expression.  We then split  *)
(* the held list into a tuple where position 1 stays held and the rest    *)
(* are released (evaluated). *)
(* ----------------------------------------------------------------------- *)

(* parseTestLine \[LongDash] import a single test line as held and split
   into {HoldComplete[input], expected[, metadata]}.

   We deliberately DO NOT wrap ImportString in Check/CheckAll: on very
   long inputs (hundreds of string elements) the importer emits internal
   non-fatal messages which would otherwise trigger the fallback.  We
   validate the shape of the returned value directly \[LongDash] a
   successful import is a non-empty list of HoldComplete[...] expressions. *)

parseTestLine[src_String] := Module[{trimmed, held},
  trimmed = StringTrim[src];
  If[trimmed === "" || StringStartsQ[trimmed, "(*"], Return[Nothing]];
  held = Quiet @ ImportString[trimmed, {"WL", "HeldExpressions"}];
  If[! ListQ[held] || held === {} || ! MatchQ[First[held], _HoldComplete],
    Return[$Failed]
  ];
  held = First[held];
  Replace[held,
    {
      HoldComplete[{a_, b_}]      :> {HoldComplete[a], b},
      HoldComplete[{a_, b_, m_}]  :> {HoldComplete[a], b, m},
      _                            :> $Failed
    }
  ]
];

splitTestLines[block_String] :=
  Select[StringSplit[block, "\n"], StringTrim[#] =!= "" &];

(* ----------------------------------------------------------------------- *)
(* Single-file load                                                        *)
(* ----------------------------------------------------------------------- *)

JofreEspigulePons`WolframChallengesBenchmark`LoadWLChallenge::notfound =
  "WLChallenge file not found: `1`.";
JofreEspigulePons`WolframChallengesBenchmark`LoadWLChallenge::missing =
  "WLChallenge file `1` is missing required section :`2`:.";
JofreEspigulePons`WolframChallengesBenchmark`LoadWLChallenge::badtest =
  "WLChallenge file `1`: failed to parse test line: `2`";

loadWLChallengeImpl[path_String] := Module[
  {text, sections, scalarPairs, blockPairs, scalars, blocks, name,
   indexStr, index, instruction, prompt, testBlock, lines, parsed, badIdx},

  If[! FileExistsQ[path],
    Message[JofreEspigulePons`WolframChallengesBenchmark`LoadWLChallenge::notfound,
      path];
    Return[$Failed]
  ];
  text = Quiet @ Check[Import[path, "Text"], $Failed];
  If[text === $Failed,
    Message[JofreEspigulePons`WolframChallengesBenchmark`LoadWLChallenge::notfound,
      path];
    Return[$Failed]
  ];

  sections    = parseWLCSections[text];
  (* Build scalar/block tables via Cases-into-Rule to avoid the WL quirk
     where two `RuleDelayed` patterns inside a single `Rule` argument can
     have their pattern variables clobber each other. *)
  scalarPairs = Cases[sections, {kn_String, "scalar", vn_} :> (kn -> vn)];
  blockPairs  = Cases[sections, {kn_String, "block",  vn_} :> (kn -> vn)];
  scalars     = Association[scalarPairs];
  blocks      = Association[blockPairs];

  name = Lookup[scalars, "Name", Lookup[blocks, "Name", None]];
  If[! StringQ[name] || name === "",
    Message[JofreEspigulePons`WolframChallengesBenchmark`LoadWLChallenge::missing,
      path, "Name"];
    Return[$Failed]
  ];

  prompt = Lookup[blocks, "Prompt", Lookup[scalars, "Prompt", None]];
  If[! StringQ[prompt] || prompt === "",
    Message[JofreEspigulePons`WolframChallengesBenchmark`LoadWLChallenge::missing,
      path, "Prompt"];
    Return[$Failed]
  ];

  testBlock = Lookup[blocks, "Tests", Lookup[scalars, "Tests", ""]];
  If[! StringQ[testBlock] || StringTrim[testBlock] === "",
    Message[JofreEspigulePons`WolframChallengesBenchmark`LoadWLChallenge::missing,
      path, "Tests"];
    Return[$Failed]
  ];

  indexStr = Lookup[scalars, "Index", "0"];
  index    = Quiet @ Check[ToExpression[indexStr], 0];
  If[! IntegerQ[index], index = 0];

  instruction = Lookup[scalars, "Instruction",
    Lookup[blocks, "Instruction", ""]];

  lines  = splitTestLines[testBlock];
  parsed = MapIndexed[
    Module[{r = parseTestLine[#1]},
      If[r === $Failed,
        Message[JofreEspigulePons`WolframChallengesBenchmark`LoadWLChallenge::badtest,
          path, #1];
        $Failed,
        r
      ]
    ] &,
    lines
  ];
  parsed = DeleteCases[parsed, Nothing];
  badIdx = Position[parsed, $Failed, {1}];
  If[badIdx =!= {}, Return[$Failed]];

  <|
    "name"        -> name,
    "index"       -> index,
    "instruction" -> instruction,
    "prompt"      -> prompt,
    (* tests : list of {HoldComplete[input], expected[, metadata]} *)
    "tests"       -> parsed
  |>
];

(* ----------------------------------------------------------------------- *)
(* Directory load + bank build                                             *)
(* ----------------------------------------------------------------------- *)

loadChallengesDirImpl[dir_String] := Module[{files, loaded, ordered},
  If[! DirectoryQ[dir],
    logWarn["LoadChallengesDir: directory not found: " <> dir];
    Return[<||>]
  ];
  files = FileNames["*" <> $wlcExt, dir];
  loaded = Map[
    Function[path,
      Module[{e = loadWLChallengeImpl[path]},
        If[e === $Failed, Nothing, e["name"] -> e]
      ]
    ],
    files
  ];
  loaded = DeleteCases[loaded, Nothing];
  ordered = SortBy[loaded, #[[2, "index"]] &];
  Association[ordered]
];

buildTestBankImpl[dir_String] := Module[
  {entries, challengesAssoc, testBankAssoc},
  entries = loadChallengesDirImpl[dir];
  If[entries === <||>, Return[{<||>, <||>}]];
  challengesAssoc = AssociationMap[
    Function[name,
      Module[{e = entries[name]},
        <|
          "index"       -> e["index"],
          "name"        -> name,
          "instruction" -> e["instruction"],
          "prompt"      -> e["prompt"]
        |>
      ]
    ],
    Keys[entries]
  ];
  testBankAssoc = AssociationMap[
    Function[name,
      MapIndexed[
        Module[{t = #1, idx = First[#2]},
          <|
            "challengeName" -> name,
            "testIndex"     -> idx,
            "input"         -> t[[1]],
            "expected"      -> t[[2]],
            "metadata"      -> If[Length[t] >= 3 && AssociationQ[t[[3]]],
                                  t[[3]], <||>]
          |>
        ] &,
        entries[name]["tests"]
      ]
    ],
    Keys[entries]
  ];
  {challengesAssoc, testBankAssoc}
];

(* writeTestBankFilesImpl \[LongDash] read .wlchallenge files from `dir`
   and emit:
     - jsonOut : the legacy challenge-prompts JSON (records with
                 "instruction" and "input" so existing LoadChallenges
                 continues to read it).
     - wxfOut  : the test bank as a WXF Association of
                 {HoldComplete[input], expected[, metadata]} entries
                 (matching the legacy ChallengesTests.wxf shape). *)

writeTestBankFilesImpl[dir_String, jsonOut_String, wxfOut_String] := Module[
  {built, challengesAssoc, testBankAssoc, jsonRecords, wxfBank,
   dirJson, dirWxf},
  built = buildTestBankImpl[dir];
  {challengesAssoc, testBankAssoc} = built;

  (* Legacy JSON shape: list of {instruction, input} records, in index order. *)
  jsonRecords = Values @ AssociationMap[
    Function[name,
      <|
        "instruction" -> challengesAssoc[name, "instruction"],
        "input"       -> challengesAssoc[name, "prompt"]
      |>
    ],
    Keys[challengesAssoc]
  ];

  (* Legacy WXF shape: name -> { {HoldComplete[input], expected, [metadata]}, ... }
     emitWXFEntries below preserves HoldComplete by walking the parsed
     test list directly \[LongDash] we never let the input evaluate. *)
  wxfBank = AssociationMap[
    Function[name,
      Map[
        Function[entry,
          With[{held = entry["input"], exp = entry["expected"], md = entry["metadata"]},
            If[AssociationQ[md] && md =!= <||>,
              {held, exp, md},
              {held, exp}
            ]
          ]
        ],
        testBankAssoc[name]
      ]
    ],
    Keys[testBankAssoc]
  ];

  dirJson = DirectoryName[jsonOut];
  dirWxf  = DirectoryName[wxfOut];
  If[StringLength[dirJson] > 0 && ! DirectoryQ[dirJson],
    Quiet @ CreateDirectory[dirJson, CreateIntermediateDirectories -> True]];
  If[StringLength[dirWxf] > 0 && ! DirectoryQ[dirWxf],
    Quiet @ CreateDirectory[dirWxf, CreateIntermediateDirectories -> True]];

  Export[jsonOut, jsonRecords, "RawJSON"];
  Export[wxfOut,  wxfBank,     "WXF"];

  <|
    "json"        -> jsonOut,
    "wxf"         -> wxfOut,
    "challenges"  -> Length[challengesAssoc],
    "tests"       -> Total[Length /@ Values[wxfBank]]
  |>
];

(* ----------------------------------------------------------------------- *)
(* Emitter (WXF -> .wlchallenge text)                                      *)
(* ----------------------------------------------------------------------- *)

(* heldToInputForm \[LongDash] convert a HoldComplete[expr] into an
   InputForm string without evaluating expr.  The HoldForm wrapper is
   used to prevent evaluation, then stripped from the resulting string
   so the emitted test line is valid WL input.

   Caveat: a held flat orderless head like `Times[a, b, c, d]` may be
   serialized by ToString[HoldForm[..], InputForm] as the operator-form
   `a*b*c*d` whose re-parse produces a left-nested `Times[Times[a,b,c],d]`.
   This is an InputForm round-trip artifact and is mathematically
   equivalent at evaluation time (ReleaseHold yields the same value), so
   we accept the drift rather than reach for a more invasive serializer. *)

stripHoldForm[s_String] := StringReplace[s,
  StartOfString ~~ "HoldForm[" ~~ body__ ~~ "]" ~~ EndOfString :> body,
  1
];

heldToInputForm[HoldComplete[expr_]] := stripHoldForm @
  ToString[HoldForm[expr], InputForm, PageWidth -> Infinity];
heldToInputForm[other_] := ToString[other, InputForm, PageWidth -> Infinity];

emitTestLine[entry_Association] := Module[
  {inHeld, exp, md, inS, expS, mdS},
  inHeld = entry["input"];
  exp    = entry["expected"];
  md     = entry["metadata"];
  inS    = heldToInputForm[inHeld];
  expS   = ToString[exp, InputForm, PageWidth -> Infinity];
  If[AssociationQ[md] && md =!= <||>,
    mdS = ToString[md, InputForm, PageWidth -> Infinity];
    "{" <> inS <> ", " <> expS <> ", " <> mdS <> "}",
    "{" <> inS <> ", " <> expS <> "}"
  ]
];

(* emitWLChallengeImpl \[LongDash] render one challenge's .wlchallenge text
   given a challenge metadata Association (as produced by
   loadChallengesImpl) and its test-bank entries (list of
   <|input, expected, metadata, ...|>). *)

emitWLChallengeImpl[challenge_Association, tests_List] := Module[
  {name, idx, instr, prompt, body, testLines},
  name   = challenge["name"];
  idx    = Lookup[challenge, "index", 0];
  instr  = Lookup[challenge, "instruction", ""];
  prompt = Lookup[challenge, "prompt", ""];

  testLines = StringRiffle[emitTestLine /@ tests, "\n"];

  body = StringJoin[
    "(* :Name: ",        name,                "  *)\n",
    "(* :Index: ",       ToString[idx],       " *)\n",
    "(* :Instruction: ", instr,               " *)\n",
    "\n",
    "(* :Prompt: *)\n",
    prompt,
    "\n\n",
    "(* :Tests: *)\n",
    testLines,
    "\n"
  ];
  body
];

(* normalizeChallengeName \[LongDash] strip all non-alphanumerics and
   lowercase.  Used to reconcile the bank's CamelCase names with the
   challenge JSON's whitespace-stripped-but-otherwise-verbatim names when
   seeding. *)

normalizeChallengeName[s_String] :=
  ToLowerCase @ StringReplace[s, RegularExpression["[^A-Za-z0-9]"] -> ""];

(* matchChallengeRecord \[LongDash] given a bank name and the challenges
   Association, find the record whose name normalizes the same way.
   Returns the record Association or Missing["NoChallenge"]. *)

matchChallengeRecord[bankName_String, challenges_Association] := Module[
  {want, direct, byNorm},
  direct = Lookup[challenges, bankName, Missing["NoChallenge"]];
  If[! MissingQ[direct], Return[direct]];
  want  = normalizeChallengeName[bankName];
  byNorm = FirstCase[
    Values[challenges],
    rec_Association /; normalizeChallengeName[Lookup[rec, "name", ""]] === want :> rec,
    Missing["NoChallenge"]
  ];
  byNorm
];

(* writeWLChallengeDirImpl \[LongDash] iterate over the TEST BANK
   (authoritative for what the runner actually exercises) and emit one
   .wlchallenge file per bank entry.  For each, reconcile the challenge
   metadata (prompt / instruction) via matchChallengeRecord so the
   seeded file is complete even when the challenge JSON's name
   convention differs from the bank's.  Orphan challenge records (in
   JSON but not in the bank) are skipped with a warning \[LongDash]
   the runner cannot evaluate them anyway.

   The emitted :Name: field is always the BANK name, which is the name
   the runner uses and the name that solutions/<model>/<name>.wl files
   are keyed against. *)

writeWLChallengeDirImpl[challenges_Association, testBank_Association,
  dir_String] := Module[{paths, orphans},
  If[! DirectoryQ[dir],
    CreateDirectory[dir, CreateIntermediateDirectories -> True]];

  orphans = Complement[
    normalizeChallengeName /@ Keys[challenges],
    normalizeChallengeName /@ Keys[testBank]
  ];
  If[orphans =!= {},
    logWarn["WriteWLChallengeDir: " <> ToString[Length[orphans]] <>
      " challenge record(s) have no bank entry and will be skipped."]
  ];

  paths = KeyValueMap[
    Function[{bankName, tests},
      Module[{rec, prompt, instr, idx, ch, text, fname},
        rec = matchChallengeRecord[bankName, challenges];
        If[MissingQ[rec],
          logWarn["WriteWLChallengeDir: no prompt found for bank entry "
            <> bankName <> "; emitting stub prompt."];
          prompt = "[no prompt]"; instr = ""; idx = 0,
          prompt = Lookup[rec, "prompt",      "[no prompt]"];
          instr  = Lookup[rec, "instruction", ""];
          idx    = Lookup[rec, "index",       0]
        ];
        ch = <|
          "name"        -> bankName,
          "index"       -> idx,
          "instruction" -> instr,
          "prompt"      -> prompt
        |>;
        text  = emitWLChallengeImpl[ch, tests];
        fname = FileNameJoin[{dir, safeSlug[bankName] <> $wlcExt}];
        Export[fname, text, "Text", CharacterEncoding -> "UTF-8"];
        fname
      ]
    ],
    testBank
  ];
  paths
];

End[];
