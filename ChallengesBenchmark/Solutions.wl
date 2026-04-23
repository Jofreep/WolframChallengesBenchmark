(* ::Package:: *)

(* :Context: ChallengesBenchmark`Private` *)
(* :Summary: On-disk storage of per-model candidate solutions.

   Layout:
     solutions/<modelSlug>/<challengeName>.wl      — WL source
     solutions/<modelSlug>/<challengeName>.meta.json — sidecar metadata

   The .meta.json sidecar records:
     { "model": "...", "challengeName": "...",
       "sourceHash": "sha256:...",
       "generatedAt": "ISO-8601",
       "rawResponseHash": "sha256:...",
       "extractor": "ExtractCode/v1" }
*)

Begin["ChallengesBenchmark`Private`"];

loadSolutionsImpl[dir_String] := Module[{files, loaded},
  If[! DirectoryQ[dir],
    logWarn["Solutions directory does not exist: " <> dir];
    Return[<||>]
  ];
  files = FileNames["*.wl", dir];
  loaded = Map[
    Module[{name, code, meta},
      name = FileBaseName[#];
      code = Quiet @ Check[Import[#, "Text"], $Failed];
      meta = Quiet @ Check[
        Import[StringReplace[#, ".wl" ~~ EndOfString -> ".meta.json"], "RawJSON"],
        <||>
      ];
      If[code === $Failed,
        logWarn["Could not read solution: " <> #]; Nothing,
        name -> <|"code" -> code, "metadata" -> meta|>
      ]
    ] &,
    files
  ];
  Association[loaded]
];

(* saveSolutionImpl — write a solution to disk.

   Optional testBank arg enables write-time audit: if the parsed code does
   not define any of the function names the test bank expects for `name`,
   we refuse the write and return $Failed (preventing the regression we
   already know happens during migration — see AuditSolutions header).

   Pass testBank -> None (or omit) to skip the audit. *)

saveSolutionImpl[dir_String, name_String, code_String] :=
  saveSolutionImpl[dir, name, code, None, <||>];

saveSolutionImpl[dir_String, name_String, code_String, testBank_] :=
  saveSolutionImpl[dir, name, code, testBank, <||>];

(* 5-arg form carries the real implementation. `extraMeta` is merged into
   the sidecar after the default fields so generators can inject
   promptHash / rawResponseHash / provider / attempts / llm without
   losing the built-in bookkeeping (sourceHash, generatedAt). Pass <||>
   to skip the injection. *)

saveSolutionImpl[dir_String, name_String, code_String, testBank_, extraMeta_] :=
Module[
  {slug, wlPath, metaPath, hash, meta, defined, expected, auditOk, extras},

  (* Write-time audit. Skipped when testBank is None or when the bank has
     no entry for `name` (caller knows what they're doing). *)
  If[AssociationQ[testBank] && KeyExistsQ[testBank, name],
    defined  = ChallengesBenchmark`Private`definedFunctionNames[code];
    expected = ChallengesBenchmark`Private`expectedFunctionNames[testBank, name];
    auditOk = Which[
      defined === {},
        Message[ChallengesBenchmark::saveAudit, name,
          "no top-level definitions found"];
        False,
      expected =!= {} && ! IntersectingQ[defined, expected],
        Message[ChallengesBenchmark::saveAudit, name,
          "code defines " <> ToString[defined] <>
          " but the test bank expects one of " <> ToString[expected]];
        False,
      True, True
    ];
    If[! auditOk, Return[$Failed]]
  ];

  If[! DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
  slug = safeSlug[name];
  wlPath   = FileNameJoin[{dir, slug <> ".wl"}];
  metaPath = FileNameJoin[{dir, slug <> ".meta.json"}];
  hash = Hash[code, "SHA256", "HexString"];
  extras = If[AssociationQ[extraMeta], extraMeta, <||>];
  meta = Join[
    <|
      "model"         -> FileNameTake[dir],
      "challengeName" -> name,
      "sourceHash"    -> "sha256:" <> hash,
      "generatedAt"   -> DateString["ISODateTime"],
      "extractor"     -> "ExtractCode/v1"
    |>,
    extras
  ];
  Export[wlPath, code, "Text", CharacterEncoding -> "UTF-8"];
  Export[metaPath, meta, "RawJSON"];
  wlPath
];

ChallengesBenchmark::saveAudit =
  "SaveSolution refused to write `1`: `2`. Pass testBank -> None to bypass.";

(* ----------------------------------------------------------------------- *)
(* AuditSolutions — pre-flight consistency check                           *)
(*                                                                         *)
(* Migration from an in-notebook solutionsAssoc can silently land code for *)
(* challenge A in the .wl file for challenge B (we have seen exactly this  *)
(* mislabeling in production Opus 4.6 output). That failure mode wastes a  *)
(* full test run: the harness correctly reports the affected tests as      *)
(* unevaluated, but nobody looks at the actuals until after the fact.      *)
(*                                                                         *)
(* This function cross-checks each solutions/<model>/<name>.wl file        *)
(* against the expected function name derived from the test bank's first  *)
(* input (testBank[name][[1,"input"]] is HoldComplete[Foo[...]] so Foo is  *)
(* the function the solution is supposed to define).                       *)
(*                                                                         *)
(* The parse is structural (held) so merely reading the file never         *)
(* installs definitions or triggers side effects.                          *)
(* ----------------------------------------------------------------------- *)

(* expectedFunctionName — peek at the first held test input for a challenge
   and return the head of the call as a string. Kept for back-compat; the
   audit now uses expectedFunctionNames (plural). Returns a Missing[...]
   token when the test bank entry is absent, empty, or its first input
   doesn't match the simple HoldComplete[Foo[args...]] / HoldComplete[Foo]
   shape. Test inputs wrapped in Show / CompoundExpression / Module / etc.
   will return the wrapper head — use expectedFunctionNames to drill in. *)

ChallengesBenchmark`Private`expectedFunctionName[testBank_Association, name_String] :=
  Module[{tests, firstInput},
    tests = Lookup[testBank, name, Missing["NoTests"]];
    If[MissingQ[tests] || ! ListQ[tests] || Length[tests] === 0,
      Return[Missing["NoTests"]]
    ];
    firstInput = Lookup[First[tests], "input", Missing["NoInput"]];
    Replace[firstInput, {
      HoldComplete[f_Symbol[___]] :> SymbolName[Unevaluated[f]],
      HoldComplete[f_Symbol]      :> SymbolName[Unevaluated[f]],
      _                           :> Missing["UnrecognizedInput"]
    }]
  ];

(* expectedFunctionNames — robust list-returning version. Walks every test
   input for a challenge, collects every Symbol used as a call head anywhere
   in the held expression, then subtracts:
     - locals declared by Module / With / Block / DynamicModule bindings
     - structural / wrapper heads (CompoundExpression, Show, Graphics, etc.)
   The remaining set is what the solution file must define one of.

   Why this is necessary: real test inputs look like
     Show[KnightPoints[...]]
     g = GraphData[...]; LongestGraphCycle[g]
     Module[{g, gPure}, ...; gPure = MakePureFunction[g]; gPure[1337]]
   The naive "head of HoldComplete[firstInput]" approach reports
   "Show", "CompoundExpression", and "Module" respectively. Returning a set
   of plausible candidates and matching with IntersectingQ side-steps that. *)

ChallengesBenchmark`Private`$candidateHeadDenylist = {
  "CompoundExpression", "Set", "SetDelayed", "UpSet", "UpSetDelayed",
  "TagSet", "TagSetDelayed", "AddTo", "SubtractFrom", "TimesBy", "DivideBy",
  "Module", "With", "Block", "DynamicModule",
  "Show", "Graphics", "Graphics3D", "GraphicsRow", "GraphicsGrid", "GraphicsColumn",
  "Framed", "Labeled", "Column", "Row", "Grid", "Panel", "Style", "Pane",
  "List", "Association", "Rule", "RuleDelayed", "Condition",
  "Hold", "HoldComplete", "HoldForm", "HoldPattern", "Unevaluated",
  "Slot", "Function", "Pattern", "Blank", "BlankSequence", "BlankNullSequence",
  "Optional", "Repeated", "RepeatedNull", "Verbatim"
};

ChallengesBenchmark`Private`candidateHeads[held_HoldComplete] := Module[{heads},
  heads = DeleteDuplicates @ Cases[
    held,
    (f_Symbol)[___] :> SymbolName[Unevaluated[f]],
    {0, Infinity}
  ];
  Select[heads, ! MemberQ[ChallengesBenchmark`Private`$candidateHeadDenylist, #] &]
];

ChallengesBenchmark`Private`localSymbolsOf[held_HoldComplete] := DeleteDuplicates @ Flatten @ Cases[
  held,
  (Module | With | Block | DynamicModule)[binding_, __] :>
    With[{heldBinding = HoldComplete[binding]},
      Cases[
        heldBinding,
        (s_Symbol | Verbatim[Set][s_Symbol, _]) :> SymbolName[Unevaluated[s]],
        {0, Infinity}
      ]
    ],
  {0, Infinity}
];

ChallengesBenchmark`Private`expectedFunctionNames[testBank_Association, name_String] :=
  Module[{tests, allHeads, allLocals},
    tests = Lookup[testBank, name, {}];
    If[! ListQ[tests] || Length[tests] === 0, Return[{}]];
    allHeads = DeleteDuplicates @ Flatten @ Map[
      ChallengesBenchmark`Private`candidateHeads[Lookup[#, "input", HoldComplete[Null]]] &,
      tests
    ];
    allLocals = DeleteDuplicates @ Flatten @ Map[
      ChallengesBenchmark`Private`localSymbolsOf[Lookup[#, "input", HoldComplete[Null]]] &,
      tests
    ];
    Complement[allHeads, allLocals]
  ];

(* definedFunctionNames — parse a WL source string and return the set of
   top-level function names it defines via SetDelayed / Set / TagSet /
   UpSetDelayed. Parsing is held (HeldExpressions) so no evaluation
   happens; the file could be adversarial. *)

(* definedFunctionNames — extract top-level function names from a WL
   source string WITHOUT ever evaluating any part of the file.

   Safety constraints:
   - We must not evaluate the AST. A file may contain e.g.
       MaxColorDistance[img_Image] := Total[img]
     where MaxColorDistance is a protected System` symbol; if we let the
     LHS evaluate, Mathematica raises SetDelayed::write and we risk
     installing side effects on live System` definitions.
   - We only want TOP-LEVEL definitions. `Module[{x = 1}, body]` contains
     an implicit Set[x, 1] that must NOT be reported as a definition,
     so we do not walk inside Module / With / Block etc.
   - A top-level ;-separated sequence is parsed as a single
     CompoundExpression, so we look one level inside that.

   Strategy:
     1. ImportString yields {HoldComplete[top1], HoldComplete[top2], ...}.
     2. A top-level assignment matches Cases at level spec {2, 3} on this
        list: depth 2 catches direct assignments; depth 3 catches
        assignments inside a top-level CompoundExpression.
     3. On match, we wrap the LHS in HoldComplete[...] so it never
        evaluates. A small helper (nameOfHeldLhs) then peels
        HoldPattern / Condition wrappers and reads the head symbol name.
     4. Condition on LHS (f[x_] /; x>0 := ...) is handled via
        Verbatim[Condition] — pattern optimizer would otherwise rewrite
        Condition[x_, _] as x_ /; _ at pattern-compile time.
*)

ChallengesBenchmark`Private`nameOfHeldLhs[HoldComplete[Verbatim[Condition][x_, _]]] :=
  ChallengesBenchmark`Private`nameOfHeldLhs[HoldComplete[x]];
ChallengesBenchmark`Private`nameOfHeldLhs[HoldComplete[HoldPattern[x_]]] :=
  ChallengesBenchmark`Private`nameOfHeldLhs[HoldComplete[x]];
ChallengesBenchmark`Private`nameOfHeldLhs[HoldComplete[(f_Symbol)[___]]] :=
  SymbolName[Unevaluated[f]];
ChallengesBenchmark`Private`nameOfHeldLhs[HoldComplete[f_Symbol]] :=
  SymbolName[Unevaluated[f]];
ChallengesBenchmark`Private`nameOfHeldLhs[_] := Nothing;
SetAttributes[ChallengesBenchmark`Private`nameOfHeldLhs, HoldAllComplete];

ChallengesBenchmark`Private`definedFunctionNames[code_String] := Module[
  {held, assignHeads, heldLhss, names},

  held = Quiet @ Check[ImportString[code, {"WL", "HeldExpressions"}], $Failed];
  If[held === $Failed || held === {} || held === Null, Return[{}]];

  assignHeads =
    {SetDelayed, Set, UpSetDelayed, UpSet, TagSetDelayed, TagSet};

  (* Collect HoldComplete-wrapped LHSs for every top-level assignment.
     Level {2, 3} never descends into Module/With/Block bodies (those live
     at depth 4+). The two Cases calls split on 2-arg (Set, SetDelayed,
     UpSet, UpSetDelayed) vs 3-arg (TagSet, TagSetDelayed) heads. *)
  heldLhss = Join[
    Cases[held,
      (h_Symbol)[lhs_, _] /; MemberQ[assignHeads, h]
        :> HoldComplete[lhs],
      {2, 3}
    ],
    Cases[held,
      (h_Symbol)[_, lhs_, _] /; MemberQ[assignHeads, h]
        :> HoldComplete[lhs],
      {2, 3}
    ]
  ];

  names = ChallengesBenchmark`Private`nameOfHeldLhs /@ heldLhss;
  DeleteDuplicates[Cases[names, _String]]
];

(* auditSolutionsImpl — audit a solutions directory against a test bank.
   Returns an Association:
     <|
       "ok"           -> True|False overall,
       "okCount"      -> n,
       "issueCount"   -> n,
       "missing"      -> {challenges with no .wl file on disk},
       "unexpected"   -> {.wl files with no corresponding challenge},
       "mismatches"   -> {<|"name" -> ..., "expected" -> ..., "defined" -> {...}|>, ...},
       "unparseable"  -> {challenges whose .wl wouldn't parse},
       "emptyCode"    -> {challenges whose .wl had no definitions},
       "byChallenge"  -> Association[name -> <|"status"->..., "defined"->...|>, ...]
     |>
*)

auditSolutionsImpl[dir_String, testBank_Association] := Module[
  {wlFiles, onDisk, expectedNames, challengesInBank, missing, unexpected,
   mismatches = {}, unparseable = {}, emptyCode = {}, byChallenge = <||>,
   okCount = 0},

  If[! DirectoryQ[dir],
    logWarn["AuditSolutions: solutions directory does not exist: " <> dir];
    Return[<|
      "ok" -> False, "okCount" -> 0, "issueCount" -> 1,
      "missing" -> {}, "unexpected" -> {}, "mismatches" -> {},
      "unparseable" -> {}, "emptyCode" -> {},
      "byChallenge" -> <||>,
      "error" -> "directory not found"
    |>]
  ];

  wlFiles = FileNames["*.wl", dir];
  onDisk  = FileBaseName /@ wlFiles;

  challengesInBank = Keys[testBank];
  missing    = Complement[challengesInBank, onDisk];
  unexpected = Complement[onDisk, challengesInBank];

  (* Audit each on-disk solution that corresponds to a bank entry. *)
  Scan[
    Function[wlPath,
      Module[{name, code, defined, expected, status},
        name     = FileBaseName[wlPath];
        If[! MemberQ[challengesInBank, name],
          Return[Null, Module]  (* handled by 'unexpected' list *)
        ];
        code     = Quiet @ Check[Import[wlPath, "Text"], ""];
        defined  = ChallengesBenchmark`Private`definedFunctionNames[code];
        expected = ChallengesBenchmark`Private`expectedFunctionNames[testBank, name];

        status = Which[
          code === "" || ! StringQ[code],
            AppendTo[unparseable, name]; "unparseable",
          defined === {},
            AppendTo[emptyCode, name]; "no-definitions",
          expected === {} || ! ListQ[expected],
            okCount++; "unknown-expected-ok",  (* can't judge, count as ok *)
          IntersectingQ[defined, expected],
            okCount++; "ok",
          True,
            AppendTo[mismatches,
              <|"name" -> name, "expected" -> expected, "defined" -> defined|>];
            "mismatch"
        ];

        byChallenge[name] = <|
          "status"   -> status,
          "expected" -> expected,
          "defined"  -> defined
        |>
      ]
    ],
    wlFiles
  ];

  <|
    "ok"          -> (Length[mismatches] === 0 && Length[unparseable] === 0
                      && Length[missing] === 0 && Length[emptyCode] === 0),
    "okCount"     -> okCount,
    "issueCount"  -> Length[mismatches] + Length[unparseable] +
                     Length[missing] + Length[emptyCode],
    "missing"     -> missing,
    "unexpected"  -> unexpected,
    "mismatches"  -> mismatches,
    "unparseable" -> unparseable,
    "emptyCode"   -> emptyCode,
    "byChallenge" -> byChallenge
  |>
];

End[];
