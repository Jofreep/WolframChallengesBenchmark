(* ::Package:: *)

(* :Context: JofreEspigulePons`WolframChallengesBenchmark`Private`          *)
(* :Summary:
     On-disk storage of per-model candidate solutions, with a write-time
     audit against the test bank that refuses to persist code that doesn't
     define the expected function.

     Layout:
       solutions/<modelSlug>/<challengeName>.wl        \[Dash] WL source
       solutions/<modelSlug>/<challengeName>.meta.json \[Dash] sidecar metadata

     The .meta.json sidecar always carries:
       { "model", "challengeName", "sourceHash",
         "generatedAt", "extractor" }

     Callers supplying extraMeta get every extra key merged in AFTER the
     defaults, so keys like "promptHash", "llm", "usage", "generationId",
     "attempts" land in the sidecar without displacing built-in
     bookkeeping.

     LoadSolutions[modelDir] reads the .wl files (and any sibling
     .meta.json) back into the Association shape RunBenchmark consumes:
       <| name -> <|"code" -> "...", "wlPath" -> "...",
                    "metaPath" -> "...", "meta" -> <|...|>|> |>
*)

Begin["JofreEspigulePons`WolframChallengesBenchmark`Private`"];


(* Message attached to the PUBLIC SaveSolution symbol so users see a      *)
(* familiar SaveSolution::saveAudit in their notebook, not an internal   *)
(* Private symbol name.                                                   *)

JofreEspigulePons`WolframChallengesBenchmark`SaveSolution::saveAudit =
"SaveSolution refused to write `1`: `2`. Pass testBank -> None to bypass.";


(* Public messages for LoadSolutions.                                     *)

JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions::nodir =
"LoadSolutions: directory not found: `1`.";

JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions::badarg =
"LoadSolutions expected a directory path String; got `1`.";

JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions::skip =
"LoadSolutions: skipping `1`: `2`.";


(* ------------------------------------------------------------------ *)
(* Held-AST inspectors                                                *)
(* ------------------------------------------------------------------ *)

(* nameOfHeldLhs — given a HoldComplete of a LHS, peel Condition /
   HoldPattern and return the head Symbol's name as a String. Never
   evaluates anything.                                               *)

SetAttributes[nameOfHeldLhs, HoldAllComplete];

nameOfHeldLhs[HoldComplete[Verbatim[Condition][x_, _]]] :=
  nameOfHeldLhs[HoldComplete[x]];
nameOfHeldLhs[HoldComplete[HoldPattern[x_]]] :=
  nameOfHeldLhs[HoldComplete[x]];
nameOfHeldLhs[HoldComplete[(f_Symbol)[___]]] :=
  SymbolName[Unevaluated[f]];
nameOfHeldLhs[HoldComplete[f_Symbol]] :=
  SymbolName[Unevaluated[f]];
nameOfHeldLhs[_] := Nothing;


(* definedFunctionNames — parse WL source (held) and return the set of
   top-level function names it defines via Set / SetDelayed / TagSet /
   TagSetDelayed / UpSet / UpSetDelayed. Level spec {2,3} keeps us at
   the top level and inside a single CompoundExpression, but never
   inside Module / With / Block bindings.                            *)

definedFunctionNames[code_String] := Module[
  {held, assignHeads, heldLhss, names},

  held = Quiet @ Check[
    ImportString[code, {"WL", "HeldExpressions"}],
    $Failed
  ];
  If[held === $Failed || held === {} || held === Null, Return[{}]];

  assignHeads =
    {SetDelayed, Set, UpSetDelayed, UpSet, TagSetDelayed, TagSet};

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

  names = nameOfHeldLhs /@ heldLhss;
  DeleteDuplicates[Cases[names, _String]]
];

definedFunctionNames[_] := {};


(* expectedFunctionNames — walk every test input for a challenge, collect
   every Symbol used as a call head, subtract locals introduced by Module
   / With / Block / DynamicModule bindings, and subtract a deny-list of
   wrapper/structural heads (Show, Graphics, CompoundExpression, ...).
   The remaining set is what the candidate code must define one of.    *)

$candidateHeadDenylist = {
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

candidateHeads[held_HoldComplete] := Module[{heads},
  heads = DeleteDuplicates @ Cases[
    held,
    (f_Symbol)[___] :> SymbolName[Unevaluated[f]],
    {0, Infinity}
  ];
  Select[heads, ! MemberQ[$candidateHeadDenylist, #] &]
];

localSymbolsOf[held_HoldComplete] := DeleteDuplicates @ Flatten @ Cases[
  held,
  (Module | With | Block | DynamicModule)[binding_, __] :>
    With[{heldBinding = HoldComplete[binding]},
      Cases[heldBinding,
        (s_Symbol | Verbatim[Set][s_Symbol, _]) :> SymbolName[Unevaluated[s]],
        {0, Infinity}
      ]
    ],
  {0, Infinity}
];

expectedFunctionNames[testBank_Association, name_String] := Module[
  {tests, allHeads, allLocals},
  tests = Lookup[testBank, name, {}];
  If[! ListQ[tests] || Length[tests] === 0, Return[{}]];
  allHeads = DeleteDuplicates @ Flatten @ Map[
    candidateHeads[Lookup[#, "input", HoldComplete[Null]]] &,
    tests
  ];
  allLocals = DeleteDuplicates @ Flatten @ Map[
    localSymbolsOf[Lookup[#, "input", HoldComplete[Null]]] &,
    tests
  ];
  Complement[allHeads, allLocals]
];

expectedFunctionNames[___] := {};


(* ------------------------------------------------------------------ *)
(* saveSolutionImpl                                                   *)
(* ------------------------------------------------------------------ *)

saveSolutionImpl[dir_String, name_String, code_String,
    testBank_, extraMeta_] :=
Module[
  {slug, wlPath, metaPath, hash, meta, defined, expected, auditOk, extras},

  (* Write-time audit. Skipped when testBank is None or when the bank
     has no entry for `name` (caller knows what they're doing). *)
  If[AssociationQ[testBank] && KeyExistsQ[testBank, name],
    defined  = definedFunctionNames[code];
    expected = expectedFunctionNames[testBank, name];
    auditOk = Which[
      defined === {},
        Message[
          JofreEspigulePons`WolframChallengesBenchmark`SaveSolution::saveAudit,
          name, "no top-level definitions found"];
        False,
      expected =!= {} && ! IntersectingQ[defined, expected],
        Message[
          JofreEspigulePons`WolframChallengesBenchmark`SaveSolution::saveAudit,
          name,
          "code defines " <> ToString[defined] <>
          " but the test bank expects one of " <> ToString[expected]];
        False,
      True, True
    ];
    If[! auditOk, Return[$Failed]]
  ];

  If[! DirectoryQ[dir],
    CreateDirectory[dir, CreateIntermediateDirectories -> True]
  ];

  slug     = safeSlug[name];
  wlPath   = FileNameJoin[{dir, slug <> ".wl"}];
  metaPath = FileNameJoin[{dir, slug <> ".meta.json"}];
  hash     = Hash[code, "SHA256", "HexString"];
  extras   = If[AssociationQ[extraMeta], extraMeta, <||>];

  meta = Join[
    <|
      "model"         -> FileNameTake[dir],
      "challengeName" -> name,
      "sourceHash"    -> "sha256:" <> hash,
      "generatedAt"   -> isoUTC[],
      "extractor"     -> "ExtractCode/v1"
    |>,
    extras
  ];

  Export[wlPath,   code, "Text",    CharacterEncoding -> "UTF-8"];
  Export[metaPath, meta, "RawJSON"];
  wlPath
];

saveSolutionImpl[___] := $Failed;


(* ------------------------------------------------------------------ *)
(* loadSolutionsImpl                                                  *)
(*                                                                    *)
(* Walks <modelDir> for *.wl files and reassembles them into the      *)
(* Association shape RunBenchmark expects:                            *)
(*                                                                    *)
(*   <| name -> <|"code" -> <wl source>,                              *)
(*                "wlPath" -> <abs path>,                              *)
(*                "metaPath" -> <abs path or Missing[..]>,             *)
(*                "meta" -> <|...|> or <||> if no sidecar |> |>       *)
(*                                                                    *)
(* Sibling .meta.json sidecars are optional (a hand-placed solution   *)
(* may not have one), but when present they are parsed and surfaced   *)
(* under "meta" so callers can read latency, finishReason, etc.       *)
(*                                                                    *)
(* Files that error on read are individually skipped with a tagged    *)
(* LoadSolutions::skip message; the rest of the directory still       *)
(* loads.  Returns $Failed only when the directory itself is invalid. *)
(* ------------------------------------------------------------------ *)

loadSolutionsImpl[modelDir_String] := Module[
  {wlFiles, entries},

  If[! DirectoryQ[modelDir],
    Message[
      JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions::nodir,
      modelDir];
    Return[$Failed]
  ];

  wlFiles = FileNames["*.wl", modelDir];

  entries = Map[
    Function[wlPath,
      Module[{name, code, metaPath, meta},
        name = FileBaseName[wlPath];
        code = Quiet @ Check[Import[wlPath, "Text"], $Failed];
        If[! StringQ[code],
          Message[
            JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions::skip,
            name, "could not read .wl file"];
          Return[Nothing, Module]
        ];
        metaPath = FileNameJoin[{modelDir, name <> ".meta.json"}];
        meta = If[FileExistsQ[metaPath],
          Module[{m},
            m = Quiet @ Check[Import[metaPath, "RawJSON"], $Failed];
            If[AssociationQ[m], m, <||>]
          ],
          <||>
        ];
        name -> <|
          "code"     -> code,
          "wlPath"   -> wlPath,
          "metaPath" -> If[FileExistsQ[metaPath], metaPath, Missing["NoSidecar"]],
          "meta"     -> meta
        |>
      ]
    ],
    wlFiles
  ];

  Association @ entries
];

loadSolutionsImpl[other_] := (
  Message[
    JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions::badarg,
    other];
  $Failed
);


(* ----------------------------------------------------------------------- *)
(* auditSolutionsImpl \[LongDash] pre-flight consistency check for a        *)
(* solutions directory against a test bank.                                 *)
(*                                                                          *)
(* Cross-checks each solutions/<model>/<name>.wl file against the expected  *)
(* function name derived from the test bank's first held input.  The parse  *)
(* is structural (held) so reading a file never installs definitions or     *)
(* triggers side effects.                                                   *)
(*                                                                          *)
(* Returns an Association:                                                  *)
(*   <|                                                                     *)
(*     "ok"           -> True|False overall,                                *)
(*     "okCount"      -> n,                                                 *)
(*     "issueCount"   -> n,                                                 *)
(*     "missing"      -> {challenges with no .wl on disk},                  *)
(*     "unexpected"   -> {.wl files without a matching challenge},          *)
(*     "mismatches"   -> {<|"name","expected","defined"|>, ...},            *)
(*     "unparseable"  -> {challenges whose .wl wouldn't parse},             *)
(*     "emptyCode"    -> {challenges whose .wl had no definitions},         *)
(*     "byChallenge"  -> <|name -> <|"status", "expected", "defined"|>|>    *)
(*   |>                                                                     *)
(* ----------------------------------------------------------------------- *)

auditSolutionsImpl[dir_String, testBank_Association] := Module[
  {wlFiles, onDisk, challengesInBank, missing, unexpected,
   mismatches = {}, unparseable = {}, emptyCode = {}, byChallenge = <||>,
   okCount = 0},

  If[! DirectoryQ[dir],
    logWarn["AuditSolutions: solutions directory does not exist: " <> dir];
    Return[<|
      "ok"          -> False,
      "okCount"     -> 0,
      "issueCount"  -> 1,
      "missing"     -> {},
      "unexpected"  -> {},
      "mismatches"  -> {},
      "unparseable" -> {},
      "emptyCode"   -> {},
      "byChallenge" -> <||>,
      "error"       -> "directory not found"
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
        name = FileBaseName[wlPath];
        If[! MemberQ[challengesInBank, name],
          Return[Null, Module]  (* handled by 'unexpected' list *)
        ];
        code     = Quiet @ Check[Import[wlPath, "Text"], ""];
        defined  = definedFunctionNames[code];
        expected = expectedFunctionNames[testBank, name];

        status = Which[
          code === "" || ! StringQ[code],
            AppendTo[unparseable, name]; "unparseable",
          defined === {},
            AppendTo[emptyCode, name]; "no-definitions",
          expected === {} || ! ListQ[expected],
            okCount++; "unknown-expected-ok",
          IntersectingQ[defined, expected],
            okCount++; "ok",
          True,
            AppendTo[mismatches,
              <|"name"     -> name,
                "expected" -> expected,
                "defined"  -> defined|>];
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
    "ok"          -> (Length[mismatches]  === 0 &&
                      Length[unparseable] === 0 &&
                      Length[missing]     === 0 &&
                      Length[emptyCode]   === 0),
    "okCount"     -> okCount,
    "issueCount"  -> Length[mismatches] + Length[unparseable] +
                     Length[missing]    + Length[emptyCode],
    "missing"     -> missing,
    "unexpected"  -> unexpected,
    "mismatches"  -> mismatches,
    "unparseable" -> unparseable,
    "emptyCode"   -> emptyCode,
    "byChallenge" -> byChallenge
  |>
];

auditSolutionsImpl[dir_, testBank_] := (
  Message[JofreEspigulePons`WolframChallengesBenchmark`AuditSolutions::badarg,
    dir, testBank];
  $Failed
);


End[];  (* `Private` *)
