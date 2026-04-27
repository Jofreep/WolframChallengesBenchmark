#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Migrate the legacy (ChallengesTestDataV1.json, ChallengesTests.wxf)
   pair into the standard LLM-benchmark-shaped layout:

     challenges.jsonl                  -- public, committed
     private/canonical_solutions.jsonl -- private, gitignored

   Format mirrors HumanEval / MBPP / BigCodeBench: one JSON record per
   line, fields task_id / name / index / instruction / prompt /
   entry_point / tests, with `tests` an array of {input_wl,
   expected_wl, metadata} triples.  Held WL is serialized as a string
   that round-trips through ImportString[..., {"WL", "HeldExpressions"}].

   Usage:
     wolframscript -file scripts/MigrateToJSONL.wls
       [--in-json ChallengesTestDataV1.json]
       [--in-wxf  ChallengesTests.wxf]
       [--out-jsonl challenges.jsonl]
       [--out-private private/canonical_solutions.jsonl]
       [--dry-run]

   Exits 0 on success, 1 on argument or IO error, 2 on shape mismatch.
*)

$scriptDir = DirectoryName[$InputFileName];
$repoRoot  = DirectoryName[$scriptDir];
$pacletDir = FileNameJoin[{$repoRoot, "WolframChallengesLLMBenchmarkPaclet"}];

Quiet @ PacletDirectoryLoad[$pacletDir];
Needs["JofreEspigulePons`WolframChallengesBenchmark`"];


(* ----------------------------------------------------------------------- *)
(* Tiny CLI parser                                                         *)
(* ----------------------------------------------------------------------- *)

parseArgs[args_List] := Module[{i = 1, n = Length[args], out = <||>, k, v},
  While[i <= n,
    k = args[[i]];
    If[StringStartsQ[k, "--"],
      v = If[i < n && ! StringStartsQ[args[[i + 1]], "--"],
             args[[i + 1]], "true"];
      out[StringDrop[k, 2]] = v;
      i += If[v === "true", 1, 2],
      i++
    ]
  ];
  out
];

abort[code_, msg_] := (WriteString[$Output, "ERROR: ", msg, "\n"]; Exit[code]);


(* ----------------------------------------------------------------------- *)
(* Held -> source-string serializer                                        *)
(*                                                                         *)
(* The test bank's `input` field is HoldComplete[expr]. We render expr    *)
(* in InputForm WITHOUT evaluating it (HoldForm wrapper) and strip the    *)
(* HoldForm[...] wrapper from the resulting string. The resulting string  *)
(* re-parses via ImportString[..., {"WL", "HeldExpressions"}] back to     *)
(* HoldComplete[expr], so the round-trip is lossless at the kernel layer  *)
(* (modulo the orderless-head InputForm artifact noted in TestBankBuilder *)
(* -- a*b*c reparses as Times[Times[a,b],c] but ReleaseHold yields the    *)
(* same value).                                                            *)
(* ----------------------------------------------------------------------- *)

stripHoldForm[s_String] := StringReplace[s,
  StartOfString ~~ "HoldForm[" ~~ body__ ~~ "]" ~~ EndOfString :> body,
  1
];

heldToInputForm[HoldComplete[expr_]] := stripHoldForm @
  ToString[HoldForm[expr], InputForm, PageWidth -> Infinity];
heldToInputForm[other_] := ToString[other, InputForm, PageWidth -> Infinity];

valueToInputForm[v_] := ToString[v, InputForm, PageWidth -> Infinity];


(* ----------------------------------------------------------------------- *)
(* Main                                                                    *)
(* ----------------------------------------------------------------------- *)

main[] := Module[
  {args, jsonPath, wxfPath, jsonlPath, privatePath, dryRun,
   challengesRaw, testBankRaw, names, records, privateRecs,
   nMissingTests, nMissingPrompt, nWritten},

  args        = parseArgs[Drop[$ScriptCommandLine, 1]];
  jsonPath    = FileNameJoin[{$repoRoot,
    Lookup[args, "in-json", "ChallengesTestDataV1.json"]}];
  wxfPath     = FileNameJoin[{$repoRoot,
    Lookup[args, "in-wxf",  "ChallengesTests.wxf"]}];
  jsonlPath   = FileNameJoin[{$repoRoot,
    Lookup[args, "out-jsonl", "challenges.jsonl"]}];
  privatePath = FileNameJoin[{$repoRoot,
    Lookup[args, "out-private", "private/canonical_solutions.jsonl"]}];
  dryRun      = KeyExistsQ[args, "dry-run"];

  If[! FileExistsQ[jsonPath], abort[1, "challenges JSON not found: " <> jsonPath]];
  If[! FileExistsQ[wxfPath],  abort[1, "test bank WXF not found: "   <> wxfPath]];

  WriteString[$Output, "Reading ",   jsonPath, " ...\n"];
  challengesRaw = Quiet @ Check[Import[jsonPath, "RawJSON"], $Failed];
  If[! ListQ[challengesRaw], abort[2, "challenges JSON did not parse as a list"]];

  WriteString[$Output, "Reading ", wxfPath, " ...\n"];
  testBankRaw = Quiet @ Check[Import[wxfPath, "WXF"], $Failed];
  If[! AssociationQ[testBankRaw],
    abort[2, "test bank WXF did not parse as an association"]];

  (* Use the test bank's keys as the canonical set of task_ids. The JSON
     order is preserved by index. We reconcile names via ReconcileNames so
     a "VigenèreCipher" prompt aligns with a "VigenereCipher" bank key. *)
  Module[{recon, jsonAsAssoc, byName},
    jsonAsAssoc =
      JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges[jsonPath];
    recon = JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames[
      jsonAsAssoc, testBankRaw];
    byName = recon["challenges"];
    names = Sort[Keys[testBankRaw], (* sort by index from byName *)
      Module[{ai, bi},
        ai = Lookup[Lookup[byName, #1, <||>], "index", 99999];
        bi = Lookup[Lookup[byName, #2, <||>], "index", 99999];
        ai < bi
      ] &
    ];

    nMissingPrompt = 0;
    nMissingTests  = 0;

    records = Map[
      Function[task,
        Module[{prompt, instruction, idx, tests, entryPoint, rec, byTb},
          byTb = Lookup[byName, task, <||>];
          prompt      = Lookup[byTb, "prompt", Missing["NoPrompt"]];
          instruction = Lookup[byTb, "instruction", ""];
          idx         = Lookup[byTb, "index", 99999];
          If[MissingQ[prompt],
            nMissingPrompt += 1;
            prompt = "[no prompt for " <> task <> "]"
          ];

          tests = Lookup[testBankRaw, task, {}];
          If[tests === {} || ! ListQ[tests],
            nMissingTests += 1;
            tests = {}
          ];

          (* Each test in the bank is either:
             {HoldComplete[input], expected}            (2-tuple)
             {HoldComplete[input], expected, metadata}  (3-tuple)
             ... so read positionally. *)
          tests = Map[
            Function[t,
              <|
                "input_wl"    -> heldToInputForm[t[[1]]],
                "expected_wl" -> valueToInputForm[t[[2]]],
                "metadata"    -> If[Length[t] >= 3 && AssociationQ[t[[3]]],
                                    t[[3]], <||>]
              |>
            ],
            tests
          ];

          (* entry_point: peek at the first held input head. We can't ask
             expectedFunctionNames here without re-loading the bank
             (chicken-and-egg) so just take the first call's head. *)
          entryPoint = Replace[
            Lookup[testBankRaw, task, {}][[1, 1]],
            {
              HoldComplete[(f_Symbol)[___]] :> SymbolName[Unevaluated[f]],
              HoldComplete[f_Symbol]        :> SymbolName[Unevaluated[f]],
              _                             :> ""
            }
          ];

          rec = <|
            "task_id"     -> task,
            "name"        -> task,
            "index"       -> idx,
            "instruction" -> instruction,
            "prompt"      -> prompt,
            "entry_point" -> entryPoint,
            "tests"       -> tests
          |>;
          rec
        ]
      ],
      names
    ];

    privateRecs = Map[
      Function[task,
        Module[{outRec},
          outRec = SelectFirst[challengesRaw,
            Module[{recName},
              recName = Lookup[#, "name",
                Lookup[#, "challengeName",
                  StringReplace[
                    First @ StringSplit[Lookup[#, "input", ""], "\n"],
                    RegularExpression["\\s+"] -> ""]]];
              recName === task
            ] &,
            <||>];
          <|
            "task_id"            -> task,
            "canonical_solution" -> Lookup[outRec, "output", ""]
          |>
        ]
      ],
      names
    ];
  ];

  WriteString[$Output, "Reconciliation summary:\n"];
  WriteString[$Output, "  challenges in test bank: ", Length[records], "\n"];
  WriteString[$Output, "  records missing prompt:  ", nMissingPrompt, "\n"];
  WriteString[$Output, "  records missing tests:   ", nMissingTests,  "\n"];

  If[dryRun,
    WriteString[$Output, "\nDry run - not writing.\n"];
    WriteString[$Output, "First record (preview):\n"];
    WriteString[$Output,
      ExportString[records[[1]], "RawJSON",
        "Compact" -> False, "ConversionRules" -> {Null -> ""}], "\n"];
    Exit[0]
  ];

  (* Write challenges.jsonl: one record per line. ExportString with
     "RawJSON", "Compact" -> True gives compact single-line output. *)
  WriteString[$Output, "\nWriting ", jsonlPath, " (", Length[records],
    " records) ...\n"];
  Module[{stream},
    stream = OpenWrite[jsonlPath, CharacterEncoding -> "UTF-8"];
    Scan[
      Function[r,
        WriteString[stream,
          ExportString[r, "RawJSON", "Compact" -> True], "\n"]
      ],
      records
    ];
    Close[stream];
  ];
  nWritten = Length[records];

  (* Write private canonical solutions. *)
  Module[{privDir = DirectoryName[privatePath]},
    If[StringLength[privDir] > 0 && ! DirectoryQ[privDir],
      CreateDirectory[privDir, CreateIntermediateDirectories -> True]
    ]
  ];
  WriteString[$Output, "Writing ", privatePath, " (", Length[privateRecs],
    " canonical solutions) ...\n"];
  Module[{stream2},
    stream2 = OpenWrite[privatePath, CharacterEncoding -> "UTF-8"];
    Scan[
      Function[r,
        WriteString[stream2,
          ExportString[r, "RawJSON", "Compact" -> True], "\n"]
      ],
      privateRecs
    ];
    Close[stream2];
  ];

  WriteString[$Output, "\nDone. Wrote ", nWritten, " challenges.\n"];
  WriteString[$Output, "  public:  ", jsonlPath,   "\n"];
  WriteString[$Output, "  private: ", privatePath, "\n"];
  WriteString[$Output,
    "Remember: ensure 'private/' is in .gitignore before committing.\n"];
];

main[];
Exit[0]
