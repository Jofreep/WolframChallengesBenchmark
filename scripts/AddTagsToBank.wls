#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Merge data/challenge-tags.m into challenges.jsonl.

   For every record in challenges.jsonl, look up the matching tag
   entry by canonicalizing both task_id and the tag-source display
   name (NFKD-decompose + drop non-alphanumerics + lowercase) so
   "Vigen\[EGrave]re Cipher" maps to "VigenereCipher" cleanly.

   Adds a "tags" array of TagSlug strings to each record.  Records
   that find no tag-source entry get "tags" -> {} (with a logged
   warning so we can spot mismatches).

   Idempotent: re-running replaces the existing "tags" field rather
   than duplicating.

   Usage:
     wolframscript -file scripts/AddTagsToBank.wls \
       [--in     challenges.jsonl] \
       [--tags   data/challenge-tags.m] \
       [--out    challenges.jsonl]
*)

$scriptDir = DirectoryName[$InputFileName];
$repoRoot  = DirectoryName[$scriptDir];

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

(* canonicalize: NFKD-decompose, drop everything that isn't an ASCII
   letter or digit, lowercase.  Same logic as Loader.wl's
   canonicalNameKey so "Vigen\[EGrave]reCipher" and "VigenereCipher"
   collide cleanly. *)
canon[s_String] := ToLowerCase @ StringDelete[
  CharacterNormalize[s, "NFKD"],
  Except[LetterCharacter | DigitCharacter]
];

main[] := Module[
  {args, jsonlPath, tagsPath, outPath, tagSource, tagsByCanon,
   lines, records, augmented, missing, stream, hits = 0},

  args     = parseArgs[Drop[$ScriptCommandLine, 1]];
  jsonlPath = Lookup[args, "in",
                FileNameJoin[{$repoRoot, "challenges.jsonl"}]];
  tagsPath  = Lookup[args, "tags",
                FileNameJoin[{$repoRoot, "data", "challenge-tags.m"}]];
  outPath   = Lookup[args, "out", jsonlPath];

  If[! FileExistsQ[jsonlPath], abort[1, "challenges.jsonl not found: " <> jsonlPath]];
  If[! FileExistsQ[tagsPath],  abort[1, "tag source not found: "       <> tagsPath]];

  WriteString[$Output, "Reading bank from   ", jsonlPath, "\n"];
  WriteString[$Output, "Reading tags from   ", tagsPath,  "\n"];

  tagSource = Quiet @ Check[Get[tagsPath], $Failed];
  If[! AssociationQ[tagSource],
    abort[2, "tag source did not parse as an Association of display name -> <|TagData -> ...|>"]];

  tagsByCanon = Association @ KeyValueMap[
    Function[{name, info},
      canon[name] -> (Lookup[#, "TagSlug"] & /@ Lookup[info, "TagData", {}])
    ],
    tagSource
  ];

  WriteString[$Output, "Tag source has ", Length[tagsByCanon],
    " challenges; bank has "];

  lines   = ReadList[jsonlPath, "String"];
  records = DeleteCases[Quiet @ ImportString[#, "RawJSON"] & /@ lines, $Failed];
  WriteString[$Output, Length[records], " records.\n\n"];

  missing = {};
  augmented = Map[
    Function[r,
      Module[{tags = Lookup[tagsByCanon, canon[r["task_id"]], None]},
        If[tags === None,
          AppendTo[missing, r["task_id"]];
          tags = {}
        , (* else *)
          hits++
        ];
        Append[r, "tags" -> tags]
      ]
    ],
    records
  ];

  WriteString[$Output, "Matched ", hits, " / ", Length[records],
    " records to tags.\n"];
  If[missing =!= {},
    WriteString[$Output, "Unmatched task_ids (no tag entry):\n"];
    Scan[WriteString[$Output, "  ", #, "\n"] &, Take[missing, UpTo[20]]];
    If[Length[missing] > 20,
      WriteString[$Output, "  \[Ellipsis] (", Length[missing] - 20, " more)\n"]];
  ];

  WriteString[$Output, "\nWriting ", outPath, "\n"];
  stream = OpenWrite[outPath, CharacterEncoding -> "UTF-8"];
  Scan[
    Function[r,
      WriteString[stream, ExportString[r, "RawJSON", "Compact" -> True], "\n"]
    ],
    augmented
  ];
  Close[stream];

  WriteString[$Output, "Done.\n"];
  Exit[0]
];

main[]
