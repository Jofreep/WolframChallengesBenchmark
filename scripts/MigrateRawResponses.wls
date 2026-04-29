#!/usr/bin/env wolframscript
(* ::Package:: *)

(* MigrateRawResponses.wls

   One-shot migrator: moves top-level *.raw.txt /
   *.audit-rejected.raw.txt / *.failed.raw.txt files into the new
   solutions/<model>/raw/ subdir layout written by the updated
   generator.

   Idempotent: if a destination file already exists with identical
   bytes, the source is just deleted.  If contents differ, the source
   is left in place and a warning is printed (manual triage).

   Usage:
     wolframscript -file scripts/MigrateRawResponses.wls
       [--solutions solutions]   root containing per-model dirs
       [--dry-run]               just print what would move
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
info[msg_]         := WriteString[$Output, msg, "\n"];
warn[msg_]         := WriteString[$Output, "  WARN: ", msg, "\n"];

main[] := Module[
  {args, solutionsRoot, dryRun, modelDirs, totalMoved = 0,
   totalSkipped = 0, totalConflicts = 0},

  args          = parseArgs[Drop[$ScriptCommandLine, 1]];
  solutionsRoot = Lookup[args, "solutions",
                    FileNameJoin[{$repoRoot, "solutions"}]];
  dryRun        = KeyExistsQ[args, "dry-run"];

  If[! DirectoryQ[solutionsRoot],
    abort[1, "no such dir: " <> solutionsRoot]];

  modelDirs = Select[
    FileNames["*", solutionsRoot],
    DirectoryQ[#] && FileBaseName[#] =!= "raw" &];

  info["Migrating raw-response files in " <> solutionsRoot
       <> If[dryRun, "  [dry-run]", ""]];
  info[""];

  Scan[
    Function[modelDir,
      Module[{rawFiles, rawDir},
        rawFiles = FileNames["*.raw.txt", modelDir];   (* top-level only *)
        If[rawFiles === {}, Return[Null, Module]];

        info[Last[FileNameSplit[modelDir]] <> ":  "
             <> ToString[Length[rawFiles]] <> " raw file"
             <> If[Length[rawFiles] == 1, "", "s"]];

        rawDir = FileNameJoin[{modelDir, "raw"}];
        If[! dryRun && ! DirectoryQ[rawDir],
          CreateDirectory[rawDir, CreateIntermediateDirectories -> True]];

        Scan[
          Function[src,
            Module[{dst, action},
              dst = FileNameJoin[{rawDir, FileNameTake[src]}];
              action = Which[
                FileExistsQ[dst] && Hash[Import[src, "Text"], "MD5"]
                  === Hash[Import[dst, "Text"], "MD5"],
                  "duplicate",
                FileExistsQ[dst],
                  "conflict",
                True,
                  "move"
              ];

              Switch[action,
                "move",
                  info["  -> " <> FileNameTake[src]];
                  If[! dryRun,
                    CopyFile[src, dst, OverwriteTarget -> False];
                    DeleteFile[src]];
                  totalMoved++,
                "duplicate",
                  (* Identical content already in raw/.  Just drop the
                     stale top-level copy. *)
                  info["  =  " <> FileBaseName[src] <> ".raw.txt (duplicate, removing source)"];
                  If[! dryRun, DeleteFile[src]];
                  totalSkipped++,
                "conflict",
                  warn[src <> " -> " <> dst <>
                       ": destination differs; left in place"];
                  totalConflicts++
              ]
            ]
          ],
          rawFiles
        ];
      ]
    ],
    modelDirs
  ];

  info[""];
  info["=== summary ==="];
  info["  moved:        " <> ToString[totalMoved]];
  info["  duplicates:   " <> ToString[totalSkipped]];
  info["  conflicts:    " <> ToString[totalConflicts]];
  If[dryRun, info["  (dry-run; no files actually changed)"]];

  Exit[If[totalConflicts > 0, 2, 0]]
];

main[]
