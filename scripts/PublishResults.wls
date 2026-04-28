#!/usr/bin/env wolframscript
(* ::Package:: *)

(* PublishResults.wls

   Extracts safe-to-publish benchmark metadata from local runs/ into
   data/results/<modelSlug>.json so the dashboards can be rebuilt on
   any checkout (CI, fresh clone, etc.) WITHOUT requiring access to
   results.wxf or to model-generated solution source code.

   Sources scanned (per run):
     runs/run-*/run.json         summary, perChallenge counts, runtime
     runs/run-*/progress.jsonl   per-test status / passed / durationSec

   NEITHER file contains LLM-generated solution source code.  This
   script never reads results.wxf and never opens any .wl under
   solutions/, so there is no path by which generated code can leak
   into the published JSON.

   Output (per model):
     data/results/<modelSlug>.json
       {
         "schemaVersion": 1,
         "model": "anthropic/claude-opus-4.6",
         "modelSlug": "anthropic_claude-opus-4.6",
         "publishedAt": "2026-04-28T16:30:00Z",
         "runs": [
           {
             "runId": "run-2026-04-28_145552-8ca911",
             "createdAt": "...", "finishedAt": "...",
             "durationSec": 2093.9,
             "summary": { total, passed, failed, ..., passRate,
                          challengesAttempted, challengesFullyPassing,
                          perChallenge: { TaskName: <int passed> } },
             "perTest": [
               { "challenge": "AliquotSequence", "testIndex": 1,
                 "passed": true, "status": "Evaluated",
                 "durationSec": 0.000107 },
               ...
             ]
           },
           ...
         ]
       }

   Idempotent: re-running with the same inputs yields the same output.
   Existing per-model files are merged (replaced-by-runId then re-sorted
   by createdAt), not overwritten, so a second run that adds one new
   model run preserves the model's history.

   Usage:
     wolframscript -file scripts/PublishResults.wls
       [--runs runs]            scan dir (default: <repo>/runs)
       [--out  data/results]    output dir (default: <repo>/data/results)
       [--run  <runDir>]        publish only this one run (default: all)
       [--quiet]                suppress per-run progress lines

   Exit codes:
     0  success (zero or more runs processed)
     2  IO / argument error

*)

$scriptDir = DirectoryName[$InputFileName];
$repoRoot  = DirectoryName[$scriptDir];

(* ----------------------------------------------------------------------- *)
(* Arg parsing / logging                                                   *)
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

$quiet = False;
abort[code_, msg_] := (WriteString[$Output, "ERROR: ", msg, "\n"]; Exit[code]);
warn[msg_]         := WriteString[$Output, "  WARN: ", msg, "\n"];
info[msg_]         := If[! $quiet, WriteString[$Output, msg, "\n"]];


(* ----------------------------------------------------------------------- *)
(* Filesystem helpers                                                      *)
(* ----------------------------------------------------------------------- *)

modelSlug[model_String] :=
  StringReplace[model, {"/" -> "_", ":" -> "_", " " -> "_", "\\" -> "_"}];
modelSlug[other_] := modelSlug[ToString[other]];

(* Discover runs/run-*/run.json paths, excluding generated-dashboard
   subdirs, leftover bank-self-test runs, etc.  Returns a list of run
   directories (NOT run.json paths). *)
discoverRunDirs[runsBase_String] := Module[{paths},
  If[! DirectoryQ[runsBase],
    warn[runsBase <> " does not exist"]; Return[{}]];
  paths = FileNames["run.json", runsBase, Infinity];
  paths = Select[paths,
    ! StringContainsQ[#, "/bank-self-test/"]   &&
    ! StringContainsQ[#, "/leaderboard/"]      &&
    ! StringContainsQ[#, "/trend/"]            &&
    ! StringContainsQ[#, "/bank-quality/"]     &&
    ! StringContainsQ[#, "/model-strengths/"]  &&
    ! StringContainsQ[#, "/compare/"]          &];
  DirectoryName /@ paths
];


(* ----------------------------------------------------------------------- *)
(* Per-test extraction from progress.jsonl                                 *)
(*                                                                         *)
(* progress.jsonl event types: run.start | test.submit | test.complete |   *)
(* run.end.  We only consume test.complete records and pull a fixed,       *)
(* hand-chosen field set:                                                  *)
(*   testId   - "ChallengeName/<int>"                                      *)
(*   passed   - bool                                                       *)
(*   status   - "Evaluated" / "TimedOut" / "MemoryExceeded" / ...          *)
(*   durationSec                                                           *)
(* No source code, no evaluation output, no canonical answer.              *)
(* ----------------------------------------------------------------------- *)

parseTestId[tid_String] := Module[{parts, idx},
  parts = StringSplit[tid, "/"];
  idx = If[Length[parts] >= 2,
        Quiet @ Check[ToExpression[parts[[2]]], parts[[2]]],
        0];
  {First[parts, ""], idx}
];

loadPerTest[runDir_String] := Module[{path, lines, parsed, completes},
  path = FileNameJoin[{runDir, "progress.jsonl"}];
  If[! FileExistsQ[path], Return[{}]];

  lines = Quiet @ Check[Import[path, "Lines"], $Failed];
  If[! ListQ[lines], Return[{}]];

  parsed = Map[
    Quiet @ Check[Developer`ReadRawJSONString[#], $Failed] &,
    lines];
  completes = Cases[parsed, KeyValuePattern["event" -> "test.complete"]];

  Map[
    Function[r,
      Module[{tid = Lookup[r, "testId", ""], ch, idx},
        {ch, idx} = parseTestId[tid];
        <|
          "challenge"   -> ch,
          "testIndex"   -> idx,
          "passed"      -> TrueQ[Lookup[r, "passed", False]],
          "status"      -> Lookup[r, "status", "Unknown"],
          "durationSec" -> N @ Lookup[r, "durationSec", 0.]
        |>
      ]
    ],
    completes
  ]
];


(* ----------------------------------------------------------------------- *)
(* Run extraction                                                          *)
(* ----------------------------------------------------------------------- *)

extractRun[runDir_String] := Module[
  {rj, perTest, status, runId, model},
  rj = Quiet @ Check[
    Import[FileNameJoin[{runDir, "run.json"}], "RawJSON"], $Failed];
  If[! AssociationQ[rj],
    warn["unreadable run.json in " <> runDir];
    Return[Missing[]]];

  status = Lookup[rj, "status", ""];
  runId  = Lookup[rj, "runId", "?"];
  model  = Lookup[rj, "model", "?"];

  If[status =!= "completed",
    warn[runId <> " status=" <> status <> "; skipping"];
    Return[Missing[]]];

  perTest = loadPerTest[runDir];

  info["  + " <> runId <> "  (" <> model
       <> ", " <> ToString[Length[perTest]] <> " tests)"];

  <|
    "runId"        -> runId,
    "model"        -> model,
    "createdAt"    -> Lookup[rj, "createdAt",  "?"],
    "finishedAt"   -> Lookup[rj, "finishedAt", "?"],
    "durationSec"  -> N @ Lookup[rj, "durationSec", 0.],
    "summary"      -> Lookup[rj, "summary", <||>],
    "perTest"      -> perTest
  |>
];


(* ----------------------------------------------------------------------- *)
(* Per-model file: load, merge, write                                      *)
(* ----------------------------------------------------------------------- *)

loadModelFile[outDir_String, model_String] := Module[{slug, path, doc},
  slug = modelSlug[model];
  path = FileNameJoin[{outDir, slug <> ".json"}];
  If[! FileExistsQ[path], Return[<|"runs" -> {}|>]];
  doc = Quiet @ Check[Import[path, "RawJSON"], $Failed];
  If[AssociationQ[doc], doc, <|"runs" -> {}|>]
];

(* Replace-by-runId so re-running over the same input is idempotent. *)
mergeRuns[oldRuns_List, newRuns_List] := Module[{newIds, kept},
  newIds = Lookup[#, "runId"] & /@ newRuns;
  kept   = Select[oldRuns, ! MemberQ[newIds, Lookup[#, "runId"]] &];
  SortBy[Join[kept, newRuns], Lookup[#, "createdAt"] &]
];

writeModelFile[outDir_String, model_String, runs_List] := Module[
  {slug, path, doc, stream},
  slug = modelSlug[model];
  path = FileNameJoin[{outDir, slug <> ".json"}];
  doc = <|
    "schemaVersion" -> 1,
    "model"         -> model,
    "modelSlug"     -> slug,
    "publishedAt"   -> DateString["ISODateTime"] <> "Z",
    "runs"          -> runs
  |>;
  If[! DirectoryQ[outDir],
    CreateDirectory[outDir, CreateIntermediateDirectories -> True]];
  stream = OpenWrite[path, CharacterEncoding -> "UTF-8"];
  WriteString[stream, ExportString[doc, "RawJSON", "Compact" -> 2]];
  WriteString[stream, "\n"];   (* trailing newline for git-diff sanity *)
  Close[stream];
  info["  -> " <> path
       <> "  (" <> ToString[Length[runs]] <> " run"
       <> If[Length[runs] == 1, "", "s"] <> ")"];
  path
];


(* ----------------------------------------------------------------------- *)
(* Main                                                                    *)
(* ----------------------------------------------------------------------- *)

main[] := Module[
  {args, runsBase, outDir, runArg, runDirs, byModel = <||>, kept = 0},

  args     = parseArgs[Drop[$ScriptCommandLine, 1]];
  $quiet   = KeyExistsQ[args, "quiet"];
  runsBase = Lookup[args, "runs", FileNameJoin[{$repoRoot, "runs"}]];
  outDir   = Lookup[args, "out",  FileNameJoin[{$repoRoot, "data", "results"}]];
  runArg   = Lookup[args, "run",  None];

  runDirs = If[runArg === None,
    discoverRunDirs[runsBase],
    (* Single run directory.  If --run pointed at a run.json or the
       containing dir, normalize to the dir. *)
    Module[{p = runArg},
      If[StringEndsQ[p, "/run.json"], p = DirectoryName[p]];
      If[! DirectoryQ[p], abort[2, "no such run dir: " <> p]];
      {p}
    ]
  ];

  info["PublishResults: scanning "
       <> ToString[Length[runDirs]] <> " run "
       <> If[Length[runDirs] == 1, "directory", "directories"]
       <> ":"];

  Scan[
    Function[runDir,
      Module[{rec},
        rec = extractRun[runDir];
        If[AssociationQ[rec],
          byModel[rec["model"]] = Append[
            Lookup[byModel, rec["model"], {}], rec];
          kept++,
          Null
        ]
      ]
    ],
    runDirs
  ];

  info[""];
  info["Kept " <> ToString[kept] <> " completed run"
       <> If[kept == 1, "", "s"] <> " across "
       <> ToString[Length[byModel]] <> " model"
       <> If[Length[byModel] == 1, "", "s"] <> "."];

  If[kept == 0,
    info["Nothing to publish.  Exiting."];
    Exit[0]];

  info[""];
  info["Writing per-model files into " <> outDir <> ":"];

  KeyValueMap[
    Function[{model, newRuns},
      Module[{prev, merged},
        prev   = Lookup[loadModelFile[outDir, model], "runs", {}];
        merged = mergeRuns[prev, newRuns];
        writeModelFile[outDir, model, merged];
      ]
    ],
    byModel
  ];

  info[""];
  info["Done.  Commit data/results/ to share these results publicly."];
  Exit[0]
];

main[]
