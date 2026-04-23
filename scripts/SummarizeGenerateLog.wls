#!/usr/bin/env wolframscript
(* ::Package:: *)

(* scripts/SummarizeGenerateLog.wls

   Triage tool for the generate-*.jsonl logs emitted by
   GenerateSolutions.wls.  Reconstructs per-challenge state from the
   event stream, prints a structured summary, and exits with a status
   code suitable for CI:

       0  -> tombstone shows generate.finished and counts.failed == 0
              and counts.auditRejected == 0
       2  -> log shows any failures or audit rejections
       3  -> no log file found / log unreadable
       4  -> log is in-progress (no tombstone yet) -- "snapshot" exit

   Usage:
     wolframscript -file scripts/SummarizeGenerateLog.wls
       --log runs/generate-minimax-20260421-143545.jsonl

     If --log is omitted the script picks the most recently modified
     file matching <repo>/runs/generate-*.jsonl.

   Optional:
     --top N          number of slowest challenges to list (default 10)
     --watch SECONDS  re-summarize every SECONDS until a tombstone is
                      seen (default 0 = single-shot).  Useful while a
                      long generation is still running.
     --quiet          suppress per-challenge breakdowns; print only the
                      one-line headline (handy for scripting / CI).
*)

(* -- arg parsing ---------------------------------------------------- *)

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

(* -- log discovery -------------------------------------------------- *)

findLatestLog[runsDir_String] := Module[{candidates},
  candidates = FileNames["generate-*.jsonl", runsDir];
  If[candidates === {}, Return[$Failed]];
  First @ SortBy[candidates, -FileDate[#, "Modification"][[1 ;; 6]] &]
];

(* -- safe JSON line parse ------------------------------------------- *)

(* During a live tail the very last line can be partial; ImportString of
   an incomplete line returns $Failed.  We silently drop those rows -- a
   second invocation a few seconds later will catch the completed line. *)
parseLine[line_String] := Module[{r},
  r = Quiet @ Check[
    ImportString[StringTrim[line], "RawJSON"],
    $Failed
  ];
  If[AssociationQ[r], r, Null]
];

readEvents[path_String] := Module[{lines, parsed},
  lines = Quiet @ Check[ReadList[path, String], $Failed];
  If[lines === $Failed, Return[$Failed]];
  parsed = parseLine /@ lines;
  DeleteCases[parsed, Null]
];

(* -- per-challenge state reconstruction ----------------------------- *)

(* Each challenge has at most one terminal event.  We track the latest
   terminal event seen, plus prompt-issued time so we can flag rows that
   started but never resolved (currently-in-flight or tombstoned). *)

terminalEvents = <|
  "challenge.saved"          -> "saved",
  "challenge.failed"         -> "failed",
  "challenge.auditRejected"  -> "auditRejected",
  "challenge.empty"          -> "empty",
  "challenge.skipped"        -> "skipped"
|>;

reconstructChallenges[events_List] := Module[
  {state = <||>, name, status, ev, lastPrompt = None},
  Scan[
    Function[ev,
      name = Lookup[ev, "name", None];
      Which[
        ev["event"] === "challenge.prompt" && StringQ[name],
          lastPrompt = name;
          state[name] = Join[
            Lookup[state, name, <||>],
            <|"promptedAt" -> Lookup[ev, "timestamp", None],
              "promptBytes" -> Lookup[ev, "promptBytes", 0]|>
          ],

        KeyExistsQ[terminalEvents, ev["event"]] && StringQ[name],
          state[name] = Join[
            Lookup[state, name, <||>],
            <|"status"      -> terminalEvents[ev["event"]],
              "rawEvent"    -> ev,
              "latencySec"  -> Lookup[ev, "latencySec", Missing[]],
              "finishReason"-> Lookup[ev, "finishReason", Missing[]],
              "attempts"    -> Lookup[ev, "attempts", Missing[]]|>
          ];
          If[name === lastPrompt, lastPrompt = None],

        True, Null
      ]
    ],
    events
  ];
  {state, lastPrompt}
];

(* -- aggregation helpers -------------------------------------------- *)

statusCounts[state_Association] :=
  KeySort @ Counts[Lookup[#, "status", "in-flight"] & /@ Values[state]];

finishReasonCounts[state_Association] := Module[{xs},
  xs = DeleteMissing[Lookup[#, "finishReason", Missing[]] & /@ Values[state]];
  KeySort @ Counts[xs]
];

slowestN[state_Association, n_Integer] := Module[{rows},
  rows = Select[
    KeyValueMap[
      Function[{name, info},
        If[NumericQ[Lookup[info, "latencySec", Missing[]]],
          {name, info["latencySec"], Lookup[info, "status", "?"]},
          Nothing]
      ],
      state
    ],
    True &
  ];
  Take[ReverseSortBy[rows, #[[2]] &], UpTo[n]]
];

(* p50 latency over saved+failed (non-skipped) for ETA. *)
medianLatency[state_Association] := Module[{xs},
  xs = Select[
    Lookup[#, "latencySec", Missing[]] & /@ Values[state],
    NumericQ
  ];
  If[xs === {}, Missing[], Median[xs]]
];

(* -- header / tombstone extraction ---------------------------------- *)

findHeader[events_List] :=
  SelectFirst[events, #["event"] === "generate.start" &, <||>];

findTombstone[events_List] := SelectFirst[
  events,
  MemberQ[{"generate.finished", "generate.aborted"}, #["event"]] &,
  None
];

(* -- formatting ----------------------------------------------------- *)

fmtPct[num_, denom_] := If[denom > 0,
  ToString @ NumberForm[100. num / denom, {3, 1}] <> "%",
  "n/a"
];

fmtSec[s_?NumericQ] := ToString @ NumberForm[N[s], {6, 1}] <> "s";
fmtSec[_]           := "?";

fmtAssocCounts[a_Association] :=
  StringRiffle[
    KeyValueMap[Function[{k, v}, ToString[k] <> "=" <> ToString[v]], a],
    ", "
  ];

(* -- one-shot summarize --------------------------------------------- *)

summarizeOnce[path_String, topN_Integer, quiet_:False] := Module[
  {events, header, tomb, state, inFlight, total, processed,
   counts, finishReasons, p50, eta, slowest, exitCode},

  events = readEvents[path];
  If[events === $Failed,
    WriteString[$Output, "ERROR: cannot read log: ", path, "\n"];
    Return[3]];

  header = findHeader[events];
  tomb   = findTombstone[events];
  {state, inFlight} = reconstructChallenges[events];

  total      = Lookup[header, "toProcess",   Length[state]];
  counts     = statusCounts[state];
  processed  = Total @ DeleteCases[Values[counts], _Missing];
  finishReasons = finishReasonCounts[state];
  p50        = medianLatency[state];
  slowest    = slowestN[state, topN];

  (* ---------------- headline ---------------- *)

  WriteString[$Output, "\n=== Generate log summary ===\n"];
  WriteString[$Output, "  log:           ", path, "\n"];
  If[AssociationQ[header],
    WriteString[$Output, "  runId:         ", Lookup[header, "runId",  "?"], "\n"];
    WriteString[$Output, "  model:         ", Lookup[header, "model",  "?"], "\n"];
    WriteString[$Output, "  outDir:        ", Lookup[header, "outDir", "?"], "\n"];
    WriteString[$Output, "  startedAt:     ", Lookup[header, "startedAt", "?"], "\n"];
    WriteString[$Output, "  toProcess:     ", total, "\n"];
    WriteString[$Output, "  totalInBank:   ", Lookup[header, "totalInBank", "?"], "\n"]
  ];

  WriteString[$Output, "\n  processed:     ", processed, " / ", total,
    " (", fmtPct[processed, total], ")\n"];
  WriteString[$Output, "  status counts: ", fmtAssocCounts[counts], "\n"];

  If[Length[finishReasons] > 0,
    WriteString[$Output, "  finish reason: ", fmtAssocCounts[finishReasons], "\n"]];

  If[NumericQ[p50],
    WriteString[$Output, "  median latency: ", fmtSec[p50], "\n"];
    If[total - processed > 0,
      eta = (total - processed) * p50;
      WriteString[$Output, "  ETA (sequential): ",
        fmtSec[eta], "  (~",
        ToString @ NumberForm[N[eta / 60.], {6, 1}], " min)\n"]
    ]
  ];

  (* ---------------- liveness ---------------- *)

  Which[
    AssociationQ[tomb],
      WriteString[$Output, "\n  tombstone:     ",
        tomb["event"], " @ ", Lookup[tomb, "finishedAt", "?"], "\n"];
      If[StringQ @ Lookup[tomb, "lastName", None],
        WriteString[$Output, "  lastName:      ", tomb["lastName"], "\n"]],

    StringQ[inFlight],
      WriteString[$Output,
        "\n  in flight:     ", inFlight,
        "  (no terminal event yet)\n"],

    True,
      WriteString[$Output,
        "\n  in flight:     (none -- log is between challenges)\n"]
  ];

  (* ---------------- breakdowns -------------- *)

  If[! quiet,

    If[Length[slowest] > 0,
      WriteString[$Output, "\n  slowest ", Length[slowest], ":\n"];
      Scan[
        Function[r,
          WriteString[$Output,
            "    ", PaddedForm[r[[1]], 36, NumberPadding -> {"", " "}],
            "  ", fmtSec[r[[2]]],
            "  [", r[[3]], "]\n"]],
        slowest]
    ];

    Module[{rejected, failed, empties},
      rejected = Sort @ Keys @ Select[state, #["status"] === "auditRejected" &];
      failed   = Sort @ Keys @ Select[state, #["status"] === "failed"        &];
      empties  = Sort @ Keys @ Select[state, #["status"] === "empty"         &];

      If[Length[rejected] > 0,
        WriteString[$Output, "\n  audit-rejected (", Length[rejected], "):\n"];
        Scan[WriteString[$Output, "    ", #, "\n"] &, rejected]];
      If[Length[failed] > 0,
        WriteString[$Output, "\n  failed (", Length[failed], "):\n"];
        Scan[WriteString[$Output, "    ", #, "\n"] &, failed]];
      If[Length[empties] > 0,
        WriteString[$Output, "\n  empty-extracted (", Length[empties], "):\n"];
        Scan[WriteString[$Output, "    ", #, "\n"] &, empties]]
    ]
  ];

  WriteString[$Output, "\n"];

  (* ---------------- exit code --------------- *)

  exitCode = Which[
    AssociationQ[tomb] && tomb["event"] === "generate.finished" &&
      Lookup[counts, "failed", 0]        === 0 &&
      Lookup[counts, "auditRejected", 0] === 0 &&
      Lookup[counts, "empty", 0]         === 0,
        0,
    AssociationQ[tomb],   2,        (* finished or aborted with issues *)
    True,                 4         (* still running *)
  ];
  exitCode
];

(* -- main ----------------------------------------------------------- *)

args   = parseArgs[Drop[$ScriptCommandLine, 1]];
here   = DirectoryName[$InputFileName];
root   = DirectoryName[here];
runs   = FileNameJoin[{root, "runs"}];

logArg = Lookup[args, "log", Automatic];
If[logArg === Automatic,
  logArg = findLatestLog[runs];
  If[logArg === $Failed,
    WriteString[$Output, "ERROR: no generate-*.jsonl found in ", runs, "\n"];
    Exit[3]];
  WriteString[$Output, "(auto-selected latest log: ", logArg, ")\n"]
];

topN  = ToExpression[Lookup[args, "top",   "10"]];
watch = ToExpression[Lookup[args, "watch", "0"]];
quiet = TrueQ @ Lookup[args, "quiet", False];

If[! IntegerQ[topN]  || topN  < 0, topN  = 10];
If[! NumericQ[watch] || watch < 0, watch = 0];

If[watch == 0,
  Exit @ summarizeOnce[logArg, topN, quiet]
];

(* watch loop -- redraw every `watch` seconds until a tombstone shows up,
   then print one final summary and exit with that summary's code.  *)
Module[{code = 4},
  While[code === 4,
    code = summarizeOnce[logArg, topN, quiet];
    If[code === 4,
      WriteString[$Output, "  (re-checking in ", watch, "s; Ctrl-C to quit)\n"];
      Pause[watch]
    ]
  ];
  Exit[code]
]
