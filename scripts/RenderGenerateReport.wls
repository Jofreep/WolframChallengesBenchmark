#!/usr/bin/env wolframscript
(* ::Package:: *)

(* scripts/RenderGenerateReport.wls

   Renders a self-contained HTML report from any generate-*.jsonl log
   produced by GenerateSolutions.wls.  Single file, inline CSS, no CDN
   dependencies.  Safe to commit, share, or attach to a CI artifact.

   Usage:
     wolframscript -file scripts/RenderGenerateReport.wls
       [--log runs/generate-minimax-20260422-153150.jsonl]
       [--out runs/generate-minimax-20260422-153150.html]

     If --log is omitted, picks the most recently modified
     <repo>/runs/generate-*.jsonl.

     If --out is omitted, derives the HTML path from the log path
     by swapping the extension (foo.jsonl -> foo.html).

   Exit:
     0  report written
     3  no log found / unreadable
*)

(* ------------------------------------------------------------------ *)
(* arg parsing                                                         *)
(* ------------------------------------------------------------------ *)

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

(* ------------------------------------------------------------------ *)
(* JSONL reading + reconstruction (same logic as Summarize script)    *)
(* ------------------------------------------------------------------ *)

findLatestLog[runsDir_String] := Module[{candidates},
  candidates = FileNames["generate-*.jsonl", runsDir];
  If[candidates === {}, Return[$Failed]];
  First @ SortBy[candidates, -FileDate[#, "Modification"][[1 ;; 6]] &]
];

parseLine[line_String] := Module[{r},
  r = Quiet @ Check[ImportString[StringTrim[line], "RawJSON"], $Failed];
  If[AssociationQ[r], r, Null]
];

readEvents[path_String] := Module[{lines, parsed},
  lines = Quiet @ Check[ReadList[path, String], $Failed];
  If[lines === $Failed, Return[$Failed]];
  parsed = parseLine /@ lines;
  DeleteCases[parsed, Null]
];

terminalEvents = <|
  "challenge.saved"          -> "saved",
  "challenge.failed"         -> "failed",
  "challenge.auditRejected"  -> "auditRejected",
  "challenge.empty"          -> "empty",
  "challenge.skipped"        -> "skipped"
|>;

reconstructChallenges[events_List] := Module[
  {state = <||>, name, ev, lastPrompt = None},
  Scan[
    Function[ev,
      name = Lookup[ev, "name", None];
      Which[
        ev["event"] === "challenge.prompt" && StringQ[name],
          lastPrompt = name;
          state[name] = Join[
            Lookup[state, name, <||>],
            <|"promptedAt"  -> Lookup[ev, "timestamp", None],
              "promptBytes" -> Lookup[ev, "promptBytes", 0]|>
          ],

        KeyExistsQ[terminalEvents, ev["event"]] && StringQ[name],
          state[name] = Join[
            Lookup[state, name, <||>],
            <|"status"        -> terminalEvents[ev["event"]],
              "rawEvent"      -> ev,
              "latencySec"    -> Lookup[ev, "latencySec",   Missing[]],
              "finishReason"  -> Lookup[ev, "finishReason", Missing[]],
              "contentSource" -> Lookup[ev, "contentSource",Missing[]],
              "attempts"      -> Lookup[ev, "attempts",     Missing[]],
              "tokenUsage"    -> Lookup[ev, "tokenUsage",   Missing[]],
              "errorMessage"  -> Lookup[ev, "errorMessage", Missing[]]|>
          ];
          If[name === lastPrompt, lastPrompt = None],

        True, Null
      ]
    ],
    events
  ];
  {state, lastPrompt}
];

findHeader[events_List] :=
  SelectFirst[events, #["event"] === "generate.start" &, <||>];

findTombstone[events_List] := SelectFirst[
  events,
  MemberQ[{"generate.finished", "generate.aborted"}, #["event"]] &,
  None
];

(* ------------------------------------------------------------------ *)
(* Aggregation helpers                                                 *)
(* ------------------------------------------------------------------ *)

statusCounts[state_Association] :=
  KeySort @ Counts[Lookup[#, "status", "in-flight"] & /@ Values[state]];

finishReasonCounts[state_Association] := Module[{xs},
  xs = DeleteMissing[Lookup[#, "finishReason", Missing[]] & /@ Values[state]];
  KeySort @ Counts[xs]
];

contentSourceCounts[state_Association] := Module[{xs},
  xs = DeleteMissing[Lookup[#, "contentSource", Missing[]] & /@ Values[state]];
  KeySort @ Counts[xs]
];

medianLatency[state_Association] := Module[{xs},
  xs = Select[Lookup[#, "latencySec", Missing[]] & /@ Values[state], NumericQ];
  If[xs === {}, Missing[], Median[xs]]
];

p95Latency[state_Association] := Module[{xs},
  xs = Sort @ Select[Lookup[#, "latencySec", Missing[]] & /@ Values[state],
    NumericQ];
  If[xs === {}, Missing[],
    xs[[Min[Length[xs], Ceiling[0.95 Length[xs]]]]]]
];

(* ------------------------------------------------------------------ *)
(* HTML helpers                                                        *)
(* ------------------------------------------------------------------ *)

(* Minimal HTML escape: enough for arbitrary challenge names + LLM    *)
(* error strings.  We don't accept user JS, so & < > " ' suffice.     *)

esc[s_String] := StringReplace[s,
  {"&" -> "&amp;", "<" -> "&lt;", ">" -> "&gt;",
   "\"" -> "&quot;", "'" -> "&#39;"}];
esc[other_]   := esc[ToString[other]];

fmtSec[s_?NumericQ] := ToString @ NumberForm[N[s], {6, 1}] <> "s";
fmtSec[_]           := "&mdash;";

fmtPct[num_, denom_] := If[denom > 0,
  ToString @ NumberForm[100. num / denom, {3, 1}] <> "%",
  "&mdash;"];

(* Inline horizontal bar rendered with a simple flex div.  Width is
   computed once per row so it survives copy-paste into other tools.  *)
hbar[label_, count_, total_] := Module[{pct},
  pct = If[total > 0, 100. count / total, 0];
  StringJoin[
    "<div class='bar-row'>",
    "  <div class='bar-label'>", esc[label], "</div>",
    "  <div class='bar-track'>",
    "    <div class='bar-fill' style='width:",
    ToString @ NumberForm[pct, {6, 1}], "%'></div>",
    "  </div>",
    "  <div class='bar-count'>", ToString[count],
    "  <span class='bar-pct'>(", ToString @ NumberForm[pct, {6, 1}], "%)</span>",
    "  </div>",
    "</div>"
  ]
];

statusBadge[status_String] := Module[{cls},
  cls = Switch[status,
    "saved",          "badge-ok",
    "skipped",        "badge-muted",
    "failed",         "badge-bad",
    "auditRejected",  "badge-warn",
    "empty",          "badge-warn",
    "in-flight",      "badge-info",
    _,                "badge-muted"
  ];
  "<span class='badge " <> cls <> "'>" <> esc[status] <> "</span>"
];

(* ------------------------------------------------------------------ *)
(* Page assembly                                                       *)
(* ------------------------------------------------------------------ *)

renderHead[runId_String] := StringJoin[
  "<!doctype html><html lang='en'><head>",
  "<meta charset='utf-8'>",
  "<meta name='viewport' content='width=device-width, initial-scale=1'>",
  "<title>", esc[runId], " &mdash; generate report</title>",
  "<style>", $stylesheet, "</style>",
  "</head>"
];

$stylesheet = "
body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
  margin: 0; padding: 0; background: #fafafa; color: #222; line-height: 1.45;}
.wrap { max-width: 1100px; margin: 0 auto; padding: 24px 32px 64px; }
h1 { font-size: 22px; margin: 0 0 4px; }
h2 { font-size: 16px; margin: 32px 0 12px; color: #444;
     border-bottom: 1px solid #e5e5e5; padding-bottom: 4px; }
.subtitle { color: #777; font-size: 13px; margin: 0 0 24px; }
.grid { display: grid; grid-template-columns: repeat(4, 1fr); gap: 14px; margin: 0 0 24px; }
.card { background: white; border: 1px solid #e8e8e8; border-radius: 8px;
  padding: 14px 16px; }
.card .label { font-size: 11px; color: #888; text-transform: uppercase;
  letter-spacing: 0.04em; margin-bottom: 4px; }
.card .value { font-size: 22px; font-weight: 600; color: #222; }
.card .sub { font-size: 12px; color: #999; margin-top: 4px; }
.bar-row { display: flex; align-items: center; gap: 10px; margin-bottom: 6px;
  font-size: 13px; }
.bar-label { width: 160px; color: #555; }
.bar-track { flex: 1; background: #eee; border-radius: 4px; height: 14px; overflow: hidden; }
.bar-fill { background: #4a8df0; height: 100%; }
.bar-count { width: 110px; color: #333; font-variant-numeric: tabular-nums; }
.bar-pct { color: #999; font-size: 11px; margin-left: 4px; }
table { width: 100%; border-collapse: collapse; font-size: 13px; background: white;
  border: 1px solid #e8e8e8; border-radius: 6px; overflow: hidden; }
th, td { text-align: left; padding: 7px 10px; border-bottom: 1px solid #f0f0f0;
  vertical-align: top; }
th { background: #f7f7f7; color: #555; font-weight: 600; font-size: 12px;
  text-transform: uppercase; letter-spacing: 0.04em; }
td.num { text-align: right; font-variant-numeric: tabular-nums; }
tr:last-child td { border-bottom: none; }
.badge { display: inline-block; padding: 2px 8px; border-radius: 10px;
  font-size: 11px; font-weight: 600; text-transform: uppercase; letter-spacing: 0.04em; }
.badge-ok    { background: #e6f4ea; color: #1b5e20; }
.badge-bad   { background: #fce8e8; color: #b71c1c; }
.badge-warn  { background: #fff3e0; color: #e65100; }
.badge-muted { background: #ececec; color: #555; }
.badge-info  { background: #e3f2fd; color: #0d47a1; }
.tomb-finished { color: #1b5e20; font-weight: 600; }
.tomb-aborted  { color: #b71c1c; font-weight: 600; }
.tomb-running  { color: #0d47a1; font-weight: 600; }
ul.bullets { padding-left: 20px; margin: 8px 0; }
ul.bullets li { margin-bottom: 4px; }
code { font-family: ui-monospace, SFMono-Regular, Menlo, monospace; font-size: 12px;
  background: #f5f5f5; padding: 1px 4px; border-radius: 3px; }
.footer { margin-top: 32px; color: #aaa; font-size: 11px; text-align: center;
  border-top: 1px solid #eee; padding-top: 14px; }
";

renderHeader[header_, tomb_, processed_, total_] := Module[{runId, model,
  startedAt, tombClass, tombText, tombName},
  runId     = Lookup[header, "runId",     "(unknown)"];
  model     = Lookup[header, "model",     "(unknown)"];
  startedAt = Lookup[header, "startedAt", "(unknown)"];
  Which[
    AssociationQ[tomb] && tomb["event"] === "generate.finished",
      tombClass = "tomb-finished";
      tombText  = "finished &middot; " <> esc[Lookup[tomb, "finishedAt", "?"]];
      tombName  = "",
    AssociationQ[tomb] && tomb["event"] === "generate.aborted",
      tombClass = "tomb-aborted";
      tombText  = "aborted &middot; " <> esc[Lookup[tomb, "finishedAt", "?"]];
      tombName  = If[StringQ @ Lookup[tomb, "lastName", None],
        " (last challenge: <code>" <> esc[tomb["lastName"]] <> "</code>)", ""],
    True,
      tombClass = "tomb-running";
      tombText  = "still running";
      tombName  = ""
  ];
  StringJoin[
    "<div class='wrap'>",
    "<h1>", esc[model], " &mdash; generate report</h1>",
    "<p class='subtitle'>",
    "<code>", esc[runId], "</code> &middot; started ",
    esc[startedAt], " &middot; ",
    "<span class='", tombClass, "'>", tombText, "</span>", tombName,
    "</p>"
  ]
];

renderSummaryCards[total_, processed_, counts_Association, p50_, p95_, etaSec_] := Module[
  {saved, failed, rejected, skipped},
  saved    = Lookup[counts, "saved",         0];
  failed   = Lookup[counts, "failed",        0];
  rejected = Lookup[counts, "auditRejected", 0];
  skipped  = Lookup[counts, "skipped",       0];
  StringJoin[
    "<div class='grid'>",
    cardHtml["Processed", ToString[processed] <> " / " <> ToString[total],
      fmtPct[processed, total] <> " of queue"],
    cardHtml["Saved", ToString[saved],
      "of which " <> ToString[skipped] <> " skipped (already on disk)"],
    cardHtml["Failed",  ToString[failed],
      "audit-rejected: " <> ToString[rejected]],
    cardHtml["Median latency",
      If[NumericQ[p50], fmtSec[p50], "&mdash;"],
      If[NumericQ[p95], "p95: " <> fmtSec[p95], ""]],
    "</div>",
    If[NumericQ[etaSec] && etaSec > 0,
      "<p class='subtitle'>ETA (median &times; remaining): " <>
      fmtSec[etaSec] <> "  &approx; " <>
      ToString @ NumberForm[N[etaSec/60.], {6, 1}] <> " minutes</p>",
      ""]
  ]
];

cardHtml[label_, value_, sub_] := StringJoin[
  "<div class='card'>",
  "<div class='label'>", esc[label], "</div>",
  "<div class='value'>", value, "</div>",
  If[sub === "" || sub === None, "", "<div class='sub'>" <> sub <> "</div>"],
  "</div>"
];

renderHistogram[title_String, counts_Association, total_Integer] := Module[{rows},
  If[Length[counts] === 0, Return[""]];
  rows = KeyValueMap[hbar[#1, #2, total] &, counts];
  StringJoin["<h2>", esc[title], "</h2>", rows]
];

renderTable[state_Association] := Module[{names, rows, total},
  names = SortBy[
    KeyValueMap[
      Function[{name, info},
        {name, Lookup[info, "status", "in-flight"],
         Lookup[info, "latencySec",   Missing[]],
         Lookup[info, "finishReason", Missing[]],
         Lookup[info, "contentSource",Missing[]],
         Lookup[info, "attempts",     Missing[]]}
      ],
      state
    ],
    {-If[NumericQ[#[[3]]], #[[3]], -Infinity], #[[1]]} &
  ];
  total = Length[names];
  rows = StringJoin @ Map[
    Function[r,
      StringJoin[
        "<tr>",
        "<td><code>", esc[r[[1]]], "</code></td>",
        "<td>", statusBadge[r[[2]]], "</td>",
        "<td class='num'>", If[NumericQ[r[[3]]], fmtSec[r[[3]]], "&mdash;"], "</td>",
        "<td>", If[MissingQ[r[[4]]], "&mdash;", esc[r[[4]]]], "</td>",
        "<td>", If[MissingQ[r[[5]]], "&mdash;", esc[r[[5]]]], "</td>",
        "<td class='num'>", If[NumericQ[r[[6]]], ToString[r[[6]]], "&mdash;"], "</td>",
        "</tr>"
      ]
    ],
    names
  ];
  StringJoin[
    "<h2>Per-challenge (", ToString[total], " rows, slowest first)</h2>",
    "<table>",
    "<thead><tr>",
    "<th>Challenge</th><th>Status</th><th>Latency</th>",
    "<th>Finish reason</th><th>Content source</th><th>Attempts</th>",
    "</tr></thead><tbody>",
    rows,
    "</tbody></table>"
  ]
];

renderProblemList[title_String, names_List] := If[Length[names] === 0, "",
  StringJoin[
    "<h2>", esc[title], " (", ToString[Length[names]], ")</h2>",
    "<ul class='bullets'>",
    StringJoin @ Map[Function[n, "<li><code>" <> esc[n] <> "</code></li>"], names],
    "</ul>"
  ]
];

renderInFlight[inFlight_, tomb_] := Which[
  AssociationQ[tomb], "",
  StringQ[inFlight],
    "<h2>Currently in flight</h2><p><code>" <> esc[inFlight] <>
    "</code> &mdash; no terminal event yet.</p>",
  True,
    "<h2>Currently in flight</h2><p>(none &mdash; log is between challenges)</p>"
];

renderFooter[logPath_String] := StringJoin[
  "<p class='footer'>",
  "Generated ", esc[DateString["ISODateTime"]], " from <code>",
  esc[logPath], "</code><br>",
  "RenderGenerateReport.wls v1",
  "</p></div></body></html>"
];

(* ------------------------------------------------------------------ *)
(* Top-level render                                                    *)
(* ------------------------------------------------------------------ *)

renderReport[logPath_String] := Module[
  {events, header, tomb, state, inFlight, total, processed, counts,
   finishReasons, contentSources, p50, p95, etaSec, runId,
   slowest, rejectedNames, failedNames, emptyNames, html},

  events = readEvents[logPath];
  If[events === $Failed, Return[$Failed]];

  header = findHeader[events];
  tomb   = findTombstone[events];
  {state, inFlight} = reconstructChallenges[events];

  total          = Lookup[header, "toProcess", Length[state]];
  counts         = statusCounts[state];
  processed      = Total @ DeleteCases[Values[counts], _Missing];
  finishReasons  = finishReasonCounts[state];
  contentSources = contentSourceCounts[state];
  p50            = medianLatency[state];
  p95            = p95Latency[state];
  etaSec         = If[NumericQ[p50] && total - processed > 0,
                     (total - processed) p50, Missing[]];
  runId          = Lookup[header, "runId", "generate-report"];

  rejectedNames = Sort @ Keys @ Select[state, #["status"] === "auditRejected" &];
  failedNames   = Sort @ Keys @ Select[state, #["status"] === "failed" &];
  emptyNames    = Sort @ Keys @ Select[state, #["status"] === "empty" &];

  html = StringJoin[
    renderHead[runId],
    "<body>",
    renderHeader[header, tomb, processed, total],
    renderSummaryCards[total, processed, counts, p50, p95, etaSec],
    renderHistogram["Status",         counts,         total],
    renderHistogram["Finish reason",  finishReasons,  Total @ Values @ finishReasons],
    renderHistogram["Content source", contentSources, Total @ Values @ contentSources],
    renderInFlight[inFlight, tomb],
    renderProblemList["Audit rejected", rejectedNames],
    renderProblemList["Failed",          failedNames],
    renderProblemList["Empty extracted", emptyNames],
    renderTable[state],
    renderFooter[logPath]
  ];
  html
];

(* ------------------------------------------------------------------ *)
(* main                                                                *)
(* ------------------------------------------------------------------ *)

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

outArg = Lookup[args, "out", Automatic];
If[outArg === Automatic,
  outArg = StringReplace[logArg, RegularExpression["\\.jsonl$"] -> ".html"]
];

html = renderReport[logArg];
If[html === $Failed,
  WriteString[$Output, "ERROR: cannot read log: ", logArg, "\n"];
  Exit[3]];

Export[outArg, html, "Text", CharacterEncoding -> "UTF-8"];
WriteString[$Output, "Wrote: ", outArg, "\n"];
WriteString[$Output, "  open with:  open \"", outArg, "\"\n"];
Exit[0]
