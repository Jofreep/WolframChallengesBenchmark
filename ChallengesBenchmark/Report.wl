(* ::Package:: *)

(* :Context: ChallengesBenchmark`Private` *)
(* :Summary: HTML + Markdown report rendering, plus a live Dynamic dashboard. *)

Begin["ChallengesBenchmark`Private`"];

writeReportImpl[run_Association, dir_String] := Module[
  {htmlPath, mdPath, jsonPath, junitPath},
  If[! DirectoryQ[dir], CreateDirectory[dir, CreateIntermediateDirectories -> True]];
  htmlPath  = FileNameJoin[{dir, "report.html"}];
  mdPath    = FileNameJoin[{dir, "report.md"}];
  jsonPath  = FileNameJoin[{dir, "report.json"}];
  junitPath = FileNameJoin[{dir, "junit.xml"}];
  Export[htmlPath,  renderHTML[run],     "Text",    CharacterEncoding -> "UTF-8"];
  Export[mdPath,    renderMarkdown[run], "Text",    CharacterEncoding -> "UTF-8"];
  Export[jsonPath,  renderJSON[run],     "RawJSON"];
  Export[junitPath, renderJUnit[run],    "Text",    CharacterEncoding -> "UTF-8"];
  <|"html" -> htmlPath, "markdown" -> mdPath, "json" -> jsonPath, "junit" -> junitPath|>
];

writeJUnitReportImpl[run_Association, path_String] := Module[{dir},
  dir = DirectoryName[path];
  If[StringLength[dir] > 0 && ! DirectoryQ[dir],
    CreateDirectory[dir, CreateIntermediateDirectories -> True]];
  Export[path, renderJUnit[run], "Text", CharacterEncoding -> "UTF-8"];
  path
];

(* ----------------------------------------------------------------------- *)
(* JSON (machine-readable summary)                                         *)
(* ----------------------------------------------------------------------- *)

renderJSON[run_Association] := Module[{meta, summary},
  meta = run["meta"];
  summary = Lookup[meta, "summary", summarizeResults[run["results"]]];
  <|
    "runId"      -> meta["runId"],
    "model"      -> meta["model"],
    "createdAt"  -> meta["createdAt"],
    "finishedAt" -> Lookup[meta, "finishedAt", Null],
    "status"     -> Lookup[meta, "status", "unknown"],
    "runtime"    -> meta["runtime"],
    "summary"    -> summary,
    "perChallenge" -> perChallengeBreakdown[run["results"]]
  |>
];

perChallengeBreakdown[results_List] := Module[{g},
  g = GroupBy[results, #["challengeName"] &];
  Association @ KeyValueMap[
    Function[{name, rs},
      name -> <|
        "total"  -> Length[rs],
        "passed" -> Count[rs, _?(#["passed"] === True &)],
        "failed" -> Count[rs, _?(#["passed"] === False &)],
        "statuses" -> Counts[#["status"] & /@ rs]
      |>
    ],
    g
  ]
];

(* ----------------------------------------------------------------------- *)
(* JUnit XML                                                               *)
(*                                                                         *)
(* Maps the benchmark's outcome vocabulary to JUnit semantics so CI        *)
(* consumers (GitHub Actions test reporter, Jenkins xUnit, GitLab's test  *)
(* report, Buildkite test analytics, etc.) can surface per-test pass/fail *)
(* without bespoke parsing.                                                *)
(*                                                                         *)
(* Status mapping:                                                         *)
(*   "Evaluated" && passed      — emit bare <testcase/> (pass)             *)
(*   "Evaluated" && !passed     — <failure>   (actual/expected mismatch)   *)
(*   "TimedOut"                 — <error type="TimedOut">                  *)
(*   "MemoryExceeded"           — <error type="MemoryExceeded">            *)
(*   "EvaluationError"          — <error type="EvaluationError">           *)
(*   "ParseError"               — <error type="ParseError">                *)
(*   "KernelDied"               — <error type="KernelDied">                *)
(*   "RunnerError"              — <error type="RunnerError">               *)
(*   "NoSolution"               — <skipped message="...">                  *)
(*                                                                         *)
(* The suite's `time` attribute is the benchmark wall-clock, not the sum   *)
(* of per-test durations (which, with Parallel > 1, would exceed it and   *)
(* produce nonsensical dashboards).                                        *)
(* ----------------------------------------------------------------------- *)

xmlEscape[s_String] :=
  StringReplace[s, {
    "&" -> "&amp;",
    "<" -> "&lt;",
    ">" -> "&gt;",
    "\"" -> "&quot;",
    "'" -> "&apos;",
    (* Strip ASCII control chars that are illegal in XML 1.0 (except \t \n \r). *)
    RegularExpression["[\\x00-\\x08\\x0B\\x0C\\x0E-\\x1F]"] -> ""
  }];
xmlEscape[x_] := xmlEscape[ToString[x, InputForm]];

xmlAttr[k_String, v_] := " " <> k <> "=\"" <> xmlEscape[v] <> "\"";

(* Compact a possibly large value for CDATA display. Long strings /
   expressions are truncated so CI log panels stay readable. *)
junitRenderValue[v_] := Module[{s},
  s = ToString[v, InputForm, PageWidth -> 120];
  If[StringLength[s] > 4000,
    StringTake[s, 4000] <> "\n... (truncated, " <>
      ToString[StringLength[s]] <> " chars)",
    s
  ]
];

junitTestCase[r_Association] := Module[
  {name, classname, duration, status, passed, expected, actual, err,
   attrs, body, msg, tag, typ},

  name      = Lookup[r, "testId", Lookup[r, "challengeName", "?"] <> "/?"];
  classname = Lookup[r, "challengeName", "Unknown"];
  duration  = N @ Max[0, Lookup[r, "durationSec", 0.]];
  status    = Lookup[r, "status", "Unknown"];
  passed    = TrueQ @ Lookup[r, "passed", False];
  expected  = Lookup[r, "expected", Missing[]];
  actual    = Lookup[r, "actualOutput", Missing[]];
  err       = Lookup[r, "error", None];

  attrs = StringJoin[
    xmlAttr["name", name],
    xmlAttr["classname", classname],
    xmlAttr["time", ToString @ NumberForm[duration, {8, 6}]]
  ];

  Which[
    status === "Evaluated" && passed,
      body = "",
    status === "Evaluated",   (* failed assertion *)
      msg = "expected " <> junitRenderValue[expected] <>
            " got " <> junitRenderValue[actual];
      body = "\n    <failure" <>
        xmlAttr["type", "AssertionError"] <>
        xmlAttr["message", StringTake[msg, UpTo[400]]] <> ">" <>
        "<![CDATA[" <> xmlEscapeCData[msg] <> "]]></failure>\n  ",
    status === "NoSolution",
      body = "\n    <skipped" <>
        xmlAttr["message", "no solution provided"] <> "/>\n  ",
    True,   (* any other non-pass status is an <error> *)
      typ = status;
      tag = If[err === None || err === Missing[],
              typ,
              ToString[err, InputForm]];
      msg = StringTake[tag, UpTo[400]];
      body = "\n    <error" <>
        xmlAttr["type", typ] <>
        xmlAttr["message", msg] <> ">" <>
        "<![CDATA[" <> xmlEscapeCData[
          "status=" <> typ <> "\n" <>
          "error="  <> ToString[err, InputForm] <> "\n" <>
          "expected=" <> junitRenderValue[expected] <> "\n" <>
          "actual="   <> junitRenderValue[actual]
        ] <> "]]></error>\n  "
  ];

  If[body === "",
    "  <testcase" <> attrs <> "/>",
    "  <testcase" <> attrs <> ">" <> body <> "</testcase>"
  ]
];

(* CDATA cannot contain the literal "]]>". Escape it by splitting it
   across two CDATA sections, and strip illegal XML 1.0 control chars. *)
xmlEscapeCData[s_String] :=
  StringReplace[s, {
    "]]>" -> "]]]]><![CDATA[>",
    RegularExpression["[\\x00-\\x08\\x0B\\x0C\\x0E-\\x1F]"] -> ""
  }];
xmlEscapeCData[x_] := xmlEscapeCData[ToString[x, InputForm]];

junitSuite[challengeName_String, results_List, model_String] := Module[
  {total, fails, errors, skipped, timeSum, casesXml},
  total   = Length[results];
  fails   = Count[results, _?(
    #["status"] === "Evaluated" && ! TrueQ[#["passed"]] &)];
  errors  = Count[results, _?(
    MemberQ[{"TimedOut", "MemoryExceeded", "EvaluationError", "ParseError",
             "KernelDied", "RunnerError"}, #["status"]] &)];
  skipped = Count[results, _?(#["status"] === "NoSolution" &)];
  timeSum = Total[Max[0, #] & /@ (Lookup[#, "durationSec", 0.] & /@ results)];

  casesXml = StringRiffle[junitTestCase /@ results, "\n"];

  StringJoin[
    "<testsuite",
    xmlAttr["name", model <> "." <> challengeName],
    xmlAttr["tests",    ToString[total]],
    xmlAttr["failures", ToString[fails]],
    xmlAttr["errors",   ToString[errors]],
    xmlAttr["skipped",  ToString[skipped]],
    xmlAttr["time", ToString @ NumberForm[N[timeSum], {10, 6}]],
    ">\n",
    casesXml,
    "\n</testsuite>"
  ]
];

renderJUnit[run_Association] := Module[
  {meta, results, summary, model, runId, suiteXml, total, fails,
   errors, skipped, wall, byName},

  meta    = run["meta"];
  results = Lookup[run, "results", {}];
  summary = Lookup[meta, "summary", summarizeResults[results]];
  model   = ToString @ Lookup[meta, "model", "unknown-model"];
  runId   = ToString @ Lookup[meta, "runId", "unknown-run"];
  wall    = N @ Max[0, Lookup[meta, "durationSec", 0.]];

  total   = Lookup[summary, "total", Length[results]];
  fails   = Count[results, _?(
    #["status"] === "Evaluated" && ! TrueQ[#["passed"]] &)];
  errors  = Count[results, _?(
    MemberQ[{"TimedOut", "MemoryExceeded", "EvaluationError", "ParseError",
             "KernelDied", "RunnerError"}, #["status"]] &)];
  skipped = Count[results, _?(#["status"] === "NoSolution" &)];

  (* Group by challenge so each suite is a cohesive logical unit. *)
  byName = GroupBy[results, #["challengeName"] &];
  suiteXml = StringRiffle[
    KeyValueMap[junitSuite[#1, #2, model] &, byName],
    "\n"
  ];

  StringJoin[
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
    "<testsuites",
    xmlAttr["name",     "ChallengesBenchmark." <> model],
    xmlAttr["tests",    ToString[total]],
    xmlAttr["failures", ToString[fails]],
    xmlAttr["errors",   ToString[errors]],
    xmlAttr["skipped",  ToString[skipped]],
    xmlAttr["time", ToString @ NumberForm[wall, {10, 6}]],
    xmlAttr["timestamp", ToString @ Lookup[meta, "createdAt", ""]],
    (* Non-standard but handy: tooling that ignores extra attrs is fine. *)
    xmlAttr["runId", runId],
    ">\n",
    suiteXml,
    "\n</testsuites>\n"
  ]
];

(* ----------------------------------------------------------------------- *)
(* Markdown                                                                *)
(* ----------------------------------------------------------------------- *)

renderMarkdown[run_Association] := Module[
  {meta, summary, lines, results, byChall, failingDetails},

  meta = run["meta"];
  results = run["results"];
  summary = Lookup[meta, "summary", summarizeResults[results]];
  byChall = perChallengeBreakdown[results];

  lines = {
    "# Wolfram Challenges Benchmark — " <> ToString[meta["model"]],
    "",
    "**Run ID:** `" <> meta["runId"] <> "`  ",
    "**Status:** " <> Lookup[meta, "status", "unknown"] <> "  ",
    "**Created:** " <> meta["createdAt"] <> "  ",
    "**Finished:** " <> ToString[Lookup[meta, "finishedAt", "—"]] <> "  ",
    "**Duration:** " <> formatDuration[Lookup[meta, "durationSec", 0]] <> "  ",
    "**Wolfram version:** " <> ToString[meta["runtime", "wolframVersion"]] <> "  ",
    "**Seed:** `" <> ToString[meta["options", "seed"]] <> "`",
    "",
    "## Summary",
    "",
    "| Metric | Value |",
    "|---|---|",
    "| Total tests | " <> ToString[summary["total"]] <> " |",
    "| Passed | " <> ToString[summary["passed"]] <> " |",
    "| Failed | " <> ToString[summary["failed"]] <> " |",
    "| Evaluation errors | " <> ToString[summary["evaluationError"]] <> " |",
    "| Timed out | " <> ToString[summary["timedOut"]] <> " |",
    "| Memory exceeded | " <> ToString[summary["memoryExceeded"]] <> " |",
    "| Parse errors | " <> ToString[summary["parseError"]] <> " |",
    "| No solution | " <> ToString[summary["noSolution"]] <> " |",
    "| Kernel died | " <> ToString[summary["kernelDied"]] <> " |",
    "| Pass rate | " <> ToString[NumberForm[100. summary["passRate"], {4, 2}]] <> "% |",
    "| Challenges attempted | " <> ToString[summary["challengesAttempted"]] <> " |",
    "| Challenges fully passing | " <> ToString[summary["challengesFullyPassing"]] <> " |"
  };

  (* Timing percentiles (only meaningful when at least one test evaluated). *)
  With[{ds = Lookup[summary, "duration", <|"count" -> 0|>]},
    If[ds["count"] > 0,
      lines = Join[lines, {
        "| Duration mean | " <> formatDuration[ds["mean"]] <> " |",
        "| Duration p50  | " <> formatDuration[ds["p50"]]  <> " |",
        "| Duration p90  | " <> formatDuration[ds["p90"]]  <> " |",
        "| Duration p95  | " <> formatDuration[ds["p95"]]  <> " |",
        "| Duration p99  | " <> formatDuration[ds["p99"]]  <> " |",
        "| Duration max  | " <> formatDuration[ds["max"]]  <> " |"
      }]
    ]
  ];

  lines = Join[lines, {
    "",
    "## Per-challenge results",
    "",
    "| Challenge | Passed / Total | Status mix |",
    "|---|---|---|"
  }];

  lines = Join[lines, KeyValueMap[
    Function[{name, s},
      StringJoin["| ", name, " | ", ToString[s["passed"]], " / ", ToString[s["total"]],
        " | ", formatStatusMix[s["statuses"]], " |"]
    ],
    byChall
  ]];

  failingDetails = Select[results, #["passed"] === False &];
  If[Length[failingDetails] > 0,
    AppendTo[lines, ""];
    AppendTo[lines, "## Failing tests"];
    AppendTo[lines, ""];
    Scan[
      Function[r,
        AppendTo[lines, "### " <> r["testId"] <> " — " <> r["status"]];
        AppendTo[lines, ""];
        AppendTo[lines, "- **Expected:** `" <> truncate[ToString[r["expected"], InputForm], 200] <> "`"];
        AppendTo[lines, "- **Actual:** `"   <> truncate[ToString[r["actualOutput"], InputForm], 200] <> "`"];
        AppendTo[lines, "- **Duration:** " <> formatDuration[r["durationSec"]]];
        If[Lookup[r, "error", None] =!= None,
          AppendTo[lines, "- **Error:** " <> ToString[r["error"]]]];
        AppendTo[lines, ""];
      ],
      failingDetails
    ]
  ];

  StringRiffle[lines, "\n"]
];

formatDuration[s_?NumericQ] := Which[
  s < 1, ToString[Round[1000 s]] <> " ms",
  s < 60, ToString[NumberForm[N[s], {4, 2}]] <> " s",
  True, IntegerString[Quotient[Round[s], 60]] <> "m " <> IntegerString[Mod[Round[s], 60]] <> "s"
];
formatDuration[_] := "—";

formatStatusMix[counts_Association] := StringRiffle[
  KeyValueMap[#1 <> "×" <> ToString[#2] &, counts], ", "];

truncate[s_String, n_Integer] :=
  If[StringLength[s] <= n, s, StringTake[s, n] <> "…"];
truncate[x_, n_] := truncate[ToString[x, InputForm], n];

(* ----------------------------------------------------------------------- *)
(* HTML                                                                    *)
(* ----------------------------------------------------------------------- *)

renderHTML[run_Association] := Module[
  {meta, summary, results, byChall, rows, failRows, title, css},

  meta    = run["meta"];
  results = run["results"];
  summary = Lookup[meta, "summary", summarizeResults[results]];
  byChall = perChallengeBreakdown[results];
  title   = "Wolfram Challenges Benchmark — " <> ToString[meta["model"]];

  css = StringJoin[
    "body{font-family:-apple-system,BlinkMacSystemFont,Segoe UI,Inter,sans-serif;",
    "max-width:1100px;margin:2em auto;padding:0 1em;color:#222}",
    "h1{border-bottom:2px solid #eee;padding-bottom:.3em}",
    "h2{margin-top:2em;color:#444}",
    "table{border-collapse:collapse;width:100%;margin:1em 0;font-size:14px}",
    "th,td{border:1px solid #ddd;padding:6px 10px;text-align:left;vertical-align:top}",
    "th{background:#f7f7f7}",
    "tr.pass td:nth-child(2){color:#178a3a;font-weight:600}",
    "tr.fail td:nth-child(2){color:#c02222;font-weight:600}",
    ".kpi{display:inline-block;padding:1em 1.5em;margin:.3em;border:1px solid #ddd;border-radius:8px;background:#fafafa;min-width:8em}",
    ".kpi .v{font-size:2em;font-weight:700;display:block}",
    ".kpi .l{color:#666;font-size:.9em}",
    ".status-Evaluated{color:#178a3a}",
    ".status-TimedOut,.status-MemoryExceeded,.status-EvaluationError,.status-ParseError,.status-KernelDied,.status-NoSolution{color:#c02222}",
    "code,pre{font-family:SF Mono,Monaco,Menlo,Consolas,monospace;font-size:13px;background:#f4f4f4;padding:1px 4px;border-radius:3px}",
    "pre{padding:1em;overflow:auto;white-space:pre-wrap}",
    "details{margin:.5em 0}",
    "summary{cursor:pointer;font-weight:600}"
  ];

  rows = KeyValueMap[
    Function[{name, s},
      StringJoin[
        "<tr class=\"", If[s["failed"] == 0, "pass", "fail"], "\">",
        "<td>", htmlEscape[name], "</td>",
        "<td>", ToString[s["passed"]], " / ", ToString[s["total"]], "</td>",
        "<td>", htmlEscape[formatStatusMix[s["statuses"]]], "</td>",
        "</tr>"
      ]
    ],
    byChall
  ];

  failRows = Map[
    Function[r,
      StringJoin[
        "<details><summary>", htmlEscape[r["testId"]],
        " — <span class=\"status-", htmlEscape[r["status"]], "\">",
        htmlEscape[r["status"]], "</span></summary>",
        "<p><b>Expected:</b><pre>", htmlEscape[ToString[r["expected"], InputForm]], "</pre></p>",
        "<p><b>Actual:</b><pre>",   htmlEscape[ToString[r["actualOutput"], InputForm]], "</pre></p>",
        "<p><b>Duration:</b> ", htmlEscape[formatDuration[r["durationSec"]]], "</p>",
        If[Lookup[r, "error", None] =!= None,
          "<p><b>Error:</b> " <> htmlEscape[ToString[r["error"]]] <> "</p>", ""],
        "</details>"
      ]
    ],
    Select[results, #["passed"] === False &]
  ];

  StringJoin[
    "<!doctype html><html><head><meta charset=\"utf-8\"><title>", htmlEscape[title],
    "</title><style>", css, "</style></head><body>",
    "<h1>", htmlEscape[title], "</h1>",
    "<p><b>Run ID:</b> <code>", htmlEscape[meta["runId"]], "</code> &middot; ",
    "<b>Wolfram:</b> ", htmlEscape[ToString[meta["runtime", "wolframVersion"]]], " &middot; ",
    "<b>Duration:</b> ", htmlEscape[formatDuration[Lookup[meta, "durationSec", 0]]],
    "</p>",

    "<h2>Summary</h2>",
    "<div>",
    kpi["Total",     summary["total"]],
    kpi["Passed",    summary["passed"]],
    kpi["Failed",    summary["failed"]],
    kpi["Errors",    summary["evaluationError"]],
    kpi["Timeouts",  summary["timedOut"]],
    kpi["OOM",       summary["memoryExceeded"]],
    kpi["Pass rate", ToString[NumberForm[100. summary["passRate"], {4, 2}]] <> "%"],
    With[{ds = Lookup[summary, "duration", <|"count" -> 0|>]},
      If[ds["count"] > 0,
        StringJoin[
          kpi["p50",  formatDuration[ds["p50"]]],
          kpi["p90",  formatDuration[ds["p90"]]],
          kpi["p95",  formatDuration[ds["p95"]]],
          kpi["p99",  formatDuration[ds["p99"]]],
          kpi["max",  formatDuration[ds["max"]]]
        ],
        ""
      ]
    ],
    "</div>",

    "<h2>Per-challenge results</h2>",
    "<table><thead><tr><th>Challenge</th><th>Passed / Total</th><th>Status mix</th></tr></thead>",
    "<tbody>", StringJoin[rows], "</tbody></table>",

    If[Length[failRows] > 0,
      "<h2>Failing tests</h2>" <> StringJoin[failRows], ""],

    "</body></html>"
  ]
];

kpi[label_, value_] := StringJoin[
  "<div class=\"kpi\"><span class=\"v\">", htmlEscape[ToString[value]],
  "</span><span class=\"l\">", htmlEscape[label], "</span></div>"
];

htmlEscape[s_String] := StringReplace[s, {
  "&" -> "&amp;", "<" -> "&lt;", ">" -> "&gt;",
  "\"" -> "&quot;", "'" -> "&#39;"
}];
htmlEscape[x_] := htmlEscape[ToString[x]];

(* ----------------------------------------------------------------------- *)
(* LiveDashboard — a Dynamic view for use from a notebook                  *)
(* ----------------------------------------------------------------------- *)

(* Exposed so the notebook can do:
     CellPrint @ ChallengesBenchmark`Private`LiveDashboard[runDir]
   or similar.
*)

ChallengesBenchmark`LiveDashboard::usage =
  "LiveDashboard[runDir] renders a Dynamic[] progress view that tails runDir/progress.jsonl. \
It works whether the run is in progress, complete, or has not started yet.";

(* --- helper: read and parse progress.jsonl robustly. ----------------------
   The file is open-append-close'd by the runner, but we may still catch it
   mid-write between lines (very unlikely) or see a partial last line if the
   kernel is preempted. ImportString returns $Failed on a broken line; we
   drop those defensively rather than letting a single bad line collapse
   the whole view. *)

ChallengesBenchmark`Private`readProgressEvents[path_String] :=
  Module[{lines, parsed},
    If[! FileExistsQ[path], Return[{}]];
    lines = Quiet @ Check[ReadList[path, "String"], {}];
    If[! ListQ[lines], Return[{}]];
    parsed = Map[
      Function[line,
        Quiet @ Check[ImportString[line, "RawJSON"], $Failed]
      ],
      lines
    ];
    Select[parsed, AssociationQ]
  ];

(* --- helper: derive a structured progress snapshot from a list of events.
   Works in all three states:
     - file absent / empty  → all zeros, state "not-started"
     - in progress          → partial counts, state "running"
     - finished             → full counts + summary, state "finished"

   Key decisions:
     - total is read from the first run.start event; if absent (should not
       happen in a healthy run) fall back to the max totalCount seen.
     - done/passed/failed are derived by *counting* test.complete events,
       not by reading doneCount off the last event. That was the original
       bug: run.end and test.submit events don't carry doneCount, so on a
       finished run or between submits the display collapsed to 0/N. *)

ChallengesBenchmark`Private`progressSnapshot[events_List] := Module[
  {completes, runStart, runEnd, total, done, passed, failed, lastComplete,
   currentStatus, state},

  runStart      = FirstCase[events, ev_Association /; ev["event"] === "run.start", <||>];
  runEnd        = FirstCase[events, ev_Association /; ev["event"] === "run.end", None];
  completes     = Cases[events, ev_Association /; ev["event"] === "test.complete"];
  total         = Lookup[runStart, "totalTests",
                    If[completes =!= {},
                       Max[Lookup[#, "totalCount", 0] & /@ completes], 0]];
  done          = Length[completes];
  passed        = Count[completes, ev_ /; TrueQ[ev["passed"]]];
  failed        = done - passed;
  lastComplete  = Last[completes, <||>];
  currentStatus = Lookup[lastComplete, "status", ""];

  state = Which[
    runEnd =!= None,       "finished",
    Length[events] === 0,  "not-started",
    True,                  "running"
  ];

  <|
    "state"          -> state,
    "total"          -> total,
    "done"           -> done,
    "passed"         -> passed,
    "failed"         -> failed,
    "rate"           -> If[total > 0, N[done / total], 0.],
    "passRate"       -> If[done  > 0, N[passed / done],  0.],
    "lastTestId"     -> Lookup[lastComplete, "testId", ""],
    "lastStatus"     -> currentStatus,
    "lastPassed"     -> Lookup[lastComplete, "passed", None],
    "runId"          -> Lookup[runStart, "runId", ""],
    "runEndSummary"  -> If[AssociationQ[runEnd], Lookup[runEnd, "summary", <||>], <||>]
  |>
];

(* --- the view itself. Two static layout helpers + one Dynamic. ---------- *)

ChallengesBenchmark`Private`liveStatePill[state_String] := Style[
  state,
  "Text",
  FontWeight -> Bold,
  FontColor  -> Switch[state,
    "finished",    Darker[Green],
    "running",     Darker[Blue],
    "not-started", Gray,
    _,             Black]
];

ChallengesBenchmark`Private`liveDashboardView[snap_Association, path_String] :=
  Framed[
    Column[{
      Row[{Style["Wolfram Challenges Benchmark \[LongDash] Live", "Subsubsection"],
           Spacer[20],
           ChallengesBenchmark`Private`liveStatePill[snap["state"]]}],
      If[snap["runId"] =!= "",
        Style[Row[{"runId: ", snap["runId"]}], "SmallText", GrayLevel[0.4]],
        Nothing
      ],
      Row[{
        Style["Progress: ", Bold],
        snap["done"], " / ", snap["total"],
        "  (",
        Style[ToString[NumberForm[100. snap["rate"], {4, 2}]] <> "%", Bold],
        ")"
      }],
      ProgressIndicator[snap["rate"]],
      Row[{
        Style["Passed: ", Bold], snap["passed"],
        Spacer[20],
        Style["Failed: ", Bold], snap["failed"],
        Spacer[20],
        Style["Pass rate: ", Bold],
        Style[ToString[NumberForm[100. snap["passRate"], {4, 2}]] <> "%", Bold]
      }],
      If[snap["state"] === "running" && snap["lastTestId"] =!= "",
        Style[Row[{"Latest: ", snap["lastTestId"],
                   " \[RightArrow] ", snap["lastStatus"],
                   If[snap["lastPassed"] === True,  " ", ""],
                   If[snap["lastPassed"] === False, " ", ""]}],
              "Text", GrayLevel[0.4]],
        Nothing
      ],
      If[snap["state"] === "finished" && AssociationQ[snap["runEndSummary"]] &&
         KeyExistsQ[snap["runEndSummary"], "challengesFullyPassing"],
        Style[Row[{"Final: ", snap["runEndSummary", "challengesFullyPassing"],
                   " / ", snap["runEndSummary", "challengesAttempted"],
                   " challenges fully passing"}], "Text", GrayLevel[0.4]],
        Nothing
      ],
      If[snap["state"] === "not-started",
        Style[Row[{"Waiting for ", path, "\[Ellipsis]"}],
              "Text", GrayLevel[0.5]],
        Nothing
      ]
    }, Spacings -> 0.6],
    FrameStyle -> GrayLevel[0.8],
    Background -> GrayLevel[0.98],
    RoundingRadius -> 6,
    FrameMargins -> 12
  ];

ChallengesBenchmark`LiveDashboard[runDir_String] := DynamicModule[
  {path},
  path = FileNameJoin[{runDir, "progress.jsonl"}];
  Dynamic[
    Refresh[
      ChallengesBenchmark`Private`liveDashboardView[
        ChallengesBenchmark`Private`progressSnapshot[
          ChallengesBenchmark`Private`readProgressEvents[path]
        ],
        path
      ],
      UpdateInterval -> 1,
      TrackedSymbols :> {}
    ]
  ]
];

End[];
