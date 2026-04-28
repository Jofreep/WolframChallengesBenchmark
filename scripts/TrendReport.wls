#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Trend dashboard.

   Groups all benchmark runs by model and emits HTML + Markdown
   showing pass rate, duration, and counts over time per model.

   Data sources, in priority order:
     1. data/results/<modelSlug>.json  (tracked, published by
        scripts/PublishResults.wls; carries each model's run history
        with safe summary fields).  This is what the public site
        reads from.
     2. runs/run-*/run.json            (local-only fallback when no
        data has been published yet).

   Useful for tracking how models drift run-over-run, comparing two
   models on the same bank, or sanity-checking that the harness
   itself stays stable across paclet edits.

   Usage:
     wolframscript -file scripts/TrendReport.wls \
       [--data data/results] \
       [--runs runs] \
       [--out  runs/trend]
*)

$scriptDir = DirectoryName[$InputFileName];
$repoRoot  = DirectoryName[$scriptDir];
$pacletDir = FileNameJoin[{$repoRoot, "WolframChallengesLLMBenchmarkPaclet"}];

Quiet @ PacletDirectoryLoad[$pacletDir];
Needs["JofreEspigulePons`WolframChallengesBenchmark`"];


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
(* Data loading                                                            *)
(* ----------------------------------------------------------------------- *)

(* Project a run record (from either source format) onto the flat shape
   the renderer expects.  Both sources share the same fields. *)
projectRunRecord[r_Association, model_String] := Module[{sum},
  sum = Lookup[r, "summary", <||>];
  <|
    "runId"       -> Lookup[r, "runId", "?"],
    "model"       -> If[Lookup[r, "model", Null] === Null, model,
                        Lookup[r, "model"]],
    "createdAt"   -> Lookup[r, "createdAt", "?"],
    "status"      -> Lookup[r, "status", "completed"],
    "totalTests"  -> Lookup[sum, "total", Lookup[r, "totalTests", 0]],
    "durationSec" -> Lookup[r, "durationSec", 0.],
    "passed"      -> Lookup[sum, "passed", 0],
    "passRate"    -> Lookup[sum, "passRate", 0.],
    "challengesAttempted"    -> Lookup[sum, "challengesAttempted", 0],
    "challengesFullyPassing" -> Lookup[sum, "challengesFullyPassing", 0]
  |>
];

(* Primary source: data/results/<modelSlug>.json. *)
loadFromPublished[dataDir_String] := Module[{paths, records = {}},
  If[! DirectoryQ[dataDir], Return[{}]];
  paths = FileNames["*.json", dataDir];
  Scan[
    Function[p,
      Module[{doc, model, history},
        doc = Quiet @ Check[Import[p, "RawJSON"], $Failed];
        If[AssociationQ[doc],
          model   = Lookup[doc, "model", "?"];
          history = Lookup[doc, "runs", {}];
          Scan[
            Function[r, AppendTo[records, projectRunRecord[r, model]]],
            history
          ]
        ]
      ]
    ],
    paths
  ];
  records
];

(* Fallback: scan runs/ directly. *)
loadFromLocalRuns[runsBase_String] := Module[{paths, records},
  If[! DirectoryQ[runsBase], Return[{}]];
  paths = FileNames["run.json", runsBase, Infinity];
  paths = Select[paths,
    ! StringContainsQ[#, "/runs/trend/"]            &&
    ! StringContainsQ[#, "/runs/leaderboard/"]      &&
    ! StringContainsQ[#, "/runs/model-strengths/"]  &&
    ! StringContainsQ[#, "/runs/bank-self-test/"]   &&
    ! StringContainsQ[#, "/runs/bank-quality/"]     &];
  records = Map[
    Function[p,
      Module[{doc = Quiet @ Check[Import[p, "RawJSON"], $Failed]},
        If[AssociationQ[doc],
          projectRunRecord[doc, Lookup[doc, "model", "?"]],
          Missing[]
        ]
      ]
    ],
    paths];
  DeleteCases[records, _Missing]
];


(* ----------------------------------------------------------------------- *)
(* Render                                                                  *)
(* ----------------------------------------------------------------------- *)

(* Pick a deterministic accent color per model. *)
colorForModel[model_String] := Module[{hash, hue},
  hash = Hash[model, "MD5"];
  hue  = Mod[hash, 360];
  "hsl(" <> ToString[hue] <> ", 65%, 45%)"
];

htmlEsc[s_String] := StringReplace[s, {
  "&" -> "&amp;", "<" -> "&lt;", ">" -> "&gt;",
  "\"" -> "&quot;", "'" -> "&#39;"}];
htmlEsc[x_] := htmlEsc[ToString[x]];

formatDuration[s_?NumericQ] := Which[
  s < 1,  ToString[Round[1000 s]] <> " ms",
  s < 60, ToString[NumberForm[N[s], {4, 2}]] <> " s",
  True,   IntegerString[Quotient[Round[s], 60]] <> "m " <>
            IntegerString[Mod[Round[s], 60]] <> "s"
];
formatDuration[_] := "\[LongDash]";

(* Tiny inline SVG sparkline of pass rate over time for one model. *)
sparkline[runs_List] := Module[{points, w = 220, h = 40, n, xs, ys, path},
  n = Length[runs];
  If[n < 2, Return[""]];
  ys = Lookup[#, "passRate", 0.] & /@ runs;
  xs = Range[0, n - 1] / (n - 1) * w;
  points = Transpose[{xs, h - ys * h}];
  path = "M " <> StringRiffle[
    Map[ToString[NumberForm[N[#[[1]]], {4, 1}]] <> "," <>
        ToString[NumberForm[N[#[[2]]], {4, 1}]] &, points],
    " L "];
  "<svg width=\"" <> ToString[w] <> "\" height=\"" <> ToString[h] <>
  "\" viewBox=\"0 0 " <> ToString[w] <> " " <> ToString[h] <>
  "\" style=\"vertical-align: middle;\">" <>
  "<path d=\"" <> path <> "\" stroke=\"currentColor\" fill=\"none\" " <>
  "stroke-width=\"1.5\" />" <>
  StringJoin @ Map[
    "<circle cx=\"" <> ToString[NumberForm[N[#[[1]]], {4,1}]] <>
    "\" cy=\"" <> ToString[NumberForm[N[#[[2]]], {4,1}]] <>
    "\" r=\"2\" fill=\"currentColor\" />" &, points] <>
  "</svg>"
];


renderHTML[byModel_Association] := Module[{css, sections, header},
  css = "
    body { font-family: -apple-system, BlinkMacSystemFont, sans-serif;
           max-width: 1100px; margin: 2em auto; padding: 0 1em;
           color: #1a1a1a; }
    h1 { border-bottom: 2px solid #eee; padding-bottom: .3em; }
    h2 { margin-top: 2em; padding: .5em .7em; border-radius: 4px;
         background: #f7f7f7; }
    h2 .spark { float: right; opacity: .8; }
    table { border-collapse: collapse; width: 100%; margin: .5em 0 2em 0;
            font-size: 14px; }
    th, td { border: 1px solid #ddd; padding: 6px 10px; text-align: left;
             vertical-align: top; }
    th { background: #f7f7f7; }
    .num { text-align: right; font-variant-numeric: tabular-nums; }
    .nm  { font-family: 'SF Mono', Monaco, Menlo, monospace; font-size: 12px; }
    .pct  { font-weight: 600; }
    .small { color: #888; font-size: 12px; }
  ";

  header = "<h1>Trend dashboard</h1>" <>
    "<p>" <> ToString[Length[byModel]] <> " models, " <>
    ToString[Total[Length /@ Values[byModel]]] <> " runs total. " <>
    "Generated: " <> htmlEsc[DateString["ISODateTime"]] <> "</p>";

  sections = StringJoin @ KeyValueMap[
    Function[{model, runs},
      Module[{sortedRuns, latest, color},
        sortedRuns = SortBy[runs, #["createdAt"] &];
        latest = Last[sortedRuns];
        color = colorForModel[model];
        StringJoin[
          "<h2 style=\"color: ", color, "\">",
          htmlEsc[model],
          "<span class=\"spark\">", sparkline[sortedRuns], "</span>",
          "</h2>",
          "<p>",
          ToString[Length[runs]], " runs &middot; ",
          "latest pass rate: <span class=\"pct\">",
          ToString[NumberForm[100. latest["passRate"], {4, 2}]], "%",
          "</span> (", ToString[latest["passed"]], " / ",
          ToString[latest["totalTests"]], " tests)",
          "</p>",
          "<table><thead><tr>",
          "<th>Created</th><th>Run ID</th><th class=\"num\">Pass rate</th>",
          "<th class=\"num\">Passed / Total</th>",
          "<th class=\"num\">Challenges full</th>",
          "<th class=\"num\">Duration</th>",
          "</tr></thead><tbody>",
          StringJoin @ Map[
            Function[r,
              "<tr>" <>
              "<td>" <> htmlEsc[r["createdAt"]] <> "</td>" <>
              "<td class=\"nm\">" <> htmlEsc[r["runId"]] <> "</td>" <>
              "<td class=\"num pct\" style=\"color: " <> color <> "\">" <>
                ToString[NumberForm[100. r["passRate"], {4, 2}]] <> "%</td>" <>
              "<td class=\"num\">" <> ToString[r["passed"]] <> " / " <>
                ToString[r["totalTests"]] <> "</td>" <>
              "<td class=\"num\">" <>
                ToString[r["challengesFullyPassing"]] <> " / " <>
                ToString[r["challengesAttempted"]] <> "</td>" <>
              "<td class=\"num\">" <>
                htmlEsc[formatDuration[r["durationSec"]]] <> "</td>" <>
              "</tr>"
            ],
            ReverseSortBy[sortedRuns, #["createdAt"] &]
          ],
          "</tbody></table>"
        ]
      ]
    ],
    KeySort[byModel]
  ];

  StringJoin[
    "<!doctype html><html><head><meta charset=\"utf-8\">",
    "<title>Trend dashboard \[LongDash] WolframChallengesBenchmark</title>",
    "<style>", css, "</style></head><body>",
    header, sections,
    "</body></html>"
  ]
];


renderMarkdown[byModel_Association] := Module[{lines},
  lines = {
    "# Trend dashboard",
    "",
    ToString[Length[byModel]] <> " models, " <>
      ToString[Total[Length /@ Values[byModel]]] <> " runs total.  ",
    "Generated: " <> DateString["ISODateTime"],
    ""
  };

  KeyValueMap[
    Function[{model, runs},
      Module[{sortedRuns, latest},
        sortedRuns = SortBy[runs, #["createdAt"] &];
        latest = Last[sortedRuns];
        AppendTo[lines, ""];
        AppendTo[lines, "## " <> model];
        AppendTo[lines, ""];
        AppendTo[lines,
          ToString[Length[runs]] <> " runs \[Bullet] latest pass rate: **" <>
            ToString[NumberForm[100. latest["passRate"], {4, 2}]] <> "%** (" <>
            ToString[latest["passed"]] <> " / " <>
            ToString[latest["totalTests"]] <> " tests)"];
        AppendTo[lines, ""];
        AppendTo[lines,
          "| Created | Run ID | Pass rate | Passed/Total | Challenges full | Duration |"];
        AppendTo[lines,
          "|---------|--------|-----------|--------------|-----------------|----------|"];
        Scan[
          Function[r,
            AppendTo[lines,
              "| " <> r["createdAt"] <>
              " | `"  <> r["runId"] <> "` " <>
              " | "  <> ToString[NumberForm[100. r["passRate"], {4, 2}]] <> "%" <>
              " | "  <> ToString[r["passed"]] <> " / " <>
                       ToString[r["totalTests"]] <>
              " | "  <> ToString[r["challengesFullyPassing"]] <> " / " <>
                       ToString[r["challengesAttempted"]] <>
              " | "  <> formatDuration[r["durationSec"]] <>
              " |"]
          ],
          ReverseSortBy[sortedRuns, #["createdAt"] &]
        ]
      ]
    ],
    KeySort[byModel]
  ];

  StringRiffle[lines, "\n"]
];


(* ----------------------------------------------------------------------- *)
(* Main                                                                    *)
(* ----------------------------------------------------------------------- *)

main[] := Module[
  {args, dataDir, runsBase, outDir, records, source, byModel, html, md},

  args     = parseArgs[Drop[$ScriptCommandLine, 1]];
  dataDir  = Lookup[args, "data", FileNameJoin[{$repoRoot, "data", "results"}]];
  runsBase = Lookup[args, "runs", FileNameJoin[{$repoRoot, "runs"}]];
  outDir   = Lookup[args, "out",  FileNameJoin[{$repoRoot, "runs", "trend"}]];

  records = loadFromPublished[dataDir];
  source  = "data/results";
  If[records === {},
    WriteString[$Output,
      "No data/results/*.json found; falling back to local runs/\n"];
    records = loadFromLocalRuns[runsBase];
    source  = "runs";
  ];
  If[records === {},
    abort[1, "no run records found in " <> dataDir <> " or " <> runsBase]];

  WriteString[$Output, "Source: ", source, "\n"];
  WriteString[$Output, "Discovered ", Length[records], " runs\n"];

  byModel = GroupBy[records, #["model"] &];

  WriteString[$Output, "Models:\n"];
  Scan[
    WriteString[$Output, "  ", #[[1]], "  (", Length[#[[2]]], " runs)\n"] &,
    SortBy[Normal[byModel], -Length[#[[2]]] &]];

  If[! DirectoryQ[outDir],
    CreateDirectory[outDir, CreateIntermediateDirectories -> True]];

  html = renderHTML[byModel];
  Module[{stream, htmlPath = FileNameJoin[{outDir, "report.html"}]},
    stream = OpenWrite[htmlPath, CharacterEncoding -> "UTF-8"];
    WriteString[stream, html];
    Close[stream];
  ];

  md = renderMarkdown[byModel];
  Module[{stream, mdPath = FileNameJoin[{outDir, "report.md"}]},
    stream = OpenWrite[mdPath, CharacterEncoding -> "UTF-8"];
    WriteString[stream, md];
    Close[stream];
  ];

  WriteString[$Output, "\nWrote:\n"];
  WriteString[$Output, "  ", FileNameJoin[{outDir, "report.html"}], "\n"];
  WriteString[$Output, "  ", FileNameJoin[{outDir, "report.md"}],   "\n"];
  Exit[0]
];

main[]
