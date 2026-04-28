#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Leaderboard dashboard.

   Walks runs/**/run.json (excluding bank-self-test / trend /
   bank-quality / compare subdirectories), takes the most recent
   benchmark run per model, ranks them by pass rate, and emits an
   HTML + Markdown leaderboard.

   HTML uses a podium-style top-3 (gold / silver / bronze) plus a
   ranked table for the rest.  Auto-populates as new runs land.

   Usage:
     wolframscript -file scripts/Leaderboard.wls \
       [--runs runs] \
       [--out  runs/leaderboard]
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


(* ----------------------------------------------------------------------- *)
(* Discover benchmark runs (not self-test, not generated dashboards)       *)
(* ----------------------------------------------------------------------- *)

discoverBenchmarkRuns[runsBase_String] := Module[{paths},
  If[! DirectoryQ[runsBase], Return[{}]];
  paths = FileNames["run.json", runsBase, Infinity];
  Select[paths,
    ! StringContainsQ[#, "/bank-self-test/"] &&
    ! StringContainsQ[#, "/trend/"]          &&
    ! StringContainsQ[#, "/leaderboard/"]    &&
    ! StringContainsQ[#, "/bank-quality/"]   &&
    ! StringContainsQ[#, "/compare/"]        &]
];

loadRun[runJsonPath_String] := Module[{r, sum},
  r   = Quiet @ Check[Import[runJsonPath, "RawJSON"], $Failed];
  If[! AssociationQ[r], Return[Missing[]]];
  sum = Lookup[r, "summary", <||>];
  <|
    "runId"      -> Lookup[r, "runId",      "?"],
    "model"      -> Lookup[r, "model",      "?"],
    "createdAt"  -> Lookup[r, "createdAt",  "?"],
    "passRate"   -> Lookup[sum, "passRate", 0.],
    "passed"     -> Lookup[sum, "passed", 0],
    "total"      -> Lookup[sum, "total",  0],
    "challengesFullyPassing" -> Lookup[sum, "challengesFullyPassing", 0],
    "challengesAttempted"    -> Lookup[sum, "challengesAttempted", 0],
    "durationSec" -> Lookup[r, "durationSec", 0.]
  |>
];

(* For each model, keep the run with the LATEST createdAt.  That's the
   model's current standing on the leaderboard.  (Use --filter or
   manual edits if you want to lock in a specific run.) *)
latestPerModel[runs_List] := Module[{groups},
  groups = GroupBy[runs, #["model"] &];
  Last[SortBy[#, #["createdAt"] &]] & /@ groups
];


(* ----------------------------------------------------------------------- *)
(* Render                                                                  *)
(* ----------------------------------------------------------------------- *)

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


podiumCard[rank_Integer, run_Association, color_String, medal_String] :=
  StringJoin[
    "<div class=\"podium-card podium-rank-", ToString[rank], "\" ",
       "style=\"--accent: ", color, "\">",
    "<div class=\"podium-medal\">", medal, "</div>",
    "<div class=\"podium-rank\">#", ToString[rank], "</div>",
    "<div class=\"podium-model\">", htmlEsc[run["model"]], "</div>",
    "<div class=\"podium-rate\">",
       ToString[NumberForm[100. run["passRate"], {4, 2}]], "<span>%</span>",
    "</div>",
    "<div class=\"podium-detail\">",
       ToString[run["passed"]], " / ",
       ToString[run["total"]], " tests",
    "</div>",
    "<div class=\"podium-detail\">",
       ToString[run["challengesFullyPassing"]], " of ",
       ToString[run["challengesAttempted"]], " challenges fully passing",
    "</div>",
    "<div class=\"podium-detail small\">",
       htmlEsc[run["createdAt"]], " \[Bullet] ",
       htmlEsc[formatDuration[run["durationSec"]]],
    "</div>",
    "</div>"
  ];

renderHTML[runs_List] := Module[{ranked, top3, rest, podium, css, table},
  ranked = ReverseSortBy[runs, {#["passRate"], #["createdAt"]} &];
  top3   = Take[ranked, UpTo[3]];
  rest   = Drop[ranked, UpTo[3]];

  css = "
    body { font-family: -apple-system, BlinkMacSystemFont, sans-serif;
           max-width: 1100px; margin: 2em auto; padding: 0 1em;
           color: #1a1a1a; }
    h1 { border-bottom: 2px solid #eee; padding-bottom: .3em;
         display: flex; align-items: baseline; gap: .5em; }
    h1 .crown { font-size: .8em; }
    .meta { color: #666; font-size: 14px; margin-bottom: 2em; }
    .podium { display: grid; grid-template-columns: 1fr 1fr 1fr;
              gap: 1em; margin: 1.5em 0; align-items: end; }
    .podium-card { background: #fff; border: 1px solid #e5e5e5;
                   border-radius: 12px; padding: 1.2em 1em 1.5em;
                   text-align: center; box-shadow: 0 2px 6px rgba(0,0,0,.04);
                   border-top: 6px solid var(--accent); position: relative; }
    .podium-medal { font-size: 2.4em; line-height: 1; margin-bottom: .2em; }
    .podium-rank-1 { transform: translateY(-12px);
                     box-shadow: 0 6px 20px rgba(212,175,55,.25); }
    .podium-rank-2 { transform: translateY(0); }
    .podium-rank-3 { transform: translateY(0); }
    .podium-rank { color: var(--accent); font-weight: 700;
                   font-size: .9em; letter-spacing: .05em; }
    .podium-model { font-size: 1.1em; font-weight: 600; margin: .3em 0 .6em;
                    word-break: break-word; }
    .podium-rate { font-size: 2.6em; font-weight: 800;
                   color: var(--accent); line-height: 1; }
    .podium-rate span { font-size: .55em; color: #888; font-weight: 600;
                        margin-left: .1em; }
    .podium-detail { color: #555; font-size: 13px; margin-top: .4em; }
    .podium-detail.small { color: #999; font-size: 11px; margin-top: .8em;
                           font-family: 'SF Mono', Monaco, monospace; }
    table { border-collapse: collapse; width: 100%; margin: 1em 0;
            font-size: 14px; }
    th, td { border: 1px solid #ddd; padding: 8px 12px; text-align: left;
             vertical-align: top; }
    th { background: #f7f7f7; }
    .num { text-align: right; font-variant-numeric: tabular-nums; }
    .nm  { font-family: 'SF Mono', Monaco, Menlo, monospace; font-size: 12px;
           color: #666; }
    .pct { font-weight: 600; color: #222; }
    .rank-cell { font-weight: 700; color: #888; text-align: center; width: 3em; }
    .empty { padding: 2em; text-align: center; color: #888;
             border: 1px dashed #ccc; border-radius: 8px; margin: 1em 0; }
  ";

  podium =
    If[Length[top3] >= 1,
      Module[{cards = {}},
        (* Order in the grid: silver | gold | bronze (so #1 sits
           visually in the middle and gets the elevated treatment). *)
        If[Length[top3] >= 2,
          AppendTo[cards, podiumCard[2, top3[[2]], "#9e9e9e", "\:1f948"]]];
        AppendTo[cards, podiumCard[1, top3[[1]], "#d4af37", "\:1f947"]];
        If[Length[top3] >= 3,
          AppendTo[cards, podiumCard[3, top3[[3]], "#cd7f32", "\:1f949"]]];
        "<div class=\"podium\">" <> StringJoin @ cards <> "</div>"
      ],
      "<div class=\"empty\">No benchmark runs yet. Run scripts/RunBenchmark.wls against any solutions/&lt;model&gt;/ to populate.</div>"
    ];

  table = If[ranked === {},
    "",
    StringJoin[
      "<h2>Full ranking</h2>",
      "<table><thead><tr>",
      "<th class=\"rank-cell\">Rank</th>",
      "<th>Model</th>",
      "<th class=\"num\">Pass rate</th>",
      "<th class=\"num\">Tests</th>",
      "<th class=\"num\">Challenges full</th>",
      "<th class=\"num\">Run date</th>",
      "<th class=\"num\">Duration</th>",
      "</tr></thead><tbody>",
      StringJoin @ MapIndexed[
        Function[{r, idxL},
          Module[{rank = First[idxL]},
            "<tr>" <>
            "<td class=\"rank-cell\">" <> ToString[rank] <> "</td>" <>
            "<td>" <> htmlEsc[r["model"]] <>
              "<div class=\"nm\">" <> htmlEsc[r["runId"]] <> "</div></td>" <>
            "<td class=\"num pct\">" <>
              ToString[NumberForm[100. r["passRate"], {4, 2}]] <> "%</td>" <>
            "<td class=\"num\">" <> ToString[r["passed"]] <> " / " <>
              ToString[r["total"]] <> "</td>" <>
            "<td class=\"num\">" <>
              ToString[r["challengesFullyPassing"]] <> " / " <>
              ToString[r["challengesAttempted"]] <> "</td>" <>
            "<td class=\"num\">" <> htmlEsc[r["createdAt"]] <> "</td>" <>
            "<td class=\"num\">" <>
              htmlEsc[formatDuration[r["durationSec"]]] <> "</td>" <>
            "</tr>"
          ]
        ],
        ranked
      ],
      "</tbody></table>"
    ]
  ];

  StringJoin[
    "<!doctype html><html><head><meta charset=\"utf-8\">",
    "<title>Leaderboard \[LongDash] WolframChallengesBenchmark</title>",
    "<style>", css, "</style></head><body>",
    "<h1><span class=\"crown\">\:1f3c6</span> Leaderboard</h1>",
    "<div class=\"meta\">",
    ToString[Length[ranked]], " models ranked \[Bullet] ",
    "Generated: ", htmlEsc[DateString["ISODateTime"]], " \[Bullet] ",
    "Each model's most recent benchmark run is shown.",
    "</div>",
    podium, table,
    "</body></html>"
  ]
];

renderMarkdown[runs_List] := Module[{ranked, lines},
  ranked = ReverseSortBy[runs, {#["passRate"], #["createdAt"]} &];

  lines = {
    "# \:1f3c6 Leaderboard",
    "",
    ToString[Length[ranked]] <> " models ranked \[Bullet] generated " <>
      DateString["ISODateTime"] <> ".",
    "",
    "Each model's most recent benchmark run.",
    "",
    "| Rank | Model | Pass rate | Tests | Challenges full | Run date | Duration |",
    "|-----:|-------|----------:|------:|----------------:|----------|---------:|"
  };

  MapIndexed[
    Function[{r, idxL},
      AppendTo[lines,
        "| " <> ToString[First[idxL]] <>
        " | " <> r["model"] <>
        " | " <> ToString[NumberForm[100. r["passRate"], {4, 2}]] <> "%" <>
        " | " <> ToString[r["passed"]] <> " / " <> ToString[r["total"]] <>
        " | " <> ToString[r["challengesFullyPassing"]] <> " / " <>
                 ToString[r["challengesAttempted"]] <>
        " | " <> r["createdAt"] <>
        " | " <> formatDuration[r["durationSec"]] <>
        " |"]
    ],
    ranked
  ];

  StringRiffle[lines, "\n"]
];


(* ----------------------------------------------------------------------- *)
(* Main                                                                    *)
(* ----------------------------------------------------------------------- *)

main[] := Module[
  {args, runsBase, outDir, paths, allRuns, latest, html, md},

  args     = parseArgs[Drop[$ScriptCommandLine, 1]];
  runsBase = Lookup[args, "runs", FileNameJoin[{$repoRoot, "runs"}]];
  outDir   = Lookup[args, "out",  FileNameJoin[{$repoRoot, "runs", "leaderboard"}]];

  paths = discoverBenchmarkRuns[runsBase];
  WriteString[$Output, "Discovered ", Length[paths],
    " benchmark run.json files\n"];

  allRuns = DeleteCases[loadRun /@ paths, _Missing];
  latest  = Values @ latestPerModel[allRuns];

  WriteString[$Output, "Models ranked: ", Length[latest], "\n"];
  Scan[
    WriteString[$Output, "  ",
      ToString[NumberForm[100. #["passRate"], {4, 2}]], "%  ",
      #["model"], "\n"] &,
    ReverseSortBy[latest, #["passRate"] &]
  ];

  If[! DirectoryQ[outDir],
    CreateDirectory[outDir, CreateIntermediateDirectories -> True]];

  html = renderHTML[latest];
  Module[{stream, htmlPath = FileNameJoin[{outDir, "report.html"}]},
    stream = OpenWrite[htmlPath, CharacterEncoding -> "UTF-8"];
    WriteString[stream, html];
    Close[stream];
  ];

  md = renderMarkdown[latest];
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
