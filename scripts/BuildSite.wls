#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Build a publishable static site bundling all four dashboards.

   Runs Leaderboard, ModelStrengths, TrendReport, and BankQualityReport
   so each writes its HTML + Markdown into runs/<name>/ as usual,
   then copies the HTML outputs into _site/<name>/index.html and
   writes a styled top-level _site/index.html that links each.

   Also copies key markdown documentation into _site/docs/ so the
   landing page can link to README, JSONL format spec, demo guide,
   etc., and they render nicely on Pages.

   Usage:
     wolframscript -file scripts/BuildSite.wls \
       [--out _site] \
       [--skip-bank-quality]   # bank-self-test must already exist; \
                               # skip to publish without re-running it

   The site is deterministic given the same runs/ contents.  Re-build
   any time a new model run lands.
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

(* Helper: invoke another wolframscript in this same process (so we
   share the kernel and the paclet load).  Returns True on success. *)
runScript[scriptPath_String, args_List : {}] := Module[{cmd, exit},
  cmd = "wolframscript -file " <> scriptPath <>
        StringJoin[" " <> # & /@ args];
  WriteString[$Output, "  $ ", cmd, "\n"];
  exit = RunProcess[
    Prepend[Flatten[{"-file", scriptPath, args}], "wolframscript"],
    "ExitCode"];
  exit === 0
];

(* Copy a file into a target dir as index.html, creating the dir if
   needed. *)
publishHtml[srcPath_String, dstDir_String] := Module[{},
  If[! FileExistsQ[srcPath],
    WriteString[$Output, "  WARN: ", srcPath, " not found; skipping\n"];
    Return[False]];
  If[! DirectoryQ[dstDir],
    CreateDirectory[dstDir, CreateIntermediateDirectories -> True]];
  CopyFile[srcPath, FileNameJoin[{dstDir, "index.html"}],
    OverwriteTarget -> True];
  WriteString[$Output, "  -> ", FileNameJoin[{dstDir, "index.html"}], "\n"];
  True
];

(* ----------------------------------------------------------------------- *)
(* Landing page                                                            *)
(* ----------------------------------------------------------------------- *)

htmlEsc[s_String] := StringReplace[s, {
  "&" -> "&amp;", "<" -> "&lt;", ">" -> "&gt;",
  "\"" -> "&quot;", "'" -> "&#39;"}];
htmlEsc[x_] := htmlEsc[ToString[x]];

renderLanding[available_Association] := Module[{cards, cssBundle},

  cssBundle = "
    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI',
           Inter, sans-serif; max-width: 900px; margin: 2em auto;
           padding: 0 1.5em; color: #1a1a1a;
           background: linear-gradient(180deg, #fafafa 0%, #fff 100%); }
    header { padding: 2em 0 1em; border-bottom: 1px solid #eee; }
    h1 { margin: 0 0 .2em; font-size: 2.2em; letter-spacing: -.02em; }
    h1 .crown { font-size: .8em; }
    .subtitle { color: #666; font-size: 1.05em; margin: 0; }
    .meta { color: #999; font-size: .85em; margin-top: .8em; }
    .grid { display: grid; gap: 1em; margin: 2em 0;
            grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); }
    .card { background: #fff; border: 1px solid #e5e5e5;
            border-radius: 12px; padding: 1.2em 1.4em;
            text-decoration: none; color: inherit;
            transition: transform .12s, box-shadow .12s, border-color .12s; }
    .card:hover { transform: translateY(-2px);
                  box-shadow: 0 6px 20px rgba(0,0,0,.06);
                  border-color: #d0d0d0; }
    .card.disabled { opacity: .5; cursor: not-allowed; pointer-events: none; }
    .card-icon { font-size: 1.8em; margin-bottom: .3em; }
    .card-title { font-size: 1.15em; font-weight: 600; margin: 0 0 .3em; }
    .card-desc { font-size: .9em; color: #555; margin: 0 0 .5em; }
    .card-status { font-size: .75em; color: #888; font-style: italic; }
    footer { margin-top: 3em; padding-top: 1.5em; border-top: 1px solid #eee;
             font-size: .85em; color: #888; }
    footer a { color: #0066cc; text-decoration: none; }
    footer a:hover { text-decoration: underline; }
  ";

  cards = {
    {"leaderboard",     "\|01f3c6", "Leaderboard",
     "Podium-style ranking of every model that has been benchmarked.  Pass rate, challenges fully passing, run date.  Top-3 highlighted."},
    {"model-strengths", "\|01f9e0", "Model strengths",
     "Per-track pass-rate matrix for every model.  Top-3 strongest / weakest tracks per model.  Where each LLM is great vs. where it falls over."},
    {"trend",           "\|01f4c8", "Trend",
     "Pass-rate over time per model with inline sparklines.  Useful once a model has accumulated several runs against the bank."},
    {"bank-quality",    "\|01f50d", "Bank quality",
     "Per-challenge classification: empty canonical / parse error / value drift / fully passing.  Drives the bank-curation worklist."}
  };

  StringJoin[
    "<!doctype html><html><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    "<title>WolframChallengesBenchmark</title>",
    "<style>", cssBundle, "</style></head><body>",

    "<header>",
    "<h1><span class=\"crown\">\|01f3c6</span> WolframChallengesBenchmark</h1>",
    "<p class=\"subtitle\">A production-grade benchmark for grading ",
    "LLM-generated Wolfram Language solutions.</p>",
    "<p class=\"meta\">Generated ", htmlEsc[DateString["ISODateTime"]], "</p>",
    "</header>",

    "<div class=\"grid\">",
    StringJoin @ Map[
      Function[entry,
        Module[{slug = entry[[1]], icon = entry[[2]],
                title = entry[[3]], desc = entry[[4]], status, cls},
          {status, cls} = If[TrueQ[available[slug]],
            {"\[FilledRightTriangle] open report",  "card"},
            {"not available",                       "card disabled"}];
          "<a class=\"" <> cls <> "\" href=\"" <> slug <> "/\">" <>
          "<div class=\"card-icon\">" <> icon <> "</div>" <>
          "<div class=\"card-title\">" <> htmlEsc[title] <> "</div>" <>
          "<p class=\"card-desc\">" <> htmlEsc[desc] <> "</p>" <>
          "<div class=\"card-status\">" <> status <> "</div>" <>
          "</a>"
        ]
      ],
      cards
    ],
    "</div>",

    "<footer>",
    "<p>Source: <a href=\"https://github.com/Jofreep/WolframChallengesBenchmark\">",
    "github.com/Jofreep/WolframChallengesBenchmark</a><br>",
    "166 challenges \[Bullet] one-shot LLM \[Rule] sandboxed runner ",
    "\[Rule] typed results \[Rule] reports.<br>",
    "MIT licensed.</p>",
    "</footer>",

    "</body></html>"
  ]
];

(* ----------------------------------------------------------------------- *)
(* Main                                                                    *)
(* ----------------------------------------------------------------------- *)

main[] := Module[
  {args, outDir, skipBankQuality, available = <||>, indexHtml,
   srcLeaderboard, srcStrengths, srcTrend, srcBankQuality},

  args            = parseArgs[Drop[$ScriptCommandLine, 1]];
  outDir          = Lookup[args, "out", FileNameJoin[{$repoRoot, "_site"}]];
  skipBankQuality = KeyExistsQ[args, "skip-bank-quality"];

  WriteString[$Output, "Building site at ", outDir, "\n\n"];

  (* Re-run each dashboard so its HTML is fresh.  Skip silently if a
     dashboard fails (e.g. no benchmark runs yet); the landing page
     marks unavailable cards as "not available". *)
  WriteString[$Output, "Running dashboards:\n"];

  WriteString[$Output, "  Leaderboard ...\n"];
  runScript[FileNameJoin[{$scriptDir, "Leaderboard.wls"}]];

  WriteString[$Output, "  ModelStrengths ...\n"];
  runScript[FileNameJoin[{$scriptDir, "ModelStrengths.wls"}]];

  WriteString[$Output, "  TrendReport ...\n"];
  runScript[FileNameJoin[{$scriptDir, "TrendReport.wls"}]];

  If[! skipBankQuality,
    WriteString[$Output, "  BankQualityReport ...\n"];
    runScript[FileNameJoin[{$scriptDir, "BankQualityReport.wls"}]],
    WriteString[$Output, "  BankQualityReport [skipped]\n"];
  ];

  (* Wipe and re-create the site dir so stale subdirs from previous
     builds don't leak through. *)
  If[DirectoryQ[outDir],
    Quiet @ DeleteDirectory[outDir, DeleteContents -> True]];
  CreateDirectory[outDir, CreateIntermediateDirectories -> True];

  WriteString[$Output, "\nPublishing into ", outDir, ":\n"];

  srcLeaderboard = FileNameJoin[{$repoRoot, "runs", "leaderboard",      "report.html"}];
  srcStrengths   = FileNameJoin[{$repoRoot, "runs", "model-strengths",  "report.html"}];
  srcTrend       = FileNameJoin[{$repoRoot, "runs", "trend",            "report.html"}];
  srcBankQuality = FileNameJoin[{$repoRoot, "runs", "bank-quality",     "report.html"}];

  available["leaderboard"]     = publishHtml[srcLeaderboard,
                                  FileNameJoin[{outDir, "leaderboard"}]];
  available["model-strengths"] = publishHtml[srcStrengths,
                                  FileNameJoin[{outDir, "model-strengths"}]];
  available["trend"]           = publishHtml[srcTrend,
                                  FileNameJoin[{outDir, "trend"}]];
  available["bank-quality"]    = publishHtml[srcBankQuality,
                                  FileNameJoin[{outDir, "bank-quality"}]];

  (* Landing page. *)
  indexHtml = renderLanding[available];
  Module[{stream, p = FileNameJoin[{outDir, "index.html"}]},
    stream = OpenWrite[p, CharacterEncoding -> "UTF-8"];
    WriteString[stream, indexHtml];
    Close[stream];
    WriteString[$Output, "  -> ", p, " [landing]\n"];
  ];

  (* Disable Jekyll on Pages so files starting with _ aren't ignored. *)
  Module[{stream, p = FileNameJoin[{outDir, ".nojekyll"}]},
    stream = OpenWrite[p];
    Close[stream];
  ];

  WriteString[$Output, "\nSite ready.  Open ",
    FileNameJoin[{outDir, "index.html"}], " locally,\n",
    "or git push to deploy via .github/workflows/publish-dashboards.yml.\n"];

  Exit[0]
];

main[]
