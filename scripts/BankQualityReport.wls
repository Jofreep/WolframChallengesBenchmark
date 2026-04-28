#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Bank-quality dashboard.

   Reads the most recent runs/bank-self-test/<runId>/results.wxf plus
   private/canonical_solutions.jsonl, classifies every challenge, and
   emits an actionable per-challenge worklist as HTML + Markdown.

   Categories (one per challenge):
     fully_passing      every test passed
     empty_canonical    canonical_solution string is empty / missing
     parse_error        canonical doesn't parse (all tests ParseError)
     value_drift        canonical runs but produces wrong output
     timeout_or_other   eval error, timeout, memory, etc.
     partial            mix of pass + fail (subset of drift; reported separately)

   Drives bank-curation work: skim the dashboard, fix the
   empty_canonical / parse_error rows first (cheapest per-test recovery),
   then triage value_drift case-by-case.

   Usage:
     wolframscript -file scripts/BankQualityReport.wls \
       [--run         runs/bank-self-test/<runId>] \
       [--canonicals  private/canonical_solutions.jsonl] \
       [--out         runs/bank-quality]
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
(* I/O helpers                                                             *)
(* ----------------------------------------------------------------------- *)

mostRecentRunDir[] := Module[{base, dirs, complete},
  base = FileNameJoin[{$repoRoot, "runs", "bank-self-test"}];
  If[! DirectoryQ[base], Return[None]];
  dirs = FileNames["run-*", base];
  (* Only consider runs that actually completed: results.wxf must
     exist (the runner writes it last).  An interrupted run leaves
     just run.json + progress.jsonl. *)
  complete = Select[dirs,
    FileExistsQ[FileNameJoin[{#, "results.wxf"}]] &];
  If[complete === {}, None,
    First @ ReverseSortBy[complete, FileDate[#, "Modification"] &]
  ]
];

loadCanonicalSolutions[path_String] := Module[{lines, parsed},
  If[! FileExistsQ[path], Return[<||>]];
  lines = ReadList[path, "String"];
  parsed = DeleteCases[Quiet @ ImportString[#, "RawJSON"] & /@ lines, $Failed];
  Association @ Map[
    Function[r,
      Lookup[r, "task_id", "?"] -> Lookup[r, "canonical_solution", ""]
    ],
    Select[parsed, AssociationQ]
  ]
];

(* ----------------------------------------------------------------------- *)
(* Classification                                                          *)
(* ----------------------------------------------------------------------- *)

classifyChallenge[name_String, results_List, canonicals_Association] :=
  Module[{passed, failed, statuses, canonical, allParse, anyDrift,
          slowest, sampleFailure},
    canonical    = Lookup[canonicals, name, Missing[]];
    passed       = Count[results, _?(TrueQ[#["passed"]] &)];
    failed       = Length[results] - passed;
    statuses     = Counts[#["status"] & /@ results];
    allParse     = Length[results] > 0 &&
                   Lookup[statuses, "ParseError", 0] === Length[results];
    anyDrift     = AnyTrue[results,
                     #["status"] === "Evaluated" && ! TrueQ[#["passed"]] &];
    slowest      = If[results === {}, 0.0,
                     Max[Lookup[#, "durationSec", 0.] & /@ results]];
    sampleFailure = First[
      Select[results, ! TrueQ[#["passed"]] &], <||>];

    Which[
      passed === Length[results] && Length[results] > 0,
        <|"category" -> "fully_passing", "passed" -> passed,
          "total" -> Length[results], "slowest" -> slowest|>,

      ! StringQ[canonical] || StringTrim[canonical] === "",
        <|"category" -> "empty_canonical", "passed" -> passed,
          "total" -> Length[results], "slowest" -> slowest,
          "note" -> "no canonical solution provided"|>,

      allParse,
        <|"category" -> "parse_error", "passed" -> passed,
          "total" -> Length[results], "slowest" -> slowest,
          "note" -> "canonical solution does not parse"|>,

      anyDrift,
        <|"category" -> "value_drift", "passed" -> passed,
          "total" -> Length[results], "slowest" -> slowest,
          "sample" -> sampleFailure|>,

      True,
        <|"category" -> "timeout_or_other", "passed" -> passed,
          "total" -> Length[results], "slowest" -> slowest,
          "statuses" -> statuses,
          "sample" -> sampleFailure|>
    ]
  ];

(* ----------------------------------------------------------------------- *)
(* Render                                                                  *)
(* ----------------------------------------------------------------------- *)

categoryColor["fully_passing"]     := "#2e7d32";   (* green *)
categoryColor["empty_canonical"]   := "#9e9e9e";   (* gray *)
categoryColor["parse_error"]       := "#d84315";   (* deep orange *)
categoryColor["value_drift"]       := "#c62828";   (* red *)
categoryColor["timeout_or_other"]  := "#6a1b9a";   (* purple *)
categoryColor[_]                   := "#000000";

categoryEmoji["fully_passing"]     := "\[Checkmark]";
categoryEmoji["empty_canonical"]   := "?";
categoryEmoji["parse_error"]       := "P";
categoryEmoji["value_drift"]       := "X";
categoryEmoji["timeout_or_other"]  := "T";
categoryEmoji[_]                   := " ";

(* htmlEscape for safe HTML embedding (this script doesn't depend on the
   paclet's internal helper, so we duplicate). *)
htmlEsc[s_String] := StringReplace[s, {
  "&" -> "&amp;", "<" -> "&lt;", ">" -> "&gt;",
  "\"" -> "&quot;", "'" -> "&#39;"}];
htmlEsc[x_] := htmlEsc[ToString[x]];

renderHTML[byChallenge_Association, summary_Association,
           runDir_String, totalTests_, totalPassed_] := Module[
  {css, rows, header},

  css = "
    body { font-family: -apple-system, BlinkMacSystemFont, sans-serif;
           max-width: 1100px; margin: 2em auto; padding: 0 1em;
           color: #1a1a1a; }
    h1 { border-bottom: 2px solid #eee; padding-bottom: .3em; }
    .kpi { display: inline-block; padding: 1em 1.5em; margin: .3em;
           border: 1px solid #ddd; border-radius: 8px; background: #fafafa;
           min-width: 8em; }
    .kpi .v { font-size: 2em; font-weight: 700; display: block; }
    .kpi .l { color: #666; font-size: .9em; }
    table { border-collapse: collapse; width: 100%; margin: 1em 0;
            font-size: 14px; }
    th, td { border: 1px solid #ddd; padding: 6px 10px; text-align: left;
             vertical-align: top; }
    th { background: #f7f7f7; cursor: pointer; }
    .cat { font-weight: 600; }
    .nm  { font-family: 'SF Mono', Monaco, Menlo, monospace; }
    .num { text-align: right; font-variant-numeric: tabular-nums; }
    details { margin: .3em 0; }
    summary { cursor: pointer; }
    pre { background: #f4f4f4; padding: 6px 10px; border-radius: 4px;
          overflow-x: auto; }
  ";

  header = "<h1>Bank-quality dashboard</h1>" <>
    "<p>Source run: <code>" <> htmlEsc[runDir] <> "</code><br>" <>
    "Generated: " <> htmlEsc[DateString["ISODateTime"]] <> "</p>" <>

    "<div>" <>
    StringJoin @ MapThread[
      Function[{label, count, color},
        "<div class=\"kpi\"><span class=\"v\" style=\"color: " <> color <>
        "\">" <> ToString[count] <> "</span><span class=\"l\">" <>
        htmlEsc[label] <> "</span></div>"
      ],
      {{"Total challenges", "Fully passing", "Empty canonical",
        "Parse error", "Value drift", "Timeout / other"},
       {Length[byChallenge],
        Lookup[summary, "fully_passing", 0],
        Lookup[summary, "empty_canonical", 0],
        Lookup[summary, "parse_error", 0],
        Lookup[summary, "value_drift", 0],
        Lookup[summary, "timeout_or_other", 0]},
       {"#000", categoryColor["fully_passing"],
        categoryColor["empty_canonical"], categoryColor["parse_error"],
        categoryColor["value_drift"], categoryColor["timeout_or_other"]}}
    ] <> "</div>" <>

    "<p><strong>Test-level pass rate:</strong> " <> ToString[totalPassed] <>
    " / " <> ToString[totalTests] <> " (" <>
    ToString[NumberForm[100. totalPassed / Max[1, totalTests], {4, 2}]] <>
    "%)</p>";

  (* Sort: worst categories first (parse_error, empty_canonical, value_drift),
     then partial passes, then fully_passing.  Within each, alphabetical. *)
  rows = StringJoin @ Map[
    Function[entry,
      Module[{name = entry[[1]], info = entry[[2]], cat = entry[[2, "category"]],
              detail = ""},
        If[KeyExistsQ[info, "sample"],
          With[{s = info["sample"]},
            detail = "<details><summary>sample failing test</summary>" <>
              "<pre>testId:   " <> htmlEsc[Lookup[s, "testId", "?"]] <>
              "\nstatus:   " <> htmlEsc[Lookup[s, "status", "?"]] <>
              "\nexpected: " <> htmlEsc[
                ToString[Short[Lookup[s, "expected", "?"], 6], InputForm]] <>
              "\nactual:   " <> htmlEsc[
                ToString[Short[Lookup[s, "actualOutput", "?"], 8], InputForm]] <>
              "</pre></details>"
          ]
        ];
        "<tr>" <>
        "<td class=\"cat\" style=\"color: " <> categoryColor[cat] <> "\">" <>
          htmlEsc[cat] <> "</td>" <>
        "<td class=\"nm\">" <> htmlEsc[name] <> "</td>" <>
        "<td class=\"num\">" <> ToString[info["passed"]] <> " / " <>
          ToString[info["total"]] <> "</td>" <>
        "<td class=\"num\">" <>
          ToString[NumberForm[N[info["slowest"]], {6, 3}]] <> " s</td>" <>
        "<td>" <> htmlEsc[Lookup[info, "note", ""]] <> detail <> "</td>" <>
        "</tr>"
      ]
    ],
    SortBy[Normal[byChallenge],
      {Switch[#[[2, "category"]],
         "parse_error", 0, "empty_canonical", 1, "value_drift", 2,
         "timeout_or_other", 3, "fully_passing", 4, _, 5],
       #[[1]]} &]
  ];

  StringJoin[
    "<!doctype html><html><head><meta charset=\"utf-8\">",
    "<title>Bank quality \[LongDash] WolframChallengesBenchmark</title>",
    "<style>", css, "</style></head><body>",
    header,
    "<table><thead><tr>",
    "<th>Category</th><th>Challenge</th><th>Passed/Total</th>",
    "<th>Slowest test</th><th>Note / sample</th>",
    "</tr></thead><tbody>", rows, "</tbody></table>",
    "</body></html>"
  ]
];

renderMarkdown[byChallenge_Association, summary_Association,
               runDir_String, totalTests_, totalPassed_] := Module[
  {lines},
  lines = {
    "# Bank-quality dashboard",
    "",
    "Source run: `" <> runDir <> "`  ",
    "Generated: " <> DateString["ISODateTime"],
    "",
    "## Headline",
    "",
    "| Category          | Count |",
    "|-------------------|-------|",
    "| Total challenges  | " <> ToString[Length[byChallenge]]                  <> " |",
    "| Fully passing     | " <> ToString[Lookup[summary, "fully_passing", 0]]   <> " |",
    "| Empty canonical   | " <> ToString[Lookup[summary, "empty_canonical", 0]] <> " |",
    "| Parse error       | " <> ToString[Lookup[summary, "parse_error", 0]]     <> " |",
    "| Value drift       | " <> ToString[Lookup[summary, "value_drift", 0]]     <> " |",
    "| Timeout / other   | " <> ToString[Lookup[summary, "timeout_or_other", 0]]<> " |",
    "",
    "Test-level pass rate: **" <> ToString[totalPassed] <> " / " <>
      ToString[totalTests] <> " (" <>
      ToString[NumberForm[100. totalPassed / Max[1, totalTests], {4, 2}]] <>
      "%)**",
    "",
    "## Per-challenge worklist",
    "",
    "Sorted with the most-actionable categories first: parse errors, empty",
    "canonicals, value drift.  Skim top to bottom \[LongDash] each row is",
    "one bank-curation TODO.",
    "",
    "| Category | Challenge | Passed/Total | Slowest | Note |",
    "|----------|-----------|--------------|---------|------|"
  };

  AppendTo[lines, ""];
  Module[{rows = SortBy[Normal[byChallenge],
    {Switch[#[[2, "category"]],
       "parse_error", 0, "empty_canonical", 1, "value_drift", 2,
       "timeout_or_other", 3, "fully_passing", 4, _, 5],
     #[[1]]} &]},
    Scan[
      Function[entry,
        Module[{name = entry[[1]], info = entry[[2]]},
          AppendTo[lines,
            "| " <> info["category"] <> " | " <> name <> " | " <>
              ToString[info["passed"]] <> " / " <>
              ToString[info["total"]] <> " | " <>
              ToString[NumberForm[N[info["slowest"]], {6, 3}]] <> " s | " <>
              StringTake[Lookup[info, "note", ""], UpTo[60]] <> " |"
          ]
        ]
      ],
      rows
    ]
  ];

  StringRiffle[lines, "\n"]
];

(* ----------------------------------------------------------------------- *)
(* Main                                                                    *)
(* ----------------------------------------------------------------------- *)

main[] := Module[
  {args, runDir, canonicalsPath, outDir, results, canonicals, byTask,
   byChallenge, summary, html, md, totalPassed},

  args         = parseArgs[Drop[$ScriptCommandLine, 1]];
  runDir       = Lookup[args, "run", mostRecentRunDir[]];
  canonicalsPath = Lookup[args, "canonicals",
    FileNameJoin[{$repoRoot, "private", "canonical_solutions.jsonl"}]];
  outDir       = Lookup[args, "out",
    FileNameJoin[{$repoRoot, "runs", "bank-quality"}]];

  If[runDir === None || ! DirectoryQ[runDir],
    abort[1, "no bank-self-test run directory found. Run scripts/BankSelfTest.wls first."]
  ];

  WriteString[$Output, "Reading run from ", runDir, " ...\n"];
  results = Quiet @ Check[
    Import[FileNameJoin[{runDir, "results.wxf"}], "WXF"],
    $Failed];
  If[results === $Failed || ! ListQ[results],
    abort[2, "could not read results.wxf from " <> runDir]];

  canonicals = loadCanonicalSolutions[canonicalsPath];
  WriteString[$Output, "Read ", Length[canonicals],
    " canonical solutions from ", canonicalsPath, "\n"];

  byTask     = GroupBy[results, #["challengeName"] &];
  byChallenge = AssociationMap[
    classifyChallenge[#, byTask[#], canonicals] &,
    Keys[byTask]
  ];

  summary    = Counts[#["category"] & /@ Values[byChallenge]];
  totalPassed = Total[Lookup[#, "passed", 0] & /@ Values[byChallenge]];

  WriteString[$Output, "\nClassification:\n"];
  Scan[WriteString[$Output, "  ", #[[1]], ": ", #[[2]], "\n"] &,
    ReverseSortBy[Normal[summary], Last]];

  If[! DirectoryQ[outDir],
    CreateDirectory[outDir, CreateIntermediateDirectories -> True]];

  html = renderHTML[byChallenge, summary, runDir,
                    Length[results], totalPassed];
  Export[FileNameJoin[{outDir, "report.html"}], html, "Text",
    CharacterEncoding -> "UTF-8"];

  md = renderMarkdown[byChallenge, summary, runDir,
                      Length[results], totalPassed];
  (* Use a low-level Put through a Stream so Export's format detection
     can't ever rewrite the path or content for `.md` extensions. *)
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
