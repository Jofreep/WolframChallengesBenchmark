#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Per-tag pass-rate matrix per model.

   "Where are LLMs strong vs weak?"

   Reads each model's most recent benchmark run, groups results by
   topic tag (loaded from challenges.jsonl), and emits an HTML +
   Markdown matrix:

       rows: tags (sorted by aggregate model pass rate, hardest first)
       cols: models (sorted by overall pass rate, best first)
       cell: pass rate for that model on that tag, color-graded

   Plus a per-model strengths/weaknesses snippet listing each model's
   top-3 strongest tags and top-3 weakest tags.

   Data sources, in priority order:
     1. data/results/<modelSlug>.json  (tracked, published by
        scripts/PublishResults.wls).  Contains per-test pass/fail
        with NO solution source code.  This is what the public site
        uses.
     2. runs/run-*/results.wxf         (local-only fallback, for
        rebuilding the dashboard mid-run before publishing).

   Usage:
     wolframscript -file scripts/ModelStrengths.wls \
       [--data    data/results] \
       [--runs    runs] \
       [--jsonl   challenges.jsonl] \
       [--out     runs/model-strengths]
*)

$scriptDir = DirectoryName[$InputFileName];
$repoRoot  = DirectoryName[$scriptDir];
$pacletDir = FileNameJoin[{$repoRoot, "WolframChallengesLLMBenchmarkPaclet"}];

Quiet @ PacletDirectoryLoad[$pacletDir];
Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

(* ----------------------------------------------------------------------- *)
(* Tag -> Track mapping                                                    *)
(*                                                                         *)
(* The Wolfram Challenges site organizes challenges into 15 official       *)
(* "tracks": Start Here, Algorithms, Calendar, Coding, Digits, Geography,  *)
(* Geometry, Lists and Arrays, Math Puzzles, Palindromes, Primes,          *)
(* Real-World Data, Roman Numerals, Sequences, Strings, Words.             *)
(*                                                                         *)
(* These are a more interpretable axis for "where are LLMs weak vs.        *)
(* strong" than the fine-grained tags (which include things like           *)
(* "computational-sciences" and "factorization" that don't translate to    *)
(* a benchmark insight).  We map the granular tag slugs to track names    *)
(* via the table below, plus a few name-pattern overrides for tracks      *)
(* whose tag signal isn't strong enough on its own (Roman / Palindromes). *)
(* A single challenge can belong to multiple tracks; the strengths         *)
(* matrix counts each tag-passing test once per track it touches.          *)
(* ----------------------------------------------------------------------- *)

$tagToTracks = <|
  (* Tags that map directly to a track of the same name. *)
  "start-here"           -> {"Start Here"},
  "algorithms"           -> {"Algorithms"},
  "calendar"             -> {"Calendar"},
  "geography"            -> {"Geography"},
  "primes"               -> {"Primes"},
  "sequences"            -> {"Sequences"},
  "strings"              -> {"Strings"},

  (* Code-related tags collapse into the Coding track. *)
  "code-as-data"         -> {"Coding"},
  "metaprogramming"      -> {"Coding"},
  "wolfram-language"     -> {"Coding"},

  (* Math + geometry tags collapse into Geometry / Math Puzzles. *)
  "geometry"             -> {"Geometry"},
  "plane-geometry"       -> {"Geometry"},
  "triangles"            -> {"Geometry"},
  "polygons"             -> {"Geometry"},
  "circles"              -> {"Geometry"},
  "conic-sections"       -> {"Geometry"},
  "symmetry"             -> {"Geometry"},
  "linear-algebra"       -> {"Geometry"},
  "matrix-transformations"-> {"Geometry"},
  "area"                 -> {"Geometry"},
  "volume"               -> {"Geometry"},

  "recreational-mathematics" -> {"Math Puzzles"},
  "game-theory"          -> {"Math Puzzles"},
  "combinatorics"        -> {"Math Puzzles"},
  "approximations"       -> {"Math Puzzles"},
  "complex-numbers"      -> {"Math Puzzles"},
  "polynomials"          -> {"Math Puzzles"},
  "roots"                -> {"Math Puzzles"},
  "series"               -> {"Math Puzzles"},
  "derivatives"          -> {"Math Puzzles"},
  "factorization"        -> {"Math Puzzles"},
  "number-theory"        -> {"Math Puzzles"},
  "unsolved-problems"    -> {"Math Puzzles"},
  "special-functions"    -> {"Math Puzzles"},
  "continued-fractions"  -> {"Math Puzzles"},
  "permutations"         -> {"Math Puzzles"},
  "sorting"              -> {"Math Puzzles"},
  "graph-theory"         -> {"Math Puzzles"},
  "cellular-automata"    -> {"Math Puzzles"},

  (* Lists / arrays. *)
  "lists"                -> {"Lists and Arrays"},
  "arrays"               -> {"Lists and Arrays"},

  (* Words / linguistics. *)
  "words-and-linguistics"-> {"Words"},

  (* Pi-themed, digit-themed challenges go in Digits. *)
  "pi"                   -> {"Digits"},

  (* Real-world / curated data lookups. *)
  "computational-knowledge" -> {"Real-World Data"},
  "people-and-history"   -> {"Real-World Data"},
  "image-processing"     -> {"Real-World Data"},
  "art"                  -> {"Real-World Data"},
  "color"                -> {"Real-World Data"},
  "colors"               -> {"Real-World Data"},
  "graphics"             -> {"Real-World Data"},
  "formatting"           -> {"Real-World Data"},
  "architecture"         -> {"Real-World Data"},
  "gravity"              -> {"Real-World Data"},
  "spiral"               -> {"Real-World Data"},
  "space-filling-curves" -> {"Real-World Data"},

  (* Crypto / hash family tags lean Coding. *)
  "cryptography"         -> {"Coding"},
  "hash"                 -> {"Coding"},
  "message-digest"       -> {"Coding"},
  "cycle"                -> {"Coding"},
  "loop"                 -> {"Coding"},
  "group"                -> {"Coding"},

  (* Misc / soft-overlap tags that don't drive any track decision. *)
  "computational-sciences"  -> {},
  "computation-knowledge"   -> {},
  "mathematics"             -> {},   (* too broad on its own; other tags carry the signal *)
  "numerical-representation-of-integers" -> {"Digits"}
|>;

(* tracksForChallenge: take the challenge name + its list of tag slugs,
   return the deduped list of track names it belongs to.  Adds Roman
   Numerals / Palindromes / Digits via name-pattern matching when the
   tag signal alone is insufficient. *)
tracksForChallenge[name_String, tags_List] := Module[{tracks},
  tracks = Flatten[Lookup[$tagToTracks, tags, {}]];

  If[StringContainsQ[name, "Roman" | "Numeral", IgnoreCase -> True],
    AppendTo[tracks, "Roman Numerals"]];
  If[StringContainsQ[name, "Palindrom", IgnoreCase -> True],
    AppendTo[tracks, "Palindromes"]];
  If[StringContainsQ[name, "Digit", IgnoreCase -> True],
    AppendTo[tracks, "Digits"]];

  (* Math-tagged challenges that didn't land in any track via above
     rules fall back to Math Puzzles so they're not orphaned. *)
  If[tracks === {} && MemberQ[tags, "mathematics"],
    AppendTo[tracks, "Math Puzzles"]];

  DeleteDuplicates[tracks]
];

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
(*                                                                         *)
(* The two source formats produce the same shape downstream:               *)
(*    model -> {<|"challengeName" -> ..., "passed" -> ...|>, ...}          *)
(* The accumulation loop below cares about exactly those two fields per    *)
(* test, so we normalize at load time and keep the rest of the pipeline    *)
(* source-agnostic.                                                        *)
(* ----------------------------------------------------------------------- *)

(* Primary: data/results/<modelSlug>.json.  Each file's "runs" array is
   sorted by createdAt; we take the last entry's perTest list. *)
loadModelTestsFromPublished[dataDir_String] := Module[
  {paths, out = <||>},
  If[! DirectoryQ[dataDir], Return[<||>]];
  paths = FileNames["*.json", dataDir];
  Scan[
    Function[p,
      Module[{doc, model, history, latest, perTest},
        doc = Quiet @ Check[Import[p, "RawJSON"], $Failed];
        If[AssociationQ[doc],
          model   = Lookup[doc, "model", "?"];
          history = Lookup[doc, "runs", {}];
          If[history =!= {},
            latest = Last @ SortBy[history, Lookup[#, "createdAt"] &];
            perTest = Lookup[latest, "perTest", {}];
            (* Normalize field names (published uses "challenge"; the
               accumulator wants "challengeName"). *)
            out[model] = Map[
              <|"challengeName" -> Lookup[#, "challenge", "?"],
                "passed"        -> TrueQ[Lookup[#, "passed", False]]|> &,
              perTest
            ];
          ]
        ]
      ]
    ],
    paths
  ];
  out
];

(* Fallback: scan runs/, take latest run per model that has a finished
   results.wxf, import each.  Same field set, normalized to challengeName. *)
loadModelTestsFromLocalRuns[runsBase_String] := Module[
  {paths, runRecs, byModel, latest, out = <||>},
  If[! DirectoryQ[runsBase], Return[<||>]];
  paths = Select[
    FileNames["run.json", runsBase, Infinity],
    ! StringContainsQ[#, "/bank-self-test/"]    &&
    ! StringContainsQ[#, "/trend/"]              &&
    ! StringContainsQ[#, "/leaderboard/"]        &&
    ! StringContainsQ[#, "/bank-quality/"]       &&
    ! StringContainsQ[#, "/model-strengths/"]    &&
    ! StringContainsQ[#, "/compare/"]            &
  ];
  runRecs = Map[
    Function[p,
      Module[{r = Quiet @ Check[Import[p, "RawJSON"], $Failed]},
        If[! AssociationQ[r], Missing[],
          <|"runId"     -> r["runId"],
            "model"     -> r["model"],
            "createdAt" -> r["createdAt"],
            "_runDir"   -> DirectoryName[p]|>
        ]
      ]
    ],
    paths];
  runRecs = DeleteCases[runRecs, _Missing];
  (* WXF must exist (runner writes it last). *)
  runRecs = Select[runRecs,
    FileExistsQ[FileNameJoin[{#["_runDir"], "results.wxf"}]] &];
  byModel = GroupBy[runRecs, #["model"] &];
  latest  = Last @ SortBy[#, #["createdAt"] &] & /@ byModel;

  KeyValueMap[
    Function[{model, runRec},
      Module[{wxf, tests},
        wxf = FileNameJoin[{runRec["_runDir"], "results.wxf"}];
        tests = Quiet @ Check[Import[wxf, "WXF"], {}];
        If[ListQ[tests],
          out[model] = Map[
            <|"challengeName" -> Lookup[#, "challengeName", "?"],
              "passed"        -> TrueQ[Lookup[#, "passed", False]]|> &,
            tests
          ]
        ]
      ]
    ],
    latest
  ];
  out
];

(* ----------------------------------------------------------------------- *)
(* Render                                                                  *)
(* ----------------------------------------------------------------------- *)

htmlEsc[s_String] := StringReplace[s, {
  "&" -> "&amp;", "<" -> "&lt;", ">" -> "&gt;",
  "\"" -> "&quot;", "'" -> "&#39;"}];
htmlEsc[x_] := htmlEsc[ToString[x]];

(* Color cell by pass rate: red 0% \[Rule] yellow 50% \[Rule] green 100%. *)
cellColor[r_?NumericQ] := Module[{h},
  h = Round[120. r];   (* 0 = red, 60 = yellow, 120 = green *)
  "hsl(" <> ToString[h] <> ", 70%, 88%)"
];
cellColor[_] := "#f5f5f5";

renderHTML[
    tags_List, models_List, matrix_Association,
    overallByModel_Association, byModelStrengths_Association] :=
  Module[{css, header, table, snippets},

  css = "
    body { font-family: -apple-system, BlinkMacSystemFont, sans-serif;
           max-width: 1300px; margin: 2em auto; padding: 0 1em;
           color: #1a1a1a; }
    h1 { border-bottom: 2px solid #eee; padding-bottom: .3em; }
    .meta { color: #666; font-size: 14px; margin-bottom: 1.5em; }
    table.matrix { border-collapse: collapse; margin: 1.5em 0;
                   font-size: 13px; }
    table.matrix th, table.matrix td { border: 1px solid #e0e0e0;
                                       padding: 6px 10px;
                                       text-align: center;
                                       font-variant-numeric: tabular-nums; }
    table.matrix thead th { background: #f7f7f7; position: sticky; top: 0;
                            text-align: left; }
    table.matrix .tag-name { text-align: left; font-weight: 600;
                             max-width: 220px; }
    table.matrix .tag-name .small { color: #888; font-weight: 400;
                                    font-size: 11px; }
    table.matrix .total { background: #f0f0f0; font-weight: 600; }
    table.matrix .empty-cell { color: #bbb; }
    .strengths { display: grid;
                 grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
                 gap: 1em; margin: 1.5em 0; }
    .model-card { background: #fff; border: 1px solid #e5e5e5;
                  border-radius: 8px; padding: .8em 1em; }
    .model-card h3 { margin: 0 0 .4em; font-size: 1em; }
    .model-card h3 .pct { float: right; font-weight: 700; color: #666; }
    .strong { color: #2e7d32; }
    .weak   { color: #c62828; }
    .tag-pill { display: inline-block; padding: 1px 8px; margin: 2px;
                border-radius: 999px; background: #eee; font-size: 11px; }
  ";

  (* Models sorted left-to-right by overall pass rate (best first). *)
  header = "<tr>" <>
    "<th class=\"tag-name\">Track</th>" <>
    StringJoin @ Map[
      "<th>" <> htmlEsc[#] <> "<div class=\"small\">" <>
        ToString[NumberForm[100. overallByModel[#], {4, 1}]] <>
        "%</div></th>" &,
      models
    ] <>
    "</tr>";

  table = StringJoin[
    "<table class=\"matrix\"><thead>", header, "</thead><tbody>",
    StringJoin @ Map[
      Function[tag,
        "<tr>" <>
        "<td class=\"tag-name\">" <> htmlEsc[tag] <>
          "<div class=\"small\">n = " <>
          ToString[Total[Map[Lookup[matrix[tag, #], "total", 0] &, models]]] <>
          "</div></td>" <>
        StringJoin @ Map[
          Function[m,
            Module[{cell = matrix[tag, m]},
              If[cell["total"] === 0,
                "<td class=\"empty-cell\">\[LongDash]</td>",
                "<td style=\"background: " <> cellColor[cell["passRate"]] <> "\">" <>
                  ToString[NumberForm[100. cell["passRate"], {4, 1}]] <> "%" <>
                  "<div class=\"small\">" <>
                    ToString[cell["passed"]] <> " / " <>
                    ToString[cell["total"]] <>
                  "</div></td>"
              ]
            ]
          ],
          models
        ] <>
        "</tr>"
      ],
      tags
    ],
    "</tbody></table>"
  ];

  snippets = StringJoin @ Map[
    Function[m,
      Module[{s = byModelStrengths[m]},
        "<div class=\"model-card\">" <>
        "<h3>" <> htmlEsc[m] <>
          "<span class=\"pct\">" <>
          ToString[NumberForm[100. overallByModel[m], {4, 1}]] <> "%" <>
          "</span></h3>" <>
        "<div><strong class=\"strong\">Strongest:</strong> " <>
          StringJoin @ Map[
            "<span class=\"tag-pill\">" <> htmlEsc[#[[1]]] <> " " <>
              ToString[NumberForm[100. #[[2]], {4, 1}]] <> "%" <>
              "</span>" &,
            s["strengths"]
          ] <>
          "</div>" <>
        "<div style=\"margin-top: .4em\"><strong class=\"weak\">Weakest:</strong> " <>
          StringJoin @ Map[
            "<span class=\"tag-pill\">" <> htmlEsc[#[[1]]] <> " " <>
              ToString[NumberForm[100. #[[2]], {4, 1}]] <> "%" <>
              "</span>" &,
            s["weaknesses"]
          ] <>
          "</div>" <>
        "</div>"
      ]
    ],
    models
  ];

  StringJoin[
    "<!doctype html><html><head><meta charset=\"utf-8\">",
    "<title>Model strengths \[LongDash] WolframChallengesBenchmark</title>",
    "<style>", css, "</style></head><body>",
    "<h1>Model strengths by track</h1>",
    "<div class=\"meta\">",
    ToString[Length[models]], " models \[Bullet] ",
    ToString[Length[tags]], " tracks \[Bullet] ",
    "Generated: ", htmlEsc[DateString["ISODateTime"]], " \[Bullet] ",
    "Each model's most recent benchmark run.",
    "</div>",
    "<h2>Top-3 strongest / weakest per model</h2>",
    "<div class=\"strengths\">", snippets, "</div>",
    "<h2>Per-track matrix</h2>",
    "<p class=\"meta\">Tracks sorted hardest-first (lowest cross-model average). ",
    "Models sorted best-first by overall pass rate. ",
    "Color-graded red 0% \[Rule] green 100%. \"\[LongDash]\" = no tests for that model on that track.</p>",
    table,
    "</body></html>"
  ]
];


renderMarkdown[
    tags_List, models_List, matrix_Association,
    overallByModel_Association, byModelStrengths_Association] :=
  Module[{lines},
  lines = {
    "# Model strengths by track",
    "",
    ToString[Length[models]] <> " models, " <>
      ToString[Length[tags]] <> " tracks. Generated " <>
      DateString["ISODateTime"] <> ". Each model's most recent run.",
    "",
    "## Top-3 strongest / weakest per model",
    ""
  };
  Scan[
    Function[m,
      Module[{s = byModelStrengths[m]},
        AppendTo[lines, "### " <> m <>
          " (overall " <>
          ToString[NumberForm[100. overallByModel[m], {4, 1}]] <> "%)"];
        AppendTo[lines, ""];
        AppendTo[lines, "**Strongest:** " <>
          StringRiffle[
            Map[#[[1]] <> " (" <>
              ToString[NumberForm[100. #[[2]], {4, 1}]] <> "%)" &,
              s["strengths"]],
            ", "]];
        AppendTo[lines, ""];
        AppendTo[lines, "**Weakest:** " <>
          StringRiffle[
            Map[#[[1]] <> " (" <>
              ToString[NumberForm[100. #[[2]], {4, 1}]] <> "%)" &,
              s["weaknesses"]],
            ", "]];
        AppendTo[lines, ""];
      ]
    ],
    models
  ];

  AppendTo[lines, "## Per-track matrix"];
  AppendTo[lines, ""];
  AppendTo[lines,
    "| Track | " <> StringRiffle[models, " | "] <> " |"];
  AppendTo[lines,
    "|-----" <> StringJoin @ Table["|-----", {Length[models]}] <> "|"];
  Scan[
    Function[tag,
      AppendTo[lines,
        "| " <> tag <> " | " <>
        StringRiffle[
          Map[
            Function[m,
              Module[{cell = matrix[tag, m]},
                If[cell["total"] === 0,
                  "\[LongDash]",
                  ToString[NumberForm[100. cell["passRate"], {4, 1}]] <> "%"]
              ]
            ],
            models
          ],
          " | "
        ] <> " |"]
    ],
    tags
  ];

  StringRiffle[lines, "\n"]
];


(* ----------------------------------------------------------------------- *)
(* Main                                                                    *)
(* ----------------------------------------------------------------------- *)

main[] := Module[
  {args, dataDir, runsBase, jsonlPath, outDir, modelTests, source,
   data, tagsByTaskId, byTagModel, allTags, allModels, latest,
   matrix, overallByModel, byModelStrengths, html, md},

  args      = parseArgs[Drop[$ScriptCommandLine, 1]];
  dataDir   = Lookup[args, "data",
    FileNameJoin[{$repoRoot, "data", "results"}]];
  runsBase  = Lookup[args, "runs",
    FileNameJoin[{$repoRoot, "runs"}]];
  jsonlPath = Lookup[args, "jsonl",
    FileNameJoin[{$repoRoot, "challenges.jsonl"}]];
  outDir    = Lookup[args, "out",
    FileNameJoin[{$repoRoot, "runs", "model-strengths"}]];

  If[! FileExistsQ[jsonlPath], abort[1, "challenges.jsonl not found: " <> jsonlPath]];

  WriteString[$Output, "Loading bank tags from ", jsonlPath, "\n"];
  data = JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL[jsonlPath];
  tagsByTaskId = AssociationMap[
    data["challenges", #, "tags"] &,
    Keys[data["challenges"]]];

  (* Prefer published data/results; fall back to local runs/+WXF. *)
  modelTests = loadModelTestsFromPublished[dataDir];
  source     = "data/results";
  If[modelTests === <||>,
    WriteString[$Output,
      "No data/results/*.json found; falling back to local runs/\n"];
    modelTests = loadModelTestsFromLocalRuns[runsBase];
    source     = "runs";
  ];
  If[modelTests === <||>,
    abort[1, "no benchmark data found in " <> dataDir <> " or " <> runsBase <>
            ".  Run scripts/RunBenchmark.wls then scripts/PublishResults.wls."]];

  WriteString[$Output, "Source: ", source, "\n"];
  WriteString[$Output, "Models: ",
    StringRiffle[Sort @ Keys[modelTests], ", "], "\n"];

  (* Group results by tag/track, accumulate per model. *)
  byTagModel = <||>;   (* tag -> model -> <|"passed", "total"|> *)
  Module[{addCell},
    addCell[tag_, model_, passed_, total_] := Module[
      {tagAssoc, prev, newCell},
      (* WL gotcha #1: Lookup[assoc, {a, b}, default] is MULTI-KEY
         lookup returning {assoc[a], assoc[b]}.  Drill explicitly.
         WL gotcha #2: assoc[k1, k2] = v silently no-ops if
         assoc[k1] doesn't already exist as an Association.  We
         build the inner Association ourselves and reassign. *)
      tagAssoc = Lookup[byTagModel, tag, <||>];
      prev     = Lookup[tagAssoc, model,
                        <|"passed" -> 0, "total" -> 0|>];
      newCell  = <|
        "passed" -> prev["passed"] + passed,
        "total"  -> prev["total"]  + total
      |>;
      byTagModel[tag] = Append[tagAssoc, model -> newCell];
    ];

    KeyValueMap[
      Function[{model, tests},
        Scan[
          Function[t,
            Module[{taskName, taskTags, taskTracks, p},
              taskName   = t["challengeName"];
              taskTags   = Lookup[tagsByTaskId, taskName, {}];
              taskTracks = tracksForChallenge[taskName, taskTags];
              p          = If[TrueQ[t["passed"]], 1, 0];
              Scan[addCell[#, model, p, 1] &, taskTracks];
              addCell["_overall", model, p, 1];
            ]
          ],
          tests
        ]
      ],
      modelTests
    ];
  ];

  (* Re-bind `latest` to the model-keyed test map so the renderer's
     downstream logic ("models present", etc.) keeps working. *)
  latest = modelTests;

  (* "tags" here is a misnomer post-rename: these are now the official
     Wolfram Challenges TRACKS (Algorithms, Geometry, Words, etc.).
     Keeping the variable name to avoid a rename across the whole
     script -- the rendered labels in the HTML/MD output say "Track". *)
  allTags = Sort @ DeleteCases[Keys[byTagModel], "_overall"];
  allModels = Sort @ Keys[latest];

  (* Build the matrix Association: tag -> model -> <|passed,total,passRate|>. *)
  matrix = AssociationMap[
    Function[tag,
      AssociationMap[
        Function[model,
          Module[{cell, tagAssoc},
            tagAssoc = Lookup[byTagModel, tag, <||>];
            cell = Lookup[tagAssoc, model,
                          <|"passed" -> 0, "total" -> 0|>];
            <|
              "passed"   -> cell["passed"],
              "total"    -> cell["total"],
              "passRate" -> If[cell["total"] > 0,
                              N[cell["passed"] / cell["total"]], 0.]
            |>
          ]
        ],
        allModels
      ]
    ],
    Append[allTags, "_overall"]
  ];

  overallByModel = AssociationMap[matrix["_overall", #, "passRate"] &, allModels];

  (* Sort models by overall (best first), tags by avg-across-models
     (hardest first). *)
  allModels = ReverseSortBy[allModels, overallByModel[#] &];
  allTags = SortBy[allTags,
    Mean[N[matrix[#, m, "passRate"]] & /@ allModels] &];

  (* Per-model strengths/weaknesses: top-3 highest, top-3 lowest tags. *)
  byModelStrengths = AssociationMap[
    Function[m,
      Module[{ranked, withSamples},
        (* Only consider tags where the model attempted at least 3 tests
           so we don't over-credit one-off luck. *)
        withSamples = Select[allTags,
          matrix[#, m, "total"] >= 3 &];
        ranked = SortBy[withSamples, matrix[#, m, "passRate"] &];
        <|
          "strengths" ->
            ({#, matrix[#, m, "passRate"]} & /@
              Reverse @ Take[ranked, -Min[3, Length[ranked]]]),
          "weaknesses" ->
            ({#, matrix[#, m, "passRate"]} & /@
              Take[ranked, Min[3, Length[ranked]]])
        |>
      ]
    ],
    allModels
  ];

  If[! DirectoryQ[outDir],
    CreateDirectory[outDir, CreateIntermediateDirectories -> True]];

  html = renderHTML[allTags, allModels, matrix,
                    overallByModel, byModelStrengths];
  Module[{stream, htmlPath = FileNameJoin[{outDir, "report.html"}]},
    stream = OpenWrite[htmlPath, CharacterEncoding -> "UTF-8"];
    WriteString[stream, html];
    Close[stream];
  ];

  md = renderMarkdown[allTags, allModels, matrix,
                       overallByModel, byModelStrengths];
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
