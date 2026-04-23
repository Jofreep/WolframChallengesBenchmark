(* ::Package:: *)

(* :Context: ChallengesBenchmark`Private` *)
(* :Summary: Cross-model comparison — load multiple run directories,
     align per-challenge and per-test results, and produce a comparison
     object + HTML/Markdown report.

     Public entry points (exposed as wrappers in ChallengesBenchmark.wl):

       CompareModels[{runDir1, runDir2, ...}]
       CompareModels[<|"modelA" -> runDirA, "modelB" -> runDirB, ...|>]
       CompareModels[parentDirContainingRuns, Automatic]

       WriteCompareReport[compareObj, outDir]

   Return shape of CompareModels:
     <|
       "models"        -> {"model-a", "model-b", ...},
       "runsByModel"   -> <|model -> <|"runId"..., "summary"..., "results"...|>|>,
       "perChallenge"  -> <|challenge -> <|model -> <|"passed"..., "total"..., "rate"...|>|>|>,
       "allChallenges" -> sorted union of challenge names,
       "uniquelyPassed"-> <|model -> {challenges}|>,
       "uniquelyFailed"-> <|model -> {challenges}|>
     |>
*)

Begin["ChallengesBenchmark`Private`"];

(* ------------------------------------------------------------------ *)
(* Run loader — load one run directory into a small association with  *)
(* everything downstream needs. Failing to read results is a soft     *)
(* error: we log and skip rather than aborting the whole comparison.  *)

loadRunDir[dir_String] := Module[{meta, resultsPath, results, runId, model},
  If[! DirectoryQ[dir],
    logWarn["CompareModels: not a directory: " <> dir];
    Return[$Failed]
  ];
  meta = Quiet @ Check[
    Import[FileNameJoin[{dir, "run.json"}], "RawJSON"],
    $Failed
  ];
  If[meta === $Failed,
    logWarn["CompareModels: missing or invalid run.json in " <> dir];
    Return[$Failed]
  ];
  resultsPath = FileNameJoin[{dir, "results.wxf"}];
  results = If[FileExistsQ[resultsPath],
    Quiet @ Check[Import[resultsPath, "WXF"], {}],
    {}
  ];
  runId = Lookup[meta, "runId", FileBaseName[dir]];
  model = Lookup[meta, "model", "unknown"];
  <|
    "dir"      -> dir,
    "runId"    -> runId,
    "model"    -> model,
    "meta"     -> meta,
    "summary"  -> Lookup[meta, "summary", <||>],
    "results"  -> results
  |>
];

(* ------------------------------------------------------------------ *)
(* Per-challenge roll-up for one model's results. *)

perChallengeRollup[results_List] :=
  KeyMap[ToString,
    Association @ KeyValueMap[
      Function[{chall, rs},
        chall -> <|
          "total"    -> Length[rs],
          "passed"   -> Count[rs, _?(#["passed"] === True &)],
          "rate"     -> If[Length[rs] > 0,
                          N[Count[rs, _?(#["passed"] === True &)] / Length[rs]],
                          0.]
        |>
      ],
      GroupBy[results, #["challengeName"] &]
    ]
  ];

(* ------------------------------------------------------------------ *)
(* compareModelsImpl accepts a list of run directories OR an          *)
(* Association model -> dir, and returns the compare object.         *)

compareModelsImpl[runs_] := Module[
  {runSpec, loaded, models, perChallengeMatrix, allChallenges,
   uniquelyPassed, uniquelyFailed, runsByModel},

  (* Normalise input to Association model -> runDir. *)
  runSpec = Which[
    AssociationQ[runs],
      runs,
    ListQ[runs],
      (* Use the model key from each run.json; fall back to runId / dirname. *)
      Association @ Map[
        Function[dir,
          Module[{m = loadRunDir[dir]},
            If[m === $Failed, Nothing, m["model"] -> dir]
          ]
        ],
        runs
      ],
    True,
      logWarn["CompareModels: unsupported input shape"];
      Return[$Failed]
  ];

  loaded = AssociationMap[loadRunDir[runSpec[#]] &, Keys[runSpec]];
  loaded = Select[loaded, # =!= $Failed &];

  If[Length[loaded] === 0,
    logWarn["CompareModels: no runs could be loaded"];
    Return[$Failed]
  ];

  models = Keys[loaded];

  runsByModel = AssociationMap[
    Function[m,
      Module[{run = loaded[m]},
        <|
          "runId"     -> run["runId"],
          "model"     -> run["model"],
          "dir"       -> run["dir"],
          "summary"   -> run["summary"],
          "byChallenge" -> perChallengeRollup[run["results"]]
        |>
      ]
    ],
    models
  ];

  (* Build the per-challenge matrix: challenge -> model -> {passed,total,rate} *)
  allChallenges = Sort @ DeleteDuplicates @ Flatten[
    Keys /@ (runsByModel[[#, "byChallenge"]] & /@ models)
  ];
  perChallengeMatrix = AssociationMap[
    Function[chall,
      AssociationMap[
        Function[m,
          Lookup[runsByModel[m, "byChallenge"], chall,
            <|"total" -> 0, "passed" -> 0, "rate" -> 0.|>
          ]
        ],
        models
      ]
    ],
    allChallenges
  ];

  (* Uniquely passed / failed challenges: a challenge is "passed" by a
     model if EVERY test in that challenge passed. Uniquely-passed by
     model m means: m passes, all others fail. Missing counts as fail. *)
  uniquelyPassed = AssociationMap[
    Function[m,
      Select[allChallenges,
        Module[{stats = perChallengeMatrix[#]},
          stats[m]["total"] > 0 && stats[m]["passed"] === stats[m]["total"] &&
          AllTrue[DeleteCases[models, m],
            Function[other,
              Module[{o = stats[other]},
                o["total"] === 0 || o["passed"] < o["total"]
              ]
            ]
          ]
        ] &
      ]
    ],
    models
  ];

  uniquelyFailed = AssociationMap[
    Function[m,
      Select[allChallenges,
        Module[{stats = perChallengeMatrix[#]},
          stats[m]["total"] > 0 && stats[m]["passed"] < stats[m]["total"] &&
          AllTrue[DeleteCases[models, m],
            Function[other,
              Module[{o = stats[other]},
                o["total"] > 0 && o["passed"] === o["total"]
              ]
            ]
          ]
        ] &
      ]
    ],
    models
  ];

  <|
    "models"         -> models,
    "runsByModel"    -> runsByModel,
    "perChallenge"   -> perChallengeMatrix,
    "allChallenges"  -> allChallenges,
    "uniquelyPassed" -> uniquelyPassed,
    "uniquelyFailed" -> uniquelyFailed
  |>
];

(* ------------------------------------------------------------------ *)
(* Report rendering                                                     *)

renderCompareMarkdown[cmp_Association] := Module[
  {lines, models, matrix, chall, perChall},
  models = cmp["models"];
  matrix = cmp["perChallenge"];

  lines = {
    "# Model comparison",
    "",
    "Generated " <> DateString["ISODateTime"] <> ".",
    "",
    "## Headline",
    "",
    StringJoin[{"| Model ", StringJoin["| Pass rate " & /@ {1}],
      "| Passed / Total | Duration p50 | p99 |"}],
    "|---|---|---|---|---|"
  };
  lines = Join[lines, Map[
    Function[m,
      Module[{s = cmp["runsByModel", m, "summary"]},
        StringJoin[
          "| ", m,
          " | ", ToString[NumberForm[100. Lookup[s, "passRate", 0.], {4, 2}]], "%",
          " | ", ToString[Lookup[s, "passed", 0]], " / ", ToString[Lookup[s, "total", 0]],
          " | ", formatDurationOrDash[Lookup[s, {"duration", "p50"}, None]],
          " | ", formatDurationOrDash[Lookup[s, {"duration", "p99"}, None]],
          " |"
        ]
      ]
    ],
    models
  ]];

  AppendTo[lines, ""];
  AppendTo[lines, "## Per-challenge pass matrix"];
  AppendTo[lines, ""];
  AppendTo[lines, StringJoin["| Challenge | ",
    StringRiffle[models, " | "], " |"]];
  AppendTo[lines, StringJoin["|---",
    StringJoin["|---" & /@ models], "|"]];
  Scan[
    Function[chall,
      perChall = matrix[chall];
      AppendTo[lines,
        StringJoin[
          "| ", chall, " | ",
          StringRiffle[
            Map[
              Function[m,
                Module[{c = perChall[m]},
                  If[c["total"] === 0, "—",
                    ToString[c["passed"]] <> "/" <> ToString[c["total"]]
                  ]
                ]
              ],
              models
            ],
            " | "
          ],
          " |"
        ]
      ]
    ],
    cmp["allChallenges"]
  ];

  AppendTo[lines, ""];
  AppendTo[lines, "## Uniquely passed challenges"];
  Scan[
    Function[m,
      AppendTo[lines, ""];
      AppendTo[lines, "### " <> m];
      Module[{us = cmp["uniquelyPassed", m]},
        If[us === {} || us === Missing["KeyAbsent", m],
          AppendTo[lines, "_(none)_"],
          AppendTo[lines, StringRiffle[("- " <> # & /@ us), "\n"]]
        ]
      ]
    ],
    models
  ];

  AppendTo[lines, ""];
  AppendTo[lines, "## Uniquely failed challenges"];
  Scan[
    Function[m,
      AppendTo[lines, ""];
      AppendTo[lines, "### " <> m];
      Module[{us = cmp["uniquelyFailed", m]},
        If[us === {} || us === Missing["KeyAbsent", m],
          AppendTo[lines, "_(none)_"],
          AppendTo[lines, StringRiffle[("- " <> # & /@ us), "\n"]]
        ]
      ]
    ],
    models
  ];

  StringRiffle[lines, "\n"]
];

(* formatDurationOrDash — tiny helper that prints "—" for missing values
   so the comparison table stays readable when a run pre-dated the
   duration percentile feature. *)
formatDurationOrDash[None] := "—";
formatDurationOrDash[Missing[___]] := "—";
formatDurationOrDash[0 | 0.] := "—";
formatDurationOrDash[x_?NumericQ] := formatDuration[x];
formatDurationOrDash[_] := "—";

renderCompareHtml[cmp_Association] := Module[
  {models, matrix, head, rows, css},
  models  = cmp["models"];
  matrix  = cmp["perChallenge"];
  css     = StringJoin[
    "body { font-family: -apple-system, sans-serif; max-width: 1200px; ",
    "margin: 2em auto; color: #1a1a1a; }",
    "h1, h2, h3 { font-weight: 600; }",
    "table { border-collapse: collapse; width: 100%; margin: 1em 0; }",
    "th, td { padding: 0.4em 0.7em; border-bottom: 1px solid #ddd; text-align: left; }",
    "th { background: #f6f8fa; }",
    "td.ok  { background: #d4edda; }",
    "td.bad { background: #f8d7da; }",
    "td.mix { background: #fff3cd; }",
    "td.na  { color: #999; }"
  ];

  head = StringJoin[
    "<tr><th>Challenge</th>",
    StringJoin["<th>" <> htmlEscape[#] <> "</th>" & /@ models],
    "</tr>"
  ];
  rows = StringJoin @ Map[
    Function[chall,
      Module[{perChall = matrix[chall]},
        StringJoin["<tr><td>", htmlEscape[chall], "</td>",
          StringJoin @ Map[
            Function[m,
              Module[{c = perChall[m], cls, content},
                {cls, content} = Which[
                  c["total"] === 0,
                    {"na", "—"},
                  c["passed"] === c["total"],
                    {"ok",  ToString[c["passed"]] <> "/" <> ToString[c["total"]]},
                  c["passed"] === 0,
                    {"bad", ToString[c["passed"]] <> "/" <> ToString[c["total"]]},
                  True,
                    {"mix", ToString[c["passed"]] <> "/" <> ToString[c["total"]]}
                ];
                "<td class=\"" <> cls <> "\">" <> htmlEscape[content] <> "</td>"
              ]
            ],
            models
          ],
          "</tr>"
        ]
      ]
    ],
    cmp["allChallenges"]
  ];

  StringJoin[
    "<!doctype html><html><head><meta charset=\"utf-8\">",
    "<title>Model comparison</title><style>", css, "</style></head><body>",
    "<h1>Model comparison</h1>",
    "<p>Generated ", htmlEscape[DateString["ISODateTime"]], ".</p>",
    "<h2>Headline</h2>",
    "<table><thead><tr><th>Model</th><th>Pass rate</th>",
    "<th>Passed / Total</th><th>Duration p50</th><th>p99</th></tr></thead><tbody>",
    StringJoin @ Map[
      Function[m,
        Module[{s = cmp["runsByModel", m, "summary"]},
          StringJoin["<tr><td>", htmlEscape[m], "</td>",
            "<td>", ToString[NumberForm[100. Lookup[s, "passRate", 0.], {4, 2}]], "%</td>",
            "<td>", ToString[Lookup[s, "passed", 0]], " / ",
              ToString[Lookup[s, "total", 0]], "</td>",
            "<td>", htmlEscape[formatDurationOrDash[Lookup[s, {"duration","p50"}, None]]], "</td>",
            "<td>", htmlEscape[formatDurationOrDash[Lookup[s, {"duration","p99"}, None]]], "</td>",
            "</tr>"
          ]
        ]
      ],
      models
    ],
    "</tbody></table>",
    "<h2>Per-challenge pass matrix</h2>",
    "<table><thead>", head, "</thead><tbody>", rows, "</tbody></table>",
    "</body></html>"
  ]
];

writeCompareReportImpl[cmp_Association, dir_String] := Module[
  {mdPath, htmlPath},
  If[! DirectoryQ[dir],
    CreateDirectory[dir, CreateIntermediateDirectories -> True]
  ];
  mdPath   = FileNameJoin[{dir, "compare.md"}];
  htmlPath = FileNameJoin[{dir, "compare.html"}];
  Export[mdPath,   renderCompareMarkdown[cmp], "Text", CharacterEncoding -> "UTF-8"];
  Export[htmlPath, renderCompareHtml[cmp],     "Text", CharacterEncoding -> "UTF-8"];
  <|"markdown" -> mdPath, "html" -> htmlPath|>
];

End[];
