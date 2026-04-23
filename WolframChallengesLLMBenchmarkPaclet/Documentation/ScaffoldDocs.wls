#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ------------------------------------------------------------------------ *)
(*  ScaffoldDocs.wls                                                         *)
(*                                                                           *)
(*  Generates the Paclet Repository documentation notebook scaffold:         *)
(*                                                                           *)
(*    Documentation/English/Guides/WolframChallengesBenchmark.nb             *)
(*    Documentation/English/ReferencePages/Symbols/<SymbolName>.nb           *)
(*    Documentation/English/Tutorials/QuickStart.nb                          *)
(*    ResourceDefinition.nb                                                  *)
(*                                                                           *)
(*  The generated notebooks are intentionally minimal but valid: they use    *)
(*  the stock "Guide", "FunctionPageSource", and "DefaultStylesheet"         *)
(*  conventions so PacletCICD's documentation build pipeline can process    *)
(*  them without hand-editing.  Detailed usage text, examples, and option    *)
(*  tables should be added per-symbol before a public Paclet Repository     *)
(*  submission.                                                              *)
(*                                                                           *)
(*  Run:                                                                     *)
(*    wolframscript -file Documentation/ScaffoldDocs.wls                     *)
(* ------------------------------------------------------------------------ *)

$scriptDir  = DirectoryName[$InputFileName];
$pacletRoot = DirectoryName[$scriptDir];
PacletDirectoryLoad[$pacletRoot];
Needs["JofreEspigulePons`WolframChallengesBenchmark`"];


$pubSymbols = {
  "BenchmarkVersion",
  "OpenRouterChatComplete",
  "GenerateSolutions",
  "SaveSolution",
  "ExtractCode",
  "LoadChallenges",
  "LoadTestBank",
  "ReconcileNames",
  "RunBenchmark",
  "DiffRuns",
  "$OpenRouterAPIKey",
  "$BenchmarkDefaults"
};


(* Pull the live usage strings out of the paclet so what ships in the docs
   tracks what is compiled into the kernel.  *)
usageOf[name_String] := Module[{sym, u},
  sym = Symbol["JofreEspigulePons`WolframChallengesBenchmark`" <> name];
  u = Quiet @ MessageName[sym, "usage"];
  If[StringQ[u], u, "No usage string defined."]
];


writeSymbolPage[name_String] := Module[{nb, path, u},
  u = usageOf[name];
  path = FileNameJoin[{$pacletRoot, "Documentation", "English",
    "ReferencePages", "Symbols", name <> ".nb"}];
  nb = Notebook[{
    Cell[name, "ObjectName"],
    Cell[BoxData @ RowBox[{name}], "Usage"],
    Cell[u, "Usage"],
    Cell["Details", "NotesSection"],
    Cell["This symbol is exported by JofreEspigulePons/WolframChallengesBenchmark.", "Notes"],
    Cell["Examples", "PrimaryExamplesSection"],
    Cell[BoxData @ RowBox[{"Needs", "[",
      "\"JofreEspigulePons`WolframChallengesBenchmark`\"", "]"}],
      "Input"]
  },
    StyleDefinitions -> FrontEnd`FileName[{"Wolfram"}, "Reference.nb",
      CharacterEncoding -> "UTF-8"],
    TaggingRules -> {
      "ModificationHighlight" -> False,
      "Metadata" -> <|
        "title" -> name,
        "type"  -> "Symbol",
        "paclet"-> "JofreEspigulePons/WolframChallengesBenchmark",
        "uri"   -> "JofreEspigulePons/WolframChallengesBenchmark/ref/" <> name
      |>,
      "SearchTextTranslated" -> "",
      "LinkTrails" -> "",
      "Created" -> DateString[],
      "summary" -> StringTake[u, UpTo[200]],
      "synonyms" -> {},
      "title" -> name,
      "windowTitle" -> name,
      "type" -> "Symbol"
    }
  ];
  Export[path, nb, "Notebook"];
  path
];


writeGuide[] := Module[{nb, path, links},
  path = FileNameJoin[{$pacletRoot, "Documentation", "English", "Guides",
    "WolframChallengesBenchmark.nb"}];
  links = Cell[#, "InlineGuideFunctionListing"] & /@ $pubSymbols;
  nb = Notebook[Join[{
    Cell["WolframChallengesBenchmark", "GuideTitle"],
    Cell["A production-grade benchmark harness for evaluating LLM-generated \
Wolfram Language solutions to Wolfram Challenges. Includes direct OpenRouter \
API integration, a sandboxed multi-kernel test runner, deterministic \
reproducibility controls, and rich per-run metadata.",
      "GuideAbstract"],
    Cell["Load the paclet", "GuideFunctionsSection"],
    Cell[BoxData @ RowBox[{"Needs", "[",
      "\"JofreEspigulePons`WolframChallengesBenchmark`\"", "]"}],
      "Input"],
    Cell["Public Symbols", "GuideFunctionsSection"]
  }, links, {
    Cell["Scripts", "GuideTutorialsSection"],
    Cell["Scripts/RunBenchmark.wls drives a full benchmark from the shell.", "GuideTutorial"],
    Cell["Scripts/GenerateSolutions.wls prompts a model via OpenRouter and audits responses.", "GuideTutorial"],
    Cell["Scripts/BuildTestBank.wls turns a .challenges plain-text file into a WXF test bank.", "GuideTutorial"],
    Cell["Related Guides", "GuideTutorialsSection"],
    Cell["Wolfram Challenges: https://challenges.wolframcloud.com", "GuideTutorial"]
  }],
    StyleDefinitions -> FrontEnd`FileName[{"Wolfram"}, "Reference.nb",
      CharacterEncoding -> "UTF-8"],
    TaggingRules -> {
      "ModificationHighlight" -> False,
      "Metadata" -> <|
        "title" -> "WolframChallengesBenchmark",
        "type"  -> "Guide",
        "paclet"-> "JofreEspigulePons/WolframChallengesBenchmark",
        "uri"   -> "JofreEspigulePons/WolframChallengesBenchmark/guide/WolframChallengesBenchmark"
      |>,
      "title" -> "WolframChallengesBenchmark",
      "windowTitle" -> "WolframChallengesBenchmark",
      "type" -> "Guide"
    }
  ];
  Export[path, nb, "Notebook"];
  path
];


writeTutorial[] := Module[{nb, path},
  path = FileNameJoin[{$pacletRoot, "Documentation", "English", "Tutorials",
    "QuickStart.nb"}];
  nb = Notebook[{
    Cell["Quick Start", "Title"],
    Cell["Load the paclet", "Section"],
    Cell[BoxData @ RowBox[{"Needs", "[",
      "\"JofreEspigulePons`WolframChallengesBenchmark`\"", "]"}], "Input"],
    Cell["Run a smoke benchmark", "Section"],
    Cell["Build a tiny challenge set + expected-output bank, then evaluate \
an in-process candidate solution.  This is the shape every larger run follows.",
      "Text"],
    Cell[BoxData @ RowBox[{
      "RunBenchmark", "[",
      RowBox[{
        "<|\"Sum\"->\"Sum\"|>", ",",
        "<|\"Sum\"->{<|\"challengeName\"->\"Sum\",\"input\"->HoldComplete[f[1,2]],",
        "\"expected\"->3,\"metadata\"-><||>|>}|>", ",",
        "<|\"Sum\"-><|\"code\"->\"f[a_,b_]:=a+b\"|>|>", ",",
        "\"Model\"->\"local/smoke\"", ",",
        "\"IsolationMode\"->\"InProcess\""
      }], "]"}],
      "Input"],
    Cell["Generate candidate solutions from a model", "Section"],
    Cell["Requires OPENROUTER_API_KEY in the environment.", "Text"],
    Cell[BoxData @ RowBox[{
      "GenerateSolutions", "[",
      RowBox[{"challenges", ",", "testBank", ",", "<|",
        "\"Model\"->\"anthropic/claude-opus-4\"", "|>"}], "]"}],
      "Input"]
  },
    StyleDefinitions -> FrontEnd`FileName[{"Wolfram"}, "Reference.nb",
      CharacterEncoding -> "UTF-8"],
    TaggingRules -> {
      "Metadata" -> <|
        "title" -> "Quick Start",
        "type"  -> "Tutorial",
        "paclet"-> "JofreEspigulePons/WolframChallengesBenchmark",
        "uri"   -> "JofreEspigulePons/WolframChallengesBenchmark/tutorial/QuickStart"
      |>,
      "title" -> "Quick Start",
      "windowTitle" -> "Quick Start",
      "type" -> "Tutorial"
    }
  ];
  Export[path, nb, "Notebook"];
  path
];


writeResourceDefinition[] := Module[{nb, path},
  path = FileNameJoin[{$pacletRoot, "ResourceDefinition.nb"}];
  nb = Notebook[{
    Cell["Paclet Resource Definition", "Title"],
    Cell[BoxData @ RowBox[{
      "ResourceObject", "[",
        RowBox[{"File", "[", "\"PacletInfo.wl\"", "]"}], "]"}],
      "Input"],
    Cell["This notebook is consumed by PacletCICD to register the paclet \
with the Wolfram Paclet Repository.", "Text"]
  },
    StyleDefinitions -> FrontEnd`FileName[{"Wolfram"}, "Default.nb",
      CharacterEncoding -> "UTF-8"]
  ];
  Export[path, nb, "Notebook"];
  path
];


Print["Generating documentation scaffold under ", $pacletRoot, "\n"];

Scan[
  Function[s,
    Print["  * ", writeSymbolPage[s]]
  ],
  $pubSymbols
];

Print["  * ", writeGuide[]];
Print["  * ", writeTutorial[]];
Print["  * ", writeResourceDefinition[]];

Print["\nDone."];
