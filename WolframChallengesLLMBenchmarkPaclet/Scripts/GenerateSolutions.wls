#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ----------------------------------------------------------------------- *)
(*  GenerateSolutions.wls                                                  *)
(*                                                                         *)
(*  CLI front-end for JofreEspigulePons`WolframChallengesBenchmark`        *)
(*  GenerateSolutions[].  Generates one candidate Wolfram Language        *)
(*  solution per challenge through the OpenRouter Chat Completions       *)
(*  endpoint, audits every candidate against the test bank, and writes   *)
(*  solutions/<model-slug>/<challenge>.wl + <challenge>.meta.json.        *)
(*                                                                         *)
(*  Usage                                                                  *)
(*  -----                                                                  *)
(*    export OPENROUTER_API_KEY=sk-or-...                                  *)
(*    wolframscript -file Scripts/GenerateSolutions.wls \                  *)
(*       --challenges path/to/challenges.json \                            *)
(*       --testbank   path/to/tests.wxf       \                            *)
(*       --model      anthropic/claude-opus-4                              *)
(*       [--model-slug claude-opus-4]                                      *)
(*       [--out solutions]                                                 *)
(*       [--filter Aliquot,FizzBuzz]                                       *)
(*       [--timeout 120]                                                   *)
(*       [--retries 3]                                                     *)
(*       [--retry-base-delay 2]                                            *)
(*       [--temperature 0.2]                                               *)
(*       [--max-tokens 2048]                                               *)
(*       [--overwrite]                                                     *)
(*       [--dry-run]                                                       *)
(*       [--log path/to/generate.jsonl]                                    *)
(*                                                                         *)
(*  Exit codes                                                             *)
(*  ----------                                                             *)
(*    0  all challenges saved                                              *)
(*    1  invalid arguments / missing files                                 *)
(*    2  one or more challenges failed (LLM error) or were audit-rejected *)
(*    4  unexpected fatal error                                            *)
(* ----------------------------------------------------------------------- *)

(* ----------------------------------------------------------------------- *)
(* Boot the paclet                                                         *)
(* ----------------------------------------------------------------------- *)

$scriptDir  = DirectoryName[$InputFileName];
$pacletRoot = DirectoryName[$scriptDir];

PacletDirectoryLoad[$pacletRoot];
Needs["JofreEspigulePons`WolframChallengesBenchmark`"];


(* ----------------------------------------------------------------------- *)
(* Argument parsing                                                        *)
(* ----------------------------------------------------------------------- *)

parseArgs[args_List] := Module[{i = 1, n = Length[args], out = <||>, k, v},
  While[i <= n,
    k = args[[i]];
    Which[
      StringStartsQ[k, "--"],
        v = If[i < n && ! StringStartsQ[args[[i + 1]], "--"],
               args[[i + 1]], "true"];
        out[StringDrop[k, 2]] = v;
        i += If[v === "true", 1, 2],
      True,
        i++
    ]
  ];
  out
];

withDefault[args_, key_, default_] := Lookup[args, key, default];

requireKey[args_, key_] :=
  If[KeyExistsQ[args, key],
    args[key],
    WriteString[$Output, "ERROR: missing required --", key, "\n"];
    Exit[1]
  ];

truthy[v_] := ! MemberQ[{"false", "False", "0", "off", "no"}, v];


(* ----------------------------------------------------------------------- *)
(* Slugify                                                                 *)
(* ----------------------------------------------------------------------- *)

modelSlug[model_String] :=
  StringReplace[model,
    {RegularExpression["[^A-Za-z0-9._-]"] -> "-"}];


(* ----------------------------------------------------------------------- *)
(* Main                                                                    *)
(* ----------------------------------------------------------------------- *)

main[] := Module[
  {args, challengesPath, testBankPath, model, modelSlugStr, outDir,
   filter, timeout, retries, baseDelay, temperature, maxTokens,
   overwrite, dryRun, logPath,
   challenges, testBank, outByModelDir, opts, llmOpts, result, failed,
   apiKey, exitCode = 0},

  args = parseArgs[Drop[$ScriptCommandLine, 1]];

  challengesPath = requireKey[args, "challenges"];
  testBankPath   = requireKey[args, "testbank"];
  model          = requireKey[args, "model"];

  modelSlugStr = withDefault[args, "model-slug", modelSlug[model]];
  outDir       = withDefault[args, "out",
                   FileNameJoin[{Directory[], "solutions"}]];
  timeout      = ToExpression @ withDefault[args, "timeout",          "120"];
  retries      = ToExpression @ withDefault[args, "retries",          "3"];
  baseDelay    = ToExpression @ withDefault[args, "retry-base-delay", "2"];
  temperature  = ToExpression @ Lookup[args, "temperature", "Missing[]"];
  maxTokens    = ToExpression @ Lookup[args, "max-tokens",  "Missing[]"];
  overwrite    = KeyExistsQ[args, "overwrite"];
  dryRun       = KeyExistsQ[args, "dry-run"];
  logPath      = Lookup[args, "log", None];

  filter = If[KeyExistsQ[args, "filter"],
    StringSplit[args["filter"], ","],
    All
  ];

  apiKey = JofreEspigulePons`WolframChallengesBenchmark`$OpenRouterAPIKey;
  If[! dryRun && apiKey === $Failed,
    WriteString[$Output,
      "ERROR: OPENROUTER_API_KEY environment variable is unset or empty.\n",
      "       Set it with:  export OPENROUTER_API_KEY=sk-or-...\n"];
    Exit[1]
  ];

  outByModelDir = FileNameJoin[{outDir, modelSlugStr}];
  If[! DirectoryQ[outByModelDir],
    CreateDirectory[outByModelDir, CreateIntermediateDirectories -> True]
  ];

  WriteString[$Output, "Loading challenges from ", challengesPath, "...\n"];
  challenges =
    JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges[challengesPath];
  If[challenges === $Failed, Exit[1]];

  WriteString[$Output, "Loading test bank from ", testBankPath, "...\n"];
  testBank =
    JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank[testBankPath];
  If[testBank === $Failed, Exit[1]];

  WriteString[$Output,
    "Generating solutions (model=", model,
    ", slug=",     modelSlugStr,
    ", dryRun=",   If[dryRun, "true", "false"],
    ", retries=",  retries,
    ", timeout=",  timeout, "s)...\n"
  ];

  (* LLMOptions is the sub-assoc the OpenRouter client reads; only forward
     temperature / max_tokens when the user actually passed a value. *)
  llmOpts = Join[<||>,
    If[MissingQ[temperature], <||>, <|"Temperature" -> temperature|>],
    If[MissingQ[maxTokens],   <||>, <|"MaxTokens"   -> maxTokens|>]
  ];

  opts = Join[
    <|
      "Model"           -> model,
      "OutputDirectory" -> FileNameJoin[{outDir, modelSlugStr}],
      "Filter"          -> filter,
      "TimeConstraint"  -> timeout,
      "Retries"         -> retries,
      "RetryBaseDelay"  -> baseDelay,
      "Overwrite"       -> overwrite,
      "DryRun"          -> dryRun,
      "LLMInfo"         -> <|"model" -> model, "slug" -> modelSlugStr|>,
      "LLMOptions"      -> llmOpts
    |>,
    (* Only forward LogPath when --log was explicitly supplied; otherwise
       let the impl pick a sensible default inside the output directory. *)
    If[logPath === None, <||>, <|"LogPath" -> logPath|>]
  ];

  result =
    JofreEspigulePons`WolframChallengesBenchmark`GenerateSolutions[
      challenges, testBank, opts];

  If[result === $Failed, Exit[4]];

  Module[{counts = Lookup[result, "counts", <||>]},
    WriteString[$Output, "\n=== ", modelSlugStr, " ===\n"];
    WriteString[$Output, "  saved:    ",
                Lookup[counts, "ok",            0], "\n"];
    WriteString[$Output, "  failed:   ",
                Lookup[counts, "failed",        0], "\n"];
    WriteString[$Output, "  rejected: ",
                Lookup[counts, "auditRejected", 0], "\n"];
    WriteString[$Output, "  other:    ",
                Lookup[counts, "other",         0], "\n"];
    WriteString[$Output, "  outDir:   ", result["outDir"], "\n"];
    WriteString[$Output, "  log:      ", result["logPath"], "\n"];

    failed = Lookup[counts, "failed", 0] +
             Lookup[counts, "auditRejected", 0];
    If[failed > 0, exitCode = 2];
  ];

  Exit[exitCode]
];


(* Top-level safety net *)
Module[{},
  Quiet @ Check[main[],
    WriteString[$Output, "FATAL: ", $MessageList, "\n"];
    Exit[4]
  ]
];
