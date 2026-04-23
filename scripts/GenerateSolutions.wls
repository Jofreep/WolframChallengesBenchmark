#!/usr/bin/env wolframscript
(* ::Package:: *)

(* scripts/GenerateSolutions.wls \[LongDash] LLM solution generator.

   Drives the JofreEspigulePons/WolframChallengesBenchmark paclet's
   GenerateSolutions with OpenRouter as the backend.  Per challenge:

       prompt  \[Rule] OpenRouterChatComplete (retry+backoff, TimeConstrained)
                \[Rule] ExtractCode (markdown-fence stripped)
                \[Rule] SaveSolution test-bank audit (refuses bad code)
                \[Rule] JSONL audit log (every attempt)

   The script calls ReconcileNames before handing off so that the prompts
   bank and the test bank use the same canonical challenge names, even
   when they diverge by case/punctuation/diacritics.

   Usage:
     wolframscript -file scripts/GenerateSolutions.wls \
       --challenges ChallengesTestDataV1.json \
       --testbank   ChallengesTests.wxf      \
       --out        solutions               \
       --model      minimax/minimax-m2.7   \
       [--max-tokens 4096] [--temperature 0.0] \
       [--filter FizzBuzz,Palindromes]       \
       [--limit 10]                          \
       [--timeout 120] [--retries 3]         \
       [--retry-base-delay 2.0]              \
       [--overwrite] [--dry-run] [--save-raw] \
       [--log path/to/generate.jsonl]        \
       [--api-key-file ~/.config/openrouter/key.txt] \
       [--no-reconcile]

   Authentication:
     OpenRouter API key is read from the OPENROUTER_API_KEY environment
     variable by default.  --api-key-file points at a plain-text file
     containing the key (trailing whitespace trimmed).  The key is never
     echoed to logs or the terminal.

   Exit codes:
     0  generator finished; every processed challenge saved successfully
     1  invalid arguments or missing input files
     2  generator finished but some challenges failed or were audit-rejected
     3  generator never ran because no API key was available
     4  unexpected fatal error
*)


(* ----------------------------------------------------------------------- *)
(* CLI parsing                                                             *)
(* ----------------------------------------------------------------------- *)

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

withDefault[args_, key_, default_] := Lookup[args, key, default];

truthy[v_] := ! MemberQ[{"false", "False", "0", "off", "no"}, v];

abort[code_Integer, msg_] := (
  WriteString[$Output, "ERROR: ", msg, "\n"];
  Exit[code]
);

requireArg[args_, key_] := If[KeyExistsQ[args, key], args[key],
  abort[1, "missing required --" <> key]];

toInt[v_, fallback_Integer] := Module[{n = Quiet @ ToExpression[ToString[v]]},
  If[IntegerQ[n], n, fallback]];
toReal[v_, fallback_] := Module[{x = Quiet @ ToExpression[ToString[v]]},
  If[NumericQ[x], N[x], fallback]];


(* ----------------------------------------------------------------------- *)
(* Help screen                                                             *)
(* ----------------------------------------------------------------------- *)

printHelp[] := WriteString[$Output,
  "GenerateSolutions.wls \[LongDash] LLM solution generator (OpenRouter)\n\n",
  "Required:\n",
  "  --challenges PATH      JSON file of challenge prompts\n",
  "  --testbank   PATH      WXF file of expected outputs\n",
  "  --out        DIR       solutions root (solutions go into <out>/<modelSlug>/)\n",
  "  --model      STRING    OpenRouter model id, e.g. minimax/minimax-m2.7\n\n",
  "LLM options:\n",
  "  --max-tokens    N      default 4096 (reasoning models need > 1000)\n",
  "  --temperature   X      default 0.0\n",
  "  --timeout       N      per-call seconds, default 120\n",
  "  --retries       N      max attempts per challenge, default 3\n",
  "  --retry-base-delay X   base seconds between retries, default 2.0\n\n",
  "Selection:\n",
  "  --filter A,B,C         comma-sep challenge names to process\n",
  "  --limit N              process at most N challenges (after filter)\n",
  "  --overwrite            regenerate even if <name>.wl already exists\n",
  "  --dry-run              don't hit the LLM; emit audit-rejecting stubs\n",
  "  --save-raw             also write <name>.raw.txt with the un-extracted reply\n",
  "  --log PATH             override JSONL audit log location\n",
  "  --no-reconcile         skip ReconcileNames (use only if banks already align)\n\n",
  "Auth:\n",
  "  OPENROUTER_API_KEY     env var (default source)\n",
  "  --api-key-file PATH    plain-text file containing the key\n\n",
  "Exits 0 on success, 2 if any challenge failed or was audit-rejected,\n",
  "3 if no API key was available, 4 on unexpected fatal error.\n"
];


(* ----------------------------------------------------------------------- *)
(* Boot the paclet                                                         *)
(* ----------------------------------------------------------------------- *)

bootPaclet[] := Module[{here, root, pacletDir},
  here     = DirectoryName[$InputFileName];
  root     = DirectoryName[here];
  pacletDir = FileNameJoin[{root, "WolframChallengesLLMBenchmarkPaclet"}];
  If[! DirectoryQ[pacletDir],
    abort[1, "could not locate paclet at " <> pacletDir]];
  Quiet @ PacletDirectoryLoad[pacletDir];
  Needs["JofreEspigulePons`WolframChallengesBenchmark`"];
];


(* ----------------------------------------------------------------------- *)
(* API key resolution (never logs the key)                                 *)
(* ----------------------------------------------------------------------- *)

resolveApiKey[args_Association] := Module[{fromFile, envKey, trimmed},
  envKey = Environment["OPENROUTER_API_KEY"];
  If[KeyExistsQ[args, "api-key-file"],
    fromFile = Quiet @ Check[Import[args["api-key-file"], "Text"], $Failed];
    If[fromFile === $Failed,
      abort[1, "could not read --api-key-file " <> args["api-key-file"]]
    ];
    trimmed = StringTrim[fromFile];
    If[trimmed === "",
      abort[1, "--api-key-file " <> args["api-key-file"] <> " is empty"]
    ];
    SetEnvironment["OPENROUTER_API_KEY" -> trimmed];
    Return[trimmed]
  ];
  If[StringQ[envKey] && StringLength[StringTrim[envKey]] > 0, envKey, None]
];


(* ----------------------------------------------------------------------- *)
(* safeSlug: mirror the paclet's slug rules for modelSlug.                 *)
(* ----------------------------------------------------------------------- *)

modelSlug[model_String] := StringReplace[model,
  RegularExpression["[^A-Za-z0-9_.-]"] -> "_"];


(* ----------------------------------------------------------------------- *)
(* Main                                                                     *)
(* ----------------------------------------------------------------------- *)

main[] := Module[
  {args, apiKey, challengesPath, testBankPath, outRoot, model,
   maxTokens, temperature, timeout, retries, retryBase,
   overwrite, dryRun, saveRaw, logPath, filterArg, limit, filter,
   noReconcile, challenges, testBank, recon, outDir, opts, result,
   counts, exitCode = 0},

  args = parseArgs[Drop[$ScriptCommandLine, 1]];

  If[KeyExistsQ[args, "help"] || KeyExistsQ[args, "h"],
    printHelp[]; Exit[0]];

  bootPaclet[];

  challengesPath = requireArg[args, "challenges"];
  testBankPath   = requireArg[args, "testbank"];
  outRoot        = requireArg[args, "out"];
  model          = requireArg[args, "model"];

  If[! FileExistsQ[challengesPath],
    abort[1, "--challenges file not found: " <> challengesPath]];
  If[! FileExistsQ[testBankPath],
    abort[1, "--testbank file not found: "   <> testBankPath]];

  maxTokens   = toInt[ withDefault[args, "max-tokens", 4096], 4096];
  temperature = toReal[withDefault[args, "temperature", 0.0], 0.0];
  timeout     = toInt[ withDefault[args, "timeout", 120], 120];
  retries     = toInt[ withDefault[args, "retries", 3], 3];
  retryBase   = toReal[withDefault[args, "retry-base-delay", 2.0], 2.0];
  overwrite   = KeyExistsQ[args, "overwrite"] &&
                  truthy[withDefault[args, "overwrite", "true"]];
  dryRun      = KeyExistsQ[args, "dry-run"] &&
                  truthy[withDefault[args, "dry-run", "true"]];
  saveRaw     = KeyExistsQ[args, "save-raw"] &&
                  truthy[withDefault[args, "save-raw", "true"]];
  noReconcile = KeyExistsQ[args, "no-reconcile"] &&
                  truthy[withDefault[args, "no-reconcile", "true"]];
  filterArg   = Lookup[args, "filter", None];
  filter      = If[filterArg === None, All, StringSplit[filterArg, ","]];
  limit       = toInt[ withDefault[args, "limit", 0], 0];
  logPath     = Lookup[args, "log", Automatic];

  apiKey = resolveApiKey[args];
  If[!dryRun && apiKey === None,
    WriteString[$Output,
      "ERROR: no OpenRouter API key available. Set OPENROUTER_API_KEY ",
      "or pass --api-key-file.\n"];
    Exit[3]
  ];

  outDir = FileNameJoin[{outRoot, modelSlug[model]}];

  WriteString[$Output, "Loading challenges from ", challengesPath, " ...\n"];
  challenges = JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges[challengesPath];
  If[challenges === $Failed, Exit[1]];

  WriteString[$Output, "Loading test bank from ", testBankPath, " ...\n"];
  testBank = JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank[testBankPath];
  If[testBank === $Failed, Exit[1]];

  If[!noReconcile,
    recon = JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames[
      challenges, testBank];
    If[! AssociationQ[recon],
      abort[4, "ReconcileNames did not return an Association"]
    ];
    WriteString[$Output,
      "ReconcileNames: matched=", recon["summary", "matched"],
      ", renamed=", recon["summary", "renamed"],
      ", unmatchedChallenges=", recon["summary", "unmatchedChallenges"],
      ", unmatchedTestBank=", recon["summary", "unmatchedTestBank"],
      "\n"];
    challenges = recon["challenges"]
  ];

  If[ListQ[filter],
    filter = Select[filter, StringQ[#] && StringLength[#] > 0 &];
    WriteString[$Output, "Filter: ",
      If[Length[filter] > 6,
        ToString[Take[filter, 6]] <> " (+" <>
          ToString[Length[filter] - 6] <> " more)",
        ToString[filter]],
      "\n"]
  ];

  WriteString[$Output,
    "\nGenerating solutions:\n",
    "  model:       ", model, "\n",
    "  maxTokens:   ", maxTokens, "\n",
    "  temperature: ", temperature, "\n",
    "  timeout:     ", timeout, "s\n",
    "  retries:     ", retries, "\n",
    "  overwrite:   ", If[overwrite, "YES", "no"], "\n",
    "  dryRun:      ", If[dryRun, "YES", "no"], "\n",
    "  outDir:      ", outDir, "\n\n"
  ];

  opts = <|
    "Model"           -> model,
    "OutputDirectory" -> outDir,
    "Filter"          -> filter,
    "MaxAttempts"     -> retries,
    "TimeConstraint"  -> timeout,
    "RetryBaseDelay"  -> retryBase,
    "Overwrite"       -> overwrite,
    "DryRun"          -> dryRun,
    "SaveRawResponse" -> saveRaw,
    "LLMInfo"         -> <|
      "model"    -> model,
      "provider" -> "openrouter",
      "note"     -> "scripts/GenerateSolutions.wls"
    |>,
    "LLMOptions"      -> <|
      "Model"       -> model,
      "MaxTokens"   -> maxTokens,
      "Temperature" -> temperature,
      "TimeConstraint" -> timeout
    |>
  |>;
  If[logPath =!= Automatic, opts["LogPath"] = logPath];

  (* Apply --limit after filter resolution.  GenerateSolutions itself
     takes Filter -> All | {names}, so we pre-trim the Assoc instead.     *)
  If[IntegerQ[limit] && limit > 0,
    Module[{keysInOrder},
      keysInOrder = If[ListQ[filter],
        Select[filter, KeyExistsQ[challenges, #] &],
        Keys[challenges]];
      keysInOrder = Take[keysInOrder, UpTo[limit]];
      (* AssociationMap preserves keysInOrder; KeyTake would reset to
         the original challenge-bank order. *)
      challenges = AssociationMap[challenges, keysInOrder];
      opts["Filter"] = All;
      WriteString[$Output, "Applied --limit ", limit,
        " -> ", Length[keysInOrder], " challenge(s) queued\n"]
    ]
  ];

  result = Quiet @ Check[
    JofreEspigulePons`WolframChallengesBenchmark`GenerateSolutions[
      challenges, testBank, opts],
    $Failed
  ];

  If[result === $Failed,
    WriteString[$Output, "FATAL: generator returned $Failed\n"];
    Exit[4]];

  counts = result["counts"];
  WriteString[$Output, "\n=== ", model, " ===\n"];
  WriteString[$Output, "  processed:      ",
    Length[Keys[result["results"]]], "\n"];
  WriteString[$Output, "  ok:             ", counts["ok"], "\n"];
  WriteString[$Output, "  audit-rejected: ", counts["auditRejected"], "\n"];
  WriteString[$Output, "  failed (LLM):   ", counts["failed"], "\n"];
  WriteString[$Output, "  other:          ", counts["other"], "\n"];
  WriteString[$Output, "  log:            ", result["logPath"], "\n"];
  WriteString[$Output, "  outDir:         ", result["outDir"], "\n"];

  If[counts["failed"] > 0 || counts["auditRejected"] > 0 ||
       counts["other"] > 0,
    WriteString[$Output,
      "\nSome challenges did not produce a valid solution. ",
      "Inspect the JSONL log for per-attempt details.\n"];
    exitCode = 2
  ];

  Exit[exitCode]
];


(* Top-level safety net *)
Module[{},
  Quiet @ Check[main[],
    WriteString[$Output,
      "FATAL: ", ToString[$MessageList, InputForm], "\n"];
    Exit[4]
  ]
];
