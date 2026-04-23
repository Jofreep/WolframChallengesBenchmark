#!/usr/bin/env wolframscript
(* ::Package:: *)

(* scripts/TestOpenRouter.wls

   Production diagnostic for the OpenRouter integration that the
   benchmark actually uses.  Where TestOpenRouterAPI.sh hits the API
   with curl (testing what OpenRouter returns), this script exercises
   the paclet's OpenRouterChatComplete entrypoint (testing what our
   Wolfram code does with that response).

   Five smoke phases (always run):
     1. Paclet load + symbol surface check
     2. Bad-args envelope contract (no network)
     3. API-key resolution
     4. Live POST to a known-good fast model (default: google/gemini-2.5-flash)
     5. Live POST to the model under suspicion (default: minimax/minimax-m2.7)

   Optional phase 6 (when --challenge NAME is given):
     6. Live POST of the ACTUAL prompt the generator would send for
        challenge NAME, against the suspect model, with --challenge-max-tokens
        and --challenge-timeout (defaults match the prod generator: 16000 / 600).
        This is the test that reproduces real failures: token-budget
        exhaustion, URLRead socket-idle hangs, reasoning-channel rescue,
        forensic dumps.

   Each live phase prints the FULL result Association so reasoning-
   channel issues, finish_reason="length" patterns, and forensic-dump
   paths are visible.  Exit code is the count of failed phases (0 = all
   passed), capped at 99 for shell convention.

   Usage:
     wolframscript -file scripts/TestOpenRouter.wls
     wolframscript -file scripts/TestOpenRouter.wls --api-key-file ~/.config/openrouter/key.txt
     wolframscript -file scripts/TestOpenRouter.wls --model openai/gpt-4o-mini
     wolframscript -file scripts/TestOpenRouter.wls --good-model anthropic/claude-3-haiku
     wolframscript -file scripts/TestOpenRouter.wls --max-tokens 256 --timeout 60
     wolframscript -file scripts/TestOpenRouter.wls --prompt "Reply with the word: pong"
     wolframscript -file scripts/TestOpenRouter.wls --skip-live          # phases 1-3 only

     # Reproduce a real failure end-to-end:
     wolframscript -file scripts/TestOpenRouter.wls --challenge AliquotSequence
     wolframscript -file scripts/TestOpenRouter.wls --challenge FivePointConic \
       --model openai/gpt-4o-mini --challenge-max-tokens 4096 --challenge-timeout 120
*)


(* ------------------------- arg parsing ------------------------- *)

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

args         = parseArgs[Drop[$ScriptCommandLine, 1]];
keyFileArg   = Lookup[args, "api-key-file",
                 FileNameJoin[{$HomeDirectory, ".config", "openrouter", "key.txt"}]];
goodModelArg = Lookup[args, "good-model",  "google/gemini-2.5-flash"];
suspectArg   = Lookup[args, "model",       "minimax/minimax-m2.7"];
maxTokensArg = ToExpression @ Lookup[args, "max-tokens", "64"];
timeoutArg   = ToExpression @ Lookup[args, "timeout",    "60"];
promptArg    = Lookup[args, "prompt",      "Reply with the single word: pong"];
skipLive     = TrueQ @ ToExpression @ Lookup[args, "skip-live", "False"];

(* Phase-6 (real challenge) knobs.  Defaults match the generator's
   defaults so a bare --challenge NAME reproduces production load.    *)
challengeArg          = Lookup[args, "challenge",       None];
challengesFileArg     = Lookup[args, "challenges-file",
                         FileNameJoin[{Directory[], "ChallengesTestDataV1.json"}]];
challengeMaxTokensArg = ToExpression @ Lookup[args, "challenge-max-tokens", "16000"];
challengeTimeoutArg   = ToExpression @ Lookup[args, "challenge-timeout",    "600"];


(* ------------------------- formatting helpers ------------------------- *)

failed = 0;

bumpFail[] := (failed = failed + 1);

write[s_String] := WriteString[$Output, s];
writeln[s_String] := WriteString[$Output, s, "\n"];
writeln[]         := WriteString[$Output, "\n"];

phase[n_Integer, label_String] := (
  writeln[];
  writeln["[Phase " <> ToString[n] <> "] " <> label];
);

pass[s_String] := writeln["  PASS  " <> s];
fail[s_String] := (writeln["  FAIL  " <> s]; bumpFail[]);
warn[s_String] := writeln["  WARN  " <> s];
info[s_String] := writeln["  " <> s];

shorten[s_String, n_Integer] := If[StringLength[s] <= n, s,
  StringTake[s, n] <> "..."];
shorten[other_, n_Integer] := shorten[ToString[other, InputForm], n];

(* Pretty-print a result envelope without dumping the entire raw response
   (which can be huge for reasoning models). *)
printResult[r_Association] := Module[{keys, raw, dump},
  KeyValueMap[
    Function[{k, v},
      Which[
        k === "rawResponse",
          info["rawResponse: <" <>
            If[AssociationQ[v], "Association, keys=" <> ToString[Keys[v]],
               If[StringQ[v],   "String, " <> ToString[StringLength[v]] <> " chars",
                                ToString[Head[v]]]] <> ">"],
        k === "content",
          info["content:     " <>
            If[StringQ[v],
              ToString[StringLength[v]] <> " chars: " <> shorten[v, 80],
              ToString[v, InputForm]]],
        k === "forensicDumpPath",
          info["forensicDumpPath: " <> ToString[v]],
        True,
          info[ToString[k] <> ": " <> ToString[v, InputForm]]
      ]
    ],
    r
  ]
];

printResult[other_] := info["(non-Association result: " <> ToString[other, InputForm] <> ")"];


(* ------------------------- Phase 1: paclet load ------------------------- *)

phase[1, "Paclet load + symbol surface"];

here     = DirectoryName[$InputFileName];
root     = DirectoryName[here];
pacletDir = FileNameJoin[{root, "WolframChallengesLLMBenchmarkPaclet"}];

If[! DirectoryQ[pacletDir],
  fail["paclet directory not found: " <> pacletDir];
  Exit[1]
];

PacletDirectoryLoad[pacletDir];

needsResult = Quiet @ Check[
  Needs["JofreEspigulePons`WolframChallengesBenchmark`"],
  $Failed
];
If[needsResult === $Failed,
  fail["Needs[\"JofreEspigulePons`WolframChallengesBenchmark`\"] failed"];
  Exit[1]
];

requiredSymbols = {
  "JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete",
  "JofreEspigulePons`WolframChallengesBenchmark`$OpenRouterAPIKey"
};

missing = Select[requiredSymbols, NameQ[#] === False &];
If[missing =!= {},
  fail["missing symbols: " <> ToString[missing]],
  pass["paclet loaded; required symbols present"]
];


(* ------------------------- Phase 2: bad-args contract ------------------------- *)

phase[2, "Bad-args envelope contract (no network)"];

(* Empty messages list should produce a typed error envelope, not a
   stack trace.  We Quiet because the function intentionally messages.  *)
badArgsResult = Quiet @ JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete[
  {}, <|"Model" -> "test/test"|>];

If[AssociationQ[badArgsResult] && badArgsResult["status"] === "bad-messages",
  pass["empty-messages call returns status=bad-messages"];
  info["latencySec: " <> ToString[badArgsResult["latencySec"]]],
  fail["unexpected envelope for empty messages: " <> ToString[badArgsResult, InputForm]]
];


(* ------------------------- Phase 3: API key ------------------------- *)

phase[3, "API key resolution"];

If[! FileExistsQ[keyFileArg],
  fail["key file not found: " <> keyFileArg];
  info["pass --api-key-file <path> to point at a different location"];
  If[skipLive,
    writeln[]; writeln["skipping live phases as requested"];
    Exit[failed]];
  writeln[]; writeln["abandoning live phases"];
  Exit[failed]
];

keyRaw = Quiet @ Check[Import[keyFileArg, "Text"], $Failed];
If[! StringQ[keyRaw],
  fail["could not read key file: " <> keyFileArg];
  Exit[1]
];

keyTrimmed = StringTrim[keyRaw];
keyLen = StringLength[keyTrimmed];

If[keyLen < 30,
  fail["key looks too short (" <> ToString[keyLen] <> " chars)"];
  Exit[1]
];

If[! StringStartsQ[keyTrimmed, "sk-or-"],
  warn["key does not start with sk-or-* (got: " <>
       StringTake[keyTrimmed, UpTo[8]] <> "...)"]
];

(* Inject into env so the paclet's resolveOpenRouterAPIKey[] sees it.
   The paclet intentionally only reads from OPENROUTER_API_KEY; this is
   how production callers wire in the key.                              *)
SetEnvironment["OPENROUTER_API_KEY" -> keyTrimmed];

(* Sanity-check the round-trip *)
envCheck = Environment["OPENROUTER_API_KEY"];
If[envCheck =!= keyTrimmed,
  fail["env var did not round-trip after SetEnvironment"];
  Exit[1]
];

pass["key loaded (" <> ToString[keyLen] <> " chars, prefix " <>
     StringTake[keyTrimmed, UpTo[8]] <> "...) and exported to env"];


(* ------------------------- live phase runner ------------------------- *)

If[skipLive,
  writeln[]; writeln["skipping live phases (--skip-live)"];
  writeln[]; writeln["Failed phases: " <> ToString[failed]];
  Exit[Min[failed, 99]]
];

runChat[modelSlug_String, phaseNum_Integer, label_String] := Module[
  {result, t0, t1, dt},
  phase[phaseNum, "POST /chat/completions  (" <> label <> ": " <> modelSlug <> ")"];

  t0 = AbsoluteTime[];
  result = Quiet @ JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete[
    {<|"role" -> "system",
       "content" -> "You are a connection test endpoint. Respond exactly as instructed."|>,
     <|"role" -> "user", "content" -> promptArg|>},
    <|"Model"          -> modelSlug,
      "MaxTokens"      -> maxTokensArg,
      "Temperature"    -> 0.0,
      "TimeConstraint" -> timeoutArg|>
  ];
  t1 = AbsoluteTime[];
  dt = t1 - t0;

  info["wall time:  " <> ToString[NumberForm[dt, {6, 2}]] <> "s"];

  If[! AssociationQ[result],
    fail["non-Association result: " <> ToString[result, InputForm]];
    Return[]
  ];

  printResult[result];

  Switch[result["status"],
    "ok",
      If[StringQ[result["content"]] && StringLength[result["content"]] > 0,
        pass["got non-empty content (source=" <> ToString[result["contentSource"]] <> ")"],
        fail["status=ok but content was empty"]
      ],

    "timeout",
      fail["request timed out after " <> ToString[timeoutArg] <> "s"];
      info["this is the same hang pattern that killed the minimax run"];,

    "http-error",
      fail["HTTP " <> ToString[result["httpStatus"]] <> " from OpenRouter"];,

    "malformed",
      fail["malformed response: " <> ToString[result["error"]]];
      If[KeyExistsQ[result, "finishReason"] && result["finishReason"] === "length",
        info["finishReason=length suggests reasoning-token exhaustion"];
        info["try raising --max-tokens or switching to a non-reasoning model"]];,

    "no-api-key",
      fail["paclet says OPENROUTER_API_KEY is unset (env-injection broken?)"];,

    "network-error",
      fail["network error: " <> ToString[result["error"]]];,

    _,
      fail["unexpected status: " <> ToString[result["status"]]]
  ];
];


(* ------------------------- Phase 4: known-good model ------------------------- *)

runChat[goodModelArg, 4, "reference model"];


(* ------------------------- Phase 5: suspect model ------------------------- *)

runChat[suspectArg, 5, "suspect model"];


(* ------------------------- Phase 6: real challenge ------------------------- *)

(* Reuse the paclet's prompt template (the canonical $defaultPromptTemplate
   defined in Generator.wl) so the prompt bytes match what the production
   generator sends.  We splice the challenge body into {{CHALLENGE}}.    *)

runRealChallenge[challengeName_String] := Module[
  {challengesPath, challenges, recon, entry, body, fullPrompt,
   templateSym, template, result, t0, t1, dt, content, rawResp,
   wallVsLatency},

  phase[6, "POST /chat/completions  (REAL CHALLENGE prompt: " <>
            challengeName <> " against " <> suspectArg <> ")"];

  (* --- Load + reconcile challenges --- *)
  challengesPath = challengesFileArg;
  If[! FileExistsQ[challengesPath],
    fail["challenges file not found: " <> challengesPath];
    info["pass --challenges-file <path> to override"];
    Return[]
  ];
  info["loading challenges from: " <> challengesPath];

  challenges = Quiet @ Check[
    JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges[challengesPath],
    $Failed
  ];
  If[! AssociationQ[challenges],
    fail["LoadChallenges did not return an Association"];
    Return[]
  ];

  (* Reconcile so casing matches whichever spelling the user typed. *)
  Module[{testBankPath, tb},
    testBankPath = FileNameJoin[{root, "ChallengesTests.wxf"}];
    If[FileExistsQ[testBankPath],
      tb = Quiet @ JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank[testBankPath];
      If[AssociationQ[tb],
        recon = Quiet @ JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames[
          challenges, tb];
        If[AssociationQ[recon] && KeyExistsQ[recon, "challenges"],
          challenges = recon["challenges"]]]];
  ];

  If[! KeyExistsQ[challenges, challengeName],
    fail["no challenge named '" <> challengeName <> "' in " <> challengesPath];
    info["available (sample): " <>
      ToString[Take[Sort @ Keys @ challenges, UpTo[10]]]];
    Return[]
  ];

  entry = challenges[challengeName];
  body  = Lookup[entry, "prompt", ""];

  If[! StringQ[body] || StringLength[body] === 0,
    fail["challenge entry has no usable 'prompt' field"];
    Return[]
  ];

  info["challenge body: " <> ToString[StringLength[body]] <> " bytes"];
  info["body sample:    " <> shorten[StringReplace[body, "\n" -> " | "], 120]];

  (* Pull the canonical template from the paclet's private context.
     We reference it by full name; if the paclet has been refactored
     and the symbol no longer exists, we fall back to a verbatim copy
     that matches the current Generator.wl source.                    *)
  templateSym = JofreEspigulePons`WolframChallengesBenchmark`Private`$defaultPromptTemplate;
  template = If[StringQ[templateSym],
    templateSym,
    "You are an expert Wolfram Language programmer. Read the following \
programming challenge carefully and write a correct, efficient, and \
idiomatic Wolfram Language solution.

Instructions:
- Output only Wolfram Language code.
- Do not include explanations, comments, or sample evaluations.
- Define the requested function exactly as specified in the challenge.
- Respect all constraints, stopping conditions, and edge cases \
described in the prompt.
- Prefer built-in Wolfram Language functionality when appropriate.
- Ensure the code is concise, robust, and consistent with the examples \
in the challenge.
- Do not add any extra text before or after the code. You may \
optionally wrap the code in a ```wl ... ``` fence.

Challenge:
{{CHALLENGE}}"
  ];

  fullPrompt = StringReplace[template, "{{CHALLENGE}}" -> body];
  info["full prompt:    " <> ToString[StringLength[fullPrompt]] <> " bytes"];
  info["max-tokens:     " <> ToString[challengeMaxTokensArg]];
  info["timeout:        " <> ToString[challengeTimeoutArg] <> "s"];

  (* --- Live POST via the same paclet path the generator uses --- *)
  t0 = AbsoluteTime[];
  result = Quiet @ JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete[
    {<|"role" -> "user", "content" -> fullPrompt|>},
    <|"Model"          -> suspectArg,
      "MaxTokens"      -> challengeMaxTokensArg,
      "Temperature"    -> 0.0,
      "TimeConstraint" -> challengeTimeoutArg|>
  ];
  t1 = AbsoluteTime[];
  dt = t1 - t0;

  info["wall time:  " <> ToString[NumberForm[dt, {6, 2}]] <> "s"];

  If[! AssociationQ[result],
    fail["non-Association result: " <> ToString[result, InputForm]];
    Return[]
  ];

  printResult[result];

  (* --- Triage classification ---

     The interesting failure modes for production:
       * status="timeout"  + wall < 130s  -> URLRead socket-idle hang (Bug A)
                                              fired before our TimeConstraint did
       * status="malformed" + finishReason="length" -> reasoning exhaustion (Bug B)
       * status="ok" + non-empty content   -> production path is healthy on this prompt
       * status="ok" + empty content       -> parser bug, shouldn't happen        *)

  Switch[result["status"],
    "ok",
      content = result["content"];
      If[StringQ[content] && StringLength[content] > 0,
        pass["got non-empty content (" <> ToString[StringLength[content]] <>
             " chars, source=" <> ToString[result["contentSource"]] <> ")"];
        info["code preview:"];
        Scan[Function[ln, info["  | " <> ln]],
          Take[StringSplit[content, "\n"], UpTo[8]]];
        If[Length[StringSplit[content, "\n"]] > 8, info["  | ..."]],
        fail["status=ok but content empty (parser bug)"]
      ],

    "timeout",
      fail["request timed out after wall=" <> ToString[NumberForm[dt, {6, 2}]] <> "s"];
      If[dt < challengeTimeoutArg - 10,
        info["wall time is BELOW our TimeConstraint of " <>
             ToString[challengeTimeoutArg] <> "s"];
        info["BUG A confirmed: URLRead socket-idle timer fired first."];
        info["The paclet's TimeConstraint -> " <> ToString[challengeTimeoutArg] <>
             " is not honored by URLRead's underlying socket."],
        info["wall time matches our TimeConstraint - the model genuinely took too long."];
        info["raise --challenge-timeout, or pick a faster model."]
      ],

    "malformed",
      fail["malformed response: " <> ToString[result["error"]]];
      Module[{fr},
        fr = Lookup[result, "finishReason", None];
        If[fr === "length",
          info["BUG B confirmed: finishReason=length with no extractable content."];
          info["The model burned all " <> ToString[challengeMaxTokensArg] <>
               " tokens on the hidden reasoning channel and never emitted .content."];
          info["remediation: raise --challenge-max-tokens, switch to a non-reasoning model,"];
          info["             or rescue from .reasoning by enabling that fallback."]
        ];
        If[KeyExistsQ[result, "forensicDumpPath"],
          info["forensic dump: " <> ToString[result["forensicDumpPath"]]]]
      ],

    "http-error",
      fail["HTTP " <> ToString[result["httpStatus"]] <> " from OpenRouter"];,

    "no-api-key",
      fail["paclet says OPENROUTER_API_KEY is unset (env-injection broken?)"];,

    "network-error",
      fail["network error: " <> ToString[result["error"]]];,

    _,
      fail["unexpected status: " <> ToString[result["status"]]]
  ];
];

If[challengeArg =!= None,
  runRealChallenge[challengeArg],
  writeln[];
  writeln["[Phase 6] (skipped - pass --challenge NAME to reproduce a real failure)"];
];


(* ------------------------- summary ------------------------- *)

writeln[];
writeln["----------------------------------------------------------------"];

phasesRun = If[challengeArg =!= None, 6, 5];

If[failed === 0,
  writeln["OVERALL: PASS  (" <> ToString[phasesRun] <> "/" <>
          ToString[phasesRun] <> " phases)"];
  Exit[0]
];

writeln["OVERALL: FAIL  (" <> ToString[failed] <> " of " <>
        ToString[phasesRun] <> " phase(s) failed)"];
Exit[Min[failed, 99]]
