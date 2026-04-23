(* ::Package:: *)

(* :Context: ChallengesBenchmark`Private` *)
(* :Summary:
     LLM-backed solution generator.  Iterates over challenges, calls a
     caller-supplied LLM function with a per-call TimeConstrained envelope
     and bounded exponential-backoff retry, strips the returned code with
     ExtractCode, audits the result against the test bank via
     saveSolutionImpl, and writes to
       solutions/<modelSlug>/<name>.wl
       solutions/<modelSlug>/<name>.meta.json
     with a per-run JSONL audit log of every prompt/response pair.

     The actual LLM call is injected via the "Generator" option:
       • tests pass a deterministic stub;
       • the CLI passes a closure around LLMSynthesize.
     This keeps the module testable without network access and makes it
     provider-agnostic — any gen[promptString] -> String function works.
*)

Begin["ChallengesBenchmark`Private`"];

(* ----------------------------------------------------------------------- *)
(* Defaults                                                                *)
(* ----------------------------------------------------------------------- *)

$generatorVersion = "GenerateSolutions/v1";

(* The legacy notebook shipped a "/n" typo to the model; fixed here. The
   challenge body is substituted in verbatim for the {{CHALLENGE}} marker. *)

$defaultPromptTemplate =
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
{{CHALLENGE}}";

(* ----------------------------------------------------------------------- *)
(* Default LLM closure                                                     *)
(* ----------------------------------------------------------------------- *)

(* defaultLLMGenerator[evaluator] — returns a closure that, given a prompt
   string, calls LLMSynthesize[prompt, LLMEvaluator -> evaluator].
   Returns $Failed if LLMSynthesize itself is not resolvable at runtime
   (older Wolfram versions, paclet not loaded) or raises a message.
*)

defaultLLMGenerator[evaluator_] := Function[{prompt},
  Module[{resp},
    If[! StringQ[prompt], Return[$Failed]];
    If[! NameQ["System`LLMSynthesize"],
      logError["LLMSynthesize is unavailable — install Wolfram 13.3+ or the LLMFunctions paclet."];
      Return[$Failed]
    ];
    resp = Quiet @ Check[
      Symbol["System`LLMSynthesize"][prompt, LLMEvaluator -> evaluator],
      $Failed
    ];
    If[StringQ[resp], resp, $Failed]
  ]
];

(* ----------------------------------------------------------------------- *)
(* Prompt assembly                                                         *)
(* ----------------------------------------------------------------------- *)

buildPrompt[template_String, challengeEntry_Association] := Module[{body},
  body = Lookup[challengeEntry, "prompt", ""];
  If[! StringQ[body] || StringLength[body] === 0, Return[$Failed]];
  StringReplace[template, "{{CHALLENGE}}" -> body]
];

buildPrompt[___] := $Failed;

(* ----------------------------------------------------------------------- *)
(* callLLMWithRetry                                                        *)
(*                                                                         *)
(* TimeConstrained envelope around the injected generator, with bounded    *)
(* exponential backoff between attempts. Returns an Association:           *)
(*     <| "status" -> "ok" | "timeout" | "failed" | "unsupported",         *)
(*        "response" -> String | None,                                     *)
(*        "attempts" -> { <|"attempt"->i,"durationSec"->..., "status"->..|> }, *)
(*        "totalSec" -> Real |>                                            *)
(* ----------------------------------------------------------------------- *)

callLLMWithRetry[gen_, prompt_String, opts_Association] := Module[
  {maxAttempts, timeoutSec, baseDelay, cap, startAll, attemptRecs = {},
   attempt = 0, response = $Failed, t0, dt, delay, attemptStatus,
   finalStatus = "failed"},

  maxAttempts = Max[1, ToExpression[ToString[Lookup[opts, "MaxAttempts", 3]]]];
  timeoutSec  = Max[1, Lookup[opts, "TimeConstraint", 120]];
  baseDelay   = Max[0.0, Lookup[opts, "RetryBaseDelay", 2.0]];
  cap         = Lookup[opts, "RetryCapDelay", 60.0];

  startAll = AbsoluteTime[];
  While[attempt < maxAttempts,
    attempt++;
    t0 = AbsoluteTime[];
    response = Quiet @ Check[
      TimeConstrained[gen[prompt], timeoutSec, $TimedOut],
      $Failed
    ];
    dt = AbsoluteTime[] - t0;
    attemptStatus = Which[
      response === $TimedOut, "timeout",
      response === $Failed,   "failed",
      StringQ[response],      "ok",
      True,                   "failed"
    ];
    AppendTo[attemptRecs, <|
      "attempt"     -> attempt,
      "durationSec" -> dt,
      "status"      -> attemptStatus
    |>];
    If[attemptStatus === "ok",
      finalStatus = "ok";
      Break[]
    ];
    finalStatus = attemptStatus;
    If[attempt < maxAttempts,
      delay = Min[cap, baseDelay * 2^(attempt - 1)];
      If[delay > 0, Pause[delay]]
    ]
  ];

  <|
    "status"   -> finalStatus,
    "response" -> If[StringQ[response], response, None],
    "attempts" -> attemptRecs,
    "totalSec" -> AbsoluteTime[] - startAll
  |>
];

(* ----------------------------------------------------------------------- *)
(* processOneChallenge                                                     *)
(*                                                                         *)
(* Prompt → call → extract → audit → save one challenge.                   *)
(* Returns a per-challenge outcome Association; every terminal state is    *)
(* also logged to the JSONL stream when LogPath is set.                    *)
(* ----------------------------------------------------------------------- *)

processOneChallenge[name_String, challengeEntry_Association, testBank_, opts_Association] :=
Module[{
   promptTemplate, prompt, genFn, callOpts, callResult,
   rawResponse, extracted, saveDir, savePath, extraMeta,
   promptHash, responseHash, outcomeRec, logPath, llmInfo
},

  promptTemplate = Lookup[opts, "PromptTemplate", $defaultPromptTemplate];
  saveDir        = Lookup[opts, "OutputDirectory", $Failed];
  genFn          = Lookup[opts, "Generator", $Failed];
  logPath        = Lookup[opts, "LogPath", None];
  llmInfo        = Lookup[opts, "LLMInfo", <||>];
  callOpts       = <|
    "MaxAttempts"    -> Lookup[opts, "MaxAttempts",    3],
    "TimeConstraint" -> Lookup[opts, "TimeConstraint", 120],
    "RetryBaseDelay" -> Lookup[opts, "RetryBaseDelay", 2.0],
    "RetryCapDelay"  -> Lookup[opts, "RetryCapDelay",  60.0]
  |>;

  If[saveDir === $Failed,
    Return[<|"status" -> "no-output-dir", "name" -> name|>]
  ];
  If[genFn === $Failed,
    Return[<|"status" -> "no-generator", "name" -> name|>]
  ];

  prompt = buildPrompt[promptTemplate, challengeEntry];
  If[prompt === $Failed,
    outcomeRec = <|"status" -> "no-prompt", "name" -> name|>;
    If[logPath =!= None,
      appendJSONL[logPath, Join[<|"event" -> "challenge.skipped"|>, outcomeRec]]
    ];
    Return[outcomeRec]
  ];

  promptHash = "sha256:" <> Hash[prompt, "SHA256", "HexString"];
  If[logPath =!= None,
    appendJSONL[logPath, <|
      "event"       -> "challenge.prompt",
      "name"        -> name,
      "promptHash"  -> promptHash,
      "promptBytes" -> StringLength[prompt]
    |>]
  ];

  callResult = callLLMWithRetry[genFn, prompt, callOpts];

  If[callResult["status"] =!= "ok",
    outcomeRec = <|
      "status"      -> "llm-" <> callResult["status"],
      "name"        -> name,
      "attempts"    -> callResult["attempts"],
      "durationSec" -> callResult["totalSec"]
    |>;
    If[logPath =!= None,
      appendJSONL[logPath, Join[
        <|"event" -> "challenge.failed", "promptHash" -> promptHash|>,
        outcomeRec
      ]]
    ];
    Return[outcomeRec]
  ];

  rawResponse  = callResult["response"];
  responseHash = "sha256:" <> Hash[rawResponse, "SHA256", "HexString"];
  extracted    = extractCodeImpl[rawResponse];

  If[! StringQ[extracted] || StringLength[StringTrim[extracted]] === 0,
    outcomeRec = <|
      "status"       -> "empty-extracted",
      "name"         -> name,
      "responseHash" -> responseHash,
      "attempts"     -> Length[callResult["attempts"]],
      "durationSec"  -> callResult["totalSec"]
    |>;
    If[logPath =!= None,
      appendJSONL[logPath, Join[<|"event" -> "challenge.empty"|>, outcomeRec]]
    ];
    Return[outcomeRec]
  ];

  extraMeta = <|
    "generator"       -> $generatorVersion,
    "promptHash"      -> promptHash,
    "rawResponseHash" -> responseHash,
    "attempts"        -> Length[callResult["attempts"]],
    "llm"             -> llmInfo
  |>;

  savePath = Quiet @ Check[
    saveSolutionImpl[saveDir, name, extracted, testBank, extraMeta],
    $Failed
  ];

  If[savePath === $Failed,
    outcomeRec = <|
      "status"         -> "audit-rejected",
      "name"           -> name,
      "responseHash"   -> responseHash,
      "promptHash"     -> promptHash,
      "extractedBytes" -> StringLength[extracted],
      "attempts"       -> Length[callResult["attempts"]]
    |>;
    If[logPath =!= None,
      appendJSONL[logPath, Join[
        <|"event" -> "challenge.auditRejected", "extracted" -> extracted|>,
        outcomeRec
      ]]
    ];
    Return[outcomeRec]
  ];

  (* Optionally keep the raw un-extracted response next to the .wl for
     forensic replay. Off by default to keep solutions/ tidy. *)
  If[TrueQ[Lookup[opts, "SaveRawResponse", False]],
    Module[{rawPath},
      rawPath = StringReplace[savePath, ".wl" ~~ EndOfString -> ".raw.txt"];
      Quiet @ Export[rawPath, rawResponse, "Text", CharacterEncoding -> "UTF-8"]
    ]
  ];

  outcomeRec = <|
    "status"       -> "ok",
    "name"         -> name,
    "path"         -> savePath,
    "responseHash" -> responseHash,
    "promptHash"   -> promptHash,
    "attempts"     -> Length[callResult["attempts"]],
    "durationSec"  -> callResult["totalSec"]
  |>;
  If[logPath =!= None,
    appendJSONL[logPath, Join[<|"event" -> "challenge.saved"|>, outcomeRec]]
  ];
  outcomeRec
];

processOneChallenge[name_, entry_, testBank_, opts_] := (
  logError["processOneChallenge: bad args for " <> ToString[name]];
  <|"status" -> "bad-args", "name" -> ToString[name]|>
);

(* ----------------------------------------------------------------------- *)
(* generateSolutionsImpl — top-level driver                                *)
(* ----------------------------------------------------------------------- *)

generateSolutionsImpl[
  challenges_Association, testBank_Association, opts_Association
] := Module[
  {model, outDir, filter, gen, llmInfo, mergedOpts, selected, names,
   overwrite, logPath, startStamp, results = <||>,
   counts = <|"ok" -> 0, "failed" -> 0, "auditRejected" -> 0, "other" -> 0|>,
   runId, headerRec, dryRun, total},

  model     = Lookup[opts, "Model", None];
  dryRun    = TrueQ[Lookup[opts, "DryRun", False]];
  outDir    = Lookup[opts, "OutputDirectory",
                FileNameJoin[{Directory[], "solutions",
                  If[StringQ[model], safeSlug[model], "unlabeled"]}]];
  filter    = Lookup[opts, "Filter", All];
  gen       = Lookup[opts, "Generator", Automatic];
  llmInfo   = Lookup[opts, "LLMInfo", <||>];
  overwrite = TrueQ[Lookup[opts, "Overwrite", False]];
  runId     = Lookup[opts, "RunId", newRunId[]];
  logPath   = Lookup[opts, "LogPath",
                FileNameJoin[{outDir, "generate-" <> runId <> ".jsonl"}]];

  If[gen === Automatic,
    gen = defaultLLMGenerator[Lookup[opts, "LLMEvaluator", <||>]]
  ];
  If[dryRun,
    gen = Function[{prompt},
      "(* dry-run stub — no LLM call was made. *)\n" <>
      "DryRunSolution[args___] := {\"dry-run\", Hash[args, \"SHA256\", \"HexString\"]}"
    ]
  ];

  selected = Which[
    filter === All, challenges,
    ListQ[filter], KeySelect[challenges, MemberQ[filter, #] &],
    True, challenges
  ];
  names = Keys[selected];

  If[! overwrite,
    names = Select[names,
      ! FileExistsQ[FileNameJoin[{outDir, safeSlug[#] <> ".wl"}]] &
    ]
  ];
  total = Length[names];

  If[! DirectoryQ[outDir],
    CreateDirectory[outDir, CreateIntermediateDirectories -> True]
  ];

  startStamp = DateString["ISODateTime"];
  headerRec = <|
    "event"       -> "generate.start",
    "runId"       -> runId,
    "model"       -> model,
    "outDir"      -> outDir,
    "filter"      -> If[filter === All, "all", filter],
    "toProcess"   -> total,
    "totalInBank" -> Length[challenges],
    "dryRun"      -> dryRun,
    "overwrite"   -> overwrite,
    "llm"         -> llmInfo,
    "startedAt"   -> startStamp,
    "generator"   -> $generatorVersion
  |>;
  appendJSONL[logPath, headerRec];

  mergedOpts = Join[opts, <|
    "OutputDirectory" -> outDir,
    "Generator"       -> gen,
    "LogPath"         -> logPath,
    "LLMInfo"         -> llmInfo
  |>];

  MapIndexed[
    Function[{name, idxList},
      Module[{res, idx = First[idxList]},
        logInfo[TemplateApply["[``/``] generating ``: ``",
          {idx, total, model, name}]];
        res = processOneChallenge[name, selected[name], testBank, mergedOpts];
        results[name] = res;
        Which[
          res["status"] === "ok",                     counts["ok"]++,
          res["status"] === "audit-rejected",         counts["auditRejected"]++,
          StringStartsQ[res["status"], "llm-"] ||
            res["status"] === "empty-extracted" ||
            res["status"] === "no-prompt",            counts["failed"]++,
          True,                                       counts["other"]++
        ]
      ]
    ],
    names
  ];

  appendJSONL[logPath, <|
    "event"      -> "generate.finished",
    "runId"      -> runId,
    "counts"     -> counts,
    "finishedAt" -> DateString["ISODateTime"]
  |>];

  <|
    "runId"      -> runId,
    "model"      -> model,
    "outDir"     -> outDir,
    "logPath"    -> logPath,
    "counts"     -> counts,
    "results"    -> results,
    "skipped"    -> Complement[Keys[challenges], names],
    "llm"        -> llmInfo,
    "dryRun"     -> dryRun,
    "overwrite"  -> overwrite,
    "startedAt"  -> startStamp,
    "finishedAt" -> DateString["ISODateTime"]
  |>
];

End[];
