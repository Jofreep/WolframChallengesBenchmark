(* ::Package:: *)

(* :Context: JofreEspigulePons`WolframChallengesBenchmark`Private`          *)
(* :Summary:
     LLM-backed solution generator, built directly on top of
     OpenRouterChatComplete (no LLMSynthesize dependency).

     Flow per challenge:
         prompt   \[Rule] callLLMWithRetry   (TimeConstrained + backoff)
                   \[Rule] extractCode       (markdown-fence stripped)
                   \[Rule] saveSolutionImpl  (test-bank audit refuses bad code)
                   \[Rule] JSONL audit log   (every attempt)

     The generator function is DI-injectable: tests pass a deterministic
     stub, production uses a closure around openRouterChatCompleteImpl.
     This keeps the module unit-testable without network access and
     leaves the door open to swap in other back-ends later.

     Per-solution meta.json is enriched with OpenRouter-specific fields
     (tokenUsage, generationId, finishReason, httpStatus, latencySec)
     on top of the defaults written by saveSolutionImpl.
*)

Begin["JofreEspigulePons`WolframChallengesBenchmark`Private`"];


(* ------------------------------------------------------------------ *)
(* Defaults                                                           *)
(* ------------------------------------------------------------------ *)

$generatorVersion = "GenerateSolutions/v2-openrouter";

(* Prompt is authored in a single place and carries a {{CHALLENGE}}
   substitution marker. Overridable via the "PromptTemplate" option.   *)

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


(* ------------------------------------------------------------------ *)
(* Prompt assembly                                                    *)
(* ------------------------------------------------------------------ *)

buildPrompt[template_String, challengeEntry_Association] := Module[{body},
  body = Lookup[challengeEntry, "prompt", ""];
  If[! StringQ[body] || StringLength[body] === 0, Return[$Failed]];
  StringReplace[template, "{{CHALLENGE}}" -> body]
];

buildPrompt[___] := $Failed;


(* ------------------------------------------------------------------ *)
(* Default OpenRouter-backed generator                                *)
(*                                                                    *)
(* Returns a Function that, given a prompt String, returns an         *)
(* Association with the fields callLLMWithRetry needs:                *)
(*   "ok" (Bool), "content" (String), plus rich metadata for logging. *)
(* ------------------------------------------------------------------ *)

defaultOpenRouterGenerator[llmOpts_Association] := Function[{prompt},
  Module[{resp},
    If[! StringQ[prompt],
      Return[<|
        "ok" -> False, "content" -> None,
        "error" -> "prompt not a string"
      |>]
    ];
    resp = openRouterChatCompleteImpl[
      {<|"role" -> "user", "content" -> prompt|>},
      llmOpts
    ];
    <|
      "ok"               -> (resp["status"] === "ok"),
      "content"          -> resp["content"],
      "status"           -> resp["status"],
      "error"            -> Lookup[resp, "error", None],
      "httpStatus"       -> Lookup[resp, "httpStatus", None],
      "usage"            -> Lookup[resp, "usage", <||>],
      "generationId"     -> Lookup[resp, "generationId", None],
      "finishReason"     -> Lookup[resp, "finishReason", None],
      "latencySec"       -> Lookup[resp, "latencySec", 0.0],
      (* contentSource records WHICH shape we extracted from:
         "content" | "content-parts" | "reasoning_content" | "reasoning"
         \[LongDash] meta.json carries this so audits can flag when an
         answer was rescued from chain-of-thought.                       *)
      "contentSource"    -> Lookup[resp, "contentSource", None],
      (* Surface the raw (un-parseable / non-2xx) body so the caller can
         persist it for forensic replay.  On ok status this is typically
         None \[LongDash] we only need it for failure diagnosis.          *)
      "rawResponse"      -> Lookup[resp, "rawResponse", None],
      (* Path to the on-disk forensic dump written by the OpenRouter
         client itself, when present.  Lets challenge.failed JSONL rows
         point at the exact bytes the provider sent.                    *)
      "forensicDumpPath" -> Lookup[resp, "forensicDumpPath", None]
    |>
  ]
];


(* dryRunGenerator — emits a stub that will be REJECTED by the audit
   (it doesn't define the expected function), so a dry-run clearly
   distinguishes "no LLM called" from "LLM produced bad code". The
   stub is still deterministic on input for reproducibility.         *)

dryRunGenerator[] := Function[{prompt},
  <|
    "ok"           -> True,
    "content"      -> "(* dry-run stub \[LongDash] no LLM call was made. *)\n" <>
                      "DryRunSolution[args___] := {\"dry-run\", Hash[args, \"SHA256\", \"HexString\"]}",
    "status"       -> "ok",
    "error"        -> None,
    "httpStatus"   -> None,
    "usage"        -> <||>,
    "generationId" -> "dry-run",
    "finishReason" -> "stop",
    "latencySec"   -> 0.0
  |>
];


(* ------------------------------------------------------------------ *)
(* callLLMWithRetry                                                   *)
(*                                                                    *)
(* TimeConstrained envelope around the injected generator, with       *)
(* bounded exponential backoff between attempts. Returns:             *)
(*   <| "status"       -> "ok" | "timeout" | "failed",                *)
(*      "response"     -> Association (last attempt's gen result)     *)
(*                        or None on hard failure,                    *)
(*      "attempts"     -> { <|"attempt", "durationSec", "status",     *)
(*                              "error", "httpStatus"|>, ... },       *)
(*      "totalSec"     -> Real,                                       *)
(*      "httpStatuses" -> {Integer...}                                *)
(*   |>                                                                *)
(* ------------------------------------------------------------------ *)

(* AttemptCallback gets {phase, payload}: phase \[Element] {"start","end"}
   payload is an Association you can extend before flushing.  The callback
   is invoked *synchronously* around every attempt so a tail-er sees a
   start record before each blocking gen[prompt] call \[LongDash] that's
   how we tell "slow but alive" from "dead".                              *)

callLLMWithRetry[gen_, prompt_String, opts_Association] := Module[
  {maxAttempts, timeoutSec, baseDelay, cap, startAll, attemptRecs = {},
   attempt = 0, response = None, t0, dt, delay, attemptStatus,
   finalStatus = "failed", httpStatuses = {}, attemptCb},

  maxAttempts = Max[1, ToExpression[ToString[Lookup[opts, "MaxAttempts", 3]]]];
  timeoutSec  = Max[1, Lookup[opts, "TimeConstraint", 120]];
  baseDelay   = Max[0.0, Lookup[opts, "RetryBaseDelay", 2.0]];
  cap         = Lookup[opts, "RetryCapDelay", 60.0];
  attemptCb   = Lookup[opts, "AttemptCallback", None];

  startAll = AbsoluteTime[];
  While[attempt < maxAttempts,
    attempt++;
    t0 = AbsoluteTime[];

    (* Heartbeat: emitted BEFORE we block on gen[prompt] so a tail-er
       sees the attempt is live even when minimax takes 4 minutes. *)
    If[attemptCb =!= None,
      Quiet @ attemptCb["start", <|
        "attempt"        -> attempt,
        "maxAttempts"    -> maxAttempts,
        "timeoutSec"     -> timeoutSec,
        "startedAtAbs"   -> t0,
        "startedAtUTC"   -> isoUTC[]
      |>]
    ];

    (* Belt-and-suspenders against Abort[] escaping the OpenRouter client:
       wrap in CheckAbort so a URLRead abort (observed on macOS during
       TCP flakes) becomes a catchable $Aborted value here and the retry
       loop can try again.  Without this, one flaky HTTP call aborts the
       whole multi-challenge run even though the per-attempt envelope
       already has a TimeConstrained.                                      *)
    response = Quiet @ Check[
      CheckAbort[
        TimeConstrained[gen[prompt], timeoutSec, $TimedOut],
        $Aborted
      ],
      $Failed
    ];
    dt = AbsoluteTime[] - t0;

    attemptStatus = Which[
      response === $TimedOut, "timeout",
      response === $Aborted,  "aborted",
      response === $Failed,   "failed",
      AssociationQ[response] && TrueQ[response["ok"]] &&
        StringQ[response["content"]],
        "ok",
      AssociationQ[response] && Lookup[response, "status", ""] === "timeout",
        "timeout",
      AssociationQ[response] &&
        Lookup[response, "status", ""] === "connection-aborted",
        "aborted",
      True, "failed"
    ];

    AppendTo[attemptRecs, <|
      "attempt"     -> attempt,
      "durationSec" -> dt,
      "status"      -> attemptStatus,
      "error"       -> If[AssociationQ[response],
                          Lookup[response, "error", None], None],
      "httpStatus"  -> If[AssociationQ[response],
                          Lookup[response, "httpStatus", None], None]
    |>];
    If[AssociationQ[response] && IntegerQ[Lookup[response, "httpStatus", None]],
      AppendTo[httpStatuses, response["httpStatus"]]
    ];

    If[attemptCb =!= None,
      Quiet @ attemptCb["end", <|
        "attempt"     -> attempt,
        (* N@ strips arbitrary-precision markers so the JSON encoder
           doesn't choke on "0.000693`3.29..." style output.           *)
        "durationSec" -> N[dt],
        "status"      -> attemptStatus,
        "httpStatus"  -> If[AssociationQ[response],
                            Lookup[response, "httpStatus", None], None],
        "finishedAtUTC" -> isoUTC[]
      |>]
    ];

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
    "status"       -> finalStatus,
    "response"     -> If[AssociationQ[response], response, None],
    "attempts"     -> attemptRecs,
    "totalSec"     -> AbsoluteTime[] - startAll,
    "httpStatuses" -> httpStatuses
  |>
];


(* ------------------------------------------------------------------ *)
(* processOneChallenge                                                *)
(* ------------------------------------------------------------------ *)

processOneChallenge[name_String, challengeEntry_Association, testBank_,
    opts_Association] :=
Module[{
   promptTemplate, prompt, genFn, callOpts, callResult, genResp,
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
    "RetryCapDelay"  -> Lookup[opts, "RetryCapDelay",  60.0],
    (* Heartbeat: flushes a JSONL row before/after each LLM attempt so
       tail -f shows whether a slow run is alive or dead.              *)
    "AttemptCallback" -> If[logPath === None, None,
      Function[{phase, payload},
        appendJSONL[logPath, Join[
          <|"event" -> "challenge.attempt." <> phase, "name" -> name|>,
          payload
        ]]
      ]]
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
      appendJSONL[logPath,
        Join[<|"event" -> "challenge.skipped"|>, outcomeRec]]
    ];
    Return[outcomeRec]
  ];

  promptHash = sha256Hex[prompt];
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
    (* Forensic replay: preserve the un-parseable / error-path body so we
       can see what the provider actually returned.  OpenRouter surfaces
       this as response["rawResponse"] on "malformed" / "http-error" /
       anything where JSON parse failed.  Writing it unconditionally on
       failure \[LongDash] rather than gating on SaveRawResponse \[LongDash]
       because the whole point of failure diagnosis is that we don't know
       in advance which runs will need it.                                *)
    Module[{rawBody, failedRawPath, lastResp, forensicDumpPath,
            lastError, lastFinishReason, lastContentSource},
      lastResp = callResult["response"];
      (* Pull every diagnostic we have so the challenge.failed row is
         self-contained \[LongDash] operators shouldn't need to re-open the
         original HTTP response to know why a call failed.                *)
      lastError         = If[AssociationQ[lastResp],
                             Lookup[lastResp, "error", None], None];
      lastFinishReason  = If[AssociationQ[lastResp],
                             Lookup[lastResp, "finishReason", None], None];
      lastContentSource = If[AssociationQ[lastResp],
                             Lookup[lastResp, "contentSource", None], None];
      forensicDumpPath  = If[AssociationQ[lastResp],
                             Lookup[lastResp, "forensicDumpPath", None], None];

      If[lastError =!= None,        outcomeRec = Append[outcomeRec, "lastError"        -> lastError]];
      If[lastFinishReason =!= None, outcomeRec = Append[outcomeRec, "lastFinishReason" -> lastFinishReason]];
      If[lastContentSource =!= None, outcomeRec = Append[outcomeRec, "lastContentSource" -> lastContentSource]];
      If[StringQ[forensicDumpPath],
        outcomeRec = Append[outcomeRec, "forensicDumpPath" -> forensicDumpPath]
      ];

      rawBody = If[AssociationQ[lastResp],
        SelectFirst[
          {Lookup[lastResp, "rawResponse", None],
           Lookup[lastResp, "content", None]},
          StringQ[#] && StringLength[#] > 0 &,
          None],
        None];
      If[StringQ[rawBody] && DirectoryQ[saveDir],
        failedRawPath = FileNameJoin[{saveDir,
          safeSlug[name] <> ".failed.raw.txt"}];
        Quiet @ Export[failedRawPath, rawBody, "Text",
          CharacterEncoding -> "UTF-8"];
        outcomeRec = Append[outcomeRec, "rawResponsePath" -> failedRawPath]
      ]
    ];
    If[logPath =!= None,
      appendJSONL[logPath, Join[
        <|"event" -> "challenge.failed", "promptHash" -> promptHash|>,
        outcomeRec
      ]]
    ];
    Return[outcomeRec]
  ];

  genResp      = callResult["response"];
  rawResponse  = genResp["content"];
  responseHash = sha256Hex[rawResponse];
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
      appendJSONL[logPath,
        Join[<|"event" -> "challenge.empty"|>, outcomeRec]]
    ];
    Return[outcomeRec]
  ];

  (* Enriched metadata: LLM info + per-call stats + OpenRouter fields. *)
  extraMeta = <|
    "generator"       -> $generatorVersion,
    "promptHash"      -> promptHash,
    "rawResponseHash" -> responseHash,
    "attempts"        -> Length[callResult["attempts"]],
    "llm"             -> llmInfo,
    "tokenUsage"      -> Lookup[genResp, "usage", <||>],
    "generationId"    -> Lookup[genResp, "generationId", None],
    "finishReason"    -> Lookup[genResp, "finishReason", None],
    "latencySec"      -> Lookup[genResp, "latencySec", callResult["totalSec"]],
    "httpStatus"      -> Lookup[genResp, "httpStatus", None],
    (* "content" | "content-parts" | "reasoning_content" | "reasoning" \[LongDash]
       cross-audit teams need this to flag answers rescued from CoT.     *)
    "contentSource"   -> Lookup[genResp, "contentSource", None]
  |>;

  (* Call saveSolutionImpl directly: it already returns $Failed on audit
     refusal.  Do NOT wrap with Check — Check would mis-catch unrelated
     messages (e.g. Symbol::undefined2 from upstream held inspection)
     and falsely flag a successful save as audit-rejected.              *)
  savePath = saveSolutionImpl[saveDir, name, extracted, testBank, extraMeta];

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

  (* Optional forensic replay file for raw, un-extracted response. *)
  If[TrueQ[Lookup[opts, "SaveRawResponse", False]],
    Module[{rawPath},
      rawPath = StringReplace[savePath,
        ".wl" ~~ EndOfString -> ".raw.txt"];
      Quiet @ Export[rawPath, rawResponse, "Text",
        CharacterEncoding -> "UTF-8"]
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
    appendJSONL[logPath,
      Join[<|"event" -> "challenge.saved"|>, outcomeRec]]
  ];
  outcomeRec
];

processOneChallenge[name_, _, _, _] := (
  logError["processOneChallenge: bad args for " <> ToString[name]];
  <|"status" -> "bad-args", "name" -> ToString[name]|>
);


(* ------------------------------------------------------------------ *)
(* generateSolutionsImpl \[Dash] top-level driver                             *)
(* ------------------------------------------------------------------ *)

generateSolutionsImpl[
  challenges_Association, testBank_Association, opts_Association
] := Module[
  {model, outDir, filter, gen, llmInfo, mergedOpts, selected, names,
   overwrite, logPath, startStamp, results = <||>,
   counts = <|"ok" -> 0, "failed" -> 0, "auditRejected" -> 0, "other" -> 0|>,
   runId, headerRec, dryRun, total, llmOpts},

  model     = Lookup[opts, "Model", None];
  dryRun    = TrueQ[Lookup[opts, "DryRun", False]];
  outDir    = Lookup[opts, "OutputDirectory",
                FileNameJoin[{Directory[], "solutions",
                  If[StringQ[model], safeSlug[model], "unlabeled"]}]];
  filter    = Lookup[opts, "Filter", All];
  gen       = Lookup[opts, "Generator", Automatic];
  llmInfo   = Lookup[opts, "LLMInfo", <||>];
  llmOpts   = Lookup[opts, "LLMOptions", <||>];
  overwrite = TrueQ[Lookup[opts, "Overwrite", False]];
  runId     = Lookup[opts, "RunId", newRunId[]];
  logPath   = Lookup[opts, "LogPath",
                FileNameJoin[{outDir, "generate-" <> runId <> ".jsonl"}]];

  (* Resolve the generator: tests can inject their own; otherwise
     wire up the real OpenRouter back-end (or the dry-run stub). *)
  If[gen === Automatic,
    gen = If[dryRun,
      dryRunGenerator[],
      defaultOpenRouterGenerator[llmOpts]
    ]
  ];

  (* When Filter is a list, honor the caller's order \[LongDash] not the
     order keys happen to appear in the underlying Assoc.  KeySelect /
     KeyTake both preserve the source Assoc's order, which surprises
     CLI users who expect "--filter A,B,C" to run in that sequence.   *)
  selected = Which[
    filter === All, challenges,
    ListQ[filter],
      AssociationMap[challenges,
        Select[filter, StringQ[#] && KeyExistsQ[challenges, #] &]],
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

  startStamp = isoUTC[];
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
    "llmOptions"  -> llmOpts,
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

  (* Tombstone tracking: completedNames lets the cleanup handler tell
     which challenge was in flight if the loop is aborted.  finishedFlag
     flips to True on a normal end-of-loop fall-through.                *)
  Module[{completedNames = {}, lastName = None, finishedFlag = False},

    (* Internal`WithLocalSettings is WL's idiomatic try/finally: the
       cleanup body runs even on Abort, Throw, or an Exit triggered by
       a containing handler.  SIGKILL still cannot be intercepted, but
       Ctrl-C / Throw / explicit Exit[] now leave a tombstone.          *)
    Internal`WithLocalSettings[
      Null,
      ( (* --- protected body --- *)
        MapIndexed[
          Function[{name, idxList},
            Module[{res, idx = First[idxList]},
              lastName = name;
              logInfo[TemplateApply["[``/``] generating ``: ``",
                {idx, total, model, name}]];
              res = processOneChallenge[name, selected[name], testBank, mergedOpts];
              results[name] = res;
              Which[
                res["status"] === "ok",             counts["ok"]++,
                res["status"] === "audit-rejected", counts["auditRejected"]++,
                StringStartsQ[res["status"], "llm-"] ||
                  res["status"] === "empty-extracted" ||
                  res["status"] === "no-prompt",    counts["failed"]++,
                True,                               counts["other"]++
              ];
              AppendTo[completedNames, name]
            ]
          ],
          names
        ];
        finishedFlag = True ),
      (* --- cleanup body, always runs --- *)
      (* Why the hand-rolled encoder:  under an ambient Abort marker (the
         reason we're in cleanup in the first place), `ExportString[...,
         "RawJSON"]` observably returns Null, even inside `AbortProtect`.
         `OpenAppend`, `WriteString`, `Close`, `StringJoin`, and
         `IntegerString` all remain usable, so we compose a JSONL line
         directly and write it through one of those that still works.
         `AbortProtect` defers any NEW aborts fired during the write, but
         the existing marker still propagates after we return \[LongDash]
         so outer `CheckAbort` handlers (tests, the CLI) still see the
         abort.                                                          *)
      AbortProtect @ Module[{tombstoneLine, finishedAtStr},
        finishedAtStr = Quiet @ Check[isoUTC[], "unknown"];
        If[! StringQ[finishedAtStr], finishedAtStr = "unknown"];
        tombstoneLine = encodeTombstoneLine[
          If[finishedFlag, "generate.finished", "generate.aborted"],
          runId,
          counts,
          Length[completedNames],
          total,
          If[StringQ[lastName], lastName, None],
          finishedAtStr
        ];
        writeTombstone[logPath, tombstoneLine]
      ]
    ]
  ];

  <|
    "runId"      -> runId,
    "model"      -> model,
    "outDir"     -> outDir,
    "logPath"    -> logPath,
    "counts"     -> counts,
    "results"    -> results,
    "skipped"    -> Complement[Keys[challenges], names],
    "llm"        -> llmInfo,
    "llmOptions" -> llmOpts,
    "dryRun"     -> dryRun,
    "overwrite"  -> overwrite,
    "startedAt"  -> startStamp,
    "finishedAt" -> isoUTC[]
  |>
];

generateSolutionsImpl[___] := $Failed;


End[];  (* `Private` *)
