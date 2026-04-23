(* ::Package:: *)

(* :Context: JofreEspigulePons`WolframChallengesBenchmark`Private`          *)
(* :Summary:
     Direct HTTP client for OpenRouter's Chat Completions endpoint.

     OpenRouter exposes an OpenAI-compatible REST API at
       https://openrouter.ai/api/v1/chat/completions

     The API key is read from the OPENROUTER_API_KEY environment
     variable ONLY.  No other source is consulted \[LongDash] that keeps
     secrets off disk, matches 12-factor conventions, and lets CI
     runners inject the key without ever persisting it.

     openRouterChatCompleteImpl is the single point of contact with
     the network.  It accepts an Association of options and returns
     a fully-typed result envelope regardless of success or failure,
     so callers can always match on "status" and access metadata
     without worrying about message aborts.
*)

Begin["JofreEspigulePons`WolframChallengesBenchmark`Private`"];


(* ------------------------------------------------------------------ *)
(* Constants                                                          *)
(* ------------------------------------------------------------------ *)

$openRouterEndpoint = "https://openrouter.ai/api/v1/chat/completions";

$openRouterDefaultReferer = "https://github.com/JofreEspigulePons/WolframChallengesBenchmark";
$openRouterDefaultTitle   = "WolframChallengesBenchmark";

$openRouterDefaultTimeoutSec = 120;


(* ------------------------------------------------------------------ *)
(* API-key resolution                                                 *)
(*                                                                    *)
(* resolveOpenRouterAPIKey[] returns a non-empty String on success or *)
(* $Failed if the env var is unset / blank.  Intentionally does NOT   *)
(* emit a message on failure so callers can decide whether the key   *)
(* is required in their context (e.g. dry-run mode doesn't need it). *)
(* ------------------------------------------------------------------ *)

resolveOpenRouterAPIKey[] := Module[{raw},
  raw = Quiet @ Environment["OPENROUTER_API_KEY"];
  If[StringQ[raw] && StringLength[StringTrim[raw]] > 0,
    StringTrim[raw],
    $Failed
  ]
];


(* ------------------------------------------------------------------ *)
(* Normalization helpers                                              *)
(* ------------------------------------------------------------------ *)

(* validateMessage — accept either an Association with "role"/"content"
   (the native OpenAI/OpenRouter shape) or a Rule of the same two keys
   (convenience for callers who don't want to wrap in <|..|>).        *)

normalizeMessage[m_Association] :=
  Which[
    ! KeyExistsQ[m, "role"] || ! KeyExistsQ[m, "content"], $Failed,
    ! StringQ[m["role"]]    || ! StringQ[m["content"]],    $Failed,
    True,
      <|"role" -> m["role"], "content" -> m["content"]|>
  ];

normalizeMessage[role_String -> content_String] :=
  <|"role" -> role, "content" -> content|>;

normalizeMessage[_] := $Failed;

normalizeMessages[list_List] := Module[{norm},
  norm = normalizeMessage /@ list;
  If[MemberQ[norm, $Failed], $Failed, norm]
];

normalizeMessages[_] := $Failed;


(* buildChatBody — OpenRouter accepts the standard OpenAI chat body.
   Optional fields (temperature, max_tokens, top_p, response_format,
   etc.) are passed through only when the caller sets them, so the
   request body stays minimal and the server defaults kick in.     *)

buildChatBody[messages_List, opts_Association] := Module[{body, optional},
  body = <|
    "model"    -> Lookup[opts, "Model", "openai/gpt-5"],
    "messages" -> messages
  |>;
  optional = <|
    "temperature"       -> Lookup[opts, "Temperature",      Missing[]],
    "max_tokens"        -> Lookup[opts, "MaxTokens",        Missing[]],
    "top_p"             -> Lookup[opts, "TopP",             Missing[]],
    "frequency_penalty" -> Lookup[opts, "FrequencyPenalty", Missing[]],
    "presence_penalty"  -> Lookup[opts, "PresencePenalty",  Missing[]],
    "seed"              -> Lookup[opts, "Seed",             Missing[]],
    "stop"              -> Lookup[opts, "Stop",             Missing[]],
    "response_format"   -> Lookup[opts, "ResponseFormat",   Missing[]]
  |>;
  body = Join[body, Select[optional, Not @* MissingQ]];
  body
];


(* buildHeaders — OpenRouter recommends (not requires) sending
   HTTP-Referer and X-Title so your app appears on their dashboard
   and request leaderboard.  Authorization is always required.    *)

buildHeaders[apiKey_String, opts_Association] := {
  "Authorization" -> "Bearer " <> apiKey,
  "HTTP-Referer"  -> Lookup[opts, "Referer", $openRouterDefaultReferer],
  "X-Title"       -> Lookup[opts, "Title",   $openRouterDefaultTitle],
  "Content-Type"  -> "application/json",
  (* Be explicit that we want a single JSON object, not SSE / chunked.
     OpenRouter still sometimes injects whitespace heartbeats on slow
     upstreams (see decodeOpenRouterBody below), but asking for JSON
     reduces the surface area on which we have to be defensive.       *)
  "Accept"        -> "application/json"
};


(* ------------------------------------------------------------------ *)
(* Body decoding                                                      *)
(*                                                                    *)
(* On long-running non-streaming requests against slow upstream       *)
(* models (notably minimax/minimax-m2.7 reasoning), OpenRouter keeps  *)
(* the HTTP connection alive by *prepending* whitespace bytes to the  *)
(* response body before the actual JSON object lands.  We've seen     *)
(* dumps with 6+ KB of leading 0x20 followed by a clean JSON payload. *)
(* Plain ImportString[..., "RawJSON"] rejects that as malformed.      *)
(*                                                                    *)
(* decodeOpenRouterBody is a small, conservative recovery layer:      *)
(*   1. Try parsing the body as-is (the happy path; cheap).           *)
(*   2. On failure, retry after StringTrim (handles the keep-alive    *)
(*      whitespace prefix observed in the wild).                      *)
(*                                                                    *)
(* Returns the decoded Association on success, $Failed on any other   *)
(* shape.  Never throws.  If we see additional body pathologies in    *)
(* the future (SSE streaming, multi-frame envelopes, junk preamble),  *)
(* add a new explicit case here rather than a speculative heuristic   *)
(* like "find the last {", which gives wrong answers on nested JSON.  *)
(* ------------------------------------------------------------------ *)

decodeOpenRouterBody[body_String] := Module[{trimmed, parsed},
  parsed = Quiet @ Check[ImportString[body, "RawJSON"], $Failed];
  If[AssociationQ[parsed], Return[parsed]];

  trimmed = StringTrim[body];
  If[trimmed =!= body && StringLength[trimmed] > 0,
    parsed = Quiet @ Check[ImportString[trimmed, "RawJSON"], $Failed];
    If[AssociationQ[parsed], Return[parsed]]
  ];

  $Failed
];

decodeOpenRouterBody[other_] := Quiet @ Check[
  ImportString[ToString[other, InputForm], "RawJSON"],
  $Failed
];


(* ------------------------------------------------------------------ *)
(* Response parsing                                                   *)
(*                                                                    *)
(* We treat every non-200 as an error but always surface the raw      *)
(* response body (when decodable) so callers can see the provider's   *)
(* error message instead of a generic "request failed".               *)
(* ------------------------------------------------------------------ *)

(* extractContentString — robust against the three shapes OpenRouter
   surfaces in the wild:

     1. Plain string   "content": "..."
                       \[LongDash] the OpenAI-classic happy path.
     2. Content parts  "content": [{"type": "text", "text": "..."}, ...]
                       \[LongDash] vision / multimodal / Anthropic-via-router.
     3. Empty content  "content": "" or null, with the actual text living in
                       "reasoning_content" or "reasoning"
                       \[LongDash] reasoning models (o1, deepseek-r1,
                       minimax-m2 thinking, qwen-qwq, etc.). The router
                       splits chain-of-thought from final answer.

   Returns a record telling the caller WHICH shape it found, so meta.json
   can record it and an audit-level warning can fire when we had to fall
   back to chain-of-thought as the answer.                                *)

extractContentString[msg_Association] := Module[
  {raw, parts, partTexts, joined, reasoning, reasoningContent},

  raw = Lookup[msg, "content", None];

  (* Shape 1: plain string. *)
  If[StringQ[raw] && StringLength[StringTrim[raw]] > 0,
    Return[<|
      "text"   -> raw,
      "source" -> "content",
      "ok"     -> True
    |>]
  ];

  (* Shape 2: content parts list. *)
  If[ListQ[raw],
    parts     = raw;
    partTexts = Cases[parts,
      a_Association /; KeyExistsQ[a, "text"] && StringQ[a["text"]] :> a["text"]];
    joined = StringJoin[partTexts];
    If[StringLength[StringTrim[joined]] > 0,
      Return[<|
        "text"   -> joined,
        "source" -> "content-parts",
        "ok"     -> True
      |>]
    ]
  ];

  (* Shape 3: reasoning models \[LongDash] try reasoning_content then reasoning. *)
  reasoningContent = Lookup[msg, "reasoning_content", None];
  If[StringQ[reasoningContent] && StringLength[StringTrim[reasoningContent]] > 0,
    Return[<|
      "text"   -> reasoningContent,
      "source" -> "reasoning_content",
      "ok"     -> True
    |>]
  ];

  reasoning = Lookup[msg, "reasoning", None];
  If[StringQ[reasoning] && StringLength[StringTrim[reasoning]] > 0,
    Return[<|
      "text"   -> reasoning,
      "source" -> "reasoning",
      "ok"     -> True
    |>]
  ];

  (* Nothing extractable. *)
  <|
    "text"   -> None,
    "source" -> None,
    "ok"     -> False
  |>
];

extractContentString[_] := <|"text" -> None, "source" -> None, "ok" -> False|>;


parseOpenRouterSuccess[resp_Association] := Module[
  {choices, firstChoice, msg, finishReason, usage, id, extracted,
   msgKeys},

  choices = Lookup[resp, "choices", {}];
  If[! ListQ[choices] || Length[choices] === 0,
    Return[<|
      "status"        -> "malformed",
      "content"       -> None,
      "contentSource" -> None,
      "error"         -> "no choices in response",
      "rawResponse"   -> resp
    |>]
  ];

  firstChoice  = First[choices];
  msg          = Lookup[firstChoice, "message", <||>];
  finishReason = Lookup[firstChoice, "finish_reason", None];
  usage        = Lookup[resp, "usage", <||>];
  id           = Lookup[resp, "id", None];

  extracted = extractContentString[msg];

  msgKeys = If[AssociationQ[msg], Keys[msg], {}];

  If[! TrueQ[extracted["ok"]],
    Return[<|
      "status"        -> "malformed",
      "content"       -> None,
      "contentSource" -> None,
      "error"         -> StringJoin[
        "no extractable content in choices[0].message ",
        "(finish_reason=", ToString[finishReason, InputForm],
        ", message keys=", ToString[msgKeys, InputForm], ")"],
      "finishReason"  -> finishReason,
      "usage"         -> <|
         "promptTokens"     -> Lookup[usage, "prompt_tokens",     None],
         "completionTokens" -> Lookup[usage, "completion_tokens", None],
         "totalTokens"      -> Lookup[usage, "total_tokens",      None]
       |>,
      "generationId"  -> id,
      "rawResponse"   -> resp
    |>]
  ];

  <|
    "status"        -> "ok",
    "content"       -> extracted["text"],
    "contentSource" -> extracted["source"],
    "finishReason"  -> finishReason,
    "usage"         -> <|
       "promptTokens"     -> Lookup[usage, "prompt_tokens",     None],
       "completionTokens" -> Lookup[usage, "completion_tokens", None],
       "totalTokens"      -> Lookup[usage, "total_tokens",      None]
     |>,
    "generationId"  -> id,
    "rawResponse"   -> resp
  |>
];


(* ------------------------------------------------------------------ *)
(* Main entry                                                         *)
(* ------------------------------------------------------------------ *)

Options[openRouterChatCompleteImpl] = {};

JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete::apikey =
"OPENROUTER_API_KEY is not set; refusing to issue a request.";

JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete::badmsgs =
"Messages must be a non-empty list of <|\"role\" -> _, \"content\" -> _|> associations.";


openRouterChatCompleteImpl[messages_List, opts_Association] := Module[
  {apiKey, norm, body, headers, req, timeout, t0, resp, dt, httpCode, decoded,
   parsed, commonMeta},

  norm = normalizeMessages[messages];
  If[norm === $Failed || Length[norm] === 0,
    Message[
      JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete::badmsgs
    ];
    Return[<|
      "status"      -> "bad-messages",
      "error"       -> "bad messages",
      "content"     -> None,
      "httpStatus"  -> None,
      "latencySec"  -> 0.0,
      "usage"       -> <||>,
      "generationId"-> None,
      "finishReason"-> None,
      "rawResponse" -> None
    |>]
  ];

  apiKey = resolveOpenRouterAPIKey[];
  If[apiKey === $Failed,
    Message[
      JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete::apikey
    ];
    Return[<|
      "status"      -> "no-api-key",
      "error"       -> "OPENROUTER_API_KEY not set",
      "content"     -> None,
      "httpStatus"  -> None,
      "latencySec"  -> 0.0,
      "usage"       -> <||>,
      "generationId"-> None,
      "finishReason"-> None,
      "rawResponse" -> None
    |>]
  ];

  body    = buildChatBody[norm, opts];
  headers = buildHeaders[apiKey, opts];
  timeout = Lookup[opts, "TimeConstraint", $openRouterDefaultTimeoutSec];

  req = HTTPRequest[$openRouterEndpoint, <|
    "Method"  -> "POST",
    "Headers" -> headers,
    "Body"    -> ExportString[body, "RawJSON"],
    "ContentType" -> "application/json"
  |>];

  t0   = AbsoluteTime[];
  (* URLRead can throw an internal Abort[] when the TCP connection flakes
     (observed on macOS during "Connecting..." phase, 3-5s in).  A raw
     Abort bypasses TimeConstrained's failure-expression form and leaks
     upward to whatever CheckAbort is ambient, terminating the whole
     generation run even though only this one HTTP call is broken.  Wrap
     URLRead in CheckAbort so connection-level aborts are caught here and
     reported as a normal network-error outcome that the retry layer can
     handle, instead of aborting the outer loop.                         *)
  resp = Quiet @ Check[
    CheckAbort[
      TimeConstrained[URLRead[req], timeout, $TimedOut],
      $Aborted
    ],
    $Failed
  ];
  dt = AbsoluteTime[] - t0;

  commonMeta = <|"latencySec" -> dt|>;

  Which[
    resp === $TimedOut,
      Return[Join[commonMeta, <|
        "status"      -> "timeout",
        "error"       -> TemplateApply["request exceeded `` seconds", {timeout}],
        "content"     -> None,
        "httpStatus"  -> None,
        "usage"       -> <||>,
        "generationId"-> None,
        "finishReason"-> None,
        "rawResponse" -> None
      |>]],

    resp === $Aborted,
      Return[Join[commonMeta, <|
        "status"      -> "connection-aborted",
        "error"       -> TemplateApply[
          "URLRead aborted after `` seconds (TCP flake); retryable",
          {NumberForm[dt, {Infinity, 2}]}],
        "content"     -> None,
        "httpStatus"  -> None,
        "usage"       -> <||>,
        "generationId"-> None,
        "finishReason"-> None,
        "rawResponse" -> None
      |>]],

    resp === $Failed || ! MatchQ[resp, _HTTPResponse],
      Return[Join[commonMeta, <|
        "status"      -> "network-error",
        "error"       -> "URLRead returned $Failed",
        "content"     -> None,
        "httpStatus"  -> None,
        "usage"       -> <||>,
        "generationId"-> None,
        "finishReason"-> None,
        "rawResponse" -> None
      |>]]
  ];

  httpCode = resp["StatusCode"];
  decoded  = decodeOpenRouterBody[resp["Body"]];

  (* Forensic dump on any non-2xx or non-JSON response: write the raw
     body to $TemporaryDirectory immediately so we preserve it even if
     the caller never sees the return value (e.g. kernel segfaults during
     retry unwinding).  The path is included in the return Association so
     the challenge.failed JSONL row can reference it.                    *)

  If[(! IntegerQ[httpCode] || httpCode < 200 || httpCode >= 300) ||
     ! AssociationQ[decoded],
    Module[{forensicPath, dumpBody},
      dumpBody = If[StringQ[resp["Body"]], resp["Body"],
                    ToString[resp["Body"], InputForm]];
      forensicPath = FileNameJoin[{$TemporaryDirectory,
        "openrouter-badresponse-" <>
        DateString[{"ISODate", "_", "Hour24", "Minute", "Second"}] <>
        "-" <> IntegerString[RandomInteger[{16^5, 16^6 - 1}], 16] <>
        ".txt"}];
      Quiet @ Export[forensicPath, dumpBody, "Text",
        CharacterEncoding -> "UTF-8"];
      If[! IntegerQ[httpCode] || httpCode < 200 || httpCode >= 300,
        Return[Join[commonMeta, <|
          "status"           -> "http-error",
          "error"            -> TemplateApply["HTTP `` from OpenRouter", {httpCode}],
          "httpStatus"       -> httpCode,
          "content"          -> None,
          "usage"            -> <||>,
          "generationId"     -> If[AssociationQ[decoded], Lookup[decoded, "id", None], None],
          "finishReason"     -> None,
          "rawResponse"      -> If[AssociationQ[decoded], decoded, resp["Body"]],
          "forensicDumpPath" -> forensicPath
        |>]]
      ];
      (* ! AssociationQ[decoded] \[LongDash] malformed JSON *)
      Return[Join[commonMeta, <|
        "status"           -> "malformed",
        "error"            -> "response body was not valid JSON",
        "httpStatus"       -> httpCode,
        "content"          -> None,
        "usage"            -> <||>,
        "generationId"     -> None,
        "finishReason"     -> None,
        "rawResponse"      -> resp["Body"],
        "forensicDumpPath" -> forensicPath
      |>]]
    ]
  ];

  parsed = parseOpenRouterSuccess[decoded];

  (* Belt-and-suspenders forensic dump for the post-parse malformed path:
     HTTP 200 + valid JSON + choices, but content was missing / empty /
     an unexpected shape (classic reasoning-model finish_reason="length"
     with empty content).  The earlier dump block above only fires on
     non-2xx or non-JSON, so without this we'd silently lose the parsed
     body the user needs to diagnose "why did this fail".                *)
  If[parsed["status"] =!= "ok",
    Module[{forensicPath, dumpBody},
      dumpBody = Quiet @ Check[
        ExportString[decoded, "RawJSON", "Compact" -> False],
        ToString[decoded, InputForm]];
      If[! StringQ[dumpBody], dumpBody = ToString[decoded, InputForm]];
      forensicPath = FileNameJoin[{$TemporaryDirectory,
        "openrouter-badresponse-" <>
        DateString[{"ISODate", "_", "Hour24", "Minute", "Second"}] <>
        "-" <> IntegerString[RandomInteger[{16^5, 16^6 - 1}], 16] <>
        ".json"}];
      Quiet @ Export[forensicPath, dumpBody, "Text",
        CharacterEncoding -> "UTF-8"];
      parsed = Append[parsed, "forensicDumpPath" -> forensicPath]
    ]
  ];

  Join[commonMeta, <|"httpStatus" -> httpCode|>, parsed]
];


(* Pattern-guard: anything that didn't match the main signature falls
   through to here and returns a well-typed error envelope instead of
   leaving unevaluated symbolic garbage in the caller's code.         *)
openRouterChatCompleteImpl[args___] := <|
  "status"      -> "error",
  "error"       -> "openRouterChatCompleteImpl called with bad arguments",
  "content"     -> None,
  "httpStatus"  -> None,
  "latencySec"  -> 0.0,
  "usage"       -> <||>,
  "generationId"-> None,
  "finishReason"-> None,
  "rawResponse" -> None
|>;


End[];  (* `Private` *)
