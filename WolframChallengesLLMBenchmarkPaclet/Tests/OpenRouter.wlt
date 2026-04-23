(* ::Package:: *)

(* Tests for OpenRouter client.  These run offline by (a) clearing the
   API key env var and asserting the public contract, and (b) exercising
   malformed-input guards on OpenRouterChatComplete.  A live HTTP test
   is intentionally excluded from the default suite \[LongDash] run
   Tests/Live.wlt separately with OPENROUTER_API_KEY set if you want
   end-to-end coverage. *)

Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

(* Wolfram 15.0's VerificationTest silently drops ActualMessages, so
   we capture $MessageList ourselves. $MessageList is Protected. *)
ClearAll[captureMsgs, hasMessageQ];
SetAttributes[captureMsgs, HoldFirst];
captureMsgs[expr_] := Module[{v, msgs},
  Unprotect[$MessageList];
  Block[{$MessageList = {}}, v = expr; msgs = $MessageList];
  Protect[$MessageList];
  {v, msgs}];
SetAttributes[hasMessageQ, HoldRest];
hasMessageQ[msgs_List, msgNameExpr_] := MemberQ[msgs, HoldForm[msgNameExpr]];


(* withApiKey[newVal, body] sets OPENROUTER_API_KEY for the duration of body
   and ALWAYS restores it (even if body throws), so the kernel env doesn't
   leak between tests or sessions.  newVal is None to unset, or a String. *)
ClearAll[withApiKey];
SetAttributes[withApiKey, HoldRest];
withApiKey[newVal_, body_] := Module[{prev, result},
  prev = Environment["OPENROUTER_API_KEY"];
  SetEnvironment["OPENROUTER_API_KEY" -> newVal];
  result = CheckAbort[body, $Aborted];
  SetEnvironment["OPENROUTER_API_KEY" -> If[StringQ[prev], prev, None]];
  result
];


VerificationTest[
  withApiKey[None,
    Module[{v, msgs},
      {v, msgs} = captureMsgs @
        JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete[
          {<|"role" -> "user", "content" -> "hi"|>},
          <|"Model" -> "anthropic/claude-opus-4"|>];
      {AssociationQ[v],
       v["status"],
       hasMessageQ[msgs,
         JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete::apikey]}
    ]
  ],
  {True, "no-api-key", True},
  (* The whole point of this test is that the api-key message DOES fire.
     Older Wolfram 15.0 kernels silently dropped messages from
     VerificationTest's ActualMessages capture (hence the captureMsgs
     workaround above), but newer kernels propagate them properly, so we
     must declare them as expected or the test fails with
     SameMessagesFailure even though the assertion succeeded.            *)
  {JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete::apikey},
  TestID -> "OpenRouterChatComplete/no-api-key"
]


VerificationTest[
  withApiKey["sk-or-dummy-will-never-be-used",
    Module[{v, msgs},
      (* Messages must be a list of associations; pass a bare string. *)
      {v, msgs} = captureMsgs @
        JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete[
          {"not an assoc"},
          <|"Model" -> "anthropic/claude-opus-4"|>];
      {AssociationQ[v],
       v["status"],
       hasMessageQ[msgs,
         JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete::badmsgs]}
    ]
  ],
  {True, "bad-messages", True},
  {JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete::badmsgs},
  TestID -> "OpenRouterChatComplete/bad-messages"
]


(* String-prompt convenience overload routes to the same impl. *)
VerificationTest[
  withApiKey[None,
    Module[{v, msgs},
      {v, msgs} = captureMsgs @
        JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete[
          "hello", <|"Model" -> "anthropic/claude-opus-4"|>];
      {v["status"],
       hasMessageQ[msgs,
         JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete::apikey]}
    ]
  ],
  {"no-api-key", True},
  {JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete::apikey},
  TestID -> "OpenRouterChatComplete/string-prompt-overload"
]


(* ---------- decodeOpenRouterBody: keep-alive padding recovery ---------- *)

(* OpenRouter pads the response body with whitespace heartbeats while
   slow upstream models (notably minimax-m2.7) are still computing.  We
   captured a real 110 KB dump in the wild that started with 6,457 bytes
   of 0x20 spaces followed by a clean JSON object.  These tests pin down
   the recovery layer added in Kernel/OpenRouter.wl:decodeOpenRouterBody. *)


(* Happy path: clean JSON body decodes to the same Association as a
   plain ImportString[..., "RawJSON"] would have produced. *)
VerificationTest[
  Module[{out},
    out = JofreEspigulePons`WolframChallengesBenchmark`Private`decodeOpenRouterBody[
      "{\"id\":\"gen-x\",\"choices\":[{\"message\":{\"content\":\"hi\"}," <>
      "\"finish_reason\":\"stop\"}]}"];
    {AssociationQ[out],
     out["id"],
     out["choices"][[1]]["message"]["content"]}
  ],
  {True, "gen-x", "hi"},
  TestID -> "decodeOpenRouterBody/clean-json"
]


(* Bug C, primary case: 6,457 bytes of leading whitespace - matches the
   real captured dump at /private/var/folders/.../openrouter-badresponse-
   2026-04-22_161313-7e1e0a.txt (110 KB body, first non-whitespace byte
   at index 6457).  The trim-and-retry branch should recover. *)
VerificationTest[
  Module[{padded, out},
    padded = StringJoin[ConstantArray[" ", 6457]] <>
      "{\"id\":\"gen-padded\",\"choices\":[{\"message\":{\"content\":\"ok\"}," <>
      "\"finish_reason\":\"stop\"}]}";
    out = JofreEspigulePons`WolframChallengesBenchmark`Private`decodeOpenRouterBody[padded];
    {AssociationQ[out],
     out["id"],
     out["choices"][[1]]["message"]["content"]}
  ],
  {True, "gen-padded", "ok"},
  TestID -> "decodeOpenRouterBody/whitespace-prefix-recovers"
]


(* Mixed whitespace (spaces + tabs + newlines + CRs) before JSON: the
   StringTrim path should still work because StringTrim treats all of
   them as whitespace by default. *)
VerificationTest[
  Module[{padded, out},
    padded = StringJoin[ConstantArray[" \t\n\r", 64]] <>
      "{\"id\":\"gen-mixed\",\"choices\":[]}" <>
      StringJoin[ConstantArray[" \r\n", 32]];
    out = JofreEspigulePons`WolframChallengesBenchmark`Private`decodeOpenRouterBody[padded];
    {AssociationQ[out], out["id"]}
  ],
  {True, "gen-mixed"},
  TestID -> "decodeOpenRouterBody/mixed-whitespace-recovers"
]


(* Unrecoverable: completely non-JSON input returns $Failed without
   throwing. *)
VerificationTest[
  JofreEspigulePons`WolframChallengesBenchmark`Private`decodeOpenRouterBody[
    "<html><body>service unavailable</body></html>"],
  $Failed,
  TestID -> "decodeOpenRouterBody/non-json-returns-failed"
]


(* Non-string input (e.g. ByteArray, Null, $Failed) is tolerated and
   returns $Failed instead of an exception. *)
VerificationTest[
  JofreEspigulePons`WolframChallengesBenchmark`Private`decodeOpenRouterBody[Null],
  $Failed,
  TestID -> "decodeOpenRouterBody/non-string-input"
]
