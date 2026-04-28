(* ::Package:: *)

(* :Context: JofreEspigulePons`WolframChallengesBenchmark`Private`          *)
(* :Summary:
     Shared utilities for the paclet: structured logging, WL code
     extraction from LLM replies, HoldComplete parsing, append-only
     JSONL writer, identifier helpers, and a runtime fingerprint.
*)

Begin["JofreEspigulePons`WolframChallengesBenchmark`Private`"];


(* ------------------------------------------------------------------ *)
(* Structured logging                                                 *)
(* ------------------------------------------------------------------ *)

$logLevel  = "Info";
$logLevels = <|"Debug" -> 10, "Info" -> 20, "Warn" -> 30, "Error" -> 40|>;

logAt[level_String, msg_String, payload_:None] := (
  If[$logLevels[level] >= $logLevels[$logLevel],
    WriteString[$Output,
      TemplateApply["[``] [``] ``\n",
        {DateString["ISODateTime"], level, msg}]
    ];
    If[payload =!= None && $logLevel === "Debug",
      WriteString[$Output, "  payload: ",
        ToString[payload, InputForm], "\n"]
    ];
  ];
  Null
);

logDebug[msg_, p_:None] := logAt["Debug", msg, p];
logInfo [msg_, p_:None] := logAt["Info",  msg, p];
logWarn [msg_, p_:None] := logAt["Warn",  msg, p];
logError[msg_, p_:None] := logAt["Error", msg, p];


(* ------------------------------------------------------------------ *)
(* Code extraction                                                    *)
(*                                                                    *)
(* LLMs occasionally emit multiple fenced blocks in a single reply    *)
(* ("wait, here's a better version").  We pick the LAST fenced block  *)
(* of the strongest language label, or fall back to the trimmed text  *)
(* and let the audit stage refuse the result.                         *)
(* ------------------------------------------------------------------ *)

extractCodeImpl[text_String] := Module[{labeled, unlabeled},

  labeled = StringCases[text,
    "```" ~~ ("wl" | "wolfram" | "mathematica" | "wolframlanguage") ~~
      WhitespaceCharacter ... ~~ Shortest[body__] ~~ "```"
    :> StringTrim[body]
  ];

  unlabeled = StringCases[text,
    "```" ~~ WhitespaceCharacter ... ~~ Shortest[body__] ~~ "```"
    :> StringTrim[body]
  ];

  Which[
    labeled   =!= {}, Last[labeled],
    unlabeled =!= {}, Last[unlabeled],
    True,             StringTrim[text]
  ]
];

extractCodeImpl[_] := $Failed;


(* parseHeldWL — parse a WL source string into a HoldComplete[...]
   expression.  Returns HoldComplete[Null] on empty input and $Failed
   on parse error.  Used by the write-time audit to inspect the shape
   of candidate code without ever evaluating it.

   Multi-statement source (e.g. a helper definition followed by the
   main function) is supported: ImportString[..., "HeldExpressions"]
   returns one HoldComplete per top-level expression, and we glue
   them into a single HoldComplete[CompoundExpression[def1, def2,
   ...]] so downstream `ReleaseHold[heldDef]` runs all of them in
   order.  Without this glue, only the first statement would survive
   into the kernel and any helper-then-main canonical (like
   AliquotSequence's `test[x_] := ...; AliquotSequence[n_] := ...`)
   would silently drop the main definition. *)

parseHeldWL[src_String] := Module[{trimmed, parsed, joinedSource},
  trimmed = StringTrim[src];
  If[trimmed === "", Return[HoldComplete[Null]]];
  parsed = Quiet @ Check[
    ImportString[trimmed, {"WL", "HeldExpressions"}],
    $Failed
  ];
  Which[
    parsed === $Failed || parsed === {}, $Failed,
    ! ListQ[parsed], $Failed,
    ! AllTrue[parsed, MatchQ[#, _HoldComplete] &], $Failed,
    (* Single top-level statement: return as-is. *)
    Length[parsed] === 1, First[parsed],
    (* Multi-statement: re-emit each held expression's body as an
       InputForm string (stripping the HoldComplete[...] wrapper),
       glue with ";", and re-parse via ToExpression with HoldComplete
       wrapper. The result is a single HoldComplete[CompoundExpression[
       def1, def2, ...]] which ReleaseHold runs in order. *)
    True,
      joinedSource = StringRiffle[
        Map[
          Function[heldExpr,
            Module[{s = ToString[heldExpr, InputForm,
                                 PageWidth -> Infinity]},
              s = StringReplace[s,
                    StartOfString ~~ "HoldComplete[" -> ""];
              s = StringReplace[s, "]" ~~ EndOfString -> ""];
              s
            ]
          ],
          parsed
        ],
        "; "
      ];
      Quiet @ Check[
        ToExpression[joinedSource, InputForm, HoldComplete],
        $Failed
      ]
  ]
];

parseHeldWL[_] := $Failed;


(* ------------------------------------------------------------------ *)
(* JSONL append writer                                                *)
(*                                                                    *)
(* Reopened per record so `tail -f` shows writes immediately and a    *)
(* kernel crash can never lose buffered output.  Record volume is     *)
(* low so the open/close overhead is irrelevant.                      *)
(*                                                                    *)
(* Sanitization: JSON has no `None` but lots of WL code uses          *)
(*   `Lookup[..., k, None]` as a "missing" sentinel.  Before encoding *)
(*   we rewrite `None` \[Rule] `Null` (which ExportString renders as  *)
(*   JSON null) and strip arbitrary-precision markers from Reals so   *)
(*   no caller has to remember to pre-clean its payload.              *)
(* ------------------------------------------------------------------ *)

(* Sanitization: RawJSON refuses `None` (WL's Missing sentinel) but the
   codebase uses `None` widely as a "absent value" marker.  Rewriting
   `None -> Null` gives RawJSON a proper JSON null.

   Deliberately NO Real-precision rewrite:  ExportString["RawJSON"]
   handles arbitrary-precision Reals natively at any precision (tested
   from ~3 digits up through AbsoluteTime's ~16 digits).  Earlier
   versions of this sanitizer tried to force machine precision via
   `r_Real :> N[r]`, but that creates a "ghost head" artifact around
   rewritten Reals in nested Associations \[LongDash] the standalone
   values encode fine, but the parent Association refuses.  A
   minimum-footprint sanitizer avoids the whole problem.               *)

sanitizeForJSON[x_Association] := x /. None -> Null;
sanitizeForJSON[x_]            := x /. None -> Null;

(* AbortProtect defers any pending Abort marker for the duration of the
   protected expression.  This is the linchpin: when the tombstone writer
   is invoked from `Internal\`WithLocalSettings`'s cleanup body under a
   propagating Abort, OpenAppend (and other built-ins) otherwise re-fire
   the abort and bail early.  AbortProtect suspends the marker until the
   protected expression returns, so the open / write / close trio runs
   to completion and the tombstone lands.  After AbortProtect exits, the
   deferred Abort resumes propagating to outer CheckAbort handlers, so
   callers (tests, the CLI) still see the abort.

   Defensive coercion: even under AbortProtect, some built-ins (observed
   in practice: ExportString under a pending Abort) return Null rather
   than their usual value.  StringQ checks after each abort-sensitive
   call promote these to a hard-coded fallback string so the tombstone
   line is guaranteed to be valid JSONL.                                *)

appendJSONL[path_String, assoc_Association] := AbortProtect @ Module[
  {s, line, dir, safe,
   hardFallback = "{\"event\":\"log.encodeError\",\"summary\":\"fallback\"}"},
  safe = sanitizeForJSON[assoc];
  line = Quiet @ Check[
    ExportString[safe, "RawJSON", "Compact" -> True],
    hardFallback
  ];
  If[! StringQ[line], line = hardFallback];
  dir = DirectoryName[path];
  If[StringLength[dir] > 0 && ! DirectoryQ[dir],
    Quiet @ CreateDirectory[dir, CreateIntermediateDirectories -> True]
  ];
  s = Quiet @ Check[
    OpenAppend[path, CharacterEncoding -> "UTF-8"],
    $Failed
  ];
  If[Head[s] === OutputStream,
    WriteString[s, line, "\n"];
    Close[s],
    logError["appendJSONL: cannot open " <> path]
  ];
  Null
];

appendJSONL[___] := $Failed;


(* writeJSONLThrough — write a single JSONL record through an already-open
   OutputStream.  This is the abort-tolerant path used by the tombstone
   writer: OpenAppend re-triggers the ambient Abort marker when called
   inside an Internal\`WithLocalSettings cleanup body, so we keep a stream
   open for the lifetime of the run and only WriteString in cleanup.

   The stream MUST be opened by the caller (OpenAppend in a normal
   execution context, before any Abort propagates).  This function only
   encodes + writes + tolerates encoder aborts; it never opens, and it
   does NOT close the stream so the caller can decide lifecycle.

   Returns Null on success, $Failed if the stream isn't an OutputStream. *)

writeJSONLThrough[s_OutputStream, assoc_Association] := Module[
  {line, safe, fallback, sawAbort = False},
  safe = sanitizeForJSON[assoc];
  fallback = "{\"event\":\"log.encodeError\",\"summary\":\"abort-in-encoder\"}";
  line = Quiet @ CheckAbort[
    Quiet @ Check[
      ExportString[safe, "RawJSON", "Compact" -> True],
      fallback
    ],
    sawAbort = True; fallback
  ];
  Quiet @ CheckAbort[
    WriteString[s, line, "\n"],
    sawAbort = True
  ];
  If[TrueQ[sawAbort], Abort[]];
  Null
];

writeJSONLThrough[___] := $Failed;


(* encodeTombstoneLine — abort-tolerant JSONL encoder for the run
   tombstone.  Why hand-rolled: under a propagating Abort (in
   `Internal\`WithLocalSettings` cleanup), `ExportString[..., "RawJSON"]`
   returns `Null` instead of a string, even inside `AbortProtect`.
   `StringJoin` and `IntegerString` make no internal abort checks, so a
   manual encoder reliably produces a valid JSON line in cleanup.

   Schema is fixed to the tombstone's known shape:
     event : "generate.finished" | "generate.aborted"
     runId : String
     counts: <|"ok", "failed", "auditRejected", "other"|>  (Integers)
     completedCount, totalQueued: Integer
     lastName : String | None
     finishedAt : String (ISO-8601 UTC)                                  *)

jsonStringEscape[s_String] := StringReplace[s, {
  "\\" -> "\\\\",
  "\""  -> "\\\"",
  "\n" -> "\\n",
  "\r" -> "\\r",
  "\t" -> "\\t"
}];
jsonStringEscape[other_] := jsonStringEscape[ToString[other]];

encodeTombstoneLine[
  event_String, runId_String, counts_Association,
  completedCount_Integer, totalQueued_Integer, lastName_,
  finishedAt_String
] := StringJoin[
  "{\"event\":\"",      jsonStringEscape[event],      "\",",
  "\"runId\":\"",       jsonStringEscape[runId],      "\",",
  "\"counts\":{",
    "\"ok\":",            IntegerString[Lookup[counts, "ok",            0]], ",",
    "\"failed\":",        IntegerString[Lookup[counts, "failed",        0]], ",",
    "\"auditRejected\":", IntegerString[Lookup[counts, "auditRejected", 0]], ",",
    "\"other\":",         IntegerString[Lookup[counts, "other",         0]],
  "},",
  "\"completedCount\":", IntegerString[completedCount], ",",
  "\"totalQueued\":",    IntegerString[totalQueued],    ",",
  "\"lastName\":", If[StringQ[lastName],
                      "\"" <> jsonStringEscape[lastName] <> "\"",
                      "null"], ",",
  "\"finishedAt\":\"", jsonStringEscape[finishedAt], "\"}"
];


(* writeTombstone — open + write + close a single pre-encoded line.  All
   three primitives observed to tolerate a pending abort under
   `AbortProtect`.  Caller is responsible for the AbortProtect wrap so
   the abort marker continues propagating after this returns.            *)

writeTombstone[path_String, line_String] := Module[{s},
  s = Quiet @ Check[
    OpenAppend[path, CharacterEncoding -> "UTF-8"],
    $Failed
  ];
  If[Head[s] === OutputStream,
    WriteString[s, line, "\n"];
    Close[s],
    logError["writeTombstone: cannot open " <> path]
  ];
  Null
];

writeTombstone[___] := $Failed;


(* ------------------------------------------------------------------ *)
(* Identifiers                                                        *)
(* ------------------------------------------------------------------ *)

newRunId[] := "run-" <>
  DateString[{"ISODate", "_", "Hour24", "Minute", "Second"}] <> "-" <>
  StringTake[IntegerString[RandomInteger[{16^6, 16^7 - 1}], 16], 6];

safeSlug[s_String] :=
  StringReplace[s, RegularExpression["[^A-Za-z0-9_.-]"] -> "_"];

safeSlug[other_] := safeSlug[ToString[other]];

sha256Hex[s_String] := "sha256:" <> Hash[s, "SHA256", "HexString"];


(* ------------------------------------------------------------------ *)
(* Runtime fingerprint                                                *)
(* ------------------------------------------------------------------ *)

runtimeFingerprint[] := <|
  "wolframVersion" -> $Version,
  "versionNumber"  -> $VersionNumber,
  "releaseNumber"  -> $ReleaseNumber,
  "system"         -> $SystemID,
  "processorCount" -> $ProcessorCount,
  "kernelId"       -> $KernelID,
  "machineName"    -> Quiet @ $MachineName,
  "timestamp"      -> DateString["ISODateTime"],
  "absoluteTime"   -> AbsoluteTime[]
|>;


(* ------------------------------------------------------------------ *)
(* ISO-8601 UTC timestamp                                             *)
(* ------------------------------------------------------------------ *)

isoUTC[] := DateString[DateObject[Now, TimeZone -> 0],
  {"ISODateTime", "Z"}];


End[];  (* `Private` *)
