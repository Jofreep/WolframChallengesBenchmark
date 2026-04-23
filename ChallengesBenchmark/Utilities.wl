(* ::Package:: *)

(* :Context: ChallengesBenchmark`Private` *)
(* :Summary: Shared utilities: structured logging, code extraction, JSONL writer. *)

Begin["ChallengesBenchmark`Private`"];

(* ----------------------------------------------------------------------- *)
(* Structured logging                                                      *)
(* ----------------------------------------------------------------------- *)

(* $logLevel: "Debug" | "Info" | "Warn" | "Error" *)
$logLevel = "Info";
$logLevels = <|"Debug" -> 10, "Info" -> 20, "Warn" -> 30, "Error" -> 40|>;

logAt[level_String, msg_String, payload_: None] := Module[{},
  If[$logLevels[level] >= $logLevels[$logLevel],
    WriteString[
      If[level === "Error" || level === "Warn", $Output, $Output],
      TemplateApply["[``] [``] ``\n",
        {DateString["ISODateTime"], level, msg}]
    ];
    If[payload =!= None && $logLevel === "Debug",
      WriteString[$Output, "  payload: ", ToString[payload, InputForm], "\n"]
    ];
  ]
];

logDebug[msg_, p_: None] := logAt["Debug", msg, p];
logInfo[msg_, p_: None]  := logAt["Info", msg, p];
logWarn[msg_, p_: None]  := logAt["Warn", msg, p];
logError[msg_, p_: None] := logAt["Error", msg, p];

(* ----------------------------------------------------------------------- *)
(* Code extraction                                                         *)
(* ----------------------------------------------------------------------- *)

(* extractCodeImpl — robustly extract the WL code block from an LLM response.
   Strategy (ordered):
     1. If the text contains a fenced ```wl|wolfram|mathematica block, take
        the LAST such block (LLMs often emit a revised block after "wait,
        let me reconsider...").
     2. Otherwise, if there is an unlabeled ``` fenced block, take the last.
     3. Otherwise, if the raw text parses as WL, return it verbatim.
     4. Otherwise, return the trimmed text and let the caller fail loudly.
*)

extractCodeImpl[text_String] := Module[
  {labeled, unlabeled, chosen},

  labeled = StringCases[
    text,
    "```" ~~ ("wl" | "wolfram" | "mathematica" | "wollframlanguage") ~~
      WhitespaceCharacter ... ~~ Shortest[body__] ~~ "```"
    :> StringTrim[body]
  ];

  unlabeled = StringCases[
    text,
    "```" ~~ WhitespaceCharacter ... ~~ Shortest[body__] ~~ "```"
    :> StringTrim[body]
  ];

  chosen = Which[
    labeled =!= {}, Last[labeled],
    unlabeled =!= {}, Last[unlabeled],
    True, StringTrim[text]
  ];

  chosen
];

(* parseHeldWL — parse a WL source string into a HoldComplete[...] expression.
   Returns HoldComplete[Null] on empty input and $Failed on parse error.
*)

parseHeldWL[src_String] := Module[{parsed},
  If[StringMatchQ[StringTrim[src], ""], Return[HoldComplete[Null]]];
  parsed = Quiet @ Check[
    ImportString[src, {"WL", "HeldExpressions"}],
    $Failed
  ];
  Which[
    parsed === $Failed || parsed === {}, $Failed,
    MatchQ[parsed, {HoldComplete[__]} | _HoldComplete], First @ Flatten[{parsed}],
    True, $Failed
  ]
];

(* ----------------------------------------------------------------------- *)
(* JSONL writer (append-only, crash-safe)                                  *)
(*                                                                         *)
(* We deliberately reopen the file on every record so a `tail -f` consumer *)
(* sees writes immediately and a kernel crash never loses buffered output. *)
(* The volume of records (one per test submit/complete) is small enough    *)
(* that the per-call open/close cost is negligible.                        *)
(* ----------------------------------------------------------------------- *)

appendJSONL[path_String, assoc_Association] := Module[{s, line, dir},
  line = Quiet @ Check[
    ExportString[assoc, "RawJSON", "Compact" -> True],
    ExportString[<|"event" -> "log.encode-error",
                   "summary" -> ToString[Short[assoc, 5], InputForm]|>,
                 "RawJSON", "Compact" -> True]
  ];
  dir = DirectoryName[path];
  If[StringLength[dir] > 0 && ! DirectoryQ[dir],
    Quiet @ CreateDirectory[dir, CreateIntermediateDirectories -> True]
  ];
  s = Quiet @ Check[OpenAppend[path, CharacterEncoding -> "UTF-8"], $Failed];
  If[Head[s] === OutputStream,
    WriteString[s, line, "\n"];
    Close[s];
    ,
    logError["appendJSONL: cannot open " <> path]
  ];
  Null
];

(* ----------------------------------------------------------------------- *)
(* Identifiers                                                             *)
(* ----------------------------------------------------------------------- *)

newRunId[] := "run-" <> DateString["ISODateTimeHyphenated"] <> "-" <>
  StringTake[IntegerString[RandomInteger[{16^6, 16^7 - 1}], 16], 6];

safeSlug[s_String] := StringReplace[s,
  RegularExpression["[^A-Za-z0-9_.-]"] -> "_"];

(* ----------------------------------------------------------------------- *)
(* Runtime fingerprint (for reproducibility)                               *)
(* ----------------------------------------------------------------------- *)

runtimeFingerprint[] := <|
  "wolframVersion"     -> $Version,
  "versionNumber"      -> $VersionNumber,
  "releaseNumber"      -> $ReleaseNumber,
  "system"             -> $SystemID,
  "processorCount"     -> $ProcessorCount,
  "kernelId"           -> $KernelID,
  "machineName"        -> Quiet @ $MachineName,
  "timestamp"          -> DateString["ISODateTime"],
  "absoluteTime"       -> AbsoluteTime[],
  "randomSeedUsed"     -> Null                     (* filled in per-run *)
|>;

End[];
