(* ::Package:: *)

(* Tests for the Utilities + ExtractCode surfaces. *)

Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

bt3 = StringJoin[ConstantArray[FromCharacterCode[96], 3]];

VerificationTest[
  JofreEspigulePons`WolframChallengesBenchmark`ExtractCode[
    "Here you go:\n" <> bt3 <> "wl\nf[x_] := x + 1\n" <> bt3 <> "\n"],
  "f[x_] := x + 1",
  TestID -> "ExtractCode/labeled-wl"
]

VerificationTest[
  JofreEspigulePons`WolframChallengesBenchmark`ExtractCode[
    "preamble " <> bt3 <> "\nfirst\n" <> bt3 <>
    " then " <> bt3 <> "wl\nlast\n" <> bt3],
  "last",
  TestID -> "ExtractCode/last-labeled-wins"
]

VerificationTest[
  JofreEspigulePons`WolframChallengesBenchmark`ExtractCode[
    bt3 <> "mathematica\nfoo[x_] := x\n" <> bt3],
  "foo[x_] := x",
  TestID -> "ExtractCode/mathematica-label"
]

VerificationTest[
  JofreEspigulePons`WolframChallengesBenchmark`ExtractCode[
    "no fences here, just trim me   "],
  "no fences here, just trim me",
  TestID -> "ExtractCode/no-fence-trim"
]

VerificationTest[
  JofreEspigulePons`WolframChallengesBenchmark`ExtractCode[42],
  $Failed,
  TestID -> "ExtractCode/non-string"
]

VerificationTest[
  StringQ @ JofreEspigulePons`WolframChallengesBenchmark`BenchmarkVersion[],
  True,
  TestID -> "BenchmarkVersion/string"
]

VerificationTest[
  AssociationQ @ JofreEspigulePons`WolframChallengesBenchmark`$BenchmarkDefaults,
  True,
  TestID -> "$BenchmarkDefaults/is-assoc"
]

VerificationTest[
  KeyExistsQ[
    JofreEspigulePons`WolframChallengesBenchmark`$BenchmarkDefaults,
    "TimeConstraint"],
  True,
  TestID -> "$BenchmarkDefaults/has-TimeConstraint"
]

(* The OpenRouter API key is not set in CI: ensure the public symbol
   doesn't crash and reports the expected sentinel.  This protects
   downstream consumers that branch on the value. *)
VerificationTest[
  Module[{prev = Environment["OPENROUTER_API_KEY"], v},
    SetEnvironment["OPENROUTER_API_KEY" -> None];   (* unset *)
    v = JofreEspigulePons`WolframChallengesBenchmark`$OpenRouterAPIKey;
    If[StringQ[prev], SetEnvironment["OPENROUTER_API_KEY" -> prev]];
    v
  ],
  $Failed,
  TestID -> "$OpenRouterAPIKey/unset-returns-failed"
]


(* ---------- parseHeldWL: single-statement source ------------------- *)

VerificationTest[
  JofreEspigulePons`WolframChallengesBenchmark`Private`parseHeldWL[
    "addTwo[a_, b_] := a + b"],
  HoldComplete[addTwo[a_, b_] := a + b],
  TestID -> "parseHeldWL/single-statement"
]


(* ---------- parseHeldWL: multi-statement source -------------------- *)
(* Regression test for the AliquotSequence-style canonical: a helper
   definition followed by the main definition, separated by blank
   lines.  Pre-fix, parseHeldWL only matched a single-element
   {HoldComplete[__]} list and returned $Failed for any 2+ statement
   source, silently making 161 of 724 bank-self-test entries
   (22%) ParseError.  Now those parse to a single
   HoldComplete[CompoundExpression[def1, def2, ...]] and run
   correctly under ReleaseHold. *)

VerificationTest[
  Module[{src, parsed},
    src = "test[x_] := !MemberQ[Most[x], Last[x]]\n\n" <>
          "AliquotSequence[n_] := test[{n}]";
    parsed = JofreEspigulePons`WolframChallengesBenchmark`Private`parseHeldWL[
      src];
    {Head[parsed],
     (* The wrapped body should be a CompoundExpression containing both
        SetDelayed defs. *)
     MatchQ[parsed,
       HoldComplete[CompoundExpression[_SetDelayed, _SetDelayed]]]}
  ],
  {HoldComplete, True},
  TestID -> "parseHeldWL/multi-statement-glues-into-CompoundExpression"
]


(* ---------- parseHeldWL: multi-statement actually defines all fns -- *)
(* Make sure the glue isn't just structural: ReleaseHold of the parsed
   output must install ALL definitions in the kernel, not just the
   first.  Uses long unique global names + explicit cleanup so the
   probe doesn't pollute Global` and doesn't fight with $Context
   binding inside parseHeldWL. *)

VerificationTest[
  Module[{parsed, result},
    Quiet @ ClearAll[parseHeldWLProbeHelperFn, parseHeldWLProbeMainFn];
    parsed = JofreEspigulePons`WolframChallengesBenchmark`Private`parseHeldWL[
      "parseHeldWLProbeHelperFn[x_] := x + 100\n\n" <>
      "parseHeldWLProbeMainFn[x_]   := parseHeldWLProbeHelperFn[x] * 2"];
    ReleaseHold[parsed];
    result = parseHeldWLProbeMainFn[5];
    Quiet @ ClearAll[parseHeldWLProbeHelperFn, parseHeldWLProbeMainFn];
    result
  ],
  210,   (* helper[5] = 105; main[5] = 105 * 2 = 210 *)
  TestID -> "parseHeldWL/multi-statement-installs-all-defs"
]


(* ---------- parseHeldWL: empty input -------------------------------- *)

VerificationTest[
  JofreEspigulePons`WolframChallengesBenchmark`Private`parseHeldWL[""],
  HoldComplete[Null],
  TestID -> "parseHeldWL/empty-source"
]


(* ---------- parseHeldWL: malformed source returns $Failed ----------- *)

VerificationTest[
  JofreEspigulePons`WolframChallengesBenchmark`Private`parseHeldWL[
    "this is [[[ not valid wolfram"],
  $Failed,
  TestID -> "parseHeldWL/malformed-returns-failed"
]
