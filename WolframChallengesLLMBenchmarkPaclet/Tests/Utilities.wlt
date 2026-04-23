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
