(* ::Package:: *)

(* Tests for Compare.wl: CompareModels + WriteCompareReport.

   Strategy: run two small InProcess benchmarks with the same challenges
   but different solutions (one good, one broken), then diff them with
   CompareModels and check the resulting matrix + the written report
   files. *)

Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

$tmp = FileNameJoin[{$TemporaryDirectory,
  "wclb-tests-compare-" <> ToString[RandomInteger[10^9]]}];
If[! DirectoryQ[$tmp],
  CreateDirectory[$tmp, CreateIntermediateDirectories -> True]];

challenges = <|
  "Sum"     -> <|"name" -> "Sum",     "prompt" -> "addTwo"|>,
  "Product" -> <|"name" -> "Product", "prompt" -> "mulTwo"|>
|>;
testBank = <|
  "Sum" -> {
    <|"challengeName" -> "Sum",
      "input"    -> HoldComplete[addTwo[1, 1]],
      "expected" -> 2, "metadata" -> <||>|>,
    <|"challengeName" -> "Sum",
      "input"    -> HoldComplete[addTwo[2, 3]],
      "expected" -> 5, "metadata" -> <||>|>
  },
  "Product" -> {
    <|"challengeName" -> "Product",
      "input"    -> HoldComplete[mulTwo[2, 3]],
      "expected" -> 6, "metadata" -> <||>|>
  }
|>;

(* Model A: both correct.  Model B: Sum correct, Product wrong. *)
solsA = <|
  "Sum"     -> <|"code" -> "addTwo[a_, b_] := a + b"|>,
  "Product" -> <|"code" -> "mulTwo[a_, b_] := a * b"|>
|>;
solsB = <|
  "Sum"     -> <|"code" -> "addTwo[a_, b_] := a + b"|>,
  "Product" -> <|"code" -> "mulTwo[a_, b_] := a + b"|>   (* wrong *)
|>;

mkRun[tag_String, sol_Association] :=
  JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
    challenges, testBank, sol,
    "Model" -> tag,
    "IsolationMode" -> "InProcess",
    "OutputDirectory" -> $tmp,
    "TimeConstraint" -> 10];

runA = mkRun["t/cmp-a", solsA];
runB = mkRun["t/cmp-b", solsB];


(* ---------- CompareModels: list input returns full shape ------------- *)

VerificationTest[
  Module[{cmp},
    cmp = JofreEspigulePons`WolframChallengesBenchmark`CompareModels[
      {runA["runDir"], runB["runDir"]}];
    Sort @ Keys[cmp]
  ],
  Sort @ {"models", "runsByModel", "perChallenge", "allChallenges",
          "uniquelyPassed", "uniquelyFailed"},
  TestID -> "CompareModels/shape"
]


(* ---------- CompareModels: perChallenge matrix has both models ------- *)

VerificationTest[
  Module[{cmp, sumRow, prodRow},
    cmp = JofreEspigulePons`WolframChallengesBenchmark`CompareModels[
      <|"a" -> runA["runDir"], "b" -> runB["runDir"]|>];
    sumRow  = cmp["perChallenge", "Sum"];
    prodRow = cmp["perChallenge", "Product"];
    {Sort @ Keys[sumRow],
     sumRow["a", "passed"],  sumRow["b", "passed"],
     prodRow["a", "passed"], prodRow["b", "passed"]}
  ],
  {Sort @ {"a", "b"}, 2, 2, 1, 0},
  TestID -> "CompareModels/matrix-values"
]


(* ---------- CompareModels: uniquelyPassed reflects model B's failure -- *)
(* Model A passes Product; model B fails Product.  So "a" uniquely passes
   Product, and "b" uniquely fails it (from a's perspective).             *)

VerificationTest[
  Module[{cmp},
    cmp = JofreEspigulePons`WolframChallengesBenchmark`CompareModels[
      <|"a" -> runA["runDir"], "b" -> runB["runDir"]|>];
    {cmp["uniquelyPassed", "a"],
     cmp["uniquelyPassed", "b"],
     cmp["uniquelyFailed", "a"],
     cmp["uniquelyFailed", "b"]}
  ],
  {{"Product"}, {}, {}, {"Product"}},
  TestID -> "CompareModels/uniquely-passed-failed"
]


(* ---------- WriteCompareReport: writes compare.md and compare.html --- *)

VerificationTest[
  Module[{cmp, dir, paths},
    cmp = JofreEspigulePons`WolframChallengesBenchmark`CompareModels[
      <|"a" -> runA["runDir"], "b" -> runB["runDir"]|>];
    dir = FileNameJoin[{$tmp, "compare-out"}];
    paths = JofreEspigulePons`WolframChallengesBenchmark`WriteCompareReport[cmp, dir];
    {Sort @ Keys[paths],
     FileExistsQ[paths["markdown"]],
     FileExistsQ[paths["html"]],
     StringContainsQ[Import[paths["markdown"], "Text"], "Product"],
     StringContainsQ[Import[paths["html"],     "Text"], "Product"]}
  ],
  {Sort @ {"html", "markdown"}, True, True, True, True},
  TestID -> "WriteCompareReport/writes-both-files"
]


(* ---------- WriteCompareReport: creates output directory ------------- *)

VerificationTest[
  Module[{cmp, dir, paths},
    cmp = JofreEspigulePons`WolframChallengesBenchmark`CompareModels[
      {runA["runDir"], runB["runDir"]}];
    dir = FileNameJoin[{$tmp, "deep", "compare-mkdir"}];
    paths = JofreEspigulePons`WolframChallengesBenchmark`WriteCompareReport[cmp, dir];
    {DirectoryQ[dir], FileExistsQ[paths["markdown"]]}
  ],
  {True, True},
  TestID -> "WriteCompareReport/creates-output-directory"
]


(* ---------- CompareModels: bad input returns $Failed (not a crash) --- *)

VerificationTest[
  Quiet @ JofreEspigulePons`WolframChallengesBenchmark`CompareModels[
    {"/nonexistent/dir/one", "/nonexistent/dir/two"}],
  $Failed,
  TestID -> "CompareModels/all-invalid-dirs"
]
