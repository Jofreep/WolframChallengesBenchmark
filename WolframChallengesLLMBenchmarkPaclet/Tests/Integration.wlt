(* ::Package:: *)

(* Integration tests: exercise isolation modes that spawn subkernels.
   These are slower than the unit tests and run serially. *)

Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

$tmp = FileNameJoin[{$TemporaryDirectory,
  "wclb-tests-integration-" <> ToString[RandomInteger[10^9]]}];
If[! DirectoryQ[$tmp],
  CreateDirectory[$tmp, CreateIntermediateDirectories -> True]];

challenges = <|"Sum" -> <|"name" -> "Sum", "prompt" -> "addTwo"|>|>;
testBank = <|"Sum" -> {
  <|"challengeName" -> "Sum",
    "input"    -> HoldComplete[addTwo[2, 3]],
    "expected" -> 5, "metadata" -> <||>|>,
  <|"challengeName" -> "Sum",
    "input"    -> HoldComplete[addTwo[100, 200]],
    "expected" -> 300, "metadata" -> <||>|>}
|>;
sol = <|"Sum" -> <|"code" -> "addTwo[a_, b_] := a + b"|>|>;


VerificationTest[
  Module[{run},
    run = JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
      challenges, testBank, sol,
      "Model"           -> "t/pertest",
      "IsolationMode"   -> "PerTestKernel",
      "OutputDirectory" -> $tmp,
      "Parallel"        -> 2,
      "TimeConstraint"  -> 30,
      "PollInterval"    -> 0.1];
    {run["meta", "summary", "passed"],
     run["meta", "summary", "total"]}
  ],
  {2, 2},
  TestID -> "RunBenchmark/PerTestKernel-happy-path",
  TimeConstraint -> 120
]


(* PooledKernels mode: verify the DistributeDefinitions wiring works. *)

VerificationTest[
  Module[{run},
    run = JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
      challenges, testBank, sol,
      "Model"           -> "t/pool",
      "IsolationMode"   -> "PooledKernels",
      "OutputDirectory" -> $tmp,
      "Parallel"        -> 2,
      "TimeConstraint"  -> 30];
    {run["meta", "summary", "passed"],
     run["meta", "summary", "total"]}
  ],
  {2, 2},
  TestID -> "RunBenchmark/PooledKernels-happy-path",
  TimeConstraint -> 180
]
