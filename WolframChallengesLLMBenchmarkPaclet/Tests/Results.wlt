(* ::Package:: *)

(* Tests for the Results module: summarizeResults, percentilesOf, diffRunsImpl
   are all Private but reachable through RunBenchmark + DiffRuns.  This suite
   exercises the summary contract directly so downstream consumers (report
   writers, CI) have a stable shape. *)

Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

$tmp = FileNameJoin[{$TemporaryDirectory,
  "wclb-tests-results-" <> ToString[RandomInteger[10^9]]}];
If[! DirectoryQ[$tmp],
  CreateDirectory[$tmp, CreateIntermediateDirectories -> True]];

challenges = <|"Sum" -> <|"name" -> "Sum", "prompt" -> "addTwo"|>|>;
testBank   = <|"Sum" -> {
  <|"challengeName" -> "Sum",
    "input"    -> HoldComplete[addTwo[1, 1]],
    "expected" -> 2, "metadata" -> <||>|>,
  <|"challengeName" -> "Sum",
    "input"    -> HoldComplete[addTwo[2, 2]],
    "expected" -> 4, "metadata" -> <||>|>,
  <|"challengeName" -> "Sum",
    "input"    -> HoldComplete[addTwo[3, 3]],
    "expected" -> 6, "metadata" -> <||>|>}
|>;


VerificationTest[
  Module[{sol, run, s},
    sol = <|"Sum" -> <|"code" -> "addTwo[a_, b_] := a + b"|>|>;
    run = JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
      challenges, testBank, sol,
      "Model" -> "t/summary",
      "IsolationMode" -> "InProcess",
      "OutputDirectory" -> $tmp, "TimeConstraint" -> 10];
    s = run["meta", "summary"];
    Sort @ Keys[s]
  ],
  Sort @ {"total", "passed", "failed", "evaluationError", "timedOut",
          "memoryExceeded", "parseError", "noSolution", "kernelDied",
          "passRate", "challengesAttempted", "challengesFullyPassing",
          "perChallenge", "fastest", "slowest", "duration", "memory"},
  TestID -> "summarizeResults/shape"
]


VerificationTest[
  Module[{sol, run, s},
    sol = <|"Sum" -> <|"code" -> "addTwo[a_, b_] := a + b"|>|>;
    run = JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
      challenges, testBank, sol,
      "Model" -> "t/percent",
      "IsolationMode" -> "InProcess",
      "OutputDirectory" -> $tmp, "TimeConstraint" -> 10];
    s = run["meta", "summary", "duration"];
    Sort @ Keys[s]
  ],
  Sort @ {"count", "mean", "p50", "p75", "p90", "p95", "p99", "max", "total"},
  TestID -> "percentilesOf/shape"
]


VerificationTest[
  Module[{sol, run, s},
    sol = <|"Sum" -> <|"code" -> "addTwo[a_, b_] := a + b"|>|>;
    run = JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
      challenges, testBank, sol,
      "Model" -> "t/rate",
      "IsolationMode" -> "InProcess",
      "OutputDirectory" -> $tmp, "TimeConstraint" -> 10];
    s = run["meta", "summary"];
    {s["passed"], s["total"], s["passRate"],
     s["challengesFullyPassing"]}
  ],
  {3, 3, 1., 1},
  TestID -> "summarizeResults/pass-rate-is-one"
]
