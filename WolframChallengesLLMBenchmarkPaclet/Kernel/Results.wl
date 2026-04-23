(* ::Package:: *)

(* :Context: JofreEspigulePons`WolframChallengesBenchmark`Private`          *)
(* :Summary:
     Result aggregation, summarization, and cross-run diffs.  Pure
     functions \[Dash] no file I/O here; the Runner owns persistence.
*)

Begin["JofreEspigulePons`WolframChallengesBenchmark`Private`"];


(* ------------------------------------------------------------------ *)
(* percentilesOf                                                       *)
(*                                                                     *)
(* Tail-latency view over a bag of numeric samples.  Returns a         *)
(* complete stat bundle (count / mean / p50..p99 / max / total) so     *)
(* downstream formatters don't branch on Missing[...] when the input  *)
(* is empty.                                                           *)
(* ------------------------------------------------------------------ *)

percentilesOf[xs_List] := Module[{clean},
  clean = Max[0., N[#]] & /@ Cases[xs, _?NumericQ];
  If[clean === {},
    <|"count" -> 0, "mean" -> 0., "p50" -> 0., "p75" -> 0., "p90" -> 0.,
      "p95" -> 0., "p99" -> 0., "max" -> 0., "total" -> 0.|>,
    <|
      "count" -> Length[clean],
      "mean"  -> Mean[clean],
      "p50"   -> Quantile[clean, 0.50],
      "p75"   -> Quantile[clean, 0.75],
      "p90"   -> Quantile[clean, 0.90],
      "p95"   -> Quantile[clean, 0.95],
      "p99"   -> Quantile[clean, 0.99],
      "max"   -> Max[clean],
      "total" -> Total[clean]
    |>
  ]
];


(* ------------------------------------------------------------------ *)
(* summarizeResults                                                    *)
(*                                                                     *)
(* One-line scoreboard + percentile distributions for a results list. *)
(* Timing / memory percentiles are computed only over tests that      *)
(* actually evaluated \[Dash] TimedOut / MemoryExceeded / NoSolution     *)
(* entries have synthetic durations that would skew the p99.           *)
(* ------------------------------------------------------------------ *)

summarizeResults[results_List] := Module[
  {byStatus, total, passed, failed, errored, timedOut, memEx, parseErr,
   noSol, kernelDied, byChallenge, perChallengePass, fastestN, slowestN,
   evaluatedResults, durations, memoryUses, durationStats, memoryStats},

  total      = Length[results];
  byStatus   = Counts[#["status"] & /@ results];
  passed     = Count[results, _?(#["passed"] === True &)];
  failed     = Count[results, _?(#["status"] === "Evaluated" && ! #["passed"] &)];
  errored    = Lookup[byStatus, "EvaluationError", 0] +
               Lookup[byStatus, "RunnerError", 0];
  timedOut   = Lookup[byStatus, "TimedOut", 0];
  memEx      = Lookup[byStatus, "MemoryExceeded", 0];
  parseErr   = Lookup[byStatus, "ParseError", 0];
  noSol      = Lookup[byStatus, "NoSolution", 0];
  kernelDied = Lookup[byStatus, "KernelDied", 0];

  byChallenge = GroupBy[results, #["challengeName"] &];
  perChallengePass = AssociationMap[
    Function[c, Boole[AllTrue[byChallenge[c], #["passed"] &]]],
    Keys[byChallenge]
  ];

  fastestN = Take[SortBy[results, #["durationSec"] &],
                 UpTo[5]] /. r_Association :>
    <|"testId" -> r["testId"], "durationSec" -> r["durationSec"]|>;
  slowestN = Take[Reverse @ SortBy[results, #["durationSec"] &],
                 UpTo[5]] /. r_Association :>
    <|"testId" -> r["testId"], "durationSec" -> r["durationSec"]|>;

  evaluatedResults = Select[results,
    MemberQ[{"Evaluated", "EvaluationError", "RunnerError"}, #["status"]] &
  ];
  durations     = Cases[#["durationSec"] & /@ evaluatedResults, _?NumericQ];
  memoryUses    = Cases[#["memoryBytes"] & /@ evaluatedResults, _?NumericQ];
  durationStats = percentilesOf[durations];
  memoryStats   = percentilesOf[memoryUses];

  <|
    "total"                  -> total,
    "passed"                 -> passed,
    "failed"                 -> failed,
    "evaluationError"        -> errored,
    "timedOut"               -> timedOut,
    "memoryExceeded"         -> memEx,
    "parseError"             -> parseErr,
    "noSolution"             -> noSol,
    "kernelDied"             -> kernelDied,
    "passRate"               -> If[total > 0, N[passed/total], 0.],
    "challengesAttempted"    -> Length[byChallenge],
    "challengesFullyPassing" -> Total[Values[perChallengePass]],
    "perChallenge"           -> perChallengePass,
    "fastest"                -> fastestN,
    "slowest"                -> slowestN,
    "duration"               -> durationStats,
    "memory"                 -> memoryStats
  |>
];


(* ------------------------------------------------------------------ *)
(* diffRunsImpl                                                        *)
(*                                                                     *)
(* Compare two runs by testId; report regressions, fixes, new tests,  *)
(* missing tests, and per-test status changes.                         *)
(* ------------------------------------------------------------------ *)

diffRunsImpl[baseline_Association, new_Association] := Module[
  {bResults, nResults, bByID, nByID, bIds, nIds,
   common, regressions, fixes, newTests, missingTests, statusChanges},

  bResults = Lookup[baseline, "results", {}];
  nResults = Lookup[new,      "results", {}];
  bByID = Association[(#["testId"] -> #) & /@ bResults];
  nByID = Association[(#["testId"] -> #) & /@ nResults];
  bIds = Keys[bByID]; nIds = Keys[nByID];
  common = Intersection[bIds, nIds];

  regressions = Select[common,
    bByID[#]["passed"] === True && nByID[#]["passed"] === False &];
  fixes = Select[common,
    bByID[#]["passed"] === False && nByID[#]["passed"] === True &];
  newTests     = Complement[nIds, bIds];
  missingTests = Complement[bIds, nIds];

  statusChanges = Association @ Map[
    # -> <|
      "before"       -> bByID[#]["status"],
      "after"        -> nByID[#]["status"],
      "passedBefore" -> bByID[#]["passed"],
      "passedAfter"  -> nByID[#]["passed"]
    |> &,
    Select[common,
      (bByID[#]["status"] =!= nByID[#]["status"]) ||
      (bByID[#]["passed"] =!= nByID[#]["passed"]) &]
  ];

  <|
    "baseline"      -> Lookup[baseline, "runId", "?"],
    "new"           -> Lookup[new,      "runId", "?"],
    "regressions"   -> regressions,
    "fixes"         -> fixes,
    "newTests"      -> newTests,
    "missingTests"  -> missingTests,
    "statusChanges" -> statusChanges,
    "summary"       -> <|
      "regressions"  -> Length[regressions],
      "fixes"        -> Length[fixes],
      "newTests"     -> Length[newTests],
      "missingTests" -> Length[missingTests]
    |>
  |>
];


End[];  (* `Private` *)
