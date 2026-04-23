(* ::Package:: *)

(* :Context: ChallengesBenchmark`Private` *)
(* :Summary: Result aggregation, summarization, and run diffs. *)

Begin["ChallengesBenchmark`Private`"];

(* summarizeResults — one-line scoreboard for a results list. *)

(* percentilesOf — tail-latency view over a bag of numeric samples.

   Returns <|"count"->n, "mean"->m, "p50"->...,"p75"->...,"p90"->...,
            "p95"->..., "p99"->..., "max"->..., "total"->sum|>.
   When the input is empty, every statistic is 0. so callers don't have to
   branch on Missing[...] when formatting reports.

   Note: we clip each sample with Max[0., x] because LocalSubmit occasionally
   reports very slightly negative timings on the order of 1e-6s for tests
   that return instantly, and negative durations would confuse downstream
   percentile math. *)

ChallengesBenchmark`Private`percentilesOf[xs_List] := Module[{clean},
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

  (* Timing / memory distributions. We only include tests that actually
     evaluated — TimedOut / MemoryExceeded / NoSolution rows have durations
     that are either synthetic (equal to the time budget) or missing, and
     folding them into the percentile would make the p99 look like the
     time limit minus epsilon instead of describing real pass/fail latency. *)
  evaluatedResults = Select[results,
    MemberQ[{"Evaluated", "EvaluationError", "RunnerError"}, #["status"]] &
  ];
  durations  = Cases[#["durationSec"]  & /@ evaluatedResults, _?NumericQ];
  memoryUses = Cases[#["memoryBytes"] & /@ evaluatedResults, _?NumericQ];
  durationStats = ChallengesBenchmark`Private`percentilesOf[durations];
  memoryStats   = ChallengesBenchmark`Private`percentilesOf[memoryUses];

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

(* diffRunsImpl — compare two runs by testId:
     "regressions" — testIds that passed in baseline, now failing
     "fixes"       — testIds that failed in baseline, now passing
     "newTests"    — testIds present only in `new`
     "missingTests"— testIds present only in `baseline`
     "summary"     — counts of all of the above
*)

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
  newTests = Complement[nIds, bIds];
  missingTests = Complement[bIds, nIds];

  statusChanges = Association @ Map[
    # -> <|
      "before" -> bByID[#]["status"], "after" -> nByID[#]["status"],
      "passedBefore" -> bByID[#]["passed"], "passedAfter" -> nByID[#]["passed"]
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

End[];
