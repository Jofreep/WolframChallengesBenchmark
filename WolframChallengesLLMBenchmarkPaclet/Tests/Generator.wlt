(* ::Package:: *)

(* Tests for GenerateSolutions: the dry-run path is deterministic and
   offline, and exercises the full pipeline including the audit gate
   and the JSONL log.  A live generator run against OpenRouter is left
   to the Live.wlt file. *)

Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

$tmp = FileNameJoin[{$TemporaryDirectory,
  "wclb-tests-generator-" <> ToString[RandomInteger[10^9]]}];
If[! DirectoryQ[$tmp],
  CreateDirectory[$tmp, CreateIntermediateDirectories -> True]];

challenges = <|
  "Sum" -> <|"name" -> "Sum", "prompt" -> "Write addTwo[a,b]."|>,
  "Sq"  -> <|"name" -> "Sq",  "prompt" -> "Write sq[x]."|>
|>;

testBank = <|
  "Sum" -> {<|"challengeName" -> "Sum",
              "input"    -> HoldComplete[addTwo[2, 3]],
              "expected" -> 5, "metadata" -> <||>|>},
  "Sq"  -> {<|"challengeName" -> "Sq",
              "input"    -> HoldComplete[sq[4]],
              "expected" -> 16, "metadata" -> <||>|>}
|>;


(* Dry-run stub doesn't define the required function -> both are rejected.
   SaveSolution::saveAudit fires once per challenge (intentionally \[LongDash]
   the audit REJECTING the dry-run stub IS the tested behavior); declare
   the expected messages explicitly so newer Wolfram kernels that
   surface them don't flag SameMessagesFailure.                          *)
VerificationTest[
  Module[{result, c},
    result = JofreEspigulePons`WolframChallengesBenchmark`GenerateSolutions[
      challenges, testBank,
      <|"Model"           -> "dry/stub",
        "OutputDirectory" -> FileNameJoin[{$tmp, "dry"}],
        "DryRun"          -> True|>];
    c = result["counts"];
    {result =!= $Failed,
     Lookup[c, "ok",            0],
     Lookup[c, "auditRejected", 0],
     Lookup[c, "failed",        0]}
  ],
  {True, 0, 2, 0},
  {JofreEspigulePons`WolframChallengesBenchmark`SaveSolution::saveAudit,
   JofreEspigulePons`WolframChallengesBenchmark`SaveSolution::saveAudit},
  TestID -> "GenerateSolutions/dry-run-rejected"
]


(* JSONL log captures the lifecycle events. *)
VerificationTest[
  Module[{outDir, logFile, lines, events},
    outDir = FileNameJoin[{$tmp, "dry-log"}];
    JofreEspigulePons`WolframChallengesBenchmark`GenerateSolutions[
      challenges, testBank,
      <|"Model" -> "dry/log", "OutputDirectory" -> outDir, "DryRun" -> True|>];
    logFile = First @ FileNames["*.jsonl", outDir];
    lines = ReadList[logFile, "String"];
    events = Lookup[Quiet @ ImportString[#, "RawJSON"], "event", None] & /@ lines;
    Sort @ DeleteDuplicates @ events
  ],
  Sort @ {"generate.start", "generate.finished",
          "challenge.prompt", "challenge.auditRejected",
          "challenge.attempt.start", "challenge.attempt.end"},
  {JofreEspigulePons`WolframChallengesBenchmark`SaveSolution::saveAudit,
   JofreEspigulePons`WolframChallengesBenchmark`SaveSolution::saveAudit},
  TestID -> "GenerateSolutions/jsonl-events"
]


(* Filter restricts which challenges are processed. *)
VerificationTest[
  Module[{result},
    result = JofreEspigulePons`WolframChallengesBenchmark`GenerateSolutions[
      challenges, testBank,
      <|"Model"           -> "dry/filter",
        "OutputDirectory" -> FileNameJoin[{$tmp, "dry-filter"}],
        "DryRun"          -> True,
        "Filter"          -> {"Sum"}|>];
    {Sort @ Keys @ result["results"],
     Lookup[result["counts"], "auditRejected", 0]}
  ],
  {{"Sum"}, 1},
  {JofreEspigulePons`WolframChallengesBenchmark`SaveSolution::saveAudit},
  TestID -> "GenerateSolutions/filter"
]


(* Filter ordering: callers expect challenges to be processed in the
   order they appear in the Filter list, NOT the order they happen to
   appear in the source Assoc.  KeySelect / KeyTake quietly preserve
   the source-Assoc order \[LongDash] this test pins the corrected
   AssociationMap-based behavior. *)
VerificationTest[
  Module[{result, jsonl, lines, names, firstAppearances},
    result = JofreEspigulePons`WolframChallengesBenchmark`GenerateSolutions[
      challenges, testBank,
      <|"Model"           -> "dry/filter-order",
        "OutputDirectory" -> FileNameJoin[{$tmp, "dry-filter-order"}],
        "DryRun"          -> True,
        (* challenges Assoc is keyed Sum-then-Sq.  Ask for the reverse. *)
        "Filter"          -> {"Sq", "Sum"}|>];
    jsonl = First @ FileNames["*.jsonl", result["outDir"]];
    lines = ReadList[jsonl, "String"];
    names = DeleteCases[
      Lookup[Quiet @ ImportString[#, "RawJSON"], "name", None] & /@ lines,
      None];
    (* Multiple name-bearing events per challenge (prompt, attempt.start,
       attempt.end, auditRejected).  Reduce to first-appearance order. *)
    firstAppearances = DeleteDuplicates[names];
    (* Keys[result["results"]] should reflect processing order; the
       JSONL log should show prompts emitted in caller-supplied order. *)
    {Keys @ result["results"], firstAppearances}
  ],
  {{"Sq", "Sum"}, {"Sq", "Sum"}},
  {JofreEspigulePons`WolframChallengesBenchmark`SaveSolution::saveAudit,
   JofreEspigulePons`WolframChallengesBenchmark`SaveSolution::saveAudit},
  TestID -> "GenerateSolutions/filter-preserves-order"
]


(* Heartbeat: per-attempt challenge.attempt.start / challenge.attempt.end
   rows land in the JSONL so tail -f shows a slow run is alive. *)
VerificationTest[
  Module[{result, jsonl, lines, events, startsForSum, endsForSum},
    result = JofreEspigulePons`WolframChallengesBenchmark`GenerateSolutions[
      challenges, testBank,
      <|"Model"           -> "dry/heartbeat",
        "OutputDirectory" -> FileNameJoin[{$tmp, "dry-heartbeat"}],
        "DryRun"          -> True,
        "Filter"          -> {"Sum"}|>];
    jsonl = First @ FileNames["*.jsonl", result["outDir"]];
    lines = ReadList[jsonl, "String"];
    events = Quiet @ ImportString[#, "RawJSON"] & /@ lines;
    startsForSum = Select[events,
      AssociationQ[#] && Lookup[#, "event", ""] === "challenge.attempt.start" &&
        Lookup[#, "name", ""] === "Sum" &];
    endsForSum = Select[events,
      AssociationQ[#] && Lookup[#, "event", ""] === "challenge.attempt.end" &&
        Lookup[#, "name", ""] === "Sum" &];
    {Length[startsForSum] >= 1, Length[endsForSum] >= 1,
     (* The end row must carry a duration and a status. *)
     KeyExistsQ[First[endsForSum], "durationSec"],
     StringQ @ Lookup[First[endsForSum], "status", None]}
  ],
  {True, True, True, True},
  {JofreEspigulePons`WolframChallengesBenchmark`SaveSolution::saveAudit},
  TestID -> "GenerateSolutions/attempt-heartbeats"
]


(* Tombstone on abort: if the driver loop is interrupted partway
   through (Ctrl-C, an Exit triggered by an outer handler, a Throw
   escaping a tagged Catch the caller forgot to install, etc.), a
   generate.aborted row still lands so logs aren't silently truncated.

   We simulate the interrupt with a Generator that throws to a tagged
   Catch around the GenerateSolutions call.  Why Throw and not Abort:
   the per-attempt envelope wraps `gen[prompt]` in CheckAbort to recover
   from URLRead aborts on flaky TCP \[LongDash] so an Abort[] from the
   Generator is *correctly* swallowed, retried, and recorded as
   status="llm-aborted" without ever reaching the WithLocalSettings
   cleanup that emits the tombstone.  CheckAbort does NOT catch Throw,
   so a Throw propagates through processOneChallenge, MapIndexed, and
   triggers the cleanup body (which writes the generate.aborted row),
   then bubbles up to the test's outer Catch. *)
VerificationTest[
  Module[{outDir, result, jsonl, lines, events, aborted, finished},
    outDir = FileNameJoin[{$tmp, "dry-abort"}];
    result = Catch[
      JofreEspigulePons`WolframChallengesBenchmark`GenerateSolutions[
        challenges, testBank,
        <|"Model"           -> "dry/abort",
          "OutputDirectory" -> outDir,
          "DryRun"          -> False,
          "Generator"       ->
            Function[prompt, Throw["interrupted", "test-abort"]]|>],
      "test-abort"];
    jsonl = First @ FileNames["*.jsonl", outDir];
    lines = ReadList[jsonl, "String"];
    events = DeleteCases[Quiet @ ImportString[#, "RawJSON"] & /@ lines, $Failed];
    aborted  = Select[events,
      AssociationQ[#] && Lookup[#, "event", ""] === "generate.aborted" &];
    finished = Select[events,
      AssociationQ[#] && Lookup[#, "event", ""] === "generate.finished" &];
    {result === "interrupted", Length[aborted] === 1, Length[finished] === 0,
     (* The tombstone identifies the challenge that was in flight. *)
     StringQ @ Lookup[First[aborted], "lastName", None]}
  ],
  {True, True, True, True},
  TestID -> "GenerateSolutions/aborted-tombstone"
]
