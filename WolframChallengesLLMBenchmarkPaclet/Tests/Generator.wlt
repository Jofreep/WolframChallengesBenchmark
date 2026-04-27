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


(* Regression test: when SaveSolution refuses to write because the
   audit fails, the JSONL audit-rejection record must carry enough
   information to triage the rejection without re-running the audit
   by hand.  Specifically:
     - definedNames, expectedNames, auditOk diagnostic fields
     - extractedDumpPath pointing at a .audit-rejected.raw.txt sidecar
       file containing the FULL extracted source (so the JSONL stays
       readable when the LLM emits multi-KB pathological responses)
     - extractedPreview field with the first 1 KB inline (or the full
       text if it's small)
     - NO "extracted" field bloating the JSONL with the full body

   Triggered by the same dry-run path as the test above: the stub
   generator emits "(* dry-run *)" which doesn't define addTwo/sq,
   so the audit gate rejects both. *)
VerificationTest[
  Module[{outDir, lines, events, rec, dumpPath},
    outDir = FileNameJoin[{$tmp, "dry-audit-diag"}];
    JofreEspigulePons`WolframChallengesBenchmark`GenerateSolutions[
      challenges, testBank,
      <|"Model"           -> "dry/audit-diag",
        "OutputDirectory" -> outDir,
        "DryRun"          -> True|>];
    lines = ReadList[First @ FileNames["*.jsonl", outDir], "String"];
    events = DeleteCases[Quiet @ ImportString[#, "RawJSON"] & /@ lines, $Failed];
    rec = First @ Select[events,
      Lookup[#, "event", ""] === "challenge.auditRejected" &];
    dumpPath = Lookup[rec, "extractedDumpPath", None];
    {
      (* Diagnostic fields are present and well-typed. *)
      KeyExistsQ[rec, "definedNames"]    && ListQ[rec["definedNames"]],
      KeyExistsQ[rec, "expectedNames"]   && ListQ[rec["expectedNames"]]
                                          && rec["expectedNames"] =!= {},
      KeyExistsQ[rec, "auditOk"]         && rec["auditOk"] === False,
      (* The full extracted lives in a sidecar, not in the JSONL. *)
      KeyExistsQ[rec, "extractedDumpPath"] && StringQ[dumpPath]
                                            && FileExistsQ[dumpPath],
      KeyExistsQ[rec, "extractedPreview"] && StringQ[rec["extractedPreview"]],
      (* Old "extracted" field is gone (was bloating multi-KB rows). *)
      ! KeyExistsQ[rec, "extracted"]
    }
  ],
  {True, True, True, True, True, True},
  {JofreEspigulePons`WolframChallengesBenchmark`SaveSolution::saveAudit,
   JofreEspigulePons`WolframChallengesBenchmark`SaveSolution::saveAudit},
  TestID -> "GenerateSolutions/audit-rejection-carries-diagnostic"
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


(* CheckAbort hardening on the per-attempt envelope.

   The per-attempt path at Generator.wl:194-200 wraps `gen[prompt]` in
   CheckAbort to recover from URLRead aborts on flaky TCP connections
   (observed on macOS).  Without this, one bad HTTPS call would Abort
   the whole multi-challenge run even though the per-attempt envelope
   already has a TimeConstrained safety net.

   This test locks in the recovery behavior so a future refactor can't
   silently drop the CheckAbort.  We use Abort[] (not Throw) because
   Abort is exactly what CheckAbort catches \[LongDash] this is the
   /complementary/ case to the tombstone test above:

     - Throw  -> escapes CheckAbort  -> tombstone fires (above test)
     - Abort  -> caught by CheckAbort -> retried, run completes
                                          (this test)

   We assert the full recovery chain:
     1. The whole call returns normally (no abort propagates).
     2. Each per-attempt envelope records status="aborted".
     3. Each challenge after maxAttempts retries records status="llm-aborted".
     4. The run completes with generate.finished, NOT generate.aborted
        (the recovery layer's whole point is the run keeps going).
     5. The driver visits both challenges (it doesn't bail after the
        first abort).                                                   *)
VerificationTest[
  Module[{outDir, result, jsonl, lines, events, attemptEnds, challengeEnds,
          finished, aborted, perChallenge},
    outDir = FileNameJoin[{$tmp, "abort-recovers"}];
    result = JofreEspigulePons`WolframChallengesBenchmark`GenerateSolutions[
      challenges, testBank,
      <|"Model"           -> "dry/abort-recovers",
        "OutputDirectory" -> outDir,
        "DryRun"          -> False,
        "MaxAttempts"     -> 2,    (* faster than the default 3 *)
        "RetryBaseDelay"  -> 0,    (* no exponential backoff in tests *)
        "Generator"       -> Function[prompt, Abort[]]|>];

    jsonl  = First @ FileNames["*.jsonl", outDir];
    lines  = ReadList[jsonl, "String"];
    events = DeleteCases[Quiet @ ImportString[#, "RawJSON"] & /@ lines, $Failed];

    attemptEnds   = Select[events, Lookup[#, "event", ""] === "challenge.attempt.end" &];
    challengeEnds = Select[events, Lookup[#, "event", ""] === "challenge.failed" &];
    finished      = Select[events, Lookup[#, "event", ""] === "generate.finished" &];
    aborted       = Select[events, Lookup[#, "event", ""] === "generate.aborted" &];
    perChallenge  = result["results"];

    {
      (* (1) Whole call returned normally. *)
      AssociationQ[result],
      (* (2) Each attempt env recorded status="aborted". *)
      AllTrue[attemptEnds, Lookup[#, "status", ""] === "aborted" &],
      (* (3) Each challenge ended as llm-aborted after retries. *)
      AllTrue[Values[perChallenge], #["status"] === "llm-aborted" &],
      (* (4) Tombstone is generate.finished, NOT generate.aborted. *)
      Length[finished] === 1, Length[aborted] === 0,
      (* (5) Driver visited BOTH challenges (didn't stop after the first). *)
      Sort @ Keys[perChallenge]
    }
  ],
  {True, True, True, True, True, Sort @ {"Sum", "Sq"}},
  TestID -> "GenerateSolutions/abort-recovers-via-CheckAbort"
]
