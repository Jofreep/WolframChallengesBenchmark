(* ::Package:: *)

(* Tests for SaveSolution: write-time audit gate, file shape, meta merge. *)

Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

$tmp = FileNameJoin[{$TemporaryDirectory,
  "wclb-tests-solutions-" <> ToString[RandomInteger[10^9]]}];
If[! DirectoryQ[$tmp],
  CreateDirectory[$tmp, CreateIntermediateDirectories -> True]];

testBank = <|
  "Sum" -> {<|"challengeName" -> "Sum",
              "input"         -> HoldComplete[addTwo[2, 3]],
              "expected"      -> 5,
              "metadata"      -> <||>|>}
|>;


(* SaveSolution writes when the candidate defines the expected fn. *)
VerificationTest[
  Module[{r, wlPath, metaPath},
    r = JofreEspigulePons`WolframChallengesBenchmark`SaveSolution[
      $tmp, "Sum", "addTwo[a_, b_] := a + b", testBank,
      <|"model" -> "smoke", "extra" -> "y"|>];
    wlPath   = FileNameJoin[{$tmp, "Sum.wl"}];
    metaPath = FileNameJoin[{$tmp, "Sum.meta.json"}];
    {r =!= $Failed,
     FileExistsQ[wlPath],
     FileExistsQ[metaPath],
     StringContainsQ[Import[wlPath, "Text"], "addTwo"],
     Lookup[Import[metaPath, "RawJSON"], "extra", None]}
  ],
  {True, True, True, True, "y"},
  TestID -> "SaveSolution/audit-pass"
]


(* SaveSolution refuses when the candidate for a known challenge name
   does not define the expected fn.  The challenge name ("Sum") must
   match a key in testBank for the audit gate to have anything to run. *)
VerificationTest[
  Module[{r, wlPath, subdir, solDir},
    (* Use a fresh subdir so a prior Sum.wl from the happy-path test
       does not falsely succeed the exists-check. *)
    subdir = FileNameJoin[{$tmp, "rejected"}];
    CreateDirectory[subdir, CreateIntermediateDirectories -> True];
    r = JofreEspigulePons`WolframChallengesBenchmark`SaveSolution[
      subdir, "Sum", "noAddTwo[x_] := x + 1", testBank];
    wlPath = FileNameJoin[{subdir, "Sum.wl"}];
    {r, FileExistsQ[wlPath]}
  ],
  {$Failed, False},
  {JofreEspigulePons`WolframChallengesBenchmark`SaveSolution::saveAudit},
  TestID -> "SaveSolution/audit-rejects-when-fn-missing"
]


(* SaveSolution with no test bank still writes (audit is a no-op). *)
VerificationTest[
  Module[{r, wlPath},
    r = JofreEspigulePons`WolframChallengesBenchmark`SaveSolution[
      $tmp, "Free", "anyOldThing[x_] := x"];
    wlPath = FileNameJoin[{$tmp, "Free.wl"}];
    {r =!= $Failed, FileExistsQ[wlPath]}
  ],
  {True, True},
  TestID -> "SaveSolution/no-testbank-skips-audit"
]


(* ---------- LoadSolutions: happy-path round-trip ---------- *)

(* Write two solutions with SaveSolution, read them back with
   LoadSolutions, and verify both the runtime shape RunBenchmark needs
   ("code") and the optional metadata surface ("meta", "wlPath",
   "metaPath") all come through.  Uses a fresh subdir so we don't pick
   up stray files from the SaveSolution tests above. *)
VerificationTest[
  Module[{subdir, loaded, sumEntry, freeEntry},
    subdir = FileNameJoin[{$tmp, "loadsolutions-roundtrip"}];
    CreateDirectory[subdir, CreateIntermediateDirectories -> True];
    JofreEspigulePons`WolframChallengesBenchmark`SaveSolution[
      subdir, "Sum", "addTwo[a_, b_] := a + b", testBank,
      <|"model" -> "smoke", "extra" -> "y"|>];
    JofreEspigulePons`WolframChallengesBenchmark`SaveSolution[
      subdir, "Free", "anyOldThing[x_] := x"];
    loaded    = JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions[subdir];
    sumEntry  = Lookup[loaded, "Sum",  None];
    freeEntry = Lookup[loaded, "Free", None];
    {AssociationQ[loaded],
     Sort @ Keys[loaded],
     StringContainsQ[sumEntry["code"],  "addTwo"],
     StringContainsQ[freeEntry["code"], "anyOldThing"],
     AssociationQ[sumEntry["meta"]],
     Lookup[sumEntry["meta"], "extra", None],
     StringQ[sumEntry["wlPath"]],
     StringQ[sumEntry["metaPath"]]}
  ],
  {True, {"Free", "Sum"}, True, True, True, "y", True, True},
  TestID -> "LoadSolutions/round-trip"
]


(* ---------- LoadSolutions: returns empty Assoc on empty dir ---------- *)

(* No .wl files = empty Association; not an error.  This matches how
   RunBenchmark handles a model that has zero saved candidates: every
   challenge grades as NoSolution.                                     *)
VerificationTest[
  Module[{subdir, loaded},
    subdir = FileNameJoin[{$tmp, "loadsolutions-empty"}];
    CreateDirectory[subdir, CreateIntermediateDirectories -> True];
    loaded = JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions[subdir];
    {AssociationQ[loaded], Length[loaded]}
  ],
  {True, 0},
  TestID -> "LoadSolutions/empty-directory"
]


(* ---------- LoadSolutions: missing sidecar surfaces Missing ---------- *)

(* A .wl file with no sibling .meta.json should load fine, with metaPath
   set to Missing["NoSidecar"] and meta set to <||>.  This matches the
   behavior of hand-placed solutions and migrated legacy data.        *)
VerificationTest[
  Module[{subdir, wlPath, loaded, entry},
    subdir = FileNameJoin[{$tmp, "loadsolutions-no-sidecar"}];
    CreateDirectory[subdir, CreateIntermediateDirectories -> True];
    wlPath = FileNameJoin[{subdir, "Bare.wl"}];
    Export[wlPath, "bareFn[x_] := x", "Text"];
    loaded = JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions[subdir];
    entry  = Lookup[loaded, "Bare", None];
    {AssociationQ[entry],
     entry["code"],
     entry["meta"],
     entry["metaPath"]}
  ],
  {True, "bareFn[x_] := x", <||>, Missing["NoSidecar"]},
  TestID -> "LoadSolutions/missing-sidecar"
]


(* ---------- LoadSolutions: nonexistent dir errors ---------- *)

VerificationTest[
  JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions[
    FileNameJoin[{$tmp, "definitely-does-not-exist-xyzzy"}]],
  $Failed,
  {JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions::nodir},
  TestID -> "LoadSolutions/missing-directory"
]


(* ---------- LoadSolutions: bad arg type errors ---------- *)

VerificationTest[
  JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions[42],
  $Failed,
  {JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions::badarg},
  TestID -> "LoadSolutions/bad-arg"
]


(* ---------- AuditSolutions: nonexistent dir returns issue record ---- *)

VerificationTest[
  Module[{r},
    r = Quiet @ JofreEspigulePons`WolframChallengesBenchmark`AuditSolutions[
      FileNameJoin[{$tmp, "audit-nonexistent"}], testBank];
    {AssociationQ[r], r["ok"], Lookup[r, "error", None]}
  ],
  {True, False, "directory not found"},
  TestID -> "AuditSolutions/nonexistent-dir"
]


(* ---------- AuditSolutions: happy path ---- *)

VerificationTest[
  Module[{subdir, r},
    subdir = FileNameJoin[{$tmp, "audit-happy"}];
    CreateDirectory[subdir, CreateIntermediateDirectories -> True];
    Export[FileNameJoin[{subdir, "Sum.wl"}],
      "addTwo[a_, b_] := a + b", "Text"];
    r = JofreEspigulePons`WolframChallengesBenchmark`AuditSolutions[subdir,
      testBank];
    {r["ok"], r["okCount"], r["issueCount"], r["missing"],
     r["unexpected"], r["mismatches"],
     r["byChallenge", "Sum", "status"]}
  ],
  {True, 1, 0, {}, {}, {}, "ok"},
  TestID -> "AuditSolutions/happy-path"
]


(* ---------- AuditSolutions: mislabeled solution is flagged ---------- *)
(* Sum.wl defines a function that does NOT match the bank's expected
   addTwo.  This is the exact failure mode AuditSolutions was built to
   catch (code for one challenge ending up in another challenge's file). *)

VerificationTest[
  Module[{subdir, r, mm},
    subdir = FileNameJoin[{$tmp, "audit-mislabeled"}];
    CreateDirectory[subdir, CreateIntermediateDirectories -> True];
    Export[FileNameJoin[{subdir, "Sum.wl"}],
      "someOtherFn[x_] := x^2", "Text"];
    r = JofreEspigulePons`WolframChallengesBenchmark`AuditSolutions[subdir,
      testBank];
    mm = First[r["mismatches"]];
    {r["ok"], r["issueCount"], mm["name"], mm["defined"], mm["expected"]}
  ],
  {False, 1, "Sum", {"someOtherFn"}, {"addTwo"}},
  TestID -> "AuditSolutions/mislabeled-solution"
]


(* ---------- AuditSolutions: missing + unexpected surfaced ---------- *)

VerificationTest[
  Module[{subdir, r, tb2},
    subdir = FileNameJoin[{$tmp, "audit-missing"}];
    CreateDirectory[subdir, CreateIntermediateDirectories -> True];
    Export[FileNameJoin[{subdir, "Extra.wl"}],
      "extraFn[x_] := x", "Text"];
    tb2 = <|
      "Sum" -> {<|"challengeName" -> "Sum",
                  "input" -> HoldComplete[addTwo[1, 1]],
                  "expected" -> 2, "metadata" -> <||>|>}
    |>;
    r = JofreEspigulePons`WolframChallengesBenchmark`AuditSolutions[subdir,
      tb2];
    {r["ok"], r["missing"], r["unexpected"]}
  ],
  {False, {"Sum"}, {"Extra"}},
  TestID -> "AuditSolutions/missing-and-unexpected"
]
