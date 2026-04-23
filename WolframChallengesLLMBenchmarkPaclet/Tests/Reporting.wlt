(* ::Package:: *)

(* Tests for Report.wl: WriteReport, WriteJUnitReport, and the dashboard
   helpers reachable via LiveDashboard.  These tests run a tiny benchmark
   (InProcess) to get a real run Association, then exercise the writers
   and parse their outputs back. *)

Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

$tmp = FileNameJoin[{$TemporaryDirectory,
  "wclb-tests-reporting-" <> ToString[RandomInteger[10^9]]}];
If[! DirectoryQ[$tmp],
  CreateDirectory[$tmp, CreateIntermediateDirectories -> True]];

(* Mixed-status run: Sum passes, Bad parse-errors, Missing has no solution.
   That gives the JUnit writer at least one of each major branch. *)
challenges = <|
  "Sum"     -> <|"name" -> "Sum",     "prompt" -> "addTwo"|>,
  "Bad"     -> <|"name" -> "Bad",     "prompt" -> "broken"|>,
  "Missing" -> <|"name" -> "Missing", "prompt" -> "absent"|>
|>;
testBank = <|
  "Sum" -> {
    <|"challengeName" -> "Sum",
      "input"    -> HoldComplete[addTwo[1, 2]],
      "expected" -> 3, "metadata" -> <||>|>,
    <|"challengeName" -> "Sum",
      "input"    -> HoldComplete[addTwo[10, 5]],
      "expected" -> 15, "metadata" -> <||>|>
  },
  "Bad" -> {
    <|"challengeName" -> "Bad",
      "input"    -> HoldComplete[broken[1]],
      "expected" -> 1, "metadata" -> <||>|>
  },
  "Missing" -> {
    <|"challengeName" -> "Missing",
      "input"    -> HoldComplete[absent[1]],
      "expected" -> 1, "metadata" -> <||>|>
  }
|>;
solutions = <|
  "Sum" -> <|"code" -> "addTwo[a_, b_] := a + b"|>,
  "Bad" -> <|"code" -> "this is not [[[ valid wolfram"|>
  (* Missing intentionally absent *)
|>;

mkRun[modelTag_String] :=
  JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
    challenges, testBank, solutions,
    "Model" -> modelTag,
    "IsolationMode" -> "InProcess",
    "OutputDirectory" -> $tmp,
    "TimeConstraint" -> 10];


(* ---------- WriteReport: writes all four files ------------------------ *)

VerificationTest[
  Module[{run, dir, paths},
    run = mkRun["t/report-all"];
    dir = FileNameJoin[{$tmp, "report-all"}];
    paths = JofreEspigulePons`WolframChallengesBenchmark`WriteReport[run, dir];
    {Sort @ Keys[paths],
     AllTrue[Values[paths], FileExistsQ],
     FileExistsQ @ FileNameJoin[{dir, "report.html"}],
     FileExistsQ @ FileNameJoin[{dir, "report.md"}],
     FileExistsQ @ FileNameJoin[{dir, "report.json"}],
     FileExistsQ @ FileNameJoin[{dir, "junit.xml"}]}
  ],
  {Sort @ {"html", "markdown", "json", "junit"}, True, True, True, True, True},
  TestID -> "WriteReport/writes-all-four-files"
]


(* ---------- WriteReport: creates the directory if it doesn't exist --- *)

VerificationTest[
  Module[{run, dir, paths},
    run = mkRun["t/report-mkdir"];
    dir = FileNameJoin[{$tmp, "newly-created-subdir", "deep"}];
    paths = JofreEspigulePons`WolframChallengesBenchmark`WriteReport[run, dir];
    {DirectoryQ[dir], FileExistsQ[paths["html"]]}
  ],
  {True, True},
  TestID -> "WriteReport/creates-output-directory"
]


(* ---------- WriteReport: report.json round-trips through Import ------ *)

(* renderJSON's report.json contract: top-level Association with at least
   runId / model / summary / perChallenge.  We deliberately do NOT inline
   the full results list \[LongDash] that's what results.wxf is for. *)

VerificationTest[
  Module[{run, dir, paths, j},
    run = mkRun["t/report-json"];
    dir = FileNameJoin[{$tmp, "report-json"}];
    paths = JofreEspigulePons`WolframChallengesBenchmark`WriteReport[run, dir];
    j = Import[paths["json"], "RawJSON"];
    {AssociationQ[j],
     KeyExistsQ[j, "summary"],
     KeyExistsQ[j, "perChallenge"],
     KeyExistsQ[j, "runId"],
     KeyExistsQ[j, "model"],
     j["summary", "total"]}
  ],
  {True, True, True, True, True, 4},
  TestID -> "WriteReport/json-roundtrips"
]


(* ---------- WriteJUnitReport: writes well-formed XML ----------------- *)

VerificationTest[
  Module[{run, path, xml},
    run = mkRun["t/junit-only"];
    path = FileNameJoin[{$tmp, "single-junit", "out.xml"}];
    JofreEspigulePons`WolframChallengesBenchmark`WriteJUnitReport[run, path];
    xml = Import[path, "XML"];
    (* The top-level element should be <testsuites>. *)
    {FileExistsQ[path],
     MatchQ[xml, XMLObject["Document"][_, XMLElement["testsuites", _, _], _]]}
  ],
  {True, True},
  TestID -> "WriteJUnitReport/well-formed-xml"
]


(* ---------- JUnit: status -> XML element mapping --------------------- *)
(* Sum has 2 evaluated tests (1 pass + 1 pass), Bad has 1 ParseError
   (-> error), Missing has 1 NoSolution (-> skipped).  We assert on
   counts of elements by tag to avoid coupling to attribute order. *)

VerificationTest[
  Module[{run, path, xml, cases, counts},
    run = mkRun["t/junit-mapping"];
    path = FileNameJoin[{$tmp, "junit-mapping.xml"}];
    JofreEspigulePons`WolframChallengesBenchmark`WriteJUnitReport[run, path];
    xml = Import[path, "XML"];
    cases = Cases[xml, XMLElement["testcase", _, body_] :> body, Infinity];
    counts = <|
      "skipped" -> Count[cases, {___, XMLElement["skipped", _, _], ___}],
      "error"   -> Count[cases, {___, XMLElement["error",   _, _], ___}],
      "failure" -> Count[cases, {___, XMLElement["failure", _, _], ___}],
      "passed"  -> Count[cases,
        body_ /; FreeQ[body, XMLElement["skipped" | "error" | "failure", _, _]]]
    |>;
    {Length[cases], counts["skipped"], counts["error"], counts["passed"]}
  ],
  {4, 1, 1, 2},
  TestID -> "WriteJUnitReport/status-mapping"
]


(* ---------- JUnit: testsuites name carries WolframChallengesBenchmark prefix *)

VerificationTest[
  Module[{run, path, xml, suitesAttrs},
    run = mkRun["t/junit-prefix"];
    path = FileNameJoin[{$tmp, "junit-prefix.xml"}];
    JofreEspigulePons`WolframChallengesBenchmark`WriteJUnitReport[run, path];
    xml = Import[path, "XML"];
    suitesAttrs = First @ Cases[xml,
      XMLElement["testsuites", a_, _] :> a, Infinity];
    StringStartsQ[Lookup[suitesAttrs, "name", ""],
      "WolframChallengesBenchmark."]
  ],
  True,
  TestID -> "WriteJUnitReport/suites-name-prefix"
]


(* ---------- HTML / Markdown reports: non-empty and contain run info -- *)

VerificationTest[
  Module[{run, dir, paths, html, md},
    run = mkRun["t/report-content"];
    dir = FileNameJoin[{$tmp, "report-content"}];
    paths = JofreEspigulePons`WolframChallengesBenchmark`WriteReport[run, dir];
    html = Import[paths["html"], "Text"];
    md   = Import[paths["markdown"], "Text"];
    {StringLength[html] > 100,
     StringLength[md]   > 50,
     StringContainsQ[html, "Sum"],
     StringContainsQ[md,   "Sum"]}
  ],
  {True, True, True, True},
  TestID -> "WriteReport/html-and-markdown-non-empty"
]
