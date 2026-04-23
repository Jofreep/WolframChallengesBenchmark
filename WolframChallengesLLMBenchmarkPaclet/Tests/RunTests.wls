#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ----------------------------------------------------------------------- *)
(*  RunTests.wls                                                           *)
(*                                                                         *)
(*  Drives the paclet's VerificationTest suite via TestReport and prints  *)
(*  a pass/fail scoreboard.  Intended to be run from CI:                   *)
(*                                                                         *)
(*    wolframscript -file Tests/RunTests.wls                               *)
(*                                                                         *)
(*  Options                                                                *)
(*  -------                                                                *)
(*    --include-integration   also run Integration.wlt (slow, subkernels) *)
(*    --junit PATH            emit JUnit XML to PATH                      *)
(*    --expected-failures IDS comma-separated TestIDs allowed to fail     *)
(*                            without triggering a nonzero exit.  Useful  *)
(*                            for gating CI on the delta rather than the  *)
(*                            absolute pass count while a known-failing   *)
(*                            test is pending a fix.                      *)
(*                                                                         *)
(*  Exits                                                                  *)
(*  -----                                                                  *)
(*    0  every failure is in --expected-failures (or there are none)      *)
(*    1  at least one unexpected failure or error                          *)
(* ----------------------------------------------------------------------- *)

$scriptDir  = DirectoryName[$InputFileName];
$pacletRoot = DirectoryName[$scriptDir];
PacletDirectoryLoad[$pacletRoot];
Needs["JofreEspigulePons`WolframChallengesBenchmark`"];


parseArgs[args_List] := Module[{i = 1, n = Length[args], out = <||>, k, v},
  While[i <= n,
    k = args[[i]];
    Which[
      StringStartsQ[k, "--"],
        v = If[i < n && ! StringStartsQ[args[[i + 1]], "--"],
               args[[i + 1]], "true"];
        out[StringDrop[k, 2]] = v;
        i += If[v === "true", 1, 2],
      True, i++
    ]
  ];
  out
];

args = parseArgs[Drop[$ScriptCommandLine, 1]];
includeIntegration = KeyExistsQ[args, "include-integration"];
junitPath          = Lookup[args, "junit", None];

(* --expected-failures is a comma-separated list of TestIDs that are
   allowed to fail without triggering a nonzero exit.  Whitespace around
   each ID is trimmed.  TestIDs in the list that did NOT actually fail
   in this run are reported (so the list stays honest over time).        *)
expectedFailures = StringTrim /@ StringSplit[
  Lookup[args, "expected-failures", ""], ","];
expectedFailures = DeleteCases[expectedFailures, ""];


(* ----------------------------------------------------------------------- *)
(* Collect test files in a deterministic order                             *)
(* ----------------------------------------------------------------------- *)

unitFiles = {
  "Utilities.wlt",      "Loader.wlt",       "Solutions.wlt",
  "Runner.wlt",         "Results.wlt",      "Reporting.wlt",
  "Compare.wlt",        "TestBankBuilder.wlt",
  "OpenRouter.wlt",     "Generator.wlt"
};
allFiles = If[includeIntegration,
  Append[unitFiles, "Integration.wlt"], unitFiles];

resolve[f_] := FileNameJoin[{$scriptDir, f}];


(* ----------------------------------------------------------------------- *)
(* Run each file and tally                                                 *)
(* ----------------------------------------------------------------------- *)

(* Run each .wlt file via TestReport.

   We intentionally do NOT wrap TestReport in Check[...]: Check returns
   the failure value whenever any message fires during expr's evaluation,
   regardless of Quiet, and many of our tests legitimately fire messages
   they then assert via ExpectedMessages.  Wrapping with Check turned
   every message-firing file into a spurious ERROR: TestReport failed,
   even though the underlying TestReportObject was perfectly fine.

   Instead, drop the Check, suppress the message printing with Quiet, and
   inspect the return value: TestReport returns $Failed only for
   catastrophic file-load problems (e.g. syntax errors in the .wlt file),
   so head-checking the result is sufficient.                              *)
reports = Map[
  Function[f,
    Module[{path = resolve[f], r},
      WriteString[$Output, "\n== ", f, " ==\n"];
      If[! FileExistsQ[path],
        WriteString[$Output, "SKIP: file not found\n"];
        f -> None,
        r = TestReport[path];
        If[r === $Failed || Head[r] =!= TestReportObject,
          WriteString[$Output, "ERROR: TestReport failed\n"];
          f -> $Failed,
          Module[{res = r["Results"]},
            WriteString[$Output,
              "  total:    ",  Length[res],                                "\n",
              "  passed:   ",  Count[res, t_ /; t["Outcome"] === "Success"], "\n",
              "  failed:   ",  Count[res, t_ /; t["Outcome"] === "Failure"], "\n",
              "  errored:  ",  Count[res, t_ /; t["Outcome"] === "MessagesFailure"], "\n",
              "  elapsed:  ",  NumberForm[Total[#["AbsoluteTimeUsed"] & /@ res], {5, 2}], "s\n"];

            Scan[
              If[#["Outcome"] =!= "Success",
                WriteString[$Output,
                  "    - ", #["TestID"], " [", #["Outcome"], "]\n"]] &,
              res];
            f -> r
          ]
        ]
      ]
    ]
  ],
  allFiles
];


(* ----------------------------------------------------------------------- *)
(* Aggregate                                                               *)
(* ----------------------------------------------------------------------- *)

totalPass = 0; totalFail = 0; totalErr = 0; totalSkip = 0;
Scan[
  Module[{f = First[#], r = Last[#]},
    Which[
      r === None,    totalSkip += 1,
      r === $Failed, totalErr  += 1,
      True,
        totalPass += Count[r["Results"], t_ /; t["Outcome"] === "Success"];
        totalFail += Count[r["Results"], t_ /; t["Outcome"] === "Failure"];
        totalErr  += Count[r["Results"], t_ /; t["Outcome"] === "MessagesFailure"]
    ]
  ] &,
  reports
];

(* Classify each failing/erroring test as expected vs unexpected. *)
failingIds = Flatten @ Map[
  Function[pair,
    Module[{r = Last[pair]},
      If[r === None || r === $Failed, {},
        Cases[r["Results"],
          t_ /; t["Outcome"] =!= "Success" :> ToString[t["TestID"]]]
      ]
    ]
  ],
  reports
];
expectedHit  = Intersection[failingIds, expectedFailures];
unexpected   = Complement[failingIds, expectedFailures];
staleAllowed = Complement[expectedFailures, failingIds];

WriteString[$Output,
  "\n================ SUMMARY ================\n",
  "  files:      ", Length[allFiles],     "\n",
  "  passed:     ", totalPass,             "\n",
  "  failed:     ", totalFail,             "\n",
  "  errored:    ", totalErr,              "\n",
  "  skipped:    ", totalSkip,             "\n"
];

If[expectedFailures =!= {},
  WriteString[$Output,
    "  expected:   ", Length[expectedHit],  " / ",
      Length[expectedFailures], " (allowlisted)\n",
    "  unexpected: ", Length[unexpected],   "\n"
  ];
  If[unexpected =!= {},
    WriteString[$Output, "  -- unexpected failures:\n"];
    Scan[WriteString[$Output, "     ", #, "\n"] &, unexpected]
  ];
  If[staleAllowed =!= {},
    WriteString[$Output,
      "  -- NOTE: these allowlist entries did not fail this run ",
      "(consider removing):\n"];
    Scan[WriteString[$Output, "     ", #, "\n"] &, staleAllowed]
  ];
];

WriteString[$Output, "=========================================\n"];


(* ----------------------------------------------------------------------- *)
(* Optional JUnit XML output                                               *)
(* ----------------------------------------------------------------------- *)

writeJUnit[path_String, reps_] := Module[{xml, suites},
  suites = KeyValueMap[
    Function[{file, rep},
      If[AssociationQ @ Quiet @ rep["Results"] ||
         ListQ    @ Quiet @ rep["Results"],
        Module[{res = rep["Results"]},
          "  <testsuite name=\"" <> file <>
          "\" tests=\"" <> ToString[Length[res]] <>
          "\" failures=\"" <>
            ToString[Count[res, t_ /; t["Outcome"] =!= "Success"]] <>
          "\">\n" <>
          StringJoin @ Map[
            "    <testcase classname=\"" <> file <> "\" name=\"" <>
              ToString[Lookup[#, "TestID", "?"]] <> "\">\n" <>
              If[#["Outcome"] === "Success", "",
                "      <failure message=\"" <> #["Outcome"] <>
                "\"/>\n"] <>
              "    </testcase>\n" &,
            res] <>
          "  </testsuite>\n"
        ],
        ""
      ]
    ],
    Association @@ reps
  ];
  xml = "<?xml version=\"1.0\"?>\n<testsuites>\n" <>
        StringJoin @ suites <> "</testsuites>\n";
  Export[path, xml, "Text"]
];

If[junitPath =!= None,
  WriteString[$Output, "Writing JUnit XML to ", junitPath, "...\n"];
  Quiet @ writeJUnit[junitPath,
    Select[reports, AssociationQ @ Quiet @ Last[#]["Results"] &]]
];


(* Exit nonzero iff there is at least one UNEXPECTED failure/error.
   Allowlisted failures are not counted; callers can track them in
   --expected-failures and remove them once fixed.                        *)
Exit[If[Length[unexpected] === 0, 0, 1]]
