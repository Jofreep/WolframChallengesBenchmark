#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Run the harness self-tests and report. Exits non-zero on any failure. *)

$scriptDir = DirectoryName[$InputFileName];
$repoRoot  = DirectoryName[$scriptDir];

PrependTo[$Path, $repoRoot];

report = TestReport[FileNameJoin[{$scriptDir, "BenchmarkTests.wlt"}]];

WriteString[$Output, "Tests passed: ", report["TestsSucceededCount"],
  " / ", report["TestsSucceededCount"] + report["TestsFailedCount"], "\n"];

If[report["TestsFailedCount"] > 0,
  WriteString[$Output, "Failures:\n"];
  Scan[
    Function[r,
      WriteString[$Output, "  ", r["TestID"], ": ",
        ToString[r["Outcome"]], "\n"]
    ],
    Values @ report["TestsFailed"]
  ];
  Exit[1]
];
Exit[0]
