(* ::Package:: *)

(* :Title: ChallengesBenchmark *)
(* :Context: ChallengesBenchmark` *)
(* :Summary:
     Production-ready harness for running LLM-generated Wolfram Language
     solutions against the Wolfram Challenges test bank.

     Public entry points:
       LoadChallenges         — parse & validate challenge prompt file
       LoadTestBank           — parse & validate expected-output file
       LoadSolutions          — read per-model solutions from disk
       SaveSolution           — write a single solution to disk
       ExtractCode            — strip markdown fences from LLM output
       AuditSolutions         — pre-flight check of a solutions directory
       GenerateSolutions      — LLM-driven generator with retry/timeout/JSONL log
       RunBenchmark           — run a full benchmark, returns a RunObject
       WriteReport            — render HTML + Markdown + JSON + JUnit from a RunObject
       WriteJUnitReport       — render only a JUnit XML file (CI integration)
       DiffRuns               — structural diff between two runs
       CompareModels          — cross-model comparison across run directories
       WriteCompareReport     — render compare.md / compare.html from a compare object
       LoadWLChallenge        — parse one .wlchallenge plain-text source file
       LoadChallengesDir      — load all .wlchallenge files from a directory
       BuildTestBank          — build {challengesAssoc, testBankAssoc} from a dir
       WriteTestBankFiles     — emit challenges.json + testbank.wxf from a dir
       WriteWLChallengeDir    — seed a .wlchallenge/ dir from an existing bank
       $BenchmarkDefaults     — default options
*)

BeginPackage["ChallengesBenchmark`"];

(* ----------------------------------------------------------------------- *)
(* Public symbols                                                          *)
(* ----------------------------------------------------------------------- *)

LoadChallenges::usage =
  "LoadChallenges[path] imports and validates a challenge-prompts JSON file.";

LoadTestBank::usage =
  "LoadTestBank[path] imports and validates the expected-output WXF file.";

LoadSolutions::usage =
  "LoadSolutions[dir] reads all <challenge>.wl solutions from a model directory.";

SaveSolution::usage =
  "SaveSolution[dir, challengeName, code] writes a single solution to disk. SaveSolution[dir, challengeName, code, testBank] additionally audits the code against the test bank and refuses to write (returns $Failed) if the code does not define any of the expected function names. SaveSolution[dir, challengeName, code, testBank, extraMeta] merges extraMeta (an Association) into the generated .meta.json sidecar after the built-in fields.";

GenerateSolutions::usage =
  "GenerateSolutions[challenges, testBank, opts] runs an LLM-backed solution generator: it iterates the challenges, calls the injected \"Generator\" function (default: a closure over LLMSynthesize driven by \"LLMEvaluator\") under a per-call TimeConstraint with bounded exponential-backoff retry, strips the reply with ExtractCode, audits each extracted code against the test bank via SaveSolution, and writes a JSONL audit log of every prompt/response pair to \"LogPath\". Returns an Association with keys runId, model, outDir, logPath, counts, results, startedAt, finishedAt.";

ExtractCode::usage =
  "ExtractCode[text] returns the Wolfram Language code block from an LLM response.";

AuditSolutions::usage =
  "AuditSolutions[dir, testBank] audits a solutions directory, structurally parsing each .wl file and checking it defines the function expected by the test bank. Returns an Association summarising matches, mismatches, missing challenges, unexpected files, and parse failures. Parsing is held: files are never evaluated.";

RunBenchmark::usage =
  "RunBenchmark[challenges, testBank, solutions, opts] runs the benchmark and returns a RunObject.";

WriteReport::usage =
  "WriteReport[run, dir] renders report.html, report.md, report.json and junit.xml for a completed run.";

WriteJUnitReport::usage =
  "WriteJUnitReport[run, path] renders a JUnit-format XML file for `run` at `path` so CI systems can ingest per-test pass/fail/skip metrics.";

DiffRuns::usage =
  "DiffRuns[baseline, new] returns regressions, fixes and unchanged tests between two runs.";

CompareModels::usage =
  "CompareModels[{runDir1, runDir2, ...}] loads run.json and results.wxf from each run directory and returns a cross-model comparison object with headline numbers, a per-challenge pass matrix, and uniquely-passed / uniquely-failed lists per model. Accepts an Association \"model\" -> dir to override the model labels.";

WriteCompareReport::usage =
  "WriteCompareReport[compare, dir] renders compare.md and compare.html from the result of CompareModels.";

LoadWLChallenge::usage =
  "LoadWLChallenge[path] parses one .wlchallenge plain-text file and returns an Association with keys name, index, instruction, prompt, tests. Test inputs are returned as HoldComplete[input] to preserve laziness.";

LoadChallengesDir::usage =
  "LoadChallengesDir[dir] loads all *.wlchallenge files from a directory, returning an Association name -> entry sorted by :Index:.";

BuildTestBank::usage =
  "BuildTestBank[dir] reads .wlchallenge files from a directory and returns the pair {challengesAssoc, testBankAssoc} matching the runtime shape consumed by RunBenchmark.";

WriteTestBankFiles::usage =
  "WriteTestBankFiles[dir, jsonOut, wxfOut] reads .wlchallenge files from a directory and emits the legacy challenges JSON and test bank WXF files.";

WriteWLChallengeDir::usage =
  "WriteWLChallengeDir[challenges, testBank, dir] emits one .wlchallenge file per challenge into dir, suitable for editing and re-building.";

$BenchmarkDefaults::usage =
  "$BenchmarkDefaults is the association of default benchmark options.";

(* Option symbols (exposed so callers don't need to quote strings) *)
TimeConstraint;
MemoryConstraint;
Parallel;
RunId;
OutputDirectory;
Filter;
ProgressHandler;
SameTestFunction;
Model;
Seed;
Sandbox;

Begin["`Private`"];

(* Needs — internal files *)
$thisDir = DirectoryName[$InputFileName];

Get[FileNameJoin[{$thisDir, "Utilities.wl"}]];
Get[FileNameJoin[{$thisDir, "Loader.wl"}]];
Get[FileNameJoin[{$thisDir, "Solutions.wl"}]];
Get[FileNameJoin[{$thisDir, "Runner.wl"}]];
Get[FileNameJoin[{$thisDir, "Results.wl"}]];
Get[FileNameJoin[{$thisDir, "Report.wl"}]];
Get[FileNameJoin[{$thisDir, "Compare.wl"}]];
Get[FileNameJoin[{$thisDir, "TestBankBuilder.wl"}]];
Get[FileNameJoin[{$thisDir, "Generator.wl"}]];

(* ----------------------------------------------------------------------- *)
(* Defaults                                                                *)
(* ----------------------------------------------------------------------- *)

$BenchmarkDefaults = <|
  "TimeConstraint"    -> 60,                (* seconds per test *)
  "MemoryConstraint"  -> 2*^9,              (* 2 GB per test *)
  "Parallel"          -> Automatic,         (* Automatic => $ProcessorCount-1, or an integer *)
  "RunId"             -> Automatic,
  "OutputDirectory"   -> Automatic,         (* Automatic => ./runs *)
  "Filter"            -> All,               (* All or a list of challenge names *)
  "ProgressHandler"   -> None,              (* None or Function[assoc, ...] *)
  "SameTestFunction"  -> Automatic,         (* Automatic dispatches on metadata *)
  "Model"             -> None,              (* required; label for this run *)
  "Seed"              -> Automatic,         (* Automatic => derived from RunId *)
  "IsolationMode"     -> "PerTestKernel",   (* "PerTestKernel" | "PooledKernels" | "InProcess" *)
  "RetryOnKernelDeath" -> 1,
  "PollInterval"      -> 0.05,              (* seconds between bag drains *)
  "Sandbox"           -> True               (* block filesystem/process/network calls *)
|>;

(* ----------------------------------------------------------------------- *)
(* Public API thin wrappers                                                *)
(* ----------------------------------------------------------------------- *)

LoadChallenges[path_String] := loadChallengesImpl[path];
LoadChallenges[path_]       := (Message[LoadChallenges::badarg, path]; $Failed);
LoadChallenges::badarg = "LoadChallenges expects a file path string; got `1`.";

LoadTestBank[path_String]   := loadTestBankImpl[path];
LoadTestBank[path_]         := (Message[LoadTestBank::badarg, path]; $Failed);
LoadTestBank::badarg = "LoadTestBank expects a file path string; got `1`.";

LoadSolutions[dir_String]   := loadSolutionsImpl[dir];
SaveSolution[dir_String, name_String, code_String] :=
  saveSolutionImpl[dir, name, code];
SaveSolution[dir_String, name_String, code_String, testBank_Association] :=
  saveSolutionImpl[dir, name, code, testBank];
SaveSolution[dir_String, name_String, code_String, None] :=
  saveSolutionImpl[dir, name, code, None];
SaveSolution[dir_String, name_String, code_String, testBank_Association,
  extraMeta_Association] :=
  saveSolutionImpl[dir, name, code, testBank, extraMeta];
SaveSolution[dir_String, name_String, code_String, None, extraMeta_Association] :=
  saveSolutionImpl[dir, name, code, None, extraMeta];
ExtractCode[text_String]    := extractCodeImpl[text];

(* Generator options are passed as a single Association (rules auto-converted),
   rather than OptionsPattern, so callers can compose option sets at runtime
   and CLI scripts can thread JSON-parsed tables through unchanged. *)
GenerateSolutions[challenges_Association, testBank_Association,
  opts_Association] :=
  generateSolutionsImpl[challenges, testBank, opts];
GenerateSolutions[challenges_Association, testBank_Association,
  opts : (_Rule | _RuleDelayed) ..] :=
  generateSolutionsImpl[challenges, testBank, Association[opts]];
GenerateSolutions[challenges_Association, testBank_Association,
  opts : {(_Rule | _RuleDelayed) ...}] :=
  generateSolutionsImpl[challenges, testBank, Association[opts]];
GenerateSolutions[challenges_Association, testBank_Association] :=
  generateSolutionsImpl[challenges, testBank, <||>];

AuditSolutions[dir_String, testBank_Association] := auditSolutionsImpl[dir, testBank];
AuditSolutions[dir_, testBank_] :=
  (Message[AuditSolutions::badarg, dir, testBank]; $Failed);
AuditSolutions::badarg =
  "AuditSolutions expects (dir_String, testBank_Association); got (`1`, `2`).";

Options[RunBenchmark] = Normal[$BenchmarkDefaults];

RunBenchmark[challenges_Association, testBank_Association, solutions_Association,
  opts : OptionsPattern[]] :=
  runBenchmarkImpl[challenges, testBank, solutions, opts];

WriteReport[run_Association, dir_String] := writeReportImpl[run, dir];

WriteJUnitReport[run_Association, path_String] := writeJUnitReportImpl[run, path];

DiffRuns[baseline_Association, new_Association] := diffRunsImpl[baseline, new];

CompareModels[runs_List] := compareModelsImpl[runs];
CompareModels[runs_Association] := compareModelsImpl[runs];
CompareModels[parent_String, Automatic] := Module[{dirs},
  dirs = FileNames["run-*", parent];
  If[dirs === {},
    logWarn["CompareModels: no run-* directories under " <> parent];
    $Failed,
    compareModelsImpl[dirs]
  ]
];

WriteCompareReport[cmp_Association, dir_String] := writeCompareReportImpl[cmp, dir];

LoadWLChallenge[path_String]     := loadWLChallengeImpl[path];
LoadChallengesDir[dir_String]    := loadChallengesDirImpl[dir];
BuildTestBank[dir_String]        := buildTestBankImpl[dir];
WriteTestBankFiles[dir_String, jsonOut_String, wxfOut_String] :=
  writeTestBankFilesImpl[dir, jsonOut, wxfOut];
WriteWLChallengeDir[challenges_Association, testBank_Association, dir_String] :=
  writeWLChallengeDirImpl[challenges, testBank, dir];

End[];    (* `Private` *)
EndPackage[];
