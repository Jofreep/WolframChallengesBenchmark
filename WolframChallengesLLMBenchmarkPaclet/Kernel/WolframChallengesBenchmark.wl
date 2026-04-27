(* ::Package:: *)

(* :Title:   WolframChallengesBenchmark                                     *)
(* :Context: JofreEspigulePons`WolframChallengesBenchmark`                  *)
(* :Author:  Jofre Espigule-Pons                                            *)
(* :Summary:
     Top-level entry point for the paclet.  Declares every public symbol
     and its ::usage string, then Gets the private sub-packages that
     actually implement the behavior.  Sub-packages re-open the
     `Private`` context of this paclet so all implementation-side
     functions live in one namespace and can call one another without
     any cross-file forward declaration.
*)
(* :License: MIT *)

BeginPackage["JofreEspigulePons`WolframChallengesBenchmark`"];


(* ------------------------------------------------------------------ *)
(* Public symbols                                                     *)
(* ------------------------------------------------------------------ *)

BenchmarkVersion::usage =
"BenchmarkVersion[] returns the installed paclet's version as a String.";

OpenRouterChatComplete::usage =
"OpenRouterChatComplete[messages, opts] issues a Chat Completions call \
to OpenRouter's /api/v1/chat/completions endpoint and returns an \
Association with keys \"status\", \"content\", \"httpStatus\", \
\"latencySec\", \"usage\", \"generationId\", \"finishReason\", and \
\"rawResponse\".  The API key is read from the OPENROUTER_API_KEY \
environment variable only \[LongDash] no other key source is consulted.";

GenerateSolutions::usage =
"GenerateSolutions[challenges, testBank, opts] generates candidate \
Wolfram Language solutions for each challenge by calling the configured \
LLM backend (OpenRouter by default), auditing every result against the \
test bank, and writing solutions/<modelSlug>/<name>.wl together with a \
sibling meta.json.  A JSONL audit log is appended for every attempt.";

SaveSolution::usage =
"SaveSolution[dir, name, code] writes a candidate solution to \
<dir>/<name>.wl and a sibling meta.json, refusing to write if the \
audit against the supplied test bank fails.";

AuditSolutions::usage =
"AuditSolutions[dir, testBank] audits a solutions directory, \
structurally parsing each .wl file (without ever evaluating it) and \
checking that it defines the function expected by the test bank.  \
Returns an Association summarising matches, mismatches, missing \
challenges, unexpected files, and parse failures.  Use this as a \
pre-flight check before RunBenchmark to catch mislabeled solutions \
that would otherwise waste a full test run.";

AuditSolutions::badarg =
"AuditSolutions expects (dir_String, testBank_Association); got (`1`, `2`).";

LoadSolutions::usage =
"LoadSolutions[modelDir] reads every <name>.wl under modelDir and \
returns an Association of name -> <|\"code\" -> source, \"wlPath\" -> \
path, \"metaPath\" -> path or Missing, \"meta\" -> sidecar Assoc|> \
suitable for passing as the third argument of RunBenchmark.  Sibling \
.meta.json files are surfaced under \"meta\" when present.  Files that \
error on read are individually skipped (with a LoadSolutions::skip \
message); the function returns $Failed only when modelDir itself is \
not a directory.";

ExtractCode::usage =
"ExtractCode[text] returns the Wolfram Language source block embedded \
in an LLM reply, preferring the last fenced ```wl block, falling back \
to any fenced block, and finally returning the trimmed text.";

$OpenRouterAPIKey::usage =
"$OpenRouterAPIKey is the currently-resolved OpenRouter API key, read \
lazily from Environment[\"OPENROUTER_API_KEY\"].  Evaluates to $Failed \
when the env var is unset or blank.";

LoadChallenges::usage =
"LoadChallenges[path] reads a challenge bank from a JSON file (a list \
of associations or an association of name -> association) and returns \
an Association of normalized challenge records keyed by challenge name. \
Returns $Failed and emits a tagged Message on shape or parse error.";

LoadTestBank::usage =
"LoadTestBank[path] reads a WXF-encoded expected-output test bank \
(Association of challengeName -> list of {HoldComplete[input], expected} \
or {HoldComplete[input], expected, <|metadata|>}) and returns an \
Association of normalized test entries keyed by challenge name. \
Returns $Failed and emits a tagged Message on shape or parse error.";

LoadChallengesJSONL::usage =
"LoadChallengesJSONL[path] reads the single-file JSONL benchmark format \
(one JSON record per line, fields: task_id, name, index, instruction, \
prompt, entry_point, tests) and returns an Association \
<|\"challenges\" -> ..., \"testBank\" -> ...|> with the same downstream \
shape as combining LoadChallenges + LoadTestBank, so callers can swap \
loaders without touching the runner.  LoadChallengesJSONL[path, \
privatePath] additionally loads canonical solutions from a private \
JSONL file when supplied; the returned Association then carries a \
\"canonicalSolutions\" key.  See docs/CHALLENGES-JSONL-FORMAT.md for \
the format spec.";

ReconcileNames::usage =
"ReconcileNames[challenges, testBank] aligns the keys of two banks so \
that names matching up to case, punctuation, and diacritics share the \
canonical form used by the test bank.  Returns an Association with \
keys \"challenges\" (the rekeyed challenges Assoc), \"testBank\" \
(returned unchanged), \"renamed\" (list of {old, new} maps), \
\"unmatchedInChallenges\", \"unmatchedInTestBank\", and \"summary\".  \
Use this before GenerateSolutions or RunBenchmark when a prompts bank \
and a test bank were authored independently.";

RunBenchmark::usage =
"RunBenchmark[challenges, testBank, solutions, opts] evaluates each \
candidate solution against its expected outputs in a sandboxed runner \
and returns an Association with keys \"runId\", \"runDir\", \"meta\", \
and \"results\".  Three isolation modes are available via the \
\"IsolationMode\" option: \"PerTestKernel\" (default), \"PooledKernels\", \
and \"InProcess\".  Mandatory option: \"Model\".";

DiffRuns::usage =
"DiffRuns[baseline, new] compares two RunBenchmark results, reporting \
regressions, fixes, new tests, missing tests, and per-test status \
changes.  Both arguments must be the full Associations returned by \
RunBenchmark.";

$BenchmarkDefaults::usage =
"$BenchmarkDefaults is an Association of the default option values \
consulted by RunBenchmark when an option is not explicitly supplied.";

WriteReport::usage =
"WriteReport[run, dir] writes report.html, report.md, report.json, and \
junit.xml into dir, creating dir if needed, and returns an Association \
with keys \"html\", \"markdown\", \"json\", and \"junit\" giving the \
absolute paths that were written.  `run` must be the Association \
returned by RunBenchmark.";

WriteJUnitReport::usage =
"WriteJUnitReport[run, path] writes a single JUnit-format XML file at \
`path` (creating the parent directory if needed) so CI systems can \
ingest per-test pass/fail/skip metrics, and returns the path written. \
Statuses map as: Evaluated+passed \[LongDash] bare testcase; \
Evaluated+failed \[LongDash] failure; NoSolution \[LongDash] skipped; \
everything else \[LongDash] error.";

LiveDashboard::usage =
"LiveDashboard[runDir] returns a Dynamic notebook view that tails \
runDir/progress.jsonl and renders a live summary of the run: totals, \
pass/fail counts, per-challenge status, and the most recent event. \
Intended for interactive notebook use while a benchmark is running.";

CompareModels::usage =
"CompareModels[runs] loads multiple benchmark run directories and \
returns an Association comparing them per-challenge.  `runs` may be a \
list of run directories (each must contain run.json + results.wxf) or \
an Association of modelLabel -> runDir.  The returned Association has \
keys \"models\", \"runsByModel\", \"perChallenge\", \"allChallenges\", \
\"uniquelyPassed\", and \"uniquelyFailed\".";

WriteCompareReport::usage =
"WriteCompareReport[compare, dir] writes compare.html and compare.md \
into `dir` (creating dir if needed) from the Association returned by \
CompareModels, and returns an Association with keys \"html\" and \
\"markdown\" giving the absolute paths written.";

LoadWLChallenge::usage =
"LoadWLChallenge[path] parses one .wlchallenge plain-text file and \
returns an Association with keys \"name\", \"index\", \"instruction\", \
\"prompt\", and \"tests\".  Test inputs are returned as \
HoldComplete[input] to preserve laziness.  Emits messages and returns \
$Failed on parse error or missing required sections.";

LoadChallengesDir::usage =
"LoadChallengesDir[dir] loads every *.wlchallenge file in `dir` and \
returns an Association keyed by challenge name, sorted by :Index:.  \
Files that fail to parse are skipped (with a per-file message).";

BuildTestBank::usage =
"BuildTestBank[dir] reads .wlchallenge files from `dir` and returns the \
pair {challengesAssoc, testBankAssoc} matching the runtime shape \
consumed by RunBenchmark.  Returns {<||>, <||>} when dir contains no \
.wlchallenge files.";

WriteTestBankFiles::usage =
"WriteTestBankFiles[dir, jsonOut, wxfOut] reads .wlchallenge files from \
`dir`, emits the legacy challenges JSON to jsonOut and the test bank \
WXF to wxfOut, and returns a small summary Association with keys \
\"json\", \"wxf\", \"challenges\", and \"tests\".";

WriteWLChallengeDir::usage =
"WriteWLChallengeDir[challenges, testBank, dir] emits one .wlchallenge \
file per bank entry into `dir`, suitable for editing and re-building. \
The emitted :Name: is always the BANK name.  Returns the list of \
written paths.";


(* ------------------------------------------------------------------ *)
(* Load sub-packages                                                  *)
(* ------------------------------------------------------------------ *)

Begin["`Private`"];

$thisDir = DirectoryName[$InputFileName];

Get[FileNameJoin[{$thisDir, "Utilities.wl"}]];
Get[FileNameJoin[{$thisDir, "OpenRouter.wl"}]];
Get[FileNameJoin[{$thisDir, "Solutions.wl"}]];
Get[FileNameJoin[{$thisDir, "Generator.wl"}]];
Get[FileNameJoin[{$thisDir, "Loader.wl"}]];
Get[FileNameJoin[{$thisDir, "Results.wl"}]];
Get[FileNameJoin[{$thisDir, "Runner.wl"}]];
Get[FileNameJoin[{$thisDir, "Report.wl"}]];
Get[FileNameJoin[{$thisDir, "Compare.wl"}]];
Get[FileNameJoin[{$thisDir, "TestBankBuilder.wl"}]];


(* ------------------------------------------------------------------ *)
(* Dispatch layer                                                     *)
(*                                                                    *)
(* Public symbols resolve to well-typed private impls.  Each impl     *)
(* validates its own arguments; the dispatcher's only job is to       *)
(* normalize common-calling-convention variants (rules vs an assoc).  *)
(* ------------------------------------------------------------------ *)

JofreEspigulePons`WolframChallengesBenchmark`BenchmarkVersion[] :=
  "1.0.0";


(* --- OpenRouterChatComplete ---------------------------------------- *)

JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete[
    messages_List, opts_Association] :=
  openRouterChatCompleteImpl[messages, opts];

JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete[
    messages_List, opts:(_Rule|_RuleDelayed)..] :=
  openRouterChatCompleteImpl[messages, Association[opts]];

JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete[
    messages_List, opts:{(_Rule|_RuleDelayed)...}] :=
  openRouterChatCompleteImpl[messages, Association[opts]];

JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete[
    messages_List] :=
  openRouterChatCompleteImpl[messages, <||>];

(* Convenience overload: single user prompt --------------------------- *)
JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete[
    prompt_String, rest___] :=
  JofreEspigulePons`WolframChallengesBenchmark`OpenRouterChatComplete[
    {<|"role" -> "user", "content" -> prompt|>}, rest];


(* --- GenerateSolutions --------------------------------------------- *)

JofreEspigulePons`WolframChallengesBenchmark`GenerateSolutions[
    challenges_Association, testBank_Association, opts_Association] :=
  generateSolutionsImpl[challenges, testBank, opts];

JofreEspigulePons`WolframChallengesBenchmark`GenerateSolutions[
    challenges_Association, testBank_Association,
    opts:(_Rule|_RuleDelayed)..] :=
  generateSolutionsImpl[challenges, testBank, Association[opts]];

JofreEspigulePons`WolframChallengesBenchmark`GenerateSolutions[
    challenges_Association, testBank_Association,
    opts:{(_Rule|_RuleDelayed)...}] :=
  generateSolutionsImpl[challenges, testBank, Association[opts]];

JofreEspigulePons`WolframChallengesBenchmark`GenerateSolutions[
    challenges_Association, testBank_Association] :=
  generateSolutionsImpl[challenges, testBank, <||>];


(* --- SaveSolution -------------------------------------------------- *)

JofreEspigulePons`WolframChallengesBenchmark`SaveSolution[
    dir_String, name_String, code_String] :=
  saveSolutionImpl[dir, name, code, None, <||>];

JofreEspigulePons`WolframChallengesBenchmark`SaveSolution[
    dir_String, name_String, code_String, testBank_Association] :=
  saveSolutionImpl[dir, name, code, testBank, <||>];

JofreEspigulePons`WolframChallengesBenchmark`SaveSolution[
    dir_String, name_String, code_String, None] :=
  saveSolutionImpl[dir, name, code, None, <||>];

JofreEspigulePons`WolframChallengesBenchmark`SaveSolution[
    dir_String, name_String, code_String, testBank_Association,
    extraMeta_Association] :=
  saveSolutionImpl[dir, name, code, testBank, extraMeta];

JofreEspigulePons`WolframChallengesBenchmark`SaveSolution[
    dir_String, name_String, code_String, None, extraMeta_Association] :=
  saveSolutionImpl[dir, name, code, None, extraMeta];


(* --- LoadSolutions ------------------------------------------------- *)

JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions[modelDir_String] :=
  loadSolutionsImpl[modelDir];

JofreEspigulePons`WolframChallengesBenchmark`LoadSolutions[other_] :=
  loadSolutionsImpl[other];


(* --- AuditSolutions ----------------------------------------------- *)

JofreEspigulePons`WolframChallengesBenchmark`AuditSolutions[
    dir_String, testBank_Association] :=
  auditSolutionsImpl[dir, testBank];

JofreEspigulePons`WolframChallengesBenchmark`AuditSolutions[
    dir_, testBank_] :=
  auditSolutionsImpl[dir, testBank];


(* --- ExtractCode --------------------------------------------------- *)

JofreEspigulePons`WolframChallengesBenchmark`ExtractCode[text_String] :=
  extractCodeImpl[text];

JofreEspigulePons`WolframChallengesBenchmark`ExtractCode[_] := $Failed;


(* --- $OpenRouterAPIKey (lazy read) --------------------------------- *)

JofreEspigulePons`WolframChallengesBenchmark`$OpenRouterAPIKey :=
  resolveOpenRouterAPIKey[];


(* --- LoadChallenges / LoadTestBank --------------------------------- *)

JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges[path_String] :=
  loadChallengesImpl[path];

JofreEspigulePons`WolframChallengesBenchmark`LoadChallenges[other_] :=
  loadChallengesImpl[other];

JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank[path_String] :=
  loadTestBankImpl[path];

JofreEspigulePons`WolframChallengesBenchmark`LoadTestBank[other_] :=
  loadTestBankImpl[other];


(* --- LoadChallengesJSONL ------------------------------------------ *)

JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL[
    path_String] :=
  loadChallengesJSONLImpl[path];

JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL[
    path_String, privatePath_String] :=
  loadChallengesJSONLImpl[path, privatePath];

JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesJSONL[
    other_, ___] :=
  loadChallengesJSONLImpl[other];


(* --- ReconcileNames ------------------------------------------------ *)

JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames[
    challenges_Association, testBank_Association] :=
  reconcileNamesImpl[challenges, testBank];

JofreEspigulePons`WolframChallengesBenchmark`ReconcileNames[a_, b_] :=
  reconcileNamesImpl[a, b];


(* --- RunBenchmark -------------------------------------------------- *)

JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
    challenges_Association, testBank_Association, solutions_Association,
    opts:(_Rule|_RuleDelayed)...] :=
  runBenchmarkImpl[challenges, testBank, solutions, opts];

JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
    challenges_Association, testBank_Association, solutions_Association,
    opts_Association] :=
  runBenchmarkImpl[challenges, testBank, solutions,
    Sequence @@ Normal[opts]];

JofreEspigulePons`WolframChallengesBenchmark`RunBenchmark[
    challenges_Association, testBank_Association, solutions_Association,
    opts:{(_Rule|_RuleDelayed)...}] :=
  runBenchmarkImpl[challenges, testBank, solutions, Sequence @@ opts];


(* --- DiffRuns ------------------------------------------------------ *)

JofreEspigulePons`WolframChallengesBenchmark`DiffRuns[
    baseline_Association, new_Association] :=
  diffRunsImpl[baseline, new];


(* --- Reports ------------------------------------------------------- *)

JofreEspigulePons`WolframChallengesBenchmark`WriteReport[
    run_Association, dir_String] :=
  writeReportImpl[run, dir];

JofreEspigulePons`WolframChallengesBenchmark`WriteJUnitReport[
    run_Association, path_String] :=
  writeJUnitReportImpl[run, path];

JofreEspigulePons`WolframChallengesBenchmark`LiveDashboard[
    runDir_String] :=
  liveDashboardImpl[runDir];


(* --- CompareModels / WriteCompareReport ---------------------------- *)

JofreEspigulePons`WolframChallengesBenchmark`CompareModels[
    runs:(_List|_Association)] :=
  compareModelsImpl[runs];

(* Convenience overload: scan `parent` for run-* subdirectories and compare
   them all.  Returns $Failed (with a logged warning) if no children match. *)
JofreEspigulePons`WolframChallengesBenchmark`CompareModels[
    parent_String, Automatic] :=
  Module[{dirs},
    dirs = FileNames["run-*", parent];
    If[dirs === {},
      logWarn["CompareModels: no run-* directories under " <> parent];
      $Failed,
      compareModelsImpl[dirs]
    ]
  ];

JofreEspigulePons`WolframChallengesBenchmark`WriteCompareReport[
    cmp_Association, dir_String] :=
  writeCompareReportImpl[cmp, dir];


(* --- TestBankBuilder ---------------------------------------------- *)

JofreEspigulePons`WolframChallengesBenchmark`LoadWLChallenge[path_String] :=
  loadWLChallengeImpl[path];

JofreEspigulePons`WolframChallengesBenchmark`LoadChallengesDir[dir_String] :=
  loadChallengesDirImpl[dir];

JofreEspigulePons`WolframChallengesBenchmark`BuildTestBank[dir_String] :=
  buildTestBankImpl[dir];

JofreEspigulePons`WolframChallengesBenchmark`WriteTestBankFiles[
    dir_String, jsonOut_String, wxfOut_String] :=
  writeTestBankFilesImpl[dir, jsonOut, wxfOut];

JofreEspigulePons`WolframChallengesBenchmark`WriteWLChallengeDir[
    challenges_Association, testBank_Association, dir_String] :=
  writeWLChallengeDirImpl[challenges, testBank, dir];


(* $BenchmarkDefaults: no dispatcher.  The public symbol is declared by
   its ::usage above, which puts it on the context path; Runner.wl's
   top-level  $BenchmarkDefaults = <| ... |>  therefore resolves to
   (and assigns) that same public symbol.  Consumers of the public
   surface get a real Association back. *)


End[];  (* `Private` *)

EndPackage[];
