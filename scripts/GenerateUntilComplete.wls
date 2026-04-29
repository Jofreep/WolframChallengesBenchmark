#!/usr/bin/env wolframscript
(* ::Package:: *)

(* GenerateUntilComplete.wls

   Robust wrapper around scripts/GenerateSolutions.wls.

   The underlying generator already isolates per-challenge failures
   (TimeConstrained + CheckAbort + tombstone logging in
   Internal`WithLocalSettings) but a flaky URLRead, a kernel-level
   message storm, or an Abort that escapes those guards can still
   make wolframscript print "$TimedOut" or "$Aborted" at top level
   and exit \[LongDash] killing every challenge that hadn't started yet.

   This wrapper sidesteps that class of failure by running each
   generation pass in a FRESH wolframscript subprocess: a kernel
   abort can't terminate what isn't in that kernel.  After each pass
   it walks solutions/<modelSlug>/ to learn what's still missing and
   either calls another pass or stops.

   Loop:
     1. Compute the set of challenges still missing a .wl file.
     2. If empty \[Rule] done, exit 0.
     3. Call scripts/GenerateSolutions.wls in a subprocess with
        --filter <missing,...> (and --overwrite from iteration 2
        onward, so audit-rejected stubs get retried).
     4. Recompute missing.  If the count didn't shrink, increment
        a "no-progress" counter; otherwise reset it.
     5. If --max-iterations reached, or no-progress counter hits
        --saturation-threshold, stop and exit 2 (partial).

   All standard GenerateSolutions flags pass through:
     --api-key-file, --max-tokens, --temperature, --timeout,
     --retries, --retry-base-delay
   (Raw LLM responses are now always saved under
   solutions/<model>/raw/, no flag needed.)

   Wrapper-only flags:
     --max-iterations N        cap on passes (default 6)
     --saturation-threshold N  stop after N consecutive zero-progress
                               passes (default 2)
     --bump-timeout-each       multiply --timeout by 1.5x each pass
                               (off by default)

   Required (mirror GenerateSolutions):
     --out DIR --model STRING

   Optional:
     --jsonl PATH (default: ./challenges.jsonl)

   Exit codes:
     0 every challenge has a .wl file
     1 invalid arguments / missing input files
     2 partial \[Dash] either max-iterations or saturation reached
     3 no API key (fast-fail before subprocesses)
     4 unexpected fatal error
*)

$scriptDir = DirectoryName[$InputFileName];
$repoRoot  = DirectoryName[$scriptDir];

(* ----------------------------------------------------------------------- *)
(* Arg parsing                                                             *)
(* ----------------------------------------------------------------------- *)

parseArgs[args_List] := Module[{i = 1, n = Length[args], out = <||>, k, v},
  While[i <= n,
    k = args[[i]];
    If[StringStartsQ[k, "--"],
      v = If[i < n && ! StringStartsQ[args[[i + 1]], "--"],
             args[[i + 1]], "true"];
      out[StringDrop[k, 2]] = v;
      i += If[v === "true", 1, 2],
      i++
    ]
  ];
  out
];

abort[code_, msg_] := (WriteString[$Output, "ERROR: ", msg, "\n"]; Exit[code]);
info[msg_String]   := WriteString[$Output, msg, "\n"];

modelSlug[model_String] := StringReplace[model,
  RegularExpression["[^A-Za-z0-9_.-]"] -> "_"];


(* ----------------------------------------------------------------------- *)
(* Bank + filesystem state                                                 *)
(* ----------------------------------------------------------------------- *)

(* Read challenge names from challenges.jsonl without booting the paclet
   \[LongDash] keeps the wrapper lightweight and the inner loop fast. *)
loadChallengeNames[jsonlPath_String] := Module[{lines, names},
  lines = Import[jsonlPath, "Lines"];
  names = DeleteCases[
    Map[
      Function[line,
        Module[{r = Quiet @ Check[Developer`ReadRawJSONString[line], $Failed]},
          If[AssociationQ[r], Lookup[r, "name", Missing[]], Missing[]]
        ]
      ],
      lines
    ],
    _Missing | Null | ""
  ];
  Sort @ DeleteDuplicates @ names
];

existingSolutions[outDir_String] := Module[{files},
  If[! DirectoryQ[outDir], Return[{}]];
  files = FileNames["*.wl", outDir];
  StringDelete[FileBaseName /@ files, ".wl"]   (* defensive *)
];

missingNames[allNames_List, outDir_String] :=
  Sort @ Complement[allNames, existingSolutions[outDir]];


(* ----------------------------------------------------------------------- *)
(* Subprocess invocation                                                   *)
(* ----------------------------------------------------------------------- *)

(* Build the argv for a single generate pass.  Filter is materialized as
   a comma-joined string per the underlying script's CLI contract. *)
buildGenerateArgs[args_Association, filterNames_List, overwrite_, timeout_,
                  iterTag_String] := Module[{argv},
  argv = {"-file", FileNameJoin[{$scriptDir, "GenerateSolutions.wls"}],
          "--out",   args["out"],
          "--model", args["model"]};
  If[KeyExistsQ[args, "api-key-file"],
    argv = Join[argv, {"--api-key-file", args["api-key-file"]}]];
  If[KeyExistsQ[args, "jsonl"],
    argv = Join[argv, {"--jsonl", args["jsonl"]}]];
  argv = Join[argv, {
    "--max-tokens",      ToString @ Lookup[args, "max-tokens", "8192"],
    "--temperature",     ToString @ Lookup[args, "temperature", "0.0"],
    "--timeout",         ToString @ timeout,
    "--retries",         ToString @ Lookup[args, "retries", "3"],
    "--retry-base-delay",ToString @ Lookup[args, "retry-base-delay", "2.0"]
  }];
  argv = Join[argv, {"--filter", StringRiffle[filterNames, ","]}];
  If[overwrite, argv = Append[argv, "--overwrite"]];
  (* --save-raw is a no-op now (raws always saved); skip propagating it. *)
  (* Per-iteration JSONL audit log so we don't clobber prior runs. *)
  argv = Join[argv, {"--log",
    FileNameJoin[{$repoRoot, "runs",
      "generate-" <> modelSlug[args["model"]] <> "-" <> iterTag <> ".jsonl"}]}];
  argv
];

(* Run a single generation pass as a hermetic subprocess.

   We use `RunProcess[..., "ExitCode"]` (NOT `..., All`).  With "ExitCode",
   RunProcess streams the child's stdout/stderr through to OUR terminal
   in real time and returns just the integer exit code when the child
   exits.  The "All" variant tries to BUFFER everything and observably
   wedges on macOS when the inner kernel exits via a top-level
   `$TimedOut` print: some pipe state ends up in a half-closed limbo
   and the parent kernel itself returns $TimedOut from RunProcess.

   By deferring all I/O to the user's terminal (tee at the wrapper-call
   site captures it for them), we sidestep the wedge entirely.

   Returns the integer exit code, or 4 on launch failure / unexpected
   non-integer return.  *)
runGenerateSubprocess[args_Association, filterNames_List, overwrite_,
                      timeout_, iterTag_String] := Module[
  {argv, exitCode},
  argv = buildGenerateArgs[args, filterNames, overwrite, timeout, iterTag];

  info["  $ wolframscript " <> StringRiffle[argv, " "]];
  info["  --- subprocess output below; wrapper resumes after exit ---"];

  exitCode = Quiet @ Check[
    RunProcess[
      Prepend[argv, "wolframscript"],
      "ExitCode",
      ProcessEnvironment -> Inherited],
    $Failed];

  info["  --- subprocess exited ---"];

  Which[
    IntegerQ[exitCode], exitCode,
    True,               4   (* RunProcess returned $Failed/$TimedOut/etc *)
  ]
];


(* ----------------------------------------------------------------------- *)
(* Pre-flight                                                              *)
(* ----------------------------------------------------------------------- *)

preflight[args_Association] := Module[{keyOk},
  (* Mandatory args. *)
  If[! KeyExistsQ[args, "out"],   abort[1, "missing --out"]];
  If[! KeyExistsQ[args, "model"], abort[1, "missing --model"]];

  (* API key reachable?  (We don't read it here \[LongDash] just check it
     exists somewhere the inner script can find.) *)
  keyOk = (
    StringQ[Environment["OPENROUTER_API_KEY"]] &&
      StringLength[StringTrim[Environment["OPENROUTER_API_KEY"]]] > 0
  ) || (
    KeyExistsQ[args, "api-key-file"] && FileExistsQ[args["api-key-file"]]
  );
  If[! keyOk,
    abort[3, "no OpenRouter API key: set OPENROUTER_API_KEY or " <>
              "pass --api-key-file PATH"]];

  (* challenges.jsonl reachable? *)
  Module[{p = Lookup[args, "jsonl",
              FileNameJoin[{Directory[], "challenges.jsonl"}]]},
    If[! FileExistsQ[p], abort[1, "challenges.jsonl not found: " <> p]]
  ];
];


(* ----------------------------------------------------------------------- *)
(* Main loop                                                               *)
(* ----------------------------------------------------------------------- *)

main[] := Module[
  {args, jsonlPath, outDir, allNames, total, missing, prevMissingCount,
   noProgressCount = 0, iteration = 0, maxIters, satThresh, bumpTimeout,
   baseTimeout, currentTimeout, exitCode, iterTag,
   priorTotal, gained},

  args = parseArgs[Drop[$ScriptCommandLine, 1]];
  preflight[args];

  jsonlPath = Lookup[args, "jsonl",
                FileNameJoin[{Directory[], "challenges.jsonl"}]];
  outDir    = FileNameJoin[{args["out"], modelSlug[args["model"]]}];

  maxIters    = Max[1,
    ToExpression @ ToString @ Lookup[args, "max-iterations", "6"]];
  satThresh   = Max[1,
    ToExpression @ ToString @ Lookup[args, "saturation-threshold", "2"]];
  bumpTimeout = KeyExistsQ[args, "bump-timeout-each"];
  baseTimeout = ToExpression @ ToString @ Lookup[args, "timeout", "180"];
  currentTimeout = baseTimeout;

  allNames = loadChallengeNames[jsonlPath];
  total    = Length[allNames];
  missing  = missingNames[allNames, outDir];

  info["GenerateUntilComplete"];
  info["  model:    " <> args["model"]];
  info["  outDir:   " <> outDir];
  info["  bank:     " <> ToString[total] <> " challenges"];
  info["  have:     " <> ToString[total - Length[missing]] <> " .wl files"];
  info["  missing:  " <> ToString[Length[missing]]];
  info["  cap:      " <> ToString[maxIters] <> " iterations, " <>
                          "stop after " <> ToString[satThresh] <>
                          " zero-progress passes"];
  info[""];

  If[Length[missing] === 0,
    info["Nothing to do."];
    Exit[0]
  ];

  prevMissingCount = Length[missing];

  While[Length[missing] > 0,
    iteration++;
    iterTag = "iter" <> IntegerString[iteration, 10, 2] <> "-" <>
              DateString[{"ISODate", "_", "Hour24", "Minute", "Second"}];

    info[StringJoin[
      "=== Iteration ", ToString[iteration],
      " / ", ToString[maxIters],
      "  \[LongDash]  missing=", ToString[Length[missing]],
      ", timeout=", ToString[currentTimeout], "s",
      ", overwrite=", If[iteration > 1, "YES", "no"],
      " ==="]];

    priorTotal = total - Length[missing];

    exitCode = runGenerateSubprocess[
      args,
      missing,
      iteration > 1,             (* overwrite from iter 2+ to retry rejected stubs *)
      currentTimeout,
      iterTag
    ];

    (* Recompute state.  An exit code of 2 is "partial" \[LongDash] some
       challenges still failed, but the script ran to completion.  An
       exit code of $TimedOut etc. shows up here as a non-int / non-zero;
       we don't trust it, we just look at the filesystem. *)
    missing = missingNames[allNames, outDir];
    gained  = (total - Length[missing]) - priorTotal;

    info[""];
    info[StringJoin[
      "  iter ", ToString[iteration],
      ": subprocess exit=", ToString[exitCode],
      ", gained=", ToString[gained], " new .wl",
      ", remaining=", ToString[Length[missing]]]];
    info[""];

    If[gained <= 0,
      noProgressCount++,
      noProgressCount = 0
    ];

    If[noProgressCount >= satThresh,
      info["Saturation reached: " <> ToString[satThresh] <>
           " consecutive iterations with zero progress.  Stopping."];
      Break[]
    ];

    If[iteration >= maxIters,
      info["Hit --max-iterations " <> ToString[maxIters] <> ".  Stopping."];
      Break[]
    ];

    If[bumpTimeout,
      currentTimeout = Round[currentTimeout * 3 / 2];
      info["  bumping per-call timeout \[Rule] " <>
           ToString[currentTimeout] <> "s"]
    ];

    prevMissingCount = Length[missing];
  ];

  info[""];
  info["=== Final state ==="];
  info["  have:     " <> ToString[total - Length[missing]] <> " / "
                       <> ToString[total]];
  info["  missing:  " <> ToString[Length[missing]]];
  If[Length[missing] > 0 && Length[missing] <= 30,
    info["  names:    " <> StringRiffle[missing, ", "]]];

  Exit[If[Length[missing] === 0, 0, 2]]
];

(* Top-level safety net.  The wrapper itself shouldn't time out (it's
   mostly orchestrating subprocesses) but defense-in-depth: turn ANY
   unexpected escape \[Dash] $TimedOut leaking from a child URL stream,
   $Aborted from a TCP flake, $Failed from a Check, or a stray
   message-emitting expression \[Dash] into a clean exit code instead
   of letting wolframscript print the symbol at top level.            *)
Module[{result},
  result = CheckAbort[
    Quiet @ Check[main[], $Failed],
    $Aborted];
  Which[
    result === $TimedOut,
      WriteString[$Output, "FATAL: wrapper kernel itself returned $TimedOut "
        <> "(unexpected escape from a subprocess pipe).\n"];
      Exit[4],
    result === $Aborted,
      WriteString[$Output, "FATAL: wrapper kernel aborted "
        <> "(probably user Ctrl-C or signal).\n"];
      Exit[130],
    result === $Failed,
      WriteString[$Output, "FATAL: wrapper failed: ",
        ToString[$MessageList, InputForm], "\n"];
      Exit[4],
    True,
      (* main[] always Exits explicitly, so we shouldn't reach here. *)
      Exit[0]
  ]
]
