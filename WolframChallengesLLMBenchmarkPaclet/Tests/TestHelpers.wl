(* ::Package:: *)

(* Shared helpers for the Tests/*.wlt files.

   Wolfram 15.0's VerificationTest no longer populates ActualMessages
   when Messages fire via the regular Message[] pipeline, so we capture
   messages ourselves via Block on $MessageList.  $MessageList is a
   Protected system symbol, which requires Unprotect/Protect around the
   Block.  We keep the helpers un-contexted (Global) since .wlt files
   run at top level during TestReport evaluation. *)


ClearAll[captureMsgs, hasMessageQ, withTempDir];


(* captureMsgs[expr] evaluates expr with $MessageList snapshotted, returning
   {value, List[HoldForm[sym::tag], ...]}.  Messages are still printed to
   the driver's $Messages stream; pipe through Quiet at the call site to
   silence them while still capturing via the snapshot. *)

SetAttributes[captureMsgs, HoldFirst];
captureMsgs[expr_] := Module[{v, msgs},
  Unprotect[$MessageList];
  Block[{$MessageList = {}},
    v = expr;
    msgs = $MessageList;
  ];
  Protect[$MessageList];
  {v, msgs}
];


(* hasMessageQ[msgs, sym::tag] tests whether the captured $MessageList
   contains a message for the named tag.  Second argument is evaluated
   *unheld* so callers can write the usual sym::tag syntax. *)

SetAttributes[hasMessageQ, HoldRest];
hasMessageQ[msgs_List, msgNameExpr_] :=
  MemberQ[msgs, HoldForm[msgNameExpr]];


(* withTempDir[body] creates a unique temp dir and returns body's value,
   binding the local symbol $tmp to the directory.  The directory is not
   auto-cleaned \[LongDash] test files are expected to stay so post-mortem
   inspection is possible. *)

SetAttributes[withTempDir, HoldFirst];
withTempDir[body_] := Module[{$tmp},
  $tmp = FileNameJoin[{$TemporaryDirectory,
    "wclb-tests-" <> ToString[RandomInteger[10^9]]}];
  If[! DirectoryQ[$tmp],
    CreateDirectory[$tmp, CreateIntermediateDirectories -> True]];
  body
];
