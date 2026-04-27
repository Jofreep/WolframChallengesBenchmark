# challenges.jsonl format

The single-file format the benchmark loads from going forward.
Modeled after well-established LLM-evaluation benchmarks
(HumanEval, MBPP, BigCodeBench, LiveCodeBench): one JSON record per
line, streams during loading, greps cleanly, diffs line-by-line.

## File layout

Two files. `challenges.jsonl` is committed. `private/canonical_solutions.jsonl`
is gitignored.

```
challenges.jsonl                    # public, committed
private/canonical_solutions.jsonl   # private, gitignored
```

## Public record schema

One JSON object per line in `challenges.jsonl`. Required fields are
**bold**; the others are optional.

| Field | Type | Notes |
|---|---|---|
| **`task_id`** | string | Canonical ID. Matches `solutions/<model>/<task_id>.wl`. Used as the dispatch key everywhere. |
| `name` | string | Display name. Usually equal to `task_id`. |
| `index` | integer | Original 1-based ordering in the bank. |
| `instruction` | string | Short imperative system-prompt-style line. |
| **`prompt`** | string | The full multi-line text the LLM sees. May contain embedded examples and the `ENTER YOUR CODE HERE` marker. Preserved verbatim. |
| `entry_point` | string | The function the candidate must define. Mirrors HumanEval; lets the audit gate validate without parsing held inputs. |
| **`tests`** | list of objects | Per-test triples (see below). |

Each `tests[i]` entry:

| Field | Type | Notes |
|---|---|---|
| **`input_wl`** | string | Held WL source string. Loader parses with `ImportString[..., {"WL", "HeldExpressions"}]` -> `HoldComplete[expr]`. Same semantics as the legacy WXF's `HoldComplete[...]` cells. |
| **`expected_wl`** | string | Value as InputForm WL source string. Loader parses with `ToExpression`. Handles complex literals like `Graph[...]`, `Image[...]`. |
| `metadata` | object | Optional Association of per-test annotations: timeout overrides, tags, source attribution, "boundary case" notes, etc. |

### Example record

Pretty-printed for readability; the actual file has one record per line.

```json
{
  "task_id": "PermutationIndex",
  "name": "PermutationIndex",
  "index": 36,
  "instruction": "Write Wolfram Language code that performs the following task",
  "prompt": "Permutation Index\nWrite code to find the k^th permutation\n...\nENTER YOUR CODE HERE\nPermutationIndex[order_Integer?Positive,index_Integer?Positive]:=",
  "entry_point": "PermutationIndex",
  "tests": [
    {"input_wl": "PermutationIndex[6, 1]",
     "expected_wl": "{1, 2, 3, 4, 5, 6}",
     "metadata": {}},
    {"input_wl": "PermutationIndex[5, 720]",
     "expected_wl": "{5, 4, 3, 2, 1}",
     "metadata": {"note": "index > n!; boundary case"}},
    {"input_wl": "PermutationIndex[17, 100000000000000]",
     "expected_wl": "{5, 14, 9, 2, 1, 11, 16, 6, 7, 4, 8, 17, 10, 13, 12, 15, 3}",
     "metadata": {}}
  ]
}
```

## Private record schema

One JSON object per line in `private/canonical_solutions.jsonl`. Always
gitignored. The runner does not need this file to grade; it's used
only for the bank-self-test path ("does the canonical solution actually
pass its own tests?") and for occasional triage.

| Field | Type | Notes |
|---|---|---|
| **`task_id`** | string | Matches a `task_id` in `challenges.jsonl`. |
| **`canonical_solution`** | string | Reference WL implementation as a single source string. |

Why private: leaking canonical solutions into LLM training data is the
single most common way to make a benchmark useless over time. Even if
the repository itself is private, defense-in-depth keeps these out of
git so they can't slip out via screenshares, shared zips, downstream
forks, or contractor hand-offs.

## Loading

```wolfram
Needs["JofreEspigulePons`WolframChallengesBenchmark`"];

(* Public-only load \[LongDash] what scripts/ and CI use. *)
data = LoadChallengesJSONL["challenges.jsonl"];
data["challenges"]   (* Association: task_id -> {name, index, instruction, prompt, entry_point} *)
data["testBank"]     (* Association: task_id -> list of <|input, expected, metadata, ...|> *)

(* With canonical solutions for the bank-self-test path. *)
data = LoadChallengesJSONL["challenges.jsonl",
                           "private/canonical_solutions.jsonl"];
data["canonicalSolutions"]   (* Association: task_id -> source string *)
```

The downstream shape (`data["challenges"]` and `data["testBank"]`) is
identical to what `LoadChallenges` + `LoadTestBank` return for the
legacy JSON+WXF pair, so callers can swap loaders without touching the
runner.

## Migrating from the legacy format

```
wolframscript -file scripts/MigrateToJSONL.wls
```

Reads `ChallengesTestDataV1.json` + `ChallengesTests.wxf` and writes
`challenges.jsonl` + `private/canonical_solutions.jsonl`. Idempotent
(re-run any time to refresh from the legacy source).

## Round-trip guarantee

The migration is lossless at the kernel layer: a benchmark run loaded
from the JSONL produces identical per-test results to one loaded from
the legacy JSON+WXF pair, modulo the InputForm round-trip artifact for
flat orderless heads (e.g. `a*b*c` reparses as `Times[Times[a,b],c]`,
which evaluates to the same value via `ReleaseHold`). This invariant
is enforced by the `Loader/jsonl-roundtrip-matches-legacy` test in
`Tests/Loader.wlt`.

## Why JSONL instead of one big JSON

- Streams during loading; no need to hold the whole file in memory.
- One record per line greps cleanly: `grep '"task_id":"FizzBuzz"' challenges.jsonl`.
- `git diff` shows per-record changes, not a whole-file blob.
- Append-only friendly (add a new challenge by appending one line).
- Standard format used by HumanEval, MBPP, APPS, BigCodeBench,
  LiveCodeBench, and most HF Datasets exports.

---

## Tutorial: adding a new challenge

End-to-end walkthrough for adding a `ReverseList` challenge.  Two
authoring paths are supported \[LongDash] pick whichever fits your
workflow.

### Path A: append a JSONL line (fastest)

For a one-off addition, just append a record to `challenges.jsonl`:

```bash
cat >> challenges.jsonl <<'EOF'
{"task_id":"ReverseList","name":"ReverseList","index":167,"instruction":"Write Wolfram Language code that performs the following task","prompt":"Reverse a list\nDefine the function reverseList[lst] that returns the elements of lst in reverse order.\nreverseList[{1,2,3}] -> {3,2,1}\nENTER YOUR CODE HERE\nreverseList[lst_List]:=","entry_point":"reverseList","tests":[{"input_wl":"reverseList[{1, 2, 3}]","expected_wl":"{3, 2, 1}","metadata":{}},{"input_wl":"reverseList[{}]","expected_wl":"{}","metadata":{"note":"empty-list edge case"}},{"input_wl":"reverseList[{42}]","expected_wl":"{42}","metadata":{}}]}
EOF
```

Validate the new line parses:

```bash
wolframscript -c '
  PacletDirectoryLoad["WolframChallengesLLMBenchmarkPaclet"];
  Needs["JofreEspigulePons`WolframChallengesBenchmark`"];
  data = LoadChallengesJSONL["challenges.jsonl"];
  Print["loaded ", Length[data["challenges"]], " challenges"];
  Print["new entry: ", data["challenges", "ReverseList"]];
  Print["new tests: ", data["testBank", "ReverseList"]]
'
```

### Path B: edit a `.wlchallenge` file (better for non-trivial changes)

For multiple edits or a structured authoring loop, work in a `.wlchallenge`
directory.  Seed it from the current bank if you don't have one yet:

```bash
wolframscript -c '
  PacletDirectoryLoad["WolframChallengesLLMBenchmarkPaclet"];
  Needs["JofreEspigulePons`WolframChallengesBenchmark`"];
  data = LoadChallengesJSONL["challenges.jsonl"];
  WriteWLChallengeDir[data["challenges"], data["testBank"], ".challenges"]
'
```

Then create `.challenges/reverselist.wlchallenge`:

```
(* :Name: ReverseList  *)
(* :Index: 167 *)
(* :Instruction: Write Wolfram Language code that performs the following task *)

(* :Prompt: *)
Reverse a list
Define the function reverseList[lst] that returns the elements of lst in reverse order.
reverseList[{1,2,3}] -> {3,2,1}
ENTER YOUR CODE HERE
reverseList[lst_List]:=

(* :Tests: *)
{reverseList[{1, 2, 3}], {3, 2, 1}}
{reverseList[{}], {}, <|"note" -> "empty-list edge case"|>}
{reverseList[{42}], {42}}
```

Then re-build `challenges.jsonl`:

```bash
wolframscript -file scripts/BuildChallengesJSONL.wls --in .challenges
```

The script reads every `.wlchallenge` file in `--in`, sorts by `:Index:`,
and writes `challenges.jsonl` (default path).  Re-run any time you edit
the authoring directory.

### Add the canonical solution (private)

The reference implementation lives in `private/canonical_solutions.jsonl`,
gitignored.  Append a record matching the new `task_id`:

```bash
cat >> private/canonical_solutions.jsonl <<'EOF'
{"task_id":"ReverseList","canonical_solution":"reverseList[lst_List] := Reverse[lst]"}
EOF
```

### Smoke-test against one model

Run the canonical solution against its own tests (this is the bank
self-test) by treating it as a one-off "model":

```bash
mkdir -p /tmp/canonical-smoke && \
  echo 'reverseList[lst_List] := Reverse[lst]' \
    > /tmp/canonical-smoke/ReverseList.wl && \
wolframscript -file scripts/RunBenchmark.wls \
  --solutions /tmp/canonical-smoke \
  --model     canonical \
  --filter    ReverseList \
  --isolation InProcess \
  --out       /tmp/wclb-canonical
```

Expected: `passed: 3 / 3`.  If anything else, the test bank's `expected_wl`
disagrees with what the canonical solution actually returns \[LongDash]
fix the bank entry (or the canonical) before shipping.

### Run a real model

Once the new challenge passes its canonical, queue it against an LLM:

```bash
wolframscript -file scripts/GenerateSolutions.wls \
  --filter ReverseList \
  --model  google/gemini-2.5-flash \
  --out    solutions
```

Then grade the result:

```bash
wolframscript -file scripts/RunBenchmark.wls \
  --solutions solutions/google_gemini-2.5-flash \
  --model     google/gemini-2.5-flash \
  --filter    ReverseList \
  --out       runs
```

The per-run `report.html` and `report.json` show the new test alongside
the rest of the bank.

### Commit

```bash
git add challenges.jsonl
# private/canonical_solutions.jsonl is gitignored on purpose; do NOT commit it
git commit -m "Add ReverseList challenge"
```
