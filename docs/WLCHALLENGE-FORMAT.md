# `.wlchallenge` plain-text authoring format

`.wlchallenge` is the human-editable source of truth for a Wolfram Challenges
benchmark item. One file = one challenge. The compiled `ChallengesTests.wxf`
test bank and `ChallengesTestDataV1.json` prompt file are derived from a
directory of `.wlchallenge` files via `BuildTestBank` /
`scripts/BuildTestBank.wls`.

The format is designed to be:

- **diffable** — section headers are line comments, prompts are plain text,
  tests are one-per-line WL list literals.
- **safe to parse** — the build step uses `ImportString[..., {"WL",
  "HeldExpressions"}]` so test inputs are *never* evaluated. The expected
  RHS *is* evaluated at build time (it's the same data already shipping
  in the WXF), and that is the trust boundary.
- **round-trippable** — `WriteWLChallengeDir` renders an existing test bank
  back to `.wlchallenge` files; `BuildTestBank` consumes the directory.

## Grammar

A `.wlchallenge` file is a sequence of *sections*. A section starts with a
header line of the form

```
(@ :Key: inline-value @)
```

(replace `@` with `*` — shown with `@` here so this Markdown excerpt itself
is safe to embed in WL doc-comments).

There are two header shapes:

- **Inline scalar** — value follows the colon on the same line:

  ```
  (* :Name: AliquotSequence *)
  (* :Index: 1 *)
  (* :Instruction: Write Wolfram Language code that performs the following task *)
  ```

- **Block** — value is empty on the header line; the value is the lines
  that follow, up to the next header (or EOF). Leading and trailing blank
  lines in the block value are stripped.

  ```
  (* :Prompt: *)
  Aliquot Sequence
  In an aliquot sequence, ...

  (* :Tests: *)
  {AliquotSequence[20], {20, 22, 14, 10, 8, 7, 1}}
  {AliquotSequence[6],  {6}}
  ```

## Sections

| Key           | Required | Shape   | Meaning                                                       |
|---------------|----------|---------|---------------------------------------------------------------|
| `Name`        | yes      | scalar  | Stable identifier, also used for the on-disk filename slug.   |
| `Index`       | no       | scalar  | Integer; controls ordering when emitting the legacy JSON.     |
| `Instruction` | no       | scalar  | Optional preamble shown to the model alongside the prompt.    |
| `Prompt`      | yes      | block   | The challenge prompt body, verbatim.                          |
| `Tests`       | yes      | block   | One WL list literal per line (see below).                     |

## Test lines

Each non-blank, non-comment line inside the `:Tests:` block is parsed as a
WL list literal of one of two shapes:

```
{ input, expected }
{ input, expected, <| "key" -> value, ... |> }
```

- `input` is parsed *held*. It is what the candidate solution will be
  applied to; it must NOT be evaluated at build time, because most
  candidate function names won't exist yet.
- `expected` is evaluated at build time. The literal you write IS the
  expected result.
- Optional third element: an `Association` of per-test metadata
  (e.g. tolerance, custom `SameTestFunction` selector). Honoured by the
  runtime if `RunBenchmark` is configured to look at it.

Lines starting with `(*` (block comments on their own line) and blank
lines inside `:Tests:` are skipped, so you can group/annotate tests:

```
(* :Tests: *)
(* edge cases *)
{Aliquot[1], {1}}
{Aliquot[2], {2, 1}}

(* steady state *)
{Aliquot[20], {20, 22, 14, 10, 8, 7, 1}}
```

## Building

```
wolframscript -file scripts/BuildTestBank.wls \
  --in   .challenges \
  --json ChallengesTestDataV1.json \
  --wxf  ChallengesTests.wxf
```

Exits non-zero on any unparseable file. The CLI prints a per-challenge
summary (test count, byte sizes) so that diffs in the compiled bank
have an obvious source-of-change.

## Seeding from an existing WXF

To migrate an existing test bank into `.wlchallenge` files for editing:

```
wolframscript -file scripts/BuildTestBank.wls \
  --reverse \
  --json ChallengesTestDataV1.json \
  --wxf  ChallengesTests.wxf \
  --out  .challenges
```

This calls `WriteWLChallengeDir` and never evaluates the held inputs,
so it is safe to run against any compiled bank.
