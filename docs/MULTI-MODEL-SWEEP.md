# Multi-model sweep runbook

Generate solutions and grade three models end-to-end, then render the
leaderboard.  Repeat the per-model block below for each new model
\[LongDash] same shape every time.

Assumes:
- `OPENROUTER_API_KEY` is in your environment (or you'll be prompted).
- You're at the repo root.
- You're OK spending OpenRouter credits.  Estimates per model below.

## 1. Set up the API key

```
export OPENROUTER_API_KEY=sk-or-v1-...   # one time, in your shell
```

Or store it in `~/.config/openrouter/key.txt` (one line, no
trailing whitespace) and pass `--api-key-file` to GenerateSolutions.

## 2. Per-model: generate \[Rule] benchmark

Pattern:

```
# --- generate (15-45 min depending on model speed) ---
wolframscript -file scripts/GenerateSolutions.wls \
  --model     <model-slug> \
  --out       solutions
# --- run (5-25 min depending on test density) ---
wolframscript -file scripts/RunBenchmark.wls \
  --solutions solutions/<model-slug-with-/-replaced-by-_> \
  --model     <model-slug> \
  --out       runs
```

Note the slug-with-`/`-replaced-by-`_` rule: solutions for
`google/gemini-3.1-pro-preview` land at
`solutions/google_gemini-3.1-pro-preview/`.  This mirrors how the
generator writes them.

### 2a. google/gemini-3.1-pro-preview

```
wolframscript -file scripts/GenerateSolutions.wls \
  --model google/gemini-3.1-pro-preview --out solutions
wolframscript -file scripts/RunBenchmark.wls \
  --solutions solutions/google_gemini-3.1-pro-preview \
  --model     google/gemini-3.1-pro-preview \
  --out       runs
```

### 2b. moonshotai/kimi-k2.6

```
wolframscript -file scripts/GenerateSolutions.wls \
  --model moonshotai/kimi-k2.6 --out solutions
wolframscript -file scripts/RunBenchmark.wls \
  --solutions solutions/moonshotai_kimi-k2.6 \
  --model     moonshotai/kimi-k2.6 \
  --out       runs
```

### 2c. anthropic/claude-opus-4.7

```
wolframscript -file scripts/GenerateSolutions.wls \
  --model anthropic/claude-opus-4.7 --out solutions
wolframscript -file scripts/RunBenchmark.wls \
  --solutions solutions/anthropic_claude-opus-4.7 \
  --model     anthropic/claude-opus-4.7 \
  --out       runs
```

## 3. Add the existing claude-opus-4.6 + gemini-2.5-flash points

You already have full solution sets for these two on disk
(`solutions/claude-opus-4.6/` and
`solutions/google_gemini-2.5-flash/`).  Just run the benchmark; no
generation needed.  Free.

```
wolframscript -file scripts/RunBenchmark.wls \
  --solutions solutions/claude-opus-4.6 \
  --model     anthropic/claude-opus-4.6 \
  --out       runs

wolframscript -file scripts/RunBenchmark.wls \
  --solutions solutions/google_gemini-2.5-flash \
  --model     google/gemini-2.5-flash \
  --out       runs
```

## 4. Render the leaderboard

```
wolframscript -file scripts/Leaderboard.wls
open runs/leaderboard/report.html
```

Auto-discovers every benchmark `run.json` under `runs/`, picks the
most recent run per model, ranks by pass rate, renders the podium
+ table.  Re-run any time a new run lands.

## 5. (Optional) Trend over time + per-challenge cross-model

```
wolframscript -file scripts/TrendReport.wls
open runs/trend/report.html

wolframscript -file scripts/CompareModels.wls --all runs --out runs/compare
open runs/compare/compare.html
```

## Troubleshooting

- **A generation run dies mid-way.**  `scripts/GenerateSolutionsResumeLoop.sh
  --model <slug>` resumes by skipping any task that already has a
  `solutions/<slug>/<task_id>.wl` on disk.
- **One challenge consistently times out.**  Either bump
  `--timeout` on RunBenchmark or filter it out with
  `--filter -SlowChallengeName` for a clean leaderboard run.
- **Pass rate looks suspiciously low.**  Run the audit:
  `wolframscript -file scripts/RunBenchmark.wls --solutions
  solutions/<slug> --model <slug> --filter FizzBuzz --verify-solutions
  --strict-verify` to surface mislabeled solutions before the grade.
- **Rate-limited / 429 from OpenRouter.**  Bump `--retry-base-delay 5`
  on GenerateSolutions, or split across multiple sessions.
