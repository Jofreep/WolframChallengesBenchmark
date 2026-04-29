#!/usr/bin/env bash
# generate_until_complete.sh
#
# Bash wrapper around scripts/GenerateSolutions.wls that retries until
# every challenge has a .wl, or saturation is reached, or max
# iterations.
#
# Why a Bash wrapper instead of the .wls one we tried first?  When the
# inner wolframscript exits via a top-level `$TimedOut` print (the
# class of failure we keep hitting on slow opus-4.7 calls), some kernel
# signal interaction on macOS knocks out a parent wolframscript that
# launched it via RunProcess.  A Bash parent doesn't share that fate:
# it just observes the child's non-zero exit code and runs the next
# iteration in a fresh kernel.
#
# Each iteration:
#   1. Compute the set of challenges still missing a .wl.
#   2. If empty, exit 0.
#   3. Invoke wolframscript -file scripts/GenerateSolutions.wls with
#      --filter <missing,...>, plus --overwrite from iter 2+ so audit-
#      rejected stubs get retried.
#   4. Recompute missing.  If unchanged, increment a no-progress counter.
#   5. Stop on max-iterations, or on N consecutive zero-progress passes.
#
# Usage:
#   scripts/generate_until_complete.sh \
#     --model anthropic/claude-opus-4.7 \
#     --api-key-file ~/.config/openrouter/key.txt \
#     [--out solutions]                      [--jsonl challenges.jsonl] \
#     [--max-tokens 8192]                    [--temperature 0.0] \
#     [--timeout 180]                        [--retries 3] \
#     [--retry-base-delay 2.0]               [--max-iterations 6] \
#     [--saturation-threshold 2]             [--bump-timeout-each]
#
# Exit codes:
#   0  every challenge has a .wl
#   1  invalid arguments / missing files
#   2  partial: max-iterations or saturation hit
#   3  no API key

set -uo pipefail

# ---- Defaults ---------------------------------------------------------
MODEL=""
OUT="solutions"
JSONL="challenges.jsonl"
API_KEY_FILE=""
MAX_TOKENS=8192
TEMPERATURE=0.0
TIMEOUT=180
RETRIES=3
RETRY_BASE_DELAY=2.0
MAX_ITERATIONS=6
SATURATION_THRESHOLD=2
BUMP_TIMEOUT=0

# ---- Arg parsing ------------------------------------------------------
while [[ $# -gt 0 ]]; do
  case $1 in
    --model)                MODEL="$2"; shift 2;;
    --out)                  OUT="$2"; shift 2;;
    --jsonl)                JSONL="$2"; shift 2;;
    --api-key-file)         API_KEY_FILE="$2"; shift 2;;
    --max-tokens)           MAX_TOKENS="$2"; shift 2;;
    --temperature)          TEMPERATURE="$2"; shift 2;;
    --timeout)              TIMEOUT="$2"; shift 2;;
    --retries)              RETRIES="$2"; shift 2;;
    --retry-base-delay)     RETRY_BASE_DELAY="$2"; shift 2;;
    --max-iterations)       MAX_ITERATIONS="$2"; shift 2;;
    --saturation-threshold) SATURATION_THRESHOLD="$2"; shift 2;;
    --bump-timeout-each)    BUMP_TIMEOUT=1; shift;;
    -h|--help)
      sed -n '2,30p' "$0"; exit 0;;
    *)
      echo "ERROR: unknown arg: $1" >&2; exit 1;;
  esac
done

if [[ -z "$MODEL" ]]; then
  echo "ERROR: --model is required" >&2; exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO="$(dirname "$SCRIPT_DIR")"

if [[ ! -f "$JSONL" ]]; then
  # Try repo-relative as a courtesy.
  if [[ -f "$REPO/$JSONL" ]]; then
    JSONL="$REPO/$JSONL"
  else
    echo "ERROR: challenges.jsonl not found at $JSONL" >&2; exit 1
  fi
fi

# API key check.
if [[ -z "${OPENROUTER_API_KEY:-}" ]] && [[ -z "$API_KEY_FILE" || ! -f "$API_KEY_FILE" ]]; then
  echo "ERROR: no OpenRouter API key.  Set OPENROUTER_API_KEY or --api-key-file PATH." >&2
  exit 3
fi

MODEL_SLUG="$(echo "$MODEL" | sed 's/[^A-Za-z0-9_.-]/_/g')"
OUT_DIR="$OUT/$MODEL_SLUG"

# ---- Helpers ----------------------------------------------------------

compute_missing() {
  # Pass JSONL + OUT_DIR as argv so we don't depend on Bash 4.4+
  # quoting (${var@Q}); macOS still ships Bash 3.2.
  python3 - "$JSONL" "$OUT_DIR" <<'PY'
import json, os, sys
jsonl, outdir = sys.argv[1], sys.argv[2]
names = set()
with open(jsonl) as f:
    for line in f:
        try:
            names.add(json.loads(line)["name"])
        except Exception:
            pass
have = set()
if os.path.isdir(outdir):
    for fn in os.listdir(outdir):
        if fn.endswith(".wl"):
            have.add(fn[:-3])
print(",".join(sorted(names - have)))
PY
}

count_wl() {
  if [[ -d "$OUT_DIR" ]]; then
    find "$OUT_DIR" -maxdepth 1 -name '*.wl' | wc -l | tr -d ' '
  else
    echo 0
  fi
}

TOTAL=$(python3 -c 'import json,sys; print(sum(1 for _ in open(sys.argv[1])))' "$JSONL")

# ---- Header -----------------------------------------------------------

initial=$(count_wl)
echo "generate_until_complete.sh"
echo "  model:                $MODEL"
echo "  outDir:               $OUT_DIR"
echo "  bank:                 $TOTAL challenges"
echo "  have:                 $initial .wl files"
echo "  missing:              $((TOTAL - initial))"
echo "  max iterations:       $MAX_ITERATIONS"
echo "  saturation threshold: $SATURATION_THRESHOLD"
echo "  bump timeout:         $([[ $BUMP_TIMEOUT -eq 1 ]] && echo YES || echo no)"
echo

if [[ $initial -ge $TOTAL ]]; then
  echo "Nothing to do."; exit 0
fi

# ---- Main loop --------------------------------------------------------

iter=0
no_progress=0
current_timeout=$TIMEOUT
prev_count=$initial

while :; do
  missing="$(compute_missing)"
  if [[ -z "$missing" ]]; then
    echo "All $TOTAL challenges have .wl files. Done."
    exit 0
  fi

  iter=$((iter + 1))
  missing_count=$(echo "$missing" | tr ',' '\n' | wc -l | tr -d ' ')
  overwrite_label=$([[ $iter -gt 1 ]] && echo YES || echo no)

  echo "=== Iteration $iter / $MAX_ITERATIONS  --  missing=$missing_count, timeout=${current_timeout}s, overwrite=$overwrite_label ==="

  ts=$(date +%Y-%m-%d_%H%M%S)
  log_jsonl="$REPO/runs/generate-${MODEL_SLUG}-iter$(printf '%02d' $iter)-${ts}.jsonl"

  args=(
    --out             "$OUT"
    --model           "$MODEL"
    --jsonl           "$JSONL"
    --max-tokens      "$MAX_TOKENS"
    --temperature     "$TEMPERATURE"
    --timeout         "$current_timeout"
    --retries         "$RETRIES"
    --retry-base-delay "$RETRY_BASE_DELAY"
    --filter          "$missing"
    --log             "$log_jsonl"
  )
  [[ -n "$API_KEY_FILE" ]] && args+=( --api-key-file "$API_KEY_FILE" )
  [[ $iter -gt 1 ]] && args+=( --overwrite )

  # Run the inner generator.  Don't `set -e` here -- the inner script
  # may exit non-zero (status 2 = some failures, or non-zero from a
  # top-level $TimedOut) and that's expected; we look at the
  # filesystem to see actual progress, not the exit code.
  echo "  $ wolframscript -file $SCRIPT_DIR/GenerateSolutions.wls ${args[*]}"
  echo "  --- subprocess output below ---"
  wolframscript -file "$SCRIPT_DIR/GenerateSolutions.wls" "${args[@]}"
  rc=$?
  echo "  --- subprocess exited (rc=$rc) ---"

  new_count=$(count_wl)
  gained=$((new_count - prev_count))
  echo
  echo "  iter $iter: gained=$gained new .wl files, remaining=$((TOTAL - new_count))"
  echo

  if [[ $gained -le 0 ]]; then
    no_progress=$((no_progress + 1))
  else
    no_progress=0
  fi

  if [[ $no_progress -ge $SATURATION_THRESHOLD ]]; then
    echo "Saturation reached: $SATURATION_THRESHOLD consecutive zero-progress passes. Stopping."
    break
  fi

  if [[ $iter -ge $MAX_ITERATIONS ]]; then
    echo "Hit --max-iterations $MAX_ITERATIONS. Stopping."
    break
  fi

  if [[ $BUMP_TIMEOUT -eq 1 ]]; then
    current_timeout=$(( current_timeout * 3 / 2 ))
    echo "  bumping per-call timeout -> ${current_timeout}s"
    echo
  fi

  prev_count=$new_count
done

# ---- Summary ----------------------------------------------------------

final=$(count_wl)
echo
echo "=== Final state ==="
echo "  have:     $final / $TOTAL"
echo "  missing:  $((TOTAL - final))"

if [[ $final -ge $TOTAL ]]; then
  exit 0
else
  exit 2
fi
