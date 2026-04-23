#!/usr/bin/env bash
# GenerateSolutionsResumeLoop.sh
#
# Auto-resume wrapper around GenerateSolutions.wls.
#
# Motivation:
#   wolframscript on macOS occasionally dies mid-run when URLRead
#   encounters a flaky TCP connection or SSL handshake failure.  The
#   symptom is a bare "$TimedOut" printed to stdout, the kernel exits,
#   and the JSONL audit log ends with generate.aborted.  The script has
#   in-kernel CheckAbort guards at two layers (OpenRouter.wl around
#   URLRead and Generator.wl around the per-attempt call), but a fatal
#   signal (SIGSEGV on OpenSSL code paths has been observed) kills the
#   kernel before Wolfram's exception machinery can catch anything.
#
#   GenerateSolutions.wls is idempotent: without --overwrite it skips
#   any challenge whose <name>.wl file already exists under
#   <out>/<modelSlug>/.  So we can just relaunch the same invocation
#   until the generator reports 0 challenges queued (everything is
#   already on disk) or we hit --max-resumes.
#
# Usage:
#   scripts/GenerateSolutionsResumeLoop.sh \
#     --model google/gemini-2.5-flash \
#     [--max-resumes 6] \
#     [--] <all other flags forwarded to GenerateSolutions.wls>
#
# The wrapper requires these GenerateSolutions args in the forwarded
# flags (or it falls back to the defaults shown):
#   --challenges ChallengesTestDataV1.json
#   --testbank   ChallengesTests.wxf
#   --out        solutions
#   --api-key-file ~/.config/openrouter/key.txt
#
# Every iteration writes its own timestamped log and jsonl under runs/,
# named generate-<modelSlug>-<ISO_UTC>.{stdout.log,jsonl}.
#
# Exit codes:
#   0  all challenges saved (generate.finished) or nothing left to queue
#   1  invocation error (bad flags, missing files)
#   2  reached --max-resumes without finishing; some challenges still
#      unprocessed.  Inspect the most recent JSONL for the last
#      generate.aborted and resume manually.

set -euo pipefail

MAX_RESUMES=6
MODEL=""
# Forwarded args default set.
declare -a FWD=()
CHALLENGES_DEFAULT="ChallengesTestDataV1.json"
TESTBANK_DEFAULT="ChallengesTests.wxf"
OUT_DEFAULT="solutions"
API_KEY_FILE_DEFAULT="$HOME/.config/openrouter/key.txt"

# ---- arg parse: pull out --model and --max-resumes, forward the rest ----
while [[ $# -gt 0 ]]; do
  case "$1" in
    --max-resumes) MAX_RESUMES="$2"; shift 2;;
    --model)       MODEL="$2"; FWD+=("$1" "$2"); shift 2;;
    --)            shift; FWD+=("$@"); break;;
    *)             FWD+=("$1"); shift;;
  esac
done

if [[ -z "$MODEL" ]]; then
  echo "ERROR: --model is required (e.g. --model google/gemini-2.5-flash)" >&2
  exit 1
fi

# Fill in GenerateSolutions defaults if caller didn't pass them.
has_flag() { local flag="$1"; shift; for a in "$@"; do [[ "$a" == "$flag" ]] && return 0; done; return 1; }
has_flag --challenges   "${FWD[@]}" || FWD+=(--challenges   "$CHALLENGES_DEFAULT")
has_flag --testbank     "${FWD[@]}" || FWD+=(--testbank     "$TESTBANK_DEFAULT")
has_flag --out          "${FWD[@]}" || FWD+=(--out          "$OUT_DEFAULT")
has_flag --api-key-file "${FWD[@]}" || FWD+=(--api-key-file "$API_KEY_FILE_DEFAULT")

# modelSlug mirrors Kernel/Generator.wl modelSlug: / -> _
MODEL_SLUG="${MODEL//\//_}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$SCRIPT_DIR/.." && pwd)"
cd "$REPO"

mkdir -p runs
echo "[resume-loop] model=$MODEL  modelSlug=$MODEL_SLUG  max-resumes=$MAX_RESUMES"
echo "[resume-loop] forwarded args: ${FWD[*]}"

for attempt in $(seq 1 "$MAX_RESUMES"); do
  TS="$(date -u +%Y%m%d-%H%M%S)"
  JSONL="runs/generate-${MODEL_SLUG}-${TS}.jsonl"
  STDOUT="runs/generate-${MODEL_SLUG}-${TS}.stdout.log"

  echo
  echo "==================================================================="
  echo "[resume-loop] attempt $attempt/$MAX_RESUMES starting at $TS"
  echo "[resume-loop] jsonl:  $JSONL"
  echo "[resume-loop] stdout: $STDOUT"
  echo "==================================================================="

  set +e
  wolframscript -file scripts/GenerateSolutions.wls \
    "${FWD[@]}" \
    --log "$JSONL" \
    >"$STDOUT" 2>&1
  RC=$?
  set -e

  # Summarize what this iteration accomplished.
  if [[ -s "$JSONL" ]]; then
    python3 - "$JSONL" <<'PY'
import json, sys, collections
events = [json.loads(l) for l in open(sys.argv[1]) if l.strip()]
c = collections.Counter(e.get("event","?") for e in events)
ok    = c.get("challenge.saved", 0)
rej   = c.get("challenge.auditRejected", 0)
start = next((e for e in events if e.get("event") == "generate.start"), None)
term  = next((e for e in reversed(events)
              if e.get("event") in ("generate.finished","generate.aborted")), None)
queued = start["toProcess"] if start else "?"
if term:
    print(f"[resume-loop]   iteration result: {term['event']}  "
          f"saved={ok}  audit-rejected={rej}  queued={queued}  "
          f"completed={term.get('completedCount','?')}  last={term.get('lastName','?')}")
else:
    print(f"[resume-loop]   iteration result: NO TOMBSTONE  "
          f"saved={ok}  audit-rejected={rej}  queued={queued}  rc={RC}")
PY
  else
    echo "[resume-loop]   iteration produced empty JSONL (rc=$RC)"
  fi

  # Probe whether there's anything left to do by dry-running a count.
  # Cheaper heuristic: the next iteration's generate.start will report
  # toProcess=0 if everything is saved.  We can peek at the last
  # generate.start in the most-recent jsonl we just wrote.
  REMAINING=$(python3 - "$JSONL" <<'PY'
import json, sys
events = [json.loads(l) for l in open(sys.argv[1]) if l.strip()]
start = next((e for e in events if e.get("event") == "generate.start"), None)
if not start:
    print("unknown"); sys.exit(0)
saved = sum(1 for e in events if e.get("event") == "challenge.saved")
queued = start.get("toProcess", 0)
# Remaining at start of NEXT run = queued - saved_this_run + rejected_this_run
# (audit-rejected leaves no .wl, so it'll be requeued; that's a content
# issue, not a transient failure, so we ignore it for terminate check.)
rej = sum(1 for e in events if e.get("event") == "challenge.auditRejected")
print(queued - saved - rej)
PY
  )
  echo "[resume-loop]   remaining after this iteration (excluding audit-rejected): $REMAINING"

  if [[ "$REMAINING" == "0" ]]; then
    echo
    echo "[resume-loop] SUCCESS: no challenges left to process."
    exit 0
  fi

  # Also stop if the tombstone was generate.finished (generator ran to
  # completion on its own) - any remaining unprocessed entries are
  # audit-rejected ones we can't fix by retrying.
  if grep -q '"event":"generate.finished"' "$JSONL"; then
    echo
    echo "[resume-loop] SUCCESS: generator reported generate.finished."
    exit 0
  fi

  # Short sleep between resumes to let any transient network issue clear.
  sleep 3
done

echo
echo "[resume-loop] FAILED: reached --max-resumes=$MAX_RESUMES without finishing."
echo "[resume-loop] Inspect the most recent JSONL for the last generate.aborted."
exit 2
