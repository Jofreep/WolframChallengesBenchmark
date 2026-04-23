#!/usr/bin/env bash
# scripts/TestOpenRouterAPI.sh
#
# Production diagnostic for the OpenRouter API path the benchmark uses.
# Hits the live API with curl in five phases, each independently usable
# for triage:
#
#   1. Key file sanity         (no network)
#   2. /api/v1/key              (cheap auth + credit check)
#   3. /api/v1/models           (lists models, validates account scope)
#   4. /api/v1/chat/completions vs a known-good fast model
#   5. /api/v1/chat/completions vs the model under suspicion
#
# Each phase prints a clear PASS/FAIL line and exits with the highest
# failure code it saw, so the script is CI-friendly.
#
# Usage:
#   scripts/TestOpenRouterAPI.sh                       # uses ~/.config/openrouter/key.txt
#   scripts/TestOpenRouterAPI.sh --key-file PATH       # explicit key file
#   scripts/TestOpenRouterAPI.sh --model SLUG          # change suspect model
#   scripts/TestOpenRouterAPI.sh --good-model SLUG     # change reference model
#   scripts/TestOpenRouterAPI.sh --max-tokens N        # both completion phases
#   scripts/TestOpenRouterAPI.sh --timeout SEC         # per-request timeout
#   scripts/TestOpenRouterAPI.sh --verbose             # print full JSON responses
#
# Exit codes:
#   0  all phases passed
#   1  setup error (no key, bad CLI args)
#   2  one or more API phases failed (network, auth, or model)
#
# Dependencies: curl (required), jq (optional, falls back to python3 -m json.tool)

set -u
set -o pipefail

# ------------------------- defaults & arg parsing -------------------------

KEY_FILE="${HOME}/.config/openrouter/key.txt"
MODEL="minimax/minimax-m2.7"
GOOD_MODEL="google/gemini-2.5-flash"
MAX_TOKENS=64
TIMEOUT_SEC=60
VERBOSE=0
PROMPT='Reply with the single word: pong'

while [[ $# -gt 0 ]]; do
  case "$1" in
    --key-file)    KEY_FILE="$2";   shift 2 ;;
    --model)       MODEL="$2";      shift 2 ;;
    --good-model)  GOOD_MODEL="$2"; shift 2 ;;
    --max-tokens)  MAX_TOKENS="$2"; shift 2 ;;
    --timeout)     TIMEOUT_SEC="$2"; shift 2 ;;
    --prompt)      PROMPT="$2";     shift 2 ;;
    --verbose|-v)  VERBOSE=1;       shift ;;
    -h|--help)
      sed -n '2,40p' "$0" | sed 's/^# \{0,1\}//'
      exit 0
      ;;
    *)
      echo "ERROR: unknown arg: $1" >&2
      exit 1
      ;;
  esac
done

# ------------------------- helpers -------------------------

EXIT=0
WORST=0   # tracks highest seen exit-code so we can return it at end

bump() { local code="$1"; if (( code > WORST )); then WORST="$code"; fi; }

# Pretty JSON either via jq or python3 fallback. Read from stdin.
pretty_json() {
  if command -v jq >/dev/null 2>&1; then
    jq .
  else
    python3 -m json.tool 2>/dev/null || cat
  fi
}

# Pull a field from JSON without requiring jq. Usage: get_field <json> <jq_path> [pyexpr]
# jq_path  example: '.data.label'
# pyexpr   example: 'd["data"]["label"]'  (only used if jq missing)
get_field() {
  local json="$1" jqp="$2" pyexpr="${3:-}"
  if command -v jq >/dev/null 2>&1; then
    printf '%s' "$json" | jq -r "$jqp // empty" 2>/dev/null
  elif [[ -n "$pyexpr" ]]; then
    printf '%s' "$json" | python3 -c "
import json, sys
try:
  d = json.load(sys.stdin)
  v = $pyexpr
  print('' if v is None else v)
except Exception:
  pass
" 2>/dev/null
  fi
}

hr() { printf -- '----------------------------------------------------------------\n'; }

phase() { printf '\n[Phase %s] %s\n' "$1" "$2"; }

pass() { printf '  PASS  %s\n' "$1"; }
fail() { printf '  FAIL  %s\n' "$1"; bump 2; }
warn() { printf '  WARN  %s\n' "$1"; }

require_cmd() {
  if ! command -v "$1" >/dev/null 2>&1; then
    fail "required command not found: $1"
    exit 1
  fi
}

require_cmd curl

if ! command -v jq >/dev/null 2>&1; then
  warn "jq not found; falling back to python3 for JSON formatting"
  require_cmd python3
fi

# ------------------------- Phase 1: key file sanity -------------------------

phase 1 "Key file sanity (no network)"

if [[ ! -f "$KEY_FILE" ]]; then
  fail "key file not found: $KEY_FILE"
  echo "  hint: scripts/TestOpenRouterAPI.sh --key-file <path>"
  exit 1
fi

KEY_BYTES=$(wc -c < "$KEY_FILE" | tr -d ' ')
KEY_PERMS=$(stat -f '%Lp' "$KEY_FILE" 2>/dev/null || stat -c '%a' "$KEY_FILE" 2>/dev/null || echo "?")
API_KEY=$(tr -d '\r\n' < "$KEY_FILE")
KEY_LEN=${#API_KEY}

printf '  file:    %s\n' "$KEY_FILE"
printf '  bytes:   %s (trimmed length %s)\n' "$KEY_BYTES" "$KEY_LEN"
printf '  perms:   %s\n' "$KEY_PERMS"

if (( KEY_LEN < 30 )); then
  fail "key looks too short ($KEY_LEN chars) — likely empty or truncated"
  exit 1
fi

if [[ "$API_KEY" != sk-or-* ]]; then
  warn "key does not start with sk-or-* (got: ${API_KEY:0:6}...) — OpenRouter keys usually do"
fi

if [[ "$KEY_PERMS" != "600" && "$KEY_PERMS" != "400" ]]; then
  warn "key file permissions are $KEY_PERMS — recommend 600 (run: chmod 600 $KEY_FILE)"
fi

pass "key file readable, ${KEY_LEN} chars, prefix ${API_KEY:0:8}…"

# ------------------------- Phase 2: /api/v1/key -------------------------

phase 2 "GET /api/v1/key  (auth + credit info)"

RESP_FILE=$(mktemp)
HTTP_CODE=$(curl -sS \
  --max-time "$TIMEOUT_SEC" \
  -o "$RESP_FILE" \
  -w '%{http_code}' \
  -H "Authorization: Bearer $API_KEY" \
  https://openrouter.ai/api/v1/key) || HTTP_CODE="curl-error"

BODY=$(cat "$RESP_FILE"); rm -f "$RESP_FILE"

printf '  http:    %s\n' "$HTTP_CODE"

if [[ "$HTTP_CODE" == "200" ]]; then
  LABEL=$(get_field "$BODY" '.data.label' 'd["data"]["label"]')
  USAGE=$(get_field "$BODY" '.data.usage' 'd["data"]["usage"]')
  LIMIT=$(get_field "$BODY" '.data.limit' 'd["data"]["limit"]')
  IS_FREE=$(get_field "$BODY" '.data.is_free_tier' 'd["data"]["is_free_tier"]')
  printf '  label:   %s\n' "${LABEL:-(none)}"
  printf '  usage:   %s\n' "${USAGE:-?}"
  printf '  limit:   %s\n' "${LIMIT:-(none = unlimited)}"
  printf '  free?    %s\n' "${IS_FREE:-?}"
  pass "key is valid"
elif [[ "$HTTP_CODE" == "401" ]]; then
  fail "401 Unauthorized — key is invalid or revoked"
  echo "$BODY" | pretty_json
  echo "  hint: rotate the key at https://openrouter.ai/keys"
elif [[ "$HTTP_CODE" == "curl-error" ]]; then
  fail "curl could not reach openrouter.ai — check connectivity / firewall / DNS"
else
  fail "unexpected status $HTTP_CODE"
  echo "$BODY" | pretty_json
fi

# ------------------------- Phase 3: /api/v1/models -------------------------

phase 3 "GET /api/v1/models  (catalog reachable)"

RESP_FILE=$(mktemp)
HTTP_CODE=$(curl -sS \
  --max-time "$TIMEOUT_SEC" \
  -o "$RESP_FILE" \
  -w '%{http_code}' \
  -H "Authorization: Bearer $API_KEY" \
  https://openrouter.ai/api/v1/models) || HTTP_CODE="curl-error"

BODY=$(cat "$RESP_FILE"); rm -f "$RESP_FILE"

printf '  http:    %s\n' "$HTTP_CODE"

if [[ "$HTTP_CODE" == "200" ]]; then
  if command -v jq >/dev/null 2>&1; then
    NMODELS=$(printf '%s' "$BODY" | jq -r '.data | length' 2>/dev/null || echo "?")
    HAS_GOOD=$(printf '%s' "$BODY" | jq -r --arg id "$GOOD_MODEL" '.data | map(select(.id==$id)) | length' 2>/dev/null)
    HAS_SUSPECT=$(printf '%s' "$BODY" | jq -r --arg id "$MODEL"      '.data | map(select(.id==$id)) | length' 2>/dev/null)
  else
    NMODELS=$(printf '%s' "$BODY" | python3 -c "import json,sys; print(len(json.load(sys.stdin)['data']))" 2>/dev/null || echo "?")
    HAS_GOOD=$(printf '%s' "$BODY" | python3 -c "import json,sys; ms=json.load(sys.stdin)['data']; print(sum(1 for m in ms if m['id']=='$GOOD_MODEL'))" 2>/dev/null)
    HAS_SUSPECT=$(printf '%s' "$BODY" | python3 -c "import json,sys; ms=json.load(sys.stdin)['data']; print(sum(1 for m in ms if m['id']=='$MODEL'))" 2>/dev/null)
  fi
  printf '  models:  %s available\n' "$NMODELS"
  printf '  %s : %s\n' "$GOOD_MODEL" "$( (( ${HAS_GOOD:-0} > 0 )) && echo present || echo MISSING )"
  printf '  %s : %s\n' "$MODEL"      "$( (( ${HAS_SUSPECT:-0} > 0 )) && echo present || echo MISSING )"
  if (( ${HAS_GOOD:-0} > 0 )) && (( ${HAS_SUSPECT:-0} > 0 )); then
    pass "both models in catalog"
  else
    fail "one or both models missing from catalog — check the slug"
  fi
else
  fail "unexpected status $HTTP_CODE"
  echo "$BODY" | pretty_json
fi

# ------------------------- shared completion runner -------------------------

# Args: <model_slug> <phase_num> <label>
run_chat_completion() {
  local model="$1" phase_num="$2" label="$3"

  phase "$phase_num" "POST /api/v1/chat/completions  ($label: $model)"

  local body
  body=$(cat <<EOF
{
  "model": "$model",
  "messages": [
    {"role": "system", "content": "You are a connection test endpoint. Respond exactly as instructed."},
    {"role": "user",   "content": "$PROMPT"}
  ],
  "max_tokens": $MAX_TOKENS,
  "temperature": 0
}
EOF
)

  local resp_file http_code resp_body t0 t1 dt
  resp_file=$(mktemp)
  t0=$(date +%s)
  http_code=$(curl -sS \
    --max-time "$TIMEOUT_SEC" \
    -o "$resp_file" \
    -w '%{http_code}' \
    -H "Authorization: Bearer $API_KEY" \
    -H "Content-Type: application/json" \
    -H "HTTP-Referer: https://github.com/JofreEspigulePons/WolframChallengesBenchmark" \
    -H "X-Title: WolframChallengesBenchmark" \
    -d "$body" \
    https://openrouter.ai/api/v1/chat/completions) || http_code="curl-error"
  t1=$(date +%s)
  dt=$(( t1 - t0 ))

  resp_body=$(cat "$resp_file"); rm -f "$resp_file"

  printf '  http:        %s\n' "$http_code"
  printf '  latency:     %ss\n' "$dt"

  if [[ "$http_code" == "curl-error" ]]; then
    fail "curl could not complete the request within ${TIMEOUT_SEC}s"
    return
  fi

  if [[ "$http_code" != "200" ]]; then
    fail "HTTP $http_code from /chat/completions"
    echo "$resp_body" | pretty_json
    return
  fi

  # Auth + transport OK; now check the actual content.
  local content content_len finish_reason prompt_tok comp_tok total_tok reasoning_present
  content=$(get_field         "$resp_body" '.choices[0].message.content'           'd["choices"][0]["message"].get("content")')
  finish_reason=$(get_field   "$resp_body" '.choices[0].finish_reason'             'd["choices"][0].get("finish_reason")')
  prompt_tok=$(get_field      "$resp_body" '.usage.prompt_tokens'                  'd["usage"]["prompt_tokens"]')
  comp_tok=$(get_field        "$resp_body" '.usage.completion_tokens'              'd["usage"]["completion_tokens"]')
  total_tok=$(get_field       "$resp_body" '.usage.total_tokens'                   'd["usage"]["total_tokens"]')
  reasoning_present=$(get_field "$resp_body" '.choices[0].message.reasoning'       'd["choices"][0]["message"].get("reasoning")' )

  content_len=${#content}

  printf '  finish:      %s\n' "${finish_reason:-?}"
  printf '  tokens:      prompt=%s completion=%s total=%s\n' "${prompt_tok:-?}" "${comp_tok:-?}" "${total_tok:-?}"
  printf '  content len: %s chars\n' "$content_len"
  if [[ -n "$reasoning_present" ]]; then
    local rlen=${#reasoning_present}
    printf '  reasoning:   present, %s chars\n' "$rlen"
  else
    printf '  reasoning:   absent\n'
  fi

  if [[ "$VERBOSE" == "1" ]]; then
    hr; echo "$resp_body" | pretty_json; hr
  fi

  if (( content_len > 0 )); then
    # Show the first line of content as evidence
    local first_line
    first_line=$(printf '%s' "$content" | head -n1 | cut -c1-80)
    printf '  reply (first line): %s\n' "$first_line"
    pass "model returned non-empty content"
  else
    if [[ "$finish_reason" == "length" ]]; then
      fail "empty content + finish_reason=length — model exhausted token budget on hidden reasoning channel"
      if [[ -n "$reasoning_present" ]]; then
        echo "  diagnosis: all tokens went to .message.reasoning instead of .message.content"
        echo "  remediation: raise --max-tokens, or pick a non-reasoning model"
      fi
    else
      fail "empty content with finish_reason=${finish_reason:-?}"
      echo "$resp_body" | pretty_json | head -40
    fi
  fi
}

# ------------------------- Phase 4: known-good model -------------------------

run_chat_completion "$GOOD_MODEL" 4 "reference model"

# ------------------------- Phase 5: suspect model -------------------------

run_chat_completion "$MODEL" 5 "suspect model"

# ------------------------- summary -------------------------

hr
if (( WORST == 0 )); then
  printf 'OVERALL: PASS  (5/5 phases)\n'
  exit 0
elif (( WORST == 1 )); then
  printf 'OVERALL: SETUP ERROR\n'
  exit 1
else
  printf 'OVERALL: FAIL  (one or more phases failed — see above)\n'
  exit 2
fi
