#!/usr/bin/env bash
#
# Bootstrap a self-hosted GitHub Actions runner for this repository on
# macOS (Apple Silicon).
#
# What it does, end-to-end:
#   1. Verifies gh is installed and authenticated.
#   2. Gets a fresh registration token via `gh api`.
#   3. Looks up the latest runner release and downloads the macOS-arm64
#      tarball into ~/actions-runner.
#   4. Runs ./config.sh --unattended with the token.
#   5. Installs and starts the LaunchAgent so the runner survives reboots.
#   6. Prints the runner's idle status.
#
# Usage:
#   scripts/setup-ci-runner.sh
#
# Prerequisites:
#   - gh CLI installed (`brew install gh` if not), already authenticated
#     (`gh auth login`).
#   - Wolfram Desktop installed with `wolframscript` on PATH.
#
# Idempotency:
#   Errors out if ~/actions-runner already contains a configured runner
#   (.runner file present). To re-bootstrap from scratch:
#     cd ~/actions-runner && ./svc.sh uninstall && ./config.sh remove
#     rm -rf ~/actions-runner
#   Then rerun this script.

set -euo pipefail

REPO_OWNER="Jofreep"
REPO_NAME="WolframChallengesBenchmark"
REPO="${REPO_OWNER}/${REPO_NAME}"
RUNNER_DIR="${HOME}/actions-runner"

err()  { printf '\033[31mERROR\033[0m  %s\n' "$*" >&2; }
info() { printf '\033[34m==>\033[0m    %s\n' "$*"; }
ok()   { printf '\033[32mOK\033[0m     %s\n' "$*"; }

# ---------------------------------------------------------------------------
# 1. Preconditions
# ---------------------------------------------------------------------------

info "Checking prerequisites"

if ! command -v gh >/dev/null 2>&1; then
  err "gh CLI not found. Install with:  brew install gh"
  exit 1
fi

if ! gh auth status >/dev/null 2>&1; then
  err "gh is not authenticated. Run:  gh auth login"
  exit 1
fi

if ! command -v wolframscript >/dev/null 2>&1; then
  err "wolframscript not on PATH. CI needs it to run the test suite."
  exit 1
fi

if [[ -f "${RUNNER_DIR}/.runner" ]]; then
  err "Runner already registered at ${RUNNER_DIR}."
  err "To re-bootstrap: see the 'Idempotency' note at the top of this script."
  exit 1
fi

ARCH="$(uname -m)"
if [[ "${ARCH}" != "arm64" ]]; then
  err "This script targets Apple Silicon (arm64). Detected: ${ARCH}."
  err "Edit RUNNER_ARCH below if you know what you're doing."
  exit 1
fi
RUNNER_ARCH="osx-arm64"

ok "gh authenticated, wolframscript found, arch=${ARCH}"

# ---------------------------------------------------------------------------
# 2. Fresh registration token via gh api
# ---------------------------------------------------------------------------

info "Requesting fresh registration token from GitHub"

# Tokens are valid for one hour and are single-use.
TOKEN="$(gh api -X POST \
  "repos/${REPO}/actions/runners/registration-token" \
  --jq '.token')"

if [[ -z "${TOKEN}" ]]; then
  err "Empty token from gh api. Does your gh user have admin on ${REPO}?"
  exit 1
fi

ok "Got registration token (single-use, 1h validity)"

# ---------------------------------------------------------------------------
# 3. Find and download the latest runner release
# ---------------------------------------------------------------------------

info "Looking up latest actions/runner release"

RUNNER_VERSION="$(gh api repos/actions/runner/releases/latest \
  --jq '.tag_name' | sed 's/^v//')"

if [[ -z "${RUNNER_VERSION}" ]]; then
  err "Could not determine latest runner version."
  exit 1
fi

TARBALL="actions-runner-${RUNNER_ARCH}-${RUNNER_VERSION}.tar.gz"
URL="https://github.com/actions/runner/releases/download/v${RUNNER_VERSION}/${TARBALL}"

ok "Latest is v${RUNNER_VERSION}"

info "Downloading ${TARBALL} into ${RUNNER_DIR}"
mkdir -p "${RUNNER_DIR}"
cd "${RUNNER_DIR}"

# -L follows redirects; --fail flips 4xx/5xx into nonzero exit.
curl -Lf -o "${TARBALL}" "${URL}"
tar xzf "./${TARBALL}"
rm -f "./${TARBALL}"

ok "Runner unpacked into ${RUNNER_DIR}"

# ---------------------------------------------------------------------------
# 4. Configure the runner non-interactively
# ---------------------------------------------------------------------------

info "Registering the runner with ${REPO}"

# --unattended skips the prompts and accepts the defaults:
#   name        = $(hostname)
#   labels      = self-hosted, macOS, ARM64 (added automatically)
#   work folder = _work
./config.sh \
  --unattended \
  --url "https://github.com/${REPO}" \
  --token "${TOKEN}"

ok "Runner registered"

# ---------------------------------------------------------------------------
# 5. Install + start as a LaunchAgent so it survives reboots
# ---------------------------------------------------------------------------

info "Installing the runner as a macOS LaunchAgent"
./svc.sh install

info "Starting the runner service"
./svc.sh start

# Give it a moment to come up before we peek at status.
sleep 2
./svc.sh status || true

# ---------------------------------------------------------------------------
# 6. Confirm via the GitHub API
# ---------------------------------------------------------------------------

info "Verifying registration on GitHub"

gh api "repos/${REPO}/actions/runners" \
  --jq '.runners[] | "\(.name)  status=\(.status)  busy=\(.busy)  labels=\([.labels[].name] | join(","))"'

cat <<EOF

==============================================================
  Runner setup complete.
==============================================================

  - Visit https://github.com/${REPO}/actions to watch CI.
  - Any queued workflow runs targeting [self-hosted, macOS]
    should pick up within a few seconds.
  - Optional: add OPENROUTER_API_KEY as a repo secret at
    https://github.com/${REPO}/settings/secrets/actions/new
    to enable the real-network OpenRouter tests.

  To manage the runner later:
    cd ${RUNNER_DIR}
    ./svc.sh status    # check status
    ./svc.sh stop      # pause CI (jobs queue instead of running)
    ./svc.sh start     # resume
    ./svc.sh uninstall # remove the LaunchAgent

  To remove the runner entirely:
    cd ${RUNNER_DIR}
    ./svc.sh uninstall
    ./config.sh remove --token \$(gh api -X POST \\
      repos/${REPO}/actions/runners/remove-token --jq '.token')
    cd ~ && rm -rf ${RUNNER_DIR}

EOF
