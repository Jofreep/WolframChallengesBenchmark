# CI setup

The `Tests` workflow (`.github/workflows/tests.yml`) runs the paclet's
VerificationTest suite on every push and pull request against `main`. It
targets a self-hosted macOS runner because Wolfram Language is not
pre-installed on GitHub's hosted runners.

## Self-hosted macOS runner (default path)

You install the GitHub Actions runner agent on a Mac that already has
Wolfram Desktop installed. Jobs pick up whenever that Mac is online and
the agent is running.

### One-time setup

1. On GitHub, go to **Settings** → **Actions** → **Runners** →
   **New self-hosted runner** for this repository. Pick **macOS /
   ARM64** (or Intel, depending on your Mac).

2. Follow the on-page instructions to download and configure the agent.
   The flow looks like:

   ```
   mkdir ~/actions-runner && cd ~/actions-runner
   curl -o actions-runner-osx-arm64.tar.gz -L <url-from-github>
   tar xzf actions-runner-osx-arm64.tar.gz
   ./config.sh --url https://github.com/<user>/WolframChallengesBenchmark --token <token-from-github>
   ```

   When prompted, add the labels `self-hosted` and `macOS` (these are
   what the workflow's `runs-on: [self-hosted, macOS]` resolves to;
   they're added automatically).

3. Start the runner as a LaunchAgent so it boots automatically:

   ```
   ./svc.sh install
   ./svc.sh start
   ```

   Or run it foreground for testing with `./run.sh`.

4. Verify on GitHub that the runner shows up as **Idle** under
   Settings → Actions → Runners.

### Secrets the workflow uses

Settings → Secrets and variables → Actions → New repository secret:

| Secret                | Required | Why                                                                                    |
|-----------------------|----------|----------------------------------------------------------------------------------------|
| `OPENROUTER_API_KEY`  | Optional | Enables the real-network OpenRouter integration checks in `Tests/OpenRouter.wlt`. Without it those tests still pass (they degrade gracefully). |

### What the workflow does

- Triggers on push to `main`, PR to `main`, and manual `workflow_dispatch`.
- Runs `wolframscript -file WolframChallengesLLMBenchmarkPaclet/Tests/RunTests.wls --junit junit.xml`.
- Uploads the generated `junit.xml` as an artifact (30-day retention).
- Publishes a pass/fail check summary to the PR via the
  `mikepenz/action-junit-report` action — any failing test shows up
  inline in the GitHub UI.
- Exit code from the test runner is authoritative: 0 = all green,
  1 = at least one failure.
- Concurrent pushes on the same ref cancel earlier in-flight runs so
  stale CI doesn't queue up behind new commits.

## Hosted runner (Docker alternative)

When / if you want to drop the Mac dependency, the same workflow file
ships a commented-out `paclet-tests-docker` job that runs on a regular
`ubuntu-latest` GitHub-hosted runner inside the official
`wolframresearch/wolframengine:14` container. This requires a free
**Wolfram Engine for Developers** entitlement:

1. Register at <https://account.wolfram.com/auth/signup> and
   <https://www.wolfram.com/engine/> to get a Wolfram Engine
   entitlement ID.
2. Add it as a repo secret named `WOLFRAM_ENGINE_ENTITLEMENT`.
3. Uncomment the `paclet-tests-docker:` block in `tests.yml` and remove
   the `if: false` guard.

## Bank self-test job

The workflow has a second job (`bank-self-test`) that runs every
canonical reference solution against its own tests. Catches bank-edit
mistakes (e.g. wrong `expected_wl`) before they leak into a real
model run.

Canonical solutions live in `private/canonical_solutions.jsonl`,
gitignored. The job stages them from a stable local path on the
runner machine, defaulting to `~/wclb-private/canonical_solutions.jsonl`.

### One-time setup

```
mkdir -p ~/wclb-private
cp ~/Documents/Claude/Projects/WolframChallengesBenchmark/private/canonical_solutions.jsonl \
   ~/wclb-private/
```

Refresh `~/wclb-private/canonical_solutions.jsonl` whenever you
regenerate the canonical set.

### Job behavior

- If `~/wclb-private/canonical_solutions.jsonl` is **present**, the
  job copies it into the workspace, runs `scripts/BankSelfTest.wls`,
  and fails with exit 2 if any canonical doesn't pass its own bank
  entry.
- If the file is **missing**, the job logs a "skipping" notice and
  exits cleanly. CI never blocks on machines that don't have the
  private file (so any future hosted-runner job stays green).

### Override the local path

Set the repo-level Actions variable `BANK_PRIVATE_DIR` to point at a
different directory:

Settings → Secrets and variables → Actions → Variables →
New repository variable: name `BANK_PRIVATE_DIR`, value e.g.
`/Volumes/PrivateBenchData`.

## Manual runs

- From the repo on github.com: **Actions** → **Tests** → **Run workflow**.
- From the CLI: `gh workflow run tests.yml`.

## Locally (no CI)

The same entry point the workflow uses can be run directly:

```
wolframscript -file WolframChallengesLLMBenchmarkPaclet/Tests/RunTests.wls
```

Add `--junit junit.xml` to emit the same JUnit XML CI consumes.
