# Going public runbook

End-to-end checklist for flipping the repo from private to public,
keeping canonical solutions and model-generated solutions out of the
new public history.

The current commit on `main` already strips `solutions/<model>/` from
the working tree (replaced with `solutions/.gitkeep`) and has
`solutions/*/` gitignored.  Older commits on `main` still contain
those files \[LongDash] step 4 below scrubs them out of history.

## 0. Pre-flight verification

Confirm canonical solutions never touched git:

```
git log --all --oneline -- 'private/**'
# Expect: empty.  If anything prints, those commits leaked the
# canonical file and need to be scrubbed too.
```

Confirm what files we're about to scrub from history:

```
git log --all --oneline -- 'solutions/claude-opus-4.6/' \
    'solutions/google_gemini-2.5-flash/' \
    'solutions/minimax_minimax-m2.7/' | wc -l
# Expect: dozens of commits.  These are the ones filter-repo will
# rewrite.
```

## 1. Backup, twice

History rewrite is irreversible.  Make a tag + a tarball.

```
# Tag the current state of main (purely a pointer; doesn't move)
git tag backup/before-public-cleanup main

# Tarball the entire repo (includes .git)
cd ..
tar czf wclb-backup-$(date +%F).tar.gz WolframChallengesBenchmark/
ls -lh wclb-backup-*.tar.gz
cd WolframChallengesBenchmark
```

If anything goes wrong, you can `tar xzf` the backup over the
working dir and continue.

## 2. Push the strip-from-HEAD commit

Sync the remote with the cleanup commit (which removes 686 tracked
solution files from the current tree, leaves them on disk locally):

```
git status        # should show solutions/.gitkeep, .gitignore changes,
                  # plus 686 cached-deletes ready to commit
git commit -m "Strip model-generated solutions from tree (history rewrite next)"
git push origin main
```

## 3. Install git-filter-repo

```
brew install git-filter-repo
git filter-repo --version
```

If you don't have homebrew, follow
https://github.com/newren/git-filter-repo/blob/main/INSTALL.md.

## 4. Rewrite history to remove every solutions/ file

```
git filter-repo --invert-paths \
  --path solutions/claude-opus-4.6 \
  --path solutions/google_gemini-2.5-flash \
  --path solutions/minimax_minimax-m2.7 \
  --path solutions/minimax-m2-7-pilot \
  --path solutions/_dryrun-validate \
  --path solutions/claude-opus-4.6.remap.json
```

This walks every commit on every branch and writes new commits with
the same content MINUS the listed paths.  All commit SHAs change.

**filter-repo strips the remote by default.**  Re-add origin:

```
git remote add origin https://github.com/Jofreep/WolframChallengesBenchmark.git
git remote -v   # confirm
```

Verify the scrub worked:

```
# Should print nothing.  If any solutions/<model>/<...>.wl files print,
# the rewrite missed them \[LongDash] re-run filter-repo with broader paths.
git log --all --oneline -- 'solutions/*/*.wl' | head
```

Verify the rest of the repo is intact:

```
git log --oneline | head -20
# Expect: ~25 commits with familiar messages, in roughly the same
# order, but with NEW SHAs.
ls challenges.jsonl docs/ scripts/ WolframChallengesLLMBenchmarkPaclet/
# Expect: all present.
```

Run the test suite one more time as a sanity check:

```
wolframscript -file WolframChallengesLLMBenchmarkPaclet/Tests/RunTests.wls 2>&1 | tail -n 8
# Expect: 99/99 passed.
```

## 5. Force-push the rewritten history

```
git push --force-with-lease origin main
```

`--force-with-lease` is safer than `--force`: refuses to push if the
remote moved unexpectedly between your fetch and your push.

After this, anyone with old clones (just you on this private repo,
unless you've added collaborators) needs to `rm -rf` and re-clone.

## 6. Flip the repo to public

```
gh repo edit Jofreep/WolframChallengesBenchmark \
  --visibility public \
  --accept-visibility-change-consequences
```

Or via the web UI: Settings \[Rule] General \[Rule] Danger Zone \[Rule]
Change repository visibility \[Rule] Make public.

Confirm the public URL works in a private window:
https://github.com/Jofreep/WolframChallengesBenchmark

## 7. Enable GitHub Pages

Settings \[Rule] Pages \[Rule] Source: **GitHub Actions**.

Then trigger the first deploy manually:

```
gh workflow run publish-dashboards.yml
gh run watch
```

Once it succeeds, the live URL is:
https://Jofreep.github.io/WolframChallengesBenchmark/

The four dashboards (Leaderboard, ModelStrengths, Trend, BankQuality)
are now public.  Re-deploys happen automatically on every push to
`main`.

## 8. Optional cleanup

Drop the now-stale collaborator-permission setting (private repos
have implicit collaborator semantics that don't apply once public):

Settings \[Rule] Manage access.  Public repos default to "everyone
can read"; collaborators only matter for write access.

## Rollback

If you change your mind in the first hour:

```
gh repo edit Jofreep/WolframChallengesBenchmark --visibility private \
  --accept-visibility-change-consequences
```

If filter-repo damaged the repo:

```
cd ..
mv WolframChallengesBenchmark WolframChallengesBenchmark.broken
tar xzf wclb-backup-<DATE>.tar.gz
cd WolframChallengesBenchmark
# Try a different approach (option 3 from the AskUserQuestion: fresh
# public mirror repo, leaving this private repo intact)
```
