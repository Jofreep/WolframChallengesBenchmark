# Publishing dashboards to GitHub Pages

The four dashboards (Leaderboard, ModelStrengths, Trend, BankQuality)
plus a styled landing page are published to GitHub Pages on every push
to `main`, by `.github/workflows/publish-dashboards.yml`.

Site URL once Pages is enabled:
**https://Jofreep.github.io/WolframChallengesBenchmark/**

## What gets published

```
_site/
  index.html                  landing page (cards link to each report)
  leaderboard/index.html      ranked podium of all models
  model-strengths/index.html  per-track pass-rate matrix
  trend/index.html            pass-rate over time per model
  bank-quality/index.html     per-challenge classification
  .nojekyll                   tells Pages to serve files starting with _
```

The dashboards are HTML+CSS+inline-SVG; no JS frameworks, no build
toolchain, fully static.

## Privacy / repo-visibility notes

GitHub Pages on **private repos** requires a paid plan
(GitHub Pro / Team / Enterprise).  Three options:

| Option | Cost | Effort | Notes |
|---|---|---|---|
| **Make repo public** | free | low | Test bank, prompts, scripts, model-generated solutions all visible. Canonical solutions stay gitignored under `private/`. |
| **GitHub Pro** | $4/mo | low | Repo stays private; Pages still works. Site visibility (public/private) is configurable. |
| **Public mirror repo** | free | medium | Push only `_site/` to a separate public repo (e.g. `WolframChallengesBenchmark-pages`); main repo stays private. |

If you go public, the only thing to double-check before flipping
visibility is that `private/canonical_solutions.jsonl` is not tracked
(`git ls-files private/` should be empty).  `.gitignore` already
excludes the whole `private/` tree.

## One-time Pages setup

1. Go to **Settings \[Rule] Pages** on the repo.
2. Under **Source**, pick **GitHub Actions**.
3. Save.  GitHub will detect the next workflow run that uploads a
   pages artifact and deploy it automatically.

You can also manually trigger the deploy:

```
gh workflow run publish-dashboards.yml
```

## Refreshing the site

Whenever a new model run lands in `runs/run-*/`, push to main (or
trigger the workflow manually) to rebuild and re-deploy.  Build is
~30 seconds + the time the dashboards themselves take.

Local preview (no deploy):

```
wolframscript -file scripts/BuildSite.wls
open _site/index.html
```

## What the workflow runs

- `actions/checkout@v4`
- `actions/configure-pages@v5`
- `wolframscript -file scripts/BuildSite.wls --skip-bank-quality`
- `actions/upload-pages-artifact@v3` with `path: _site`
- `actions/deploy-pages@v4`

Action versions are pinned to majors (see comment in
`.github/workflows/tests.yml` for the policy rationale).

## Why self-hosted runner

The dashboards read `runs/run-*/results.wxf` and
`runs/run-*/run.json`, which are gitignored and only exist on the
machine that ran the benchmark.  GitHub-hosted runners would have
neither the data nor Wolfram Language available.  The same M3
self-hosted runner that powers the test suite handles the publishing.
