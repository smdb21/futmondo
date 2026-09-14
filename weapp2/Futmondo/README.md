# Futmondo Insights

An R Shiny companion for Futmondo: inspect rosters and transfers, compare players, forecast rival bids, plan trading scenarios and choose a legal XI. Predictions are advisory; observed data, model estimates and missing evidence are shown separately.

## Run locally

Open this directory as the project and run:

```r
shiny::runApp(".")
```

The application uses `global.R`, `ui.R` and `server.R`; there is no separate `app.R`. Install the dependencies recorded in `manifest.json` for your R environment. Core packages include Shiny, shinydashboardPlus, dplyr, data.table, httr, jsonlite, reactable, plotly, lpSolve, openssl, later and **callr**. `callr` is newly required for persistence in a separate R process. The regenerated manifest records the full dependency set.

Login fields start empty. Users enter their own Futmondo credentials, select a league, and can switch leagues during the session. Logout clears the session and its API cache. Login passwords and tokens are not printed or deployed.
The desktop navigation sidebar is 180px wide so full menu labels remain readable; the existing responsive mobile navigation is unchanged.

## Multiple accounts and leagues

A Futmondo **championship ID identifies a separate game**, with its own managers, roster ownership, cash, rules, scoring system and bonus configuration. API caches include account and league/team context. Switching leagues reloads these observations and resets action selections. Forecasts, saved scenarios, alerts and automation jobs retain their league and account identifiers; a worker also verifies current membership before acting.

Global `players` and `real_clubs` contain shared identities. League-specific positions, ratings, ownership, prices and points live in championship-scoped observations. Team histories carry an explicit championship ID. Rule snapshots preserve configured media weights and bonuses; weights are not normalized to 100%.

The API does not always expose a reliable season identifier. Unknown-season match records are retained for audit but excluded from historical point training. If the season is independently verified, configure an explicit **per-league** mapping, for example:

```text
FUTMONDO_SEASON_CONTEXTS='{"league-id-a":"2026-27","league-id-b":"2026"}'
```

Never use a global inferred season or another league's rules to fill missing data. See [data contracts](docs/data_contracts.md) and [multi-league storage](docs/multi_league.md).

## Database configuration and migrations

Set server-side environment variables through your hosting secret store, or a local ignored `.Renviron`:

```text
supabase_project_url=https://YOUR_PROJECT.supabase.co
supabase_secret_key=YOUR_SERVER_SERVICE_ROLE_KEY
admin=YOUR_ADMIN_LOGIN_EMAIL
```

`admin` is optional; every administrative handler checks the authenticated account. The service-role key stays on the server. Missing database configuration leaves live Futmondo inspection usable but persistence and historical analysis unavailable.

For a fresh database, apply `scripts/schema.sql`. For existing and fresh databases, review and apply these migrations in order:

1. `scripts/migrations/20260905_reliable_insights.sql`
2. `scripts/migrations/20260907_private_history.sql`
3. `scripts/migrations/20260907_observation_lifecycle.sql`

They create observation, forecast, scenario and worker tables; archive exact legacy transaction duplicates; add explicit league history context; and restrict private tables to the server role. Ambiguous legacy records are preserved. Back up the database and inspect the archive/reconciliation behavior before production application. **These migrations have not been applied to the connected production database.** PostgreSQL integration checks require a staging database; the offline tests do not substitute for them.

Collection runs in a separate process after login/refresh. Failed saves distinguish missing configuration, denied access, missing schema, and connection problems. Use Refresh to retry; a successful refresh clears the warning for the same operation and league/team. Failures for other operations or leagues remain visible. Server diagnostics include only the affected table and HTTP/database error codes, never response payloads or credentials. [Background persistence](docs/background_worker.md) describes lifecycle and failure handling.

## Predictions and management

A team may temporarily borrow down to half its current value. Bids subtract held funds and existing commitments and still respect the API ceiling. Cash must be strictly positive when a round begins to score points, so a persistent top bar shows the next-round countdown and a live solvency warning.
The All Players release-clause affordability filter uses this same verified spending capacity; when it cannot be verified, it does not infer that a clause is affordable.

- FIS is a descriptive snapshot rating, not a success probability. Missing appearances remain distinct from real zero points.
- Rival bids use shrunk manager participation rates and pooled log bid/value distributions. Participation and win curves remain unavailable when auction visibility cannot support non-bid labels. Conditional amounts can still be shown where historical reference prices exist.
- Resale forecasts compare no-change and regularized trends over one, three and seven days. Executable proceeds use an explicit offer assumption and fees; displayed value is not guaranteed sale proceeds.
- The profit planner allows no bid and considers commitments, cash, roster capacity and the remaining XI. It imposes no minimum points preservation requirement.
- Your Team includes a checked-by-default automatic coach preference for each league. Hiring is pending a verified endpoint; the current preference lasts only for the app session and sends no requests. See [coach integration status](docs/coach_hiring.md).
- Point forecasts use recency and positional shrinkage. The optimizer assigns eleven distinct players to eligible positions and displays captain/bench guidance and transfer scenarios. Optional features require verified league settings.
- Notifications provide a bell, unread/source/league/type filters and player links. Read state changes only after an explicit user interaction and successful acknowledgement.

[Prediction interfaces and evaluation](docs/prediction_engine.md), [dashboard](docs/intelligence_module.md), [UI corrections](docs/ui_reliability.md), [notifications](docs/notifications_module.md).

## Optional unattended worker

In Automation, connect an encrypted session and enable observations separately for each league. Observation collection does not require a trading policy and runs every 15 minutes while enabled, rotating through 30 catalog players per pass. Deploy a single observation scheduler; distributed observation leases still require implementation. Pause observations independently of trading. Coach preferences persist per account/league/team after the third migration, but hiring remains unavailable pending a successful endpoint capture.

The app now uses a shared black/green monospace theme without external fonts. The stylesheet is embedded in the UI to avoid stale browser CSS; restart the running Shiny app after stylesheet changes, then reload the page. See [theme](docs/terminal_theme.md), [observation/evaluation contracts](docs/insights_runtime.md) and [joint profit scenarios](docs/portfolio_planner.md). No new R package dependency was introduced by this follow-up. Source timestamps and revisions are retained; legacy unknown timestamps are excluded from predictive training rather than treated as fresh.

Automation is off by default. Deploy the worker separately from Shiny on a host that supports a persistent process. It continues when the browser closes. Generate a random 32-byte base64 `AUTOMATION_SESSION_KEY` in your secret manager and give **the same key** to Shiny and the worker. It encrypts stored API sessions; losing/changing it requires reconnecting accounts. Do not commit or print it.

From this project directory:

```sh
Rscript scripts/automation_worker.R --once
Rscript scripts/automation_worker.R
```

Run the second command under a service manager with automatic restart. Configure the same Supabase secrets and season mapping in both processes. Users connect a session and create explicit per-league policies with allowed players, action types, expiry, acquisition ceilings and sale floors. Pause and reconciliation controls are in the app.

Live execution is gated on fourteen verified observation days, current financial/eligibility checks and model validation when applicable. **Live rollout is not complete:** deadline/solvency and all formation/club restrictions still require contract verification. Lineup submission remains unavailable. Uncertain mutations hold the account lock until positively reconciled. See [automation contracts and recovery](docs/automation.md).

## Deployment and verification

`.rscignore` excludes `.Renviron`, HAR captures, tests and internal documentation from application bundles. Keep captures local: they may contain private sessions. `.gitignore` also excludes local credentials and captures; it does not erase previously tracked history.

After a dependency change, regenerate and inspect:

```r
rsconnect::writeManifest()
```

This prepares dependencies; it does not deploy the app. Do not use old manifests that bundled local credentials.

### Optional isolated browser and SQL checks

These are development-only Node dependencies, not Shiny deployment packages:

```bash
npm install --prefix /tmp/futmondo-review-tools --no-audit --no-fund playwright @electric-sql/pglite
/tmp/futmondo-review-tools/node_modules/.bin/playwright install chromium
node test/test_observation_migration.cjs
# Start the synthetic offline server in another terminal:
Rscript test/test_browser_app.R
node test/test_browser_theme.cjs
```

`FUTMONDO_TEST_NODE_MODULES` optionally selects another Node module directory. Browser fixtures listen only on `127.0.0.1:38765` and block external browser requests. Screenshots are written under `/tmp`. The SQL check executes migrations in isolated PostgreSQL (PGlite), covering repeat ingestion, historical replay, reapplication and access restrictions. It does not connect to Supabase or establish multi-process production concurrency. Production migrations and deployment have not been performed.

Run focused tests for the affected behavior. For application or shared infrastructure changes, also run:

```sh
Rscript test/test_shiny_simulation.R
```

This is now an offline application lifecycle harness, with HTTP blocked, including login, league switching, module rendering, refresh/logout and authorization checks. Focused scripts and actual results are listed in [delivery status](docs/implementation_status.md). HAR replay tests require the local captures and never use their credentials or send captured requests.

The [roadmap](docs/v3_roadmap.md) distinguishes implemented code from production rollout and validation still pending.
