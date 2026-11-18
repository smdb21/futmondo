# LigaRadar

An R Shiny companion for inspecting players, squads, transfers, standings, league finances, official notifications, and historical results. Transactions are explicit manual actions.

## Run locally

Open this application directory and run:

```r
shiny::runApp(".")
```

Each user chooses their own display timezone under Welcome → Display timezone. Without an explicit personal choice, the browser timezone is detected automatically; Spain (`Europe/Madrid`, including daylight saving) is the fallback if detection is unavailable. Personal choices are stored in the database and reused across sessions; LigaRadar administrators cannot change another user's timezone. Apply `scripts/migrations/20261115061026_user_timezones.sql` for existing databases before releasing this app version. No new packages or environment variables are needed. See [timezone preferences](docs/timezone_preferences.md).

Startup uses `global.R`, `ui.R`, and `server.R`. Install the packages recorded in `manifest.json`. Enter Futmondo credentials in the login screen; select a league after login. Credentials and HAR captures must not enter application bundles or logs.

The [data redesign plan](docs/data_architecture_redesign_plan.md) defines the current work. [Executed results](docs/data_review_results.md) distinguish implementation, isolated verification, and deployment.

## Product scope

The app retains observed data, historical accounting, descriptive statistics, official notifications, and manual roster/market actions. Predictions, Smart Bid, forecast-driven planners/optimizers, generated trading recommendations and unattended trading remain excluded. Explicit exceptions cover login-time coach hiring and factual acquisition through the Python worker. From 2026-11-16, signing in automatically connects factual sync; ordinary browsing reads the database.

The user-triggered **Lineup** page is an explicit exception: it compares observed recent-round averages to select a valid XI and outfield-covering bench. It makes no predictions or unattended changes; **Save to LigaRadar** is a separate manual action. Existing databases need `scripts/migrations/20261107191054_lineup_snapshots.sql` for durable lineup snapshots and the saved navigation page. See [lineup behavior and verification](docs/lineup_module.md).

Production runs on a Contabo VM using Docker/Dokploy. The Docker image starts Shiny and a supervised Python acquisition service. Accounts remain in interactive mode until they explicitly enable background sync. Opted-in browsing reads durable database snapshots, while the worker acquires source data and prepares finances independently of browser sessions. Pending work is distinct from acknowledged persistence. See [Python worker setup and contracts](docs/python_worker.md); production migration, deployment and authenticated acceptance are separate rollout steps.

## Database configuration

Configure these values on the R server, through the hosting secret store or a local ignored `.Renviron`:

```text
supabase_project_url=https://YOUR_PROJECT.supabase.co
supabase_secret_key=YOUR_SERVER_SERVICE_ROLE_KEY
admin=YOUR_ADMIN_LOGIN_EMAIL
```

The optional `admin` value identifies the account authorized for administrative actions. Database credentials remain server-side. Every new browser session checks database connectivity before enabling login, including administrator and remembered login. Missing database configuration or a failed check shows Database unavailable and keeps login disabled, with a Retry connection action. The read-only check uses the existing application_access main row; ensure the app-access migration is installed and the server key can read it. Its overall deadline is ten seconds, including transport queue time. Browsing uses stored database observations; there is no live-source fallback. No new environment variable, dependency or migration is required.

Use the project base URL for `supabase_project_url`, without `/rest/v1`. The shared URL getter also accepts a copied REST base URL and removes its trailing `/rest/v1` and slashes before callers add the endpoint. A duplicated `/rest/v1/rest/v1/` returns HTTP 404 and prevents both reading and saving history. After correcting a hosted environment value, restart or redeploy the app so R receives it.

When the API cannot establish a season, an independently verified per-league mapping may be supplied:

```text
FUTMONDO_SEASON_CONTEXTS='{"league-id":"2026-27"}'
```

Do not infer a season from the calendar or borrow another league's scoring configuration.

For new databases, use `scripts/schema.sql`, which creates factual tables and explicit server-role privileges. Existing databases use the forward migrations described in [database migration and rollback](docs/database_migration.md). Football-only league blocking and account-wide last-league restoration require `scripts/migrations/20261112024500_football_leagues_and_selection.sql`; it installs durable blocking without deleting existing records. The separately approved `scripts/purge_basketball_leagues.sql` removes source-identified basketball league data. See [league selection and sport policy](docs/league_preferences.md). Do not replay old feature migrations after installing the factual schema.

## Data behavior

Resource reads share cached observations. Historical points are separate from cumulative totals and price snapshots. Unknown or partial history is shown explicitly; the app must not manufacture values to fill charts. League/account context remains attached to data and pending UI actions.

[Database schema](docs/database_schema.md), [player card](docs/selected_player_module.md), and [multi-league state](docs/multileague_state.md) document the retained contracts.

## Development and deployment

Run focused tests for the changed behavior. Shared infrastructure or application startup changes also require:

```sh
Rscript test/test_shiny_simulation.R
```

This harness blocks HTTP and uses offline account/league fixtures. Never run real trades as verification.

The isolated PostgreSQL checks use `@electric-sql/pglite`; browser checks use Playwright. These are development tools, not deployed R dependencies. Set `FUTMONDO_TEST_NODE_MODULES` to their installation directory or use the documented default `/tmp/futmondo-review-tools/node_modules`. Browser fixtures that support `FUTMONDO_TEST_CHROMIUM` can use an existing Chromium executable at that path; otherwise Playwright uses its installed default browser.

```sh
node test/test_factual_schema.cjs
node test/test_observation_migration.cjs
Rscript test/test_page_information.R
Rscript test/test_player_availability_reuse.R
Rscript test/test_player_availability_store.R
node test/test_page_information.cjs
```

The page-information browser fixture starts its own synthetic server and checks three viewport widths in both themes. For full-app integration, start `Rscript test/test_browser_app.R` in a separate terminal, then run `node test/test_page_information_app.cjs`. Both use offline fixtures; the latter blocks external browser requests. See [information panels](docs/ui_information_disclosures.md) for the source/binding contract.

After a dependency change, regenerate and inspect the deployment manifest:

```r
rsconnect::writeManifest()
```

Exclude `.Renviron`, HAR files, `*.orig`, tests, internal docs, and development scripts. The installed bundler reads literal names per directory: keep both `.rscignore` and `Modules/.rscignore` updated, and run `Rscript test/test_deployment_contract.R` after regeneration. Manifest generation does not deploy the app. Production migration/deployment status is recorded separately in the results document.


Login-time coach hiring is an explicit user-requested exception to the removed automation scope. Today's market offers **Automatically hire coach**, enabled for a new browser preference and scoped to account/league. Choices are remembered in that browser only; no new environment variables, package dependencies, worker or schedule is required. Admin can explicitly retrieve/save missing full-XI and MVP selections. See [coach and round-award behavior](docs/coach_and_round_awards.md) for request limits, finality and persistence semantics.

## Optional seven-day remembered login

Remembered login is an explicit user-requested interactive-session feature. It never saves passwords or runs unattended account work. Install the pinned `sodium` dependency from `manifest.json` (`libsodium-dev` is needed when building that R package from source). Apply `scripts/migrations/20261101154802_remembered_logins.sql` for existing databases; new installations include the table in `scripts/schema.sql`.

Configure the following **server-only hosting secrets**, in addition to the existing Supabase URL and server-role key:

```text
FUTMONDO_REMEMBER_ENABLED=false
FUTMONDO_REMEMBER_KEY=YOUR_RANDOM_64_HEX_CHARACTER_KEY
```

Generate the key once using a cryptographically secure generator, for example `sodium::bin2hex(sodium::random(32))` in a private R console. Retain the same key across restarts and deploys. Never commit it, put it in a URL/client script, or use the database credential as the encryption key. Key rotation invalidates old remembered sessions.

HTTPS and same-origin cookie requests are required. Posit Connect Cloud may omit the original WebSocket `Origin`; in that case the cookie endpoint verifies the HTTP `Origin` against `Host`, or requires browser-controlled `Sec-Fetch-Site: same-origin` when the proxy rewrites Host. No paid hosting feature, background service or additional environment variable is needed. Proxies must preserve the HTTPS Origin and CSRF headers, plus either the public Host or browser Fetch Metadata. Keep `FUTMONDO_REMEMBER_ENABLED=false` for production until the hosted cookie flow has been checked in a private test deployment, then explicitly set it to `true`. Missing configuration disables the opt-in checkbox while ordinary login remains available. The local HTTPS browser check is `node test/test_remember_login_browser.cjs`; hosted smoke testing must confirm opt-in login, browser closure/reopen, app restart, logout and Android persistence. See [login behavior](docs/login_module.md) for expiry, retry, revocation, data retention and request costs.


Locker clause payments are read from Futmondo's authenticated `/2/locker/news` endpoint during active-session transfer loading. The `xml2` package parses the observed HTML payment template with network access disabled. Its dependency is included in the deployment manifest. No additional environment variables, saved authentication, scheduled worker, or database migration is needed.

My Team's incoming and leaving clause panels reuse safe, scoped player-clause observations from the existing private database evidence store within their original 60-second source deadline. `clause_transfer_store.R` adds no environment variables, packages or migration; existing server-side database configuration is required for durable reuse. Missing database access falls back to active-session source acquisition, while unsuccessful persistence remains retryable. No authentication secrets are included in these observations.


## Incremental transaction and player history

When background collection is enabled, the round-history display reads the saved league dataset in one shared batch, checking for updates at most once a minute while incomplete and every five minutes once loaded. Successful history survives freshness expiry; unchanged observations update check times without rebuilding the market cards. No new configuration or dependency is required. See [worker display and history recovery](docs/worker_display_history_recovery.md).

Active refreshes reuse verified history and query a 24-hour overlap behind the latest recorded source timestamp. Player score queries use observation time so corrections to older rounds are retained. Initial loads and periodic completeness audits still retrieve full histories. No new package or environment variable is required.

For transfer reuse across app restarts, apply `scripts/migrations/20261105201845_complete_history_baselines.sql` before deploying the updated app. New databases include it in `scripts/schema.sql`. The new server-only cache contains normalized factual records and completion metadata, never login tokens or passwords. Missing schema or an invalid snapshot safely falls back to source acquisition. The configured hosted database received this migration on 2026-11-05 (recorded version `20261105220658`). Release verification and performance limits are recorded in `docs/data_review_results.md`.

The last application page is saved per Futmondo account and restored on normal or remembered login, across devices and app restarts. Existing databases need `scripts/migrations/20261107050308_user_page_preferences.sql` (already applied to the configured hosted database on 2026-11-06); new databases include it in `scripts/schema.sql`. No new environment variables or dependencies are needed. See [saved page behavior and verification](docs/page_preferences.md).


## Docker / Dokploy hosting

For deployment on your own VPS, use [the Dokploy guide](docs/docker_deployment.md).
The Docker build publishes code-only availability fingerprints at `/player-card-runtime.json`
so the deployed player-card implementation can be verified without account data.
The production Dockerfile is `weapp2/Futmondo/Dockerfile`. In Dokploy set Provider
Build Path to `/weapp2/Futmondo`, Dockerfile Path to `Dockerfile`, and Docker Context
Path to `.`; disable Create Environment File. It uses R 4.6.1 and serves the app directly at `/` on container
port 3838. Enter the main domain in Dokploy's Domains settings and configure
HTTPS there; DNS contains only the server IP. Set existing server-only credentials
in Dokploy's runtime Environment tab, keeping the database as the durable store.
The development settings under `weapp2/docker` are separate from this image.

The Docker image retains an interrupted live Shiny session for up to **30 minutes**
after the server detects disconnection, with a matching browser reconnect window.
Redeploy in Dokploy to build/apply this customization; no new environment variable
is required. The same retained tab can resume its existing session, but page
discard/reload, container restart or expiry still ends that session. Retained
sessions use memory and can continue their existing timers; see the deployment
guide for build validation and resource limits.


## Multiple competitions and player transfers

Football competitions use their source competition and season identities. Existing databases must apply `scripts/migrations/20261110225848_general_competition_context.sql` before deploying this version; fresh databases use the updated schema. No new environment variables or R packages are required. Database migration and application deployment remain separate rollout steps, and the admission setting is unchanged.

Player IDs remain opaque source identities. An unchanged ID can accumulate club observations across competitions; a changed ID stays separate without inferred person merges. Existing unknown historical contexts remain intact and are not assigned to a season by date. See [competition context and transfer behavior](docs/general_competition_context.md) for durable metadata, history repair, exact scope, migration and source-evidence limitations.


## Python background acquisition

Before enabling the worker, apply `scripts/migrations/20261111214239_background_acquisition.sql` to an existing database; new installations include it in `scripts/schema.sql`. Configure a separate server-only `FUTMONDO_WORKER_KEY` (64 random hex characters) in Dokploy, retain existing database variables, and deploy the Docker image. Do not place this key in Git, Docker build arguments or browser settings, and do not reuse `FUTMONDO_REMEMBER_KEY`. Generate it privately with `openssl rand -hex 32`.

`FUTMONDO_WORKER_ENABLED` defaults to `true`; missing key/configuration or `false` leaves acquisition idle. Browsing still reads saved database observations and reports unavailable sync; it never falls back to collection inside Shiny. Optional capacity settings are `FUTMONDO_WORKER_LANES=2`, `FUTMONDO_WORKER_CONNECTIONS=2` and `FUTMONDO_WORKER_ACCOUNT_RPS=2`. Start with one lane if VM memory is constrained, then measure before increasing concurrency. Python and its pinned dependencies are installed from `worker/requirements.txt`; the image includes Python/venv and timezone data. No new R dependency is introduced.

Signing in automatically connects background sync, as disclosed on the login screen. Collection continues at its normal cadence around the clock, including after logout. Welcome → Background sync selects included leagues; **Disconnect account and stop sync** revokes factual access and signs out. Tokens are encrypted separately from browser remembered login; no password is saved. Page activity and Refresh read saved data without changing acquisition schedules or submitting transactions. Individually authorized scheduled clause purchases retain their separate consent and revocation controls.

All accounts display stored observations immediately when available; connection acknowledgement runs independently. Explicit manual-action preflights and readback remain live, including the separately enabled login coach check. See [worker behavior, failure recovery, limits and rollout](docs/python_worker.md). Container startup does not apply migrations or enroll users; successful interactive login connects the account. Active finance context/progress improvements require `scripts/migrations/20261112035000_resolved_finance_jobs.sql` followed by `scripts/migrations/20261112035500_finance_progress_publication.sql` before the corresponding app image; source history is preserved. Dokploy auto-deploys pushes to the application branch, per the owner. See [finance worker progress](docs/finance_worker_progress.md).

Database-first login requires `scripts/migrations/20261116183421_database_first_sync.sql` before deploying the matching app and worker. Production database version: `20261116213815_database_first_sync`. It adds server-only bootstrap/connection RPCs, preserves job checkpoints during credential rotation, removes quiet-hour throttling for enabled accounts, and consolidates guarded worker reads. Fresh databases include it in `scripts/schema.sql`. No new environment variables, packages, worker processes or concurrency limits are introduced. See [the data route, lifecycle and verification limits](docs/database_first_sync.md).

Focused offline checks: `Rscript test/test_worker_contract.R`, `Rscript test/test_worker_provider.R`, `node test/test_worker_schema.cjs`, then `python -m unittest discover -s test -p test_worker.py -v` from the application directory with the pinned Python requirements installed. The R contract check generates synthetic cross-language fixtures in `/tmp`; tests never use real accounts or transactions.

Worker discovery queue repair: apply
`scripts/migrations/20261113043655_worker_discovery_batches.sql` before deploying
the corresponding worker image. The server-only batch RPC replaces thousands of
sequential discovery queue calls, preserves existing queue context/consent guards,
and reuses unchanged durable jobs without rewriting them. It was applied to the
configured production database as `20261113043948_worker_discovery_batches`.
No new package or environment variable is required; see
[worker discovery behavior](docs/python_worker.md#bounded-durable-discovery-writes-2026-11-13).

For the worker freshness/history-write repair, apply
`scripts/migrations/20261113053241_worker_display_priority.sql` followed by
`scripts/migrations/20261113054538_worker_points_cadence_guard.sql` before the
matching image. Both are applied to the configured production database. No new
package or environment variable is required. See
[worker freshness and durable acknowledgement reuse](docs/worker_freshness_recovery.md)
for foreground lane behavior, five-minute recent-point cadence, mixed-version
rollout safety, partial finance results and validation limits.

The follow-up `scripts/migrations/20261113055439_worker_core_read_lane.sql`
keeps due short core reads eligible for the reserved lane without a live browser,
while long finance/bulk work remains excluded. Applied in production as
`20261113055513_worker_core_read_lane`; no new collection scope or runtime setting.

Also apply `scripts/migrations/20261113060230_worker_core_prefetch.sql` before the
source-timestamp/prefetch image. It schedules short core reads before their true
expiry while preserving quiet hours, manual refresh and source deadlines; no
new package or environment setting is required.

Apply `scripts/migrations/20261113063652_worker_clause_scope_guards.sql` before
redeploying the clause-freshness worker image. It enforces the existing one-minute
ownership deadline and avoids duplicate unresolved roster/history acquisitions.
Production migration: `20261113064338_worker_clause_scope_guards`. Redeploy both
R app and Python worker to activate the clause cache age guard. No dependency or
environment-variable changes. See `docs/worker_freshness_recovery.md` for isolation,
source-time and performance details.

Current clause policy supersedes the preceding one-minute rule: apply
`scripts/migrations/20261113065753_worker_clause_cadence.sql`, then redeploy the R
app and Python worker. Background clause acquisition uses five minutes regardless
of GUI navigation; source times remain original and transaction preflights keep
their existing validity. Production migration is `20261113070822_worker_clause_cadence`.
The app also contains a safe missing-column diagnostic for the still-unresolved
history completion-marker REST failure. No new dependencies or settings.

For history-marker and unresolved-worker recovery, apply
`scripts/migrations/20261113115929_worker_resolved_handoff.sql`, then deploy the
new app/worker container together. Production SQL version:
`20261113121612_worker_resolved_handoff`. The migration adds bounded canonical
resource redirects and preserves independent five-minute clause scheduling;
the container fixes implicit R `_row` fields in database writes and retries
failed history saves without reacquiring their retained source payloads.
No new settings or dependencies. See `docs/worker_resolved_recovery.md` for the
contracts, measured checks and remaining production acceptance work.

Dokploy is configured to auto-deploy after a push (owner confirmation2026-11-13).
The follow-up `20261113142221_worker_clause_fairness.sql` prevents ordinary core
reads starving overdue clause jobs; production version is20261113142423. Cold
transfer panels now distinguish last-observed stored schedules from current
roster verification, retaining stale warnings and rejecting capacity use of
historical observations. Details: `docs/worker_resolved_recovery.md`.

Current database-only browsing requires `20261114043816_worker_database_read_boundary.sql` followed by `20261114043932_worker_autonomous_cadence.sql` before the new image. Enrollment seeds discovery in the database; the GUI reads stored snapshots, including on Refresh. The worker discovers dependencies and materializes history/clause views independently. The existing two bounded slots share query families fairly; completion schedules the full interval rather than retrying to chase an expired timestamp. Source validity and manual-action checks stay strict. No environment variables or dependencies change. See [database-only browsing and autonomous collection](docs/worker_database_boundary.md).

Saved clause transfers are displayed with their original roster/transaction observation times; short freshness expiry alone is not an acquisition error. The balance box explains `(cached)` and shows balance/offer observation times. Existing worker databases require `scripts/migrations/20261114144534_worker_shorter_cadence.sql` so a worker-selected shorter player-summary interval updates its next due time; the configured production database received it as `20261114144934_worker_shorter_cadence`. No new secret, environment variable or package is required.


## Scheduled clause purchases

Apply `scripts/migrations/20261115075411_scheduled_clause_purchases.sql` after the existing worker migrations, then deploy the updated app/worker image. Fresh databases include it in `scripts/schema.sql`. No migration runs on application startup.

Set `FUTMONDO_CLAUSE_SCHEDULING_ENABLED=true` in Dokploy to expose scheduling and start the elected purchase scheduler; default is `false`. It requires the existing server-only database settings, `FUTMONDO_WORKER_KEY`, `FUTMONDO_WORKER_ENABLED=true`, and the Docker-supervised Python service. Individual users authorize only their selected purchases; factual Background sync enrollment is not required. Normal logout does not cancel a schedule. The balance disclosure supports cancelling pending schedules and revoking purchase access.

For the temporary diagnostic only, set `FUTMONDO_CLAUSE_DIAGNOSTIC_ACCOUNT` to the intended Futmondo account ID (default unset). That account sees **Test locked clause (one real request)** on eligible Championship player cards. Its confirmation sends a real request and could purchase the player. It bypasses only the future-date check, preserves other preflights, and sends no automatic retry. Capture the displayed native response code, unset the variable, and remove the diagnostic bypass once the code is established. Never submit a password, token or raw response as diagnostic evidence.

The native closed-clause rejection code has not been captured. The worker's closed-code allowlist is deliberately empty; an unknown rejection is terminal and an ambiguous response is reconciled. Populate the allowlist only from verified source evidence and its fixture. The UI states this current retry limitation.

The scheduler uses existing Python/R dependencies. It performs at most one global idle claim per second per elected worker (about 86,400 lightweight RPCs/day), without polling distant players. It starts at most one additional headless R helper lazily near due work and closes it after 30 idle seconds. Existing source concurrency and account pacing remain shared with factual acquisition. Verify Contabo memory headroom, network latency, source-rate limits and opening-time lag before enabling production. Missed execution windows expire; timing and purchase success are not guaranteed.

See [scheduling, security and state contracts](docs/scheduled_clauses.md). Synthetic checks: `Rscript test/test_scheduled_clauses.R`, `node test/test_scheduled_clause_schema.cjs`, and `PYTHONPATH=. <worker-venv>/bin/python test/test_scheduled_clauses.py`. No new dependency or manifest regeneration is required.

Player-card availability shares current team checks across cards and restores successful observations from the existing private evidence store. No new environment variable, dependency or migration is required; the existing server database access must include `rival_finance_evidence` and its latest-observation view. See [the availability contract](docs/player_availability.md) for expiry, persistence and verification. For the isolated card browser check, start `Rscript test/test_worker_card_availability_browser_app.R`, then run `node test/test_worker_card_availability_browser.cjs`.

Worker collection cadence: `FUTMONDO_WORKER_BASE_INTERVAL_MINUTES` defaults to `30`
when unset or empty. Set a positive whole number of minutes (maximum 35791394,
the integer-seconds storage limit); invalid values stop worker startup. Set this
on the Python worker container and restart it. The elected worker reconciles the
durable schedule; R does not need the variable. Routine intervals use the greater
of the resource cadence and this base. Login refreshes current resources, coalesced
per account for five minutes. Historical/profile schedules remain slower. This
setting does not change purchase timing or live transaction preflights.

Apply `scripts/migrations/20261118150000_worker_bounded_cleanup.sql` for bounded
worker storage cleanup. The elected worker runs a separate cleanup task every
minute: at most 500 obsolete chunks and 500 obsolete checkpoints per call.
Unreferenced chunks/uploads have a one-hour grace period; current snapshots and
current-run checkpoints are protected. `worker_cleanup_status` records durable
success timestamps and deletion counts. Stale cleanup progress and
`worker_cleanup code=cleanup_failed` require investigation. Cleanup removes rows;
physical disk reclamation may require separately scheduled database maintenance.
