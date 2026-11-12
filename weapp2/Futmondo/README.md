# Futmondo

An R Shiny companion for inspecting Futmondo players, squads, transfers, standings, league finances, official notifications, and historical results. Transactions are explicit manual actions.

## Run locally

Open this application directory and run:

```r
shiny::runApp(".")
```

Startup uses `global.R`, `ui.R`, and `server.R`. Install the packages recorded in `manifest.json`. Enter Futmondo credentials in the login screen; select a league after login. Credentials and HAR captures must not enter application bundles or logs.

The [data redesign plan](docs/data_architecture_redesign_plan.md) defines the current work. [Executed results](docs/data_review_results.md) distinguish implementation, isolated verification, and deployment.

## Product scope

The app retains observed data, historical accounting, descriptive statistics, official notifications, and manual roster/market actions. Predictions, Smart Bid, forecast-driven planners/optimizers, generated trading recommendations and unattended trading remain excluded. Explicit exceptions cover login-time coach hiring and, from 2026-11-11, per-account opt-in background acquisition of factual data through the Python worker described below.

The user-triggered **Lineup** page is an explicit exception: it compares observed recent-round averages to select a valid XI and outfield-covering bench. It makes no predictions or unattended changes; **Save to Futmondo** is a separate manual action. Existing databases need `scripts/migrations/20261107191054_lineup_snapshots.sql` for durable lineup snapshots and the saved navigation page. See [lineup behavior and verification](docs/lineup_module.md).

Production runs on a Contabo VM using Docker/Dokploy. The Docker image starts Shiny and a supervised Python acquisition service. Accounts remain in interactive mode until they explicitly enable background sync. Opted-in browsing reads durable database snapshots, while the worker acquires source data and prepares finances independently of browser sessions. Pending work is distinct from acknowledged persistence. See [Python worker setup and contracts](docs/python_worker.md); production migration, deployment and authenticated acceptance are separate rollout steps.

## Database configuration

Configure these values on the R server, through the hosting secret store or a local ignored `.Renviron`:

```text
supabase_project_url=https://YOUR_PROJECT.supabase.co
supabase_secret_key=YOUR_SERVER_SERVICE_ROLE_KEY
admin=YOUR_ADMIN_LOGIN_EMAIL
```

The optional `admin` value identifies the account authorized for administrative actions. Database credentials remain server-side. Missing database configuration leaves live factual inspection available but prevents historical persistence.

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

The isolated PostgreSQL checks use `@electric-sql/pglite`; browser checks use Playwright. These are development tools, not deployed R dependencies. Set `FUTMONDO_TEST_NODE_MODULES` to their installation directory or use the documented default `/tmp/futmondo-review-tools/node_modules`.

```sh
node test/test_factual_schema.cjs
node test/test_observation_migration.cjs
```

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

Active refreshes reuse verified history and query a 24-hour overlap behind the latest recorded source timestamp. Player score queries use observation time so corrections to older rounds are retained. Initial loads and periodic completeness audits still retrieve full histories. No new package or environment variable is required.

For transfer reuse across app restarts, apply `scripts/migrations/20261105201845_complete_history_baselines.sql` before deploying the updated app. New databases include it in `scripts/schema.sql`. The new server-only cache contains normalized factual records and completion metadata, never login tokens or passwords. Missing schema or an invalid snapshot safely falls back to source acquisition. The configured hosted database received this migration on 2026-11-05 (recorded version `20261105220658`). Release verification and performance limits are recorded in `docs/data_review_results.md`.

The last application page is saved per Futmondo account and restored on normal or remembered login, across devices and app restarts. Existing databases need `scripts/migrations/20261107050308_user_page_preferences.sql` (already applied to the configured hosted database on 2026-11-06); new databases include it in `scripts/schema.sql`. No new environment variables or dependencies are needed. See [saved page behavior and verification](docs/page_preferences.md).


## Docker / Dokploy hosting

For deployment on your own VPS, use [the Dokploy guide](docs/docker_deployment.md).
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

`FUTMONDO_WORKER_ENABLED` defaults to `true`; missing key/configuration leaves the service idle and accounts in interactive mode. Optional capacity settings are `FUTMONDO_WORKER_LANES=2`, `FUTMONDO_WORKER_CONNECTIONS=2` and `FUTMONDO_WORKER_ACCOUNT_RPS=2`. Start with one lane if VM memory is constrained, then measure before increasing concurrency. Python and its pinned dependencies are installed from `worker/requirements.txt`; the image includes Python/venv and timezone data. No new R dependency is introduced.

After login, use Welcome → Background sync to explicitly opt in, select included leagues and edit quiet hours/timezone. The default quiet window is midnight–07:00 in the detected user timezone, with hourly unattended checks; user activity can request immediate priority work. Tokens are encrypted separately from browser remembered login. Background access continues after logout until disabled or rejected by Futmondo; no password is saved and no automatic trade is performed.

Existing interactive behavior remains for non-opted-in accounts. Opted-in accounts display stored data and acquisition progress rather than silently falling back to direct source reads. Manual-action preflights and readback remain live. See [worker behavior, failure recovery, limits and rollout](docs/python_worker.md). Container startup does not apply migrations or enroll users automatically. Active finance context/progress improvements require `scripts/migrations/20261112035000_resolved_finance_jobs.sql` followed by `scripts/migrations/20261112035500_finance_progress_publication.sql` before the corresponding app image; source history is preserved. Dokploy auto-deploys pushes to the application branch, per the owner. See [finance worker progress](docs/finance_worker_progress.md).

Focused offline checks: `Rscript test/test_worker_contract.R`, `Rscript test/test_worker_provider.R`, `node test/test_worker_schema.cjs`, then `python -m unittest discover -s test -p test_worker.py -v` from the application directory with the pinned Python requirements installed. The R contract check generates synthetic cross-language fixtures in `/tmp`; tests never use real accounts or transactions.
