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

The app retains observed data, historical accounting, descriptive statistics, official notifications, and manual roster/market actions. Predictions, Smart Bid, forecast-driven planners/optimizers, generated trading recommendations, automatic coaches, scheduled workers, and unattended trading are excluded from current and future development.

The user-triggered **Lineup** page is an explicit exception: it compares observed recent-round averages to select a valid XI and outfield-covering bench. It makes no predictions or unattended changes; **Save to Futmondo** is a separate manual action. Existing databases need `scripts/migrations/20261107191054_lineup_snapshots.sql` for durable lineup snapshots and the saved navigation page. See [lineup behavior and verification](docs/lineup_module.md).

The app runs on Posit Connect Cloud's free plan. It does not require a persistent worker, scheduler, child process, or server configuration. Session termination can interrupt pending history saves. The UI must distinguish pending saves from acknowledged persistence.

## Database configuration

Configure these values on the R server, through the hosting secret store or a local ignored `.Renviron`:

```text
supabase_project_url=https://YOUR_PROJECT.supabase.co
supabase_secret_key=YOUR_SERVER_SERVICE_ROLE_KEY
admin=YOUR_ADMIN_LOGIN_EMAIL
```

The optional `admin` value identifies the account authorized for administrative actions. Database credentials remain server-side. Missing database configuration leaves live factual inspection available but prevents historical persistence.

When the API cannot establish a season, an independently verified per-league mapping may be supplied:

```text
FUTMONDO_SEASON_CONTEXTS='{"league-id":"2026-27"}'
```

Do not infer a season from the calendar or borrow another league's scoring configuration.

For new databases, use `scripts/schema.sql`, which creates factual tables and explicit server-role privileges. Existing databases use the forward migrations described in [database migration and rollback](docs/database_migration.md). Do not replay old feature migrations after installing the factual schema.

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


## Incremental transaction and player history

Active refreshes reuse verified history and query a 24-hour overlap behind the latest recorded source timestamp. Player score queries use observation time so corrections to older rounds are retained. Initial loads and periodic completeness audits still retrieve full histories. No new package or environment variable is required.

For transfer reuse across app restarts, apply `scripts/migrations/20261105201845_complete_history_baselines.sql` before deploying the updated app. New databases include it in `scripts/schema.sql`. The new server-only cache contains normalized factual records and completion metadata, never login tokens or passwords. Missing schema or an invalid snapshot safely falls back to source acquisition. The configured hosted database received this migration on 2026-11-05 (recorded version `20261105220658`). Release verification and performance limits are recorded in `docs/data_review_results.md`.

The last application page is saved per Futmondo account and restored on normal or remembered login, across devices and app restarts. Existing databases need `scripts/migrations/20261107050308_user_page_preferences.sql` (already applied to the configured hosted database on 2026-11-06); new databases include it in `scripts/schema.sql`. No new environment variables or dependencies are needed. See [saved page behavior and verification](docs/page_preferences.md).


## Docker / Dokploy hosting

For deployment on your own VPS, use [the Dokploy guide](docs/docker_deployment.md).
The production Dockerfile is `weapp2/Futmondo/Dockerfile`, with build context
`weapp2/Futmondo`. It uses R 4.6.1 and serves the app directly at `/` on container
port 3838. Enter the main domain in Dokploy's Domains settings and configure
HTTPS there; DNS contains only the server IP. Set existing server-only credentials
in Dokploy's runtime Environment tab, keeping the database as the durable store.
The development settings under `weapp2/docker` are separate from this image.
