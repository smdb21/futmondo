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
