# Python background acquisition

Status: implemented for local verification; production migration, Docker build/deployment and authenticated source acceptance are separate rollout checks. This is the explicit 2026-11-11 scope exception for opt-in unattended factual acquisition. Background trading, lineup writes, notification acknowledgements and coach actions remain prohibited.

## Ownership and process lifecycle

The Docker image preserves Rocker's `/init` and existing Shiny service. `/etc/services.d/futmondo-worker/run` starts the Python virtual environment as `shiny`; s6 restarts hard failures after the service's five-second finish delay. No cron, Python web port or host service is required. Python supervises up to two private R helpers. Helpers inherit no source/database credentials and communicate using bounded JSON pipes, never a browser session or R object serialization. The existing Shiny session reconnection customization remains in place.

Python owns HTTP, scheduling, persisted response checkpoints, delivery acknowledgements and recovery. Headless R reuses the application's actual parsers, factual persistence operations, award finality and finance controller/accounting engine. It creates a private reactive domain for that controller; it does not share a live user's session. stdout carries only protocol messages; Python discards helper stderr because arbitrary parser exceptions may contain private data. Service logs use fixed failure codes, never bodies, cookies, passwords or tokens.

A database leader lease prevents overlapping deployments from doubling acquisition. Leaders renew every 15 seconds with a 60-second expiry; job guards and final publication enforce that leadership. Graceful shutdown cancels tasks/helpers and releases the lease. After an abrupt stop, a replacement can acquire leadership after expiry. Separate 60-second job leases recover interrupted work. Empty job lanes check every five seconds; foreground work has a reserved lane when two lanes are enabled. Existing data remains readable while new work waits.

## Required setup and rollout

1. Apply `scripts/migrations/20261111214239_background_acquisition.sql` to an existing database. Fresh databases use `scripts/schema.sql`. Rehearse with the isolated SQL fixture and verify server-only grants. No migration is executed by Docker startup.
2. Keep the existing server-only `supabase_project_url`, `supabase_secret_key` and optional `admin` runtime settings. Set `FUTMONDO_WORKER_KEY` to an independently generated 64-hex-character secret in Dokploy. Generate it privately, for example with `openssl rand -hex 32`; never copy it into Git, logs, Docker arguments or client code. It must differ from the browser remembered-login key.
3. Build/redeploy the existing Docker application. Python starts automatically. Missing configuration leaves it idle with a fixed `configuration_missing` diagnostic; schema/database failures retry with backoff. Shiny's ordinary interactive mode remains available when worker support is not configured.
4. Log in and explicitly enable **Background sync** under Welcome. The opt-in explains that collection continues after logout, uses an encrypted token and saves no password. Save is acknowledged by the database before the account becomes enrolled. League discovery is queued immediately; all source-verified active leagues are included unless excluded in settings.
5. Confirm source checks, bootstrap progress, balances/ledger reconciliation and restart recovery in a controlled hosted acceptance run before broader enrollment. Local synthetic tests do not certify Futmondo account access or VM capacity.

Runtime settings:

| Variable | Default | Meaning |
| --- | --- | --- |
| `FUTMONDO_WORKER_ENABLED` | `true` | Starts acquisition when configuration and opted-in accounts exist; `false` disables the provider and keeps interactive acquisition. Restart after changes. |
| `FUTMONDO_WORKER_KEY` | unset | Required 32-byte secretbox key, represented as 64 hex characters. R/Python interoperability is independently tested. |
| `FUTMONDO_WORKER_LANES` | `2` | One routine helper plus one foreground helper. Set `1` on a memory-constrained VM; accepted range is 1–2. |
| `FUTMONDO_WORKER_CONNECTIONS` | `2` | Source connection cap per elected worker, bounded to 1–8; lanes also limit actual concurrency. |
| `FUTMONDO_WORKER_ACCOUNT_RPS` | `2` | Starts per second per account, bounded to 0.1–10; only one source request per account is active at once. |

Python dependencies and transitive dependencies are pinned in `worker/requirements.txt`. Docker installs Python/venv/tzdata and uses existing R dependencies; no Python package is imported by Shiny. Helpers limit BLAS/OpenMP threads to one. Measure total RSS on the Contabo VM before increasing any limits. API limits have not been established by synthetic tests; the defaults are configurable safety budgets, not verified service entitlements.

Rollback: disable the worker feature and redeploy the previous compatible application if necessary. Additive tables and factual observations remain intact. Rotating the worker key requires users to reauthorize through login or Save; it does not affect browser remembered login. Disabling an account removes its ciphertext and invalidates queued/in-flight work, without deleting its historical observations.

## Authentication and membership

`worker_accounts` retains only authenticated-encrypted source tokens, consent/scheduling preferences and monotonically increasing authorization generations. There is no local time-based expiration: enrollment lasts until disabled or source rejection. A successful login of an enrolled account renews its stored access; ordinary logout does not revoke background sync. Browser remembered-login records are never read by the worker.

A confirmed HTTP 401 pauses acquisition for reauthorization. HTTP 403 and unknown API-body errors are not guessed to be token expiration; they retain separate safe failure states and backoff. Application admission checks fail closed, with the configured administrator exception. Every source dispatch verifies account access and a source-confirmed league membership; known superseded season/scoring/competition scopes stop rather than acquiring current data into an old context. Discovery retires jobs for removed or superseded leagues; stored history is preserved.

## Resource and scheduling contracts

`worker_contract.R` defines canonical descriptors and a versioned JSON codec. Descriptors contain the handler, non-secret normalized arguments and account/league/team/player/competition/season/scoring scope; token changes do not change resource identity. Typed frames retain empty columns, NULL/NA, zero/negative values, dates and source metadata. Unknown R classes, executable values and credential-bearing nodes are rejected. Resource payloads are limited to 16 MiB uncompressed and decoded to at most 50,000 rows per frame.

Default normal targets are 60 seconds for transactions, market, squads, offers, notifications and finances; five minutes for catalog/configuration, standings and hot summaries; one hour for remaining player summaries/history; six hours for profiles/match metadata; daily finalized ranking/correction audits. Capacity reads retain their tighter 15-second source validity. The pressroom receives an explicit daily full audit; ordinary refreshes retain existing incremental overlap behavior. Unchanged ledgers reuse the existing accounting engine's input/cache checks rather than introducing another accounting formula.

Per-account quiet hours default to 00:00–07:00 in the detected IANA timezone, editable in settings. Unattended tasks have at least an hourly interval during that window; slower tasks retain their slower cadence. Database timezone rules handle DST. Foreground demand and explicit Refresh take priority regardless of quiet hours. New enrollment performs a bounded initial bootstrap; the entire player universe is not promised to be immediately warm. Request limits and fair account scheduling can delay targets, so source ages and failures remain explicit.

Nested successful acquisitions also publish reusable child snapshots, avoiding another source retrieval when a second screen needs data already acquired by finance/history work. Child snapshot preparation is bounded to 8 MiB per job. Immutable source checkpoints retain credential-free factual response envelopes, compressed and hashed, under the job's run identity; retries resume the same observation instead of renewing its source time. Checkpoint storage is bounded to 16 MiB per account; beyond that bound acquisition can continue but additional responses are not checkpointed. New cycles receive new run identities. Old checkpoints/orphan chunks are cleaned hourly, while every chunk referenced by a current snapshot is retained.

## Database interfaces and correctness

All worker tables and RPCs are server-only, with RLS enabled and explicit grants only to `service_role`; this is Futmondo account isolation enforced by the R/Python servers, not Supabase Auth `auth.uid()` ownership.

- `worker_authorize`: acknowledges consent/settings changes, increments generation and revokes old work. `worker_renew` updates credentials without overwriting scheduling preferences.
- `worker_enqueue` / `worker_claim` / `worker_guard`: durable deduplication, priority, quiet-hours admission, fair leases and generation/leader checks. A refresh arriving during an existing run stays due after that run and prevents its output appearing fresh.
- `worker_checkpoint`: lease-guarded, bounded source-response reuse across helper/process restarts.
- `worker_write_fact`: validates factual tables/columns/context, preserves existing conflict identities and requires successful writes. Monotonic heads prevent old mutable catalog observations overwriting newer worker observations.
- `worker_publish` / `worker_publish_child`: publish immutable chunks before the complete current manifest, reject revoked/foreign generations and preserve original source/check times. Complete balances and transaction ledgers remain together in one financial resource.
- `worker_read`: returns account policy, leader health and only changed versions of demanded resources, with ordered chunks and safe job errors.
- `worker_elect` / `worker_release` / `worker_cleanup`: cross-container leadership and reference-safe retention.

Known scoring/season contexts remain separate from unknown archives. Historical revisions, source timestamps and empty Market counterparties retain their existing meanings. Successful DB reads do not prove complete source acquisition. Persistence failure prevents a job claiming success; retry-safe observations and checkpoints permit recovery. Manual mutations never enter this queue.

## Shiny behavior and limits

Configured accounts are checked before selecting interactive versus worker-owned acquisition. Existing factual adapter signatures remain compatible, with an internal `worker_bypass` flag reserved for explicit direct paths. Synchronous source getters use the same provider, and the rival page reads the prepared finance snapshot instead of running its acquisition/reconstruction controller in Shiny for opted-in accounts. Lineup display resources use the worker; save preflight and uncertain-response reconciliation explicitly request live reads.

Active, visible sessions poll a combined account/resource revision query every five seconds, coalesced across sessions in one R process; payloads are fetched only for changed versions. The session tracks at most 128 demanded resources/waiters, and the shared decoded snapshot cache is capped at 256 entries/32 MiB. Pending UI waits are bounded to 120 seconds and then report that background work remains pending. Account/league changes and logout cancel owned display waits; durable background collection continues. Ordinary status polls do not reconstruct widgets or reset unsaved account settings. Countdown ticks are browser-local.

Saved results display immediately; missing data queues priority work and shows progress. Expired data retains its real source time and stale state. Worker outages never silently restore duplicate Shiny acquisition for opted-in accounts. Accounts without opt-in retain existing active-session acquisition. Explicit manual actions still check current funds, ownership/capacity, context and source acknowledgement. Source/database outages and historical evidence that the source cannot supply remain actionable limitations, never zero-filled successful calculations.

Performance considerations: opting in increases unattended source/database work compared with purely on-demand use, even while Shiny becomes faster. Fresh persisted child resources are reused before acquisition, account-level HTTP 429 retry deadlines are honored, and source streams have a 16 MiB/15-second admission bound. Checkpoint byte accounting uses stored sizes rather than repeatedly decoding old response bodies. Each published version can update active views; hosted CPU, database volume, factual-history growth and multi-account freshness must be measured before raising limits. The 32 MiB decoded cache bound does not include temporary JSON/gzip buffers or total R-process memory.
