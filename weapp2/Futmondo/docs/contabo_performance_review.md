# Contabo performance and background-processing review

Date: 2026-11-11. Status: review complete; recommendations are not implemented or deployed.

## Recommendation

Keep the Shiny application and its existing asynchronous HTTP transport. First measure the hosted slow paths, move expensive factual computations into a bounded R worker, reduce retained-session timer/rendering work, and make accepted observation saves durable. Add application replicas only when concurrent-user measurements justify them. Python is available on a VPS, but changing language alone does not remove blocking work or reduce source/database latency.

The user reports that production now runs on a Contabo VM. The repository already includes a Rocker/Shiny Server Docker image and Dokploy deployment guide. The old Posit Connect Cloud restrictions on child processes, services and schedules no longer describe the target host. Database persistence, context isolation, manual-action preflights and truthful completeness remain requirements: containers and processes can still restart.

This review authorizes no production changes and introduces no worker, dependency, migration or environment setting. Background processing of acquired facts is distinct from unattended authenticated acquisition. This request explores background capabilities; it does not select a schedule, authorize stored background authentication, or restore trading/prediction automation.

## What the code already provides

| Evidence | Current behavior | Consequence |
| --- | --- | --- |
| `Dockerfile`, `shiny-server.conf` | Rocker Shiny Server, one application location, no explicit multi-worker arrangement | Extra vCPUs do not automatically parallelize application R execution. The OSS simple scheduler normally assigns one R worker to an application. |
| `async_transport.R`: `fm_transport_pump()` | Four HTTP connections, three normal slots with capacity reserved for interactive work, 32 admissions, cooperative curl pump | Network waits are already asynchronous. Raising all limits or moving every HTTP request to a child process is not the first optimization. |
| `async_transport.R`: `async_adapter_fetch()` | Composite expressions replay after responses; completed responses are memoized; adapter-local function environments are prepared for each fetch | Network calls are reused, but parsing and transformations encountered on replay can repeat on the main R thread. Profile CPU per replay before rewriting selected adapters as explicit stages. |
| `rival_finance_runtime.R`: `publish()` / `build_rival_cash_dataset()` | Reuses calculation inputs, defers reconstruction during acquisition batches, publishes saved previews | Preserve these savings; isolate remaining measured expensive reconstruction rather than recalculating every time evidence arrives. |
| `rival_calculation_store.R`, `history_baseline_store.R` | Scoped database snapshots and compressed ledger/history archives | Warm reads already avoid source work, but JSON encoding/decoding, hashing and compression still consume synchronous R time. |
| `background_runtime.R`: `defer_persistence()` | Bounded, deduplicated, asynchronous in-memory save queue; one active write; session-owned cancellation | Good responsiveness, but unacknowledged work is lost on process exit or cancellation. A persistent service needs a durable acceptance boundary. |
| `server.R`: `round_countdown` | `renderUI()` every second on relevant tabs | Rebuilds/sends server UI for a clock. The 30-minute reconnect patch can retain sessions and buffered outputs. |
| `Modules/Notifications_Module.R`, `Modules/Login_Module.R` | Unread-count refresh every minute; non-admin admission check every two minutes | Retained sessions may continue eligible work. Admission verification has security meaning and must remain current. |
| `data_service.R`: `resource_metrics()` | Cache hits/misses/coalescing, requests, bytes and retained cache size | Useful baseline instrumentation exists, but process-local counters alone do not establish hosted page timings. |

Shiny Server scheduling and idle-process behavior are documented in the [Posit administration guide](https://docs.posit.co/shiny-server/). A warm R process and a retained disconnected session solve different problems.

## Prioritized changes

| Order | Change | Expected benefit | Cost and acceptance condition |
| --- | --- | --- | --- |
| 1 | Hosted timing and resource baseline | Finds whether delay is source I/O, DB round trips, CPU, initialization or browser rendering | Low implementation cost. Record cold/warm and multi-session p50/p95 before claiming improvements. |
| 2 | Bounded R compute worker | Heavy ledger/history work no longer stalls navigation or other users | Medium cost. One worker initially, subject to measured RAM/CPU headroom; heartbeat stays responsive during held/large computations. |
| 3 | Browser clock and suspended optional work | Less R work, WebSocket output and retained-session memory, particularly on phones | Low/medium cost. Clock remains accurate after resume; optional reads pause; reconnection/admission tests pass. |
| 4 | Durable observation-save jobs | Accepted saves survive app logout/restart and support durable warm reuse | Medium/high cost, primarily reliability with indirect speed benefit. Crash/restart, duplicate delivery and stale-write tests must pass. |
| 5 | Fewer DB round trips and derived-data reuse | Faster warm history/finance loads where DB/network time dominates | Medium cost. Measure plans, transferred bytes and request counts; preserve exact contextual results and corrections. |
| 6 | More app processes when needed | Concurrent users are less affected by each other's CPU work | High operational cost. Requires routing, cross-process invalidation and memory/concurrency acceptance tests. |
| 7 | Python service for a demonstrated workload | Independent worker deployment and Python-library access | Optional. Adopt only after measuring an advantage or selecting Python-specific work; require R/Python known-answer parity. |

### 1. Measure the VM before sizing workers

Collect actual vCPU/RAM, container limits, VM region, database region, peak active/retained sessions, and the screens that feel slow. These were not supplied at review time; do not use this development executor's hardware as Contabo evidence.

Measure login-to-primary-view, first/repeat player-card open, first/repeat rival balances, refresh and league switching. Separate source wait, transport queue wait, database wait, parsing/computation, serialization and browser render time. Run a synthetic second session navigating while the first restores a large ledger. Capture CPU, RSS, swap, event-loop delay, requests and response bytes; use sanitized resource names instead of private payloads or tokens.

Use existing `resource_metrics()` plus targeted `Rprof()`/timing spans first. Keep collection bounded and aggregate samples; a monitoring stack is optional. Proposed initial responsiveness gate: p95 navigation/heartbeat below 500 ms during background work. This is a validation target, not a promise that external data arrives within 500 ms. Preserve first-use compilation measurements separately.

Container resource limits need deliberate sizing. Docker does not impose CPU/memory limits by default; see [Docker resource constraints](https://docs.docker.com/engine/containers/resource_constraints/). Reserve capacity for the OS, Dokploy, proxy and worker serialization copies. Worker count must use available container capacity and measured job RSS, not raw host CPU count.

### 2. Offload CPU while keeping existing R contracts

Pilot archive encoding/restoration, then measured `build_rival_cash_dataset()` work. Use pure functions with explicit immutable data/context arguments in a small reusable process pool. Shiny's [ExtendedTask guidance](https://shiny.posit.co/r/articles/improve/nonblocking/) describes returning a promise backed by separate-process execution; an `ExtendedTask` wrapper or timer alone is insufficient. The documented R approach uses `future_promise()` with a configured process backend. New packages would require Docker/manifest updates and verification of the installed Shiny version.

Do not export `.GlobalEnv`, a Shiny session, reactive objects, curl handles, login tokens or DB credentials into compute jobs. Extract pure archive transforms from any DB reads and pass already-fetched, scoped payloads. Every result carries account/league/team/player/competition/season/scoring context as applicable, input fingerprint, algorithm version and request generation. Publish only if that context/generation still matches; cap/coalesce queued work, discard obsolete display results, and provide structured failure states.

A worker makes the UI more responsive even if total computation time stays similar. Serialization and extra process memory can make tiny tasks slower; keep small work inline unless measurements support offloading. Persist reusable successful derived snapshots with their source timestamps, input/version identity and completeness, and reuse valid DB snapshots before submitting another job. Never renew source freshness merely because a calculation finished later.

Separately profile composite-adapter replay and staged parsing: HTTP memoization prevents duplicate requests but does not automatically memoize CPU work. Convert only expensive adapters to explicit stages, preserving priority, partial-error behavior, cancellation and request budgets.

### 3. Reduce timer and browser costs

Move the countdown text to JavaScript using a server-supplied deadline/time offset. Refresh factual schedule data through the existing resource lifecycle, not once per second. Recalculate the display from time after browser resume rather than accumulating timer ticks.

Introduce explicit visibility/disconnection handling for optional unread refreshes and display work, with revalidation on return. Preserve server-side admission/expiry checks before restored access or manual actions. Measure the existing 30-minute retained-session window under multiple phones: it retains R state and can buffer outputs, so longer retention has a real cost.

Keep table pagination, but measure serialized widget size as well as visible rows: pagination alone does not establish that only one page is sent to the browser. For large histories, project only required columns and the demanded time window; consider server-side queries only if payload measurements warrant them. Check 360/390 px rendering, scroll position, reconnect and no new source calls on local filter/clock updates.

### 4. Separate accepted saves from the UI lifetime

Use a dedicated service/container for prepared factual writes and computations that should survive the browser. Start with an R consumer and a database job/outbox table; Redis is not required for the initial design. Before reporting a job as durable, acknowledge its insertion in the database. A process-memory queue is insufficient even on a VPS.

Proposed job metadata: unique ID/deduplication key, job type/schema version, complete access/data context, source timestamps and completeness, input hash/version, state, attempt count, not-before time, lease owner/expiry, and sanitized error code. Job payloads contain bounded normalized observations or references to acknowledged stored inputs, never authentication secrets. A worker leases jobs atomically, retries eligible idempotent failures with bounds/backoff, and records completion only after result writes succeed. Leases permit recovery after a crash; deduplication does not imply exactly-once execution.

Preserve catalog-before-dependent-history and chunks-before-manifest ordering. Prevent old mutable metadata overwriting newer source state; existing retry restrictions must survive the new queue. Persist results and freshness metadata in the database, then notify/invalidate only affected resources. Logout may discard display work; already durably accepted factual saves can finish under an explicitly documented ownership policy. Restrict job access on the server and define retention/size limits so durable jobs do not become an unbounded private-data archive.

There remains an unavoidable interval before durable admission is acknowledged. Keep pending/failed states truthful and test failure during admission, lease expiry, restart, duplicate delivery, DB outage, old/new races and account switching. No manual trade, coach action or uncertain transaction response belongs in this retry queue.

### 5. Optimize the DB path before moving the database

The code uses Supabase REST with scoped baselines and incremental history. Moving the R host does not move that database or eliminate remote round trips. Measure from Contabo, batch compatible reads/writes, preserve existing correction overlap and inspect actual filtered queries with read-only `EXPLAIN (ANALYZE, BUFFERS)` on suitable synthetic/staging data. Index changes need measured plans and write-cost review; no missing-index claim was established here. See [PostgreSQL EXPLAIN](https://www.postgresql.org/docs/current/sql-explain.html).

Evaluate direct PostgreSQL access only if REST overhead is material. A synchronous DBI call on Shiny's event loop can regress responsiveness; run it in a worker or preserve asynchronous access. Account for connection budgets, TLS, role restrictions, deployment secrets and IPv4/IPv6 reachability; [Supabase connection guidance](https://supabase.com/docs/guides/database/connecting-to-postgres) distinguishes direct and pooled connections.

Keep Supabase as the durable authority initially. A local PostgreSQL migration or replica adds backup/restore, revision/lag and security work; select it only after measuring latency and establishing recovery requirements. Redis or a mounted file cache can accelerate reads later, but reusable observations/results and provenance still require DB persistence and context-safe invalidation.

### 6. Scale sessions deliberately

First add the compute worker and remeasure. If concurrent sessions still contend, compare multiple Shiny containers with session affinity against a per-user/container platform such as [ShinyProxy](https://www.shinyproxy.io/documentation/configuration/). Neither is a free drop-in change.

The initial page, WebSocket, same-origin cookie endpoint and reconnect must reach compatible session state. Container replicas do not migrate live R sessions. Preserve HTTPS/Origin/CSRF behavior and the remembered-login key. Current process-local caches/invalidation do not synchronize across replicas: establish shared version/invalidation signaling, retain DB snapshots, and require manual-action freshness/preflights independently. Budget each R process's session state, caches, retained connections and worker copies. Recheck aggregate source/API limits across all processes rather than multiplying today's four-connection limit blindly.

### 7. Where Python fits

A Python worker is reasonable for a selected Python-library workload, CPU-heavy transforms with demonstrated gains, or a larger independent job service. Celery is one option, but adds broker/worker operation and explicit delivery configuration; its [task documentation](https://docs.celeryq.dev/en/stable/userguide/tasks.html) explains idempotency, acknowledgement and worker-loss caveats. Installing Celery alone does not make delivery durable.

For the first optimization, R reuses tested finance/history semantics and avoids maintaining two parsers and accounting engines. If Python is later selected, version the JSON contract and independently verify NULL versus zero, negative points, timestamps, opaque IDs, Market counterparties, rewards and complete ledger totals against the same fixtures. Retain source context and DB acknowledgement semantics. Do not rewrite the UI or existing data acquisition simply to change language.

A scheduler could also handle bounded maintenance over already-stored facts, such as cleanup of expired jobs or recomputation after a versioned calculation change. Authenticated collection while nobody is using the app is a separate scope decision requiring explicit sources, cadence, access lifecycle and request budgets; the seven-day browser-login tokens remain for interactive access only.

## Verification and delivery limits

Executed against the current working tree with synthetic/local sources:

- `Rscript test/test_async_transport_priority.R`: 10 passed / 0 failed. Sequential capacity reads: normal 938 ms versus interactive 179 ms; foreground with three slow background sockets 42 ms; maximum four connections, unchanged request counts.
- `Rscript test/test_rival_ledger_archive.R`: passed fresh-process restoration of 5,001 entries, three immutable chunks/manifest ordering, corruption/context rejection, confirmed-write retry/deduplication and durable transfer-baseline reuse. Encoding 0.207 s, restoration 0.360 s, encoded archive 297.6 KiB.

These single-run development measurements identify opportunities; they are not Contabo load tests or claimed production speedups. The working tree includes unrelated user edits; these checks are review evidence for that tree, not certification of all changes. No live account, trade, production DB query/write or Contabo process inspection was performed. Actual VM resources, hosted latency, source-rate limits, database region, deployed package versions and concurrent-user behavior remain unverified.

Performance-impact assessment: this delivery changes documentation/hosting guidance only. It adds zero runtime API/DB requests, timers, observers, memory retention or rendering work. No runtime performance tests or lifecycle harness are required for the documentation change; the two existing offline checks above were run to ground the recommendations. Implementation phases require focused tests, delayed-work/cross-session checks, DB durability/restart checks, mobile verification, the offline Shiny lifecycle for shared/startup changes, and dependency/manifest validation where applicable.
