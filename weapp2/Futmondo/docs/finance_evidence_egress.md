# Financial evidence reuse — 2026-11-18 (Pacific)

## Cause and evidence

read_rival_finance_evidence in rival_finance_runtime.R paginates the full latest
account/championship/competition/season evidence, including JSON payloads. Its
five-minute R cache is process-local. Worker helper restarts and independent
consumers can repeat this download even when the normalized factual states are
unchanged. Other snapshot/ledger readers also request the full evidence-row
contract. The earlier Store snapshot cache covered worker_read RPC results, not
these direct evidence-table GETs.

In the supplied 00:28:50–00:32:53 UTC window, edge logs showed 18 financial-evidence
reads. Worker counters grouped these with purchase-queue claims under `other`;
that group transferred 1,158,744 wire-body bytes. Attribution of every byte was not
possible from those mixed counters. Named telemetry now separates the evidence
view, metadata view, state-payload reads, purchase claims and round datasets.

An aggregate-only live measurement of the largest non-rival-cache scope returned
581 latest rows: full row JSON totals 7,369,223 bytes; metadata totals 187,434 bytes.
That is approximately 97.5% less decoded JSON for a fully warm read, not a measured
wire/billing saving. Metadata EXPLAIN ANALYZE for the same sampled scope was
97.284 ms, 2,597 shared buffer hits, zero disk reads. This includes scope discovery;
it is a focused measurement, not a production latency percentile.

## Read contract

The service-only security-invoker rival_finance_evidence_versions view exposes the
same current identity/timestamp columns plus the normalized state_id, without
joining/returning payload JSON. It uses the existing latest-observation ordering.
Migration 20261119004554_finance_evidence_version_metadata was applied before the
new worker code. No existing tables, histories, timestamps, retention rules or
views were rewritten. anon/authenticated cannot select the new view.

Store.request optimizes GET rival_finance_evidence_latest only for the exact full
factual-row projection with an explicit account equality filter and supported
simple filters/order/limit/offset. Requests with alternative headers, partial/JSON
projections, payload filters/order or complex OR filters keep their previous path.
This preserves existing specialized reader contracts.

1. Read the current metadata page with the original scope, filters, ordering,
   limit and offset. Do not reuse a cached row list.
2. Identify unseen (account,state_id) pairs. Download only their immutable payloads
   from finance_history_states, at most 128 IDs per request. Validate the complete
   returned ID set and object payload shape; missing/malformed rows fail the read.
3. Reconstruct the original eight-column rows using fresh metadata and the exact
   stored payload. State IDs never escape into the existing R row interface.

Normalized finance writers insert a new state for a factual change; they never
update an existing state's payload in place. This is the version contract the
cache relies on. Later confirmations of identical facts change observed_at but
reuse state_id, so they refresh timestamps without downloading payloads. Corrections
at an existing timestamp use the corrected state, and A/B/A changes can reuse the
previous A payload. Metadata changes, new/deleted rows, filters and page order are
seen on every read. A DB error never falls back to an old page or fabricates zero.
A deletion between metadata and payload acquisition produces a retryable error.
Administrators must preserve immutable states when doing future direct repairs;
in-place payload mutation would violate this existing normalization contract.

The cache is an accelerator in the long-lived Python Store, surviving R helper
restarts but not container restarts. Database history remains the sole durable
source. Cached JSON is stored as immutable UTF-8 bytes, account/state scoped, capped
at 16 MiB and 4,096 payloads, with oldest-entry eviction. Container restart/eviction
simply redownloads current needed states. Returned objects are decoded copies, so
consumer mutation cannot corrupt future reads. Per-call result/payload references
and ordinary object overhead are additional transient memory, as with the former
full-page response. No credentials or private payloads are logged.

## Performance tradeoffs and verification

A warm page costs one metadata GET instead of one full-payload GET. A cold/changed
page adds payload requests (one per 128 missing states); this intentionally trades
some small request count for avoiding repeated large transfers. Existing guards,
source cadence, manual purchase checks and persistence acknowledgements remain.
No new source requests, timers, async lanes, DB writes or UI redraws are added.

Six synthetic tests cover full-row reconstruction, timestamp-only changes without
payload refetch, corrections/A-B-A, deletions, DB failures, incomplete payload
batches, account isolation, fallback projections/filters, page parameters, consumer
mutation and memory eviction. A 100,000-character fixture measured 100,215 bytes
of cold response JSON versus 170 bytes on an unchanged reread. These are decoded
JSON sizes; the fixture is compressible and not a billing forecast.

SQL fixture verifies normalized-view reconstruction exactly matches the existing
latest view, first/last observation semantics, A/B/A state reuse, historical row
preservation, account filtering and denied anonymous access. Existing egress,
diagnostic, worker, clause and lifecycle checks are recorded in the review tracker.
Live privilege verification: anon=false, authenticated=false, service_role=true.
Health advisors are clear; unrelated existing grant/GraphQL/index findings remain
as documented in worker_error_diagnostics.md.

Production warm-cache bytes still require the deployed reader to run through
normal work. Check worker_traffic endpoints rival_finance_evidence_versions and
finance_history_states separately: repeated unchanged scopes should keep metadata
traffic but cease payload downloads. Direct R application reads and unsupported
specialized projections are not changed by this optimization. Monthly 5 GB quota
compliance cannot be guaranteed from one short window; all clients and HTTP overhead
must fit the allowance. No historical data was removed to obtain these savings.
