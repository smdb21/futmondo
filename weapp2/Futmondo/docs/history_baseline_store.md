# Complete transaction history baselines

`history_baseline_store.R` restores verified complete transfer history after the process cache has expired or the app has restarted. It contains no authentication/session state and performs no work until an active view requests or saves history.

## Runtime interface

- `read_persisted_history_baseline(resource, context)` makes one scoped database read and returns a normalized data frame with its source metadata, or `NULL` when the row/schema is absent, unavailable, incompatible or invalid. `supabase_get()` remains inside the cooperative asynchronous read adapter. A missing snapshot is a cache miss; the normal source acquisition still runs.
- `persist_completed_history_baseline(resource, data, context)` accepts only `fetch_status = "ok"` with valid `full_history_at`, `observed_at` and, for nonempty data, `source_latest_at`. It queues one atomic upsert using the existing bounded persistence queue. The snapshot itself includes both the complete data and the completion metadata, so it cannot acknowledge coverage ahead of its rows.

The resource must be exactly `pressroom_<championship_id>` or `pressroom_locker_payments_<championship_id>`. Identity consists of account, league, season, scoring version and resource. `transaction_history_context()` resolves a configured known season when the caller uses `unknown`; explicit season identities remain separate. Team/session identifiers and credentials are excluded. Full-feed reconstruction must finish before either complete baseline can be replaced.

`record_acquired_resource()` invokes saving after a cooperative read completes. Saving inside the replay adapter would be ineffective because replay deliberately suppresses persistence. The raw locker resource is saved before its normal persistence early return. Locker baselines can contain zero rows while retaining a watermark from non-clause news.

## Representation and limits

The server-only `complete_history_baselines` table contains the identity fields, `encoding`, `payload`, `payload_sha256`, `json_bytes`, `snapshot_hash`, `observed_at` and server `recorded_at`. The encoding is `json-gzip-base64-v1`.

The JSON envelope stores the scope; a frame described by column names, primitive types, `AsIs` flags, row count and column values; the last full-history audit time; the newest raw source timestamp; the source observation time; and a semantic fingerprint. Explicit type descriptors retain empty data frames, missing scalar values and nested bid lists. Source timestamp normalization rounds epoch microseconds before formatting, so fractional timestamps survive repeated restoration without fingerprint changes.

Only known normalized transfer columns are accepted. Nested bid data is checked for unsupported objects, excessive depth and credential-like fields. Arbitrary data-frame attributes are removed. Unknown counterparties remain missing/empty, preserving the Futmondo System/Market convention.

Accepted baselines are limited to 20,000 rows, 16 MiB of JSON, 32 MiB of normalized in-memory data and 900 KiB of encoded payload. The entire queue argument must also remain below the queue's 1 MiB per-job limit. Standard gzip is produced in a temporary mode-0600 file that is immediately removed. Restoration uses a gzip connection capped at the declared JSON size plus one byte; corrupted or oversized payloads cannot cause unrestricted decompression. Unsupported or oversized snapshots fall back to full source acquisition, without truncating history or declaring it complete.

The bounded acknowledgement map retains at most 256 scalar fingerprints/timestamps. Queued completion callbacks capture only key/fingerprint scalars, not full frames or temporary serialization buffers. Unchanged content and completion metadata avoid another write after successful acknowledgement. New observations with the same content do not themselves produce writes; changed facts, raw source watermark or full-audit timestamp do. Failed writes remain retryable. A database trigger prevents an older concurrent source observation from replacing a newer complete snapshot.

## Database delivery and verification

The local migration is `scripts/migrations/20261105201845_complete_history_baselines.sql`, generated with the Supabase CLI. Its definition is also included in `scripts/schema.sql`. The table enables RLS, revokes browser/public access and grants access only to `service_role`; the ordering trigger uses invoker security and restricted execution grants. No hosted migration or deployment is performed by the local implementation.

`Rscript test/test_history_baseline_store.R` verifies normalized restoration, scope isolation, subsecond timestamps, semantic write suppression, failed-write retry, empty locker metadata, invalid/corrupt input, bounded decompression, minimal callback retention, and a delayed cooperative read with a heartbeat. It also reports synthetic encoding cost. A 2,500-row fixture produced 332,381 JSON bytes and 11,512 encoded bytes in approximately 0.4 seconds on the development host. This is local serialization work, not a production latency measurement; very large accepted histories can incur greater CPU cost.

`node test/test_history_baseline_schema.cjs` uses isolated PGlite (set `FUTMONDO_TEST_NODE_MODULES` to its parent `node_modules` directory). It checks repeatable migration application, atomic replacement, rejection of older updates, storage bounds and restricted grants, and fresh-schema integration. The broader existing fresh schema contains unrelated non-repeatable statements, so this check applies the fresh schema once and then repeats only the new migration. Production database latency and deployment remain unverified.


## Incremental refresh policy — 2026-11-10

Verified durable baselines no longer expire into automatic seven-day full source audits. Normal calls fetch only newest pages through the existing one-day revision overlap and reuse stored historical rows, including after restart. Explicit `full_audit=TRUE` on the pressroom/locker read functions bypasses the display cache and retrieves all source pages for older revisions/deletions; absent/incompatible baselines still require initial full acquisition. Exhausted/reset source feeds remain complete replacements. `transaction_history_window(previous, full_audit=FALSE)` controls this choice. No change to the baseline schema or acknowledgement contract is required.

`history_baseline_compress(bytes, max_bytes=.history_baseline_limits$payload_bytes)` now accepts an explicit output bound for the separate full cash-ledger archive; its transfer-baseline default and all transfer storage limits remain unchanged. See `docs/rival_cash_accounting.md` for archive/manifest linkage and limits. Saving finances restores this verified transfer baseline before scanning legacy transaction rows.
