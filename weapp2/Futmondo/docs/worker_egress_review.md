# Database request and egress review — 2026-11-18 (Pacific)

## Measured traffic

Supabase edge-log aggregate for 2026-11-18 18:22:00–18:52:15 UTC (30 minutes,
15 seconds): 10,242 requests. This is project traffic during acquisition and
maintenance, not 10,242 Futmondo calls or a steady-state monthly forecast.

| Database endpoint | Calls | Calls/minute | Purpose / response |
|---|---:|---:|---|
| worker_guard | 2,711 | 89.6 | Account/lease safety and renewal; 4-byte body |
| clause_claim | 1,387 | 45.9 | Timely dispatch of explicitly scheduled purchases; 4-byte null bodies in this window |
| worker_chunks POST | 1,215 | 40.2 | Snapshot uploads; empty response body |
| worker_write_fact | 1,050 | 34.7 | Durable factual writes; mostly 4-byte acknowledgement |
| worker_prepare_source | 753 | 24.9 | Current credentials/context and restart checkpoint |
| worker_publish_child | 671 | 22.2 | Publish dependent observations; 4-byte acknowledgement |
| worker_checkpoint | 560 | 18.5 | Save source checkpoint; previously echoed the entire uploaded payload |
| worker_claim | 552 | 18.2 | Acquire pending factual jobs |
| worker_publish | 542 | 17.9 | Publish completed observations; 4-byte acknowledgement |
| clause_observe | 360 | 11.9 | Persist factual purchase evidence |
| worker_read | 269 | 8.9 | Stored snapshot reads; worker always requested an empty version |
| rival_finance_evidence_latest | 34 | 1.1 | Stored financial evidence |
| worker_maintenance | 31 | 1.0 | Leader heartbeat / scheduling policy |
| worker_enqueue_batch | 31 | 1.0 | Discover work in batches |
| worker_cleanup_batch / clause_cleanup | 29 each | 1.0 each | Bounded housekeeping |

Content-Length is absent for all worker_read, checkpoint, prepare_source and claim
responses in that window. Missing length is UNKNOWN, not zero egress. The numerous
4-byte responses cannot explain bandwidth without measuring the larger bodies;
headers/transport overhead also count beyond these body sizes. Historical edge logs
cannot establish a complete byte ranking. No private queries/payloads were logged.

A separate 23:50–00:00 UTC ten-minute window had 98 clause_read calls, 20
player_round_dataset reads and six worker_read calls, with the factual worker
blocked by its maintenance error. It is not a comparable post-fix traffic window.
These clause reads come from the shared five-second purchase-list UI poll, even
when the list is empty or all purchases are settled.

## Live startup defect fixed

Python received FUTMONDO_WORKER_BASE_INTERVAL_MINUTES=45, but worker_maintenance
failed with HTTP 400 / SQLSTATE 21000 while updating worker_jobs. The API's
`authenticator` role preloads `safeupdate`; the function's UPDATE lacked a WHERE
clause. The SQL editor and PGlite did not reproduce that API-session extension.
The fix scopes UPDATE to rows whose effective interval actually changes; it does
not disable safeupdate. Native hourly/daily jobs whose interval is unchanged also
avoid an unnecessary rewrite/reschedule.

Applied migration 20261119000714_worker_maintenance_scoped_interval_update.
Production subsequently published 2700 seconds, acquired one active leader, and
advanced automatic cleanup at 00:10:47 UTC. This is live verification of recovery.
Reference: https://github.com/eradman/pg-safeupdate

## Changes reducing bandwidth / reads

1. New server-only worker_checkpoint_ack wraps the existing guarded checkpoint
   writer and returns a boolean, rather than sending the uploaded payload back.
   Legacy read/write RPC remains compatible. Restart checkpoints remain durable;
   false still means the existing optional checkpoint storage budget declined
   storage, not a durable acknowledgement. The caller's factual persistence and
   final publication still require their existing independent acknowledgements.
2. New server-only worker_read_cached uses the existing versioned database reader
   and returns current manifest metadata for unchanged versions without chunks.
   Python's Store supplies previously seen versions and reuses only their verified
   immutable chunks. Every read still checks the DB; missing/deleted records and DB
   failures cannot silently fall back to memory. Freshness/status/context comes
   from the latest database manifest. Cache is account/resource scoped, capped at
   128 records and an approximately 8 MiB serialized-content budget. Runtime object
   overhead is additional but bounded. The durable database remains authoritative;
   a restart simply downloads needed versions again. Both factual and clause
   worker readers use this Store path; all purchase preflights/guards remain live.
3. Empty or fully settled purchase-list UI polling now reads at most once per
   minute instead of every five seconds (12 to 1 reads/minute/account). Unknown,
   failed-read and active/uncertain reservation states retain five-second polling.
   Explicit schedule/cancel/acknowledge/manual actions still force immediate reads.
   The local five-second timer remains for independent context/reconciliation work;
   it is cheap and no longer implies a database request. Other-session changes to
   an idle list may take up to a minute to display; live purchase safety does not
   rely on that poll. The execution service's deadline-sensitive claim loop is unchanged.
4. Bounded per-endpoint worker_traffic summaries every maintenance cycle report
   calls, decoded_bytes and wire_body_bytes from HTTPX. These measure real bodies,
   including responses without Content-Length, without an extra DB/source request.
   They exclude HTTP headers and R-process reads and are not billing totals. Endpoint
   names are allowlisted and unknown names group under other. Counters reset each
   cycle; no tokens, filters, row bodies or per-request success logs are added.

Applied server migration 20261119001151_worker_conditional_payload_reads before
shipping Python callers. New functions are SECURITY INVOKER, service_role-only;
anon/authenticated access checks were false. No history or timestamps were deleted.

## Measurements and verification

- Synthetic unchanged read: 2,804 to 286 response bytes (~90% reduction), with
  current partial/expired metadata retained. Real DB sample, server-rendered JSON:
  3,827 to 1,616 bytes (~58%); this is one sample, not a workload-wide saving.
- Existing checkpoints sampled during resumed acquisition: 27 rows, 167,748 bytes,
  largest 53,586 bytes. Previously each write echoed its payload; new writes return
  true/false (4/5 bytes). This sample is not the historical 560-write byte total.
- Six Python egress tests pass: versions, changes/deletion, account isolation,
  failure handling, corrupt payload rejection, bounded entries and safe telemetry.
- Isolated SQL egress test passes: unchanged metadata, no chunks, changed status,
  deletion, cross-account isolation, lease rejection, persisted checkpoint reuse,
  tiny acknowledgement and anonymous denial.
- Cadence regression (including 45 minutes), eight diagnostic tests, 40 existing
  factual-worker tests, 13 scheduled-clause Python checks, R idle-poll/action-bypass
  check, R scheduled-clause tests and offline Shiny lifecycle (13 checks/eight tabs)
  pass. No live accounts or purchases were used in tests.
- The older test_worker_schema.cjs suite fails its pre-existing one-job assertion
  because enrollment now seeds a discovery job. The same failure was reproduced against baseline HEAD; this obsolete test also reapplies older migrations over the
  final schema. Current-schema focused tests above cover the changed interfaces.

Performance: no new source/API/database requests or reactive redraws. Unchanged
snapshot reads avoid network payload download; newly received chunks undergo
integrity verification before caching. Reused chunks are not decompressed merely
for cache admission or re-encoded for size accounting. Consumers retain their
existing integrity/parser checks. Idle UI reads fall twelvefold; safety guards,
restart durability, source timestamps and acquisition cadence remain intact.
Health advisors are clear. Existing unrelated GraphQL/grant/index advisories are
unchanged; see worker_error_diagnostics.md for links and scope.

## Quota verification still required

The recovered worker must first drain pending work and deploy the new image. The
old high-traffic window and the blocked-worker window cannot establish the new
steady-state rate. At 5 GB per 30-day cycle, average total egress must be under
about 166.7 MB/day, with margin for bursts and other clients. Existing consumed
usage cannot be undone. Do not claim a guaranteed monthly ceiling or use missing
log lengths as proof. Compare fresh worker_traffic summaries and a bounded edge
window after deployment, then the Supabase egress usage delta across normal use.
