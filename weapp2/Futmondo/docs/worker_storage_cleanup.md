# Worker storage cleanup — 2026-11-18

Production inspection measured database size 2,585,087,123 bytes, including
1,356,554,240 bytes of worker_chunks. Of 43,659 chunks, 4,642 were referenced
by current snapshots. The previous hourly, unbounded cleanup left 34,140
unreferenced chunks older than its one-day retention. The deployed function had
DELETE permission. No captured error establishes the exact historical cause of
that backlog; unbounded transactions and cleanup sharing the heartbeat loop were
confirmed implementation weaknesses.

Bounded maintenance removed 34,219 eligible rows as more aged into retention.
Current referenced chunks remained 4,642. With explicit owner authorization and
production stopped, VACUUM FULL ANALYZE reclaimed physical space: chunks fell to
170,172,416 bytes; database to 1,398,697,107 bytes. History and finance facts were
not deleted. Remaining database size exceeds the free allowance; this change
does not resolve historical/index storage growth or the Round awards defect.

## Runtime contract

`worker_cleanup_batch(p_limit=500)` accepts 1..1000 and returns chunk/checkpoint
deletion counts. Each table's deletion is bounded; locked rows are skipped.
Only chunks unreferenced by current snapshot manifests and older than one hour
qualify. The grace window protects unpublished uploads and in-flight readers.
Current-run checkpoints survive regardless of age; superseded-run checkpoints
have the same one-hour grace. Existing worker_cleanup() remains compatible and
performs one 500-row batch. No historical evidence table is touched.

The Python leader runs cleanup separately once per minute, with a 20-second RPC
wait cap. Failed or unacknowledged calls retry next minute; no memory-only success
flag suppresses recovery. Failure does not clear healthy leadership or delay
heartbeat renewal. Cancellation is propagated on shutdown. Durable server-only
worker_cleanup_status records successful batches, cumulative counts and last
batch counts. A success older than five minutes while the leader runs, repeated
full batches, or cleanup_failed logs warrants investigation; no autonomous alert
service is introduced. An app that remains stopped cannot perform cleanup.

## Performance and verification

At most 1,440 batch calls/day while elected, and up to 720,000 deletions/day per
table; no source API traffic is added. Batches avoid an unbounded delete and keep
current manifests indexed by account/resource. The one-hour grace bounds retained
obsolete versions by recent publication volume, rather than keeping a full day.
Current snapshots are durable and still reused by readers. This is not an absolute
byte quota: published payload size, source rate and persistent DB failures still
require monitoring. Automatic VACUUM FULL is deliberately excluded because it
locks/rebuilds tables and needs spare disk.

Synthetic SQL covers batch bounds, draining/retry, protected current snapshots,
recent uploads, old active checkpoints, durable counters and denied anon access.
Python checks cover failure isolation/retry, standby behavior and yielding during
a delayed cleanup. Existing worker and offline Shiny lifecycle checks are also
required before delivery.

Production migrations worker_bounded_cleanup and worker_configurable_cadence were
acknowledged on 2026-11-18. A production batch removed exactly 500 obsolete chunks
and 500 obsolete checkpoints, with durable success status acknowledged. All 40
worker tests, three cleanup Python checks, synthetic cleanup SQL, cadence SQL,
and offline Shiny lifecycle (13 checks and eight tabs) passed. Worker restart
and scheduled cleanup after deployment still require verification.
