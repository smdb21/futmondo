# Price, match and finance history periods — 2026-11-18

The original read interfaces (`player_history`, `player_match_observations`,
`rival_finance_evidence`) become security-invoker views over per-domain identity,
factual-state and timestamp tables. The owner approved retaining first/last confirmations per unchanged period and
discarding intermediate confirmations on 2026-11-18. Every factual change and
return to a prior state remains. Valuation endpoints retain their original bigint
row ID, observed_at, and recorded_at. Nullable identity/time rows and nonfinite
timestamps are conservatively retained. Match and finance endpoints retain their
real source timestamps and complete factual revisions. Source round IDs,
scoring versions, account/competition/season context, negative values, true zero,
unknowns, dates and JSON payload structure remain distinct.

## Durable writes and compatibility

`price_history_write`, `match_history_write`, and `finance_history_write` accept
`p_payload` (original row/array), `p_conflict` and `p_ignore`. Supported conflicts
are each table's original natural uniqueness key (in its documented order), plus
`id` for price history; a null conflict performs a plain insert. Duplicate-ignore
and merge semantics preserve existing rows and omitted fields. Original defaults
are supplied only for new rows. The price ID sequence remains owned by the new
timestamp table and retains its position. Original source uniqueness permits
nullable legacy player/time identities without collapsing their observations.

Same-identity writes are serialized with transaction-scoped advisory locks. States
are located using identity plus a hash accelerator AND full typed field equality;
a hash collision cannot merge distinct facts. Repeated facts reuse the state;
new facts add a state and timestamp reference. Each write removes only an
interior confirmation whose immediately preceding and following states are equal.
This happens under the same transaction/identity lock; endpoints remain durable
across restarts and retries. Current fields can return to an older state without
losing intervening changes (A/B/A is three distinct periods). Account scope is part
of finance identity: identical payloads across accounts are not shared.

Worker and clause factual RPCs route these rows after existing lease, account,
league, time and field checks. The direct and deferred R writers share
`supabase_write_request`, including the previous membership RPC. JSON nulls stay
null through the envelope; normal non-history table writes retain their original
path. Writes to the read-only views via an older direct client require adapter
rollout; production worker RPC callers remain compatible through SQL routing.
The historical blocked-league guard is preserved in each new writer. All internal
tables use RLS with server-only grants; public roles cannot read/write them.

## Migration and performance

For large existing databases, use [resumable batches](history_batch_migration.md).
The atomic migration for empty/small databases uses one bounded transaction per
domain, locks its original
table, builds compact storage,
and derives the first/last rows of each factual run using full typed facts, then
checks reconstructed endpoints with bidirectional EXCEPT ALL over the full JSONB
row. It includes every retained field and duplicate multiplicity, independent of
historical column order. Any mismatch/error rolls back that domain. Each transaction has a five-second lock
wait, 120-second statement timeout and 150-second overall timeout. Routing checks
which normalized writer exists so unmigrated domains keep their original path.
Dependent latest-result views are
rebound before dropping old tables, without CASCADE. No cutoff, sampling,
timestamp rounding or guessed values are used. New installations include the
same schema. Original data tables are dropped only after exact endpoint equality
checks. Intermediate confirmation timestamps are intentionally not recoverable.
Historical cutoff reads return an actual retained confirmation at or before the
cutoff; they never substitute a future last confirmation. This gives a conservative
confirmation time, not the exact last poll before that historical cutoff. Late
changes split the retained sequence; discarded intermediate confirmations cannot
be recovered or used to infer an exact change time. No continuity is invented.

Latest score/evidence views use an indexed newest-event lookup per identity
rather than sorting every historical revision. Historical filters can use compact
identity/time indexes. No extra source acquisition or memory cache is introduced.
Bulk endpoint extraction uses sequential reads, typed record comparisons, 64 MB
work_mem and no parallel workers within that step; transaction-local planner and
memory settings return to their defaults afterward. Normal application indexes
and runtime settings are unchanged. Hash joins support the bulk copy, with refreshed planner statistics after loading
entities/states; indexes support indexed state reuse afterward.
Normalization trades small indexed joins for repeated wide strings and payloads.
Writes scan only the matching entity’s compact endpoint list, using identity/time
indexes; total work grows with real changes, not poll count. Ordinary autovacuum
reuses space from superseded interior rows. It does not bound meaningful changes
or unique states. Membership retains its previously deployed every-confirmation
policy; this migration changes the three named histories only.

## Verification

Synthetic fixtures prove exact endpoint migration roundtrip for all fields, player row IDs,
microsecond timestamps, negative/zero scores, JSON null/empty-array payloads,
nullable legacy uniqueness, repeated-write bounds, A/B/A, historical cutoffs,
duplicate/merge behavior, late changes, latest views,
and public-role denial. R fixtures cover RPC envelopes and unchanged ordinary
writes. Persistence recovery (11 groups), continuation bounds, 40 Python worker
tests, and offline Shiny lifecycle (13 checks/eight tabs) passed. Production size
and query measurements are recorded after acknowledged migration; the 500 MB
objective must be checked against measured total size, not projected savings.

## Production status — 2026-11-18

The resumable production migration is complete for all three histories. Initial
atomic attempts timed out; the successful alternative copied and verified complete
identities in separately committed batches, tracked concurrent source changes,
and switched only when the pending queue was empty. Full details, exact counts,
index maintenance and performance checks are in
[the batch migration record](history_batch_migration.md).

Application database size after normalization and concurrent index rebuilding
was 495,479,955 bytes, down from 1,101,270,163 bytes at the start of this rollout.
That excluded system/template databases, so it did not prove quota compliance.
Subsequent physical compaction reduced the full cluster to 347,508,913 bytes; see
[Database storage headroom](database_storage_headroom.md). Every
factual transition and first/last confirmation survives; intermediate confirmation
timestamps are intentionally discarded. Membership retains every timestamp.

The SQL source schemas and application write/read contracts are identical between
the atomic empty-database path and the batched existing-database path. Use only
one migration path per domain. The adapter changes were committed and pushed. The owner confirmed production
remains intentionally stopped, so the running container revision and fresh
automatic-cleanup activity require verification after restart.
