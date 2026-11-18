# Resumable history migration

This is the operational alternative to the atomic normalization migration for
large existing databases. It preserves every factual transition and first/last
confirmation of each unchanged period. Intermediate confirmation timestamps are
discarded under the owner's approved retention rule. Fresh empty databases still
use `scripts/schema.sql`; do not run both migration paths on the same domain.

## Administration sequence

Use `scripts/history_batches/prepare_<domain>.sql`, with domain `price`, `match`,
or `finance`, to create private staging relations, a preview view and a source
change tracker. Preparation does not replace the source table or copy its data.
The source remains the application's authoritative read/write interface.

Call `<domain>_migration_seed(10000)` repeatedly until the status table's `seeded`
field is true. Discovery uses a durable JSON keyset cursor over the source primary
key, reading only identity/key columns. Pages are bounded to 1–20,000 source rows;
there is no OFFSET and no payload-wide discovery sort. The function returns the
number of source keys scanned; zero after completion is an idempotent no-op.

Call `<domain>_migration_batch(100, 10000)` until the returned `pending` count is
zero. The arguments bound identity count (1–200) and elapsed budget between
identities (1–20,000 ms). Each complete identity is copied, compacted and checked
with bidirectional full-row `EXCEPT ALL` before its queue entry is removed.
Identities are not split across batches, so change boundaries cannot fall between
pages. An unusually large single identity may exceed the soft time budget; use a
server statement timeout (normally 25 seconds in production calls). One finance
identity contained 67 MB across 305 payloads and required a 60-second allowance.
Queue locks can temporarily delay writes to identities in the active batch; reads
remain available. Failure rolls back
that batch's data and progress together. A repeated call resumes the durable queue.

Inspect `<domain>_migration_verified` counts and `<domain>_migration_queue`, then
apply `scripts/history_batches/cutover_<domain>.sql`. Cutover takes a short source
table lock, refuses pending/unverified identities, rebinds the original view and
latest-result views, installs the normalized writer and existing worker/clause
routing, and drops the old table only after these checks. No CASCADE is used.
The price ID sequence keeps its current position and changes ownership at cutover.
The status and verification records remain as a private migration audit.

Check catalog state after an ambiguous connection timeout before retrying a
preparation or cutover. Do not rerun one that has already committed. Deploy the R
adapters only after all three domains have cut over. A push to the current remote
triggers deployment. Measure actual total size afterward; estimated savings are
not proof that the database is below the quota.

## Concurrent changes and verification

An AFTER INSERT/UPDATE/DELETE trigger creates the relevant identity, marks its
queue entry pending and invalidates prior verification in the source transaction.
Updates enqueue both old and new identities. Deletes do not recreate identities
removed by parent foreign-key cascades. TRUNCATE is rejected during migration.
This catches changes behind the discovery cursor as well as changes after copying.

A copy batch locks queue rows with FOR UPDATE SKIP LOCKED. A source change that
waits for that lock requeues its identity after the batch commits, so a copy cannot
silently acknowledge a change it did not see. Batch and cutover lock the source
before acquiring other migration locks. The final ACCESS EXCLUSIVE source lock
waits for in-flight source writes and prevents new invalidations during cutover.
Every identity must have a verified copy and the dirty queue must be empty.

Staging tables and audit relations use RLS and have no public-role grants.
Seed/batch administrative functions are revoked from public, authenticated and
service roles; they run through the administrative database connection. Trigger
and application writers remain SECURITY INVOKER. No authentication secrets or
private payloads enter migration progress output. Nothing schedules source API
requests or background transactions.

## Performance and tests

Discovery uses existing primary-key indexes. Identity copying builds equality or
IS NULL predicates as appropriate, keeping indexed reads usable for legacy null
keys. Each batch holds locks and temporary data only for its bounded group; progress
is durable across connection/session loss. Temporary source and retained rows are
reused per identity. Payload signatures are computed once per retained row and
the temporary signature column is analyzed before joining states; full field
equality still protects against hash collisions. That temporary column is excluded
from the original-row comparison. Several separately committed batches may share
one tool request to reduce connection overhead without losing checkpoints.
The change tracker adds one queue write per changed identity
while migrating, then is removed at cutover. Normal application timers, network
requests, UI redraws and caches are unchanged.

`node test/test_history_batches.cjs` checks interrupted transaction rollback,
resumption, discovery pagination, behind-cursor inserts, source insert/update/
delete invalidation, A/B/A endpoints, nullable keys, independent JavaScript
known-answer results, premature-cutover refusal and denied public access.
`node test/test_history_normalization.cjs` checks the common normalized writer
contract. These are isolated synthetic fixtures; no real account transactions
are used. Multi-connection lock races are reasoned from PostgreSQL locking rules,
not simulated by the single-connection PGlite fixture.

Operational SQL is generated by `python scripts/history_batches/generate.py`
from the tested atomic migration's schemas/writers, keeping the final read and
write contracts identical. No new package or environment variable is required;
`scripts/` is already excluded from deployment bundles.

References inspected 2026-11-18:
- https://www.postgresql.org/docs/17/sql-createtrigger.html
- https://www.postgresql.org/docs/17/explicit-locking.html


## Production result — 2026-11-18

All three preparations, copies and cutovers succeeded. Price was discovered in one
initial identity scan; match/finance used the subsequently added paged discovery.
A timed-out finance batch was resumed after adding materialized payload hashes;
no completed progress was lost. Each domain had zero pending identities at cutover
and verified source counts matched the original table count.

| Domain | Source rows verified | Retained endpoints | Final normalized table/index bytes |
|---|---:|---:|---:|
| Player valuation | 444,610 | 77,298 | 24,690,688 |
| Match observations | 196,129 | 49,042 | 16,965,632 |
| Finance evidence | 188,505 | 35,875 | 118,366,208 |

Nine existing indexes were rebuilt concurrently without changing definitions or
removing rows. Their names and before/after byte sizes are recorded below.

| Index | Before | After |
|---|---:|---:|
| membership_times_pkey | 20,258,816 | 12,304,384 |
| user_team_history_source_time | 11,132,928 | 6,389,760 |
| idx_team_history_team | 6,381,568 | 3,588,096 |
| team_history_full_context_page | 5,283,840 | 5,251,072 |
| team_history_context_page | 4,153,344 | 4,120,576 |
| player_round_history_checks_pkey | 3,989,504 | 3,047,424 |
| player_round_checks_league_context | 3,981,312 | 3,047,424 |
| player_daily_snapshots_context_date_key | 3,801,088 | 2,260,992 |
| idx_snapshots_player | 1,605,632 | 1,196,032 |

Database size changed from 1,101,270,163 to 495,479,955 bytes (495.5 decimal MB).
This is below 500,000,000 bytes, with limited headroom; useful new history can still
grow. Membership observation timestamps were not removed. All indexes were valid
and ready. Sample indexed production reads executed in 7.468 ms (price), 79.778 ms
(match latest) and 224.207 ms (finance latest), with no temporary blocks written.
These are single sampled reads, not production latency percentiles.

Security advisors reported no warning on the new histories. RLS-without-policies
info notices are intentional for service-only tables. Existing warnings concern
GraphQL schema visibility of the unrelated futmondo_players table and public
EXECUTE on the rls_auto_enable event-trigger function; those predate this work
and were not altered. References:
- https://supabase.com/docs/guides/database/database-linter?lint=0008_rls_enabled_no_policy
- https://supabase.com/docs/guides/database/database-linter?lint=0026_pg_graphql_anon_table_exposed
- https://supabase.com/docs/guides/database/database-linter?lint=0028_anon_security_definer_function_executable

All resumable-migration fixtures, including large finance payloads, passed. The
offline Shiny lifecycle rerun passed 13 checks/eight tabs. Before deployment the
last cleanup success was 18:55:07 UTC and latest worker heartbeat 21:16:53 UTC,
both stale at 22:52 UTC; current automatic cleanup still needs verification after
deployment. No manual cleanup call was used to disguise stale automatic status.
