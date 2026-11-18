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

The forward migration uses one bounded transaction per domain, locks its original
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
Hash joins support the bulk copy, with refreshed planner statistics after loading
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

## Production status and rollout limitation — 2026-11-18

The combined production transaction timed out and rolled back. Subsequent
per-domain attempts were blocked by connection timeouts during migration-history
initialization; one tool confirmation expired. At final verification all three
normalized timestamp relations were absent and the original tables remained.
Database size was 1,101,221,011 bytes; the below-500-MB target is **not achieved**.
The last automatic cleanup success was 18:55:07 UTC. This does not establish a
cleanup failure while the app is stopped, nor verify current automatic operation.

The single forward SQL file contains three independently committed domain blocks
(price, match, finance), each with its corresponding writer routing and checks.
Apply these blocks in order once production SQL connections are reliable; verify
catalog state after any ambiguous timeout before resuming. Do not rerun a domain
that already committed. Deploy the R adapter only after all three writers exist.
Then measure total and per-domain storage, sample indexed reads, verify fresh
worker writes and advancing cleanup counters, and run security advisors. Health
advisors returned no findings, but security advisors timed out and are outstanding.

The final isolated SQL fixture processed 200 identical confirmations per domain
in 148/125/123 ms (price/match/finance), retaining one additional endpoint and no
additional state. These are synthetic single-entity measurements, not production
latency estimates. The split migration passed endpoint, A/B/A, late arrival,
cutoff, context isolation and access-control fixtures. R envelope, membership,
persistence recovery (11 groups), continuation bounds, 40 Python worker checks,
and the offline Shiny lifecycle (13 checks/eight tabs) passed. An initial Python
unittest module invocation failed to locate the module; running the test file
with the project PYTHONPATH completed all 40 checks successfully.

Changes are locally committed; push/deployment is withheld because the production
RPC migration has not succeeded. No production reduction is claimed for this task.
