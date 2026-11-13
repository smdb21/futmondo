# History marker and resolved-resource recovery

Implemented 2026-11-13; production screen recovery requires the new container.

## Reproduced defects

`fm_time()` uses a named `vapply` result for ISO inputs. Formatting that timestamp
inside the history marker constructor gives the frame a nonnumeric row name.
By default jsonlite serializes that name as an extra `_row` property. The actual
marker table has exactly eight factual columns and no `_row`; production marker
POSTs returned HTTP400/PGRST204 while score writes succeeded. Earlier fixtures
used unnamed POSIX values and missed this path. Default `fromJSON()` can also
consume `_row` as a row name and conceal the extra wire field: the new regression
inspects literal row-object keys, not reconstructed data-frame columns.

`supabase_payload_json(payload)` disables implicit row-name serialization for
both direct and queued database writes. The persistence pipe also sets
`rownames=FALSE`. Marker frames explicitly use automatic row names, retain
microsecond source precision, and still write scores before their completion
marker. No database column or source value is invented to accept R metadata.

The duplicate acquisition guard previously skipped unresolved jobs without
rebinding their GUI consumers. Production metadata showed the GUI still
requesting an old unresolved pressroom key while its resolved peer was publishing
new observations. This left the old request stale/pending indefinitely.

## Additive resolved-resource contract

Migration `20261113115929_worker_resolved_handoff.sql` adds server-only
`worker_resolved_peer(account, descriptor)` and a `redirects` array to
`worker_read(account, versions)`. Each element contains `requested_key` and the
canonical `descriptor`. Reads return canonical resource chunks and distinct
canonical job statuses; existing version checks avoid repeated chunk transfer.
A call considers at most 128 requested keys and two matching peer candidates.
The existing account/handler/argument-hash index narrows peer lookup.

Only roster, pressroom and clause-summary reads can redirect. The target must
be unique, current-generation, enabled, unblocked and match all three known
competition/season/scoring dimensions in verified account membership. Wire
arguments, protocol version, account and other context dimensions must match
exactly. Known requested dimensions cannot change. Without authoritative known
membership, no redirect or duplicate suppression occurs, preserving bootstrap.
Old observations, checkpoints, identities and source times remain untouched.

The R provider verifies target identity/hash, cancels obsolete waiters, registers
the canonical descriptor and retires the old interest. A bounded account map
(128 entries, 300-second identity lease) lets factual adapters rebuild under the
canonical context. A newer source check can extend this identity hint; rereading
an old version cannot. Rows are never cached under the old unresolved key.
`factual_read_adapter(..., state_only=TRUE)` now includes a `context` field.
Transfer verification passes that resolved scope explicitly into summary reads
and persistence, and refuses mismatched prerequisite contexts. Page/account
ownership still follows the current selection, so old-context subscriptions stop.
Status counts and oldest checks cover active canonical interests, not retained
inactive descriptors. No authentication secrets cross this interface.

## Independent progress and failed-save recovery

Clause cadence remains 300 seconds/base priority3, independent of page activity.
The reserved short lane admits normal due clause work. In that lane normal due
core reads receive effective urgency9, clauses8, and bulk demand retains its
ordinary priority. Explicit priority10 demand remains ahead. This prevents
527 initial history demands or a long finance composite from starving a short
clause check without increasing worker count, source cadence or rate limits.
Quiet hours and source deadlines remain unchanged.

The history controller retains failed normalized payloads for write-only retry
(up to25 payloads/8MiB per retained entry). Explicit Refresh retries these writes
without another source acquisition; other requested history corrections keep
their ordinary behavior. Errors clear only on successful acknowledgement. A
newer source attempt cannot be overwritten by an older callback. These memory
references are bounded retry accelerators: opted-in successful acquisitions also
remain in durable worker snapshots, which cold controllers can reuse even after
their current-source display deadline expires. Existing source completeness and
context validation still apply; score rows alone never fabricate a source check.

## Checks and performance

Executed regressions include the real marker constructor/queued writer/direct
writer/bridge with a named fractional ISO timestamp; failed-save source-free
retry; actual async adapter and typed compressed snapshot handoff; foreign
account/season/integrity and prerequisite-context rejection; SQL migration replay,
canonical version reuse, bootstrap and clause dispatch with527 bulk jobs plus a
running finance job. Python worker30 checks, incoming24/outgoing24, provider9,
live updates4, clause refresh11, persistence recovery11, session ownership3,
deployment3 and the offline lifecycle13/eight tabs passed. The existing 527-player
worker replay completed with526 stored histories plus one durable acquisition
and zero source HTTP. Seven acknowledgements at527x38 rounds took0.234s total,
with0.119s worst heartbeat gap and zero full-matrix rescans, fitness recalculations,
extra reads or publications in that fixture. These are local measurements.

One extra cache-boundary fixture (`test_round_history_cache_invalidation.R`)
fails its future-ID eligibility assertion on both unchanged HEAD and this change;
its preceding acquisition/save checks pass. This existing failure is recorded,
not represented as a passed check or changed by this task.

Performance review: the same RPC carries bounded redirect metadata and changed
chunks, avoiding an extra source request or polling timer. Hints/watchers stay
bounded; failed-write references have an explicit byte cap. Foreground ordering
changes dispatch priority only, not acquisition volume. No dependencies or
application bundle additions are needed. No live account or transaction tests
were run. Production migration version: `20261113121612_worker_resolved_handoff`.
The environment has no connected Docker daemon/Dokploy deployment tool, so
container deployment and two five-minute production acceptance cycles remain
outstanding. Do not mark production recovery complete until marker POSTs succeed,
reported transfer times resolve, and reloads reuse acknowledged stored results.

The production policy check also found the enabled account inside its configured
quiet hours during rollout. Ordinary jobs therefore retain their hourly quiet
floor; explicit interactive demand can still bypass it. Quiet-hour settings were
not changed to manufacture a successful freshness measurement.

## Auto deployment, dispatch aging and cold stale pages — 2026-11-13

The owner confirmed that Dokploy auto-deploys pushes. A push is the normal rollout
trigger; lack of a local Docker daemon does not imply that a manual deployment
is needed. Public static artifacts had a Last-Modified time immediately after
`c1f9dd3` was pushed and the production worker heartbeat was current. That is
consistent with automatic deployment, but is not an exact deployed-commit hash.
No completion-marker POSTs appeared in the inspected12:25–14:20UTC window, so
absence of errors in that window is not evidence of successful marker writes.

Production checks found normal core reads continuously outranking autonomous
clause jobs. `20261113142221_worker_clause_fairness.sql` promotes a clause job
from effective urgency8 to9 after its due time is at least60 seconds old in the
reserved lane. It then competes with ordinary core reads by oldest due time;
explicit priority10 demand stays ahead. This prevents starvation under continuous
normal core demand without changing stored priority3,300-second cadence, leases,
quiet-hour eligibility, source rate limits or worker count. It is an aging rule,
not a guarantee that dispatch occurs within60 seconds under all loads. Production
SQL version is `20261113142423_worker_clause_fairness`. The previously stalled
three active clause jobs all refreshed at14:24:48–14:25:03UTC, then again at
14:32:03–14:32:18UTC. At14:33:09 all three snapshots were fresh with no job errors.

A new screenshot still showed empty panels because a cold page required roster
and history to be fresh simultaneously before displaying anything, even when
successful durable observations and clause summaries already existed.
`fm_worker_restore()` now retains the original manifest acquisition status in
`worker_acquisition_status`, separately from current expiry. This metadata already
exists durably in the manifest; it is not a new success assertion.

`reconcile_pending_clause_transfers(..., observed_only=FALSE)` preserves the
strict default contract. The explicit display-only mode can derive observed
schedules from successful stored roster/history acquisitions, including expired
ones, in matching account/league/competition/season/scoring contexts. Summaries
must identify the requested player and either be currently usable or carry
successful worker acquisition proof with a valid original observation time and
no source failure. Expired successful summaries remain historical facts, not
fresh ownership claims. Partial, unavailable, malformed or foreign observations
cannot provide successful timing. Source flags, observation times and deadlines
are never cleared or renewed.

That mode always returns overall status `unavailable`, outbound/inbound counts
`NA`, no capacity-adjustment IDs, and partial outgoing verification. It only
supplies last-observed display rows. The cold-page path keeps stale prerequisite
warnings, adds an explicit last-observed label and reads existing snapshots
through passive interests. It does not start source acquisition. The optional
`allow_observed=FALSE` cache-reader flag is enabled only there; ordinary reads,
financial calculations and manual preflights still enforce their freshness rules.

Checks: four SQL groups reproduce the prior starvation, verify idempotent replay,
aging boundary, normal core precedence, quiet hours and explicit Refresh; existing
cadence/prefetch groups pass. Four observed-display groups cover strict capacity
rejection, partial/expired-source validation, preserved historical timestamps and
two cold sessions with zero source reads. Four passive-worker groups ensure
historical reuse is explicit and never renews a deadline. Incoming24/outgoing24
and required lifecycle13/eight tabs pass. Performance: aging changes order only;
observed rendering uses a small candidate set and bounded existing snapshot
polling, with no new timer, worker, database table or source request. Full mobile
production screen verification and successful completion-marker POST evidence
remain separate acceptance work.

A subsequent production log check starting14:20UTC confirmed three score POSTs
and three completion-marker POSTs, all HTTP201, with no PGRST204 in that window.
This confirms the earlier marker serialization repair is active in production.
The observed-panel change additionally displays original UTC observation times in
both panels; it still needs to be seen on the user's page after auto deployment.
Manual capacity recheck4 and durable clause persistence11 checks also pass.
