# Worker freshness and history-write recovery

Implemented 2026-11-13 after the deployed parser/discovery repairs still left
clause panels repeatedly stale. Production metadata established continuing
acquisition delay and 93,307 notification revisions for 138 immutable clause
payment source events (3,486 revisions in the preceding hour). This change fixes
repeat work and scheduling; it does not manufacture freshness or reinterpret
rejected source responses as successful observations.

## Source demand and acquisition lanes

`shared_resource(..., invalidate_refresh)` adds an optional predicate receiving
previous/new refresh versions. It defaults to the original explicit invalidation
behavior. `factual_read_adapter()` supplies separate `manual` and `expiry`
versions: automatic expiry fetches/reuses the current provider result without
invalidating its running job; explicit Refresh retains invalidation and a
follow-up acquisition. Durable worker version replay still bypasses older
session results without creating another acquisition request.

`fm_worker_priority(handler)` defines source-read urgency separately from source
validity. Roster, transactions, market, offers, lineup, notifications and account
discovery use 10; resolved context/round discovery 9; finance/capacity aggregates
8; remaining historical/display resources 6. Explicit Refresh requests 10.
`worker_request_priority(text)` mirrors this policy for queued default demand.
No timestamp, lease, fact or freshness deadline is changed by that normalization.

With the existing two lanes, `worker_claim(..., p_foreground=true)` excludes
`fm_worker_finances`, `get_acquisition_capacity`,
`get_team_acquisition_capacity` and `get_financial_snapshot`. Those longer
composites retain the routine lane, including when explicitly requested.
When no browser is open, the foreground lane can also claim due short core
resources whose policy urgency is at least 9 (normal priority 3/4). This keeps
opted-in roster/history/configuration observations warm instead of leaving the
lane idle behind a long routine job. Low-priority bulk catalogs and long
composites remain excluded. A one-lane configuration has no separately reserved
foreground lane. Source
connection/RPS limits, account admission and all existing lease/context checks
remain. No worker/process count was increased.

Recent-point resources now share their upstream summary's existing five-minute
cadence. The migration normalizes legacy 60-second jobs; the enqueue guard also
normalizes requests from an older live R/browser process during rollout. Manual
Refresh still requests work immediately. Existing source/embedded deadlines
remain intact, and private transaction preflights remain direct/live.

## Durable acknowledgement reuse

`pressroom_locker_ack_callback(context, hashes)` records event fingerprints only
after all requested notification-evidence writes acknowledge success.
`persist_rival_finance_evidence(evidence, context, on_complete=NULL)` retains its
existing interface and adds an optional all-batches callback. Rejected context,
oversized facts or failed writes cannot certify the full set. Empty input is an
acknowledged no-op. Revised events remain new writes; unknown fields stay unknown.

The R helper attaches `history_write_ack` (native auction facts) and
`locker_write_ack` (payment notification facts) as typed named character vectors
in its existing persisted pressroom snapshot metadata. Each maps source event ID
to the acknowledged canonical fingerprint. Metadata is captured after deferred
callbacks drain; it contains no credentials and is bounded to 20,000 hashes per
kind, within existing 16 MiB snapshot/8 MiB child publication limits.

The private `history_ack` pipe request accepts only a scoped
`get_championship_pressroom` descriptor. Python reads and verifies the existing
snapshot, then returns only those two metadata fields. Expired snapshot rows are
never returned by this path and cannot become fresh source data. The R helper
validates vector types, bounds, names and hashes before restoring its bounded
write cache. Account/league/competition/season/scoring contexts must match.
Thus a new helper/process can skip unchanged, already-persisted facts while
still performing the required current source checks. A missing acknowledgement
or cache read is a miss and retries persistence. Normalized facts remain in the
DB; memory is only a bounded accelerator. Existing historical revisions are not
deleted or rewritten by this repair.

## Rejected optional finance reads

For the finance handler only, an explicit application `source_rejected` is sent
to R as a fixed error condition. Existing R `tryCatch()` decisions govern which
reads are optional. Rejected bodies are neither forwarded as data nor
checkpointed. Required reads can still fail the finance job. Authentication,
revocation, context, rate limit, integrity and persistence failures remain fatal.

The finance manifest retains partial completeness and the safe source-error
code when such failures occur. Other successfully acquired child resources keep
their own source status, rather than inheriting an unrelated optional failure.
Unverified child data is marked partial. Known ledger calculations can be shown
while an unavailable private balance stays unknown/unreconciled; zero is never
substituted. Safe rejection diagnostics include only allowlisted route names,
never query IDs, credentials or raw source bodies. The exact reasons for the
production source rejections remain unverified; this is error isolation and
truthful partial progress, not a claim that those endpoints now succeed.

## Setup, validation and limits

Apply `scripts/migrations/20261113053241_worker_display_priority.sql`, then
`scripts/migrations/20261113054538_worker_points_cadence_guard.sql`, then
`scripts/migrations/20261113055439_worker_core_read_lane.sql` before the matching
image. Production recorded these as
`20261113054456_worker_display_priority` and
`20261113054649_worker_points_cadence_guard` and
`20261113055513_worker_core_read_lane`. New databases use `scripts/schema.sql`.
The functions are invoker/server-only; there are no new tables, runtime packages,
environment variables, background services or transactions.

Regression evidence is in `data_review_results.md`. Freshness deadlines and
unavailable states are deliberately preserved. The source pipeline must still
retrieve and acknowledge current inputs; a transient outage can therefore show
stale data. Two successful snapshots seen once do not establish sustained mobile
acceptance, so deployed repeated-cycle verification remains necessary.

## Core prefetch and source timestamp isolation

The further `scripts/migrations/20261113060230_worker_core_prefetch.sql` repairs
completion-based scheduling: a snapshot's source validity starts at acquisition,
while old queue timing added the entire interval after processing completed.
A 20-second acquisition therefore had an unavoidable stale gap even with an
empty queue. `worker_next_refresh_due(handler, interval, valid_until, started,
now)` schedules successful short core resources before their real deadline,
using the last duration plus five seconds, capped at half their interval. It
never changes the source check or validity. Next work is at least five seconds
away; already-expired output receives a bounded 7–15-second retry. Non-core and
partial/failing job cadence/backoff remain. Quiet hours still impose their
hourly floor, and explicit Refresh during a run still invalidates its output
and stays immediately queued. Early due claims outside quiet hours now obey
`due_at` without the old second completion-plus-interval gate.

Python publication now uses the result's own observed timestamp, bounded by its
check timestamp. Another older child in the same composite cannot backdate a
fresh roster sibling. Helper results without an explicit observation retain
the earliest actual source/checkpoint time; they do not become fresh merely
because parsing completed. Legacy results lacking observation metadata retain
the previous conservative source-stamp fallback.

The change can increase short core acquisition frequency, at most approximately
two cycles per configured source interval, using the existing two lanes and
account/connection/RPS bounds. That is an explicit freshness/performance tradeoff,
offset by five-minute points demand and avoiding unchanged history writes. It
does not guarantee freshness through a slow/outage cycle longer than the budget;
such data remains truthfully stale. Production rollout checks must examine
multiple cycles rather than one successful pair.

Production recorded the prefetch migration as `20261113061754_worker_core_prefetch`.

## Clause validity and resolved prerequisite scopes (initial policy superseded below)

`get_clause_transfer_summary` now requests a sixty-second worker interval and
priority 10. The previous generic summary policy used 300 seconds, exceeding
the ownership check's existing sixty-second source deadline. The database enqueue
RPC enforces sixty seconds even for an older browser image. The protected lane
and existing prefetch schedule apply; publication retains original source times.
Worker-only clause acquisition renews saved clause observations aged thirty
seconds or more. The Python resource bridge applies that same age limit to a
nested general player summary, whose five-minute validity is insufficient for
this check. Younger persisted observations remain reusable; interactive reads
retain the entire original sixty-second deadline. No unknown owner or transfer
time is converted into a successful result.

Migration `20261113063652_worker_clause_scope_guards.sql` adds
`worker_has_resolved_peer(account, descriptor)`. Only unresolved roster and
pressroom jobs yield to an existing current-generation, membership-compatible
peer with identical wire arguments and logical context. Every already-known
competition/season/scoring dimension must match. Other account, league, team,
player and context fields must match exactly. Fully resolved and historical
jobs remain eligible. No old snapshot, checkpoint or evidence is rewritten or
deleted. An account/handler/argument-hash index bounds peer lookup, with exact
JSON equality still required after the hash comparison. The helper is restricted
to the server role, like the claim RPC.

Performance review: two clause jobs existed during rollout. Their shorter
interval adds up to approximately two acquisitions per minute each under core
prefetch, sharing the existing two bounded lanes and source rate limits. This
cost buys ownership verification within its existing source deadline. Production
metadata previously identified one redundant roster and one redundant pressroom
job; skipping those avoids duplicate source/parsing/persistence work. No UI timer,
reactive redraw, per-row browser database query or unbounded memory cache is
added. Old observations remain durably reusable under their original context.
Large-account production latency and continued source rejections are not proven
resolved by these local tests. A container redeploy is required for the R/Python
age guard; the SQL migration alone cannot run new helper code.

Verified with 30 Python worker tests, 11 clause persistence checks, 20 incoming
and 24 outgoing checks, six incoming-source recovery groups, three SQL scope
and cadence groups, four prefetch groups, and the offline lifecycle harness
(13 checks plus eight tabs). SQL migration replay was idempotent. Production
recorded the migration as `20261113064338_worker_clause_scope_guards`.

## Independent clause cadence and stable verification (current policy)

The owner clarified that background acquisition must remain independent of GUI
navigation. `20261113065753_worker_clause_cadence.sql` supersedes the one-minute
clause acquisition policy above: successful clause jobs use 300 seconds and
base priority 3; explicit read demand uses urgency 8, and explicit Refresh still
uses 10. Routine claims continue with no browser or active page. This handler is
outside short-core prefetch, so its successful completion is not scheduled again
after half an interval. The enqueue RPC normalizes older 60-second requests to
300. Existing source failures/backoff, consent, leases and account isolation stay
in force. Production applied `20261113070822_worker_clause_cadence`; metadata
confirmed all four existing clause jobs use interval 300 and base priority 3.

`get_clause_transfer_summary()` is used by the transfer display, not transaction
preflight. Its durable safe-subset payload is version 3, with a new source-ID
namespace and an absolute source-observed-plus-300-second validity. Version 2
observations remain stored separately. Worker and interactive reads reuse valid
version 3 results across minute boundaries without renewing their observation
time. The previous worker-only thirty-second renewal guards are removed. Raw
player-summary and financial/manual-action policies are unchanged.

`incoming_clause_source()` hashes sorted relevant roster membership/clause fields,
transaction identities/parties/times/prices and full account/league/competition/
season/scoring context. A new source timestamp alone does not cancel a batch or
empty successful summaries. Transient stale prerequisites pause further queue
starts while already-started requests settle; their original captured context
and generation guards remain. Retained results stay accompanied by truthful
prerequisite warnings. Refreshed unchanged prerequisites resume the same batch.
Actual transfer evidence, manual refresh and context changes still trigger
reconciliation. Routine prerequisite refresh does not advertise a new clause
check when an existing snapshot is displayed.

`incoming_clause_cached_summary(..., is_current=NULL, owner='')` can consume a
newer fresh durable worker publication through the existing bounded snapshot
cache and resource revision. Its optional interest is passive: `fm_worker_poll`
does not enqueue expired clause subscriptions. Waiting clause reads, unlike
ordinary stale-while-refresh reads, settle only with an unexpired publication;
completion removes their acquisition interest. Passive subscriptions consume
new source changes without altering the independent schedule. No new timer,
HTTP lane, worker count, database table or dependency is added.

Performance review: unchanged timestamp publications add zero summary
acquisitions in the regression fixture, preserve the joint two-request cap and
avoid repeated cancellations. Four production clause jobs now have a normal
aggregate cadence of 0.8 job acquisitions/minute, rather than minute-based core
prefetch. HTTP counts can differ with source reuse/failures. Hashing covers only
the small relevant roster/candidate fields; passive reads reuse existing
account-wide polling and fetch chunks only for changed versions. Existing
bounded caches/watchers and durable database writes are reused. No synthetic
benchmark is presented as measured production latency.

Verification: 30 Python worker tests; 24 incoming checks (four new churn/expiry
cases); 24 outgoing; 11 durable clause checks; nine worker-provider groups;
three passive observation groups; two SQL cadence groups (idempotent replay,
quiet independent background dispatch); four ordinary worker-expiry checks;
11 history persistence recovery groups; offline lifecycle 13 checks/eight tabs.
The narrow marker-schema diagnostic regression also verifies that private
responses/queries are never logged and rejected writes stay failed.

### Separate production history-save blocker

Production logs in 06:40–06:55 UTC showed 22 marker POST failures with HTTP 400 /
PGRST204, while 21 score POSTs returned 201 and one returned 200. Live metadata
has every expected marker column and the expected context/check-time primary
key. These facts do not identify the rejected column. Local server database
credentials are absent, so a limit-zero REST schema probe could not run; log
metadata did not expose the offending field. The defect is not declared fixed.
`fm_transport_finish()` now emits only a validated missing-column identifier for
a small, standard PGRST204 response on that exact marker route. It never logs raw
body, payload, query, credential or source values. After the app redeploys, the
next failed marker response can identify the field/schema-cache mismatch needed
for a root fix. A failed marker remains failed and cannot certify durable
history completion. Container rollout and successful production transfer/history
cycles remain outstanding.
