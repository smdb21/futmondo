# Interactive player availability

## Database display versus transaction checks — 2026-11-16

Production browsing follows [database-first sync](database_first_sync.md). The
older acquisition-on-selection behavior below applies only to the native/offline
path. Opening, reopening and Refresh read persisted worker observations; they do
not verify availability against Futmondo. Database-mode cards no longer offer
**Retry availability**, and a late event from that removed control cannot start a
live request. Missing/incomplete saved evidence remains unverified and refers to
the existing Background sync status instead of inviting interactive collection.

`player_availability_observed(data, kind, now)` validates successful stored
`summary`/`listings` observations for display independently of action expiry. It
requires original nonfuture source time, a finite original deadline, successful
worker acquisition and no failed/partial completeness or fetch error. Listings
also require valid unique IDs (an explicitly successful empty list is valid).
Legacy unknown envelope completeness is not promoted: summary target fields
still have to satisfy `player_card_summary_row(player, summary, observed=TRUE)`:
matching immutable player ID, explicit ownership and the captured market-price/
listing contract. Unknown ownership/listing stays unknown.

This projection retains the original stale flag, timestamps, action deadlines
and unrelated player statistics. Existing bounded session display markers allow
reopening, while the underlying facts continue to come from durable database
snapshots after a restart. Newer successful saved facts update the open card.
No additional cache, acquisition, persistence write, timer or worker is added.

The card's `player_target_current()` separately checks original source freshness
when a market offer is explicitly opened. `ensure()` joins/reuses one current
target check or starts it with manual-action intent. Submit waits for both target
ownership/listing and current financial capacity; a display marker alone cannot
authorize it. Final transaction checks and uncertain-write protections remain.

## Target-only selection and source deadlines — 2026-11-15 Pacific

Opening/selecting a player verifies only that player's ownership, market listing,
asking price and available clause fields. It never starts team capacity, balance,
roster or transfer-history acquisition. These financial preflights start only on
explicit market-editor demand; native source freshness repair and final purchase
checks are retained. Own-player listing verification remains a separate sale gate.
Points, profile, lineup and historical chart reads run independently of purchase
capacity; their failures cannot masquerade as availability checks.

`player_availability_deadline(data, resource)` preserves an explicit source
`valid_until`, including a stricter worker/database deadline; legacy native
observations fall back to their original observation plus the existing resource
policy. `championship_market_prices()` carries separate
`availability_owner_valid_until` and `availability_market_valid_until` attributes,
plus the original market clock and completeness. Catalog observations therefore
keep their five-minute lifetime while market facts retain their own deadline.
`player_availability_market_rows(data)` preserves endpoint listing proof and the
market deadline for the Market and Today tables, including persisted worker data.
`player_card_recent_table_facts(player, now)` checks both deadlines and rejects
missing/future clocks, unknown owner/market fields and failed/partial joins. The
previous blanket 60-second catalog cutoff is removed from this joined path.

Recent table facts are sufficient for card/editor entry, even while history is
pending. For stale/unknown rows the existing scoped summary provider retrieves
only this player. `player_card_summary_row(player, summary)` requires a matching
immutable ID and explicit owner/market contracts. It maps `_id`/`id` and
`computer` ownership, market `p` asking price and the captured listing markers;
a `bids`-only response establishes no listing. When `championship.owner` is absent, an explicit logical
`data.computer=TRUE` also establishes a System/free-agent player. Dani Lorenzo's
actual saved summary uses this shape. Missing/false/NA/textual flags do not prove
ownership, and a mismatching returned player ID remains invalid. Missing evidence
is unknown. Successful retrieval updates ownership/listing controls and clears stale asking
prices/clause fields, preserving original score/statistic values and source
clocks. An obsolete player or league completion cannot replace the current view.

`player_availability_remember_target(session, identity, row=NULL)` retains
successful target display checks for reopening, switching, source expiry and
worker publications. Identity includes exact account/league/team/player,
competition/season/scoring scope, explicit Refresh and summary invalidation epoch.
The root-session registry holds at most 64 rows / 4 MiB, with a 128-KiB row cap
and inactive/LRU eviction. It stores no authentication, timer or polling job.
The underlying catalog/market/summary facts already use their context-isolated
successful database snapshots and acknowledged persistence queues. This separate
session marker is not durable authorization: another session must validate its
persisted sources' original freshness and completeness before reusing them.

`player_availability_source()` retains its `retry` and `verification` attributes
and adds `ensure()`: explicit editor demand first attaches a dormant existing
entry, reuses it if actually fresh, joins a pending check or requests one fresh
calculation. An inactive accessor's loading shell cannot force a duplicate read.
Market Submit waits for both the current target facts and financial preflight;
completion of funds alone cannot authorize unknown ownership/listing. The editor
uses the updated matching target row. Final submission preflights, immutable
context, duplicate protection and uncertain-response reconciliation are intact.

A failed/expired financial completion stays blocked in its editor; it does not
start an outer whole-card retry. Explicit editor reopening is the retry path.
The native acquisition engine still takes short-lived funds/offers after
history/roster/transfer candidates and performs its existing bounded source
repairs without changing deadlines. Card Retry retries target facts only.



Implemented locally on 2026-11-15. This refines the existing manual player-card
workflow; it adds no unattended acquisition or transaction. Production latency
and rollout are distinct from the offline checks in `data_review_results.md`.

## Shared team state and target summaries

`player_availability_source(session, kind, context, login, refresh, active,
stored, current_context, mode, opening)` returns shared resource state and the
`retry` and `verification` attributes used by the card. `kind` is `team`,
`summary` or `listings` (the own-team sale-listing gate). A root-session registry shares pending and completed results across
player-card modules. Team identity includes account, league, own team,
competition, season, scoring version, worker mode, relevant invalidation epoch
and explicit refresh revision. The selected player and chart-history refresh
are excluded. Summary identity additionally includes the immutable player ID.
Accessors reject the previous context/revision immediately, before new reads
settle. Returned player IDs, when supplied by the source, must match the request.

Opening or switching cards joins/reuses target verification. Financial requests
are admitted only by an explicit editor and can share a completed fresh team result. Errors settle until explicit retry; unrelated metadata redraws and
worker publications cannot become acquisition loops. A closed card detaches its
consumer; a finite already-started read may finish within the same interactive
session for the next card. Account/team/context/refresh/mode changes and session
end invalidate old completions. Per-player confirmation generations and final
manual preflights remain independent and unchanged.

The provider checks valid decoded worker observations and the native factual
cache first, then makes one cooperative database lookup before a source read.
Missing, expired or incomplete selected-player summaries use the existing
`get_player_summary` adapter with interactive priority and `worker_bypass=TRUE`;
they do not wait for a worker publication. Owned-player listing checks similarly
share the existing `get_my_market_players` read across owned cards, preserving
its effective 20-second market lifetime. Explicit editor team verification uses the same live
asynchronous capacity calculation. Optional lineup/profile/match/valuation
requests are independent of financial availability. Independent transport
limits, reserved interactive capacity and bounded freshness repairs are retained.

Offer dialogs join the shared financial provider on demand. Opening an
expired/failed editor explicitly revalidates actual freshness. A target summary
and financial check can settle in either order; both must succeed before Submit
is enabled. Original team source deadlines (including the at-most-15-second
composite deadline) and summary's 60-second acquisition lifetime remain intact.
A first card after explicit Refresh cannot restore pre-refresh observations.

The root registry retains at most 64 entries, 8 MiB per decoded result and
16 MiB of retained result data per session. Inactive settled entries are evicted
first; active requests/consumers are not silently replaced with another context.
Budget failures remain explicit unavailable states. The existing global native
cache retains its own separate limits. No polling timer is added.

## Durable facts and invalidation

`player_availability_scope()` and `player_availability_id()` define a versioned
`rival-cache:availability-v1:` namespace in the existing server-only
`rival_finance_evidence` table and `rival_finance_evidence_latest` view. These rows
remain excluded from financial evidence by the existing cache-prefix rule.
No new schema, access grant, worker schedule or authentication storage is added.
Valid worker observations are already durable and do not need duplicate
serialization on the readiness path.

`player_availability_save(data, context, kind, started_at, on_complete, deferred=FALSE)` stores
only successful reusable facts through the existing bounded persistence queue.
The payload contains `kind=player_availability`, `version`, exact `scope`,
`encoding`, compressed `data`, `json_bytes`, `sha256`, `completeness`, `source`,
`source_observed_at`, `valid_until` and `started_at`. The encoded body holds the
factual value and an allowlist of original source attributes. Durable summaries
require a matching returned player ID; team snapshots require the existing
complete-observation proof, including transfer identities and financial fields. The encoder reuses
the primitive snapshot codec, rejects authentication/secret fields and preserves
negative/zero/missing values. Doubles and times use round-trip decimal text so
JSON cannot round source timestamps or renew a deadline. Raw JSON is capped at
8 MiB and the compressed/base64 payload at 220 KiB. Cache rows are ordered by the
original factual observation time, so a slow older save cannot overwrite a
newer observation merely by finishing later.

Active card saves begin after the ready output flush. The existing primitive
encoder is bounded synchronous CPU work; `player_availability_json_async()` then
emits the vetted JSON in finite approximately 4ms steps with 1ms cooperative
yields. Preparation and queue acknowledgement are tracked separately; no timer
is treated as a background CPU worker. A pending save prevents duplicate
preparation and eviction of its entry. Serialization/queue failures clear the
pending flag and leave durability false, permitting a later retry.

`get_saved_player_availability(context, kind)` reads the exact account/league/
competition/season scope and validates team/player/scoring identity, source
metadata, integrity, completeness and original expiry. It runs through
`async_adapter_fetch` as a database-only getter, preserving cooperative HTTP.
The query includes relevant invalidation markers in the same bounded GET.
Malformed, expired, unknown or failed snapshots are misses, never proof of zero
funds, zero pending transfers or empty history. Valid live retrieval remains
usable while saving is pending; only a successful acknowledgement or validated
database restore is considered durable. Unacknowledged saves stay retryable on
subsequent demand and through the existing bounded queue.

`player_availability_invalidate(context, resources, pattern)` publishes separate
team, own-listing, all-summary or per-player invalidation markers in that namespace. Separate
rows prevent independent writers from dropping another target's invalidation.
Marker payloads contain `kind=player_availability_epoch`, `scope`, `applies_to`
and exact `invalidated_at`. Later sessions/restarts reject snapshots whose
verification began before the relevant marker. Account/league invalidations
expand only already-known interactive scopes, without enumerating source data.
The normal worker helper does not publish these interactive invalidations.

`invalidate_resources(..., notify_availability=TRUE)` retains its existing cache
behavior and additionally notifies this provider for affected facts. Internal
resource-wrapper reloads and the capacity engine's freshness repairs pass
`notify_availability=FALSE`, avoiding recursive invalidation. Native and worker
observations older than an invalidation cannot satisfy a new verification.
Historical transfer baselines continue through their existing durable,
context-specific incremental acquisition path.

## Timings and verification

The existing bounded `resource_timings()` sample ring accepts the fixed stages
`availability_team`, `availability_summary`, `availability_listings`, `availability_reuse`,
`availability_database`, `availability_queue`, `availability_prerequisites`,
`availability_transfers`, `availability_refresh` and `availability_actions`.
Samples contain only a stage and duration, never account/player IDs,
credentials, response bodies or query strings. Queue time is measured before
interactive HTTP dispatch. The acquisition engine records prerequisite,
transfer-candidate and freshness-repair stages without changing request budgets.

Focused checks: `test_player_availability_reuse.R`,
`test_player_availability_store.R`, the existing card freshness/recovery,
worker/manual-offer, configured-rule and transport tests, plus the actual-card
browser fixture and mandatory offline lifecycle harness. Timings are synthetic
local measurements; a first uncached read may still wait for required source
responses, complete history acquisition, unrelated already-running requests or
existing synchronous parsing. No source deadline is extended to hide that wait.

Internal helper groups are `player_availability_scope/id/epoch/invalidated` for
identity and invalidation signals, `player_availability_valid` for reuse gates,
`player_availability_encode/decode/json_async` for safe exact-value wire data,
and `player_availability_publish` for result-memory limits and context-safe
publication. Global invalidation signals are bounded to 256 known scopes and
are independent of individual reactive-session lifetimes.

Shared worker account/status/resource revision signals are created outside an
individual reactive-session domain. This lets another card/session continue
reading the same account after the originating session closes; account detach
and bounded worker snapshot cleanup retain their existing behavior.
