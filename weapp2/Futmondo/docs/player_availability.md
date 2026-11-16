# Interactive player availability

## Session reuse and recent table facts — 2026-11-15 Pacific

The owner's final interaction contract supersedes the expiry-driven card behavior
below: a successful team/target check is sufficient for subsequent selections in
the same browser session and exact account/league/team/season/scoring scope.
`player_availability_source()` now returns `session_verified` after an acquisition
was valid on completion. Card display reuses that result across source expiry,
reopening, switching players and background publications. Explicit Refresh,
affected-resource invalidation, context changes and session end still reset it.
Failed/partial acquisitions never receive this marker. The existing bounded
registry and successful database persistence/acknowledgement queue remain in use;
a session marker is never persisted as transaction authorization. Later sessions
must validate the stored source's original freshness and completeness again.
Unacknowledged saves remain retryable while eligible and through the durable queue.

`player_card_recent_table_facts(player, now)` recognizes explicit known ownership
and market flags with a nonfuture observation younger than 60 seconds, matching
the existing target-summary lifetime. Missing timestamps/flags are unknown.
`championship_market_prices()` retains `availability_market_observed_at` and
`availability_complete`: a joined market observation cannot borrow a newer
catalog timestamp, and failed/partial joins cannot qualify. Existing table-context
isolation protects the captured row. Recent facts admit the card/editor controls
without a team-capacity acquisition or a wait for optional points/history.
Old/unknown table facts use the bounded check once, then session reuse applies.
Player summaries still load when needed for history, independently of the card's
availability presentation; they share pending/completed requests per player.

The session marker admits an **editor**, not a financial action. Actual-time
source deadlines and `evaluate_acquisition_preflight()` are unchanged. Opening an
expired/unverified market editor joins one asynchronous current check; Submit is
disabled until it succeeds. Direct-owner/clause confirmations retain their
current preflights, and every submission keeps its independent final checks,
immutable action context and uncertain-response policy. An expired session result
cannot authorize a purchase. Ordinary display expiry adds no Retry control,
source acquisition, timer or polling. Genuine initial failures retain Retry.

Cold native acquisition now settles history/roster/transfer candidates before the
short-lived funds/offers reads, reusing existing source caches on first demand.
This avoids immediately discarding successful early financial reads behind slow
history. The two finite repair passes use a five-second refresh margin without
extending any deadline. `verification` additionally exposes `completed_at` and
`generation` so an initially unusable live completion can receive the existing
single repair in worker mode; later ordinary expiry and explicit failed retries
cannot create a loop. A successful session check never needs that repair merely
because a target's history finished later.


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

Opening or switching cards joins pending team verification or reuses a successful
session result. Errors settle until explicit retry; unrelated metadata redraws and
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
its effective 20-second market lifetime. Team verification uses the same live
asynchronous capacity calculation. Optional lineup/profile/match/valuation
requests wait for availability admission, not completion. Independent transport
limits, reserved interactive capacity and bounded freshness repairs are retained.

Offer dialogs join the card's pending verification. Opening an expired/failed
editor explicitly retries that same provider rather than starting a separate
live check. An initial acquisition that never produced usable facts can request one repair;
that repair updates the existing shared entry. A failed/expired target summary
keeps acquisition blocked and Retry refreshes just the failed/expired sources. Worker expiry publications and
points-only redraws do not initiate that repair. Original team source deadlines
(including the at-most-15-second composite deadline) and summary's 60-second
source lifetime are unchanged. A first card opened after an explicit refresh
cannot restore an observation from before that refresh.

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
