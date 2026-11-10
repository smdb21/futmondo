# Player/round score matrix

The canonical observations remain in `player_match_observations`. Every row identifies a player, championship, season, scoring version, round, source score status, and observation time. Corrections are retained as revisions rather than overwriting the historical record.

`player_round_points_latest` projects the latest revision for each complete identity key. A latest NULL score stays unknown; the view does not resurrect an older numeric value. It uses the existing `player_round_context_revision` index. A second index or a physically wide table is unnecessary. The view is `security_invoker=true`, has no PUBLIC/anon/authenticated grants, and grants SELECT only to the server service role. Credentials remain on the R server. This follows the [Supabase view/RLS guidance](https://supabase.com/docs/guides/database/postgres/row-level-security#views).

`read_league_round_points(championship_id, max_rows=50000L)` loads the league's latest observations as one logical batch. REST pagination uses deterministic player/season/scoring/round order, 1,000-row requested pages, and actual returned row counts. Short server pages are followed until an empty page; the maximum-row cap produces explicit partial coverage. N is not a query parameter. Callers must preserve the season and scoring scopes when calculating averages. The result is a long-form matrix that can be indexed or pivoted locally without additional database queries.

## Applied database change

Migration `20261030075801_player_round_points_latest.sql` was applied to the configured Supabase project through the connector. Existing observations were not rewritten. The live source table contained seven observations for one player at inspection time, so population of the remaining matrix requires Futmondo retrieval; the view alone does not create missing scores.

Live known-answer tests inserted synthetic revisions inside a transaction and rolled back: latest revision selection, zero, negative, latest NULL, and season/scoring/league separation passed, with zero retained test rows. Grant checks confirmed server SELECT and denied anon/authenticated SELECT. The actual query used `player_round_context_revision`, returning the seven existing rows in 1.039 ms execution time (not representative of larger hosted loads). Independent SQL validation using PGlite also passed. Existing security-advisor findings concern unrelated pre-existing objects; the new view has no browser-role grants.

The application reader's independent fake-PostgREST test returned all 3,020 rows despite a 700-row server page cap; capped and empty results were reported correctly. Direct R-to-live-REST validation was not available because this workspace lacks database credentials; the configured Supabase connector was used for live SQL verification. The hosted application must use its existing server credentials.

Rollback: restore the preceding application reader, then drop only `public.player_round_points_latest`. Source observations and their existing index remain intact. No backfill or live Futmondo account access has been performed from this workspace.

## Shared database-first acquisition — 2026-10-30

The current table path is `read_league_round_dataset(championship_id, max_rows=50000L)`. It reads `player_round_dataset` once per retained root-session league/team dataset, with deterministic REST pagination. The view combines latest score revisions with latest successful acquisition checks, including players whose returned history was explicitly empty. `read_league_round_points()` remains available for compatibility and the focused score-reader checks. N never enters either query.

`new_shared_round_points_controller()` is owned by the root Shiny session. The Today, My Team, All Players, and other table module namespaces register their active context and visible IDs with this controller; they do not create separate league loaders. Each table selects its players from a shared published matrix and calculates its requested N locally. An inactive page does not initiate player reads. At most two retained league/team entries share a 50,000-row stored/live score budget; old inactive entries are evicted when asynchronous reads or live observations reach that budget. Catalog acquisition is capped at 2,000 IDs; only normalized observations are retained. Logout and account replacement clear private state before reading dependent league/team reactives.

Saved successful checks and legacy score observations remain reusable without an age expiry. Observation times order revisions; they do not expire historical facts. Reuse still requires exact championship/season/scoring context, a valid timestamp and no corrupt timestamp more than 60 seconds in the future. Known and unknown seasons remain separate. A successful source check acknowledges sparse or empty history, so missing appearances or NULL scores do not cause repeated acquisition. Legacy/mapped rows without a successful full-history check satisfy any requested N when all represented round rows already exist; genuinely absent demanded rounds can trigger one targeted acquisition for that visible player. No startup live probe is required for agreeing saved data. An explicitly unknown source context cannot authorize use of a different known context.

Initial acquisition visits absent catalog histories and requested missing windows, with visible players first and two API requests in flight. Elapsed time alone never requeues the catalog. A successful response is acquired once per active-session dataset, including an empty response. Transient failed acquisitions receive bounded active-session recovery as described below; exhausted or permanent failures remain explicit. Manual Refresh forces only the current table's player IDs, including when primary loading temporarily hides optional content. It does not invalidate every catalog player's history. Acquisition pauses at the existing write-queue backpressure thresholds and resumes from write-completion signals. Navigation makes outstanding callbacks obsolete, retains completed work, and starts no new player requests until an active table returns.

Migration `20261030234619_player_round_dataset.sql` adds immutable `player_round_history_checks` rows keyed by championship/player/season/scoring/checked_at and the server-only `security_invoker` union view. Latest-by-timestamp selection prevents a late older check from replacing a newer check. These are factual league observations shared across authorized server-side league reads; private runtime resources remain account-specific. No historical scores or check markers were manufactured from existing observations. See [production application and rollback evidence](round_dataset_migration_20261030.md).

`get_player_summary()` retains `points_received`; `recent_points_summary_valid()` rejects a missing top-level points field, malformed container/round/score, stale response, or error rather than treating it as an empty history. An explicit empty points container means a successful empty history; an identified round with an omitted or NULL score remains a recorded round with unavailable points, preserving all other rounds. `persist_recent_points_snapshot()` submits an ordered bounded job: score chunks first, then a success-check marker. `defer_persistence(..., on_complete=...)` acknowledges only after every operation succeeds; an earlier failed write prevents the marker, and queued/active cancellation settles all coalesced listeners as unsuccessful. Cancellation may remain in the existing account-level persistence diagnostics; the table's saving message tracks this dataset's own outcomes. A marker proves a successful source check, not that every player appeared in every round.

Shared matrix assembly/publication is bounded to five-second intervals, with an immediate initial snapshot or newly settled visible group. Off-table completions update progress without repeatedly assembling or rebuilding the visible table. Switching pages can immediately publish already acquired scores for the new visible IDs. Module wrappers update their local history only when their player subset or the league-round anchor changes.

Local acceptance passed: 22 shared-controller groups; four union-reader groups; nine existing observation-recording groups; six existing matrix/arithmetic groups; and independent source/write-order validation. The 500-player × 38-round N=8 calculation took 0.056 seconds in the focused fixture. These are local deterministic checks, not production latency measurements. Hosted deployment, authentic new API response validation, and a complete live backfill remain separate from the applied database migration.


## Saving source last-five snapshots — 2026-10-30

The user confirmed that `average.fitness` represents consecutive league rounds, oldest to newest: `2,5,12,12,6` has 6 in its latest represented round. `recent_points_fitness_rows()` carries only each player's vector, original source `observed_at`, and available context from the active tables and already-loaded catalog (at most 2,000 players). It does not manufacture a timestamp when that source time is absent. Catalog and table rows from different observation timestamps do not share an anchor.

A complete agreeing saved window avoids both writes and a source probe. Database maximum rounds are used only for this work-avoidance comparison; they never authorize new round assignments. When newer fitness evidence conflicts with saved history or supplies missing rounds, it overrides durable reuse only for the affected players. The existing two-slot queue first requests one affected qualifying player's explicit round history. A newer acknowledged full-history check or a complete newer explicit-round window supersedes an older conflicting vector, avoiding rollback or repeated requests. `recent_points_fitness_anchor()` requires exact player/league/team/account/season/scoring context, a probe observation time at least as new as the snapshot, and at least two distinct known scores. Exactly one consecutive window in the returned source history must match every known position, and that window must end at the latest explicitly reported source round. A repeated pattern, old-only match, conflicting context, empty/failed probe, or sole constant vector cannot establish an anchor. Neither upcoming rounds, current-round subtraction, nor the database maximum supplies an anchor. No authentic HAR or live account response was verified for this change.

`recent_points_fitness_observations()` then maps the same snapshot's vectors to explicit rounds. The vector length must equal `min(5, latest_round)`; a shorter vector is accepted only for that early-season prefix. Zero, negative points, and unavailable slots keep their positions; unknown slots are never compressed or replaced by zero. For a verified round-10 anchor the example maps to rounds 6–10, last-four mean 8.75 and last-three mean 10. Unknown finality stays unknown.

`recent_points_fitness_changes()` skips unchanged values and equal/newer existing revisions, including NULL corrections, so a new snapshot does not downgrade known finality. Changed observations join the shared matrix and enter observation-only `supabase_post_direct` jobs of at most 100 rows with the existing full identity conflict key. Fitness writes never create `player_round_history_checks`; only an actual successful summary endpoint check follows the existing score-first, marker-last acknowledgement path. Transient observation-write failures receive the bounded persistence retry described in background_worker.md. Exhausted or permanent failures remain available for explicit Refresh. Pending and failed buffers deduplicate full player/context/round keys and are each capped at 10,000 rows. The existing owner/global queue budgets and 50,000 retained-score budget remain in force.

Mapped fitness supplies requests up to five rounds without a per-player source replay. The `required_rounds` registration field preserves older-history acquisition for visible N>5 demand; a five-score snapshot is never treated as a full-history fetch. Existing wider matrix coverage is reused regardless of age, and acknowledged sparse full-history responses are not fetched again just to fill genuine source gaps. Switching N within saved coverage is local. Anchor attempts and successes retain at most 32 snapshot entries per root dataset, canceled probes can resume on navigation, and the existing publication throttle avoids per-player table rebuilding. The dynamic `Avg Last N` column appears immediately after Name. All shared player tables default to descending average order with unavailable values last; users can change the sort. The column retains numeric data and renders only a rounded number or an empty missing-value cell, with no coverage or reported labels. Installed reactable infers its numeric type from that data (`colDef` has no `type` argument); `sortNALast=TRUE` controls missing-value order.


## Durable history and targeted correction — 2026-11-02

The 24-hour/one-hour age-based history reuse policy is removed. A new session reads the retained matrix and successful acquisition markers once, then joins and averages it locally. Current source fitness differences can trigger one snapshot anchor and bounded repairs; failed/ambiguous anchors permit targeted explicit-round retrieval only for affected players. A failed read preserves existing scores and reports the failure. A source response is still persisted score-first/check-last, and failed writes cannot create acquisition evidence. Manual Refresh retains its explicit current-table scope; no timer, polling, new migration or additional database read is introduced.

Historical corrections outside the last-five source vector require a labelled source response or explicit Refresh; no reliable source-wide revision token is available in this path. An unchanged/repeated score pattern alone cannot prove a round shift, and upcoming schedules are not used to guess its anchor. Explicit full-history checks and missing-window requests remain available to establish labelled rounds. Production database contents and hosted persistence were not inspected for this change.


## Acquisition completion and recovery — 2026-11-05

`new_shared_round_points_controller(root, publish_interval=5, retry_delays=c(1,3), retry_budget=32L)` keeps the two acquisition slots and existing persistence backpressure. After the transport's bounded retry, only connection, transient HTTP or capacity failures can re-enter those slots, at most twice per player and at most 32 extra acquisitions per retained context/refresh pass. One cancellable timer wakes the controller at the next retry deadline. Navigation, logout and context changes prevent obsolete callbacks from publishing. A fitness snapshot keeps its held players while its anchor probe is retryable, avoiding an unnecessary whole-catalog fallback during a brief failure.

Both rejected promises and synchronous acquisition exceptions settle pending/loading state. Explicit Refresh reacquires only failed initial matrix/lineup/catalog reads while reusing successful initial sources, and invalidates obsolete per-player attempt tokens. Season/scoring changes cannot count incompatible observations as completed; a known context rejects mismatched late responses. Stale summaries retain their original sanitized failure category/reference. A successful source payload is checked for matching identity/context before merging, then saved through the score-first/marker-last path. Missing row scores stay unavailable; absent top-level history, invalid rounds and malformed scores cannot acquire a success marker.

Progress exposes `completed`, `remaining`, `pending`, `retrying`, `failed`, `failure_details` and `initial_errors`, separately from `saving` and `save_error`. Completed counts include only successfully acquired or reusable histories; failed and fitness-held players never masquerade as loaded. Persistent failures retain safe diagnostic categories/references, while previously acquired values remain visible. Counts do not claim that every round has a numeric score. The existing publication throttle and bounded score buffers remain in force.


`read_league_round_dataset()` preserves the underlying rows' `fetch_status` and sanitized `fetch_error` on its returned list. An incremental database failure can therefore retain previously saved points/checks while reporting the actual stale source failure; a later successful read clears that metadata. This adds no query or row copy beyond the existing list assembly.


## Player-card responsiveness during saves (2026-11-05)

The shared controller caches IDs with valid stored observations, round-window coverage for each requested N, and the latest observed round. These caches belong to the existing account/league/team entry, carry the season/scoring scope, and keep ID sets restricted to the current requested/catalog players (at most 2,000). A changed player set invalidates those ID projections. Replacing the saved matrix invalidates these projections and fitness reconciliation; merging new live points invalidates window coverage. Advancing the latest round also rechecks fitness immediately, before persistence finishes. A different N gets its own projection (the UI permits 1–20); at most 20 keys are retained even for fractional client input, and season/scoring changes rebuild the scoped projections. Stored-ID validity also expires when an excluded future-dated observation reaches the existing sixty-second eligibility boundary. No clock timer is added. Check markers remain read from current state, so acknowledgement, missing scores, sparse histories and failed saves preserve their existing semantics.

A successful save only invalidates fitness reconciliation when that player's vector still needs reconciliation. Already resolved fitness vectors do not change merely because their observations have finished saving. This removes repeated scans of the full league on every decrement of the pending-save counter. Save failures, bounded acquisition/backpressure, explicit retries, context isolation and score-first/check-last persistence remain in effect. No additional requests, timers, workers, packages or mobile layout elements are introduced.

### Clause purchases retain historical data and the table — 2026-11-05

`round_history_refresh_trigger(session)` is shared by the root session and advances only from the explicit global Refresh control. Table history clients consume this correction revision rather than the general market-mutation revision. The card's own Refresh history button also remains explicit; its selected-player summary still refreshes after mutations for ownership/bid facts. New or missing scores and changed fitness vectors continue through the existing targeted controller checks. A successful purchase does not clear successful acquisition markers, live scores, attempted reads or pending save acknowledgements.

Same-context resource refreshes retain the previous display data with loading status and stale freshness, replacing it when the fresh result arrives. Expired table data also stays visible during one asynchronous refresh; repeated reactive reads do not enqueue further refreshes while it is loading or stale. Context changes discard the previous data, and acquisition actions retain their loading/freshness preflight gates. No recurring polling or extra history database read is added.


## Source-window averages without guessed round IDs — 2026-11-06

`add_recent_points_average()` now uses a complete trailing N `average.fitness` sequence for N≤5 when explicit numbered history cannot provide that window. Source order and unavailable positions are retained. This is a local reported average, not a numbered observation: an unresolved anchor still cannot write inferred rounds or create a successful history check. Complete numbered history remains preferred, and the N=5 scalar average is only a final fallback. N>5 continues to demand and reuse actual numbered history; the eight-round regression includes zero and negative values and excludes the older two rounds of a ten-round fixture. Historical acquisition, save acknowledgement, context isolation and bounded request scheduling are unchanged.


### Wider recent-round demand — 2026-11-06

A durable `scores` or `empty` check proves that a source request succeeded at its original time. It does not prove that its stored rows cover a later request for eight rounds. For active table clients requesting N>5, `availability_for()` compares the demanded numbered window with the stored/live matrix even when an old check exists. Only missing visible IDs enter the existing two-slot acquisition queue; already covered players and offscreen catalog IDs remain reusable. If the same session previously fetched only a short window, a wider request can recheck that player once. A successful wider source response then prevents further automatic replay for N changes or navigation in that session. A genuinely sparse response stays unavailable rather than inventing older scores.

`recent_points_covered_players()` now requires the league's latest labelled round to be at least N; a seven-round season cannot report complete eight-round coverage. Pending wider demand no longer counts as a completed player. The visible completion signature resets when the targeted request starts so newly retrieved scores publish immediately when all affected visible requests settle. Source observations still persist before the success check; a fresh session reuses all eight recorded rounds without a source replay. Explicit Refresh remains available for later corrections.

The existing check table does not store the requested N or a source revision token. A fresh session may recheck an old sparse/empty marker when it asks for a wider missing window; bounded in-session acquisition and completed-window reuse prevent a request loop. Current captures establish that player summary responses can return seven labelled rounds without an N parameter, but they do not prove any specific user's current source has eight. If the authoritative response has fewer than eight recorded scores, the cell stays blank.


## Database base URL normalization — 2026-11-10

`get_sb_url()` selects the current `supabase_project_url`, retaining the existing
`SB_URL` fallback when the environment value is empty. It trims surrounding
whitespace and trailing slashes, then removes terminal `/rest/v1` segments. The
result is the project base URL; existing synchronous reads/writes, asynchronous
reads and queued score/check writes append `/rest/v1/<table>` exactly once. Custom
gateway prefixes, HTTP loopback fixtures and dynamic environment lookup remain
supported. The getter neither logs configuration nor changes the server-side key.

Live database logs established HTTP 404 with `PGRST125` for
`GET /rest/v1/rest/v1/player_round_dataset` and
`POST /rest/v1/rest/v1/player_match_observations`. The required view/tables and
server-role privileges exist; correctly addressed reads/writes have successful
responses. The duplicated API prefix prevents the initial matrix load and the
score writes that must precede a success marker, explaining the combined source,
saving and incomplete-history warning. `FM-HTTP-000003` is a process-local failure
reference, not a stable HTTP status or a missing-round identifier.

This corrects acquisition and persistence addressing without manufacturing missing
rounds or acknowledging failed writes. Existing context, source timestamp,
completeness, bounded active-session acquisition, durable database reuse and
score-first/check-last contracts remain in force. Deploy the code or configure the
project base URL and restart the hosted R process; existing failed requests do not
become successful merely because the source code changed. Hosted recovery requires
an active history load to retrieve and persist anything that was not previously
saved. No unattended backfill is introduced.
