# My team

## Players not fit (2026-11-22)

**Players not fit (N)** follows **Leaving players** in My Team, spans the
transfer panels on desktop and stacks with them on mobile. The box shares the
transfer panel's border, row dividers, identity layout, club crest and position
badges. Each row shows the reported status and links to the existing player card.

`team_nonfit_roster()` selects the current saved roster rows with recognized
doubt, injury, suspension or unavailable codes and unique player IDs.
A player with a fit, blank, missing or unrecognized status is outside the
reported count. `team_nonfit_panel()` returns no box when there are no qualifying
rows. The saved roster's observation clock continues to apply. The display
projection reuses the existing roster state and only publishes changed markup.
The links use the same context-checked modal as transfer and offer links.

## Current and effective squad totals — 2026-11-14 Pacific

The position breakdown now presents the actual roster count and an adjacent effective purchase count. For example, 18 current players, two bought but not yet arrived and one leaving displays **Total: 18 Players · Effective: 19 Players**. Position badges and squad valuation still describe the actual roster. The count concerns pending transfer commitments, independently of the next-kickoff highlighting cutoff.

`effective_squad_count(roster, incoming_ids, outgoing_ids)` in `futmondo_functions.R` is the shared pure identity ledger for display and `calculate_team_acquisition_capacity()`. It removes outgoing IDs present in the roster and adds incoming IDs absent from it. Repeated transfer IDs do not double count, arrived players do not add another slot, and already-absent departures do not subtract a slot. Confirmed empty sets use `character(0)`; NULL, missing/blank identities, duplicate roster IDs or contradictory incoming/outgoing identities return `NA_integer_`.

`team_effective_squad_count(roster, transfers)` adapts the existing context-isolated `incoming()` resource to `list(count, status, detail)`. Complete `ok` or successful historical `observed` inbound/outbound records can supply this descriptive count. Loading stays pending; failed/partial roster or transfer evidence and absent identity columns stay unavailable rather than becoming zero. The tooltip explains the arithmetic, observed basis and purchase-time recheck. Strict purchase preflights still require fresh verified reconciliation and never accept this display state as authorization.

`players_in_teams_Server()` supplies a stable active-navigation-gated `effective_squad` projection to the existing shared player table. Source-clock-only publications preserve the card DOM. This adds no acquisition, refresh timer, persistence queue or API/database work: reusable successful observations continue to be persisted/reused by the existing durable roster and clause-transfer infrastructure. The derived count is cheap to reconstruct and is not a separately stored fact.


## Saved offer observations and inline transfer information — 2026-11-14 Pacific

`team_received_offers_state(roster_state)` separates successful stored observations from current-action freshness. Previously any expired worker roster inherited `stale`, creating a red “could not be verified / Refresh” error even when the acquisition succeeded. A dated `observed` state now requires successful worker acquisition, no parent/nested error or partial completeness, valid normalized `received_offers` rows, explicit `received_offers_status` (`ok`/`empty`), and a valid nonfuture source timestamp. Unknown global completeness is accepted only with this explicit offer parser proof, matching the worker’s existing per-resource contracts. Missing/malformed rows, failed/partial sources and future timestamps stay unverified. Attribute lookup is exact so an absent row attribute cannot partially match its timestamp metadata. Explicit old empty-roster acquisitions can reconstruct observed empty offers; new `get_players_from_team()` empty responses include the complete empty offer attributes for persistence/reuse.

`team_received_offers_panel(state, ns, source_note=NULL)` shows stored rows as “N observed” with their original clock, or “No offers observed” for an authoritative historical empty list. It does not claim current availability or change manual preflights. Snapshot data remains in the existing server-only database worker snapshots / offer-evidence store; no browser cache, schema, credentials or extra acquisition is introduced. New empty roster acquisitions enqueue one complete empty-offer observation through the existing bounded idempotent persistence queue, and subsequent reads reuse valid stored data. Restored display snapshots are not rewritten. Existing context isolation remains in effect. Failed/incomplete results retain red diagnostics.

The module publishes the observation time through a separate `received_offers_source_time` output. Its stable `offers_panel_display` projection preserves the mounted panel when only clocks change, and both projections are gated by active navigation. `clause_transfer_panel()` groups each player’s observation/overdue/timing details into one native disclosure beside the before/at/after-round label. Scoped two-column CSS keeps the icon to the right while expanded details occupy the row below; keyboard interaction and phone wrapping are preserved.

Verification: `test/test_offer_observation_contract.R` restores the actual worker codec, checks success/empty/legacy-empty and failed/partial/missing/future cases, and measures ten clock publications without extra panel renders or source/database writes (0.235s). Existing offer persistence (8 checks), received-offer UI (8), incoming/outgoing reconciliation (24 each), observation and disclosure tests pass. Chromium at 320/390/1440px verifies inline icons open/closed, keyboard toggles, no horizontal overflow, stable timestamp updates and zero I/O; heartbeats measured 67/67/66ms. Full offline lifecycle passes. Production inspection was read-only aggregate snapshot metadata: expired successful roster/offer snapshots had no associated job error, consistent with the inherited-stale root cause. Hosted acceptance after deployment remains pending.

My team displays the actual roster, observed player details, and explicit listing controls. Player selection opens the shared player card. Bulk listing remains a deliberate manual operation and must validate the current account, league, team, and market state.

The module no longer contains lineup forecasts, optimized XI selection, transfer simulations, generated trading advice, or automatic coach preferences.

`players_in_teams_UI(id)` creates the view. `players_in_teams_Server()` receives the current reactive authentication/league/team context and returns selected-player state. Context changes discard pending selection/action state. Shared resource invalidation refreshes affected data after acknowledged transactions.

## Incoming clause signings

The **Incoming players (N)** panel above the current roster shows the verified incoming count and its own status while checking. The squad table and capacity summary show current roster counts. Pending arrivals are outside the current roster used for value trends, lineup inputs and bulk listing actions.

Candidate purchases come from the existing shared pressroom transaction history. The existing player-summary response confirms `championship.clause.transferred`, the current owner and the authoritative `transferDate`; `data` supplies the player's name, role and club crest. No league-wide player scan is needed. The reconciliation helper now also returns presentation records: player, seller, purchase instant, scheduled arrival, observation instant and any specific timing issue. Existing capacity reservation outputs keep their strict fail-closed behavior, including overdue transfers.

The reported arrival is an absolute instant; presentation uses `Europe/Madrid` with CET/CEST. No fixed four-times-24-hour calculation, Monday cutoff, guessed processing offset or roster membership is inferred. The module receives the root's shared round catalog and compares arrival with `next_round_context()` using strict before/equal/after labels. A changed/postponed round start is consumed from that same catalog. Timing before the deadline does not guarantee lineup selection or scoring eligibility.

`clause_transfer_panel()` groups Incoming and Leaving separately under **Before round starts**, **At round start**, and **After round starts**, with one heading per populated group. The renderer preserves the order within each group and shows the exact Madrid-local time beside each player and their timing disclosure. A missing transfer date appears under **Timing unverified** when kickoff is known. When kickoff is unavailable, the schedule message accompanies a single ungrouped list of all players and their available details.

Previously confirmed incoming rows survive failed/stale observations, malformed dates and a history response that omits their purchase. A fresh roster/owner observation, explicit cancellation, or a newer confirmed sale resolves the entry. A passed scheduled arrival remains visible as awaiting source confirmation. Partial summary failures preserve the other successful rows and show the specific source problem.

Retrieval is demanded only while My Team is active, on stale navigation, explicit Refresh and acknowledged actions. Incoming and outgoing candidate summaries share one queue, at most two at once. Cards, preflight and these panels reuse the existing account/league/team/player-scoped raw summary cache (60 seconds). Safe clause observations also persist in the database and can be reused within their original source deadline; memory is a bounded accelerator. Existing roster and history observations are reused. Account/league/team changes discard UI state; generation guards reject old callbacks. In-flight slots remain reserved through a context change, preserving the two-read limit. There is no recurring API timer or additional login-blocking read, and no database migration.

Verification: `Rscript test/test_incoming_clause_signings.R` covers reconciliation, partial failures, history omission, confirmed resolution, DST/Madrid midnight, strict round timing, cached navigation, manual invalidation and callback isolation. Existing acquisition/deterministic regressions and the mandatory offline app lifecycle cover compatibility. Browser verification uses synthetic signings and no external requests; hosted behavior must be checked after deployment.


## Incoming-signing prerequisite recovery — 2026-11-05

`locker_resolve_parties(rows, pressroom, teams, players=data.frame(), previous=NULL)` can reuse already-verified player/buyer/seller IDs and historical club identity from the previous complete merged transaction baseline. Reuse requires a unique identical locker event ID and unchanged source payment timestamp, amount and party/player labels. This preserves paid, undelivered signings after a team rename or removal from the current player catalog, and avoids a redundant catalog lookup for known identities. Partial, ambiguous or revised evidence cannot supply that identity. Fresh source acquisition is still required; stale data cannot certify current incoming signings.

`incoming_clause_source_unverified()` centralizes the existing roster/history completeness gate. `incoming_clause_prerequisite_failure()` identifies the failing prerequisite and preserves safe failure categories/references in the state's `source_errors` field. Previously confirmed incoming rows remain visible while a prerequisite fails. Acquisition/manual-action preflights continue to reject stale, partial or unverified sources; no arrival date or identity is inferred to remove an error.


## Asking price and received offers — 2026-11-07

My Team now exposes one numeric **Selling by** column for the manager’s listing asking price, and **Offer** for the highest received offer. Raw listing-price aliases and the old In market badge stay hidden in this table, while remaining available to player-card actions. Unlisted players have no asking-price cell. Prices retain numeric sorting; other player tables retain their existing price columns.

**Incoming players** and **Leaving players** occupy two equal columns, stacking below 768px, with **Offers for your players** spanning the row below. The offers panel shows every observed bid in one sortable table. Rival and unknown bidders appear before **Liga Radar** system bids in the initial order. The From column uses a person icon for rivals and a robot icon for Liga Radar. Classification uses the observed bidder ID or the captured `userTeam.name`/`slug`. Missing or empty team identity denotes the system, while malformed nonempty identity stays unknown. Clicking a player with a bid ID opens the card with that bid selected. See [the offers display contract](squad_table_prices.md) for labels, formatting and mobile layout.

`normalize_received_offer()`, `merge_received_offers()` and `highest_received_offers()` retain the full normalized bid set as the `received_offers` attribute of the existing highest-per-player `get_roster_bids()` result and merged roster. Unknown highest amounts remain unknown. Both roster `market.bids` and supplemental roster-bid responses are reconciled by player and bid ID; no offer disappears merely because a system bid is higher. `team_received_offers_state()` consumes the exact shared roster state used by My Team and preserves loading, stale, partial and unavailable states. Only a verified empty result says no offers. Offer source timestamps/errors remain separate attributes.

Durable observations use the existing server-only evidence store through `persist_roster_offers()`/`read_saved_roster_offers()`, scoped to account, league, team, season and scoring context. Complete endpoint and merged-roster snapshots have separate keys so an empty supplemental endpoint cannot overwrite roster evidence. A fresh saved snapshot can be reused for at most 15 seconds from its original observation; failed writes remain retryable and manual actions/Refresh invalidate older saved observations. No authentication fields are stored and no database migration or polling timer is introduced.

Local verification: `test/test_team_received_offers.R` covers complete lower rival offers, source identity, escaped labels, canonical numeric asking prices, truthful states, responsive layout markup, exact-offer card selection, account/team context clearing and shared acquisition reuse. The synthetic loader measured three existing cold source reads, zero added reads for the offers panel, and zero warm reads. The existing player-data availability regression retains unknown/zero/negative amounts and explicit-empty behavior. Durable replay and browser/lifecycle evidence are recorded in `data_review_results.md`; live accounts and transactions are excluded.

## Leaving clause signings — 2026-11-10

**Leaving players (N)** mirrors incoming signings for roster players bought away through a release clause. Each row opens the shared player card and shows the authoritative `championship.clause.transferDate` as **Leaves**, formatted in Spain's CET/CEST, plus its strict before/at/after comparison with the next round. Players remain in the actual roster, current squad count/value and existing manual controls until source evidence confirms departure.

`outgoing_clause_empty()` defines presentation records with player identity, seller/buyer IDs, payment instant (`purchased_at`), `leaves_at`, `observed_at`, role/club/crest, `timing_error` and `overdue`. `pending_clause_candidates()` and `reconcile_pending_clause_transfers()` accept optional `previous_outgoing_records`. Reconciliation returns `outgoing_records` and independent `outgoing_status` alongside existing capacity/arrival outputs. Current ownership and explicit pending clause evidence identify leaving players; ordinary/cancelled sales are excluded. A wrong-player summary cannot certify a transfer or capacity. Failed, stale, malformed or overdue observations preserve confirmed rows; fresh roster departure, changed owner, explicit cancellation or a newer repurchase resolves them. A missing summary without clause evidence remains an incomplete check, never a verified zero.

`incoming_clause_source()` retains its incoming `records`/`status` contract and adds `outgoing_records`/`outgoing_status`/`outgoing_error`. Both directions share prerequisites, generations and the two-read queue. `clause_transfer_panel()` renders the incoming/outgoing wrappers; `clause_transfer_card_snapshot()` preserves observed current ownership through both card wrappers. Departure clicks use the existing card and explicit action preflights.

`clause_transfer_store.R` provides `persist_clause_transfer_summary(data,context,on_complete=NULL)`, `read_saved_clause_transfer_summary(context,now)`, `cached_clause_transfer_summary(context,now)`, `get_clause_transfer_summary(login,championship_id,user_team_id,player_id)` and `invalidate_saved_clause_transfers(context)`. Safe snapshots contain only player display identity and championship owner/clause fields. Existing private evidence storage isolates account, league, team, season, scoring version and player; payloads retain source/completeness metadata and the original 60-second deadline. Cold candidates check the database before the API; warm candidates use bounded memory without another database/API read. Failed or unacknowledged writes stay retryable through the existing persistence queue. Explicit Refresh and acknowledged actions invalidate older saved observations. Safe subsets never replace full player-card summaries, and tokens/passwords are excluded.

`clause_transfer_now()` supplies the wall clock separately from the asynchronous adapter's fixed source clock. Each database replay and source completion respects the original expiry; an expired completion gets at most one direct fresh summary check using its existing queue slot. A second expiry remains incomplete. Raw memory reuse checks both the original observation age and any absolute deadline.


### Empty roster rendering

My Team initializes a missing `market_inMarket` column with a logical vector sized to the observed roster. Explicitly empty source rosters remain zero rows through position/change/price transformations; source status and timestamps are retained. Pending roster acquisition continues through the existing shared-resource loading contract and is not converted into a successful empty response. No API, database, cache, timer or manual-action behavior changes. `test/test_empty_squad_module.R` exercises the actual module reactive and delayed resource completion.


## Departure eligibility and roster highlights — 2026-11-14

My Team gives roster players a reddish background when their observed clause departure is at or before the next scheduled round kickoff. Affected player names carry the departure explanation in their tooltip; the Leaving players panel retains the exact departure time and source status. Later departures are not highlighted. Missing dates or a missing next-round schedule do not produce invented eligibility. Known dates remain visible while later verification is incomplete; confirmed cancellation, departure or reacquisition clears them through the existing reconciler. Actual roster counts/value and manual market actions retain their existing source-based behavior.

`clause_departure_round_state(transfers, next_round)` projects `outgoing_records`/`outgoing_status` into sorted `excluded_ids`, `unknown_ids`, `complete`, `status` and `error`. It compares absolute timestamps inclusively at kickoff. Missing records, identities, dates or schedule and incomplete source states cannot certify completeness. The same helper serves Lineup and My Team. `players_in_teams_Server(..., clause_transfers_source=NULL)` accepts the root session's shared transfer source; standalone instances retain their existing source. The root activates one existing bounded queue while either My Team or Lineup is visible, preserving context isolation and the existing database-first clause snapshot reuse. No additional storage or acquisition is introduced for the row highlight.


## Shared roster observation note — 2026-11-15

Roster and transaction observation timestamps now appear only in the global
Background sync note above My Team, Market (Today), and All Players. Incoming
and Leaving retain loading, source failures, schedule failures and individual
player timing diagnostics, without repeating the roster/history note.

`fm_worker_session(session, login, leagues, source_note=function()"")` accepts an
optional reactive text callback and renders it through the independent
`worker_roster_source_times` output. Root `roster_note_active`,
`roster_note_readers` and `roster_source_note` project original source clocks
from the existing scoped factual adapters on those three pages, only while
Background sync is enabled. Missing/future timestamps are omitted. Account,
league, team, competition, season and scoring isolation remains in the adapters;
navigation and logout clear irrelevant clocks. `incoming_clause_panel()`,
`outgoing_clause_panel()` and `clause_transfer_panel()` no longer accept the
obsolete `source_note` argument or mount duplicate clock outputs. Per-player
observation details remain available beside the transfer timing label.

The note reuses database-persisted worker snapshots through existing bounded
memory replay and the asynchronous database poll. A cold page can subscribe to
the two existing roster/history resources; it does not request clause summaries,
enqueue source acquisition, write restored observations, or change acquisition
cadence, persistence acknowledgement, freshness deadlines or manual preflights.
Clock-only updates change text without replacing the note or transfer panels.
No new dependency, environment variable, migration or product capability is added.

## Team value and daily trend — 2026-11-25

My Team's first card shows **Team value**, followed by a smaller **Total trend**
row. The row places the signed euro change to the left of two stacked counts
with a green up arrow and a red down arrow. Screen readers get the direction
of each count. Players with a zero change contribute zero to the sum and leave
both directional counts unchanged.

`players_in_teams_Server(..., team_info_source = NULL)` receives the root's
existing scoped team-information state. `team_value_summary()` uses the native
`teamValue` field or its saved `team_value` alias, with the complete roster's
sum of market values as a fallback. The trend uses the sum of the signed euro
`change` fields on the same saved roster used for the player table and offers.
It measures the current roster's 24-hour player price changes. An explicitly
empty roster yields zero trend and zero counts. Missing changes or incomplete
roster coverage leave the trend and counts unavailable. Stale values are
identified as last saved. [Captured team information](har_api_reference.md#12-post-1userteaminformation),
[captured roster](har_api_reference.md#15-post-1userteamroster).

`team_value_summary_panel()` renders the two rows. The summary reuses the existing
database-backed team and roster observations, independently of the market and
incoming-transfer panels. Its reactive projection publishes only when a displayed
amount, count or status changes. It adds no reads, writes, timers or cache entries.
