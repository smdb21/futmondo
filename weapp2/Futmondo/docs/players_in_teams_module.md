# My team

My team displays the actual roster, observed player details, and explicit listing controls. Player selection opens the shared player card. Bulk listing remains a deliberate manual operation and must validate the current account, league, team, and market state.

The module no longer contains lineup forecasts, optimized XI selection, transfer simulations, generated trading advice, or automatic coach preferences.

`players_in_teams_UI(id)` creates the view. `players_in_teams_Server()` receives the current reactive authentication/league/team context and returns selected-player state. Context changes discard pending selection/action state. Shared resource invalidation refreshes affected data after acknowledged transactions.

## Incoming clause signings

A separate **Incoming players (N)** panel appears above the current roster. The squad-value subtitle also reports current and incoming counts separately (for example, “18 players in your squad · 2 incoming”). Initial verification says “checking incoming”; it never turns an unfinished check into a verified zero. Incoming players are excluded from current squad valuation/counts, lineup inputs and bulk listing actions.

Candidate purchases come from the existing shared pressroom transaction history. The existing player-summary response confirms `championship.clause.transferred`, the current owner and the authoritative `transferDate`; `data` supplies the player's name, role and club crest. No league-wide player scan is needed. The reconciliation helper now also returns presentation records: player, seller, purchase instant, scheduled arrival, observation instant and any specific timing issue. Existing capacity reservation outputs keep their strict fail-closed behavior, including overdue transfers.

The reported arrival is an absolute instant; presentation uses `Europe/Madrid` with CET/CEST. No fixed four-times-24-hour calculation, Monday cutoff, guessed processing offset or roster membership is inferred. The module receives the root's shared round catalog and compares arrival with `next_round_context()` using strict before/equal/after labels. A changed/postponed round start is consumed from that same catalog. Timing before the deadline does not guarantee lineup selection or scoring eligibility.

Previously confirmed incoming rows survive failed/stale observations, malformed dates and a history response that omits their purchase. A fresh roster/owner observation, explicit cancellation, or a newer confirmed sale resolves the entry. A passed scheduled arrival remains visible as awaiting source confirmation. Partial summary failures preserve the other successful rows and show the specific source problem.

Retrieval is demanded only while My Team is active, on stale navigation, explicit Refresh and acknowledged actions. Only incoming candidate summaries are queued, at most two at once; cards, preflight and this panel reuse the existing account/league/team/player-scoped raw summary cache (60 seconds). Existing roster and history caches are reused. Account/league/team changes discard UI state; generation guards reject old callbacks. In-flight slots remain reserved through a context change, preserving the two-read limit. There is no recurring API timer or additional login-blocking read, and no database migration.

Verification: `Rscript test/test_incoming_clause_signings.R` covers reconciliation, partial failures, history omission, confirmed resolution, DST/Madrid midnight, strict round timing, cached navigation, manual invalidation and callback isolation. Existing acquisition/deterministic regressions and the mandatory offline app lifecycle cover compatibility. Browser verification uses synthetic signings and no external requests; hosted behavior must be checked after deployment.


## Incoming-signing prerequisite recovery — 2026-11-05

`locker_resolve_parties(rows, pressroom, teams, players=data.frame(), previous=NULL)` can reuse already-verified player/buyer/seller IDs and historical club identity from the previous complete merged transaction baseline. Reuse requires a unique identical locker event ID and unchanged source payment timestamp, amount and party/player labels. This preserves paid, undelivered signings after a team rename or removal from the current player catalog, and avoids a redundant catalog lookup for known identities. Partial, ambiguous or revised evidence cannot supply that identity. Fresh source acquisition is still required; stale data cannot certify current incoming signings.

`incoming_clause_source_unverified()` centralizes the existing roster/history completeness gate. `incoming_clause_prerequisite_failure()` identifies the failing prerequisite and preserves safe failure categories/references in the state's `source_errors` field. Previously confirmed incoming rows remain visible while a prerequisite fails. Acquisition/manual-action preflights continue to reject stale, partial or unverified sources; no arrival date or identity is inferred to remove an error.


## Asking price and received offers — 2026-11-07

My Team now exposes one numeric **Selling by** column for the manager’s listing asking price, and **Offer** for the highest received offer. Raw listing-price aliases and the old In market badge stay hidden in this table, while remaining available to player-card actions. Unlisted players have no asking-price cell. Prices retain numeric sorting; other player tables retain their existing price columns.

**Offers for your players** sits beside **Incoming players** in two equal columns, stacking below 768px. It lists every observed received offer, including rival bids lower than the Futmondo offer. A highlighted person icon and **Rival · team name** distinguish rivals from the robot icon and **Futmondo system** label. Classification uses observed bidder ID or the captured `userTeam.name`/`slug`; missing or empty team identity denotes the system, while malformed nonempty identity stays unknown. Amounts are never replaced with zero. Clicking a bid with a source ID opens the existing player card with that exact bid selected. Existing confirmation/context and manual-action handling is reused.

`normalize_received_offer()`, `merge_received_offers()` and `highest_received_offers()` retain the full normalized bid set as the `received_offers` attribute of the existing highest-per-player `get_roster_bids()` result and merged roster. Unknown highest amounts remain unknown. Both roster `market.bids` and supplemental roster-bid responses are reconciled by player and bid ID; no offer disappears merely because a system bid is higher. `team_received_offers_state()` consumes the exact shared roster state used by My Team and preserves loading, stale, partial and unavailable states. Only a verified empty result says no offers. Offer source timestamps/errors remain separate attributes.

Durable observations use the existing server-only evidence store through `persist_roster_offers()`/`read_saved_roster_offers()`, scoped to account, league, team, season and scoring context. Complete endpoint and merged-roster snapshots have separate keys so an empty supplemental endpoint cannot overwrite roster evidence. A fresh saved snapshot can be reused for at most 15 seconds from its original observation; failed writes remain retryable and manual actions/Refresh invalidate older saved observations. No authentication fields are stored and no database migration or polling timer is introduced.

Local verification: `test/test_team_received_offers.R` covers complete lower rival offers, source identity, escaped labels, canonical numeric asking prices, truthful states, responsive layout markup, exact-offer card selection, account/team context clearing and shared acquisition reuse. The synthetic loader measured three existing cold source reads, zero added reads for the offers panel, and zero warm reads. The existing player-data availability regression retains unknown/zero/negative amounts and explicit-empty behavior. Durable replay and browser/lifecycle evidence are recorded in `data_review_results.md`; live accounts and transactions are excluded.
