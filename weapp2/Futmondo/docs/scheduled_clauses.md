# Scheduled clause purchases

## Manual offer dispatch

Market bids and direct owner offers from the player card send the native request without refreshing Futmondo data first. With scheduling enabled, the service-role RPC `clause_dispatch_manual_offer` uses the existing team lock and writes one dispatched manual job and one attempt. It protects outstanding local commitments across sessions and processes. While there are reserved funds or slots, it compares saved capacity using the original source deadline. Incomplete or expired capacity can then block dispatch with a specific message. With no local commitments, league restrictions are handled by the native response. Scheduling-disabled installations send the one asynchronous request directly.

Futmondo acknowledgement releases the popup immediately. Durable settlement proceeds separately. The account's saved player, market and balance results are refreshed after a native rejection or uncertain response. Existing observation-based reconciliation releases held funds and slots when authoritative evidence confirms the outcome. The request is never automatically resent. Keep the existing live verification for fixed-price clause purchases and bid modifications. [Database functions and permissions](https://supabase.com/docs/guides/database/functions).

## Clause and bid modification verification (2026-11-19)

An asynchronous manual confirmation can spend the first cash observation's validity window waiting for transfer history. If the live check returns an expired deadline, `fm_clause_execute_async()` starts one more live verification before creating a reservation. Current inputs are reused through the existing resource layer and expired facts are read again. The normal preflight runs on the result, and an expired or unaffordable result prevents admission. SQL admission and dispatch still enforce freshness, context, funds and capacity. A source write or uncertain response cannot trigger this recheck. Before dispatch, the log identifies the mode and failing stage using fixed labels without private payloads.

Two successful captured market offers in `app.futmondo.com.har` identify the existing `POST /1/market/bid` request. The query contains `championshipId`, `userteamId`, `player_id`, `player_slug`, `price` and `isClause: false`. The empty root `answer` is a JSON object and the response code is `api.general.ok`. `build_market_bid_payload()` preserves that object in both the direct and asynchronous callers.

Implemented locally on 2026-11-15 under the owner's explicit scope exception. Deployment and native closed-rejection evidence remain separate acceptance steps. This authorizes selected one-shot purchases, not general trading automation.

## Product behavior

Players in Championship passes `allow_scheduled_clauses=TRUE` through the shared table to the selected-player module. Other callers default to false. A verified rival owner, finite positive clause price, explicit untransferred state and parseable future `clause.date` expose scheduling. Unknown facts never become a free player, zero price or open clause. `unlockDate`/delivery dates do not replace the existing published-opening mapping.

Confirmation records the account/league/team/player/competition/season/scoring scope, original rival, price cap and consent version. It warns that execution is an attempt, success is not guaranteed, and borrowing may produce negative cash. The job reserves one offer slot and its cap immediately after database acknowledgement. Lower execution prices are accepted, higher prices cancel. The service honors current roster capacity, outstanding commitments, API bid limits and configured borrowing.

A verified extension by the original rival moves the opening and five-minute execution window. There is no maximum date horizon. Existing factual-worker observations update schedules without new source acquisition; independently enrolled purchases otherwise recheck near their saved opening. Early manual unlocks are not continuously watched. An already-observed earlier opening can advance the attempt. Another rival's verified transfer or paid clause cancels a pending job, including before delivery. Source acquisition failure is not proof of a transfer.

The card supports cancellation before dispatch. The balance disclosure lists pending schedules and dates, amounts and status, plus account-wide pending cancellation/revocation. A dispatched purchase cannot be unsent. Durable accepted/uncertain results retain reservations until matched source evidence and current financial observations establish their accounting. A stale displayed balance retains the accepted purchase amount until that view's own source timestamp is newer than dispatch. The existing official-offer calculation remains `max(withheld, official offers)`; scheduled amounts are added separately.

## R interfaces and UI lifetime

`clause_scheduler.R` owns `fm_clause_*` interfaces and `get_scheduled_clause_check(login, championship_id, user_team_id, player_id, require_clause=FALSE)`. The check reuses the actual R acquisition engine and source parsers, validates current membership/team and contextual identity, and returns a bounded credential-free JSON-compatible object:

- `context`: account, league, buying team, player, competition, season and scoring version.
- `player`: canonical ID/slug/name, owner ID and verification flag, clause price, explicit transfer state and UTC opening.
- `roster`, `funds`, `outstanding`: existing capacity-engine contracts; `offers` contains own player/price pairs.
- `owned_ids`, source `transactions` for reconciliation, `checked_at`, original `financial_observed_at`, and the shortest original `valid_until`.

The selected-player scheduling confirmation runs source verification through the existing asynchronous read adapter, then submits an asynchronous durable admission RPC. Context changes discard unsubmitted confirmations. Server-generated request IDs and SQL uniqueness protect repeat clicks and lost acknowledgements. An uncertain database acknowledgement tells the user to inspect pending schedules before repeating intent.

`fm_clause_install_guards()` wraps explicit offer/bid/clause writes and modifications. The shared preflight projects persisted reservations; final SQL admission serializes the buying team's commitment check. Manual dispatch intent and uncertain responses use the same durable reservation ledger. Selected-player clause and modification confirmations use `fm_clause_execute_async()` for asynchronous source verification, admission, durable dispatch, one native write and settlement. New market and direct offers use the dispatch path above. Held reads/writes leave the Shiny event loop available. Pure `build_market_bid_payload()`, `build_modify_bid_payload()` and `build_roster_clause_payload()` preserve the existing native wire contracts. Direct legacy callers retain synchronous guards; hosted latency remains a rollout measurement. Lost responses are reconciled with bounded read-only handoff rather than resubmitted.

`get_financial_snapshot()` adds `scheduled_reservations` when its foreground capacity already projects them, preventing the Championship funds filter from subtracting them again. Factual worker snapshots retain their factual source contract. Passive card checks use the shared account snapshot and introduce no source read.

Reads return all active jobs (at most 256 per account), the next 100 unread outcomes and 100 recent acknowledged successes; acknowledgement exposes further results. Proven newer league/season metadata retires superseded pending schedules without moving their authorization into a new context.

Active sessions share one asynchronous account poll every five seconds, with no status-driven redraw when job contents are unchanged. Closing the last session cancels its timer and releases the account's in-memory read state. These are display accelerators only. Outcomes remain in PostgreSQL, across logout and restart, until explicit acknowledgement; recent confirmed jobs remain available to protect stale balance views. Acknowledged terminal records are retained for 90 days, manual terminal records for seven days. Unread/uncertain records are not silently discarded.

## Database and service boundary

Migration `20261115075411_scheduled_clause_purchases.sql` is additive and mirrored in the fresh schema. All four new tables have RLS and server-only grants. Functions are SECURITY INVOKER with a restricted search path; browser/Supabase public roles have no execution or table access.

- `clause_authorizations`: encrypted token, credential binding, consent/revocation, authorization generation and rate-limit deadline. It is separate from remembered browser login and factual sync.
- `clause_jobs`: immutable authorization identity/cap, current deadline, reservations, lease, dispatch and outcome state. One active scheduled authorization per account/league/season/player.
- `clause_attempts`: durable intent for each send and its safe response code.
- `clause_evidence`: successful credential-free response/resource observations with exact scope, original timestamps, expiry and completeness. Upserts acknowledge persistence before reuse; bounds are 256 entries/32 MiB per account, with a 16 MiB entry bound.

Admission/cancellation/reconciliation use a per-team transaction advisory lock. `clause_dispatch` rechecks facts, consent, leases and commitments and writes intent before HTTP. `clause_claim` recovers an abandoned dispatched job as uncertain, never as a fresh send. `clause_settle` rejects an unproven dispatched-to-scheduled transition. Ownership/price/lock observations can update or cancel pending jobs but cannot authorize a new purchase.

`clause_write_fact` retains the existing normalized fact writer's table/field guards, contextual checks, conflict identities and monotonic catalog heads. Successful helper observations are persisted and reused before acquisition. Existing worker accounts can also receive acknowledged immutable chunks/manifests through `clause_publish`, preserving source times and avoiding older publication over newer observations. This does not enroll accounts in factual sync or enqueue acquisition jobs.

`worker/clauses.py` runs inside the existing supervised service and leader lease, independent of Shiny and browser cookies. One scheduler performs an indexed claim per second, with one lazily started reusable headless R helper. No process is created per player and no distant-player HTTP polling is added. Preparation starts ten seconds before opening and refreshes near two seconds before it; dispatch waits until the published instant. The purchase helper preserves per-resource HTTP/cache source timestamps so older membership metadata cannot backdate fresh cash. The helper closes after 30 idle seconds. Source HTTP slots/account pacing are shared with acquisition; persisted `Retry-After` deadlines constrain retries. No VM resources or production rate entitlement are inferred from the development executor.

The only write route is the captured dedicated `/1/market/rosterclause` contract: `header{token,userid}`, `query{championshipId,userteamId,player_slug,player_id,price}`, and `answer{}`. It is excluded from the factual route allowlist. Purchase dispatch has no transport retry or redirect following. The exact success code is `api.general.ok`; acceptance can precede squad delivery.

A proven closed-clause rejection can retry at 1, 2, 4, then capped 10-second intervals, within five minutes of the verified opening. The native closed-code allowlist is currently empty. Synthetic tests exercise that branch only with an explicitly artificial code. Explicit unknown native rejection codes end the attempt; lost/malformed/ambiguous responses retain uncertainty and trigger read-only reconciliation, backing off from one minute to one hour. Ownership alone does not prove which account paid a still-pending clause. Authentication failure during uncertainty cannot release its reservation as though failure were proven.

Unused purchase ciphertext is deleted after the final outstanding schedule/reconciliation ends. Revocation prevents new sends; ciphertext needed to reconcile an already-dispatched purchase is retained only for that purpose. Freshly logged-in tokens can renew outstanding authorization without changing its scope or re-enabling revoked consent.

## Temporary diagnostic and rollout

`FUTMONDO_CLAUSE_DIAGNOSTIC_ACCOUNT` defaults to unset. It exposes a separate real-request test only to the configured authenticated account on eligible Championship cards. Confirmation and submission bypass only the future-date condition, preserving current owner, price, transfer, funds, capacity and single-submission checks. A fresh recheck aborts if the advertised future lock changed. A local preflight stop keeps the available verification or rejection reason visible and says that the request stopped before reaching Futmondo. For a source response, the diagnostic continues to display the bounded response code. No automatic retry occurs.

After the owner captures a native closed-clause rejection, add its exact verified mapping and fixture, then remove the diagnostic UI/date bypass and unset the variable. A timeout requires reconciliation before another test. No automated test uses a real account or transaction.

Apply the migration before enabling `FUTMONDO_CLAUSE_SCHEDULING_ENABLED=true`, then redeploy the existing Docker image. The default-off flag avoids using an uninstalled schema. Verify source rejection mapping, dispatch lag, restart recovery, overall helper RSS, manual-action latency, mobile rendering, and real database/API request budgets in a controlled hosted acceptance run. Local checks do not certify production migration, deployment, Contabo capacity or real purchase success.


## Production database migration — 2026-11-16

Applied the exact reviewed `scripts/migrations/20261115075411_scheduled_clause_purchases.sql` to project `ozhnextrcxxksswpnglt` through the Supabase migration tool. Production migration history records version `20261116161538`, name `scheduled_clause_purchases`. Source SHA256: `fc1f7b61ac53e2a62484aa8aeb42809d7617e2e546822c243a84a89c5209a91e`.

Verified four RLS-enabled server-only tables, all 23 SECURITY INVOKER functions with fixed search paths and restricted execution grants, 11 valid indexes and the enabled `market_transactions` observation trigger. The service-role read smoke check used a synthetic absent account inside a rolled-back transaction; it made no Futmondo request or purchase. The empty due-job lookup used `clause_jobs_deadline` and measured 0.129 ms execution time (2.804 ms planning); this does not establish loaded scheduler throughput or opening-time lag.

Database installation is complete. Set `FUTMONDO_CLAUSE_SCHEDULING_ENABLED=true` in Dokploy with the existing worker settings and redeploy to activate it. This task did not change deployment settings or execute the diagnostic. Actual closed-clause rejection mapping, removal of the temporary diagnostic and controlled hosted execution/restart/resource checks remain open.


## Completed market bids

An accepted bid can disappear from active offers before the first readback if Futmondo settles it first. Live reconciliation also accepts a matching completed purchase. Today, My Team, and Players reuse their saved pressroom and team balance observations to reconcile older reservations. The transaction must match the player, buyer, seller and exact bid amount, and be at or after dispatch within the existing two-second timestamp tolerance. A System/Market seller identifies a market purchase. A rival seller requires the source to identify the transfer as non-clause. A balance observed after the purchase is required before releasing the held money and slot.

`clause_reconcile_completed_bids` checks both current worker snapshot versions and full scope, and releases matching manual bid reservations under the existing team lock. It is callable by the service role only. The Shiny observer shares requests across sessions, retries failed writes on the existing poll, and does not acquire source data. Active-offer readback and normal live action safeguards continue to apply.

### Active manual offers

Futmondo can acknowledge a submission immediately while LigaRadar still shows
the previous balance or market. The balance summary shows accepted actions
under **Accepted offers awaiting balance update**. **Offers awaiting
confirmation** applies to a dispatch whose outcome is still being checked.

The existing server observer tries saved active-offer evidence before transfer
history. The saved team market must contain the exact player and price with
an unambiguous own bid ID. Both the market and balance observations must
postdate dispatch, match the account, league, team, competition, season and
scoring context, and carry successful acquisition metadata. The service-only
`clause_reconcile_active_offers` RPC verifies the referenced versions and clears
the duplicate manual cash hold and squad slot under the team lock. The offer
continues to count through the official market ledger. Missing or inconsistent
evidence retains the reservation. The reconciler uses the existing account
poll and existing persisted snapshots without a new acquisition or browser timer.

Source inspection on 2026-11-22 Pacific used the [official Futmondo web
bundle](https://app.futmondo.com/main.dart.js) (SHA-256
`08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c`) and the
[captured market contract](har_api_reference.md#6-post-1marketplayers).
The official client requests `/1/market/players` with fantasy championship ID,
fantasy user-team ID and `type: "market"` in the authenticated POST envelope
and reads the answer as player rows. Its row parser reads the singular
`bid.id` and `bid.price`; LigaRadar exposes those as `bid_id` and `bid_price`.
The live saved market and balance corroborated that mapping for the repaired bid.
