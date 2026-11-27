# Manual bid management

## Market offer cancellation (2026-11-26)

When a player card opens, `player_card_market_offer()` projects the selected player's outgoing offer from the same account, league and team market observation used by Today. A verified, saved offer is shown as the last recorded bid, including between worker updates. The existing finance ledger supplies the submitted amount immediately after an acknowledged market offer until a newer market observation arrives. Cancel Bid can then look up the native bid ID. A published listing with no own bid clears the display. An arbitrary player-table bid, a bid count or a pending request cannot establish ownership.

The official client sends the authenticated championship and user-team IDs with `type = "market"` to `POST /1/market/players`. The recorded market response carries the player's own offer in `answer[].bid.id` and `bid.price`. Both confirmation steps recheck that source. The final write sends the confirmed native ID as `query.bid` to `POST /1/market/cancelbid`. The asynchronous write is single-attempt and guarded by the player context and live verification deadline. An uncertain acknowledgement triggers a scoped worker refresh. The existing database market snapshots retain their original source time and continue to serve subsequent sessions. Source checked 2026-11-26 against the official bundle with SHA-256 `08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c` and the captured market field names. [Official client](https://app.futmondo.com/main.dart.js) and [captured market contract](har_api_reference.md#6-post-1marketplayers).

Users enter bid amounts explicitly in the player/market controls. The app validates the current player/listing, selected league/team, available spending capacity, API minimum/maximum, and existing commitments before sending an action.

Submission, modification, and cancellation are separate acknowledged events. An uncertain response requires reconciliation with current Futmondo state; it must not automatically replay a transaction. Successful actions invalidate the affected offers, market/roster, and finances.

Smart Bid, recommended bids, forecast-based amounts, and automatic bidding are removed. Futmondo's own auction settlement remains an upstream game behavior.

The retained bid and offer tests exercise amount validation, context changes, and the manual handlers without real API writes.

Player cards also read the direct own-bid ID and price from the team-scoped market response already acquired for capacity checks. This preserves Update Bid and Cancel Bid when an existing offer fills the final roster slot, even if the player summary omits the offer. Market evidence retains its own source deadline independently of unrelated roster or funds failures. Selected-row rival or received bids are never used as own-offer evidence. Each manual update/cancellation rechecks the scoped market source and compares the confirmed bid ID and amount before writing; changed, stale or ambiguous offers require a new confirmation. Updates retain existing-offer funds checks; cancellation does not require acquisition capacity. Passive card rendering adds no requests; each action opening and submission adds one current market read (plus a summary read for offers outside the market).


## Configured borrowing and minimum bids — 2026-11-02

Capacity uses observed `configuration$mnmp` instead of a fixed 50% squad-value allowance. `funds$debt_fraction` preserves the input and `rules$minimum_bid_at_value` preserves `configuration$mbmv`. `market_bid_minimum(player_row, minimum_at_value=NA)` applies the player-value floor only when the league enables it, while preserving an explicit listing/asking price. Missing rule evidence or no establishable minimum remains unknown; no automatic minimum is fabricated.

Both passive card eligibility and final bid/modify preflights use this rule. `run_acquisition_preflight()` returns its verified `minimum_bid`, reused by the dialog label, initial value and input minimum; opening a dialog does not add a separate configuration read. Submission checks the current capacity again. Existing API `maxBid`, roster constraints, pending transfers, positive modification deltas, manual confirmation and uncertain-response protections remain active. The standalone legacy `calculate_futmondo_ranking_prizes()` helper now requires an explicit prize amount rather than defaulting to €30M; the active ledger already uses configured reward values.

Direct-sale proceeds and clause prices are not recalculated from guessed constants: the current direct-sale UI already labels market value as reference only, and clause actions use the source clause price/status. The newly decoded `dspct`, `ccr`, protection, blocking and loan settings are shown in Admin; this change does not invent a local clause-price algorithm or enable unsupported actions.


## Market-offer percentage buttons (2026-11-26)

Place Market Offer and Update Your Active Bid share one row of +1%, +2%, +4%, +5% and +10% buttons. Each tap adds that percentage of the displayed reference price to the offer. New offers use the saved minimum market bid when available, otherwise the listed price. Updates use the verified minimum and start from the existing bid or that minimum, whichever is higher.

The editor accumulates percentage points from the last typed amount and rounds the resulting offer up to whole euros. On a €150 listing, two +1% taps produce €152 and then €153. Typing €180 and tapping +10% produces €195. The colored percentage beside the total always compares the amount in the field with the reference price, including whole-euro rounding. A small inline message identifies an amount below that reference or below a separately known player valuation. The valuation message is advisory because the league controls whether valuation is a bidding floor.

The browser updates the preview on each input event using the values already in the dialog. Buttons edit the amount. The submit control confirms the offer through its existing manual handler.
