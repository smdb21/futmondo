# Manual bid management

Users enter bid amounts explicitly in the player/market controls. The app validates the current player/listing, selected league/team, available spending capacity, API minimum/maximum, and existing commitments before sending an action.

Submission, modification, and cancellation are separate acknowledged events. An uncertain response requires reconciliation with current Futmondo state; it must not automatically replay a transaction. Successful actions invalidate the affected offers, market/roster, and finances.

Smart Bid, recommended bids, forecast-based amounts, and automatic bidding are removed. Futmondo's own auction settlement remains an upstream game behavior.

The retained bid and offer tests exercise amount validation, context changes, and the manual handlers without real API writes.


## Configured borrowing and minimum bids — 2026-11-02

Capacity uses observed `configuration$mnmp` instead of a fixed 50% squad-value allowance. `funds$debt_fraction` preserves the input and `rules$minimum_bid_at_value` preserves `configuration$mbmv`. `market_bid_minimum(player_row, minimum_at_value=NA)` applies the player-value floor only when the league enables it, while preserving an explicit listing/asking price. Missing rule evidence or no establishable minimum remains unknown; no automatic minimum is fabricated.

Both passive card eligibility and final bid/modify preflights use this rule. `run_acquisition_preflight()` returns its verified `minimum_bid`, reused by the dialog label, initial value and input minimum; opening a dialog does not add a separate configuration read. Submission checks the current capacity again. Existing API `maxBid`, roster constraints, pending transfers, positive modification deltas, manual confirmation and uncertain-response protections remain active. The standalone legacy `calculate_futmondo_ranking_prizes()` helper now requires an explicit prize amount rather than defaulting to €30M; the active ledger already uses configured reward values.

Direct-sale proceeds and clause prices are not recalculated from guessed constants: the current direct-sale UI already labels market value as reference only, and clause actions use the source clause price/status. The newly decoded `dspct`, `ccr`, protection, blocking and loan settings are shown in Admin; this change does not invent a local clause-price algorithm or enable unsupported actions.
