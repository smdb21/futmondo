# Manual bid management

Users enter bid amounts explicitly in the player/market controls. The app validates the current player/listing, selected league/team, available spending capacity, API minimum/maximum, and existing commitments before sending an action.

Submission, modification, and cancellation are separate acknowledged events. An uncertain response requires reconciliation with current Futmondo state; it must not automatically replay a transaction. Successful actions invalidate the affected offers, market/roster, and finances.

Smart Bid, recommended bids, forecast-based amounts, and automatic bidding are removed. Futmondo's own auction settlement remains an upstream game behavior.

The retained bid and offer tests exercise amount validation, context changes, and the manual handlers without real API writes.

Player cards also read the direct own-bid ID and price from the team-scoped market response already acquired for capacity checks. This preserves Update Bid and Cancel Bid when an existing offer fills the final roster slot, even if the player summary omits the offer. Market evidence retains its own source deadline independently of unrelated roster or funds failures. Selected-row rival or received bids are never used as own-offer evidence. Each manual update/cancellation rechecks the scoped market source and compares the confirmed bid ID and amount before writing; changed, stale or ambiguous offers require a new confirmation. Updates retain existing-offer funds checks; cancellation does not require acquisition capacity. Passive card rendering adds no requests; each action opening and submission adds one current market read (plus a summary read for offers outside the market).


## Configured borrowing and minimum bids — 2026-11-02

Capacity uses observed `configuration$mnmp` instead of a fixed 50% squad-value allowance. `funds$debt_fraction` preserves the input and `rules$minimum_bid_at_value` preserves `configuration$mbmv`. `market_bid_minimum(player_row, minimum_at_value=NA)` applies the player-value floor only when the league enables it, while preserving an explicit listing/asking price. Missing rule evidence or no establishable minimum remains unknown; no automatic minimum is fabricated.

Both passive card eligibility and final bid/modify preflights use this rule. `run_acquisition_preflight()` returns its verified `minimum_bid`, reused by the dialog label, initial value and input minimum; opening a dialog does not add a separate configuration read. Submission checks the current capacity again. Existing API `maxBid`, roster constraints, pending transfers, positive modification deltas, manual confirmation and uncertain-response protections remain active. The standalone legacy `calculate_futmondo_ranking_prizes()` helper now requires an explicit prize amount rather than defaulting to €30M; the active ledger already uses configured reward values.

Direct-sale proceeds and clause prices are not recalculated from guessed constants: the current direct-sale UI already labels market value as reference only, and clause actions use the source clause price/status. The newly decoded `dspct`, `ccr`, protection, blocking and loan settings are shown in Admin; this change does not invent a local clause-price algorithm or enable unsupported actions.


## Market-offer percentage buttons — 2026-11-15

The Place Market Offer dialog adds the same +0.5%, +1%, +2%, +4%, +5% and
+10% presets as the bulk sale-listing dialog. Each preset sets `bid_amount`
to the verified minimum market bid plus its percentage premium, rounded up
to whole euros. The minimum, rather than player value or an edited offer,
is the fixed base for every click; repeated clicks do not compound. For a
€150 minimum the presets yield €151, €152, €153, €156, €158 and €165.
Users can still type any amount and explicitly submit it.

`output$market_offer_body` renders six native buttons with `data-target-input`
and `data-amount`. A click updates the namespaced numeric input and dispatches
its existing change event, which updates `bid_amount_preview`. The wrapping
flex group fits narrow phones. Buttons appear only after availability succeeds.
They do not submit an offer or acquire data; all existing current-state,
context, capacity, funds and duplicate-submission checks remain in force.
No API payload, persisted observation, cache policy or dependency changes.

Verification: `Rscript test/test_market_offer_opening.R`, the existing offline
lifecycle, and `test/test_market_offer_premium.cjs` against
`Rscript test/test_browser_app.R`. Browser tests use synthetic accounts and
never submit an offer. Results and performance limits: `data_review_results.md`.
