# Manual bid management

Users enter bid amounts explicitly in the player/market controls. The app validates the current player/listing, selected league/team, available spending capacity, API minimum/maximum, and existing commitments before sending an action.

Submission, modification, and cancellation are separate acknowledged events. An uncertain response requires reconciliation with current Futmondo state; it must not automatically replay a transaction. Successful actions invalidate the affected offers, market/roster, and finances.

Smart Bid, recommended bids, forecast-based amounts, and automatic bidding are removed. Futmondo's own auction settlement remains an upstream game behavior.

The retained bid and offer tests exercise amount validation, context changes, and the manual handlers without real API writes.
