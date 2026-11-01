# Offer error explanations

Market bids and direct offers in the player card share `offer_error_message(result)`. The known `api.market.max_number_players_in_roster` response explains the effective squad limit, possible pending arrivals, and completing a sale before refreshing. Readable server explanations are preserved; unknown API codes get a plain-language rejection, and unconfirmed responses tell the user to check existing offers before retrying. No automatic retry or submission/preflight behavior changes.

Both offer notifications remain visible until dismissed, with wrapping text and a viewport-bounded notification panel. This is a presentation-only change: no additional requests, persistence, timers, data transformations or cache invalidations. Focused offline tests cover the translated code, legacy message-only response, readable server message, unknown code and uncertain result. The combined lifecycle check is run with the rival ledger change. No live offer was submitted.
