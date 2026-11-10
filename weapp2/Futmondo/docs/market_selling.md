# Market Selling & Withdrawal Documentation

This document describes the API integration handlers and user workflows for selling squad players on the transfer market individually (`put_player_on_market`), in bulk through verified per-player requests, updating a listing (`update_player_market_listing`), and withdrawing listed players (`cancel_player_sell`).

---

## 1. API Functions

### Direct sale without an offer

Verified by `app.futmondo.com.direct_sel.har`: `POST /1/market/directsell` accepts `{header:{token,userid}, query:{championshipId,userteamId,player_id}, answer:{}}` and returns `{answer:{code:"api.general.ok"}}` on success. No price or bid ID is submitted. The captured flow withdraws an existing listing before the direct sale; the UI therefore requires users to remove existing listings themselves first.

`build_direct_sell_payload(login, championship_id, team_id, player_id)` returns that payload, preserving the empty JSON object. `direct_sale_preflight(...)` evicts only the selected player's summary cache and fetches fresh data through `get_player_summary()`/`get_cached_data()`. It requires matching player/owner IDs, a non-stale summary and an unlisted market shape; returns `{ok,code,message,value}` (`value` only on success). Summary listing fields `pr`, `p`, `vom` or `pom` block the direct sale. `direct_sell_player(...)` repeats preflight, contains request errors, never retries a trade and returns `{success,uncertain,code,message}`.

Example: `result <- direct_sell_player(login, championship_id, team_id, player_id)` **executes a sale** and is called only after user confirmation. The card shows current market value as a reference, not a guaranteed quote: this capture supplies no guaranteed direct-sale amount. Success emits `player_sold_direct` and refreshes authoritative roster/funds; uncertain responses emit only `refresh` and tell the user to check the outcome. Cash is never adjusted locally. Verified free-agent/rival rows and stale confirmation contexts cannot execute the action.

Focused offline verification: `Rscript test/test_direct_sell.R`. Captured credentials are never replayed.

### A. `put_player_on_market(login, championship_id, team_id, player_id, price)`
Lists an individual squad player on the transfer market via `POST https://api.futmondo.com/1/market/putonmarket`.
* **Parameters**:
  - `login`: Auth token list (`token`, `userid`).
  - `championship_id`: Active championship ID string.
  - `team_id`: User team ID string.
  - `player_id`: Player ID string.
  - `price`: Asking numeric listing price in EUR.
* **Returns**: `list(success, code, message)`.

### B. `cancel_player_sell(login, championship_id, team_id, player_id)`
Withdraws a currently listed player from the transfer market via `POST https://api.futmondo.com/1/market/cancelsell`.
* **Parameters**:
  - `login`: Auth token list (`token`, `userid`).
  - `championship_id`: Active championship ID string.
  - `team_id`: User team ID string.
  - `player_id`: Player ID string.
* **Returns**: `list(success, code, message)`.

### C. `update_player_market_listing(login, championship_id, team_id, player_id, price)`
Updates a listed player by calling the verified single-player endpoints in order: withdraw the existing listing, then create the replacement listing at `price`. If withdrawal succeeds but re-listing fails, the result explicitly reports that partial state so the user can refresh and act on it.
* **Parameters**: `login`, `championship_id`, `team_id`, `player_id`, and positive numeric `price`.
* **Returns**: `list(success, code, message)`.

### D. `get_my_market_players(login, championship_id, user_team_id)`
Queries all players currently listed on the transfer market by the logged-in user via `POST https://api.futmondo.com/1/market/myplayers`.
* **Returns**: Data frame of the user's listed market players.

### E. `accept_bid(login, championship_id, team_id, player_id, bid_id)`
Accepts a received offer, selling the player and crediting funds immediately via `POST https://api.futmondo.com/1/market/acceptbid`.
* **Parameters**:
  - `login`: Auth token list (`token`, `userid`).
  - `championship_id`: Active championship ID string.
  - `team_id`: User team ID string.
  - `player_id`: Player ID string.
  - `bid_id`: The ID of the incoming bid to accept.
* **Returns**: `list(success, code, message)`.

### F. `reject_bid(login, championship_id, team_id, player_id, bid_id)`
Rejects a received offer via `POST https://api.futmondo.com/1/market/rejectbid`.
* **Parameters**:
  - `login`: Auth token list (`token`, `userid`).
  - `championship_id`: Active championship ID string.
  - `team_id`: User team ID string.
  - `player_id`: Player ID string.
  - `bid_id`: The ID of the incoming bid to reject.
* **Returns**: `list(success, code, message)`.


## Bulk listing premium and preview — 2026-11-10

**Put All Players on Market** offers the same editable percentage for current market value and closed-clause pricing. The dialog starts at 0%; +0.5%, +1%, +2%, +4%, +5% and +10% buttons replace that percentage. Custom fractional percentages are accepted. A live preview shows each eligible player's base price, additional euros and final asking price, plus the corresponding squad totals and skipped-player count. Prices round up to whole euros. Blank, non-finite, negative or over-1000 percentages show a validation message and cannot submit; correcting the percentage keeps the confirmation usable.

`bulk_market_listing_plan(roster, mode, clause_premium_pct=5, now=Sys.time(), premium_pct=...)` is the shared pure calculation for preview and submission. Explicit `premium_pct` applies to either mode; omitted values preserve the legacy helper defaults (0% market value, 5% closed clause). Entries retain `player_id`, `price` and `already_listed`, and add `base_price` and `increase`. The source's integer-euro base and rounded additional amount avoid the binary `100 * 1.10` over-rounding; only machine-precision differences from integer increases are normalized, including 10,000 at 0.07%. Real fractional increases still round upward. Open, expired, transferred or missing clauses remain ineligible, and unknown values remain unavailable.

`bulk_market_listing_preview(plan, roster)` renders escaped player identities, amounts and truthful validation/skip states. The modal output uses the existing roster reactive and only renders for its matching confirmation context. Percentage edits make no additional source/database requests, write no observations and execute no market action. Existing durable roster acquisition and context isolation are unchanged. Confirmation retains the account/league/team guard and consumes valid confirmation once before the existing per-player actions; no uncertain action is automatically retried. No dependency, environment variable or schema is added.

Checks: `Rscript test/test_bulk_market_premium.R`, `Rscript test/test_market_listing_actions.R`, `Rscript test/test_shiny_simulation.R`. Responsive browser coverage: start the synthetic `Rscript test/test_browser_app.R`, then run `PLAYWRIGHT_BROWSERS_PATH=/tmp/futmondo-playwright-browsers node test/test_bulk_market_premium.cjs` using the existing local Playwright setup. All fixtures are offline and use synthetic accounts; browser coverage never confirms a market action. Actual outcomes and performance limits are recorded in `data_review_results.md`.
