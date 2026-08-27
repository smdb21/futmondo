# Selected Player Module Documentation

This document describes the `Selected_Player_Module.R` Shiny module, which renders detailed player profile cards, performance history plots, and interactive player acquisition features.

---

## 1. Module Overview

The Selected Player Module provides two exported functions:
* `selected_player_UI(id)` -- Renders the player detail box card, action button container, and valuation plot.
* `selected_player_Server(id, selected_player, login_token, championship_id, user_team_id, on_bid_updated = NULL, open_action = NULL, capacity_fetcher = NULL)` -- Drives dynamic content update, modal dialogs, and buy/offer execution.
  * `on_bid_updated` (optional) -- callback invoked after successful bid/offer/listing writes so the caller can invalidate cached data.
  * `open_action` (optional) -- reactive returning a stable action code. When a selected player is currently valid, the module routes exactly two codes (guarded against stale startup reactive values; all other codes, e.g. `"view"`, are ignored):
    * **`"market_bid"`** (Today's "Place Bid" recommendation) -> opens the SAME market-offer modal as the "Make Market Offer" button (identical `run_acquisition_preflight` behavior).
    * **`"clause_buyout"`** (Today's "Exercise Clause" recommendation) -> opens the SAME clause-buyout confirmation modal as the "Buy Release Clause" button via the shared `open_clause_buyout_modal(sp)` helper. It **rechecks the strict open-clause state before showing** (finite positive `clause_price`, explicitly `FALSE` `clause_transferred`, parseable `clause_date <= now` -- the same `today_is_clause_open()` policy helper used by the Today module), runs the clause preflight, and shows the confirmation modal. It **never opens the market bid modal**, and **only the recomputed `clause_price` is shown and executed** -- no comparison price (e.g. `max(market, clause)`) is ever sent to the clause endpoint.
  * `capacity_fetcher` (optional) -- test seam: a function used in place of the network `get_acquisition_capacity()` call inside `run_acquisition_preflight`, so preflight behavior can be exercised deterministically (no network write).

---

## 2. Player Acquisition Modalities & Detection Logic

The card dynamically inspects player ownership, market status, and release clause unlock timers to offer three acquisition options (hidden for players owned by the logged-in user, which display a "Player in Your Squad" badge instead):

### Market Price Normalization (`effective_market_price`)
Market prices vary depending on the API endpoint:
- Roster endpoint (`/1/userteam/roster`) returns market prices in `market_price`.
- Market endpoint (`/1/market/players`) returns market prices in `price`.
The module normalizes these into `effective_market_price`, checking `effective_market_price`, `market_price`, then `price`.

### Release Clause Unlock Timer & Cooldown (`is_clause_open`)
A release clause is **OPEN** for buyout if:
1. `clause_price` > 0
2. `clause_transferred` == FALSE (not in cooldown lock from a recent purchase)
3. `clause_date` <= current timestamp (clause protection timer has elapsed)

If a clause exists (`clause_price > 0`) but is currently locked (`clause_transferred == TRUE` or `clause_date > Sys.time()`), a **"Release Clause Locked until [Date]"** indicator badge is displayed informing the user when the clause will unlock.

---

### Option 1: Market Offer ("Make Market Offer")
* **Condition**: Player is listed on the transfer market (`effective_market_price` > 0).
* **UI**: `btn_bid_market` action button.
* **Modal**: Prompt with `numericInput` pre-filled with `effective_market_price`.
* **API Handler**: Calls `buy_clause(..., price = offer_amount, isClause = FALSE)`.

### Option 2: Direct Offer to Owner ("Offer to [Owner]")
* **Condition**: Player is owned by a rival user team (`user_team_id` != current user team) and not currently listed on the market.
* **UI**: `btn_offer_owner` action button.
* **Modal**: Prompt displaying current market valuation (`value`) and release clause reference, with `numericInput` for custom purchase offer.
* **API Handler**: Calls `buy_clause(..., price = offer_amount, isClause = FALSE)`.

### Option 3: Release Clause Buyout ("Buy Clause: [Price]")
* **Condition**: Player is owned by a rival team and has an OPEN release clause (`is_clause_open == TRUE`).
* **UI**: `btn_pay_clause` action button.
* **Shared helper**: Both the button and the external `"clause_buyout"` action event (Today's "Exercise Clause" recommendation) go through the single shared helper `open_clause_buyout_modal(sp)`:
  1. **Open-state recheck (fail closed)** -- the strict open-clause state is rechecked from the player row before showing (`today_is_clause_open()`: finite positive `clause_price`, **explicitly** `FALSE` `clause_transferred`, parseable `clause_date <= now`). A stale / locked / transferred clause shows a warning and opens **no modal**.
  2. **Preflight** -- `run_acquisition_preflight(sp, "clause", amount = NULL)` (roster slot + verified spendable funds, fail closed).
  3. **Modal** -- confirmation dialog displaying the fixed `clause_price` (recomputed locally from the player row, not from modal-scope state). The module's `clause_modal_opened_RV` records whether the modal opened (test seam mirroring `offer_modal_opened_RV`).
* **API Handler**: On submit, the strict open state is **rechecked again before the write** (the clause may have locked/transferred between modal-open and submit), then `buy_roster_clause(..., price = clause_price)` is called against the dedicated `POST /1/market/rosterclause` endpoint (see `docs/api_endpoints.md` section 9). The clause price is recomputed locally in both the modal-open and submit handlers so a stale/undefined `clause_price` can never be sent. **Only the clause price is transmitted** -- a comparison price (e.g. `max(market price, clause price)` from a dual-route Today recommendation) is never sent to the clause endpoint. On success, `log_market_transaction()`, `clear_api_cache()`, and the `on_bid_updated()` callback are preserved.

---

## 2b. Acquisition Capacity Preflight

Every acquisition path (market offer, direct owner offer, clause buyout, bid modification) runs a single internal preflight, `run_acquisition_preflight(sp, mode, amount, existing_bid_amount)`, which wraps `get_acquisition_capacity()` + `evaluate_acquisition_preflight()` (see `docs/bid_management.md`).

* **When it runs**: at modal-open time (amount unknown) and again immediately before the write (amount known).
* **Fail-closed**: if the capacity snapshot is unavailable/ambiguous (`status != "ok"`), the action is blocked with reason `unavailable` (distinct from `capacity` and `funds`).
* **Rules**:
  - New offers (`bid`/`offer`) are rejected when `roster_count + outstanding_count >= cap`.
  - Clause buyout (`clause`) is rejected when `roster_count >= cap`.
  - Bid modification (`modify`) does not consume another slot but requires a verifiable existing own bid.
  - When the amount is known, the required spend (amount, or the positive delta for `modify`) must not exceed verified spendable funds.
* **Notifications**: failures surface a toast that distinguishes unavailable verification from capacity and funds rejections. On success, `log_market_transaction()`, `clear_api_cache()`, and the `on_bid_updated()` callback are preserved.

---

## 3. Server Reactives & Handlers

### Parameters
* `id`: Module namespace ID.
* `selected_player`: Reactive returning the selected player data frame row.
* `login_token`: Reactive returning user authentication token list (`token`, `userid`).
* `championship_id`: Reactive returning active championship ID string.
* `user_team_id`: Reactive returning logged-in user's team ID string.

### JSON Serialization
All outbound HTTP requests from `buy_clause()` use `toJSON(payload, auto_unbox = TRUE)` to produce compact JSON with no wrapper arrays. Scalar fields are explicitly cast to comply with Futmondo's API specifications:
- `as.character` for string IDs (e.g., `championship_id`, `user_team_id`).
- `as.numeric` for monetary values (e.g., `price`).
- `as.logical` for flags (e.g., `isClause`).

This ensures that numeric and boolean values are serialized as bare scalars rather than single-element arrays, which the Futmondo API rejects.

### Error Response Handling
`buy_clause()` always returns a structured list with three keys: `success`, `code`, and `message`.

- On success: `success` is `TRUE`, `code` is the HTTP status, and `message` is a confirmation string.
- On failure (offer or buyout): `success` is `FALSE`, `code` is the HTTP error code, and `message` contains Futmondo's specific error description extracted from the response body.

The caller in `selected_player_Server` checks `response$success`. If `FALSE`, the module displays `response$message` in the notification toast, surfacing the API-provided error to the user without crashing.

### Transaction Logging & Cache Management
On successful offer or clause buyout:
1. Calls `log_market_transaction()` to log purchase records into Supabase table `market_transactions`.
2. Displays formatted notification toast.
3. Calls `clear_api_cache()` to invalidate cached market and roster data.

---

## 4. Active Bid Management

### Active Bid Check
Calls `get_player_summary` to detect if the user has an active offer on the selected player. The response includes `my_bid_id` and `my_bid_price` fields that indicate an existing active bid.

### Active Bid Banner
Renders `"Your Active Bid: [Price]"` inside the player card, providing immediate visibility of the user's pending offer.

### "Update Bid" Modal (`btn_modify_bid`)
Prompts with `numericInput("new_bid_amount")` pre-filled with the active bid price. On submission, calls `modify_bid()` to update the existing offer to the new price.

### "Cancel Bid" Modal (`btn_cancel_bid`)
Prompts a confirmation dialog to withdraw the active bid. On confirmation, calls `cancel_bid()` to cancel the pending offer entirely.

---

## 5. Market Selling & Withdrawal

### "Put on Market for Sale" (`btn_put_on_market`)
Prompts a modal dialog with a `numericInput` for the asking price. On submission, executes the listing via `put_player_on_market()`, which calls `POST https://api.futmondo.com/1/market/putonmarket`. On success, invalidates the API cache and displays a confirmation notification.

### "Remove from Market" (`btn_cancel_sell`)
Displays a confirmation modal to withdraw the listed player from the transfer market. On confirmation, executes the withdrawal via `cancel_player_sell()`, which calls `POST https://api.futmondo.com/1/market/cancelsell`. On success, invalidates the API cache and displays a confirmation notification.

### "Accept Received Offer" (`btn_accept_offer`)
Displays a modal confirming the sale of the player to the bidder. On confirmation, calls `accept_bid()` to finalize the transaction, selling the player and crediting the user's funds immediately. On success, invalidates the API cache and displays a confirmation notification.

### "Reject Received Offer" (`btn_reject_offer`)
Displays a modal confirming the rejection of the incoming offer. On confirmation, calls `reject_bid()` to decline the bid. On success, invalidates the API cache and displays a confirmation notification.

### "Bidder" Column (`bid_user`)
Displays the name of the user who placed the incoming offer. When the offer is generated by the Futmondo system (detected by `userTeam.name == ""`), the column displays `"Futmondo"` instead (standardized system bidder label).

### Column Alignment
All currency and market-related columns use left alignment (`align = "left"`) to ensure consistent formatting across the module's data tables.

---

## 6. FIS 5-Pillar Breakdown Panel

When a player is selected, the module renders an FIS Score breakdown panel that decomposes the composite FIS Score into its five constituent pillars. This panel appears within the player modal/detail view.

### Pillar Breakdown

Each pillar displays:
- **Pillar name** (Form, Momentum, Ownership, Scarcity, Fixture)
- **Normalized score** (0-100)
- **Weight** applied in the composite formula (e.g., Form = 30%, Momentum = 20%)
- **Visual bar** proportional to the pillar score

### Verdict and Confidence

Below the pillar breakdown, the panel displays:
- **1-Sentence Verdict**: An auto-generated summary sentence that interprets the player's overall FIS profile (e.g., "Strong recent form and favorable fixtures make this player a high-value target despite moderate market momentum.").
- **Confidence Pill**: A colored badge showing the prediction confidence percentage (High/Medium/Low), derived from data freshness, sample size, historical accuracy, and market volatility per the Prediction Confidence formula in `docs/v3_roadmap.md` Section 4.

---

## 7. Smart Bid & Bid Competition Panel

When a player is available for acquisition, the module renders a Smart Bid widget that provides data-driven bidding guidance and live competition analytics.

### Smart Bid Metrics

The panel displays the following computed values:

| Metric | Description |
|---|---|
| Live Fair Value | The estimated intrinsic value of the player based on Moneyball Metrics and recent performance |
| Recommended Bid | The optimal bid amount, adjusted for the user's Manager DNA profile (aggression score) |
| Expected Winning Range | The bid range where the probability of winning the auction exceeds the user's target threshold (default 70%) |
| Max Rational Bid | The highest bid that still yields a positive expected ROI; bidding above this is flagged as an overpay |
| Expected ROI % | Projected return on investment percentage based on the recommended bid vs. fair value |

### Competition Analysis

The panel also provides competitive intelligence:

| Metric | Description |
|---|---|
| Competition Level | Qualitative assessment (Low / Medium / High) based on the number of active bidders and historical auction intensity for this player |
| Likely Competitor Managers | Predicted count of rival managers expected to bid on this player, derived from Manager DNA clustering and historical bidding patterns |

### "Use Smart Bid" 1-Click Pre-Fill

A prominent **"Use Smart Bid"** button is provided. When clicked, it automatically pre-fills the bid amount input field with the `recommended_bid` value, enabling the user to execute the bid with a single additional confirmation click. This streamlines the acquisition flow from analysis to execution.

### Contextual Data (no hardcoded budget)

The widget no longer hardcodes a 300,000,000 budget. It computes the recommendation from live, verified context:

* **Verified capacity** -- `get_acquisition_capacity(login, championship_id, user_team_id, target_player_id = <player>)` supplies the verified spendable funds (`max(0, budget - withheld)`) and the target's highest competing bid.
* **Pressroom history** -- `get_championship_pressroom()` (cached) provides market/competition context.
* **Market high bid** -- the target's `highest_bid` from the capacity snapshot is passed as `market_high_bid` so the minimum winning bid reflects the live auction.
* **Unverified funds** -- `user_cash = NA` is passed so the engine treats `user_cash` as unverified; the verified figure comes from the capacity snapshot. When no capacity is available, the engine falls back to its default and marks `funds_verified = FALSE`.

The resulting `recommended_bid` and `max_rational_bid` are always bounded by the verified spendable funds, so the widget never recommends a bid the team cannot afford.

---

## 8. Player Points Trend Trace

The player valuation/points trend chart (`player_trend_plot`) renders two series:

* **Market Valuation** -- a line+markers series from the player's historical `value` snapshots (with a pre-season simulated fallback when the DB is empty).
* **Points** -- built by the pure helper `build_player_points_trace(history_df, sp)`:
  - **One marker per completed round**: each historical snapshot carrying a valid recorded point value (finite, `>= 0`) becomes a marker; when a round has several snapshots, the latest is kept, so the trace has exactly one marker per round.
  - **Markers only**: the points series uses `mode = "markers"` (no interpolated line) so it never implies points that were not recorded.
  - **Graceful no-points state**: when there are no valid points (NULL/empty history, all-NA points, or all-negative), the points axis is hidden and a "No points recorded yet" annotation is shown instead of a fabricated zero line.