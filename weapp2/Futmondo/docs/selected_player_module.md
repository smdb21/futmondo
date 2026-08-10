# Selected Player Module Documentation

This document describes the `Selected_Player_Module.R` Shiny module, which renders detailed player profile cards, performance history plots, and interactive player acquisition features.

---

## 1. Module Overview

The Selected Player Module provides two exported functions:
* `selected_player_UI(id)` -- Renders the player detail box card, action button container, and valuation plot.
* `selected_player_Server(id, selected_player, login_token, championship_id, user_team_id)` -- Drives dynamic content update, modal dialogs, and buy/offer execution.

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
* **Modal**: Confirmation dialog displaying fixed `clause_price`.
* **API Handler**: Calls `buy_clause(..., price = clause_price, isClause = TRUE)`.

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