# Selected Player Module Documentation

This document describes the `Selected_Player_Module.R` Shiny module, which renders detailed player profile cards, performance history plots, and interactive player acquisition features.

---

## 1. Module Overview

The Selected Player Module provides two exported functions:
* `selected_player_UI(id)` -- Renders the player detail box card, action button container, and valuation plot.
* `selected_player_Server(id, selected_player, login_token, championship_id, user_team_id)` -- Drives dynamic content update, modal dialogs, and buy/offer execution.

---

## 2. Player Acquisition Modalities

The card dynamically inspects player ownership and market status to offer three acquisition options (hidden for players owned by the logged-in user):

### Option 1: Market Offer ("Make Market Offer")
* **Condition**: Player is listed on the transfer market (`market_price` is present and > 0).
* **UI**: `btn_bid_market` action button.
* **Modal**: Prompt with `numericInput` pre-filled with `market_price`.
* **API Handler**: Calls `buy_clause(..., price = offer_amount, isClause = FALSE)`.

### Option 2: Direct Offer to Owner ("Offer to [Owner]")
* **Condition**: Player is owned by a rival user team (`user_team_id` != current user team) and not listed on the market.
* **UI**: `btn_offer_owner` action button.
* **Modal**: Prompt displaying current market valuation (`value`) and release clause reference, with `numericInput` for custom purchase offer.
* **API Handler**: Calls `buy_clause(..., price = offer_amount, isClause = FALSE)`.

### Option 3: Release Clause Buyout ("Buy Clause: [Price]")
* **Condition**: Player is owned by a rival team and has an active release clause (`isClause == TRUE`, `clause_price > 0`).
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

### Transaction Logging & Cache Management
On successful offer or clause buyout:
1. Calls `log_market_transaction()` to log purchase records into Supabase table `market_transactions`.
2. Displays formatted notification toast.
3. Calls `clear_api_cache()` to invalidate cached market and roster data.