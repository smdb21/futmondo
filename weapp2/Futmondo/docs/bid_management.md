# Market Bid Management Documentation (Modify & Cancel Bids)

This document describes the API integration handlers and user workflows for active bid detection, bid modification (`modify_bid`), and bid cancellation (`cancel_bid`).

---

## 1. API Functions

### A. `get_player_summary(login, championship_id, user_team_id, player_id)`
Fetches complete player data, historical market prices, and active market bids via `POST https://api.futmondo.com/1/player/summary`.
* **Returns**: A list with player `data`, `prices`, `bids` array, and extracted active bid fields `my_bid_id` and `my_bid_price`.

### B. `modify_bid(login, championship_id, team_id, player_id, bid_id, new_price)`
Updates an existing active market offer via `POST https://api.futmondo.com/5/market/modifybid`.
* **Parameters**:
  - `login`: Auth token list (`token`, `userid`).
  - `championship_id`: Active championship ID string.
  - `team_id`: User team ID string.
  - `player_id`: Player ID string.
  - `bid_id`: Bid ID string.
  - `new_price`: Numeric updated offer price in EUR.
* **Returns**: `TRUE` if `answer$code == "api.general.ok"`.

### C. `cancel_bid(login, championship_id, team_id, bid_id)`
Withdraws an active market bid via `POST https://api.futmondo.com/1/market/cancelbid`.
* **Parameters**:
  - `login`: Auth token list (`token`, `userid`).
  - `championship_id`: Active championship ID string.
  - `team_id`: User team ID string.
  - `bid_id`: Bid ID string.
* **Returns**: `TRUE` if `answer$code == "api.general.ok"`.

---

## 2. Table Column Display
In player tables (`get_reactable_columns_for_players`), active bids are displayed in the `"Your Bid"` column formatted with an emerald badge (`.badge-active-bid`), making active bids clearly visible across market and player tables.