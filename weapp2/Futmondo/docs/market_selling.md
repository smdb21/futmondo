# Market Selling & Withdrawal Documentation

This document describes the API integration handlers and user workflows for selling squad players on the transfer market individually (`put_player_on_market`), in bulk through verified per-player requests, updating a listing (`update_player_market_listing`), and withdrawing listed players (`cancel_player_sell`).

---

## 1. API Functions

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