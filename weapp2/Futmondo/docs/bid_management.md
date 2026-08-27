# Market Bid Management Documentation (Modify & Cancel Bids)

This document describes the API integration handlers and user workflows for active bid detection, bid modification (`modify_bid`), and bid cancellation (`cancel_bid`).

---

## 1. API Functions

### A. `get_player_summary(login, championship_id, user_team_id, player_id)`
Fetches complete player data, historical market prices, and active market bids via `POST https://api.futmondo.com/1/player/summary`.
* **Returns**: A list with player `data`, `prices`, `bids` array, and extracted active bid fields `my_bid_id` and `my_bid_price`.
* **Cache**: `player_summary_<userid>_<champ>_<team>_<player>` — viewer/team-specific. Because `my_bid_id`/`my_bid_price` depend on the requesting user's team, they must never be served from a shared `player_summary_<champ>_<player>` key. A missing/empty `userid` maps to `anonymous`; a missing/empty `user_team_id` maps to `none`.

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

### D. `get_roster_bids(login, championship_id, user_team_id)`
Fetches the team's active roster (market) bids via `POST https://api.futmondo.com/1/market/rosterbids`.
* **Query**: includes `type = "roster"` so only roster (market) bids are returned.
* **Returns**: a data frame with one row per player containing `id`, `bid_price`, `bid_user`, `bid_id`, and `bidder_team_id` (the immutable team ID of the bidder, extracted via `extract_bidder_team_id`). Cached as `roster_bids_<champ>_<team>`.

### E. `extract_bidder_team_id(bid)`
Normalizes the bidder's immutable team ID from a bid object. Checks `userTeam[["_id"]]`, `userTeam[["id"]]`, then the flat keys `userteamId` / `userTeamId` / `userteam_id` / `userTeam_id` / `bidder_team_id`. Returns `NA_character_` when none is present. This is how "my own" active bids are identified (never by name or list order).

### F. `get_acquisition_capacity(login, championship_id, user_team_id, target_player_id = NULL)`
Computes a **verified** acquisition-capacity snapshot for the logged-in team. Cached as `acq_capacity_<userid>_<champ>_<team>_<target>`.
* **Sources**: `get_user_team_info` (budget/withheld/config), `get_players_from_team` (roster count), `get_roster_bids` (outstanding own bids), and `get_player_summary` (target's own bid / highest competing bid / bid count) when `target_player_id` is given.
* **Spendable funds**: conservative `max(0, budget - withheld)`; `withheld` defaults to `0` when missing (with a diagnostic note).
* **Returns** a structured list:
  - `status`: `"ok" | "partial" | "unavailable"` (degraded when any source is missing/ambiguous).
  - `roster`: `list(count, cap, remaining_slots)`.
  - `funds`: `list(reported_budget, withheld, spendable_budget)`.
  - `outstanding`: `list(offers, count, total_amount, completeness)`.
  - `target`: `list(my_bid_id, my_bid_amount, highest_bid, bid_count)` (empty when no target).
  - `diagnostics`: character vector of data-availability notes.

### G. `evaluate_acquisition_preflight(capacity, mode, amount = NULL, existing_bid_amount = NULL)`
Pure decision function that gates every acquisition path. **Fails closed**: if `capacity` is `NULL` or `status != "ok"`, it returns `reason = "unavailable"` (verification could not be confirmed).
* **Modes**:
  - `"bid"` / `"offer"` (new offers): rejected with `reason = "capacity"` when `roster_count + outstanding_count >= cap`.
  - `"clause"` (release-clause buyout): rejected with `reason = "capacity"` when `roster_count >= cap`.
  - `"modify"` (bid update): does **not** consume another slot; requires a verifiable existing own bid (`existing_bid_amount > 0`), else `reason = "unavailable"`.
* **Funds**: when `amount` is known, the required spend (the amount, or the positive delta for `"modify"`) must not exceed verified spendable funds, else `reason = "funds"`.
* **Returns**: `list(ok = logical, reason = "ok"|"unavailable"|"capacity"|"funds", message = character)`.

### H. `build_roster_clause_payload(login, championship_id, team_id, player_id, player_slug, price)`
Builds the exact JSON payload for the dedicated roster-clause buyout endpoint. Serializes exactly:
`header{token, userid}`, `query{championshipId, userteamId, player_slug, player_id, price}`, `answer{}`.
* **Note**: `isClause` is intentionally **not** part of this payload (the endpoint itself implies a clause purchase).

### I. `buy_roster_clause(login, championship_id, team_id, player_id, player_slug, price, url = ROSTER_CLAUSE_URL)`
Executes a release-clause buyout via `POST https://api.futmondo.com/1/market/rosterclause`. The entire request is wrapped in `tryCatch()` so a network failure can never block the user thread or crash the parent server.
* **Returns**: `list(success = logical, code = character, message = character)`.
* `url` is overridable for tests/mocks.

---

## 2. Acquisition Preflight in the UI

`Selected_Player_Module` runs a single internal preflight (`run_acquisition_preflight`) on **every** acquisition path, both when opening the modal and again immediately before the write:

| Path | Mode | Amount | Notes |
|---|---|---|---|
| Market offer (`submit_bid`) | `bid` | offer amount | new offer must have a free slot + funds |
| Direct owner offer (`submit_owner_offer`) | `offer` | offer amount | new offer must have a free slot + funds |
| Release-clause buyout (`submit_clause`) | `clause` | clause price (recomputed locally) | requires a free roster slot; uses `buy_roster_clause` |
| Bid modification (`submit_modify_bid`) | `modify` | new price | delta vs existing bid; no new slot consumed |

Failures are surfaced as notifications that distinguish **unavailable verification** from **capacity** and **funds** rejections. On success, logging (`log_market_transaction`), cache invalidation (`clear_api_cache`), and the `on_bid_updated` callback are preserved.

---

## 3. Table Column Display
In player tables (`get_reactable_columns_for_players`), active bids are displayed in the `"Your Bid"` column formatted with an emerald badge (`.badge-active-bid`), making active bids clearly visible across market and player tables.