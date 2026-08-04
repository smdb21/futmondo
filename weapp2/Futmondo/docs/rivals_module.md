# Rivals Module - Rivals Explorer Scouting Page

This document describes the `Rivals_Module.R` Shiny module that implements the "Rivals Explorer" scouting page. It allows the logged-in user to select any team in the championship, view their financial overview, and inspect their full player roster with clause-to-value ratio indicators.

---

## 1. Module Overview

The Rivals Module provides two exported functions:

* `rivals_UI(id)` -- Shiny UI module that renders the scouting interface.
* `rivals_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV)` -- Shiny server module that drives reactivity and data fetching.

---

## 2. `rivals_UI(id)`

### Parameters

| Parameter | Type   | Description                              |
|-----------|--------|------------------------------------------|
| `id`      | `char` | Unique namespace ID for the Shiny module. |

### Return Value

Returns a `tagList` containing:
1. A team selector box (`selectInput`) for choosing a rival to scout.
2. A `uiOutput` placeholder for the financial summary cards.
3. A nested `players_table_UI` call for the scouted player roster table.

### Usage Example

```R
rivals_UI("rivals_explorer")
```

---

## 3. `rivals_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV)`

### Parameters

| Parameter          | Type       | Description                                                      |
|--------------------|------------|------------------------------------------------------------------|
| `id`               | `char`     | Unique namespace ID matching the UI module.                      |
| `is_module_active` | `reactive` | Boolean reactive indicating whether the tab/module is currently active. |
| `login_token`      | `reactive` | The user's authenticated session token (string).                 |
| `championship_id`  | `reactive` | The active championship ID (string).                             |
| `user_team_id`     | `reactive` | The logged-in user's own team ID. Forwarded to `players_table_Server` so clause-buying executes on the user's behalf. |
| `user_teams_RV`    | `reactive` | Data frame of all teams in the championship (columns: `teamid`, `teamname`). |

### Return Value

Returns `selected_player_RV`, a reactive supplied by the nested `players_table_Server` call. Contains the currently selected player data from the scouted roster table.

### Internal Reactives

| Reactive Name                    | Purpose                                                              |
|----------------------------------|----------------------------------------------------------------------|
| `selected_rival_team_id`         | Holds the team ID chosen by the user in the dropdown selector.       |
| `rival_financial_summary_box_RV` | Fetches financial stats via `get_user_team_info()` for the selected rival. |
| `rival_players_table_RV`         | Fetches the full squad via `get_players_from_team()`, computes `clause_ratio`, and pipes through `translate_player_positions()`, `calculate_player_changes()`, and `unify_columns()`. |

### Financial Summary Cards

The module renders four summary boxes in a fluid row:

| Card              | Data Field      | Source                     |
|-------------------|-----------------|----------------------------|
| Standings Position| `info$position` | `get_user_team_info()`     |
| Liquid Cash Budget| `info$budget`   | `get_user_team_info()`     |
| Active Bid Funds  | `info$withheld` | `get_user_team_info()`     |
| Squad Valuation   | `info$teamValue`| `get_user_team_info()`     |

### Clause Ratio Calculation

For each player in the scouted roster, the module computes:

```
clause_ratio = clause_price / value
```

This ratio drives the visual scouting indicators:
* **STEAL** -- clause_ratio is significantly below 1 (cheap buyout relative to player value).
* **GOOD VALUE** -- clause_ratio is close to 1 (fair buyout).
* **OVERPRiced** -- clause_ratio is well above 1 (expensive buyout).

The calculation is performed defensively: if `clause_price` or `value` columns are missing, `clause_ratio` defaults to `NA_real_`.

### Empty Roster Handling

If `get_players_from_team()` returns `NULL` or a zero-row data frame, the module returns a gracefully shaped empty data frame with the expected column schema:

```R
data.frame(
  id = character(0), name = character(0), role = character(0), role2 = character(0),
  value = numeric(0), change = numeric(0), points = numeric(0), buyPrice = numeric(0),
  clause_price = numeric(0), isClause = logical(0), clause_ratio = numeric(0),
  stringsAsFactors = FALSE
)
```

### Usage Example

```R
rivals_Server(
  id = "rivals_explorer",
  is_module_active = reactive(TRUE),
  login_token = reactive("USER_TOKEN"),
  championship_id = reactive("CHAMP_ID"),
  user_team_id = reactive("MY_TEAM_ID"),
  user_teams_RV = reactive(all_teams_df)
)
```