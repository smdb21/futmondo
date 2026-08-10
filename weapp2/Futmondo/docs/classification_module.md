# Classification & Matchday Rounds Module Documentation

This document describes the `Classification_Module.R` Shiny module, which presents cumulative standings classification, matchday round filtering, inverted rank position evolution charts, and matchday dream team rewards.

---

## 1. Module Overview

The Classification Module provides two exported functions:
* `classification_UI(id)` -- Renders matchday round range controls, inverted rank evolution plot, classification standings table, and dream team rewards box.
* `classification_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV)` -- Drives round window filtering, rank position progression calculations, and reactable standings.

---

## 2. Features & User Controls

### A. Matchday Round Selection & Window Filter
* **Round Range Slider** (`round_range_slider`): Allows users to select a window [X, Y] of completed matchday rounds (e.g. Round 1 to Round 15) to inspect points accumulated specifically within that matchday window.
* **Single Round Inspector** (`single_round_select`): Dropdown to inspect a specific matchday round.

### B. Rank Position Evolution Chart (`rank_evolution_plot`)
* Displays an interactive multi-line Plotly chart tracking each team's standings position across completed matchday rounds.
* **Inverted Y-Axis**: Configured with `autorange = "reversed"` so Rank 1 (1st place) is displayed at the top.
* **Empty State Handling**: If no matchday scores exist yet, displays a clean centered note: *"No matchday round data available yet."*

### C. Standings Classification Table (`classification_table`)
* Reactable table listing all user teams sorted by rank position.
* Columns:
  - `Rank #`: Current standings position.
  - `User Team`: Team name; former teams display an `Inactive` badge.
  - `Total Points`: Cumulative points earned across all matchday rounds.
  - `Point Earnings (€)`: Points multiplied by $70.000$ €/pt.
  - `Ranking Prize (€)`: Prize awarded based on the Futmondo rank ratio formula (`calculate_futmondo_ranking_prizes`).
  - `Total Money Earned (€)`: Sum of point earnings and ranking prize.
  - `Squad Value (€)`: Current market valuation of the team roster.

### D. Dream Team & Round Rewards (`dreamteam_box_ui`)
* Highlights matchday prize rewards: 1.000.000 € for the Matchday MVP and 500.000 € for each player in the Dream Team.