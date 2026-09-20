# Reliable actions, finances, classification and squad planning

## Market action events

Direct sale to Futmondo emits `player_sold_direct` only after API-confirmed success. An unconfirmed direct-sale response emits `refresh` to reconcile roster and funds without inventing a completed transaction. Both use the shared invalidation callback; no local cash or ownership mutation is applied. See `test/test_direct_sell.R`.

`players_table_Server(..., refresh_trigger = NULL)` accepts the shared Shiny reactive value used to invalidate authenticated data. Its child player module emits `on_bid_updated(action_type, player_id, new_bid_price, is_cancel)` after success. The explicit action is one of `bid_modified`, `bid_cancelled`, `bid_placed`, `owner_offer_placed`, `clause_paid`, `player_listed`, `listing_cancelled`, `offer_accepted`, or `offer_rejected`. Legacy amount/cancellation arguments remain accepted, but they no longer overwrite unrelated bid/listing fields. The table increments its render trigger and the shared snapshot trigger; callers retrieve authoritative roster, market and financial data. Row selection captures the immutable player ID, so refreshed ordering cannot silently change a player action’s target.

Player-card opening uses a separate `player_selection_event_RV` counter. Every valid row-selection event records the immutable player ID, increments the event, opens the card, and clears only the reactable's visual selection with `updateReactable(..., selected = NA_integer_)`. The player identity remains available to the nested action handlers while the modal is open. Because the browser row is no longer left selected, clicking the same row after closing the modal emits a fresh event and reopens the card; table refreshes alone do not reopen it.

Market and roster persistence uses `defer_persistence(function, list(arguments))`. These UI reads do not wait for database writes. All API mutations remain explicit user actions. Bulk listing guards transport errors and refreshes the authoritative roster only after verified success; uncertain results never optimistically mark every player listed.

`selected_player_fis_row(player)` accepts a one-row data frame or named list and returns a one-row data frame. It preserves the already displayed descriptive rating; it calculates a rating only when absent. Missing component indicators remain `NA`. The player panel calls data coverage an observation-completeness measure, never forecast accuracy. Heuristic bid bounds/value gaps are not represented as winning probabilities or realized ROI.

```r
row <- selected_player_fis_row(data.frame(id = "p1", fis_score = 70))
stopifnot(nrow(row) == 1, row$fis_score == 70)
```

## Financial states

Today and Squad use `get_financial_snapshot()` for cash, withheld funds, spendable budget, legal bid limit and championship configuration. Zero and negative cash remain valid values. `ui_financial_amount(value)` accepts one numeric amount and returns formatted currency, or `"Unavailable"` for missing/nonfinite/non-scalar input. Current roster purchase cost is displayed as current investment rather than lifetime spending or total earnings.

## Lineups and hypothetical scenarios

The squad optimizer consumes the authoritative financial snapshot’s normalized `lineup_rules` and a one-round fantasy-points baseline. The formation selector includes automatic selection across supported legal formations. An infeasible XI is visibly invalid. Fetched roster and market rows are timestamped as current observations. `read_player_match_history(championship_id, final_only = TRUE)` supplies finalized point observations, with season/scoring/round identity used to deduplicate histories. One shared forecast reactive feeds lineup optimization, transfer simulations and recommendations; absent history uses the transparent aggregate-average baseline.

`squad_lineup_comparison(submitted, optimized)` accepts the cached lineup response (`players` data frame with immutable player IDs) and optimizer result (`feasible`, `starting_xi`). It returns `status = "ok"` plus `add`/`remove` ID vectors, or an explicit `unavailable`/`invalid` status and message. It does not submit the lineup.

Named transfer scenarios call `save_transfer_scenario(user_id, championship_id, user_team_id, name, scenario)` with `scenario = list(sell_ids, buy_ids, formation, mode)`. Loading calls `read_transfer_scenarios()` using the same user/league/team scope and restores selections. Current prices, cash, availability and roster constraints are recalculated. Unavailable saved player IDs remain visible so validation can explain stale scenarios. Invalid/unverified scenarios show diagnostics; estimated transfer proceeds are explicitly labelled. Saving/loading never trades players.

## Classification contracts

`classification_Server(..., refresh_trigger = NULL)` and `rivals_Server(..., refresh_trigger = NULL)` now participate in global snapshot invalidation.

`classification_round_rows(answer, team_id, team_name, rounds)` accepts per-team API round records (a list/data frame, optionally under `rounds`) and an optional catalog containing `round_id`/`round_number`. It returns `team_id`, `team_name`, numeric `round`, and numeric `points`. Supported explicit round fields are `round_number`, `roundNumber`, `number`, `round`, `id`, `_id`; opaque IDs require a catalog match. Missing points stay `NA` and duplicate team/round records retain the last observation. Empty/unrecognized responses return an empty typed data frame.

`classification_window_table(rows, window = NULL, single = "all")` returns per-team selected-window points, observed-round count and tied ranks. A selected single round takes precedence over the inclusive window. Missing observed scores and entirely absent team-round records propagate as unavailable. Every team is compared against the same published rounds in the selection; missing teams remain visible with unavailable rank. The plot recomputes cumulative ranks from actual round results. When history is absent, the table clearly labels current API cumulative totals, and does not pretend the controls filtered unavailable history.

`classification_dreamteam_rows(answer)` accepts `players` with `id` or `_id` identity plus an optional `mvp` ID or identity object and returns player ID/name/role, points and MVP flag. The UI queries the selected catalog round ID and displays the published players or an unavailable state. Reward amounts come from `normalize_league_rules()` and are shown as configuration, not invented recorded earnings.

```r
rows <- classification_round_rows(list(list(number = 1, points = 0)), "team1", "Team")
classification_window_table(rows, window = c(1, 3))
```

## Verification

Run `Rscript --vanilla test/test_ui_reliability.R`. It uses fixtures/mocked mutation functions and verifies signed/missing cash, score-row preservation, round filtering, dream-team missing/MVP values, submitted-lineup comparison, authoritative action refresh, cancellation IDs/actions, and UI controls. No live account or database mutations are involved.

## Lineup presentation

`squad_lineup_display_rows(players, captain_id = NA_character_, bench_order = character(), starting = FALSE)` takes optimizer player rows with immutable `id`, `name`, `pos_group`, optional `expected_points`, `forecast_lower`, `forecast_upper`, and `value`. It returns a display data frame containing `player`, assigned `position`, `expected_points`, `lower`, `upper`, `value`, and `selection`. Captain labels and ordered bench selections only use the optimizer’s explicit IDs; other non-starters are reserves. Numeric zero is retained; absent point estimates remain `NA`. Raw API fields are excluded.

```r
squad_lineup_display_rows(data.frame(id = "p1", name = "Player", pos_group = "DEF", expected_points = 0),
                         captain_id = "p1", starting = TRUE)
```

The lineup summary shows expected points separately from descriptive FIS, and reports optimizer diagnostics. Fixture optimization is hidden until a validated fixture adjustment exists. Transfer scenarios display the recomputed legal XI, formation, expected points, and captain; transfer recommendation cards use expected XI point gain rather than a descriptive score gap. The full offline lifecycle also renders starting, reserve, and projected lineup tables.

## Rival financial evidence

`rivals_buying_power_values(pressroom_df, teams, metric = "cash", start_date = NULL, end_date = NULL, initial_budget = NA_real_)` requires an explicit championship budget for a transfer-only balance estimate. Missing budget remains unavailable; valid zero/negative values remain signed. The estimate excludes unknown rewards and adjustments and is labelled accordingly. Official current cash is displayed from the account API, including zero or negative balances.

`rivals_observed_transfers(pressroom_df, rival_id)` accepts settled feed records containing buyer/seller IDs, price, creation timestamp and optional event/player IDs. It returns the existing money-movement table shape (`id`, `concept`, `type`, `category`, signed `money`, `date`, timestamp/batch metadata) with unknown historical balances as `NA`. Missing counterparty IDs represent the market. Known event IDs are deduplicated; invalid prices/timestamps are excluded. API restrictions produce observed transfers only, never invented starting-budget entries, round/ranking rewards, purchase dates or historical balances.

```r
rivals_buying_power_values(data.frame(), data.frame(teamid = "t1", teamname = "Team"), initial_budget = 0)
```


## All Players release-clause filters

The All Players table adds **Max Clause (M)**, which keeps only positive release clauses at or below the selected euro amount, and **Clause within available funds**. The latter uses the current verified debt-aware `spendable_budget`, including active-offer commitments; if that snapshot is unavailable, it returns no rows rather than guessing affordability.
