# Round MVPs Module

`round_mvps_UI(id)` renders the **Round MVPs** view. `round_mvps_Server(id, is_module_active, login_token, championship_id, refresh_trigger)` loads saved MVP selections only while its sidebar tab is active.

## Data source

`get_round_mvps(championship_id)` in `supabase_connector.R` reads `round_dream_team` with these filters: the selected `championship_id`, `is_mvp = true`, and `is_finished = true`. It returns a cached data frame ordered by descending `round_number`; an unavailable database read raises an error so the module can show a retry state.

The response uses `round_number`, `player_id`, `player_name`, `player_role`, and `points`. `round_mvp_rows(rows, championship_id)` validates those records, keeps one finished MVP per round, and orders the cards newest first.

## Usage

```r
round_mvps_Server("round_mvps", reactive(input$tabs == "round_mvps"),
  login_token_RV, championship_id_RV, refresh_trigger)
```

Use **Sync Round Dream Teams** in Admin to backfill completed rounds when the view reports no saved MVP records.
