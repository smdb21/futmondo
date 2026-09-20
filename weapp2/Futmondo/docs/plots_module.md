# Plots Module

`plots_UI(id)` and `plots_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger = NULL)` provide the top-level **Plots** page. It presents one full-width, phone-friendly chart at a time: league standings, squad values, cash and transfers, rank progression, bid probability, and real-team ownership. Chart controls belong to their chart tab and do not alter Classification-table filters.

`plots_owned_by_real_team(players)` is a pure helper returning `real_team_id`, `real_team`, and `owned_players`. It counts each immutable player ID once only when both a current fantasy owner ID and a real-team ID/name are present. Free agents and rows with unverifiable club data are excluded. Rows are ordered by descending count then club name.

```r
plots_owned_by_real_team(data.frame(id = 'p1', user_team_id = 'manager-1', teamId = 'club-1', team = 'Club'))
# real_team_id real_team owned_players
# club-1       Club      1
```

For crowded multi-team history series, persistent legends are intentionally hidden so they do not shrink the phone plot; every trace retains its team label in its hover tooltip. Focused verification: `Rscript test/test_plots_module.R`.
