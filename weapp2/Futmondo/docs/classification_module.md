# Classification and matchday results

`classification_UI(id)` displays round-window and single-round controls, standings, rank progression, published dream-team players and configured league rewards.

`classification_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger = NULL)` fetches cached per-team round records. A single selected round overrides the inclusive window; a global refresh updates the data. Cumulative ranks are computed from observed round points. When round history is unavailable, the UI explicitly labels current cumulative API totals and does not fabricate filtered results.

Dream-team requests use the selected round's immutable API ID, and MVP flags come from its published response. Reward configuration comes from `normalize_league_rules()`; no fixed reward amounts or calculated totals are presented as recorded earnings.

See [UI reliability contracts](ui_reliability.md) for normalization parameters, input/output shapes, examples and focused tests. Run `Rscript --vanilla test/test_ui_reliability.R` for offline verification.
