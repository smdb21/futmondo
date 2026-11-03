# Classification and matchday results

`classification_UI(id)` displays round-window and single-round controls, standings, rank progression, published dream-team players and configured league rewards.

`classification_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger = NULL)` fetches cached per-team round records. A single selected round overrides the inclusive window; a global refresh updates the data. Cumulative ranks are computed from observed round points. When round history is unavailable, the UI explicitly labels current cumulative API totals and does not fabricate filtered results.

Dream-team requests use the selected round's immutable API ID, and MVP flags come from its published response. Reward configuration comes from `normalize_league_rules()`; no fixed reward amounts or calculated totals are presented as recorded earnings. Reconstructed totals are explicitly labelled separately in the standings.

See [UI reliability contracts](ui_reliability.md) for normalization parameters, input/output shapes, examples and focused tests. Run `Rscript --vanilla test/test_ui_reliability.R` for offline verification.

## Selected-round bonus totals (2026-11-03)

The standings contain four numeric euro columns: Points bonus, MVP bonus, Best XI bonus and Position bonus. Both the inclusive round slider and the single-round selector apply to every total; the single-round selector takes precedence. Position bonuses retain each actual round’s position allocation, not the rank within the selected window. Columns remain sortable and the Reactable scrolls horizontally on narrow screens.

`classification_bonus_table(standings, history, finances, window = NULL, single = "all")` appends `points_bonus`, `mvp_bonus`, `best_xi_bonus` and `position_bonus`. It consumes `cash_ledgers[[team_id]]` from the existing account/league-scoped `shared_rival_finance_source`, with its shared MVP source, and performs no I/O. All published rounds in the selection must have finite coverage-aware amounts for a total to display. Missing history, incomplete ownership, unscoped payments and unknown amounts remain unavailable. Current cumulative fallback standings do not claim filtered bonus amounts.

Each cash ledger now carries a `round_bonus_totals` data-frame attribute: `round_id`, `round_number`, `bonus_type` (`points`, `mvp`, `dreamteam`, `ranking`) and numeric `amount`. Amounts sum only included ledger entries, preserving recorded-payment replacement and signed values. Confirmed zero calculations survive as metadata when zero-valued reconstructed cash rows are removed. Verified award ownership establishes zero for non-recipients; an unknown owner cannot. Award coverage requires complete shared coverage or the full eleven unique published players, with the existing eligibility/rule checks. Missing evidence for one selected round prevents presenting a partial sum as a total. No ledger amounts, running balances, payment dates or persistence schemas are changed.

Opening Classification can initiate the same bounded asynchronous acquisition already used by Rivals. Navigation reuses shared session state; changing rounds only projects stored totals, with no extra requests, timers, cache invalidations or persistence. Regression: `Rscript test/test_classification_bonuses.R`.
