# Joint profit scenarios

Implemented advisory workflow, 2026-09-08. `portfolio_engine.R` never dispatches account actions. Intelligence exposes a joint seven-day scenario, expected euro profit, downside, cash, purchases/sales, resulting XI and observed realized profit. Your Team uses the same league/season/scoring context and separates fantasy-point impact from money.

## Function contracts

* `profit_portfolio_inputs(roster, market, prices, offers, execution, as_of=Sys.time(), context=NULL)` accepts player frames with `id,value`, point/history fields, historical prices, current offers (`id,bid_id,bid_price`), a fitted sale distribution and league context. Returns enriched `roster`, `market` with `expected_sale_proceeds`, and `sales` with `player_id,proceeds,hold_proceeds`. Missing executable offers stay unknown.
* `build_profit_candidates(market, model, prices, execution, cash, max_bid, horizon=7, fees=0, as_of=Sys.time(), user_team_id=NULL, context=NULL)` returns supported positive-profit bid alternatives: `player_id,price,p_win,expected_profit,expected_proceeds,downside_profit,horizon,offer_observations`. It uses a 41-price grid and removes dominated options. Insufficient win-probability or sale evidence returns an empty frame.
* `plan_profit_portfolio(roster, market, candidates, financial, sales=data.frame(), rules=NULL, forecast_df=NULL, beam_width=64L)` accepts spendable cash and remaining roster capacity after commitments. Returns `status`, `reason`, purchases/sales, remaining cash, profit/downside, projected roster/XI, points change and execution availability. An unavailable result carries a reason. The bounded beam search is approximate, not a proof of global optimality. It reserves every recommended bid as if all win, and retains a no-bid option. Sales must settle before dependent bids; every intermediate sale retains a feasible XI. Unknown rules produce provisional advisory lineups and never authorize execution. There is no minimum points-improvement constraint.
* `realized_trade_ledger(transactions, user_team_id, season_start=NULL)` accepts `id,player_id,price,buyer_team_id,seller_team_id,created`; returns chronological sales with proceeds, acquisition cost, realized profit and counterparty. Source IDs deduplicate transactions. Unknown acquisition costs remain NA; empty counterparties mean Futmondo (System). Optional season filtering retains earlier purchases for cost basis.

```r
inputs <- profit_portfolio_inputs(roster, market, prices, offers, execution, context=context)
candidates <- build_profit_candidates(inputs$market, model, prices, execution,
  cash=finance$spendable_budget, max_bid=finance$api_bidding_limit, context=context)
plan <- plan_profit_portfolio(inputs$roster, inputs$market, candidates, finance,
  inputs$sales, rules=finance$lineup_rules)
```

The learned executable-price ratio initially uses at least five distinct observed system offers; it is descriptive evidence, not calibrated sale certainty. The single-player 95% input remains explicitly a user scenario. Joint downside is unavailable when a sale has no supported holding-value downside. Fees must be supplied where verified. The planner does not establish realized trading profitability or enable automation.

`recommend_transfers()` now requires expected sale proceeds and executable current sale prices, ranks expected euro gains and permits negative projected-points changes. Preview buttons carry account/league/team and immutable player IDs, then validate against the currently displayed recommendations.

`squad_lineup_comparison(submitted, optimized)` returns status, added/removed IDs and textual changes for available formation, assigned positions, captain and bench evidence. Numeric API slot IDs are not guessed. `optimize_starting_xi()` returns `legality_verified`, `legality_status`, assumptions and diagnostics; `club_limit_status` distinguishes unknown, unrestricted and numeric limited rules. A future `acquisition_effective_at` excludes a player before the verified deadline (or current time when deadline is unknown).
