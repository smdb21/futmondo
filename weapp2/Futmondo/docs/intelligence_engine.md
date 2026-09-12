# Intelligence engine

The functions in `intelligence_engine.R` are pure local computations. They do not place bids, change a lineup, or write to an external service. Predictive models are documented separately in [prediction_engine.md](prediction_engine.md). All amounts use euros.

## Descriptive player indicators

`calculate_fis_score(players_df, weights = NULL)` returns the input dataframe with `perf`, `form`, `efficiency`, `momentum`, `fixture_risk`, `fis_score`, `fis_tier`, `fis_summary`, `data_coverage` and `fis_status` appended. Input fields are `id`, `name`, `points`, `value`, `change`, `average.average`, `average.averageLastFive`, `average.matches`, `role` and `status`; absent fields remain unavailable. `average.average` can be recovered from observed points/matches. Zero recorded appearances does not establish a usable average.

The indicators use fixed mappings, so a player's result is identical in a catalog, filtered table, roster, or detail view. Performance maps observed average `a` to `100*(1-exp(-max(a,0)/6))`; form maps the recent-average difference relative to `max(abs(a),1)` around 50; efficiency is `100*max(a,0)/(max(a,0)+value/1e6)`; momentum is `50+50*tanh(20*change/value)`. The legacy `fixture_risk` name now consistently means **availability quality**: healthy 100, doubtful 60, known injury/suspension 0, unknown NA. It does not measure an opponent's difficulty.

Roster, market, and catalog responses normalize nested API match statistics to the same `average.*` columns before scoring. Data coverage of 20% means **one of five** indicators is available, usually price momentum when no usable match average or availability status is supplied. With reported match averages, recent form, price/value data, and an unknown availability status, coverage is 80%; the rating uses those four observed indicators. The availability component says **Not reported** when the source status is blank/unknown. It does not imply injury. Missing match data and zero appearances still produce an unavailable overall rating.

Default weights are performance .30, form .20, efficiency .20, momentum .15 and availability .15. Named custom weights must be finite nonnegative scalar numerics; invalid entries use their default and weights are normalized. The weighted average includes observed indicators only. `data_coverage` is the proportion observed, not accuracy. No observed match average means `fis_score=NA`, `fis_tier="Unavailable"` and `fis_status="unavailable"`; incomplete inputs otherwise receive `fis_status="partial"`.

Historical tier names remain for existing table compatibility (`Strong Buy`, `Buy`, `Hold`, `Sell`). They are descriptive labels, not an economic recommendation or prediction. No confidence probability is derived from the rating.

```r
rated <- calculate_fis_score(players)
stopifnot(identical(rated$fis_score[1], calculate_fis_score(players[1, , drop=FALSE])$fis_score))
```

## Legal starting XI

```r
optimize_starting_xi(squad_df, formation="4-3-3", mode="max_fis",
                    rules=NULL, forecast_df=NULL,
                    locked_ids=character(), excluded_ids=character())
```

`squad_df` requires unique nonempty `id` and position information in `primary_role`, `role`, `role2`, `position` or list-column `eligible_positions`. English/Spanish names and GK/DEF/DF/MID/MF/MD/FWD/FW aliases work; comma, semicolon, slash and pipe delimiters preserve multiple eligible positions. Unless `rules$multiposition = TRUE` is explicitly enabled, only `primary_role` (falling back to the first primary `role`/`position`) is eligible, even when display text includes a secondary role. When explicitly enabled, all resolved positions remain eligible; callers should always supply the league setting. Unknown configuration defaults to primary-only eligibility. Optional club identifiers are read from `club_id`, `real_club_id`, `teamId`, then `team`.

`formation="auto"` evaluates every allowed formation. `rules` accepts `formations` (character vector), `club_limit`, `captain_enabled`, `captain_multiplier` (at least 1, default 2 when captain is enabled), `multiposition`, `bench_enabled`, `bench_size`, and `exclude_unavailable` (default TRUE). Default formations are 4-3-3, 4-4-2, 3-5-2, 3-4-3, 4-5-1, 5-3-2 and 5-4-1. A custom formation must have three nonnegative outfield counts totaling ten. An unknown requested formation fails explicitly. League adapters must supply their actual allowed formations and club/captain rules.

The exact assignment search respects eleven distinct players, every positional quota, club caps, locked/excluded players and known unavailability. It uses an admissible bound and deterministic tie ordering. It returns an infeasible result instead of filling missing positions with unrelated players. The reserve table is ordered by the selected objective. `bench_order` includes only available, eligible, nonexcluded reserves up to the configured size; a disabled bench or unknown/missing size produces an empty order without invalidating the XI. This is an advisory priority order, not a simulation of same-position automatic replacements; it does not require every optional slot to be filled. Automatic substitutions themselves are not simulated.

`forecast_df` accepts `player_id`, `expected_points`, optional `horizon` (next-round rows use 1), `lower` and `upper`. When available, the standalone prediction engine supplies the baseline. `expected`/legacy `max_fis` optimize expected points; `safe` uses the lower scenario, `upside` the upper scenario, `form` the observed recent average, and explicit `fis` the descriptive rating. `fixture` currently uses expected points; it does not invent an opponent adjustment. Without a forecast, observed average is a descriptive baseline. Missing safe/upside intervals use point averages for selection while displayed intervals stay unknown. Players without point estimates rank below those with estimates; FIS orders a wholly unobserved squad only. Unavailable expected points remain NA in results.

Return fields preserve `starting_xi`, `bench`, `formation`, `mode`, `total_score`, `avg_fis`, `feasible`, `formation_counts`; additions are `expected_points`, `lower_points`, `upper_points`, `captain_id`, `bench_order`, and `diagnostics`. Player rows add assigned `pos_group`, `opt_score`, `expected_points`, `forecast_lower`, and `forecast_upper`. Aggregate lower/upper values are sums of player scenarios, not calibrated joint prediction intervals. Captain multiplication applies to the chosen captain's score and points. Empty/invalid results have `feasible=FALSE` and diagnostics.

```r
forecast <- forecast_fantasy_points(roster, match_history, horizons=1)
xi <- optimize_starting_xi(roster, "auto", "expected",
  rules=list(formations=c("4-4-2", "3-5-2"), club_limit=3,
             captain_enabled=TRUE, captain_multiplier=2),
  forecast_df=forecast)
if (xi$feasible) print(xi$starting_xi)
```

## Transfer scenarios and recommendations

`simulate_transfer_scenario(squad_df, current_budget=0, sell_player_ids=character(), buy_player_ids=character(), market_df=NULL, rules=NULL, buy_prices=NULL, sale_proceeds=NULL, forecast_df=NULL)` returns a hypothetical roster, cash accounting, descriptive FIS changes and a newly optimized lineup. Explicit `buy_prices`/`sale_proceeds` are named numeric vectors keyed by player ID. Purchase references otherwise prefer `effective_market_price`, `price`, then `value`; sale references prefer `executable_sale_price`, `sale_offer`, then `value`. A value-based fallback is a **scenario assumption**, not verified proceeds.

Missing IDs, duplicate ownership, buying and selling the same player, missing prices and nonfinite budget fail with explicit `status`, `diagnostics`, `is_budget_valid=FALSE` and `is_lineup_valid=FALSE`. Different roster/market column sets are aligned safely. Signed and zero cash are preserved. `rules$roster_cap` constrains final size. A financially affordable scenario can still be invalid because it lacks a legal XI. Transfer legality permits injured players in valid positions (their next-round point scenario is zero), so it does not impose an unstated minimum sporting-quality constraint; tactical selection excludes known unavailable players by default. No error returns a valid empty squad.

Return fields: `projected_squad`, `total_sell_proceeds`, `total_buy_cost`, `projected_budget`, `initial_total_val`, `projected_total_val`, `initial_avg_fis`, `projected_avg_fis`, `delta_avg_fis`, `is_budget_valid`, `is_lineup_valid`, `status` (`ok`, `invalid`, `error`), `diagnostics`, `projected_lineup`, and `prices_verified` on successful calculation. This function does not authorize spending unsettled sale proceeds. Automation must use `evaluate_trade_basket()` and current verified balances.

`recommend_transfers(squad_df, market_df, current_budget=0, max_transfers=5, rules=NULL, forecast_df=NULL)` compares feasible individual sale/purchase scenarios against the best current XI. It recommends only positive **XI expected-point improvement**. Output preserves legacy sale/buy identifiers, labels, values and FIS fields, adding `delta_expected_points` and `points_per_million`. `roi_pct` is NA because fantasy-point gain is not a financial return. Rows are alternatives, not an independently executable multi-transfer basket. The profit-first planner uses the separate resale/profit model instead.

```r
scenario <- simulate_transfer_scenario(roster, current_budget=2500000,
  sell_player_ids="owned-id", buy_player_ids="market-id", market_df=market,
  buy_prices=c("market-id"=2000000), sale_proceeds=c("owned-id"=900000))
if (scenario$status != "ok") print(scenario$diagnostics)
```

## Legacy smart-bid compatibility

`calculate_smart_bid(player_row, championship_id, pressroom_df=NULL, user_teams_df=NULL, user_cash=NA_real_, market_high_bid=NULL, capacity=NULL)` remains a **descriptive heuristic**, not the rival-bid model. Its return includes `fair_value`, `league_premium_pct`, `min_winning_bid`, `recommended_bid`, `max_rational_bid`, `expected_roi_pct`, `competition_level`, `likely_competitors`, `spendable_funds`, `funds_verified`, `market_high_bid`, `minimum_bid`, `can_compete`, `action`, `reason`, `message`, `method`, `calibrated`, `confidence_pct`, and `data_coverage_pct`.

The recommendation never exceeds its rational ceiling or supplied spendable ceiling; `can_compete=FALSE` when the estimated required bid is above the ceiling. Only `capacity$status=="ok"` verifies funds. When supplied, `capacity$funds$api_bid_limit` also caps the rational/recommended amounts; an explicit unavailable/invalid API limit returns no bid. Legacy synthetic capacity objects may omit this field, while production capacity always supplies it. Missing, invalid or partial capacity returns a no-bid error with zero recommendation and unknown spending/rational limits, even if `user_cash` contains a number. Zero verified available funds remains a valid zero ceiling. `expected_roi_pct`, `confidence_pct` and `data_coverage_pct` are NA; `calibrated=FALSE`. The separate `valuation_discount_pct` is a descriptive valuation discount, never a profit forecast. New economic decisions use `fit_rival_bid_model()`, `forecast_resale_values()` and `select_profit_bid()`.

```r
advisory <- calculate_smart_bid(player, "league-id", capacity=verified_capacity)
if (isTRUE(advisory$funds_verified) && isTRUE(advisory$can_compete)) print(advisory$recommended_bid)
```

### Executable market minimum

`market_bid_minimum(player_row)` accepts a one-row player data frame or named list. It returns the whole-euro minimum as a numeric scalar: the greater of current `value` and the first positive finite listing price among `effective_market_price`, `market_price`, and `price`. Missing, empty, zero, negative, and non-finite prices are skipped; an entirely unknown minimum returns `NA_real_`. Numeric strings are supported.

`calculate_smart_bid()` preserves its independent form/injury valuation and enforces this market minimum before recommending an executable amount. `min_winning_bid` also covers the heuristic competition premium and any observed higher bid. If the minimum winning bid exceeds verified funds, the API bid limit, or the rational valuation ceiling, the result is `action="no_bid"`, `recommended_bid=0`, `can_compete=FALSE`, with an explanatory `reason` and `message`. The player card displays **No bid** and disables its Smart Bid button. It never presents a below-minimum amount as an actionable recommendation.

```r
market_bid_minimum(list(value=10000000, price=12000000)) # 12000000
advisory <- calculate_smart_bid(player, "league-id", capacity=verified_capacity)
if (identical(advisory$action, "bid")) {
  stopifnot(advisory$recommended_bid >= advisory$minimum_bid)
}
```

Offline regression coverage: `Rscript test/test_smart_bid_minimum.R`.

## Manager history and command-center feed

`calculate_manager_dna(team_id, pressroom_df, user_teams_df=NULL)` requires transaction `player_id`, `buyer_team_id`, `seller_team_id`, `price`, `created` and optional unique `id`. It returns observed trade count, trades per observed seven-day window, chronological FIFO holding periods, favorite purchased role where supplied, and average premium only when `market_value_at_time`, `reference_value` or `value_at_transaction` exists. No €25M baseline or synthetic overpayment is used. Sell-only histories work. Return fields are `team_id`, `aggressiveness`, `avg_overpayment_pct`, `fav_position`, `trading_frequency`, `avg_holding_days`, `total_trades`, `insights`, `premium_observations`, `method`. The aggressiveness compatibility indicator is `clamp(50+100*median(observed premium))`, not a probability. Missing counterparties remain the system.

`generate_command_center_feed(login, championship_id, user_team_id, user_teams_df, players_df, pressroom_df=NULL, market_candidates=NULL, clause_candidates=NULL)` preserves the existing explicit market/clause route policy and dataframe fields `type`, `title`, `description`, `confidence_pct`, `action_label`, `action_code`, `player_id`. `priority_score` orders descriptive suggestions; `confidence_pct` is unavailable. Empty supplied candidate tables are authoritative; NULL retains compatibility behavior. Neither this feed nor any tier performs an account action.

```r
profile <- calculate_manager_dna("team-id", pressroom)
feed <- generate_command_center_feed(NULL, "league-id", "team-id", teams, rated,
  market_candidates=market_candidates, clause_candidates=clause_candidates)
```

## Local helpers and verification

`analytics_numeric_column(df,column,default=NA_real_)` returns an n-row numeric vector with nonfinite observations replaced by the explicit default. `analytics_time(x)` parses ISO or space-separated UTC timestamps. `analytics_player_positions(squad_df,multiposition=FALSE)` returns a list of canonical eligible positions; only an explicitly true flag admits secondary positions. `analytics_known_unavailable(squad_df)` returns a logical vector for known injury/suspension and explicitly false logical `available` flags. Existing helpers `safe_numeric(x,default=NA_real_)`, `safe_clamp(x,lo=0,hi=100)`, and `default_if_null_na(x,default)` handle numeric conversion, bounds, and scalar defaults; they perform no I/O.

```r
roles <- analytics_player_positions(roster)
values <- analytics_numeric_column(roster, "value")
```

Run `Rscript --vanilla test/test_intelligence_correctness.R`. It tests contextual score identity, missing data, exact assignment, availability, positions, club/captain/lock constraints, cash and scenario validation, economically bounded heuristics, meaningful transfer deltas and observed manager history.

Rating input regression: `Rscript test/test_player_rating_inputs.R` verifies real parser output shapes through the selected-player panel without external requests.

When no executable bid exists, the result identifies the binding ceiling in `binding_constraint` and uses a specific `reason`: `no_spendable_capacity`, `api_bid_limit_zero`, or a `<constraint>_below_minimum` value. This distinguishes a zero balance/debt headroom from a zero limit reported by Futmondo and from the model valuation ceiling.

### Clause distribution and deadline affordability

Clause recommendations compare `clause_price / value` across every currently observed, valid rival clause. Eligible Buy/Strong Buy players are ordered from the lowest ratio upward; the feed flags the lowest quartile, with at least two candidates when available and a maximum of three. The card reports the candidate's empirical percentile and the clause premium as `N% above market value` when applicable.

`clause_deadline_affordability(clause_price, financial, roster_df, next_round)` returns `status`, `affordable`, `required_sales`, `sale_proceeds`, `sell_player_ids`, and `deadline`. `financial` must have `status="ok"` and a finite `projected_committed_balance`; `next_round` must have `available=TRUE` and `starts_at`. The helper first checks whether the clause leaves at least €1 at the deadline. If it does not, it selects the fewest highest current positive `bid_price` offers needed to restore a positive balance, while retaining at least 11 players after the incoming player joins. It never counts hypothetical future market sales. Results are `affordable_now`, `affordable_after_sales`, `unaffordable`, or `unverified`.

In the live Today feed, `generate_command_center_feed(..., financial, roster_df, next_round)` suppresses unaffordable and unverified clause actions. Calls omitting `financial` keep compatibility behavior for non-live consumers.

```r
plan <- clause_deadline_affordability(5000000, financial, squad, next_round)
feed <- generate_command_center_feed(NULL, league_id, team_id, teams, rated,
  market_candidates=market, clause_candidates=clauses,
  financial=financial, roster_df=squad, next_round=next_round)
```

Offline regression coverage: `Rscript test/test_clause_affordability.R` and `Rscript test/test_clause_value_wording.R`.

### Recommendation conflict resolution

A player cannot appear as both `Hold` and `Sell` in one command-center feed. If an asynchronous refresh or duplicate source record temporarily produces both actions for the same immutable `player_id`, `Sell` is retained because it is actionable and the conflicting `Hold` card is removed. Buy, Bid, and Clause cards retain their separate transaction semantics.

Focused regression coverage: `Rscript test/test_command_center_conflicts.R`.

### Sell recommendations with received offers

For an owned Sell-tier player with a finite positive `bid_price`, the command center creates one Sell card with `Accept Offer` and stable action code `accept_offer`. Its description includes the received amount. The generic Bid card is suppressed for that player, avoiding duplicate acceptance advice. A Sell-tier player without an offer remains `List on Market` with action code `view`.
