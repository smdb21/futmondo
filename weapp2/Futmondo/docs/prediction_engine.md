# Offline prediction engine

`prediction_engine.R` contains pure base-R computation. Sourcing it loads no packages, makes no requests, reads no account state, and writes no files. Callers obtain cached observations through the application's existing adapters and persist forecasts separately. Run `Rscript --vanilla test/test_prediction_engine.R` for its focused semantic tests.

The objective for bidding is expected profit from conservative resale proceeds. Fantasy-point forecasts are separate from financial value. A trade basket must retain a legal XI; it does not impose a minimum points improvement.

## Data contracts and interpretation

Partition inputs by championship and by the competition, season, and scoring configuration relevant to the forecast. IDs are character strings and money is in euros. Timestamps accept UTC ISO strings, `POSIXct`, `Date`, Unix seconds, or numeric Unix milliseconds. Unknown values are `NA`; unobserved bids are never represented as zero.

`as_of` is the information cutoff. Auction and bid rows require both `settled_at` and `observed_at` at or before this cutoff. Match and price observations require `observed_at`; optional `captured_at` must also precede the cutoff. `observed_at` must represent when the datum was actually available, not a backdated event label. A database's collection timestamp should be supplied as `captured_at` when it differs. Current player aggregates and values need a matching `observed_at`; do not backdate a current API snapshot for historical evaluation.

Intervals and win curves are model-based scenarios, not measured predictive accuracy or calibrated confidence. Keep `method`, sample counts, freshness, and missing values visible. A completed-purchase feed alone does not establish complete auction opportunities or prove that a missing manager did not bid. Pass observed unsold expiries when coverage and eligibility are verified.

## Auction evidence and rival models

### `normalize_prediction_auctions(auctions, captured_at = Sys.time())`

Accepts a data frame with the following columns:

| Column | Shape and meaning |
| --- | --- |
| `championship_id` | Championship key; defaults to `""` for a caller-partitioned input. |
| `auction_id` | Stable source event key; `event_id` and `id` are accepted aliases. |
| `player_id` | Player key; repeated listings retain distinct auction IDs. |
| `settled_at` | Settlement timestamp; `created` is an alias. |
| `observed_at` | Capture timestamp; `captured_at` is an alias and the function argument is the default. |
| `winner_id` | Winning manager; `buyer_team_id` is an alias. Empty/NA means the Futmondo system, never a manager. |
| `winning_bid` | Settled price; `price` is an alias. |
| `reference_value`, `reference_observed_at` | Value known strictly before settlement and its timestamp; optional. |
| `visibility` | `complete`, `partial`, or `unknown`; `bid_visibility` is an alias. Optional logical `bids_visible` takes precedence. |
| `eligible_manager_ids` | List column of managers actually eligible for each opportunity; absent coverage stays unknown. |
| `bids` | List column containing per-auction data frames or lists of bid records, each with `manager_id`/`bidder_id`/`team_id` and `amount`/`price`. `list()` means observed empty; `NULL` means unavailable. |

Returns `list(auctions, bids, issues)`. The canonical auction frame uses the names above and numeric UTC seconds. The bid frame has `championship_id`, `auction_id`, `player_id`, `manager_id`, `amount`, `is_winner`, `settled_at`, and `observed_at`. `issues` is a character vector of parsing problems. Invalid nested bids downgrade visibility. It reconstructs the winner from the settlement, overrides a repeated winner's nested amount, and deduplicates captures by championship/event/bidder. Missing source IDs are rejected from the normalized result. This adapter expects already canonical bid fields; the API-specific pressroom adapter handles raw nested `_buyer`, `_seller`, and `_team` objects.

```r
events <- data.frame(championship_id = "league", auction_id = "sale-1",
  player_id = "p1", settled_at = "2026-08-01T12:00:00Z",
  winner_id = "m1", winning_bid = 1100000, visibility = "complete")
events$eligible_manager_ids <- I(list(c("m1", "m2")))
events$bids <- I(list(list(list(manager_id = "m2", amount = 1050000))))
observations <- normalize_prediction_auctions(events,
  captured_at = as.POSIXct("2026-08-01 12:01:00", tz = "UTC"))
```

### `fit_rival_bid_model(auctions, bids, manager_ids = NULL, as_of = Sys.time(), prior_strength = 20, context = NULL)`

Accepts canonical auction and bid frames, including directly persisted observations. `event_id`/`auction_id`, `bidder_id`/`manager_id`, and `bid_visibility`/`visibility` aliases are supported. Nested `bids` are unnecessary when the separate bid frame is provided. `manager_ids`, when supplied, selects current league members for model output, including managers with no bids; supply every rival to avoid omitting a cold-start opponent. Historical inactive managers remain pooled training evidence but are excluded from the current win product. `prior_strength` is a positive number of pooled pseudo-observations, initially 20, which must be chosen using chronological evaluation before claiming validation.

Participation uses only fully visible opportunities with an explicit eligible-manager list. The league pooled probability has a Jeffreys half-count correction. Individual probabilities use a beta posterior centered on that league rate. Partial-auction positive bids remain usable for conditional amount estimates but do not create selected non-bid labels. When no complete eligible denominator exists, participation and resulting win probabilities are `NA`.

Conditional amounts use `log(amount / reference_value)` only when the reference timestamp is strictly before settlement. Each manager's mean shrinks toward the pooled mean; residual variance is shared initially. Fewer than two usable pooled prices cannot support an amount distribution. A 0.01 log-scale variance floor prevents a numerically degenerate CDF; it is an explicit conservative modeling default, not evidence of uncertainty calibration.

Returns `list(managers, metadata)`. Manager columns:

- `manager_id`, `participation_probability`, `participation_low`, `participation_high` (80% marginal beta interval).
- `eligible_opportunities`, `observed_participations` (matching denominator), `observed_bids` (all valid positive observations), `premium_samples`, and `last_observed_at` (latest known bid capture in UTC seconds, or NA for a cold start).
- `log_ratio_mean`, `log_ratio_sd`, `log_ratio_mean_se`, `conditional_bid_ratio_median`, `conditional_bid_ratio_low`, `conditional_bid_ratio_high` (conditional 10th/90th percentiles).
- `method`: `shrunk_history`, `pooled_prior`, `insufficient_opportunity_visibility`, or `insufficient_historical_values`.

Metadata contains cutoff, prior strength, deduplicated auction/bid counts, usable premium count, pooled participation, and interval assumptions.

```r
model <- fit_rival_bid_model(observations$auctions, observations$bids,
  manager_ids = c("m1", "m2", "new-manager"),
  as_of = as.POSIXct("2026-08-02", tz = "UTC"))
model$managers[, c("manager_id", "observed_bids", "method")]
```

### `predict_auction_win_curve(model, bid_grid, reference_value, user_id = NULL, active_bids = NULL, ties = "lose")`

`model` is the fitted list. `bid_grid` contains candidate nonnegative euro amounts; `reference_value` is the current positive valuation. `user_id` excludes the user from competitors. Optional `active_bids` has `manager_id`/`bidder_id` and `amount`/`price`; an observed participant is certain to have participated, and its visible amount conditions its final amount from below. `ties` selects strict (`lose`, the default) or non-strict (`win`) comparison at the visible bound. The initial continuous amount distribution does not estimate a point mass at exact euro ties; do not claim measured tie-winning probabilities from it.

Returns a sorted data frame with `bid`, `p_win`, `p_win_low`, `p_win_high`, and `method`. Zero means no bid and has zero win probability. The formula is the product of each rival's nonparticipation probability plus participation times its amount CDF. This assumes conditional independence. Bounds combine marginal participation and amount-mean uncertainty into a sensitivity envelope; they are not a joint calibrated 80% confidence interval. Missing required coverage or amounts returns `NA` probabilities, not 50% placeholders. An active rival absent from the fitted manager list also forces abstention. Verified active participation can override unknown historical participation when the amount model is available.

```r
curve <- predict_auction_win_curve(model, seq(900000, 1400000, 10000),
  reference_value = 1000000, user_id = "m1")
```

## Fantasy points and resale values

### `forecast_fantasy_points(players, history = NULL, as_of = Sys.time(), horizons = c(1, 3), prior_strength = 5, context = NULL)`

`players` has `id`/`player_id`, optional `role`, `status`, logical `available`, timestamp `observed_at`, `average.average`/`average_points`, and `average.matches`/`matches`. `history` has `player_id`/`id`, `points`, `observed_at`, and optional `round_id` (or `round`), `played`, `role`, `captured_at`, `is_final`, and `score_status`. If finality fields are present, only explicitly final observations enter training. Explicit `played = FALSE` produces zero points; missing points with unknown participation are excluded. Revisions are deduplicated by player/round, retaining the last known observation. Without a round key, observation time is the deduplication key, so callers should provide `round_id` for revisions.

`horizons` are positive whole rounds. `prior_strength` is a positive role-prior weight, default 5. The baseline gives history a 35-day recency half-life and shrinks its weighted average toward observed role peers, falling back to the league if role data is unavailable. Aggregate-only averages are labeled and have no invented interval. A player without usable individual data can receive a role prior; with no usable source at all, points stay `NA`. Known current injured/suspended/unavailable statuses from a snapshot available at the cutoff give a next-round exclusion scenario of zero; longer horizons stay unknown without a return date. An unknown status remains `availability = "unknown"`.

Returns one row per player/horizon: `player_id`, `horizon`, `expected_points`, `lower`, `upper`, `n_observations`, `effective_observations`, `availability`, `method`, `observed_at`, `as_of`. Intervals are approximate 80% predictive ranges, including baseline mean uncertainty. Negative fantasy scores remain possible; they are not clamped to zero. Multi-round ranges assume independent outcome noise and a shared uncertain mean. These forecasts contain no opponent, xG, captain, or fixture effect; the lineup optimizer applies legal configuration separately.

```r
now <- Sys.time()
players <- data.frame(id = "p1", role = "MF", status = "ok",
  average.average = 5.5, average.matches = 8, observed_at = now)
forecast_fantasy_points(players, as_of = now)
# An aggregate baseline, with unknown intervals until round observations exist.
```

### `forecast_resale_values(players, history = NULL, as_of = Sys.time(), horizons = c(1, 3, 7), execution_ratio = NULL, fees = 0, prior_strength = 5, context = NULL)`

`players` has `id`/`player_id`, current `value`, and `observed_at`. `history` has `player_id`/`id`, positive `value`, `observed_at`, and optional `captured_at`. `horizons` are positive days. `execution_ratio` is either one scalar, one value per input player, or a named vector indexed by player ID; its permitted range is `(0, 1]`. Supply a verified executable fraction or clearly label a user-entered scenario assumption. `NULL`, unknown, or invalid ratios yield unknown proceeds. `fees` is a nonnegative fixed euro cost per future sale. `prior_strength` is positive pooled drift weight in observed days.

The model uses log returns from observations spaced at least one day apart, accounts for irregular intervals, and shrinks player drift toward league drift. It requires at least three pooled return observations for an interval and uses a minimum daily log-volatility of 0.005. If evidence is insufficient, it returns the last observed value as a labeled no-change baseline and cannot produce conservative proceeds. Stale observation age is included in the extrapolation horizon. Current prices with absent/future timestamps cannot leak into a historical forecast.

Returns `player_id`, `horizon`, `expected_value`, `lower_value`, `upper_value`, `conservative_proceeds`, `execution_ratio`, `fees` (explicit scenario sale cost), `n_observations` (usable own returns), `pooled_observations`, `observed_at`, `as_of`, and `method`. Conservative proceeds are `max(0, lower_value * execution_ratio - fees)`, using the model's 10th percentile value. A modeled valuation is never itself a guaranteed sale offer. Expected values use the lognormal mean; bounds use lognormal 10th/90th percentiles.

```r
now <- Sys.time()
prices <- data.frame(player_id = "p1", value = c(100, 102, 101, 104, 103) * 1e4,
  observed_at = now - (4:0) * 86400)
players <- data.frame(id = "p1", value = 1030000, observed_at = now)
resale <- forecast_resale_values(players, prices, as_of = now, execution_ratio = 0.95)
```

## Profit choices and joint feasibility

### `select_profit_bid(win_curve, conservative_proceeds, spendable_cash, max_bid = Inf, fees = 0)`

`win_curve` has `bid` and `p_win`; all its other columns are retained. `conservative_proceeds` is one nonnegative euro estimate, already net of any sale fees deducted by the resale model. `spendable_cash` is verified available funding after commitments; `max_bid` is an additional legal/economic cap. `fees` means additional acquisition costs, so do not pass sale costs twice.

Evaluates `p_win * (conservative_proceeds - bid - fees)` on the caller's legal candidate grid, discards unaffordable/unsupported candidates, and chooses the highest positive expected profit. Equal-profit candidates choose the lower bid. Unknown economics or nonpositive profit yields a zero/no-bid recommendation. This helper does not choose a confidence threshold or optimize multiple auctions jointly; validate the whole proposed basket below before action.

Returns `recommended_bid`, `expected_profit`, `win_probability`, `action` (`bid`/`no_bid`), machine-readable `reason`, and `candidates` with `expected_profit`.

```r
choice <- select_profit_bid(data.frame(bid = c(500000, 800000), p_win = c(0.5, 0.9)),
  conservative_proceeds = 1000000, spendable_cash = 900000, max_bid = 850000)
stopifnot(choice$recommended_bid == 500000)
```

### `evaluate_trade_basket(roster, purchases = data.frame(), sales = data.frame(), verified_cash, committed_cash = 0, max_roster = Inf, formations = NULL, legal_check = NULL)`

`roster` contains unique `id`/`player_id` and the attributes required by the legality checker. `purchases` contains unique player IDs, their roster attributes, and verified `price`/`bid`/`amount`; all purchases are budgeted as if they win. `sales` contains owned player IDs, `proceeds`/`price`, and `completed = TRUE`. Unsettled sale promises cannot fund the basket. For an explicitly labeled scenario, setting `completed = TRUE` represents the prerequisite that those sales settle first; it does not execute them.

`verified_cash` is the cash balance *before* the proposed scenario's sales/purchases and before deducting `committed_cash`. Do not start with an already-net spendable balance and subtract the same commitments twice, or add sale proceeds already included in that balance. `max_roster` is the league capacity; unknown capacity defaults to unbounded for analysis, and the action preflight must verify actual limits. `formations` optionally limits formation candidates when using the loaded optimizer. `legal_check` is a pure function accepting the final squad and returning a scalar logical or `list(feasible = TRUE/FALSE, ...)`. Pass a closure with the verified league rules. When absent, the helper uses an available `optimize_starting_xi(..., formation = "auto", rules = list(exclude_unavailable = FALSE))`; without any checker it fails closed. This default verifies a legally shaped roster, allowing injury flags because they do not remove positional registration. Tactical forecasts separately treat known injury as a next-round exclusion scenario. A custom checker can apply the exact verified league rules.

Returns `valid`, character-vector `reasons`, `cash_remaining`, the merged final `roster`, and `lineup` (checker result). It rejects duplicate/unknown ownership, missing prices, unresolved sale funds, an overdrawn full basket, capacity violations, fewer than 11 players, an illegal XI, and checker errors. It aligns differing roster/market columns and imposes no points-preservation condition. It is a scenario validator, not an account-writing function.

```r
squad <- data.frame(id = paste0("p", 1:12), role = c("GK", rep("DF", 4), rep("MF", 4), rep("FW", 3)))
result <- evaluate_trade_basket(squad, verified_cash = 2000000,
  committed_cash = 500000, max_roster = 25,
  legal_check = function(x) optimize_starting_xi(x, formation = "auto", rules = verified_rules))
```

## Private helpers and validation boundary

Private `.pe_*` functions are implementation details: `.pe_num(x)` coerces vectors to numeric; `.pe_col(x, names, default)` chooses the first available data-frame column; `.pe_time(x)` returns numeric UTC seconds; `.pe_ids(x)` returns distinct nonempty IDs; `.pe_key(championship, event)` builds composite keys; `.pe_empty_bids()` returns a typed zero-row bid frame; `.pe_past(x, as_of, time_columns)` filters a data frame using every required timestamp column. For example, `.pe_time("2026-08-01T12:00:00Z")` returns Unix seconds. None performs I/O.

Deterministic tests cover winner reconciliation, visibility/eligibility, duplicated captures, historical cutoffs, cold start, monotonic win curves, observed active bids, self-exclusion, missing versus did-not-play points, shrinkage, injury scenarios, revision deduplication, resale horizons, fees, unknown execution proceeds, whole-basket commitments, legal-XI preservation, schema differences, and explicit failure states. They establish implementation correctness, not profitable performance.

Before automated acquisition is enabled, freeze forecasts before settlement/kickoff and run expanding-window evaluation by settlement day or round. Keep auctions and their bidders in the same split. Compare participation Brier/log loss, bid quantile loss and coverage, win calibration, point prediction error, lineup regret, price forecast error, and realized net profit against pooled-premium, no-change, simple lineup, and no-trade baselines. Shared auction-demand effects and market regimes are not modeled yet. Persist model/version, input cutoff, scenario assumptions, and realized outcomes in the caller's existing observation layer.


## Chronological auction evaluation

### `rolling_bid_backtest(auctions, bids, min_train_dates = 3, as_of = Sys.time(), bid_ratios = c(1, 1.05, 1.1, 1.25), user_id = NULL, prior_strength = 20, context = NULL)`

Inputs use the same auction and bid schemas as `fit_rival_bid_model`. `min_train_dates` is the positive minimum number of distinct earlier UTC settlement dates. `as_of` bounds observations available for the entire evaluation. `bid_ratios` is a positive candidate grid fixed before looking at test outcomes, multiplied by each auction's earlier known reference valuation. An optional `minimum_bid`/`reserve_price` removes known invalid candidates. `user_id` excludes the real user from rival comparison; absent it, the comparison is a hypothetical bid against all observed participants. `prior_strength` configures each fold's model.

The function freezes training at midnight before each test settlement day, admits only earlier settlements and captures already available then, and keeps every bidder from an auction in the same fold. Test manager eligibility and historical reference values must describe information available for that auction; the function never substitutes today's values. Late imports do not become invented earlier observations. Fully visible, eligible test auctions produce participation and fixed-grid win outcomes. Positive bid labels from partial observations can support amount evaluation, while unknown non-bids cannot.

Returns a list:

- `status`: `evaluated`, `insufficient_history`, or `insufficient_test_coverage`.
- `folds`: cutoff, training-date count, training-auction/bid counts, test-auction count, and latest training settlement.
- `participation`: cutoff, championship/auction/manager IDs, forecast probability, observed participation, and pooled-prior baseline.
- `premiums`: cutoff and IDs, predicted/observed log bid-to-value ratio, and lower/upper model quantiles.
- `wins`: cutoff, championship/auction IDs, predetermined bid, forecast win probability, and observed strict win outcome.
- `metrics`: rows with `metric`, `value`, and actual `observations`; includes participation Brier and log loss, pooled participation Brier, premium log MAE and 80% interval coverage, lower/upper pinball loss, and fixed-grid win Brier when supported.
- `assumptions`: a description of the split and interval interpretation.

```r
evaluation <- rolling_bid_backtest(observations$auctions, observations$bids,
  min_train_dates = 3, bid_ratios = c(1, 1.05, 1.1, 1.25))
evaluation$status
evaluation$metrics
```

This first evaluator measures held-out prediction correctness. It does not certify a profitable automated strategy or estimate realized resale outcomes. The fixed grid may have unknown auction-specific constraints when no reserve was captured. Maintain those caveats in reporting; automated actions still require verified league limits, executable proceeds, and separately validated economic performance.


## Chronological point and resale evaluation

`rolling_point_backtest(players, history, min_train_rounds=3, as_of=Sys.time(), prior_strength=5, context=NULL)` accepts the point-forecast inputs plus required `history$round_id` and `round_start_at` (verified first kickoff for each round). Partition inputs by league, season, and scoring configuration. Predictions freeze before the whole round's first kickoff; both point observation time and optional capture time must be earlier than cutoff. Current aggregate statistics and availability cannot enter older folds without a historical snapshot timestamp. Final held-out round scores are deduplicated by player/round using the latest available revision. The baseline is each player's last five observed rounds' arithmetic mean, with league mean for a new player. `min_train_rounds` is a positive whole count of earlier observed rounds.

Returns `list(status, folds, predictions, metrics, assumptions)`. `status` is `evaluated`, `insufficient_timing`, `insufficient_history`, or `insufficient_test_coverage`. `folds` records cutoff, round, training count, test count, and latest training observation. `predictions` records cutoff, player and round, prediction, observed points, lower/upper bounds, baseline, and method. `metrics` contains mean absolute error, mean squared error, baseline MAE, 80% interval coverage and lower/upper pinball loss; each row includes its actual observation denominator. Without verified kickoff timestamps it returns `insufficient_timing`, rather than inventing retrospective cutoffs from capture times.

```r
point_validation <- rolling_point_backtest(roster, timestamped_final_rounds,
  min_train_rounds=3, as_of=Sys.time())
point_validation$metrics
```

`rolling_resale_backtest(players, history, min_train_dates=4, as_of=Sys.time(), horizons=c(1,3,7), tolerance_hours=12, prior_strength=5, context=NULL)` accepts the resale-forecast player/price frames. `players` selects player IDs; current prices are not used in historical folds. Training expands by UTC day and admits only prices and optional captures available strictly before midnight cutoff. The snapshot is each player's latest eligible training price. Targets use the first actual price at/after the requested day horizon within `tolerance_hours` (0–24), with unsupported outcomes skipped. The no-change baseline is the last price available before cutoff. `min_train_dates` is a positive whole count.

Returns the same list shape. `folds` contains cutoff, training dates/count, held-out count and latest training observation. `predictions` contains cutoff, player, horizon, target and actual outcome timestamps, prediction, observed valuation, lower/upper bounds, baseline and method. Metrics match the point evaluator and are reported separately for each horizon. This measures valuation error, **not realized trading profit**; executable-sale labels and recorded trades are required for economic validation.

```r
price_validation <- rolling_resale_backtest(catalog, observed_prices,
  min_train_dates=4, horizons=c(1,3,7))
price_validation$metrics
```

Private `.pe_validation_metrics(predictions)` accepts a data frame with `prediction`, `observed`, `lower`, `upper`, and `baseline`, and returns metric/value/observation-count rows. It performs no I/O. The evaluators use it to avoid counting unavailable intervals as measured coverage. Tests verify whole-round isolation, baseline arithmetic, final-outcome perturbations, unavailable kickoff times, late-import refusal, daily cutoff boundaries, and target-time tolerances.


## Multiple leagues and scoring contexts

The bid fit, point/resale forecasts, and all three chronological evaluators accept an optional final `context = list(championship_id, season, scoring_version)` argument. Each supplied value must select one nonempty identifier. These are partitions, not modeling predictors: points from one scoring configuration must never act as evidence for another configuration. League reward/bonus configuration is retained by the dashboard alongside every prediction; point forecasts do not treat bonuses as fantasy points or automatically monetize them.

Frames may carry matching `championship_id`, `season` and `scoring_version` columns. Explicit context selects matching labeled rows. Without explicit context, a single context in the first input frame is selected; otherwise a unique context may be inferred from all frames. Ambiguous mixed contexts raise an error instead of averaging incompatible data. A frame without a context column is accepted only as caller-partitioned input; this compatibility path cannot verify historical provenance, so production readers must supply verified labels or isolate their queries. Unknown historical labels are never rewritten as current season/scoring labels in the dashboard.

Point/resale outputs include resolved context columns, rival-fit `metadata$context` records the partition, and evaluator results contain `context`. The optimizer ignores explicitly foreign forecast rows sharing a player ID and refuses mixed-context squads. Transfer scenario and basket validators reject known cross-league, cross-season or cross-scoring transactions, including shared player IDs.

```r
context <- list(championship_id="league-A", season="2026", scoring_version="configured-media-hash")
points <- forecast_fantasy_points(roster, final_round_observations,
  context=context, horizons=c(1,3))
rivals <- fit_rival_bid_model(auction_observations, bid_observations, context=context)
validation <- rolling_resale_backtest(catalog, observed_prices, context=context)
```

Private `.pe_context_data(frames, context=NULL, columns=c("championship_id","season","scoring_version"))` accepts a named list of data frames, an optional named context list and context-column names; it returns `list(data, context)` containing filtered frames and the resolved scalar partition. `.pe_add_context(df, context)` returns the data frame with resolved context columns appended. Neither performs I/O.

`Rscript --vanilla test/test_multileague_analytics.R` checks identical player, round, manager and auction IDs in two leagues with distinct scoring, captain, formations and bonus values; cross-context transfer rejection; context-specific historical evaluation; in-session selection/scenario reset; and persisted rule attribution. These tests establish isolation, not forecast profitability.
