# Intelligence dashboard

`Modules/Intelligence_Module.R` provides a read-only Shiny dashboard for historical rival bids, estimated bid-win curves, 1/3/7-day resale scenarios, profit-oriented bid comparison and the best legal XI. It uses the existing Bootstrap fluid grid and does not add a dependency or perform account writes.

## Public interface

- `intelligence_UI(id)` returns namespaced Shiny tags. The page has a market-player selector, refresh button, rival table/curve, resale horizon and percentage/fee scenario controls, profit candidates and a lineup table.
- `intelligence_Server(id,is_module_active,login_token,championship_id,user_team_id,refresh_trigger=NULL,loaders=NULL)` registers the module. Identity/active arguments may be reactive functions or direct values. `loaders` optionally maps existing service function names to injectable functions for offline testing. Return value is a list of reactives `data`, `model`, `curve`, `profit`, `lineup`, and `point_forecasts`.
- `intelligence_value(x)` resolves a reactive/function or passes a direct value through.
- `intelligence_prepare_auctions(auctions,bids,price_history=NULL,championship_id="")` returns `list(auctions,bids)` aligned with the model contract. It accepts event IDs as `id`/`event_id`/`auction_id`, bidder IDs as `manager_id`/`bidder_id`, settlement aliases and capture aliases. Missing visibility remains unknown and missing eligible managers remain an empty list. Missing reference values may be joined to the **latest strictly earlier** player-price observation at most 24 hours before settlement; a supplied capture timestamp must also precede settlement. No current price is used to normalize an old auction.

## Inputs and outputs

Loaders use `get_market_players`, `get_players_from_team`, `get_financial_snapshot`, `read_player_price_history`, `read_model_auctions` (preferred), `read_auction_observations`, `read_bid_observations`, `get_teams`, and optional `read_player_match_history`. When stored auctions are absent, the module can use `get_championship_pressroom` and `normalize_bid_observations`. It does not infer participation negatives from a transaction list. Existing cached API handlers remain responsible for freshness and isolation; dataframe `observed_at` metadata is propagated to model inputs. Display timestamps identify loading, not model calibration.

Financial input is a list with `status`, `spendable_budget`, `legal_bid_limit`, `roster_cap`, and optional normalized `lineup_rules`. Only status `ok` permits an authoritative cash limit. A profitable model suggestion must also pass `evaluate_trade_basket()` for the new purchase and legal XI. Unknown roster capacity suppresses an actionable bid. Existing commitments must already be reflected in the provided spendable balance.

Auction and bid frames follow [prediction_engine.md](prediction_engine.md). Price history requires `player_id`, `value`, `observed_at`. Player data uses the existing catalog/roster shape; current frame observation time is read from its column or metadata. Raw player names and team names are rendered as text. Current league managers are included even if they have no historical bids.

Future participation and bids are labeled estimates; actual post-auction observations are separately counted. Model intervals are explicitly uncalibrated. A missing/incomplete store results in useful empty states, not fabricated ratings or probabilities. The default resale offer assumption is 95% of future value, editable from 1–100%; it is a user scenario, **not a verified future offer**. Scenario fees are nonnegative. Conservative proceeds use the resale model's lower scenario and explicit offer assumption. A no-bid result is valid when the evidence or projected profit is insufficient.

```r
intelligence_UI("intelligence")
intelligence_Server("intelligence",
  is_module_active=reactive(input$tabs == "intelligence"),
  login_token=login_token, championship_id=championship_id,
  user_team_id=user_team_id, refresh_trigger=refresh_version)

# Standalone data preparation for offline modeling:
prepared <- intelligence_prepare_auctions(auction_rows,bid_rows,price_rows,"league-id")
model <- fit_rival_bid_model(prepared$auctions,prepared$bids)
```

## Focused checks

`Rscript --vanilla test/test_intelligence_module.R` uses injected offline loaders and `shiny::testServer()` to check controls, historical-only price joins, unknown-visibility preservation, actual table/model rendering, unavailable finances and empty stores. `test/test_intelligence_correctness.R` covers the lineup/transfer implementation and `test/test_prediction_engine.R` the forecast math.


## Forecast evidence and persistence

The rival table shows selected-player conditional median and 10th/90th percentile bids in euros, alongside participation estimates, sample counts, historical-value references and latest bid-capture time. The best-XI page displays an advisory configured eligible bench order (optional reserve slots are not forced to fill) and one/three-round point forecasts with evidence counts. Unknown bench capacity produces an explicit unavailable state; no arbitrary reserve count is submitted.

When `login_token()[["userid"]]` is available, an observer calls the injectable `persist_forecast(user_id, championship_id, player_id, model_version, forecast_type, cutoff, horizon, prediction, season=NULL, scoring_version=NULL)` provider. The application's provider queues private snapshots outside the request's network path. Types and versions are `fantasy_points`/`points-shrinkage-v1` (horizons 1 and 3), `resale_value`/`resale-log-return-v1` (1, 3 and 7 days), and `rival_bid`/`rival-bids-v1` (next auction, horizon 1). Point/resale `prediction` payloads contain the complete named forecast row, including uncertainty and explicit scenario assumptions; rival payloads contain `reference_value`, `curve`, `rivals` and model `metadata`. The authenticated account ID is distinct from the manager/team ID. Missing account identity skips persistence, and queue errors do not disable forecasts. Providers should deduplicate the same account/player/model/cutoff/scenario record.

```r
loaders <- list(persist_forecast=function(user_id, championship_id, player_id,
  model_version, forecast_type, cutoff, horizon, prediction) {
  # An offline test double; production uses the private background queue.
  stopifnot(nzchar(user_id), is.list(prediction))
  TRUE
})
```


## League switching and rule isolation

Every analysis refresh resolves the active authenticated account, championship and team. `finance$rules$season` and `finance$rules$scoring_version`, when known, become the explicit model context and are passed to the match-history reader. Labeled market, roster, auction, bid, price and match rows from another context are excluded before any player-ID join. Current live roster/market frames inherit the requested verified context; historical frames keep their original provenance and are never relabeled to the current season. Unsupported historical context must be addressed in the reader/migration layer rather than guessed here.

Changing account, league or team resets the selected market player to the first current choice and restores the scenario defaults (seven days, 95% assumed resale offer, zero fees), even when player IDs overlap between leagues. Server-owned selection/scenario state prevents stale browser controls from briefly using another league's assumptions. Refreshing the same league preserves valid selections. Current league identity appears with the financial summary.

Forecast persistence also passes `season` and `scoring_version`. The prediction payload includes `league_rules`, preserving the actual scoring configuration and rewards such as money per point, MVP or ranking bonuses alongside the model cutoff. These rewards are not silently converted into modeled fantasy points. Lineup rules are read from the current league on every refresh, so differing captain, formation, multiposition and bench settings remain isolated. The same athlete can consequently have different point forecasts in different leagues.

The focused multi-league test switches one Shiny session between two leagues with reused player/manager IDs, different point histories, formations, captain settings and cash-per-point rewards. It checks context-specific forecasts, model observations, control reset and persisted bonus/scoring attribution.
