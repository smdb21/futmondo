# Observation and evaluation runtime

Implemented 2026-09-08. Apply the observation-lifecycle migration after the two earlier migrations. All private operations carry account/league context. API sessions and account execution locks remain account-wide. The worker is independent of Shiny; observation subscriptions are independent of trading policies.

## Public contracts

* `preserve_observation_time(data, source=data)` returns a data frame retaining source `observed_at` and `fetch_status` attributes as appropriate. Missing source times stay NA. Cached data never becomes fresh through a UI refresh.
* `normalize_fixture_observation(summary, player_id, championship_id, season, scoring_version)` reads verified `match._id`, `match.r.number`, `match.info.date` and source observation attributes. Returns fixture/round occurrence rows, or an empty frame. `normalize_player_match_observations(..., rounds=NULL)` separately retains source observation, occurrence and explicitly supplied round start timestamps. Process timestamps are not kickoff timestamps.
* `normalize_listing_opportunities(market, championship_id)` reads `id,expirationDate` and source observation attributes. Returns listing identity, player, first/last observation, expiry, outcome and visibility. An expired/disappearing listing is unknown, never inferred unsold.
* `record_verified_auction_outcome(championship_id, listing_id, auction, bids, evidence)` writes explicit settlement evidence. `auction` contains auction/player IDs, observed/settled times, outcome and winning fields; bids use scoped auction/bidder IDs and amounts. Evidence requires `settlement_verified=TRUE,source`; complete participation additionally requires `bid_visibility_verified=TRUE,eligible_manager_ids`. Returns logical acceptance. This extension point has no verified live complete-visibility/unsold provider yet.
* `insights_rows(data)` converts a frame with nested JSON/list columns into a list of row records.
* `collection_player_batch(ids, after=NULL, limit=30L)` returns a sorted rotating subset; cursor persistence prevents always collecting the first 30 players.
* `read_league_preference(user_id, championship_id, user_team_id)` returns logical coach preference or NULL when unavailable. `save_league_preference(..., enabled)` writes the explicit boolean with a three-part conflict key and returns acknowledged success. The UI distinguishes persistence from session fallback.
* `set_observation_subscription(login, championship_id, user_team_id, enabled)` verifies current membership, saves the subscription and returns acknowledged success. `schedule_observations(now=Sys.time())` visits due enabled subscriptions, validates encrypted sessions, collects observations, matches outcomes and runs evaluations; it returns the number visited invisibly. Next retry is 15 minutes later; expired sessions and failed feedback remain errors. Run one observation scheduler; distributed observation leases are not implemented.
* `normalize_sale_observations(offers, roster, user_id, championship_id, user_team_id)` returns scoped observed offer rows. `read_sale_observations(user_id, championship_id)` loads them. `fit_sale_execution(observations, as_of=Sys.time())` returns `mean,lower,upper,n` ratios from distinct system offers observed before cutoff; fewer than five returns unavailable estimates.
* `forecast_observed_outcome(record, prices, matches, auctions, now=Sys.time())` returns a supported future outcome or NULL. Price outcomes require the target horizon; point outcomes require occurrence/finality evidence; rival outcomes require exact auction or listing identity. Player identity alone is insufficient.
* `reconcile_forecast_outcomes(user_id, championship_id, user_team_id, now=Sys.time())` fills outstanding forecast outcomes within the scoped context, returning count invisibly.
* `summarize_forecast_evidence(records)` returns per-forecast errors, baseline errors, coverage and identity fields. Repeated same-player/type/horizon/day predictions are reduced to the earliest cutoff.
* `evaluate_saved_forecasts(user_id, championship_id, now=Sys.time())` persists advisory summaries by model version, with separate error/baseline/coverage metrics for each horizon. `run_chronological_evaluations(login, championship_id, user_team_id, now=Sys.time())` calls expanding auction, round and price backtests and persists daily version/cutoff/metric/fold records. Neither automatically approves a model. The performance tab shows named metrics and insufficient-data states.

```r
batch <- collection_player_batch(catalog$id, after=subscription$last_player_id)
fixtures <- normalize_fixture_observation(summary, player_id, league_id, season, scoring_version)
execution <- fit_sale_execution(read_sale_observations(account_id, league_id))
# In the separately configured worker:
schedule_observations()
```

## Persistence and deployment limits

`read_observation_revisions(table, championship_id, cutoff, keys)` selects the latest event revision known at cutoff. Auction/bid history views retain revisions, immutable first knowledge and subsequent last-seen times. Legacy guessed timestamps are excluded from training; a new verified observation establishes knowledge now and archives the uncertain legacy row. Price references require both source and storage capture times before auction settlement. Source price/team-history writes are idempotent by league/player-or-team/observation time.

`automation_opportunity_key(policy, player_id, action, payload)` returns a stable hash of policy content and listing/offer/clause identity, including amount/expiry. Same-day relisting is a new opportunity. Unknown outcomes still block retries; full league rules and existing observation/model gates remain required for live actions. `background_sync_status(user_id)` aggregates pending work and unresolved per-job failures; an unrelated successful job cannot clear another failure.

`fetch_account_insights_alerts(login)` reads only authorized memberships and returns combined league-labelled Insights alerts. Header unread counts include both sources; unavailable counts stay unknown. Reading one alert uses its own authorized league and requires acknowledgement.

Current captures do not establish all round start/finality metadata, complete auction eligibility/visibility, hiring or lineup submission. Such paths remain unavailable. Staging migration/backfill and distributed worker execution-lock validation, operational deployment, chronological statistical acceptance and at least fourteen observation days are separate pending acceptance gates. Isolated PostgreSQL tests do not establish production concurrency.
