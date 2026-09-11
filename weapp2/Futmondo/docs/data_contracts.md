# Data, account and league contracts

`data_contracts.R` is sourced by the API adapter. Scalar helpers `fm_scalar(x,default)` and `fm_number(x,default)` accept one value and preserve unknowns; `fm_time(x)` accepts UTC/offset ISO strings, SQL timestamps, POSIX times or epoch seconds/milliseconds and returns UTC POSIXct values.

`fetch_result(data=NULL,status="ok",source="futmondo",observed_at=Sys.time(),error=NULL)` returns those five named fields. Status is `ok`, `empty`, `partial`, `stale` or `unavailable`. `observed_data(data,source,status,observed_at)` attaches equivalent metadata to legacy return objects; `as_fetch_result(data,source)` adapts them. Empty is a successful empty response, never a substitute for an error. `%||%` only replaces NULL.

`valid_login(token)` requires named, nonempty `token` and `userid`; `is_authorized_admin(token,admin)` additionally matches the verified login name to the server configuration. `api_cache_key(key,login,scope)` prefixes account identity. `get_cached_data(key,expr,timeout_sec=300)` caches successful observations, preserves stale timestamps on failures, and raises a sanitized unavailable condition when no previous observation exists. `clear_api_cache(user_id)` clears only that account in a session. New loaders use this boundary. Mutation requests are never automatically retried.

`get_active_championships(login)` preserves the raw `answer.championships` list. Each entry contains league ID/name and the user's team context. Root server selection canonicalizes `id`/`_id`; only authenticated memberships appear in the selector. `get_lineup_from_team(login,championship_id,user_team_id)` preserves `players`, `bench$players`, `bench$config`, `custom`, `pointSystem`, and `multiposition`, including an empty bench.

`normalize_league_rules(info,lineup=NULL,championship_id=NULL)` returns league/season/scoring context, initial budget, point and ranking bonus settings, captain/multiposition/bench capabilities, roster cap and raw configuration. Configured media weights are preserved. `league_season_context(championship_id,observed_season,configured)` uses a supplied observed season or the explicit per-league JSON configuration described in README; otherwise `unknown`. Scoring versions hash the observed point-system/custom configuration. The four optional bench slots are documented by [Futmondo](https://help.futmondo.com/article/159-entrenador-automatico); enabled state still comes from the league response. Full rule verification and deadline solvency remain false until their complete contracts are known.

`get_acquisition_capacity(login,championship_id,user_team_id,target_player_id=NULL)` returns `status`, roster count/cap/slots, funds, outgoing commitments, optional own target bid and diagnostics. Commitments come from `market/players` own `bid_id`/`bid_price`, not incoming roster offers. Sealed rival amounts remain unknown. `get_financial_snapshot(login,championship_id,user_team_id)` is the common model/UI snapshot: signed `cash`, `withheld`, `legal_bid_limit`, `spendable_budget`, `commitments`, roster constraints, rules and observation time. Recommended spending conservatively subtracts withheld funds and outgoing commitments; unknown commitments produce NA. Borrowing permission never replaces cash. Capacity snapshots expire after fifteen seconds. Read failure/staleness blocks execution.

Temporary acquisition credit is verified from the current team value. `acquisition_headroom(cash,team_value,withheld=0,commitments=0,debt_fraction=0.5)` permits spending down to `minimum_balance = -0.5 * team_value`, while subtracting held funds and outgoing bids. It returns `spendable_budget`, `debt_limit`, `minimum_balance`, and `projected_committed_balance`; unknown inputs remain unavailable. `get_acquisition_capacity()` and `get_financial_snapshot()` expose these fields. Positive cash is still required at the next round start to score points.

```r
fin <- get_financial_snapshot(auth, league_id, own_team_id)
if (identical(fin$status, "ok")) print(fin$spendable_budget)
# Do not turn missing cash into an estimated starting budget.
```

`normalize_player_average(average)` accepts the API's nested point-statistics list, for example `list(average=11, matches=2, averageLastFive="11.00", fitness=list(11,11), total="22.0")`. It returns a flat named list with the `average.` prefix (`average.average`, `average.matches`, `average.averageLastFive`, `average.fitness`, `average.total`, and supplied venue averages). Roster, market, and catalog parsing all use this same shape. A missing or empty `average` returns an empty list and preserves the player's identity, valuation, and other fields. Missing optional `clause`/`market` fields also preserve roster rows.

The fitness array contains recent **point scores**, not physical-health measurements. Its observed values are retained as a comma-separated string; a nonempty complete numeric array supplies the recent total and a recent mean only when the API has not supplied a usable mean. Missing/empty/incomplete scores do not manufacture recent form. Blank `status` remains unknown availability; the card says **Not reported** rather than declaring a player injured or healthy.

```r
stats <- normalize_player_average(list(average=11, matches=2, fitness=list(11,11)))
stopifnot(stats$average.average == 11, stats$average.averageLastFive == 11)
# get_players_from_team() attaches the same canonical statistics to roster rows.
```

Focused regression: `Rscript test/test_player_rating_inputs.R` exercises actual response parsing into roster/card ratings, endpoint consistency, absent optional data, and caching.

`normalize_bid_observations(pressroom_df,championship_id,observed_at)` reconstructs a system-auction winner from settlement price, combines nested losing bids and removes repeated manager/auction bids. It returns league/auction/player/bidder IDs, amount, winner flag, source bid ID and settlement/observation times. `normalize_auction_observations(...)` returns sold outcomes and unknown visibility. Missing buyer/seller IDs mean Futmondo, not missing counterparties. Open/relisted opportunities use distinct listing identifiers in market observations; absence from a later snapshot is not proof of an unsold auction.

`normalize_player_match_observations(summary,player_id,championship_id,scoring_version,season,observed_at)` retains round points, score finality, venue and raw minutes/start fields. Ambiguous minutes never become participation labels. Finality comes from explicit response flags or verified finished rounds, not missing timestamps. Player-summary adapters retain points, fixture, championship and owner context.

`supabase_conflict_key(table_name)` supplies explicit natural keys. `supabase_post(table_name,payload,conflict)` queues Shiny telemetry through `defer_persistence`; `supabase_post_direct` performs a bounded write outside the UI process. `supabase_read_all(table_name,query,max_rows=100000)` is a cached paged read. `supabase_patch(table_name,payload,filters)` acknowledges bounded explicit user writes. Empty/error responses remain distinguishable.

History readers: `read_player_price_history(championship_id,player_ids=NULL)` returns player/value/observation time; `read_auction_observations(championship_id)`, `read_bid_observations(championship_id)` return observed rows; `read_model_auctions(championship_id)` attaches a price observed within 24 hours before settlement. `read_player_match_history(championship_id,cutoff,final_only=TRUE,season=NULL,scoring_version=NULL)` excludes future observations, unverified seasons/scoring and provisional results, selects the latest requested context and deduplicates each player/round. It returns points, round identity, finality and provenance. NULL means unavailable; a zero-row frame means no usable history.

`collect_account_observations(login,championship_id,user_team_id)` verifies membership, collects league snapshots, normalized auctions/bids, up to thirty prioritized player summaries and generated alerts. `persist_auction_history(pressroom_df,championship_id)` persists the source ledger and normalized observations. All private data stays league-scoped; global catalog writes contain identity only.

`persist_forecast(user_id,championship_id,player_id,model_version,forecast_type,cutoff,horizon,prediction,season=NULL,scoring_version=NULL)` stores a JSON prediction with deterministic account/league/model/cutoff/context/scenario identity. Identical retries upsert; different scenario assumptions create separate records. `record_forecast_outcome(user_id,championship_id,forecast_id,outcome,evaluated_at)` updates only an unevaluated, matching account/league forecast. Attaching outcomes does not certify model quality.

`save_transfer_scenario(user_id,championship_id,user_team_id,name,scenario)` stores an arbitrary JSON scenario and returns TRUE only on acknowledgement; successful saves invalidate the account cache. `read_transfer_scenarios(...)` requires the same three IDs. `log_user_team_history(teams_df,round_number=NULL,championship_id=NULL)` records an explicit league; a compatibility database trigger validates/derives the team membership for older callers.

```r
persist_forecast("account", "league", "player", "points-v1", "points",
  Sys.time(), 1L, list(mean=4, lower=-1, upper=9), season="2026-27", scoring_version="hash")
```

Tests: `test_data_contracts.R`, `test_history_persistence.R`, `test_har_contracts.R` and the offline application lifecycle.


## Reviewed private-history migration

`20260907_private_history.sql` enables RLS and revokes browser-role access to existing private league/history tables, granting the server role the required table and owned-sequence privileges. Its `scope_user_team_history` trigger derives or validates a new row's league from immutable team membership; preserved orphaned history remains unscoped for audit. `reconcile_legacy_market_transaction` adopts an incoming source ID only when exactly one legacy row matches league, player, counterparties, price, clause flag and transaction time. It retains the historical row ID and provenance; ambiguous matches remain with a `legacy_ambiguous` annotation. No rows are deleted by this follow-up migration. These SQL contracts passed static guards but still require staging PostgreSQL execution before production use.
