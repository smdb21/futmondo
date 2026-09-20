# Round MVPs Module

`round_mvps_UI(id)` renders the **Round MVPs** view. `round_mvps_Server(id, is_module_active, login_token, championship_id, refresh_trigger)` shares a session source with Rivals. Saved selections render first; missing official selections load one round per reactive timer callback while a consumer is active.

## Data source

`get_round_mvps(championship_id)` in `supabase_connector.R` reads `round_dream_team` with these filters: the selected `championship_id`, `is_mvp = true`, and `is_finished = true`. It returns a cached data frame ordered by descending `round_number`; an unavailable database read raises an error that the shared source catches before trying official live results.

The response uses `round_number`, `player_id`, `player_name`, `player_role`, and `points`. `round_mvp_rows(rows, championship_id)` validates those records, keeps one finished MVP per round, and orders the cards newest first.

## Usage

```r
round_mvps_Server("round_mvps", reactive(input$tabs == "round_mvps"),
  login_token_RV, championship_id_RV, refresh_trigger)
```

Use **Sync Round Dream Teams** in Admin to backfill completed rounds when the view reports no saved MVP records.

## Official discovery and availability

`mvp_text(x, default="")` accepts scalar atomic text/IDs, trims whitespace, and returns the default for missing or malformed values. This prevents absent IDs from matching each other.

`mvp_runtime.R` is sourced by `futmondo_functions.R`. `get_finished_round_catalog(login, championship_id)` returns `{status, rounds, complete, reason}`. It matches the selected championship's `league` to `/2/league/list`, preserves immutable round IDs, recognizes `done`, and joins display numbers from matching IDs in the active championship metadata. An upcoming-only or empty catalog does not establish historical absence. Missing completion flags remain `NA`. The existing `get_finished_rounds()` wrapper returns its data frame for legacy consumers.

`mvp_api_answer(login, url, query)` returns `{status, answer}` on success or a safe `mvp_result(status, rows, reason, code, complete)` on failure. It distinguishes authentication, connection, HTTP failure, rejected API requests and unsupported shapes. Both fetching handlers use `get_cached_data()`; offline tests perform no HTTP. An empty dream-team answer is **unknown publication status**, not proof of an unpublished award. The recorded API evidence does not currently establish an explicit unpublished response contract.

`normalize_mvp_catalog(active, leagues, championship_id)` accepts decoded active-championship and league-list answers. `mvp_records(x)` adapts list or data-frame collections. `normalize_official_dreamteam(answer, championship_id, reference)` accepts players with `id`/`_id` and a scalar or identity-object `mvp`. It returns all official players with championship/round identity, name, role, points, and MVP/finished flags, or an explicit failure reason. Zero and missing scores are preserved. Missing display numbers do not prevent live cards but prevent writes to the current database schema.

```r
reference <- round_reference("opaque-round-id", 3)
result <- normalize_official_dreamteam(
  list(mvp="p1", players=list(list(id="p1", name="Player", points=0))),
  "league-1", reference)
stopifnot(result$status == "ok", result$rows$points == 0)
```

`get_official_dreamteam_result(login, championship_id, reference)` caches typed official results. `shared_mvp_source(session, login, championship, active, refresh)` takes reactive identity/activity inputs and returns a reactive result. Root-session state deduplicates requests across consumers, resets on account/league/refresh changes, suspends when consumers are inactive and ends with the session. Display remains usable when saving fails. Partial results retain cards and explain missing evidence.

Verification: `Rscript test/test_mvp_runtime.R`, `Rscript test/test_ui_reliability.R`, and `Rscript test/test_shiny_simulation.R`.

Compatibility helpers `official_round_mvp_row(answer, championship_id, reference)` and `get_official_round_mvps(login, championship_id, rounds)` use the same parser and typed fetcher. The former returns only MVP rows; the latter aggregates supplied verified rounds and reports partial/error reasons. An empty supplied frame cannot prove no historical rounds. Example: `get_official_round_mvps(login, "league-1", catalog$rounds)`. Interactive consumers use the cooperative source instead of this batch helper.
