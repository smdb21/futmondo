# Background observation persistence

`background_runtime.R` uses `callr` and `later`. `defer_persistence(fn,args=list())` accepts a sourced function name and serializable argument list and returns whether the request was queued. Jobs carry account ownership; identical queued/running requests are deduplicated by the full payload. Failure tracking uses a separate operation/context key, so refreshed timestamps or a new login token do not leave an old warning behind after a successful retry. Collection failures are scoped by league/team; direct writes additionally include the target table, stable entity keys and payload account/league/team fields. A queue rejection remains visible until the same operation finishes successfully; accepting or completing unrelated work cannot clear it. Queue capacity is 200 and batches contain at most ten jobs.

`persistence_tick()` supervises one child R process per batch. The child loads only project files and server environment, executes jobs in order (preserving foreign-key dependencies), and reports failed writes without printing credentials. Failed/unacknowledged work is surfaced for manual refresh, not replayed as a trade. No network write occurs in the Shiny telemetry event loop. A process launch failure is reported; jobs are not claimed as saved.

`background_sync_status(user_id)` returns `{pending,error}` for that account only. Root UI polls this status while logged in. Offline mode suppresses process launches and HTTP. This in-memory telemetry queue is not a durable worker and stops with Shiny; the separate [automation worker](automation.md) provides persistent scheduling when the app is closed.

```r
defer_persistence("collect_account_observations", list(auth, league_id, own_team_id))
status <- background_sync_status(auth[["userid"]])
```

Explicit user saves/read acknowledgements still use bounded acknowledged writes. Configure Supabase and `callr` as described in README. Tests cover account-isolated status and full startup without external calls.

## Failure and recovery contracts

`persistence_job_scope(name, args)` accepts a sourced function name and its positional argument list and returns a SHA-256 string identifying the operation/context. For example, `persistence_job_scope("log_player_history", list(players, "league-id"))` stays stable when refreshed prices change, and collection scopes ignore refreshed login tokens. For `supabase_post_direct`, arguments are `list(table_name, payload, conflict)` with optional conflict columns. The scope uses those conflict columns or `supabase_conflict_key(table_name)`, excluding only `observed_at`; standard catalog/entity tables use `id`, and manager DNA uses `team_id`. Account, league and team fields also participate whenever present.

Direct-write payloads may be a data frame, one named row list, or an unnamed list of row lists. Each complete row identity is hashed before sorting the row identities, so reordered batches match while different entity pairings cannot collide. Changing prices or observation timestamps for the same identified entities is a retry; different entities, snapshot dates or account contexts remain distinct. Unknown/generated-ID payloads, empty payloads and rows missing reliable keys use their full serialized payload conservatively. For example, an auction A failure cannot be cleared by a successful auction B write in the same league. Success clears only the matching operation/context; unrelated failures remain visible. Queue-overflow failures use that same scope and clear only after a successful matching completion, not when a new request is merely accepted.

`record_persistence_failure(table_name, http_status = NULL, error_code = NULL, category = NULL)` records a failed database write in process-local options and invisibly returns `{table, category, http_status, code}`. HTTP status is numeric or `NA`; table and database code are validated identifiers. Categories include `configuration`, `authorization`, `schema`, `connection`, and `write`. For example, `record_persistence_failure("player_history", 400L, "PGRST204")` records a missing-schema failure. Empty payloads are skipped before configuration checks and do not count as failed saves.

Child jobs return `{ok: logical, issues: list}`. `persistence_failure_message(issues = list())` maps these safe categories to a user-facing string and never displays table names, raw server response bodies, account IDs, or credentials. For example, `persistence_failure_message(list(list(category = "connection")))` asks the user to retry with Refresh. Unknown failures request a retry without assuming that migrations are missing. Queue overflow and process startup failures remain scoped to the affected account/job.

Focused offline regression: `Rscript test/test_background_sync.R` covers changed-payload recovery, per-entity and league/account isolation, row-list and compound-key identities, conservative unknown-key fallback, queue-rejection recovery, incomplete child results, safe response diagnostics, and configuration/HTTP/connection outcomes. Application lifecycle coverage runs separately in `Rscript test/test_shiny_simulation.R`.
