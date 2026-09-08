# Background observation persistence

`background_runtime.R` uses `callr` and `later`. `defer_persistence(fn,args=list())` accepts a sourced function name and serializable argument list and returns whether the request was queued. Jobs carry account ownership; identical queued/running requests are deduplicated. Queue capacity is 200 and batches contain at most ten jobs.

`persistence_tick()` supervises one child R process per batch. The child loads only project files and server environment, executes jobs in order (preserving foreign-key dependencies), and reports failed writes without printing credentials. Failed/unacknowledged work is surfaced for manual refresh, not replayed as a trade. No network write occurs in the Shiny telemetry event loop. A process launch failure is reported; jobs are not claimed as saved.

`background_sync_status(user_id)` returns `{pending,error}` for that account only. Root UI polls this status while logged in. Offline mode suppresses process launches and HTTP. This in-memory telemetry queue is not a durable worker and stops with Shiny; the separate [automation worker](automation.md) provides persistent scheduling when the app is closed.

```r
defer_persistence("collect_account_observations", list(auth, league_id, own_team_id))
status <- background_sync_status(auth[["userid"]])
```

Explicit user saves/read acknowledgements still use bounded acknowledged writes. Configure Supabase and `callr` as described in README. Tests cover account-isolated status and full startup without external calls.
