# Saving acquired factual observations

`background_runtime.R` records data already fetched for an active view. It does not discover players, log in independently, poll accounts, or execute trades. `record_acquired_resource(resource, data, context)` prepares catalog identities, prices, daily observations, team snapshots, pressroom events, and changed round scores from the supplied observation.

`defer_persistence(fn, args, context)` accepts only prepared `supabase_post_direct` writes or validated `supabase_fact_batch_direct`, `supabase_round_history_batch_direct` and `supabase_pressroom_history_batch_direct` operations. Each job carries account and session ownership. Limits are 25 jobs per account, 200 total, 1 MiB per job, and 16 MiB queued. Writes use batches of at most 100 rows. The process retains at most 256 acknowledged payload keys, 64 errors per account, and 128 error-owning accounts.

Exact payloads deduplicate queued/running/recently acknowledged work. Failure identity uses natural entity keys independently of observation timestamps, so successful reacquisition of the same entities clears its error. Different entities cannot clear it. Pressroom transfers use an ordered two-operation batch: historical player identities must be acknowledged before dependent transfers are sent. Unknown player identities are rejected; existing player metadata is preserved.

Only newly observed or corrected points/finality/date rows are saved. Reopening cached data, replaying the same observation, or receiving unchanged stored scores creates no new history. Zero cash and zero/negative points remain valid; missing values stay unknown. Stale observations do not get fresh timestamps.

`persistence_tick()` drives a single async write using the curl event loop. The UI reports `background_sync_status(account_id)` as pending or failed; only a numeric 2xx response acknowledges a save. Configuration, authorization, schema, connection, and write errors remain distinguishable internally, with safe user-facing messages. A failed save does not discard live API data.

Logout cancels that session's queued and active jobs. Process suspension/restart loses pending work; this queue is bounded in-memory work, not durable delivery. An explicit reacquisition can retry an unacknowledged fact. There is no persistent scheduler, child process, autonomous collector, or stored login session.

Focused coverage: `test_background_sync.R`, `test_observation_recording.R`, `test_pressroom_player_parents.R`, and `test_session_resource_ownership.R`.


## Save completion and transient recovery — 2026-11-05

`persistence_operation_valid()` validates a queued operation before admission and dispatch. `persistence_operation_retry_safe()` requires explicit conflict identity and either a valid immutable observation timestamp (`observed_at`, `checked_at`, `recorded_at`), duplicate-ignore insertion, or the monotonic `complete_history_baselines` database guard. Mutable merge-upserts such as current player/team metadata are not automatically retried; an old failed write cannot replay behind a newer value.

`persistence_error_issue()` retains sanitized structured HTTP status before classifying connection/transient HTTP failures versus authorization, schema and other failures. Eligible history jobs receive at most two additional attempts across the job, after cooperative 250/500 ms delays, resuming the failed operation only. Earlier acknowledged chunks are not replayed and later acquisition markers cannot run before score chunks succeed. Unrelated ready jobs can proceed during backoff. One retry timer shares the existing bounded queue, and logout cancels it together with the owned jobs.

Serialization, operation extraction and synchronous transport exceptions release the active slot and notify completion listeners. Stable round-history failure identity uses league/player/season/scoring without `checked_at`, so a newer successful source check clears the prior save error for that identity. `persist_recent_points_snapshot()` rejects observations from a different player/context before preparing its marker. A failed save retains live data but never certifies durable acquisition. Healthy write counts, batch sizes and payload/memory bounds remain unchanged.
