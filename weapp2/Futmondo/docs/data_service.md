# Resource, cache, and transport contracts

Retained screens bind factual adapters through `bind_factual_reads()` in `factual_helpers.R`; the selected-player chart and recent panel share one card source. `data_service.R` owns context, cache policy, resource state, in-flight reuse, invalidation and lifecycle. `history_repository.R` owns scoped historical queries and reconciliation. Adapters retain endpoint parsing in `futmondo_functions.R` and `supabase_connector.R`.

`resource_context()` requires an account and carries championship, team, player, season, scoring version and session. Raw completed adapter results can be shared within the same account/context. Async view results and pending work are session scoped; consumers in one session share one pending fetch. Logout cancels and invalidates only that session's pending resources. Completed account data remains bounded and cannot be selected by another account. Actual data mutations invalidate matching account/league/team/player resources across sessions.

| Resource class | Fresh lifetime | Additional stale window |
| --- | --- | --- |
| Finances, capacity, market, bids, team cash | 20 s | 60 s |
| Player summary/card and notifications | 60 s | 3600 s |
| Real club identities | 900 s | 3600 s |
| Other factual resources | 300 s | 3600 s |

Expired entries and least recently used completed entries are evicted. The global cache is capped at 256 entries / 32 MiB; oversized resources and admission when every slot is pending return unavailable. Session adapter registries are capped at 128 resources and dispose evicted observers. `resource_metrics()` reports hits/misses/stale returns/evictions/coalescing, request/response bytes and retained cache size. These are process-local diagnostics.

`get_cached_data(resource, expression, context=...)` preserves source observation time and fetched time on reuse. `get_cached_resource_async(resource, context, fetch, refresh=FALSE)` returns a promise of a resource state. `shared_resource()` handles active/context/refresh signals and rejects results from abandoned generations. States distinguish loading, empty, partial, stale and unavailable. Stale financial evidence cannot authorize a manual transaction.

`async_transport.R` uses curl multi transfers, a nonblocking 10 ms later pump, four connections, at most 32 retained tasks, a 15 s request timeout and 5 s connection timeout. `async_adapter_fetch()` replays read-only adapter expressions with completed HTTP responses memoized; at most 32 HTTP reads are allowed per composite adapter. Read errors replay as errors so adapters can preserve partial live results. Cancellation stops replay immediately. Trade routes are denied by this transport adapter; explicit user actions retain acknowledged preflight/write handlers.

Refresh invalidates only relevant resources, including both financial wrappers and underlying team-info cash. Countdown ticks update local text and do not reacquire data. Inactive view bindings stop new work; context switches clear UI state. Manual actions reconcile uncertain results instead of retrying trades automatically. Explicit administrative diagnostics remain synchronous.

## History

`get_player_card_history(login, championship_id, user_team_id, player_id, refresh=FALSE, season=NULL, scoring_version=NULL)` reads summary, scoring metadata, stored round rows and prices. It merges points only inside the full context. Latest observation wins; finality and API authority break timestamp ties. Later provisional corrections remain provisional. Unknown context is kept separate; a known round without a date still displays. No cumulative-total differencing or synthetic valuations is used.

History queries select required columns and paginate in stable order. Card reads use keyset `(round, observed_at)` and `(recorded_at, id)` with a 5000-row cap; general history has a 20000-row cap. Short server pages continue until empty or capped; reaching a cap reports partial coverage. Stored partial history never suppresses the summary request. Completeness requires known context, explicit complete API coverage, and complete database reads; ordinary API results are conservatively partial.

Each round score keeps its source and status. Stored unknown observation times are preserved. No blanket historical backfill invents season, scoring or time. New genuinely observed rounds can fill gaps as users inspect players.

See [executed results](data_review_results.md), [saving observations](background_worker.md), and [player card](selected_player_module.md).
