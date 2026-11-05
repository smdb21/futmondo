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


## Incremental history reads — 2026-11-05

`read_history_incremental(table, query, context, identity_fields, watermark_fields, max_rows, page_size, cursor_fields, overlap_days=1, full_refresh_after=86400)` retains only complete baselines in the existing bounded account/context cache. Each demanded refresh queries from the newest saved timestamp minus 24 hours, merges by the supplied source identity, and retains older records. Price snapshots use ingestion `recorded_at`; score observations use `observed_at`, so a new correction to an older round is included. The league dataset combines `observed_at` and `checked_at` alternatives, preserving successful empty acquisitions as well as signed and unknown scores. These alternatives remain separate from pagination cursor conditions.

`read_card_price_history()` and `read_card_round_history()` use this helper. `read_league_round_points()` and `read_league_round_dataset()` accept optional `context`; the shared table controller passes the current account/league/team context explicitly. A mismatched league is rejected before reading. Callers without context retain the existing uncached full-read contract. Limits and deterministic pagination are unchanged. Complete baselines survive ordinary view refreshes for up to seven days inside the existing 256-entry/32-MiB cache. They are never presented as freshly verified without a source query. A full database audit once per day catches older delayed insertions and deletions; partial/failed reads cannot advance the baseline.

Futmondo's captured pressroom/news contracts support `championshipId` and `from`, not a date-range parameter. `get_championship_pressroom()` and `get_locker_clause_payments()` therefore page from the current source head until they pass the newest verified source event minus 24 hours. Pressroom retains the same-second ObjectId boundary replay. Locker watermarks include all news, including non-clause events. `source_latest_at` tracks original source time independently of a clause payment's earlier accounting time; `full_history_at` certifies a complete acquisition. Missing source identities, timestamps, or amounts cannot certify completion. Results still contain the complete merged history.

`read_completed_history_baseline()` / `write_completed_history_baseline()` retain source snapshots independently of the short display cache, bounded to 32 entries/16 MiB and 30 days idle. Explicit account/global cache clearing removes them; ending one session preserves completed snapshots shared by another session of that account. A full source audit after seven days repairs changes/deletions outside the overlap. A new or invalid baseline requires a complete acquisition. Unknown season identity stays unknown; no season is derived from the calendar. Baselines use known configured league season identity when available.

`history_baseline_store.R` provides `read_persisted_history_baseline(resource, context)` and `persist_completed_history_baseline(resource, data, context)`. The server-only `complete_history_baselines` table stores one atomic normalized snapshot plus its completion/watermark metadata for each account, league, season, scoring version and source resource. The recorder saves after a successful async acquisition, never during replay. It contains no authentication/session data. Reads validate scope, shape, hashes and size; missing/corrupt/unavailable snapshots fall back to source acquisition. Only acknowledged writes suppress unchanged snapshot saves. See the new migration and database schema documentation. This is a factual data cache, with no stored login or background acquisition.

Transaction persistence compares source content with acknowledged fingerprints. `pressroom_history_fingerprints()` ignores ingestion metadata and numeric storage-type differences. `pressroom_history_batch_operations()` creates ordered player-parent, transfer, auction and bid writes; `supabase_pressroom_history_batch_valid()` / `_direct()` validate and execute them. `persist_auction_history()` queues at most one 100-transfer group at a time and advances acknowledgement only after all its writes succeed. Unchanged rows cause no writes; failed/unconfirmed groups remain retryable on the next active refresh. Fingerprints use the existing bounded cache. The `history_delta_ids` result attribute describes acquisition changes, but never substitutes for persistence acknowledgement.

Upstream player summaries still return their supported complete payload: no unverified date parameters were added. Database/cursor deltas reduce repeated historical reads; first acquisitions, periodic audits, genuinely new data, and recovery after cache eviction still require more work.


Persistence continuation interfaces: `defer_persistence(..., retained_bytes=0)` includes explicitly retained callback state in the shared 16-MiB queue budget while keeping the 1-MiB HTTP-payload ceiling. Additional callbacks on an already queued/active job add their retained state to its accounted bytes. `pressroom_history_next_batch(state)` holds only unsaved rows/signatures and queues one bounded group; `pressroom_history_ack_callback(state, hashes)` records successful hashes, discards the saved rows, and queues the next group immediately. Cancellation/failure drops the continuation, so logout cannot revive a delayed save chain. Durable snapshot acknowledgements use a separate small callback factory holding only the key and fingerprint.

Snapshot internals in `history_baseline_store.R` separate scope/key normalization, scalar timestamp validation, credential-field rejection, typed-column JSON encoding/restoration, semantic hashing, bounded compression/decompression and bounded acknowledgement tracking. The normalized frame has an explicit column/type descriptor and count; metadata keeps full-history verification, source high watermark and observed time separately. Transaction baseline internals normalize configured season context, prune bounded entries, calculate/pass the overlap boundary and compare changed source IDs. These helpers never infer missing season identity or turn unknown values into zero.

Detailed persisted payload encoding, bounds, migration checks and recovery behavior are documented in [durable history baselines](history_baseline_store.md).


## Bounded factual-read recovery — 2026-11-05

`fm_async_read_http()` coalesces the entire read, including retry backoff, by URL/method/payload/headers/session owner. `fm_async_read_retry_allowed()` limits automatic replay to GETs and the existing allowlisted factual POST routes. `fm_async_read_retryable()` accepts connection failures and HTTP 408/429/500/502/503/504; authentication, malformed responses, permanent source errors and capacity failures do not receive a transport retry. There are at most two HTTP attempts, with a cooperative 250 ms delay (one second for 429), and at most 32 retained reads including delayed retries. Normal successful reads add no requests. The adapter's existing logical-read budget remains separate from HTTP attempt counts.

The transport retains a bounded `read_states` entry with owner, attempt count, cancellation and timer state alongside each shared pending promise. `fm_transport_cancel(owner)` cancels delayed retries as well as active curl tasks and releases capacity. Synchronous dispatch exceptions settle through the same cleanup path. Authentication and explicit manual actions continue to use the single-attempt transport directly. This is recovery of user-demanded reads; it adds no background harvesting or polling.


## Absolute snapshot expiry (2026-11-05)

`resource_snapshot_unexpired(data, now=Sys.time())` checks a composite list's optional `valid_until` before synchronous or asynchronous fresh-cache reuse. When present it must be one finite numeric deadline strictly later than `now`; an expired or malformed deadline triggers the existing coalesced acquisition even when the ordinary TTL has time remaining. Data without this field preserves the existing policy. A failed refresh may still return clearly stale evidence, with its original deadline and failed manual-action gate. No timestamps, cache limits or background timers are extended. The asynchronous team-capacity loader uses the same predicate. Transport scheduling is documented in [transport priority](transport_priority.md).
