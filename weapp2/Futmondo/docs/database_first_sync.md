# Database-first login and automatic factual sync

Approved 2026-11-16. This supersedes optional factual worker enrollment, quiet hours and interactive acquisition fallback. It changes data delivery, not accounting formulas or transaction authority. See [verification](data_review_results.md) and [rollout tracking](data_architecture_redesign_plan.md).

## Data route

1. Interactive authentication establishes the account identity. The sign-in dialog discloses continued encrypted factual sync after logout.
2. `worker_bootstrap(p_account)` returns the existing `worker_read` envelope (sanitized account policy, leader health, resources, redirects and safe job errors), plus `preferred_league: {championship_id, selected_at} | null`. It reads the canonical active-league snapshot and saved preference together, without modifying jobs or source clocks.
3. R restores the verified league context and schedules one immediate coalesced `worker_read` for the selected team's information/balance, market/offers and roster. Page interests share this batch. A missing interest can read immediately. `fm_worker_preference()` shares bootstrap with league selection; it does not issue another preference GET.
4. Independent `worker_connect()` acknowledgement enrolls or renews collection. A slow or failed enrollment does not block stored data. The existing Docker-supervised worker acquires source observations, reuses durable results/checkpoints and publishes them to the database. The existing R engine parses and reconciles financial data.
5. Visible sessions normally poll for changed database versions every 15 seconds. Same-context data stays mounted during reads; unchanged facts and metadata-only changes do not rebuild the dashboard/table. Changed facts update affected outputs. Source times, stale status, missing data and failures remain truthful.

`fm_worker_configured()` now identifies production display mode, independently of collector configuration. `fm_worker_can_connect()` checks the enable flag, secret key, sodium and server database configuration. Missing collector setup, revoked access or source errors cannot switch browsing to Futmondo acquisition. Missing database setup rejects promptly with a saved-data error. Offline tests/headless helpers and explicit manual reads retain separate paths.

`fm_worker_schedule_read()` coalesces new interests on the next event-loop turn; `fm_worker_prefetch()` registers the selected core descriptors after bootstrap. Read callbacks verify that the same account state is still attached. Context/account switches cancel owned waiters. Bounds remain 128 resource interests per account, 128 consumers per resource, 128 waiters, 256 decoded snapshots/32 MiB; temporary decompression buffers are outside that cache bound.

## Access lifecycle and RPCs

All new RPCs are `SECURITY INVOKER` with restricted search paths, executable by the server role only. Authentication envelopes never enter factual snapshots or browser data.

| RPC | Input and behavior |
| --- | --- |
| `worker_connect` | Account, encrypted token, random credential binding, expected authorization generation and IANA timezone. New accounts use generation 0. Renewals preserve generation, leases, attempts, due times, checkpoints and run IDs. Only idle jobs explicitly stopped for source authentication become due immediately. Returns sanitized account policy or null on a stale generation. |
| `worker_preferences` | Account, expected generation, excluded-league string array. Saves exclusions and stops excluded jobs; preserves unrelated work. Returns sanitized policy or null on conflict. |
| `worker_disconnect` | Account and expected generation. Removes factual ciphertext/credential, disables collection, increments generation and removes its jobs. Retains snapshots/history. Returns acknowledged policy, then the app clears interactive/remembered login. |
| `worker_prepare_source` | Account/resource/owner/generation/request hash. Checks lease, leadership, current context, exclusions and access; returns the encrypted server-only account plus a matching current-run checkpoint. It replaces separate account/checkpoint/authorization round trips. |

The expected generation is captured once per login and retained across retry. A failed connection retries after 30 seconds while the session is visible; a rejected/stale generation requires a new sign-in. The session marks connection successful only on acknowledgement. Account/token changes reset enrollment state; late responses from a previous identity cannot affect the new one. A late renewal cannot undo Disconnect. Ordinary logout only detaches browser interests and leaves factual access running. Disconnect does not revoke separately authorized scheduled purchases; use their existing cancellation/revocation controls.

Legacy `worker_authorize`/`worker_renew` remain compatible during rollout but no longer reset active work or restore quiet hours. Normal cadence operates 24/7; rate limits, backoff, leadership and access checks remain enforced. The shared two lanes are unchanged. The short lane prioritizes primary observations, while family fairness prevents a large player backlog from starving catalogs/history/finances. Expensive context/peer checks run on ordered claim candidates rather than every due row before sorting.

## Display versus actions

`async_adapter_fetch(..., intent='display'|'manual_action')` explicitly separates reads. The existing `worker_bypass=TRUE` argument remains compatible with manual callers. A display adapter rejects attempts to reach the source through nested getters. Opening, reopening or refreshing a player card registers stored-data interests only. Successful persisted ownership/listing observations remain display facts after their action deadline, with original timestamps and stale flags. Production cards do not show **Retry availability** or acquire when an obsolete retry event arrives. `player_availability_source(..., intent)` permits live checks for explicit transaction dialogs or confirmations. Missing ownership/capacity remains unverified; stale data cannot authorize a purchase. See [player availability](player_availability.md).

Manual preflight observations retain their scoped durable evidence store, original clocks, strict expiry and invalidation rules. Worker replay is never persisted back as a new observation. Root Refresh and coach read preflight no longer write action-invalidating epochs; actual action paths still invalidate affected evidence. The separately enabled coach-at-login behavior, manual lineup/market checks and individually authorized scheduled purchases retain their safety controls.

## Explicit league refresh

The floating Refresh button queues a Futmondo source check for the selected account, league and team. Confirmed manual clause buys, direct sales and accepted sale offers do the same for the transaction's captured context, with the affected player's existing summary job also requested where available. An uncertain direct sale may request a factual read for reconciliation and is never resubmitted by refresh. A request acknowledgement means work is queued. The browser keeps its prior observation until the worker publishes a newer version.

`worker_league_refresh` advances existing team information, market, own roster, pressroom, player catalog and spending-capacity jobs. The worker rechecks Futmondo and saves successful observations to the existing snapshot store. For affected live inputs it skips checkpoints and nested snapshots from before the request, while sharing newer snapshots and reusing checkpoints from retries of the same request. Requests arriving during a lease leave it intact and queue one follow-up. Verified pending-transfer candidates identified by refreshed roster/history can have their existing summary work advanced in one bounded batch. The All Players default view and active-clause filter exclude verified incoming purchases until ownership observations catch up.

A service-only RPC authorizes the account and league, honors exclusions and source backoff, and coalesces rapid repeated clicks. The existing claim lanes prioritize explicitly due jobs and continue using the current pacing and retry limits. The browser keeps its normal persisted-version polling and resource-specific updates. Idle worker polling can add up to 30 seconds before a claim, and visible browser polling can add up to 15 seconds after publication. Queue contention, rate limits, and incomplete source observations can extend this. Apply the league-refresh migration before the matching app/worker version.

## Performance and rollout

Apply `scripts/migrations/20261116183421_database_first_sync.sql` before deploying the matching R/Python image. The configured production database received it as `20261116213815_database_first_sync`; nine server-only invoker function/grant checks passed. No dependency, secret setting, process count or source concurrency increase is required. `FUTMONDO_WORKER_ENABLED=false` pauses collection and automatic connection while stored-data browsing remains available. An older application can use the compatibility RPCs during rollback, but its previous display behavior is outside this contract.

One uncached worker source exchange now uses three database calls (prepare, dispatch guard, checkpoint), compared with five previously; checkpoint reuse uses one. Dispatch still checks revocation immediately before HTTP. Snapshot chunks are posted in bounded groups of four (about 512 KiB), with access checks between groups and atomic guarded publication. An old credential's HTTP 401 can pause only that credential, not a newly rotated one.

Local synthetic checks cover two-read cold startup while connection acknowledgement is held, restart reuse, changed-version rendering, mobile settings, revocation races, source request counts, publication bounds and SQL queue fairness. They do not certify production login latency, end-to-end completeness or VM resource capacity. Hosted targets remain primary cards within two seconds after authentication when snapshots exist and p95 heartbeat below 500 ms. Missing first-time data still requires worker acquisition; errors are not replaced with zeros. Existing native source rejections need their exact failing query/response evidence before a separate accounting/acquisition fix can be claimed.
