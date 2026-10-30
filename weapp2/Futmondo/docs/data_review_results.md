# Futmondo data review and implementation results

Updated 2026-10-26. Local implementation and release preparation are complete under the [redesign plan](data_architecture_redesign_plan.md). This record separates observed findings, executed verification, and rollout. **Update:** the production DB migrations were applied on 2026-10-27 UTC; application deployment remains pending. See [production migration evidence](production_migration_20261027.md).

## Baseline

The source baseline is preserved at `/tmp/futmondo-redesign-baseline-gtos1hyz` (134 source/documentation assets, excluding credentials and HAR captures). The workspace already contained modifications; comparisons use that snapshot rather than assuming the Git base reflects the starting application.

R 4.6.1; Shiny 1.14.0; httr 1.4.8; curl 7.1.0; later 1.4.8; promises 1.5.0. Installed versions describe this test environment, not production.

| Baseline check | Result |
| --- | --- |
| `test/test_player_trend_plot.R` | Passed 16 existing checks. These checks did not establish all new history contracts. |
| `test/test_shiny_simulation.R` | Failed before changes: the selected-player modal-context assertion expected one entry. Log: `/tmp/futmondo-baseline-lifecycle.log`. |

## Read only database inspection

The connected Supabase project was inspected through schema, count, advisory, log-summary, and EXPLAIN queries. No production data or schema was modified.

| Resource | Observed state |
| --- | --- |
| `player_history` | 113,367 rows covering 573 players; 43,317 rows have no source `observed_at`. |
| `player_match_observations` | Zero rows. Stored per-round history cannot currently supply player-card points. |
| Match-history schema | Season, scoring version, round, status, observation time, optional round/start time columns already exist. |
| Player-history query | EXPLAIN uses the player index, filters league separately, then sorts by recorded time and ID. This is a query plan, not a measured runtime. |

Legacy snapshot timestamps cannot establish source observation time. The redesign must retain their provenance and retrieve actual round scores from available API observations.

Performance advisories identify missing foreign-key indexes and some unused indexes. The redesign adds only indexes justified by retained query paths. See [Supabase's index advisory reference](https://supabase.com/docs/guides/database/database-linter?lint=0001_unindexed_foreign_keys).

Security advisories identify legacy automation RPC execution grants and schema visibility. Retirement scripts must remove the obsolete RPCs and isolate retired tables. Service-role-only historical tables intentionally use RLS without browser policies. See [the privileged-function advisory](https://supabase.com/docs/guides/database/database-linter?lint=0028_anon_security_definer_function_executable). Aggregated logs did not expose useful failure codes in the selected fields; they do not establish error-free ingestion.

## Retained screen data paths

| Screen | Principal source reads |
| --- | --- |
| Login and league selection | Authentication, active championships, team identity |
| Today | Financial snapshot, roster, market, pressroom transfers |
| Your Team | Roster, own listings, incoming offers, observed finances |
| Market | Current league/team market listings |
| All Players | League player catalog and verified spending capacity |
| Player card | Player summary/profile/matches, scoped stored prices/round points, manual-action state |
| Rivals | Teams, rosters, pressroom transactions, standings history, observed payments/MVP evidence |
| Classification | Team round results, completed rounds, official dream-team results |
| Round MVPs | Shared official-result loader and saved round results |
| Plots | Catalog, market, pressroom, team rounds, finances, standings history |
| Notifications | Official events/unread state and player links |
| Administration | Authorized retained table inspection and explicit administrative actions |

Shared API caching previously reduced some repeated calls, but refresh cleared the account cache and triggered broad collection. Persistence also fetched rotating player batches unrelated to the current view. The new boundary must reuse demanded observations and remove model/worker collection.

## Implemented design

The retained app uses an explicit resource service, async curl transport, scoped history repository, and a bounded queue that saves acquired observations. Policy and interfaces are documented in [data service](data_service.md) and [persistence](background_worker.md).

The highest-priority correctness findings were empty stored round coverage, cumulative totals treated as per-round scores, undated scores discarded, context/refresh gaps, and optional database/profile failures hiding useful live content. The independent baseline fixture reproduced cumulative values 4/12 displayed as rounds and loss of undated 0/-2. The replacement reconciles partial sources and corrections within league/player/season/scoring context, keeps provisional status, and plots known rounds without dates. Its independent fixture verifies final scores 0/-4/8/-2 with mean 0.5 and a separate newer provisional score of 3.

Efficiency changes remove broad refresh collection, duplicate save-time reads, render-triggered persistence, unbounded cache growth, redundant pending requests, and unrelated inactive-view work. Raw completed observations share account/context caches; pending view resources have session ownership so one logout cannot cancel another session's read. Actual transactions invalidate cash and other affected state. Unknown counts and failed saves remain unknown/failed.

Prediction/automation engines, tabs, workers, optimizer/advice features, Smart Bid, bid-probability placeholders, model-only schema, and their active documentation/tests are removed. The roadmap and `Agents.md` explicitly exclude their future development. Factual arithmetic and explicit manual actions remain. Historical applied migrations and an isolated legacy-schema fixture remain as upgrade evidence; they are excluded from deployment.

## Measurements and budgets

Local R 4.6.1 / Shiny 1.14.0, loopback HTTP fixture, 200 ms response delay. These are controlled measurements of request behavior and event-loop blocking, not production latency or throughput estimates.

| Scenario | Executed result | Budget / interpretation |
| --- | --- | --- |
| Same slow endpoint, synchronous httr baseline (5 samples) | Median 203.2 ms; range 203.0–210.8 ms before return | Demonstrates blocking I/O in the former transport approach. |
| Async dispatch, same endpoint (5 samples) | Median 0.6 ms; range 0.5–28.5 ms before return | Every dispatch below 100 ms; heartbeat ran before all 5 responses. Response completion still includes the 200 ms network delay. |
| Actual card adapters with database HTTP 503 | Cold: 4 reads; warm reopen: 0; explicit refresh: 4 | Single-player fixture budget: summary, scoring metadata, round query and price query. Supplementary profile/match reads are separate. Pagination requires extra pages when present. |
| Concurrent identical read | One actual request for two consumers | Verified at both transport and same-session resource boundaries. |
| Cache retention | Cap/eviction assertions pass at 256 entries / 32 MiB | Admission and oversize rejection tested; this is a data-cache bound, not a bound on total R process memory. |
| Persistence | Duplicate replay and unchanged rounds produce no new writes; point/finality corrections do | Queue bounded at 200 jobs / 16 MiB, 25 per account / 1 MiB per job; 100-row batches. |
| Reactivity | Inactive consumers start no reads; local countdown/navigation and context-isolation checks pass | Delayed socket checks use real curl; UI lifecycle uses synthetic HTTP-blocked adapters. |

Reproduce transport measurements with `Rscript test/benchmark_data_transport.R`. The recorded five async requests transferred 55 body bytes. The separate actual-card transport check dispatched in 25.1 ms. No comparable end-to-end production baseline, p95 load test, or process-memory benchmark was available; no live speedup percentage is claimed.

## Executed verification

All results below were executed centrally. Failures discovered during integration were fixed and the affected suites rerun. Test counts refer to grouped assertions rather than individual expectations.

| Area | Passing evidence |
| --- | --- |
| Core service and transport | `test_data_service.R` (9), `test_async_transport.R` (4), `test_session_resource_ownership.R` (3). |
| Historical correctness | Independent known-answer fixture; `test_player_trend_plot.R` (9), `test_recent_points_average.R` (6), `test_history_persistence.R` (7), `test_player_rating_inputs.R` (4). |
| Factual ingestion | Contracts (12), catalog (5), observation recording (7), background persistence (6), pressroom parent ordering (4). |
| Retained UI/manual behavior | Deterministic fixes (30), UI reliability, market listing (4), notification details (5), debt/countdown (9), factual helpers (7), factual review (5), plots (3), official MVP runtime (12), player profile endpoints (6), scalar/login handling (2). |
| Application lifecycle | `test_shiny_simulation.R`: 12 checks plus all 9 retained tabs; HTTP and live database mutations blocked. |
| Browser | Playwright at 1440, 390 and 360 px: login, 9 tabs, no visible output errors/sideways page overflow, player-card 0/-2 chips and round plot, unavailable-profile fallback and history refresh. Screenshots were visually inspected. |
| Schema | Isolated PGlite fresh/upgrade suite (10 groups) and historical migration suite (6): repeatability, correction/revision retention, unknown times, grants, archive preservation and transactional rollback. |
| Administration/deployment | Factual schema helper checks; regenerated manifest and deployment contract (3) exclude credentials, captures, backups, development files and retired engines. |

The independent score/isolation fixture was authored by Luna and executed by the primary agent. Two Sol 6.1 engineers supplied substantial data-layer/UI implementation and focused regression corrections; the primary agent integrated, executed and reviewed the result. No child-agent success assertion substitutes for the recorded central runs.

Local evidence logs are under `/tmp/futmondo-redesign-checks`; SQL logs are `/tmp/futmondo-factual-schema.log` and `/tmp/futmondo-legacy-migration.log`. Browser captures are `/tmp/futmondo-player-card-{1440,390,360}.png` plus corresponding login/navigation images. Temporary logs/screenshots are not deployment assets.

## Release state and remaining limits

Implementation and local release preparation are complete. Existing historical migration files are byte-identical to the task-start snapshot. Fresh schema plus two forward migrations and rollback instructions are prepared in [database migration](database_migration.md). At local implementation completion, no production schema/data changes, deployment, or live trades had been performed. The subsequent authorized database rollout is complete and verified in [production migration evidence](production_migration_20261027.md); application deployment and live trade verification remain unperformed.

The database rollout is now complete. The application release and hosted environment smoke checks remain pending. PGlite establishes SQL semantics locally, not production locking/concurrency behavior. Explicit administrative counts/schema verification and manual trade preflights remain synchronous; normal factual view reads use the async transport. Pending saves are in memory and can be interrupted by process suspension/logout. API coverage and unknown season/scoring contexts remain honestly partial; no unverified historical backfill is applied.

Private-HAR direct-sale/clause suites could not run because their captures were absent. Credential-dependent live-finance checks were not completed; production transaction verification was deliberately skipped. Offline payload, preflight, listing, uncertainty and context checks passed. The entire legacy test directory was not executed indiscriminately because some scripts issue live requests and other suites covered removed features.

## Availability correction 2026-10-29

User reports: Today showed an unavailable component below cash, missing active-offer/spending totals, and My Team did not show the roster. Offline reproduction identified two separate parsing failures: optional null fields caused market row construction to fail, and empty roster market/bid metadata caused an invalid names assignment. A failed optional club lookup also discarded an otherwise valid roster. The exact private live response was not captured.

The correction retains players with empty optional fields, preserves unknown numeric data as `NA`, keeps valid roster/catalog rows when optional club labels fail, and distinguishes failed responses from verified empty lists. Missing offer amounts remain unverified for manual actions. Today now uses the already-loaded finance snapshot’s team value instead of depending on the league-teams read. The countdown warning explicitly names the missing balance after offers. See [the loader contract](player_data_availability.md).

Verification executed for this correction:

| Check | Result |
| --- | --- |
| Player data availability | 11 grouped checks passed, including actual JSON loaders, optional-field failures, missing offer prices, and cold/warm cache behavior. |
| Existing rating-input regression | 4 grouped checks passed; zero, negative, and incomplete observed scores retained. |
| Today availability | 3 grouped checks passed: unavailable league teams cannot hide the finance value; zero/negative cash and unknown team value remain distinct. |
| Existing debt/countdown regression | 9 grouped checks passed. |
| Required offline application lifecycle | 12 checks plus all 9 retained tabs passed, including My Team rendering, unavailable history, refresh, logout, and blocked HTTP/live mutations. Existing Plotly fixture warnings remain; no assertions failed. |
| Deployment contract | 3 grouped checks passed; changed source checksums refreshed and all 30 intended release-file checksums verified. |

Performance-impact review passed for the changed paths. The roster/market fixture uses 5 existing cold reads; three repeated roster/market/capacity accesses add 0 reads, 0 parses/normalizations, and no cache-entry or retained-byte growth. The Today KPI fixture serves both values with 1 finance read and 0 league-teams reads, with no additional read on repeated output access. This removes that KPI dependency; it does not claim the entire application no longer requests league teams. Club matching cannot multiply player rows and skips the club request if no team IDs exist. Added normalization only traverses the existing response on a cache miss. No retries, timers, background work, cache policies, dependencies, CSS, or mobile layout were added. Documentation and manifest checksum changes have no runtime cost. Full production concurrency, process-memory benchmarks, and new browser/device tests were deliberately not run for this scoped correction; hosted behaviour remains unverified.

Luna investigated shared-table dependencies; Sol 6.1 implemented loader fixes and focused tests; the primary agent reviewed the integrated diff, implemented the Today correction, and ran its checks and the required lifecycle/finance verification. Implementation and validation made no production calls, account transactions, database migrations, or deployments. Local code is ready for deployment; the live incident is not marked resolved.


## Continued availability incident: request contracts and diagnostics — 2026-10-29

The user's new screenshots show the previous warning text and team-value correction in the hosted UI, but Today/My Team availability remains broken. The provided Connect logs show successful multi-page pressroom pagination interleaved with repeated roster, market, and lineup progress lines; no underlying exception or response status was retained. Repeated processing lines are consistent with asynchronous adapter replay and cannot establish duplicate HTTP requests.

A local request inspection confirmed that seven older factual reads serialized scalar credentials, IDs, strings, and booleans as single-element JSON arrays, contrary to the documented HAR request contract. The working team-information read already uses scalar serialization. Clubs, championships, roster, market, catalog, lineup, and league-team reads now use `auto_unbox=TRUE`. This is a confirmed request-contract correction, but the hosted API's rejection reason is still unconfirmed; the provided logs contain no private response or original exception.

Fetch failures now retain an allowlisted source/category, optional HTTP status, and short reference across nested caches and asynchronous replay. UI errors expose only that safe category/reference. Optional failures caught by composite reads remain attributable in safe metadata. Raw messages, calls, bodies, credentials, IDs, and query strings are not copied to the new diagnostics. Adapter-local failure logging is deduplicated and capped at 32 entries; warm reads and repeated rendering do not log again. The misleading per-page pressroom progress line, including its cursor ID, was removed. This is not a repository-wide audit of older logging.

Executed verification:

| Check | Result |
| --- | --- |
| New read diagnostics | 6 groups passed: sanitization, nested cause/reference retention, stale fallback, async failure handling, replay log deduplication, and actual UI error formatting. |
| New request contracts | 4 groups passed: all seven corrected synchronous and asynchronous request shapes, roster/market rows and warm reuse, paginated replay, and preserved pressroom HTTP failure. |
| Existing data service | 9 groups passed, including bounds, coalescing, context isolation, and invalidated generations. |
| Existing session ownership | 3 groups passed: same-session coalescing, independent sessions/logout, cancellation, and stable persistence identity. |
| Existing player availability | 11 groups passed, including zero/negative/missing values, optional enrichment, incomplete money, and cold/warm cache behavior. |
| Existing debt/countdown | 9 groups passed; uncertain acquisition data still fails closed. |
| Existing async transport | 4 groups passed with local HTTP only. Dispatch took 0.0278 seconds for the delayed fixture; heartbeat ran before its response. Cold card reads=4, warm=0, explicit refresh=4. |
| Required offline lifecycle | 12 checks plus all 9 retained tabs passed; no external HTTP or live mutations. Existing Plotly fixture warnings remain. |
| Deployment contract | 3 groups passed. Four runtime checksums refreshed; all 30 staged bundle checksums verified. Exact 10-file scope, credential-pattern scan, and whitespace check passed. |

Performance-impact review passed within fixture limits. The real async roster/market path uses 5 cold requests and 0 warm requests. Three pressroom cursors cause exactly 3 requests and 0 warm requests, despite adapter replay. A repeatedly encountered optional parser failure emits 1 safe log across 3 responses and 0 warm logs. Existing player checks show 0 warm parses and no cache-entry/retained-byte growth. No retries, timers, reactive consumers, request scheduling changes, new dependencies, or CSS changes were introduced. Diagnostic construction is limited to failures and stores bounded small metadata; rendering adds only short error text. Documentation and release checksum updates have no runtime cost. Production load/process-memory and new browser/mobile checks were deliberately not run.

Luna investigated the request contract; Sol 6.1 implemented the correction, diagnostics, and focused fixtures; the primary agent reviewed the actual changes and ran the existing integration/performance checks. No account login, live trade, database change, or Posit deployment was performed. The legacy synchronous HTTP wrapper still obscures its original cause; explicit HTTP/network attribution was verified for the actual asynchronous UI transport. Hosted verification after redeploy remains pending, and this incident is not marked resolved.


## Progressive login, Today, and player cards — 2026-10-30

Authentication now dispatches one asynchronous uncached request. Duplicate attempts are suppressed; logout/session changes discard obsolete replies. Password references are cleared after dispatch. Visible feedback continues through league and Today loading, with a cancellable five-second slow-source notice and failed-section retries. Today balance/team value and market start independently before optional finance, pressroom, history, and notification reads. Promise completion handlers explicitly return NULL from observers so Shiny can flush intermediate UI states; state-only simulation would not establish this browser behavior.

Cards retain the clicked row for identity/statistics and progressively acquire summary, scoring context, stored observations, profile, matches, and passive action eligibility. Completed warm reads are reused; explicit history refresh invalidates the selected underlying source caches. Obsolete consumers stop further adapter pagination. Final manual transaction preflights remain unchanged. Round boxes combine scores/results only with verified identifiers or compatible season/competition context; unknown associations remain visible separately. Zero/negative points are retained, long names wrap on phones, and nonpositive height/weight is omitted.

Finality review confirmed an application bug: absence of a per-player finished flag previously became provisional. Missing, malformed, or conflicting evidence now remains unknown; explicit logical true/false remains final/provisional. Reconciliation and observation dedup preserve this distinction. Historical stored provisional rows are not bulk rewritten. The known flag names are compatibility handling, not a verified live wire contract. Raw HAR files were unavailable in this workspace; the user confirmed they are on another machine and will provide them later. Actual Futmondo flag paths/types and match identifiers therefore remain a follow-up. Neither dates nor completed football matches are used to manufacture final fantasy-score status.

Verification: focused progressive loading 15 groups, login 5, Today 3, profile 6, contracts 13, resource service 10, session ownership 3, observation recording 8, history persistence 7, chart 10, and async transport 4 passed. Independent validation passed eight round-association cases plus finality/reconciliation/recording known-answer cases. The mandatory offline lifecycle passed 12 checks and all nine tabs with zero external HTTP/live database mutations. Existing empty Plotly fixture warnings remain. Browser tests use actual Shiny UI with delayed synthetic sources and block external requests; these establish progressive rendering, not production source latency.

Performance review: fixed resource slots reuse the existing bounded caches and four-connection pool. No new dependencies, database queries/schema, polling, or unattended automation. One cancellable startup timer per session and a 64-sample fixed-label timing ring are bounded. Round association averaged 0.00355 seconds for 1,000 fixture rows; finality decoding averaged 0.0000281 seconds per synthetic point. Documentation and manifest updates have no runtime cost. Local browser timing is reported separately below; hosted cold starts, network latency, concurrency, and production process memory were not measured. No production login, trade, migration, or deployment was performed.

Sol 6.1 implemented runtime changes; Luna independently checked round association and finality; the primary agent reviewed changes and ran integrated browser/resource/release verification. Hosted verification after redeployment remains pending.

Final browser acceptance passed at 1440, 390, and 360 pixels. In-browser mutation/animation-frame measurement recorded cold identity at 349, 342, and 340 ms, respectively; login feedback was 3.6–11.4 ms. Earlier roughly one-second readings included automation locator polling and are not paint timings. Identity precedes slow remote sections; summary points precede profile/history; the chart is created after an identity-paint acknowledgment. All widths showed matched zero/negative scores and wrapped long club names; closing/reopening added zero card reads and all external HTTP remained blocked. These delayed synthetic fixtures demonstrate local rendering behavior, not hosted response-time guarantees. Deployment contract checks passed and all 30 staged runtime bundle checksums were verified. The release excludes unrelated CRLF changes, private files, and captures.


## Current-player round display correction

The screenshot demonstrated that live player points and match results both carried round numbers but lacked the extra identifiers required by the previous join. That rule was too restrictive for current responses already fetched for the same selected player. The card now opts into numeric-round association for live Futmondo points in its current player context. Explicit context/identifier contradictions still block fallback; saved-only history is not broadly joined by round number. The Round 10 regression expects 18 points, Barcelona 1–2 Real Madrid, and 2026-10-25 in one group, with no duplicate unmatched fixture. Existing zero/negative, multi-fixture, ordering, and conflict checks remain.

Performance review: only the pure display join changes, adding bounded vector comparisons to at most five displayed point groups. No API/database calls, cache changes, timers, reactive consumers, dependencies, or persistence writes are added. Existing CSS and finality handling remain unchanged. Ten round-association checks and ten chart/display checks passed; the full lifecycle suite is deliberately not repeated for this localized display change. Browser checks use missing optional metadata to reproduce this incident. No hosted verification or HAR finality verification is claimed. Implementation and review were performed directly, without delegation.

Browser validation passed at 1440/390/360 px with metadata-free current responses: matched round boxes, long-name wrapping, zero warm reads, and zero external HTTP. Identity paint on the complete run measured 350/360/326 ms. An initial process-cold attempt exceeded the 500 ms timing threshold at 619 ms; the subsequent complete run passed, so the half-second figure is not a guaranteed cold-process bound. All 30 staged deployment hashes and scoped whitespace checks passed.


## Average last N: observed-score loading correction

The configurable column skipped history unless season and scoring were known, required a completed-round catalog, and filtered out all scores without explicit final status. N=5 alone could use the API aggregate fallback. The table now reads observed scores in its exact context (including the separate unknown scope), without a finality filter or latest-round window; the unchanged arithmetic sorts each player's recorded rounds newest first. This also avoids mistaking the latest N league rounds for the player's latest N recorded games.

Performance review: one bounded existing shared history read replaces the gated/windowed query; the 5,000-row safety cap remains. Removing the range can read more historical rows on a cold load, but N changes now reuse the same query instead of requesting another range. There are no per-player API requests, new dependencies, timers, writes, or cache-policy changes. Known-answer checks cover (18+19+7)/3 and negative/zero points; the actual table fixture verifies one read and zero additional reads when N changes. Unknown context remains isolated. No fitness ordering, round scores, or finality is fabricated. Players without recorded scores still lack arbitrary-N averages; raw HAR verification remains pending as previously agreed. This correction does not claim universal coverage on the hosted table. Work and review were performed directly without delegation.

Verification passed: eight focused average assertions/groups, plus the mandatory offline lifecycle (13 checks and nine tabs), including actual table recalculation with unknown context/finality and one cached read across N=3 to N=2. No external HTTP or live database mutations. Existing empty Plotly fixture warnings remain. Browser layout was deliberately not repeated because no markup/CSS changes were made. Hosted coverage and fitness-array order remain unverified.


## Consolidate Market into Today

Removed the duplicate Market sidebar item, tab, and server instance. Today now exposes the existing Your Bids Only and Bidded Only controls; the same shared table and manual player actions remain. Performance review: fewer module observers/outputs are instantiated; the transferred filters only subset existing rows. No extra API/database requests, cache changes, timers, dependencies, or writes. Documentation has no runtime cost. Direct implementation and review, no delegation. Mandatory offline lifecycle results are recorded below; browser screenshots are deliberately not repeated for this standard shared-control/navigation change. Hosted verification is pending.

Mandatory lifecycle passed: 13 checks and all eight retained tabs, zero external HTTP/live database mutation attempts. Existing empty Plotly fixture warnings remain. Deployment hashes were refreshed from staged runtime bytes.


## Older card rounds incorrectly reported as unassociated

The builder previously truncated points to five rows before pairing fixtures, then displayed older fixtures as unassociated even when their points were already loaded. It now associates first and limits displayed groups afterward. The player card requests up to 50 point groups. No database reads/writes or data contracts change. Persistence records retrieved observations; this display correction neither proves production coverage nor backfills unobserved rounds.

Eleven independent known-answer association cases and ten chart/display checks passed, including ten available rounds, a hidden-but-paired older round, and one genuinely absent score. Performance review: no additional API/database requests, cache changes, reactive consumers, timers, or dependencies; more loaded rows can be paired/rendered. A local 50-round/1,000-fixture benchmark averaged 0.00985 seconds across 20 runs. Display is bounded at 50 groups; existing acquisition bounds remain. Full lifecycle/browser checks were deliberately not repeated for this localized pure display-limit correction. Hosted records and rendering remain unverified. Implemented and reviewed directly, without delegation.
