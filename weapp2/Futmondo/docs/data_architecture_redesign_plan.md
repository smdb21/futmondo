# Futmondo data architecture redesign plan

Created: 2026-10-26
Status: Local implementation complete; production DB migrations applied and verified on 2026-10-27 UTC; application deployment pending. See data_review_results.md for executed evidence.
Project: /home/rstudio/workspace/futmondo
Authoritative application: /home/rstudio/workspace/futmondo/weapp2/Futmondo

## Objective and scope

Redesign retrieval, storage, caching, and reactive data consumption so Futmondo loads only the data it needs, shares that data across screens, and displays reliable historical records. Repair missing player-card points through the new data path. Remove prediction and automation features and their future development commitments.

This plan tracks the implemented redesign. Prediction, advisory optimization, and automation milestones in [the previous roadmap](v3_roadmap.md) are retired. Their application implementations have been removed; production database retirement was applied and verified on 2026-10-27 UTC.

| Retain | Remove |
| --- | --- |
| Authentication and account/league switching | Automated actions, policies, schedules, workers, and stored background sessions |
| Rosters, player profiles, official round scores and MVP results | Points, price, resale, and rival-bid predictions; model training and evaluation |
| Observed prices, transactions, standings, and finances | Smart Bid, generated trading recommendations, forecast-based rankings and planners |
| Official notifications and their player links | Predictive intelligence screens, optimizer/sandbox scenarios, recommendation-only alerts |
| Explicit manual transactions with current-state checks | Automatic coach preferences and unfinished hiring promises |
| Transparent descriptive statistics and historical accounting | Synthetic history and future promises to restore removed capabilities |

Ordinary data loading, caching, and saving observations obtained while the app is in use remain infrastructure. Unattended collection and scheduled business actions are excluded. Manual actions must remain explicit and must never be replayed automatically after an uncertain response.

Preserve factual historical records during migration. Unknown balances, dates, finality, and seasons remain unknown. Empty buyer/seller IDs retain the established Futmondo System counterparty meaning.

## Evidence and questions to resolve

The initial source investigation found the following review targets. The full audit must establish their runtime impact and the exact missing-history failure.

| Source | Finding | Review requirement |
| --- | --- | --- |
| [futmondo_functions.R](../futmondo_functions.R), `get_cached_data()` and `api_cache_key()` | Cache scope is inferred from caller/session context; entries have no capacity limit or expiry eviction. | Inventory key construction, retention, freshness, and account/session isolation. |
| [server.R](../server.R), refresh and collection observers | Shared refresh invalidates many consumers and starts account collection. | Measure actual request fan-out and identify necessary resource-specific refreshes. |
| [background_runtime.R](../background_runtime.R) and [supabase_connector.R](../supabase_connector.R) | Cooperative collection stages still execute synchronous HTTP and can contain several requests. | Measure event-loop stalls; distinguish deferred work from genuinely non-blocking I/O. |
| [supabase_connector.R](../supabase_connector.R), history readers | Some readers make a single request; `supabase_read_all()` pages through broad histories with offsets. | Verify completeness, row limits, query scope, ordering, index use, and incremental reads. |
| [Selected_Player_Module.R](../Modules/Selected_Player_Module.R), points trace selection | The first nonempty stored source can suppress more complete live history; aggregate snapshots can become apparent round scores. | Reproduce missing and incorrect points with known-answer fixtures. |
| The same card module and round-history reader | Season/scoring context is omitted from the card query; absent timestamps can discard valid round scores; chart and recent-points paths duplicate selection. | Establish one context-aware history contract and refresh path. |
| Prediction/intelligence/runtime modules | Factual helpers and manual-action checks share files with features being removed. | Extract retained dependencies before deleting modules. |

The working tree already contains changes. Capture the existing state and preserve unrelated edits; use scoped diffs rather than repository resets.

## Target architecture

Keep R Shiny and Supabase on the existing Posit Connect Cloud free-plan deployment. Introduce explicit boundaries within the application before considering additional infrastructure.

| Boundary | Responsibility |
| --- | --- |
| UI modules | Render shared reactive data and submit explicit user intents; no independent persistence or hidden network writes. |
| Session data service | Own selected context, resource state, refresh generations, dependency invalidation, and the shared player-card view. |
| Resource cache | Store bounded, context-keyed results and share pending reads for identical resources. |
| API adapters | Fetch and normalize Futmondo responses with explicit timeouts and read retry rules. |
| History repository | Query and reconcile persisted observations using scoped filters and deterministic ordering. |
| Persistence coordinator | Batch already-acquired observations, record acknowledged outcomes, and retry eligible writes without duplicate history. |

### Retrieval and cache contracts

- Pass context explicitly. Keys include the resource and every dimension that changes its meaning or access: account, league, team, player, season, scoring version, and normalized query parameters where applicable.
- Keep authenticated state isolated by default. Share genuinely public catalog data only after verifying its semantics and access. Reject missing context for private resources.
- Return one consistent resource state: data, source, observed time, fetched time, freshness, completeness, and error status. Distinguish valid empty results from unavailable data.
- Reuse fresh results across consumers. Coalesce simultaneous identical reads. Separate API transport caching from derived-view recomputation.
- Define freshness per resource: mutable cash/offers differ from catalog identities and finalized historical rounds. Use baseline evidence to choose durations, stale-data limits, entry/byte caps, and eviction rules.
- Retain source timestamps through joins and caching. Display stale factual data with its age when appropriate; revalidate the inputs needed for a manual transaction.
- Invalidate only affected resources after acknowledged changes. Refreshing a card must not clear the entire account cache.
- Ignore late responses from earlier account/league generations. Cancel session-owned work on logout or session end.
- Keep the countdown clock local. Network refresh cadence must not follow per-second rendering or inactive tabs.
- Validate a hosting-compatible non-blocking HTTP approach in milestone M2. Timers, `tryCatch()`, and smaller batches alone do not establish responsiveness. Do not assume permission to create child processes, persistent workers, or cron jobs.

### Storage and history contracts

- Separate shared identities, current league state, and history. Retain only tables and fields required by the factual/manual product.
- Identify a player-round result by league, player, season, scoring version, and round. Track observation revisions and choose the latest valid revision within that context; never deduplicate solely by round number.
- Preserve source/event identities and observation timestamps. Retried writes are idempotent; unchanged cached reads do not create new history. Distinguish meaningful point corrections from ingestion duplicates.
- Query only required columns, players, and time/round windows. Use a stable total ordering and deterministic pagination; detect partial results rather than silently accepting a server row cap.
- Design indexes from actual filters and query plans. Use small realistic datasets and a pagination-boundary fixture before large benchmarks.
- Persist existing fetched observations instead of reacquiring the same resources to save them. Batch compatible writes, bound queued work, and distinguish queued, acknowledged, failed, and interrupted saves.
- Live factual views remain usable when persistence is unavailable. Saving failure must not erase usable API data or be reported as a successful save.
- Use explicit access grants and appropriate private-data protections in migrations. Confirm the deployed schema and privileges rather than assuming repository SQL has been applied.

### Player history contract

Use one shared history view for the card chart and recent-points panel. Reconcile stored and API observations by context and round, with explicit source precedence, completeness, revision, and finality rules.

A partial database history must not prevent retrieving missing rounds from the API. Valid round scores remain visible without timestamps when the round is known. Zero and negative scores are valid. Missing participation and missing scores must not become zero. Provisional scores must remain clearly identified.

Remove cumulative-total-to-round-score fallbacks rather than differencing totals with unknown coverage or corrections. Remove fabricated valuation curves and default historical prices. If evidence is missing, show a clear empty or partial-history state. Unknown season/scoring records must not be silently combined with known contexts; define their display or quarantine treatment in M2.

## Milestones and dependencies

Local implementation and release preparation completed on 2026-10-26 (America/Los_Angeles). Evidence and remaining production-only checks are recorded in [data_review_results.md](data_review_results.md). M1–M6 local deliverables are complete. The subsequent production database rollout is complete; application deployment remains pending. See [production migration evidence](production_migration_20261027.md). Controlled fixture budgets replace an unavailable end-to-end production baseline; no production speedup or hosted smoke-test claim is made.

| Milestone | Depends on | Deliverable | Exit criterion |
| --- | --- | --- | --- |
| M1 Audit and baseline | None | Data-flow inventory, removal dependency map, reproducible baseline, ranked findings | Every retained screen has a traced read/write path; the player-history failure is reproduced or its unresolved evidence is recorded. |
| M2 Architecture and contracts | M1 | Resource/cache policy, history contract, schema mapping, transport feasibility result, performance budgets | Design decisions and measurable targets are recorded; hosting constraints are satisfied. |
| M3 Remove excluded features | M2 | Factual helper extraction and complete feature/dependency/documentation cleanup | Retained app starts; prediction and automation entry points and future commitments are absent. |
| M4 Shared data layer and player card | M3 | Data service/cache/repository foundation and repaired player history | Card history meets known-answer and request-count checks through the new architecture. |
| M5 Migrate remaining consumers and storage | M4 | Migrated modules, batched persistence, tested migration and rollback artifacts | Remaining retained flows use the shared contracts; staging preserves factual records. |
| M6 Integrated verification and release preparation | M5 | Test results, before/after measurements, deployment manifest, release/rollback instructions | Local completion criteria pass within documented fixture limits; production rollout is separately pending. |

### M1 Audit and baseline

- [x] Inventory endpoint callers, cache keys, reactivity, timers, database queries, writes, table dependencies, and unused paths for every retained screen.
- [x] Trace player history from summary/match endpoints through normalization, persistence, reads, reconciliation, refresh, and rendering.
- [x] Inspect deployed schema/version, row counts, player-round coverage, representative query plans, and failure states with read-only access. Record access gaps without replacing evidence with assumptions.
- [x] Establish cold/warm navigation, player-card, manual-refresh, league-switch, and failed-request scenarios using controlled fixtures first.
- [x] Record controlled request counts, transferred bytes, dispatch/blocking samples, cache caps, write/queue invariants and UI responsiveness. End-to-end production latency and total process-memory comparisons remain unmeasured; see the results limitations.
- [x] Rank issues by correctness, observed user delay, unnecessary traffic/storage, and implementation cost.

### M2 Architecture and contracts

- [x] Finalize the target boundaries above and map existing helpers into them.
- [x] Define source authority, history completeness, revision precedence, season/scoring isolation, and unknown-data behavior.
- [x] Set numerical cache caps, freshness rules, request budgets, and latency/responsiveness targets from M1; record the measurement environment.
- [x] Prove the chosen I/O mechanism can keep the Shiny session responsive under delayed requests without unsupported hosting features.
- [x] Specify schema changes, indexes, access grants, batched-write identities, retained tables, and retirement candidates.
- [x] Identify the minimal fixture and regression additions needed for meaningful uncovered risks.

### M3 Remove excluded features

- [x] Extract factual finance, acquisition-cost, identity, position, observation, and roster-validity helpers used by retained flows.
- [x] Remove prediction/automation engines, UI tabs, server wiring, forecast-dependent team features, recommendations, coach preferences, scripts, configuration, and model-only persistence calls.
- [x] Separate historical collection cursors from obsolete subscription/scheduler structures before removing those structures.
- [x] Remove prediction-only dependencies, assets, fixtures, and tests; retain factual assertions from mixed suites.
- [x] Rewrite README/module documentation and replace the previous roadmap with the factual/manual scope. Update `Agents.md` to track this plan, reflect the new data boundary, and explicitly exclude future prediction and automation development.
- [x] Clean fresh-install schema and deployment file lists. Preserve already-applied migration history; use forward migrations for existing installations.

### M4 Shared data layer and player card

- [x] Implement explicit context, resource state, bounded caching, shared pending reads, targeted invalidation, and session/generation lifecycle handling.
- [x] Implement scoped player-history reads and reconciliation using the M2 contract.
- [x] Route chart and recent-points consumers through one reactive source; retrieve only missing/stale data.
- [x] Remove synthetic history and aggregate-points fallbacks; render honest loading, partial, unavailable, and empty states.
- [x] Verify direct, roster, market, and notification entry paths; reopen, refresh, switch leagues, and logout.
- [x] Independently verify a small known-answer score/revision example before migrating other modules.

### M5 Remaining consumers and storage

- [x] Migrate rosters, market/manual actions, finances/countdown, standings, rivals, plots, official notifications, and MVPs to the shared data boundary.
- [x] Replace broad account collection with bounded recording of demanded observations and explicit refresh work.
- [x] Implement incremental reads and batched idempotent writes; verify persistence failures do not block usable factual views.
- [x] Rehearse schema migration and backfill on an isolated or staging database, including grants, row reconciliation, interrupted execution, and repeated execution.
- [x] Prepare retirement and rollback artifacts for forecast/model/scenario/automation objects after confirming no retained callers or factual dependencies remain.

### M6 Verification and release preparation

- [x] Run the smallest relevant focused suites during implementation, then the required cross-cutting Shiny lifecycle harness.
- [x] Inspect desktop/mobile player cards and retained navigation, including slow and unavailable data.
- [x] Verify controlled cold/warm/refresh, coalescing, context and failure fixtures against recorded budgets; compare synchronous/async blocking with the same delayed endpoint. Report cache/queue caps separately from unmeasured process memory and production throughput.
- [x] Search executable source, configuration, bundles, dependencies, and active documentation for excluded feature paths. Do not confuse preserved migration history with runnable functionality.
- [x] Regenerate and inspect the manifest when dependency changes require it.
- [x] Deliver a scoped diff, migration/rollback instructions, results, and remaining deployment constraints. Mark production rollout complete only after actual deployment and smoke checks.

## Availability follow-up — 2026-10-29

Status: **the prior UI correction is visible in user screenshots, but the account symptoms persist. The follow-up request-contract correction and diagnostic checks are complete locally; redeployment and hosted verification remain pending**.

- [x] Reproduce My Team failures from empty optional market metadata and failed club enrichment; preserve a valid roster and source names/logos.
- [x] Handle optional null player fields without losing market/catalog rows; distinguish failed reads from verified empty results and keep unknown money unavailable.
- [x] Reuse Today’s finance snapshot for its team-value box and clarify the balance-after-offers warning.
- [x] Complete focused parser, cache/request-budget, Today, finance, and offline lifecycle checks; review performance impact.
- [x] Review the continued incident logs; distinguish asynchronous replay progress from measured HTTP requests.
- [x] Correct the seven factual-read scalar JSON envelopes and verify actual async roster/market/pagination requests.
- [x] Preserve safe failure categories/references across nested reads; bound and deduplicate diagnostic logs.
- [x] Run focused regression and required lifecycle checks; record request, cache, and responsiveness evidence.
- [ ] Redeploy the follow-up correction and verify the reported Today/My Team account symptoms against the live application.

See [availability behavior and parser checks](player_data_availability.md) and [integrated review results](data_review_results.md#availability-correction-2026-10-29). The reported live payload was not captured, so the exact incident trigger is not established by the offline reproductions.

## Acceptance and verification

| Area | Required evidence |
| --- | --- |
| Warm reuse | Reopening a fresh card or switching between consumers of the same fresh resource issues zero additional API/history requests for that resource. |
| Request ownership | Multiple consumers requesting one uncached resource share one pending fetch within the session; explicit refresh uses the documented resource budget. |
| Reactive behavior | Countdown ticks and inactive tabs do not independently start network reads or writes; delayed-request fixtures verify continued UI responsiveness. |
| Cache bounds | A multi-league/player navigation fixture remains within configured entry/byte caps and evicts expired/unused data. |
| Isolation | Account and league changes cannot display or apply stale prior-context results; logout cancels session-owned queued work. |
| Historical points | Known results match by round across partial sources, corrections, zero/negative values, missing dates, repeated round numbers, and incomplete round catalogs. |
| Honest history | No cumulative totals represented as round scores, fabricated valuation series, or invented zero scores. |
| Storage correctness | Replay creates no duplicate factual records; corrections survive; pagination returns all intended rows with stable ordering. |
| Failure behavior | Timeouts, stale data, missing database configuration, and failed writes remain distinguishable; no uncertain manual action is retried automatically. |
| Feature removal | No executable prediction/automation paths, model-driven recommendations, scheduler configuration, or active future commitments remain. |
| Performance | Baseline comparisons satisfy the numerical budgets defined in M2, without weakening freshness or correctness. |
| Migration | Staging reconciliation preserves factual histories and access boundaries; interruption/retry and rollback procedures are exercised. |

Reuse existing focused coverage where relevant:

- `test/test_player_trend_plot.R`, `test/test_player_profile_endpoints.R`, and `test/test_recent_points_average.R`.
- `test/test_history_persistence.R`, `test/test_data_contracts.R`, `test/test_background_sync.R`, and `test/test_catalog_sync.R`.
- `test/test_notification_player_details.R`, `test/test_session_resource_ownership.R`, and affected manual-transaction tests.
- `test/test_deployment_contract.R` and `test/test_observation_migration.cjs` where retained deployment/schema contracts apply.
- `Rscript test/test_shiny_simulation.R`, which invokes the offline application lifecycle harness, after cross-cutting integration changes.

Add focused tests only for meaningful gaps, particularly cache eviction/context, source reconciliation, request coalescing, and pagination. Use independent known-answer calculations for score transformations. Retire obsolete behavior assertions rather than retaining features merely to satisfy old tests.

## Migration and rollback sequence

1. Record the working-tree baseline, deployed application version, schema state, and a restorable database backup.
2. Apply additive staging changes and preserve original factual records; backfill only where context can be established.
3. Validate keys, revisions, counts, representative history, and access grants. Keep unresolved records separate with an explicit reason.
4. Cut retained readers/writers over to the new contracts and reconcile results before retiring old structures.
5. Remove unused feature objects through a reviewed forward migration. Keep backup/restore instructions and avoid cascading deletion of factual history.
6. Release only after the verification gate. Restore the compatible factual application/schema path if a check fails; do not reactivate automation as a rollback side effect.

Implementation is authorized. Production migration, destructive retirement, and deployment remain separate execution steps. Prepare concrete scripts, verification evidence, and rollback material before any required approval for those actions.

## Responsibilities and tracking

The primary agent owns requirements, architecture decisions, scope control, and final review. Sol 6.1 handles substantial implementation. Use Luna for a bounded unresolved investigation only when it helps; use independent targeted validation for changed historical transformations and isolation behavior. No fixed agent count or repeated review cycle is required.

Record each milestone as planned, in progress, blocked, or complete, with its completion date and evidence links. Record code completion separately from staging and production rollout. Do not mark a milestone complete on static review alone when its exit criterion requires runtime evidence.

Decisions are recorded in `docs/data_service.md` and `docs/data_review_results.md`. No context is inferred from the calendar; incomplete history stays partial. Local curl/later transport needs no child process or persistent worker. Hosted rollout and production concurrency remain separate verification steps.

## Technical references

- [Shiny non-blocking operations](https://shiny.posit.co/r/articles/improve/nonblocking/) for the distinction between scheduling work and keeping the event loop responsive; its separate-process example is not assumed available on this hosting plan.
- [Supabase query limits](https://supabase.com/docs/reference/javascript/select) and [ordered pagination](https://supabase.com/docs/reference/javascript/using-modifiers-range) for completeness checks; the application uses REST through R rather than the JavaScript client.
- [Supabase explicit Data API grants](https://supabase.com/changelog/45329-breaking-change-tables-not-exposed-to-data-and-graphql-api-automatically) for new-table access during migration.


## Follow-up: progressive opening — 2026-10-30

Local implementation separates asynchronous authentication, primary Today content, optional enrichment, and independently loaded player-card sections. Browser flushing is part of acceptance: an observer must not return its pending request promise. Recent rounds combine points and results only when source association is established, with one explanation for missing finality evidence. No automated/prediction features are added.

See `docs/progressive_loading.md` for runtime behavior and `docs/data_review_results.md` for measured verification and remaining limits. Production deployment/latency verification is separate from local implementation. HAR inspection of actual per-player finality fields is deferred until the user supplies captures from the other machine; missing evidence stays unknown in the meantime.

Current-player round display correction: use the shared selected-player request context and round number for live points/matches when optional identifiers are absent. Keep explicit contradictions and saved-only historical context checks. See the review results for regression and mobile verification.

Recent-average correction: average observed scores regardless of finality and allow the separately scoped unknown context; changing N reuses the same bounded history read. Full coverage for players without recorded round observations remains dependent on verifying the fitness-array contract in the forthcoming HAR.

Navigation simplification: consolidate market listings under Today and retain the bid filters there; remove the separate Market tab and server instance.

Older-round display correction: associate loaded point rows before applying display limits and show up to 50 available rounds in the card. Completeness of the production history archive remains a separate verification/backfill concern, not established by this UI correction.


## League round matrix and active-session initial fill

Supersedes the earlier observed-games average correction: calculate the last N consecutive observed league rounds from one scoped player/round matrix. Load the latest-observation view in a bounded batch, retrieve source player summaries with at most two requests in flight, persist acquired scores, and reread the matrix once writes drain. Changing N performs no IO. The initial fill visits the active league catalog, prioritizing the visible table; it stops launching work on navigation/logout and resumes from shared cache or saved complete history. Missing scores stay unknown. No scheduled automation or prediction is introduced.

The migration has been applied; see `round_points_matrix.md` for schema, verification, performance bounds, and source-coverage limitations. Unknown season/scoring remains isolated, so permanent reuse cannot be assumed until source context is verified. Deployment and authenticated Futmondo backfill are separate from the completed SQL migration.


## Follow-up: shared balance and verified banner warnings — 2026-10-30

Local implementation complete: the server owns one primary team-information state and one market state and passes them to Today. The banner calculates display finances from these shared states, so an optional composite finance failure cannot blank already available cash. Red indicates only a fresh confirmed nonpositive cash/projected balance; unknown/loading/stale/failed data remains neutral and clearly labeled. The reservation is `max(withheld,outgoing offers)` and missing fields remain unknown. Full manual-action preflights are unchanged.

Performance and acceptance checks are recorded in `data_review_results.md`: one team/market read, no clock/navigation refetch, focused stale/context and manual-safety checks, mandatory offline lifecycle, and 360/390-pixel browser checks. Hosted account responses and Posit deployment remain separate verification steps.


## Follow-up: stable round-history retrieval and table publication — 2026-10-30

Local implementation and focused acceptance are complete. Keep acquisition/progress independent from the rendered table, publish table-player score snapshots in bounded five-second intervals or when the visible group settles, and ignore unrelated catalog completions unless the league-round anchor changes. Retain completed/empty acquisitions across view navigation. Reuse exact-context recent saved rows after a live source probe, requiring an observation for that probe round without demanding every earlier round. Known context uses a 24-hour observation window; unknown context uses one hour and remains explicitly unknown. Missing scores retain incomplete coverage.

The progress text has a stable DOM node below the average input. The primary browser comparison reduced additional table updates from 72 to two over the same 15-second/83-player fixture. Focused controller checks (33), arithmetic checks (9), and the mandatory offline lifecycle (13 checks plus eight pages) passed; see `data_review_results.md`. Database schema and request concurrency are unchanged. Hosted deployment, authentic account API responses, persistence acknowledgment, and production timings remain separate verification steps.
