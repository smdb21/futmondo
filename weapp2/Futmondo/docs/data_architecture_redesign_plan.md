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

## Follow-up: one shared saved round dataset — 2026-10-30

Local implementation supersedes the per-module/probe-based reuse above. A root-session controller loads the league's score/check union view once, joins each page's visible players locally, and shares acquired observations and progress across module namespaces. Fresh explicit successful checks, including empty results, and explicitly bounded legacy observation reuse avoid a blanket API replay. Missing per-round appearances do not imply an unfinished source request. Manual Refresh forces current table IDs only and survives temporary optional-content hiding.

The immutable success-check migration is applied; [the migration report](round_dataset_migration_20261030.md) separates live SQL verification from deployment. Ordered score chunks must all succeed before a success marker is submitted or acknowledged. Failed/canceled writes never create a successful check. Account replacement/logout clear root state, known/unknown season and scoring stay separate, API concurrency remains two, write backpressure is event-driven, and stored/live score retention is bounded. No scheduled collection, stored login, prediction, or transaction automation is introduced.

Acceptance evidence and remaining hosted limitations are in `data_review_results.md`; the full interfaces, reuse windows, bounds, and cancellation behavior are in `round_points_matrix.md`.


## Follow-up: persist verified last-five league rounds — 2026-10-30

Local implementation maps the user-confirmed oldest-to-newest fitness vector to explicit rounds only after a matching fresh player-summary probe establishes a unique latest-round anchor for the exact source snapshot/context. An agreeing saved window skips that probe. The active table rows and already-fetched catalog share this bounded mapping; an incomplete legacy window can therefore be filled without a per-player API replay. Database maxima and upcoming/current-round arithmetic never establish write identities. Ambiguous patterns, missing timestamps, conflicting context, and late short vectors remain unresolved.

Observation-only chunks preserve source timestamps, zero/negative/NULL positions and newer corrections, and never manufacture full-history check metadata. Failed chunks wait for explicit Refresh, bounded buffers deduplicate retained work, and N>5 can still acquire older histories. The dynamic average is the first data column after Name, displays only numbers or blank missing values, and supplies the shared tables' default descending sort with blanks last. No migration, dependency, deployment, or authenticated backfill is part of this change. Focused known-answer/controller checks, independent validation, the offline lifecycle and the primary synthetic browser evidence are tracked in `data_review_results.md`.

## Rival cash accounting — 2026-10-31

Implemented a root-session DB-first rival ledger, numeric descending cash table without opening-cash column, league-specific user-confirmed €300M fallback, immutable account/league/season evidence, full versioned ideal-XI snapshots and correction-aware payment identities. Notifications trigger rechecks rather than payments. Historical eligibility and verified settlement adapters remain conservative where actual Futmondo contracts are unavailable; exact rival coverage is not marked complete. See [rival_cash_accounting.md](rival_cash_accounting.md) for interfaces, limits and gaps, and [rival_cash_migration_verification.md](rival_cash_migration_verification.md) for primary-agent deployment/database checks.

## Implemented locally: automatically hire coach on login — 2026-10-31

Status: implemented locally. Explicitly requested by the user as a narrow exception to the previous removal of coach automation. See [coach and round-award interfaces](coach_and_round_awards.md). Hosted deployment/authenticated hiring are not verified.

- Add an **Automatically hire coach** checkbox on Today, the first page after login. Default it to enabled for a new preference; remember an explicit disabled choice for subsequent logins, scoped to the account and championship. Resolve the preference before starting the action. Disabling it stops any action that has not been submitted.
- After each successful login, for the selected championship and the user's own team, reuse the fresh shared team-information response to inspect `staff.coach`. The supplied `hirecoach` capture shows `0` before hiring and `1` in a subsequent information response. A confirmed hired state causes no mutation; unknown, stale or failed reads do not authorize hiring. Do not fan out to all championships or trigger hiring on navigation, refresh or timers.
- Confirm the league permits the captured coach mode using its configuration. The capture has `configuration.enableCoach = "automatic"`; other modes require a verified contract before support.
- The captured action is `POST /1/userteam/hirecoach`, with query `{championshipId, userteamId, roundId: null, type: "coach"}` and the usual server-held authentication header. The response has `code = "api.general.ok"`. Its embedded `coach` field is still `0`, so verify completion by refetching team information: the capture's subsequent `staff.coach` is `1`. Do not infer failure from the mutation's embedded staff value.
- Use a dedicated asynchronous mutation path, separate from the replaying read adapter. Guard against duplicate login observers/concurrent submissions for the same account/team; recheck fresh status before submission and invalidate late callbacks after logout or account/league changes. Submit at most once per login context. An ambiguous timeout requires a status read, never an automatic mutation retry.
- Keep Today usable while checking/hiring. Show concise status next to the checkbox: checking, hiring, hired, disabled, unavailable or failed. Refresh only affected team/staff/balance state after success. Do not block the first-page render or redraw player tables for coach progress.
- Acceptance: synthetic captured-shape tests for default enabled, remembered opt-out, already hired, unknown state, unsupported mode, success followed by confirmed status, ambiguous response, duplicate callbacks, logout/context switching, and a new login. Verify delayed requests keep the event loop responsive; run the mandatory offline lifecycle harness. No real hires in tests. Review request volume: reuse the initial shared read, at most one hire plus one confirmation read when needed, no polling loops or per-player work.

The raw `hirecoach` capture stays private and outside Git/deployment bundles. This milestone authorizes only login-time coach hiring; it does not restore trading automation, prediction features, or unattended workers.

## Implemented locally: Admin round-awards retrieval and rival reward reconciliation — 2026-10-31

Status: implemented locally; source and hosted verification limits remain explicit. User supplied a successful dreamteam response and confirmed that its round is still in progress. This supersedes the earlier missing-successful-response limitation; historical-round success and final settlement still need verification.

1. **Use the observed endpoint contract.** Request `POST /1/userteam/dreamteam` with query `{championshipId, type: "dreamteam", round: round_id}` and server-held authentication. `answer.players` is the full ideal XI; `answer.mvp` identifies the single MVP by player ID. Store the request round ID with the response because the answer does not repeat it. Preserve selected roles, points, formation, pointSystem/custom scoring context and observation time. Resolve round numbers/season from explicit catalog metadata, never calendar arithmetic. Do not use nightmareteam data for rewards: its supplied response also contains high-scoring players and an MVP, and its meaning is not established.
2. **Show award coverage in Admin.** Bulk-read saved selections for the selected account/league/season and join the round catalog locally. Show each round's number, selection coverage, MVP, last fetched time and persistence status. Distinguish missing, provisional/in-progress, finality unknown, and confirmed final using verified source evidence. Saved is a storage state, not proof of finality or payment. The submitted current-round selection must remain provisional; missing finality fields must never mark every historical round provisional.
3. **Add explicit retrieval controls.** Show **Retrieve missing round awards** when round selections are missing. Queue one request per missing round ID, validate unique player IDs and a matching MVP ID, and record malformed/empty/error responses as unresolved without erasing a valid saved selection. Offer **Refresh provisional or changed rounds** and selected-round refresh to revisit postponed matches or corrections. A publication notification requests a recheck; it does not prove final settlement. Historical retrieval must be validated with a completed-round response.
4. **Confirm durable saving.** Reuse the existing immutable award-evidence storage and latest-snapshot view; add a narrowly scoped migration only if necessary for scoring context/finality metadata. Keep whole-selection revisions so a corrected XI replaces prior membership instead of accumulating players. Repeated identical data must not duplicate rewards. Show separate fetched, saved and failed counts; confirm saved coverage only after database acknowledgement/readback. Save failures leave live data available and offer explicit retry of the failed work.
5. **Recalculate rival estimates from shared evidence.** Use championship-specific reward configuration, the saved XI/MVP, verified historical rival eligibility and observed movements. Count each applicable ideal-XI and MVP component once; actual payments replace corresponding estimates. Revisions must also remove outdated recipient bonuses. Keep unfinished-round awards separate from credited cash. Update the numeric sortable rival cash column and its coverage/details after a batch completes; saved awards alone must not promote an estimate to a confirmed balance. Continue to expose missing historical eligibility, ranking allocation or payment coverage. Reconcile with accessible own-team cash without invented balancing adjustments.
6. **Keep retrieval responsive and bounded.** Reuse the root-session award cache and one bulk coverage read. Run at most two round reads concurrently under the existing global request budget; persist bounded batches with backpressure. No per-player API requests, repeated history reload on login, background schedules or table redraw per saved player. Keep progress independent of tables; publish result batches or completion. Stop launching work on leaving Admin/logout/context change, discard stale callbacks, and resume from saved coverage when explicitly requested.
7. **Acceptance and delivery.** Use sanitized synthetic fixtures matching the supplied structure: full XI plus single MVP, different round IDs, current-round provisional state, unknown finality, corrected/removed selections, partial failures, durable-save failure/retry, repeat clicks and account/league/season/scoring isolation. Independently verify known-answer rival rewards and duplicate-payment prevention, including postponed-round corrections. Measure delayed-request responsiveness and bounded request/render counts; run the offline lifecycle harness. Document actual checks and source limitations, update the deployment manifest for runtime changes, and keep raw captures/credentials out of Git and deployment.

Completion means Admin can retrieve and durably confirm missing round selections and rival estimates use those saved awards with explicit eligibility/payment coverage. It does not mean every rival's exact cash is known merely because all XI lists are saved. The coach-on-login milestone above is implemented locally. Tests and limitations are tracked in [the integrated results](data_review_results.md) and [runtime interfaces](coach_and_round_awards.md).


### 2026-10-31 reload and award-catalog corrections — implemented locally

- Remove minute-based banner market/team/round refresh; retain the visual countdown, explicit Refresh, targeted retries and transaction invalidation.
- Update existing player table data for new averages without remounting on progress or source timestamps; preserve page/search/filter state. Visible non-average changes regenerate the R-formatted cell payload.
- Stop hidden history clients from demanding table sources.
- Treat captured compact future/next statuses as unverified; explicitly hydrate missing round numbers from identity-checked match-list reads, reuse scoped saved numbers and show distinct IDs until resolved.
- Verify historical match-list query acceptance on the hosted account separately; no live transaction or database migration is part of these corrections.

### 2026-10-31 missing Rivals table — implemented locally

Resolve the selectable team directory independently of financial and award readiness. Pending Shiny read conditions cannot interrupt saved/seed completion callbacks. A late directory starts only unscheduled team reads; one cancelable trailing publication delivers coalesced updates without polling. Show loading/unavailable guidance instead of an unexplained empty panel; preserve unknown cash and existing accounting rules. Focused regression, offline lifecycle and actual-app synthetic browser results are recorded in `data_review_results.md`; hosted account verification remains separate.


### 2026-10-31 continued missing Rivals display — bounded resilience implemented locally

Keep a native team selector independent of the financial Reactable, backed by the already fetched directory and stable account/league-scoped IDs. Preserve an empty initial selection and explicit scouting requests. Report real team/pending-task counts; catch financial build, formatting and fulfilled-callback processing failures, retain safe directory rows and report redacted diagnostic references. Do not change accounting rules, add polling, or infer that a detailed widget rendered successfully from data readiness. Captured-shape replay and synthetic malformed-data regressions are local evidence; the hosted blank-widget cause remains unconfirmed pending its runtime diagnostics.


Selected-rival summary correction (2026-10-31): budget cards reuse the shared league ledger estimate and confidence, with expandable evidence gaps. Position uses the observed API position or total-points rank (ties share rank), explicitly labeled. No additional API/DB requests, timers, caches, or accounting changes; bounded ranking over the league directory and one ledger-row lookup. Historical reward settlement/eligibility integration remains incomplete; this change does not certify estimates or validate hosted data. Focused regression and offline lifecycle checks recorded with delivery.


### 2026-10-31 rival balance reconstruction — implemented locally

Connect the actual `/1/userteam/rounds` source and persisted historical `/1/userteam/roundlineup` snapshots to point, per-round ranking and XI/MVP subtotals. Present the reconstructed numeric balance as the rival result; an official private budget is optional. Reuse the existing scoped evidence table with typed payloads and full per-team snapshot replacement. Use the official ranking calculator, top/flop mapping and tie-sharing rules; exclude known negative starts and open rounds. Reuse saved final lineups and refresh only changed round signatures with bounded concurrency. Keep current-rule and absent ownership/manual-adjustment gaps explicit, including exact unassigned award round/player IDs. Historical roster-only award eligibility remains unconfirmed; no current-roster or undocumented cutoff inference is permitted. Source contracts and local verification limits are recorded in the accounting and review documents.


Admin league settings (2026-10-31): added a read-only, mobile-stacked explanation of starting cash/squad, round rewards and ranking distribution, market auction duration/listing count, and squad/league limits. Explicitly distinguishes API configuration from the user-confirmed opening-cash fallback and missing from zero/false. Only documented fields are displayed. Server passes existing selected league and primary team resource states; no extra API/DB queries, timers, persistence, cache or authorization changes. Rendering is bounded to 17 small setting rows. Focused display checks and mandatory offline lifecycle verification accompany delivery; live configuration/deployment remains unverified.

### 2026-10-31 rival running cash — implemented locally

Use one per-team ledger for the reconstructed league amount, selected-team budget and transaction history. Apply opening cash once, signed source transfers/adjustments and non-overlapping reward components. Calculate each balance on the complete ledger before display filtering/sorting. Keep actual dated movements separate from components without payment dates; undated reward rows show calculation subtotals for reconciliation and never invent payment chronology. The final subtotal is the reconstructed estimate. Preserve missing/zero/negative values and explicitly excluded ambiguous opening allocations. An accessible observed balance stays separate with freshness and discrepancy; it cannot replace or synthetically balance the reconstruction. Transfer summary cards and pivot reuse shared source data. No new retrieval, persistence, polling or schema change is required. Verification and performance measurements are in `data_review_results.md`; hosted account verification remains separate.


Pending offer-error presentation completed with the running-ledger change: market bids and direct offers translate the squad-cap API code into an actionable explanation, wrap on narrow screens, and remain until dismissed. Unknown codes receive a readable fallback; uncertain outcomes advise checking existing offers before retrying. No submission, clause-buyout, preflight, persistence or network behavior changes. Performance impact is limited to notification text/style. `test/test_offer_error_messages.R` passed; combined offline lifecycle passed. No live offer or hosted/mobile verification was performed.


Inflow breakdown (2026-10-31): transaction history shows total positive inflow split into player sales and bonuses, with other credits separately when present. Starting cash, excluded ledger entries and expenses are not inflow; undated estimates are included and labeled. Missing amounts retain a partial-data notice. The squad investment card now labels sales explicitly. `rivals_inflow_totals()` is a single linear pass over the existing shared ledger, with no API/DB reads, writes, caches or timers. Focused running-balance regression covers known-answer 30 sales + 15 bonuses + 7 other = 52 inflow, exclusions and missing values. No accounting formula changes; full lifecycle/browser/live deployment checks are not rerun for this localized display change.

- Implemented locally: rival outflow split into market and clause purchases, with ambiguous purchases and other debits separately reconciled to total outflow. No new data requests or migrations.

### Seven-day remembered browser login — implemented locally; hosted activation pending (2026-11-01)

- Added initially-unchecked browser opt-in, encrypted server-only authentication records, protected HttpOnly cookies, asynchronous restoration, fixed expiry, truthful retry/revocation feedback and account-switch/logout cancellation.
- Reuses the validated league response for startup; preserves the coach preference handshake. Ordinary login works without remember configuration. No scheduled job, polling, unattended harvesting or password persistence.
- Additive schema includes hash and expiry indexes, RLS and server-role-only grants. Production schema was applied separately; hosted secrets, Posit/Android cookie smoke testing and activation remain pending. See `docs/data_review_results.md` and `docs/login_module.md`.
- Exit criteria: focused security/browser/lifecycle checks, independent targeted review, verified production table permissions, and a separate hosted cookie check before setting the explicit enable flag.
- 2026-11-02 follow-up: user reports hosting key/enable variables configured and browser opt-in checked. Fixed locally: saved-access success/failure now stays visible above all page tabs after automatic navigation to Today. Browser regression uses the real dashboard layout with synthetic success/failure and mobile visibility checks. Hosted cookie issuance/restoration remains unverified; configuration alone does not establish that it worked.

### 2026-11-01 rival cash reconciliation — implemented locally; classification migration applied

- Include starting cash once in inflow; use the same ledger for all totals, running cash and rival summaries. Preserve numeric amounts independently of transfer-method coverage. Separate ordinary/clause sales/purchases, position, point, MVP and Best XI rewards.
- Normalize observed saved/live movement types and preserve source fields; replace false assumed pressroom methods with nullable unknown and protect legacy replay/adoption. Share confirmed classifications across buyer/seller through strict unique evidence matching, with one-second recording jitter only.
- Correct the missing `roundlineup` asynchronous read permission so the existing bounded historical eligibility queue works. Group award calculations with player details and date reconstructed position/MVP/XI rows only from unambiguous recorded settlement anchors. Split rounds retain separate recorded events and undated phase-unverified estimates; no duplicate full rewards or invented timing.
- Production classification migration applied and financial/identity digest verified separately. Historical ownership/completeness, unknown clause-completion matching and hosted deployment remain evidence limits. Focused calculation/read-path and mandatory lifecycle results are recorded in `data_review_results.md`.

### 2026-11-01 player-card availability — implemented locally; hosted incident verification pending

- Reuse one team-scoped capacity resource across player cards and project target bids locally. Capacity consumes minimal roster IDs/value through a shared raw payload, removing three presentation-only acquisition dependencies.
- Correct System counterparty handling and ISO chronology in pending-clause reconciliation; do not perform candidate-summary reads for obsolete same-day trades. Preserve source ages across derived caches, inherit source expiry and keep existing 15-second maximum preflight validity.
- Show one loading status and specific prerequisite explanations without relaxing transaction checks. Explicit app Refresh invalidates capacity and failed prerequisite caches. No polling or new background work.
- Real asynchronous synthetic and saved-HAR replay validate successful retrieval/reuse. The captured prerequisites are valid; the exact hosted screenshot `partial` cause is not established, and deployment/live runtime verification remains an exit criterion. Local checks/performance are recorded in `data_review_results.md`.

### 2026-11-01 optional player-filter disconnect — fixed locally

- Reproduced the original module's fatal schema-observer error and destroyed Shiny test session when Active Clause Only encounters a native row without optional `clause_date`. Also cover raw rows without derived `isClause`.
- Normalize only filter inputs, preserve unknowns and original display columns, share authoritative ownership/free-agent semantics, and use positive price/transfer state plus optional UTC lock dates for active clauses. Empty results render and recover without hiding a data exception.
- Scope initial defaults to Today's market (Free Agents Only) and Players (Active Clause Only); retain Clear Filters and other table defaults. Clarify Today's market heading while retaining its existing content.
- Focused filter/widget and existing UI/lifecycle checks pass. No new reads, polling, dependencies or schema changes; hosted/mobile deployment verification remains separate.

### 2026-11-01 numeric player metric colors — implemented locally

- Apply independent red/yellow/green source-wide color scales to Avg Last N in shared player tables and to existing points-to-price columns. Include zero/negative values, leave missing values uncolored and use yellow for equal ranges.
- Compute enrichment and finite ranges before application filters; use Reactable metadata for constant-time client styling unaffected by search, sort or pagination. Publish data and metadata together without widget recreation on history-only changes, including changed hidden extremes.
- Focused generated-JavaScript/module, ratio/average, refresh-stability, deployment and mandatory offline lifecycle checks passed. No I/O, polling, dependency or schema changes. Hosted/mobile visual verification remains separate.

### 2026-11-02 rival bonus recovery and presentation — implemented locally

- Fix nested PostgREST/jsonlite snapshot decoding so saved full XIs, rounds and historical lineups retain their actual arrays; existing scope, scoring, XI validity and finality checks remain authoritative.
- Recover missing completed-round standings with the captured official unpaged ranking endpoint. Persist signature-bound source rankings within existing eligibility evidence; use actual historical participants without manufacturing a score for an absent directory member. Reuse saved rankings/lineups with two concurrent reads, 120 ranking tasks and the existing 600-lineup cap per refresh.
- Replace automatic expanded raw audits on team selection with concise status plus optional collapsed calculation notes. Preserve the full cash ledger and all reconciliation surfaces.
- Focused real-transport synthetic, storage-roundtrip, known-answer, malformed/open/context and offline lifecycle checks pass. No migration/dependency change. Live ranking/lineup API verification, confirmed production saving and hosted/mobile deployment remain separate exit criteria.

- Implemented locally (2026-11-02): positive-only theme-background-to-green numeric pill scale; existing source-wide domains and sorting retained. See data_review_results.md for verification.

Implemented (2026-11-02): abbreviated shared position pills with full-name tooltips.

Implemented (2026-11-02): wrapping shared headers, unclipped offer badges and source-wide duplicate-price suppression; verification in data_review_results.md.


### 2026-11-02 incoming clause signings — implemented locally

- Add a separate incoming-player panel and an incoming count alongside the actual squad count in My Team. Reuse shared transactions and player summaries to show authoritative `transferDate`, Madrid CET/CEST, and strict before/at/after the next shared round start.
- Preserve partial/overdue purchases until fresh source evidence confirms arrival, cancellation or a later sale. Incoming card snapshots use championship ownership and pending-clause state; incoming players stay outside current-roster valuation, counts, lineup and listing actions. Existing acquisition-capacity checks remain fail-closed.
- Queue only incoming candidates with at most two summary reads, reuse existing raw caches, guard contexts/delayed callbacks, and refresh only on stale navigation, manual demand or acknowledged actions. No polling, league-wide scan, migration or new login dependency.
- Focused regressions, existing acquisition checks, mandatory offline lifecycle and synthetic desktop/phone browser checks passed. See `data_review_results.md` for exact results. Production account data and hosted deployment remain separate verification steps.


### 2026-11-02 observed league settings and rival directory — implemented locally

- Use public team-directory squad values, last-access timestamps and signed points consistently in the Rivals table and selected card. Add the requested theme-background-to-red points pills and sortable UTC access column, including financial-error recovery.
- Replace the league-specific €300M/user-confirmed opening-cash fallback with observed `configuration.budget`, retaining saved configuration reuse and actual opening movement precedence. Remove the obsolete defaults-table read.
- Replace the fixed 50% borrowing assumption with `configuration.mnmp` in both display and acquisition checks; respect `mbmv` in bid minimums and retain authoritative API ceilings and final manual preflights.
- Expand Admin to the supplied configuration keys using verified public client/translation evidence, compact responsive disclosure rows, dynamic clause delay and explicit unresolved codes. Exact incoming arrival continues to use source `transferDate`.
- Local focused accounting/configuration, desktop/mobile and lifecycle checks are tracked in `data_review_results.md`. No live account transactions, database migration, deployment or hosted verification are included. This supersedes earlier opening-cash fallback descriptions in this plan.


### 2026-11-02 local chat attachment exclusion — complete locally

- Ignore `.codex-attachments/` in the repository's shared Git rules. All 24 existing untracked images remain on disk and no longer appear in Git changes; future attachments inherit the rule. This repository housekeeping change has no application runtime effect.


### 2026-11-02 durable round history — implemented locally

- Remove the history/check age expiry that could restart league-wide acquisition. Reuse exact-context stored facts, including old acknowledged sparse/empty source responses and complete legacy windows for any requested N.
- Request genuinely absent demanded legacy rounds only for affected visible players. Newer conflicting last-five evidence selects affected players and uses the existing verified snapshot anchor; ignore vectors superseded by newer labelled history/checks.
- Keep manual current-table Refresh, persistence acknowledgement, context isolation, bounded concurrency/backpressure and publication throttling. No migration, new query, polling or live account call.
- Focused reuse/correction/failure checks, the 527-player x38-round local fixture and required offline lifecycle passed. Older corrections beyond available last-five evidence need explicit labelled-source retrieval or Refresh; hosted persistence remains unverified. See data_review_results.md.


### 2026-11-02 remembered login through hosted proxies — implemented and verified locally

- Remove the silent dependency on an original browser WebSocket Origin, which hosted Shiny proxies may omit. Verify HTTPS/same-origin on the cookie HTTP request using its Origin and Host, while retaining an available WebSocket origin, CSRF header, one-use ticket and same-site checks.
- Confirm the returned browser cookie before showing seven-day success. Keep the stable public cookie path across changing Posit worker routes and persistent server storage across process restarts; no new hosting variable, paid hosting feature or background worker.
- Proxy reproduction failed before the fix and passed afterward. Security, actual browser closure plus R restart, worker-route changes, desktop/mobile presentation and offline lifecycle checks passed; detailed evidence and hosted verification limits are in `data_review_results.md`.

- Hosted follow-up: requests now reach the endpoint; its own 403 required support for rewritten backend Host. The strict browser Fetch Metadata fallback and fixed diagnostic labels passed focused security, proxy restart, lifecycle and deployment checks. Final hosted/device verification remains separate.

- Hosted unauthenticated routing/security verification passed after automatic deployment of `a7e0107`: HTTP 200 / absent-cookie response. Authenticated device save/reopen verification remains pending user confirmation; no hosting upgrade or security-setting change was required.


### Table-only rival selection — 2026-11-02

- Remove duplicate rival dropdown and synchronization observers; keep Reactable selection/context safeguards.
- Show all common ledger rows in one financial history, including reconstructed rewards; retain reconciliation and explicit date-source labels. Verify offline lifecycle and saved-bonus recovery; hosted bonus completeness is separate.


### 2026-11-02 reconstructed reward dates from official publications — implemented locally

- Keep each recorded movement's real timestamp. For calculated position/Best XI/MVP ledger entries, prefer an unambiguous unsplit shared position-payment time, then the matching official round-close notification creation instant. Point estimates use notification dates, without borrowing position-payment times. Mark all inferred dates explicitly and retain amount precedence.
- Reuse saved notification IDs/round metadata. Repeated/split-round publications update the one cumulative reward's assumed date to the latest creation time without duplicating cash; read-state updates cannot move it.
- Recover missing dates with verified `notification/list` older-ID pagination, one page per existing asynchronous task, at most two concurrent finance reads and 20 pages per refresh. Persist successful events plus exhausted/continuation cursors in existing evidence and merge fresh seed notifications immediately.
- Focused notification transport/queue, saved-date reuse, failures, context isolation, missing configured-rate, duplication and finance regressions pass. Root lifecycle/deployment checks are reported separately. Older-source retention and hosted date recovery are not established by local tests; precise missing-round evidence remains visible if the source is exhausted.

- Final validation correction: suppress finite zero-valued reconstructed reward cashflows/date demands while preserving actual zero movements and unknown amounts; no totals change.

### 2026-11-02 inactive rival and position bonuses — implemented locally

- Refine historical participants only for a rival with an explicitly saved empty full history when configured allocations match actual position payments in at least two independent closed rounds. Preserve missing/partial/malformed-history safeguards, actual negative-start eligibility and official ranking precedence.
- Reconstruct position rewards for the complete remaining participants, including unpaid rounds, through the shared ledger. Reuse the same participant resolver in refresh scheduling to avoid unnecessary historical-ranking requests. Keep the inactive team in the directory; no invented zero score or duplicate recorded payment.
- Passed: 6 focused participant checks, 18 finance-runtime checks, 16 reconciliation checks, 3 deployment checks and the mandatory offline lifecycle's 13 checks plus 8 tabs. A production-sized pure resolver benchmark averaged 61.5 ms over 20 runs, with no API/database reads. Documentation and the changed runtime deployment checksum are updated. Hosted rollout/data verification remains separate.

### Point settlements validated against reference payments — 2026-11-03

Implemented locally: native point movement normalization, split settlement grouping, independently verified whole-round income, known league payment date / reconstructed amount separation, final-settlement consolidation for rivals, scoped one-batch async repair, persistence reuse, and truthful incomplete point subtotals. Reference records remain separate. Offline acceptance covers supplied rounds 1/6, stale and mismatched round 11, context isolation, corrections, negative points and bounded repair across sessions.

Hosted exit criteria remain open: deploy the change, allow active-session reads to refresh round 11, verify its independently observed final score and eligibility reconcile with recorded payments, and verify the saved result is reused. Production still held 95 running points at read-only inspection; a fixture's 105 closed points is not production evidence. No production mutation or deployment was performed for this work.

## 2026-11-03 — Classification bonus columns (implemented locally)

Added four selected-round euro totals (points, MVP, Best XI, position) backed by the shared reconstructed cash ledger. Both round controls apply consistently, recorded payments replace estimates, and missing coverage stays distinct from confirmed zero. Focused known-answer checks, UI reliability and the offline lifecycle harness passed. Selection performance measured locally; hosted rendering/mobile inspection and deployment are not claimed. See `classification_module.md` and the review record.

### Reference points/position and independent awards — 2026-11-03

Implemented and hosted (commit `fd72e80`, release marker verified): position bonuses require a reference position payment; points retain exact reference reconciliation. Best XI/MVP remain independent of reference award receipt, but require final-category settlement evidence from any recorded league recipient, plus selections and historical eligibility. Interim closed histories and point payments cannot release final awards. Pending rows remain excluded and totals unknown. User accepted official full-round completion on 2026-11-03. Focused final validation and deployment are recorded in data_review_results.md; authenticated round 11 recovery remains unverified in production.

### Recent rounds collapsed by default — 2026-11-03

Implemented locally: the player-card Recent rounds section uses a closed native disclosure with a clickable, keyboard-accessible heading. Existing round content remains available on expansion. Focused profile/round checks and the offline lifecycle harness passed; deployment and browser/mobile visual verification are not claimed.


### Payment-driven cumulative rival points — 2026-11-03

Existing live accounting verified to require reference payments and retain one cumulative derived entry per rival/round. Added a sequential no-payment/first-payment/later-payment/score-recovery/replay regression. Corrected latest-payment wording so it cannot imply final completion. No synthetic transaction persistence or destructive cleanup is needed; real instalments and source revisions remain intact. Validation and rollout are recorded in data_review_results.md.


### ID-less native bonus payment ingestion — 2026-11-03

Fixed the source-contract defect that discarded 14 point and three Best XI payments without `_id`. Explicit local settlement identity now preserves real payments and revisions through the existing storage path. Sanitized full-response regression reconciles cash to 52,129,125 EUR; original ID-preserving synthetic fixtures did not cover this contract. Restored the 17 omitted source records from the user-provided capture without overwriting existing identities. Production evidence totals now match observed cash exactly; reference point amounts also independently match saved round histories 1–11, including refreshed 105-point round 11. See review results for validation and hosted deployment evidence.


### Combined award reconciliation and default financial history — 2026-11-03

Full saved-evidence replay reproduced the remaining 2,000,000 EUR overstatement: native dreamTeam receipts already included two MVP premiums. Added exact independent combined-payment attribution, retained original cash transactions, removed overlapping MVP estimates, and split the same recorded total across bonus columns. Reference reconstructed cash is 52,129,125 EUR. Fixed renderUI's unknown-subtotal Boolean failure and made Transaction & Financial History the default selected-rival tab. Exact isolated checks and rollout are recorded in review results.


### Hide unpaid estimates from Financial History — 2026-11-03

Implemented the requested display change: excluded bonus estimates remain auditable in calculation details but no longer appear in Financial History for either the reference team or rivals. Accounting and balances are unchanged. Focused projection, combined-award/history rendering and deployment checks passed; lifecycle and rollout checks are recorded in review results.


### Rival payment-history presentation — implemented locally 2026-11-03

Added round points/actual position, browser-local date labels, separate Total income/Total expenses boxes, paid category subtotals despite pending rounds, plain balance labels, and Current team tab without the redundant roster heading. Excluded pending bonuses remain absent from payment history. Accounting evidence and cash totals remain unchanged. Focused accounting, recovery, rendering and offline lifecycle checks pass; production startup verification is recorded separately.


### Full league transfer history and aligned rival table — implemented locally 2026-11-03

Removed default maximum-value/maximum-clause caps. Added full fetched league history with local pagination, shared crest presentation, exact source metadata joins for selected-team ledger rows and publication-aware method labels. Removed fixed source page cap while preserving cycle detection and partial coverage. Focused and delayed-loading checks plus offline lifecycle pass. Live authenticated oldest-history coverage remains separate from source-contract fixtures and public rollout verification.


### Locker payment/delivery reconciliation — implemented locally 2026-11-03

Integrated authenticated locker news clause payments into the shared transfer feed and rival cash ledger. Preserved distinct payment/delivery times, linked duplicate source representations, persisted stable normalized source evidence, reused saved facts and preserved context/refresh isolation. Added plain local table dates and matching-size bonus icons. Captured-source replay and offline checks pass; public hosted rollout proof is recorded separately. Current-season source evidence establishes 121 clause payments, 110 later deliveries and 11 paid purchases still absent from pressroom. No claim is made that unrelated rival bonus/manual-adjustment uncertainties have disappeared.


### Transfer pagination boundary recovery — implemented locally 2026-11-03

Fixed pressroom's differing timestamp sort/ObjectId filter contract. Source replay recovered seven omitted purchases across three rivals. HOPE's three missing expenses total 47,843,546 EUR; corrected ledger is 426,122,711.50 EUR versus the owner-reported 426,122,711 EUR. The remaining half euro is a fractional round-6 tied ranking allocation; exact payment/rounding evidence is still requested, and no balancing adjustment is inserted. Deployment proof is tracked separately.


### Tied ranking reconciliation — implemented and locally verified 2026-11-03

Confirmed actual round-6 payment establishes whole-euro truncation of shared position rewards. After recovered purchases and this calculation correction, HOPE ledger equals independently reported current balance 426,122,711 EUR exactly. Previous half-euro evidence limitation is resolved; hosted rollout remains separately verified.


### Rival balance recovery, incremental updates and selection — implemented locally 2026-11-03

Reproduced hosted HOPE 632,606,939 EUR as clause-only history: omitted ordinary transfers net -206,484,228 EUR. Existing database recovery plus saved clauses produces the confirmed 426,122,711 EUR. Added adoption of a concurrently recovered complete feed, semantic calculation reuse, per-team invalidation, bounded shared process cache, and incremental transfer pages with full-audit safeguards. Stabilized team-ID selection through redraw/reorder and applied requested simplified columns/labels. Local regression/browser/lifecycle results and hosted delivery are tracked separately. Persistent serialized calculation snapshots were not implemented; server restarts still rebuild from the existing saved source evidence.


### Settled award freshness — 2026-11-04

Implemented locally: detect award snapshots predating final-category payments; recover them through shared async acquisition and existing snapshot persistence; reuse after successful save. Correct sole decimal position-settlement date handling. Exit checks include a sanitized €500,000 Best XI revision, persistence/reuse across sessions, and full offline lifecycle. Hosted authenticated balances remain a separate verification step.


## Incremental query follow-up — 2026-11-05

Status: **deployed with the database migration applied; public startup verified; authenticated cold/warm history verification pending**.

- [x] Trace slow transaction refreshes through pressroom, locker news, display-cache eviction, replay, and repeated persistence.
- [x] Reuse complete source baselines across normal refreshes; scan the current head through a one-day overlap; retain identity, same-second pagination, payment/delivery and truthful failure behavior.
- [x] Add atomic, account/league/context-scoped persisted source snapshots without storing authentication or enabling unattended collection.
- [x] Make card and shared league history reads incremental by ingestion/observation timestamps; preserve old-round corrections, empty acquisitions, zero, negative and unavailable scores.
- [x] Skip acknowledged unchanged transaction writes; retain retry eligibility after failed/unconfirmed writes.
- [x] Run focused request-count, correction, isolation, failed-save, delayed-response and offline lifecycle checks; review memory, CPU, queue limits and bundling.
- [x] Apply the prepared baseline-cache migration (hosted version `20261105220658`) and deploy commit `3bbb12de0e1bea9e138f1cd49e9bab46eb19b165`; verify the public release marker and connected Shiny session.
- [ ] Verify authenticated cold/warm loading with the user's actual league. No production latency reduction is claimed from fixture results.

Periodic full reads remain explicit correctness safeguards: daily for database history and every seven days for source transaction/news history during active demand. Changes older than the overlap are detected by these audits; source APIs do not expose a verified date-filter query.


## History availability and incoming-signing recovery — 2026-11-05

Status: **deployed and public startup verified; authenticated hosted history recovery remains unverified**.

- [x] Preserve an identified round with an omitted score as unavailable rather than reject the player's full history; retain the missing-container/malformed-response acquisition gate.
- [x] Settle synchronous/rejected reads, retry transient factual reads within shared bounds, count only genuinely completed/reused players, and preserve fitness-anchor reuse during recovery.
- [x] Prevent stranded save jobs, retain sanitized failure categories, retry only safe historical writes, keep acquisition markers after acknowledged score writes, and cancel retries on logout.
- [x] Reuse exact verified locker-payment identities after team renames/catalog removal; keep incomplete/stale source and manual-action gates intact.
- [x] Finish focused/integrated checks and document request counts, responsiveness, memory bounds and verification limits.
- [x] Deploy the application and verify HTTP 200, the new release marker and a connected Shiny session.
- [ ] Verify the reported authenticated league path. The status text alone does not identify the user's failing source response or player; no authenticated recovery is claimed from synthetic fixtures.

This recovery change adds no dependency or database migration. The prior durable transaction-baseline migration is now applied as recorded above.


2026-11-05 authorized rollout update: the private history-cache migration is applied in the configured database (hosted version `20261105220658`), with schema, privileges and update-trigger checks passed. The isolated application release passes its focused and full offline checks; publication and authenticated production behavior are tracked separately in the review results.

Hosted rollout confirmed on 2026-11-05: release `history-loading-recovery-2026-11-05`, commit `3bbb12de0e1bea9e138f1cd49e9bab46eb19b165`, HTTP 200 and a connected Shiny session. Public release hashes match the tested runtime.


## Player availability delayed during history saving — 2026-11-05

Status: **deployed and public startup verified; authenticated hosted timing remains unverified**.

- [x] Reproduce repeated server processing while seven save acknowledgements update a 527-player history matrix.
- [x] Reuse scoped, bounded point-coverage projections and avoid rechecking already resolved fitness after saves.
- [x] Exercise the actual Players-table card path with seven held saves and preserve verified acquisition controls.
- [x] Complete independent cache-invalidation review, focused regressions, responsiveness measurement and offline lifecycle checks.
- [x] Deploy the scoped follow-up and verify the public release marker and Shiny connection.
- [ ] Confirm the reported delay in an authenticated hosted session; local timings are not hosted latency measurements.

Hosted follow-up verified: `790cc1bdab777449f79beaf26a242176c493ea60`, release `player-card-save-responsiveness-2026-11-05`, HTTP200, connected Shiny session and matching tested runtime hash.


## Player-card availability expiry — 2026-11-05

Status: **exact release verified locally; hosted publication pending**.

- [x] Reproduce the reported expiry in the actual card with a synthetic 31-second history delay; trace frozen composite time, sequential reads, queue contention and nested cache expiry.
- [x] Acquire independent prerequisites concurrently, refresh only expired facts once, reconcile changed pending players, and retain source-based deadlines and manual preflights.
- [x] Prioritize interactive queued reads without raising concurrency/request limits; exclude connection-queue time from network timeout; preserve fairness, owner cancellation and single-attempt manual operations.
- [x] Honor absolute validity in outer caches and recheck once if a late summary outlives team checks; use Boolean action-button disable flags.
- [x] Independently review scheduling/cancellation, cache reuse, candidate changes and failure gates; run focused delayed/reopening fixtures.
- [ ] Pass the final exact-release lifecycle and bundle checks, deploy and verify public startup/hash.
- [ ] Confirm authenticated hosted Berenguer behavior; fixture timing does not establish production latency.

## Clause-purchase history refresh scope — 2026-11-05

Status: **implemented and verified offline; authenticated hosted verification pending**.

- [x] Trace successful clause submission through scoped resource invalidation, table callback, shared mutation revision and full history requeue.
- [x] Separate explicit global/card history correction demand from market mutation updates; retain saved/live points, acquisition checks and pending saves.
- [x] Keep the current table visible during a same-context refresh, label retained resource state stale/loading, and preserve acquisition gates and context isolation.
- [x] Verify the actual successful clause callback against a 527-player matrix, delayed catalog refresh, explicit correction refresh, retained acknowledgement and targeted changed-score behavior.
- [x] Run focused regression and required offline lifecycle checks; refresh scoped deployment checksums.
- [ ] Verify authenticated hosted clause purchase after deployment. No real purchase was performed for offline verification.


## Configurable averages and player identity columns — 2026-11-06

Status: **implemented and verified locally; authenticated hosted behavior remains unverified**.

- [x] Trace five-round values versus blank two-round values to the ignored source recent-score sequence; compare sanitized captured source shapes and values.
- [x] Calculate short source windows without inventing numbered observations; preserve explicit historical windows for N>5, including eight, and keep zero/negative/unavailable semantics.
- [x] Move the club crest into Name, remove visible Team, and put Position immediately after Name.
- [x] Complete independent known-answer, acquisition/persistence reuse, desktop/mobile browser, performance and offline lifecycle checks. Delivery uses a scoped commit to the current branch remote, preserving unrelated local/staged work.

No dependency, database migration or unattended activity is introduced. Authenticated hosted data and timing remain separate from local verification.


## Eight-round average recovery — 2026-11-06

Status: **implemented and verified locally; authenticated hosted behavior remains unverified**.

- [x] Reproduce N2 working while N8 stays blank behind an old successful five-round source check.
- [x] Make active N>5 demand fetch missing numbered history, publish on completion, and persist/reuse retrieved scores.
- [x] Preserve bounded requests, source errors, sparse scores, season/scoring isolation and manual Refresh.
- [x] Complete focused, offline lifecycle, request-count and performance checks; deliver the scoped change to branch `2.0`.

Authenticated hosted behavior remains unverified; no live transaction was used.


### Admin initial round labels and future awards — 2026-11-06

Implemented locally: restore the captured league-ID current-round marker; populate initial Admin labels with one cached, ID-joined official bulk-calendar metadata read; exclude current/future rounds from retrieval, table/selector, grouped issues and actionable missing coverage. All explicit retrieval modes refresh the scoped calendar before choosing rounds, including newly completed rounds after a warm cache. Unknown future finality alone causes no metadata read or warning. Historical missing/conflicting evidence remains explicit.

Verification: focused initial-load/reuse/delayed-request, future-boundary and newly-closed-round fixtures, plus the mandatory offline lifecycle. The supplied capture verifies the marker relationship and saved official-client code establishes the bulk endpoint; no authenticated bulk-calendar response or hosted account session was exercised. See `docs/coach_and_round_awards.md` and `docs/data_review_results.md`; local checks do not establish hosted deployment.


## Navigation names — 2026-11-06

Status: **implemented locally; verification in progress**.

- [x] Name the `today` menu item **Today's market** and the `yourteam` menu item **My team**; retain the existing tab identifiers and page behavior.
- [x] Align the visible first-page loading message and current user guides with the menu names.
- [ ] Verify the actual menu, mobile sidebar, full offline lifecycle and generated bundle; push only this task's changes.

This is a text-only presentation change. It adds no request, reactive dependency, timer, cache, database operation, package or environment variable.


## Rival Net Gain ledger reconciliation — 2026-11-06

Status: **implemented and verified locally; authenticated hosted behavior remains unverified**.

- [x] Trace the reported −€4,864,399,502 to squad value minus cumulative historical purchases, which omitted other ledger activity.
- [x] Publish one signed net gain from the shared cash ledger, excluding initial funding and excluded entries; keep observed squad valuation separate.
- [x] Preserve unknown amounts, estimate labels, source revisions, account/league/season isolation and versioned cache reuse.
- [x] Complete independent known-answer, rendered-card, cache/performance and offline lifecycle checks; push only this task's implementation and regression changes to remote branch `2.0` (`c18b045`).

No authenticated account or transaction is used for local verification. Hosted balances and deployment require separate verification; this change does not establish complete source evidence for the reported rival.


## Separate realised trading profit and overall wealth gained — 2026-11-06

Status: **implemented and verified locally; authenticated hosted behavior remains unverified**. This supersedes the earlier Net Gain display interpretation while retaining the internal cash-ledger compatibility helper.

- [x] Display completed player sale/purchase profit separately from reconstructed cash plus current squad value minus starting cash.
- [x] Reuse one source-backed, included shared ledger for matching, related fees, card totals and the player pivot; preserve held assets, revisions, unknowns and context isolation.
- [x] Keep evidence notes/estimate labels, cache reuse and responsive stacking; add no source reads, workers, dependencies or schema changes.
- [x] Complete independent known-answer, source-mapping/cache, rendered-card, mobile/performance and offline lifecycle checks; commit and push only this task's implementation to remote branch `2.0` (`9fb89aa`).

Local fixtures do not certify the user's authenticated history or a hosted deployment. Wealth is measured relative to actual starting cash; no unsupported historical squad allocation is created.


## Rival financial row, standings metrics and calculation details — 2026-11-06

Status: **implemented and verified locally; authenticated hosted evidence and deployment remain unverified**.

- [x] Put all six selected-team financial cards in one desktop row, with responsive smaller-screen wrapping and no page overflow.
- [x] Add sortable Overall wealth gained and Trading profit to the top table using the same known-input gates and cached ledger calculations as the cards; preserve selection, negative values, true zero and unavailable states.
- [x] Show actual numeric formula components and readable grouped coverage notes; retain full internal diagnostics and unresolved historical inputs.
- [x] Reproduce and repair the lineup revision completion race, preserving only current evidence in memory and persistence, with bounded acquisition and reuse.
- [x] Complete known-answer, delayed-response, cache/persistence, desktop/mobile, performance and required offline lifecycle checks on the scoped release; deliver only this task's changes to current remote branch `2.0`.

No dependency, migration, background worker or unattended action is introduced. Available historical lineups still cannot prove absent roster-only ownership or an unavailable historical cutoff; the presentation and local race fix do not establish complete authenticated account totals.

## Live classification round acquisition — 2026-11-06

Status: **implemented and verified locally; authenticated current-account behavior remains unverified**.

- [x] Replace highest scheduled/lineup-round selection with at most two calendar-ID-backed native round-ranking candidates; prefer explicit playing metadata and exclude not-started rankings.
- [x] Keep Latest/General presets through delayed source arrival and preserve manual filters; keep official cumulative General separate from per-round scores and incomplete custom windows.
- [x] Record/reuse scoped display-only score snapshots without changing closed-round financial eligibility or unsettled reward treatment.
- [x] Verify focused request/parser/cache/persistence/selection/coverage fixtures, existing classification regressions and required offline lifecycle. Primary-agent browser verification and integrated review are recorded separately before delivery.

No migration, dependency, autonomous activity or external account/database mutation was performed for verification.


## Persistent rival calculation reuse — 2026-11-06

Status: **implemented and verified locally; hosted durability and latency remain unverified**.

- [x] Persist bounded, scoped calculation snapshots through the existing acknowledged database queue; paint saved table rows before full history acquisition.
- [x] Reuse unchanged ledgers across R processes, rebuild affected transaction participants, and preserve live metadata, unknown values and original cash observation timestamps.
- [x] Verify cache/uncached parity, scope and invalidation, stale callbacks, malformed payload rejection, independent known answers, mobile/desktop saved-table rendering and the required offline lifecycle.

No migration, dependency or unattended worker is introduced. Oversized caches retain a table-only snapshot; award metadata rechecks may conservatively rebuild. This tracking update has no runtime or request cost.


## Round Awards position pills — 2026-11-06

Completed locally: Best XI positions reuse the established colored badges, retaining source roles and explicit unknown values. Focused rendering and 390px/1440px browser checks passed. No shared infrastructure, source, persistence or dependency changes; hosted deployment remains unverified.

## Classification cold-read recovery — 2026-11-06

Status: **deployed; isolated cold-load/browser checks and public hosted startup/release hashes verified; authenticated league data remains unverified**.

- [x] Reproduce the actual bound-directory validation condition cancelling round-filter/status dependencies on a cold page.
- [x] Initialize native calendar/playing-round controls and display bulk live points independently of directory, history, settings and finance completion; retain their reactive recovery and visible loading/failure states.
- [x] Require official cumulative directory points for General, keep incomplete historical windows/bonuses unknown, and reuse legacy catalog plus persisted display snapshots without another ranking request/write.
- [x] Pass the real-bound-adapter cold/failure regression and existing classification loading/live/preset/bonus checks; record the proportional performance review in the review results.

This module-local change adds no API route, schema, dependency, persistence behavior, financial calculation or unattended work. Browser verification, integrated release review and delivery are recorded by the primary agent separately.

## Saved page per account — 2026-11-06

Status: **implemented and verified locally; hosted schema installed; signed-in hosted app behavior remains unverified**.

- [x] Persist the authenticated account's last main page and restore it on normal/remembered login; exclude Login and recheck Admin authorization.
- [x] Keep database reads/writes asynchronous, serialize/coalesce saves, preserve newer visits across sessions and retain truthful retry feedback for failures.
- [x] Pass focused isolation/race/retry/REST checks, the required offline lifecycle, desktop/mobile browser restoration and deployment contract; verify hosted SQL upsert/order/privilege behavior using rolled-back synthetic data.

No dependency, environment variable, worker, source acquisition or transaction behavior is added. Commit/push delivery is recorded in the final task report.

## My Team asking prices and received offers — 2026-11-07

Implemented locally: one numeric Selling by asking-price column, separate received Offer, and a responsive top offers panel beside Incoming players. Complete rival/system offer lists survive highest-offer projection; explicit icons/text identify rival offers, and links select the exact bid in the existing player card. Loading/stale/partial states remain distinct from verified empty results. Existing scoped roster acquisition is shared without extra Futmondo reads or polling.

Complete normalized offer observations now persist in the existing server-only evidence store under an accounting-excluded cache namespace, with account/league/team/season/scoring isolation, source timestamps, completeness and a 15-second source freshness deadline. Fresh database observations are reused before bid acquisition; manual actions/Refresh invalidate earlier saved observations, and failed saves stay retryable. No migration/dependency change. Focused parsing/UI/persistence checks, the mandatory offline lifecycle and desktop/mobile browser checks passed; details and performance limits are in data_review_results.md. Production database round trips, authenticated offers and hosted deployment remain unverified.

## Administrator-controlled app access — 2026-11-07

Implemented: default-enabled Admin switch with server-only database persistence, normal/remembered admission gating, fail-closed errors, admin access preservation, and serialized active-session rechecks. Hosted migration is installed with the enabled seed. Focused synthetic and hosted schema verification are recorded in data_review_results.md; hosted application deployment remains separate.

## App access recheck interval — 2026-11-07

Changed active non-admin access rechecks to two minutes at the user’s request. Initial admission still checks the persisted policy immediately. The timer-boundary regression, deployment contract and mandatory offline lifecycle passed; see data_review_results.md.


## Classification failure identity — 2026-11-07

Implemented locally: preserve original structured read failures across Shiny validation and Classification's status formatting, with safe source labels for catalog/ranking/history/lineup reads. This fixes misclassified error categories and displayed references that did not match server logs. Focused diagnostic and required lifecycle verification are recorded in data_review_results.md. The screenshot's underlying production acquisition failure remains unconfirmed without a failing response or matching diagnostic evidence; it is not marked fixed.


## Previous-round award component coverage — 2026-11-07

Implemented and independently verified locally: round-filtered MVP coverage is independent of unrelated Best XI ownership gaps, and combined-payment attribution requires complete round-specific recipient evidence. The durable calculation version was advanced; recorded cash and shared source evidence are retained. Read-only hosted evidence confirmed that round 11's final XI/MVP is saved, but four award players lack verified historical ownership in the saved lineup/bench records. MVP coverage is corrected; complete Best XI attribution remains unresolved pending authoritative historical ownership evidence. No missing owners were invented, and hosted application deployment is unverified. See `rival_cash_accounting.md` and the executed results.
