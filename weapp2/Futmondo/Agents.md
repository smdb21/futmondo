# Futmondo development rules

## Scope and product boundary

The authoritative application is `/home/rstudio/workspace/futmondo/weapp2/Futmondo`. Work inside this directory; parent Java/R prototypes are unrelated legacy projects. Preserve existing user changes and review scoped diffs.

Futmondo provides observed player/team/market data, historical records, official notifications, descriptive statistics, and explicit manual transactions. Prediction models, forecast-based recommendations/optimizers, Smart Bid, trading automation, coach automation, scheduled observation workers, and stored background sessions have been removed from the product scope. Do not reintroduce them or list them as future milestones without a new explicit user request.

Explicit scope exception requested on 2026-10-31: implement login-time coach hiring controlled by the default-enabled **Automatically hire coach** checkbox, with an already-hired check and duplicate-submission protection. See the implemented-local milestone in `docs/data_architecture_redesign_plan.md`; other removed automation remains out of scope.

Normal active-session loading, caching, and recording of already-fetched observations are data infrastructure. They must not perform autonomous trading or unattended harvesting.

## Official Futmondo client as an API reference

Use [Futmondo's official web app](https://app.futmondo.com/) and its [public JavaScript bundle](https://app.futmondo.com/main.dart.js) as first-party implementation evidence for how to query Futmondo data. This is the code a browser downloads to run the official app. Inspect it alongside existing application adapters and captured requests/responses before guessing an endpoint or asking the user to supply one.

- Trace the relevant request and its response parser together: establish the HTTP method, API route, authentication/request envelope, query parameters, identifier meanings, response nesting and field mappings. Distinguish source league IDs from fantasy championship and user-team IDs.
- Follow the app entry point/bootstrap if the bundle URL changes. Search for route strings and field names; minified function names can change between builds. Record the source URL, inspection date and relevant evidence in `docs/`, with a bundle digest when useful.
- Corroborate the client contract with available captured responses and focused synthetic fixtures. Separate code-level evidence from a live response or authenticated UI verification; public client code alone does not establish that an account has complete or available data.
- Integrate verified reads through the existing authenticated factual worker and durable database observations. Browsing continues to read persisted data; endpoint discovery does not change manual-action safeguards or authorize new transactions. Keep credentials and private response bodies out of documentation and diagnostic output.

Verified example (2026-11-16): `POST https://api.futmondo.com/2/league/matches` uses `query: {leagueId: <source league ID>}` in the existing authenticated request envelope. Matches are in `answer.rounds[].matches[]`; `h` and `a` identify the home and away club objects, and `info.date` supplies kickoff. See [the recorded fixture contract](docs/lineup_module.md#source-and-payload) and `mvp_api_answer()` / `get_award_round_metadata()` in `mvp_runtime.R` for the implemented request and parsing path.

## Data architecture

Follow `docs/data_architecture_redesign_plan.md` and the implemented contracts in the data-layer documentation. UI modules consume shared resource state. Pass account and league/team context explicitly, preserve source timestamps, bound caches and queues, and invalidate only affected resources.

Whenever API results are described as cached, include database persistence in the design. Production now runs on a Contabo VM with Docker/Dokploy, as reported by the owner on 2026-11-11. Container/session/process memory remains temporary; results needed in later sessions or after a restart/redeployment must be stored in the database. Reuse valid persisted observations before refetching, retaining account/league/season/scoring context, source timestamps, freshness and completeness metadata. Memory caches are bounded accelerators only. A failed or unacknowledged database write must remain retryable rather than being treated as durable storage. This applies to factual API data, subject to the authentication restrictions below.

A timer or `tryCatch()` does not make synchronous HTTP non-blocking. Verify responsiveness with delayed-request checks. The Contabo VM permits evaluating child processes and managed services; verify actual VM resources and deployment capabilities before sizing or configuring them. See docs/contabo_performance_review.md. Reviewing background-processing options does not authorize deploying services or reintroducing unattended authenticated collection, stored background sessions, or automatic transactions.

Do not manufacture historical values, scores, dates, balances, or season identity. Keep unavailable values distinct from true zero; preserve negative points. Keep different leagues, seasons, and scoring versions separate. Retried persistence must not duplicate source observations.

In transaction records a missing or empty buyer/seller ID means the Futmondo System/Market, not a corrupt counterparty. Preserve this domain rule.

Keep explicit manual-action preflights. An uncertain transaction response must be reconciled, never automatically retried.

## Documentation and delivery tracking

Document new or changed functions, modules, interfaces, payload shapes, and data behavior in `docs/`. Document environment variables, dependencies, and setup in `README.md`.

Track work in `docs/data_architecture_redesign_plan.md`; record actual checks and limitations in `docs/data_review_results.md`. Mark milestones complete only when their stated exit criteria are met. Distinguish local implementation and isolated validation from production migration and deployment.

The former prediction/automation roadmap is superseded. Keep `docs/v3_roadmap.md` as a short pointer to the current scope.

## Feature comparison and login inventory

Every time a feature is added or materially expanded, compare its actual behavior with the current official Futmondo app and authoritative official guides/release notes. Record the dated sources, official equivalent, specific LigaRadar addition and any verification limits in `docs/feature_comparison.md`. If it adds a new capability beyond the official app, add it to `login_feature_tiers()` in `Modules/Login_Module.R` and the comparison list in the same task. Use Essentials for exploration and Advanced for higher-value lineup, ranking and financial analysis. Keep the two tiers side by side on desktop and stacked on mobile, without pricing, subscription language or access gates. Do not advertise planned/unimplemented features, treat undocumented behavior as proven absent, or claim shared capabilities are exclusive. Recheck existing entries when contrary official evidence appears.

## Verification

Every modification must pass a performance-impact review before it is considered complete. Evaluate each affected data and UI path for API/database request volume, cache reuse and invalidation, blocking work, reactive updates and timers, memory growth, and rendering cost, including on mobile. Keep the app responsive during loading, refreshes, and navigation. For changes that may affect runtime, use the smallest relevant existing check or measurement to verify the impact, and resolve responsiveness regressions before completion. Record the review outcome and verification limits with the change. For documentation-only or other changes with no runtime effect, explicitly record that assessment; no performance tests are required.

Use the smallest meaningful existing tests for each changed behavior. Add focused coverage for uncovered data, cache, isolation, or manual-action risks. Independently verify changed point/history transformations with known-answer fixtures.

For shared infrastructure, startup, or cross-cutting UI changes, run `Rscript test/test_shiny_simulation.R`, the offline lifecycle harness. Never use real accounts or transactions in tests. Report executed results honestly; repair failures before claiming the affected behavior is complete.

Regenerate and inspect `rsconnect::writeManifest()` after dependency changes. Exclude credentials, HAR captures, tests, internal documentation, obsolete source backups, and development scripts from the application bundle.

## Security and presentation

Keep database credentials on the R server. Maintain context isolation, restricted database access, and safe diagnostic logging. Do not print tokens, passwords, or raw private response bodies.

Use responsive Bootstrap structures and mobile-safe CSS; avoid fixed-width controls that overflow narrow screens.

Explicit scope exception requested on 2026-11-01: optional seven-day remembered browser login may retain only encrypted Futmondo authentication tokens with server-side expiry/revocation and a protected HttpOnly cookie. This restores interactive access only; it does not authorize unattended API harvesting or stored-session workers.


Explicit scope exception approved on 2026-11-11: implement a Docker-supervised Python service for per-account opt-in unattended factual acquisition, with durable DB snapshots and reuse of the R parsing/accounting engine. Consent is separate from browser remembered login; encrypted worker tokens persist until disabled or rejected. This does not authorize background transactions or coach/lineup automation. See `docs/python_worker.md`.

Explicit scope update approved on 2026-11-16: factual sync connects automatically on successful login, continues after logout, and uses normal cadence around the clock. Browsing always reads persisted observations; navigation, card opening and Refresh must not acquire source data. Explicit manual-action safety checks remain live. Disconnect revokes factual worker access and signs out; scheduled purchase consent remains separate. Preserve the existing bounded shared worker lanes and checkpoint progress during credential renewal. See `docs/database_first_sync.md`.


Explicit scope exception requested on 2026-11-15: permit individually confirmed, capped, one-shot scheduled clause purchases at a rival player's published opening. Purchase consent and encrypted access are independent of factual Background sync. Each job reserves an offer slot/funds, follows verified lock extensions, expires five minutes after opening, and reconciles uncertain writes without blind retries. Closed-clause retries require an evidenced native rejection code. This does not authorize other trading automation. The account-scoped, disabled-by-default temporary locked-clause diagnostic sends only an explicitly confirmed real request; remove it after capturing the rejection fixture. See `docs/scheduled_clauses.md`.
