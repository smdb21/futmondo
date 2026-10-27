# Futmondo development rules

## Scope and product boundary

The authoritative application is `/home/rstudio/workspace/futmondo/weapp2/Futmondo`. Work inside this directory; parent Java/R prototypes are unrelated legacy projects. Preserve existing user changes and review scoped diffs.

Futmondo provides observed player/team/market data, historical records, official notifications, descriptive statistics, and explicit manual transactions. Prediction models, forecast-based recommendations/optimizers, Smart Bid, trading automation, coach automation, scheduled observation workers, and stored background sessions have been removed from the product scope. Do not reintroduce them or list them as future milestones without a new explicit user request.

Normal active-session loading, caching, and recording of already-fetched observations are data infrastructure. They must not perform autonomous trading or unattended harvesting.

## Data architecture

Follow `docs/data_architecture_redesign_plan.md` and the implemented contracts in the data-layer documentation. UI modules consume shared resource state. Pass account and league/team context explicitly, preserve source timestamps, bound caches and queues, and invalidate only affected resources.

A timer or `tryCatch()` does not make synchronous HTTP non-blocking. Verify responsiveness with delayed-request checks. Do not assume control of hosting processes, child processes, cron, or services: the app uses Posit Connect Cloud's free plan.

Do not manufacture historical values, scores, dates, balances, or season identity. Keep unavailable values distinct from true zero; preserve negative points. Keep different leagues, seasons, and scoring versions separate. Retried persistence must not duplicate source observations.

In transaction records a missing or empty buyer/seller ID means the Futmondo System/Market, not a corrupt counterparty. Preserve this domain rule.

Keep explicit manual-action preflights. An uncertain transaction response must be reconciled, never automatically retried.

## Documentation and delivery tracking

Document new or changed functions, modules, interfaces, payload shapes, and data behavior in `docs/`. Document environment variables, dependencies, and setup in `README.md`.

Track work in `docs/data_architecture_redesign_plan.md`; record actual checks and limitations in `docs/data_review_results.md`. Mark milestones complete only when their stated exit criteria are met. Distinguish local implementation and isolated validation from production migration and deployment.

The former prediction/automation roadmap is superseded. Keep `docs/v3_roadmap.md` as a short pointer to the current scope.

## Verification

Use the smallest meaningful existing tests for each changed behavior. Add focused coverage for uncovered data, cache, isolation, or manual-action risks. Independently verify changed point/history transformations with known-answer fixtures.

For shared infrastructure, startup, or cross-cutting UI changes, run `Rscript test/test_shiny_simulation.R`, the offline lifecycle harness. Never use real accounts or transactions in tests. Report executed results honestly; repair failures before claiming the affected behavior is complete.

Regenerate and inspect `rsconnect::writeManifest()` after dependency changes. Exclude credentials, HAR captures, tests, internal documentation, obsolete source backups, and development scripts from the application bundle.

## Security and presentation

Keep database credentials on the R server. Maintain context isolation, restricted database access, and safe diagnostic logging. Do not print tokens, passwords, or raw private response bodies.

Use responsive Bootstrap structures and mobile-safe CSS; avoid fixed-width controls that overflow narrow screens.
