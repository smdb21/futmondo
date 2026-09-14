# Implementation and verification status

2026-09-08. See [roadmap](v3_roadmap.md) for delivered milestones and rollout gates. This status reports executed checks, not assumed production readiness.

## Specification-audit follow-up

Delivered: provisional XI through real rules adapters; own-listing reconciliation; immutable source freshness and auction revisions; profit-first transfers and joint cash/roster scenarios; shared terminal theme; independent rotating observations and feedback callers; all-league alerts; persistent coach preference; detailed submitted-XI changes. [Runtime](insights_runtime.md), [planner](portfolio_planner.md) and [theme](terminal_theme.md) document contracts and limitations.

This is an advisory implementation, not a validated trading system. Complete auction visibility/eligibility and verified unsold ingestion still need upstream evidence. Current captures do not establish all round timing/finality or advanced legality rules. Settled-sale calibration, distributed observation leases, staging worker concurrency, production migration/backfill/deployment, chronological acceptance and fourteen observation days remain pending. Hiring and lineup submission still require successful request/response captures.

The earlier test table below is the prior baseline; follow-up results are recorded separately rather than treating old counts as newly executed acceptance.

### Follow-up verification executed 2026-09-08

The following focused suites were selected for changed adapters, history, models, private state and UI flows; all passed with zero failures:

| Script | Passed |
|---|---:|
| `test/test_review_completion.R` | 22 |
| `test/test_data_contracts.R` | 11 |
| `test/test_history_persistence.R` | 10 |
| `test/test_prediction_engine.R` | 54 |
| `test/test_intelligence_correctness.R` | 22 |
| `test/test_intelligence_module.R` | 10 |
| `test/test_multileague_analytics.R` | 10 |
| `test/test_automation_runtime.R` | 16 |
| `test/test_ui_reliability.R` | 14 |
| `test/test_coach_preference.R` | 8 |
| `test/test_deployment_contract.R` | 3 |

Total: **180 focused checks**. The required cross-cutting check, `test/test_shiny_simulation.R`, passed **7 lifecycle groups and 9 rendered tabs** with zero HTTP attempts. It includes an owned listing with a disjoint, nonempty general market and account/league switching. `test/test_observation_migration.cjs` passed **6 isolated PostgreSQL groups**, including uncertain legacy provenance. `test/test_browser_theme.cjs` passed **3 viewport groups** (1440/390/360 pixels), each covering nine tabs; the mobile screenshot was inspected. All 31 application/module/worker R files parsed; scoped whitespace checks passed. Empty fixture charts emit harmless Plotly trace warnings.

The deployment manifest was regenerated. Tests use offline fixtures and an isolated database; no production account mutations, migrations or deployment were performed.

## Earlier baseline

The application implementation includes correctness repairs, account/league isolation, notifications, historical ingestion contracts, rival-bid and resale/point models, profit planning, exact XI assignment, saved scenarios, and an observation-first worker. Configuration/deployment changes are in [README](../README.md).

Executed checks:

| Script | Result |
|---|---|
| `test/test_data_contracts.R` | 11 passed, 0 failed |
| `test/test_history_persistence.R` | 10 passed, 0 failed |
| `test/test_har_contracts.R` | 5 passed, 0 failed; zero HTTP |
| `test/test_final_audit.R` | 6 static migration guards passed; SQL not executed |
| `test/test_supabase_schema_helpers.R` | 15 passed, 0 failed |
| `test/test_prediction_engine.R` | 54 passed, 0 failed |
| `test/test_intelligence_correctness.R` | 22 passed, 0 failed |
| `test/test_intelligence_module.R` | 10 passed, 0 failed |
| `test/test_multileague_analytics.R` | 10 passed, 0 failed |
| `test/test_deterministic_fixes.R` | 55 passed, 0 failed |
| `test/test_today_recommendation_policy.R` | 37 passed, 0 failed |
| `test/test_automation_runtime.R` | 16 passed, 0 failed |
| `test/test_ui_reliability.R` | 14 passed, 0 failed |
| `test/test_deployment_contract.R` | 3 passed, 0 failed |
| `test/test_shiny_simulation.R` → `test/test_application_offline.R` | 7 lifecycle groups plus 9 rendered tabs passed; zero HTTP |

The focused scripts contain **268 passing checks**. The broad lifecycle check was selected because authentication, shared caching/persistence and multiple module flows changed. It tests actual startup/login, navigation, refresh/logout, forged administrative inputs, escaped notifications, failed read acknowledgements, and A→B→A league switching with overlapping players but different cash/rivals/scoring. Stale player confirmations and policy selections cannot mutate the newly selected league. Plotly emits warnings for empty synthetic charts, without test failure.

HAR tests replay response bodies only. No captured request is sent; no live trade, lineup, production migration or notification acknowledgement is part of testing. A separate authorized read-only API inspection confirmed current lineup/bench and league configuration shapes.

The deployment manifest includes `openssl`, `later` and the new runtime/modules; it excludes `.Renviron` and every local HAR capture. Code whitespace checks pass; the pre-existing user change in `player_columns_to_hide.txt` was preserved.

Production requirements remain: staging PostgreSQL migration/locking validation, reviewed backfill, worker/secret deployment, complete league deadline and advanced eligibility contracts, chronological model acceptance and fourteen verified observation days. Unknown data remains visible and blocks dependent mutations. Realized-profit validation requires actual settled purchases and executable sales; a price-model backtest does not establish that result.


Coach preference follow-up (2026-09-07): `test/test_coach_preference.R` passed 7/7 and `test/test_ui_reliability.R` passed 14/14. The checkbox is delivered; hiring remains pending a verified endpoint. No network requests or account mutations were made by the new preference. See [coach integration status](coach_hiring.md).
