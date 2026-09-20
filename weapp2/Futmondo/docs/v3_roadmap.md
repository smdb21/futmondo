# Futmondo Insights delivery roadmap

Updated 2026-09-08. Milestone tags below identify delivered code, not completion of all original product workflows. The implementation is profit-focused over a rolling seven-day horizon, while preserving a legal XI. No minimum projected-points preservation requirement applies. Multiple accounts and independently configured leagues are first-class contexts.

## Specification-audit follow-up

**[IMPLEMENTED] ✅ 2026-09-20 — MVP availability and fund-attribution corrections.** Shared cooperative official-MVP discovery/display, explicit unknown/error states, evidence-based MVP bonus breakdowns without altering verified cash, truthful queued/saved sync counts, and persistence errors retained across collection stages. Historical eligibility/payment contracts remain explicitly unresolved where upstream evidence is absent; no inferred award is recorded as a payment.

**[IMPLEMENTED] ✅ 2026-09-08 — critical integration corrections and terminal theme.** Real adapter output can yield an explicitly provisional XI; automation requires verified rules. Your Team reconciles own listings, preserves source freshness and allows profitable points-reducing transfer previews. Shared black/green monospace tokens cover Shiny, Reactable and Plotly. Desktop/mobile browser fixtures exercise nine tabs.

**[IMPLEMENTED] ✅ 2026-09-08 — observation and advisory planning integrations.** Revision-preserving SQL/history replay, rotating independent observation subscriptions, saved-outcome matching and production worker callers for chronological evaluators. Added listing/fixture evidence storage, observed executable-offer estimates, bounded joint purchase/sale scenarios, cost-basis realized-profit ledger, detailed XI comparison, authorized all-league Insights unread counts and durable coach preferences. Per-job failures persist; action identities include listing/offer and policy content.

**[PARTIAL] end-to-end original product acceptance.** Complete upstream eligibility/auction visibility, verified unsold-settlement provider, round timing/finality, learned settled-sale calibration, distributed observation leases and all advanced rule contracts are not established. The explicit settlement-ingestion interface is not proof that live evidence exists. Joint planning uses approximate bounded search and conditional sale funding. Statistical approval, fourteen observation days, staging worker concurrency, reviewed production migration/backfill and operational deployment remain pending. Coach hiring and lineup submission remain unavailable.

## Delivery 1: correctness and notifications

**[IMPLEMENTED] ✅ 2026-09-07 — application correctness milestone.** Corrected player-detail statistics, stable descriptive FIS, injury/eligibility ranking, complete eleven-player assignments, rational bid ceilings, signed cash and outgoing commitments. Market actions use explicit event types and reconcile API state. Classification uses selected-round results and dream-team/MVP identities. Login fields are empty, logout and account-scoped caching are implemented, and administrative handlers verify identity. Added notification bell/inbox, filters, explicit acknowledged read actions and generated alerts.

**[IMPLEMENTED] ✅ 2026-09-07 — multi-league application milestone.** Login lists the account's leagues and supports switching. Histories, forecasts, rules, bonuses, actions and policies retain their championship context; overlapping player IDs do not transfer pending action state. Regression tests exercise two different league contexts.

**Production pending:** apply reviewed migrations and verify deployed session isolation/hosting setup. Full formation/club restrictions and reliable deadline metadata remain contract-gated.

## Delivery 2: clean history and rival explorer

**[IMPLEMENTED] ✅ 2026-09-07 — ingestion/explorer code milestone.** Added source-event identities, nested losing bids and authoritative winners, explicit visibility/finality/cutoffs, market listing snapshots, scoped rule and match history, legacy archive/adoption SQL, and conditional rival bid ranges with freshness/evidence counts. Background collection moves telemetry writes out of Shiny's event loop.

**Production pending:** staging/production SQL execution and backfill review. Unknown auction visibility must not become fabricated nonparticipation; unknown season/scoring records do not become training examples. Automatic matching of unobserved unsold outcomes remains unavailable.

## Delivery 3: validated bid/resale models and profit planner

**[IMPLEMENTED] ✅ 2026-09-07 — advisory models and evaluation tooling.** Participation shrinkage, pooled log bid/value amounts, tie-aware combined win scenarios, no-change/trend resale forecasts, expected euro profit/downside, collective cash/slot constraints and no-bid outcomes. Persist versioned forecasts and scenario assumptions. Expanding chronological evaluators compare point/price/bid estimates against baselines and prevent timing leakage.

**[PENDING] statistical acceptance.** No claim of calibrated probabilities, profitable trading or validated live automation is made. Real chronological evidence, executable sale outcomes, sufficient participation visibility and at least fourteen observation days are still required. Price error/coverage is not realized trading profit. Features such as venue/opponent effects or xG/xA wait for trustworthy inputs and demonstrated improvement.

## Delivery 4: expected-points XI and saved scenarios

**[IMPLEMENTED] ✅ 2026-09-07 — advisory optimization milestone.** Recency/position point forecasts for one and three rounds, exact player-to-position assignment across supported formations, explicit multiposition/captain/bench rules, submitted-lineup comparison, transfer preview and saved account/league scenarios. Bench priority is advisory; substitutions are not simulated as guaranteed points.

**Remaining capability gates:** advanced formation/club restrictions must be verified for each league; unknown configuration is not guessed. Lineup submission has no verified mutation contract and is unavailable.

**[IMPLEMENTED] ✅ 2026-09-07 — coach preference UI.** Your Team has a default-selected, account/league/team-scoped preference with explicit pending status; 7 dedicated and 14 existing UI checks pass. **Coach hiring remains [PENDING]** until a successful per-round hiring request/response is captured. No hiring endpoint or worker action was invented. See [coach integration](coach_hiring.md).

## Delivery 5: unattended automation

**[IMPLEMENTED] ✅ 2026-09-07 — observation worker and policy infrastructure.** Encrypted sessions, per-league policies/limits/expiry, independent scheduler, account serialization, idempotency keys, execution fencing, pause/history/failure alerts and conservative reconciliation are implemented with offline tests. Automation starts off; saved policies begin in shadow mode.

**[PENDING] live rollout.** PostgreSQL concurrency integration tests, operational worker deployment, verified deadlines/solvency/eligibility contracts and observation/validation gates precede live use. An uncertain response retains the account lock; absence of an offer is not sufficient evidence to retry. No production trades, lineups or notification read states were changed during implementation.

## Verification and setup

Actual focused results and limits are recorded in [implementation status](implementation_status.md). Setup/dependencies/secrets/migration order are in [README](../README.md). The offline full application harness replaces the previous simulation path that could call real services. Model, UI and worker details are documented alongside their modules.
