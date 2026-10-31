# Rival cash accounting

Rivals now shows numeric **Estimated cash**, sorted descending, with separate **Confidence**, **Last updated**, and debt status. There is no opening-cash column. Expanding a row shows the recorded and estimated components, missing evidence, and the difference between reconstructed cash and an accessible authoritative balance. No balancing adjustment is invented.

## Sources and interfaces

`rival_finance_runtime.R`, sourced by `futmondo_functions.R`, implements:

- `normalize_money_movements(answer)`: decoded movement array or nested `answer` array. Preserves `_id`/`id`, concept, type, category, signed amount, date, and optional explicit bonus/round/player/source identifiers. A missing amount stays `NA`; descriptions never establish a reward category. `complete` means the returned rows parsed successfully, not that all historical activity is covered.
- `read_rival_finance_evidence(account_id, championship_id, season, max_rows=20000)`: one logical paginated read of `rival_finance_evidence_latest`, scoped by the authenticated account, league and explicit season. `get_rival_finance_saved()` also reads the league opening-cash default. Legacy transfer archives without a verified season/reset boundary are excluded. Restricted database credentials stay on the server.
- `build_rival_cash_dataset(..., account_id, season="unknown")`: pure calculation returning `team_finances`, per-team `breakdowns`, and compatibility `all_purchases`. Missing account context fails closed. Supplied evidence is filtered again by account/league/season. The caller must supply the same season for award/eligibility inputs; optional context columns are checked when present.
- `rival_finance_directory(teams)`: selectable league-team rows before financial sources settle. Unavailable cash, reconciliation and squad values remain numeric `NA`; observed directory points/value can still display.
- `shared_rival_finance_source(...)`: one root-session dataset, saved data first, followed by bounded asynchronous source refreshes. The directory resolves independently of award/financial readiness, with explicit loading/unavailable state. Late directory arrivals enqueue each team once per refresh; source callbacks never evaluate potentially pending UI dependencies. A league/account switch clears old data and invalidates late callbacks. Same-context navigation reuses the session result.

Opening cash comes from retrieved league configuration when present. Otherwise league `5b55f9d767214483120b87cf` uses the user-confirmed €300,000,000 per team, also stored in `league_cash_defaults`. No other league inherits that fallback. A recorded opening allocation replaces the separate opening amount; it is not added twice. Multiple budget allocations require reset/season evidence.

## Recorded money and estimates

Immutable movement revisions use source identity `(account, league, season, team, movement ID)`; the latest observed revision replaces its earlier amount. Distinct payment IDs remain distinct, including additional payments after postponed matches. The movement ledger takes precedence over the pressroom transfer ledger as a whole: feeds without common identities are not added together. Unclassified recorded reward income suppresses potentially overlapping estimates. A historical payment with explicit round/player/reward identity suppresses the corresponding estimate. Season points are never multiplied by a rate and presented as already-paid cash.

Full ideal-XI selections and their separate MVP flag are retained. Verified historical eligibility can attribute both components, using historical rules where available and explicitly labelled current-rule assumptions otherwise. Current roster ownership is never used to assign old rewards. The verified settlement-evidence adapter supports credited point totals and known per-round ranking allocations. Repeated player-level evidence is deduplicated to one team/round/reward; conflicting amounts are excluded. This adapter is an internal evidence contract, not an assertion that Futmondo returns these fields.

**Verified** means an accessible authoritative budget observed within five minutes. **Reconstructed** requires explicit history-period coverage, complete accounting and no unresolved assumptions. **Estimated** covers the remaining defensible subtotal. The live movement response currently cannot establish history-period completeness, so successful parsing alone never produces Reconstructed. A user-confirmed opening amount does not itself reduce confidence.

## Publication and split rounds

The shared MVP source reads all saved XI members. New snapshots replace the complete selected-player set for the round, including removed recipients, and preserve nullable scores. The legacy selection table remains intact. Saved snapshots are refreshed after 24 hours, explicit Refresh, or a changed observed notification list. At most 60 catalogued completed rounds are processed in one load, one request at a time.

Notifications are refresh signals only. Neither notification text nor the existence of an official XI creates a financial movement or proves final settlement. A changed list observed in Notifications also invalidates the active-session round/finance refresh key; this does not run unattended collection. Point payments may be partial, while ranking and ideal-XI payments wait for the whole round according to [Futmondo's postponed-match rules](https://help.futmondo.com/article/228-procedimiento-en-partidos-aplazados).

The available endpoint documentation does not establish historical award eligibility, ranking-mode allocation semantics, an authoritative empty-award snapshot, or notification-to-round identifiers. No HAR captures or authenticated live Futmondo responses were available for this implementation. Those gaps are shown in the breakdown. Empty/malformed award responses cannot erase a known selection; a validated revised nonempty selection can. Exact cash for every rival is not claimed.

## Performance and verification

The root dataset caps retained evidence at 20,000 latest identities. Reads use 1,000-row pages, team refreshes cap at 100 teams with at most two composite requests in flight, and each team movement response caps at 5,000 rows. The existing async adapter retains its 32-request budget per composite read. Evidence revisions cap at 256 KiB each; writes use sequential 50-row batches through the existing bounded queue. No writes trigger table updates. UI publications are coalesced to at most one per two seconds during background progress, with one cancelable trailing callback so the last update is delivered, plus immediate directory/source/final-state updates; detail tables paginate ten rows. Existing global cache/queue limits still apply.

Focused tests cover revisions, notification non-payments, signed/unknown money, separate XI/MVP components, historical eligibility, overlap avoidance, partial settlement, duplicate/conflicting settlement evidence, account isolation, saved navigation reuse, delayed-request heartbeat and a maximum of two in-flight reads. Independent accounting validation is recorded separately. `test/test_shiny_simulation.R` is the mandatory offline lifecycle harness. Hosted latency and real Futmondo payment contracts require separate verification.

The applied schema and live database checks are documented in [rival_cash_migration_verification.md](rival_cash_migration_verification.md).
