# Factual data contracts

`data_contracts.R` contains pure normalization and context helpers. Normalizers preserve identifiers, source observation time, and missing values; they do not fetch, predict, or execute transactions.

Player-round observations carry `player_id`, `championship_id`, `season`, `scoring_version`, `round`, `points`, `score_status`, and `observed_at`; round/match dates are optional. Keep missing/zero/negative scores distinct. Finality uses only explicit per-player logical `isFinished`/`finished` flags: consistent true means `final`, consistent false means `provisional`; absent, malformed, or conflicting evidence means `unknown`. Missing scores remain `unavailable`. Round or fixture completion cannot establish player-score finality. Reconciliation and persistence preserve unknown status. Older stored provisional labels have uncertain provenance and are not bulk reclassified. These existing flag names remain compatibility handling; their real wire paths/types must be checked against the raw HAR files when supplied.

`league_season_context()` uses an explicit API identity or independently verified per-league configuration. Unknown context stays unknown. League scoring settings retain their original values.

Auction/bid/transfer normalization preserves actual event identities and counterparties. Empty buyer/seller IDs mean Futmondo System/Market. Observation time records when evidence was obtained, not a reconstructed transaction time.

The shared repository reconciles revisions and source coverage within a single context. Existing raw normalization helpers are reused where their factual contracts remain valid. See [database schema](database_schema.md) and [player card](selected_player_module.md).

Authoritative active-league metadata now supplies source competition/season through `competition_context.R`. `fm_scoring_version()` canonicalizes object ordering for awards, points and lineups. Source round/match IDs are preserved; explicit unknown archives remain unknown. See [general competition contracts](general_competition_context.md).


## Stable observed finance card — 2026-11-15

`round_finance_card_display(finance)` projects only the amounts, offer count,
manual pending amount, missing-value loading labels and observed-amount styling
needed by the global balance card. `round_finance_display()` retains its strict
freshness, completeness, error and solvency contract. The new projection never
changes source state, extends a deadline or authorizes a manual action. Known
positive amounts use the same neutral observed style through refresh/expiry;
known nonpositive balances remain visually marked even when their observation
is stale. Unknown amounts remain unavailable or initially loading.

Root `round_finance_card_RV` stabilizes this projection. Independent
`round_finance_status_display`/`round_finance_status` renders the actual financial
message, unverified/expired checks and source-clock placeholder. Source timestamps
still update through `round_finance_checked` without replacing the card or its
browser countdown. `round_clause_pending_display` compares rendered scheduled
purchase details so job metadata polls do not rebuild the header; changed
reservations and meaningful job states still update. No source acquisition,
database write, persistence context, polling interval or action preflight changes.
