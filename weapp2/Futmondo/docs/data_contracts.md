# Factual data contracts

`data_contracts.R` contains pure normalization and context helpers. Normalizers preserve identifiers, source observation time, and missing values; they do not fetch, predict, or execute transactions.

Player-round observations carry `player_id`, `championship_id`, `season`, `scoring_version`, `round`, `points`, `score_status`, and `observed_at`; round/match dates are optional. Keep missing/zero/negative scores distinct. Finality is established from supplied evidence, not fabricated dates.

`league_season_context()` uses an explicit API identity or independently verified per-league configuration. Unknown context stays unknown. League scoring settings retain their original values.

Auction/bid/transfer normalization preserves actual event identities and counterparties. Empty buyer/seller IDs mean Futmondo System/Market. Observation time records when evidence was obtained, not a reconstructed transaction time.

The shared repository reconciles revisions and source coverage within a single context. Existing raw normalization helpers are reused where their factual contracts remain valid. See [database schema](database_schema.md) and [player card](selected_player_module.md).
