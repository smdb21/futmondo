# Factual database schema

The canonical new-install schema is [scripts/schema.sql](../scripts/schema.sql). Existing installations use the forward migrations in [migration and rollback](database_migration.md); historical migration files are not a fresh-install sequence.

## Data ownership

`championships`, `real_clubs`, and `players` store league/catalog identities. `user_teams` stores current team identity and observed state. League-specific roles and ratings belong in league observations rather than a shared player identity.

`player_history` stores actual value snapshots and cumulative totals. It is not a source of per-round points. `user_team_history` stores league-scoped standings/finance observations. Both retain a nullable source `observed_at` separately from ingestion `recorded_at`; old missing source times remain missing.

`player_match_observations` stores round scores keyed by player, championship, season, scoring version, round, and observation time. Revisions remain separate. Zero and negative points are valid, and match/start dates are optional. `league_rule_snapshots` preserves the scoring context.

`market_transactions`, auction/bid observations and their revision history retain actual transfer evidence. Missing buyer or seller IDs mean Futmondo System/Market. `round_dream_team` stores official round results, including MVP status. `player_daily_snapshots` contains league-specific factual snapshots.

## Ingestion and queries

Catalog synchronization seeds known club identities before player foreign keys. Source-time uniqueness makes repeated identical observations idempotent. Team-history ownership is checked against its team; conflicting league attribution is rejected. Legacy transfer reconciliation preserves the established historical record and source-event identity.

Indexes support league/player/time/ID pages, league standings pages, contextual round revisions, and retained foreign-key lookups. Readers must use deterministic ordering and pagination; SQL indexes alone do not establish complete API results.

The R server uses explicit service-role grants. Browser roles cannot read private histories. RLS remains enabled without browser access policies. Catalogs are also accessed through the R server.

## Verification

`node test/test_factual_schema.cjs` checks fresh installation, repeat execution, known-answer score revisions, league ownership, private access, existing-install preservation, retirement, and transaction rollback. `test/test_observation_migration.cjs` checks preserved historical migration behavior using the pre-redesign schema fixture.
