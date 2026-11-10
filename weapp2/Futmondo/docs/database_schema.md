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


## Complete source history baselines

The additive migration `scripts/migrations/20261105201845_complete_history_baselines.sql` adds `complete_history_baselines`; fresh installations include the same DDL in `scripts/schema.sql`. This is a server-only cache with RLS enabled and no public, anon, or authenticated privileges. Only `service_role` can read/write it. Its primary key is `(account_id, championship_id, season, scoring_version, resource)`; allowed resources are the league's pressroom and locker-payment feeds.

The bounded, compressed normalized JSON snapshot and completion metadata are stored atomically with encoding, compressed-payload SHA-256, uncompressed byte count, semantic snapshot hash, source observation time and database recorded time. The app rejects partial, malformed, mismatched, oversized or credential-bearing content. Per snapshot limits: 20,000 rows, 16 MiB JSON and 900 KiB encoded payload; the existing queue still caps each job at 1 MiB. A trigger prevents an older concurrent observation overwriting a newer snapshot. This table does not replace factual transfer/auction/bid ledgers or prove their separate writes succeeded. Removing cache rows only forces reacquisition. The normal reset helper clears this cache before factual tables.

The authorized rollout applied this migration to the configured production database on 2026-11-05, recorded as `20261105220658`. Column/constraint/RLS/grant/trigger metadata checks passed; authenticated application cache saves are verified separately.

## Application admission policy

`application_access` is a server-only singleton (`id = main`) with `admin_only BOOLEAN NOT NULL DEFAULT TRUE` and `updated_at TIMESTAMPTZ`. The migration seeds TRUE; RLS and revoked browser grants protect reads/writes. Only service_role can select/insert/update; deletion is not granted and factual-data resets exclude this table. See [app_access.md](app_access.md).


## Competition context and club observations

The general-competition migration `20261110225848_general_competition_context.sql` adds competition dimensions to historical identities and caches, source round/match identities to score observations, immutable `league_context_observations` and `player_membership_observations`, and scoped bulk indexes. Source metadata never grants account membership. New tables have server SELECT/INSERT only with duplicate-safe writes; historical facts remain unchanged under explicit unknown context. Fresh-schema retries preserve admission and navigation settings. See [the full context/transfer and rollout contract](general_competition_context.md).
