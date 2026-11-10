# Competition, season and player-transfer context

## Source identity and access

A Futmondo championship is a fantasy game; its `league` identifies a real competition. `activechampionships.leagues[]._id` and `.season` supply the authoritative real competition and opaque source season string. They are matched by ID, never by names or dates. An authenticated active-championship response establishes membership. Database metadata cannot grant membership or make a championship absent from that response accessible.

`competition_context.R` provides bounded account-scoped metadata (128 entries / 8 MiB, with a 1 MiB per-entry admission bound), normalization, durable context hydration and observed club membership. `resource_context()` appends the named `competition_id` argument without changing positional arguments. Omitted season/scoring/competition values use valid metadata for that account/championship; explicitly supplied `unknown` remains unknown for archival queries. `league_context_revision()` invalidates consumers when semantic context changes or expired metadata becomes usable again.

`get_resolved_league_context(login, championship_id, user_team_id)` checks authenticated membership, resolves competition/season, and reuses valid saved scoring metadata before another scoring acquisition. Historical table loading resolves context before the scoped database matrix. Known authenticated or valid persisted scoring skips an additional lineup read; unresolved scoring acquires one lineup before the matrix. Primary market/cash loading remains independent. Source metadata failures settle as unavailable; they must not start an unknown-context archive read. Finance and awards wait for the root context source. Metadata is rechecked on normal demand/navigation or explicit refresh, without polling workers.

Context freshness is 300 seconds. Scoring evidence separately retains its original observation and expiry in the payload. A compact metadata response may retain still-valid scoring configuration, but cannot renew its deadline. The database hydration query excludes unknown/expired scoring before choosing a snapshot. Payloads contain normalized names, source IDs and configuration only, never login headers, tokens or passwords. Failed saves remain retryable and are not treated as durable success.

## Scoring and historical queries

`fm_scoring_version()` canonicalizes JSON object keys recursively, preserves arrays, NULLs and configuration values, and returns a plain SHA-256 string. Awards, player scores and lineup snapshots use this same function. Missing point systems or missing custom configuration remain unknown. Equivalent object order is not a scoring change.

Private resource state includes account, championship, team, player and relevant competition/season/scoring dimensions. Shared adapters capture the complete owner context and create a different entry when those dimensions change; late callbacks cannot publish into the new entry. Manual player confirmations and retained lineup drafts follow the same rule.

Score/check reads apply exact championship, competition, season and scoring filters before pagination and limits. Card price/team histories additionally filter their competition and season. The league matrix is still shared across visible tables and N values; N is not a source-query parameter. Existing two-request history acquisition, bounded queues and cache limits remain in force.

Score observations preserve source `round_id`, `match_id`, `source_round_number` and `source_round_key`. Distinct IDs with a repeated label demand the official schedule only on that exceptional path; a unique verified mapping supplies its official number while the original label remains stored. An absent or ambiguous source mapping remains unresolved without discarding usable player identity/manual-action data. An official round ID produces `id:<source-id>`; otherwise the numeric observation retains `number:<number>` as a scoped fallback identifier, not an invented official ID. Legacy rows retain an empty key. Distinct official round IDs remain separate in storage; ambiguous numeric presentation is rejected explicitly rather than selecting an arbitrary score. Reconciliation retains revisions, latest NULL scores, zero and negative values.

Player-only profile/match requests keep their verified existing wire shape (`playerId` only). Returned player identity is checked, and nested `answer.league._id`/`.season` are retained. Fixtures from another competition or season are not joined to selected fantasy points by matching round numbers. Unassociated factual fixtures can remain visible with their association identified as unavailable.

## Real-world club changes

Source player IDs are opaque source records, not permanent people. `player_membership_observations` records player, competition, season, nullable real club, observation time, nullable effective time and source. Membership is many-to-many: domestic and European participation can coexist.

An unchanged ID changing clubs appends another observation. A changed ID creates a separate record. Returning to a former club appends another observation. None of these operations rewrites older history, moves cash/ownership, or infers aliases from names, photos, slugs or club similarity. Catalog disappearance does not establish a completed transfer. First observation time is not an actual transfer date.

The mutable `players.real_club_id` remains current catalog metadata; it cannot establish a historical event's club. Historical source club names and fixture identities remain attached to their original events. When event-specific club evidence is missing it remains unknown; a current-club observation must not be stamped onto older scores fetched later.

The captured historical-season endpoint's row IDs have not been proven to be alternate player IDs. This version adds no alias acquisition, automatic person merge or combined career view. Midseason player-ID continuity and real Premier League/Champions League player payload compatibility remain unverified; synthetic fixtures cover both ID outcomes and common response contracts.

## Database rollout and compatibility

New installations use `scripts/schema.sql`. Existing installations require `scripts/migrations/20261110225848_general_competition_context.sql` before deploying the new readers/writers. It adds immutable server-only context/membership tables, contextual columns/keys, retained official round IDs, scoped views/indexes and incoming-write ownership guards. Empty transaction counterparties remain the Futmondo System/Market. Team source IDs cannot be reassigned to another championship.

Context/membership tables grant SELECT/INSERT only to the server role; writes ignore exact duplicate observations. Browser roles have no access. Diagnostic table inventories include them; the destructive factual reset intentionally excludes these immutable observations.

Existing unknown seasons, competition identities and timestamps are preserved. Current metadata and ingestion dates do not prove the historical context of a past row. No migration infers a season, club, transfer date or player alias. New verified contexts reacquire demanded source history through the normal active-session flow and persist acknowledged results. Existing award snapshots can recompute canonical scoring from their retained raw configuration; metadata-free legacy hashes do not establish equivalence to current settings. Incompatible derived-cache payloads are versioned out rather than relabeled.

The migration is transactional and repeatable. Rehearse with the isolated fixture, compare original records, and test replacing final COMMIT with ROLLBACK. Application rollback restores the previous compatible release while leaving additive data intact; preserve a database backup before an authorized production rollout. No production migration, deployment or admission-policy change is part of the local implementation.

## Focused verification

`test_competition_context.R`, `test_competition_adapter.R`, `test_competition_history.R`, `test_competition_snapshots.R` and `test_competition_schema.cjs` exercise source resolution, account isolation, transfer identities, configuration ordering, strict hydration, late callbacks, expiry, idempotence, unchanged legacy facts, rollback and server-only access. Browser checks use `test_competition_browser_app.R` and `test_competition_browser.cjs`, blocked external HTTP and synthetic delayed responses. Executed counts, measurements and limits belong in `data_review_results.md`.
