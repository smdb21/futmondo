# LigaRadar additions to the official Futmondo workflows

Reviewed 2026-11-14 against the official public material below and the current local implementation. This is the maintained comparison inventory behind the login page, not a roadmap or an access policy. Essentials contains exploration tools; Advanced contains higher-value lineup, player ranking and financial analysis. Both groups remain available under the existing app access rules.

## Evidence and limits

- [Official rules](https://help.futmondo.com/article/91-reglas-del) describe lineup editing, market actions, round/general standings, player point breakdowns, published Best XI, squad inspection and the user's own financial movements.
- [Official app feature guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/) explicitly includes **suggested lineups**, lineup average totals, market table views, player ownership/purchase histories, and top-five scoring rankings. Its publication is from 2021; retrieval today does not make it an exhaustive current app specification.
- [Official automatic coach](https://help.futmondo.com/article/159-entrenador-automatico) substitutes eligible bench players after a round, keeping the formation fixed. It does not document LigaRadar's configurable-window, cross-formation optimization objective.
- [Official postponed-round procedure](https://help.futmondo.com/article/228-procedimiento-en-partidos-aplazados) confirms that final automatic substitutions wait for all matches. This is distinct from explicitly calculating a next-round draft in LigaRadar.

The differences below are **specific implemented additions beyond the workflows described in those sources**. Absence from public documentation alone is not proof of absence from every version of the official app. No authenticated current official UI comparison was performed for this task. The owner reports that their league has no suggested-lineup option. Official documented availability is therefore kept separate from availability in a configured league: administrator settings, league mode or source entitlements may affect visibility, but this task has not verified which setting explains that league. Do not assume the official feature is enabled for every league, or describe the owner’s observed absence as proof of a particular administrator restriction. Entries must be narrowed or removed if an equivalent official implementation is verified. Generic lineups/optimizers, averages, standings, historical player cards, price histories, rival squads, notifications, market bids and clause purchases must not be advertised as exclusive. In particular, the existing official suggested-lineup feature prevents claiming that lineup suggestions in general are new.

## Essentials

| Login ID / addition | Official baseline | Specific LigaRadar behavior | Local implementation evidence |
| --- | --- | --- | --- |
| `scouting_filters` — Combined scouting filters | Player/squad inspection and market table views. | Combine owner, primary/secondary position, value ceiling, daily price-change threshold and active-clause filters in shared player tables. All Players excludes the current team by default and can include it with an unchecked-by-default checkbox, preserving the other filters. | `Modules/Players_Table_Module.R`; [player tables](players_table_module.md). |
| `round_range_standings` — Standings for any round range | Single-round and cumulative standings, plus published Best XI. | Select an inclusive round interval for points and separate points/position/Best XI/MVP reward totals. Incomplete selected-round evidence remains unavailable. | `Modules/Classification_Module.R`; [classification](classification_module.md). |
| `league_charts` — League comparison charts | Standings and squad values are available. | Combine league squad-value and rank-progression comparisons with a count of fantasy-owned players by real club. Unknown/unowned players are excluded from club counts. | `Modules/Plots_Module.R`; [plots](plots_module.md). |

## Advanced

| Login ID / addition | Official baseline | Specific LigaRadar behavior | Local implementation evidence |
| --- | --- | --- | --- |
| `lineup_optimizer` — Best XI + bench optimization | Suggested lineup and aggregate lineup average already exist; the automatic coach performs post-round substitutions. | An explicit click maximizes observed average totals over a selected 1–20 completed-round window across permitted formations, then chooses coverage-aware substitutes. Applies injury policy and verified departures by next kickoff. It is a LigaRadar calculation independent of opening the official suggested-lineup control, which the owner does not see in their league. Permitted formation entitlements, multiposition, player eligibility and official save preflights still apply. A complete eligible history/check is required; the user reviews and saves the draft. | `Modules/Lineup_Module.R`, `lineup_runtime.R`; [lineup](lineup_module.md). |
| `recent_average` — Player rankings by last-N average | Player averages and a top-five total-points ranking already exist. | Choose 1–20 recent rounds and numerically sort market, squad and all-player tables by that window's average. Table windows and lineup completed-round windows have distinct documented finality contracts. Unknown scores remain unknown; source zero and negative scores count. | `Modules/Players_Table_Module.R`, `factual_helpers.R`; [player tables](players_table_module.md), [score reuse](round_points_matrix.md). |
| `price_efficiency` — Points per million | Official market/clause prices and player points. | Sort the same recent average divided by market value in Today or clause price in Players, expressed as points per €1M. Missing/nonpositive prices do not produce ratios. | `Modules/Players_Table_Module.R`; [player tables](players_table_module.md#points-to-price-ratios). |
| `rival_cash` — Reconstructed rival balances | Own financial movements and other teams' squad values. | Reconcile an estimated rival cash balance and running history from opening funds, transfers, configured rewards and available historical eligibility. Estimates, gaps and discrepancies remain labelled. | `Modules/Rivals_Module.R`; [cash accounting](rival_cash_accounting.md), [rivals](rivals_module.md). |
| `trading_profit` — Trading profit and wealth comparisons | Player ownership/purchase history and own financial records. | Compare league completed-trade profit and estimated wealth gained; inspect matched purchase/sale cycles and related fees with missing costs kept unavailable. | `Modules/Rivals_Module.R`; [rivals](rivals_module.md#trading-profit-and-overall-wealth-gained--2026-11-06). |

## Squad/filter follow-up review — 2026-11-14 Pacific

Rechecked the [official rules](https://help.futmondo.com/article/91-reglas-del) and [official app guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/) for squad inspection, ownership and market tables. The [official clause guide](https://help.futmondo.com/article/156-que-son-las-clausulas-y-como-activarlas) documents configurable delayed arrivals (0–7 processing runs), explaining why bought players can differ from the current roster. These public guides were retrieved for this follow-up; the app guide dates from 2021 and the clause guide was last updated in 2022. A current authenticated official UI was not inspected.

The own-team inclusion toggle extends the existing `scouting_filters` item, whose login description is updated accordingly. The My Team effective badge exposes the already implemented purchase-capacity identity calculation next to the actual total, accounting for pending bought/leaving players without changing the actual positional breakdown. These sources do not establish an equivalent official checkbox or adjusted-count display, and lack of documentation does not establish their absence. Generic squad counts, ownership filtering and pending clause transfers are shared domain behavior; the badge is not presented as a new exclusive capability or a separate login tier item. Implementation: `Modules/Players_in_Championship_Module.R`, `Modules/Players_Table_Module.R`, `Modules/Players_in_Teams_Module.R`, `effective_squad_count()`; [contracts](players_in_teams_module.md).

## Rendering and maintenance contract

`login_feature_tiers()` in `Modules/Login_Module.R` returns two ordered lists with `id`, `title`, `summary` and `features`; each feature has a stable `id`, `title` and `description`. This is the runtime copy source. Keep the IDs and tier assignments above aligned with it whenever a feature is added or its differential changes.

`login_features_UI(id)` renders semantic, namespaced sections and ordinary lists with official reference links. Each item uses a native `details`/`summary` disclosure, initially closed: clicking/tapping its title or using Enter/Space reveals the explanation. A decorative chevron turns when open, and keyboard focus is visible. No JavaScript handler or reactive output is involved in disclosure changes. `login_UI(id)` mounts it outside the reactive authentication-status output, so initial load, rejected authentication, restoration, successful sign-in and logout cannot replace the inventory. CSS uses two equal columns at viewport widths of at least 992px and one below that, with shrinkable columns and wrapping text. The inventory does not enforce entitlements or change app access.

This is static shipped copy: rendering performs no API/database request, acquisition, persistence, invalidation, timer or optimizer calculation. Existing factual history and lineup features continue to reuse acknowledged database observations with account/league/season/scoring context and original source clocks; this task creates no result cache or persistence path. No dependencies, environment variables or schema changes are introduced.

For each new or expanded feature, recheck current official sources/UI, record the dated equivalent and precise addition here, and add a login item only for an implemented new capability. Record uncertainty explicitly, revisit contradictory evidence, and retain the two-tier presentation without commercial or access-policy copy. See `Agents.md` / `AGENTS.md` for the standing rule.

## Player-table scrolling refinement — 2026-11-14

Rechecked the [official app feature guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/)
(published 2021-12-23) on 2026-11-14. It documents market table views; it does not
specify their pinned-column behavior. LigaRadar now pins a separate club crest
and Avg Last N while Name and Position scroll, with a seamless crest/name boundary,
in every shared player table. This is a presentation refinement to the existing
scouting/market/squad views, without a new analytical capability or login tier
entry. No exclusivity claim is made. Current authenticated official-app behavior
and hosted LigaRadar appearance were not verified.

The 2026-11-15 scrolling refinement replaces the pinned Position column with an
84px Avg Last N column (previously 110px). This changes the existing presentation,
not the recent-average calculation or capability inventory; the official-source
comparison above and its verification limits still apply. No login-tier entry is
added.


## Scheduled clause purchases — reviewed 2026-11-15

The [official clause guide](https://help.futmondo.com/article/156-que-son-las-clausulas-y-como-activarlas) documents manual payment from player cards, timed locks/renewals, rival limits and delayed player delivery. The [official borrowing guide](https://help.futmondo.com/article/103-quiero-pujar-por-un-jugador-del-mercado-y-me-dice-que-no-tengo-saldo-suficiente) ties permitted debt to league configuration. Both were retrieved on 2026-11-15; their source publication/update dates do not establish an exhaustive current app specification. No authenticated official-app scheduling comparison was performed.

Advanced login ID `scheduled_clauses` describes LigaRadar's implemented durable authorization for one capped purchase attempt at the published opening, local offer/funds reservation, cancellation, and saved next-login outcomes. This comparison does not claim ordinary clause purchases are exclusive or that official scheduling is absent. The new feature is disabled until the migration, deployment settings and individual consent are in place. The closed-rejection retry branch remains gated on a captured native code; its current absence is disclosed in the confirmation. See [the implementation contract](scheduled_clauses.md).
