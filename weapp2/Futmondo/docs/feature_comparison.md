# LigaRadar additions to the official Futmondo workflows

## Listing deadlines and received-offer counts (2026-11-26)

Futmondo documents market end-time sorting and offer indicators. Its official client reads `expirationDate` from market listings. LigaRadar shows the localized deadline in My Team and Today’s market. The Essentials entry `squad_listings` also describes Home’s compact count of listed players and squad players with incoming offers. The current signed-in official Home layout remains unverified. [Official selling guide](https://help.futmondo.com/article/195-poner-jugadores-a-la-venta-en-el-mercado) · [Official market release notes](https://magazine.futmondo.com/nueva-version-de-la-app-en-futmondo/) · [Official client](https://app.futmondo.com/main.dart.js)

## Points and ranks (2026-11-26)

Futmondo’s guide shows how to view a past round’s ranking and the general classification. LigaRadar’s existing league charts also let managers switch between cumulative points and league rank over the same selected rounds. Choosing a starting round compares points earned from that round onward against a common zero baseline. The public app entry point and guide were reviewed on November 26. Availability of an equivalent chart control in the signed-in official app remains unverified. [Official guide](https://help.futmondo.com/article/90-empieza-a-jugar-a-futmondo) · [Official app](https://app.futmondo.com/)

## Round status and squad value changes on Home (2026-11-26)

Futmondo’s numbered match calendar recognizes `running` and `closed` rounds. Its guide explains that a match played later can still contribute to its original round. The official client also reads the team’s cash balance from `budget` and individual player price movements from `change`. The public client reviewed on November 26 had SHA-256 `08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c`. The current signed-in Home layout remains unverified. [Official client](https://app.futmondo.com/main.dart.js) · [Early and postponed matches](https://help.futmondo.com/article/228-procedimiento-en-partidos-aplazados) · [Player price guide](https://help.futmondo.com/article/95-como-fluctua-el-precio-de-los-jugadores)

LigaRadar Home shows the round’s playing or closed status and accounts for gaps between groups of fixtures in the same round. The Balance box shows the manager’s cash, squad market value and net daily change. Squad size shows how many players rose or fell in value. The existing Advanced welcome item `squad_value_trend` covers this calculation. [Squad value and daily trend](players_in_teams_module.md#team-value-and-daily-trend--2026-11-25)

Futmondo’s guides describe bidding against the league’s spending limit and a configurable delay before a clause signing joins the buyer’s squad. LigaRadar Home brings the existing active outgoing offer count and total into Balance. Squad size pairs daily value changes with pending arrivals and departures. This brings existing finance and transfer views onto Home. The public app entry point and guides were reviewed on November 26. The signed-in official layout remains unverified. [Official app](https://app.futmondo.com/) · [Bidding guide](https://help.futmondo.com/article/103-quiero-pujar-por-un-jugador-del-mercado-y-me-dice-que-no-tengo-saldo-suficiente) · [Clause transfer timing](https://help.futmondo.com/article/104-he-fichado-un-jugador-por-clausula-y-no-ha-pasado-a-mi-equipo)

## Welcome and Home inventory (2026-11-25)

The public welcome page leads with the decisions a manager can make before kickoff. Its Essentials and Advanced sections now include two existing workflows that had been missing from the inventory.

| Welcome item | Documented Futmondo workflow | What a LigaRadar user sees |
| --- | --- | --- |
| Essentials · `kickoff_balance` | Social leagues require positive cash at kickoff. The official app shows a countdown and the rules govern bids and borrowing. | The Balance summary brings open offers, cash after offers and remaining spending capacity together with the round deadline in Today's market, My Team and Players. [Game rules](https://help.futmondo.com/article/91-reglas-del) · [Borrowing guide](https://help.futmondo.com/article/103-quiero-pujar-por-un-jugador-del-mercado-y-me-dice-que-no-tengo-saldo-suficiente) · [Balance calculation](debt_and_round_countdown.md) |
| Advanced · `bulk_listing_preview` | The official selling guide describes choosing a squad player and entering an asking price. | In My Team, managers see how many players qualify for market-value or closed-clause pricing, add a premium with repeatable buttons, preview individual and total asking prices, and confirm the listings. The result identifies exclusions and any unconfirmed requests. The official selling guide was rechecked on November 26, 2026. The signed-in official bulk workflow remains unverified. [Selling guide](https://help.futmondo.com/article/195-poner-jugadores-a-la-venta-en-el-mercado) · [Batch pricing and confirmation](market_selling.md#bulk-listing-premium-and-preview--2026-11-10) |

Futmondo also documents navigation from Home, suggested lineups, a lineup average, market tables and historical player records. Its release notes describe an administrator setting that makes participants' balances visible. LigaRadar's historical-window lineup comparison and estimated, itemized rival cash history are the specific benefits behind those entries. The new Home gives direct access to LigaRadar's own pages. These comparisons use the official public app entry point and published guides reviewed on November 25. Current signed-in official screens and league-specific settings could not be verified from those sources. [Official app](https://app.futmondo.com/) · [Official app guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/) · [Home and balance settings](https://magazine.futmondo.com/nueva-version-de-la-app-en-futmondo/)

## Immediate balance updates after offers and clause purchases (2026-11-25)

Futmondo documents market bids, clause purchases and personal finance
movements. The public client reviewed on 2026-11-25 uses the authenticated
market and team-information queries with fantasy championship and team IDs.
It reads outgoing `bid` records from market rows and separately parses
`budget`, `withheld` and `maxBid`. The bundle includes native bid and
release-clause writes. The checked bundle SHA-256 is
`08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c`.
[Official rules](https://help.futmondo.com/article/91-reglas-del) ·
[Official release notes](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/) ·
[Official public client](https://app.futmondo.com/main.dart.js).

LigaRadar updates its existing Balance summary on native acknowledgement and
reconciles the displayed figures with its persisted observations. Captured
successful bid and clause responses supply `answer.code`. The change corrects
delivery of the existing offer and purchase workflow. The official signed-in
refresh timing and hosted LigaRadar rendering remain unverified.
[Implementation](scheduled_clauses.md#immediate-balance-summary-2026-11-25).

## League transfer activity over time (2026-11-23)

Futmondo documents personal finance movements, including transfers and prizes,
and player purchase histories with prices and dates. Its public client was
reviewed on 2026-11-23 and implements both the championship Pressroom and the
individual team's financial movements. No signed-in official UI was inspected,
so the current availability of an equivalent league histogram remains unverified.
[Official rules](https://help.futmondo.com/article/91-reglas-del) ·
[Official app release notes](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/) ·
[Official public client](https://app.futmondo.com/main.dart.js).

LigaRadar sums each saved league transfer once and displays a histogram by day,
Monday-start week or calendar month. The Advanced login entry describes this
league-wide aggregation. [Plots implementation](plots_module.md#money-movements--2026-11-23).

## Player points on Lineup match cards (2026-11-23)

The official club match cards show both clubs' player scores during and after a
match, offer scoring-mode selection, and show the lineup on a pitch. The
recorded official card showed the finished Levante–Elche 1–1 result and the
championship's custom points. The public client corroborates status, minute
and the custom scoring calculation. [Official getting-started guide](https://help.futmondo.com/article/90-empieza-a-jugar-a-futmondo) ·
[Official release notes](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/) ·
[Official public client](https://app.futmondo.com/main.dart.js).

LigaRadar opens these shared facts from the existing Lineup fixture cards. It
presents the two teams as compact lists, uses saved background observations
and exposes when they were checked. The local verification covers a captured
finished match and a synthetic live match. This is the official match-card
capability presented within LigaRadar's existing workflow.

## Player status indicators and My Team summary (2026-11-22)

The [official app's public client](https://app.futmondo.com/main.dart.js), inspected
2026-11-22 (SHA-256 `08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c`),
builds status images from the reported player's status and suppresses them for
blank and `ok` codes. The [official 5.0.5 release notes](https://magazine.futmondo.com/nueva-version-de-la-app-en-futmondo/)
describe injury, doubt and suspension categories in lineup selection. The
[official getting-started guide](https://help.futmondo.com/article/90-empieza-a-jugar-a-futmondo)
describes squad lists and opening player cards from them.

LigaRadar displays the shared status information next to table names and card
portraits. Its Essentials addition is a conditional My Team summary of affected
owned players below Leaving players, with links to their cards. The public sources
do not establish whether the signed-in official app uses this exact summary or
placement. Current authenticated official screens and hosted LigaRadar rendering
were outside this comparison.

## Fixture crests and trailing filter icons — reviewed 2026-11-17

Rechecked the [official app guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/)
(published 2021-12-23) and the
[current public client](https://app.futmondo.com/main.dart.js). The guide explicitly
places next-round matches at the bottom of the official lineup page. This
clarifies the existing comparison below: fixtures in that location are a shared
official workflow. The current client supplies `logo` in both home and away club
objects; the dated mapping and bundle digest are recorded in
[lineup_module.md](lineup_module.md#match-crests--2026-11-17).

LigaRadar now retains those observed logos in its durable calendar and displays
crests beside both opponents. Its shared player filters put decorative icons
after the visible labels, including Clear Filters. These refine existing
presentation and introduce no new analytical capability or login-tier item.
Neither crest presentation nor the match list is claimed as exclusive. The
public sources do not establish current official filter icon placement; no
current authenticated official UI or production API response was inspected.

## Next-round match calendar beside the lineup — reviewed 2026-11-16

Rechecked the [official rules](https://help.futmondo.com/article/91-reglas-del)
(last updated 2021-12-17), the [official app guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/)
(published 2021-12-23), and the [current public app bundle](https://app.futmondo.com/main.dart.js).
The rules connect lineup selection to real matches; the current client supplies
a numbered real-match calendar via `/2/league/matches`, including home/away clubs
and kickoff. The verified field mapping and bundle digest are in
[lineup_module.md](lineup_module.md#save-placement-and-next-round-fixtures--2026-11-16).

LigaRadar places those existing factual fixtures below the editable pitch, with
kickoffs in the account timezone, and moves its existing Save control above the
pitch. This is a contextual presentation of a shared official capability, not a
new exclusive analysis or automation feature, so no additional login-tier item
is added. No authenticated current official UI or production response comparison
was performed; the official app's exact placement is not asserted.

## Automatic factual sync review — 2026-11-16

Rechecked the [official rules](https://help.futmondo.com/article/91-reglas-del) and [official app feature guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/) for own financial movements, market/ownership data and team browsing. The guide was published in 2021; neither source establishes the current official app's internal data-loading architecture, and no authenticated UI comparison was performed. LigaRadar now connects its existing factual collector on login, reads persisted observations for all browsing, and updates changed values quietly. This improves delivery of existing features rather than adding a new analytical capability; there is no new login tier item or exclusivity claim. Login discloses continued sync after logout and the Disconnect control revokes factual access. [Implementation and limits](database_first_sync.md).

Reviewed 2026-11-14 against the official public material below and the current local implementation. This is the maintained comparison inventory behind the login page, not a roadmap or an access policy. Essentials contains exploration tools; Advanced contains higher-value lineup, player ranking and financial analysis. Both groups remain available under the existing app access rules.

## Evidence and limits

- [Official rules](https://help.futmondo.com/article/91-reglas-del) describe lineup editing, market actions, round/general standings, player point breakdowns, published Best XI, squad inspection and the user's own financial movements.
- [Official app feature guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/) explicitly includes **suggested lineups**, lineup average totals, market table views, player ownership/purchase histories, and top-five scoring rankings. Its publication is from 2021; retrieval today does not make it an exhaustive current app specification.
- [Official automatic coach](https://help.futmondo.com/article/159-entrenador-automatico) substitutes eligible bench players after a round, keeping the formation fixed. It does not document LigaRadar's configurable-window, cross-formation optimization objective.
- [Official postponed-round procedure](https://help.futmondo.com/article/228-procedimiento-en-partidos-aplazados) confirms that final automatic substitutions wait for all matches. This is distinct from explicitly calculating a next-round draft in LigaRadar.

The differences below are **specific implemented additions beyond the workflows described in those sources**. Absence from public documentation alone is not proof of absence from every version of the official app. No authenticated current official UI comparison was performed for this task. The owner reports that their league has no suggested-lineup option. Official documented availability is therefore kept separate from availability in a configured league: administrator settings, league mode or source entitlements may affect visibility, but this task has not verified which setting explains that league. Do not assume the official feature is enabled for every league, or describe the owner’s observed absence as proof of a particular administrator restriction. Entries must be narrowed or removed if an equivalent official implementation is verified. Generic lineups/optimizers, averages, standings, historical player cards, price histories, rival squads, notifications, market bids and clause purchases must not be advertised as exclusive. In particular, the existing official suggested-lineup feature prevents claiming that lineup suggestions in general are new.

### Matchday Dream team presentation — 2026-11-23

The official rules document opening the published Best XI from Classification. The public client bundle confirms that the lineup parser receives player roles, club logos, scores and the published MVP. LigaRadar’s existing matchday card now displays those fields in a descending points table and highlights the official MVP. This is a presentation update to the existing Best XI capability. The signed-in official view and any alternative ordering controls were outside the comparison. [Official rules](https://help.futmondo.com/article/91-reglas-del), [official public bundle](https://app.futmondo.com/main.dart.js), [implementation](classification_module.md#dream-team-display-2026-11-23).

### Player injury and card icons — 2026-11-23

The official public client’s `Dy.p` renderer uses the scalar status to select an image. Its public assets show one red card for `redcard`, a pair of yellow cards for `yellowcard` and a separate whistle for `suspended`. The bundle SHA-256 is `08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c`. The official scoring guide separately documents double yellows and direct red cards as match events. LigaRadar uses solid card shapes and a red medical cross in its existing status displays. Captured market and catalog responses corroborate the red-card status. The yellow-card rendering was verified with a synthetic status and the public asset. [Official bundle](https://app.futmondo.com/main.dart.js), [red asset](https://app.futmondo.com/assets/assets/images/player_status/redcard.png), [yellow asset](https://app.futmondo.com/assets/assets/images/player_status/yellowcard.png), [suspension asset](https://app.futmondo.com/assets/assets/images/player_status/suspended.png), [official scoring guide](https://help.futmondo.com/article/204-modo-prensa-futmondo-liga-ea-sports).

### Real Club filter — 2026-11-23

The official guides document market table views and browsing fantasy-owned players. The official public bundle’s `Hi` → `cHH` → `a8i` path confirms the authenticated `POST /1/league/championshipteams` request with a fantasy `championshipId` and real-club `id`, `name` and `logo` response fields. The inspected bundle SHA-256 is `08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c`. LigaRadar adds a searchable, alphabetical dropdown with club crests that combines with the existing fantasy-owner and scouting filters. Its Essentials **Combined scouting filters** item now mentions real clubs. An equivalent dropdown in the signed-in official app was outside the verification. [Official feature guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/), [official onboarding guide](https://help.futmondo.com/article/90-empieza-a-jugar-a-futmondo), [official bundle](https://app.futmondo.com/main.dart.js).

### Cash cap for clause browsing — 2026-11-23

Futmondo documents configurable borrowing in its maximum bid. LigaRadar’s existing clause browsing checkbox now compares each clause with the current Balance, while transaction verification follows the configured league rules. The equivalent signed-in official filter was outside the verification. [Official market configuration guide](https://help.futmondo.com/article/141-configura-tu-mercado-en-un-campeonato-social), [player table contract](players_table_module.md#maximum-clause-input--2026-11-22).

## Essentials

| Login ID / addition | Official baseline | Specific LigaRadar behavior | Local implementation evidence |
| --- | --- | --- | --- |
| `scouting_filters` — Combined scouting filters | Player/squad inspection and market table views. | Combine real club, fantasy owner, primary/secondary position, value ceiling, daily price-change threshold and active-clause filters in shared player tables. All Players excludes the current team by default and can include it with an unchecked-by-default checkbox, preserving the other filters. | `Modules/Players_Table_Module.R`; [player tables](players_table_module.md). |
| `squad_status` — Squad status at a glance | Official squad browsing, player cards and reported injuries, doubt and suspension. | My Team groups affected owned players in a conditional box below Leaving players, showing each reason and a direct link to the player card. | [My Team](players_in_teams_module.md#players-not-fit-2026-11-22). |
| `clause_transfer_timing` — Show incoming and leaving players before or after next round starts | The official clause guide, updated 2022-10-28, documents announced sales and league-configured delivery delays. The public web app exposes a sign-in shell. | My Team lists pending clause arrivals and departures with verified times and compares them with next kickoff, including transfers scheduled exactly at kickoff. An equivalent signed-in official view was outside the review. | [Clause guide](https://help.futmondo.com/article/156-que-son-las-clausulas-y-como-activarlas), [official web app](https://app.futmondo.com/), [My Team](players_in_teams_module.md#incoming-clause-signings). |
| `round_range_standings` — Standings for any round range | Single-round and cumulative standings, plus published Best XI. | Select an inclusive round interval for points and separate points/position/Best XI/MVP reward totals. Incomplete selected-round evidence remains unavailable. | `Modules/Classification_Module.R`; [classification](classification_module.md). |
| `league_charts` — League comparison charts | Standings and squad values are available. | Switch between cumulative points and ranks across a selected span of published rounds, and compare squad values. Count fantasy-owned players by real club. Unknown/unowned players are excluded from club counts. | `Modules/Plots_Module.R`; [plots](plots_module.md). |
| `local_timezone` — Read deadlines in your timezone | The public client displays dates in the device's locale. | Choose a personal timezone for kickoffs, deadlines and history across sessions. | [Timezone behavior](timezone_preferences.md) |

## Advanced

| Login ID / addition | Official baseline | Specific LigaRadar behavior | Local implementation evidence |
| --- | --- | --- | --- |
| `lineup_optimizer` — Best XI + bench optimization | Suggested lineup and aggregate lineup average already exist; the automatic coach performs post-round substitutions. | An explicit click maximizes observed average totals over a selected 1–20 completed-round window across permitted formations, then chooses substitutes using their primary positions for coverage; an optional full outfield coverage requirement can also constrain the XI and formation. Always excludes injured, suspended and unavailable players and verified departures by next kickoff; doubtful players are optional and excluded by default. It is a LigaRadar calculation independent of opening the official suggested-lineup control, which the owner does not see in their league. Permitted formation entitlements, multiposition, player eligibility and official save preflights still apply. A complete eligible history/check is required; the user reviews and saves the draft. | `Modules/Lineup_Module.R`, `lineup_runtime.R`; [lineup](lineup_module.md). |
| `round_award_rankings` — Best XI and MVP leaders | Published round Best XI and MVP, and historical editorial summaries of award counts. | Switch the leader charts and searchable ranking between players and their real clubs for the selected championship and season. The rankings report gaps in award coverage. | [Round awards](round_mvps_module.md#player-award-statistics--2026-11-25). |
| `recent_average` — Player rankings by last-N average | Player averages and a top-five total-points ranking already exist. | Choose 1–20 recent rounds and numerically sort market, squad and all-player tables by that window's average. Table windows and lineup completed-round windows have distinct documented finality contracts. Unknown scores remain unknown; source zero and negative scores count. | `Modules/Players_Table_Module.R`, `factual_helpers.R`; [player tables](players_table_module.md), [score reuse](round_points_matrix.md). |
| `price_efficiency` — Points per million | Official market/clause prices and player points. | Sort the same recent average divided by market value in Today or clause price in Players, expressed as points per €1M. Missing/nonpositive prices do not produce ratios. | `Modules/Players_Table_Module.R`; [player tables](players_table_module.md#points-to-price-ratios). |
| `live_substitutions` — Review live bench substitutions | The official Classification pitch supports manual bench changes before both clubs kick off. | Lineup and Classification share an eligibility preview, the first of the two deadlines, confirmed club-XI badges for La Liga, the Premier League and Serie A, and verification of both slots after confirmation. | [Comparison and sources](#live-bench-substitutions--2026-11-26) |
| `scheduled_lineups` — Prepare for confirmed signings | Manual lineup saves, suggested XIs and league-configured clause delivery. | Preview an XI and bench with confirmed arrivals due before kickoff. Where enabled, approve one submission timed for the latest selected signing. | [Dated comparison](#lineups-for-incoming-signings--reviewed-2026-11-23) · [Lineup contract](lineup_module.md#staged-lineups-for-incoming-signings--2026-11-23) |
| `squad_value_trend` — Squad value trends | Futmondo documents squad valuations and player price movements. | Home shows cash, squad value and the net daily price change. Rising and falling player counts appear below squad size. My Team and the selected rival’s Current team tab show the same trend beside squad value. | [Squad value and daily trend](players_in_teams_module.md#team-value-and-daily-trend--2026-11-25). |
| `rival_cash` — Reconstructed rival balances | Own financial movements, other teams' squad values and participant balances where the administrator enables visibility. [Official release notes](https://magazine.futmondo.com/nueva-version-de-la-app-en-futmondo/) | Reconcile an estimated rival cash balance and running history from opening funds, transfers, configured rewards and available historical eligibility. Estimates, gaps and discrepancies remain labelled. | `Modules/Rivals_Module.R`; [cash accounting](rival_cash_accounting.md), [rivals](rivals_module.md). |
| `transfer_histogram` — League transfer activity over time | Personal financial movements and dated player transfer history. | Sum recorded transfer amounts across the championship and compare daily, weekly or monthly periods. | [Plots](plots_module.md#money-movements--2026-11-23). |
| `trading_profit` — Trading profit and wealth comparisons | Player ownership/purchase history and own financial records. | Compare league completed-trade profit and estimated wealth gained; inspect matched purchase/sale cycles and related fees with missing costs kept unavailable. | `Modules/Rivals_Module.R`; [rivals](rivals_module.md#trading-profit-and-overall-wealth-gained--2026-11-06). |

## Squad/filter follow-up review — 2026-11-14 Pacific

Rechecked the [official rules](https://help.futmondo.com/article/91-reglas-del) and [official app guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/) for squad inspection, ownership and market tables. The [official clause guide](https://help.futmondo.com/article/156-que-son-las-clausulas-y-como-activarlas) documents configurable delayed arrivals (0–7 processing runs), explaining why bought players can differ from the current roster. These public guides were retrieved for this follow-up; the app guide dates from 2021 and the clause guide was last updated in 2022. A current authenticated official UI was not inspected.

The own-team inclusion toggle extends the existing `scouting_filters` item, whose login description is updated accordingly. The My Team effective badge exposes the already implemented purchase-capacity identity calculation next to the actual total, accounting for pending bought/leaving players without changing the actual positional breakdown. These sources do not establish an equivalent official checkbox or adjusted-count display, and lack of documentation does not establish their absence. Generic squad counts, ownership filtering and pending clause transfers are shared domain behavior; the badge is not presented as a new exclusive capability or a separate login tier item. Implementation: `Modules/Players_in_Championship_Module.R`, `Modules/Players_Table_Module.R`, `Modules/Players_in_Teams_Module.R`, `effective_squad_count()`; [contracts](players_in_teams_module.md).

## Rendering and maintenance contract

`login_feature_tiers()` in `Modules/Login_Module.R` returns two ordered lists with `id`, `title`, `icon`, `summary` and `features`. Each feature has a stable `id`, `title` and `description`. This is the runtime copy source. Keep the IDs and tier assignments above aligned with it whenever a feature is added or its differential changes.

`login_features_UI(id)` renders the two semantic, namespaced tiers in the public welcome. Each includes examples and a native disclosure for its feature details. Visitors can expand the lists by touch, mouse or keyboard. The tiers use side-by-side desktop columns and stack on narrow screens. Login and authentication feedback live outside the inventory. Each feature ID continues to identify its existing workflow across wording and presentation changes.

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


## Scheduled clause buyouts on Home (2026-11-27)

Futmondo documents paying a rival player’s clause from a player or squad card and configuring the delivery time after payment. LigaRadar’s existing Advanced scheduled-purchase feature also lists the selected team’s pending attempts on Home, including the reserved amount, opening time and status. This comparison used the public guide and app entry point. The authenticated official Home screen was not verified. [Official clause guide](https://help.futmondo.com/article/156-que-son-las-clausulas-y-como-activarlas) · [Official web app](https://app.futmondo.com/)

## Scheduled clause purchases — reviewed 2026-11-15

The [official clause guide](https://help.futmondo.com/article/156-que-son-las-clausulas-y-como-activarlas) documents manual payment from player cards, timed locks/renewals, rival limits and delayed player delivery. The [official borrowing guide](https://help.futmondo.com/article/103-quiero-pujar-por-un-jugador-del-mercado-y-me-dice-que-no-tengo-saldo-suficiente) ties permitted debt to league configuration. Both were retrieved on 2026-11-15; their source publication/update dates do not establish an exhaustive current app specification. No authenticated official-app scheduling comparison was performed.

Advanced login ID `scheduled_clauses` describes LigaRadar's implemented durable authorization for one capped purchase attempt at the published opening, local offer/funds reservation, cancellation, and saved next-login outcomes. This comparison does not claim ordinary clause purchases are exclusive or that official scheduling is absent. The new feature is disabled until the migration, deployment settings and individual consent are in place. The closed-rejection retry branch remains gated on a captured native code; its current absence is disclosed in the confirmation. See [the implementation contract](scheduled_clauses.md).


## Market-offer percentage shortcuts (2026-11-26)

Futmondo’s public guide describes selecting a market player and entering a private bid in the player dialog. The public app entry point and guide were checked on November 26. The current signed-in layout and its percentage controls remain unverified. [Official app](https://app.futmondo.com/) · [Getting-started guide](https://help.futmondo.com/article/90-empieza-a-jugar-a-futmondo)

LigaRadar’s manual market bid editor adds cumulative shortcuts, a live percentage beside the total and inline feedback for offers below the price or player valuation. These controls refine the existing manual offer workflow. [Bid editor behavior](bid_management.md#market-offer-percentage-buttons-2026-11-26)

## Consolidated information panels — reviewed 2026-11-15

Rechecked the [official app feature guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/)
(published 2021-12-23) on 2026-11-15. It describes page navigation, market
views, coach hiring and lineup warnings, but does not establish how the current
authenticated app groups source/freshness diagnostics. That UI was not inspected;
absence from this guide does not establish absence from the official app.

LigaRadar now groups its existing source status, timestamps, errors and passive
explanations behind one icon beside each page title and one per dialog. A failed
source makes the icon red while successful messages retain their normal color.
Actions and transaction failures stay immediately visible. This refines existing
presentation without adding a data, analysis or automation capability, so no
new login-tier item or exclusivity claim is added. Contract:
`ui_information_disclosures.md`.

## Player-card availability latency — reviewed 2026-11-15

The official [app feature guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/)
(published 2021-12-23, rechecked on 2026-11-15 for the related UI work) describes
player/market interaction and manual transactions. It does not establish current
authenticated availability latency or the official app's internal caching.
That UI and its timings were not measured; no comparative speed claim is made.

LigaRadar reuses its existing team verification across player cards and directly
retrieves a selected player's missing/expired summary during active browsing.
It preserves the existing transaction capability, freshness requirements and
manual confirmation/preflight behavior. This is a loading/persistence refinement,
not a new analytical or automated feature, so no login-tier item is added.
See `player_availability.md` and the measured limits in `data_review_results.md`.


## Player valuation-history repair — reviewed 2026-11-15 Pacific

Rechecked the [official app guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/)
(published 2021-12-23) on 2026-11-15. Existing local official-app response captures
provide more specific price evidence: `/1/player/summary` has seven dated
`price` rows; `/1/player/fullprofile` has 71 dated `social`/`classic` rows in the
same captured player flow. All seven overlapping summary prices equal that
profile's social values. These response shapes were replayed offline; they do
not establish current history length for every player or an exhaustive current
UI specification.

LigaRadar's existing combined points/valuation chart now consumes that longer
source curve after verifying its context and valuation type, with durable
historical reuse and a working profile refresh. This repairs the existing
history feature; ordinary player price history is an official shared capability,
so there is no new exclusive capability or login-tier entry. After the owner's
explicit approval, read-only replay of Dani Lorenzo's saved source response
verified 133 daily profile prices versus seven summary prices, all seven shared
prices matching the social series, and all 133 values/dates in the corrected
Plotly output. This resolves the initial payload-export verification blocker.
The deployed browser chart is still unverified. See the player-card contract
and dated review for evidence and limits.


## Shared balance/squad summary — reviewed 2026-11-16

Rechecked the official [game rules](https://help.futmondo.com/article/91-reglas-del)
(updated 2021-12-17) and [clause guide](https://help.futmondo.com/article/156-que-son-las-clausulas-y-como-activarlas)
(updated 2022-10-28) on 2026-11-16. They document own finances, squad inspection,
market bids and configurable delayed clause arrivals. LigaRadar now puts its
existing financial/identity ledger together at the top of My Team, Today's market
and Players, showing actual/effective ownership, configured limit and reserved
offer slots, with a full-capacity message. This extends presentation of existing
capabilities, without a new calculation/transaction capability or login-tier item.
The public sources do not establish whether the current official app has an
identical combined box; no authenticated official UI comparison was performed,
and no exclusivity claim is made. See [presentation contract](ui_information_disclosures.md).


## Multiple position filters — reviewed 2026-11-16

Rechecked the [official app feature guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/)
(published 2021-12-23) on 2026-11-16. It documents market table views, squad
inspection and multiposition players. It does not specify the current Position
control's multi-selection, All-exclusion or debounce behavior. The authenticated
official UI was not inspected; undocumented behavior is not claimed absent.

LigaRadar expands the existing Essentials `scouting_filters` entry: shared player
tables accept several primary/secondary positions together, start at All, keep
All exclusive, and apply choices after a 300 ms pause. The existing login entry
now says “multiple positions.” This extends the existing filter capability; it
adds no separate tier item or exclusivity claim. Contract: `players_table_module.md`.


## Startup database availability — reviewed 2026-11-17

Rechecked the [official public app entry point](https://app.futmondo.com/) and [official app guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/) (published 2021-12-23). These public sources do not establish the official app's internal database checks or outage/login behavior; no authenticated outage simulation was performed against Futmondo. LigaRadar now checks its own database before password/remembered login and shows a blocking error with a retry action when unavailable. This is reliability infrastructure for its existing database-first browsing, not a new analytical or football capability, so no login-tier item or exclusivity claim is added. See [implementation](login_module.md#startup-database-check--2026-11-17) and [verification limits](data_review_results.md#startup-database-admission-check--2026-11-17).


## Lineup bench constraint, picker priority and status icons — reviewed 2026-11-18

Rechecked the [official app](https://app.futmondo.com/) and its
[public client](https://app.futmondo.com/main.dart.js), SHA-256
`08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c`,
together with the [official app feature guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/),
[automatic-coach guide](https://help.futmondo.com/article/159-entrenador-automatico)
and [official 5.0.5 release notes](https://magazine.futmondo.com/nueva-version-de-la-app-en-futmondo/).
The guides document suggested lineups, player selection, ordered substitutes,
position-compatible coaching and injury/doubt/suspension information. Current
public client code renders observed player status images and parses home/away
club logos. These are shared capabilities, with no exclusivity claim.

LigaRadar adds an unchecked-by-default requirement that the exact historical
average optimizer leave three distinct substitutes covering DEF, MID and FWD.
It can change the XI/formation, excludes goalkeeper cover and retains the draft
with an explanation when infeasible. The existing Advanced `lineup_optimizer`
login entry and comparison row now name this implemented option. No separate
tier or access restriction is introduced.

The picker now presents bench players, unused squad and starters in that order,
retaining average ordering within groups. The pitch, bench and picker share
observed injury/doubt/suspension icons with accessible labels and tooltips.
These are refinements of existing lineup editing and factual player information,
not additional exclusive login capabilities. The requested real-match crests
already existed and were reverified through stored fixtures and browser checks.

The public sources do not establish an equivalent mandatory coverage optimizer
or the current signed-in picker ordering. No authenticated current official UI
or hosted LigaRadar acceptance was performed. Missing documentation is not proof
of official feature absence. See [lineup contract](lineup_module.md) and
[executed checks](data_review_results.md).


## Admin worker activity — 2026-11-18

Reviewed the [official Futmondo web app](https://app.futmondo.com/) and
[official app guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/)
(guide published 2021-12-23; reviewed 2026-11-18). These public sources do not
establish an equivalent for LigaRadar's own acquisition-process monitoring.
No authenticated official admin interface was inspected, so absence/exclusivity
is not claimed. LigaRadar adds an administrator-only operational view of its own
shared factual workers, with latest sampled request counts by route and current
account versus other accounts. This is internal operational infrastructure,
not a public football-analysis capability, and is excluded from the public
Essentials/Advanced login inventory. Sampling, authorization and limits:
[admin worker monitor](admin_worker_monitor.md).


## Doubtful-only lineup inclusion — 2026-11-18

The existing lineup optimizer checkbox now opts into doubtful players only;
injured, suspended and unavailable players cannot be admitted by that option.
The same rule governs starting XI, substitutes and required score completeness.
This refines the existing `lineup_optimizer` policy, without a new login-tier
capability or exclusivity claim. The current Advanced copy's injury/departure
checks remain accurate.

The official 5.0.5 release notes and public-client status rendering inspected
above already establish shared injury, doubt and suspension information. They
do not establish the current official suggested-lineup eligibility policy; no
new authenticated official comparison is claimed. This correction follows the
owner's requested selection rule and reuses the previously verified source codes
and database observations. See [lineup contract](lineup_module.md).

## Dates and times in your timezone (added to login inventory 2026-11-20)

Official app: shows times in device locale; no documented in-app timezone selector was found (verification limited to the public web client). LigaRadar addition: explicit timezone preference applied across kickoffs, deadlines and history.


## Explicit league refresh — reviewed 2026-11-22

The official Futmondo [getting-started guide](https://help.futmondo.com/article/90-empieza-a-jugar-a-futmondo) describes buying and selling through the market and reviewing completed signings in the pressroom. Its [market configuration guide](https://help.futmondo.com/article/141-configura-tu-mercado-en-un-campeonato-social) describes direct sale. The [clause guide](https://help.futmondo.com/article/156-que-son-las-clausulas-y-como-activarlas) documents immediate and delayed delivery. The current [official web app](https://app.futmondo.com/) was available as a public shell. Authenticated official UI refresh behavior was not verified.

The floating Refresh button and confirmed manual trades can request an immediate league-scoped check by LigaRadar's existing worker. The app updates cash, market and squad views from published snapshots and excludes verified incoming clause purchases from the active-clause list.

## Lineups for incoming signings — reviewed 2026-11-23

Futmondo documents manual pre-kickoff saves, suggested lineups and league-configured clause delivery. LigaRadar adds a preview for confirmed pre-kickoff arrivals, marks incoming players in grayscale, and optimizes the XI and bench together with all eligible signings. The pitch shows the selected signings in position. Save asks for confirmation of the latest selected arrival time before queuing one lineup. An all-owned selection uses the usual manual save. Its worker uses official squad ownership and the existing lineup save contract. The welcome page lists this workflow under Advanced. Preview is available by default and automatic saves require scheduling to be enabled. The comparison used public guides and the public client bundle, with execution tested through offline fixtures. A signed-in comparison of scheduling controls remains unverified. [Official lineup guide](https://help.futmondo.com/article/92-como-hago-mi-equipo-en-alineacion), [official app feature guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/), [clause arrival guide](https://help.futmondo.com/article/157-he-fichado-un-jugador-por-clausula-y-no-ha-pasado-a-mi-equipo), [implementation](lineup_module.md#staged-lineups-for-incoming-signings--2026-11-23).


## Plot presentation and round history — reviewed 2026-11-23

The current [official app](https://app.futmondo.com/) exposes a public application
shell. The [official getting-started guide](https://help.futmondo.com/article/90-empieza-a-jugar-a-futmondo)
(last updated 2021-12-17) documents current/past round classifications, overall
standings, squad inspection and money movements. These are shared underlying
capabilities. LigaRadar's existing league plots now show cumulative points once
per observed round/subround, shorter valuation dates, clearer financial controls,
and club crests/counts in ownership bars, with explanations on all five charts.
This refines the existing plot capability and does not add a separate login-tier
entry. The public guide and shell do not establish today's authenticated official
chart layout, subround axis formatting or equivalent ownership chart. No absence
or exclusivity claim is made. Local synthetic browser checks cover LigaRadar;
signed-in official comparison remains unverified. See [plot contracts](plots_module.md).

### Primary-position bench coverage · 2026-11-23

LigaRadar uses the owner-requested primary-position rule for bench selection and coverage indicators. Futmondo lists Buchanan as a primary midfielder with forward eligibility. Its public manual and automatic coach guides describe substitutions using either position for multiposition players. Signed-in substitution behavior for this league was not verified. [Position list](https://help.futmondo.com/article/220-jugadores-multiposicion-en-la-liga-santander), [manual coach](https://help.futmondo.com/article/158-entrenador-manual), [automatic coach](https://help.futmondo.com/article/159-entrenador-automatico).

### Editable lineups and expected XI points · 2026-11-23

Futmondo documents manual editing, an XI estimate based on player averages, and a five-point deduction for each empty position. LigaRadar applies the selected completed-round window and empty-slot penalties to unsaved edits as well as projected squads with incoming signings. The regular player picker includes verified pre-kickoff signings. An approved future lineup opens directly on the pitch, and editing can change its submission date. The comparison uses public documentation. Equivalence of rounding, captain treatment and scoring windows in a signed-in official app was not verified. [Official lineup features](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/), [official empty-position rule](https://help.futmondo.com/article/92-como-hago-mi-equipo-en-alineacion).

### Standings load time and historical participation — 2026-11-25

Futmondo's guide documents past/current round rankings and the general
classification. LigaRadar now reuses those official saved round rankings to
validate participation and restore missing scores in its existing cumulative
Standings and Rank progression plots. The first curve can render during the
remaining saved reads. This refines the existing comparison entry and adds no
login-tier capability. The public official shell and guide establish the
source workflows. Signed-in official chart layout and network latency were
outside this comparison. [Official app](https://app.futmondo.com/),
[Futmondo guide](https://help.futmondo.com/article/90-empieza-a-jugar-a-futmondo),
[implementation](plots_module.md#historical-participation-and-faster-first-curves--2026-11-25).

### Squad value trends — reviewed 2026-11-25

Futmondo's rules document squad valuations in Administration's Teams section.
Its price guide describes individual player value movements. LigaRadar shows team value, the net daily change and counts of
rising and falling players in My Team and in the selected rival's Current team
tab. Its Transaction & Financial History tab opens the reconstructed ledger
for that rival, with an optional per-player view. Futmondo's rules cover
personal financial movements, and its finance release notes list purchases,
sales and prizes. The public official app exposed its shell on
2026-11-25. Its signed-in rival trend and ledger layouts remain unverified. [Official web app](https://app.futmondo.com/),
[game rules](https://help.futmondo.com/article/91-reglas-del),
[player price guide](https://help.futmondo.com/article/95-como-fluctua-el-precio-de-los-jugadores),
[finance release notes](https://magazine.futmondo.com/version-1-6-como-van-mis-finanzas/).

## Best XI and MVP leaders — 2026-11-25

Futmondo documents selecting past rounds and viewing the Best XI. Its release
notes also describe the MVP for an individual round. The public client inspected
on November 25 still uses the round-specific dream team request and player/MVP
fields recorded in the classification contract. The bundle SHA-256 was
`08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c`.
[Getting started](https://help.futmondo.com/article/90-empieza-a-jugar-a-futmondo),
[official release notes](https://magazine.futmondo.com/version-1-6-como-van-mis-finanzas/),
[public client](https://app.futmondo.com/main.dart.js),
[recorded request contract](classification_module.md#dream-team-display-2026-11-23).

Futmondo has also published a season review with cross-round Best XI and MVP
counts by scoring mode. LigaRadar makes that comparison available directly in
the selected league with two charts, searchable player totals and round shares
computed from confirmed selections. Signed-in official navigation and equivalent
interactive season filters were outside this check.
[Official 2016–17 season review](https://magazine.futmondo.com/balance-futmondo-123-temporada-16-17/).

The official Dream Team player record supplies a playing role, club and logo.
LigaRadar displays the club crest and the positions a player held in confirmed
Best XIs alongside the cross-round chart and table totals. The signed-in
official season-ranking layout remains unverified.
[Public client and field mapping](classification_module.md#dream-team-display-2026-11-23),
[Round awards](round_mvps_module.md#club-crests-and-positions--2026-11-25).

### Club award totals · 2026-11-26

Futmondo’s guide and release notes place each round’s Best XI and MVP in Classification. The public client checked on November 26 matches the November 25 digest above and supplies the club with each Dream Team player. LigaRadar opens Round Awards on Statistics, where users can switch the charts and ranking between individual players and the clubs they represented each round. By round opens the individual selections. An equivalent interactive club ranking in the signed-in official app remains unverified. [Getting started](https://help.futmondo.com/article/90-empieza-a-jugar-a-futmondo), [official release notes](https://magazine.futmondo.com/version-1-6-como-van-mis-finanzas/), [public client](https://app.futmondo.com/main.dart.js).

## Split matchdays · 2026-11-26

Futmondo describes one Round 23 XI for the two December games and the eight games in February. A Social league’s postponed-lineup setting governs edits on reopening. Its notice assigns the December fixtures to Round 21 in Superliga and gives separate treatment to the Cup and divisions. LigaRadar follows the selected competition’s IDs and shows the early fixtures while the full XI remains available at the opening. It also keeps the partially played round in the existing standings, recent-history and award views. The existing lineup and standings inventory covers this behavior. The public client, published rules and saved LaLiga calendar were checked. The future signed-in editing windows have yet to occur. [Futmondo’s November 25 notice](https://magazine.futmondo.com/partidos-adelantados-de-la-jornada-23-de-laliga-ea-sports-funcionamiento-y-preguntas-frecuentes-en-futmondo/) · [LaLiga fixture announcement](https://www.laliga.com/es-US/noticias/horarios-de-los-partidos-adelantados-de-la-jornada-23-de-laliga-ea-sports)

For LaLiga Round 23, Futmondo schedules the first per-point installment for the morning of December 3. The remaining eight fixtures generate a second point installment, and final position and Best XI bonuses follow closure. LigaRadar's cash and standings views use the same recorded credits throughout the break. The notice does not specify a separate MVP payment date. LigaRadar checks the championship's configured MVP amount, finalized winner and historical recipient before attributing that component. [Futmondo's Round 23 announcement](https://magazine.futmondo.com/partidos-adelantados-de-la-jornada-23-de-laliga-ea-sports-funcionamiento-y-preguntas-frecuentes-en-futmondo/) · [Official bonus settings](https://help.futmondo.com/article/142-configura-las-primas-automaticas-en-un-campeonato-social)

## Live bench substitutions · 2026-11-26

Futmondo's manual coach guide describes starter-to-bench changes from Classification, compatible positions and the official kickoff cutoff. LigaRadar also offers the action from Lineup, identifies eligible reserves, shows the earlier kickoff in the user's timezone, and checks the resulting round pitch. The Advanced login inventory describes that review workflow. The public client and local read captures establish the request and slot contract. Authenticated production substitution acceptance remains unverified. [Futmondo manual coach](https://help.futmondo.com/article/158-entrenador-manual) · [Official client](https://app.futmondo.com/main.dart.js) · [LigaRadar implementation](lineup_module.md#manual-bench-substitutions-in-a-running-round--2026-11-26)

### Club team-sheet comparison · 2026-11-26

Futmondo documents match notifications when lineups are confirmed and manual substitutions before kickoff. After a user selects **Check club XI** for a fixture, LigaRadar places the corresponding club starter, substitute or omission directly on the fantasy starter’s tile in both owned round pitches. LigaRadar selects the StrikerAPI league using the Futmondo source competition ID for La Liga, the Premier League and Serie A and matches the players to the Futmondo roster. Each account gets an initial check and one late retry per fixture. Users can reuse public fixture lists, squads and team sheets with the same button. The Advanced login feature describes the combined review. The official signed-in notification flow and a production pre-kickoff provider response have not been observed during this comparison. [Futmondo release notes](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/) · [Official manual coach](https://help.futmondo.com/article/158-entrenador-manual) · [StrikerAPI competitions and lineups](https://strikerapi.com/docs/get-lineups)

## Player cards from awards (2026-11-27)

Futmondo documents the round Best XI and MVP in Classification and opening player windows by name in the market and squads. LigaRadar now opens its existing player card from the dream team, the by-round awards and the cross-round player rankings. The public app presents a sign-in shell. The exact award-to-card gesture in the signed-in official app remains unverified. This extends navigation within the existing award rankings inventory. [Official app](https://app.futmondo.com/) · [Getting started](https://help.futmondo.com/article/90-empieza-a-jugar-a-futmondo) · [Official release notes](https://magazine.futmondo.com/version-1-6-como-van-mis-finanzas/)

## Optimizing before the round opens (2026-11-27)

Futmondo’s lineup guide ties round start to the first match. Its round 23 announcement places two LaLiga fixtures on December 2 and the remaining eight in February. LigaRadar’s existing lineup optimizer can now draft the full squad during the gap between editable rounds, with clause timing based on the first published kickoff. Offline tests cover the handoff to the official editing window. The existing Advanced lineup entry covers this correction. [Official lineup guide](https://help.futmondo.com/article/92-como-hago-mi-equipo-en-alineacion) · [Round 23 announcement](https://magazine.futmondo.com/partidos-adelantados-de-la-jornada-23-de-laliga-ea-sports-funcionamiento-y-preguntas-frecuentes-en-futmondo/)
