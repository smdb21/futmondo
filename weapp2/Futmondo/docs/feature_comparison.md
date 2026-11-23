# LigaRadar additions to the official Futmondo workflows

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
| `round_range_standings` — Standings for any round range | Single-round and cumulative standings, plus published Best XI. | Select an inclusive round interval for points and separate points/position/Best XI/MVP reward totals. Incomplete selected-round evidence remains unavailable. | `Modules/Classification_Module.R`; [classification](classification_module.md). |
| `league_charts` — League comparison charts | Standings and squad values are available. | Combine league squad-value and rank-progression comparisons with a count of fantasy-owned players by real club. Unknown/unowned players are excluded from club counts. | `Modules/Plots_Module.R`; [plots](plots_module.md). |

## Advanced

| Login ID / addition | Official baseline | Specific LigaRadar behavior | Local implementation evidence |
| --- | --- | --- | --- |
| `lineup_optimizer` — Best XI + bench optimization | Suggested lineup and aggregate lineup average already exist; the automatic coach performs post-round substitutions. | An explicit click maximizes observed average totals over a selected 1–20 completed-round window across permitted formations, then chooses coverage-aware substitutes; an optional full outfield coverage requirement can also constrain the XI and formation. Always excludes injured, suspended and unavailable players and verified departures by next kickoff; doubtful players are optional and excluded by default. It is a LigaRadar calculation independent of opening the official suggested-lineup control, which the owner does not see in their league. Permitted formation entitlements, multiposition, player eligibility and official save preflights still apply. A complete eligible history/check is required; the user reviews and saves the draft. | `Modules/Lineup_Module.R`, `lineup_runtime.R`; [lineup](lineup_module.md). |
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


## Market-offer percentage shortcuts — reviewed 2026-11-15

Rechecked the official [getting-started guide](https://help.futmondo.com/article/90-empieza-a-jugar-a-futmondo)
and [game rules](https://help.futmondo.com/article/91-reglas-del) on 2026-11-15.
They document entering an offer in a player dialog and budget-limited secret
market bidding; the rules were last updated on 2021-12-17. These public guides
do not establish whether the current authenticated official app has percentage
shortcuts; that UI was not inspected.

LigaRadar extends its existing manual market-offer amount editor with the same
six percentage shortcuts as its sale-listing dialog, based on the verified
minimum bid and followed by explicit submission. This is a convenience within
an existing shared market capability, without a new analysis/automation feature
or exclusivity claim; no new login-tier item is added. Contract: `bid_management.md`.

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

LigaRadar's Essentials inventory includes **League refresh**. The floating button and confirmed manual trades can request an immediate league-scoped check by LigaRadar's existing worker. The app updates cash, market and squad views from published snapshots and excludes verified incoming clause purchases from the active-clause list.

## Lineups for incoming signings — reviewed 2026-11-23

Futmondo documents manual pre-kickoff saves, suggested lineups and league-configured clause delivery. LigaRadar adds a preview for confirmed pre-kickoff arrivals, marks incoming players in grayscale, and queues one approved lineup per arrival plus an approved immediate lineup. Its worker uses official squad ownership and the existing lineup save contract. The welcome page lists this workflow under Advanced and notes that automatic saves require scheduling to be enabled. The comparison used public guides and the public client bundle, with execution tested through offline fixtures. A signed-in comparison of scheduling controls remains unverified. [Official lineup guide](https://help.futmondo.com/article/92-como-hago-mi-equipo-en-alineacion), [official app feature guide](https://magazine.futmondo.com/novedades-en-la-nueva-app-de-futmondo/), [clause arrival guide](https://help.futmondo.com/article/157-he-fichado-un-jugador-por-clausula-y-no-ha-pasado-a-mi-equipo), [implementation](lineup_module.md#staged-lineups-for-incoming-signings--2026-11-23).
