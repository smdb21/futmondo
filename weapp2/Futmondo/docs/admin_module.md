# Administration

Users manage their own display timezone under **Welcome → Display timezone**, with an automatically detected default and Spain (`Europe/Madrid`) as the fallback when detection is unavailable. The former Admin user-timezone directory and editor have been removed. See [personal timezone contracts and verification](timezone_preferences.md).

`admin_UI(id)` and `admin_Server(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV)` expose explicit database diagnostics to the configured administrator. Every operation rechecks the authenticated account and active module. Reset confirmation captures the current account/league; changing context invalidates it.

`get_table_definitions()` derives the 15 retained factual tables from `supabase_known_tables()`. Counts load only after **Refresh table counts**; activation does not start a synchronous scan. Missing count headers and failed requests remain unknown. **Verify database connection** checks minimal identity columns; it does not create schemas. These explicit administrative probes are synchronous and can take up to the configured HTTP timeout per table.

**Save official round MVP results** preserves fetched/queued/acknowledged/failed counts. Queued work is never reported as saved. The ordinary MVP page uses the shared async loader. There is no full-database population or rotating player collection action.

**Reset retained database tables** requires the explicit confirmation modal and removes factual records in child-to-parent order. Take a restorable backup first. The redesign does not execute a reset. Archived retired feature tables are outside this operation.

See [database lifecycle helpers](database_reset_init.md) and [migration and rollback](database_migration.md). Offline coverage includes schema helpers and the full application lifecycle.


## Round awards — 2026-10-31

Admin now reads the shared full-XI snapshot coverage and exposes explicit missing/provisional/selected retrieval and failed-save retry. Numeric round ordering, selection/finality/MVP, last fetched and acknowledged save states are independent. Acquisition never starts on navigation. See [coach_and_round_awards.md](coach_and_round_awards.md) for the API, persistence, concurrency and finality contracts.


## League settings — 2026-10-31

Admin starts with a read-only **League settings** section for the currently selected league. Starting conditions, round rewards, market settings, squad/league limits, clauses/transfers and lineup settings use compact label/value rows in three desktop columns and one phone column. Explanations open on demand with native keyboard-accessible disclosure controls. Database and award controls remain below.

`admin_league_settings(league, info, championship_id)` is a pure presentation adapter returning group, setting, value and explanation columns. It combines the league configuration with the primary team configuration, with the latter taking precedence. It exposes an explicit allowlist, preserves zero/false, and marks missing values. Starting cash comes only from `configuration$budget`; top-level current `budget` and legacy user-confirmed defaults never substitute for it. Unknown ranking modes are not interpreted. Ranking direction/distribution uses the established contracts documented in `rival_cash_accounting.md`; this display does not calculate payments.

`admin_Server(..., league_settings_source = NULL)` accepts a reactive snapshot with `league`, `info` and `stale` fields. The application supplies its existing selected league and primary team resource state. Rendering is gated by active Admin navigation and the existing administrator check. No new requests, writes, caches, timers, or dependencies are added. The module can still initialize without this optional source.

Configuration changes must be made in Futmondo. The existing app refresh updates shared resource observations. The display describes current rules, not proof of historical rules or bonus payments. Focused coverage: `test/test_admin_league_settings.R`; shared wiring is covered by the offline lifecycle harness.

## Expanded configuration mapping — 2026-11-02

`admin_league_settings_UI(rows, stale=FALSE)` renders the pure adapter output, pairs category cards within responsive Bootstrap columns, and keeps explanations and unresolved codes collapsed. It adds no reads, writes, reactive sources, timers or dependencies. Authorization and the existing shared team/league resources are unchanged. Raw arbitrary fields are never exposed.

The mapping was checked against the captured public [Futmondo application script](https://app.futmondo.com/main.dart.js) and its [Spanish setting labels](https://app.futmondo.com/assets/assets/translations/es.json), without replaying authenticated requests. The script's `a9h.v()` serializer links response codes to model properties; configuration update handlers and UI labels establish the meanings and enum choices below. These are observed client contracts, not guessed expansions of abbreviations.

| Response field | Meaning / observed value interpretation |
| --- | --- |
| `configuration.budget` | Starting cash allocation; 300,000,000 in the supplied example. Top-level `budget` is current cash. |
| `rtv` | Deduct initial squad value from starting cash: false / true / `automatic`. Update key `removeTeamValue`. |
| `playerMoveInDays` | Clause delivery delay in daily transfer runs; zero means immediate. No four-day default. |
| `ctt` | Daily clause transfer time: false → 00:05; true → 12:05, as shown by Futmondo. Update key `clauseTransferTime`. This field alone does not establish a timezone. |
| `enablingClause` | Days after acquisition before a player's clause becomes available to rivals. |
| `playerRetention` | Minimum days after buying a player before selling. |
| `mnmp` | Fraction of squad value available as bidding credit; 0.5 → 50%. Update key `maxBid` takes the percentage; the information response's top-level `maxBid` is a monetary cap. |
| `dspct` | Direct-sale percentage; 1 → 100%. Actual transaction responses remain authoritative. |
| `mbmv` | Require bids to meet player value; explicit listing prices remain separate observed floors. |
| `mpm` | Maximum players offered for sale; 0 means unlimited. Update key `maxMarketPlayers`. |
| `vmb` | Number of visible offers; -1 means all. Update key `visibleBids`. |
| `avmb` | Whether users may see a player's number of bids. |
| `mtype` | Market type (`normal` or `auction`). |
| `marketTimes` | Markets per day. |
| `ccr` | Clause price range: 1 cheap, 2 intermediate, 3 expensive. Update key `clausePriceRange`. |
| `mcpw` | Maximum clauses paid against one team per week; the client displays values below 1 as unlimited. |
| `dbp` | Prohibit player-clause blocking. Update key `disallowBlockPlayers`. |
| `mbp` | Maximum blocked players, including protection after signing; 0 means unlimited. Update key `maxBlockedPlayers`. The prohibition above still applies independently. |
| `ddt` | Prohibit clause renewal. Update key `disallowDT`; Spanish label confirms renewal, not the coach feature. |
| `rcp` | Recoverable fraction of investment in clause increases. The client displays non-positive settings, including -1, as no recovery. |
| `loans`, `acl` | Enable loans; allow loan cancellation (`allowCancelLoan`). |
| `cpt` | Captain enabled. |
| `apm` | Alternative player positions enabled (`allowPlayerMultiposition`). |
| `aes` | Extra formations enabled (`allowExtraStrategies`). |
| `blc` | Lock lineups for postponed matches (`blockLineupDelayedMatches`). |
| `enableCoach` | Reported coach mode; `automatic` is displayed as Automatic. This is separate from this app's login-time coach-hiring preference. |

Existing reward, roster-size, team-count and season fields remain displayed. `extraPay` retains its reported number; this evidence does not establish its timing or accounting units. `ac`, `avub`, `mdbp`, `nsnr` and `mtoffset` remain under **Other reported settings**, with exact codes and values rather than invented meanings. Together, the adapter accounts for all 48 configuration keys in the supplied example, plus the top-level initial-empty-squad flag. In particular, `mtoffset=-60` is not treated as a verified timezone or delivery offset, and `remainingNumberOfRounds=-11` is retained rather than clamped or turned into a fabricated schedule.

[Official clause help](https://help.futmondo.com/article/156-que-son-las-clausulas-y-como-activarlas) establishes daily processing and cutoff behavior. An exact arrival must use the player's `championship.clause.transferDate`, not purchase time plus `playerMoveInDays * 86400`. Incoming-player reconciliation already follows that timestamp. Current settings are not proof of historical rules or paid rewards.

Top-level `withheld` is reserved funds, `teamValue` is squad valuation, `points`/`position` describe standings, and `maxBid` is the API monetary bidding limit. League identity, permissions, sponsorship, subscription and staff metadata are not additional configurable financial rules and do not become budget assumptions.

## Application access

The first Admin section contains the database-backed, default-enabled Block non-admin access switch. Normal and remembered logins enforce it on the server; active non-admin sessions recheck it every 2 minutes. See [app_access.md](app_access.md) for failure handling, privileges and request bounds.


## Boxed settings and independent loading — 2026-11-15

`admin_UI()` groups App access, League settings, Database
administration and Round awards into bordered sections.
`admin_settings_styles()` supplies singleton, module-scoped styles for these
sections and the standalone `admin_league_settings_UI()` renderer. Each league
category, including Other reported settings, has its own box; disclosure rows
use consistent label/value grid columns, with tabular numerals and wrapping.
The existing three-column desktop / single-column phone layout and native
keyboard disclosure controls remain.

The timezone directory previously returned its pending promise through the
activation observer, making Shiny wait for that request before flushing the
whole page. `fm_admin_timezones()` now starts the same asynchronous load and
returns immediately. Its existing loading/failure state, authorization checks,
generation guards, 200-user bound, persisted preferences and save confirmation
remain authoritative. This changes no storage or acquisition contract and adds
no cache, request or timer. Browser navigation and heartbeat checks deliberately
hold directory reads for five seconds. See `test/test_admin_settings_browser_app.R`
and `test/test_admin_settings_browser.cjs`; run the R fixture, then the Node
check with the project's existing Playwright browser configuration.


## Settings freshness and authority — 2026-11-15

The read-only explanation now states: “Only league's admin (might be different
than LigaRadar admin) can make changes in Futmondo official app.”

`admin_league_settings_UI(rows, stale=FALSE, status=NULL, error=NULL,
observed_at=NULL, retry=NULL)` delegates its source note to
`admin_league_settings_status()` using the existing information disclosure.
The shared pending-information convention suppresses a failure note during
loading. A genuinely expired observation is labeled out of date, with its
original last-checked time; a failed read retains the safe error diagnostic.
Fresh observations display their original check time. Zero/false values and
known values during a refresh remain intact.

The root `league_settings_source` passes `observed_at`, `freshness` and `source`
from the same shared team state, alongside the existing data/status/error.
`admin_Server(..., retry_league_settings=NULL)` accepts an optional callback
for the **Reload settings** control. The application increments only the
existing primary team retry revision. Inactive, unauthorized and pending
requests cannot invoke it. In Background sync mode this rereads database
snapshots, preserving the independent worker schedule; interactive mode uses
the existing factual read and persistence path. Reload cannot promise a newer
source result when the collector/source is delayed.

`factual_read_adapter()` now respects an explicit persisted worker observation's
source deadline instead of expiring its display at the shorter memory-cache
TTL. Invalid/elapsed source deadlines remain stale; legacy snapshots without a
deadline and interactive reads retain the ordinary TTL. No source deadline is
extended, source time renewed, failed read hidden, or manual preflight relaxed.


## Personal timezone ownership — 2026-11-15

The user clarified that timezone choices belong to each user. Admin no longer
renders the User timezones section or instantiates `fm_admin_timezones()`. This
also removes its directory request on navigation. The earlier directory-loading
notes above describe the superseded implementation. Both ordinary users and
LigaRadar administrators use Welcome → Display timezone for their own account;
`fm_timezone_store()` rejects every cross-account read or write.


## Blank section during Background sync renewal — 2026-11-15

A pending credential-renewal promise returned through the worker session's
observer could hold the entire Shiny flush. Opening Admin during that wait
left League settings blank even though the selected league and configuration
were available. `fm_worker_session()` now returns immediately from its local
`authorize()` helper after installing asynchronous acknowledgement/error
callbacks. Both renewal and explicitly saved Background sync settings use
this helper; their database acknowledgement, consent, encryption, generation
guards and polling contract remain authoritative. No configuration request,
cache, API retry or source-age relaxation is added.

The full-root regression holds renewal for five seconds and opens the real
Admin UI with existing synthetic configuration. It verifies visible boxed
values and interactive input before renewal completes, then one acknowledged
renewal. An explicit disable during renewal also remains authoritative over
an older reply. The earlier standalone Admin fixture could not cover this
opted-in session path. See `progressive_loading.md` and `data_review_results.md`.


## Worker activity — 2026-11-18

The admin-only Worker activity section separates shared live process totals from
latest reported requests, grouped by route with My account / Other accounts
columns. Positive fresh counts are green; zeros and stale counts are neutral.
Opening this section only reads durable operational metadata. It does not start
acquisition or create workers. See [authorization, sampling, database contract,
and performance bounds](admin_worker_monitor.md).
