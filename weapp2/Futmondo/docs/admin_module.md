# Administration

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

The first Admin section contains the database-backed, default-enabled Block non-admin access switch. Normal and remembered logins enforce it on the server; active non-admin sessions recheck it every 15 seconds. See [app_access.md](app_access.md) for failure handling, privileges and request bounds.
