# Admin league settings

Admin starts with a read-only **League settings** section for the currently selected league. Starting conditions, round rewards, market settings and squad/league limits use stacked label/value/explanation rows that wrap on phones. Database and award controls remain below.

`admin_league_settings(league, info, championship_id)` is a pure presentation adapter returning group, setting, value and explanation columns. It combines the league configuration with the primary team configuration, with the latter taking precedence. It exposes only known fields, preserves zero/false, marks missing values, and labels the user-confirmed opening cash fallback separately from API values. Unknown ranking modes are not interpreted. Ranking direction/distribution uses the established contracts documented in `rival_cash_accounting.md`; this display does not calculate payments.

`admin_Server(..., league_settings_source = NULL)` accepts a reactive snapshot with `league`, `info` and `stale` fields. The application supplies its existing selected league and primary team resource state. Rendering is gated by active Admin navigation and the existing administrator check. No new requests, writes, caches, timers, or dependencies are added. The module can still initialize without this optional source.

Configuration changes must be made in Futmondo. The existing app refresh updates shared resource observations. The display describes current rules, not proof of historical rules or bonus payments. Focused coverage: `test/test_admin_league_settings.R`; shared wiring is covered by the offline lifecycle harness.
