# Account and league state isolation

The root application passes reactive account, championship and user-team identities to each module. A real player ID may exist in several championships with different ownership, scoring, value and action permissions. Player identity alone is therefore insufficient for a pending selection or confirmation.

## Player tables and confirmations

`players_table_Server(id, players_table_RV, user_teams_RV, login_token, championship_id, user_team_id, hide_bid_column, refresh_trigger)` keeps a selected player ID together with the exact `{user, championship, team}` context. Its selection reactive returns the matching current row only when both identity and context still agree. Context changes clear the reactable selection and close existing modals. Within an unchanged context, table reordering retains the selected player's identity.

`selected_player_Server(id, selected_player, login_token, championship_id, user_team_id, on_bid_updated, open_action, capacity_fetcher)` binds each pending confirmation to `{user, championship, team, player}`. Its internal helpers are:

- `player_action_context_RV()`: returns that identity list for a valid authenticated selection, otherwise `NULL`.
- `remember_action_context(action)`: records the context for the opening modal and returns invisibly. `action` is one of `bid`, `modify`, `cancel`, `owner_offer`, `clause`, `list`, `delist`, `accept`, or `reject`.
- `consume_action_context(action)`: returns `TRUE` once for an exactly matching pending confirmation, otherwise `FALSE`. Every account mutation handler requires it before reading mutation inputs or calling an API.

Changing the account, league, team or selected player clears pending confirmations, verified own-bid state, cached smart-bid results and acquisition-modal flags. The confirmation guard also protects against a click already queued when the league selector changes. A failed or rejected attempt requires reopening its confirmation before another attempt. External Today market/clause routes use the same modal helpers and context capture as direct buttons.

These changes do not add a public network interface. Existing action callbacks continue to report explicit action types and trigger authoritative refreshes.

## Rival selection

`rivals_Server(...)` captures the rival's immutable team ID and the same account/championship/user-team context when a row is selected. Refreshes resolve that ID against the current finance table. League/account changes clear the selection; an old row index can no longer silently select another league's rival.

## Integration verification

`test/test_application_offline.R` sources the real application startup, UI and root server, and blocks all HTTP methods. The two-league fixture deliberately shares real-player IDs across leagues while changing roster membership, market values, cash, rival managers, legal formations, multiposition, captain settings and scoring weights (100 versus 200 total).

The test switches A → B → A and verifies current roster/rules/forecasts/finance/model cohorts. It opens an actual listing confirmation before switching and asserts that replaying its submit event performs no mutation. It verifies rival selection clearing, private notification filtering and detail-state clearing, rejection of a stale private-alert acknowledgement, and inability to pause an old league's automation policy after switching.

`test/test_ui_reliability.R` additionally checks identity retention when the same league's table reorders and explicit action callback payloads. The small callback fixture supplies an approved cancellation-context guard; actual context matching and invalidation are exercised through the root-server integration test.

Run:

```sh
Rscript --vanilla test/test_ui_reliability.R
Rscript --vanilla test/test_application_offline.R
```

The latest focused results are 14 UI reliability checks and seven root lifecycle checks plus nine rendered tabs, all passing. No live requests, database mutations or migration application are performed by these tests.
