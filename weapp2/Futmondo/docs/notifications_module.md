# Notification inbox

`notifications_UI(id)` returns responsive filters, refresh/status views and event cards. `notifications_Server(id,is_module_active,login_token,championship_id,user_team_id,user_teams_RV,refresh_trigger=NULL,on_market_event=NULL)` receives reactive authentication/league context and returns `list(unread=reactiveVal)`. It polls the unread count every sixty seconds in an authenticated session and loads the list when opened. League changes clear selected player context. Successful player actions invalidate account caches and invoke `on_market_event(...)` so the root reconciles affected views.

`notification_request(login,action,query="")` returns a fetch-result contract. `get_notifications(login)` caches/normalizes `/1/notification/list` (`answer.items`). `get_notification_unread(login)` reads `/1/notification/unread` (integer). `mark_notification_read(login,id)` sends `{id}` to `/1/notification/markreaded` and returns TRUE only for `notification.markReaded.ok`; failure retains unread state.

`normalize_notifications(items)` accepts event objects with `_id`, `created`, `updated`, `type`, `action`, `readed`, `directObject`, `subject`, and `context`. It returns ID/timestamps/type/action/read state/player/league/source/message columns. Unknown event types retain their original action label. Remote text is escaped, and links verify active league membership. Opening the inbox never marks all items read.

`build_insight_alerts(user_id,championship_id,user_team_id,financial,squad=NULL,offers=NULL,now=Sys.time(),market=NULL)` returns idempotent, account/league/day-scoped alert rows. It checks signed cash, legal XI, availability, reported clause times, incoming offers, owned auction deadlines and verified round deadlines. Alerts distinguish Insights from Futmondo. Worker failures appear through the same private alert table. Solvency warnings use Futmondo's positive-cash rule. No hypothetical profit notification is emitted without adequate model evidence.

```r
rows <- get_notifications(auth)
ok <- mark_notification_read(auth, selected_notification_id) # explicit interaction only
```

Focused coverage: `test_data_contracts.R`, `test_history_persistence.R`, `test_har_contracts.R`; Shiny lifecycle checks escaping, failed acknowledgements, navigation and account changes.

## Player-details eligibility

`notification_has_player_details(notification_row)` returns one logical value for a normalized, single-row notification. It requires a non-empty `player_id` and rejects round-closure events detected from the normalized `type`, `action`, and `message` fields, including values such as `roundClose`, `round close 4`, and `Round closed: 4`. The items renderer uses this result to omit the **Player details** button, and the `open_player` observer repeats the same check so a synthetic client event cannot open a player panel for a round closure.

```r
notification_has_player_details(data.frame(
  type="round", action="roundClose", message="round close 4", player_id="4"
))
# FALSE
```

Focused regression coverage: `Rscript test/test_notification_player_details.R`.
