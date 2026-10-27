# Official notifications

The notification bell and inbox show official Futmondo events, unread counts, filters, and player links. Generated recommendation alerts and model-driven event storage are excluded.

`notifications_UI(id)` creates the view. `notifications_Server()` uses reactive account/league/team context and returns unread state for the header. Opening a player reference uses the same context-scoped player card as the roster and market.

Read state changes only after explicit interaction and successful Futmondo acknowledgement. A failed acknowledgement must leave the item unread. Switching context clears pending event/action state. Notification content is escaped before display.

Relevant checks: `test/test_notification_player_details.R` and the offline lifecycle harness.
