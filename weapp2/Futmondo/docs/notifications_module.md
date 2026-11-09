# Official notifications

The notification bell and inbox show official Futmondo events, unread counts, filters, and player links. Generated recommendation alerts and model-driven event storage are excluded.

`notifications_UI(id)` creates the view. `notifications_Server()` uses reactive account/league/team context and returns unread state for the header. Opening a player reference uses the same context-scoped player card as the roster and market.

Read state changes only after explicit interaction and successful Futmondo acknowledgement. A failed acknowledgement must leave the item unread. Switching context clears pending event/action state. Notification content is escaped before display.

Relevant checks: `test/test_notification_player_details.R` and the offline lifecycle harness.

## Header bell placement — 2026-11-09

The header displays an icon-only Notifications bell beside the Futmondo title on the same line, including mobile. An unread dot appears only for a verified positive unread count; the tooltip and accessible label report the exact available count. Unknown counts remain unknown without a dot. Clicking the bell uses the existing Notifications tab handler. The bell remains available when the desktop sidebar is collapsed; the regular sidebar route is retained.

This relocates the existing `notification_bell` output and formats its existing unread state; it adds no data acquisition, polling, persistence, dependencies or reactive observers. The focused browser check is `node test/test_header_notifications.cjs`, using the existing offline `Rscript test/test_browser_app.R` fixture.
