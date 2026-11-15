# Official notifications

The notification bell and inbox show official Futmondo events, unread counts, filters, and player links. Generated recommendation alerts and model-driven event storage are excluded.

`notifications_UI(id)` creates the view. `notifications_Server()` uses reactive account/league/team context and returns unread state for the header. Opening a player reference uses the same context-scoped player card as the roster and market.

Read state changes only after explicit interaction and successful Futmondo acknowledgement. A failed acknowledgement must leave the item unread. Switching context clears pending event/action state. Notification content is escaped before display.

Relevant checks: `test/test_notification_player_details.R`, `test/test_notification_datetime.R`, and the offline lifecycle harness.

## Header bell placement — 2026-11-09

The header displays an icon-only Notifications bell beside the LigaRadar title on the same line, including mobile. An unread dot appears only for a verified positive unread count; the tooltip and accessible label report the exact available count. Unknown counts remain unknown without a dot. Clicking the bell uses the existing Notifications tab handler. The bell remains available when the desktop sidebar is collapsed; the regular sidebar route is retained.

This relocates the existing `notification_bell` output and formats its existing unread state; it adds no data acquisition, polling, persistence, dependencies or reactive observers. The focused browser check is `node test/test_header_notifications.cjs`, using the existing offline `Rscript test/test_browser_app.R` fixture.


## Unread card layout isolation — 2026-11-10

The header dot uses `fm-notification-unread-dot`, scoped beneath `fm-notification-link`. Inbox cards retain `fm-notification-unread` and normal full-width document flow. Keeping these styles separate prevents the 7px absolutely positioned header indicator from collapsing unread cards at the right edge.

`test/test_header_notifications.cjs` checks the actual unread card's positioning, width, message, action bounds and horizontal overflow at 320, 390 and 1440px, alongside the header dot and navigation. It uses synthetic offline data and introduces no application data or persistence changes.


## Notification dates and headings

Notification card headers contain only the league and a readable UTC timestamp,
for example **OHY CAMPEÓN · 12 Nov 2026, 12:34 UTC**. The source name is omitted
from each header. The existing source filter and canonical source data remain.
`notification_datetime_labels(values)` validates complete ISO timestamps before using the shared parser and
returns day/month/year plus hours/minutes with an explicit UTC suffix; missing
or invalid times remain **Date unavailable**. It also formats the last-refresh
notice. Valid original timestamps remain in the HTML `time` element's `datetime`
attribute and in the underlying observations, preserving ordering and precision.

Formatting occurs once per existing render over the displayed rows. No new
requests, database reads/writes, persistence behavior, caches, reactive inputs,
timers, dependencies or CSS changes are introduced. Existing wrapping supports
narrow screens; the labels omit ISO separators, seconds and milliseconds.


## Collapsible filters

The read-status, source, event and current-league controls, together with Refresh,
share a full-width **Filters** box that starts collapsed. It uses the existing
`shinydashboardPlus::box()` collapse behavior and responsive Bootstrap columns.
Expanding or collapsing the box retains selections; notification status and
cards remain outside it. Input IDs, defaults, filtering, refresh, source reads
and database-backed reuse are unchanged. No new dependency, observer, timer,
request, persistent setting or CSS is needed.
