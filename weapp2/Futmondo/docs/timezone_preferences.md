# Personal timezone and uniform dates

Current behavior, corrected 2026-11-15: each user chooses their own timezone in
**Welcome → Display timezone**. With no explicit personal choice, the browser's
IANA timezone is detected automatically. Spain (`Europe/Madrid`, including
mainland Spain's daylight-saving rules) is the fallback if detection is missing
or invalid. A saved personal choice always overrides browser detection. The same
control is available to ordinary users and LigaRadar administrators; Admin no
longer contains a user timezone directory or cross-user editor.

All displayed timestamps retain `DD/MM/YYYY HH:MM TZ` and a 24-hour clock.
Date-only chart axes and date pickers use `DD/MM/YYYY`. The banner, incoming/
leaving clauses and observations, notifications, transaction tables, rival
histories, player fixtures/charts, league charts, awards and worker source-check
text share the account's display preference. Original UTC/offset instants,
expiry checks, countdown durations, ledger values and context identity retain
their original meaning. `Europe/Madrid` is a named zone, not a fixed UTC offset;
users elsewhere, including the Canary Islands, can select their own IANA zone.

## Selection and durable storage

`fm_timezone_preferences(session, login, store)` immediately starts one
asynchronous personal preference lookup at login, without waiting for browser
reporting or a fallback timer. A missing row is an unset preference, so the
current detected default is immediately usable without an automatic write.
Detection uses `Intl.DateTimeFormat().resolvedOptions().timeZone`, without
coordinates or geolocation permission. The server validates the IANA value.
Late detection updates only an unset default; it causes no database read or
write. A missing/invalid detection uses Madrid until valid detection arrives. Saving an explicit choice
persists it in `user_timezone_preferences` through the existing server-only
`set_user_timezone` RPC with `source=user`, including when the user is a
LigaRadar administrator. A successful save must acknowledge the matching
account, timezone and personal source before the UI applies it. Saved choices
survive logout, browser/device changes, process restarts and redeployments.

Legacy `source=browser` rows were automatic defaults and `source=admin` rows
could be overrides by another person. They remain stored, but do not establish
an explicit personal choice: the current browser supplies the default until
the user saves a timezone. Existing `source=user` choices remain authoritative. No historical
values are deleted or relabeled. The existing schema and migration remain
compatible; no new migration or bulk production rewrite is required.

A read failure displays the temporary detected default (Spain fallback) and an actionable failure notice;
it does not imply a missing preference. Saves remain disabled until the saved
state can be checked. A failed or uncertain save stays unconfirmed; retry
reads the authoritative row before another write, and never blindly replays a
write. Loading/saving yields to Shiny so navigation remains responsive.
Overlapping saves are suppressed, and logout/account changes invalidate late
callbacks. Rendering performs no database request or initialization write.

`www/worker-state.js` publishes visibility and detected timezone on connection/
visibility changes. Equal inputs are deduplicated by Shiny. Its local countdown
remains independent of timezone choice and sends no per-second server traffic.
Background-sync **Quiet-hours timezone** remains a separate personal scheduling
setting; its existing initial value follows the display timezone if no saved
quiet-hours setting exists. Changing display timezone does not reschedule an
already configured worker.

## Interfaces and authorization

- `fm_timezones()` / `fm_valid_timezone(x)`: bounded IANA catalog and validation.
- `fm_display_timezone(session)`: account-local reactive display zone; Madrid
  outside an initialized session.
- `fm_format_datetime(x, timezone, date_only, missing)`: vectorized labels with
  unavailable dates distinct from real instants.
- `fm_plot_datetime(x, timezone)`: civil Plotly axis strings and explicit hover
  zones, preventing a second browser timezone conversion.
- `fm_local_midnight(date, timezone)`: calendar boundaries preserving 23/25-hour
  DST days and inclusive local date filters.
- `fm_timezone_store(user_id, timezone=NULL, mode=get|set, token, owner)`: only
  the authenticated account's row can be read or written, even for an admin.
  Unsupported initialize/list modes are rejected before I/O. GET accepts a
  missing row as unset, validates persisted provenance, and SET verifies the
  acknowledged personal choice. Credentials remain on the R server.
- `fm_timezone_preferences(session, login, store)`: owns account generation,
  personal selector, detected default, acknowledgement, retry and readiness state.
  The former `fm_admin_timezones()` controller has been removed.

Legacy SQL initialization/directory functions remain server-only for stored
schema compatibility; the application no longer calls them. Existing restricted
privileges and RLS are unchanged. There is no client account selector, public
preference directory, process-global TZ mutation, new dependency or environment
variable. Database preferences are durable; session state only accelerates the
current user's display.

## Verification and performance

Focused tests cover detected defaults without a stored choice, late detection,
missing/invalid detection with Spain fallback, explicit-override precedence, legacy provenance, durable personal saves/reloads, administrator
self-only authorization, uncertain-save readback, late replies, held read/write
responsiveness, DST/offset known answers and shared module dates. Synthetic
browser checks cover personal controls at phone/desktop widths, a Los Angeles
default in a Los Angeles browser, a saved Madrid override, detection failure,
account isolation, save acknowledgement and reload
reuse without live accounts or database writes. Admin browser checks assert
zero timezone-directory reads. The offline lifecycle is mandatory for these
login changes; actual results and limits are in `data_review_results.md`.

Performance: one bounded asynchronous GET per login/retry and one confirmed
write per explicit save. A detected default creates no row. The cross-user
directory remains removed. Detection adds one guarded session observer and
bounded browser-state publications, with no one-second wait or timer. There
are no detection-triggered database operations, new polling, source acquisition, cache, queue or worker is introduced. The
bounded IANA catalog and mobile-safe selector are reused. Hosted rollout and
physical-device/network latency remain separate from local verification.
