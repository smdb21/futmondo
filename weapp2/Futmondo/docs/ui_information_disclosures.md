# Information and error disclosures

## Coach control and account summary placement — 2026-11-15 Pacific

`today_UI()` groups the existing coach checkbox and `coach_status` output in
`.fm-coach-control`. A two-column CSS grid places the information trigger beside
the checkbox label; expanded details span the next row. Native disclosure
keyboard behavior, pending/empty suppression and coach hiring checks are unchanged.

The root `round_countdown` output is the first content output, above startup
and preference notices and each page body. Its existing server visibility rule
shows the account/selected-league finances on Today's market, My Team and Players.
The redundant client conditional wrapper is removed. Balance, active offers,
after-offers balance and spending allowance remain immediately visible; only
source/error details use a disclosure. The root browser fixture
reproduced the output retaining `fm-info-waiting` after Shiny removed its typed
pending error class. The observer now clears the leftover marker on recovery,
and CSS hides an output only while that marker and its actual pending error
class both exist. Success can paint immediately without waiting for observer
delivery; a leftover marker cannot hide/suspend the recovered summary. Real
settled failures and empty/pending icon suppression retain their behavior.

Both changes present existing capabilities. Account/league isolation, durable
successful API observation reuse, original source/freshness/completeness metadata,
and manual-action eligibility remain unchanged. No API/database request, timer,
persistence schema, dependency or environment setting is added. The fixed grid
and existing wrapping finance row remain bounded on mobile.


## Empty and pending visibility — 2026-11-14 Pacific

`fm_info(..., error=FALSE, class=NULL, pending=FALSE)` now returns `NULL` for explicit pending states and recursively empty content (including empty nested tags/lists/whitespace). `fm_info_content(x)` distinguishes meaningful content from dynamic Shiny text/HTML placeholders. Dynamic disclosures start with `fm-info-empty`, so an empty information icon cannot paint before its output binds. The client reveals them only after their nested output values settle and the body has content, and hides them again when emptied or recalculating. Confirmed failures retain red disclosures and Retry actions. The red departure legend is replaced by a native tooltip on affected player names, preserving the club tooltip and updating when departure metadata changes. All selectable player/rival tables hide Reactable’s `.selection` column while retaining the selection model, row clicks and card navigation. Player-card headers omit the redundant Market: prefix. These are presentation changes to existing facts and controls, with no new data capability. No error text is guessed to be loading.

Pending reads use the existing explicit `fm-loading` validation class in the factual adapter and worker provider. Notification refresh preserves that pending condition instead of setting a false failure. My Team, header, Today, player cards/history, standings/lineups, awards, Rivals, Login and Admin pass explicit pending flags. Saving/transaction preflights and factual availability remain unchanged. Existing loading visuals continue to show progress; pending information icons are omitted.

The screenshot’s empty transfer box came from an unpopulated nested source-clock output. The incoming/outgoing/offer clocks, header finance clock and worker status text now use `suspendWhenHidden=FALSE`: they format already-loaded shared state even inside a closed/hidden disclosure. This breaks the hidden-output visibility deadlock without acquiring data. Their existing resource projections still prevent metadata-only whole-panel replacement. Values arriving before their DOM node binds are handled safely.

`www/info-disclosures.js` watches inserted/changed local subtrees, character data and pending classes, deduplicating affected panels with a temporary Set. An output-element WeakSet tracks settled bindings; the existing WeakMap preserves open panels across UI replacement. There is no polling or full-document scan per clock update. Only read-only metadata text is unsuspended, adding bounded changed-value WebSocket delivery; no HTTP/API/database request or cache/persistence policy changes. CSS hides empty/pending/recalculating panels before paint, including inline transfer icons.

Passed focused empty/pending/ready/failure and notification transition tests, existing disclosure/offer/transfer and worker-stability checks, six Chromium layouts (320/390/1440px, light/dark), and the offline lifecycle harness. Browser tests cover filling/clearing hidden information, delayed success/error, keyboard toggles, retained open panels, no overflow and zero source/database I/O. Measured 1,000 nested renders at 0.200s, browser heartbeats 61–62ms and mutation gaps 10ms. Production hosted acceptance remains separate; no dependencies, environment variables or schema changes.

Data availability, query/source explanations, database/cache status, update times,
settled diagnostics and calculation coverage appear behind a small round `i`.
Panels start collapsed and expand in place on click, Enter or Space. A red icon
identifies confirmed errors; ordinary information uses the muted theme color. Waiting and empty information icons remain hidden.
The accessible names are `Information` and `Error details`.

This is presentation only. Known figures and football facts remain visible;
missing values remain missing. Original source clocks, completeness, persistence
acknowledgements, context isolation and preflight/action checks are unchanged.
An error does not become a successful operation when its explanation is
collapsed. Explicit Retry controls remain inside the corresponding disclosure.
Consent and transaction-confirmation controls retain their existing behavior. Failed market bids and direct-owner offers are an explicit exception: `.fm-offer-error` notifications keep their full message visible until dismissed, including when Shiny reuses a previously compact notification ID. These actionable submission failures do not require opening an information icon.

## Interfaces and coverage

- `fm_info(..., error = FALSE, class = NULL, pending = FALSE)` in `data_contracts.R` returns a
  native closed `details` element with a round summary icon and an escaped/tagged
  content body, or `NULL` for empty content. It introduces no Shiny input, timer,
  network call or stored preference. Put block content in the body; do not nest
  the helper under a paragraph or inline text tag.
- `www/custom_style.css` provides the compact trigger, keyboard focus, theme-aware
  red state and wrapping for long diagnostics on phones. `ui.R` loads
  `www/info-disclosures.js`; its new asset is listed in the deployment manifest.
- The JavaScript handles Shiny-generated error/warning notifications and output
  validation errors, which do not pass through an R UI helper. It moves existing
  content into a disclosure rather than discarding or reinterpreting it. Notification
  dismissal keeps its own touch target; Shiny's generated `Error:` prefix is
  suppressed only when the red disclosure is present. Explicit
  `errorClass = "fm-loading"` validation remains hidden while waiting. Only inserted subtrees
  and changed notification/error containers are inspected; class changes catch
  severity added after asynchronous insertion, with no polling or text matching.
- Open panels survive replacement of their containing Shiny HTML output through
  an element-keyed `WeakMap`. Independently rendered source-clock text stays
  inside the existing panel. Opening a panel can resume an existing hidden Shiny
  output, but does not request refresh or initiate source acquisition.
- The header retains its numerical amounts; the former cached/unverified suffixes,
  financial status explanation and source clocks share a disclosure. Worker
  status projects an explicit `error` flag separately from message and timestamp,
  so successful timestamp changes do not replace its panel.
- Today, My Team, player tables/cards, Lineup, Rivals, Classification, Round Awards,
  Plots, Notifications, Login, preference feedback and Admin use the same helper.
  Access-setting polling notes and official-lineup provenance/legend are included.
- `rivals_status_message(message, loading = FALSE, error = FALSE, details = NULL)`
  accepts extra diagnostic content. `rivals_update_details(rows, timezone = "UTC")`
  displays per-team source times in that full-width panel; the former timestamp
  table column is hidden rather than forcing expanded text into a narrow cell.
- `plots_empty(message, error = TRUE)` attaches `fm_info` to its plot result.
  One reactive plot result feeds the plot and adjacent status output, avoiding
  duplicate reads/builds. `player_trend_information(history, player,
  matches = data.frame())` moves player-chart diagnostics out of Plotly annotations
  while preserving traces, signed points, axes and source/round associations.
- Lineup notice severity is tracked explicitly beside its existing message. Save
  eligibility, departure checks, final preflight, readback and uncertain-result
  reconciliation remain unchanged.

## Persistence and performance

No new API result or cache is introduced. Successful factual observations still
use the existing durable database persistence/reuse contracts across sessions and
restarts, with their original context, source times, freshness and completeness.
Failed writes remain retryable. Bounded process caches remain accelerators.

Native toggles need no server-side action. Source clocks remain separate outputs;
plot/status consumers share computed results. The browser observer has no timer,
retains no growing list of elements and never rescans the document on a normal
clock update. Rendering adds small markup wrappers and reduces default visible
text and chart margins. No dependency, environment variable, schema change,
worker schedule or rate change is required.

The focused R fixtures cover concealed diagnostics, real offer links, unknown
values, neutral loading, red failures and shared plot reads. The browser fixture
uses production CSS/JavaScript with synthetic data and blocked backend I/O. See
`data_review_results.md` for executed checks, timings and remaining limits.


## LigaRadar branding

All app-owned headings, buttons, source labels, notices and errors use LigaRadar.
The login notice remains: “Log in with your Futmondo account to authenticate
directly with the official Futmondo server.” API hosts, source identifiers,
authentication fields, browser preference keys and stored provenance retain their
existing contracts. User-supplied team/player names remain source data. Legacy
labels without identity use exact brand-alias matching in the view only.

`offer_bidder_label(name=NULL, source=NULL, bidder_id=NULL)` renders current and
previously saved system offers as LigaRadar without editing their canonical
`bid_user`, bid IDs or source. Player-card banners/confirmations and shared-table
tooltips reuse it; explicit rival identity and unknown bidders stay intact.
Notifications label the existing Futmondo filter option and source as LigaRadar
while keeping its submitted value. `rivals_cash_history_rows()` projects saved
system-counterparty labels in its view only; the shared accounting ledger and
balances remain unchanged. Existing saved observations require no migration or
refetch. No new dependencies, configuration, requests, timers or persistence.
