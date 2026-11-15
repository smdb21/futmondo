# Information and error disclosures

Data availability, query/source explanations, database/cache status, update times,
loading diagnostics and calculation coverage appear behind a small round `i`.
Panels start collapsed and expand in place on click, Enter or Space. A red icon
identifies errors; ordinary information and loading use the muted theme color.
The accessible names are `Information` and `Error details`.

This is presentation only. Known figures and football facts remain visible;
missing values remain missing. Original source clocks, completeness, persistence
acknowledgements, context isolation and preflight/action checks are unchanged.
An error does not become a successful operation when its explanation is
collapsed. Explicit Retry controls remain inside the corresponding disclosure.
Consent and transaction-confirmation controls retain their existing behavior.

## Interfaces and coverage

- `fm_info(..., error = FALSE, class = NULL)` in `data_contracts.R` returns a
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
  `errorClass = "fm-loading"` validation stays neutral. Only inserted subtrees
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
