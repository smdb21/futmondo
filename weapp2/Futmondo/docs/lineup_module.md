# Next-round lineup

## Match crests — 2026-11-17

Each home and away club now displays its observed crest beside its name in the
next-round fixtures. Following the 2026-11-18 layout adjustment, the home crest
sits on the outside left and the away crest on the outside right, with both
names toward the middle. The 24px images have fixed dimensions, lazy loading and
asynchronous decoding; long names still wrap on mobile. The adjacent text remains
the accessible club label. Missing or failed images leave the name visible.

Rechecked the [official public client](https://app.futmondo.com/main.dart.js) on
2026-11-17 (SHA-256
`08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c`):
`ces` parses each match's `h`/`a` through `Ij`, which reads the club's `logo`.
`league_fixture_matches()` retains those exact values as `home_logo` and
`away_logo` in inner fixture payload version 2. The existing outer calendar
evidence payload and worker snapshot format are unchanged. The source-field
check used public code and synthetic responses, not a signed-in account.

`league_fixture_empty(version = 2L)` defines the current row schema and can
construct the original version-1 schema. `league_fixture_matches_valid()` and
`league_fixture_matches_decode()` accept either; `league_fixture_matches_current()`
identifies the version that carries logos. `get_award_round_metadata(...,
include_matches = TRUE)` upgrades version-1 evidence through the existing worker
read, persists the successful result and reuses it after a restart.
`get_finished_rounds()` retains a valid saved schedule and its original clocks
if that upgrade fails, adding `fixture_calendar$refresh_error` for
`lineup_fixture_view()` to show a stale/error note. Normal sync can retry;
opening or refreshing the page never acquires the calendar.

`lineup_fixture_crest_url(logo)` is a pure formatter. It accepts an observed HTTPS
URL or a safe observed filename under the existing `TEAM_LOGO_URL` CDN prefix,
adding `.png` when no supported image extension is present. Empty/unsafe values
produce no image. It does not infer filenames from club names. Absent logos do
not erase matches or change schedule-completeness claims.

No schema migration, dependency, environment setting, additional worker job or
poll is needed. Durable source reuse, source clocks and context isolation are
preserved. The display adds at most two lazy crest images per match (100 at the
existing 50-match limit), with ordinary browser asset reuse and no extra API or
database read. Focused persistence, legacy/failure and responsive-browser checks
passed; measurements and hosted verification limits are in
[data review](data_review_results.md).

## Save placement and next-round fixtures — 2026-11-16

The existing Save to LigaRadar control now sits above Formation and the pitch.
Its validation, live manual-action preflight, duplicate prevention and readback
remain unchanged. Below the pitch, substitutes and optional Captain selector,
`lineup_Server` renders `fixtures`: home/away match cards for the same verified
round selected by `lineup_target()`, ordered by kickoff. Cards use two columns on
desktop and one on mobile; long club names wrap. Kickoffs use the account's
selected timezone. Missing opponents/times remain unconfirmed, incomplete
schedules are labelled, and expired observations retain their source time and an
awaiting-sync note. No date, opponent, round identity or missing count is invented.

### Source and payload

Verified 2026-11-16 in the [public official client](https://app.futmondo.com/main.dart.js)
(SHA-256 `08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c`):
`qT/axL` calls `/2/league/matches` with `{leagueId: <source league ID>}`;
`cwe` maps `answer.rounds` and `ces` maps each round's `matches`. The club parser
`Ij` establishes `h` as home and `a` as away club objects, `_id`/`id`, `name` and
`shortname` (plus `logo`, rechecked above); `info.date` supplies kickoff and `st` the source status. These are
real clubs, separate from fantasy user teams. The public bundle and synthetic
responses establish the mapping; no authenticated production response or
current official UI was inspected for this task.

`mvp_runtime.R` adds:

- `league_fixture_empty(version = 2L)` defines the display row columns: `round_id`,
  `match_id`, `home_id`, `home_name`, `home_logo`, `away_id`, `away_name`,
  `away_logo`, `kickoff`, `status`.
- `league_fixture_matches(records)` whitelists these facts into
  `{version: 2, rounds: frame, rows: frame}` (original version 1 remains readable). Coverage rows contain `round_id`,
  `fixture_count` (unknown for an absent/malformed/oversized collection) and
  `complete`. A complete round requires all returned match identities, distinct
  home/away clubs, club names and valid dates. Duplicate match identities are
  excluded and coverage stays partial. Bounds are 120 rounds and 50 games per round.
- `league_fixture_matches_valid(value, round_ids = NULL)` verifies types, bounds,
  identity/count consistency and any asserted completeness. Optional round IDs
  require an exact source-calendar identity set.
- `league_fixture_matches_decode(value)` restores the typed frames from JSON;
  `league_fixture_observation(metadata)` carries these frames with scope, original
  `observed_at`, absolute `valid_until`, source status and failure reason.

`get_award_round_metadata(..., include_matches = FALSE)` retains the normalized
match facts on acquisition. With `include_matches = TRUE`, a legacy calendar
missing these fields is reacquired once in the worker and then reused.
`persist_award_calendar_snapshot()` adds optional `fixture_matches` to the
existing calendar evidence payload (its existing version 2 remains compatible).
The outer calendar completeness describes round metadata; fixture completeness
is independently recorded per round. Invalid stored fixture identity coverage
causes a source cache miss, not a successful empty schedule.

`get_award_round_catalog()` carries already-acquired fixture metadata in a
`fixture_calendar` attribute. `get_finished_rounds()` reuses it, including failure
state, so a failed request is not repeated merely to obtain fixture details. If
that catalog did not need the bulk endpoint, the worker reads it through the
same database-first metadata cache. The shared round frame's `fixture_calendar`
attribute is included by the existing worker codec and durable resource snapshot.
`lineup_fixture_view(rounds, target, scope, now)` selects the exact target round,
checks account/championship/competition/season, and prepares truthful display
state without I/O. The outer worker envelope retains team/scoring context too.

### Persistence, performance and delivery

Both compact calendar evidence and the worker round snapshot use the existing
server-only database stores. Fresh successful evidence is read before acquisition;
reopening or restarting preserves the source deadline and creates no replacement
write. Failed persistence retains the existing retry behavior. Only whitelisted
factual fields enter storage; no credentials or raw private response bodies do.
No migration, dependency, environment setting, job, lane, timer or source
concurrency increase is needed. Old worker snapshots populate the new field on
the normal calendar sync after deployment.

The display uses the existing shared round resource: opening, navigating,
editing, resizing or Refresh adds no fixture HTTP/database request. A worker
calendar that previously needed only compact league metadata may now need one
bulk calendar read when cold/expired; callers that already needed it share that
read. The parser processes at most 6,000 matches, and the UI renders at most 50.
A synthetic 380-match season used 96,848 bytes; parsing took 0.274–0.332 seconds in
the worker and display selection/validation averaged 17–19 milliseconds. Mobile
and desktop checks and the full offline lifecycle passed; see
[data review](data_review_results.md). These measurements do not establish
production worker latency, authenticated fixture completeness or hosted rollout.

The **Lineup** sidebar page edits the selected league's current editable Futmondo lineup. Its heading names the next scheduled round; the source mutation API itself has no round parameter. The page never submits a historical or arbitrary future-round lineup. Saving is disabled without an identified next round and rechecks its identity/time immediately before submission.

The pitch supports the twelve official 11-player formations, including 4-6-0. Available formations come from `/5/strategy/availables`; premium entries remain disabled unless the current source reports the relevant purchase or it is the existing formation. The app never buys formations. Player pickers use the squad's primary role and permitted alternative role, prevent duplicates across pitch/bench, and move/swap players only when the other position remains valid. Incoming clause transfers do not enter this squad before authoritative ownership arrives.

**Refresh lineup** reads the latest official lineup, current squad and formation/rule availability from Futmondo and persists successful observations through the existing snapshot store. It preserves the current on-screen draft, including unsaved optimized or manual selections, and never saves that draft to Futmondo. Use **Undo changes** to replace the draft with the latest loaded or successfully saved official lineup.

**Clear lineup** empties starters, four substitutes and captain locally, preserving the selected formation, N and doubtful-player toggle. It makes no request and cannot submit an invalid empty XI. **Undo changes** restores the latest lineup loaded from or successfully saved to Futmondo, including a save confirmed by readback. Drafts survive navigation during the session; official successful observations survive restarts through database snapshots. A direct account replacement discards prior private drafts.

Player tiles display a compact club crest beside the name for both starters and substitutes, in the editor and historical round lineups. Names wrap within their slot on narrow screens. The roster parser retains the source `team.logo` (or flat `logo`) in the existing database snapshot; hydration reuses it without new API or database requests. Older snapshots use the existing club-name image lookup until the normal 30-second refresh supplies source metadata. Images load lazily and share browser caching by URL. Position colors underline names without a separate abbreviation; bench colors use the main role only.

The **Include doubtful players** checkbox occupies its own full-width row below Optimize and recent-round controls on mobile and desktop. Its label uses the available row width and wraps naturally on narrow screens.

## Best XI and bench

**Best XI + bench** compares arithmetic means over the same latest N source-confirmed completed rounds for all squad players, default five, selectable from one to twenty. Before N rounds exist, the actual completed count is displayed. Ongoing rounds and unanchored last-five vectors are excluded. Explicit zeroes and negative scores count normally; absent scores remain unknown. Missing histories are requested through the shared bounded controller and persisted through its existing score-first/check-last workflow. Best XI waits for complete scores for every eligible squad player before changing the draft. A single click remains pending while the schedule or histories load, with visible progress; verified publication completes that request automatically. Each click calculates once. Changing N updates displayed averages while keeping starters, bench, formation and captain unchanged. Changing N or the doubtful-player toggle cancels pending calculation; another explicit click is required. A generated lineup carries its calculation settings and completed-round window with its session draft, including when navigating between retained league/round contexts. When these differ from the current controls, a warning explains that the lineup does not correspond to Best XI for the new settings and asks the user to recalculate. Returning to the original settings clears the warning. The warning applies only while the lineup still matches the generated selection, and remains available after a verified save. Manual edits, Undo changes, explicit lineup refresh, save and account/league/round changes cancel pending calculation.

The button shows **Calculating…** with an animated spinner while a request is pending. Waiting-history messages remain visible, and the **Calculating Best XI + bench…** status is flushed to the browser before the optimizer runs on the next event-loop turn. Completion, no eligible XI, cancellation and optimizer errors settle the busy state; failures retain the draft. No artificial delay or recurring work is added. Average/history reads retain their existing durable reuse; changing controls never runs the optimizer by itself or introduces lineup API/database requests. The warning compares small local metadata and the displayed lineup identity.

A vectorized positional-count dynamic program selects the maximum-total XI across every allowed formation with each player used at most once. The selected formation is a manual-edit starting point, not a constraint on Best XI: the calculated formation replaces it. For equal historical totals, the optimizer first maximizes the number of dual MID/FWD players assigned to midfield, then prefers the existing formation, retaining existing starters, and stable formation/player order. This midfield preference is included in both player assignment and formation selection, applies only when multiposition is enabled and both roles are eligible, and allows a forward assignment whenever it produces a higher historical total or is required for a legal XI. It does not add an estimated defensive-stat bonus to observed scores. The captain is retained if still starting; otherwise the highest-average starter is selected when captains are enabled.

By default, the bench is selected from the remaining players after fixing the best starting XI. A separate bounded dynamic program first maximizes distinct outfield positions covered by distinct substitutes, then their total historical averages. It fills up to four places and orders them by average descending. Goalkeeper-only players are excluded from automatic substitutes; manual bench selection may still include them where the official rules permit. Injured, suspended and unavailable players are always excluded from the optimized XI and bench. Doubtful players are excluded by default and may be included with the checkbox. Missing coverage and unfilled places stay visible. Coverage indicators use the same distinct-player matching as the optimizer, so one multiposition substitute cannot claim three simultaneous replacements. The optional **Require full outfield bench coverage** checkbox constrains the XI to leave this coverage available; see the contract below.

The lineup history client requests squad IDs and the exact completed-round window. It does not initialize the championship catalog until an ordinary player-table client requires it. New completed windows can request one bounded new acquisition despite an older successful same-N attempt. Existing table clients preserve their defaults and wider-window semantics.

## Departure eligibility for the next round

Lineup and My Team consume one context-scoped `incoming_clause_source` through the optional `clause_transfers_source` module argument. The optional `clause_transfers_refresh` callback retries that shared source on explicit **Refresh lineup**, preserving the local draft; standalone modules invalidate and refresh their own clause source. This refresh is scoped to clause evidence and the existing lineup reload. The root activates that source while either page is open; a standalone Lineup module creates its own source only when none is supplied. This reuses the existing persisted clause summaries and shared roster/history resources, with the same bounded two-read candidate queue and truthful partial/observed states. Entering Lineup first may activate those existing roster/history prerequisites and up to two candidate summary reads concurrently. Production shares that queue with My Team; moving between the pages reuses its observations. The normalized eligibility state publishes only material changes, so source timestamp updates alone do not redraw the picker or status.

`clause_departure_round_state()` compares authoritative outgoing `leaves_at` instants with the selected round start. A player leaving **before or exactly at kickoff** is excluded from both optimized starters and substitutes, even when Include doubtful players is enabled. A player leaving later remains eligible. The optimizer's optional `excluded_ids` argument applies before positional optimization and bench selection. Missing history for an excluded player does not hold up calculation; the existing history display remains available.

Optimize waits for a complete departure check, including known departure times and the next-round schedule. Unknown/partial evidence retains the draft and explains the missing check; a verified publication completes the pending explicit calculation once. Known departures remain excluded when a partial update retains their last verified dates. A later change in exclusions warns that a generated lineup needs recalculation, without changing the draft automatically.

The manual picker retains departing players with a **leaves by round start** explanation and a disabled selection button. Server-side selection and save checks also reject them. Existing official/draft selections stay visible for review and replacement. Save requires complete departure evidence, checks both XI and bench, and checks the current shared state directly again after its existing asynchronous official preflight, before submitting; this validation does not wait for the deduplicated display projection. It introduces no extra mutation or automatic retry.

Offline regressions cover one second before/at/after kickoff, the doubtful-player option, missing histories only for excluded players, unknown departure dates, deferred completion, disabled picker buttons/server-side rejection, bench validation, and a departure discovered while save preflight is pending. These changes add a bounded local set-membership check and no timer, per-player table fetch, or snapshot format change.

## Official save and durable observations

The public Flutter client at <https://app.futmondo.com/main.dart.js> establishes the `/5/userteam/multichanges` request shape: `championshipId`, `userteamId`, `current`, `modified`, and ordered `changes`. Current/modified states contain `strategy` and player arrays with `_id`, zero-based `position`, `mp`, `pr`, `b`, and `cpt`. `pr` is purchase price (falling back to value), not a role code. Normal placement events use `multiposition=false`; the slot carries the assigned role. Source captain/lock fields are `cpt` and `locked`. Synthetic contract fixtures cover removal-before-placement swaps, formation events, captain events, purchase-price mapping and lock preservation.

Save performs a fresh parallel read of lineup, roster, market-type information and formation entitlement. It validates ownership, every assigned role, bench/captain capabilities, current source locks, season/scoring context and the editing baseline. An external change is a conflict, with the draft retained. One explicit batch is followed by a fresh readback; timeouts and ambiguous responses never replay the mutation. A matching readback establishes success. Failed/mismatching readback leaves the draft and an actionable status. Context generations discard late results after account/league/round changes.

`lineup_store.R` stores only normalized successful observations in `team_lineup_snapshots`, with account/league/team/target/season/scoring identity, observation time, 30-second expiry and completeness. The read enforces independently known active season/scoring dimensions; first-entry unknown dimensions remain the source snapshot's labelled context until live refresh/preflight. UTC timestamps compare as instants, and JSON assignment arrays normalize back to character vectors. Fresh hydration never writes the same observation back. Stale saved data can display while a live refresh runs. No token/password is stored. Missing storage configuration or persistence failure is visible separately from successful live retrieval.

Existing databases require `scripts/migrations/20261107191054_lineup_snapshots.sql`; the fresh schema includes the table. The migration also adds `lineup` to the last-page preference constraint. RLS and grants restrict snapshots to the server role; stale concurrent writes cannot replace a newer snapshot. No new R package, environment variable, worker, scheduler or recurring poll is introduced.

## Local verification

- `test/test_lineup_runtime.R`: all formations, exact completed arithmetic, empty state, bench coverage/order, injury policy, source fields and batch events. The 100-player/all-formation XI + bench calculation averaged 41.8 ms over twenty local runs.
- `test/test_lineup_history.R`: four groups covering squad-only saved reuse, exact-window acquisition, same-N window advance and lazy catalog handoff.
- `test/test_lineup_module.R`: seventeen groups covering save/conflict lifecycle, context invalidation, complete-history waiting, one-shot explicit calculation, flushed progress, unchanged selections and changed averages across N, stale-settings warnings, cancellation, manual picker edits and failure-state cleanup.
- `test/test_lineup_store.R`: normalized durable hydration, timestamp equivalence, context rejection and authentication exclusion.
- `test/test_lineup_schema.cjs`: three groups covering fresh/repeatable schema, newer-write retention, bounded complete payloads, page constraint and server-only access.
- Independent exhaustive small-fixture validation covers optimizer/bench correctness and multiposition cases. Actual browser fixtures cover clearing, best XI + bench, position selection, one verified synthetic save, reset and viewport width. Warm picker interaction measured 111 ms at mobile width and 101 ms at desktop width after removing the picker-specific modal fade.

These implementation checks are local and synthetic/offline. No authenticated live lineup mutation or application deployment is claimed. Source/API failures remain visible; the code does not invent unavailable facts or infer unsupported lock windows.

## Lineup loading recovery — 2026-11-07

Authenticated read-only reproduction received successful lineup, roster, information and formation responses, then failed with `Formation entitlement is unknown`. The live `/5/strategy/availables` response omits `pro` for its seven standard formations and sets it to `true` for paid formations. The official client's premium test is `pro === true`. The parser now accepts absent/null standard markers while rejecting malformed non-null markers; unpurchased paid formations remain locked. Normalization also removes the OpenSSL hash class from the scoring-version string so successful observations can serialize into database snapshots.

`test/test_lineup_loading.R` exercises the real module, all four acquisition parsers and the snapshot store at an offline HTTP boundary. It verifies pitch rendering, one successful database write, fresh persisted reuse by a new session without extra Futmondo requests or writes, malformed-flag rejection and paid-formation locks. Exact live-response replay and fresh authenticated reads also verify normalization and snapshot JSON hydration. No Futmondo mutation or hosted snapshot write is used for verification. Acquisition still uses four parallel reads; the fixes add no requests, queries, polling or dependencies. Hosted application deployment remains separate.

## Completed-round discovery recovery — 2026-11-07

The captured `/2/league/list` response contains 38 round IDs with only `next`/`future` labels and no round numbers. Those compact labels do not establish historical completion. The shared `get_finished_rounds` schedule now uses the existing numbered-calendar resolver, joining `/2/league/matches` results by round ID. Lineup averages and history acquisition therefore receive the same verified completed window. Unverified calendar metadata blocks optimization while retaining the draft; missing scores remain unknown.

Successful compact calendar observations use the existing server-only database evidence table, isolated by account, championship, season and source league, with the active round, observation time, completeness and expiry recorded. Fresh observations survive process-cache clearing without another calendar request or write; their original absolute expiry also survives outer resource caches. Explicit award retrieval and a changed active round recheck the source. Failed or conflicting acquisitions cannot replace successful stored metadata. No schema migration, dependency or recurring poll is added; a cold unresolved schedule adds one bulk calendar request and a bounded one-row database lookup. Player history continues using the existing durable score/check workflow and squad-only demand.

Focused verification covers the real asynchronous round getter with compact-list HTTP fixtures, exact completed-round averages, calendar persistence/reopening, refresh, active-round advancement and unavailable/conflicting metadata. The lineup module regression covers partial-calendar recovery; the history regressions verify saved reuse and bounded squad acquisition. The offline browser fixture passes at 390px and 1440px, including Best XI, draft controls, player selection and a simulated verified save. These checks do not submit a live lineup or deploy the hosted application.

## Hosted migration — 2026-11-07

Applied `20261107191054_lineup_snapshots.sql` to Supabase project `ozhnextrcxxksswpnglt` at the user's request. Migration history records version `20261107191054`, name `lineup_snapshots`; its source SQL matches the committed file exactly (MD5 `60aaa1675b7f92ebc5193d85a286aa48`). A quoting error confined to the stored migration-history text was corrected after comparing it with the source; the applied database objects and their checks were unaffected.

The connector's migration-confirmation flow expired twice without executing or recording a migration, as verified by subsequent database reads. The connector's SQL interface then applied the reviewed source and its migration-history record in one transaction, with five-second lock and sixty-second statement limits.

Live verification confirmed the six-column primary key, enabled RLS, exactly one stale-write trigger, SECURITY INVOKER execution, all four table operations for `service_role`, and no table or trigger-function access for `anon` or `authenticated`. The page constraint accepts `lineup`; the existing preference-row count remained one. Server-role fixtures verified upsert behavior, rejection of older snapshots, independent scoring contexts and saving the Lineup page. Every fixture was rolled back, with zero fixture rows remaining.

The security advisor added only the expected informational RLS-without-client-policies notice for this server-only table; no new warning or error concerned the migration. Existing unrelated warnings were unchanged. The change adds one indexed snapshot table and validates the small page-preference constraint without rewriting application history or introducing recurring requests. No delegation was used for the rollout; runtime regressions were not repeated for this documentation-only update. Application deployment and an authenticated live lineup submission remain separate from this verified database rollout.


## Round-state and Best XI interaction recovery — 2026-11-07

The official client defines round states `closed`, `future`, `next` and `running`. The shared compact and numbered-calendar parsers previously did not recognize `running` as explicitly unfinished. This left an ongoing round unknown and blocked the otherwise verified completed window. Both parsers now recognize the source state; existing database/process calendar rows carrying `running` with an unknown completion flag normalize to unfinished without refetching, rewriting, or extending their original expiry. Earlier postponed or ongoing rounds stay excluded until the source confirms completion. No round number or score is invented.

Best XI now keeps an explicit calculation request while its exact completed window loads and displays the missing schedule/history state. It waits for complete histories for eligible squad players, then generates the XI and bench. Changing five rounds to two updates the active calculation; draft edits, saving, Undo changes, explicit lineup refresh and identity/round changes prevent late acquisition from overwriting user choices. The renamed Undo changes button restores the latest observed or successfully saved Futmondo lineup.

Focused module checks cover a known-answer five-versus-two goalkeeper reversal, queued calendar/history recovery without a second click, cancellation after manual edits/Undo, returning from a wider unavailable window, and restoring the latest verified save, alongside the existing guarded save lifecycle. The browser fixture checks the displayed two-round window, unchanged request counters during local calculation, the Undo changes label, and restoration after a synthetic verified save at mobile and desktop widths. These are offline fixture checks; they do not submit an authenticated live lineup or deploy the application.

The UI observer adds no API/database request, polling, dependency, or durable data path. The existing shared history controller acquires and persists scores. A complete calculation signature avoids repeating the optimizer on unchanged inputs; only explicit requests, changed settings or newly relevant source/score data require recomputation. Existing calendar persistence and absolute expiry remain authoritative.


## Selectable player list and individual removal — 2026-11-07

Clicking a starter or substitute opens a scrollable list of eligible squad players: current bench first, unselected squad players next, and current starters last. Each group is ordered by the existing exact-window average descending, then name and player ID, with unknown averages last within their group. Membership follows the displayed draft, including unsaved swaps. Each button shows the club crest, abbreviated name (full name on hover), positions, source total points and average over the selected N completed rounds. Current selections and injured/suspended players remain labelled. A row click applies the existing role-aware move/swap immediately and closes the picker. Invalid selections retain the draft and show their explanation inside the dialog. Picker identities and account/league/round/formation checks reject obsolete clicks. History arriving while the dialog is open updates its scores through the existing reactive source.

**Remove player** clears only the clicked starter or substitute, without requiring an immediate replacement. Removing the captain also clears the captain selection. Empty positions can be reopened and filled later; the existing incomplete-XI validation disables saving until all eleven starters are selected. These edits remain local until **Save to Futmondo**.

Dialog footer dismissal buttons occupy their own centered row, including the picker's Cancel button with or without Remove player, keeping them clear of the floating Refresh control on narrow screens. The picker help retains the completed-round window but omits the sentence explaining missing-score dashes; unknown scores still render as —. Shared footer CSS changes presentation only, with no additional data requests or persistence changes.

The lineup roster parser now retains the authoritative roster `points` field in the existing database snapshot. Fresh snapshots reuse it across sessions; older snapshots without that field display while a normal live refresh acquires and persists it. Zero and negative totals remain numeric; unknown values display as —. Existing shared history persistence supplies the averages. No migration, dependency, polling or extra per-picker API/database request is introduced. Images reuse the pitch crest URL helper and load lazily with browser URL caching.

Verification: all 13 module groups, four snapshot-store groups and three acquisition/loading groups pass. Checks cover immediate selection, swaps, invalid/obsolete clicks, removal without disturbing other positions, captain clearing, incomplete-save protection, source totals through JSON persistence and fresh-session reuse. The offline browser fixture passes at 390px and 1440px with the displayed crest/name/total/N-average, immediate selection, starter/bench removal and refill, viewport fit and unchanged API/database counters during local edits. Picker opening measured 111 ms and 212 ms respectively in that run. Review confirmed bounded squad-only rendering with one event observer, reusing the existing reactive score calculation. The broader suite and authenticated live submission were deliberately not run for this localized UI change; no hosted deployment is claimed. No delegation was used.

## Picker after optimization and draft membership — 2026-11-09

Optimizing redraws the pitch. Previously, a newly inserted Shiny action button could be visible before its input binding attached; a click during that interval did not reach the server. Player tiles now send an explicit event directly from their click handler, so opening the picker does not depend on rebinding a recreated action button. The same path handles starters and substitutes after optimization and other pitch redraws. Existing picker identity, context, formation and manual-save checks remain in force.

Picker rows identify membership in the current on-screen draft: starting XI or bench position, with the clicked slot marked separately. These labels include unsaved optimized/manual selections and update when the draft changes. They do not substitute the last saved official lineup for the current selection. Refresh reloads official facts without overwriting this draft; Undo changes replaces it with the latest official baseline.

Verification: `Rscript test/test_lineup_module.R` passed all 18 groups. The offline `test/test_lineup_browser.R` fixture plus `node test/test_lineup_browser.cjs` passed at 390 and 1440 px: clicking immediately on optimized DOM insertion opens the popup; current draft labels match starters/bench across edits, removal/refill, repeated optimization, formation redraw, a verified synthetic save and undo. Local edits preserve all API/submission/database counters. Warm picker opening measured 43 ms and 109 ms respectively in this run. Scoped whitespace review passed. Broader lifecycle/scientific suites were deliberately skipped for this localized UI event/presentation change. No authenticated Futmondo action or hosted deployment/testing was performed.

## Compact picker membership icons — 2026-11-09

Picker rows now place a compact icon beside the name instead of the long membership/current-slot text: check-circle for a starter, chair for a substitute, and crosshairs for the clicked slot. Unselected players have no mark. Accessible labels and tooltips retain the full membership and numbered bench position; the current slot's tooltip includes both its slot status and membership. A short shared legend explains the marks and that unsaved selections are included. Membership still derives from the displayed draft. No selection, optimizer, save or acquisition behavior changes.

## Lineup control placement — 2026-11-09

The formation selector sits directly above the pitch, replacing its repeated static formation heading. Optimize and the **Last N rounds** input share a single row, including narrow phones; the input can shrink to fit. The doubtful-player toggle keeps its own full-width row. Input identities, formation entitlements, draft selection and optimization/save behavior are unchanged.


## Horizontal desktop lineups — 2026-11-10

At viewport widths of at least 992px, the shared `fm-lineup-layout` wrapper places the bench on the far left and the pitch beside it. Position groups form vertical columns ordered visually as goalkeeper, defenders, midfielders and forwards; the original slot identities and source order remain intact. The field markings and stripes follow the horizontal orientation. Groups with zero players, such as forwards in 4-6-0, do not reserve a blank column. Below 992px the existing vertical pitch and bench underneath remain unchanged.

Both the editable lineup and `round_lineup_pitch_ui()` use this wrapper. Desktop cards use 48px portraits and content-driven column height so six midfielders and four ordered substitutes stay visible without overlap. This is responsive presentation only: resizing does not fetch, recalculate or save a lineup. Existing acquisition, durable database snapshots, draft validation, player selection and explicit save behavior are unchanged.


## Concise status and observed round schedules — 2026-11-14

The routine departure-exclusion paragraph is removed; the My Team tint, disabled picker entries and save validation still explain or enforce affected selections. A generated lineup shows one short recalculation warning naming only actual changed inputs: Last N, doubtful-player setting, a verified completed-round window, or verified departure eligibility. Changing N is not also reported as a calendar change. Unavailable calendar/transfer evidence stays an unresolved check rather than being presented as a changed optimization result.

`lineup_catalog_problem(catalog, state=NULL, expected_context=NULL, now=Sys.time())` accepts successful complete worker observations for historical completed-round calculations even when their original freshness deadline expires. It preserves source timestamps/deadlines and does not reacquire or repersist observations. Real read/fetch errors, incomplete worker acquisition, invalid observation times and account/league/team/competition/season/scoring mismatches remain rejected. The existing unknown-finality contract remains: known completed rounds can be used when the calendar has no acquisition/conflict reason, even if another round's finality is unknown.

`lineup_Server(..., round_catalog_state=NULL)` receives the root's state-only view of the same shared catalog adapter, retaining read errors and context that its data-only view omits. Both views share one registry entry and source request. Manual action preflights and general freshness checks are unchanged.

`lineup_snapshot_failure(error)` reports only fixed validator labels or sanitized transport categories/references. A failed app snapshot remains visible separately from a successfully verified Futmondo save; private error text, response bodies and credentials are never displayed. Refresh retains the existing retry path. This diagnostic improvement does not establish or claim a fix for the separate reported persistence failure.


## Blank source scoring and durable calendar completeness — 2026-11-15 UTC

The lineup endpoint can return an empty `pointSystem` while returning the full custom scoring configuration. `lineup_resolve_scoring(lineup, context)` resolves only this omitted selector from already-loaded authoritative league metadata. Account, championship, competition, season and scoring version must all be known and match; metadata must be complete, its selector must be `custom`, and the canonical custom-configuration fingerprint must equal both the source settings and the requested scoring version. Explicit source selectors are never replaced. Missing/conflicting evidence still fails. Shared historical point rows can fill unknown scoring scope but cannot overwrite independently known current league identity.

`lineup_api_answer(envelope, worker=FALSE, observed_at=Sys.time())` keeps worker observation time, absolute expiry, acquisition status and completeness when extracting `answer`. Known failed/partial worker reads are rejected. Direct live reads retain the existing 30-second policy. `lineup_bundle_observation(answers, now=Sys.time())` uses the oldest component observation and earliest absolute deadline; missing/invalid worker clocks fail. Bundles retain `source.fetch_status` and `source.worker_replay`, and explicit stale status prevents `lineup_snapshot_fresh()` from certifying freshness. Worker replay does not republish scoring metadata or renew its clock. A direct response with an omitted selector also cannot manufacture a new scoring observation.

Validated bundles use the existing strictly scoped `team_lineup_snapshots` database path. The successful write must be acknowledged; failure remains retryable. A fresh saved bundle is reused in a subsequent session without repeating the four component reads or writing another copy. Stale observations remain stale through persistence. No schema, credential storage, new source request, dependency or polling change is required. Explicit lineup submission still performs its live preflight.

`get_finished_rounds()` now publishes `attr(complete)` alongside `attr(catalog_complete)` so new worker envelopes persist the parser's completeness correctly. For existing `get_finished_rounds` snapshots whose envelope says `unknown`, `fm_worker_restore()` recognizes only saved successful complete-parser proof with valid unique bounded round rows and a nonfuture observation clock. It sets `worker_completeness` to `complete` and records `worker_completeness_source="catalog_complete"` on the restored value. The persisted manifest, original clocks, deadlines and context remain unchanged. Partial/error/malformed observations are not upgraded. This permits immediate reuse of complete saved calendars without a new acquisition or database rewrite.

## Lineup note visibility

The Best XI summary is always visible above the save reminder, with no information-icon disclosure for either. Other ordinary lineup status messages also render inline and retain the polite live region and calculation busy state. Static refresh help is removed; the refresh button keeps its accessible label and tooltip, and load/refresh errors remain available through the existing error disclosures.

Score coverage appears only when a squad player lacks a finite average for the selected completed-round window. Zero and negative averages count as complete. History read and database-save failures remain reported even when all displayed scores are complete, without repeating the full-coverage message. Recovery removes the corresponding warning. This presentation change adds no acquisition, persistence, cache invalidation or timers; the existing database-backed lineup/history reuse and save preflights are unchanged.


## Shared departure prerequisites — 2026-11-16

The reported My Team zero-departure / Lineup refreshing mismatch was reproduced with the actual root-server declarations and worker adapters. The source-time note created the root roster/history registry entries first, binding their lifetime to its own visible pages. Those pages excluded Lineup. Entering Lineup deactivated both entries and reset them to loading even after My Team had reconciled a successful empty result. The inverse creation order could also prevent the note from reading on its own pages. This was a reader-ownership defect, not evidence of a departure or failed background acquisition.

The root now owns one pair of `roster_source_readers`, active while either the source-time note or My Team/Lineup needs the prerequisites. Their refresh signature includes the global refresh and clause-transfer revision. The note keeps its existing visibility rules; the transfer reconciler keeps its narrower activity and bounded summary queue. `incoming_clause_source(..., prerequisite_readers=NULL)` accepts optional named `roster` and `transactions` state-only reader callbacks with the existing factual-adapter argument/result contracts. The root passes its readers explicitly; standalone callers retain the existing default adapters. There is no competing root reader with a different activation or refresh owner.

Successful empty departure results satisfy `clause_departure_round_state()` without inventing zero from missing data. Missing/partial evidence and account/league changes still clear eligibility until matching observations arrive. Existing selection/save gates and draft retention are unchanged. Successful factual observations continue through the existing worker database snapshot/chunk storage and restoration, preserving account/league/team/competition/season/scoring scope, source timestamps, absolute expiry and completeness. GUI reuse neither rewrites successful snapshots nor treats a failed write as durable. No schema, dependency, environment variable, acquisition schedule or new timer is introduced.

Regressions cover all three entry orders (Today, My Team and Lineup), repeated navigation, delayed/partial recovery, league changes, logout/account replacement and metadata-only publications. The browser fixture runs the real Lineup module, root declarations, worker snapshot decoder and page information registry against synthetic persisted observations at 390/1440px. It verifies zero departures, absence of the stale warning, unchanged source times, draft retention and the explicit shared refresh. Details and limitations are in `data_review_results.md`.


## Outfield bench requirement, picker order and status icons — 2026-11-18

**Require full outfield bench coverage** is an enabled checkbox that starts
unchecked. Its tooltip explains that Optimize must leave separate defender,
midfielder and forward substitutes, excluding goalkeeper cover. It can change
the XI or permitted formation to achieve this. If no eligible XI can leave that
cover, or the league disables substitutes, the calculation stops with a specific
message and retains the draft. Three substitutes suffice; a fourth is filled
when another eligible outfield player remains. Manual editing and saving retain
the existing source rules and are not forced to meet this optional optimization
preference.

`lineup_optimize(..., require_bench_coverage = FALSE)` appends the option to its
existing interface. When enabled, its positional dynamic program also tracks an
eight-value coverage mask, assigning three distinct eligible players to reserve
roles without awarding their points to the XI. This finds the highest observed
starting total among feasible XI/bench combinations, preserving the existing
tie rules, allowed formations, multiposition, injury policy and departure
exclusions. The existing bench optimizer then chooses and orders the remaining
substitutes. Off keeps the original objective. No scores are manufactured.

`best_request` and `best_basis` carry the local `coverage` setting. Changing it
cancels pending work, leaves selections intact and warns when a generated lineup
uses different settings. Calculation still requires an explicit Optimize click;
its busy state flushes before the bounded synchronous calculation. Retained
league/round drafts keep their calculation basis.

Both starter and substitute pickers now group the current draft's bench first,
unused squad next and starters last. Each group keeps descending observed
average, name/ID ties and unknown averages last. Unsaved swaps, removals and Undo
changes immediately update membership; eligibility and obsolete-click checks
are unchanged.

`lineup_status_icon(status)` renders the same observed status beside each
occupied pitch/bench portrait and beside the name in picker rows. `doubt`
(and the supported `doubtful` alias) shows a warning triangle, `injured` a minus
circle, `injured2` a hospital, `redcard`/`suspended` a square, and `unavailable`
a barred circle. Tooltips and accessible labels distinguish these meanings.
Empty/`ok` statuses add no icon; unrecognized nonempty codes show **Status
unconfirmed**, never a guessed fitness label. Doubtful players are eligible only when Include doubtful players is checked;
injured, suspended and unavailable players remain excluded in either state. Status changes only redraw the existing
roster projection. Current status is not added to historical round lineups.

The current official public bundle still has SHA-256
`08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c`.
Its `Dy.p` renderer builds `assets/images/player_status/<status>.png` for source
statuses. Existing captured responses corroborate `ok`, `doubt`, `injured`,
`injured2` and `redcard`; no private response bodies or credentials are included
here. `lineup_roster_records()` already persists `status` in the existing
context-scoped database lineup snapshot; hydration reuses it without new reads.
The new icons use the app's existing font icons, adding no image requests.

The requested bottom match-card crests were already implemented in this branch.
Their observed `h.logo`/`a.logo` mapping, durable worker round-trip and legacy
readability were rechecked. Both clubs render beside their names at 320, 390 and
1440px in the offline browser fixture. Public Real Madrid and Barcelona crest
URLs returned HTTP 200 PNGs. No captured authenticated match response was
available, so this does not establish a particular deployed account's logo
coverage or deployment state.

Performance review: the 100-player/all-formation optimizer averaged 41.1 ms with
the option off and 64.0 ms with it on over twenty runs each. The enabled lattice
has at most 3,360 states; the two 100-player predecessor matrices total about
2.6 MiB and are released after the call. Picker sorting remains O(n log n), with
one extra local membership key. Icons have fixed dimensions and picker labels
wrap. There are no new API/database requests, timers, result caches, persistence
formats, dependencies or environment settings. Existing durable histories,
rosters and calendars retain their context and original source clocks. See
[data review](data_review_results.md) for final executed checks and limits.


## Doubtful-only optimization option — 2026-11-18

The former injury/suspension override is replaced by **Include doubtful players**,
unchecked by default. Its tooltip explains that the option can admit only
observed `doubt`/`doubtful` players. Injured (`injured`/`injured2`), suspended
(`redcard`/`suspended`) and unavailable players, including an explicit
`available = FALSE`, are always excluded from both optimized starters and bench.
Verified departure `excluded_ids` remain unconditional exclusions as well.

`lineup_status_eligible(roster, include_doubtful = FALSE)` returns one eligibility
mask for the optimizer and the module's complete-score preflight. This prevents
missing histories for ineligible players from blocking calculation, while
requiring doubtful-player histories when the option is selected. The fifth
`lineup_optimize()` argument and Shiny input are now named `include_doubtful`;
all internal callers use the new name. Changing it keeps the draft, cancels
pending work and uses the existing stale-settings warning until another explicit
Optimize click. There is no automatic save or recalculation.

This is a local selection policy over already persisted roster/status and score
observations. Shared availability facts, history arithmetic, context isolation,
source clocks and durable database reuse are unchanged; no new acquisition,
cache, persistence schema, timer, dependency or environment setting is added.

## Fixture crests from the saved club catalogue — 2026-11-18

The reported match section appeared after the user's page reload. Read-only
inspection then confirmed the remaining crest defect: the current durable
calendar contained 380 matches with all 760 logo fields empty. Its ten next-round
matches had twenty distinct club IDs, all matching nonempty logos in the already
persisted twenty-club `get_real_clubs` snapshot. The former display depended only
on optional `h.logo` / `a.logo`, so it could not render those crests.

`lineup_fixture_view(rounds, target, scope, now, clubs_state = NULL)` now fills
absent home/away logos using exact `home_id` / `away_id` → `teamId` matches.
Direct calendar logos take precedence. `lineup_fixture_clubs(state, scope, now)`
validates account/championship/competition/season, a successful observation,
original observation/deadline clocks and the bounded club frame (2,000 rows).
Duplicate IDs are excluded; names never determine image filenames. Version-1
calendars can use the same saved catalogue. Partial, failed, future-dated or
foreign observations cannot supply crests. Expired successful observations remain
usable with a separate crest source-time/stale note; calendar clocks and
completeness are unchanged.

The module registers the existing `get_real_clubs` factual read adapter only when
selected matches lack logos. Its nonblocking state keeps games visible while the
catalogue arrives and updates their crests without changing the draft. Both
calendar and club facts already persist through worker snapshots; no migration,
new source endpoint, acquisition job, timer, dependency or environment setting is
needed. A cold display adds one small existing catalogue interest to the
coalesced database read; warmed sessions share it. Opening/refreshing the page
never fetches Futmondo data. No extra database write or redundant copy of the
joined facts is needed, and restart hydration retains the original source clocks.

Rechecked the [official public client](https://app.futmondo.com/main.dart.js) on
2026-11-18 (SHA-256
`08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c`).
`qT/axL`, `cwe`, `ces`, `Ij` retain the documented league/matches, round, club ID
and optional logo contracts; `JZ.p` formats a supplied logo beneath the existing
`futmondo/img/teams/64/` CDN path. The saved club catalogue comes from the existing
`POST /1/league/championshipteams` adapter (`query.championshipId`,
`answer[].id/name/logo`). Live saved-observation inspection and pure replay
confirmed twenty of twenty crest mappings; no source authentication/transaction
was performed, and no private bodies were printed or committed. This repairs
the existing crest display, with no new login-tier capability.
