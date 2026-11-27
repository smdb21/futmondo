# Player card

## Compact player details (2026-11-26)

The player card shows the portrait, name, position and club first. A single plain-text line gives the owner and market status, including the asking price for a listed squad player when available. Received offers, active bids and locked clauses use text beside their corresponding actions. In dialogs, Close shares the heading with player details and the information disclosure.

The action row gives its buttons, including **Offer to owner** and **Buy Clause**, a 2 px outline and a tinted background in both themes. On narrow screens each button fills a row, with a minimum height of 44 px. Ownership and availability appear as plain text.

On phones, total points and the last five matches share one row. Market value has its own row with the signed change alongside when space permits. Wider layouts show all three measures across one row. Recent rounds expands in place, followed by value and points history.

## Purchased release clauses — 2026-11-23 Pacific

For a rival player, an explicit `championship.clause.transferred` flag shows
**Release clause purchased**. When `championship.clause.transferDate` is a
valid future instant, the clause status also shows **Transfer scheduled for** with the
date in the account's display timezone. A missing transfer flag and a past
opening leave availability unverified. **Release Clause Locked until** applies
to a future opening. The card reuses its scoped, durable player summary.

The [official Futmondo web bundle](https://app.futmondo.com/main.dart.js),
inspected on 2026-11-23 Pacific (SHA-256
`08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c`),
uses authenticated `POST /1/player/summary` with `championshipId`,
`userteamId`, and `playerId`. Its championship parser passes `clause` to
`cbs`, which reads `date`, `unlockDate`, `transferDate`, and `transferred`
separately. A scoped production snapshot corroborated that the opening was
already past and the purchase had a separate future delivery time. The user
confirmed the same pending transfer in the official app. This corrects the
existing LigaRadar status to reflect a shared capability.


## Manual offer and purchase timing — 2026-11-22 Pacific

Offer actions appear immediately below the player's identity when the selected row has a listing or a rival owner. A newer saved summary can update those facts. Opening the market or direct-owner editor renders the input and applicable premiums in the same modal response. A saved league minimum is used as the suggested market amount when available. Otherwise the editor uses the observed listing price and labels it as such. Saved capacity keeps its original source deadline.

A confirmed ordinary offer carries the editor generation and current amount in one Shiny event. The browser keeps the controls visible and disabled and announces that it is waiting for Futmondo. It animates the spinner unless the browser requests reduced motion. The server sends the existing native offer payload once, with zero Futmondo preflight reads. Manual confirmations use the session’s scheduled purchases as described in [manual dispatch](scheduled_clauses.md#manual-offer-dispatch). A rejection or missing acknowledgement queues the existing scoped factual refresh. A refresh reads results and existing offers. It cannot resend the offer.

Buy Clause opens its confirmation from the card's observed owner and price. Confirmation submits the player and exact price saved when the dialog opened. Waiting for the purchase result uses the same browser status. The chart widget mounts after identity paint and when the card has points or valuations to plot. Existing mounted charts retain their data while refresh is pending.

The dated offer-editor verification sections below describe earlier behavior. The current path is covered by [the executed verification](data_review_results.md).


## Status beside the portrait (2026-11-22)

The identity header overlays a small badge on the portrait's lower corner using
the same `player_status_icon()` mapping as tables and lineup. It uses the selected
source row's `status` and can display it in the first identity paint while
optional history and the separate transaction-safety checks are pending.
Fit and missing statuses have no badge. The portrait remains 58 px on desktop
and 46 px at narrow widths.

## Player fixture and chart dates — 2026-11-19

The card reads player fixtures from the existing durable `get_player_matches`
snapshot and uses the existing league-scoped `get_finished_rounds` snapshot
to resolve questionable context. The [official web bundle](https://app.futmondo.com/main.dart.js)
inspected on 2026-11-19 (SHA-256
`08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c`)
calls `POST /2/player/matches` with `query.playerId` in its standard
authenticated envelope. The `axY`, `cxV`, `b8v`, `k4` and `Ij` functions
read `answer.matches[]` (`id`, `r`, `st`, `h`, `a`, `info.date`) and the
enclosing `answer.league` (`_id` or `id`, `season`). Club objects supply
`name` and `score`. The same bundle's `a9p`/`cxZ` use
`POST /1/player/summary` with championship, user-team and player IDs and
read `answer.points[]` separately. The calendar's matching round and fixture
fields are documented in [the lineup contract](lineup_module.md#source-and-payload).

A saved match response reports `league.season = "2025-2026"` and has five
fixtures dated August–September 2026. The matching player-summary capture
contains numeric round labels and scores for rounds 1–5 without fixture IDs.
The previous join rejected the fixture rows if their inherited season differed
from the selected league's season, leaving their scores without fixture dates.
`normalize_player_matches()` now retains fixture-level season evidence
separately from the enclosing league label. `player_card_fixture_evidence()`
can establish fixture scope, round identity and a missing kickoff through a unique
match ID in the existing saved calendar for the same account, championship,
source competition and season. Explicit fixture-level season, round-ID and
competition conflicts still block repair. Timestamps alone supply no season
identity. The stored source payload and historical rows are preserved.

Recent rounds shows unmatched scores and match records along with one notice
summarizing unresolved fixture/points links and any remaining date-axis
fallback. The chart uses existing point timestamps or dates of associated
fixtures first. An undated score can use the last confirmed kickoff of its
fully dated, completed round, labeled **Round calendar** in the chart hover.
A calendar date supplies no evidence of player participation. Unresolved
dates keep the independent round-number axis. The existing autonomous worker
and database snapshots supply both resources, with their original observation
clocks and scope. The card adds one league-scoped saved-resource interest
and shares the in-process projection across its chart and recent rounds.
There is no new Futmondo request, acquisition job, table, migration or timer.


## Market offer button emphasis (2026-11-19)

The player card's **Make Market Offer** action uses a 2 px border in the theme’s foreground color, a tinted fill and a bold label. The caption can wrap within the card on narrow screens. Hover changes the tint and keyboard navigation uses the shared focus outline.

## Rival ownership for release clause actions (2026-11-19)

`player_card_has_rival_owner()` requires distinct, nonempty owner and manager team IDs. The card uses the existing owner-ID aliases from the table and saved player summary. `player_card_can_buy_clause()` also requires an open clause. The locked label, schedule control and diagnostic control use the same ownership requirement. A listed free agent can still show **Make Market Offer**, even when a saved summary includes a clause price. The clause modal and its submit handler repeat the owner check against the card's latest facts. The live dispatch check still verifies actual ownership before a clause purchase.

## Database-backed card status correction — 2026-11-16

Expired successful database snapshots remain usable observed player facts, with
their original stale status and timestamps. They no longer generate an
availability-retry prompt during browsing. Missing or incomplete saved evidence
stays explicit. See [display and transaction contracts](player_availability.md).

`card_history()` now retains errors from its shared summary, lineup, points and
valuation resource states independently of their freshness. A stale result alone
shows a neutral saved-history/background-observation note; it cannot claim a
refresh failed. Actual read failures retain their safe diagnostic reference and
error severity, including when older data is retained. Points, score finality,
profile-value counts, historical calculations and chart projections are unchanged.
This corrects existing data delivery/status behavior, with no new feature tier.

## Target-only selection correction — 2026-11-15 Pacific

The remaining roughly 32-second report exposed a path missed by the previous
fresh-row tests: a two-minute catalog observation was rejected by the card's
60-second cutoff despite the catalog's five-minute policy, triggering full
financial/transfer-history acquisition on selection. Card selection now verifies
only target ownership/listing. Joined observations carry their independent
original deadlines; stale targets use one scoped summary and update the actual
ownership/listing controls. No roster/funds/transfer scan starts from selection.
Historical/profile reads remain independent. See [availability](player_availability.md).

Successful display checks are retained in a bounded, context-isolated root-session
registry. Their factual source results remain database-backed; the session marker
never extends source deadlines or authorizes a purchase. Opening a market editor
attaches/reuses fresh team evidence or starts one current asynchronous preflight.
Both target and financial checks must complete before Submit can be enabled;
final submission checks are unchanged. The obsolete outer whole-card repair is
removed; native financial source repairs remain bounded inside the engine.

The actual catalog -> price join -> filters -> selection -> modal path is covered,
including two-minute observations, a real 32-second held history browser check,
known captured ownership/asking-price mappings, failures, scope changes and
explicit purchase protection. Hosted authenticated acceptance remains separate.

## Reuse on selection and compact points attribution — 2026-11-15 Pacific

The owner's final request is to reuse recent table ownership/market facts and
successful session checks, without expiry-driven repeat checks on selection.
The current contract is in [interactive availability](player_availability.md).
Recent explicit table observations show the editor controls without starting a
team check or waiting for optional history. Older/unknown table observations use
the scoped provider once; a successful result survives reopening and switching.
Original source clocks/deadlines remain intact. Entering an expired editor and
submitting a purchase still require the existing current preflights; the session
marker does not authorize a write. Explicit Refresh and affected-context/action
invalidation remain available. Real initial failures keep their diagnostics.

The points disclosure now says **Points are reported by Futmondo. Final
confirmation is not provided for some rounds.** It omits **About these points**
and passes `source=""` to suppress the redundant **Player history** label in the
consolidated dialog information panel. Other source headings and unknown point
finality remain intact. The disclosure registry treats an explicitly empty source
as an instruction to omit its label, with no new listener, timer or request.
This corrects existing behavior/presentation, without a new feature-tier entry.


## Successful saved availability and completeness — 2026-11-15 Pacific

Opening a rival player could report **Availability checks expired before they completed** even though the saved check had completed successfully. The worker collector records `attr(value, "complete")`, but capacity calculations did not publish it. Both inspected stored capacity envelopes had `status=ok`, successful roster/funds/commitments, no diagnostics and an original 15-second action window, yet envelope completeness was `unknown`. The card's ordinary-expiry recovery path required proven completeness, so these valid historical results appeared as failed acquisitions.

`acquisition_capacity_observation_complete(capacity)` validates the existing parser contract: successful overall/transfer status, finite deadline, consistent nonnegative integer roster/transfer counts, explicit unique disjoint transfer ID sets, all required finite cash/debt/spendable/API-limit fields, verified outstanding count/amount, and no source failure/diagnostics. Reported negative cash/API limits remain facts, not unknown/zero. `calculate_team_acquisition_capacity()` now publishes this boolean as `attr(result, "complete")` for the existing collector and database persistence path. Completeness remains independent of freshness and transaction eligibility.

`fm_worker_restore()` also recognizes legacy **capacity only** envelopes with unknown completeness when the saved contract passes, the envelope acquisition succeeded, and its source timestamp is valid/nonfuture. It sets `worker_completeness=complete` and `worker_completeness_source=capacity_contract`; failed/partial/future or malformed evidence is never promoted. Existing immutable manifest metadata is retained and restored observations are not rewritten. The original source timestamps, payload deadline and more restrictive envelope deadline are preserved. Durable database observations remain the source of reuse across sessions/restarts; no new cache, schema or acquisition is introduced.

Restored successful results retain their proven completeness while expired observations still block purchases. Following the owner's 2026-11-15 request, opening the card automatically runs the existing availability recovery when needed, as described below. Failed/partial acquisitions keep diagnostics; verification and independent submission preflights retain freshness checks, context isolation and duplicate/cancellation protection. Opening never submits a transaction.


The selected-player module displays actual player identity, current values and availability, profile/match data, price history, and round points. It preserves explicit manual bid, sale, offer, and release-clause handlers with their current-state checks.

## Shared history

Chart and recent-points consumers use one shared reactive history result. Round scores are reconciled within player, championship, season, scoring version, and round context. A partial stored result must not suppress available API rounds. The current source/revision contract is implemented in the shared data layer.

Zero and negative scores are valid. A known round does not need a timestamp to be plotted by round number. Provisional/unknown finality remains visible as such; unavailable scores and participation do not become zero. Conflicting or unknown season contexts cannot silently merge with a known season.

Cumulative price-snapshot totals never become per-round scores. Price charts use actual observed values only; no synthetic trend or default historical value fills gaps.

The history chart displays both series together: solid green valuation on the left EUR scale and solid orange round points on the right scale. The coloured axes identify the series without a legend. The horizontal axis uses 10px labels on the 1st and 15th of each month, rotated 90 degrees and formatted as day plus abbreviated month (for example, 01 Nov). Tick positions follow the displayed account-local calendar across both dated series, so month lengths, February and year boundaries do not shift them. Undated round-axis labels also use 10px text. Explicit title spacing and a larger bottom margin prevent the vertical labels from colliding with Date on narrow phone plots. Vertical date grid lines remain hidden; exact dates, timestamps and values remain in hover details. Each vertical scale keeps its own padded range. A shared date axis uses saved point dates, the last associated player fixture date, or a confirmed completed-round date from the synced calendar. If any score remains undated, every score is preserved on a separate top round axis with an explicit notice that it is not aligned with valuation dates. Dated scores connect in chronological order, with round number as a stable tie-breaker for shared dates; postponed rounds therefore cannot draw backward across the date axis. Scores and hover provenance move together. Round axes also use sparse, whole-number labels. Points alone use round numbers when undated. Missing series retain their availability explanation.

This presentation reuses the card's shared history and already requested match resource. It adds no requests, database reads, polling or cache entries; match completion can redraw the chart once to align its dates. Focused verification: `Rscript test/test_player_trend_plot.R`.

In light mode, the valuation line and left-axis labels/title use dark green
`#15803D` for contrast against white. Dark mode retains `#00ff88`; points remain
orange. The chart's `meta.fm_player_valuation` flag lets the existing browser
theme adapter update the valuation trace and its axis together on initial render
and theme changes, preserving history, hover details and zoom. This is a palette
adjustment without a new feature or data acquisition.

The chart waits for the current identity's browser paint acknowledgement. A one-time `shiny:value` check could run before asynchronous identity HTML insertion, or acknowledge cached HTML from an earlier card generation, leaving **Loading chart…** indefinitely. The listener now observes only the identity output, waits two animation frames for each new player/generation, rejects replaced HTML, and disposes the previous observer on remount. Duplicate updates send no additional acknowledgements. This adds no API requests, database reads, cache entries, polling, or whole-page mutation observer. Existing database-backed history retrieval and reuse are unchanged. `node test/test_player_chart_paint.cjs` reproduces delayed insertion, context changes and reopening at desktop/mobile widths; progressive-loading and trend-plot checks cover the server gate, history reuse and score rendering. De Jong's live summary and hosted deployment still require verification.

## Interaction

Reopening the same fresh player shares cached resources. Refresh updates the card resources; changing account/league/player rejects old results and clears pending confirmations. Notification, market, roster, and catalog entry paths use the same card behavior.

`selected_player_UI(id)` creates the view and `selected_player_Server()` binds the supplied reactive player/context inputs. The optional `refresh_trigger` reactive integrates the application Refresh; the card also has a dedicated **Refresh history** button. The shared repository API and limits are documented in [data service](data_service.md).

Focused checks cover history reconciliation, date-free round scores, partial sources, notification entry, and manual transaction preflights.

## Acquisition availability (2026-11-01)

The non-owned player card now shows one **Checking availability…** status while its team checks run. A partial check identifies the missing prerequisite in plain English (roster, league limit, current cash, reserved offer funds, pending clauses or outgoing offers); it no longer exposes only `status: partial`. These messages do not relax the checks or hide missing expected data. The app's existing Refresh evicts capacity as well as its source caches, so a recently cached failed check can actually recover immediately.

Capacity is a team resource, independent of the selected player. `get_team_acquisition_capacity(login, championship_id, user_team_id)` caches the shared roster/funds/commitment calculation and a small internal market-bid projection. The existing `get_acquisition_capacity(..., target_player_id=NULL)` interface performs a local target lookup and removes the internal projection before returning. Switching between two other players preserves an already-running or completed team check; account/league/team changes still invalidate it. Market-offer opening locally validates the shared current facts or waits asynchronously as described below; submission independently calls the existing preflight helper. The passive display cannot authorize a submission.

The roster loader accepts optional `facts_only=FALSE`. Capacity requests only verified player IDs/values, avoiding club enrichment, incoming roster offers and listing-presentation requests. Full and minimal results have separate caches and share the same raw roster payload; either order can reuse the one roster request. Original observation times bound derived-cache validity to 30 seconds. A capacity snapshot inherits the earliest source expiry and an absolute maximum of 15 seconds; parsing it again does not extend the source observation. The pure preflight accepts optional `now` for deterministic expiry checks and rejects an expired snapshot even if fetching consumed its lifetime. Unknown/stale/partial amounts or roster identities remain blocked.

Pending clause reconciliation shares one latest-event scan. It parses full ISO timestamps including fractions/time zones and treats missing counterparties as System. This fixes phantom missing-counterparty rows and selecting an older same-day trade from a newest-first feed, which previously created false pending candidates and unnecessary individual summary requests. Unrelated players' malformed dates do not enter this team's scan; unverified relevant dates or summaries remain explicit defects. Saved HAR replay succeeds through the real asynchronous adapter. The screenshot's particular partial prerequisite has not been reproduced from those captures; hosted runtime verification remains necessary before claiming that live incident resolved.


## Opening from Players while history saves (2026-11-05)

The card's availability sources do not depend on persistence acknowledgements. The Players table previously rescanned its full saved matrix and rebuilt resolved fitness on each save completion; this synchronous work could delay the card's asynchronous callbacks and rendering. The shared round controller now reuses valid coverage projections and only rechecks affected fitness. `test_player_card_during_saves.R` exercises the actual table-selection/module wiring with a synthetic Berenguer row and seven held saves: availability dispatches and verified controls render before those saves finish, then the counter settles without another card read. Manual-action source validation and freshness deadlines are unchanged.


## Availability completion after slow sources (2026-11-05)

The asynchronous card path now calls `get_team_acquisition_capacity_async(login, championship_id, user_team_id, context, is_current, on_progress, priority, owner)`. It starts team information, roster facts, complete transaction history and market commitments together. Candidate summaries use two concurrent slots and at most 32 unique player IDs. Each underlying adapter has its own source clock. Before calculating the result, one bounded freshness pass rereads only observations with at most two seconds remaining, using the original 15/30/300/20-second source deadlines. Candidate IDs are recomputed after refreshed roster/history; obsolete summary failures are removed and new arrivals are verified. Failed sources retain their diagnostics and do not get another freshness retry.

`calculate_team_acquisition_capacity(info, roster_df, pressroom, market, summaries, user_team_id, now)` is a pure shared calculation used by both asynchronous display and synchronous manual preflights. The payload and arithmetic are unchanged; `valid_until` still inherits the earliest original source expiry, capped at now plus 15 seconds. A still-expired asynchronous result remains partial. A valid complete cached team calculation makes no additional reads.

The card has one event-driven capacity recheck per selection/context/Refresh if the player summary finishes after team facts expire. It has no polling timer and never changes old timestamps. Reopening an expired card refreshes even inside the outer resource's 20-second TTL. Acquisition buttons and the listed-player direct-sale button pass Boolean `disabled` values to Shiny; failed gates now produce genuinely disabled controls. Server-side manual preflights still independently verify every action. See [request scheduling](transport_priority.md) for queue behavior and [review results](data_review_results.md) for exact fixtures and deployment limits.

## Responsive market offers (2026-11-08)

**Make Market Offer** previously called the synchronous team-capacity loader before sending its dialog, so expired availability could hold the click while roster, balance, history and market requests completed. The button and external `market_bid` action now evaluate the already loaded card capacity locally. A verified result with a future numeric freshness deadline opens the amount editor without another source or database read. Missing, expired or pending facts open a cancellable **Checking availability…** dialog immediately with submission disabled. An existing pending card read is shared; otherwise the same card resource refreshes once. Completion updates the mounted body/footer instead of replacing or reopening the modal. Unavailable, stale, partial and undated results cannot enable submission. The configured league/listing minimum and the independent current-state submission preflight remain enforced.

Close events carry the opening generation, and pending completion checks its player/account/league/team context. Closing, switching context or receiving an old close event cannot authorize or reopen another offer. No new cache, persistence layer, polling, package or worker is introduced; existing historical database storage is unchanged. Focused verification: `Rscript test/test_market_offer_opening.R` (14 checks), `Rscript test/test_configured_acquisition_rules.R`, and existing offer controls. With capacity deliberately held, the server test produced its shell in 34 ms. A separate actual-browser fixture held the refresh for six seconds: the modal appeared in 379 ms at desktop width and 382 ms at 390 px, responded to a heartbeat during the wait, and stayed closed after cancellation and late completion. These are offline fixture measurements; the hosted incident's exact latency has not been measured after deployment.

The broader `test_deterministic_fixes.R` run reported 29 passes and one failure, reproduced with the unchanged HEAD player module: `capacity_own_market_bid_is_counted_once_and_sealed_amounts_remain_unknown` expects a target-bid amount from duplicate market rows, while the unchanged capacity loader declines that ambiguous target projection. This existing failure remains outside the dialog-opening change.


## Explicit recovery of expired worker availability (2026-11-15 UTC)

The initial click-to-verify behavior described in this section is superseded by **Automatic availability on opening** below; its factual read, persistence and purchase-safety contracts remain in use.

Worker capacity observations are collected every 300 seconds, while their original transaction deadline remains at most 15 seconds. A saved result therefore usually cannot authorize a purchase. The passive card previously disabled every acquisition button at that deadline, leaving the existing live modal preflight unreachable; ordinary Refresh could only reread the same database observation.

Non-owned cards now expose **Verify availability** when worker capacity is missing, loading, expired or otherwise unverified. This is an explicit factual read, not a purchase. It calls the existing asynchronous `get_team_acquisition_capacity` adapter with `worker_bypass=TRUE`, propagating the live check to its prerequisites. Repeated clicks while pending share one check. The control stays reachable outside the ordinary purchase-readiness gate; market, owner-offer and clause actions still require verified capacity and their existing player-summary prerequisites.

`stored_capacity` retains the database-backed resource. `capacity_verification_RV` holds only the current explicit check and `card_states$capacity` projects its pending/result state for the current account, league, team, player, season, scoring context, refresh revision and worker mode. Older database replays cannot replace newer live evidence; a genuinely newer observation can. Context changes, card closure, Refresh, sync-mode changes and session end invalidate late completions. Missing or malformed deadlines are schema failures; partial, expired and failed checks keep purchases blocked and allow another explicit verification. Only ordinary aging of a previously successful stored result uses a neutral explanation; source failures retain their error details.

The existing acquisition adapters retain source timestamps, bounded cache reuse and persistence hooks. Worker observations and factual history remain durably stored in the database and reused with their existing context/completeness metadata. This UI projection adds no durable cache and does not extend a source deadline or treat an unacknowledged write as persisted. Submission still performs the independent live `get_acquisition_capacity` preflight; verification never sends a transaction. No passive source acquisition, recurring timer, worker cadence change, dependency or schema change is introduced.

Unlisted rival offers use the `offer` preflight mode rather than a market `bid` minimum. This permits a verified owner-offer editor without inventing a listing price; amount and funds checks still apply at submission.


## Live availability source ordering and context repair (2026-11-15 UTC)

The follow-up after explicit Verify exposed a second failure in the actual acquisition engine. A refreshed market response or a newly discovered transfer summary could finish after the refreshed 15-second funds observation expired. `get_team_acquisition_capacity_async()` now keeps its initial four parallel prerequisite reads, then repairs longer-lived roster/history facts and verifies changed transfer candidates before acquiring expiring market/funds facts. It checks all original source deadlines at settlement and permits one final repair cycle for successful observations that expired while a sibling request was pending. At most two repair cycles follow the initial pass; only observations within two seconds of their original expiry are reread. Null, failed, partial or stale fallback results retain their failure evidence and do not start a new freshness retry. A still-expired final result remains blocked.

The source observation clocks and the 15/30/300/20/60-second funds/roster/history/market/summary deadlines are unchanged. For the actual card fixture with 31 seconds of delayed history and 20 seconds of delayed market retrieval, source funds observations remain at 0/31/51 seconds and the final deadline is 61 seconds, limited by the roster. It does not become a fabricated new 15-second lifetime. A new pending transfer discovered after roster refresh is checked before final funds and remains in the effective roster count.

Live bypass also retains the captured account/league/competition/season/scoring identity for implicitly scoped nested reads. Previously bypass disabled worker mode and lost expired-but-known identity, so a durable historical baseline could be queried under `unknown` instead of its original scope. The adapter now fills missing identity from its caller only within the matching account/league, preserves explicit archive/team/player scope, and rejects conflicting current metadata. `resource_context()` retains observed identity during the existing explicit `futmondo.worker_live` submission scope, so the independent synchronous preflight can reuse the exact just-verified capacity instead of starting a second cold scan. Source freshness checks remain separate and strict. See [transport context](transport_priority.md) and the exact verification record in [review results](data_review_results.md).

The top-level asynchronous capacity cache also uses the supplied known scope and rejects conflicting current competition/season/scoring identity before lookup; it cannot return another context’s otherwise fresh cached balance.


## Automatic availability on opening — 2026-11-15

Opening a non-owned player now starts the existing live availability check
automatically when Background sync's saved capacity is missing, pending, stale,
expired or incomplete. A valid scoped database observation is reused first; the
card also checks the already decoded worker snapshot if its reactive replay is
still queued. Fresh successful team calculations reuse the existing native cache
without repeating source requests. The ordinary non-worker path is unchanged.

`verify_card_availability()` is the shared module-local entry for automatic
opening and the retained retry input. It uses the same asynchronous
`get_team_acquisition_capacity` adapter with `worker_bypass=TRUE`; duplicate
inputs share an in-flight verification. Purchase controls show checking until
verified facts arrive. **Retry availability** is available after a genuine
failure or a later expiry; an initial verification click is no longer required.
Partial/error/expired/undated results remain blocked with their diagnostics.

One cancellable 10ms deferral gives immediate stored replay its first flush.
The check is latched to account/league/team/player/season/scoring, refresh/history
revision and worker mode. Identical context redraws cannot reset the result or
launch more work. Closing, context changes and session end cancel the deferral
and invalidate late completions. Each new opening or explicit refresh may check
once; no recurring timer or source polling is added. Owned players skip the
acquisition check.

This changes interactive loading only. It adds no transaction, background job,
worker consent/cadence, durable cache, database schema, dependency or environment
variable. Existing durable worker observations and acquisition persistence hooks
remain in use with their original context, completeness and source deadlines;
session memory is only the current card's pending/result projection. Independent
manual-action submission preflights remain required.

Updated worker recovery, native capacity, legacy completeness and mobile/desktop
browser fixtures cover automatic opening, fresh stored/native reuse, delayed
sources, retry safety, redraws, context changes and missing worker publications.
See `data_review_results.md` for measured request counts and verification limits.


## Phone chart dates and series-colored axes — 2026-11-15

`player_trend_plot` requests three automatic date ticks and uses
`%d %b` with 90-degree date labels, omitting the year from axis labels at the
owner's request. Full dates, including years, remain in hover details. Date-title
standoff is 20px and the bottom margin is 95px, retaining automatic margins. It
omits date `dtick` and retains padded independent
value/points ranges and full hover provenance. Numeric undated-round axes keep
their integral spacing and never supply invented dates.

Valuation tick labels and the Value (EUR) title use the valuation line's green;
Points ticks/title use the dashed points line's amber. The independent top Round
axis, when needed for undated scores, also uses amber. The shared R plot adapter
and browser theme updater now respect declared series-axis colors instead of
replacing them with generic muted/text colors. Uncolored axes keep normal theme
defaults. No new resize handler, timer, listener, data request or cache is added.

`test/test_player_trend_plot.R` verifies layout, signed/zero scores, all source
observations, dates, chronology and neutral theme defaults.
`test/test_player_trend_browser.R` and its `.cjs` runner render the actual card
plot expression with synthetic combined, valuation-only, dated-points-only and
undated-score histories. They verify tick geometry, rendered colors, theme/zoom
retention and phone/desktop overflow; no authenticated collection is used.

## Shared availability while browsing — 2026-11-15

The former per-player automatic verification is now a shared root-session team
provider. Switching cards joins its pending check or reuses its fresh result;
chart-history refresh does not invalidate team capacity. Target summaries have
an independent live fallback when a worker observation is missing/expired.
Own-team listing availability follows the same shared fallback. Retry reacquires
only failed/expired sources; valid worker snapshots avoid redundant encoding.
New snapshot preparation follows the ready-output flush and yields during JSON
emission. Offer editors join the existing verification instead of launching another one.
Successful team/summary facts are stored and restored through the existing
private evidence store with original context, completeness and deadlines.
Manual confirmation generations and independent submission preflights remain
unchanged. Full interfaces, bounds, payloads, invalidation and timing stages:
[player availability](player_availability.md). Executed verification and limits:
[data review results](data_review_results.md).


## Full source valuation history — 2026-11-15 Pacific

The combined chart now passes the already requested full-profile response into
`assemble_player_card_history(..., profile=NULL)` and
`normalize_card_valuation(stored, summary, profile=NULL, context=NULL)`. A captured
native summary has seven price dates, whereas its full profile has 71. Previously
only the summary and current-price observations reached the chart. The profile
panel's latest value did not imply that its older prices were charted.

`player_valuation_history.R` owns the longer curve's normalization and durable
reuse. `player_valuation_profile()` whitelists player identity, source competition
and season, dated social/classic values, per-row source observation clocks and
returned-row completeness. It rejects contradictory identities, invalid source
clocks and more than 5,000 rows. Completeness describes the returned source rows,
not proof that every day of the season exists. Invalid rows remain partial;
missing prices never become zero.

`player_profile_valuation()` merges saved and current source rows by exact source
date and observation clock. It verifies social/classic selection against every
finite overlapping summary price, using direct values or the source's explicit
positive divider. It never fits an arbitrary ratio. Contradictory/missing context,
no overlap, a mismatch or differing historical curves with an ambiguous mode
leave the longer curve unmerged and retain a specific diagnostic. Exact source
values and dates are preserved; current summary prices take precedence on shared
dates. Points and scoring provenance are unchanged.

`persist_player_valuation_profile()` records successful already-fetched price
curves in the existing server-only `rival_finance_evidence` table, under an
`eligibility` payload with `kind=player_valuation_profile`, `format=1`, `scope` and
`profile`. The reserved `rival-cache:player-valuation:v1:<hash>` source ID excludes
these snapshots from financial evidence queries. Scope contains account, fantasy
league, team, player, competition, season and scoring version. The profile keeps
original source dates, observation clocks, mode columns, freshness and partial
metadata. No token, header or authentication state is stored. Shorter new
responses retain saved older prices and their original observation clocks.

The existing bounded asynchronous persistence queue uses an immutable source-time
conflict key and ignore-on-conflict. Only acknowledged writes are reusable as
database results; failed/unacknowledged writes remain retryable. Payloads are
limited to 220 KiB. `read_player_valuation_profile()` makes one scoped, limit-one
lookup in the existing latest-evidence view. `read_card_price_history()` attaches
it as `attr(result, "valuation_profile")`, preserving its existing current-price
baseline and completeness metadata. The card's read now uses resolved history
context. Saved curves render before the current profile arrives and survive
memory resets. The independent profile resource still acquires current profile
facts under the existing source/worker policy. No separate price-history endpoint
is requested; explicit Refresh now includes the existing full-profile read.

**Refresh history** now invalidates both the full-profile inner cache and its
outer card resource, including obsolete pending reads from a prior opening, and
advances the profile's refresh revision. Profile history no longer waits for
purchase availability. Refresh retains account/player isolation, and closing the
card rejects late profile completion. In Background sync mode the existing
worker provider continues to replay durable source observations; no new worker
job, cadence, purchase or bypass is added.

Dani Lorenzo's hosted metadata showed 627 current-price observations dating back
to August 16, all with unknown season/competition, plus a successful full-profile
worker snapshot. These old observations are not silently assigned to today's
season. The longer authoritative profile is used instead when its identities
and price mapping pass. An initial automatic-review export rejection was resolved
by the owner's explicit approval. Read-only replay of his saved full profile now
verifies 133 daily prices from 2026-07-06 through 2026-11-15, all seven matching
summary dates, and matching source player/competition/season. The actual Plotly
expression retains all 133 dates/prices and the latest value, 10,027,832 EUR.
The normalized snapshot is complete for those returned rows and occupies 17,322
JSON bytes, below the 220-KiB bound. This was local replay of saved source data;
the deployed browser chart and production snapshot-save/readback remain
unverified. See the dated review results.


## Dani Lorenzo's omitted owner object — 2026-11-15 Pacific

Read-only inspection of the actual availability snapshot observed at
2026-11-16 04:19:12 UTC confirmed a successful matching player summary with
`data.computer=TRUE`, a published `market.p`, and no `championship.owner` field.
The previous projection returned NULL despite these explicit source facts, so
the card displayed Retry after successful acquisition. The projection now accepts
the explicit data-level System flag when the owner object is absent. It preserves
the exact asking price and source clocks; missing ownership alone is never proof
of a free agent. Existing durable results are reused through the same decoder and
context/deadline gates. No persistence schema, eligibility clock, financial check,
background cadence or manual submission policy changes.

The actual saved source passes the corrected projection with zero source/database
requests during replay. A minimized synthetic form covers missing/false/unknown
flags, mismatching IDs and real table -> modal readiness on phone/desktop. Full
private payloads and authentication material are not recorded in fixtures/docs.

## Stable player trend chart — 2026-11-16 Pacific

The chart remains mounted when the selected player's surrounding record changes but its account, league, team and player context stays the same. A chart-specific reactive value publishes history and match inputs only when they differ; an empty loading state retains the previous curve until a replacement arrives. Context changes clear it, and actual history changes and explicit refresh still update it. The valuation trace is a line without markers; points use a solid orange line. Existing scoped database history, persistence, freshness and acquisition policies are unchanged.
