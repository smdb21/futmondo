# Futmondo data review and implementation results

## Welcome transfer loading label (2026-11-27)

The offline Home fixture checks the entire Transfers value while the history read is pending and expects exactly “Loading…”. Chromium showed exactly that text at 320 and 1440 px, then showed zero incoming and zero leaving. The change renders fewer elements while loading and adds no requests, timers or reactive dependencies.

## Unresponsive offer submission (2026-11-26)

Public reads against ligaradar.com returned HTTP 404 for `/fm-offer-dialog-2/offer-dialog.js` and served the expected JavaScript at `/offer-dialog.js`. In offline Chromium, reproducing the 404 left Submit Offer enabled with an empty status after a click. Separately, the server fixture reproduced a lost confirmation when a league metadata revision left the selected player and all context IDs unchanged.

The corrected browser path loaded the script once per page at 320, 390 and 1440 px. Market and rival offers showed the pending state immediately and displayed the delayed simulated rejection or acceptance. The shared clause dialog also completed. The server test refreshed metadata before and during submission, dispatched exactly one request with the confirmed amount, and rejected a stale confirmation after switching players. The existing offer control checks and the offline Shiny lifecycle passed, including 14 lifecycle checks and eight tabs. Opening the cold editor took 20 ms in the server fixture and added zero source or database reads. The fix adds no polling or retries. All transaction checks used synthetic accounts. [Browser check](../test/test_player_offer_browser.cjs) and [server check](../test/test_market_offer_opening.R)

## Market bid editor (2026-11-26)

Offline Chromium verified five buttons on one row at 320, 390 and 1440 px in light and dark themes. On a €150 listing, two +1% taps showed €152 (+1.33%) and €153 (+2%). Typing €180 immediately showed +20%, and +10% then showed €195 (+30%). The checks covered below-listing and below-valuation messages, clearing the field, cancellation and reopening. The longest observed Playwright click and preview check was 49 ms across these six runs.

The browser formats the total and percentage locally from the dialog’s existing price and valuation, replacing the server-rendered market bid preview. Editing adds no API calls, database reads, timers or stored observations. The offline opening test measured 19 ms with card reads held. Synthetic new-offer and existing-bid tests verified single submission and context checks. The Shiny lifecycle passed 14 checks and eight tabs. Production UI verification remains open.

## Incoming transfers on Home (2026-11-26)

The offline Home fixture held the transfer history response open. The incoming row showed “Loading…” while the squad size was visible, then showed one incoming player when the response arrived. A completed empty history showed zero. The fixture used one roster read and one transfer history read. Ten unrelated inputs added no reads. The label reuses the existing reactive display and adds no timer, database work or source request.

The deployed server and transfer module fingerprints matched the initial fix when the remaining dashes were reported. The first fix covered pending transfer history only on the incoming side. In the saved-data fixture, the roster and history were ready before the player summaries. The transfer source had already published a partial result during that database read.

The follow-up checks held one batched database read for both players, returned only a verified departure, then delivered the arrival on the next read. Each direction settled independently with zero Futmondo source requests. Home also showed visible “Unavailable” text for both directions in an account without a playable league. The database reader tracks the first completed lookup on the existing bounded descriptors and uses per-resource notifications. The change adds no requests or timers. Offline Chromium showed both loading labels at 320 and 1440 px and both zero counts after the held history read completed. The Shiny lifecycle passed 14 checks and eight tabs.

## Points from a selected round (2026-11-26)

The chart previously filtered season totals after accumulation. In the offline browser fixture, choosing round 6.2 showed 0 before the round, 5 after it and 2 after the next round’s −3. A single-round interval showed both 0 and 5. Rank switching and restoring the season view passed in both themes at 320, 390 and 1440 px, with zero additional saved reads or horizontal overflow.

The plot module suite passed 13 checks. The Shiny loading fixture verified a negative starting score, a missing score and reuse of existing reads. Known-answer fixtures also covered an earlier missing score and confirmed nonparticipation. The interval calculation runs over the visible team-round rows and adds one plot marker per team. Verification used synthetic saved observations. [Plot behavior](plots_module.md#points-and-ranks-2026-11-26)

## Compact lineup controls (2026-11-26)

In the offline editor fixture, the distance from the Lineup heading to the pitch fell from 674 to 354 CSS pixels at a 320 px viewport, from 576 to 354 at 390 px and from 546 to 261 at 1440 px. The four draft actions fit on one row in both themes at all three widths. Keyboard checks covered opening Player options and reaching the first player with a visible focus outline. Selecting Current round hid the editor controls and selecting Upcoming lineup restored them. Expanding and collapsing the options preserved the fixture read and write counts. The native disclosure and responsive styles add no server observers, timers or data requests.

The editor and scheduled-save browser fixtures exercised formation edits, player selection, optimization, save confirmation and cancellation. The lineup module suite and offline Shiny lifecycle passed (14 lifecycle checks and eight tabs). Verification used synthetic data.

## Market offer cancellation (2026-11-26)

The previous card lost its controls after the capacity observation's 20-second own-bid deadline, including when the persisted market still showed the offer. A 524-player offline fixture placed a market offer and rendered Cancel Bid on acknowledgement, before the worker published its native bid ID. The card continued to show cancellation after the updated market and balance arrived. A separate card reopened against a 90-second-old saved market, with expired capacity and no usable player summary.

The transport fixture ran the production market parser and cancellation request builder. Display made zero additional source reads. Opening the confirmation and submitting it each made one asynchronous market read. The event loop stayed responsive while verification was pending. Duplicate clicks and a lost acknowledgement produced one withdrawal request in total. A replacement bid, duplicate bid ID, stale live verification and team changes prevented dispatch. The market state and receipt reuse the existing team-scoped sources without new polling or database reads on card opening.

The existing-offer, manual finance, withdrawal transport and UI reliability checks passed offline. The Shiny lifecycle passed 14 checks and eight tabs. Production withdrawal remains unverified. [Source contract](bid_management.md#market-offer-cancellation-2026-11-26)

## Listing deadlines and Home market counts (2026-11-26)

The published expiry was present in all 163 market rows and 32 own-listing rows in the recorded market capture. The My Team roster projection previously kept the asking price but dropped the listing expiry. The shared player table now sorts by the source instant and formats it in the selected account timezone. A known-answer fixture verified a deadline crossing local midnight, withdrawal of the last listing, a changed deadline after relisting, protection against older observations, and the worker snapshot encode/decode round trip.

The received-offer and squad-table suites passed 11 and eight checks respectively. Player availability passed 11 checks, durable team offers passed eight, and the saved-offer observation and five captured response-contract checks passed. The four Home fixtures passed with zero external HTTP or database mutations. Ten unrelated inputs caused zero additional reads. The full Shiny lifecycle passed 14 checks plus eight tabs.

Offline Chromium at 390 and 1440 px showed three listed players and two distinct players with offers, including a direct offer on an unlisted player. Both tables rendered three matching localized expiry cells. Home fit both viewports. The feature uses existing roster, own-listing and market reads. The worker stores one expiry per listed player and a listing observation timestamp alongside roster data. Rendering parses each distinct expiry once per table projection and adds no timers or per-player reads. Production rendering remains unverified. [Implementation contract](players_in_teams_module.md#listing-deadlines-and-home-market-counts-2026-11-26)

## Received offer decisions (2026-11-26)

The reported rejection exposed a routing defect. LigaRadar sent direct roster offers to the listing-bid endpoint. Both accept and reject now verify the selected offer and use its source collection. The old path could also replace an unidentified bid with the first bid on a player. [Request contract and evidence](market_selling.md#e-accepting-and-rejecting-received-offers-2026-11-26)

`test/test_received_offer_decisions.R` passed all four route fixtures and exercised the actual card with a delayed summary, duplicate confirmation, changed price/party/ID, duplicate ID, context switch, explicit rejection, confirmed success and a lost acknowledgement. A normal confirmation makes one asynchronous player-summary read and one single-attempt decision request. Opening the dialog made zero requests. Another event-loop callback completed with the summary still pending. The existing worker refresh reuses the scoped database observation path after decisions. No schema, polling, or dependencies changed.

My Team offer checks passed (10), durable offer persistence checks passed (8), and the offline Shiny lifecycle passed (14 checks and eight tabs). No live sale or rejection was attempted. Production deployment and the result for the reported offer remain unverified.

## Received offer decisions (2026-11-26)

The reported rejection exposed a routing defect. LigaRadar sent direct roster offers to the listing-bid endpoint. Both accept and reject now verify the selected offer and use its source collection. The old path could also replace an unidentified bid with the first bid on a player. [Request contract and evidence](market_selling.md#e-accepting-and-rejecting-received-offers-2026-11-26)

`test/test_received_offer_decisions.R` passed all four route fixtures and exercised the actual card with a delayed summary, duplicate confirmation, changed price/party/ID, duplicate ID, context switch, explicit rejection, confirmed success and a lost acknowledgement. A normal confirmation makes one asynchronous player-summary read and one single-attempt decision request. Opening the dialog made zero requests. Another event-loop callback completed with the summary still pending. The existing worker refresh reuses the scoped database observation path after decisions. No schema, polling, or dependencies changed.

My Team offer checks passed (10), durable offer persistence checks passed (8), and the offline Shiny lifecycle passed (14 checks and eight tabs). No live sale or rejection was attempted. Production deployment and the result for the reported offer remain unverified.

## Round schedule wording (2026-11-26)

The Home browser fixture uses a stale saved calendar. Chromium displayed the closed round, the next round’s kickoff with its timezone, a separate “Time until kickoff” row with duration units, and no freshness notice. The rows fit at 320, 390 and 1440 px. Returning Home reused the overview reads at each width. The fresh calendar and unavailable calendar checks also passed.

The labels reuse the existing calendar read and browser clock. Ten unrelated inputs caused zero additional reads. The change adds no requests, database writes, reactive timers or per-tick server work. All four Home fixture groups, the 14 debt and countdown checks and the offline lifecycle (14 checks and eight tabs) passed locally. [Home fixtures](../test/test_welcome_primary_loading.R) · [Browser check](../test/test_welcome_home_browser.cjs)

## Button visibility (2026-11-26)

Offline Chromium verified all four edges of the squad market-listing confirmation in light and dark themes at 320, 390 and 1440 px. The dialog fit at each width and the synthetic listing flow passed. Opening an individual player from the roster at 390 px also showed a 2 px solid confirmation border in each theme. The same transaction styles cover bids, offers and clause confirmations. The player-card and lineup browser suites passed at all three widths. Received-offer buttons retained keyboard activation and compact rows at 320, 360, 390 and 1440 px in both themes.

The runtime change uses CSS on existing controls. It adds no requests, database work, timers or reactive renders. The offline Shiny lifecycle passed 14 checks and eight tabs. [Listing check](../test/test_bulk_market_premium.cjs) · [Offer table check](../test/test_offer_transfer_browser.cjs)

## Points and ranks switch (2026-11-26)

Chromium verified that the same two selected rounds showed 15 and 12 cumulative points or ranks 1 and 1 for the leading team. First place appeared at the top. After narrowing Ranks to one subround, switching back to Points showed its 15 season points. Switch and range changes added zero saved reads at 320, 390 and 1440 px in both themes.

The shared plot publishes only the selected metric and rounds. Switching metrics reuses calculated totals, and publishing identical saved observations caused zero chart redraws in the delayed-history browser test. The 13 plot checks, delayed reads, status disclosures, 10 round-award checks for the shared switch styling, and offline lifecycle (14 checks and eight tabs) passed with synthetic data. [Range and switch checks](../test/test_plots_browser.cjs) · [Delayed-history check](../test/test_plots_loading_browser.cjs)

## Standings round range (2026-11-26)

A separate offline initialization check against revision `165580a` reproduced a startup error in Standings. The verified-history lookup was outside the reactive that defines the team history. Keeping that lookup beside the saved ranking read lets the module initialize and supplies the existing participation evidence to the chart.

In the offline Chromium fixture, rounds 6, 6.2 and 7 had cumulative scores of 10, 15 and 12. Selecting 6.2 through 7 showed 15 and 12. Selecting 6.2 alone showed 15. Restoring both defaults returned all three points. Reversed endpoints displayed a correction prompt. All six combinations of 320, 390 and 1440 px with light and dark themes passed without horizontal overflow.

The delayed-read test preserved an unknown team score and counted zero additional adapter calls when narrowing and restoring the range. The controls reuse the published round list and the shared season totals. Each selection filters the in-memory display rows and redraws the standings plot. All 13 plot checks and the offline lifecycle (14 checks and eight tabs) passed. Verification used synthetic data. [Browser check](../test/test_plots_browser.cjs) · [Read-reuse check](../test/test_plots_loading.R)

## Home offers and squad movement (2026-11-26)

The signed-in Home fixture counted two distinct outgoing offers worth €350,000 across three market rows with one duplicate bid ID. The 17-player squad showed two rising, one falling, one incoming and one leaving. A complete empty transfer history produced zero arrivals and departures. Holding the market response kept offers pending while cash, squad size and navigation stayed available.

Home now consumes the shared market and transaction-history resources on first display alongside the existing roster read. In production the factual worker supplies these from durable, scoped observations. Transfer verification reuses the existing player-summary reader and bounded queue. The change adds no persistence, polling or per-player UI components. In the synthetic fixture each of the three shared resources was read once and ten unrelated input changes added zero reads in 0.332 s. Chromium confirmed that the layout fit at 320, 390 and 1440 px and that a warm return added zero overview reads. All four Home fixture groups, the manual finance regressions and the offline lifecycle (14 checks and eight tabs) passed with synthetic accounts. [Home regression](../test/test_welcome_primary_loading.R) · [Responsive layout check](../test/test_welcome_home_browser.cjs)

## Compact player details (2026-11-26)

The rival card’s offer and clause buttons inherited a borderless style. The 320 px browser regression reproduced the missing outline on the owner offer. After the CSS change, Chromium verified a 2 px solid outline, a fill distinct from the modal surface and a minimum 44 px target for both actions at 320, 390 and 1440 px in light and dark themes. Each opened its corresponding editor or confirmation in the offline fixture. The scoped styling uses the existing controls and theme colors and adds no requests, database reads, timers or reactive renders. [Browser regression](../test/test_player_offer_browser.cjs)

Local Chromium rendered a listed free agent with 66 points, 33 over the last five matches, a 6.6 average and a €1,852,568 valuation. The signed change appeared with the value. Ownership and listing status appeared once as plain text. The pending availability message used a status role. At 320 and 390 px the scoring figures shared a row, the card fit the viewport and the history heading started within the first 475 px. The desktop layout placed the valuation on the scoring row. The browser exercised Close, Recent rounds and the synthetic market, owner and clause actions. [Browser regression](../test/test_player_offer_browser.cjs)

Removing the hidden dashboard header also removed its server update. The delayed-source fixture counted zero box updates, zero action rebuilds on optional history completion and six existing source reads. The saved-card checks covered database reuse and zero browsing source requests. The UI reliability suite, 17 history checks and offline lifecycle (14 checks and eight tabs) passed locally. Verification used fixture data. [Loading regression](../test/test_player_card_loading_work.R)

## Home kickoff label and timestamp (2026-11-26)

The Home fixture checks that the kickoff label and localized timestamp start on the same row in separate columns and that the countdown sits below both. Chromium verified the layout at 320, 390 and 1440 px without horizontal overflow. The offline Shiny lifecycle passed 14 checks plus eight tabs. The layout uses CSS on the existing elements and adds no API calls, database reads, reactive updates or timers. [Browser regression](../test/test_welcome_home_browser.cjs)

## Home squad value and player count layout (2026-11-26)

The synthetic Home fixture showed a €34,000,000 squad value directly above its +€34,000 daily change. Squad size showed 17 players, two rising and one falling. With a missing player change, the player total and cash still rendered and both direction counts were marked unavailable. Both cards share one reactive summary of the existing saved team and roster. The fixture counted one roster read and zero additional reads across ten unrelated inputs. The layout introduces no acquisition, writes or timers. [Home regression](../test/test_welcome_primary_loading.R)

Local Chromium verified the order of the value and trend, the count placement, and a page without horizontal overflow at 320, 390 and 1440 px. Returning Home reused the overview reads. The offline lifecycle passed 14 checks and eight tabs. [Browser fixture](../test/test_welcome_home_browser.cjs)

## Home round status and squad trend (2026-11-26)

The calendar regression replayed an early Round 23 game, the intervening Round 15 while playing and after closure, and the later Round 23 kickoff. It also covered an undated postponement, overlapping matchdays, older calendars and a finished fixture list awaiting official round closure. A 38-round, 380-match season took 2.1–3.1 ms per warm projection across two local runs of 100 iterations. The open Home card uses one replaceable session timer for the next relevant calendar boundary. [Round regression](../test/test_home_round_context.R)

The signed-in Chromium fixture displayed Round 11 closed, the Round 12 kickoff, €10,000,000 cash and a +€40,000 daily squad trend. Two players rose, one fell and fourteen had unchanged prices. The overview fit 320, 390 and 1440 px viewports. A warm return added zero overview reads. The Shiny tests verified that cash and squad size remain readable when a player’s change is missing, and that stale figures and the sync notice on Home omit observation times. Ten unrelated inputs caused zero further reads. Home shared one roster read across both cards. Summarizing 50 players averaged 0.254 ms over 1,000 local calls.

In a separate Shiny session, the scheduled callback fired as the wall clock passed kickoff. Home advanced from Round 11 closed to Round 12 in progress with zero additional calendar reads. The shared My Team and Rivals trend checks, existing round countdown tests and offline lifecycle also passed. The lifecycle covered 14 checks and eight tabs. Verification used synthetic accounts and local Chromium. Hosted rendering remains unverified. [Home behavior](page_preferences.md#league-overview-and-saved-destination)

## Saved Round awards after league metadata expiry (2026-11-26)

A read-only check decoded the production catalog for the league previously reported here. Its 04:55 PST snapshot contained 38 calendar rounds, 13 completed rounds, and all 143 saved player selections for rounds 1–13, including 13 MVPs. The separate resolved league context had been checked at 04:32 PST. The corrected display uses the verified historical scope when live membership metadata expires after five minutes.

The offline Round awards module reproduced the screenshot on sign-in before it requested any awards. With the fix, the same scenario restored all eleven fixture players from a compressed database snapshot, including the MVP and a negative score. A subsequent saved revision replaced the winner in the rendered lineup. A genuine partial read displayed its saved XI and a coverage warning. Changing to an unresolved or foreign scope cleared the previous lineup. Returning to the league with expired account-scoped membership reused the stored selection.

The focused worker-display fixture measured one catalog read on first display, one for the corrected publication and zero for an unchanged poll. Browsing issued zero source requests or persistence writes, including when the saved catalog contained a settlement recheck. The existing worker poll supplies version changes. The 38-round, 418-player offline render took 0.664 seconds. The award projection, crests, initial catalog, scoring, closed-round, Admin refresh, Admin awards and settlement checks passed. The offline lifecycle passed 14 checks and rendered eight tabs. These rendering checks used synthetic accounts.

## Floating Home and Refresh controls (2026-11-26)

Home and Refresh share one fixed row on signed-in destination pages. The mobile scroll listener is passive and uses one pending idle timer. Scrolling changes the row's visibility locally. It adds no Shiny input, data acquisition, persistence or recurring polling.

In Chromium, both buttons measured 109.8 × 48 px with matching top edges at 320, 390 and 1440 px. Desktop controls stayed visible in light and dark themes. Phone viewport and nested-table scrolling revealed them. Across the two local runs, the controls were fully hidden after 575–577 ms with reduced motion and 713–724 ms with the fade enabled. Hidden buttons passed taps through. Keyboard focus revealed them, a deliberate Refresh produced exactly one refresh input, and scrolling alone produced zero. The isolated offline lifecycle passed 14 checks and eight tabs. [Navigation behavior](page_preferences.md#floating-home-and-refresh-controls)

The mobile idle delay was increased from 550 ms to three seconds. In local Chromium, a second scroll kept both controls tappable beyond the first deadline. They were fully hidden 3.014 seconds after the last scroll at 320 px with reduced motion and 3.165 seconds after it at 390 px with the fade. Page and nested-table scrolling reused the same passive listener and single timer and produced zero Refresh inputs. The offline lifecycle passed 14 checks and eight tabs, and the deployment contract passed all three checks.

Refresh feedback appeared above the button in synthetic Chromium checks at 320, 390 and 1440 px. At phone widths it remained readable after the normal scroll deadline. One press sent one refresh input. Offline worker checks held the queue reply open, then tested a positive count, a zero count and a rejected request. The previous balance stayed visible until the worker published its new version. A client check verified that late replies cannot overwrite a newer press or reappear after a league change or disconnect. Feedback reuses the existing request, adds one server-to-browser result and at most one dismissal timer, and adds no polling or data reads. Refresh stability passed 11 checks, the lifecycle passed 14 checks and eight tabs, and the deployment contract passed three checks. Verification used local fixtures.

## Public welcome and signed-in Home (2026-11-25)

The welcome inventory review traced the Balance summary to its three active pages and the squad-wide pricing preview to its explicit confirmation in My Team. Public Futmondo sources confirm individual market listings, optional public team balances, suggested XIs and Home navigation. The comparison records the two additional LigaRadar entries and the setting that qualifies the rival-cash comparison. [Source review](feature_comparison.md#welcome-and-home-inventory-2026-11-25)

An offline R check loaded the shipped inventory and matched all 18 distinct feature IDs to the documented comparisons. It covers seven Essentials and eleven Advanced entries. Only three per tier are initially expanded into view. The inventory and its disclosures use shipped HTML and CSS without any source, database or Shiny request when expanded.

Local browser checks at 320, 390 and 1200 px verified the public layout and login dialog. The animation measured 180 × 120 px on phones and 350 × 233 px on desktop. Pausing stopped canvas draws. Reduced-motion mode shows the static pitch.

Full-app Chromium checks covered 320 px in dark mode, 390 px in light mode and 1440 px in both themes. The fixture showed Round 12, €10,000,000 cash and 17 owned players. All nine ordinary destinations opened from Home. The mobile sidebar closed on selection, and the Home button returned from the bottom of a page. Continue opened the saved destination and logout returned to the public welcome. A warmed return to Home took 34–51 ms and made zero repeat overview reads. Holding the market response open left Home available. An empty league selection kept navigation accessible and all three overview figures explicitly unavailable.

The remembered-login browser suite passed 9/9 with the original proxy and 9/9 with its rewritten-host configuration. Both covered restart and observed zero frames showing credentials during restoration. The offline Shiny lifecycle passed all 14 checks and rendered eight tabs, the Balance banner passed 18/18 checks and the deployment contract passed 3/3. The fixtures made no external HTTP requests or real transactions.

The isolated staged app passed the four-layout Home browser check and the full offline lifecycle. All 15 changed packaged files matched their manifest checksums. The dedicated theme-switching and loading-animation browser checks also passed. Two broader browser suites still expect visible radio buttons or historical player-card data from an older fixture. The standalone UI reliability suite also stops at a pre-existing missing roster-fixture dependency. The Round awards navigation assertion from that suite passed independently.

The screen and login checks use local synthetic accounts. Production deployment, authenticated official screens and physical-device rendering were outside these checks.

## Rivals team selection and detail tabs (2026-11-25)

- An offline browser entered Rivals with its saved results held back. The loading message appeared before the first team row and cleared when the directory arrived. Desktop and phone layouts passed at 1600, 1440, 1200, 390 and 360 px, including light/dark hover, mouse and keyboard selection, sorting, the financial pivot, expanded breakdown, roster trend and viewport overflow. A separate browser fixture verified selection through a balance update, reorder and league change.
- The financial fixture verified the default tab and content order, a €52,129,125 balance, the €2M MVP and €1.5M Best XI income components, and stable tab controls after a same-team update. The running-balance and trading-profit regressions also passed.
- The roster fixture verified a €24M team with a −€30,000 daily trend, two rising players, one falling player and one unchanged player. It covered stale, partial and confirmed-empty rosters. Opening three squads required three roster reads. Viewing history and reopening a previously loaded squad added none. The table and summary share each scoped observation.
- Hover is CSS-only. Selection sends one team ID, and finance updates reuse the same ledger. The held-read latency check passed in both response orders with a 5.1 ms event-loop heartbeat and one rebuild each. The offline lifecycle passed 14 checks and rendered eight tabs. The change adds no source endpoint, database writes, timers or deployment dependencies.
- Verification used synthetic accounts and locally rendered pages. The Rivals assertions in the mixed league-disclosure test passed. Its separate Plots section still references a retired standings output. Hosted rendering and production latency remain unverified.

## Immediate balance updates for manual offers and clauses (2026-11-25)

- The focused Shiny fixture exercised real confirmation observers with a
  held native response and independently published synthetic snapshots. It
  verified acknowledgement-only changes, unchanged totals when the market
  arrives first or the balance arrives first, the saved summary of an unlisted
  offer, league isolation and logout. Clause cases covered immediate cash
  and Today card updates, a balance published during the POST, conflicting
  or partial history and a successful handoff to the exact saved transfer.
  The offline lifecycle passed 14 checks and rendered eight tabs. The
  balance banner, unchanged-refresh, native offer, clause refresh and saved
  reconciliation regressions also passed.
- The 524-listing fixture rendered the updated offer summary in 108 ms from
  the local acknowledgement through the test's Shiny flushes, with no
  additional acquisition call on the immediate path. The projection reuses
  the existing team, market and transaction states. It filters saved history
  by buyer and pending player before parsing dates. An unlisted offer uses one
  scoped player-summary interest through the existing bounded snapshot reader.
  There are no new source endpoints, persistence writes, timers or table
  redraw dependencies. The unchanged-refresh fixture preserved the monetary
  card across ten source freshness cycles. The notice reuses the existing
  information disclosure and the amount cards keep their wrapping layout.
- Successful bid and buyout captures and the official client support the
  acknowledgement and accounting fields. The unlisted-offer handoff and
  delayed-purchase cases were verified with synthetic observations. Hosted
  transaction latency, reconciliation timing and authenticated rendering
  remain unverified. [Source comparison](feature_comparison.md#immediate-balance-updates-after-offers-and-clause-purchases-2026-11-25).

## Manual clause settlement in the balance summary (2026-11-23)

- A read-only production check found one accepted manual clause reservation whose amount exactly explained the screenshot’s deduction and whose slot explained the reduced capacity. Replaying the matching durable worker snapshots through the existing decoder found a positive balance with zero source-held funds and one qualifying pressroom transfer with an explicit locker clause marker. The source balance was observed after the transfer. The saved reconciler previously excluded manual clause jobs.
- The offline balance and squad fixtures cover zero active offers, the difference between held funds and active offers, true cash debt, and two free slots at 18 of 20 after settlement. The saved observer, scheduled-clause and isolated PostgreSQL checks cover matching identity and paid price, absent or conflicting method, successful acknowledgement, stale or foreign evidence, and one-time durable release. The Shiny lifecycle passed 14 checks and eight tabs.
- The summary reuses its existing market scan and current scalar projection. The observer reuses the existing scoped snapshots and reservation poll, skips market lookup for clause-only candidates, and issues one asynchronous settlement RPC for a matching snapshot pair. No added source fetches, timers, retained payloads, or background jobs. Ten timestamp-only updates kept the squad card unchanged, and ten full stale/loading/fresh cycles kept the balance card unchanged. The existing mobile wrapping layout handles the conditional slot labels. Hosted rendering remains unverified.
- After isolated PostgreSQL verification and explicit owner approval, production applied migration version `20261124064145`. The function remains SECURITY INVOKER with service-role execution. The approved recovery replayed the exact saved pressroom and balance observations, settled the matching reservation and read back `clause_purchase_confirmed` with zero cash and slot holds. This used only the database and sent no Futmondo transaction. The signed-in browser view was outside the verification scope.

## Session-only scheduled purchase checks for manual actions (2026-11-23)

The scheduling-enabled offline offer and clause fixtures submitted the confirmed player and amount once, with zero pre-submission Futmondo reads or reservation RPCs. Duplicate confirmations did not replay a held request. The offer fixture covered native rejection, lost acknowledgement, a scoped refresh and local conflicts over the target, funds and slots. Inactive reservations skipped the capacity lookup. The clause refresh suite passed all 11 checks, including one catalog refresh and zero player-history reads after success.

The existing account poll supplies scheduled jobs, and saved worker capacity is read from memory only for active reservations on other players. Confirmation adds no timers, database queries or reactive redraw loops. Multiple sessions can briefly disagree between poll updates. Browser tests at 320 and 1440 pixels held each acknowledgement for one second and verified pending controls. The offline lifecycle passed 14 checks and eight tabs. Hosted transaction latency was not measured.

## Real club match points (2026-11-23)

- The captured finished Levante–Elche response matched the 1–1 score, minute 90 and all eleven visible home points under the championship's custom formula. The focused regression exercised an in-progress response, scheduled and finished cards, valid zero and negative points, missing scoring components, identity checks, failed refresh retention and bounded retry ordering. A delayed database response populated the dialog without changing the draft or requesting Futmondo. Reopening another card reused the same snapshot. The offline lifecycle passed 14 checks and eight tabs. The fixture, crest, lineup module, lineup snapshot context, autonomous discovery and Python worker suites passed.
- Parsing the captured match averaged about 32 ms over 30 repetitions on this host. Its normalized snapshot held 31 playing records and used about 35 KiB before compression and 2.1 KiB after compression. The worker performs two scoped snapshot lookups and one rolling publication per successful sweep. It makes up to six source reads for due matches, including retries, through the existing rate limit and worker lanes. It reuses future calendar rows and settled final details. A cycle with no due matches makes zero source calls and still publishes its scoped snapshot. Time-only changes leave fixture cards, the scoring selector and the player tables mounted. Only the open modal projects its selected match.
- Chromium verified keyboard opening, score-mode switching, closing, scheduled state, and one- or two-column rendering without horizontal overflow at 320, 390 and 1440 px. The capture verifies the finished match. In-progress state and database behavior used local fixtures. Production collection lag and authenticated hosted rendering remain unmeasured.

## Compact lineup pitch (2026-11-23)

- The historical-lineup suite passed 17 checks, and the editor module checks and offline lifecycle passed. The lifecycle exercised 14 checks and all eight application tabs.
- The historical Chromium fixture checked the two themes, name truncation, portraits and crests, the tilt, six-midfielder spacing, touch close, and resize reuse. Coverage spans phone portrait and landscape, desktop, and resizing across the 992 px breakpoint. The editor fixture covered 320, 390 and 1440 px, including dense and forwardless formations, slot selection and verified synthetic saves. Authenticated hosted rendering remains unverified.
- With the same historical 3-6-1 fixture, the pitch shrank from 582 to 378 CSS px at 320 px viewport width, from 561 to 378 at 390 px, and from 725 to 509 at 1440 px. The transform affects one static decorative layer, and the layout reuses all existing player nodes and lazy asset URLs. There are no added reactive invalidations, timers, cache entries, API calls or database operations.
- Abbreviations without a space after the period passed the 17 historical checks and the editor browser fixture at 320, 390 and 1440 px. Direct formatter checks covered compound surnames and single-word names. The same substitution pass produces one fewer character per abbreviated name, with no added rendering work, reactive dependencies, asset requests or data access.

## Clause filter cash balance (2026-11-23)

- The table regression uses a €2M cash balance and €5M spending limit. It includes a €2M clause, excludes a €2.01M clause, applies a stricter typed cap, and covers zero, negative and unknown cash. The championship module tests verified the shared cash field with an unresolved market read and rejected stale or missing balances. The table suite passed 21 checks, the championship suite passed 12 groups and the offline lifecycle passed 14 checks plus eight tabs.
- The filter uses the existing account/league/team balance reactive and a single row mask. Typing or toggling filters does not rerun balance acquisition, and the deployed path supplies the already loaded Balance summary source. This removes the full financial snapshot, acquisition-capacity and reservation dependencies from the clause browsing filter. There are no new API/database operations, persistence, timers or image requests. Authenticated hosted behavior was outside these checks.

## Real Club filter (2026-11-23)

- The player-table suite passed 19 checks. The new fixtures cover accented names, crest URLs, duplicate and name-only club rows, unknown clubs, combined club/owner filtering, retained choices after snapshot updates and reset on league changes. The offline lifecycle passed 14 checks and rendered all eight tabs.
- Chromium exercised both Shiny table layouts at 320, 390 and 1440 px in both themes. It verified alphabetical order, crest rendering in options and the selected item, escaped labels, search, selection, clearing, no dropdown overflow and no control replacement on a price-only update. Fixtures used synthetic data and crest responses. Authenticated hosted behavior remains unverified.
- For 524 synthetic players across 20 clubs, median local projection time was approximately 2.5 ms. Building and serializing the 20-club control took approximately 7.9 ms and produced 3.7 KB of HTML. Club projection runs once per loaded snapshot change and control markup publishes only when choices or context change. Club selection uses one row mask and search is local to the browser. The control can request one lazy image per distinct crest URL when shown. No additional API/database reads, writes, timers or durable cache entries were introduced.

## Player injury and card icons (2026-11-23)

- The player-name regression checked the rendered cross and separate red-card, double-yellow and generic-suspension labels. Lineup and My Team regressions checked the shared display and existing roster reads. The yellow-card code enables the icon but carries no additional eligibility or absence inference. The offline lifecycle passed 14 checks and all eight tabs.
- Chromium checked the actual name-cell markup with the app stylesheet at 320, 390 and 1440 px in both themes. The cross and card marks fit beside the labels. Both yellow shapes and the red card have opaque fills. Authenticated hosted rendering was outside the checks.
- The table still builds markup once per distinct observed status. The card glyphs use one span each and CSS pseudo-elements, and the injury cross uses the installed icon font. The change adds no asset requests, reactive dependencies, API or database work, cache entries or timers.

## Matchday Dream team display (2026-11-23)

- The Classification loading regression decoded a synthetic worker snapshot and checked crests, position codes, ties, zero and negative scores, unavailable data, the official MVP, and reuse after switching tabs. The Classification presets and live regressions passed. The offline lifecycle passed 14 checks and rendered all eight tabs.
- Isolated Chromium checks applied the app stylesheet and theme tokens to the rendered table at 320, 390 and 1440 px. Both themes kept the crests and long names inside the layout and showed a distinct MVP background, accent and label. Image responses were synthetic. Authenticated production rendering and live crest delivery were outside these checks.
- On an 11-player fixture the median parse and HTML render took 21.4 ms, up from 7.5 ms, and markup grew from 803 to 5,025 bytes. The work is confined to the selected round’s existing output. Each crest adds a lazy 22 px browser image, with repeated URLs eligible for browser reuse. There are zero added factual API or database calls, writes, timer ticks or cache entries. Settings and standings retain their existing dependencies.

## Player status indicators (2026-11-22)

- The focused table and card checks passed with synthetic fit, missing, injury,
  doubt and suspension states. The status was available in the initial card
  identity while optional reads were pending. Persisted card browsing and lineup
  checks also passed.
- My Team tests covered positive, fit/missing-only and legacy rosters, panel
  order, safe row links, context changes and exact received-offer selection.
  My Team used one existing roster read, and opening a status-row link acquired
  no additional data in the fixture. Outgoing transfer and saved-observation
  contracts passed.
- Chromium checked the table at 320, 390 and 1440 px in both themes. The icon
  stayed inside its name cell, fit players had no icon and the status column was
  absent. The scroll, selection and heartbeat checks passed in all six runs.
  A separate browser fixture at the same widths and themes verified that the
  My Team box matches the Leaving players border and padding, the badge stays
  on the portrait corner and both conditional indicators disappear on recovery.
  The empty panel receiver stays active so it can display a later observation.
  The offline lifecycle passed 14 checks and all eight tabs.
- On a synthetic 524-row widget with 15% non-fit codes, the median server-side
  build/serialization over eight runs was 1,108 ms versus 1,626 ms before this
  change, with 350,277 versus 440,570 bytes of rendered markup. A 25-player
  roster with three displayed statuses took about 10 ms per panel projection
  and serialization over 100 iterations. These are isolated local measurements.
  The panel reuses existing context-scoped roster snapshots and does not
  republish equal markup. The portrait badge is an overlay within the existing
  photo dimensions. No additional requests, database writes, timers, history
  redraw dependencies or persistent caches are required. Hosted rendering was
  outside the verification.

## Market offer button emphasis (2026-11-19)

The shared purchase style gave **Make Market Offer** a 0 px border in both themes. A scoped stylesheet rule now applies a 2 px border, tinted fill and bold label to the player card’s opening action. Headless Chromium checked 320, 390 and 1440 px with both themes. The resting border contrast against the card was 9.1:1 in light and 15.0:1 in dark, and label contrast against the fill was 7.6:1 and 10.2:1 respectively. The 45 px control fit at every width, had a visible keyboard focus outline, and opened the offer editor with verification still required before submission. Narrow screenshots were inspected.

The offline offer-opening suite passed 18 checks, and the offline lifecycle passed 13 checks and eight tabs. Performance review found a single matched control and an existing CSS color transition with zero added source requests, database reads, reactivity, scripts or assets. The fixture used synthetic market data and did not submit a live offer. Hosted rendering on a physical phone remains unverified.

## Player-card opening latency review (2026-11-19)

Today’s market uses `players_table_Server()`. The Reactable click travels over the Shiny connection. The server captures the existing selected row and sends modal HTML, then enables the selected-player reactive in `session$onFlushed()`. The first shell contains the close control and output placeholders. The identity arrives through a subsequent output flush even though the clicked row already contains the name. Its server metric starts when the card context activates, so it omits the click, modal delivery and browser paint. The browser’s identity acknowledgement unlocks the chart separately. History also depends on resolved league, summary and lineup facts.

Headless Chromium measured the actual Today’s market table in the offline application, timing DOM visibility from the click. Requests outside the local fixture were blocked. The fixture delays summary by 2 seconds and match details by 2.8 seconds. It uses one R process across browser sessions.

| Rows | Viewport | First shell | First name | Match details | Reopen name |
| ---: | ---: | ---: | ---: | ---: | ---: |
| 3 | 390 px | 304 ms | 502 ms | 3,256 ms | 196–203 ms |
| 3 | 1440 px | 213 ms | 378 ms | 3,005 ms | 199–216 ms |
| 524 | 390 px | 366 ms | 581 ms | 3,335 ms | 227 ms |
| 524 | 1440 px | 260 ms | 422 ms | 3,088 ms | 221 ms |

In the dense fixture the click reached the WebSocket in 20–29 ms. The first modal frame arrived at 110–215 ms, before the shell became visible. A separate first-open R profile sampled 0.40 seconds of byte compilation in a 1.94-second window spanning selection and early rendering. It does not attribute all of that work to the interval before identity. `test/test_player_card_database_display.R` also passed, including saved-snapshot reuse, pending optional reads, and zero source acquisition from browsing.

This separates a measured startup/second-flush cost from the reproducible multi-second wait for match details. The local fixtures did not reproduce several seconds before the shell appeared. A delay before the shell on the hosted app requires an authenticated browser trace from click to selection send, modal frame and DOM visibility to distinguish session/network delay from browser initialization. A follow-up speed change could include the captured row’s name in the initial shell and leave optional history deferred. Public fingerprints match all four runtime files from `9a66a7f`. This review makes no runtime change and has no performance impact.

## Player-card clause controls and Market offers (2026-11-19)

The player card previously treated every non-owned player row as a possible clause purchase. Saved System ownership and a retained positive clause price reproduce the screenshot. The focused database-backed card test exercises open and locked rival clauses, a listed free agent with the same positive clause price, stale clause entry and confirmation, market-offer availability and zero browsing source calls.

The Market maximum already read `value`. The filter now names that column explicitly and the label matches the player valuation. The real Market module test with parsed synthetic rows keeps a €5M player with a €12M clause and €8M asking price, excludes a €6M player with a €4M clause and €3M asking price, and excludes an unknown valuation only while the cap is active. Opening and editing/clearing the filter share one Market read. The table suite passed 15 groups.

The reported post-confirmation message was reproduced through the live adapter with transfer history completing 20 seconds after verification began. Production read-only reservation and RPC-log queries found no corresponding admission evidence. The delay reproduces the symptom locally, and the exact cause of the hosted attempt remains unverified. The focused async test refreshes the expired team-information read once, reuses the other five factual responses and confirms one synthetic `isClause: false` write after one admission and one dispatch. Repeated expiry, insufficient funds, a context switch and a lost native acknowledgement all exercise the applicable no-send or no-repeat rules. Two successful historical native bid captures establish the empty-object envelope, without replaying captured credentials.

Performance review: the rival guard uses row IDs and avoids a passive clause check for free agents. The valuation filter applies one vectorized comparison over the existing reactive table with no extra read, cache or redraw dependency. Manual submit adds no request on the fresh path and at most one verification recheck after expiry, reusing valid inputs. In the delayed fixture only fresh team information was fetched again. Source observation clocks, persisted reuse, reservation and transaction safeguards remain in effect. No schema, dependency, timer, CSS or background acquisition change. Authenticated hosted rendering, physical-phone timings and a real offer were outside the offline fixture.

Exact task tree verification passed the saved-card suite, 15 filter groups, 11 clause refresh groups, the manual and scheduled clause checks, 17 isolated database schema checks, three deployment checks, and the offline Shiny lifecycle with 13 checks and eight tabs. The supplemental UI reliability script passed its clause ownership checks and stopped because its standalone bootstrap does not load `player_availability_source()`. The full lifecycle and saved-card tests cover that module through the application bootstrap.

## Market balance card contrast — 2026-11-18

AdminLTE applied white text to the balance and team value cards even after the
shared theme replaced their colored backgrounds. The card content now uses the
active theme’s text color.

Headless Chromium reproduced the light-theme contrast defect with the original
CSS. Using the actual value-box markup and dashboard styles, the corrected
labels and amounts had minimum contrast ratios of 9.11:1 in light mode and
15.03:1 in dark mode at 320, 390 and 1440 px. The fixture had no horizontal
overflow. The deployed site was unverified.

`Rscript test/test_today_availability.R` passed all 3 checks, including one
shared team read and preserved zero, negative and unavailable amounts.
`Rscript test/test_shiny_simulation.R` passed 13 checks plus 8 tabs.

The CSS changes only text color. It adds no layout work, requests, database
operations, cache changes, timers, reactive updates, or persistent browser
state. The deployment manifest reflects the stylesheet checksum.

## Player-card light-mode valuation green — 2026-11-18

Changed the light-mode valuation line, left-axis ticks and title to `#15803D`.
The existing browser theme adapter updates the explicitly marked player chart
on initial render and palette changes; dark green becomes the original neon
green in dark mode. Orange points and ordinary plot behavior are preserved.

Verification: all 16 `test/test_player_trend_plot.R` groups pass. The existing
`test/test_player_trend_browser.cjs` fixture passes at 320/390/1440px across four
history modes and both themes, checking exact colors, axis matching, layout,
source data and zoom preservation, and no browser errors. Both 390px screenshots
were inspected. Browser heartbeat samples were 81/82/90 ms. The mandatory offline
lifecycle passes 13 checks plus eight tabs with no external I/O.
All three deployment-contract checks pass; both runtime checksums match the
task-only Git index, which excludes unrelated working-tree changes.

Performance review: one existing browser plot update now includes the valuation
line color together with the layout; trace selection inspects at most the card's
two series. No API/database calls, cache invalidation, persistence work, timers,
server reactive dependencies, extra plot nodes or growing state are added.
Existing durable history reuse is unchanged. No dependency or environment change;
only affected runtime manifest checksums are updated. Documentation has no
runtime cost. Checks are local and synthetic; hosted visual acceptance remains
unverified.

## Player-card calendar labels — 2026-11-17

The history chart now uses 10px horizontal labels (previously the inherited
13px size), with date ticks on the 1st and 15th of each account-local calendar
month. It retains the vertical day/month format, title spacing, plotted data,
hover details and separate undated-round axis. No feature-tier expansion.

Verification: `Rscript test/test_player_trend_plot.R` passes all 16 groups.
The existing browser fixture and `test/test_player_trend_browser.cjs` pass at
320/390/1440px across four history modes and both themes, including February,
a year boundary, exact calendar labels, font size, nonoverlapping bounds,
source/zoom preservation and no page errors. Inspected both 390px screenshots.
The fixture was updated to supply the existing `chart_data()` input and current
orange points colour. Browser checks used the installed Chromium binary through
`FUTMONDO_TEST_CHROMIUM`; its default Playwright binary was unavailable.

Performance review: tick construction scans already formatted dates and creates
two positions per covered month. No source/API/database reads or writes, cache
entries/invalidation, persistence changes, timers or reactive dependencies are
added. Existing durable history reuse is unchanged. Phone/desktop heartbeat
samples were 83/88/83 ms with no overflow; this is local fixture responsiveness,
not production latency. Documentation has no runtime effect. No dependency or
environment setup change; only the affected runtime checksum is updated in the
deployment manifest. Hosted visual acceptance remains unverified.

An isolated export of the staged application also passed all 16 plot groups,
all responsive browser modes and the three deployment checks; its runtime
checksum matches the manifest. Staged heartbeat samples were 81/81/63 ms.
Unrelated working-tree changes remain excluded.

## Match crests and trailing filter icons — 2026-11-17

Retained the official client's `h.logo` and `a.logo` fields in the existing
persisted fixture calendar (inner payload version 2), displayed 24px crests
beside both club names, and moved all shared filter icons after their labels,
including Clear Filters. Source URLs/digest and contracts are in
[lineup_module.md](lineup_module.md) and
[players_table_module.md](players_table_module.md). Version-1 calendars remain
readable; a failed normal-worker upgrade preserves their known matches and
source times with an explicit stale/error note. A successful upgrade is stored
and reused after restart.

Executed with synthetic accounts, responses and database adapters:

- `Rscript test/test_lineup_fixtures.R`: source filename/URL mapping, actual
  durable worker codec, legacy decoding, missing/unsafe logos, failed-upgrade
  display, original clocks, scope isolation, known-answer fixtures and bounds pass.
- `Rscript test/test_lineup_round_catalog.R`: nine groups pass, including saved
  source reuse, exactly one successful legacy upgrade/write, restart reuse
  without acquisition, and preservation/retry after failed upgrade.
- `Rscript test/test_award_fixture_calendar.R`: all five groups pass.
- `Rscript test/test_player_table_filters.R`: all 14 groups pass, including
  all 13 labels/actions in both layouts and existing filtering/debounce behavior.
- `Rscript test/test_lineup_module.R`: all checks pass, including asynchronous
  lifecycle, draft editing and duplicate-safe manual save/readback.
- The lineup browser fixture and `test/test_lineup_browser.cjs` pass at
  320/390/1440px: 20 source-mapped crest images decode at 24px beside the club
  names, long names fit, Save stays above the pitch, and existing editing,
  verified save and resize checks pass. Picker samples: 57/107/95 ms.
- The filter browser fixture and `test/test_position_filters_browser.cjs` pass
  at 320/390/1440px in dark/light themes, with trailing icon geometry, wrapping,
  All exclusivity, role unions, debounce, Clear Filters and table isolation.
  Heartbeat samples: 59–93 ms. Focused phone/desktop crest and phone filter
  screenshots were inspected; crest assets were mocked in browser tests.
- `Rscript test/test_shiny_simulation.R`: 13 checks plus eight tabs pass, without
  HTTP or live database mutations. `Rscript test/test_deployment_contract.R`:
  all three groups pass.

Performance review: the crest fields add no browser source-API/database reads,
reactive timers, worker jobs/lanes or cache entries. A legacy calendar may need
one existing bulk read in normal worker sync to obtain the new fields; valid
persisted version-2 evidence prevents repeated acquisition or replacement writes.
A failed upgrade remains retryable. The UI adds two lazy CDN image elements per
match (at most 100), with fixed geometry, async decoding and ordinary browser
asset reuse. A synthetic 380-match season used 194,480 bytes including logo
strings; parsing took 0.312–0.418 seconds in the worker and display selection/
validation took 17–21 ms. The filter change only reorders rendering; existing
524-row normalization/combined filtering took 0.002 seconds (95.1 KiB), and
multiposition filtering averaged 0.658 ms over 1,000 passes. No dependencies,
environment variables or schema changed. Only the five changed runtime-file
checksums are refreshed in the deployment manifest; documentation has no runtime
cost.

Final delivery verification used an isolated copy of the exact staged
application, without unrelated working-tree changes. The fixture, round-catalog,
award-calendar, filter, lineup-module, deployment and required lifecycle checks
all pass there. All five changed runtime checksums match the staged manifest.
The isolated 380-match sample parsed in 0.332 seconds and selected/validated in
17.7 ms; multiposition filtering averaged 0.721 ms. No runtime changes followed
these checks.

Limits: no real account or transaction was used. The public-client mapping and
synthetic fixtures do not establish authenticated production response completeness,
CDN availability or hosted latency. Existing snapshots gain crests during normal
calendar sync after the matching worker code is deployed; hosted acceptance
remains unverified.

## Official-client API reference guidance — 2026-11-17

Added the official web app and public JavaScript bundle to `Agents.md` as sources
for verifying request methods, routes, query/identity fields and response parsers.
The dated match-calendar example was checked against `docs/lineup_module.md` and
`mvp_api_answer()` / `get_award_round_metadata()` in `mvp_runtime.R`. This task
records the existing 2026-11-16 evidence; it does not claim a new live API or
authenticated UI verification. Reviewed the task-only diff and whitespace.

Performance review: documentation only. No runtime code, API/database requests,
cache/persistence behavior, worker jobs, reactive updates, timers, memory or
rendering changes. No runtime/performance tests, dependency or manifest changes
are needed. Existing unrelated working-tree and staged changes are preserved.

## Lineup save placement and real next-round fixtures — 2026-11-16

Moved the existing Save control above Formation/the pitch and added next-round
home/away matches below the pitch and Captain selector. The current official
public client's `/2/league/matches` request and `h`/`a`/`info.date` mapping were
verified; the parser now retains these previously discarded facts. Existing
calendar evidence and worker snapshots persist them with scope, source times,
absolute expiry and per-round completeness. No browser acquisition is added.

Executed locally with synthetic accounts and I/O:

- `Rscript test/test_lineup_fixtures.R`: known-answer target selection, home/away
  mapping, ordering, Madrid/Los Angeles times, actual worker codec hydration,
  account/league/competition/season rejection, missing opponents/dates/counts,
  duplicate games, corrupt counts, legacy/error/stale states and bounded costs pass.
- `Rscript test/test_lineup_round_catalog.R`: eight groups pass, including actual
  asynchronous getter/database filters, saved reuse without renewed clocks or
  writes, one-time legacy-field acquisition/persistence, corrupted coverage
  repair, failed-source preservation and existing exact zero/negative averages.
  Its fixture now explicitly runs in worker acquisition mode, matching the
  database-first production boundary.
- `Rscript test/test_award_fixture_calendar.R`: all five groups pass, including
  original postponed-date and ownership-calendar behavior. Corrected this test's
  database fixture to include the required competition scope; the original test
  also failed against the unchanged baseline without that field.
- `Rscript test/test_lineup_worker_calendar.R`: eight groups pass, including
  actual worker publication, legacy metadata recovery, scope checks and zero-I/O
  hydration. One hundred small calendar hydrations took 0.135 seconds.
- `Rscript test/test_lineup_module.R`: all checks pass, including held asynchronous
  requests, context-switch cancellation, duplicate-safe save/readback, history
  completeness and departure eligibility.
- `Rscript test/test_lineup_browser.R` with
  `PLAYWRIGHT_BROWSERS_PATH=/tmp/futmondo-browsers node test/test_lineup_browser.cjs`:
  320, 390 and 1440px pass. Save appears exactly once above Formation/the pitch;
  ten matches render below it, long names wrap, missing kickoffs remain explicit,
  and the existing editing/verified one-shot save/dense formations/resize tests
  pass without extra read/write counts. Player-picker timings were 75/106/127 ms.
  Inspected the phone and desktop screenshots. The first launch needed the
  existing Chromium location; no dependency was installed or changed.
- `Rscript test/test_shiny_simulation.R`: 13 checks plus eight tabs pass, with no
  HTTP or live database mutations. `Rscript test/test_deployment_contract.R`:
  three groups pass; private captures, tests and docs remain excluded.

Performance review: no new display API/DB reads, poll, timer, invalidation, worker
lane or job. Existing cold compact-only calendars may need one additional bulk
match read in the worker; an already-required numbered-calendar read is shared,
and valid database evidence prevents repeated acquisition. Failed calendar reads
are reused by both consumers in that attempt. The whitelisted parser is bounded
to 120 rounds × 50 games and displays at most one round. A 380-match synthetic
season occupied 96,848 bytes, parsed in 0.274–0.332 seconds, and took 17–19 ms per
display selection/validation. Current drafts do not affect fixture outputs. No
schema/dependency/environment changes; documentation has no runtime effect.

Final delivery verification used an isolated copy of the exact staged application,
without unrelated working-tree changes. Fixture/caching, award-calendar, lineup
module, deployment and lifecycle checks pass there; all four changed runtime
checksums match the staged manifest. The full browser sequence passes at
320/390/1440px (picker samples 57/72/109 ms). One earlier isolated desktop run
failed the existing picker-row geometry assertion; a desktop-only diagnostic and
the full diagnostic rerun passed without runtime changes. No failing geometry
was reproduced, so its cause remains unverified. This intermittent check is not
evidence of a hosted performance measurement. The diagnostic script stayed in
`/tmp` and is not part of the application or commit.

Limits: no real account or transaction was used. The current public client and
source-shaped fixtures establish the mapping, not a live production response.
Authenticated hosted fixture completeness, worker latency and deployment remain
unverified. Legacy snapshots gain match details on normal calendar sync after
the matching worker code is deployed. Contract: [lineup_module.md](lineup_module.md).

## Independent balance summary and squad slots — 2026-11-16

The deployed public server fingerprint and inline summary assets matched local
source before this task; synthetic rendering also showed the previous box. That
does not establish the cause in the owner's signed-in session. The previous
single `round_countdown` render depended on schedule, financial and pending-purchase
projections. The repair gives the first-content **Balance summary** a stable,
source-independent shell, independent child outputs and an explicit factual-output
marker excluded from disclosure adoption/pending hiding. Held context/source reads
keep labels visible; settled failures keep truthful unavailable fields and their
independent diagnostics. No financial value or manual-action preflight is bypassed.

Added actual/effective players, the observed configured squad limit and reserved
offer slots. The existing identity ledger accounts for bought/leaving players
without adding actual and effective totals together. The requested **No more
offers allowed.** appears at/over the known effective-plus-reservation limit.
Known saved limits retain observed status/source times; missing, partial or invalid
counts/limits never become zero. No availability claim is made below the limit.

Executed final isolated-source checks: `test/test_banner_finance.R` passes 18
checks; `test/test_debt_and_round_countdown.R` passes 14; the required
`test/test_shiny_simulation.R` passes 13 checks plus eight rendered tabs with
zero HTTP/live database mutation attempts; `test/test_deployment_contract.R`
passes all three groups. `test/test_unchanged_refresh_display.R` passes schedule
failure isolation, refresh/error recovery, independent squad timestamps, context
clearing and existing table stability. Ten squad timestamp updates took 0.212s,
leaving the shell and count card unchanged while source times advanced. The
existing effective-count/filter suite also passes in the shared workspace.

The real-root synthetic browser fixture passes 320/390/1440px in light/dark
(the six layouts), with visible facts on all three pages, the full-capacity
message, closed information panels, pending schedule recovery, stale pending
classes on factual outputs, retained shell/amount DOM, no page overflow/browser
errors and no live HTTP/transaction. After warm-up, navigation repeats zero team,
market, roster, history or transfer-summary acquisitions. Maximum measured warm
navigation was 31–52ms per layout in the isolated-source run. Phone/light and
desktop/dark screenshots were inspected. Fixtures use synthetic accounts only;
blocked external images are a test limitation, not missing production data.

`test/test_clause_transfer_persistence.R` passes 11 checks, including delayed
DB replay, original deadlines/context isolation and retryable unacknowledged
writes. Cold valid replay uses one scoped database read/zero API reads; warm
replay uses zero reads. The same root readers and bounded two-summary transfer
queue now serve Today/Players earlier. Their first entry may demand the existing
roster and transaction prerequisites and candidate summaries; My Team/Lineup and
navigation reuse those observations. There is no new result store, source polling
timer, dependency, schema, worker service or automatic transaction. Counts are
small identity-set projections; source clocks update independently. Existing
successful factual observations remain database-persisted with original context,
source deadlines and completeness, with bounded memory accelerators and retryable
failed writes. Local timings do not establish hosted network latency.

The worker rendering fixture initially stopped because local `sodium` is absent.
Its existing synthetic RPC boundary now also stubs only the synthetic credential
envelope, with an exact fake-auth assertion. Production encryption is unchanged;
this display check does not verify cryptography. The final persisted-worker
browser passes 320/390/1440px: source-version/expiry/recovery and delayed refresh
preserve the header, amounts and player-table DOM; changed facts repaint;
zero acquisition jobs are enqueued. During the 800ms delayed database read,
heartbeat completion measured 178–210ms; ordinary heartbeats measured 60–95ms.
These are synthetic local measurements, without hosted latency claims.

Unrelated working-tree/staged changes are preserved. Dependency/schema/setup are
unchanged; only this task's five runtime manifest checksums change. An isolated
source copy excludes unrelated uncommitted runtime changes for the release
checks. No shared-browser/session tools are available to inspect the owner's
signed-in tab, so authenticated hosted acceptance of the original failure remains
open. Public rollout verification cannot replace it.

Production source rollout verified after the successful non-force push of
`90ac68e`: `https://ligaradar.com/lineup-departure-runtime.json?balance-summary=90ac68e`
returned HTTP 200 with the exact committed `server.R` SHA-256. A fresh public root
request returned HTTP 200, exactly one `account_finance_summary` output, and
inline JavaScript/CSS matching the committed files byte-for-byte after whitespace
trimming. This verifies the automatic source rollout, not the owner's signed-in
session or real-account values. Authenticated hosted acceptance remains open.
This delivery-record update has no runtime/performance effect; no additional
performance test is required.


## Title-free information notes — 2026-11-16

Removed source-heading inference and label insertion from the shared browser
registry, including its label bookkeeping and CSS. All page/dialog notes now
show their original content directly. Reviewed the shared note producers across
Today, My Team, Players/cards/history, Rivals, standings/lineups, awards, charts,
notifications, preferences, Login and Admin. Source clocks, coverage messages,
errors, Retry controls and meaningful field labels remain intact. Non-visible
explicit source metadata remains available. This is a presentation reduction,
with no new feature or login-inventory/comparison change.

Executed verification:

- `Rscript test/test_page_information.R`: both groups pass, including eleven page panels, visible action validation and context republication.
- `node test/test_page_information.cjs` with existing Chromium in `/tmp/futmondo-browsers`: six layouts pass (320/390/1440px, light/dark). Asserts no generated titles before/after updates and in dialogs, original message text, severity/recovery, Retry binding, pending/empty states, removal, context reset and keyboard toggling. No overflow, browser errors or external requests; zero source/database attempts. Inspected the expanded 390px dark error-panel screenshot.
- `node test/test_page_information_app.cjs` against `Rscript test/test_browser_app.R`: all nine main pages pass at 320/390/1440px with populated panels, no generated titles, no overflow and no browser errors. Deterministic offline account only.
- `Rscript test/test_shiny_simulation.R`: 13 checks plus eight tabs pass; zero HTTP/live database mutation attempts.

Performance review: removes one label node and its creation, heading/category
lookup and stored reference per grouped note. API/database volume, durable
successful observation reuse, cache invalidation, source freshness/completeness,
reactive outputs, timers and acquisition concurrency are unchanged. The registry
retains its existing coalesced local updates and pruning. Browser mutation gaps
were 10–14ms and Shiny heartbeats 57–71ms in the six-layout fixture. Message
rendering occupies fewer lines on phones. No dependencies, schema, configuration
or environment variables changed. Only affected deployment asset checksums need
updating (the scoped asset hashes are updated in this commit). These are local synthetic results; hosted deployment and authenticated
production appearance remain unverified. Unrelated working-tree edits are
preserved and excluded from the task commit.

Final isolated verification of the exact staged application also passed the six
responsive browser layouts, both focused R groups, all three deployment contract
checks, matching changed-asset checksums and the required 13-check/eight-tab
offline lifecycle. Isolated browser heartbeats were 54–71ms with 10–13ms mutation
gaps and zero acquisition attempts.

## Coach icon and account summary placement — 2026-11-15 Pacific

The coach status icon shares a row with Automatically hire coach. Expanded native
information details occupy the next row. The root finance summary now precedes
startup/preference notices and page content, using its existing server visibility
rule on My Team, Today's market and Players; the duplicate client conditional
wrapper is removed. Existing amounts remain visible outside information details.

The real root browser fixture reproduced a missing summary: Shiny removed its
pending error classes while `fm-info-waiting` remained, hiding the entire output
and allowing suspension even though it held loading/recovering content. The
shared disclosure observer now clears that marker when an error recovers. CSS
hides it only while the actual typed pending error class exists, so success is
visible immediately, before observer delivery. Real settled errors remain
inspectable; transient/empty information icons remain suppressed. No source
value, availability or transaction preflight is bypassed.

Executed `Rscript test/test_banner_finance.R`: 15 checks pass, including held
and failed reads, one leading summary output, three-page visibility, saved
snapshot/source-time replay, scoped navigation reuse and clearing old league
amounts. Executed `Rscript test/test_shiny_simulation.R` on the final runtime:
13 checks plus eight tabs pass, with zero HTTP/live database mutation attempts.
`Rscript test/test_deployment_contract.R` passes all three groups.

`test/test_account_summary_browser.cjs` exercises the production root UI at
320/390/1440px in light/dark themes, including pending-error-to-success class
recovery, adjacent coach icon and details, visible financial amounts first on
all three pages, absence on Welcome/Notifications/Lineup, and no page overflow.
After existing page-specific resources mount, repeated navigation adds zero
team/market adapter reads and no HTTP or coach mutations. Phone screenshots
including expanded coach details were inspected. The synthetic fixture uses
acknowledged in-memory preference stubs solely to avoid live test persistence;
application persistence is unchanged. Local Chromium is selected using the
existing FUTMONDO_TEST_CHROMIUM override. Hosted/physical-device acceptance
remains separate. Maximum measured warm navigation was 28–35ms per layout.
The existing disclosure browser checks also pass all six layouts, with 54–60ms
heartbeats, 10–12ms measured browser gaps and zero source/database I/O. An initial
run timed out awaiting a synthetic error notification; rerunning with temporary
failure screenshot capture passed without changing its assertions or fixture.
No recurrence was observed in that rerun.

Performance review: the summary consumes the same shared team/market projections;
no acquisition, invalidation, source query, database write, cache policy, queue,
timer or growing collection is added. Existing valid durable observations and
source/freshness/completeness context remain reusable. Recovery cleanup checks
only changed local outputs and removes a class only when present; no observer
polling or extra document scan. One fixed grid positions the coach control; the
existing finance row wraps on mobile. Removing the conditional wrapper removes
a client visibility gate. Documentation has no runtime effect. No dependency,
environment, schema change or deployment is included. Only this task's four
runtime checksums are updated in the manifest. Unrelated working-tree/index
changes are preserved and excluded from the commit.


## Successful saved player availability — 2026-11-15 Pacific

Root cause: the capacity parser omitted the collector's completeness attribute. Read-only production metadata showed two successful stored capacity observations with unknown completeness. Replay of their actual codec payloads confirmed successful roster/funds/commitments, empty diagnostics and a genuine 15-second original action window; these checks had completed, then aged. Because the card required proven completeness to distinguish ordinary aging, it rendered the generic red expiry failure. No raw private payload, authentication secret, account identity or financial amount was logged, and no database row or account setting was changed.

`acquisition_capacity_observation_complete()` now proves the complete factual capacity contract. The calculator publishes the boolean through the established worker/database observation path. Restoration recovers only legacy unknown successful capacity evidence with this full proof and a nonfuture source timestamp, recording its proof source. Existing stored observations are reused without rewriting immutable manifests. All original clocks/deadlines remain intact; a complete but expired observation still fails the purchase preflight. The card keeps its existing neutral explicit-verification control, with matching purchase-button tooltips. Failed/partial/missing facts remain blocked and diagnostic. No error is dismissed by inventing a value or granting stale transaction eligibility.

The new `test/test_capacity_observation_completeness.R` passes five groups: source publication, real legacy codec restoration/deadline preservation, negative malformed/partial/future cases, actual card opening without passive acquisition/red failure, and explicit verified recovery without a transaction. Both inspected production capacity payloads also replay successfully through the actual new restore path, recover complete proof and retain blocked expired purchase eligibility. Existing worker card recovery, live-capacity timing (three scenarios), acquisition availability (seven), card delayed-summary recheck (four scenarios), slow-history card rendering, deployment (three), and required offline lifecycle (13 checks plus eight tabs) are executed with synthetic reads/no real transactions. The old recheck fixture initially failed an assertion expecting the now-intentionally-hidden pending notice; reproducing the same failure with pre-task source established the fixture mismatch. It now checks actual summary/capacity loading state without restoring waiting icons. Initial new-fixture purchase verification also required the real API bid-limit field; adding that synthetic source field corrected its setup. The auxiliary worker-contract suite passes its first six typed-codec/key checks, then stops because the local `sodium` package is absent before encryption assertions; encryption is unchanged and that full suite is not claimed passing. The new regression independently runs the actual snapshot codec and legacy restore path.

Performance: 1,000 strict completeness validations took 0.100–0.175 seconds locally across runs. Validation walks small existing lists/ID sets, once per calculation/legacy restoration; it adds a boolean/provenance field to existing durable results. No extra API/database read/write, source request, persistence queue entry, timer, cache invalidation, observer, listener, growing state or dependency is introduced. Successful new results use the same existing database publication; legacy reconstruction adds no write. Opening the real synthetic card performs zero passive source/database reads; explicit Verify performs its single existing live check with no transaction/retry loop. UI structure/mobile layout is unchanged, and tooltip copy adds no redraw source. Documentation has no runtime effect. Three runtime manifest checksums change; packages and other entries remain. Verification is local/offline plus read-only stored-data replay; authenticated hosted/physical-device acceptance and rollout remain unverified.


## Own-team filter and effective squad count — 2026-11-14 Pacific

All Players in Championship adds **Show players in my team**, unchecked/off by default. It excludes current-team IDs until included and intersects with the existing filters; Clear Filters resets off. Ownership uses normalized immutable IDs, including per-row source aliases, not manager display names. Other tables keep their defaults. The login's existing scouting-filter description and dated official comparison were updated, without claiming a generic squad-count capability is exclusive.

My Team groups current/effective totals in the position card. A shared `effective_squad_count()` identity ledger supplies both observed display and purchase capacity, deduplicating bought/leaving IDs and avoiding double application of arrived/already-departed players. Current + bought awaiting arrival − leaving is explained in the native tooltip. Complete successful historical observations may supply a descriptive count; failed/partial/missing evidence stays unavailable and loading says Checking. Actual roster/position counts remain independent of filters. Purchase preflights retain their fresh-source requirements.

Executed: focused own-team/count checks passed eight groups, including known-answer arithmetic, fallback ownership, intersecting filters, context changes, reset, missing/partial state and confirmed-empty rendering. Acquisition availability passed seven, existing shared-table filters nine, incoming/outgoing signings 24 each, clause-observation contract six, empty-squad two, login five and deployment three. Required offline lifecycle passed 13 checks plus eight tabs with zero real HTTP/live database mutation attempts. The exact task-only release also passed the focused eight groups, acquisition seven, shared filters nine, login five, deployment three and required lifecycle checks; unrelated local changes do not underpin validation. After rebasing onto the concurrent championship-price and pinned-column commits, the focused counts, championship-column regressions and full offline lifecycle passed again. The auxiliary championship suite initially lacked its intentionally excluded HAR fixture in the isolated directory; a local read-only fixture link resolved that setup failure. Captures remain outside the application manifest and commit.

The synthetic browser fixture uses the actual championship/My Team/shared-table modules. Chromium at 320/390/1440px in light/dark passed unchecked default, real inclusion/exclusion, actual18/effective19 arithmetic/tooltip, pending recovery, clock-only DOM stability and narrow-screen wrapping with no page overflow or JavaScript errors. Final 320px dark and 390px light screenshots were inspected. Initial scratch runs exposed fixture-only setup mistakes: an unsupported resource-state argument, an unopened collapsed filter and synthetic club logos making blocked external image attempts. Corrected fixtures pass without product changes for those issues. There was no authenticated official or hosted/physical-device acceptance.

Performance review: one vector ownership mask over already enriched rows; ID-set effective arithmetic is linear in the small roster/transfer lists. Toggle/clock/pending recovery left catalog, roster and market source reads and enrichment counts unchanged (one each source and two enrichments per synthetic session), with zero external I/O. Local browser toggle-to-render measurements were 265/179/150ms at 320/390/1440px, including scroll and Playwright actionability; these are not server request/production latency measurements. The existing 524-row normalization/combined-filter benchmark took 0.004s locally. No new API/database work, cache invalidation, acquisition, timer, listener, dependency or growing retained memory is added. Successful factual API observations continue to use established context-isolated durable database persistence/reuse; the count is a derived projection. Documentation/static login copy has negligible runtime cost. Five runtime manifest checksums change, preserving packages and unrelated entries. No schema/setup changes or real transactions.


## Empty and waiting information icons — 2026-11-14 Pacific

Root causes: empty nested tags/Shiny output placeholders passed the former shallow content check; closed details could suspend the text needed to populate them; generic pending read validations could be rendered as red failures, including notification refresh. Shared `fm_info_content()`/`fm_info(...,pending)` now omit empty/pending content and mark dynamic placeholders hidden before paint. Local client visibility updates reveal only meaningful settled content, suppress recalculating details, and hide a panel again when cleared. Read-only metadata clocks render while hidden, without requesting data. Pre-binding Shiny value delivery is guarded. True settled failures remain red and inspectable.

Passed `test/test_info_visibility.R` (nested empty/placeholder/pending/error and actual notification transitions), existing player disclosures, notification dates, clause observation, received-offer and worker-redraw checks, `test/test_info_disclosures.cjs` (320/390/1440px light/dark; delayed success/failure, filling/clearing, keyboard/open-state preservation, no overflow or backend I/O), and `Rscript test/test_shiny_simulation.R` (13 checks plus 8 tabs). Performance: 1,000 nested renders 0.200s, browser heartbeat 61–62ms, largest 150-mutation-burst gap 10ms. Metadata-only panel reuse is retained; a bounded set of already-computed text outputs now sends changed values while collapsed. No source/API/database reads, polling, cache/persistence or manual-action changes. No dependency/environment/schema change. Additional requested presentation changes remove the departure legend (its text is now a conditional name tooltip), hide all player/rival selector columns with row selection retained, and remove Market: from card headers. Departure widget/identity tests (9 checks), UI reliability and six browser layouts cover the tooltip lifecycle and selector-free clicks; rival selection retains IDs through reordering and clears across leagues. The name tooltip uses a WeakMap and only updates changed row subtrees; no roster enrichment or source work is added. Hosted acceptance awaits deployment.

## False offer errors and transfer information alignment — 2026-11-14 Pacific

Root cause: the offers adapter inherited generic `stale` from expired successful worker snapshots and rendered it as acquisition failure with a generic Refresh instruction. Read-only production aggregates confirmed expired `ok`/`empty` roster/bid snapshots without job errors. The adapter now requires validated offer-specific parser proof plus successful worker provenance before presenting dated observations; missing/partial/failed/future evidence remains unverified. Empty roster acquisition now retains explicit complete empty offers for durable recording and reuse, including reconstruction of authoritative old empty snapshots. Manual-action freshness and original source timestamps are preserved. The per-player information disclosure now sits beside its before/at/after-round label and expands below it.

Passed the new `test/test_offer_observation_contract.R` using the actual database-worker codec, existing offer persistence (8), received-offer UI (8), incoming/outgoing reconciliation (24 each), clause observation/disclosure tests, and `Rscript test/test_shiny_simulation.R`. Chromium checks at 320/390/1440px passed inline/open/closed/keyboard layout, no overflow, dated offers without false red errors, and independent source-clock updates. Performance: ten clock publications caused zero offer-panel redraws/acquisitions/writes (0.235s); browser heartbeats 67/67/66ms. Presentation adds only bounded validation, with no new display acquisition, DB reads, polling or replay writes. Newly acquired empty rosters now enqueue one complete empty-offer observation through the existing bounded idempotent persistence queue; the focused test verifies fresh DB reuse without HTTP or duplicate writes. No dependency/schema change. No real account/API/transaction test writes were used; hosted acceptance awaits deployment. See [module contracts](players_in_teams_module.md).

## Transfer panel copy — 2026-11-14 Pacific

Removed “Last observed transfer details. Saved observations update automatically; scheduled times do not confirm completion.” from both clause-transfer panels. Existing independent source-clock nodes, observed timestamps, factual rows and error details are preserved. Passed `test/test_player_info_disclosures.R`, `test/test_clause_observed_reload.R` and `test/test_clause_observation_contract.R`. Performance review: only one text node is removed; no request, persistence, cache, timer, reactive or mobile-layout work is added. No performance benchmark is required for this copy-only change.

## Uniform account dates — 2026-11-14 Pacific

Implemented browser-first IANA timezone selection (no geolocation), Spain fallback and durable per-user overrides from Welcome/Admin. All date presentation adapters share one session source; UTC source timestamps, amounts and countdown instants are preserved. Details: [timezone contracts and measured review](timezone_preferences.md).

Passed `test/test_timezone_preferences.R`, `node test/test_timezone_schema.cjs`, actual 390/1440px `test_timezone_browser` flows, notification datetime, Today transfers, incoming/outgoing clauses (24 checks each), rival-history context, player-trend plot (15 checks), page-preference transport (4 checks), worker status redraw and `Rscript test/test_shiny_simulation.R` (13 checks plus 8 tabs). Delayed preference responses retain event-loop responsiveness; failed saves stay unconfirmed and reload authoritative state. Formatting measured 0.001s/1,000 parsed instants and 0.032s/1,000 notification ISO labels; mobile/desktop browser heartbeats 84/97ms with no horizontal overflow. No additional factual API requests, polling or unbounded caches.

Production additive `user_timezones` migration applied to the established Futmondo project. Read-only verification: RLS enabled, zero preference rows before application use, one discoverable known account; all three SECURITY INVOKER RPCs have empty search paths and deny anon/authenticated execution while allowing service_role. The security advisor reports only the expected server-only RLS-without-policies INFO for the new table; existing legacy GraphQL/function warnings are unchanged. No live user preference or transaction test writes were performed. Application deployment and hosted end-to-end checks remain pending; production latency/large-directory query plans were not load-tested.

Updated 2026-10-26. Local implementation and release preparation are complete under the [redesign plan](data_architecture_redesign_plan.md). This record separates observed findings, executed verification, and rollout. **Update:** the production DB migrations were applied on 2026-10-27 UTC; application deployment remains pending. See [production migration evidence](production_migration_20261027.md).

## Baseline

The source baseline is preserved at `/tmp/futmondo-redesign-baseline-gtos1hyz` (134 source/documentation assets, excluding credentials and HAR captures). The workspace already contained modifications; comparisons use that snapshot rather than assuming the Git base reflects the starting application.

R 4.6.1; Shiny 1.14.0; httr 1.4.8; curl 7.1.0; later 1.4.8; promises 1.5.0. Installed versions describe this test environment, not production.

| Baseline check | Result |
| --- | --- |
| `test/test_player_trend_plot.R` | Passed 16 existing checks. These checks did not establish all new history contracts. |
| `test/test_shiny_simulation.R` | Failed before changes: the selected-player modal-context assertion expected one entry. Log: `/tmp/futmondo-baseline-lifecycle.log`. |

## Read only database inspection

The connected Supabase project was inspected through schema, count, advisory, log-summary, and EXPLAIN queries. No production data or schema was modified.

| Resource | Observed state |
| --- | --- |
| `player_history` | 113,367 rows covering 573 players; 43,317 rows have no source `observed_at`. |
| `player_match_observations` | Zero rows. Stored per-round history cannot currently supply player-card points. |
| Match-history schema | Season, scoring version, round, status, observation time, optional round/start time columns already exist. |
| Player-history query | EXPLAIN uses the player index, filters league separately, then sorts by recorded time and ID. This is a query plan, not a measured runtime. |

Legacy snapshot timestamps cannot establish source observation time. The redesign must retain their provenance and retrieve actual round scores from available API observations.

Performance advisories identify missing foreign-key indexes and some unused indexes. The redesign adds only indexes justified by retained query paths. See [Supabase's index advisory reference](https://supabase.com/docs/guides/database/database-linter?lint=0001_unindexed_foreign_keys).

Security advisories identify legacy automation RPC execution grants and schema visibility. Retirement scripts must remove the obsolete RPCs and isolate retired tables. Service-role-only historical tables intentionally use RLS without browser policies. See [the privileged-function advisory](https://supabase.com/docs/guides/database/database-linter?lint=0028_anon_security_definer_function_executable). Aggregated logs did not expose useful failure codes in the selected fields; they do not establish error-free ingestion.

## Retained screen data paths

| Screen | Principal source reads |
| --- | --- |
| Login and league selection | Authentication, active championships, team identity |
| Today | Financial snapshot, roster, market, pressroom transfers |
| Your Team | Roster, own listings, incoming offers, observed finances |
| Market | Current league/team market listings |
| All Players | League player catalog and verified spending capacity |
| Player card | Player summary/profile/matches, scoped stored prices/round points, manual-action state |
| Rivals | Teams, rosters, pressroom transactions, standings history, observed payments/MVP evidence |
| Classification | Team round results, completed rounds, official dream-team results |
| Round MVPs | Shared official-result loader and saved round results |
| Plots | Catalog, market, pressroom, team rounds, finances, standings history |
| Notifications | Official events/unread state and player links |
| Administration | Authorized retained table inspection and explicit administrative actions |

Shared API caching previously reduced some repeated calls, but refresh cleared the account cache and triggered broad collection. Persistence also fetched rotating player batches unrelated to the current view. The new boundary must reuse demanded observations and remove model/worker collection.

## Implemented design

The retained app uses an explicit resource service, async curl transport, scoped history repository, and a bounded queue that saves acquired observations. Policy and interfaces are documented in [data service](data_service.md) and [persistence](background_worker.md).

The highest-priority correctness findings were empty stored round coverage, cumulative totals treated as per-round scores, undated scores discarded, context/refresh gaps, and optional database/profile failures hiding useful live content. The independent baseline fixture reproduced cumulative values 4/12 displayed as rounds and loss of undated 0/-2. The replacement reconciles partial sources and corrections within league/player/season/scoring context, keeps provisional status, and plots known rounds without dates. Its independent fixture verifies final scores 0/-4/8/-2 with mean 0.5 and a separate newer provisional score of 3.

Efficiency changes remove broad refresh collection, duplicate save-time reads, render-triggered persistence, unbounded cache growth, redundant pending requests, and unrelated inactive-view work. Raw completed observations share account/context caches; pending view resources have session ownership so one logout cannot cancel another session's read. Actual transactions invalidate cash and other affected state. Unknown counts and failed saves remain unknown/failed.

Prediction/automation engines, tabs, workers, optimizer/advice features, Smart Bid, bid-probability placeholders, model-only schema, and their active documentation/tests are removed. The roadmap and `Agents.md` explicitly exclude their future development. Factual arithmetic and explicit manual actions remain. Historical applied migrations and an isolated legacy-schema fixture remain as upgrade evidence; they are excluded from deployment.

## Measurements and budgets

Local R 4.6.1 / Shiny 1.14.0, loopback HTTP fixture, 200 ms response delay. These are controlled measurements of request behavior and event-loop blocking, not production latency or throughput estimates.

| Scenario | Executed result | Budget / interpretation |
| --- | --- | --- |
| Same slow endpoint, synchronous httr baseline (5 samples) | Median 203.2 ms; range 203.0–210.8 ms before return | Demonstrates blocking I/O in the former transport approach. |
| Async dispatch, same endpoint (5 samples) | Median 0.6 ms; range 0.5–28.5 ms before return | Every dispatch below 100 ms; heartbeat ran before all 5 responses. Response completion still includes the 200 ms network delay. |
| Actual card adapters with database HTTP 503 | Cold: 4 reads; warm reopen: 0; explicit refresh: 4 | Single-player fixture budget: summary, scoring metadata, round query and price query. Supplementary profile/match reads are separate. Pagination requires extra pages when present. |
| Concurrent identical read | One actual request for two consumers | Verified at both transport and same-session resource boundaries. |
| Cache retention | Cap/eviction assertions pass at 256 entries / 32 MiB | Admission and oversize rejection tested; this is a data-cache bound, not a bound on total R process memory. |
| Persistence | Duplicate replay and unchanged rounds produce no new writes; point/finality corrections do | Queue bounded at 200 jobs / 16 MiB, 25 per account / 1 MiB per job; 100-row batches. |
| Reactivity | Inactive consumers start no reads; local countdown/navigation and context-isolation checks pass | Delayed socket checks use real curl; UI lifecycle uses synthetic HTTP-blocked adapters. |

Reproduce transport measurements with `Rscript test/benchmark_data_transport.R`. The recorded five async requests transferred 55 body bytes. The separate actual-card transport check dispatched in 25.1 ms. No comparable end-to-end production baseline, p95 load test, or process-memory benchmark was available; no live speedup percentage is claimed.

## Executed verification

All results below were executed centrally. Failures discovered during integration were fixed and the affected suites rerun. Test counts refer to grouped assertions rather than individual expectations.

| Area | Passing evidence |
| --- | --- |
| Core service and transport | `test_data_service.R` (9), `test_async_transport.R` (4), `test_session_resource_ownership.R` (3). |
| Historical correctness | Independent known-answer fixture; `test_player_trend_plot.R` (9), `test_recent_points_average.R` (6), `test_history_persistence.R` (7), `test_player_rating_inputs.R` (4). |
| Factual ingestion | Contracts (12), catalog (5), observation recording (7), background persistence (6), pressroom parent ordering (4). |
| Retained UI/manual behavior | Deterministic fixes (30), UI reliability, market listing (4), notification details (5), debt/countdown (9), factual helpers (7), factual review (5), plots (3), official MVP runtime (12), player profile endpoints (6), scalar/login handling (2). |
| Application lifecycle | `test_shiny_simulation.R`: 12 checks plus all 9 retained tabs; HTTP and live database mutations blocked. |
| Browser | Playwright at 1440, 390 and 360 px: login, 9 tabs, no visible output errors/sideways page overflow, player-card 0/-2 chips and round plot, unavailable-profile fallback and history refresh. Screenshots were visually inspected. |
| Schema | Isolated PGlite fresh/upgrade suite (10 groups) and historical migration suite (6): repeatability, correction/revision retention, unknown times, grants, archive preservation and transactional rollback. |
| Administration/deployment | Factual schema helper checks; regenerated manifest and deployment contract (3) exclude credentials, captures, backups, development files and retired engines. |

The independent score/isolation fixture was authored by Luna and executed by the primary agent. Two Sol 6.1 engineers supplied substantial data-layer/UI implementation and focused regression corrections; the primary agent integrated, executed and reviewed the result. No child-agent success assertion substitutes for the recorded central runs.

Local evidence logs are under `/tmp/futmondo-redesign-checks`; SQL logs are `/tmp/futmondo-factual-schema.log` and `/tmp/futmondo-legacy-migration.log`. Browser captures are `/tmp/futmondo-player-card-{1440,390,360}.png` plus corresponding login/navigation images. Temporary logs/screenshots are not deployment assets.

## Release state and remaining limits

Implementation and local release preparation are complete. Existing historical migration files are byte-identical to the task-start snapshot. Fresh schema plus two forward migrations and rollback instructions are prepared in [database migration](database_migration.md). At local implementation completion, no production schema/data changes, deployment, or live trades had been performed. The subsequent authorized database rollout is complete and verified in [production migration evidence](production_migration_20261027.md); application deployment and live trade verification remain unperformed.

The database rollout is now complete. The application release and hosted environment smoke checks remain pending. PGlite establishes SQL semantics locally, not production locking/concurrency behavior. Explicit administrative counts/schema verification and manual trade preflights remain synchronous; normal factual view reads use the async transport. Pending saves are in memory and can be interrupted by process suspension/logout. API coverage and unknown season/scoring contexts remain honestly partial; no unverified historical backfill is applied.

Private-HAR direct-sale/clause suites could not run because their captures were absent. Credential-dependent live-finance checks were not completed; production transaction verification was deliberately skipped. Offline payload, preflight, listing, uncertainty and context checks passed. The entire legacy test directory was not executed indiscriminately because some scripts issue live requests and other suites covered removed features.

## Availability correction 2026-10-29

User reports: Today showed an unavailable component below cash, missing active-offer/spending totals, and My Team did not show the roster. Offline reproduction identified two separate parsing failures: optional null fields caused market row construction to fail, and empty roster market/bid metadata caused an invalid names assignment. A failed optional club lookup also discarded an otherwise valid roster. The exact private live response was not captured.

The correction retains players with empty optional fields, preserves unknown numeric data as `NA`, keeps valid roster/catalog rows when optional club labels fail, and distinguishes failed responses from verified empty lists. Missing offer amounts remain unverified for manual actions. Today now uses the already-loaded finance snapshot’s team value instead of depending on the league-teams read. The countdown warning explicitly names the missing balance after offers. See [the loader contract](player_data_availability.md).

Verification executed for this correction:

| Check | Result |
| --- | --- |
| Player data availability | 11 grouped checks passed, including actual JSON loaders, optional-field failures, missing offer prices, and cold/warm cache behavior. |
| Existing rating-input regression | 4 grouped checks passed; zero, negative, and incomplete observed scores retained. |
| Today availability | 3 grouped checks passed: unavailable league teams cannot hide the finance value; zero/negative cash and unknown team value remain distinct. |
| Existing debt/countdown regression | 9 grouped checks passed. |
| Required offline application lifecycle | 12 checks plus all 9 retained tabs passed, including My Team rendering, unavailable history, refresh, logout, and blocked HTTP/live mutations. Existing Plotly fixture warnings remain; no assertions failed. |
| Deployment contract | 3 grouped checks passed; changed source checksums refreshed and all 30 intended release-file checksums verified. |

Performance-impact review passed for the changed paths. The roster/market fixture uses 5 existing cold reads; three repeated roster/market/capacity accesses add 0 reads, 0 parses/normalizations, and no cache-entry or retained-byte growth. The Today KPI fixture serves both values with 1 finance read and 0 league-teams reads, with no additional read on repeated output access. This removes that KPI dependency; it does not claim the entire application no longer requests league teams. Club matching cannot multiply player rows and skips the club request if no team IDs exist. Added normalization only traverses the existing response on a cache miss. No retries, timers, background work, cache policies, dependencies, CSS, or mobile layout were added. Documentation and manifest checksum changes have no runtime cost. Full production concurrency, process-memory benchmarks, and new browser/device tests were deliberately not run for this scoped correction; hosted behaviour remains unverified.

Luna investigated shared-table dependencies; Sol 6.1 implemented loader fixes and focused tests; the primary agent reviewed the integrated diff, implemented the Today correction, and ran its checks and the required lifecycle/finance verification. Implementation and validation made no production calls, account transactions, database migrations, or deployments. Local code is ready for deployment; the live incident is not marked resolved.


## Continued availability incident: request contracts and diagnostics — 2026-10-29

The user's new screenshots show the previous warning text and team-value correction in the hosted UI, but Today/My Team availability remains broken. The provided Connect logs show successful multi-page pressroom pagination interleaved with repeated roster, market, and lineup progress lines; no underlying exception or response status was retained. Repeated processing lines are consistent with asynchronous adapter replay and cannot establish duplicate HTTP requests.

A local request inspection confirmed that seven older factual reads serialized scalar credentials, IDs, strings, and booleans as single-element JSON arrays, contrary to the documented HAR request contract. The working team-information read already uses scalar serialization. Clubs, championships, roster, market, catalog, lineup, and league-team reads now use `auto_unbox=TRUE`. This is a confirmed request-contract correction, but the hosted API's rejection reason is still unconfirmed; the provided logs contain no private response or original exception.

Fetch failures now retain an allowlisted source/category, optional HTTP status, and short reference across nested caches and asynchronous replay. UI errors expose only that safe category/reference. Optional failures caught by composite reads remain attributable in safe metadata. Raw messages, calls, bodies, credentials, IDs, and query strings are not copied to the new diagnostics. Adapter-local failure logging is deduplicated and capped at 32 entries; warm reads and repeated rendering do not log again. The misleading per-page pressroom progress line, including its cursor ID, was removed. This is not a repository-wide audit of older logging.

Executed verification:

| Check | Result |
| --- | --- |
| New read diagnostics | 6 groups passed: sanitization, nested cause/reference retention, stale fallback, async failure handling, replay log deduplication, and actual UI error formatting. |
| New request contracts | 4 groups passed: all seven corrected synchronous and asynchronous request shapes, roster/market rows and warm reuse, paginated replay, and preserved pressroom HTTP failure. |
| Existing data service | 9 groups passed, including bounds, coalescing, context isolation, and invalidated generations. |
| Existing session ownership | 3 groups passed: same-session coalescing, independent sessions/logout, cancellation, and stable persistence identity. |
| Existing player availability | 11 groups passed, including zero/negative/missing values, optional enrichment, incomplete money, and cold/warm cache behavior. |
| Existing debt/countdown | 9 groups passed; uncertain acquisition data still fails closed. |
| Existing async transport | 4 groups passed with local HTTP only. Dispatch took 0.0278 seconds for the delayed fixture; heartbeat ran before its response. Cold card reads=4, warm=0, explicit refresh=4. |
| Required offline lifecycle | 12 checks plus all 9 retained tabs passed; no external HTTP or live mutations. Existing Plotly fixture warnings remain. |
| Deployment contract | 3 groups passed. Four runtime checksums refreshed; all 30 staged bundle checksums verified. Exact 10-file scope, credential-pattern scan, and whitespace check passed. |

Performance-impact review passed within fixture limits. The real async roster/market path uses 5 cold requests and 0 warm requests. Three pressroom cursors cause exactly 3 requests and 0 warm requests, despite adapter replay. A repeatedly encountered optional parser failure emits 1 safe log across 3 responses and 0 warm logs. Existing player checks show 0 warm parses and no cache-entry/retained-byte growth. No retries, timers, reactive consumers, request scheduling changes, new dependencies, or CSS changes were introduced. Diagnostic construction is limited to failures and stores bounded small metadata; rendering adds only short error text. Documentation and release checksum updates have no runtime cost. Production load/process-memory and new browser/mobile checks were deliberately not run.

Luna investigated the request contract; Sol 6.1 implemented the correction, diagnostics, and focused fixtures; the primary agent reviewed the actual changes and ran the existing integration/performance checks. No account login, live trade, database change, or Posit deployment was performed. The legacy synchronous HTTP wrapper still obscures its original cause; explicit HTTP/network attribution was verified for the actual asynchronous UI transport. Hosted verification after redeploy remains pending, and this incident is not marked resolved.


## Progressive login, Today, and player cards — 2026-10-30

Authentication now dispatches one asynchronous uncached request. Duplicate attempts are suppressed; logout/session changes discard obsolete replies. Password references are cleared after dispatch. Visible feedback continues through league and Today loading, with a cancellable five-second slow-source notice and failed-section retries. Today balance/team value and market start independently before optional finance, pressroom, history, and notification reads. Promise completion handlers explicitly return NULL from observers so Shiny can flush intermediate UI states; state-only simulation would not establish this browser behavior.

Cards retain the clicked row for identity/statistics and progressively acquire summary, scoring context, stored observations, profile, matches, and passive action eligibility. Completed warm reads are reused; explicit history refresh invalidates the selected underlying source caches. Obsolete consumers stop further adapter pagination. Final manual transaction preflights remain unchanged. Round boxes combine scores/results only with verified identifiers or compatible season/competition context; unknown associations remain visible separately. Zero/negative points are retained, long names wrap on phones, and nonpositive height/weight is omitted.

Finality review confirmed an application bug: absence of a per-player finished flag previously became provisional. Missing, malformed, or conflicting evidence now remains unknown; explicit logical true/false remains final/provisional. Reconciliation and observation dedup preserve this distinction. Historical stored provisional rows are not bulk rewritten. The known flag names are compatibility handling, not a verified live wire contract. Raw HAR files were unavailable in this workspace; the user confirmed they are on another machine and will provide them later. Actual Futmondo flag paths/types and match identifiers therefore remain a follow-up. Neither dates nor completed football matches are used to manufacture final fantasy-score status.

Verification: focused progressive loading 15 groups, login 5, Today 3, profile 6, contracts 13, resource service 10, session ownership 3, observation recording 8, history persistence 7, chart 10, and async transport 4 passed. Independent validation passed eight round-association cases plus finality/reconciliation/recording known-answer cases. The mandatory offline lifecycle passed 12 checks and all nine tabs with zero external HTTP/live database mutations. Existing empty Plotly fixture warnings remain. Browser tests use actual Shiny UI with delayed synthetic sources and block external requests; these establish progressive rendering, not production source latency.

Performance review: fixed resource slots reuse the existing bounded caches and four-connection pool. No new dependencies, database queries/schema, polling, or unattended automation. One cancellable startup timer per session and a 64-sample fixed-label timing ring are bounded. Round association averaged 0.00355 seconds for 1,000 fixture rows; finality decoding averaged 0.0000281 seconds per synthetic point. Documentation and manifest updates have no runtime cost. Local browser timing is reported separately below; hosted cold starts, network latency, concurrency, and production process memory were not measured. No production login, trade, migration, or deployment was performed.

Sol 6.1 implemented runtime changes; Luna independently checked round association and finality; the primary agent reviewed changes and ran integrated browser/resource/release verification. Hosted verification after redeployment remains pending.

Final browser acceptance passed at 1440, 390, and 360 pixels. In-browser mutation/animation-frame measurement recorded cold identity at 349, 342, and 340 ms, respectively; login feedback was 3.6–11.4 ms. Earlier roughly one-second readings included automation locator polling and are not paint timings. Identity precedes slow remote sections; summary points precede profile/history; the chart is created after an identity-paint acknowledgment. All widths showed matched zero/negative scores and wrapped long club names; closing/reopening added zero card reads and all external HTTP remained blocked. These delayed synthetic fixtures demonstrate local rendering behavior, not hosted response-time guarantees. Deployment contract checks passed and all 30 staged runtime bundle checksums were verified. The release excludes unrelated CRLF changes, private files, and captures.


## Current-player round display correction

The screenshot demonstrated that live player points and match results both carried round numbers but lacked the extra identifiers required by the previous join. That rule was too restrictive for current responses already fetched for the same selected player. The card now opts into numeric-round association for live Futmondo points in its current player context. Explicit context/identifier contradictions still block fallback; saved-only history is not broadly joined by round number. The Round 10 regression expects 18 points, Barcelona 1–2 Real Madrid, and 2026-10-25 in one group, with no duplicate unmatched fixture. Existing zero/negative, multi-fixture, ordering, and conflict checks remain.

Performance review: only the pure display join changes, adding bounded vector comparisons to at most five displayed point groups. No API/database calls, cache changes, timers, reactive consumers, dependencies, or persistence writes are added. Existing CSS and finality handling remain unchanged. Ten round-association checks and ten chart/display checks passed; the full lifecycle suite is deliberately not repeated for this localized display change. Browser checks use missing optional metadata to reproduce this incident. No hosted verification or HAR finality verification is claimed. Implementation and review were performed directly, without delegation.

Browser validation passed at 1440/390/360 px with metadata-free current responses: matched round boxes, long-name wrapping, zero warm reads, and zero external HTTP. Identity paint on the complete run measured 350/360/326 ms. An initial process-cold attempt exceeded the 500 ms timing threshold at 619 ms; the subsequent complete run passed, so the half-second figure is not a guaranteed cold-process bound. All 30 staged deployment hashes and scoped whitespace checks passed.


## Average last N: observed-score loading correction

The configurable column skipped history unless season and scoring were known, required a completed-round catalog, and filtered out all scores without explicit final status. N=5 alone could use the API aggregate fallback. The table now reads observed scores in its exact context (including the separate unknown scope), without a finality filter or latest-round window; the unchanged arithmetic sorts each player's recorded rounds newest first. This also avoids mistaking the latest N league rounds for the player's latest N recorded games.

Performance review: one bounded existing shared history read replaces the gated/windowed query; the 5,000-row safety cap remains. Removing the range can read more historical rows on a cold load, but N changes now reuse the same query instead of requesting another range. There are no per-player API requests, new dependencies, timers, writes, or cache-policy changes. Known-answer checks cover (18+19+7)/3 and negative/zero points; the actual table fixture verifies one read and zero additional reads when N changes. Unknown context remains isolated. No fitness ordering, round scores, or finality is fabricated. Players without recorded scores still lack arbitrary-N averages; raw HAR verification remains pending as previously agreed. This correction does not claim universal coverage on the hosted table. Work and review were performed directly without delegation.

Verification passed: eight focused average assertions/groups, plus the mandatory offline lifecycle (13 checks and nine tabs), including actual table recalculation with unknown context/finality and one cached read across N=3 to N=2. No external HTTP or live database mutations. Existing empty Plotly fixture warnings remain. Browser layout was deliberately not repeated because no markup/CSS changes were made. Hosted coverage and fitness-array order remain unverified.


## Consolidate Market into Today

Removed the duplicate Market sidebar item, tab, and server instance. Today now exposes the existing Your Bids Only and Bidded Only controls; the same shared table and manual player actions remain. Performance review: fewer module observers/outputs are instantiated; the transferred filters only subset existing rows. No extra API/database requests, cache changes, timers, dependencies, or writes. Documentation has no runtime cost. Direct implementation and review, no delegation. Mandatory offline lifecycle results are recorded below; browser screenshots are deliberately not repeated for this standard shared-control/navigation change. Hosted verification is pending.

Mandatory lifecycle passed: 13 checks and all eight retained tabs, zero external HTTP/live database mutation attempts. Existing empty Plotly fixture warnings remain. Deployment hashes were refreshed from staged runtime bytes.


## Older card rounds incorrectly reported as unassociated

The builder previously truncated points to five rows before pairing fixtures, then displayed older fixtures as unassociated even when their points were already loaded. It now associates first and limits displayed groups afterward. The player card requests up to 50 point groups. No database reads/writes or data contracts change. Persistence records retrieved observations; this display correction neither proves production coverage nor backfills unobserved rounds.

Eleven independent known-answer association cases and ten chart/display checks passed, including ten available rounds, a hidden-but-paired older round, and one genuinely absent score. Performance review: no additional API/database requests, cache changes, reactive consumers, timers, or dependencies; more loaded rows can be paired/rendered. A local 50-round/1,000-fixture benchmark averaged 0.00985 seconds across 20 runs. Display is bounded at 50 groups; existing acquisition bounds remain. Full lifecycle/browser checks were deliberately not repeated for this localized pure display-limit correction. Hosted records and rendering remain unverified. Implemented and reviewed directly, without delegation.


## League-wide round matrix and first-time retrieval

Replaced per-table historical-score loading with the latest player/round observation view and active-session league catalog fill. Exact N uses consecutive league rounds, includes zero/negative scores, and exposes incomplete coverage rather than dividing a partial set by N. N=5 source aggregates are explicitly labeled reported. Latest NULL revisions suppress older numeric scores and remain part of the round window; persistence detects NULL corrections without repeatedly saving unchanged NULLs.

Performance review: one logical paginated DB load (50,000-row bound), a shared catalog/lineup read, at most two pending player summaries, and one completion reread. The finite fill prioritizes displayed players and caps the catalog at 2,000 IDs. Queue backpressure uses write-completion signals, without a polling timer. Cached N changes perform zero IO; normalized matrix rows are grouped by player. The engineer measured a synthetic 500-player × 38-round average calculation at 0.058 seconds locally; this is not a hosted latency guarantee. Initial full history acquisition costs more API calls but persists useful observations and preserves responsive scheduling. Unknown context or genuinely absent rounds can still require future source checks; no unsupported endpoint or fitness-array ordering is guessed.

The Supabase migration and rolled-back live SQL assertions passed, including grants and query-index use. Independent reader pagination and exact-N/NULL arithmetic checks passed. Parent history/persistence (seven groups), card association (eleven cases), and observation recording (nine groups) passed. Sol 6.1 implemented the controller; Luna independently validated SQL, pagination, arithmetic and NULL behavior; primary review covered integration and meaningful diffs. Mandatory offline lifecycle passed: 13 checks and all eight tabs, with zero external HTTP/live mutation attempts. Focused async/controller checks passed 22/22, arithmetic checks 9/9, and independent matrix checks 6/6. Browser screenshots were deliberately not repeated; no layout/CSS change was introduced. Live hosted backfill and mobile timing are not verified; no authenticated Futmondo backfill was run from this workspace.


## Shared balance and verified banner warnings — 2026-10-30

The screenshot showed a known Today balance with an unavailable, red global banner. Two code paths caused the mismatch: the banner obtained all fields through the expensive composite `get_financial_snapshot` and discarded its entire result on error, while Today loaded team information independently; the banner classified an unknown projected balance as a financial warning. The exact hosted composite exception was not captured and is not claimed to be established.

The server now owns shared account/league/team team-information and market-listings states and passes those exact states plus targeted retry functions to Today. A pure `round_finance_display` calculation retains available cash even if offers are loading/unavailable and uses `max(withheld,outgoing offers)` once. Red requires fresh confirmed nonpositive cash or a verified nonpositive projected balance. Partial/stale/loading/failed/missing data is neutral, stale cash is labeled, and missing held funds never become zero. The banner no longer traverses roster, paginated pressroom, player summaries, lineup, or full acquisition capacity. The full financial snapshot and final manual transaction preflights remain in their existing action paths. The market loader explicitly normalizes absent-own-bid columns for valid responses; malformed nonempty frames stay unverified. Banner items wrap on phones, and a missing schedule no longer hides available finances.

Executed checks:

- Debt/countdown/banner regression: 13 groups passed, including retained debt/preflight boundaries, independent cash, zero/negative cash, the greater held reservation, missing withheld, stale and malformed inputs, and actual no-own-bid market JSON normalization with warm reuse.
- Actual-server banner integration: 10 groups passed. Exactly one team-information and one market read serve Today and banner; known cash renders while market is pending and after its failure; three clock ticks and navigation add zero primary/composite reads; targeted retry fetches only failed market; a verified empty market proves zero offers; confirmed debt warns; league changes clear old cash. All HTTP was blocked.
- Existing progressive loading: 15 groups passed. Existing player-data/market/acquisition availability: 11 groups passed.
- Required offline lifecycle: 13 checks and all eight tabs passed with zero external HTTP/live database mutations. Existing Plotly fixture warnings remain.
- Primary-agent browser validation passed at 360 and 390 pixels with delayed synthetic reads, known 180,546,496 cash, a deliberately failing composite finance function, and both normalized no-own-bid and failed-market responses. Cash stayed visible, uncertain fields were neutral, and banner finance/status text stayed within its bounds. These are local UI checks, not hosted API latency measurements.

Performance-impact review: the banner shares the two primary requests already needed by Today and adds zero composite requests; its one 60-second active-session refresh version serves the same two states across views. A separately cached pure calculation runs only when those states change, not on the one-second clock. A synthetic 524-market-row calculation averaged 0.000167 seconds across 1,000 local iterations. Normalization adds two absent-column assignments on a market cache miss. The existing bounded resource registry/cache and transport are reused; no dependencies, database/schema queries, processes, or unattended workers were added. Display-only CSS/docs have no acquisition cost. Production concurrency, memory, authentic account payloads, and deployed behavior remain unverified. Sol 6.1 implemented the fix and focused checks; the primary agent reviewed integration and performed mobile browser validation.


## Round-history counter blinking and table redraws — 2026-10-30

Every acquired player previously invalidated the entire reconciled matrix, table filters, and `renderReactable`; the progress used `renderUI`, replacing its markup for each update. Leaving the view cleared all acquired histories. Stored-player reuse additionally required every round from one through the league maximum and known context, so valid gaps and unknown season identity could replay the whole catalog.

The controller now retains acquired, empty, and failed results across same-context view navigation, invalidates only obsolete pending generations, and clears acquisition state on identity change or explicit refresh. During an explicit refresh pass, current table IDs bypass saved-row reuse so corrections or missing scores can be retried; other catalog players still reuse matching recent history. This flag survives temporary inactivity during primary-page loading. `recent_points_saved_players` filters exact league/season/scoring context, accepts fresh observations (24 hours known context, one hour unknown), and requires an observation for the latest round established by a successful current probe. It does not infer a season, complete earlier gaps, replace NULL with zero, or call a partial matrix complete. An incomplete latest-round DB fill cannot skip another player still missing that round.

Acquisition/progress continues independently of a table-player-only history snapshot. Ordinary snapshots publish at most once every five seconds; initial/context/table changes and settlement of the visible player group publish immediately. Off-table completions with an unchanged round anchor publish no table changes. A new off-table round still advances the average window. The one publication timer is cancelled on inactivity/session close; no polling timer or unattended work is added. Progress is stable text below the average input with wrapping and scoped recalculation opacity, and no longer repeats a long explanatory paragraph.

Executed verification: controller/async 33 checks passed; arithmetic nine checks passed; mandatory offline lifecycle 13 checks plus all eight tabs passed with zero external HTTP/live database mutations. The focused tests establish zero table invalidations for 28 off-table completions, only two publications for 30 visible responses, bounded timer delivery, navigation/empty-result retention, scope/freshness isolation, partial latest-round DB coverage, off-table new-round advancement, a refresh pass that retries current table IDs while reusing other catalog players even across temporary inactivity, and existing N3/N8/signed/NULL/backpressure/close behavior. The unchanged synthetic 500-player × 38-round average calculation measured 0.045 seconds locally. Existing empty Plotly fixture warnings remain in lifecycle output.

Primary-agent actual Shiny browser verification used an identical 83-player league with three table players and a 15-second sample: before, widget updates increased from four to 76 (72 additional) while retrieval reached 71/83; after, updates increased from two to four (two additional) and all 83 finished. The stable text output has recalculation dimming disabled; sampled progress opacity was one and text remained inside a 390-pixel viewport. Continuous opacity sampling was not performed. These are delayed synthetic-source checks, not hosted response-time or persistence-success claims.

Performance review: maximum two concurrent player requests, 2,000-player catalog, existing bounded cache/write queue, one initial logical DB batch and one final reread remain. The controller's normalized acquisition list survives navigation independently of the 256-entry shared cache. Per-response full-table reconstruction is removed; reconciliation runs only for table players on publication. Observation freshness may still require source checks after expiry, and an API-omitted latest round may be checked in a new controller/session. Saved timestamps describe changed persisted observations rather than an empty/completed-fetch marker; no permanent complete-archive claim is made. No migration/dependency/environment changes. Sol 6.1 implemented the controller, focused checks, CSS, and documentation; primary review and browser validation are separate. Authenticated Futmondo retrieval, production memory/concurrency, save acknowledgment, and Posit deployment remain unverified.

## Shared database-first round dataset — 2026-10-30

The earlier fix still used one acquisition controller per player-table module and required live probes before accepting stored rows. This caused repeated player requests across pages despite a populated points table. The replacement uses a single root-session controller and one paginated league read containing both latest score revisions and successful fetch metadata. No source probe or every-round coverage test is required to reuse fresh saved history. Sparse/NULL observations remain unknown for averages, while a successful empty response is retained as a distinct source check.

The applied migration adds immutable successful checks and a server-only union view. Source scores/checks are shared factual league data; account-specific active runtime entries and pending API resources remain isolated. [Production SQL assertions, grants, migration version, and rollback](round_dataset_migration_20261030.md) are recorded separately from app deployment. Existing observations are not rewritten or relabeled as complete acquisitions.

| Verification executed | Result |
| --- | --- |
| `test/test_recent_points_async.R` | 22 groups passed: one shared league read, bounded two-request acquisition, local N=3/N=8 values, no I/O on N changes, marker acknowledgement/failure behavior, fresh sparse legacy reuse, empty-check reuse in a new session, cross-page module namespace sharing, targeted/inactive Refresh, logout with guarded league reactives, account replacement and obsolete callbacks, write backpressure, and stable off-table publication. |
| `test/test_round_dataset_reader.R` | Four groups passed: all points/checks despite a 700-row REST cap, explicit empty-player check, NULL/zero/negative scores, partial row cap, complete empty dataset, and unavailable database distinguished from empty. |
| `test/test_observation_recording.R` | Nine existing groups passed, including parent-before-child saving, unknown-score corrections, and deduplication. |
| `test/test_round_matrix_independent.R` | Six existing groups passed, including pagination, exact N=3/N=8 arithmetic, latest NULL revision, and NULL transitions during persistence. |
| `test/test_shared_round_dataset_independent.R` | Independent validation passed 10 groups: valid empty versus missing/malformed/stale source payload, source checked_at provenance, exact context isolation/account API cache, ordered 100/100/5 score chunks followed by marker, no marker on failure, and all coalesced queued/active callbacks settled unsuccessfully on cancellation. |
| `test/test_shiny_simulation.R` | Final fixture with acknowledged saving passed 13 lifecycle checks plus all eight tabs; no HTTP or live mutation attempts. |

Performance review: fresh saved points/checks need zero per-player API calls in the focused fixture, including after opening a different module namespace or a new root session. Changing N performs local arithmetic only. The 500-player × 38-round N=8 mean took 0.056 seconds. Twenty-eight off-table response completions caused zero additional shared matrix publications within the bounded publication interval. Progress and write completion invalidate their own state; they do not reconstruct a table for each acquired player. At most two API summaries are pending, acquisition yields when 20 owner write jobs are pending, catalog IDs are capped at 2,000, and retained stored/live score rows across at most two root entries are capped at 50,000.

Known context uses a 24-hour freshness window, unknown season/scoring one hour. Fresh legacy observations receive bounded reuse without falsely creating successful-fetch metadata. Once this window expires an active session may check stale histories again. A success check records the response's source observation time, including a cached response's original time; it never claims every requested round exists. Exact partial averages remain unavailable until all requested round scores exist. Cancellation may add an account-level diagnostic through the existing persistence error mechanism, while round-table messages track only their own dataset's failures.

The app uses existing server credentials. Hosted deployment/latency, actual current API shapes, and a complete authenticated catalog backfill have not been verified by these local fixtures. Database migration success does not establish app deployment or full source coverage.

Primary final browser check of the actual Shiny app at 390 pixels used 83 synthetic player histories with acknowledged fixture persistence: the table rendered once initially and twice more over 15 seconds; all loading/saving text cleared, text did not overflow, and sampled opacity was one. This verifies rendering and fixture completion, not hosted API latency or real persistence. Sol 6.1 implemented the change, Luna independently validated source/persistence boundaries, and the primary agent reviewed the runtime diff, live migration, and browser behavior.


## Persist verified source last-five scores — 2026-10-30

The table displayed a last-five source vector while fresh sparse legacy history still lacked one of the requested four rounds. The shared controller now retains minimal fitness evidence from active table rows and the existing catalog. A complete matching saved window skips work; otherwise one qualifying source probe can establish an exact snapshot's unique latest explicit round. The confirmed vector `2,5,12,12,6` with anchor 10 maps to rounds 6–10 and yields 8.75 for N=4 and 10 for N=3. A database maximum, upcoming round, repeated source pattern, or shorter late-season vector cannot authorize this mapping.

Fitness observations use their original source time, preserve unavailable/zero/negative positions, skip unchanged values and equal/newer corrections, and save in existing acknowledged 100-row observation-only jobs. They never create successful full-history-fetch metadata. Failed chunks retry only on explicit Refresh. The normal endpoint path retains score-first/marker-last checks, and N>5 remains eligible for older-history retrieval. The dynamic average column now appears immediately after Name, renders only numeric values or blank unavailable cells, and defaults all shared tables to descending average order with blanks last. The numeric source column remains sortable; user-selected sorting remains available.

Verification executed: the focused `Rscript test/test_fitness_round_snapshot.R` covers known-answer mapping/averages, stale/older/repeated/constant/context-conflicting anchors, missing timestamps, NULL/negative/zero and early-season lengths, unchanged/newer corrections, bounded replay buffers, a fresh partial legacy matrix repaired with one probe, the entire catalog's observation-only chunk writes and source timestamps, ACK/failure/retry, navigation/N reuse, N>5 acquisition, complete saved-window reuse in a new session, and an empty response that cannot fabricate scores. Existing shared-controller checks passed 22 groups; mandatory offline lifecycle passed 13 checks plus all eight tabs, with zero external HTTP/live mutation attempts. Existing synthetic empty-Plotly/schema fixture warnings remain. The final focused mapping/controller test passed 14 groups; `test/test_recent_points_average.R` passed ten arithmetic/display groups, including numeric zero/negative cells, blank NA, and missing-values-last configuration.

Performance review: a single probe repairs the same snapshot's catalog rows while API concurrency remains two. Repeated unchanged snapshots issue no writes; N<=5 changes and unchanged page navigation issue no reads. Fitness mapping runs on snapshot/anchor changes, rather than each unrelated player response. Buffers deduplicate by full context/round and cap pending and failed rows separately at 10,000; queue submission observes the existing owner/global budgets. Root dataset and table publication bounds are retained. The isolated 500-player × five-round acquisition/map/merge/queue/publication fixture took 1.715 seconds after vectorizing score construction and splitting incoming players once (the earlier allocation-heavy candidate took 1.999 seconds). A targeted profile covering initial load and one repair measured 0.71 seconds in fitness preparation, including 0.29 seconds in live merge; the existing shared matrix assembly took 1.83 seconds across those publications. These overlapping profile totals are not an end-to-end before/after baseline. They identify the remaining publication cost without rewriting reconciliation. These synthetic local timings do not establish hosted latency. The primary actual-Shiny browser fixture at 390 pixels used 20 synthetic players and a properly partial saved matrix: one league read, one source probe, N=4 displayed 8.8 (rounded from 8.75), no remaining 3/4 label, three total widget updates, cleared status text, Name then Avg Last 4 headers, and no horizontal text overflow.

Sol 6.1 implemented the module, focused checks and these integrated documents; Luna independently validates mapping/persistence boundaries and Astra owns final integration review. No full suite was run. No schema/dependency changes, actual HAR validation, authenticated Futmondo retrieval, live database write, hosted timing, or deployment was performed. Missing timestamps or an ambiguous source anchor leave the fitness mapping unresolved and retain the existing honest history path.

Final primary browser acceptance used four visible players with averages 8.75, 3.5, zero, and unavailable. At 390 pixels the initial order was 8.8, 3.5, 0, blank; clicking the average header produced 0, 3.5, 8.8, blank. Name and Avg Last 4 were the first data columns. No coverage or reported labels appeared in average cells. The shared fixture made one league dataset read and one round-summary probe, rendered three total table updates, cleared progress, and made no live HTTP/database calls. Luna independently reproduced and then verified rejection of late short vectors and repeated-window ambiguity, plus context/snapshot isolation. Available metadata still cannot prove two separately fetched responses represent an identical upstream moment; ambiguous mappings are rejected, and hosted behavior remains unverified.

## Rival cash accounting — 2026-10-31

Replaced the Rivals synchronous all-team financial calculation and MVP-only supplementation with a root-session, saved-first asynchronous ledger. Cash estimates retain source identities, corrected movements replace earlier revisions, unknown rewards remain qualified, and season/account boundaries are enforced. Full XI snapshots replace prior round selections; nullable points remain unknown. The OHY CAMPEÓN-only €300M opening fallback has no table column.

Performance review: one logical latest-evidence read, bounded two-team request concurrency, one-round award concurrency, source-keyed cache reuse across navigation, 20,000 retained evidence identities, two-second interim publication coalescing and ten-row detail pagination. Persistence is sequential 50-row batches through existing queue limits; persistence acknowledgements do not redraw tables. No new dependencies, services or unattended workers. Focused delayed-promise tests verify event-loop heartbeat and two requests maximum; independent accounting checks cover high-risk transformations. Mandatory offline lifecycle passed (13 checks and 8 tabs); final focused/independent results are reported with delivery. Live Futmondo/hosted Posit behavior was not tested, and historical eligibility/payment contracts remain explicit coverage limits. See [rival_cash_accounting.md](rival_cash_accounting.md) and [database migration verification](rival_cash_migration_verification.md).

## Coach-on-login planning — 2026-10-31

Inspected the user-supplied private `hirecoach` network capture without replaying requests. Confirmed POST hirecoach query type `coach`, null roundId, `api.general.ok`, and team-information staff.coach transition 0 to 1. The action response itself retains coach=0, so the plan requires a fresh confirmation read. Added a planned, user-authorized scope exception and default-enabled Today checkbox with remembered opt-out. No runtime changes, network mutations, database changes or deployment were performed. Performance review: documentation-only change has no runtime impact; the implementation acceptance criteria require shared-read reuse, bounded asynchronous action/confirmation, no automatic mutation replay and no table redraws. Runtime tests deliberately skipped for this plan-only update. No delegation.

## Admin round-awards planning — 2026-10-31

Added the user-requested Admin coverage/retrieval/save-confirmation workflow and rival reward reconciliation to the plan. Evidence: supplied successful dreamteam response contains 11 players, a matching MVP ID, scoring configuration and request round identity; user confirms the round is unfinished. Finality and settlement are deliberately separate from successful retrieval/storage. Nightmareteam semantics remain unverified. Performance review: documentation-only change has no runtime cost; planned acceptance bounds concurrent round reads, uses shared bulk coverage and separates progress from table publication. Scoped diff/whitespace checks passed; runtime tests deliberately skipped. No delegation, live requests, database changes or implementation in this update.

## Login coach and Admin round awards — 2026-10-31

Local implementation reuses a fresh shared team-information response, waits for the browser preference handshake, guards the own-team login mutation, and confirms staff state with one targeted information read. A narrow process-local five-minute/128-key guard also prevents concurrent same-worker submissions. Cross-worker idempotency is not established by the captured API. Browser-local preference read failures disable this login's action and write failures are visible. No real hire, token replay or hosted deployment was performed.

Admin now loads account/league/season-scoped saved full selections once, joins the catalog locally, and retrieves missing/provisional/selected rounds only after an explicit click. Successful responses must contain eleven unique players and one matching MVP. Explicit future rounds are Not due. Fetched, Saving, Saved and Save failed are separate; database callbacks, not enqueue success, acknowledge Saved. Failed saves retry the same immutable revision without another source query. Whole revisions replace membership; provisional/unknown-finality awards cannot enter credited rival income. New metadata retains formation, selected position, pointSystem/custom fingerprint and source time. Known expected scoring comes from the already-fetched active championship; incompatible saved snapshots are not reused. The existing JSON evidence schema requires no migration.

Executed verification:

- `Rscript test/test_admin_awards_coach.R`: eight focused groups passed, covering envelope account/league/season/scoring isolation, strict-XI/finality, unfinalized cash exclusion, captured coach envelope and scoped preferences, opt-out/duplicate protection/unknown/stale/unsupported checks, logout callback isolation, two concurrent explicit round reads, no automatic navigation backfill, future-round coverage, durable-save failure and API-free retry, and league replacement.
- `Rscript test/test_mvp_runtime.R`: ten parser/catalog/eligibility/payment/invalidation groups passed. Prior shared-source expectations that silently fetched missing awards and treated failed persistence as complete were replaced by the acknowledged explicit-controller fixture above.
- `Rscript test/test_rival_finance_runtime.R`: nine accounting/shared-source groups passed, including revised selections, separate XI/MVP components, payment replacement, partial points settlements and no duplicate team-round income.
- `Rscript test/test_progressive_loading.R`: fifteen groups passed after the per-context Today-settled latch keeps coach confirmation from suspending optional player tables or reopening initial loading.
- Mandatory `Rscript test/test_shiny_simulation.R`: thirteen lifecycle checks plus eight pages passed with zero external HTTP or live database mutation attempts. Existing synthetic empty-Plotly/schema warnings remain.
- `Rscript test/benchmark_data_transport.R`: five actual localhost 200ms responses; all five heartbeat callbacks ran before completion. Synchronous median blocking was 0.2034s, asynchronous median dispatch 0.0006s (maximum 0.0237s).
- `Rscript test/test_async_transport.R`: first two actual-socket heartbeat/coalescing groups passed; the subsequent legacy player-card zero/negative score-order assertion failed. The primary independently reproduced that same failure using unchanged HEAD runtime in an isolated directory, so it is an existing unrelated regression; that player-history path was not changed here.

Performance review: no added packages, subprocesses, hosted workers or schedules. Login normally reuses its initial information read; absent coach work adds at most one mutation plus one confirmation read. Confirmation invalidates only team/staff/balance; settled Today content and player-table optional readiness stay stable. Round acquisition caps requests at two concurrent, yields under write backpressure, caps explicit batches at 120 rounds, and reuses the existing globally bounded persistence queue. Table/progress publication is capped at two-second intervals or completion, with no per-player requests. Leaving Admin cancels launch of queued work even when another award-display consumer is active. No production throughput or process-memory benchmark is claimed.

The successful supplied dreamteam response is for an unfinished round. Authenticated completed-round retrieval, exact historical rival eligibility, ranking allocation and full payment coverage remain unverified source gaps; saved XI lists do not establish exact rival cash. Primary mobile/browser acceptance, manifest inspection and delivery results are recorded by the primary agent separately.

Primary acceptance: the actual Shiny UI passed a 390px mobile Chromium flow with offline delayed adapters: default-enabled coach hired once, two team reads, two round requests and two acknowledged saves, zero external HTTP attempts and zero JavaScript errors. A reload/new login preserved the disabled browser preference. The supplied unfinished-round dreamteam response also parsed offline as eleven players, one MVP, and provisional finality. Live Supabase read-only schema inspection confirmed the existing evidence JSON payload supports the metadata; no migration or live write was required.

Offer column label: renamed Received Offer to Offer. Scoped diff verified; performance review found no change to requests, caching, blocking work, reactive updates, timers or memory, and only shorter header text in desktop/mobile rendering. Runtime tests deliberately skipped for this label-only change.

## 2026-10-31 — Stop repeated table reloads and repair award catalog retrieval

The supplied runtime log exposed two separate paths: a minute-based root finance/schedule invalidation kept demanding market/team reads while Admin was open, and each published history snapshot rebuilt the player Reactable. The new clock regression failed against pre-fix HEAD (`b9849b2`) after advancing 180,000 ms and passed with the new server. The timer now changes only countdown text. Explicit global Refresh, targeted retries and transaction invalidation continue to fetch the relevant primary states; navigation reuses cached root primary content.

Average-only changes now update the existing widget using a JavaScript average cell, preserving numeric source sorting, search, filters and page. A browser check caught that general `updateReactable(data=...)` leaves static R-rendered cell content stale; proxy updates are therefore limited to averages. Other visible field changes regenerate their R-formatted widget payload, including prices/offers. Numeric integer/double storage changes share a schema, and source timestamp/hidden coverage changes send no widget update. Inactive history clients no longer demand a player source. One-time backfill progress may still advance while work is running; it is independent of the table output.

The supplied coach capture also explains `Round NA`: league/list contains 38 ID/status objects without numbers; activechampionships supplies only one detailed round. The captured current #12 is incorrectly labelled `future` in the compact list. Compact future/next statuses now imply neither provisional finality nor Not due. Unknown numbers use distinct official-ID labels. On explicit Admin retrieval only, missing numbers are read through cached match/list queries and accepted only when the returned round ID exactly matches the request. Unique scoped saved numbers are reused. A verified later number relative to the single active-round anchor is Not due and avoids the dreamteam request. Missing/unknown finality remains excluded from credited rival income. First-save row replacement now handles an empty data frame without the missing `round_id` warning.

Executed local checks:

- `Rscript test/test_refresh_stability.R`: 11 checks passed. Includes three-minute virtual idle intervals in Today, Admin and Round MVPs with no new market/team/round/hidden-history requests, explicit-refresh reads, progress-only/no-visible-change equality, changed averages without remount, fresh R-rendered values after other changes, and hidden-client gating. Zero HTTP attempts.
- `Rscript test/test_admin_awards_coach.R`: 11 groups passed. Includes the 38-object compact catalog shape, stale-future/unknown finality, strict schedule identity, saved-number reuse, bounded explicit historical acquisition, acknowledged saving, verified later-round exclusion and mismatch rejection.
- `Rscript test/test_mvp_runtime.R`: 10 groups passed; the obsolete compact-future-is-confirmed-unfinished assertions were corrected.
- `Rscript test/test_recent_points_average.R`: 10 checks passed, including actual Node evaluation of the JavaScript formatter for missing, zero, negative and fractional known answers.
- Required `Rscript test/test_shiny_simulation.R`: 13 lifecycle checks plus all eight tabs passed with zero HTTP/live database mutation attempts.
- Primary actual Chromium acceptance: 70 seconds idle in Admin produced no additional team/market requests (two initial/coach-confirmation team reads, one market read); no JavaScript errors. A 45-player history update changed the visible Player 25 average to 125 while retaining the same widget, page 2 and search text. A subsequent value update displayed the fresh formatted 3,000,000 amount.
- Primary actual Admin browser fixture with compact future r1/r12/r13 plus active #12: explicit retrieval resolved all three labels, fetched and confirmed saved two selections, marked verified #13 Not due, and retained unknown finality. Zero JavaScript errors or external HTTP.

Performance review: removes the periodic primary request path and repeated history-driven widget construction. Data equality/signatures are linear in the displayed table, bounded by the existing 2,000-player source limit; the last displayed table and ID vector are retained once. Existing history publication interval, two-read acquisition concurrency, shared cache/queue/memory bounds and manual transaction preflights remain unchanged. Schedule enrichment runs only after an explicit Admin action, with at most two round tasks concurrently; each uncached unknown round adds one metadata read before its existing award read. Future-round metadata can avoid the award read. No polling, dependencies, schema changes, live API mutations or database migrations were introduced.

Verification limits: historical non-null `roundId` acceptance by the live match-list endpoint is unverified; its supplied capture covers the current `roundId:null` response. Identity mismatch or unavailable metadata cannot fabricate a number or finality. Hosted deployment and actual account coverage are not established by these offline/browser fixtures, and saved award selection alone still does not establish historical eligibility or paid cash.

## Missing Rivals directory — 2026-10-31

The ledger completion callbacks previously evaluated the asynchronous team directory directly. A pending Shiny validation condition could abort callback processing before the next task started. A directory/partial-award update arriving within two seconds of publication was also discarded without a trailing flush, leaving the table empty after loading completed.

The directory now resolves in its own observer, publishes selectable names independently of financial/award completeness, and exposes loading/unavailable guidance. Callbacks use resolved snapshots; late directory arrivals enqueue each team once. Unknown amounts stay numeric `NA`, and the accounting calculations are unchanged. Context changes clear old names/data and invalidate callbacks.

- `Rscript test/test_rival_finance_runtime.R`: 10 groups passed, including existing known-answer reward/payment cases and the new delayed-directory, failed saved/seed, unavailable-directory, account-switch, no navigation replay and trailing-publication fixture.
- `Rscript test/test_shiny_simulation.R`: 13 lifecycle checks plus eight tabs passed; zero HTTP or live database mutation attempts.
- Primary actual-app browser fixture: delayed team retrieval by three seconds while finance had no evidence and awards were unavailable. Both fixture teams appeared, unknown cash stayed blank, and selecting the rival opened its details. Zero JavaScript errors/external HTTP; one directory read, one saved read, one seed read and two team observations.

Performance review: no timed data refresh, extra API/database source, per-award/player request, or accounting recomputation on idle was introduced. Team reads retain the existing maximum of two in flight and 100 teams per refresh. At most one cancelable two-second trailing callback is pending, canceled on forced publication, context replacement or session end; no recurring timer or idle network requests. Existing session caches, evidence caps and persistence bounds remain. The directory can render before financial reads complete. No dependency, environment or database migration changes. Hosted deployment and authentic account responses were not verified.


## 2026-10-31 — Rivals selection fallback and financial failure recovery

The continued hosted screenshot has a ready directory/loading-finance status but no visible Reactable. Its precise hosted cause has not been established. A local actual-app Chromium replay of the supplied HAR's ten-team response shape passed table rendering and row selection, both immediately and after spending twelve seconds on Today; there were no JavaScript errors or external HTTP attempts. This is not proof of hosted behavior.

Added a native **Choose a team** selector backed by the existing shared directory. Financial progress does not replace its DOM or resend identical choices. The detailed table now falls back to basic selectable Team/Points rows on a display error, and missing/invalid timestamps and amounts format safely. Source publication retains the directory if the accounting builder throws. Completion processing catches errors inside fulfilled promise callbacks, reports a redacted reference and continues remaining bounded reads; a processing exception cannot silently leave a stale loading status. Status now reports actual team/pending-task counts without claiming a rendered table is available.

Executed focused check: `Rscript test/test_rival_finance_runtime.R` — 13 checks passed. New regressions reproduce a malformed scoped saved team payload that throws in the original pure builder, verify safe directory/unknown-cash fallback with a drained queue, inject a persistence-boundary exception inside a fulfilled seed callback and verify the remaining reads finish, and verify incomplete display-schema fallback plus missing/invalid dates and zero/negative cash. These are synthetic failure fixtures; they do not establish that the hosted account has malformed evidence.

Performance review: no new API/database read, timer, worker, dependency or persistence path. The selector reuses the current directory and requires an explicit team choice before scouting; guarded updates leave it stable across financial publications. Existing two-read concurrency and 100-team caps remain. Fallback rendering is bounded by the displayed directory; error references are cached per failed build and cleared after successful publication or context replacement. No accounting formula or evidence-finality rule changed. Required `Rscript test/test_shiny_simulation.R` passed all 13 lifecycle checks and eight tabs, with zero HTTP/live database mutation attempts. The primary independently passed the actual app at 390px with 800 synthetic transfers: the native selector began empty, deliberately removing the Reactable DOM did not prevent choosing the rival and opening its details, and there were zero JavaScript errors/external HTTP attempts. This flow used one directory read, one saved read, one seed read and two team observations. Live hosted behavior remains unverified.


Selected-rival summary correction (2026-10-31): budget cards reuse the shared league ledger estimate and confidence, with expandable evidence gaps. Position uses the observed API position or total-points rank (ties share rank), explicitly labeled. No additional API/DB requests, timers, caches, or accounting changes; bounded ranking over the league directory and one ledger-row lookup. Historical reward settlement/eligibility integration remains incomplete; this change does not certify estimates or validate hosted data. Focused regression and offline lifecycle checks recorded with delivery.

## 2026-10-31 — Actual rival balance reconstruction

Rival cash now follows the intended accounting result: opening cash plus signed public transfers, published round points, configured round-ranking allocations, historical XI/MVP ownership and known signed adjustments. The numeric league column and summary card identify this as reconstructed cash; a rival's private official balance is optional. Native sources no longer depend on synthetic settlement flags that no retrieval path populated.

Implemented source ingestion: `/1/userteam/rounds` full arrays (`id`, `number`, `points`, `status`, `negative`) and missing completed `/1/userteam/roundlineup` snapshots (`players`, `bench.players`, `negative`, `status`). The public Futmondo production client supplied the contracts; provided HAR captures establish the league configuration and empty preseason round response, not a nonempty authenticated historical response. Full team-round arrays and lineups use typed payloads/namespaced source IDs within the existing account/league/season evidence store, requiring no schema change. Source errors and malformed or partial arrays preserve saved history; explicitly successful empty arrays can replace it after a reset.

Ranking uses the documented all-participant weighted distribution, top/flop direction, fixed three/five-place percentages, JavaScript rounding and tie sharing. Negative-start eligibility from either round history or matching lineup excludes automatic premiums; absent flags are explicit assumptions. Open rounds and cumulative totals are excluded. Recorded bonus identities suppress overlapping estimates, while known negative fees remain signed cash adjustments. Duplicate eligibility is idempotent; conflicting owners or positive/negative eligibility exclude that award and identify its round/player. Revised score/selection signatures invalidate stale lineup evidence and replace prior derived components without duplicate cash.

Executed local verification:

- `Rscript test/test_rival_reconstruction.R`: 14 checks passed. Covers actual source payload/storage names, strict empty-history retention, top-level error-envelope rejection, opening/transfers/points/rank arithmetic, top/flop/ties/fixed slots/rounding, debt and open-round exclusions, XI/MVP and bench source contract, duplicate/conflicting eligibility, changed/removed rounds, bonus overlap, signed fees, native request payload, saved lineup reuse, affected-signature refresh and no navigation refetch. Delayed promises retained at most two composite requests in flight.
- `Rscript test/test_rival_finance_runtime.R`: 14 checks passed, including prior known answers, context isolation, directory/failure recovery, numeric table and selected-rival summaries.
- Mandatory `Rscript test/test_shiny_simulation.R`: all 13 lifecycle checks and eight tabs passed, with zero external HTTP/live database mutation attempts.
- Independent Luna validator: 16 synthetic accounting checks passed plus a separate strict-history-retention check. Independent known answer: €300,000,000 + €5,000,000 sales − €2,000,000 purchases + point/ranking/XI/MVP components = €303,000,280. No production files were changed by validation.

Performance review: one additional full-history read per team is added to the existing information/movement composite, at most 100 teams and two concurrent composites per refresh. There is no per-player request. Missing completed lineups form a one-time, maximum 600-task batch with the same concurrency limit; final snapshots persist and are reused across navigation and sessions. Only changed round/award signatures refresh saved final lineups. The evidence cap (20,000 identities), 120-round per-team parser bound, 256 KiB row limit, sequential 50-row persistence batches, cache policies and two-second UI publication cap remain. Shared history normalization runs once per publication, and the native mobile selector remains stable across finance updates. No polling, new timers, workers, dependencies, environment variables or migrations were added. Runtime manifest checksums were refreshed for the three changed deployed R sources.

Remaining material limits: the precise ownership cutoff for roster-only XI recipients is not established by the available source; starter/bench inclusion proves positive historical ownership but their absence does not prove non-ownership. Missing awarded ownership lists exact round/player IDs. Historical kickoff/ownership intervals are absent from saved round records, so dated public transfers do not yet prove stable ownership through the complete relevant interval. Current reward rules and historical league membership are labeled assumptions. Manual/private adjustments invisible to the public feed remain unknown. An explicitly provisional saved award stays excluded until Admin's existing provisional/selected-round refresh replaces it using current catalog/schedule finality; per-team scores cannot override a genuine postponed-round flag. No live account, hosted deployment, API latency, or production database write was tested by these fixtures.


Admin league settings (2026-10-31): added a read-only, mobile-stacked explanation of starting cash/squad, round rewards and ranking distribution, market auction duration/listing count, and squad/league limits. Explicitly distinguishes API configuration from the user-confirmed opening-cash fallback and missing from zero/false. Only documented fields are displayed. Server passes existing selected league and primary team resource states; no extra API/DB queries, timers, persistence, cache or authorization changes. Rendering is bounded to 17 small setting rows. Focused display checks and mandatory offline lifecycle verification accompany delivery; live configuration/deployment remains unverified.

## 2026-10-31 — Shared reconstructed running cash

The selected rival history previously fetched a separate movement/pressroom response and explicitly left all running balances unavailable. It now consumes the same account/league/season-scoped per-team ledger that sets the reconstructed summary and sortable league amount. Opening cash is applied once; signed transactions/fees and non-overlapping reward components retain source identities. Latest movement corrections replace prior amounts; known pressroom IDs keep the latest input revision, consistent with the existing saved/current seed merge. Distinct transfers without IDs are retained. Equal timestamps use deterministic IDs for display arithmetic while the UI explicitly states that their exact order is unknown.

Actual dated rows each show running cash, including same-minute rows. Undated awards and recorded movements are separate reconciliation components with **Calculation subtotal**; no payment date is manufactured from observation/round/current time. The final cumulative ledger amount is the reconstructed summary, including when observed cash differs. Accessible observed zero/negative cash stays separate with freshness and observed-minus-reconstructed difference; no balancing adjustment is inserted. Missing amounts stay unknown and their excluded contribution is labeled. Transfer cards reuse purchase/sale subtotals, so opening cash/rewards/fees cannot inflate sales or purchase costs. History and pivot reuse already-fetched shared data, and the pivot selection survives ledger publications.

Executed local verification:

- `Rscript test/test_rival_running_balances.R`: 10 checks passed. Independent known answer inside the fixture: 100 opening − 30 purchase + 20 sale − 5 fee + 10 recorded points + 5 XI + 10 MVP = 110. Dated subtotal is 95; undated reward reconciliation ends at 110. Covers every same-time row, subset/sort invariance, correction/removed award replacement, recorded-versus-derived overlap, known/unknown source IDs, missing amounts, zero/debt, explicit opening replacement, undated recorded cash and observed/reconstructed separation. Actual Reactable widgets rendered in both sections. Bulk 1,000-transfer construction measured 0.039 seconds with a 183.9 KiB ledger in this environment; this is local calculation cost, not hosted latency.
- `Rscript test/test_rival_finance_runtime.R`: 14 checks passed, including revised observed/reconstructed zero/debt expectations, context isolation, cached navigation, failure recovery and selected-team cards.
- `Rscript test/test_rival_reconstruction.R`: 14 checks passed, retaining point/rank/XI/MVP known answers, negative/open-round exclusions, source contracts and bounded saved-lineup retrieval behavior.
- Mandatory `Rscript test/test_shiny_simulation.R`: 13 lifecycle checks plus eight tabs passed; zero external HTTP/live database mutation attempts. Log: `/tmp/futmondo-rival-ledger-lifecycle.log` (local development artifact).
- Independent validator passed a separate manual answer: 120 opening − 30 purchase + 20 sale − 5 fee + 10 paid points + 5 XI + 10 MVP = 130; dated subtotal 115 plus undated 15 reconciles to 130. Also checked observed zero/difference −130 without a balancing row, summary transfer subtotals, equal-time ordering, revision replacement, filtered/sorted invariance and source-ID deduplication. Conflicting raw transfer revisions intentionally use the documented latest-input contract, not arbitrary-input-order invariance.

Performance review: the previous extra transaction/summary/pivot movement and pressroom demand paths are removed in favor of the existing shared result. No API/DB fetch, cache invalidation, timer, worker, dependency, persistence or migration is added. One O(N log N) ledger ordering and O(N) accumulation occurs per already-coalesced source publication, retaining existing evidence/team/movement caps. UI projections copy only the selected team's rows and both tables paginate fifteen rows; display filters/sorts do not trigger re-accumulation. The shared pressroom reference is reused for the pivot. Hosted/mobile rendering, actual private adjustments and completeness of production reward/ownership evidence remain unverified. Existing evidence gaps and current-rule assumptions remain visible.


Pending offer-error presentation completed with the running-ledger change: market bids and direct offers translate the squad-cap API code into an actionable explanation, wrap on narrow screens, and remain until dismissed. Unknown codes receive a readable fallback; uncertain outcomes advise checking existing offers before retrying. No submission, clause-buyout, preflight, persistence or network behavior changes. Performance impact is limited to notification text/style. `test/test_offer_error_messages.R` passed; combined offline lifecycle passed. No live offer or hosted/mobile verification was performed.


Inflow breakdown (2026-10-31): transaction history shows total positive inflow split into player sales and bonuses, with other credits separately when present. Starting cash, excluded ledger entries and expenses are not inflow; undated estimates are included and labeled. Missing amounts retain a partial-data notice. The squad investment card now labels sales explicitly. `rivals_inflow_totals()` is a single linear pass over the existing shared ledger, with no API/DB reads, writes, caches or timers. Focused running-balance regression covers known-answer 30 sales + 15 bonuses + 7 other = 52 inflow, exclusions and missing values. No accounting formula changes; full lifecycle/browser/live deployment checks are not rerun for this localized display change.

Outflow breakdown (2026-10-31): inspected saved pressroom captures without exposing private payloads; their news entries do not provide a purchase-method flag. Removed the blanket market classification for rival-to-rival transfers. Added source-category-based expense subtotals and explicit unclassified/other amounts. Focused running-balance tests: 13 passed, including 30 market + 20 clause + 10 unknown + 8 other = 68 outflow and unchanged cash reconciliation. Performance review: linear scans of the existing selected-team ledger only; no added API/DB requests, persistence, cache invalidation, timers or blocking I/O. Existing 1,000-transfer fixture completed in 0.039 seconds (183.9 KiB ledger). Browser/mobile and live deployment verification not performed.
Mandatory offline lifecycle: 13 checks plus eight tabs passed, with zero HTTP/live database mutation attempts. No delegation for this change.

### Remembered browser login — 2026-11-01

Implemented optional seven-day remembered login with encrypted server-only records and host-only `Secure; HttpOnly; SameSite=Lax` cookies. Passwords remain transient. Fixed expiry is bound inside authenticated encryption, including the browser credential hash; restore does not extend it. Same-origin POST, CSRF header, one-use operation tickets, serialized browser requests and generation checks protect issuance/restoration/logout races. Ordinary login works without the enable flag/key; disabled restoration still permits logout cleanup.

Executed locally with synthetic accounts and no live Futmondo calls:

- `Rscript test/test_remember_login.R`: **16 passed**, including actual PostgREST timestamp shape, fractional timestamps, tampering/key mismatch/revocation/expiry, cookie protections, rejected origins/CSRF/GET, missing/invalid cookie, opt-out before endpoint readiness, transient retry, upstream401 rejection, deletion failure/retry, late issuance/account-switch cancellation, and feature-gated fallback.
- The same focused check verifies a delayed asynchronous league validation allows a timer heartbeat, returns normalized league IDs, and seeds the existing cache so startup reuses the single official response.
- `node test/test_remember_login_browser.cjs`: **3 passed**, exercising the actual JavaScript and Shiny HTTP endpoint behind a local HTTPS proxy: cookie protection/issuance, new-session restore with unchanged expiry, and logout followed by signed-out reopen.
- `Rscript test/test_login_feedback.R --browser`: **5 server + 5 browser checks passed**. `Rscript test/test_shiny_simulation.R`: **13 lifecycle checks plus 8 tabs passed**, with no HTTP or live database mutation attempts.
- Regenerated and inspected `rsconnect::writeManifest()` for `sodium`1.4.0 and the new runtime/browser assets; retained unrelated assets/checksums. `Rscript test/test_deployment_contract.R`: **3 passed**. Unrelated working changes remain untouched.

Performance review: an absent/malformed cookie performs no database or upstream API requests. A normal restore performs one indexed hash lookup plus one asynchronous Futmondo league validation whose response is reused for startup; there is no second league request. Issuance adds one insert, deletion of any prior browser credentials, and at most one indexed expired-row deletion per process/day. Logout deletes this browser's record. All network work uses the existing cooperative asynchronous transport. There is no periodic refresh, scheduled worker or account harvesting; the sole new timer is a cancellable one-shot expiry sign-out. The UI adds small status/checkbox controls and a bounded encrypted JSON payload; tables and mobile rendering are not invalidated by persistence progress. Physical expiry cleanup is opportunistic: records remain unusable after seven days, but deletion waits for another issuance or manual server-role cleanup if the application has no new logins.

Production schema evidence supplied by the primary agent: migration `remembered_logins` was applied through Supabase MCP to project `ozhnextrcxxksswpnglt` as version`20261101155112`. Verified RLS enabled, no public policies, no `anon`/`authenticated` SELECT access, server-role CRUD access, primary hash/expiry indexes, and an actual server-role zero-row query. The repository SQL was created using `supabase migration new` before transfer into the existing migrations directory; its generated filename differs from the live MCP migration timestamp. No hosting encryption key, enable flag or publishing credentials were configured by this implementation. Posit Connect Cloud/Android persistence and restart checks remain unperformed; keep production activation disabled until those checks pass. Independent targeted review is tracked separately by the primary agent.

Independent Luna security review found no remaining actionable issue in cookie/CSRF, authenticated encryption, or revocation. Separate known-answer checks passed for timestamp offsets, ciphertext/metadata tampering, cross-row binding, revocation and expiry; reviewer also ran the then-current 14-case focused suite. Its early browser run failed on a fixture asset route; the engineer corrected the fixture and the final three browser checks passed. Primary review checked the integrated fixes, deployment manifest and live database permissions. Sol implemented; Luna reviewed; primary agent reviewed integration. A private deployment-key file was generated outside the repository for the user to configure hosting; it was not printed or committed.

## 2026-11-01 — Reconciled flows, bonus timing and clause evidence

Corrected observed source types before accounting: `clause` is a purchase, `clause_sell` is a sale, and `ranking` is a position bonus whose concept establishes a numeric/split round label. Saved evidence is normalized on use, retaining raw type/category. This fixes missing clause purchase/sale totals, erroneous positive-clause suppression of bonuses, and ranking payments not previously recognized as replacement for estimates.

Starting cash now contributes once to total inflow. Every included positive/negative ledger amount determines the two totals; inflow minus outflow equals final running cash, the selected-rival card and league estimate. Award components are separate position/MVP/Best XI rows; grouped player calculations expand for inspection. Single nonsplit recorded position-payment times anchor reconstructed reward entries under the user-confirmed same-time rule. Split/repeated settlement evidence from any team blocks full-latest-selection dating, preserving undated reconciliation instead of guessed timing. Recorded payment IDs remain separate, and no repeated full reward is manufactured.

Missing historical award eligibility had an implementation cause: the real asynchronous read adapter rejected `/1/userteam/roundlineup`. Both factual read allowlists now include the captured route. The existing signature-aware queue can retrieve/store historical starters/bench; a failed response leaves an explicit evidence gap. It remains bounded to two concurrent reads and 600 tasks, reuses saved snapshots, and does not poll or use today's roster as historical evidence.

Executed local checks (synthetic accounts, no live Futmondo calls):

- `Rscript test/test_rival_cash_reconciliation.R`: **9 passed**. Covers all six actual movement types; raw field preservation; same-time separate bonus rows/player calculations; exact totals/summary reconciliation; recorded round-level MVP replacement and typed rewards missing round IDs suppressing only their potentially overlapping reward type; split-round and cross-team ambiguity; strict unique clause propagation across buyer/seller with <=1 second source-recording jitter; refusal of ambiguous and multi-day matches; numeric unknown-method totals, zero and debt; real `async_adapter_fetch()` → historical lineup getter → mocked HTTP → persistable eligibility, rather than stubbing the getter itself.
- `Rscript test/test_rival_running_balances.R`: **13 passed** with the new opening-inclusive inflow expectation. Existing 1,000-transfer ledger fixture measured **0.042 seconds / 223.9 KiB**.
- `Rscript test/test_rival_reconstruction.R`: **14 passed**. `Rscript test/test_rival_finance_runtime.R`: **14 passed**; its preexisting heartbeat fixture initially failed because one `later::run_now()` may process an unrelated due callback and return before the heartbeat. The fixture now polls within an 80 ms window while its synthetic source is held for 100 ms, directly testing responsiveness without that scheduler race.
- `Rscript test/test_pressroom_player_parents.R`: **5 passed**; ordered parent-before-transfer persistence and failure handling remain intact, with actual import assertions for nullable unknown and explicit TRUE/FALSE classifications.
- Mandatory `Rscript test/test_shiny_simulation.R`: **13 checks plus eight tabs passed**, zero external HTTP/live database mutation attempts. Existing benign Plotly warnings remain.
- `Rscript test/test_deployment_contract.R`: **3 passed**; deployment remains free of test/docs/captures/credentials. `Rscript test/test_supabase_schema_helpers.R`: **3 passed**, using mocked reset/probe transports only. No dependency changes; deployment checksums updated only for the five changed app runtime assets.

Performance review found and repaired an initial new matcher cost: parsing all pressroom timestamps inside every movement comparison took **6.420 seconds** for 1,100 transfers/170 movements. Parsing once per shared publication reduced the same known-answer benchmark to **0.341 seconds**, **342.1 KiB**, retaining all 170 unique expected classifications. The full ledger orders/accumulates once; UI totals are linear scans of the selected existing ledger, and the paginated table adds small type/detail cells. No additional classification API/DB query, cache invalidation, timer, dependency or blocking network work was introduced. The corrected historical lineup queue now performs its previously intended bounded reads on first missing history and reuses persisted signatures thereafter; it is not a periodic refresh. Hosted/mobile timing remains unmeasured.

The primary agent applied and verified the nullable classification migration: remote **20261101164116**, local SQL **20261101163613_rival_cash_classification.sql**. **1,101** rows and financial/identity digest **172596241afefd2a193078f116827327** stayed unchanged; **15 TRUE retained**, **1,086 FALSE → 976 FALSE + 110 NULL**. No amount, date, identity or provenance changed. Existing legacy adoption/replay preserves known methods when the new method is unknown. See `rival_cash_migration_verification.md`.

Remaining evidence limits: exact multi-day clause completion association lacks a verified source contract; unresolved methods remain within numeric totals with separate coverage. Historical roster-only eligibility, opaque combined/manual rewards, missing source amounts and phase-specific split awards cannot be invented. Deployment/hosted Android verification has not been performed; the old deployed importer can still write assumed FALSE until updated. Independent targeted accounting review and primary integration review are tracked by the primary agent.

Independent Luna accounting review passed the final nine-case suite and a separate known-answer ledger: 300,000,160 inflow minus 70 outflow equals 300,000,090 reconstructed cash. Review identified a duplicate-risk for typed bonus payments missing round IDs; the fix retains recorded amounts and suppresses only overlapping reward types. An independent points/ranking case then reconciled 100 + 4 + 7 = 111 with no duplicate estimates. Primary review checked the meaningful diffs, cross-team split-settlement safeguard, unchanged production financial digest and all five staged deployment checksums. Sol implemented, Luna independently validated, and the primary agent reviewed integration. Live Futmondo and hosted/mobile checks were deliberately not performed; HTTP contract verification used a controlled response at the actual transport boundary.

## 2026-11-01 — Player-card acquisition availability

Confirmed and fixed data-path defects rather than changing only the warning:

- Pending transfer reconciliation used logical row subsetting with missing System IDs, producing phantom NA candidates. It also parsed ISO datetimes without an explicit format, dropping time-of-day and potentially selecting an older same-day trade from the descending feed. A shared `fm_time()`/membership scan now selects the true latest trade, considers only players involving the current team, and does not query summaries for obsolete candidates.
- Target-specific acquisition caches and card resource contexts repeated the same team calculation on player switches. Team-scoped capacity is now shared; target bid details are a local projection. Capacity only needs raw roster IDs/value, so it bypasses three full-roster presentation dependencies (club enrichment, incoming roster bids and own listing decoration). Full presentation and facts retain distinct cache entries but reuse one native roster response.
- Derived caches now preserve source freshness: full/facts/raw roster projections expire from the original observation+30 seconds; capacity expires at the earliest prerequisite deadline and at most 15 seconds. Pure preflight rejects an expired result after retrieval. All missing/partial/stale inputs still fail closed; no zero/default or removed safety check is used.
- Loading had both a capacity warning and a spinner containing the same text. Only one visible loading status remains. Partial checks expose allowlisted, actionable missing prerequisites. Global Refresh previously omitted `capacity` from cache invalidation and could retain a cached failed result for 15 seconds; it now invalidates that result too. Actual server regression seeds a failed capacity entry and verifies pressing Refresh removes it.

Executed local checks:

- `Rscript test/test_acquisition_availability.R`: **7 passed**. Exercises the actual async adapter and real info/raw-roster/market/pressroom parsers, five cold factual HTTP reads, no presentation dependency reads, heartbeat responsiveness, zero additional reads across two targets, full-roster reuse without another roster request, System/same-day source chronology, explicit missing-withheld recovery, stale/partial/unknown identity exclusion, inherited source deadlines and post-retrieval expiry rejection.
- `Rscript test/test_progressive_loading.R`: **16 passed**, including exactly one availability loading message and one pending team check across two player cards, existing history refresh isolation, logout cancellation and Today progressive loading.
- `Rscript test/test_player_data_availability.R`: **11 passed**; `Rscript test/test_deterministic_fixes.R`: **30 passed**; `Rscript test/test_debt_and_round_countdown.R`: **13 passed**. The latter's obsolete final static expectation of a removed 60-second financial poll was corrected to the existing user-requested manual-refresh contract; no financial polling was reintroduced.
- Mandatory `Rscript test/test_shiny_simulation.R`: **13 checks plus eight tabs passed**, zero external HTTP/live database mutation attempts. Includes actual server capacity-Refresh invalidation. Existing benign Plotly warnings remain.
- Response-only saved HAR replay through real `async_adapter_fetch(get_team_acquisition_capacity)` returned **status ok**, no diagnostics, **7 reads**: one information, one roster, one market and four pressroom pages. It required no club/roster-offer/listing/individual-summary reads and used synthetic authentication; captured credentials were not replayed or printed. This validates the stored contract, not the current hosted failure.

Performance review: the five-read synthetic cold path with 5 ms delayed mock responses completed in **0.560 seconds**, preserving a heartbeat, with a **9.1 KiB** capacity result; warm target projections performed zero further reads. These are local test measurements, not hosting latency promises. Team-scoped sharing, minimal raw roster facts and corrected latest-trade selection remove repeated network/parse work; snapshots remain bounded by existing cache memory limits. No new timer, polling, dependency, schema change or persistence was added. Existing full pressroom pagination remains necessary for unresolved delayed clauses and can still dominate a genuinely cold check; it uses the existing bounded cache. Actual transaction preflights retain the existing synchronous helper and bounded source freshness; this change does not claim zero-age or nonblocking mutation submission.

The exact live `status: partial` screenshot has not been reproduced: captured information fields and own bid amounts are valid and saved-HAR acquisition succeeds. The fixed local defects are verified, but hosted rollout and checking any remaining concrete failed prerequisite/diagnostic are required before declaring that particular live incident resolved. No live Futmondo calls or transactions were performed.

Independent targeted review (Luna) found no remaining blocker. It ran the final seven acquisition checks and independently verified expiry blocks bid/offer/clause/modify, System inbound/outbound handling, account/team/target isolation and modal/submit calls. `Rscript test/test_deployment_contract.R`: **3 passed**; the four changed app runtime checksums were updated. No dependencies or schema changed; captured responses and development tests remain excluded from deployment.

## 2026-11-01 — Active Clause / Free Agents filter disconnect

The failure was reproduced against the prior committed module in an isolated R process: a native market-shaped row with `isClause=TRUE`, positive `clause_price`, `clause_transferred=FALSE`, and no optional `clause_date` throws `object 'clause_date' not found` in `table_display_data`'s schema observer when the checkboxes are enabled. Shiny destroys that module session. A raw row without derived `isClause` fails similarly. This is a data-contract/filter defect, not a legitimate empty-result condition. Primary-agent inspection of 1,143 captured market/catalog player entries confirmed absent optional lock dates; captured ownership also includes unowned `computer=TRUE` rows and owned `computer=FALSE` rows with only a nonempty owner name.

The fix supplies typed missing filter fields without making up zero/false data or exposing blank placeholder columns. The active-clause mask uses observed price/transfer state and optional lock dates; absent/empty dates mean no supplied lock, while malformed dates and future UTC locks are excluded. Free Agents uses normalized owner ID/name evidence, with a conservative System flag fallback; the dropdown and checkbox agree. All optional filters can operate on missing columns, and genuine empty/zero-match tables render and recover. No blanket exception handler was introduced.

Today now says **Today's market**, describes market players/offers, and initializes only its market table's Free Agents checkbox. Players initializes only its Active Clause checkbox. New UI arguments default FALSE for existing call sites, so My Team and rival tables retain their previous defaults. Clear Filters still clears all filters. These are initial values, not enforced filters.

Executed checks:

- `Rscript test/test_player_table_filters.R`: **6 passed**, using the actual schema observer and Reactable output for absent `clause_date`/`isClause`, all optional checkbox fields, zero matches, genuine empty sources and recovery. Also verifies source-derived clause masks including UTC offsets, owner aliases/names versus conflicting or absent computer flags, and rendered checkbox defaults scoped to Today/Players.
- `Rscript test/test_ui_reliability.R`: **25 checks passed**. Its existing standalone fixture now initializes `cfg_player_columns_to_hide` before the first table-module test (it previously initialized that required global only later); production configuration handling is unchanged.
- Mandatory `Rscript test/test_shiny_simulation.R`: **13 lifecycle checks plus eight tabs passed**, zero external HTTP/live database mutation attempts. Existing benign Plotly warnings remain.

Performance review: the 524-player normalization plus combined clause/free-agent calculation measured **0.002 seconds**, with a **95.1 KiB** normalized fixture. Work is bounded linear scans and typed vectors over the existing source data; no API/DB request, cache invalidation, timer, persistence, dependency or background worker was added. The active-clause helper extracts only the four vectors it needs, and synthetic filter placeholders/internal free-agent flags are omitted before table schema/rendering. Rendering still uses the existing paginated table and shared source states. The Today heading/defaults add no retrieval or recurring work. Hosted Android behavior and deployment have not been verified by these local checks.

`Rscript test/test_deployment_contract.R`: **3 passed**. Only the three changed module asset checksums were updated; no dependency/schema changes or new bundle contents. Scoped diff whitespace check passed. Primary-agent review found no remaining blocker; no separate independent validator was needed for this localized display/filter change.

## 2026-11-01 — Points-to-price columns

Today now shows Avg Last N divided by market value in millions of euros; Players uses clause price in millions. Both numeric sortable columns immediately follow the average, share its exact N/source and leave invalid denominators blank. Existing average-descending sorting remains. Performance review: one vector calculation and numeric column on the existing filtered rows; no I/O, polling, persistence or dependency changes. Ratio-only/history updates use the existing in-place widget update path.

Passed: `test_player_points_ratios.R` (both bases, column order, N/history changes, unchanged widget schema, invalid prices and zero/negative averages); player-table filters 6/6; offline lifecycle 13 checks plus eight tabs; deployment contract 3/3. Primary implementation and review, no delegation. Hosted/mobile rendering was not tested.

## 2026-11-01 — Stable source-wide player metric colors

Avg Last N now uses a continuous red/yellow/green background in every shared player table. Existing points-to-price columns use an independent domain. Each min/max is calculated from all loaded source players before filters/search/pages, includes signed/zero numbers and ignores unavailable/nonfinite values. Equal domains are yellow. The style uses dark text and preserves numeric values, missing blanks and average-descending sorting. Prepared averages/ratios are shared before filters; changing a filter no longer recalculates them.

Executed local checks:

- `Rscript test/test_player_metric_colors.R`: **8 passed**, including the actual generated client JavaScript via Node. Covers exact endpoints/midpoint/interpolation, equal/all-missing/negative ranges, independent ratio bounds, metadata changes, search/sort/page invariance without client row scans and text contrast ≥4.5 across 101 palette samples. Actual module/widget checks verify both styles, initial metadata, no enrichment recomputation on a filter, hidden-player history changing metadata while visible values/widget remain identical, and N/source changes.
- `Rscript test/test_player_points_ratios.R`: both page bases, column order, N/history changes, numeric/invalid denominators and unchanged history-update schema passed. `Rscript test/test_recent_points_average.R`: **10 checks passed**, including actual numeric JavaScript formatting.
- `Rscript test/test_refresh_stability.R`: **11 passed**; in-place history updates retain page/selection, progress/timestamp-only updates do not redraw, inactive tabs do not refetch and explicit Refresh remains effective.
- Mandatory `Rscript test/test_shiny_simulation.R`: **13 checks plus eight tabs passed**, zero external HTTP/live DB mutations; existing benign Plotly warnings remain. `Rscript test/test_deployment_contract.R`: **3 passed**. Only the changed table module's deployment checksum was updated; no dependency/bundle-content changes.

Performance review: a 524-player, 5,240-history-row synthetic fixture measured **51.4 ms** for a complete average+ratio+range publication across 100 iterations; resulting data **54.8 KiB**, metadata **1.6 KiB**. The new two-column range scan alone measured **0.0460 ms** per publication across 10,000 iterations. It runs once per enriched dataset, not per filter or rendered cell. Client styling is constant work per visible metric cell. Metadata-only changes reuse the existing widget and send data+metadata together; page/selection stay intact. No API/DB request, cache invalidation, timer, background work or persistence was added. These are local measurements; hosted/Android visual rendering and live data were deliberately not exercised. Sol 6.1 implemented the change; primary review confirmed the pre-filter domains, independent metadata updates, numeric sorting and scoped diff. Hosted deployment remains separate.

## 2026-11-01 — Mobile metric headers and rounded badges

Primary implementation, no delegation. Replaced full-cell coloring with padded rounded value badges; fixed the theme's forced green text inheritance with dark badge text. Added explicit two-line metric headings and scoped wrapping to override global nowrap/truncation. Source domains, numeric sorting and in-place metadata updates are unchanged. Performance review: one lightweight span per metric cell; no extra calculation, request, timer or table update.

Focused metric colors (8), average formatting/calculation (10) and ratio checks passed. A local real Reactable/Shiny browser fixture with the production CSS passed at 360, 390 and 1200px: dark badge text, rounded padding, uncolored surrounding cells and non-truncated header content. Inspected the 360px screenshot; wide tables retain horizontal scrolling. Hosted Android rendering was not verified.

The mandatory offline lifecycle passed 13 checks plus eight tabs; deployment checks passed 3/3.

## 2026-11-01 — Position color badges

Primary implementation, no delegation. Shared player tables render position labels with padded 7px-radius badges: green goalkeeper, orange defender, blue midfielder, red forward, all with dark text. Primary/secondary position columns reuse the existing position renderer when present. Verified all four shared role mappings and actual CSS rendering at 390px, including colors, text contrast styling, padding and radius. Performance review: existing per-cell badge markup is reused; no requests, timers, refreshes or new dependencies. Hosted rendering was not tested; full lifecycle deliberately skipped for this presentation-only change.

## 2026-11-01 — Total points color scale

Primary implementation, no delegation. Added the existing red/yellow/green rounded numeric badge to Points in every shared player table. Points have an independent full-source domain before filters, preserving numeric sorting and signed/zero/missing handling. Extended metric regressions passed 8/8, checking Points alongside average and ratio in the actual widget and stable source ranges after filtering. Performance review: one additional linear numeric range scan per source update and the existing constant-time visible-cell renderer; no network, database, timer or new refresh work. Full lifecycle deliberately skipped for this small presentation extension; hosted rendering not verified.

## 2026-11-01 — Team crests in player tables

Primary implementation, no delegation. The shared Team column displays a 22px club crest beside the unchanged sortable/filterable team name. Retains source logo metadata as a hidden column and reuses the existing player-card image resolver/CDN; missing images leave the name visible. Verified row-specific source logo/name, hidden metadata, lazy image attributes and missing names; metric suite 8/8 and ratio checks passed. Performance review: small hidden logo strings and one lazy, asynchronously decoded image per rendered Team cell; browser caching can reuse club URLs. No new API/database reads or timers. Hosted image loading not verified; full lifecycle skipped for this localized presentation change.

## 2026-11-01 — Readable Today transfer history

Primary implementation, no delegation. Resolved party display names from the existing team directory and captured names; normalized system parties to Futmondo. Added zero-decimal separated euro prices, Date/Name headers, UTC minute precision, player club crest and evidence-based transfer labels. Inspected saved pressroom HAR schema only: all 268 news entries have `_playerTeam` with club name/slug, but none have a transfer-method flag. Parser now retains historical club metadata. Clause methods reuse the isolated current-context rival ledger when it has already classified matching event IDs; otherwise rival method remains explicitly unconfirmed.

Focused fixtures cover buyer/seller labels, source fallback, system IDs, zero/missing prices, rounding, escaping malicious names, historical crest preference, 20-row limit, explicit true/false/unknown clause status and timestamp offsets/minute precision. Performance review: decoration is limited to 20 rows, uses loaded source data and small cached-name/ID lookups; lazy club images add browser CDN fetches only. No new Futmondo/DB reads, finance tasks, timers or progress-driven redraws. Hosted rendering not verified.

Validation completed: focused transfer checks passed; offline lifecycle passed 13 checks plus eight tabs with no external mutations; deployment checks passed 3/3.

## 2026-11-02 — Consolidated filter panel

Primary implementation, no delegation. Reused the checkbox-control group inside both shared filter-panel layouts. My Team disables value/trend controls as well as its already disabled owner/favorite/free-agent controls; Position and Active Clause Only remain. Position uses the available panel width in this simplified layout. DOM checks confirmed all checkbox/dropdown controls share one panel and My Team contains only its requested filters; existing player filter suite passed 6/6. Performance review: static UI relocation/configuration only, no extra reactive computation, requests, timers or data copies. Full lifecycle and hosted/mobile rendering deliberately not run for this presentation-only change.

## 2026-11-02 — Visible Rivals loading feedback

Primary implementation, no delegation. Added a prominent loading message and spinner as the Rivals status output's initial HTML, so feedback is present before its first server render. Existing loading flags keep the message visible during retrieval; once the team directory exists, text explains that balances are updating and team selection is available. Completed/failed state removes the spinner while preserving existing error details. Focused UI rendering checks cover immediate fallback, accessible live status and completed-state spinner removal. Performance review: presentation only, using existing reactive state; no requests, polling or table redraw changes. Full lifecycle and hosted phone checks skipped for this localized status change.

## 2026-11-02 — Portrait phone filter alignment

Primary implementation, no delegation. Replaced inline checkbox-row layout with scoped responsive CSS. Phone controls use one column, equal left edges, 40px touch labels with 4px gaps and a full-width Clear Filters button; reduced nested filter padding. Desktop retains a wrapping row. Removed Bootstrap absolute checkbox positioning within this group only. Local Shiny/browser fixture at 360/390px verified all five checkboxes aligned, 40–48px vertical spacing, button alignment and no panel overflow; 768/1200px render checks passed and 390px screenshot inspected. Performance review: CSS/static class changes only; no reactive/data/request work. Full application lifecycle skipped; hosted Android appearance not verified.

## 2026-11-02 — Rivals saved bonuses and concise selection

Two concrete causes were identified. First, the saved JSON decoder retained an extra list-column wrapper around nested player frames. A valid eleven-player XI was consequently parsed as one malformed player and rejected, leaving the historical lineup queue without award inputs. Fixed decoding recovers the original arrays without relaxing scope, scoring or finality validation. Read-only production inspection by the primary agent found 11 saved award snapshots (rounds 1–10 final, 11 unknown), ten team-round snapshots, no historical lineups and 172 recorded movements belonging to one team. Nine team snapshots contain eleven rounds each; the remaining team has an empty list and no observed financial fields. Replaying the sanitized 21-record award/round fixture now yields **121 award-player rows and 99 team-round rows**. The expected scoring hash matches the stored version; a speculative scoring fallback was unnecessary and was not retained.

Second, reconstruction required every current directory member to have a completed score before paying any ranking pool. The empty member therefore blocked the other nine teams' position bonuses. Missing standings can now be retrieved once per closed round using the exact official captured `/1/ranking/round` contract. A validated closed ranking supplies historical participants and observed points, then persists for reuse in existing eligibility evidence. The absent member retains unknown history; it is never assigned a fabricated zero score. Ranking, MVP and Best XI results still enter the same ledger and retain recorded-payment overlap protection. Running/unverified round 11 remains excluded as expected.

Presentation now keeps the directory compact on team selection, summarizes repetitive gap notes and puts full explanations behind **Calculation details**. Full dated and undated ledgers remain in transaction history.

Executed local checks:

- `Rscript test/test_rival_saved_bonus_recovery.R`: **6 passed**, including actual async DB/award-catalog decoding through mocked HTTP, captured ranking URL/query through the real adapter, finite/unique/closed response validation, context/scoring/malformed-XI exclusions, stale ranking signatures and real serialized storage roundtrips across sessions. Known answer: 100 opening +20 point income +20 position income +55 Best XI +10 MVP = **205**. One ranking and two lineup requests occur initially; a new session reuses all three saved observations with no further historical requests. Concurrency stays at two; ledger and summary cash agree.
- `Rscript test/test_rival_reconstruction.R`: **14 passed** after the preparation optimization. `Rscript test/test_rival_cash_reconciliation.R`: **9 passed**; recorded reward overlap, split settlements, shared source classifications and numeric reconciliation remain covered.
- `Rscript test/test_rival_finance_runtime.R`: **16 passed**, including no automatic audit expansion, bounded concise status with full original notes preserved, existing account/navigation isolation and queue error completion.
- Mandatory `Rscript test/test_shiny_simulation.R`: **13 checks plus eight tabs passed**, zero external HTTP/live DB mutation attempts; existing benign Plotly warnings remain. Final deployment/independent review results are recorded separately below when executed.

Performance review: a synthetic full publication shaped like production (ten directory teams, 99 team-round rows, 121 award-player rows, ninety lineups and ten rankings) initially measured **1.731 seconds**. Profiling showed repeated parsing/signatures and full lineup scans; bounded preparation reuse reduced the same fixture to **1.334 seconds**, with **165.8 KiB** output. The 21-record nested snapshot batch decoded in **4.37 ms** over 100 iterations. Historical participant/lineup preparation shares parsed ranking rows and signatures and indexes lineups per team/round. Remaining publication CPU cost is material for a complete league and is still bounded by the existing two-second publication coalescing; these local measurements are not a hosted responsiveness guarantee. Initial missing-history demand can be roughly ten ranking plus ninety lineup reads; persisted unchanged data is reused thereafter. No recurring polling, extra per-card reads, dependency, schema change or unattended worker was introduced.

No live Futmondo rankings/lineups, production writes or hosted/mobile rendering were exercised by the implementation checks. Production inspection and sanitized decoding establish existing saved-data recovery, not successful live backfill. The exact returned historical participant set, delayed source responses and real save success still require hosted verification. Missing roster-only award ownership or inaccessible source history remains an explicit evidence gap rather than a zero or claimed exact balance.

Final primary review and independent validator checks: saved-data replay, ranking allocations (including ties and negative-start exclusion), stale-signature rejection, account/league/season isolation and recorded-payment deduplication passed. Deployment contract passed 3/3; only the three changed runtime checksums were updated. Live API verification was not performed: automatic approval review rejected reuse of a HAR token without specific authorization, and the permission question remains pending.

Final frozen-code verification after preparation reuse: `test_rival_cash_reconciliation.R` again passed **9/9**, and mandatory `test_shiny_simulation.R` again passed **13 checks plus eight tabs**, with zero external HTTP/live DB mutation attempts. No further runtime changes followed these checks.

## 2026-11-02 — Positive-only numeric pill scale

No delegation. Shared Points/Avg N/ratio badge palette now runs from theme surface to green, with neutral equal domains and luminance-selected black/white text. Existing metric checks passed 8/8, including exact emitted JavaScript, simulated dark/white/light-gray backgrounds at 4.5:1 contrast, numeric domain stability, filtering and updates. Performance review: constant work per rendered cell plus a computed-theme-property read; no data queries, timers, source scans or additional reactive invalidations. Browser/device rendering was not checked; existing application theme is fixed dark, not device-switched.

Required offline lifecycle passed: 13 checks plus eight tabs, with no live HTTP or database mutations.

Position-pill abbreviation change: no delegation. Presentation-only mapping to GK/DEF/MID/FWD with full-name tooltips; existing colors and source/filter values retained. Performance review: constant string lookup, no extra requests or reactive work. Focused renderer assertion used; full lifecycle and phone checks skipped for this label-only change.

Header/price presentation, 2026-11-02: no delegation. Fixed nowrap headers and actual rt-text-content clipping; widened offer columns to 180px. Full-source price deduplication preserves distinct asking prices and NA versus zero. Existing metric suite passed 9/9; offline lifecycle passed 13 checks plus eight tabs. Final Chromium fixture at 1440px and 390px confirmed five full headers and two offer amounts (including a billion-value amount) fit. Performance: CSS/width changes add no data work; duplicate detection compares at most three numeric vectors on existing table updates, no API reads or timers. Hosted browser not verified.


## Incoming clause signings — 2026-11-02

Implemented a separate My Team incoming-signings panel and a separate incoming count in the current squad subtitle. Captured summary responses supply the actual `transferDate` (including Madrid-midnight deliveries), rather than a guessed 96-hour delay. The shared round catalog drives strict before/at/after start labels; source-confirmed arrivals/cancellations and later sales retire rows, while overdue, stale, omitted-history and failed-summary observations retain prior confirmed purchases with an actionable explanation. Current-roster actions and fail-closed acquisition checks retain their original scope.

Performance review: the active My Team source reuses cached roster/history and raw player summaries, queues only unmatched incoming purchases with at most two reads, and caps concurrency across navigation/context changes. Warm navigation with three candidate summaries makes zero new summary requests; after one failure it retries only that candidate. Explicit invalidation resumes with two in flight. No API polling, whole-league scans, new dependencies, additional database reads or login-blocking work were added. Rendering concerns only the small incoming group; no recurring roster table redraw is introduced.

Verification: incoming-signing regressions 20/20, acquisition availability 7/7, deterministic fixes 30/30, mandatory offline lifecycle 13 checks plus 8 tabs passed. A synthetic actual-app Chromium check passed at 1440, 390 and 360 px: incoming rows/counts, before/after labels, row/stack layout, no panel overflow, and visible player-card identity on clicking an incoming player, authoritative rival ownership and no own-roster sale controls; no JavaScript or visible Shiny errors. No live account request, database migration or hosted deployment verification was performed.


Sol implemented the feature; an independent validator checked timing, capacity safety and callback isolation, and the primary agent reviewed the integration and phone panel screenshot. Primary review also corrected incoming-card ownership mapping before delivery. Hosted/live-account verification was deliberately not performed.

### Remembered-login feedback after navigation — 2026-11-02

The user reported a checked remember-device option but no confirmation after login. Traced the feedback from the persistence completion through `Login_Module.R` to the Login tab: the app automatically selects Today on authentication, hiding and suspending the feedback output. The previous browser fixture left Login visible, so it missed this path. Moved the existing namespaced `remember_feedback` output into the shared dashboard layout above the tabs. Success, persistence errors and existing retry controls remain visible after navigation; cookie issuance, storage and authentication logic are unchanged.

Verification:
- Updated the synthetic HTTPS fixture to use the real `ui.R` dashboard and automatic Login-to-Today transition. Before the fix, it failed waiting for `#login-remember_feedback`: the output was hidden/recalculating after Today opened.
- `node test/test_remember_login_browser.cjs`: **4 passed / 0 failed** after the fix. Checks visible success after navigation, protected cookie issuance, fresh-session restoration without expiry renewal, logout/reopen, and an injected persistence failure visibly reported on Today without issuing a cookie. Success and error views do not overflow at 390px.
- `Rscript test/test_remember_login.R`: **16 passed / 0 failed**, including asynchronous validation/cache reuse, security, expiry, isolation and cancellation checks.
- `Rscript test/test_shiny_simulation.R`: **13 checks plus 8 tabs passed**, including login, navigation, refresh, logout and zero live HTTP/database mutation attempts. The harness emitted fixture/loading diagnostics and empty-plot warnings but no failed checks.
- The executor initially lacked `sodium` and Playwright. Installed the existing manifest's `sodium` 1.4.0 into `/tmp/futmondo-r-test-library` and browser tools into `/tmp/futmondo-review-tools` and `/tmp/futmondo-playwright-browsers`; checks used `R_LIBS_USER` and `PLAYWRIGHT_BROWSERS_PATH` for those temporary locations. No application dependency or deployment-manifest changes.

Performance review: this relocates one small existing reactive output instead of duplicating it. It adds no API/database requests, cache invalidations, timers, blocking work or growing state; the single feedback render now runs while Today is visible. Authentication/data loading and tables are unaffected. Actual mobile dashboard success/error visibility and overflow checks passed. Documentation has no runtime effect.

Limitations: this confirms the local visibility defect and synthetic cookie flow, not the user's hosted cookie. User reports configuring both hosting variables and checking the option. No live browser cookie, hosting routing, Android persistence or restart behavior was inspected, and nothing was deployed. The current deployed Login tab can still be opened to inspect its saved-access result; hosted issuance/restoration must be checked separately.



### Configured league rules, compact Admin and public rival fields — 2026-11-02

Fixed the rival squad-value loss at publication: optional team-information `NA` previously masked a finite public-directory `teamValue`. The financial table, loading/error directory and selected card now prefer the public source, preserve true zero/negative points, retain `lastAccess`, and avoid a fabricated zero roster valuation. Points use a stable whole-league background-to-red pill scale with contrast-aware text; timestamps sort numerically and display in UTC.

Starting cash now uses observed `configuration.budget`, including zero, with recorded opening allocations taking precedence once. Both the hardcoded league-specific €300M and stored user-confirmed fallback are removed. Saved observed configuration is still reused across failures; no obsolete `league_cash_defaults` query remains. No table or migration was deleted.

Public Flutter serializer/update handlers plus Spanish translation assets establish the new Admin meanings, units and sentinel values (see `admin_module.md`). All 48 supplied configuration keys are accounted for, with five unresolved codes left explicit. Clause delay uses `playerMoveInDays`; exact incoming time continues to use `transferDate`, never an assumed four-day duration. Admin uses six compact category cards, three columns on desktop and one on phones; explanations/unknown codes open on demand.

A further audit found the hardcoded 0.5 borrowing fraction in shared acquisition/display calculations and an unconditional player-value bid floor. `configuration.mnmp` now supplies the fraction; absent/invalid rules leave purchases unverified. `mbmv` controls the value floor while explicit asking prices remain observed floors. The real dialog and final preflight reuse the same verified minimum. Reported `maxBid`, held-fund deduplication, pending transfers, roster capacity, expiry checks and manual-action protections remain. No live account request or transaction was sent.

Executed verification:

- `test/test_admin_league_settings.R`: six check groups passed, covering zero/false, information precedence, no fixed opening amount, 0/1/2/7/10-day delays, all added mapping choices and compact disclosure markup.
- `test/test_rival_team_directory.R`: **7 passed**, including the previously failing NA-over-public-value path, cached account/league isolation, selected-card consistency, numeric sorting inputs and no extra requests. `--browser` via `test/test_rival_admin_browser.cjs` passed at **1440px and 390px**: values, stable colors after sort, at least 4.5:1 text contrast, UTC access, disclosure controls, responsive columns and no page overflow. Final desktop screenshot inspected.
- `test/test_rival_finance_runtime.R`: **18 passed**, including varying configured budgets, rejection of legacy defaults and restoration of saved observed rules when live information fails.
- Accounting checks: reconciliation **9**, reconstruction **14**, running balances **13**, saved bonus recovery **6** passed. Saved transport verifies zero additional requests when the shared evidence cache is already warm, and no defaults-table request.
- `test/test_configured_acquisition_rules.R`: **6 passed**. Known-answer borrowing at 0%, 20%, 50%; invalid/missing settings; API ceilings; reservation reuse; the supplied numeric example; true/false/unknown bid floors; actual player-card dialog input minimum and final preflight. All HTTP and mutation paths are blocked or synthetic.
- Existing debt/countdown **13**, acquisition availability **7**, deterministic fixes **30**, factual helpers **7**, data contracts **13**, history/persistence **7**, and player-data availability checks passed. Successful fixtures now declare their assumed league rules explicitly.
- Required `test/test_shiny_simulation.R`: **13 checks plus 8 tabs passed** after the final runtime/UI changes, including zero real HTTP/database mutation attempts. Existing fixture/schema diagnostics and empty-plot warnings remain; no failed checks.
- Scoped whitespace review passed with existing CRLF respected. No application dependency/environment-variable changes; no manifest regeneration was required. Temporary browser tooling from the remembered-login validation was reused.

Performance review:

- No new API/database requests, polling, timers or async tasks. One obsolete defaults-table read is removed. Directory metadata reuses the already cached response; selected valuation no longer depends on roster aggregation. Existing account/league/season isolation, bounded cache sizes, two-request acquisition concurrency, and coalesced finance publication remain unchanged.
- Local warmed table-construction comparison (10 iterations): 10-team table **12.0 → 15.4 ms**, 100-team table **49.8 → 105.0 ms**. Extra formatted access cells/pills add bounded linear work; colors are calculated once per table, not per sort. These are server construction timings, not hosted browser latency. Existing maximum directory refresh is 100 teams; the ordinary supplied directory has 10.
- Expanded Admin adapter plus UI averaged **18.18 ms** over 100 local iterations for 46 rows (44 descriptive rows plus two present opaque fields). Native disclosures require no extra JS or requests; full supplied configuration adds three more opaque rows. No state grows with repeated rendering.
- Actual async acquisition fixture retained **5 cold factual reads**, **0 additional warm target reads**, a responsive heartbeat during delayed responses, **0.690s** local cold completion and **9.7 KiB** result. These figures include the unchanged underlying reads and are not hosting guarantees.
- Running-ledger check measured **0.094s** for 1,000 transfers, **223.9 KiB**. Documentation and fixture changes have no production runtime cost.

Limitations: implementation and validation are local. Nothing was deployed; current hosted data, hosting latency and actual database persistence acknowledgements were not replayed. Unknown code meanings and extra-payment timing remain unresolved rather than guessed. Direct-sale and clause prices continue to rely on source facts rather than invented formulas; current settings do not establish historical payments or rule changes.


### Local chat attachment exclusion — 2026-11-02

Added `.codex-attachments/` to the repository-root `.gitignore`. Verified that all 24 images were untracked, all 24 match the ignore rule, Git reports no changes within that directory, and all images remain on disk. Existing ignore entries and unrelated working changes are preserved. Other checkouts receive the rule when they pull it; already tracked attachments, if any elsewhere, would need separate index removal.

Performance review: Git ignore rules and documentation only; no application runtime, API/database, cache, reactive rendering, timer or memory impact. No application tests are needed for this change.


### Durable round history and targeted repairs — 2026-11-02

The reappearing history run traced to `recent_points_saved_players()` expiring every saved player/check after 24 hours, or one hour for unknown context, combined with a full catalog queue. Removed that age expiry while keeping exact league/season/scoring context, valid observation times and the original future-timestamp guard. Successful sparse/empty source checks remain durable and never imply invented zero scores. Legacy or mapped rows already covering a requested N avoid source replay without requiring an acquisition marker; only visible players missing demanded rows require that acquisition.

Newer conflicting fitness evidence now overrides durable reuse for affected players. A shared snapshot probe chooses an affected player, then the existing explicitly verified round anchor supports bounded mapping/repair. Ambiguous or failed anchoring leaves affected players eligible for individual labelled-source retrieval instead of suppressing repairs behind old reusable rows. Older vectors cannot roll back newer complete explicit windows or successful full-history checks. Failed reads preserve saved scores; persistence remains score-first/marker-last.

Verification:
- `Rscript test/test_recent_points_async.R`: **27 passed / 0 failed**, covering 90-day-old N3/N5/N8 reuse with zero summaries, old sparse/empty checks, targeted missing legacy rounds, corrupt future markers, unchanged navigation, two in-flight requests, backpressure, acknowledgement failure and no per-player publication/redraw regression.
- `Rscript test/test_fitness_round_snapshot.R`: **18 passed / 0 failed**, including affected-only changed-score acquisition/persistence, preserved successful/saved entries on failure, stale-vector rejection, ambiguity/constant patterns, bulk repairs, N>5 demand and failed-save explicit retry.
- `Rscript test/test_shared_round_dataset_independent.R`: **10 passed / 0 failed**, including exact-context durable checks and existing ordered persistence/cancellation contracts.
- `Rscript test/test_application_offline.R`: **13 checks plus 8 tabs passed**, zero live HTTP/database mutation attempts. Existing fixture/schema diagnostics and empty-plot warnings appeared; no failed checks.

Performance review: no new source/database endpoint, polling, recurring timer or migration. A fully populated **527-player x38-round**, 90-day-old matrix used **one logical database read and zero per-player summary requests** across N3/N5/N8. Actual controller startup/publication and those N changes in the synthetic test harness took **4.489s**; the existing 500-player N8 arithmetic alone took **0.045s**. The 500-player five-round sparse bulk acquisition/map/merge/publication fixture took **2.220s**, using **one** source probe. These include local R/test/reactive overhead and are not hosted latency measurements. Concurrency, queue/row caps and publication throttling are retained. Documentation has no runtime effect.

Limitations: no hosted deployment, live API call, live database inspection or production save replay was performed. Corrections older than available last-five evidence need explicit labelled-source retrieval or manual Refresh; unchanged/repeated score patterns cannot establish a new round anchor and are never guessed from schedules.


### Remembered login behind the hosting proxy — 2026-11-02

User reports closing/reopening both devices (without logout), with no saved-login message. Read-only inspection of the supplied public Connect Cloud page confirmed that the checkbox is enabled, the deployed browser script matches the repository, the shared feedback output exists, and no saved-login HTTP exchange starts. The public browser pathname is `/`; the document base includes a changing `_w_.../` worker route. Read-only production checks found zero remembered rows, RLS enabled, server-role access and no anon/authenticated reads. No tokens, keys, private response bodies, live account login or production writes were used.

The initialization observer returned silently when `session$request$HTTP_ORIGIN` was missing. The [public Posit Shiny proxy implementation](https://github.com/rstudio/shiny-server/blob/master/lib/proxy/sockjs.js) creates a separate backend WebSocket. A synthetic HTTPS proxy stripping only the WebSocket Origin reproduced the user's exact missing-feedback failure before the fix. This establishes the code defect and a matching hosted failure path; the managed host's internal request headers were not directly inspected.

`remember_login_same_origin()` now checks the cookie HTTP request's HTTPS Origin against its Host when the original WebSocket origin is unavailable; an available original origin remains binding. It does not trust browser input or forwarded-host headers. The existing random CSRF capability, one-use action ticket, fetch-site check, cookie attributes, encryption, expiry and revocation remain enforced. A bounded follow-up `confirm` request checks that the browser actually returns its cookie before showing success. Missing-cookie confirmation gives a truthful session-only warning without a database read.

Executed checks:

- `Rscript test/test_remember_login.R`: **19 passed**, including no-Origin save/restore, mismatched/missing HTTP Origin/Host and wrong CSRF rejection before storage, default HTTPS port normalization, cookie confirmation without extra DB work, rejected-cookie feedback, expiry/tamper/revocation/account-switch isolation and delayed validation heartbeat/cache reuse.
- `node test/test_remember_login_browser.cjs`: **5 passed** with original WebSocket Origin; **5 passed** with `FUTMONDO_TEST_PROXY_NO_WS_ORIGIN=true`. Both use the real dashboard, synthetic credentials, a persistent Chromium profile and temporary JSON storage. Each verifies visible mobile success after cookie confirmation, reload, actual browser closure plus R process restart with a different worker base URL, unchanged absolute expiry, logout/reopen and visible write-failure feedback. Mobile and desktop widths are 390 and 1440 px. The temporary proxy initially needed its HTML rewrite to request identity encoding; that fixture issue was corrected before the passing runs.
- Required `Rscript test/test_shiny_simulation.R`: **13 checks plus 8 tabs passed**, zero live HTTP/database mutations. Existing empty-plot warnings and fixture read diagnostics remain.

Performance review: one additional small same-origin HTTP confirmation only after successful explicit opted-in login, with no additional Futmondo/database request. Restoration still performs one indexed lookup and reuses one asynchronous league validation; absent cookies perform zero storage reads. No polling, new workers, added timers, growing cache, per-table redraw or login-blocking synchronous I/O. Header comparisons are bounded constant work. Browser restart checks exercise the actual cookie path rather than copying cookies into a new context. No dependency or schema change; only the changed runtime checksum in the deployment manifest is updated.

Limits: the above restart and authenticated cookie checks are synthetic. Actual phone persistence and hosted account restoration must be verified after rollout. Existing hosting secrets should be retained unchanged; this fix needs no new environment variable and no paid hosting capability.


Hosted follow-up: the first pushed correction became visible on the public app: the previously absent request now reached the registered endpoint, which returned the application's own JSON `status: forbidden` with HTTP 403. This is an application guard rejection, not evidence of a Posit plan restriction. The synthetic proxy reproduced that second failure when it rewrote HTTP Host. Extended the origin check to accept browser-controlled `Sec-Fetch-Site: same-origin` when no original WebSocket Origin is available and the backend Host differs. HTTPS Origin, valid Host syntax, CSRF, operation tickets and fetch-site rejection remain required. `same-site`, `cross-site`, `none`, malformed and absent metadata cannot use this fallback. Page JavaScript cannot set this forbidden request header; see the [Fetch Metadata specification](https://www.w3.org/TR/fetch-metadata/#sec-prefix). Fixed failure-stage labels aid diagnosis without exposing headers or credentials.

Final local verification for this follow-up: remembered-login suite **20 passed**; browser suite with both omitted WebSocket Origin and rewritten Host **5 passed**, including real browser/process restart; mandatory offline lifecycle **13 checks plus 8 tabs passed** with no live HTTP/database mutation; deployment contract **3 passed**. Performance adds one bounded header comparison and a short failure label, with no extra requests, timers, caches or database work beyond the already documented one-time cookie confirmation. Live authenticated device verification remains outstanding.


Hosted rollout verification: after GitHub revision `a7e0107` reached the automatic Connect Cloud deployment, a fresh unauthenticated Chromium visit to the user-supplied public URL performed the real session-bound remembered-login POST and received HTTP **200** with `status: absent`; the feedback error was gone. This confirms live routing and the origin/CSRF checks for the no-cookie path without using an account or writing production credentials. The earlier 403 was generated by this application, not a Posit subscription/security-setting block. The user has been asked to sign in once with opt-in on each device to verify issuance and actual reopen; those authenticated checks are not claimed by this public probe. Documentation-only rollout notes have no runtime/performance effect.


## Table-only rival selection — 2026-11-02

Removed the redundant Choose a team dropdown and its two synchronization/selection observers. The existing Reactable selection remains scoped by account, league and own-team context. Updated table failure text so it no longer directs users to the removed control. The financial history now displays the full shared ledger, including reconstructed bonus rows, without a separate undated Components table. Date-source labels distinguish actual recorded timestamps, inferred shared payment times and round-publication dates. Selection alone adds no reads or timers and removes reactive/browser synchronization work. The integrated date-recovery queue and its verification are documented below; historical ranking endpoint validation and hosted reward completeness remain unverified.


## Notification-based reconstructed reward dates — 2026-11-02

The saved production notification inspected read-only by the primary agent establishes round 10 publication at `2026-10-27T14:21:52.133Z`, `type="round"`, `action="close"`, round ID in the generic `player_id` field and round number in `player_name`. The supplied notification HAR contains market events only; the verified public client additionally documents `type="rounds"`, `directObject.name` as round number, and older-page query `{to: oldestNotification._id}` with `answer.items`. No captured account token was replayed.

Following the user's confirmed precedence, calculated position, Best XI and MVP entries prefer an unambiguous unsplit shared position-payment time, then the matching notification creation instant. Point estimates use their notification creation instant and never borrow the position-payment date. Every inferred date is marked reconstructed. Actual recorded money movements retain their source dates and suppress corresponding amount estimates. Multiple round publications date the one cumulative reward at the latest creation time; they never add another reward. Missing applicable payment and publication evidence remains a specific round-level defect, and missing point-rate configuration never creates point income.

The current first-page result was previously persisted without being merged into the in-session evidence, delaying its availability until a later read. Fresh notifications are now merged immediately. Missing known completed-round dates trigger bounded older pages in the existing async queue, with successful events, exhausted-range identity and capped continuation cursor persisted for reuse. Other leagues advance the global cursor without entering this league's accounting.

Executed focused verification:

- `Rscript test/test_rival_cash_reconciliation.R`: **16 passed / 0 failed**, including shared-payment-before-notification precedence, split/repeated anchor rejection, separate point dates, four configured bonus kinds, repeated/replayed publications, round/account isolation, persisted JSON reuse, no point income without its setting, unchanged split recorded totals and full ledger reconciliation.
- `Rscript test/test_rival_finance_runtime.R`: **18 passed / 0 failed**, retaining account resets, saved configuration, asynchronous two-slot queue, navigation reuse, partial failures, balance calculations and selectable directory behavior. Existing deliberate malformed-fixture schema diagnostics appeared; no failed checks.
- `Rscript test/test_rival_notification_dates.R`: **6 passed / 0 failed**, including shared-anchor zero-history-read reuse, split-anchor notification recovery, real-adapter synthetic notification POST/query/response parsing; malformed, repeated and empty pages; two-page retrieval of two missing round dates; another league's cursor; saved storage roundtrip with zero repeated history reads; partial failure preserving the successful date and all cash amounts; exhausted scan reuse; and 20-page cap with saved-cursor continuation. No live API or database mutation was performed.

Performance review: notifications are parsed/indexed once per ledger publication and reused across all rivals. Historical pages run only for missing applicable completed-round dates with a configured reward potential, one page per async task, within the existing two-read limit. There is no periodic timer, new source endpoint, per-player lookup, login-blocking work, schema or dependency change. Each page has at most 1,000 rows; each refresh at most 20 pages; saved evidence retains the existing 20,000-identity cap. Completed saved dates require zero historical-notification requests; exhaustion markers prevent replaying the same unavailable historical range across sessions. These are synthetic request-count checks, not hosted latency measurements.

Limits: live historical-notification retention and production backfill success are not claimed. A missing close event after source exhaustion cannot establish an exact historical payment date. A reconstructed notification date is explicitly an assumption, particularly for split settlements; real recorded transfers remain exact. Mandatory final offline lifecycle, deployment checksum and primary/independent review are reported by the integrating agent.

Independent validation identified a configured-zero edge case: existing zero point/position estimates became undated bonus rows even though no bonus was configured. Finite zero-valued reconstructed reward rows are now suppressed before date assignment; actual recorded zero movements and unknown amounts remain. The focused reconciliation suite now has **16 passing checks**, including no fictional zero-reward/date-gap rows, unchanged cash totals and preservation of an actual dated zero reward. The filter adds one bounded vector pass over each existing ledger and reduces later date/UI work; it performs no I/O.


Final integration: primary reviewed the payment-first/notification-second rule, unified history and table-only selection. Sol implemented notification recovery; Luna investigated source contracts and independently validated account/league/season/round isolation, configured-zero handling and a separate known-answer balance of 135. Final checks passed: reconciliation16, notification pagination6, running balances14, deployment3, and mandatory offline lifecycle13 checks plus8 tabs. The finance-runtime18 checks also passed before the isolated zero-row filter, which received focused revalidation. No live Futmondo requests or production writes were made. Read-only production evidence confirmed a 70,000 EUR per-point setting and round10 publication; older-date recovery remains to be verified in the hosted app. Shared position-payment dates for reconstructed awards remain explicitly inferred from available movement evidence, not proof that all historical installments were captured.


## Inactive rival and historical position bonuses — 2026-11-02

The zero position-bonus subtotal came from requiring all ten current directory members in each historical standings set when one rival has an explicitly saved empty history. The primary agent's read-only comparison reproduced all nine saved own-team position payments exactly among the nine actual participants using the configured €30M `flop` pool. No corresponding Round 8 payment was saved; that amount remains reconstructed.

The shared `rival_ranking_participants()` resolver now accepts this reduced participant set only after at least two distinct completed rounds reproduce their actual position payments exactly. Latest successful empty full-history evidence qualifies; absent, malformed or nonempty histories do not. Validation requires finite points and known starting-cash eligibility. Latest movement-ID revisions replace earlier values, separate split-payment IDs add once per team/round, and contradictory known payments reject the inference. Missing active-round scores still block that round, while saved closed official rankings retain precedence. Both the calculation and ranking queue reuse the same decision. The inactive team remains selectable; no missing score becomes zero.

Executed verification:

- `Rscript test/test_rival_inactive_participants.R`: **6 passed / 0 failed**. Covers exact nine-participant allocations, unpaid Round 8, ledger totals and recorded-payment replacement; missing/malformed/nonempty/unknown-eligibility history; one-round/replayed/partial/contradictory payments; latest revisions and split settlements; saved JSON roundtrip with an explicit empty list; incomplete active rounds; official ranking priority; and zero redundant ranking calls on initial recovery/navigation.
- `Rscript test/test_rival_finance_runtime.R`: **18 passed / 0 failed**.
- `Rscript test/test_rival_cash_reconciliation.R`: **16 passed / 0 failed**.
- `Rscript test/test_deployment_contract.R`: **3 passed / 0 failed**; changed runtime checksum refreshed.
- Required `Rscript test/test_shiny_simulation.R`: **13 checks plus 8 tabs passed**, with zero live HTTP/database mutation attempts. Existing empty-plot warnings and deliberate fixture schema diagnostics remain.
- `git diff --check` passed for the implementation, accounting documentation and manifest.

Performance review: one pure preparation helper reuses already loaded histories/movements. A local 20-run fixture with ten directory members, 90 completed score rows and nine payment anchors averaged **61.5 ms per call**. This introduces no source request, database read/write, cache growth, polling, dependency, migration or login work. It avoids ranking requests when existing payment evidence already establishes the historical set; ordinary missing-history acquisition remains bounded by the existing two-read queue. Hosted latency and production balances after rollout were not verified by these offline checks.

### Reference-validated point settlements — 2026-11-03

Implemented native `points` classification and numeric/split concept mapping, shared exact monetary reconciliation, final observed settlement dates for combined rival point rewards, and separate known-date/reconstructed-amount UI labels. Actual reference-team movements remain separate. Missing or mismatched reference evidence and unknown rival eligibility exclude unverified reconstructed rows and mark the point subtotal unavailable; detailed gaps and differences remain visible. Repairs use one bounded batch after ordinary reads, refresh affected round/movement caches and reference configuration, and reuse persisted evidence.

Executed offline checks:
- `test/test_rival_point_settlements.R`: **6 passed**, including sanitized supplied payments, all ten completed rounds, round 1 **5,740,000 EUR / 82 points**, round 6 **5,600,000 EUR / 80 points**, final timestamps, 14 unchanged actual instalments, duplicate/revised movements, negative points, unknown eligibility and account/league/season isolation. Synthetic stale round 11 (95 running) repairs to independently supplied 105 closed; a second session requires no repair reads. A persistent 104-point mismatch remains excluded after exactly one repair, with an unavailable point subtotal. Delayed asynchronous reads preserve event-loop progress and maximum concurrency two; no live account/transaction is used.
- Saved bonus recovery: **6 passed**; the async fixture now supplies a real-shaped reference point payment to meet the validation contract.
- Cash reconciliation: **16 passed**; round reconstruction: **14 passed**; running balances: **13 passed**, plus main-history reconciliation assertion; finance runtime: **18 passed**.
- Required offline lifecycle: **13 checks plus 8 tabs passed**, zero live HTTP/database mutation attempts. Existing fixture-schema diagnostics and empty-plot warnings remain.

Performance review: twenty full two-team, eleven-round fixture publications took **1.805 seconds total** (about 90 ms/publication), with **49,576 bytes** output. This includes existing accounting work and is not a ten-team/mobile/hosted latency benchmark. Reconciliation is prepared once per shared publication, not on selection or per UI card. Repair adds at most one extra team read per affected team per generation (rounds/movements, plus reference configuration); unresolved eligibility can use the existing capped lineup queue. Saved eligibility prevents repeated lineup retrieval; successful round repair is reused next session. Existing concurrency two, bounded caches/queues and two-second publication coalescing remain. No recurring timer, dependency or schema change; docs have no runtime effect. Updated only changed runtime checksums in the deployment manifest.

Production limitation: read-only inspection still found reference round 11 at **95 points, running**, observed 2026-11-03 03:38 UTC. Its supplied 7,350,000 EUR payment implies 105 points but does not independently establish that score. No production write, live authenticated acquisition or deployment was performed. Hosted completion remains open until the app fetches final source history, reconciles it and demonstrates persistence reuse. Local fixtures must not be treated as that evidence.

Final review additionally verified that a legacy partial-credit estimate cannot override the independently calculated whole-round total (round 1 remains 5,740,000 EUR). The targeted suite still passes all six groups. Deployment contract: **3 passed**, excluding the sanitized test fixture, credentials, captures and internal docs from the application bundle; scoped diff whitespace checks pass.

## 2026-11-03 — Classification selected-round bonus totals

Implemented four categories requested (despite the request saying three columns): points, MVP, Best XI and position. Standings reuse shared finance/MVP state. Ledger metadata preserves confirmed zero rewards and coverage; the UI sums only complete selected-round amounts. Existing user edits to finance settlement handling and documentation were preserved.

Validation: `Rscript test/test_classification_bonuses.R` passed known answers for one/two rounds, single-round precedence, recorded-payment replacement, missing selected evidence, unaffected earlier rounds, missing shared state, unknown ownership and verified non-recipient zero. It also runs the existing running-balance suite (13 checks plus full-history check). `Rscript test/test_ui_reliability.R` passed. `Rscript test/test_shiny_simulation.R` passed 13 lifecycle checks plus all eight tabs, including Classification and no live HTTP/database mutations. Scoped diff whitespace check passed.

Performance review: opening Classification now activates the existing bounded shared asynchronous finance/award acquisition; round selection performs zero API/database calls and no ledger reconstruction. Shared session cache/context invalidation is reused. Metadata has four rows per team-round and is replaced on publication, with no accumulated queue/timer. Award validation reuses the existing successful check and rechecks individual rounds only when gaps exist. The 1,000-transfer ledger fixture took about 0.10 seconds locally. A 10-team/38-round projection took 0.346 seconds for 100 selections (about 3.5 ms each); its synthetic shared object occupied 132.1 KiB. Additional columns use Reactable horizontal scrolling with readable minimum widths. Hosted latency, physical mobile layout and live league evidence completeness were not verified; no deployment or production writes were performed.

Push preparation: staged only the Classification feature and its ledger projection, leaving pre-existing settlement and other workspace edits uncommitted. Deployment checksums for the two changed runtime files match the staged source. The exact staged application passed the bonus suite, all 3 deployment checks and 13 lifecycle checks plus 8 tabs in an isolated temporary checkout. Its 100-selection benchmark took 0.370 seconds.

### Point-settlement hosted rollout — 2026-11-03

User authorized deployment. Published only the ten scoped release files to the existing public `smdb21/futmondo` branch `2.0`, commit `5b39a1568f6961c3e23a1fed3649578404f1d35c`. An isolated checkout of the exact release passed six point-settlement groups, classification bonus/running-balance checks, all three deployment checks and the required 13 lifecycle checks plus eight tabs. The initial staging archive path was corrected before testing; no incomplete archive was published.

A fresh unauthenticated Chromium session at `https://smdb21-futmondo-v2.share.connect.posit.cloud/` returned HTTP 200, established a Shiny connection and served `release-info.json` with release `point-settlements-2026-11-03` and runtime SHA-256 `319ab40b8a90ac288d09a4fb4a233ba09531dd42387331a14340218acf7613ae`, matching the isolated tested runtime. The same resource returned 404 before rollout. The static identifier contains no credentials or account information and adds no polling or automatic request to application sessions. This confirms hosted rollout and unauthenticated startup, not live authenticated round-11 reconciliation. Existing position/Best-XI split-date gaps remain outside this point-payment change.

The connector upload was initially rejected by automatic review. Verified the configured destination and local GitHub owner's admin/push permission before the successful retry through the owner account; the connector itself was read-only. Local Git object storage permissions prevented fetching the published commit back into this workspace, so the local HEAD/index were left unchanged and working files preserved. The remote release branch and hosted app were verified independently.

### Recent rounds disclosure — 2026-11-03

Replaced the player-card Recent rounds wrapper with native details/summary, closed by default, and adapted its heading styling. Expansion uses browser behavior; reactive rebuilds reset it to collapsed. Passed `test/test_player_profile_endpoints.R` (6 checks), `test/test_recent_rounds_independent.R` (11 checks), and `test/test_shiny_simulation.R` (13 checks plus 8 tabs, no HTTP/live database mutations). Scoped diff whitespace check passed.

Performance review: no added requests, database work, cache invalidations, timers, reactive dependencies, or JavaScript handlers. Existing bounded round computation and DOM content are unchanged; collapsed content saves visible space. Responsive wrapping remains unchanged and no fixed width was added. Browser/mobile appearance and hosted deployment were not verified.


### Final settlement gate and independent awards — 2026-11-03

User accepted official whole-round completion for postponed games. Native award movements now normalize and deduplicate by the existing identity/revision contract. Reference position payments govern position income; point reconciliation remains unchanged. Best XI/MVP require independent selections/eligibility and a positive dated final-category payment to any recorded recipient as corroborating settlement evidence. Reference award receipt is not required. Interim closed team history, points and publication notifications cannot release final awards. Pending rows remain visible, excluded and undated; shared cash, summary and classification amounts agree.

Exact isolated release checks: seven reference/finality groups; six point-settlement groups (including asynchronous round-11 repair and cross-session reuse); six saved recovery groups; 16 cash reconciliation groups; 18 finance runtime groups; classification bonuses/running-balance suite; three deployment checks; required offline lifecycle 13 checks plus eight tabs. All passed. Tests use synthetic/sanitized evidence, never real accounts or transactions.

Performance: finality is one vector scan over the existing normalized shared movement dataset per publication; no added API/database reads, invalidations, timers or selection-triggered calculation. Existing bounded acquisition and persistence are reused. Thirty synthetic two-team publications took 1.301 seconds (43 ms each), result 36,104 bytes. Classification projection measured 0.332 seconds for 100 selections across ten teams/38 rounds. UI adds no new layout width or polling. Physical mobile appearance and authenticated hosted data were not checked.

Limitations: settlement evidence establishes completion without a new match endpoint; a league lacking any recorded final-category recipient needs payment evidence recovery before awards can be published as paid. MVP finality is inferred from its dependence on final Best XI; the official postponement article explicitly names ranking and Best XI. No finish timestamp is invented. Newly closed production round 11 still needs independent authenticated history recovery; synthetic successful repair does not prove that it happened live. Hosted verification is recorded separately after deployment.


Hosted verification: deployed scoped commit `fd72e80817f6ad14c9f7ade27d5bfacb77246033` to the existing `2.0` release branch without force. A fresh Chromium session returned HTTP 200, connected to Shiny, and served `final-award-settlement-2026-11-03` with runtime SHA-256 `c97aa69015832521250cb72db40b42e53eccfd5ed2885cd0513d6070e6ef9c96`, matching the tested isolated release. The first check still served the prior release; the subsequent fresh check confirmed rollout. No authenticated league read or production reconciliation was claimed. Unrelated Recent rounds edits were preserved locally and excluded from the published documentation. Local Git HEAD was left untouched because its object storage remains unwritable; remote publication and hosted verification are independent.


### Cumulative reference-triggered point entries — 2026-11-03

Verified existing accounting already implements the requested behavior: no reference payment means no reconstructed rival point entry, and later payments replace one cumulative rival/round entry after exact reference reconciliation. Source evidence is persisted; derived entries are not stored as synthetic transactions. Legacy partial-credit evidence is excluded, so no destructive data cleanup is needed. Corrected the source explanation from final settlement to latest observed payment, explicitly avoiding a round-completion claim.

Added a sequential known-answer regression: no payment plus notification/legacy credit produces no entry; first payment validates 28 reference points and a 20-point rival; second payment with stale history blocks reconstruction; independently refreshed 82/84-point histories produce exactly one 5,880,000 EUR rival entry at the second date. Stable entry identity, original two reference instalments, repeat loads, saved-evidence rebuild, inflow total and final cash are checked.

Exact isolated release passed seven point-settlement groups (including bounded async recovery and persisted cross-session reuse), seven final-bonus gate groups, three deployment checks, and the required offline lifecycle harness (13 checks plus eight tabs). No real-account calls occurred in tests. Performance review: accounting algorithms, request counts, storage, cache invalidation, reactive work and layout are unchanged; only explanatory text and regression coverage changed. No new benchmark was warranted. Authenticated production round-11 reconciliation remains outside this isolated validation.


Hosted rollout verified: commit `d3502b32179013176d7b49dad8ce572d632add79` on branch `2.0`; fresh public browser session returned HTTP 200, connected to Shiny and served `cumulative-point-payments-2026-11-03` with runtime SHA-256 `4669e1978f759f97723912dcb3f7516686ce6fa1c30ac0369a95fef951e710a1`, matching the tested runtime. This verifies deployment/startup, not authenticated league balances.


### ID-less settlement loss and exact cash recovery — 2026-11-03

Investigated reported Rivals -7,930,875 EUR versus Today 52,129,125 EUR. Read-only production evidence showed 175 saved movements and no native points or dreamTeam rows. The supplied response has 192 rows: precisely those 17 bonus records lack `_id`. The persistence adapter discarded them. Fourteen point payments total 60,060,000 EUR, exactly the UI discrepancy; three Best XI payments total 3,500,000 EUR, previously replaced by estimates.

Added explicit derived settlement identities while retaining absent source IDs, unchanged real amounts/dates, and amount-independent revision matching. Full sanitized capture test covers API-shaped parsing, evidence creation, JSON storage decoding, exact observed cash, replay/shuffle, source revisions, original actual bonus retention, and continued rejection of unidentified generic transfers. Exact isolated release passed this regression, seven point groups, seven final-bonus gate groups, six saved recovery groups, 18 finance-runtime groups, 16 cash groups, three deployment checks and offline lifecycle 13 checks/eight tabs. Tests use sanitized/synthetic data only.

Performance: one key calculation per ID-less native settlement on acquisition (17 in 192 rows), not on selection; no extra API/database requests or timers. Existing scoped keys, revision handling and bounded deferred persistence are reused. 100 full-response normalizations took 11.824 seconds (~118 ms each); normalized frame 78,352 bytes. The formerly dropped 17 records now add expected bounded evidence writes. No dependency, schema or UI layout changes.

Production repair inserted only the 17 missing settlement identities from the user's supplied response, marked recovered_from=user_supplied_money_movement_response; existing identities were not overwritten. A follow-up query verified all stored actual movements sum to 52,129,125 EUR, matching the independently stored observed budget exactly (difference zero). A second comparison joined recorded cumulative point amounts with independently fetched historical scores/eligibility: all rounds 1–11 matched at 70,000 EUR/point; round 11 is now 105 points, closed, eligible, and paid 7,350,000 EUR. Round 8's interim closed/point match does not establish final award settlement. Source-contract repair and saved production evidence are verified; authenticated browser rendering is separate from public hosted startup verification.


Hosted verification: scoped commit `227c90e37f67188f174e7a1c3b977c256365da67` deployed on branch `2.0`. A fresh Chromium session returned HTTP 200, connected to Shiny and served release `idless-bonus-payments-2026-11-03`, runtime SHA-256 `c614809631f3619832a63464153d235819eba7781bb35b662f3467999251408c`, matching the isolated tested runtime. Initial check served the previous release; the next confirmed rollout. Production database recovery and exact cash/round-point comparisons were verified separately; authenticated browser rendering was not inspected.


### Full-evidence award overlap and history UI repair — 2026-11-03

The earlier transaction-only cash check did not exercise saved award estimates. Replayed all 341 scoped production evidence records through the actual builder and reproduced 54,129,125 EUR exactly. Two extra 1,000,000 EUR MVP estimates for rounds 1/7 sat on top of native dreamTeam payments. Independent historical eligibility and configured 500,000/1,000,000 rewards establish that each 1,500,000 EUR receipt already covers XI and MVP. The corrected full replay gives 52,129,125 EUR; summary attribution is XI 1,500,000 and MVP 2,000,000, with no new cash entries. No database mutation was required for this correction.

User logs identified `missing value where TRUE/FALSE needed` in output$rivals-rival_transactions_tab_ui. Reproduced the actual renderUI expression with round-8 position totals unknown. The unguarded comparison of total bonuses with an NA component caused the failure; it now remains unknown safely. A synthetic/captured-shape regression executes that actual UI expression, renders the ledger widget, validates separate Classification bonus columns and original combined transactions, and verifies mismatches preserve actual cash while leaving attribution unknown. The requested financial-history default is configured by explicit stable tab values.

Exact isolated release passed the new combined regression (including the full sanitized ID-less capture), seven point groups, seven reference/finality groups, six saved recovery groups, 18 finance runtime groups, 16 cash reconciliation groups, Classification/running-balance checks, three deployment checks, and offline lifecycle 13 checks plus eight tabs. All tests are offline and sanitized; the separate production evidence replay was read-only.

Performance: no extra HTTP/database calls, timers, invalidations or per-selection calculations. Attribution runs only for teams/rounds with native payments during shared publication. Three full saved-dataset builds took 9.429s versus 8.692s previously (~0.246s additional per publication); result size 583,672 versus 571,528 bytes. This is a measurable bounded CPU cost, not a network operation; full publication remains about three seconds locally and hosted CPU latency is unmeasured. Classification projection was 0.341s for 100 selections; 1,000-transfer ledger check ~0.10s. UI layout width is unchanged. Public hosted release verification is separate from authenticated browser rendering.


Hosted verification: commit `7a1be662af40a2bc7c9d598f5a883059265a17a5` pushed to branch `2.0`. Fresh public Chromium session returned HTTP 200, connected to Shiny and served `combined-award-payments-2026-11-03` with runtime SHA-256 `16de2b4209f015a662f64e53da9a119199899475c58f1c32ebc9c127a2b877cd`, matching the tested isolated release. Full saved-evidence accounting and actual history render expression were tested locally; no authenticated browser session was inspected.


### Financial-history projection excludes unpaid estimates — 2026-11-03

Removed estimated/non-included rows from the history projection only. Regression verifies pending rows disappear while remaining in the ledger and that the final displayed balance equals the full ledger balance. Seven reference/finality groups, 13 running-balance checks plus main history projection, combined-award/actual renderUI regression and three deployment checks passed in the isolated release.

Performance review: one bounded vector filter in the existing in-memory projection; no API/database requests, persistence changes, ledger recalculation, cache invalidation, timers or new reactive dependencies. Fewer rows are rendered; mobile layout is unchanged. No new performance benchmark is warranted for this low-impact filter.


### Rival history, local times and income/expense cards — 2026-11-03

Isolated release checks passed: round score/rank/tie and winter/summer timezone fixture; reference bonus gate; 13 running-balance checks; combined XI/MVP allocation and actual history renderUI; point settlement and persistent mismatch coverage; six saved bonus recovery groups; 18 finance-runtime checks; three deployment contract checks; offline Shiny lifecycle (13 checks plus eight tabs). The initial test command referenced a nonexistent deployment test filename; the correct test/test_deployment_contract.R was then run successfully. Saved production evidence replay remains 52,129,125 EUR cash with paid position bonuses 32,000,000 EUR, points 60,060,000 EUR, XI 1,500,000 EUR and MVP 2,000,000 EUR. This replay does not claim a fresh authenticated hosted read.

Performance review: rank metadata is computed in the existing shared history pass, once per dataset publication, and adds three scalar columns per ledger row. Display filtering and formatting use the existing ledger; two responsive boxes do not duplicate accounting work. One browser timezone input per connection affects only display, with no API/database requests, persistence changes, cache invalidation, timers or extra acquisition. Existing runtime tests confirm shared asynchronous loading and reuse on navigation; lifecycle verifies no live HTTP/database mutation. The actual timezone-reporting script was also checked in Chromium contexts for America/Los_Angeles and Europe/Madrid. No dependency or schema changes. Authenticated hosted visual checks remain separate from these local tests.

Hosted rollout verified for commit `179bda9784e43638a6b3b3e37b88a75a25c1c487`: public page returned HTTP 200, Shiny connected, and `history-rounds-local-time-2026-11-03` matched runtime SHA256 `5a647c82bd733de54ba1c04cb2d830f4120a3fb07021d971857d07e63e730962` and Rivals module SHA256 `b98039f8c2c71b503975f48779f38547a318a9d9ac1ae6fb06c5c6d51c19960f`. This verifies deployed assets and startup, not an authenticated financial-screen inspection.


### League transfers, crest metadata and uncapped filters — 2026-11-03

Checks passed in isolated release: Today party/system/HTML/crest/timezone/full-history fixtures; seven player-filter groups including blank maximum and values above 1000M; transfer metadata with unique/ambiguous matches, bonuses and unchanged balances; 31-request synthetic cursor fixture (30 populated pages then empty), and repeated-cursor partial fixture; 13 running-balance checks; 18 finance-runtime checks; combined-award/history renderUI; round/timezone reference gates; three deployment checks. Progressive loading passed 16 checks, including delayed primary resources and targeted retry without extra optional-history fetches. Offline Shiny lifecycle passed 13 checks plus eight tabs. No real account or transaction was used in tests.

Performance review: vectorized event-ID matching and shared crest markup reduced the representative 1,000-transfer metadata/display projection from 0.797 seconds to 0.037 seconds locally, with a 122 KiB display frame. Metadata is attached once per shared publication, not per rival selection. Today listens to a separate equality-checked transfer value, avoiding redraws on unchanged financial loading progress. Pagination renders 15 rows by default and makes no HTTP/database requests. Blank filters add no work. First uncached history acquisition can now exceed 25 requests by design, one per source cursor page until completion; existing async transport, context cache and persistence are retained. Full history is held in memory/client table data, so extremely large leagues can cost more memory than the fixture; no live largest-league benchmark was performed. No dependencies or schema changes.

Hosted rollout: commit `f557718e7c2feb6a9729c61d924047a55611224c` is live. Public startup returned HTTP 200 with Shiny connected and release `league-transfer-history-2026-11-03`; runtime, Rivals and Today hashes match the tested release. The additional Today reactive fixture passed: newly published verified methods become visible without initiating finance acquisition. No authenticated live oldest-transfer coverage claim is made.


### Locker clause payments: verified cash timing and one-entry accounting — 2026-11-03

Read-only authenticated source inspection (not a test account mutation): locker news returned 121 clause events dated 25 August–2 November 2026. Full pressroom retrieval returned 1105 page rows, 1074 unique event IDs after boundary deduplication. 110 locker events match one pressroom record by exact parties/player/amount, with delivery 3.0117–4.0036 days later; 11 have no delivery yet. All 51 reference-team locker payments match actual signed money movements, whose timestamps precede locker news by only 0.010–0.107 seconds. Their actual current movement total remains 52,129,125 EUR.

Captured-source replay through the real parser/merge yields 1085 unique financial transfers, including all 121 clause payments and 11 pending deliveries. Ledger replay changes seven teams' balances when those pending payments are included; reference cash stays 52,129,125 EUR. Private captures and authentication were kept outside the repository/bundle. These checks establish the clause defect and do not certify every unrelated bonus or private adjustment for rivals.

Passing local checks: locker parser/unique and ambiguous identity/delayed overlap/pending cash/actual movement precedence; normalized scoped persistence and null decoding; 36-page cooperative locker read beyond the old composite limit, heartbeat, zero-request warm reuse, explicit refresh and account isolation; source full-history/cycle checks; Today rendering/reactive methods; round score and local-date tests; 18 finance-runtime checks; combined-award renderUI; six saved-bonus recovery groups; deployment contract; 16 progressive-loading checks; offline lifecycle (13 checks plus eight tabs). The general async transport suite passes socket heartbeat/coalescing, then fails its player-card points expectation on both this candidate and the unchanged deployed baseline. That pre-existing unrelated fixture failure is not reported as a passing check. Locker-specific asynchronous coverage passes.

Performance review: parsed raw locker facts and merged feeds reuse existing bounded caches; explicit transfer refresh also invalidates the locker subresource. Current source acquisition adds two locker page reads (data then empty), plus the team directory only if not already cached; player catalog recovery is conditional. Normalized evidence uses existing bounded deferred persistence batches, with no new table/schema or per-selection request. Parsing timestamps once per source frame reduced the complete captured-feed merge from 6.4 seconds to 1.98 seconds locally. Network waits remain cooperative; no worker/process or timer was added. Exact-source matching is performed once per shared publication, and source cycles/1024-page budget fail explicitly rather than silently inventing complete coverage.

Regenerated and inspected rsconnect::writeManifest(): xml2 and locker_transfers.R added, no package version changes. This environment lacks sodium and has different installed curl/data.table/httr versions. The generated manifest was reviewed and existing deployed package records were retained; only xml2 was added, avoiding unrelated dependency changes. All credentials, captures, test files, internal docs and scripts remain excluded.

Hosted rollout verified for commit `7187d1b09b9c33cbfc94e4f3825d954fcfbf91ef`: public HTTP 200, Shiny connected, and release `locker-clause-payments-2026-11-03` matched all tested hashes (locker parser `0c4e797eb6795cbd1eddf2f167e3838f91c19c0c79f9d35bd1978c99b0df3774`, finance runtime `628dfbfde30ba37abd8e30df91e870648451e433ad5ebd5d34babc79b98f30f3`, Rivals `638f606f98fddb4940ccd9b29d64af62058a6512ca897b19960c9c5b11d39cb7`, Today `5e9cf9a466185cdb66329f6132fa2215e36543a45d55e28006e1443e95fe6b1f`). Public startup/deployed assets are verified; authenticated hosted financial rendering and subsequent production persistence are not claimed as directly inspected.


### HOPE balance audit and skipped transfer pages — 2026-11-03

Reproduced the displayed 473,966,258 EUR (underlying ledger 473,966,257.50). Fresh locker/pressroom first pages contained no new events; fresh team rounds confirmed all eleven scores and negative=false. Private rival information/money-movement endpoints returned api.error.not_found, so no private balance was inferred. Ownership continuity found Uche sold without a preceding purchase, leading to the pagination contract defect.

Overlapping ObjectId boundary seconds recovered seven purchases: three HOPE expenses (1,276,591 + 23,566,956 + 22,999,999 = 47,843,546 EUR), three for another rival and one for a third. The raw unique transfer count increases 1074→1081 and merged financial count 1085→1092. Corrected HOPE ledger is 426,122,711.50 EUR; the owner independently reports 426,122,711 EUR. The half-euro residual comes from round 6 tied ranking allocation (5,666,666.50 EUR), whose actual private payment/rounding remains unverified. Do not hide it with a forced balance adjustment. Reference actual movement cash remains 52,129,125 EUR.

Performance review: overlap uses existing cooperative HTTP/cache/persistence paths and deduplicates native IDs before publication. Current capture recovery required nine nonempty reads (with a final empty read needed for completeness), versus eleven reads in the earlier incomplete capture. Warm cache and rival selection add no new requests. Replaying 1081 raw transfers through the parser/locker merge took 1.985 seconds. No schema, dependency, background process, timer, or UI redraw path changed. Captures remain private outside the repository.

Executed local checks: full-history pagination, new adversarial same-second ID/time ordering fixture, repeated-cursor partial coverage; locker identity/overlap/scoped persistence; locker asynchronous heartbeat/cache/isolation; 18 finance-runtime checks. Hosted authenticated balances are not yet directly verified.

Additional checks: offline Shiny lifecycle passed all 13 checks plus eight tabs; deployment contract passed all three checks. No authenticated hosted screen was used as a substitute for these local checks.

Hosted rollout proof: commit `8c06d6f0ffb71cda73b7b1ea026e96f33edc0392` pushed non-force to branch `2.0`. Fresh browser reached HTTP 200 and a connected Shiny session with release `transfer-page-boundaries-2026-11-03` and verified `futmondo_functions.R` SHA256 `c708ec6ab23678c449b03e7e3eef83e8cfad3e0c2e76e3c91ef08ecbe052d320`. The live source also returned an empty page after the recovered earliest boundary second, establishing source exhaustion. Authenticated hosted financial screens and subsequent database persistence were not directly inspected; the half-euro tied-prize residual remains unresolved pending actual payment evidence.


### Exact HOPE reconciliation after confirmed tied payment — 2026-11-03

User relayed actual round-6 position payment of 5,666,666 EUR. Updated shared tie allocation to floor the mean of rounded slot prizes. Captured-source replay now equals independently reported 426,122,711 EUR exactly; reference actual cash stays 52,129,125 EUR. No balance adjustment or fictional transaction was added. Fourteen reconstruction checks and a sanitized known-answer tied payout fixture pass. Performance review: one scalar floor per existing tied-score group; no added API/DB request, cache invalidation, persistence work, reactive publication, or memory structure. Existing shared calculation reuse remains intact. No dependency/schema/bundle membership changes.

Tied-payment delivery checks: 18 finance-runtime checks, seven reference-payment gate checks and three deployment checks also passed. Commit `1a59734e9954c8981a66ffeec979ccd1eebb7b31` pushed non-force to `2.0`; a fresh hosted browser returned HTTP 200, connected Shiny and release `tied-position-whole-euros-2026-11-03` with runtime hash `e5cc07058de7542eb9b93c2f4ccd4e02f2e0742275b6327a7609d6c3ac29fa11`. Exact balance equality was verified in the captured-source ledger replay; authenticated hosted financial screens were not directly inspected.


### Hosted clause-only balance root cause and calculation reuse — 2026-11-03

The reported 632,606,939 EUR exactly equals opening cash plus clause transfers and bonuses. Ordinary transfers net -206,484,228 EUR; restoring them produces 426,122,711 EUR. User logs show pressroom FM-NET-000005 followed by a different successful 1118-row/10-page acquisition. The rivals seed swallowed its own failure, restored only locker transactions and did not consume the later successful shared feed. This release restores existing ordinary database records and subscribes to fresh scoped transfer publications. Captured real database rows for the affected rival plus saved clauses independently reproduce the exact confirmed balance with live HTTP absent. Source replay still gives reference cash 52,129,125 EUR. A read-only live async seed also returned all 1092 merged transfers; this is acquisition evidence, not an authenticated hosted UI check.

Performance: captured ten-team preparation/full ledgers measured 4.669 seconds initially and 0.394 seconds on unchanged reuse (about 92% less calculation time). Pure loading/status publications reuse the prior result directly. New/revised transfers rebuild only affected teams. Existing memory cache bounds (32 MiB/256 entries policy) apply to reusable calculation entries; no background process, polling timer, new database table, or new database calculation payload is introduced. Initial CPU preparation still runs on the Shiny thread and remains a cold-load limitation. Restoring ordinary transfers adds paginated reads of the existing source-identified market table (two pages for current full league history). Subsequent unchanged source refresh needs one pressroom page versus ten for a full scan; warm cache needs zero requests. A daily active-session full audit and explicit Refresh recover older revisions. Full cache eviction/server restart requires the baseline acquisition/calculation again.

Checks cover exact equality of cached and uncached ledgers, additions/revisions, affected-team build counts, rules/account invalidation, bounded in-memory storage, recorded-opening date scope, exclusion of unidentified legacy transfers, source Market counterparties, concurrent transfer recovery with zero extra requests and account isolation. Browser checks exercise both row click and radio selection through reorder and live balance changes, league-change clearing, and removed Status/Confidence columns. Historical labels and existing settlement/round metadata checks pass. No real account or transaction was used in tests. Private read-only investigation captures remain outside the repository/bundle.

An automatic approval review rejected a proposed serialized database calculation checkpoint as a new sensitive payload. That proposal was not implemented or deployed; this release uses existing persisted source facts and bounded server memory.

Final executed checks: 18 finance-runtime checks; seven reference-payment gates; round/date/history display checks; new reuse/recovery tests; incremental and boundary pagination tests; locker identity/persistence tests; cooperative locker heartbeat/cache isolation; browser row-click and radio selection/reorder/context checks; offline Shiny lifecycle (13 checks plus eight tabs); three deployment-contract checks. Status-only updates no longer replace the table data, and unchanged directory membership no longer invalidates the selected-team resource reads. No authenticated hosted financial screen has yet been inspected.

Hosted delivery: pushed scoped commit `6e65b61f87ac06a3b3d8a3e31749d6e8df792db3` non-force to `2.0`. Fresh browser verified HTTP 200, connected Shiny, release `rival-reuse-and-transfer-recovery-2026-11-03`, finance hash `0f605f25abecb37ca1d0ae9d2a9b9eee731a63e303ee6d17e0ecd76979f0b1c5`, rivals hash `028b0b1b34632cde1a34a4041f7931da21e1cf21b90e18c37277cefe53a053e0`, data-service hash `4f167f83dee70ab641308f41b4866937d84c4db9ab1203029dc21d487df4012a`, and source-functions hash `2f433b3cf04105aaa7c2b421415127e95cfc26fea25eaa3933fc0927fc37694d`. Authenticated hosted balance presentation is not claimed verified; exact value was established through existing-DB and captured-source replays.


### Settled award freshness and missing date — 2026-11-04

Read-only source investigation reproduced adri_g42's €33,768,672. Saved round 11 Best XI omitted Raphinha; a fresh official selection and historical lineup establish the missing €500,000 award. Replaying the full ledger with those source updates yields €34,268,672, exactly the supplied official screenshot. No balancing adjustment was introduced. Round 6's €500,000 Best XI and €1,000,000 MVP were already included; their missing date came from rejecting the sole `6.2` position-settlement label. That single settlement now dates them at 2026-10-22T13:47:09.015Z under the confirmed league timing rule. The screenshot alone does not independently prove each private award payment.

Verification: sanitized settlement freshness/reuse tests, 16 cash-reconciliation checks, 11 Admin award/coach checks, saved-bonus recovery, reference bonus gates, calculation reuse, and offline Shiny lifecycle. Private read-only capture replay is separate from tests and remains outside the bundle. No authenticated hosted financial screen was inspected.

Performance review: freshness comparison scans the evidence already loaded for the award catalog, adding no database read. Only affected settled rounds need award HTTP requests, with the existing concurrency limit of two and acknowledged persistence. A changed selection can refresh historical lineups through the existing signature queue; unchanged saved selections do not repeat this work. The new fixture verifies one acquisition/save across two sessions and no request on repeated navigation. Calculation cache version advances so old ledgers cannot retain the date bug. No new timers, workers or unbounded caches; first recovery still incurs the existing full shared calculation cost.

Delivery verification: commit `8bfd3a9c3c5abd6f7b7fc015c92db8fe5bde9ba1` pushed to `2.0`. A fresh hosted browser returned HTTP 200, connected to Shiny, and served release `settled-award-refresh-2026-11-04` with matching finance and award runtime hashes. This verifies the deployed bundle/startup, not the authenticated hosted rival balance; the exact €34,268,672 comparison was the separate read-only source replay.


## Incremental query optimization — 2026-11-05

Implemented locally in the existing edited working tree; no hosted schema change, application deployment, real-account request, or transaction was performed. The new durable source cache needs `scripts/migrations/20261105201845_complete_history_baselines.sql` before its cross-restart reuse is available in production.

Root causes: pressroom incremental state was tied to short-lived display entries and a daily full-read gate; locker/news always paged to the beginning; player cards repeatedly reread historical database rows; successful transfer refreshes wrote the complete merged history again. Source evidence supports cursor pagination, not a date-range API field. The implementation scans from the current source head to the latest verified source timestamp minus 24 hours, keeps complete baselines separately, and applies observation/ingestion-time predicates to database history.

Measured controlled fixtures (request counts are fixture measurements, not claims about hosted latency):

| Path | Before / full baseline | Incremental / reuse |
| --- | --- | --- |
| Native transactions, 1,200 events with dense recent activity | 11 API pages | 3 overlap pages; fresh-cache reuse 0 |
| Locker news | 5 API pages | 1 overlap page; fresh-cache reuse 0 |
| Player history, 1,200 rows | 4 DB requests / 1,200 rows | 2 requests / 26 rows (97.8% fewer transferred rows) |
| Shared league dataset, 1,504 point/check rows | 3 DB requests | 2 requests; corrected old round and empty acquisition markers preserved |
| Transaction persistence, 310 events | 310 transfer, auction and winning-bid rows | Unchanged refresh 0 writes; single correction 1 affected row in each fact table, plus parent validation |
| Source fingerprint comparison, 1,200 rows | Not previously used | 0.141 s elapsed, 297,808 bytes retained fingerprints in the local measurement |

Executed checks include `test_pressroom_incremental.R`, `test_pressroom_full_history.R`, `test_locker_async.R`, `test_locker_transfers.R`, `test_player_history_incremental.R`, `test_league_history_incremental.R`, `test_transaction_delta_persistence.R`, `test_round_dataset_reader.R`, `test_recent_points_async.R` (27 checks), `test_observation_recording.R` (9), `test_history_persistence.R` (7), `test_pressroom_player_parents.R`, `test_data_service.R` (10), `test_history_independent.R`, `test_progressive_loading.R` (16), `test_background_sync.R` (6), and `test_async_transport.R` (4). Delayed async fixtures preserved UI callbacks, including the 36-page locker history fixture. The required `Rscript test/test_shiny_simulation.R` completed 13 lifecycle checks plus eight rendered tabs with zero live HTTP/database mutation attempts. Existing Plotly fixture warnings remain unrelated to the change.

Two pre-existing known-answer assertions in the data-service/async fixtures discarded a known round with unavailable points. The existing HEAD implementation already retains that round as `NA`, as required by the history contract; the assertions now explicitly check its unavailable score/status. Native pressroom-only fixtures now stub the independently covered locker endpoint rather than interpreting an unrelated capture as locker data.

Performance review: no new timer, worker, automatic trade, or synchronous HTTP path was added. Active consumers still use the asynchronous transport. Account/context-scoped history reuse reduces page count and replay parsing; locker HTML is parsed after required pages arrive. Player baselines and acknowledged transfer fingerprints share the existing 256-entry/32-MiB cache. Complete transaction/news baselines additionally allow 32 entries/16 MiB, with a 30-day idle bound and seven-day source audit. Database histories audit daily and retain baselines for seven days within the existing global cap. Persistence groups contain at most 100 transfers, queue the next group only after acknowledgement, and account retained continuation memory against the existing 16-MiB queue limit; individual request payloads remain capped at 1 MiB. No mobile layout/rendering changes were made.

Limitations: first loads, missing/invalid/oversized snapshots, evicted caches, and periodic audits still require full histories. Revisions/deletions older than the overlap are found by the daily DB or seven-day source audit, not by every incremental read. Upstream player-summary APIs still return their supported full payload. A separate 50,000-row direct-pager microbenchmark measured 0.438 s / 51 calls versus 0.086 s / four calls / 2,500 rows; it bypassed the adapter's existing composite read budget and is not a production-capacity claim. Production DB plans, mobile memory peaks, network latency, cold-login wall time and hosted speedups remain unmeasured.


The continuation review additionally exercised `test/test_persistence_continuation_bounds.R`: retained state decreased from 217,360 to 178,544 bytes after saving the first 100 records; cancellation discarded the remainder and did not restart the chain. Duplicate callback state exceeding the shared 16-MiB bound was rejected. The snapshot acknowledgement callback is independently checked to retain only scalar identity/hash values.


Final durable-storage checks passed: `test/test_history_baseline_store.R` (13 groups, including 1,000 fractional timestamp round trips, bounded decompression, scope checks, failed saves, minimal callback memory and delayed-read responsiveness) and `test/test_history_baseline_schema.cjs` (five isolated PostgreSQL groups: repeatable migration, concurrent-observation ordering, bounds/privileges and fresh-schema integration). The actual 1,203-row parser integration restores after clearing all process caches with two narrow DB reads plus three source pages; a failed cold fetch leaves the stored checkpoint unchanged. The 2,500-row snapshot preparation benchmark used 332,381 JSON bytes, 11,512 encoded bytes and 0.402 seconds locally. This CPU work is bounded but is not a hosted performance guarantee; larger snapshots still require encoding/validation work.

After the memory/cancellation fix, the required lifecycle harness was rerun and again passed all 13 checks plus eight tabs. Continuation-bound coverage passed three groups, including callbacks attached during an active HTTP operation. The updated schema helper checks and deployment contract also passed. `rsconnect::writeManifest()` was regenerated and inspected: the new runtime is bundled; credentials, captures, backups, tests, docs and scripts remain excluded. The local R installation lacks the already-required `sodium` package, so regeneration omitted it; the unchanged committed `sodium` 1.4.0 package record was preserved in the manifest and the deployment dependency check passes. No runtime dependency was added or upgraded by this change, and remembered-login cryptography was not newly validated in this environment.


### History availability and incoming-signing recovery — 2026-11-05

The reported counters exposed multiple reproducible application defects. The table validator rejected an entire player history when one otherwise valid round omitted its score, although the observation model already supported an unavailable score. Failed acquisitions were excluded from future work as if permanently finished, outer rejected promises could retain a pending slot, and progress counted failed/held players as loaded. Saving could strand the active slot on a synchronous serialization/transport exception; sanitized HTTP errors lost classification; a later successful save with a new timestamp could leave the old error visible. Incoming transfers lost previously verified payment identities before re-resolving current team/player names, so a rename or removed player could invalidate the complete feed.

Fixes retain unknown scores as unknown, settle every acquisition/write outcome, bound transient recovery, preserve score-first/check-last persistence, keep fitness groups held during retryable anchor failures, and reuse only exact unchanged uniquely identified locker payments from a complete prior baseline. Permanent/malformed/stale data remains truthfully unverified. Safe diagnostics now distinguish roster versus transaction failures. No manual transaction is automatically retried. Existing source captures inspected for contract evidence contain 17 summary responses, eight explicit empty histories and 50 valid score rows; they do not identify the user's particular failed player or prove that the missing-score variant caused that hosted incident. No source-wide empty-history fallback or unscoped profile-score substitution was added.

Executed root checks: asynchronous read recovery (7 groups), real local HTTP transport (4), safe read diagnostics (6), shared data service (10), progressive loading (16), observation recording (9), and persistence continuation memory/cancellation (3). The observation failure fixture now expects three bounded attempts for an idempotent parent insert returning HTTP 503, still with no dependent write after failure. The real HTTP card fixture measures five cold requests including one retry of the failing score-history DB read, zero warm requests and five explicit-refresh requests; it preserves zero, negative and unavailable scores. Its dispatch took 0.033 seconds with heartbeat before the delayed response. A first offline lifecycle run passed all 13 checks plus eight tabs, with zero external HTTP or live database mutation attempts; the final controller changes also passed the same required lifecycle, as recorded below.

Performance review: successful reads/writes add no retry traffic. Factual transport limits are two attempts per read and 32 retained requests including timers; the history controller retains two acquisition slots, at most three attempts per player and 32 extra context acquisitions. Eligible persistence jobs have at most two extra attempts, one cooperative retry timer, existing queue/payload/memory limits and no replay of earlier acknowledged chunks. Mutable merge-upserts are excluded from automatic retry, preventing a late old snapshot from overwriting a newer one. Known locker payment identities avoid the second catalog lookup. Continuation coverage still measured retained state shrinking from 217,360 to 178,544 bytes after the first batch. Tab inactivity can leave one already-scheduled transport retry; obsolete consumers reject publication, and logout cancels the retry. Timers support bounded recovery during active demand, not polling or unattended acquisition.

Limitations: these are isolated fixtures and offline lifecycle checks, not measurements of production latency, the actual 527 player response set, mobile rendering or hosted database acknowledgement. The earlier transaction-baseline migration has not been applied by this follow-up. The recovery fix itself introduces no schema or package change. Local deployment checksum verification and final test results are recorded below.


Final verification: `test_round_history_recovery.R` passed 14 groups, covering the exact 527-player case, persistent/malformed failures, finite retry budgets, fitness-anchor exhaustion/recovery, explicit N>5 demand, canceled callbacks, season/team/scoring changes, failed-only initial Refresh and stale database metadata. `test_recent_points_missing_score.R` passed three actual local HTTP/ordered-persistence groups: 0, -2 and NA survive normalization, all three score observations precede the acquisition marker, and the safe HTTP failure survives the parser boundary. Existing controller tests passed 27 groups and fitness mapping passed 18. The exact 527 fixture reused 512 histories and acquired 15 using 17 logical reads (16 adapter invocations), 15 snapshot writes and three shared publications; all four held saves acknowledged to zero. Warm hide/show added no initial/player reads or shared publications. The fixture's complete synchronous assertion/settlement workload took 9.663 CPU seconds / 9.661 wall seconds locally; warm hide/show took 1.029 seconds. These include mock-session flushing and assertions, not hosted network or user-visible latency. The existing 527×38 saved-matrix/N-change fixture took 4.587 seconds with zero summaries, and the 500-player mapping fixture took 2.410 seconds with one probe. Availability calculations now share their reusable/needed scan instead of duplicating it for completion counts.

Persistence recovery passed 11 groups, background synchronization six, independent dataset/persistence contracts ten, and incremental league history three. Incoming recovery passed six groups, existing incoming signings 20 and acquisition availability seven; locker reconciliation and incremental/cold-restoration transaction tests also passed. The acquisition fixture was corrected to supply its explicitly empty locker feed, which its native preflight mock previously omitted. Request safety and retained evidence gates were not relaxed. The unchanged native acquisition fixture measured five requests, 0.723 seconds and a responsive heartbeat. Incremental league retrieval still uses three full-read requests versus two delta requests; a failed delta retains 1,500 saved points/five checks and the safe source diagnostic, then successful recovery clears it without extra queries.

The final `Rscript test/test_shiny_simulation.R` passed all 13 checks plus eight tabs with zero HTTP/live database mutation attempts. Expected synthetic empty-plot/schema warnings remain. All 38 bundled file checksums match final runtime files; all three deployment-contract checks and scoped whitespace checks pass. No runtime dependency changed, so existing package records (including the already-required sodium pin) were preserved while refreshing source checksums. Credentials, captures, backups, tests, docs and development scripts remain excluded. No deployment, production migration or authenticated hosted recovery was performed by this fix.


### Authorized history-recovery rollout — 2026-11-05

The user authorized deployment. The Supabase connector applied `complete_history_baselines` as version `20261105220658` after its separate confirmation was approved; the two earlier confirmation expirations made no schema change. Production metadata checks confirm the expected 12 columns, scope primary key, payload/hash/resource constraints, RLS enabled, no browser-role object grants and service-role-only privileges. The update trigger is enabled, SECURITY INVOKER, with an empty search path. The new table was empty at verification; no existing observations or account records were rewritten. Its intentional private-table/no-policy advisory is informational ([Supabase advisory reference](https://supabase.com/docs/guides/database/database-linter?lint=0008_rls_enabled_no_policy)); preexisting unrelated warnings were unchanged. Local REST credentials were unavailable, so an actual hosted server REST acknowledgement remains distinct from SQL privilege verification.

Prepared the release from deployed branch head `b18f1b930b0f17dfc9a42ae447cbff295fbb39be`, preserving unrelated workspace changes in round-MVP presentation, player columns, theme and utilities. Existing deployed package records were retained; no package addition or version change was included. The exact isolated release passed round recovery14, real HTTP/missing-score persistence3, incoming-source recovery6, deployment3 and the mandatory lifecycle13 plus eight tabs. All38 bundled files have matching checksums. The static `history-loading-recovery-2026-11-05` marker reports hashes for the11 changed runtime files and adds no application timer or source query. Hosted publication verification follows below after push.


Hosted publication succeeded: commit `3bbb12de0e1bea9e138f1cd49e9bab46eb19b165` was pushed non-force to `2.0`. A fresh public Chromium session at [Futmondo](https://smdb21-futmondo-v2.share.connect.posit.cloud/) returned HTTP200, connected to Shiny and served `history-loading-recovery-2026-11-05` with the11 prepared runtime SHA256 values. Key deployed hashes: Players Table `ee41a628de4e77f05402126e926e9dec07a67ea95fb80c426c1ba9df93eb4c33`, Incoming Players `f3a9faa85510a98cf6da277fdf2b4587806fbd640096745accf1d0b4e70d84c7`, source functions `f253e0fe347d623b3e75e7ec1a31537f1098f508195711b2d919ad6ada876300`, and persistence runtime `24fe816e01923d04ad6e2dbcf6139c0ae6031beac165708ab3576fa8ec639ff8`. The exact committed bundle's38 checksums were independently rechecked after push. No account login or trade was performed; authenticated 527-player recovery, incoming-signing verification and cache-save acknowledgement still require actual active-session evidence. Updating release documentation/static metadata has no additional runtime request, database or rendering cost.


### Player availability delayed by history-save processing — 2026-11-05

Opening a player from Players had no explicit dependency on saving, but each successful recent-history acknowledgement rescanned the saved matrix and marked all fitness reconciliation dirty. This blocked the R event loop while the pending-save count decreased, delaying otherwise asynchronous card callbacks. A synthetic 527-player ×38-round scenario with seven remaining saves reproduced 18.071 seconds of acknowledgement processing and a 2.640-second maximum continuous heartbeat gap. Without fitness vectors the same redundant matrix scans alone consumed about 5.2 seconds. These are local reproducible causes, not measurements of the user's hosted Berenguer session.

The controller now retains bounded, context-scoped stored-ID/window-coverage/latest-round projections, invalidates them when their inputs change, and rechecks fitness on acknowledgement only for a still-unresolved player. Independent review caught and fixed two invalidation edges: a newly retrieved global round rechecks other players before acknowledgement, and an initially future-dated observation becomes eligible at the existing now+60-second boundary. Replacing a saved matrix also invalidates all projections and fitness. No unknown score is changed to zero, no acquisition marker precedes successful persistence, and manual-action preflights are unchanged.

Clean final measurement: seven acknowledgements took **0.232 seconds**, with **0.127 seconds** maximum heartbeat gap; zero repeated full-matrix/fitness scans, additional reads or shared-history publications. All527 acquisition checks remained present. Retained cache size was 139,048 bytes for the active N and 782,160 bytes across all 20 cached windows, including the scalar latest round, within each existing bounded controller entry. The two-entry/50,000-row limits remain; caches retain IDs rather than another full matrix. No new packages, network requests, timers, workers, database migration, DOM elements or mobile layout changes.

`test_player_card_during_saves.R` passed 4 actual table-to-card groups with a synthetic Berenguer row and seven held saves: availability dispatches and verified controls render before saves finish, then acknowledgement counts settle without refetching the card or losing zero/negative points. `test_round_history_ack_responsiveness.R` passed the 527×38 benchmark and requested-N/live-round/season invalidation assertions. Independent `test_round_history_cache_invalidation.R` passed 7 groups for cache bounds, membership, changed rounds, held/failed saves, truthful completion, future timestamps and season isolation. The offline lifecycle passed 13 checks plus eight tabs before the final edge-case corrections; the exact release is rechecked below.

Separate localhost transport analysis confirmed bounded request contention remains possible: four sequential availability reads with 80ms total synthetic server delay finished in 1.78s when queued after slower optional/background reads. This release leaves the four-connection transport and freshness checks unchanged. Remaining upstream latency and authenticated hosted responsiveness are not established by these fixtures.


Final exact-release checks on frozen Players Table SHA256 `4619557fe5e92068c7136aa9a059fe5be4a60190e5b01ce6782ac22bc263be71`: table/card4, cache invalidation7, round recovery14, deployment3, and required offline lifecycle13 plus eight tabs all passed. Existing recent-points controller27 also passed after cache bounds; the clean responsiveness fixture passed2 groups on the same frozen source. The final private-helper test supplies its requested catalog IDs explicitly, matching the new cache membership contract. All38 bundle checksums match and all78 deployed package records remain unchanged. The only runtime-code modification is the Players Table controller; the release marker and documentation/tests accompany it. No account login or live transaction was performed.


Hosted rollout verified: commit `790cc1bdab777449f79beaf26a242176c493ea60` was pushed non-force to `2.0`. A fresh Chromium session at [Futmondo](https://smdb21-futmondo-v2.share.connect.posit.cloud/) returned HTTP200, connected to Shiny and served release `player-card-save-responsiveness-2026-11-05`. Its Players Table SHA256 is `4619557fe5e92068c7136aa9a059fe5be4a60190e5b01ce6782ac22bc263be71`, matching the tested bundle. This verifies publication/startup; no authenticated player session was used to claim a hosted latency measurement. Documentation/static metadata updates have no additional query or rendering cost.


### Player-card availability expiry and request scheduling — 2026-11-05

The user's second screenshot identifies the exact failure: “Availability checks expired before they completed” after roughly 30 seconds. The deployed adapter froze the whole serial capacity read at its start while capacity expired after 15 seconds. Thus a successful slow composite could already be unusable. The outer 20-second card cache could also reuse an expired nested snapshot, and a slow player summary could outlive previously successful team checks. Separately, queued libcurl requests consumed their network timeout before a connection was available. These are independently reproduced defects; the user's private source timings were not recorded.

The actual Selected Player module reproduced that error against deployed adapter/module code with synthetic Berenguer data and a 31-second virtual delay. The fixed native-parser fixture explicitly holds history while information, roster and market settle, then advances 31 seconds: eight physical reads total (two history pages and two each for information, roster and market). Only the three expired mutable sources are reread; history is retained. The completed preflight is valid, actions render enabled, and immediate close/reopen adds zero reads. A separate four-group module test verifies late-summary recovery, warm reuse, reopening at 16 seconds inside the outer 20-second TTL, and a repeatedly expired source stopping after one card recheck with actions disabled. Testing exposed Shiny's Boolean `disabled` contract: passing the string “disabled” had left gated actions clickable, although server preflights still protected submissions. The four affected card flags now use Booleans.

New isolated checks: parallel acquisition9, source-expiry cache6, card late/reopen4, native slow-history/reuse2, real-socket priority9. Existing checks passed: acquisition availability7, incoming-source recovery6, clause preflights6, configured acquisition rules6, data service10, transport4, async read recovery7, progressive loading16, session resource ownership3, market listing actions4 and table-to-card during saves4. Independent review passed candidate replacement/removal, retained failure diagnostics, funds/effective-roster calculations, warm reuse and account isolation. An additional mixed-owner cancellation check settled all16 queued promises, preserved the other owner's eight jobs after the first cancellation, and left zero tasks/read states/timers without dispatching HTTP.

Performance review: independent prerequisites use at most four slots; pending summaries use two slots/max32 identities. One freshness pass rereads only expiring observations, retaining verified complete transaction/history baselines. The card has one event-driven recheck, no new polling timer. Warm capacity reuse costs zero reads. The transport keeps four active requests and32 combined logical admissions including retry delays; queued interactive work is FIFO with one normal dispatch after at most four interactive dispatches. Queue scans/memory remain bounded. The same12-request loopback fixture improved availability from779ms ordinary priority to189ms interactive priority, maximum four active connections, with responsive heartbeat. A20ms response queued behind250ms succeeds with a100ms network timeout; the old direct-libcurl queue timed out before the request reached the server. This changes queueing, not retry limits or manual-action semantics. No new dependencies, schema, worker, DOM structure or mobile layout was added.

Limitations: already-running requests still occupy slots; a cold complete history can still take time. Source deadlines remain based on actual observations; an exceptionally slow final phase can still settle partial after the bounded refresh rather than authorize stale actions. No source failure is converted to zero/success. No real account or transaction is used in tests. The full exact-release lifecycle/bundle and hosted publication are recorded below when complete; synthetic measurements are not authenticated hosted timing claims.


Final exact-release verification passed native card2, late-summary/reopening4, parallel acquisition9, source expiry6, transport priority9, deployment3 and the required offline lifecycle13 plus eight tabs (zero HTTP/live database mutation attempts). The four runtime files were frozen before those checks. Direct-sale6 passed after repairing two obsolete fixture assumptions: contextless cache insertion and a missing scoped manual-invalidation mock; production sale behavior was unchanged. All38 bundle checksums match, all78 deployed package records are unchanged, and scoped whitespace review is clean. Credentials, captures, tests, docs and development scripts remain excluded. The new release marker includes all four changed runtime hashes; no new dependency requires manifest dependency regeneration. Public publication follows below after the authorized push.


Hosted publication verified: commit `1cfdd4aa42c615356cb637cc8a98af562e0c512c` pushed non-force to the GitHub `2.0` branch. A fresh public Chromium session returned HTTP200, connected to Shiny and served `player-availability-freshness-2026-11-05` with all12 marker hashes matching the prepared release. The changed runtime SHA256 values are transport `cd9e20b307643909f58c357ef051e344eda07b7b3073e80a628ee87bd1946a2e`, data service `69cfe9abd65a15acddebf16cd51b70be2cafde0f5ea5f48fbe757f41fa80f1d1`, source functions `77830a364a0d1fc66b0eac2568cb3cf384bdd0bb86475d6ca4827d513e18e09f` and Selected Player `46e506656c87ed8dedd9f1a14c52e4a0f3a56fd93047880983a0d180372d8fe8`. The isolated release checkout is clean. No authenticated account, real transaction or production data mutation was used to verify this follow-up. Publication/startup is confirmed; the user's live Berenguer timing remains distinct from the reproduced expiry and delayed-source tests. Updating these delivery notes/static metadata has no query or rendering cost.


### Clause purchase restarted league-wide round-history retrieval — 2026-11-05

The successful clause callback advanced the same global revision used by the table history controller for explicit correction requests. The controller therefore forced all current table players, cleared their attempts and invalidated summary caches even though no score changed. Separately, shared-resource refreshes replaced same-context values with an empty loading state, so the table vanished while catalog/ownership data refreshed. An expired factual table read also discarded retained rows and could demand repeated refresh work.

The root session now has a separate explicit history-correction revision. Global Refresh advances both data and history revisions; successful market actions advance only the mutable-data revision. Card Refresh history remains explicit, and the selected player's summary still refreshes for current ownership/bid facts. Saved/live points, acquisition markers, pending saves and acknowledgement callbacks are retained. Same-context refreshes keep old display values in a stale/loading resource state; acquisition actions continue to block on loading and verify source deadlines. Context changes clear prior data. Failed refreshes retain stale evidence and the source error without manufacturing fresh facts or looping.

`test_clause_refresh_scope.R` passed 11 groups through the actual Players table, player card, successful synthetic clause endpoint and normal callback. A 527-player matrix completed with one save acknowledgement held; buying the selected player caused one catalog read and **zero** player-history or matrix reads. The table remained unchanged while that catalog response was held, the fresh transfer state replaced it afterward, and the original acknowledgement still settled correctly. Explicit history refresh dispatched two bounded correction reads. Separate assertions cover stale/loading acquisition state, failed refresh, context isolation and expired-table refresh coalescing. The integrated 527-player scenario took 9.755 seconds locally, including initial rendering and simulated card/purchase/manual-refresh lifecycle; this is not hosted latency.

Existing focused checks passed: durable shared-history controller 27, fitness/changed-score recovery 18, progressive card/history refresh 16, source-expiry 6 and late/reopened acquisition-capacity 4. Required `test/test_shiny_simulation.R` passed 13 lifecycle checks plus eight rendered tabs, with zero HTTP/live database mutation attempts. Deployment contract 3 passed after updating the five affected runtime checksums. No new dependency, schema migration, timer or recurring API query was introduced.

Performance review: ordinary mutations now reuse the historical matrix rather than scheduling up to 527 source reads. The fixture verifies one mutable catalog request and zero historical queries after purchase. Retention reuses the same bounded session resource/cache infrastructure and marks prior evidence stale until a result arrives; repeated reads during loading do not add requests. Source invalidation, money/roster/capacity/incoming refresh signals and manual transaction preflights are preserved. Authenticated hosted behavior, mobile rendering and actual purchase latency remain unverified; no real transaction was performed for these tests.


### Recent averages and compact player identity — 2026-11-06

The reported failure is reproducible when N=5 uses the source scalar but numbered history cannot fill N=2. All eight screenshot players have complete five-score vectors in the captured catalog. Sanitized summary examples give trailing-two means 7, 2, 20 and 9; explicit summary history includes seven rounds in one capture, confirming it is not restricted to the five-score fitness array. Some older source-list entries have round labels 2,1,3,4,5, so the whole vector cannot always establish the existing strictly numbered anchor. No live account request was used to diagnose this path.

`add_recent_points_average()` now computes N≤5 from the trailing N reported sequence values when complete numbered history is unavailable. It preserves source order, missing slots, zero and negative points, and records internal provenance as `reported`. It creates no round IDs, score observations or acquisition markers, and leaves retrieval, context isolation, anchor validation and ordered persistence intact. Full explicit history takes precedence. N=8 continues to use eight genuine numbered round scores; it never extends or rescales a five-score vector. Captured evidence does not prove a new numbered round when a repeated source vector is unchanged, and source omissions remain unavailable rather than zero.

The shared Name formatter now includes the existing club crest and accessible club label, hiding the separate Team and logo columns. Position follows Name, then the average and optional value/clause ratio. Names retain raw sortable/filterable values, and long names wrap. The same image is moved, so no additional image/API/database request is introduced.

Verification on the clean remote-branch copy: recent-average arithmetic/display 13 checks; independent captured-score/controller suite 15; crest/name/order suite 4; both ratio bases with N5→N2→N8 changes and explicit-history replacement; deployment contract 3. The working-copy shared-controller suite passed 27 groups and fitness mapping/correction suite passed 18, preserving bounded acquisition, successful save acknowledgement and cross-session reuse. Browser checks at 1440px and 390px passed crest placement, Name/Position order, absent Team, N5→2→8→2 values, no visible errors and no page overflow; screenshots were inspected with synthetic crest images. Browser table history was synthetic; actual controller acquisition and persistence were verified separately by the independent suite.

Performance: 527 players across 20 source-window recalculations took 0.623 seconds on the clean release (31 ms mean, without browser rendering); changing N over retained history issued zero additional requests or saves. Independent helper measurement at the 2,000-player bound took 0.127 seconds median/0.155 maximum. Crest markup for 527 rows took 0.070 seconds. The existing enrichment dependency/cache means filters do not repeat those calculations. No new timer, queue, cache, worker, dependency or database migration was added. Existing lazy image behavior and horizontal table scrolling remain. Generated bundle checksums were refreshed (including pre-existing stale checksum metadata); package records and exclusions remain unchanged.

The required offline lifecycle passed 13 checks plus eight tabs in both the working copy and the exact clean release, with zero external HTTP/live mutation attempts. All 38 clean-release bundle checksums match; all 78 package records are unchanged. The scoped release is based on remote commit d1bc904 and targets branch 2.0 without force. Authenticated hosted recovery, current private histories and production response times were not exercised. No hosted deployment is claimed by these local checks.


### Eight-round history recovery — 2026-11-06

A synthetic reproduction used five saved rounds (6–10), a valid old source-success marker and an agreeing current last-five vector. N2 produced 18.5; N8 showed a blank value and zero player-history reads despite a ten-round source response being available. The controller had treated the old marker as arbitrary-N coverage; explicit Refresh fetched the missing rows. Separate captured summaries use only championship/team/player identifiers and include up to seven numbered source rows while fitness contains at most five; no N parameter or verified eight-round live response was present.

The corrected controller issues one targeted active-table request for the uncovered N8 player, retains two-request concurrency and existing write backpressure, publishes 79/8 from ten signed/zero source scores on visible completion, writes every returned score before acknowledging the check, and reuses the saved matrix in a new session. A fully covered visible player and an offscreen partial catalog player trigger zero extra reads. Switching N during a held request does not duplicate it. A sparse successful response remains blank and does not replay within the same active context. The older short-window source sequence and N2 behavior remain unchanged.

The exact-release wider-demand regression passed 10 groups with zero external HTTP. Existing shared controller tests passed 27 groups, fitness mapping/correction 18, cache invalidation 7, and the independent source/table suite 15 after updating old expectations for N8 demand. The required offline lifecycle passed 13 checks plus eight tabs with zero external HTTP/live mutation attempts; the deployment contract passed 3. The 527-player × 38-round fully covered N3/N5/N8 fixture took 2.294 seconds with zero player-source requests; this includes mock Shiny session setup and is not production latency. New demand may add one source request per uncovered visible player on a wide N selection, at most two concurrently. It adds no timer, background worker, new endpoint, database query, schema migration, package or mobile UI element. The request and persistence budgets remain bounded. A genuinely sparse source can still require one new check in another session because the current durable marker lacks requested-N/source-revision metadata. No authenticated live account or hosted response time was measured.


### Initial Admin round labels and future-award warnings — 2026-11-06

The supplied capture reproduces the actual boundary defect: `activechampionships.answer.rounds[].championshipId` matches the competition league ID, while the parser required the fantasy championship ID. It dropped the current-round anchor, so unknown finality and missing display numbers let all 38 rounds enter explicit retrieval. Number hydration occurred only inside award acquisition, explaining why initial navigation displayed internal IDs.

The parser now retains the verified league-scoped current ID/number. Shared active-page loading obtains explicit numbers and completion evidence from one cached bulk `POST /2/league/matches` `{leagueId}` read, joined by round ID. Saved official-client request/parser code establishes `answer.rounds` and `_id/number/status`; no actual authenticated bulk-calendar response was supplied. Future/current rounds remain outside Admin rows, selector, grouped issues and award requests. Missing historical data or contradictory metadata remains unavailable with its actual issue. Every explicit retrieval mode refreshes the scoped base/bulk calendar, so a warm earlier in-progress flag cannot suppress a newly completed round. Unknown future finality alone needs neither another metadata read nor a warning.

Final isolated current-remote release passed **38 focused checks**: initial Admin 6, refresh 2, closed-round 4, awards/coach 11, scoring 5 and MVP 10. Deployment exclusions/dependencies/files 3 passed. The required `Rscript test/test_shiny_simulation.R` passed 13 lifecycle checks plus eight tabs, with zero external HTTP/live database mutation attempts. The 38-round native-adapter/Admin fixture displays numbered historical choices before any dreamteam query, fetches 11 completed awards with 27 excluded and zero failures, persists all 11 acknowledged snapshots, and reopens a new consuming session with zero additional HTTP requests. It also verifies fresh closure after warm metadata, foreign league rejection, unordered identity joins, missing/duplicate metadata truthfulness and future-only unknown finality.

Performance review: one additional bulk-calendar request when cold replaces per-round numbering lookups; normal warm navigation adds zero requests. Explicit retrieval refreshes only the selected account/championship's base and bulk calendar (up to three metadata HTTP reads), then fetches only due awards. Award concurrency remains at most two. Delayed synthetic metadata/award responses preserve an event-loop heartbeat. Only at most 120 compact calendar records enter the existing bounded scoped cache; nested matches are discarded, and no per-player redraw/request is added. No schema, package, timer, worker or layout structure changed. All 38 release bundle checksums match and all 78 package records are unchanged; credentials, captures, tests, docs and development scripts remain excluded. Documentation-only edits have no runtime cost.

Delivery is prepared from remote branch 2.0 head 1a9d926 in an isolated checkout, preserving unrelated local/staged work. These checks establish the implemented behavior and request budget with synthetic HTTP/storage boundaries; authenticated bulk-calendar success, live account behavior, browser/mobile rendering and hosted deployment are not claimed.


### Navigation names — 2026-11-06

The actual sidebar menu now labels `today` as **Today's market** and `yourteam` as **My team**. Their tab identifiers, page modules, loading order and actions are unchanged. The first-page loading status uses the same new market name. The Today heading was already **Today's market**. Current guides use the new page names.

Performance impact: these static strings are rendered in the existing menu and status output. They add zero API/database requests, cache invalidations, timers, reactive redraws or stored state. The longer label is checked against the existing mobile sidebar width; no runtime measurement is needed for the text-only change. The existing offline lifecycle fixture asserts both rendered labels and unchanged tab IDs. Actual checks and any limits are recorded after verification.


### Rival Net Gain totals the shared ledger — 2026-11-06

The reported squad value €711,915,656 and displayed gain −€4,864,399,502 exactly match the old formula when lifetime purchases are €5,576,315,158. This established the display defect: the card subtracted every historical purchase from the current squad while ignoring sale proceeds, rewards and adjustments. Those inferred purchases explain the old calculation; they are not a retrieved ledger for the user's account.

`rival_cash_net_gain()` now totals signed included non-opening entries from the same cash ledger used by Financial history. The shared dataset publishes this once as `net_profit_loss`, with `net_gain_estimated`; the selected card consumes the published total and explains the starting-cash exclusion. Observed squad valuation remains separate. A recorded opening allocation replaces configured starting cash without changing activity gain. Unknown included transaction amounts/types/inclusion stay unavailable, unknown opening cash can coexist with known activity gain, and genuine zero/negative gains are retained. Excluded reset allocations and unsettled rewards do not enter the total. Source acquisition, historical reward reconstruction, persistence and revision deduplication are unchanged. Advancing the calculation version prevents reuse of older summary shapes.

Verification passed in both the working copy and an isolated current-remote release: `test/test_rival_net_gain.R` 12 groups, including independent arithmetic, exact reproduction of the old reported number with a synthetic replacement ledger, all signed reward components, excluded entries, opening replacement, unknown source amounts through normalization/dataset/summary, unknown opening cash, source revision/replay, cache hits, valuation independence and the actual selected-rival Shiny card. The exact release also passed running-balances 14 groups (the existing script prints its count before the last assertion), cash reconciliation 16, financial reuse 3, deployment contract 3, and `test/test_shiny_simulation.R` 13 lifecycle checks plus eight rendered tabs. The workspace lifecycle also passed 13 plus eight. Lifecycle guards reported zero HTTP/live database mutation attempts. A one-observation plot warning in the synthetic card harness is unrelated to the financial assertions.

Performance review: the new calculation scans existing inclusion/type/amount vectors once per affected shared team publication and caches its numeric result. The exact-release synthetic 20,001-entry ledger occupied 470 KiB; 100 evaluations took 0.108 seconds (about 1.08 ms each), without a full data-frame copy. Repeated builds and valuation-only changes reuse the cached ledger; revised source movements rebuild only through existing invalidation. There are no additional API/database requests, source reads, timers, reactive subscriptions, queues, stored observations, dependencies or schema changes. The added explanatory text wraps in the existing responsive grid. Documentation has no runtime cost. All 38 bundle checksums match and all 78 package records are unchanged; tests, internal docs, credentials, captures and development files remain excluded.

Limits: the user's authenticated ledger and corrected live amount were not retrieved. Local synthetic checks establish the formula, render and reuse behavior; they do not certify missing source evidence, authenticated hosted data, mobile browser rendering or production deployment. The scoped implementation, regressions and documentation were committed as `c18b045` and pushed non-force to remote branch `2.0` from `93f9ab5`. Unrelated working-tree and staged changes were preserved. This subsequent delivery-note update has no runtime or performance effect and requires no additional runtime test.


### Separate realised trading profit and overall wealth gained — 2026-11-06

The user clarified that the former signed-cash Net Gain figure was not the requested profit. The selected rival now displays **Trading profit** for completed player ownership cycles and **Overall wealth gained** for reconstructed cash plus observed current squad value minus starting cash. Observed squad valuation remains separate. Starting cash is the actual included opening allocation; the thirteen supplied configuration observations all report €300M, an empty initial squad, zero initial assigned players and no initial squad-value deduction. Thus no additional starting squad value is required in this user's league. The formula is explicitly relative to starting cash; it does not invent a historical squad allocation for other leagues.

The pure `rival_realized_trades()` calculates from the final enriched shared cash ledger, once per affected team build. Source-backed IDs, revisions, included entries and signed amounts remain authoritative. Unique saved pressroom event matches can restore player metadata without adding a second financial entry. Completed buy/sale cycles include source-linked fees; unsold acquisition costs, opening money, rewards and unrelated coach/general fees are excluded from trading profit. Zero/loss margins remain numeric. Missing sold costs/dates/identity, conflicting source identity or unresolved explicit transfer fees keep the total unavailable with exact notes. A name without an authoritative ID bridge cannot establish a player. Equal-time ownership can force an order; fully closed ambiguous batches retain one exact grouped total and original member IDs instead of inventing individual pairings. Full-season source coverage is distinguished from successful pairing and disclosed in the card's retrieved-sales caption and notes.

`rivals_realized_trade_rows()` projects those cached pairs, related fees and unmatched buys/sales into the player pivot, so the card and table agree. Unknown opposite costs remain unknown. Row tooltips use the actual row's dates/types. `cash_activity_complete` also requires an available source and known included activity, so a finite old known-amount cash subtotal or opening-only loading skeleton cannot create false known wealth. Missing cash/starting funds/squad valuation remains unavailable; existing reward/source assumptions keep the wealth estimate label and details. The internal signed-cash compatibility helper remains, but its Net Gain display is removed. The calculation version prevents reuse of old result shapes.

Exact isolated release verification passed: `test/test_rival_profit_wealth.R` **26 groups**, prior `test/test_rival_net_gain.R` 12, running balances 14 (including the final assertion after that script's printed total), cash reconciliation 16, finance cache/reuse 3, transfer metadata/display checks and deployment contract 3. The new suite independently verifies +€5M from +€10M/−€5M completed cycles; a separate +€10M realised / +€25M overall fixture includes a retained €25M player and a €10M reward. It covers revisions/replay, ordinary Market counterparties, saved source-event recovery/conflicts, missing source/amount/baseline inputs, equal-time ambiguity, explicit/general fees, context isolation, cached valuation changes, shared pivot projection and actual selected-rival Shiny render. A deterministic fixture compares the vectorized common calculation with the general resolver; implementer/reviewer checks also compared 30 and 25 seeded randomized fixtures respectively. A benign single-observation Plotly warning in the synthetic module fixture does not affect the assertions.

Both the working copy and exact release passed `Rscript test/test_shiny_simulation.R`: 13 lifecycle checks plus eight tabs, with zero HTTP/live database mutation attempts. Synthetic Chromium checks passed at **1440, 390 and 360 pixels**: both values/formulas, expandable calculation notes, switching to the shared player-profit pivot, related fees and no page/card overflow or visible Shiny/JavaScript errors. Desktop/mobile screenshots were inspected. Browser boundaries used synthetic account/history data and blocked non-local requests; they did not verify an authenticated hosted account.

Performance review: no additional HTTP/database read, historical acquisition, timer, worker, persistence payload, dependency or migration is added. Matching scans/group-orders retained source rows at shared publication; the common one-cycle-per-player case is vectorized, with the general resolver retained for repeats, fees and uncertainty. The exact-release 2,001-row/1,000-cycle fixture took **0.072 seconds** and occupied 637.5 KiB. At the 5,001-row/2,500-cycle fixture, local median was **0.171 seconds**, maximum **0.317 seconds**, with 1,255.5 KiB source ledger. Cold matching remains synchronous CPU work within existing bounded/coalesced publication and is not a hosted responsiveness guarantee. Instrumented repeated builds and valuation-only changes invoked the matcher only once; the second valuation changes wealth without re-pairing trades. Existing 1,000-row metadata/display work took 0.040 seconds. Sorting/paging/opening details reuses numeric results and paired rows, and responsive columns stack without new images/requests. Documentation-only edits have no runtime cost.

All 38 exact-release bundle checksums match and all 78 package records are unchanged. Credentials, captures, tests, internal docs and development files remain excluded. Runtime, module and regression copies match the tested workspace. The scoped implementation was committed as `9fb89aa` and pushed non-force to remote branch `2.0` from `34931dc`, preserving unrelated working-tree/staged changes. GitHub returned a temporary internal error on the first push; retry succeeded. This subsequent delivery-note update has no runtime or performance effect and needs no repeated runtime tests. The user's actual corrected values, incomplete source evidence, hosted latency and production deployment remain unverified.


### Rival financial row, table metrics and readable calculations — 2026-11-06

The requested six financial cards now occupy one desktop row and wrap on smaller screens. The top standings table displays numeric **Overall wealth gained** and **Trading profit**, using the same cash-activity, known-input and completed-trade gates as the selected cards. Zero, negative and unavailable values remain distinct and sortable. The directory fallback retains both columns without inventing values, and sorting retains the selected team.

Expanded details show the actual arithmetic: cash + squad value − actual starting cash for wealth, and cached completed sales − their matched purchase costs − related fees for trading profit. Grouped messages retain historical ownership, reward eligibility/rules, payment, history coverage and source limitations without dumping raw round/player IDs. Original diagnostic notes remain in the shared dataset. Unknown wealth retains both its explicit missing inputs and source coverage notes. The screenshot's €61,495,971 + €393,516,996 − €300,000,000 independently equals €155,012,967; this is a known-answer display fixture, not authenticated retrieval.

Investigation found and reproduced a concrete source race: after a corrected historical lineup completed, an older response could finish later and overwrite/persist over it by observation time. Completion now compares against the current scoped round/award signature before merging or saving; superseded responses cannot erase corrected recipients/eligibility. Current demand stays on the existing idempotent bounded queue. The focused callback fixture verifies corrected Best XI/MVP amounts, one current persisted snapshot, reuse with no added request, <=2 concurrent requests, settled pending work and the ranking-derived round fallback.

Verification: `test/test_rival_summary_layout.R` 11 groups; `test/test_rival_lineup_revision_race.R` 3; existing `test/test_rival_profit_wealth.R` 26, `test/test_rival_team_directory.R` 7, historical reconstruction 14 plus its confirmed tie fixture, and saved bonus recovery 6. The required `test/test_shiny_simulation.R` passed 13 offline lifecycle checks and eight rendered tabs with zero live HTTP/database mutation attempts. The exact isolated current-remote release was verified separately before delivery. Synthetic Chromium checks with the normal sidebar passed at 1600, 1440, 1200, 390 and 360 pixels: six desktop columns, mobile stacking, readable numeric details, missing eligibility disclosure/no raw IDs, real numeric sorting of losses/zero/profit/unavailable, selection retention, billion-euro value fit and no page/card overflow or visible JavaScript/Shiny error. Desktop/mobile screenshots were inspected. No real account or transaction was used.

Performance review: no additional API/database request, persistence payload, source acquisition, reactive subscription, timer, worker, cache, dependency or migration. Metric projection is vector arithmetic over the league directory; 1,000 four-team projections took 0.157–0.160 seconds locally and the result occupied 928 bytes. Details use cached matched pairs and group notes once per render; opening details and sorting add no R source reads or trade matching. Existing 2,001-row/1,000-cycle realised-profit regression took 0.072 seconds in the initial workspace check. The new response guard decodes only the requested raw round in its common path: a ten-team/twelve-round/132-award-row synthetic sample measured median 1.99 ms per completion (five 100-evaluation samples), with 141,536 bytes of retained source evidence. The existing ranking fallback performs broader reconstruction when direct round evidence is absent. Requests, queue bounds and memory caches are unchanged; stale evidence writes are avoided. Documentation has no runtime effect. Only affected application file checksums are refreshed; package records and private/test/documentation bundle exclusions remain unchanged.

Limits: the race is reproduced and fixed locally; the screenshot does not prove it caused all missing evidence in the user's account. Available historical lineup contracts do not prove historical non-ownership for absent roster-only recipients or the exact missing ownership cutoff. Those inputs remain unresolved and disclosed. Synthetic checks do not establish authenticated live balances, hosted latency, production persistence or application deployment.

### Live classification round acquisition — 2026-11-06

The captured per-team round-history calls can return successful empty lists, while the old page derived Latest from all catalog/history numbers and lacked a native bulk round-ranking read. The saved official client's JavaScript confirms `/1/ranking/round` query `{championshipId, roundNumber, userteamId}` and `answer.metadata.status` / `answer.ranking[].id,name,points`; the General/teams sources contain cumulative points and must not supply an individual round score. No saved HAR contains an actual round-ranking exchange or establishes whether the user's current lineup marker is 12 or 13.

The classification-only catalog merges explicit numbered calendar status by official ID, and the page checks at most two round candidates. A next 13 marker with a not-started ranking falls back to playing 12; a known-playing 12 source failure remains unavailable at 12. Live rows replace the same history key exactly once. General uses authoritative cumulative directory points and tied ranks; arbitrary windows require every expected numbered round and never sum only the available subset. Latest survives delayed data publication, while manual filter changes persist. Missing scores stay unknown; genuine zero and negative scores remain numeric. Provisional selected rounds expose no settled bonus totals.

A new display-only evidence namespace records successful compact score observations using the existing account/championship/season-scoped asynchronous queue. Fresh cached or persisted snapshots reuse the original observation deadline and do not issue a second HTTP request or persistence event. Existing financial round/eligibility/award readers ignore the new kind, and the established closed-round ranking parser is unchanged. No external database was written during tests. Supabase guidance was read for this existing recorder integration; no schema, grants, auth, client credential or deployment configuration changed.

Verification in the isolated checkout: `Rscript test/test_classification_live.R` passed the known 26/18/15/9/8/6/2/0/0 score fixture, missing/negative/zero values, next 13 rejection, one cold ranking HTTP and zero warm/reopen HTTP reads, source-time persistence reuse, calendar-status hydration, display/finance isolation, delayed Latest convergence, General cumulative 100 and rank without source position, whole-round missing coverage, manual selection retention, same-round replacement and known-live failure. Existing `test/test_classification_presets.R`, `test/test_classification_loading.R` and `test/test_classification_bonuses.R` passed. The required `test/test_shiny_simulation.R` passed 13 lifecycle checks plus eight tabs, reporting zero HTTP/live database mutation attempts. The final failure-path change was checked with the focused live regression; passing unrelated checks were not repeated. Synthetic Plotly no-coordinate/palette warnings remain confined to the unused rank-evolution output in the test harness.

Primary-agent browser verification passed at 390px and 1440px with the real module and HTTP-blocked fixture: lineup marker 13 selected playing round 12, Latest displayed 26 points, General displayed the fixture's official cumulative 126, prior single-round selection cleared, a live update to 27 retained Latest, and switching presets added no ranking reads. Independent Luna checks confirmed the nine screenshot-shaped scores sum to 84, General uses official totals with missing history, incomplete custom windows remain unknown, saved snapshots retain scope, and display evidence cannot enter financial reconstruction. Astra reviewed the integrated production diff. Deployment checksums were refreshed only for the three changed runtime files.

Performance review: one cached bulk-calendar metadata read is reused alongside existing catalog acquisition, and at most two bulk ranking candidates replace any need for per-team/per-round live fanout. Existing per-team history reads remain scoped and cached. Score snapshots expire after 60 seconds; caches retain their established entry/byte limits, and compact persistence uses the existing bounded queue. Filter changes, presets and General totals use in-memory projections and add zero source requests. The existing bonus projection benchmark passed 100 selections for ten teams × 38 rounds in 0.309 seconds locally. There is no new timer, worker, schema, dependency or responsive layout element. Authenticated source behavior, production persistence and hosted latency were not exercised.


### Persistent rival calculation reuse — 2026-11-06

The table now restores the latest account/league/season/reference-team/version-scoped database snapshot before full evidence acquisition, with a saved/checking label. Typed, bounded JSON preserves reusable ledgers across process restarts; derived snapshots are excluded from financial evidence and signatures. Ordinary new, revised or removed transactions rebuild affected teams, while rules/round/award dependencies retain conservative invalidation. Live directory fields and cash freshness update independently. Storage success requires acknowledgement; failed saves, obsolete callbacks and inferred-season changes remain guarded. No schema, dependency or authentication storage change was required.

Passed: the six-section persistent-cache regression, including a fresh R process with zero history preparation/trade matching on unchanged inputs; existing finance reuse and 18 finance-runtime checks; offline lifecycle (13 checks plus eight tabs, no HTTP/live database mutation attempts); deployment contract; and whitespace/diff checks. Luna independently verified cache/uncached parity with newly unknown values, changed squad size and undated cash, malformed/scope rejection, self-evidence exclusion and save acknowledgements. The write-ack season guard was reviewed, with no separate simulated season change during a pending write. Astra reviewed the integrated changes. Primary-agent HTTP-blocked browser checks at 390px/1440px showed the saved table before every authoritative history request completed, retained exact known count/profit/wealth values and numeric sorting, and displayed the production saved/checking reason.

Performance: 12 teams/600 transfers measured cold reconstruction 1.697 s, snapshot encoding 0.279 s, hydration 0.231 s, unchanged verification 0.057 s (12 hits/zero builds), and one new transaction 0.058 s (one build/11 hits). Snapshot size was 24,440 encoded bytes/322,230 JSON bytes; hydration used one database-shaped read. First local browser render was about 1.48 s and the next viewport about 0.31 s. These are synthetic local measurements, not hosted latency. There is one small latest-snapshot read per context; unchanged renders/metadata updates add no persistence, source requests or trade matching. Snapshot encoding/decompression are bounded; oversized caches fall back to saved table rows. Authenticated retrieval, hosted durability and live database writes were deliberately not exercised. Documentation and tracking changes add no runtime work.


### Round Awards position pills — 2026-11-06

The Best XI role cells now reuse badge-gk/df/md/fw background colors for GK/DEF/MID/FWD; unknown roles remain plain text. Existing Round Awards checks passed all five groups, including escaped names, unknown roles and preserved scores. Browser checks at 390px and 1440px verified all four computed background colors, dark readable text and no page overflow. The 38-round/418-player fixture rendered in 0.446 s, with no added API/database requests or source lookups. The change adds one span per known position; existing cached data and badge CSS are reused. No delegation or broad lifecycle suite was needed for this localized presentation change. Documentation has no runtime cost; live account data and hosted deployment were not exercised.

### Classification cold-read recovery — 2026-11-06

A real `factual_read_adapter`/async replay fixture reproduced the cold-page defect: `historical_rows_RV()` read the pending directory before the filter/status projection, and its Shiny validation condition cancelled the dependent output and round-control observer. The catalog and native ranking could already be loading independently, but their scores and initialized controls were gated by the directory. Previous loading fixtures used a constant directory; live/preset fixtures replaced complete read handlers and missed this dependency.

The module now captures pending/error states without losing reactive subscriptions, projects catalog/live rankings independently, and uses source ranking names until the directory arrives. Latest initializes to playing 12 despite the next-round 13 lineup marker. General waits for the official cumulative directory `points` column; a legacy identity-only directory cannot label live 26 as a cumulative result. Pending/failing history and finance leave live points visible, while missing historical coverage and bonus amounts stay unknown. Settings and dream-team pending/error states return visible paragraphs. Failed catalog acquisition remains unavailable even if directory/settings requests are still pending. Explicit numbered playing metadata recovers a current ranking identity from legacy compact catalogs without lineup markers.

Passed in the isolated checkout: `Rscript test/test_classification_cold.R` (five scenarios), `Rscript test/test_classification_loading.R`, `Rscript test/test_classification_live.R`, `Rscript test/test_classification_presets.R`, and `Rscript test/test_classification_bonuses.R` (including its 13 running-balance groups and additional final assertions). The cold fixture runs actual bound reads, replay, coalescing, catalog parsing and persisted JSON restoration; it verifies live 26 before directory/history/settings/finance, General loading then official cumulative 100, independent source failures, legacy catalog/directory shapes, and one ranking HTTP/read plus one display-evidence write across warm and cleared-process-cache persisted reuse. Existing synthetic Plotly no-coordinate/palette warnings are confined to the unused rank-evolution test output. No live API account or database write was used. Shared infrastructure and startup code are unchanged; the broad offline lifecycle suite was deliberately not repeated for this module-local change. Primary-agent browser checks passed at 390px and 1440px using the real bound adapter and native catalog/ranking parsers: round 12/live 26 displayed before directory/history/settings/finance, General visibly waited then displayed official 100, settings recovered, and failed histories/finance left live points visible without JavaScript or visible Shiny errors. The static release marker now records current runtime hashes, including Classification, to distinguish this delivery during public rollout verification. Authenticated hosted account behavior is not claimed by the fixture.

Performance review: native bulk ranking is requested before per-team historical reads where they share a projection. The cold fixture's heartbeat executes while directory/settings/finance remain pending; filter actions add no ranking read. No new endpoint, source fanout, retry, invalidation, timer, worker, cache store, dependency or persistence payload is introduced. Small typed state/projection frames replace validation cancellation, and the existing bounded context caches/acknowledged database persistence remain in use. The existing bonus projection benchmark measured 100 selections for ten teams × 38 rounds in **0.586 s**, retaining **195.7 KiB** of fixture metadata; the underlying 1,000-transfer ledger fixture took **0.118 s**. These are local synthetic timings, not hosted latency guarantees. Documentation has no runtime effect. Authenticated current-account completeness, hosted responsiveness and production persistence remain unverified.


Hosted delivery verification: implementation commit `a9e1599` was pushed non-force to `2.0`. A fresh unauthenticated Chromium visit to `https://smdb21-futmondo-v2.share.connect.posit.cloud/` returned HTTP 200, established a Shiny session and served `classification-loading-recovery-2026-11-06` with all 17 runtime hashes matching the tested release, including the Classification module. This confirms automatic rollout and public startup, not signed-in league retrieval. No account credentials, real transactions or live database writes were used. Delivery-note changes have no runtime or request cost.

### Saved page per account — 2026-11-06

No delegation. Primary-agent review covered the new account-scoped controller/store, dynamic-sidebar startup selection, explicit read/write failures, deployment bundle and restricted schema. Focused `test/test_page_preferences.R` passed account/fresh-session isolation, Login exclusion, invalid/Admin page fallback, duplicate/restore no-write behavior, delayed reads after manual navigation/logout, read/write retries, unacknowledged writes, serialized/coalesced rapid navigation and real REST filter/body construction at a synthetic HTTP boundary. The required `test/test_shiny_simulation.R` passed 13 lifecycle checks plus eight tabs with zero HTTP/live database mutation attempts. `test/test_deployment_contract.R` passed all three groups. Focused, deployment and mandatory lifecycle checks also passed on the isolated task-only release, with changed runtime manifest checksums verified. Initial verification caught the R/Postgres regex repetition limit; both were corrected before the passing checks.

The production UI/server with HTTP-blocked fixtures passed 1440px and 390px browser checks: saved Classification initially opened, a later Plots selection survived Login/logout/login and a full page reload/new Shiny session, with no settled visible errors or horizontal overflow. The final browser pass repeated saved Plots restoration at both widths; the initial Classification assertion also passed before a transient league-loading assertion required an explicit settle wait. No real account or transaction was used.

Hosted SQL checks under service_role used synthetic rows inside a rolled-back transaction, with assertions for newer-visit preservation and subsequent latest-page upsert/readback. Direct privilege checks confirmed RLS and denied browser reads/writes. The installed migration's account-ID constraint was corrected during validation; the checked-in migration/schema contain the final definition. Security advisors report the expected informational no-policy notice for this intentionally server-only table; unrelated pre-existing warnings concern futmondo_players GraphQL discovery and rls_auto_enable(). No public access was granted. Authenticated hosted navigation and the app deployment were not exercised. Direct REST verification from local R could not run because the local database URL/key are absent; hosted SQL verification used the connected Supabase project.

Performance review: one bounded asynchronous database read per account login; restoration and duplicate selections add zero writes. At most one asynchronous write is in flight per session; the focused rapid-navigation case sent the first and latest pages only. Responses are limited to one tiny row, the table has a primary-key lookup and one small row per account, and the controller holds at most one pending page. No timer, repeated source read, new dependency, full-page redraw per navigation or blocking HTTP is added. Sidebar rendering changes only during account restoration; the existing page input continues to drive modules. A failed save cannot survive process termination; visible Retry and later navigation remain available while the session is active. Local fixtures establish responsiveness and request bounds, not hosted latency. Documentation adds no runtime cost. Broad unrelated tests were deliberately skipped.

## My Team asking prices and received offers — 2026-11-07

The former In market badge and Market Price both represented asking prices. My Team now shows one numeric **Selling by** price and the separate highest received **Offer**. Verified empty listings clear the asking price. The new top **Offers for your players** box sits beside Incoming players on desktop and stacks on mobile. It includes every received offer, highlights rivals with a person icon and explicit Rival/team label, and uses a robot/Futmondo label for the system. Lower rival bids survive higher system bids; selecting one carries that exact bid into the existing player card and confirmation flow.

`received_offers` retains normalized full bid rows independently of the legacy highest-per-player fields. Complete snapshots are stored through the existing bounded acknowledged persistence queue in the server-only `rival_finance_evidence` store. Eligibility records under the reserved `rival-cache:received-offers-v1:` prefix are excluded from rival history/accounting queries. Endpoint and combined snapshots use separate keys. Payloads allow only normalized bid facts plus account/league/team/season/scoring scope, source time, completeness and a 15-second source deadline. Successful empty results are stored; stale/partial/error observations are not promoted to complete. Failed writes remain retryable. Saved observations are read before repeat bid acquisition; manual actions and Refresh reject older saved snapshots. No schema/dependency change or deployment was performed.

Verification passed: `test/test_team_received_offers.R` (8 checks), existing `test/test_player_data_availability.R` (11), `test/test_team_offer_persistence.R` (8, including JSONB object-key reordering/PostgREST nested-frame round trips, original freshness, exact context isolation, empty/unknown/negative values, failed enqueue retry, acquisition-hook recording outside async replay, API-free fresh durable reuse and invalidation), and mandatory `test/test_shiny_simulation.R` (13 checks plus eight retained tabs). All use synthetic data; no live account or transaction was used. Browser fixtures using the actual UI/panel/table renderers passed at 1440px and 390px: equal side-by-side versus stacked panels, three complete offers including two rivals, distinct icons, canonical headings, numeric asking-price sorting, no JavaScript errors and no viewport overflow. Screenshots were inspected locally.

Performance review: the summary and table share the same existing roster resource; no extra Futmondo request, polling timer, full-catalog traversal, or independent reactive acquisition is introduced. A synthetic cold acquisition used the existing three roster/bids/listings reads; warm reuse used zero. Persistence adds at most one two-key, limit-two database lookup on a bid-cache miss and queued bounded normalized snapshots (maximum 5,000 rows/256 KiB), reusing existing cache and queue limits. Fresh durable reuse skipped offer HTTP acquisition in the fixture. Source timestamps are retained, including the oldest roster/endpoint timestamp for combined observations; restoring does not restart bid freshness. Layout cost is proportional to the manager's received offers, and old row handlers are destroyed before replacement. Hosted latency, actual production database write acknowledgment, authenticated offers and deployment were not exercised. Existing broad unrelated workspace changes were preserved; this task's commit is isolated against the saved starting snapshot.

## Administrator-controlled app access — 2026-11-07

Primary implementation and final review with a Luna validator for the access boundary. Admin now has a keyboard-accessible default-enabled Block non-admin access switch, backed by a server-only singleton in Supabase. Both password and remembered admission await an explicit stored open policy before publishing a non-admin token. Admins bypass the policy read, failed reads block admission, and active non-admin sessions recheck every 15 seconds. An uncertain save disables further editing until a reload verifies the persisted state.

Passed on the isolated task-only release: `test/test_app_access.R` (9 groups: strict fail-closed policy, normal/remembered admission, admin recovery, late logout completion, pending-read heartbeat, session-clock revocation, save failure/reload and forged writes, REST validation/acknowledgement), `test/test_remember_login.R` (21 checks including asynchronous denial at the real remembered endpoint), `test/test_deployment_contract.R` (3 groups), and mandatory `test/test_shiny_simulation.R` (13 checks plus eight tabs, zero live HTTP/database mutation attempts). Initial verification caught a recurring global timer preventing Shiny's test output queue from draining; the implementation now uses a session-bound Shiny timer and the lifecycle passed. Browser checks at 1440px and 390px exercised click/keyboard switching and saved state after page reload/new session using a synthetic asynchronous store; screenshots were inspected, with no overflow or JavaScript errors. Broad unrelated tests were deliberately skipped. Hosted signed-in app behavior and deployment were not exercised.

The hosted migration was applied. SQL verified the enabled seed, RLS, denied anon/authenticated reads and writes, server SELECT/INSERT/UPDATE and denied server DELETE. A service-role transaction changed/read the policy to FALSE and rolled back; a subsequent query confirmed TRUE. Security advisors show the expected informational no-policy notice for this server-only table; unrelated existing warnings concern futmondo_players GraphQL exposure and rls_auto_enable(). No browser policy or credential exposure was added. Source schema and deployment manifest include the new runtime and default state; factual table resets exclude the policy.

Performance review: each non-admin admission adds one asynchronous, primary-key, limit-one boolean database read before application loading. Active non-admins add at most four such reads/minute, with no overlapping policy reads; admins and signed-out sessions do not poll. Admin activation/reload reads once and only an actual change writes once. Eight-second transport timeout, bounded session state and session-managed timer cleanup preserve responsiveness. The delayed-read test confirmed another event-loop callback runs while admission remains pending. No extra Futmondo endpoint, dependency, data scan, full-page redraw or authentication secret is introduced. Revocation happens at the next completed poll, so transport/queue latency adds to the 15-second interval; hosted latency was not measured. Unrelated shared-workspace changes are preserved and excluded from the task commit.

## App access rechecks every two minutes — 2026-11-07

No delegation. Changed the active-session policy timer from 15 seconds to 120 seconds and updated Admin text and documentation. The focused access suite passed all 9 groups, including no periodic read or sign-out at 119,999 ms and revocation after crossing 120,000 ms. Deployment contract passed 3 groups; mandatory offline lifecycle passed 13 checks plus eight tabs with zero live HTTP/database mutations. Broader tests and browser rechecks were deliberately skipped for this interval/text-only change; hosted deployment was not run.

Performance review: periodic policy reads fall from four/minute to one every two minutes per active non-admin session, an 87.5% reduction. Admission still checks the database immediately; existing-session revocation can now wait up to two minutes plus request latency. Database state, access decisions, async execution and bounded session memory remain unchanged.


## Classification failure identity — 2026-11-07

Luna investigated the ranking request/parser against the saved official-client contract and found no evidenced mismatch. Primary-agent investigation reproduced a separate diagnostic defect: factual_read_adapter() converted a structured unavailable read to display text inside a Shiny validation error. Classification caught and reformatted that text, changing a network/HTTP/parser/limit error into a schema error and assigning a new unlogged reference. The fix retains only the original sanitized failure fields while preserving Shiny validation behavior. Explicit allowlisted Classification reader labels identify the catalog, ranking, history or lineup source without exposing private suffixes.

The new nested diagnostic regression failed before the fix on category preservation. The task-only isolated checkout passed test/test_read_diagnostics.R (7 groups, including all six error categories through the actual Classification read-state formatter), test/test_classification_cold.R (5 scenarios, retained scores, responsiveness and persisted reuse), mandatory test/test_shiny_simulation.R (13 checks plus 8 tabs, zero live HTTP/database mutations), and test/test_deployment_contract.R (3 groups). Changed runtime manifest/release hashes and whitespace checks passed. Existing synthetic Plotly no-coordinate warnings remain. Earlier workspace checks also passed classification loading and all 18 shared finance runtime groups. Browser checks and broader unrelated suites were deliberately skipped for this condition-only change; authenticated hosted retrieval/deployment was not tested. The original screenshot's acquisition failure is not reproduced or fixed; there is no authenticated current-account response or production log available. No source query/parser relaxation, fabricated values, real-account request or live database mutation was used.

Performance review: constant-size error-condition wrapping only on unavailable reads and five fixed diagnostic labels. No additional API/database request, retry, polling, reactive subscription, cache invalidation, persistence payload, dependency or successful-path data transformation. Existing database persistence/reuse is unchanged. No expensive path changed, so a new benchmark is unnecessary; the lifecycle and classification cold-load checks cover loading and responsiveness. Documentation has no runtime effect.


## Previous-round bonus coverage diagnosis and correction — 2026-11-07

Read-only hosted evidence and an offline replay reproduced round 11's missing bonus cells. The final eleven-player selection and historical MVP recipient were persisted; four non-MVP award players had no verified historical owner in saved starting-XI/substitute records. A single combined missing-evidence flag incorrectly withheld the separately known MVP. The correction scopes coverage to each round and reward component, keeps unknown Best XI totals explicit, and advances the durable calculation version. Independent validation also caught a pre-existing combined-payment weakness: exact agreement with a partial recipient subtotal could validate attribution. Allocation now additionally requires complete recipient evidence for that payment round; actual combined cash is retained.

Executed: `test/test_classification_bonuses.R` (13 inherited ledger checks, selection/known-zero checks, independent component ownership/conflict/round-isolation regressions, serialized ledger reuse, and combined-payment cash preservation); `test/test_rival_reference_bonus_gate.R` (7 settlement/isolation groups); `test/test_idless_bonus_payments.R` (captured parser/persistence/replay, cash 52,129,125 EUR); `test/test_rival_persistent_cache.R` (six groups including fresh-process reuse and acknowledged-write failures); mandatory `test/test_shiny_simulation.R` (13 checks plus eight tabs, zero HTTP/live database mutation attempts); and `test/test_deployment_contract.R` (three groups). Primary-agent diff review and independent Luna known-answer validation passed. Existing synthetic lifecycle diagnostics/Plotly warnings are unrelated to this change.

Performance review: no additional API/database requests, polling, reactive subscriptions, network concurrency or source persistence payloads. Component checks are bounded local operations over at most eleven award players per round; warm reads reuse existing durable ledgers. The version change rebuilds derived calculations once while retaining source evidence. Three cold saved-evidence builds each: old median 2.837s, changed median 3.110s in this container (ten directory teams, eleven historical rounds); all cash balances stayed identical. One hundred table projections over ten teams and 38 rounds took 0.315s with 195.7 KiB of ledger metadata. These local measurements exclude hosted network/rendering latency. Documentation has no runtime cost.

Limitations: full Best XI ownership remains unresolved; neither lineup absence nor current squads can establish the missing historical owners. No authenticated Futmondo acquisition, production data writes or hosted application deployment was performed. Browser checks and unrelated suites were deliberately skipped for the calculation-coverage-only change. Unrelated working-tree edits are excluded from this task's commit.


## Round 11 confirmed-unowned evidence — 2026-11-07

The user clarified that De Frutos, Bartra, Cabrera and Fortuño were owned by no league user in round 11. Persisted four explicit, scoped `user_confirmed_unowned` eligibility records using the existing evidence contract; all four were acknowledged and independently read back from the hosted latest-evidence view. No official source response was rewritten or inferred from current squads. Existing records and recorded cash were retained. Fresh persisted-evidence replay through the actual decoder/calculation path produced known Best XI totals for all nine participants: seven owned award recipients at 500,000 EUR each = 3,500,000 EUR, plus the separately verified 1,000,000 EUR MVP. All cash balances were unchanged. The earlier round-11 missing-owner limitation is therefore resolved by explicit user confirmation; generic absent historical ownership is still unknown.

Executed `Rscript test/test_classification_bonuses.R`: inherited ledger/selection checks and new known-answer fixture with seven owned and four unowned award players passed; serialized evidence reuse, account/league/round isolation, missing confirmation and conflicting ownership checks passed. Independent Luna known-answer checks of the award estimator passed; its separate full standings fixture was inconclusive, so complete-path verification comes from the primary regression and persisted-data replay. No production calculation/interface changed, so the lifecycle, browser, deployment and unrelated suites were deliberately not rerun. Hosted UI display was not inspected.

Performance review: four compact durable evidence records; no application code, network acquisition, timer, concurrency, dependency or rendering change. Existing evidence hashes invalidate derived calculations when the new records are loaded, and existing snapshot persistence reuses the result. The focused table projection benchmark took 0.309s for 100 selections over ten teams and 38 rounds. Documentation/tests have no deployed runtime cost.


## Full-width lineup availability checkbox — 2026-11-07

Verified the offline lineup browser fixture at 320, 390 and 1440px: the checkbox wrapper spans the controls width, starts below both other inputs, fits the viewport and remains interactive. Scoped `git diff --check` passed. No new tests were added for this localized layout change; broader lifecycle/data suites were deliberately skipped. Hosted deployment was not run. No delegation.

Performance review: one static wrapper and a scoped flex rule, with the checkbox input explicitly using 100% width. No API/database requests, cache changes, timers, reactive work or meaningful memory/rendering cost are added.


## Best XI formation and dual-position priority — 2026-11-07

Sol implemented the bounded optimizer change; Astra reviewed the actual diff and browser behavior; Luna independently checked the new objective. `Rscript test/test_lineup_runtime.R` passed 13 groups, including cross-formation MID preference, assignment priority, strictly higher-score forward fallback, disabled multiposition, entitlement and deterministic ordering. `Rscript test/test_lineup_module.R` passed 14 groups, including changing 4-4-2 to optimal 3-3-4 and keeping the rendered selection/draft consistent across reactive flush and browser-input acknowledgement. Primary offline Playwright checks at 390 and 1440px confirmed selector/pitch agreement after Best XI and recent-round recalculation. Scoped `git diff --check` passed.

Independent `Rscript /tmp/futmondo_lineup_priority_oracle.R` passed 24 exhaustive synthetic cases: equal-total selection of 3-6-1 with five duals in MID instead of current 4-4-2 with at most four; fixed-formation assignment; primary-role-only behavior without multiposition; a 124-point optimum requiring the dual at FWD; and 20 seeded small rosters matching brute-force score/MID-count optima with eleven distinct eligible players. No discrepancies. Scratch validation uses small integer-score fixtures, not live Futmondo data or independent validation of league scoring policy. No defensive-score bonus is fabricated or historical score recalculated. Broader lifecycle/data suites were deliberately skipped for this localized pure optimizer change; authenticated acquisition and hosted deployment were not run.

Performance review: the existing 100-player/all-formations optimizer-plus-bench benchmark averaged 40.8 ms over 20 runs, below its existing 100 ms limit. The bounded state lattice adds one integer priority vector and comparisons, with unchanged asymptotic complexity. No API/database requests, persistence/cache changes, timers, reactive dependencies or redraw paths were added. Existing complete historical observations and snapshot reuse remain unchanged.


## 2026-11-08 My Team clause visibility and sale profit

Sol 6.1 implemented; the primary agent reviewed the scoped diff, known-answer calculations, configuration precedence, retained action IDs and request impact. Suggested Clause is hidden when renewal is explicitly prohibited; Owner team ID is hidden in My Team. Listing and offer amounts display prospective profit/loss against the observed roster acquisition cost, in millions with two decimals. No valuation or invented-zero fallback.

Passed: `test/test_squad_table_prices.R` (7 focused groups), `test/test_team_received_offers.R` (8 checks), mandatory `test/test_shiny_simulation.R` (13 checks plus 8 tabs), and scoped whitespace checks. Lifecycle validation made no HTTP/live database mutations; existing empty-trace Plotly warnings remain. The offer regression measured 3 cold source reads, 0 additional panel reads and 0 warm reads. Performance review found bounded squad/offer-row formatting, one hidden numeric cost per squad row and no additional API/database requests, persistence, timers or dependencies. Hosted/browser/mobile validation was not run; broader unrelated suites were deliberately skipped. See [display contracts and checks](squad_table_prices.md).


## Lineup picker after optimization and current-draft labels — 2026-11-09

Sol 6.1 implemented; the primary agent reviewed the event validation, draft membership and actual mobile picker. The pre-fix browser reproduction clicked a newly inserted optimized tile before its Shiny binding attached and failed to open the popup. The same immediate-click reproduction opens it with direct click events. Slot events reject obsolete generations/formations and invalid indexes, while picker choice/context checks and save preflight/readback remain intact. Rows identify current draft starters, numbered substitutes, unselected players and the clicked slot. Refresh help explains reload-with-draft-preservation rather than saving or undoing edits.

Performance review: one shared slot observer replaces fifteen action-button observers; native tiles no longer require dynamic input binding. Membership uses bounded matches against eleven starter/four bench IDs over the existing squad-only picker. No new API/database requests, repeated acquisitions, timers, dependencies, caches or persistence payloads are introduced. Existing official snapshots and shared score observations retain durable reuse. No expensive path or scientific/statistical calculation changes; separate timing benchmarks and independent scientific validation are unwarranted.

Verification: `Rscript test/test_lineup_module.R` passed all 18 groups. The offline `test/test_lineup_browser.R` fixture plus `node test/test_lineup_browser.cjs` passed at 390 and 1440 px: clicking immediately on optimized DOM insertion opens the popup; current draft labels match starters/bench across edits, removal/refill, repeated optimization, formation redraw, a verified synthetic save and undo. Local edits preserve all API/submission/database counters. Warm picker opening measured 43 ms and 109 ms respectively in this run. Scoped whitespace review passed. Broader lifecycle/scientific suites were deliberately skipped for this localized UI event/presentation change. No authenticated Futmondo action or hosted deployment/testing was performed.


## Compact lineup picker marks — 2026-11-09

No delegation. Replaced the per-row Starting XI/Bench/Current slot labels with compact inline icons; full state remains available through accessible labels and tooltips. The existing browser fixture passed at 390 and 1440px, including correct marks for current draft membership, no membership text in row contents, immediate post-optimization opening, save/undo and viewport fit. Picker opening measured 110/95 ms in this run; the primary agent inspected the mobile screenshot. Scoped whitespace checks passed. Broader lifecycle/data suites and hosted testing were deliberately skipped for this presentation-only change.

Performance review: no API/database, cache/persistence, observer, timer or calculation changes. Icon nodes replace status text in the same bounded squad list, use existing Font Awesome resources and reduce row height. Existing browser assertions confirmed unchanged request/write counters during local edits. No added dependency or expensive path warrants a benchmark.

The existing module regression also passed all 18 groups, including icon accessibility and one current-slot mark.


## Lineup control placement — 2026-11-09

No delegation. The formation selector now directly precedes the pitch; Optimize and Last N rounds share a row. Existing offline browser regression passed at 390/1440px, including new geometry checks, formation edits, immediate picker opening, optimization, save/undo and unchanged local request/write counters. A targeted 320px browser check confirmed the same-row controls, selector placement and no page overflow; the primary agent inspected its screenshot. Scoped whitespace checks passed. Module/data/lifecycle suites and hosted testing were deliberately skipped for this layout-only change.

Performance review: existing controls and reactive output IDs are relocated; one redundant static heading is removed. No API/database reads or writes, cache/persistence changes, dependencies, timers, observers or new calculations. The selector stays in a stable UI container outside pitch redraws. Small scoped flex rules preserve mobile fit with no expensive path requiring measurement.


## Shorter lineup heading — 2026-11-09

No delegation. Removed the requested phrase from the heading; reviewed the one-line diff and scoped whitespace check. Runtime/browser tests deliberately skipped for this static copy edit. Performance review: no changed requests, persistence, reactive behavior or calculations; only less heading text. Hosted testing not run.


## Icon-only lineup refresh button — 2026-11-09

No delegation. Rendered the actual Lineup UI and verified that the refresh button has no visible text, retains its rotate icon and tooltip, and exposes the accessible name Refresh lineup. Scoped whitespace check passed. Browser/full runtime suites and hosted testing deliberately skipped for this small presentation change. Performance review: only the static button label/ARIA attributes change; no new API/database work, persistence, observers, redraws, timers or dependencies.


## Shared player-table hidden columns and Latest 5 heading — 2026-11-09

No delegation. Shared table display filtering now omits Suggested Clause, Fav and Owner team ID on every page. Full source rows retain these fields for favourites filtering, owner selection and player-card actions. Renewal settings cannot restore the hidden Suggested Clause column. The shared `average.fitness` definition displays Latest 5 while preserving its key and values. Existing user edits to `player_columns_to_hide.txt` were preserved and excluded.

Passed `test/test_player_name_crest.R` (5 groups, including the actual widget schema, Latest 5 heading, preserved fitness values and favourites/action metadata), `test/test_squad_table_prices.R` (7 groups, including visibility under different renewal settings), and mandatory `test/test_shiny_simulation.R` (13 checks plus 8 tabs; no HTTP/live database mutation attempts). The lifecycle emitted existing empty Plotly trace warnings. Scoped whitespace/diff review passed. Browser/hosted testing and unrelated suites were deliberately skipped for this display-only change.

Performance review: omitted columns reduce widget payload/rendering work; removing configuration-dependent visibility removes an unnecessary settings dependency from schema updates. No new requests, database reads/writes, persistence/cache changes, timers, dependencies or score calculations. Existing database-backed observations and filtered row metadata are reused. No expensive path requires a separate benchmark.


## Sum last 5 heading — 2026-11-09

No delegation. Reviewed the one-line shared heading rename from Last 5 Points to Sum last 5 and passed scoped whitespace checks. Runtime/browser suites and hosted testing deliberately skipped for this static copy edit. Performance review: no changed requests, database/persistence work, reactive behavior, values or calculations.


## Notifications icon beside the title — 2026-11-09

No delegation. Relocated the existing header Notifications output beside the Futmondo title and removed its visible text. A dot indicates a positive unread count; tooltip/accessibility text retains the available count, while unknown counts remain unknown. Existing sidebar navigation and the notification-tab action handler remain intact.

Passed the focused offline `test/test_header_notifications.cjs` browser check at 320, 390 and 1440px: icon-only markup, title/bell alignment, viewport fit, synthetic unread tooltip/dot, real tab navigation and collapsed desktop header interaction. The primary agent inspected the 320px screenshot. The existing `test/test_browser_app.R` supplied the HTTP-blocked application fixture. Mandatory `test/test_shiny_simulation.R` passed 13 checks plus 8 tabs with no HTTP/live database mutation attempts; existing empty Plotly trace warnings remain. Scoped whitespace and browser-script syntax checks passed. Unrelated suites and hosted testing/deployment were deliberately not run.

Performance review: the same single UI output and unread reactive source are reused; no new API/database requests, polling, persistence, observers, dependencies or calculations. The previous text/count node becomes an existing bell with one small unread-dot span; layout uses small scoped flex rules. No expensive path changed, so a separate benchmark is unwarranted.


## Remove minimum-value filtering app-wide — 2026-11-09

No delegation. Removed both Min Val controls, the clear-filter reset entry and the minimum-value filter pass. An active application-code scan found no remaining references to the control/label/filter. Existing maximum-value and other filter checks still pass.

Passed `test/test_player_table_filters.R` (9 groups), including rendered controls for both layouts, obsolete minimum-value input having no effect, and existing owner/clause/default/maximum filters. Its existing 524-row normalization/filter metric was 0.002s and 95.1 KiB. Mandatory `test/test_shiny_simulation.R` passed 13 checks plus 8 tabs with no HTTP/live database mutation attempts; existing empty Plotly trace warnings remain. Scoped diff/whitespace checks passed. Separate browser/hosted checks and unrelated suites were deliberately skipped for this shared control removal.

Performance review: one fewer Shiny input/binding per table, no obsolete reset message or minimum-value reactive dependency, and one fewer local valuation-filter pass. No new API/database calls, acquisition/persistence/cache work, dependencies, observers or timers. Existing row data and pagination are reused. No expensive path requires a new benchmark.


## 2026-11-09: Dokploy root-path deployment packaging

- Added `Dockerfile` (R 4.6.1), `.dockerignore`, and `shiny-server.conf`; the app is
  served directly at `/` on internal port 3838. The repository previously had
  development-container settings under `weapp2/docker` but no Dockerfile.
- PASS: `Rscript test/test_shiny_simulation.R`: 13 offline lifecycle checks plus
  all 8 tabs; no HTTP or live database mutation attempts. Plotly emitted warnings
  about inferred trace types/modes; no test failed.
- PASS: `Rscript test/test_deployment_contract.R`: 3 checks, 0 failures.
- PASS: focused static context/route review: all 46 manifest runtime files plus
  the Shiny configuration are included (1,215,535 bytes); README excluded as
  documentation. Sensitive/development fixture paths are excluded. R version,
  port, root application route, inherited startup, and dependency layer ordering
  agree. This static check does not replace a Docker image build.
- Performance review: package installation is cached before source copying and
  defaults to two build jobs (configurable with `NCPUS`). App data/API/database
  paths are unchanged; no polling, additional reads, timers, or redraws are added.
  Runtime observations still use the existing durable database, with no new
  in-memory-only cache. No app dependency or statistical calculation changed.
- Not run: Docker build/container smoke test (`docker info` reports no running
  daemon), target-server resource measurements, public DNS/HTTPS/WebSockets, and
  hosted remembered-login checks. Package versions resolve from the image's CRAN
  repository during build rather than restoring the manifest exactly.


## Screenshot-supported historical award ownership — 2026-11-09

Applied the scoped, idempotent two-record repair in
`scripts/repairs/20261109_confirm_award_ownership.sql` to the hosted evidence store
and read back both records from `rival_finance_evidence_latest`. Independent Luna
review checked screenshot intervals, player/round/team identities, Hala's saved
eligibility and the System/Market interpretation. A validator initially mistook
`futmondo` for a user; the explicit domain rule and absent transfer counterpart IDs
corrected that interpretation before any production write.

Executed offline preflight and fresh database-readback replay through the existing
ledger: Hala's cash rises 1,500,000 EUR (1,000,000 MVP plus 500,000 Best XI), all
other cash balances are unchanged, round-8 MVP is confirmed zero for all nine
historical participants, and incomplete Best XI totals remain unknown. The
reference team's round-4 MVP component remains unavailable because its separate
combined-payment allocation is unresolved. The database readback produced exactly
the preflight cash ledgers and team summaries. Existing
`test/test_classification_bonuses.R` passed its inherited 13 ledger checks,
component/round isolation, combined-payment safeguards, persisted reuse and
confirmed-unowned/missing/conflicting-evidence regressions.

Performance review: two small evidence records are reused by the existing durable
read/cache and signature invalidation paths. No new application requests, timers,
reactive subscriptions, redraws, dependencies, schema or runtime code. The focused
fixture measured 100 selections for 10 teams x 38 rounds at 0.330 seconds and the
1,000-transfer ledger at 0.196 seconds; these are local synthetic measurements.
The actual evidence replay took 6.171 seconds offline and is diagnostic work, not
a new interactive path. Broader lifecycle/browser suites were deliberately skipped
because runtime behavior is unchanged. Authenticated hosted UI rendering and
complete historical bonus recovery remain unverified; no financial transaction
was submitted.


## 2026-11-09: Dokploy generated-env path failure

No delegation. The deployment log establishes successful cloning followed by a
pre-build `.env` write to `code/weapp2/Futmondo/weapp2/docker/.env`, a non-existent
combination of app and old development-container paths. Corrected the guide and
README to explicitly set Provider Build Path `/weapp2/Futmondo`, Dockerfile Path
`Dockerfile`, Context `.`, and disable Create Environment File while retaining
runtime variables. Checked against Dokploy's official subdirectory example and
verified that the resulting local Dockerfile and context exist. Scoped staged
whitespace check passed. Documentation-only; no runtime, requests, cache, database,
reactivity, memory, or performance behavior changes. Tests were deliberately not
rerun. Target-server settings and redeployment are not accessible here and remain
unverified; image build and hosted operation are not declared fixed.


## 2026-11-09: 30-minute exact-session reconnection on mobile

Participants: Sol 6.1 implemented Dockerfile/.dockerignore plus the build-only
`patch-shiny-reconnect.cjs` helper and `test_shiny_reconnect.cjs`; Luna independently
validated a fresh official-package extraction; primary reviewed the integrated
source changes, ran app checks, and documented the result.

The installed server's robust connection constructor now receives 1,800 seconds
and its R adapter injects a browser `reconnectTimeout` of 1,800,000 milliseconds.
All source guards run before either patch target is written. Docker build runs
the actual installed registry/client tests and parses the patched R adapter.
App files are copied explicitly; build checks are removed from the merged runtime
filesystem (their earlier Docker COPY layers still contain their non-secret
source bytes). Existing `/init`, app root routing, auth and manual actions remain.

- PASS: implementation and independent focused runs each passed 5/5 groups against
  official Shiny Server 1.5.25.1051, with its bundled Node v24.22.1. Package SHA256:
  `bc298a0213817372b369375d41666b83fa7bd1d6f9f95859e0d0082cb8aca4ac`.
- PASS: virtual-clock checks resume the same logical session at 16 and 1,799
  seconds; CONTINUE replays pending output and ACK discards it; expiry at exactly
  1,800 seconds emits close/end once, cancels retention, removes registry ownership
  and rejects a late old session ID. Changed upstream shape fails before writes.
  Independent source tracing confirms old-ID resumption returns before creating a
  new application worker/channel, so it does not run a fresh app login flow.
- PASS: JavaScript syntax checks and patched R adapter parse; scoped whitespace
  check. No additional runtime dependency or app manifest regeneration needed.
- PASS: `Rscript test/test_shiny_simulation.R`: 13 offline checks plus 8 tabs, no
  HTTP/live database mutation attempts; existing Plotly trace warnings remain.
  `Rscript test/test_deployment_contract.R`: 3 checks, 0 failures.
- Performance: build checks take about 0.3 seconds on the extracted package and
  are cached before app COPY. Existing browser retry backoff, R session data and
  reactive work can continue longer. Upstream output buffers have no byte cap:
  the focused fixture accumulates 1,799 countdown messages; ACK frees entries,
  expiry removes registry ownership, and unreachable buffers rely on normal GC.
  Existing notification refreshes may also continue. No new app polling or source
  query is introduced, but total lifetime memory/requests can increase. Durable
  factual observations still use the existing database; this grace buffer is not
  a reusable API cache. Target-VM memory/concurrency costs are not measured.
- Not run: complete Docker image build (daemon unavailable), real elapsed
  30-minute test, live phone sleep/return, or deployed Dokploy checks. Tab discard,
  page reload, container/R-process restart or the deadline can still end live
  state. No production deployment or prevention of browser suspension is claimed.


## Missing full-range bonus columns — 2026-11-10 UTC

Root causes: the ledger did not acquire player-summary ownership histories; the
bulk match-calendar parser discarded the dates needed to bound those histories;
compact calculation snapshots discarded per-round bonus metadata when detailed
cache size exceeded its budget; and postponed round 8 was treated as a completed
round despite no final payment. The user confirmed its game is postponed to
9 December. Applied and read back the scoped pending-status repair in the hosted
evidence store; no payment, date of payment or zero income was invented.

Readback replay with the real saved source records and earlier two ownership
corrections restores position totals for 9/9 historical participants and MVP totals
for 8/9, with identical cash balances. Full-range Best XI remains unknown until
missing source histories are retrieved; the reference team's combined-payment
allocation also remains guarded. Diagnostic ledger build took 4.909 seconds locally.
This is observed stored-data behavior, not an authenticated hosted backfill claim.

Sol implemented the source/queue path; Luna investigated the official contract and
independently checked owner boundaries, unknown/duplicate names, System/Market,
negative eligibility, source-ID/round/player isolation, explicit ownership conflicts,
future timestamps and pending status. Independent hand calculation matched one
ledger: 100 opening + 10 position + 55 Best XI + 10 MVP = 175. Pending annotations
left cash and ledger amounts unchanged. No live account request or financial
transaction was used in validation.

Focused checks passed: ownership recovery (10 groups), fixture calendar (5), initial
award catalog (6), reference bonus gates (7), compact bonus snapshots (4), pending
settlement (3), classification loading/status and durable calculation reuse. Local
ownership derivation for 50 histories, 11 rounds and 121 memberships took 0.345 s
and retained 223.4 KiB; these are synthetic/local measurements. Calendar and owner
reads share the existing two-request cap, skip players without usable timing,
reuse persisted observations and retry failed writes without repeating successful
acquisition. No new dependency, schema, polling worker or authentication storage.
Final release/lifecycle results and verification limits follow below.

Final isolated release verification passed ownership/batching 10 groups, fixture
calendar 5, pending settlement 3, compact snapshots 4, partial bonus accounting,
classification loading/status, durable reuse 6, initial award catalog 6 and
reference bonus gates 7. The mandatory offline lifecycle passed 13 checks plus
8 tabs with no HTTP or live database mutation attempts. The final type-preserving
pending projection and reserved-name collision guard passed their focused checks.
An existing strict fixture caught a round-number type regression, corrected without
changing selection membership or reward amounts. Existing synthetic Plotly warnings
remain. Deployment checks passed all 3 groups, all 49 bundle checksums and all 21
runtime release hashes. Pre-existing stale release hashes were reconciled against
the isolated committed source; unrelated working-tree edits remain separate.

The delayed two-owner fixture confirms zero intermediate ledger rebuilds while
responses arrive and exactly one rebuild at batch drain; the previous ledger stays
available and loading remains truthful. This prevents a costly full reconstruction
for each returned owner. No browser/authenticated hosted backfill was run. The new
release must be served by the hosting deployment before phone behavior is verified;
remaining unsupported histories still produce honest incomplete totals.


## Unread notification cards collapsed against the right edge — 2026-11-10

The header bell reused the inbox's `fm-notification-unread` class for its 7px absolutely positioned dot, applying the dot's geometry to entire unread cards. Rename the header indicator to `fm-notification-unread-dot` and scope its CSS beneath `fm-notification-link`; retain the original inbox card class. Shiny wraps the action-link label in `span.action-label`, so the scoped selector matches descendants rather than requiring a direct child.

Verification: the expanded `node test/test_header_notifications.cjs` first failed on the original regression (expected static card positioning, received absolute). After the fix, Chromium checks passed at 320, 390 and 1440px: full inbox width, visible message and both actions, contained action bounds, no horizontal overflow, compact 7px header dot, accessible unread count, navigation and collapsed desktop header. Inspected phone and desktop screenshots. Run with `PLAYWRIGHT_BROWSERS_PATH=/tmp/futmondo-playwright-browsers` against the existing synthetic `Rscript test/test_browser_app.R` fixture. The required `Rscript test/test_shiny_simulation.R` passed 13 checks plus eight tabs with no HTTP or live database mutation attempts; existing Plotly fixture warnings remain. Notification player-details guards passed 5/5; deployment contract checks passed 3/3; JavaScript syntax and scoped whitespace checks passed. Independent source review found the same class collision and no further correctness issues.

Performance review: one class rename and one scoped CSS selector; unchanged element count, requests, database work, persisted observations, reactive updates, timers and memory. Restoring normal flow removes the character-per-line overflow without adding loading work. No dependency or environment change. Tests used synthetic local data; hosted deployment and authenticated production behavior have not been verified. Unrelated working-tree and documentation edits are preserved.


## Compact Position, Bids and Points columns — 2026-11-10

Reduce Position from its 170px minimum to 112px; set Bids to 64px and Points to 84px in the shared formatter and the Points metric override. Use `minWidth` with explicit `resizable=TRUE`. Installed Reactable source and browser inspection show that `width` fixes both minimum and maximum and disables resizing even when TRUE is requested, so the final implementation leaves `width` unset.

Verification: `Rscript test/test_player_name_crest.R` passed 5 checks and `Rscript test/test_player_metric_colors.R` passed 9, including numeric zero/negative values and metadata updates without extra enrichment. The required offline lifecycle passed 13 checks plus eight tabs, with no HTTP or live database mutation attempts; existing Plotly fixture warnings remain. Final independent Chromium inspection of Today and My Team at 390px and 1440px measured exactly 112/64/84px, contained heading text, functioning sorting for all three columns and resize handles. Dragging Position widened it from 112px to 140px in all four views. The DEF+MID row fits its two 39.61px badges in 112px without clipping; 42px metric pills fit Points. No page horizontal overflow; wide data remains inside the existing table scroll container. Scoped whitespace checks passed. The browser used the existing offline app fixture with synthetic login and external requests blocked; no new persistent test or runtime dependency was added.

Performance review: static column settings only; unchanged row payloads, formatters, element counts, API/database requests, durable caching, reactive calculations, timers and memory. Narrower columns reduce horizontal table space. No acquisition, persistence, calculation, security or manual-action behavior changes. Hosted deployment and authenticated production appearance remain unverified; unrelated edits are preserved.


## Desktop Avg Last N score hover — 2026-11-10

Shared player-table average pills now expose native multiline `title` text using their exact selected score window, for example `- round 13: 5`, `- round 12: 12`, `- round 11: 4`, newest first. Unlabelled source sequences show their exact scores with an explicit missing-round-label explanation; scalar-only averages report the absence of component scores. No round number, missing score or zero is invented. Hidden details are excluded from client search/filtering and the R formatter signature, preserving in-place updates even for score corrections with an unchanged average.

Verification: the new `Rscript test/test_recent_points_hover.R` passed six groups covering exact N/order, revised/zero/negative/decimal values, latest-round anchoring, incomplete and fallback cases, actual safe React title formatting, hidden metadata and equal-average detail updates. Independent review passed with no findings; `test_recent_points_average.R` passed all 13 checks and `test_player_points_ratios.R` passed both ratio bases, N changes, source fallback and eight-round scenarios. `test_player_metric_colors.R` passed nine groups and its generated-client checks. `test_recent_average_source_independent.R` passed 15 groups, including persistent history reuse, source-only/wider windows, responsive unresolved requests and zero network attempts. The required offline lifecycle passed 13 checks plus eight tabs with no HTTP/live database mutation attempts; existing synthetic Plotly warnings remain. Deployment contract checks passed all three groups; no dependency or manifest regeneration is required.

Performance review: preparation adds one bounded string of at most 20 score entries per player to existing enrichment; filters reuse it and the client only reads the current row's text. No new API/database request, persistence write, cache, timer, reactive observer, dependency or geometry change. The existing independent source fixture measured 527 players across 20 N changes in 0.794 seconds total (0.040 seconds mean calculation). An independent 2,000-player by 20-round calculation took 0.338 seconds locally. These synthetic figures establish local bounded cost, not hosted latency. Native title text avoids custom overlays clipped by table scrolling. Database-first history acquisition, successful-write acknowledgement and durable context isolation are unchanged.


Chromium acceptance passed on Today and My Team at 1440px and 390px using the actual app with synthetic score history and external requests blocked: exact multiline title after changing N to three, numeric pills and sorting, hidden details, zero hover-triggered requests and no page horizontal overflow. At both widths, swapping two round scores refreshed the title while retaining the same average and table DOM node. Inspected desktop and phone screenshots; native browser tooltip text was verified through the hovered pill's `title` property because native tooltips are outside page screenshots. The temporary browser fixture resets its synthetic history for each new session. No live account or transaction was used. Hosted deployment/authenticated appearance remains unverified. Unrelated working-tree and existing documentation edits are preserved.


## Leaving clause players in My Team — 2026-11-10

The existing capacity reconciler tracked pending clause departures but My Team only rendered arrivals and only requested incoming summaries. Added outgoing presentation records, independent verification status, and a matching Leaving players panel with authoritative `transferDate`, Spain CET/CEST, next-round comparison and shared player-card navigation. Incoming/Leaving use equal desktop columns and stack below 768px; received offers span the row below. Actual roster membership/count/value remains source-based. Confirmed rows survive failed/stale/malformed/overdue observations and omitted history; ordinary sales are excluded and unverified empty candidates never become a confirmed zero. Mismatched player summaries cannot certify transfers or capacity.

The prior summary cache had no durable clause snapshot. `clause_transfer_store.R` persists only safe player/owner/clause fields and source timestamp/completeness/deadline metadata through the existing server-only evidence store, isolated by account/league/team/season/scoring/player. Valid database observations are reused before source acquisition; bounded memory only accelerates reads. Failed/unacknowledged writes remain retryable and explicit refresh/actions invalidate older saved observations. No authentication fields, schema, dependency or environment variable is introduced. An independent delayed-read probe exposed the asynchronous adapter's pinned clock accepting an observation that expired during the database read; wall-clock replay and completion guards now reject it and perform at most one fresh summary recheck while retaining the same queue slot. Repeated expiry remains partial.

Verification: outgoing known-answer/source/UI checks passed 24 groups; durable snapshot JSON/isolation/retry/expiry checks passed 10; incoming compatibility 20; prerequisite recovery 6; received offers 8; acquisition availability 7 and parallel acquisition 9. The required offline lifecycle passed 13 checks plus eight tabs with no HTTP/live database mutation attempts; existing synthetic Plotly warnings remain. Independent Chromium checks passed at 320, 390 and 1440px with long names: correct departure date and round label, equal desktop/stacked phone panels, unchanged 17-player squad, correct outgoing-card click/close, no Buy Release Clause action, no page/panel/card horizontal overflow or JavaScript page errors. Phone/desktop screenshots were inspected. Browser fixtures deliberately omit unrelated finance/history data and block external images. Deployment contract and scoped whitespace checks pass.

Performance review: one shared incoming/outgoing queue remains capped at two reads, including delayed requests, context switches and the single expiry recheck. Only the current team's unresolved transfer candidates are inspected; no league-wide scan or polling is added. Each cold uncached candidate adds at most one scoped database lookup and one source read when needed; valid persisted replay uses one DB read and zero API reads, warm navigation uses zero DB/API reads. Safe snapshots are capped at 32 KiB and use the existing bounded cache and deduplicated persistence queue. Rendering adds one roster-sized panel and corresponding bounded click observers. The unchanged cold capacity fixture measured five factual reads, responsive heartbeat, 0.868 seconds locally and a 9.9 KiB snapshot. These synthetic measurements establish local behavior, not hosted latency. No real account, transaction or live database was used; deployment and authenticated hosted behavior remain unverified. Unrelated working-tree, staged and documentation edits are preserved.


## Horizontal desktop lineups — 2026-11-10

Both the editor and historical lineup modal now wrap pitch and bench in `fm-lineup-layout`. At viewport widths ≥992px the bench is far left and the field contains vertical goalkeeper, defence, midfield and forward columns. CSS reverses the existing position-group display without changing source slot identities or manual edit/save events. Markings rotate; 48px desktop portraits and natural content height accommodate six midfielders. The existing vertical pitch and bench below remain below the breakpoint.

Verification: `Rscript test/test_classification_lineup.R` passed 17 groups and `Rscript test/test_lineup_module.R` passed 18. The editor Chromium suite passed at 390 and 1440px, including exact player selection, manual removal/refill, synthetic verified save, 3-6-1 and 4-6-0. Historical Chromium checks passed at 360×640, 390×640, 568×320 and 1440×1000, including holes, substituted players, custom positive/negative scores, keyboard selection and phone dismissal. Shared geometry assertions check all 15 slots, placement and order, card containment/nonoverlap and goal/halfway-line orientation. Resizing through 991, 992, 390 and 1440px preserved source/submission/database counters. Inspected synthetic desktop previews; a 3-6-1 historical layout measured 870×692px. The mandatory offline lifecycle passed 13 checks plus eight tabs without HTTP or live database mutation attempts; existing synthetic Plotly warnings remain. Deployment contract passed 3/3; JavaScript syntax and scoped whitespace checks passed.

Independent review: isolated Chromium checks at 992px with a 230px sidebar passed for 3-6-1 and 4-6-0 with and without a bench, including long player names, card nonoverlap and viewport fit. Dense long-name columns can reach about 820px high and use normal vertical scrolling. No further runtime issue was found.

Performance review: one static wrapper per lineup and CSS layout rules; unchanged bounded 11+4 player tiles, API/database request volume, persistence/cache reuse, invalidation, reactive work, timers and acquisition behavior. Browser resize only relayouts existing elements, with no new source or storage work. Six-player columns can require vertical scrolling; no fixed height clips content. No runtime dependency, environment variable, schema or manifest dependency change. Verification used offline synthetic data; no authenticated transaction or hosted deployment was performed. Unrelated working-tree and staged edits are preserved.

Final isolated verification of the exact staged artifact passed outgoing 24, persistence 10, incoming 20, source recovery 6, offers 8, parallel capacity 9, availability 7, deployment 3 and the mandatory lifecycle 13 plus eight tabs. All 50 bundle checksums match. Three inherited stale checksum entries were reconciled against already committed player-table, server and stylesheet files; their source content and unrelated local changes were not included in this task. No dependency regeneration was required.


## Round history HTTP404 from a duplicated database API path — 2026-11-10

Root cause established with live database log metadata: eight GETs to
`/rest/v1/rest/v1/player_round_dataset` and 245 POSTs to
`/rest/v1/rest/v1/player_match_observations` returned HTTP404 / `PGRST125` in the
inspected 24-hour window. Other database resources had the same duplicated prefix.
Correctly addressed requests succeeded; the dataset/check/observation objects and
service-role privileges exist. Stored observations/checks remain present. No raw
private response bodies, keys or Futmondo account requests were used for diagnosis.

Normalize `get_sb_url()` after dynamic environment/fallback selection so copied
REST bases and project roots reach the same endpoint. All runtime database URL
builders use this getter. The fix preserves server-side credentials, context
isolation, successful-write acknowledgements, bounded acquisition and durable
reuse. README and the round-matrix contract describe configuration and recovery.

Verification: the new synthetic regression first failed on the original getter
before starting its local HTTP fixture, then passed all four groups. Both URL
forms used exactly seven requests: empty matrix read, score write, success-check
write, paginated readback, and a fresh controller read after clearing process
caches. The controller made zero player-summary requests. Scores 0, -2 and NULL
survived; the check was acknowledged only after scores. A heartbeat executed while
the database response was delayed. Independent source review confirmed every
caller is covered. Focused reader checks passed 4 groups; persistence recovery
passed 11; final isolated offline lifecycle passed 13 checks plus eight tabs with
no HTTP or live database mutation attempts. Existing synthetic Plotly warnings
remain. Fixture JavaScript syntax and deployment-contract checks passed. Bundle
metadata was reconciled against the isolated committed source plus this change,
including pre-existing stale tooltip hashes; no unrelated source edits are bundled.

An additional broader recovery run stopped at an existing wider-window fixture
assertion: it expects two completed N8 histories while both new wider requests are
still pending. Independent replay with the original and normalized getters failed
identically, with zero calls to either getter. This unrelated fixture was left
unchanged; the complete broader suite is not claimed to pass.

Performance review: bounded string normalization adds no API/database requests,
cache entries, polling, reactive redraws or UI elements. Both fixture variants
have identical seven-request paths and reuse persisted history after memory is
cleared. Initial adapter dispatch was 0.175s and the subsequent setup 0.007s in
the isolated run; these are local measurements of existing adapter setup, not
hosted latency guarantees. Delayed-response responsiveness passed. Mobile layout,
manual actions, source timestamps and persistence retry budgets are unchanged. No
dependency, environment-variable name, schema migration or authentication storage
was added. The hosting environment is not accessible here; the user will correct
its URL. Hosted restart/deployment, subsequent successful history acquisition and
complete recovery of previously unsaved observations remain unverified.


## My Team bulk listing button (2026-11-26)

An offline render of My Team verified that the bulk listing button appears once, immediately after the team value summary and before the grid that starts with Incoming players. All nine offline bulk market checks passed. Reordering the existing HTML adds no requests, observers, timers or DOM nodes.

## Bulk listing eligibility and additive premiums (2026-11-26)

The previous preview styled expected exclusions as errors. It also compared offset timestamps as local clock values, and a malformed first date could abort the calculation for the whole squad. The revised plan reports the eligible count for each pricing method. Open clauses and transfers in progress receive separate explanations. Missing data stays unknown. The result identifies unconfirmed players among the requests actually sent.

The 14 focused bulk-pricing checks and four market-action checks passed offline. Fixtures covered mixed and empty selections, unavailable data, UTC offsets, malformed input, exact prices with either base, verified and unconfirmed requests, duplicate confirmation and account context. The offline Shiny lifecycle passed 14 checks and eight tabs. The separate UI reliability script stopped in a player-table fixture on the missing `lineup_fixture_crest_url` helper. Its unmodified baseline also stopped there, and its bulk-listing regression passed in isolation.

Chromium at 320, 390 and 1440 px showed 17 of 17 eligible at market value and 2 of 17 at closed-clause value. Two +1% presses gave 2%. Adding +1% to a typed 1.25% gave 2.25%, which carried across a mode change. The dialog reopened with confirmation enabled even after cancelling at the default 0%. A 1.25% clause submission confirmed one synthetic new listing and one synthetic replacement at €4,050,000 each. With all 17 clauses open, the dialog showed zero eligible and disabled confirmation. The browser used an HTTP-blocked fixture with synthetic transaction handlers.

Each preview reuses the existing persisted roster projection. Premium presses change the numeric input locally and trigger the usual preview render without extra source or database reads. Listing requests remain limited to eligible players on explicit confirmation. In 50 local 30-player renders, market-value previews took 1.83 s versus 1.78 s previously. Closed-clause previews with ten eligible players took 1.04 s versus 0.68 s, including parsing the opening times. These timings cover local calculation and HTML rendering.

## Bulk market listing premiums — 2026-11-10

The bulk listing dialog now accepts a premium for either current market value or a closed clause, with a custom percentage and all six requested quick buttons. Its 0% default preserves an unmarked market-value listing. Preview and submission share the same plan, with explicit base/increase/final amounts and totals over eligible players only. Missing values, ineligible clauses and invalid percentages remain distinct from zero. Whole-euro calculation corrects binary over-rounding for both 100 at 10% and 10,000 at 0.07%, while retaining genuinely fractional increases. Valid confirmation remains single-use and account/league/team scoped; invalid pricing can be corrected in the same dialog.

Verification: the new `Rscript test/test_bulk_market_premium.R` passed nine groups covering all six presets on both bases, custom/zero/tiny percentages, independent rounding fixtures, invalid and missing data, escaped preview names/totals, exact submitted prices for both modes, correction, duplicate/stale confirmation and bounded rendering. Existing market listing checks passed 4/4. Required `Rscript test/test_shiny_simulation.R` passed 13 checks plus eight tabs with no HTTP or live database mutation attempts; existing synthetic Plotly warnings remain. Deployment contract passed 3/3. Independent source review passed.

The new `node test/test_bulk_market_premium.cjs` passed at 320, 390 and 1440px against the existing HTTP-blocked synthetic browser fixture: six buttons, 17 exact per-player increases/final prices and total, custom 1.25%, clause-mode eligibility and percentage retention, negative/over-limit/blank validation, recovery, cancel/reopen reset, no horizontal overflow and no JavaScript errors. Phone and desktop screenshots were inspected. Buttons and amounts wrap; small phones use normal vertical modal scrolling to reach the footer. No browser confirmation or real transaction was submitted.

The broader existing UI reliability script passes its bulk-action regression and then fails its final unrelated classification assertion for `class-single_round_select`. Independent replay using the pre-task module produces the identical failure; that classification fixture is left unchanged. The full broader script is not claimed to pass.

Performance review: premium/mode changes only invalidate the new preview, using the existing roster reactive; no additional API/database reads, writes, durable results, invalidation, timers or acquisition queue are added. One local pass computes and renders the squad; a synthetic 30-player fixture rendered 100 previews in 3.778s total (about 38ms each), with 10.6 KiB HTML per preview. The player list scrolls within 280px. The existing synchronous confirmation loop and its per-player request count are unchanged; this task does not establish hosted transaction latency. Existing database-backed roster reuse remains intact. No dependency, environment, migration or authentication change. Hosted deployment and authenticated production behavior remain unverified. Unrelated working-tree/staged edits are preserved.

Final verification of the isolated staged application passed premium 9/9, listing 4/4, deployment 3/3 and the mandatory offline lifecycle (13 checks plus eight tabs). All 50 staged bundle checksums match; only this task’s module checksum is changed.


## General competition context and real-world transfers — 2026-11-10

Status: **implemented locally and verified with isolated/synthetic sources; production migration and application deployment remain separate, pending rollout work**. Existing unknown histories were preserved, not assigned to current seasons or clubs. The admission policy was not changed. No real account, trade or production database write was used for these checks.

### Executed checks

- New context suite: 14 groups covering authoritative mapping, explicit unknown archives, canonical scoring, durable hydration/retry, source-age preservation, compact metadata, both transfer-ID outcomes, cache eviction under multiple accounts and bounded memory.
- New round-label suite: four known-answer checks verify an exceptional official schedule lookup, preservation of original labels/zero/negative values, zero extra reads for ordinary history and truthful source-failure behavior.
- New adapter suite: three scenarios cover a season change while a read is pending, explicit historical-competition cache isolation and same-page expiry renewal without polling.
- New history suite: 15 checks cover exact scoped reads, official round identities/ambiguity, cross-competition fixtures, bootstrap sequencing, warm reuse, expiry, unresolved contexts and known-scoring reuse without another lineup read.
- New snapshot suite: nine groups cover exact lineup/finance/offer/clause/history scopes, incompatible legacy versions, restart reuse, pending-write acknowledgements and late lineup completion.
- New isolated database suite: 11 groups cover repeat fresh initialization, preserved administration preferences, both transfer-ID outcomes, simultaneous competition membership, source-round identity, latest NULL corrections, ownership/Market rules, browser grants, upgrade preservation and rollback. Existing factual-schema suite: 10 groups passed after updating its conflict fixture to the new source-time identity.
- Required `test/test_shiny_simulation.R`: 13 lifecycle checks plus eight retained tabs passed, with HTTP/live mutations blocked. Dedicated actual-server onboarding checks passed for zero leagues and a league without a playable team.
- A 19-script focused regression batch passed across data service/contracts, session ownership, asynchronous transport/recovery, observation recording, history persistence/recovery, request contracts, round readers, awards, finance, notification details, deployment packaging and the new competition suites. Further controller/snapshot checks covered 27 recent-point async groups, full recovery, fitness snapshots, incremental history, lineup store/module/runtime, complete history baselines, clause/offer persistence, rival cache restart/ack recovery and classification/round lineups. Stale recovery fixture assumptions were reproduced against the pre-task controller before being updated to await the existing bounded short-to-wide acquisition.
- Offline Chromium acceptance passed at 1440, 390 and 360 px: duplicate labels, selector switching, shared player IDs with separate competition points, no round-number-only foreign fixture association, empty roster/no completed rounds, no document/modal overflow, no JavaScript errors and zero external HTTP attempts. Mobile screenshots were inspected. Test artifacts are `/tmp/futmondo-competition-*.png`; they are not deployment assets.

### Performance review and limits

Known scoring now goes directly from verified context to the scoped matrix, avoiding a redundant lineup acquisition. Ordinary 38-round summaries added zero schedule reads; 1,000 ambiguity checks took 0.301 s (0.301 ms per check) in a synthetic sample. An official schedule is demanded only for an actual repeated-label/round-ID collision, and original labels remain stored. Warm context/table/snapshot reuse adds zero source requests in the focused fixtures. Existing roster/market request-contract check remains five cold reads / zero warm reads; pressroom remains three cursor reads / zero warm reads. Incremental history remains four to two requests and 1,200 to 26 transferred rows in its fixture. Historical acquisition retains two in-flight reads and existing cache/queue bounds; metadata has a separate 128-entry / 8 MiB bound with 1 MiB entry admission. Observation source timestamps are never renewed by cache hydration.

A synthetic 3,000-row mixed-context database query used the new contextual index before LIMIT and took 3.665 ms in one isolated sample. Added indexes increase write/storage work; production throughput and total R-process memory were not benchmarked. A 2,500-row durable baseline prepared in 0.408 s and restored with one nonblocking database read; encoded payload was 11,548 bytes from 332,425 JSON bytes. The existing 100-player lineup fixture averaged 42.5 ms across 20 runs.

After normal interpreter initialization, held-I/O browser heartbeat roundtrips were 19–60 ms (strict acceptance below 500 ms). Cold card initialization was measured separately: current identity-plus-next-heartbeat 1,479 ms versus original 1,457 ms in a controlled pair. Profiles attributed about 0.63 s/current and 0.71 s/original to first-use R compilation. No material increase was established; this remains a first-use latency limitation, not a claim that every cold interaction completes below 500 ms. No JIT settings or hosting processes were changed.

### Source evidence and rollout

Captured authenticated metadata proves the championship-to-real-league season mapping; captured profile/match responses prove their nested league context. Historical-season row IDs are opaque and are not used as player aliases. Midseason source-ID continuity and actual Premier League/Champions League player payload compatibility remain unverified; fixtures exercise both ID behaviors, not live league certification.

Apply `scripts/migrations/20261110225848_general_competition_context.sql` only in an authorized production rollout before deploying these readers/writers. Rehearsal preserved original legacy records and unknown timestamps, and rollback restored the original schema/data. No bulk historical relabeling or migration of hashes without direct configuration evidence was performed. New code, schema, tests and documentation are packaged for review; internal tests/docs/migrations and captures remain excluded from the application bundle. No new dependency or environment variable was introduced.

## Login crash from masked Shiny validation — 2026-11-10

The supplied Posit log reports `Error in validate: is.character(txt) is not TRUE` in `championship_RV()` at `server.R:63`. Reproducing hosted startup with Shiny already attached makes the subsequent JSON import resolve unqualified `validate()` to `jsonlite::validate()`. For a valid league, `need(TRUE, ...)` returns NULL, which triggers that exact JSON-validator error. Both league and playable-team guards now explicitly call `shiny::validate(shiny::need(...))`.

The offline lifecycle previously loaded Shiny after JSON parsing and missed the collision. Its startup now preloads Shiny, matching hosting. Under that order, the unchanged empty-league onboarding assertion failed before the runtime correction. The focused regression now requires JSON masking to be present and checks normal authentication/league/team resolution, repeated selection without additional league fixture calls, an authenticated empty league list, and a league without a playable team. Focused onboarding passed all three scenarios. The exact staged application independently passed the same focused regression, the mandatory offline lifecycle (13 checks plus eight tabs), all three deployment contracts, and matching server/release checksums for `login-validation-2026-11-10`. Scoped whitespace checks passed. No dependency regeneration was required; unrelated working-tree and metadata edits remain excluded from this commit.

Performance review: two qualified validation calls add no API/database work, invalidation, timers, blocking I/O, reactive consumers, growing state or rendering changes at any viewport. Repeated-selection coverage checks reuse; the required lifecycle passed 13 checks plus eight tabs with HTTP and live database mutations blocked. Existing data persistence contracts are unchanged; no schema, dependency or environment-variable change is needed. Documentation and checksum updates have no runtime data-path cost. Independent source review found no additional application validation collisions. No real account or transaction was used, and hosted redeployment/authenticated verification remain pending.


## Rival balance loading latency — 2026-11-10

Saved evidence previously set `finance_ready`, replacing an already displayed database snapshot or rejecting a snapshot arriving later. Team observations also waited for the entire seed's pressroom and notifications. The loader now keeps the labelled saved preview until seed completion and initial team acquisition, accepts either database response order, and defers ledger reconstruction during this batch. Configuration progress releases team reads while history continues within the existing two-task limit, with generation/season checks.

Executed checks: `test/test_rival_loading_latency.R` passes both response orders, preserves saved positive/negative cash during held I/O, starts team A before seed history finishes and team B after A, requests each team once, performs one final reconstruction, and clears private previews on account changes. Held-I/O heartbeat callbacks measured 5.0 and 5.1 ms in the initial run (acceptance below 500 ms). These are event-loop measurements, not hosted page-load timings. `test/test_rival_finance_runtime.R` passed 18 checks; `test/test_rival_finance_reuse.R` and `test/test_rival_persistent_cache.R` passed, including fresh-process snapshot reuse and write failure/retry. `Rscript test/test_shiny_simulation.R` passed 13 checks plus eight tabs with no HTTP or live database mutation attempts; existing Plotly fallback warnings were emitted.

Performance review: source/database request counts and queue bounds are unchanged; team reads overlap slow history without increasing concurrency. Snapshot previews avoid repeated historical calculations while initial observations accumulate. Existing durable storage, acknowledgement/retry, source timestamps, completeness and context isolation remain enforced. No additional reactive timer, unbounded retained state, dependency, schema, environment variable, mobile layout or rendering structure is introduced. Documentation has no runtime effect. Tests used synthetic data only. A first visit without a valid saved snapshot still depends on authoritative source latency; hosted deployment and authenticated timing remain pending.


## Financial-history date/name spacing — 2026-11-10

The Date and Name columns previously shared default sizing. Date now uses 112 px with date/time stacked; Name has a 210 px minimum and wraps rather than clipping cell text. Non-date labels retain their original text and wrap. Only `rivals_cash_history_widget()` is affected. Existing horizontal scrolling remains necessary to access all financial-history columns on mobile.

Executed `Rscript test/test_transfer_history_metadata.R`: passed metadata, crest, escaping, bonus and unchanged-balance checks; 1,000-row metadata/display projection measured 0.038 s and 122.0 KiB. Rendered the actual widget with a synthetic long player name in installed Playwright Chromium at 360, 390, 576 and 1440 px: Date measured 112 px, Name at least 210 px, date/time remained present, and neither cell had internal text overflow. Inspected the 390 px screenshot. Browser fixture was offline and used no account or transaction; hosted styling/deployment remain unverified.

Performance review: column sizing and small presentation spans add no API/database requests, cache changes, invalidation, timers, blocking I/O or retained state. Existing 15-row pagination bounds mounted date spans and text wrapping. Mobile rows may become taller when names wrap; the table retains its existing horizontal scroll. This is a widget-local presentation change; shared infrastructure/startup and cross-cutting UI are unaffected. Documentation has no runtime impact; no dependency or manifest changes were needed.


## Durable balance/transaction linkage and incremental refresh — 2026-11-10

New balance snapshots retain the complete associated cash ledgers rather than falling back to summaries when shared preparation is oversized. Small compact archives remain inline; larger ledgers use immutable scoped/content-hashed chunks in the existing private evidence table, followed by an acknowledged balance manifest. Missing/corrupt/foreign chunks fail restoration, not transaction completeness. Legacy summary-only snapshots remain previews and are eligible for an upgraded save even with unchanged financial signatures. Saved finance acquisition first reuses the complete durable pressroom baseline rather than repeating a full transaction-table scan.

Normal transfer refreshes preserve the DB baseline regardless of full-audit age and fetch the source head through the existing one-day correction overlap. Explicit `full_audit=TRUE` still retrieves all source history, including corrections/deletions older than that overlap. No unsupported API cursor is assumed: a metadata-only inspection of the captured finances HAR found only championship/team IDs in money-movement queries. That current-list read remains; unchanged movement payloads avoid DB revisions, while new/corrected movements persist. Comparison uses confirmed DB state so a failed prior write retries on Refresh; JSONB ordering and numeric restoration do not create false changes.

Executed focused checks: `test/test_rival_ledger_archive.R` restored all 5,001 entries and the exact -4,900 known balance in a fresh R process, used three chunks plus the manifest, required acknowledgements in order, retried failure, rejected missing/corrupt/foreign chunks, checked confirmed-DB movement dedup/retry and durable transfer-baseline reuse. One final isolated run measured 0.268 s to encode and 0.412 s to restore a 297.6 KiB archive; earlier runs ranged 0.243–0.288 s / 0.402–0.454 s. These include synthetic DB-shaped decoding and exclude hosted latency. An isolated PGlite PostgreSQL query check loaded the existing schema/context migration and actual synthetic archive rows, verified complete manifest-to-chunk retrieval, idempotent upserts and denied browser-role reads. No production SQL or private account was used.

`test/test_pressroom_incremental.R` passed: 11 full versus 3 overlap API pages, zero warm requests, complete 1,203-row recovery after clearing memory using two DB reads/three overlap pages, old-baseline reuse, explicit old-correction audit, failed checkpoint retention and context isolation. `test/test_locker_async.R` passed: five full versus one overlap page, explicit audit, corrections/late payments and delayed 36-page heartbeat. `test/test_rival_bonus_snapshot.R`, `test/test_rival_persistent_cache.R`, `test/test_rival_finance_reuse.R`, `test/test_rival_loading_latency.R` and 13 groups in `test/test_history_baseline_store.R` passed. The known-answer `test/test_rival_running_balances.R` passed 13 checks plus the main-history reconciliation check; its 1,000-transfer ledger took 0.135 s. Finance runtime passed 18 checks. Required final offline lifecycle results are recorded after its completion.

Performance review: warm history reuse adds no API calls; durable baselines eliminate ordinary full rescans and redundant transaction-table scans. Only changed source movements append revisions. Large archive restoration adds one scoped chunk query; ordinary full-cache/inline snapshots retain one read. Serial chunk writes preserve queue concurrency/entry limits and publish completeness last. Serialization is bounded but synchronous (measured above); accepted maximum-size archives can cost more CPU than this fixture and hosted timing remains unmeasured. Memory retains bounded DB-confirmed movement references within the existing 20,000-evidence limit, and archive JSON/compressed/chunk limits are explicit. No new timer, background harvesting, UI redraw dependency, mobile rendering structure, schema, dependency or environment setting is added. Documentation changes have no runtime effect. Existing credentials/RLS/context protections and truthful unknown/stale values remain in place.

Final verification: the offline Shiny lifecycle passed 13 checks plus eight tabs with no HTTP/live database mutations. The archive regression also reorders JSONB object keys before restoration; the R reader restored the exact 5,001-entry ledger from actual isolated PostgreSQL JSONB output. Scoped whitespace checks passed. No production migration or deployment was performed.


## Cookie-first login presentation — 2026-11-11

The login module now gates credentials on completion of the existing remembered-cookie probe. Its unsuspended visibility outputs keep the welcome card visible during probing/restoration and reveal credentials only when ordinary login is required. Successful sign-in hides credentials, with Logout in the welcome card. Restore retry, missing/expired credentials, access denial, expiry and disabled configuration retain a usable manual login path. The synthetic HTTPS fixture now supplies an explicitly open access-policy response through the real admission gate, avoiding live storage.

Executed `Rscript test/test_login_restore_visibility.R`: passed initial probe, completion without authentication, restoration, expiry, admission denial and disabled-configuration states. `R_LIBS=/tmp/futmondo-login-r-library Rscript test/test_remember_login.R`: 21 passed. `R_LIBS=/tmp/futmondo-login-r-library PLAYWRIGHT_BROWSERS_PATH=/tmp/futmondo-login-browsers node test/test_remember_login_browser.cjs`: eight checks passed using temporary sodium/Chromium installations. The browser held cookie requests, sampled rendered frames (zero visible credential frames on successful restoration), checked welcome visibility, simulated transport failure/retry, restored after browser/server restart, confirmed fixed cookie expiry, exercised logout and failed persistence, and checked no horizontal document overflow at 390/1440 px. No real account, API or database was used. The required offline Shiny lifecycle passed 13 checks plus eight tabs. Hosted deployment and real-device behavior remain unverified.

Performance review: presentation-only gating adds two small scalar outputs and one bounded session-local flag; no API/database calls, cache reads/writes/invalidation, polling, timers, blocking work or accumulating state are introduced. Existing encrypted database persistence, cookie checks and validated league-result reuse are unchanged. The delayed-cookie browser check verified the welcome card remains rendered while acquisition is pending. Flex wrapping expands the welcome card when credentials are hidden and bounds both cards on mobile. Documentation has no runtime effect; no dependency, environment setting or manifest change is needed.


## Chronological player points line — 2026-11-11

The chart sorted points by round number even when authoritative match dates were out of round order (for example postponed fixtures). Plotly connects supplied rows, producing backward/crossing amber segments. The renderer now sorts complete score rows by the displayed horizontal coordinate, with round number breaking date ties. Score values, finality and hover round/date provenance remain associated. Undated scores retain the existing independent round axis; no dates are manufactured.

Executed `Rscript test/test_player_trend_plot.R`: all 15 checks passed, including unsorted/postponed rounds, duplicate dates, zero/negative scores, provisional provenance, undated fallback and distinct scales. Actual-widget HTML generation succeeded. Attempted Playwright mobile/desktop rendering was blocked by the missing Chromium headless-shell executable; hosted appearance and the screenshot player’s source dates remain unverified. No account or transaction was used.

Performance review: one local row sort adds O(n log n) work and O(n) row-copy memory for the existing per-player score history. No additional API/database request, cache/persistence change, invalidation, polling, reactive dependency, blocking I/O, point count or mobile layout change. Existing durable shared history is reused. This widget-local change does not affect startup/shared infrastructure; the focused rendering regression is the applicable check. Documentation has no runtime effect. No dependency or manifest change.


## Contabo performance and background-processing review — 2026-11-11

Completed source/deployment review in `docs/contabo_performance_review.md`; corrected current hosting guidance in README and AGENTS while retaining historical evidence. Production hosting on Contabo is owner-reported; no VM inspection, deployment, production database access or account operation was performed. Recommendations are not implemented.

Executed existing offline checks against the current working tree: `Rscript test/test_async_transport_priority.R` passed 10/10 (938 ms normal versus 179 ms interactive sequential capacity fixture; 42 ms foreground with three slow background sockets; unchanged request counts/four-connection cap). `Rscript test/test_rival_ledger_archive.R` passed complete 5,001-entry fresh-process restore, three chunks and acknowledged manifest, corruption/context rejection, confirmed-write retry/deduplication and persisted baseline reuse. Archive encoding took 0.207 s, restoration 0.360 s, encoded size 297.6 KiB. These are single-run synthetic development measurements, not hosted speedups or certification of unrelated working-tree edits.

Performance review: documentation-only delivery adds no runtime API/database requests, invalidation, observers, timers, blocking work, retained memory or browser/mobile rendering cost. No runtime/dependency/schema/environment changes were made; lifecycle and manifest regeneration are not required for this delivery. The focused tests ground the review rather than verify new runtime behavior. Worker sizing, Contabo CPU/RAM/latency, database region/round-trip cost, deployed package compatibility, concurrency, mobile reconnection and actual source-rate limits remain unverified. Preserve unrelated local/staged changes; implement and validate later phases separately.


## LigaRadar application title — 2026-11-11

Updated `ui.R` to display **LigaRadar** in the header and set the dashboard page title explicitly. Generated HTML assertions verified both the head title and visible brand; R syntax/title-argument checks passed. `Rscript test/test_shiny_simulation.R` passed 13 checks plus eight tabs, with no HTTP or live database mutation attempts. No production deployment or browser viewport check was performed.

Performance review: static text replacement and a scalar page-title argument add no API/database requests, cache changes, blocking work, reactive updates, timers or retained state. Existing responsive header styling is reused; the brand is one character longer. No material rendering cost, dependency or manifest change. Documentation adds no runtime cost.


## Compact player-table statistics — 2026-11-11

Reduced Selling by from 150px to 125px; Rating/Played use 72px, Avg Points/Home Avg/Away Avg use 84px, and Avg Last 5/Sum last 5 use 90px. All remain resizable without fixed widths or maximum caps. Acquisition Price is unchanged. Offline assertions verified effective definitions, widget construction, retained acquisition defaults and negative-average formatting. `Rscript test/test_squad_table_prices.R` passed 7/7; `Rscript test/test_shiny_simulation.R` passed 13 checks plus eight tabs with no HTTP/live database mutation attempts. No browser viewport or production deployment verification was performed.

Performance review: constant column-definition metadata only; no additional API/database requests, cache invalidation, persistence changes, blocking work, reactive dependencies, timers, per-row rendering elements or growing memory. Narrower columns reduce horizontal table space on mobile; existing scrolling/header wrapping and resize handles are reused. No material runtime cost or dependency/manifest change. Documentation has no runtime effect.


## Python background acquisition and prepared Shiny reads — 2026-11-11

Status: implemented locally and verified with isolated/synthetic sources. Docker supervises Python alongside Shiny; the service acquires factual data for expressly opted-in accounts and delegates existing parsing/accounting to private R helpers. Non-opted-in accounts retain interactive acquisition. Encrypted worker tokens are distinct from remembered-browser credentials, bound to account/credential generation, and retained until disabled/rejected. Explicit manual actions/preflights/readback remain live and never enter the worker retry queue. Setup and exact interfaces are in `docs/python_worker.md`.

Executed checks: 13 R typed-contract groups (empty/unknown/zero/negative values, context class, secret rejection, canonical keys, account/scoring isolation, original timestamps and corrupt payloads); six provider groups with zero source HTTP attempts (warm sync/async reads, coalesced stale acquisition, unknown-policy waiting, opt-out and abandoned-generation rejection); 13 Python tests covering encryption, DST/quiet hours, fresh DB reuse, restart checkpoint reuse, source-auth rejection without retry, Retry-After, denied mutation/SSRF routes, revocation/membership, actual R parsing through the Python transport, acknowledged publication order and replayed source timestamps. Nine isolated PGlite groups cover repeat schema, guarded leases/leadership, snapshots/chunks, cross-account/version filtering, monotonic metadata writes, source checkpoints, refresh-during-run, revocation, excluded leagues, browser-role denial and reference-safe cleanup. The existing factual-schema suite passed 10 groups with the additive objects installed.

The actual headless finance controller completed with synthetic evidence and independently reconciled opening cash 100 minus a Market purchase of 10 to a complete included ledger total of 90. The actual standalone settings fixture passed Chromium checks at 360, 390 and 1440 px: consent/save, preserved unsaved quiet-hour edits through status polls and credential renewal, browser-only countdown, no horizontal overflow/JavaScript errors, and 66–67 ms heartbeat roundtrips. The 390 px screenshot was inspected; synthetic artifacts remain in `/tmp`. Browser checks found and drove fixes for empty selections, panel state, credential-renewal preference races and stale policy responses.

Existing lineup runtime passed 13 groups; lineup module passed 18 groups including uncertain-response reconciliation and no automatic replay. Existing async transport priority passed 10/10 (foreground reserved-slot fixture 42 ms, unchanged source request/connection counts). Existing rival persistent-cache checks passed typed fresh-process reuse, changed-input invalidation and failed-write retry. The mandatory offline Shiny lifecycle passed 13 checks plus all eight retained tabs with no HTTP/live database mutation attempts; existing synthetic Plotly warnings remained. Deployment contracts passed 3/3. Manifest regeneration found 78 R dependencies with no new packages/version changes; worker service files, tests, docs, captures and credentials remain outside the alternate Shiny bundle.

Performance review: normal opted-in browsing adds one coalesced asynchronous revision/status read per visible account/session group every five seconds; payloads are transferred only for changed versions. Non-opted-in accounts on a worker-configured deployment also check policy while visible. The decoded snapshot cache is limited to 256 entries/32 MiB and demanded resources/waiters to 128 per account. Only affected resource revisions notify data consumers; credential renewal does not rebuild unsaved settings. Countdown ticks add no server timer or API/database work. A synthetic 5,001-row ledger restored in 0.105–0.154 s from about 467.6 KiB JSON / 54.4 KiB gzip, below the 500 ms focused gate. Temporary decode buffers and total R/helper RSS exceed cache accounting and require hosted measurement.

Background acquisition increases unattended API/database/storage work compared with purely on-demand use. Limits default to two helper lanes/two source connections, one active source read per account and two starts/sec/account, with five-second idle claim polling, fifteen-second leadership/health renewal and hourly cleanup. Quiet-hours checks, persisted child reuse, incremental history, checkpoints and monotonic acknowledgements limit duplicate work. Source streaming is bounded by 16 MiB and fifteen seconds. Each published version can update active views; hosted request volume, history growth, sustained RSS and multi-user freshness were not benchmarked.

Limitations: no real account, trade, production SQL or Contabo process was used. The Docker CLI is present but `/var/run/docker.sock` is absent and the executor is UID 1000, so it cannot start a daemon; actual image build/service startup must be verified on the target host. Production migration/deployment, source-rate entitlements, token-expiry body codes beyond verified HTTP rejection, VM capacity, hosted timezone data and authenticated full-catalog/historical acceptance remain unverified. Missing source data remains partial/actionable rather than fabricated or zero-filled. Unrelated working-tree and staged changes are preserved.

Final isolated artifact checks passed the typed/provider/Python contracts, known-answer headless finance, large-ledger restore, nine database groups, existing factual schema, eighteen lineup-module groups, all three deployment contracts and the mandatory 13-check/eight-tab offline lifecycle. The actual staged settings fixture passed all three browser widths. All 54 staged bundle checksums were verified; eight inherited checksums for already-committed source files were refreshed for coherent packaging without including unrelated source edits. Existing R package metadata/versions were retained. Docker build and production rollout remain unverified for the specific daemon/access limitations above.


### Python worker production migration — verified, 2026-11-11

Applied the approved acquisition SQL to production and verified hosted migration history, all eight RLS-protected worker tables, thirteen server-only invoker functions, a rolled-back service-role read and the indexed due-job query plan. No accounts or factual test records were created. Performance assessment and deployment limits are recorded in [migration verification](background_acquisition_migration_verification.md); production acquisition throughput remains unmeasured. Repository changes for this task are documentation only.


### Post-login My Team empty roster — 2026-11-11

The new actual-module regression reproduced the supplied production traceback before the fix: assigning scalar FALSE to missing `market_inMarket` on a zero-row roster raised `replacement has 1 row, data has 0`. Initializing a length-nrow logical vector resolves that exception without adding data. Two focused checks pass: delayed acquisition stays loading without invented empty data or listing demand; a verified empty result renders with source status/time intact and exactly one shared roster read plus one listing read. HTTP is blocked and acquisition uses synthetic resources only. No real account/source payload was supplied, so why this account received an empty roster is not established; this fix does not certify completeness of that account's squad.

Performance review: linear vector initialization replaces scalar recycling, with zero elements for empty results; no added requests, persistence, invalidation, timers, redraw dependencies, DOM or layout changes. Existing offer regression passed eight checks, retaining three cold endpoint reads and zero extra offer-panel/warm reads. Full offline lifecycle validation is recorded below. No dependency change; only the changed module checksum is updated in the deployment manifest. Hosted redeployment/reproduction remains pending.

Executed verification: `Rscript test/test_empty_squad_module.R` passed 2 checks; `Rscript test/test_team_received_offers.R` passed 8 checks; `Rscript test/test_shiny_simulation.R` passed 13 checks plus 8 rendered tabs with no HTTP/live database mutation attempts.


### Background resource parsing and missing production context schema — 2026-11-11

Reproduced `Invalid worker names` using the actual `get_active_championships` descriptor after removing its only named argument, `login`. An empty named R list serializes `names` as `[]`; `jsonlite::fromJSON(..., simplifyVector=FALSE)` restores an empty list, and `unlist()` returns NULL. The decoder rejected that valid zero-length names vector. `fm_worker_decode()` now restores character(0) for empty list/vector names while retaining length, missing-name and secret-name checks. Existing descriptor encoding, keys and database snapshots remain compatible; no acquisition bypass, fabricated data or opt-in change was introduced.

Read-only production diagnostics corroborated the failing named-empty descriptor shape for league discovery, plus `resource_parse` on notifications and finance. Separate safe edge-log aggregates identified `/rest/v1/rpc/worker_write_fact` HTTP 404 / `42P01`, and missing metadata reads (`PGRST205`). Catalog inspection confirmed both `league_context_observations` and `player_membership_observations` are absent, as are competition columns on all eleven prerequisite factual tables. The existing `20261110225848_general_competition_context.sql` migration is required; applying only the later worker migration did not install those prerequisites. Production schema repair approval is pending; no production data/schema was changed by these diagnostics. Decoder deployment and authenticated hosted acceptance also remain pending. This does not certify the reported production incident resolved.

Verification: R worker contracts passed 18 groups; Python worker suite passed 14 tests, including real R helper league discovery and notification reads with named-empty arguments, source request counts and chunk/manifest restoration. Provider suite passed six groups with zero source HTTP attempts, including fresh saved reuse and coalesced stale acquisition. The actual headless finance controller retained the known-answer ledger (100 opening cash minus a 10 Market purchase equals 90). Full offline lifecycle passed 13 checks plus eight rendered tabs with no HTTP/live database mutation attempts. Existing competition migration rehearsal passed 11 isolated PostgreSQL groups, including repeat upgrades, rollback preservation, denied browser access and indexed context queries. Synthetic/captured source responses only were replayed; no real account or transaction was used.

Performance review: the change adds one NULL-to-empty-vector normalization per named decoded list/vector; traversal and memory bounds remain unchanged. No additional API/database requests, polling, invalidations, timers, UI redraws or DOM/mobile-layout work are added. Durable snapshot persistence/reuse and source timestamps remain unchanged. The 5,001-entry ledger restore measured 0.207 s (467.7 KiB JSON, 54.6 KiB gzip). The isolated 3,000-row scoped SQL query measured 3.082 ms; neither measure establishes hosted throughput or VM capacity. No dependency change; only the changed runtime codec checksum is refreshed in the deployment manifest. Existing unrelated working-tree changes are preserved.


### Live database 404 and catalog conflict recovery — 2026-11-11

Applied the existing competition-context prerequisite migration to production (`20261112021200_general_competition_context`, source `20261110225848_general_competition_context.sql`). Verified both metadata tables, all eleven competition columns, server-only append access, RLS and invoker views. Existing consent-aware discovery jobs were requeued and both completed, persisting two `ok` league snapshots and four metadata observations. Original checkpoint source timestamps were retained. This supersedes the preceding pending-schema status; source bodies, authentication material and account identifiers were not retrieved for diagnosis.

Recovery exposed duplicate-key errors (`worker_write_fact` / `23505`) on user-team catalog writes: omitted PostgREST conflict targets normally imply a primary-key merge, whereas the SQL bridge did plain inserts. Added `20261112021500_worker_catalog_upserts.sql` and the same four-line default to fresh `scripts/schema.sql`. Only the four mutable catalogs default to `id`; explicit conflicts, immutable history, account/league guards and monotonic source-time heads are preserved. Applied the repair as production `20261112021614_worker_catalog_upserts` and requeued five already-consented failed jobs. Team directory, pressroom and the financial-snapshot resource subsequently completed without errors; league discovery, roster, lineup and notification reads also have saved results. No application source redeployment is required for these SQL repairs.

Verification: isolated `node test/test_worker_schema.cjs` passed 10 groups, including default-key merges for all four catalogs, one retained row per identity, rejection of older observations, leases, durable checkpoints, revocation and browser-role restrictions. `Rscript test/test_shiny_simulation.R` passed 13 checks plus eight tabs, without HTTP/live database mutation attempts. Live verification used safe schema/job aggregates and ordinary consented worker recovery; no test accounts or manual transactions were created. Both migrations are in hosted migration history. Security function/grant checks and the migration record are in `competition_context_migration_verification.md`.

Performance review: the prerequisite rebuilds contextual keys/indexes on existing factual tables; production estimates included ~192k player-history and ~52k point rows. It completed successfully; lock duration and hosted throughput were not measured. The writer adds a constant-size catalog check and uses existing primary-key indexes, removing duplicate-insert retry work. No extra source requests, polling, reactive dependencies, UI redraws or mobile rendering work are introduced. There are no new dependencies or bundle files; migration scripts/tests/docs remain excluded from deployment. Documentation has no runtime effect.

Acceptance limits: the specific missing-schema 404 and catalog duplicate-key failures are repaired and successful affected jobs are observed. Prepared finance jobs now stop with `fact_league` (the guard rejects a foreign-league factual write); market acquisition still reports `resource_parse`. Finance descriptors have nonempty league/team IDs and matching argument leagues, so malformed empty scheduling is not the cause. Those remaining paths need nested acquisition/persistence context tracing and a controlled helper parser reproduction; reconciled balances and complete market acquisition are not certified. No guard was weakened or unavailable data replaced with zero to force success. Other unrelated pre-existing security advisories and working-tree edits are preserved.


### Football-only leagues, last selection and market codec — 2026-11-11

The reported market loading was not healthy. Production sport metadata identified one `basket` league and one `soccer` league; the failing market job belonged to soccer. A synthetic response through the actual market parser reproduced the root cause: its canonical `data.table`/`data.frame` result was rejected by the private worker codec as `Unsupported worker class`. The codec now transports that exact class as a plain factual frame without process-local internal pointers; unknown subclasses remain rejected. The real Python/R regression persists/restores the market result and restarts its helper, retaining -2 points and reusing the original checkpoint with one source HTTP request total. Recovered jobs also clear a stale paused/failure status even when their payload version is unchanged.

Basketball filtering uses source sport fields in the R live/cache path, account league selector, normalized context, worker hydration and Python live/checkpoint paths. It retains unknown sport and timestamps instead of guessing from names. New `league_preferences.R` performs one asynchronous account-scoped database read per login, delays default-league resource demand until restoration, persists the initial/changed valid choice, and bounds saves to one in flight plus the latest pending choice. Valid restoration/refresh adds no write. Delayed responses cannot change a different account or override manual selection. Failed reads expose Retry without overwriting the unknown saved choice; failed/unacknowledged writes remain retryable. An all-basketball account settles with no supported selection rather than endless market loading. Documentation/setup and deployment packaging include the new module without a dependency or environment change.

Production: automatic review initially rejected the combined dynamic permanent purge. The user explicitly approved permanent basketball cleanup. The separately rehearsed block/preference migration was applied as `20261112030711_football_leagues_and_selection`; the independent fixed-table cleanup script then completed. Live checks found one blocked ID, zero basketball championship/job/snapshot/context rows and the retained football league. Both new tables have RLS and deny anonymous/authenticated roles; server-only grants and the existing worker guard/publish isolation remain intact. Only minimal blocked ID/sport tombstones are retained to prevent reinsertion. Shared global players/clubs, football facts, authentication records and Futmondo league membership were preserved. See `league_preferences.md` for exact scripts/hashes and interfaces. Application/worker rebuild/restart, hosted market success and browser session restoration remain deployment acceptance checks; the live database cleanup alone does not activate R/Python/UI changes in an older container.

Executed checks: league preference/sport tests passed 8 groups, including no wrong-default demand while a read is delayed, manual/account races, cross-session restoration, coalescing, acknowledgements and all-basketball fallback. Worker codec passed 20 groups; Python suite passed 17 tests, including real market persistence/restart reuse and live/legacy basketball checkpoint filtering. Worker provider passed 7 groups with zero source HTTP, including recovered-status clearing without a payload download. Existing competition context passed 14 groups. Isolated football policy SQL passed 6 groups, including block-only preservation, approved cleanup of exact basketball records, old-session reinsertion denial, old discovery publication guards, durable/latest preferences and denied browser access. Existing worker SQL passed 10 groups through the full migration order. The final offline lifecycle passed 13 checks plus eight tabs with no real HTTP/live database mutation attempts. Deployment packaging passed 3 checks. All automated acquisition/account fixtures were synthetic; production work was the explicitly approved policy/cleanup, not a test transaction or account enrollment.

Performance review: preferences add one bounded async database read per login, bounded timestamped selection writes, no polling/timer, and no source HTTP. Restoring a valid choice avoids an initial fetch/redraw for another league. Existing durable resource/checkpoint reuse and source-age semantics remain unchanged. Sport filtering traverses small source league lists; Python uses a competition lookup. Policy probes use the blocked-ID primary key; the final isolated run inserted 1,000 guarded football history rows in 54.3 ms. Existing 5,001-entry ledger restore measured 0.141 s. The market frame conversion runs in the existing private helper under the same row/byte budgets. These measurements exclude hosted latency and do not size VM capacity. No new fixed-width controls or recurring redraws were added. Existing unrelated security advisories and working-tree edits are preserved; no dependency change required manifest regeneration, only scoped source checksums and the new module entry.

### Long-loading finance worker: context ownership and durable previews — 2026-11-11

Production metadata confirmed two concurrently leased finance jobs for the same account/league/team: one unresolved competition/season/scoring scope and one resolved current scope. The unresolved job continued acquiring/replaying over 100 responses without publishing a financial resource. Source inspection also showed that the private helper discarded the accounting controller's intermediate updates and the parent publisher waited for all child snapshots. A later old run reported generic `resource_parse`; its exact private exception is unavailable, so it is not claimed to be proven solely by that code.

Added current-finance context matching to admission/claim/lease guards and a serving-process wait for known competition/season before requesting the job. Applied production migration `20261112035051_resolved_finance_jobs`; the unresolved job became inactive with `context_unresolved`, and all 131 source-response checkpoints were retained. No archive or fact was deleted, recontextualized or silently merged with the current season. The new predicate remains invoker/server-only; the resolved job continued. Existing sport/account/generation/leader guards remain.

The accounting controller now exposes an optional internal `on_update` callback, used only by the private headless helper. The actual progress sender preserves the existing ledger/calculation values, emits only usable finite results without a calculation error, and forces partial/loading state. Python durably publishes the same scoped resource through the existing child-publication RPC without settling the job. It requires acknowledged chunks/manifest, limits ordinary writes to 15-second intervals, allows one prepared preview before bulk child persistence, and keeps source timestamps unchanged. Main-process stale reads do not requeue an already-running resource; explicit manual Refresh still does. Final publication alone completes the job. Saved final Best XI/MVP selections are reused for one day only when validated/scoped/acknowledged and not flagged by later settlement. Authoritative round closure is retained at the helper parser boundary. Unknown/provisional/malformed/foreign/expired award data still acquires and persists. Actual controller timeouts now have a fixed safe `finance_timeout` code, without exposing arbitrary exceptions.

Verification: actual finance controller/helper tests passed five groups, including the independent ledger 100 opening cash minus a 10 Market purchase equals 90, early usable calculations, real progress/ready envelopes and required award rechecks. Python worker suite passed 20 tests, including partial durability/cadence, acknowledgement failures/retry and progress-before-final protocol. Provider suite passed 8 groups with zero source HTTP, covering suppressed automatic requeue of running work and preserved explicit refresh. Isolated finance SQL passed 6 groups: current-only execution, archived checkpoint/fact preservation, partial snapshot visibility while lease remains active, required chunks, final-only job completion, truthful older-input final publication and protection of newer independent child observations. The additional additive publication migration was applied as `20261112041332_finance_progress_publication`: snapshots record their publishing run, allowing an active finance aggregate to replace its preview even when its source timestamp moves backward. Source age remains unchanged; independent child evidence keeps its newer-source guard. Existing worker SQL passed 10 groups through the final migration order. Codec regression passed 20 groups. Offline lifecycle passed 13 checks plus eight tabs without HTTP/live database mutation attempts. Deployment packaging passed 3 checks. All acquisition/account fixtures were synthetic; production work was the authorized context-gate repair of existing factual jobs.

Performance review: eliminates a duplicate full-league acquisition/replay and redundant final-award reads without increasing source limits, lanes or polling. The predicate compares JSON on the account row already read/joined by queue functions. Preview persistence adds up to four ordinary bounded chunk/manifest publications per minute plus one ready preview per job; unchanged output is suppressed, existing source-age/row/byte/storage-cleanup contracts remain, and encoding/DB waits run in the private helper. Ordinary UI callers keep the NULL callback and existing ledger calculations/reactivity. The existing 5,001-entry ledger restore measured 0.190 s locally. Hosted CPU/storage/throughput remain unmeasured; no VM capacity claim is made. No dependency or environment variable changed; only affected runtime checksums are refreshed. Full historical finance completion and reconciled production balances still require post-auto-deployment acceptance; partial progress is not marked complete. Existing unrelated working-tree changes are preserved. See `finance_worker_progress.md` for interfaces, payloads, exact migration hash and limits.


### Finance deployment acceptance and cold-helper archive reuse — 2026-11-11

After automatic deployment, production saved a partial finance resource at `2026-11-12 04:30:36 UTC`. This establishes actual progress publication: the private sender only emits values containing a finite calculated balance without a calculation error, with partial/loading coverage. The full historical scan and final reconciliation remain unverified; neither the partial manifest nor an active lease is treated as final success.

Cold-start review then identified two parser/reuse defects: helper identity defaults depended on absent process-local league metadata despite a verified job descriptor; and a genuine empty DB row array decoded as an R list, failing the pagination contract. The private helper now scopes missing identity defaults to its exact admitted account/league context without creating source metadata/timestamps or changing explicit unknown/foreign requests. It rejects cached award catalogs with a mismatched season and reacquires them. Successful empty arrays become empty data frames; missing/null/malformed envelopes still fail. Nonempty database arrays must decode to frames. Historical facts and checkpoint contexts are not relabeled or deleted.

Verification: the focused cold-helper regression passed five groups covering actual known-season/competition DB query construction, explicit unknown/foreign isolation, no fabricated metadata, actual wrong-season cache reacquisition and malformed reply failure. The actual finance/controller/helper suite passed five groups, retaining the independent 100 minus 10 equals 90 ledger, early output, award reuse/rechecks and progress envelopes. Python suite passed 20 tests. The preceding full offline lifecycle passed 13 checks/eight tabs for the serving-process/shared-controller changes; this follow-up touches only the private helper and has no serving-process/DOM/reactive change. No real-account test transaction, source body or authentication secret was used or printed.

Performance review: scalar identity fallback and constant-time empty-array normalization avoid wrong-archive misses/parser retries; no extra HTTP/database request, worker lane, polling, timer, dependency or environment change is introduced. Valid persisted evidence remains the first source, with existing context/age/byte bounds. Only inconsistent cached award catalogs are reloaded. Helper changes are Docker assets excluded from the R bundle, so no R manifest checksum changes are required for this follow-up. No new capacity claim or complete-production accounting claim is made. Existing unrelated working-tree changes are preserved.

### Safe finance timeout diagnostics — 2026-11-12

Owner-provided Dokploy logs confirm `worker_job code=finance_timeout lane=0` at 08:00:12 UTC. Normal R package masking/startup output does not identify a finance cause. Automatic approval review rejected exporting the compressed production finance snapshot despite the owner's approval, and no direct local DB connection is configured. No private snapshot was written or decoded. The underlying completion defect remains open.

The private R helper now emits only fixed queue/status labels and bounded integer counters on its existing actual timeout. Python strictly validates the entire ASCII grammar before existing logging and leased job-error persistence; arbitrary exception messages, appended strings, source IDs, account/league IDs, credentials and private response/ledger bodies are excluded. This enables the next production failure to identify the pending queue without snapshot extraction. No source times, cached accounting data, acquisition checks, timeout budget, retry schedule or completion claims change.

Verification passed: new R timeout regression three groups (actual timeout branch with synthetic clock, known-answer queue counts, privacy/bounds); Python 21 tests including exact diagnostic allowlisting and malformed/private suffix rejection; actual headless accounting/progress five groups retaining 100-10=90; offline lifecycle 13 checks and eight tabs with no real HTTP/database mutations. The first Python invocation used an invalid unittest module path and failed to import; the corrected project-root invocation passed all 21 tests. Scoped diff whitespace checks passed.

Performance review: one pass through the existing bounded queue only when timeout already occurs; no extra HTTP/database request/write, polling, timers, retained response data, Shiny redraws or serving-process work. Worker assets are excluded from the R manifest; no dependencies/configuration changes. Production acceptance requires the next image and a resulting timeout code; successful completion and reconciliation remain unverified. Preserve unrelated working-tree changes.

### Historical lineup backlog, persistence sizing and task encoding — 2026-11-12

Owner logs identified seed ready, two reads active, ten teams ready, one team repair and 42 queued historical lineups; later read-only production metadata showed the same team-repair/backlog pattern with 74 lineups. Scoped archive counts showed team histories but no saved current-season historical lineups. Supabase health advisories were empty and a bounded error-log query returned no rows; neither proves source/DB completeness. No private production snapshot/body/credential was exported or decoded.

Reproduced a concrete persistence failure: a detailed synthetic lineup expanded above 752 KiB in R but was only 80,867 JSON bytes; the prior R-object-size gate dropped it before any write. The gate now measures JSON bytes in the existing acknowledged batches of 50. This avoids synchronously serializing thousands of rows before returning: the 5,001-record synthetic admission call measured 1.969 seconds when validation was performed up front, then 0.442 seconds after validation moved into bounded batches. Continuation still waits for the write callback; source timestamps, full raw lineup details, context filters and the 256 KiB wire bound remain intact. Oversized/foreign rows are still rejected, and failed/absent persistence acknowledgements are not durable success.

Reproduced a scheduling defect in the prior controller: a point-history team repair was after the lineup batch. Repairs now prepend, normal lineup scheduling waits for team reads/repairs, and a generation-scoped pending-lineup counter permits one final accounting rebuild after the batch instead of recalculating each slow arrival while a repair remains pending. Interim results retain their existing loading/missing-evidence labels. Superseded lineup signatures, negative points, the cash ledger and unknown-versus-zero rules are unchanged. Private finance task envelopes no longer create additional typed shared snapshots; HTTP checkpoints and normalized DB evidence remain durable/reusable, and normal display getters retain snapshot publication. Warning logs add only a validated handler name for attribution of future generic parse errors. Historic parse warnings remain unattributed; `access_revoked` alone does not establish a source-token rejection. Current account metadata reported enabled/active; admission/lease/auth guards remain unchanged.

Executed verification: detailed evidence/byte-bound/account-isolation/acknowledged batching/private-envelope regression five groups; real controller ten-team/twelve-round regression saves 120 lineups with three accounting builds, confirms all repairs precede lineups, then a fresh controller restores evidence with zero new lineup reads and identical balances. A prior-controller replay had five builds and failed the repair-before-lineups condition. Local real private-helper bridge with 120 roughly 80 KiB synthetic detailed lineup HTTP replies completed in 168.69 seconds, loading false, all 120 evidence writes acknowledged (synthetic DB/HTTP transport; not hosted throughput). Existing accounting/progress five groups including independent 100-10=90; timeout three; cold context five; lineup race three; historical reconstruction 14 plus tie fixture; cash reconciliation 16; persistent cache six; ownership ten; classification lineup 17; Python 21; offline lifecycle 13 plus eight tabs; deployment contract three passed. An initial extra cache invocation used a nonexistent filename, then the correct existing persistent-cache suite passed. Test transport used no real account/HTTP/database mutation. Dependency manifest regenerated/inspected; staged checksum only for changed finance runtime, preserving unrelated entries. Private screenshot attachments found in the regenerated local manifest were excluded via `.rscignore`; Docker already excludes them.

Performance review: no added source requests, worker lanes, polling, regular timers or UI redraws. Existing queue/batch bounds remain; scalar lineup counters reset with generations and decremented on success/rejection. JSON byte validation adds serialization for actual writes, distributed across acknowledged bounded batches. Rich histories still expand in R and have material memory/serialization costs; avoiding redundant task snapshots and reusing DB evidence reduces repeated work. The rich-fixture completion and fast-fixture build counts do not certify production VM capacity or final historical reconciliation. No dependency, environment, schema, database migration, timeout extension or source-freshness change. Production acceptance requires the next automatic image, current-season lineup persistence and a successful final finance publication; the full production defect remains open until verified.

### Exact-identity historical lineup archive reuse — 2026-11-12

Production acceptance of `854e630`: new worker heartbeat/request counters observed after deployment; finance started at 13:39:09 UTC and still timed out, now with team0, 48 lineups and one notification page queued. No current-season lineup record was saved during that run. The queue/persistence regressions were fixed locally, but that run did not establish final production recovery. Captured local HARs contained team-round calls but no historical round-lineup response; no response contract was invented from them.

A read-only identity join established 99 unresolved archived lineup records matching current authoritative team/opaque-round IDs in the same account/league; all had signatures, boolean eligibility, closed labels and populated ownership arrays. Added a bounded, account/league-only archive candidate lookup for missing known-context lineup pairs, plus a pure exact-revision validator and controller reuse/persistence. Candidates stay separate until current competition/season, identity, closure, full round/award signature, valid source timestamp and ownership/eligibility shape match. Known conflicting context, unknown current context, foreign identities, missing/open/ambiguous rounds, changed revisions, tied contradictory observations and invalid rows cannot resolve. Newer archived corrections cannot fall back to older matching records; existing known rows are never overwritten. Original records/timestamps remain untouched; new scoped rows retain source time and record resolution provenance. Missing records still follow ordinary source acquisition, never zero-filling or bypassing checks.

Executed checks: new five-group archive regression, including actual controller acknowledged-path invocation with no source lineup request and independent 100 + 1 + 5 + 10 = 116 ledger; 120-lineup batch/restart reuse (three builds, no repeated lineup acquisition on fresh controller); revision race three; historical reconstruction 14 plus tie fixture; finance/progress five; cold context five; offline lifecycle 13/eight tabs passed. All checks use synthetic data, with no real account/API/database mutation. Regenerated/inspected the R manifest; only the changed runtime checksum is staged, preserving unrelated working-tree changes. Prior full Python 21 tests apply to the unchanged helper/Python protocol.

Performance review: only missing verified-context pairs trigger the extra DB scan (at most 600 rows, pages of 50), with no source request, timer, lane or autonomous action added. Same-source/time groups avoid a quadratic conflict scan. Accepted records persist through the existing retry/idempotency path and suppress redundant lineup reads; only the affected saved-evidence memory key is invalidated so subsequent controllers reuse DB facts. Rich archive memory and hosted latency remain limitations; no VM capacity claim. No schema, dependency, credential, timeout, source-time or freshness relaxation. Automatic follow-up deployment, actual signature acceptance/current-context inserts and successful final finance publication remain production acceptance checks.

### Production finance completion; direct private finance-row decoding — 2026-11-12

Read-only live acceptance: 108 current-context historical lineups are stored, including 24 exact-verified archive recoveries. Across 74 records available in both contexts, all ownership sets agreed (ignoring list order), and 73 revision signatures agreed. No private payload was printed/exported for these checks. The finance snapshot was published `ok` at 14:10:53 UTC and the job completed at 14:11:26 UTC, with last_error NULL and attempts zero. The controller's final-result guard establishes loading false. This resolves the demonstrated indefinite-loading failure; snapshot completeness remains `unknown` and full production balance/private-adjustment reconciliation is not certified or relabeled complete.

Added `fm_worker_database_rows(table, rows)` to the private helper. Known finance-view text columns form an ordinary data frame directly and nested `payload` stays exact as a list column. This avoids serializing and parsing an entire rich saved history again, and avoids unnecessary heterogeneous nested-frame expansion. Existing finance decoders already accept the list-column shape. Unknown projections/other tables retain the generic decoder; real empty arrays remain empty frames and malformed bridge arrays remain failures. No query, database/HTTP request, persistence operation, source timestamp, freshness deadline, context axis, confidence or accounting amount changes.

Verification: new decoder regression three contract groups plus local comparison of ten rich records: direct 0.022 seconds versus prior JSON round trip 3.582 seconds. Tests cover exact deep payload/empty-array preservation, context/time/projected fields, outer frames, invalid envelopes and other-table negative/missing values. Cold-context five, accounting/progress five, detailed evidence five, archive five and Python 21 tests pass. Offline lifecycle 13 checks/eight tabs passes. Fixtures remain synthetic with no real-account/API/database mutation. No dependencies, schema, environment, serving R file or R manifest checksum change. Helper assets remain outside the R bundle and are copied by Docker.

Performance review: linear outer-record/field validation and column construction avoids repeated JSON materialization and deep normalization, with no extra timer, source call, DB work, queue or UI redraw. Existing batch/record bounds still apply; raw histories remain in memory when needed, so hosted memory/latency and final performance-image acceptance remain separate checks. Completion evidence predates this final performance-only helper rollout; existing source-age/completeness limitations are retained. Unrelated working-tree changes remain preserved.

### Attributed roster/ranking investigation and ranking cache recovery — 2026-11-12

Owner logs report parser failures in round rankings and team rosters, plus a finance `access_revoked` at 15:24:30 UTC. Read-only metadata shows account enabled/active, credential renewal at that same second, replacement finance activity and a completed finance run at 15:33:58 UTC. Failed roster requests refer to the financial reference team; no speculative rival permissions change was applied. Synthetic and fully anonymized captured roster/club/market response shapes pass actual private-helper parsing; no raw capture bodies, account IDs, names, financial values or secrets were printed or used as live test credentials. Historic roster exception remains unresolved until its attributed retry is observed.

Reproduced/fixed an optional saved-ranking lookup failure contaminating a separately successful authoritative API result. `resource_optional_cache_read(read)` contains only that read's diagnostic/miss; the safe warning stays logged, awaiting async conditions propagate, and the ranking response still must validate and persist. Required API/write failures are not suppressed. The helper preserves category/HTTP status/sanitized resource on bridge errors, propagates structured parent fetch failures, and maps only fixed codec/database-shape codes or whitelisted source diagnostics. Python validates the entire grammar; arbitrary condition text/private suffixes are rejected. These diagnostics identify the next roster failure without exporting production snapshots. Top-level results also avoid an unused duplicate typed encode; reusable child publication remains unchanged.

Verification passed: three R failure/recovery/encode groups; actual Python-helper ranking failed-cache-read recovery, durable publication/restore and checkpoint reuse after helper restart; strict diagnostic-code rejection tests; Python total 23 tests; classification live/cached source checks; data-service ten; lifecycle 13/eight tabs. No real account, authenticated source request or transaction used in tests. The recovered known-answer standings preserve -2 points rather than zero-filling. Manifest regenerated and inspected; staged checksums only for data_service.R and futmondo_functions.R.

Performance review: a failed optional cache lookup can recover using the same existing authoritative source query rather than force another whole job retry. Memory cache hits remain unchanged and no extra successful-path lookup/poll/timer/lane is introduced. Skipping duplicate top-level encoding reduces JSON/typed-node work; fixed error classification occurs only on failures. No source freshness, authorization, context, manual-action gate, dependency, schema or environment change. The ranking recovery is isolated-verified; production roster cause, banner offer freshness and final deployment/retry acceptance remain open, with truthful unavailable states preserved. Unrelated changes are preserved.

### 2026-11-12 — Active worker replay and native rejection handling

Reproduced an active resource losing polling interest after two minutes and shared-resource early-return/memory-cache guards suppressing a later durable worker revision. Added bounded per-view current-context subscriptions and replay revisions, without source invalidation or duplicate foreground acquisition. Source-rejection regression also reproduces an HTTP-200 native error being checkpointed as successful data; the worker now rejects it and skips such legacy checkpoints. Hard context/revocation failures reach the owner lane unchanged.

Verification: Python worker suite 27 tests passed; `test_worker_live_updates.R` four groups passed (real factual adapter/shared resource renders 100→200 after aging demand 300 seconds, no enqueue/source HTTP, inactive sibling/browser isolation, final detach and bounded storage). Worker provider eight groups passed with zero source HTTP; data service ten passed; classification live fixture passed; offline Shiny lifecycle 13 checks plus eight tabs passed. `rsconnect::writeManifest()` regenerated and dependency list inspected (78 packages, no new dependency).

Performance review: existing coalesced five-second DB poll retained, changed versions alone replay; fresh memory entries are reused only for the matching worker revision. No additional source requests on a revision event. Subscription storage bounded to 128×128 callbacks per account and released by browser detach; inactive views do not request payloads. Production heap/latency under multi-account load has not been measured.

Production limitation: before this deployment the current own-roster job still last succeeded at 04:59 UTC and reports `resource_parse_unknown_roster`; round 13 ranking reports `resource_parse_schema_classification_ranking`. Synthetic valid and anonymized captured roster shapes succeed; a synthetic native rejection reproduces the roster error, but the actual production response body has not been inspected. Neither upstream acquisition defect is declared resolved by this local regression fix. A newer completed finance snapshot does not certify complete roster/round-ranking data. Verify post-deploy retries and original source timestamps before claiming recovery.

### 2026-11-12 — Official future calendar and round standings

Safe production metadata confirms round 13's complete official fixture calendar starts 2026-11-20 20:00 UTC. Added a scoped, freshness-checked persisted-calendar guard before requesting standings, after reuse of any fresh actual ranking. Compact `next` labels alone remain insufficient; explicit running/final states prevent the future shortcut. Unknown contexts still require acquisition/context resolution, and source rejection is not translated into empty success.

`test_classification_future_calendar.R` passed: fully dated future round, preserved source timestamp, database JSON and cleared-memory reuse with forbidden HTTP, foreign context, expired/overlong validity, conflicting newer calendar, malformed/undated fixture evidence, running/final round and wrong round identity. Classification live fixture and three worker failure groups passed; offline lifecycle 13 checks plus eight tabs passed. Manifest regenerated/inspected, no dependency change. Performance: uses the evidence lookup the getter already performs, adds only a bounded local scan, no extra DB/API calls; a valid upcoming-round result avoids a futile source request and expires no later than kickoff.

Post-deploy check for commit 68d1493: worker heartbeat is live, but the own-roster retry still fails (`resource_parse_unknown_roster`), and finance/team-info tasks report explicit `source_rejected`. This confirms the remaining acquisition defect is not fixed merely by the UI replay change. Reading/exporting compressed production response snapshots was rejected by automatic approval review; no workaround or raw private response export was used. Current source response shape remains unverified; retrieval success, acknowledged persistence and browser rendering remain required production exit criteria.

### Cached banner finance verification — 2026-11-12

- Root cause reproduced: `round_finance_display()` required fresh market/team states even for display calculations, discarding complete expired database snapshots while retaining cached cash. Reused the existing durable snapshot data without renewing source time or permitting stale solvency/preflight verification.
- `Rscript test/test_debt_and_round_countdown.R`: 14 checks passed; added cached nonempty/empty/malformed/partial cases, duplicate IDs, mixed fresh/stale evidence, greater-reservation arithmetic, configured borrowing and API cap. Updated an obsolete server-clock assertion to the existing browser countdown contract.
- `Rscript test/test_banner_finance.R`: 14 checks passed; actual server rendering from typed/compressed expired database snapshot envelopes retains original observation time and labels every cached amount; delayed/failing reads, targeted retry, shared requests, no clock/navigation finance requests and context clearing are covered. Updated its synthetic async seam to forward the current `on_progress` interface; rich market fixtures satisfy the real UI schema. No live HTTP attempted.
- `Rscript test/test_configured_acquisition_rules.R`: 6 checks passed, including real manual bid dialog/preflight checks. `Rscript test/test_worker_live_updates.R`: all 4 groups passed, including version replay without source HTTP or enqueue, shared interests and bounded cleanup. `Rscript test/test_shiny_simulation.R`: 13 checks plus 8 tabs passed, with no HTTP/live database mutation attempts.
- Broader optional `Rscript test/test_worker_contract.R` stopped at its encryption fixture because this local environment lacks R package `sodium`; its full suite did not pass. The changed display's compressed database hydration and source-age behavior are independently covered by the banner regression. No dependency changes were made.
- Performance review: unchanged linear market scan/deduplication and scalar calculations; no added API/DB requests, polling, timers, retained payloads or reactive dependencies. Existing responsive wrapping styles are reused. Production payload inspection, deployed mobile-browser acceptance and deployment are not claimed. Existing successful database snapshot persistence/reuse and refresh behavior remain intact; missing factual fields are never filled with invented values.

### Worker-backed 0/527 round-history recovery — 2026-11-12

- Baseline reproduction: the unchanged controller generated two `FM-UNKNOWN` errors for expired successful lineup/catalog worker snapshots, blocked the database matrix and failed the 527-completed assertion. The fix reuses context-compatible factual evidence and subscribes to affected stored versions instead of converting expiry into source failure.
- `Rscript test/test_round_history_worker_replay.R`: all 6 scenario groups passed. Real typed/compressed database envelopes complete 527 players from 526 retained histories plus one expired sparse source snapshot; original check time, zero/-2 scores, corrected score publication, failure acknowledgement, idle read counts, unchanged-metadata reuse, inactive interests and source-context guards are checked. A real rejected initial waiter remains truthful and later recovers from a new stored version without Refresh. The synthetic first replay measured about 5.3 seconds CPU/wall locally with one initial dataset read, one targeted player acquisition and one later stored-score replay; repeated identical scoring adds zero dataset reads and source HTTP attempts are zero. This is not hosted source/database latency or a production mobile benchmark.
- `Rscript test/test_recent_points_async.R`: 27 checks passed, including delayed-response heartbeat, durable sparse/empty reuse, account/context cleanup and 527-player x38-round navigation without requests (about 2.7 seconds local controller exercise). `Rscript test/test_round_history_recovery.R`: all scenarios passed, including exact 527 recovery, bounded source retries, delayed persistence, permanent authorization failure and stale database diagnostics. Its final synthetic 512-reused/15-acquired exercise measured about 7.0 seconds, with 17 logical reads, 16 adapter calls, 3 publications and zero warm-navigation reads/publications.
- `Rscript test/test_recent_points_missing_score.R`: all 3 groups passed, including real mocked HTTP normalization and ordered observation-before-marker persistence for 0/-2/unknown. `Rscript test/test_competition_history.R`: 15 checks passed. `Rscript test/test_shiny_simulation.R`: 13 checks plus 8 tabs passed, without HTTP or live database mutation attempts.
- Performance review: replay observes at most 128 retained resource interests per controller entry (within the existing two-entry/50,000-score bounds), uses existing worker polling, and preserves the two acquisition slots, save backpressure and publication throttle. An affected source version may add a targeted stored-result replay and normalized persistence; unchanged bootstrap metadata avoids a full-matrix DB reread. No new timers, polling loops, autonomous transactions, dependencies or migration. Existing worker-wide bounds still limit active subscriptions; this is not an unbounded subscription to every historical player. The visible data remains available while asynchronous replay settles; rendering/CSS structure is unchanged on mobile.
- Limits: no live private responses or production database contents were accessed. The screenshot's exact reference numbers are session-specific; the matching baseline failure is established locally, not attributed to a captured production response. Deployment and live round-history completeness must still be confirmed after the commit deploys.

### Authorized roster checkpoint repair — 2026-11-13

- After explicit owner authorization, privately inspected the failing factual
  checkpoints and reproduced `resource_parse_unknown_roster` offline. Root cause:
  nested listing bids expanded into multiple columns before positional renaming,
  so an integer bid field became `id`. The actual join then rejected character
  roster IDs against integer listing IDs. Corrected the field mapping itself;
  no error suppression, fabricated values or acquisition bypass.
- Independent offline source comparison recovers 20 roster/15 listing rows with
  exact source IDs, names, values/prices and points. Private captures are outside
  Git; committed regression fixtures are entirely synthetic. No live Futmondo
  requests, real accounts or transactions were used in tests.
- `Rscript test/test_player_nested_fields.R`: 5 groups passed, including zero/one/
  multiple nested bids, sorted JSON fields, unknown/zero/-2 points, leading-zero
  IDs, real roster/listing joins and compressed typed snapshot reuse after
  clearing ordinary caches. Four cold roster reads, zero warm/restart source
  reads. Synthetic 527-player parser: 0.054 s, 736,136 retained bytes locally.
- Python `test_worker.WorkerTests`: 22 tests passed. New test uses the real R
  helper to acquire, acknowledge durable publication and restart with saved
  source checkpoints, preserving exact rows with no additional source requests.
- Existing R checks passed: player availability (11), rating inputs (4), request
  contracts (4), incoming source recovery (6), incoming clauses (20), outgoing
  clauses (24), HAR response contracts (5), manual market listing actions (4),
  and `test/test_shiny_simulation.R` (13 checks plus eight rendered tabs).
- Performance review: bounded per-player field traversal replaces expanding
  data-frame construction; retains nested facts once per row. No added source/
  database requests, timers, queues, subscriptions or redraw triggers. Existing
  bounded caches and acknowledged database snapshot persistence/reuse remain;
  source age and partial/stale states are preserved. No dependencies/schema or
  UI/CSS changes. Mobile uses the existing layout; hosted browser timing was
  not measured. Production deployment/fresh roster and transaction acceptance,
  plus the independent source-rejection failures, remain separate checks.

### Catalog discovery blocked acquisition lanes — 2026-11-13

- Read-only production metadata at 04:34–04:36 UTC: healthy worker, 14 completed
  jobs, no live job leases, and catalog publication at 04:32:29 followed by no
  further claims. Source inspection identified the post-publication loop issuing
  one RPC for each of 2,108 discoveries per 527-player catalog. Both lanes can
  enter this loop; pending roster/pressroom reads then expire or time out. These
  are additional defects after the independently reproduced roster mapping fix.
- Replaced the loop with acknowledged batches of 128 descriptors/512 KiB; the
  SQL RPC enforces 128/1 MiB, leader/current generation, consent/authentication,
  account context, interval bounds and existing league/finance checks. Unchanged
  jobs are reused without row writes; foreground requests/leases are retained.
  No acquisition check, factual timestamp or unknown value is bypassed.
- `node test/test_worker_discovery_batches.cjs`: 5 groups passed, covering exact
  durable identities, repeat batches with unchanged physical rows, preserved
  priority/leases, tighter intervals, byte/count/shape limits, transactional
  foreign-context rejection, generation/leader/consent/exclusion guards and
  browser denial. 2,108 cold discoveries used 17 calls, 1,287.8 ms local PGlite
  SQL; no hosted end-to-end speedup is inferred from that measurement.
- Python `test_worker.WorkerTests`: 25 tests passed, including count/byte bounds,
  descriptor immutability, acknowledged failure propagation and actual R-helper
  acquisition/publication/restart reuse. Existing SQL queue suite: 10 groups
  passed. Offline Shiny lifecycle: 13 checks plus eight rendered tabs passed,
  with no live source requests or database mutations by tests.
- Production applied the exact additive source migration as
  `20261113043948_worker_discovery_batches`. Verified `SECURITY INVOKER`, only
  service-role execution, and empty guarded acknowledgement 0 without queue
  mutations. Health advisory returned no findings. Existing informational RLS
  no-policy findings match the server-only model; unrelated pre-existing
  GraphQL/function-exposure advisories were not changed by this task (see
  https://supabase.com/docs/guides/database/database-linter?lint=0026_pg_graphql_anon_table_exposed
  and https://supabase.com/docs/guides/database/database-linter?lint=0028_anon_security_definer_function_executable).
- Performance: removes 2,091 remote round trips per catalog and avoids rewriting
  unchanged rows; each batch retains references to at most 128 already-returned
  descriptors. Existing R/UI polling, source RPS/connections, cache limits,
  durable observations and manual actions are unchanged. Account locks last for
  a bounded SQL batch rather than a whole catalog. No runtime dependencies,
  environments or UI/CSS changed; mobile layout and browser rendering are
  unchanged. Actual hosted freshness and source capacity still need acceptance
  after the image rolls out; pending stale data remains explicitly unverified.

### Discovery repair production acceptance evidence — 2026-11-13 04:46 UTC

- The new image restarted the worker; the new RPC appeared in production SQL
  statistics, 22 calls across five normalized statement forms (including the
  empty validation statement). Maximum recorded server execution was 120.309 ms;
  this excludes network latency and is not a full source-refresh benchmark.
- The parser repair is also confirmed on the formerly failing acquisition path:
  production roster jobs now complete with `last_error=NULL` and publish `ok`
  source snapshots. Source timestamps retain their actual check times.
- Queued existing full-roster and pressroom descriptors through `worker_enqueue`
  at foreground priority for the already-enabled active account. These are
  factual acquisition requests, not trades, new consent or fabricated cache
  writes. The worker performed normal source retrieval and persistence.
- At 04:46:37 UTC one roster/transaction pair was simultaneously fresh and `ok`:
  source checks 04:45:49 / 04:45:52, acknowledged job completion 04:46:00 /
  04:46:35, both job errors cleared. Other queued contexts were still acquiring.
  This confirms recovered durable prerequisites; it does not certify the live
  authenticated browser's final rendering, continuous freshness under load or
  unrelated `source_rejected`/schema messages. Private source responses were
  not retrieved for this follow-up. Verification notes only; no runtime effect
  and no further performance tests required for this documentation update.

### Continuing freshness failure: scheduling and durable fact acknowledgements — 2026-11-13

- The owner's 05:23 screenshot remained stale after isolated prior successes.
  Production metadata showed finance occupying acquisition capacity, historical
  points repeatedly demanding one-minute work from a five-minute summary source,
  and repeated immutable payment persistence: 93,307 revisions for 138 clause
  source events, including 3,486 revisions in the preceding hour. Historical
  records were preserved; no cleanup, fabricated balance or timestamp rewrite.
- `test/test_worker_expiry_demand.R`: 4 groups passed. Real adapter expiry joins
  a running worker without invalidating it, manual Refresh still invalidates and
  queues priority work, and a later durable version restores fresh display with
  no extra source HTTP. `test/test_worker_display_priority.cjs`: 4 groups passed,
  including foreground demand, lease/demand/deadline retention,
  long-job exclusion from the foreground lane, routine finance progress, five-
  minute normalization including legacy requests, and server-only privileges.
- `test/test_worker_payment_ack.R`: 5 groups passed. Unacknowledged writes retry,
  unchanged acknowledged payments skip revisions, typed hashes survive cleared
  caches, revised facts write/retry, and foreign/oversized facts cannot certify
  success. The real R-helper restart Python test reuses acknowledgement metadata
  from an expired stored snapshot, acquires the factual source path normally via
  existing checkpoints, retains its original source time, and performs zero
  repeat native player/transaction/auction/bid writes. Stale source rows are not
  returned by the acknowledgement-only path.
- Python `test_worker.WorkerTests`: 28 passed. New finance error tests retain
  fixed rejection conditions for R, keep authentication/revocation fatal, mark
  the finance parent partial, and isolate unrelated successful child resources.
  `test/test_worker_optional_finance.R` passes: actual optional getter creates no
  fake team evidence and actual accounting completes ledgers 90/110 while the
  missing private rival balance remains unknown/unreconciled.
- Existing checks passed: source expiry (7 groups), worker provider (8), live
  version replay (4), manual clause refresh (11, including the 527-player matrix),
  finance/award/progress controller, detailed lineup/evidence batching, complete/
  incremental/persisted pressroom history, locker transfers, and offline Shiny
  lifecycle (13 checks plus eight tabs). Tests used synthetic accounts and no
  real source calls or transactions. Earlier source-response captures were not
  exported for this follow-up: automatic approval review rejected the broader
  snapshot query; safe aggregate metadata and offline fixtures were used instead.
- Production scheduling migrations recorded as
  `20261113054456_worker_display_priority` and
  `20261113054649_worker_points_cadence_guard`. Verified recent-point jobs use
  300 seconds and finance demand normalizes to priority 8. These are not proof
  of sustained browser rendering or repair of rejected external endpoints.
- Performance review: adds one scoped acknowledgement metadata read on a cold
  pressroom acquisition, then suppresses acknowledged unchanged native/payment
  writes across sessions/restarts. Hashes stay within existing 20,000-row and
  worker payload/cache limits; child re-encoding is bounded by 8 MiB. No new
  timers/processes, source fan-out, VM sizing assumption or dependency. Historical
  point polling demand drops to the existing upstream five-minute cadence;
  explicit Refresh remains immediate. Existing source deadlines, stale/partial
  states, account/context guards and transaction preflights are unchanged. UI
  markup/CSS is unchanged on mobile; real hosted end-to-end latency and repeated
  freshness still require acceptance after rollout.

- Additional performance/ownership checks: a 5,001-row ledger hydrated in 0.107 s
  locally; 1,203 acknowledgement hashes used 91,587 JSON bytes / 87,000 retained
  bytes and decoded in 0.063 s. Session ownership/cancellation (3 groups) and
  deployment exclusions/dependency contract (3 groups) passed. Production
  `worker_claim`, `worker_enqueue` and priority helper are invoker functions with
  no anon/authenticated execution grants. No dependency/manifest regeneration
  was required; existing unrelated manifest edits were preserved.

### No-browser core lane correction — 2026-11-13

- The newly deployed worker was healthy, but with no active browser promotion,
  default priority-4 roster/history jobs could wait behind long routine work
  while the reserved lane stayed idle. Extended its SQL eligibility to due
  short core handlers (policy >=9, normal priority >=3), retaining all long-job
  exclusions and existing consent/context/quiet-hours/lease guards. Old idle
  recent-point priority-4 entries normalize to their new base priority 3 without
  changing manual demand, leases or source timestamps.
- `node test/test_worker_display_priority.cjs`: 5 groups passed. Ordinary
  no-browser team-info reads are claimable in the protected lane; ordinary bulk
  catalogs remain unclaimable there. Priority order, manual finance routing,
  mixed-version 300-second cadence and restricted roles still pass. This is the
  only runtime change since the previously passing full offline lifecycle;
  isolated SQL tests exercise the changed branch directly.
- Production applied `20261113055513_worker_core_read_lane` and immediately
  showed an ordinary priority-4 roster acquisition alongside routine capacity
  work (05:55:21 UTC). This is scheduling acceptance, not yet sustained freshness
  or authenticated browser rendering. No source/secret payload was retrieved.
- Performance review: one immutable urgency predicate per eligible claim,
  indexed existing due-job selection, no added process/connection/RPS cap, and
  no change to the approved collection scope. A previously idle lane can now
  service due core observations, reducing waiting at the cost of using its
  already-configured capacity. Long/bulk jobs remain in the routine lane;
  source/DB pressure stays within the existing shared account/connection limits.

### Prefetch timing and per-resource source publication — 2026-11-13

- At 05:59:55 UTC the warm pressroom jobs completed in 12.52 and 22.59 seconds
  (versus earlier 44.76/49.75 cold runs), but completion+interval scheduling still
  left expired source snapshots between cycles. Source publication also used
  the oldest source anywhere in a composite, even for an independently fresh
  child. Fixed timing and attribution without extending or inventing a source
  timestamp/deadline.
- `node test/test_worker_core_prefetch.cjs`: 4 groups passed. Independent known-
  answer delayed acquisitions complete before the prior expiry; expired output
  has bounded retry, finance timing stays unchanged, actual publication preserves
  source timestamps, early claims obey due time outside quiet hours, quiet-hour
  budgets remain, and a genuine manual refresh during a run still publishes
  stale output with immediate follow-up demand. Repeated migration is safe.
- Python worker suite: 29 tests passed. Added unrelated-old-source/fresh-child
  publication known answer and rich nested-bid restart acknowledgement coverage;
  original checkpoint observation time remains unchanged. Actual finance/award/
  progress fixture and expiry-versus-manual demand checks also passed.
- Performance review: lead time is last core-job duration+5 seconds, capped at
  half a 15–300-second interval, with a five-second scheduling floor. Expired
  output gets a bounded 7–15-second retry. Successful short core acquisition may
  approach twice its prior interval frequency; existing process/source RPS and
  connection caps remain. No extra source queries are introduced per cycle.
  Historical points and long finance/partial failures are excluded from this
  prefetch policy. Quiet-hour and explicit-demand behavior are preserved. The
  increase is offset by source-aligned points cadence and durable unchanged-fact
  reuse; hosted load and continuous UI freshness still require measurement.
- Production migration applied after one tool confirmation expired without any
  database change; the exact tested retry succeeded. The underlying external
  rejection reasons remain unverified; optional finance read isolation does not
  claim those API requests now succeed. No broad private snapshot export was used.

- Final offline lifecycle after source-time isolation: 13 checks plus eight tabs
  passed. Production recorded the prefetch migration as
  `20261113061754_worker_core_prefetch`; a preliminary expired confirmation made
  no change before the successful retry. Source-data validity was not extended.

### 2026-11-13 — Clause deadlines and resolved prerequisite peers

Checks: Python worker 30; durable clause persistence 11; incoming 20; outgoing24;
incoming source recovery six groups; SQL scope/cadence three groups; prefetch
four groups; offline lifecycle 13 plus eight tabs. All passed. SQL applied twice
offline and once in production (`20261113064338_worker_clause_scope_guards`).
Fixtures verify exact account/team/season/argument isolation, unknown-context
bootstrap, retained old jobs, mixed-version cadence enforcement, unchanged source
timestamps, fresh durable reuse and worker renewal of ageing data. Performance:
two production clause jobs have shorter cadence; existing two lanes/rate limits
bound acquisition. Indexed exact peer matching avoids redundant roster/history
work without new UI timers or memory growth. Full production latency, API source
rejections and post-redeploy transfer verification remain unverified. Details in
`docs/worker_freshness_recovery.md`.

### 2026-11-13 — Independent clause acquisition / GUI churn

Verified Python worker 30, incoming24/outgoing24, clause persistence11, worker
provider9, passive observation3, SQL cadence2, ordinary expiry4, history write
recovery11, and required lifecycle13/eight tabs. Targeted schema-diagnostic
fixture checks failed writes stay failed and private bodies/queries stay unlogged.
New delayed fixtures reproduce timestamp churn, stale prerequisites mid-request,
resumption without restart and completed-result reuse with zero extra source
requests; joint two-request cap remains. SQL idempotent replay confirms normal
five-minute scheduling and background dispatch without a browser. Production SQL
`20261113070822` confirmed four clause jobs at 300 seconds/base3. Performance:
normal aggregate 0.8 clause jobs/minute for four jobs; unchanged observations add
zero requests in fixture; no additional timer, lane, dependency or unbounded
retained data. Existing revision-based bounded snapshot reuse and DB persistence
remain. No claim of measured production latency or completed deployment.

Unresolved: production score writes succeeded while 22 completion marker writes
returned400/PGRST204. All expected DB columns/indexes exist; the offending REST
column is not exposed by available logs. Local REST schema probe was blocked by
absent server credentials. New route-specific sanitized missing-column diagnostic
will identify it after app deployment; source history is not falsely acknowledged.

### 2026-11-13 — Marker `_row` and canonical-worker handoff

Reproduced the marker defect with actual `fm_time(ISO)` output: its vector name
becomes a frame row name and jsonlite adds `_row`. Literal JSON-key regression
covers queued/direct/bridge serializers, fractional source time, negative points,
failed acknowledgement and identical source-free replay. Added actual async
adapter + compressed durable snapshot handoff, cancelled obsolete waiter,
canonical-only cache, account/season/hash rejection and route-switch checks.
SQL tests cover idempotent migration, authoritative membership/argument isolation,
version and status deduplication, bootstrap, and clause dispatch ahead of527 bulk
jobs with finance already running. Failed save Refresh retains/replays up to25
payloads/8MiB per entry and clears errors only on successful acknowledgement.

Passed Python worker30, incoming24/outgoing24, provider9, live updates4, clause
refresh11, persistence recovery11, session ownership3, deployment3 and required
lifecycle13/eight tabs. Existing527 durable replay reused526 plus one snapshot,
zero source HTTP. Seven ACKs for527x38 histories:0.234s total/0.119s worst heartbeat
gap; zero matrix rescans, fitness recalculations, extra reads or publications.
Performance: no new timers/lanes/dependencies; bounded hints and write retention;
indexed peer lookup returns at most two candidates; canonical versions avoid
repeat chunk transfer. Priority adjustment changes dispatch, not cadence.

Additional cache-boundary fixture fails future-ID eligibility on both HEAD and
changed code (verified by substituting the unchanged table implementation);
preceding acquisition/save assertions pass. This pre-existing failure is not
claimed resolved or passed. SQL deployed as20261113121612. Container deployment
and multi-cycle production transfer/history acceptance remain outstanding because
no Docker daemon or Dokploy deployment tool is connected here. See
`docs/worker_resolved_recovery.md`; no claim of production recovery yet.
Production policy inspection found the enabled account in configured quiet hours;
normal hourly floor remains respected. No settings were altered to force a test.

### 2026-11-13 — Independent clause fairness and stale cold-page display

Production normal core demand starved clause jobs; new SQL aging promotes due
clauses to ordinary core urgency after60s, then orders by oldest deadline.
Four SQL groups reproduce the old starvation and verify aging boundary, migration
replay, quiet-hour eligibility and explicit demand. Cadence and prefetch tests
also pass. SQL20261113142423 is live. All three active clause jobs refreshed at
14:24:48–14:25:03 and again14:32:03–14:32:18UTC; all were fresh at14:33:09, no errors.

Observed-only transfer mode restores dates/rows from successful durable worker
observations on cold stale pages. Capacity remains unavailable/NA with no IDs;
partial/foreign source proof fails closed. Four display groups cover two cold
sessions, preserved timestamps and zero source acquisition; four passive groups
verify expired reuse is explicit and never renews freshness. Incoming24/outgoing24
and required lifecycle13/eight tabs pass. Performance: order changes only, bounded
existing passive snapshots/small candidate set, no new timer, table, workers or
HTTP acquisition. Ordinary manual-action freshness remains unchanged.

Auto deploy after push is now documented as the standing rollout mechanism.
Static artifact modification time shortly after prior push and current heartbeat
support deployment; no exact deployed SHA was exposed. No marker POSTs in the
12:25–14:20 log window, so successful marker writes are not yet demonstrated.
Observed mobile screen recovery remains unverified until the new auto deployment.
Later production log check since14:20UTC: three score POSTs and three completion
marker POSTs all201, no PGRST204 in that window. Marker serialization recovery is
now confirmed live. Observed panels include original UTC source times; manual
capacity recheck4 and durable clause persistence11 passed. User-page rendering
still requires confirmation after the push-triggered deployment.

### 2026-11-13 — Background-rendering regression review

Reproduced two publications/renders per durable worker update: shared state
first emitted stale/loading despite the new observation already being local.
Five updates now produce exactly five renders (0.272s), preserving manual
Refresh loading/priority10 and original timestamps on expired records. Ten
unrelated resource updates produce no primary render. Banner status now
projects only changed visible content, scoped to its account:20 polls cause
zero repeated renders/source reads/enqueues (0.380s), and other accounts cannot
invalidate it. Queue errors and worker-read failures remain visible.

Browser fixtures at360/1440px use real Today cards/adapters and encoded synthetic
snapshots, with external requests blocked. Two five-second polls produce zero
banner/card redraws; a changed balance paints once. Heartbeats46/56ms, no horizontal
overflow. These measurements are local, not production latency guarantees.
Performance review: same worker schedule and DB poll frequency; one bounded
status value/session and one existing-cache lookup per replay; no new source
requests, DB writes, timers or durable cache. Existing DB snapshots remain the
cross-session authority; stale and context checks are not relaxed.

Saved-page recovery uses the existing interactive safe-read coordinator with
at most one transient retry and8s per attempt; POST remains single-attempt.
Preference4+transport4, safe-read recovery7 and transport-priority10 pass.
Foreground fixture42ms amid three400ms background requests; four-socket cap
unchanged. Production aggregate logs show one preference200 at01:19:28UTC on
Nov14, no HTTP rejection. Prior logs lack delivery/parser detail, so the exact
original preference failure remains unconfirmed. New diagnostics emit only
allowlisted resource/category/status/reference, never bodies or credentials.

Worker status4, replay4, live4, context5, provider9, expiry4, progressive refresh2,
lineup6 and clause-scope11 groups pass. Required offline lifecycle13/eight tabs
and deployment contract3 pass. No new dependencies or schema changes. Push
triggers the existing Dokploy deployment; authenticated production behavior
is not inferred from local tests. See `docs/worker_render_stability.md`.

### 2026-11-14 UTC — Database-only display / autonomous source plan

Root causes established in code and live aggregate metadata: missing/stale GUI
reads re-enqueued every30s; some families had no autonomous discovery; early
prefetch rescheduled core within5–15s; priority-driven scheduling starved history
and left the reserved slot idle despite short work. Named-empty argument lists
and inherited league scope produced different keys for account-only endpoints.
Before repair, aggregate jobs included1606 summaries,1054 redundant card-history
jobs and527 each profiles/matches overdue, while core timestamps kept advancing.
Counts span contexts and are not individual-player completion estimates.

Implemented worker->DB->GUI boundary: no GUI acquisition enqueue function/call,
Refresh is a coalesced stored-data reread, one expiry publication, passive card
version subscriptions, no duplicate history writes without durable ACK proof.
A database-only getter remains a DB reader. Explicit offer intent performs one
async live preflight with every nested prerequisite bypassing snapshots; foreign
context completions are rejected and uncertain mutations are not retried.

Source plan uses saved dependencies for discovery (zero discovery HTTP), with
summary-derived points/completion markers and clauses persisted before publishing.
Three source families per catalog player replace four. SQL persists descriptors
for read-only canonical lookup without jobs, transactionally seeds enrollment,
retires obsolete idle jobs while preserving snapshots, and gives query families
fair turns with fixed completion+interval scheduling. Short slot helps other
short queries when core is caught up; existing source connection/rate limits,
quiet hours, consent/context/lease guards and strict source deadlines remain.
Legacy capacity interval becomes300s; its original15s action deadline stays.

Verification: SQL8groups includes read-only transaction, exact R/SQL seed hash,
cold no-job child lookup, foreign/oversized rejection, acknowledged duplicate
retirement, expired source not accelerating schedule,527-backlog fairness and
quiet hours. Seven claims measured0.189–0.327s locally after fixing an initial
5.2s lookup regression with a known-context fast path. GUI7groups span two cold
sessions,12polls/session across6simulated minutes, Refresh and20 invalidations:
zero acquisition/source calls; five further expired polls cause zero redraws.
527-history replay completes with zero GUI writes/source reads and rejects a
legacy snapshot's missing durability proof. Manualoffer4/cardupdates2 pass;
worker Python31, codec20, discovery7, derivedsummary4 and related history/lineup/
finance suites pass. Required lifecycle13/eight tabs and deployment contract3
pass. Browser360/1440: two real polls zero idle redraws, changed balance onepaint,
Refresh zero acquisition jobs, heartbeat65/99ms, no horizontal overflow.

Performance review: no additional service/lane/source-rate budget; bounded
version polling and memory retained. Removes GUI-generated source/DB writes,
validity-chasing and redundant jobs. Indexed canonical lookups and grouped job
scheduling measured on synthetic queue; actual hosted latency/backlog drainage
still needs observation. No dependency/environment change. Existing Supabase
health advisories empty; unrelated legacy security/index advisories unchanged.

Rollout: Supabase migration confirmation expired twice before application; a
metadata query verified descriptor column absent and old two-argument read still
present. User confirmation requested through the connector. Hold the automatic
push/deployment until both migrations are acknowledged. This is not a production
completion claim. Exact original private responses were never exported.

Production rollout update after owner approval: both migrations succeeded as
`20261114053800` and `20261114053847`. Metadata verification confirms worker_read
has3 arguments and STABLE/SECURITY INVOKER behavior; all new/changed RPCs deny
anon execution and grant service_role. Redundant card-history jobs count0;
existing team-capacity jobs both use300s. Commit `df6b8d9` pushed to branch2.0
for the configured automatic deployment. This resolves the expired connector
confirmation blocker above. No factual history or snapshot records were removed.


### November 14: round-history progress reset and repeated market redraw

- Production aggregate diagnosis at 13:52UTC: the authoritative competition/season/scoring scope already contained 527 successful per-player checks and 6134 numbered point rows for 527 players. The low UI count did not reflect missing source acquisitions. Canonical 527 summary jobs were already checked; the 553 unchecked summary descriptors were obsolete shadowed scopes.
- Root causes: catalog fitness changes demoted saved history; the GUI still replayed individual snapshot reads with only 128 tracked interests; new database histories beyond those interests were not reread. Separately, every new source timestamp invalidated raw shared-state consumers and rebuilt unchanged cards/header. Expiring metadata also changed resource identity to unknown.
- Fixed database-mode history reuse/bulk updates, stable visible-value projections and historical identity retention. Default fresh metadata and explicit manual preflights remain strict. Added discovery-only expired identity reuse without treating stale observations as fresh source responses. No new packages, credentials, environment variables, timer, worker lane or source rate change.
- Checks: worker observation-identity 4 groups and competition context 14/adapter 3 passed; manual-offer 4 and cold-worker-context 5 passed; required `Rscript test/test_shiny_simulation.R` passed 13 checks plus 8 tabs, with no live HTTP/database mutations. Updated the adapter test bootstrap to load current worker helpers. Other focused results and rollout are recorded below.
- Performance review: known saved history is loaded once per league/session rather than through 527 per-player snapshot requests. Reads remain asynchronous and bounded at 60s while incomplete/300s settled, retain incremental correction overlap and stop for hidden views. Unchanged visible projections stop downstream DOM replacement while source/check metadata still advances. Hosted latency and the owner's signed-in mobile screen remain separate acceptance checks.

- Final focused results: history regression 10 groups, non-worker recent-points 27, wider-window 10, incremental reader 3, worker GUI read-only 7, presentation 4, Python worker 38, R discovery 7 and SQL retirement/isolation 2 passed. The production-sized reader fixture (6134 points/527 checks) uses 8 paginated reads; local client CPU was 0.043s full/0.059s incremental, excluding network/server time. Completed views poll every 300s; incomplete views at 60s. A per-entry factual-column signature reuses assembled points without repeating reconciliation for unchanged database envelopes; entry/row bounds remain in force.
- Real browser before/after at 360px and 1440px: old code made 16 redundant output replacements in 11s across banner/cards/header/transfers; updated code made zero despite new snapshot versions/timestamps at each poll. Check-time text advanced, countdown node stayed mounted and ticking, actual balance change rendered once, Refresh queued zero acquisitions; heartbeat 65ms and no horizontal overflow. Today availability 3, progressive 16 and debt/countdown 14 checks passed.
- Hosted migration `20261114140350_worker_summary_job_retirement` succeeded. Aggregate verification:1893→567 summary jobs,1340 stored summary snapshots preserved, all 527 current-context history checks retained, worker heartbeat current. The implementation push triggers automatic deployment; signed-in mobile acceptance is still unobserved.


### November 14: orange clause warnings and cached-balance meaning

- Production metadata showed successful own-roster and balance source results, about 101–158 seconds old with 60-second worker validity. Logs showed no database request failures in the sampled 15-minute window. Expiry between completion-based collection cycles was being treated as an acquisition failure by the clause display; the source observations themselves were available.
- Added an explicit observed-only display path for successful, scoped worker roster/history/summary evidence. Original timestamps, completeness checks and actual failure diagnostics are preserved. Strict reconciliation still exposes no verified capacity/action IDs from stale observations; later transactions require later summary evidence before they can be resolved.
- Financial cached labels keep their freshness meaning and now explain last-saved amounts. Actual read failures are not hidden behind routine cached wording. Observation timestamps render separately from card/panel containers, preserving the prior redraw fix. Passive GUI clause reads do not repeat worker persistence.
- Performance review: no additional source requests, acquisition jobs, timers, dependencies or database migration; bounded existing observation reads are reused. Timestamp-only changes should update text without replacing the panels. Test results follow below.

- Additional root cause reproduced: promoting an existing successful player summary from 3600s to 300s changed its interval but left its next run 50 minutes away. Migration `20261114144534_worker_shorter_cadence.sql` now applies the shortened interval from the previous completion only for successful idle jobs, with no priority escalation. Equal/slower rediscovery, active leases, unchecked work and failure backoff remain unchanged. Its guarded repair handles already-promoted current own-team summaries identified by successful exact-scope market evidence; source snapshots/deadlines are untouched.
- Verification: new observed-clause contract 6 groups, cold observed reload 4, worker observation 4, incoming 24/outgoing 24, prerequisite recovery 6, banner finances 14, debt/countdown 14 and display projections 4 passed. Ten new source clocks caused zero panel redraws, source requests or GUI writes (~0.52s total); expiry does not renew timestamps or create failure state. Known failures, partial proof, wrong context and a summary predating its transfer remain rejected. Manual-offer 4 and deployment 3 checks passed. Required offline lifecycle passed 13 checks plus 8 tabs with no HTTP/live database mutation attempts.
- Browser 360px/1440px: balance/offer and worker timestamps advance in separate text nodes; unchanged cards/header/banner/transfers have zero redundant redraws, actual balance changes once, Refresh queues zero acquisition, heartbeat 99ms, no horizontal overflow. Authenticated hosted screen acceptance remains unobserved.
- SQL cadence 7 groups and database-boundary 8 groups passed, including unchanged row version under equal/slower rediscovery, repeat-safe repair and retry/lease/scope isolation. Shortening took 3ms locally; seven fair family claims 175ms. This increases collection only where the worker had already selected a shorter interval; it adds no GUI scheduling, service slots or recurring cleanup scan.
- Production migration acknowledged as `20261114144934_worker_shorter_cadence`. Verified updated invoker function, service-only execute permission and current worker heartbeat. No data freshness or source timestamps were extended. Push-triggered application deployment follows this tested change.


### November 14: compact system-offer presentation

- `team_received_offers_panel()` renders rival/unknown bids first and system bids under one namespaced, accessible **Futmondo system** heading. Source order within each group, exact bid links, amount/gain/loss formatting, unknown amounts and truthful loading/stale/partial/empty messages remain intact. Offer-only CSS removes repeated system badges, tightens system-row padding and permits long names/amounts to wrap.
- Verification: `Rscript test/test_team_received_offers.R` passed all 8 groups, including exact rival-offer selection and context clearing. Local Chromium fixtures at 320/360/390/1440px verified nine system offers share one label, all original prices and action IDs survive, mixed rival/unknown bids stay labeled, unknown amounts stay unavailable, no empty system group appears, and long unbroken names cause no horizontal overflow. The 390px screenshot was inspected. Independent code review found no issue.
- Performance review: for the nine-offer stale-state fixture, panel height fell from 1089 to 752px at 320px, 969 to 664px at 360px, 852 to 571px at 390px, and 820 to 539px at 1440px (31–34% reduction). Descendant nodes fell from 74 to 51. Grouping is one linear partition of already-loaded rows with no additional API/DB reads, writes, cache entries, invalidation, timers or reactive subscriptions. Existing synthetic acquisition measured 3 cold reads, 0 added panel reads and 0 warm reads; durable database reuse is unchanged.
- Scope is this panel's rendering/CSS and documentation, with no shared infrastructure/startup change or dependency/environment/schema change. Checks used synthetic local data, not live accounts or transactions; authenticated hosted appearance remains unverified. Unrelated concurrent working-tree changes are excluded from the commit.


### November 14: next-round departures in Lineup and My Team

- Root cause: Lineup optimized current ownership/injury eligibility without consulting already-supported pending clause departures. It now shares one scoped transfer source with My Team and excludes observed departures at/before the next kickoff from XI and bench, including with the injury override. Unknown/partial evidence retains the draft and blocks certification. Manual selection and the final asynchronous save preflight read current evidence directly; already-saved selections remain visible for correction. Refresh Lineup retries the shared departure source through scoped invalidation without clearing the draft.
- My Team highlights affected actual roster rows in a theme-aware reddish background with a legend. Later/unknown schedules are not colored as confirmed departures; retained known dates survive incomplete verification. Sorting/filtering retain player identity, and cancellation/context clearing removes the tint. Existing ownership, counts and valuation are unchanged.
- Verification: Lineup module 23 groups, optimizer/runtime 14, snapshot loading/reuse 3, competition snapshots 9, new departure/table regression 9, outgoing reconciliation 24, observed-source contract 6, empty squad 2, existing metric colors 9, durable clause persistence 11 and deployment contract 3 passed. The snapshot-loading fixture now supplies independently known synthetic season/scoring scope, matching the existing exact-context hydration contract; unknown scope is not relaxed in production. Required offline lifecycle passed 13 checks plus 8 tabs after the final shared refresh wiring; synthetic Plotly warnings remain, with zero live HTTP/database mutations. Scoped whitespace review passed.
- Browser acceptance: actual My Team at 390px/1440px in light/dark themes passed before/exact/after-cutoff coloring, sort/search identity, context clearing, stable inner table DOM and no JavaScript errors or page overflow. Screenshots were inspected. Authenticated hosted appearance and deployment completion remain unverified.
- Performance review: the tint itself performs no API/database acquisition; metadata updates preserve the mounted table and reuse enriched roster/history. Ten timestamp-only updates took 0.114s with zero table updates or enrichment; Lineup also suppresses timestamp-only eligibility redraws. The 100-player/all-formation optimizer averaged 65.6ms over 20 local runs under concurrent tests. First Lineup entry now activates the existing roster/history prerequisites and bounded two-summary queue when evidence is missing; My Team and Lineup share it and reuse database snapshots with original context, freshness and completeness. Durable replay measured one scoped database read/zero API reads when cold and zero reads when warm; failed writes stay retryable. No dependency, schema, polling timer or new service was added. Documentation changes have no runtime cost; local timings exclude hosted latency.


### November 14: redundant Lineup warnings and false changed-settings status

- Reproduced the screenshot's same-N warning: ordinary worker expiry marked a successful persisted calendar stale, `catalog_issue()` rejected it, and `wanted()` became empty. Comparing that empty result with the saved optimization window falsely reported changed settings and removed known averages. Successful complete scoped worker observations now retain their known completed-round facts without extending source timestamps/freshness. The root supplies a state-only view of the same shared read so real source errors and context mismatches remain distinguishable from age. Explicit failures, incomplete transport evidence, invalid clocks and foreign scope remain rejected; general/manual-action acquisition checks are unchanged.
- Removed the routine departure-count explanation. One compact recalculation message now names only actual changed inputs; N changes do not also claim a calendar change, and unresolved window/departure checks do not claim a new optimization result. Existing departure exclusions, My Team tint and manual/save gates remain intact.
- Checks: Lineup module 28 groups, store diagnostics/JSON/isolation 5, runtime 14, persisted round catalog 6 and exact-window history 4 passed. Required offline lifecycle passed 13 checks plus 8 tabs, including no live HTTP/database mutation attempts; synthetic Plotly warnings remain. Actual Lineup browser checks at 390px/1440px confirmed the routine notice is absent, the departed player remains excluded, the compact N-only message fits, and there is no page overflow or JavaScript error. Screenshots were inspected.
- Performance: the state-only and data-only readers use the same root registry/normalized request key, adding no source/database acquisition. Expiry preserves the known window/averages and draft with no optimization rerun, source read, write or timestamp renewal. Scope checks are six scalar comparisons; no new timer, dependency, cache or persistence schema. Runtime's existing 100-player/all-formation benchmark averaged 42.7ms across 20 runs. Documentation has no runtime cost. Hosted signed-in appearance remains unverified.
- Separate snapshot-save investigation remains unresolved. Read-only production aggregates confirmed the table/key/grants, 12 successful GETs and zero POSTs in the sampled 24 hours, and current route contexts matching persisted competition/season/scoring. Synthetic validation/round-trip succeeds; this does not identify the actual failing stage. Automatic review rejected exporting private snapshots to temporary files; no export was written. A distinct approved in-memory, read-only replay emitted only a read/decode failure category and wrote no payload, credentials or database rows. The app now retains a safe validator/transport reason and reference while preserving verified Futmondo save status. The next failure's safe diagnostic is needed to distinguish local validation from transport admission; the persistence defect is not marked fixed or hidden.

### 2026-11-15 UTC — Compact information and error icons

- Availability, acquisition/source notes, saved-result/cache meaning, completeness,
  latest-check timestamps and errors now use closed round `i` disclosures across
  the application. Error icons are red; loading has an explicit neutral state.
  Details retain existing diagnostics and Retry buttons. Cache labels moved out
  of financial figures; no data, timestamps, missing-value semantics, permission
  rules, manual preflights or persistence acknowledgements were changed.
- Shared source clocks stay separate from their panel. Rivals' per-team timestamps
  share the full-width status panel. Plot status and chart rendering share one
  reactive result; player-chart notes moved out of Plotly annotations without
  changing the known-answer traces/axes. Browser handling covers Shiny-generated
  notifications and validation errors, including severity assigned after Shiny's
  asynchronous content insertion. Opening a disclosure does not request refresh.
- Focused checks passed: player disclosures 5, league disclosures 4, player trend
  15, received offers 8, Today availability 3, incoming/outgoing clause signings
  24 each, card-during-save 4, card loading 7, Lineup 28, login feedback 5 and
  restore visibility, notification links 5, page preferences 4, league preferences
  8, Admin settings 6, access control 9, round-lineup 17, finance header 14,
  debt/countdown 14, worker status 4, classification loading/partial bonuses,
  Best XI crests, round awards 6, rival summary 12 and plots 3. The required
  offline lifecycle passed 13 checks plus 8 tabs, including zero HTTP/live database
  mutation attempts; existing synthetic Plotly warnings remain.
- Updated obsolete assertions for collapsed chart/login/header markup. The isolated
  login fixture also needed its existing access-check mocks. The outgoing async
  fixture's fixed November 14 departure had become past due; it now uses a future
  synthetic departure while retaining fixed-date known-answer cases. An additional
  `test_classification_cold.R` assertion expecting the word `missing` fails identically
  with the pretask module; that unrelated existing fixture mismatch is not repaired
  or counted as a passing check.
- Performance: 100 offer-state renders took 0.016s with zero source/database reads.
  Repeated paired chart/status reads added zero acquisitions. Twenty successful
  worker polls/heartbeats produced zero repeated renders or source/enqueue calls
  (0.645s). No new timer, source request, worker job, database query/write, cache,
  dependency, environment setting or schema is introduced. Existing successful
  results remain durably persisted and reused with their original context and
  freshness/completeness; failed writes remain retryable. Documentation has no
  runtime impact. Browser measurements and delivery status follow below.
- Browser acceptance passed at 320/390/1440px in both light/dark themes using the
  production stylesheet and JavaScript: native click/Enter/Space, initial collapse,
  red errors versus neutral loading, long unbroken text, separate notification
  info/close targets, Shiny validation and ordinary output errors without exposed
  prefixes, explicit Retry, stable nested source-clock DOM and preserved expansion
  after `renderUI` updates. Shiny heartbeat measured 69–108ms and browser task gaps
  during a burst of DOM updates measured 11–16ms. No page overflow, JavaScript
  error, extra HTTP request, backend I/O attempt or unintended Retry occurred.
  Light/dark phone screenshots were inspected. No live account or transaction was
  used; hosted authenticated appearance and deployment completion remain unverified.
- Final isolation check exported the task-only staged source (excluding credentials,
  captures and unrelated working edits) and passed the offline lifecycle again:
  13 checks plus 8 tabs, zero HTTP/live database mutations. Player/league disclosure,
  login feedback and deployment-bundle checks also passed on that exact source.
  Existing unrelated source edits and manifest changes remain outside this commit.


### 2026-11-15 UTC — Lineup app-copy and round-calendar root causes

- The approved limited in-memory inspection confirmed successful complete calendar parser proof paired with `unknown` worker-envelope completeness. It also confirmed an empty lineup `pointSystem` with custom configuration matching authoritative league scoring metadata and current scope. Older contexts remain distinct. These completeness/scoring comparisons establish the two failing mappings without retaining private payloads or request logs. No production mutations or source transactions were performed.
- `get_finished_rounds()` now supplies the shared completeness attribute consumed by worker persistence. Strict legacy hydration repairs only the missing metadata mapping from saved complete parser proof, valid bounded unique round rows, successful transport and a valid observation clock. Existing database snapshots become reusable without refetching, rewriting, changing context or renewing source time/expiry. Actual partial, error and malformed calendars retain their red diagnostics.
- The source's omitted scoring selector now resolves only from complete exact-scope authoritative metadata and matching custom-configuration fingerprints. Explicit source settings are not overridden. Independently known current identity also takes precedence over older shared history. Successful normalized bundles reach the existing database write, while foreign scope and failed writes remain rejected/retryable.
- Worker answer extraction previously discarded source metadata and the bundle stamped replay as newly observed. It now retains original worker clocks/status and uses the oldest observation/earliest absolute deadline across four components. Stale bundles stay stale after storage, and replay cannot renew league scoring metadata. Fresh durable reuse performs one scoped database GET and zero additional component reads or writes. The existing explicit live manual-action preflight bypass remains intact.
- Focused checks passed: new snapshot-context regression 10 groups, new calendar producer/encode/restore regression 8, worker observation identity 4, round catalog 6, Lineup module 28, loading/reuse 3, snapshot store 5, competition snapshots 9, runtime 14 and manual worker prerequisites 4. Independent review reran the context fixture and found no blocking issue. Known-answer checks retain zero and negative points and keep unknowns distinct. Required offline lifecycle passed 13 checks plus 8 tabs, with zero HTTP/live database mutation attempts; existing synthetic Plotly warnings remain. The broader worker contract suite stops at the local environment's missing `sodium` package, so it is not claimed as passing; this change adds no dependency.
- Browser fixtures reproduce both exact screenshot errors using the previous source and verify the corrected real module at 390/1440px in light/dark themes. Successful data has no red error; deliberately incomplete data remains a closed red disclosure, expands on click and preserves the draft. Optimize retains a signed negative average and adds no acquisition/write. Reload reuses the saved snapshot with one database read, no repeated source reads/writes and unchanged observation time. Tests perform zero lineup submissions. Screenshots were inspected; no page overflow or JavaScript errors occurred. Authenticated hosted navigation and post-deployment acceptance remain unobserved.
- Performance review: 100 small legacy snapshot hydrations took 0.134s with zero added source/database reads or writes. For a synthetic 100-player bundle over 100 runs, explicit-selector normalization averaged 2.880ms and verified missing-selector normalization 3.870ms. The extra work is bounded local validation/hashing, not acquisition. Mobile/desktop browser heartbeats measured 60–81ms with zero idle pitch mutations over each 500ms observation. The fix restores the previously suppressed initial snapshot POST; the acknowledged copy prevents duplicate component reads/writes in a fresh session. No additional timer, invalidation, worker job, cache, schema or dependency is introduced. Documentation has no runtime effect; measurements exclude production network latency.


### 2026-11-15 UTC — Collapsible player filter boxes

- Both shared filter-box layouts now start closed behind **Filters** and a
  right/down arrow, using native browser disclosures. Input IDs, defaults and
  active selections are retained; the position breakdown, recent-average input
  and tables remain visible. Click, Enter and Space work without a server toggle.
- Existing checks passed: `Rscript test/test_player_table_filters.R` (9 checks)
  and `Rscript test/test_shiny_simulation.R` (13 checks plus 8 tabs), with zero
  HTTP/live database mutations. Existing synthetic Plotly warnings remain.
- An isolated actual-Shiny Chromium fixture passed both layouts at 320, 390 and
  1440px in light/dark themes: initial collapse and bound defaults, arrow state,
  keyboard activation, retained checkbox/select/numeric values, visible adjacent
  controls, and no page overflow or JavaScript errors. Mobile screenshots were
  inspected. The final centered SVG arrow has the same visible vertical center
  as the label in both states/layouts at 390/1440px, verified from rendered pixels.
  All source data was synthetic and external I/O was blocked; hosted
  authenticated appearance and deployment completion remain unverified.
- Performance review: opening/closing added zero HTTP requests, source reads,
  enrichment runs, table renders or updates. A burst of 100 toggles with forced
  layout took 11.3–12.9ms locally. The existing 524-row filter benchmark took
  0.001s with 95.1 KiB of data. The change adds only
  bounded static markup/CSS, no observers, timers, dependencies, cache or database
  work. Existing durable database reuse and acquisition behavior are unchanged.
  Documentation has no runtime effect; local measurements exclude hosted latency.


### 2026-11-15 UTC — Average points × euro trend

- Today and Players now place **Avg N × Trend (€)** beside the existing
  average/price ratio. The numeric product uses the same unrounded N-round
  average and observed euro trend. Signed/zero arithmetic is preserved; absent,
  missing/nonfinite inputs and overflow remain blank. The existing badge palette
  uses a separate finite source-wide range, stable through filtering/search.
- Focused checks passed: `test/test_player_points_ratios.R` (four groups) and
  `test/test_player_metric_colors.R` (10 checks including generated JavaScript).
  Known-answer fixtures cover both price bases, exact signed/zero eight-round
  averages, missing/empty/nonfinite values, N/source/history updates, optional
  column scope and filtering without repeated enrichment. Required
  `Rscript test/test_shiny_simulation.R` passed 13 checks plus 8 tabs, with zero
  HTTP/live database mutations; existing synthetic Plotly warnings remain.
- Actual Shiny/Chromium checks passed both tables at 390/1440px in light/dark:
  adjacent placement, numeric sorting with missing values last, signed/zero
  values, stable independent colors through client search and application filters,
  and exact N/history updates. A 17/3 average multiplied by €100 displays 566.7,
  without rounding the input to 5.7 first. Screenshots were inspected. No page
  overflow, JavaScript errors, extra HTTP or external I/O occurred. Tests use
  synthetic data; hosted authenticated appearance/deployment remain unverified.
- Performance: for 2,000 synthetic players over 200 iterations, existing
  three-column range extraction averaged 0.145ms/run; the product calculation
  plus four ranges averaged 2.520ms/run. The added numeric vector is 15.7KiB.
  Browser N changes took 75–98ms and shared history changes 100–124ms; the latter
  used two in-place table updates with zero widget rebuilds. Calculations run
  only in existing enrichment, and filters reuse it. Rendering adds one column
  within the existing scrollable table. No new acquisition, database work, cache,
  persistence, observer, timer, dependency or schema is introduced; underlying
  successful observations retain durable database reuse. Documentation has no
  runtime cost and local timings exclude hosted latency.


### Worker player-card availability recovery — 2026-11-15 UTC

The screenshot's expired availability gate is reproduced with a saved worker capacity result. Worker collection runs at 300 seconds but the original transaction validity is at most 15 seconds. The passive gate disabled all acquisition controls, including the route to the existing manual live preflight; Refresh could only reread the same expired database result. No private account response was needed to reproduce that control-flow defect.

The card now exposes explicit **Verify availability** outside the ordinary purchase-readiness gate. One asynchronous live capacity check restores market/owner/clause controls only when verified; duplicate clicks while pending share the check. The current scoped result survives older worker replays, yields to newer observations, and is cleared on selection/context/Refresh/sync-mode changes or session closure. Missing/invalid deadlines, partial results and source failures stay blocked and retryable. Expected aging does not erase partial/error diagnostics. Testing also reproduced an unlisted rival offer incorrectly applying the market listing minimum; its passive gate now uses owner-offer rules. Submission retains its independent live preflight and no verification sends a transaction.

Focused verification: new recovery regression 9 groups; existing worker manual-offer 4, worker card publications 2, card late-summary/reopen 4, native slow-history/reuse 2, market-offer opening 14 and parallel acquisition 9 passed. Required offline lifecycle passed 13 checks plus eight tabs with zero real HTTP/live database mutation attempts. Recovery coverage includes real worker revision replay, missing-provider loading, older/newer observations, mode-disable cancellation, context isolation and malformed results. MockShinySession output extraction waits on a pending real provider promise, so that fixture checks the pending state separately; the actual browser fixture covers the rendered pending case.

Actual Chromium checks at 360 and 1440 pixels use the real module and theme with synthetic sources: enabled Verify before input, disabled purchases during a held check, successful modal opening without another live read, network/partial failures and explicit retry, plus recovery while the saved capacity promise remains held. Initial browser heartbeats were 63–66 ms with no horizontal overflow or JavaScript errors. Screenshots were inspected. These are local synthetic timings, not authenticated production latency.

Performance review: no new passive source request, database read, polling timer, worker job, or recurring redraw. Explicit clicks use the existing bounded parallel acquisition adapter (four prerequisite slots and existing bounded summary work/freshness pass). One pending check and one current result are retained per card, with constant-size timestamp comparisons; reopening/context changes discard that projection. Existing database snapshots and historical persistence/reuse remain authoritative; the transient UI projection is not durable storage, and no source deadline or persistence acknowledgement is manufactured. Dependencies, schema and worker cadence are unchanged. The additional native button wraps safely on mobile. Separate missing player-summary evidence still follows its existing readiness/acquisition path; this change repairs capacity acquisition without bypassing that prerequisite. Production acceptance remains separate from local verification and the authorized branch push.


Exact task-only release verification: all focused groups above and deployment-contract 3 passed again, together with required lifecycle 13 plus eight tabs. Final Chromium checks passed at both widths, including held stored capacity; heartbeats were 64–66 ms. The tested Selected Player SHA256 is `116f4663afce1e78461f99fdb5a0c002c719ce269858d7a93117b55f0f5f19c4`. Its manifest checksum was updated; the other 34 checksum mismatches are inherited unchanged from HEAD (no dependency or deployment-file-list change). The Docker source runtime was tested directly; this task does not claim to repair that unrelated legacy manifest metadata. Only task code/tests/documentation and the module checksum are included; unrelated staged/working edits are preserved. No authenticated production check or transaction was performed.


### Live availability expiry after verification — 2026-11-15 UTC

The previous change restored the explicit verification control but did not fix live checks that expired during acquisition. A native-parser reproduction with history held for 31 seconds and the refreshed market held for 20 seconds returned exactly the reported message: funds had expired five seconds before the result settled. The old focused worker-card fixture supplied a prebuilt fresh capacity and therefore could not expose this failure. The new regression drives the actual card/button, async adapter, source getters, caches and pure preflight; only HTTP, time and unrelated card resources are synthetic.

A second reproduced defect lost known competition/season/scoring identity when live bypass disabled worker mode. The nested history baseline lookup used `unknown` context and could not reuse its acknowledged historical coverage. The async adapter now retains the captured identity within the matching account/league, preserves explicit archives/team/player fields, and rejects a conflicting new implicit identity. The explicit synchronous manual-live scope retains observed identity too, preventing submission from missing the just-verified capacity and repeating cold serial acquisition. Identity retention does not renew metadata or transactional freshness.

The asynchronous freshness repair now settles expired history/roster and transfer candidates before expiring market/funds checks, then rechecks source deadlines with at most one further repair cycle. Successful expiring facts are the only refresh candidates; failed/stale fallback facts retain their diagnostics and do not receive extra freshness retries. Source deadlines and configured financial/roster arithmetic remain unchanged. A pathological sequence that continues to outlive those deadlines stops partial and blocked rather than looping or authorizing stale facts.

Read-only production metadata inspection through the Supabase connector returned two stored team-capacity snapshots with successful envelopes and unknown envelope completeness. That aggregate metadata does not establish live browser request timings or underlying payload validity. No production mutation, token retrieval, authenticated account test or real transaction was performed. The reproduced causes and source-clock assertions are established by the native fixtures, not inferred from those aggregate timestamps.

Focused results: native delayed acquisition 3 groups, actual-card/live Verify integration 3 groups, live context/durable baseline/submission handoff 8 groups, existing parallel acquisition 10, card slow-history/reuse 2, worker manual offers 4, source-expiry cache 7, async-read recovery 7, competition context 14 and durable history baseline storage 13 passed. The integrated card retains source funds stamps 0/31/51 seconds, market stamps 0/51 and roster stamps 0/31; its deadline remains at 61 seconds after the check settles at 51. It coalesces duplicate clicks and opens the normal offer modal with no further source reads or transactions. A new arrival discovered during refresh retains its known-answer effective roster count after a delayed summary. Failed/expired results remain blocked.

Performance review: initial prerequisites still use four slots; repair phases use two, and candidate summaries keep two slots and the 32-identity bound. There are at most two repair cycles, not an unbounded validity chase. The late-market fixture needs one additional short funds read; immediate capacity reuse issues zero HTTP calls. Correct context restores one scoped database GET plus one correction-window history page, retains old history, persists one acknowledged merged baseline, and restores it after clearing memory with one DB GET and zero source reads. Immediate submission adds zero DB/source calls. Three-field identity comparisons add no recurring work, cache, timer, queue, schema or dependency. No UI layout changes are made. Broader transport/lifecycle and exact task-only release verification follow below; local virtual delays do not measure production latency.


Additional shared checks passed: async transport 4, actual-socket scheduling 10, data service 10, session resource ownership 3 and worker observation identity 4. The socket fixture kept the event loop responsive (23 ms initial dispatch; 41 ms foreground completion with three slow background sockets; unchanged maximum four connections). Independent review also caught the top-level capacity cache preceding the adapter context wrapper: it now canonicalizes the caller's known scope and rejects conflicting season/scoring metadata before cache lookup. The context fixture verifies both conflict rejection and canonical-cache reuse when local metadata is absent. No data deadline is extended.


Exact task-only release checks all passed again: native acquisition 3, actual-card/live verification 3, live context/persistence/handoff 8, parallel acquisition 10, observed identity 4, native card freshness 2, worker card recovery 9, manual offer 4, async transport 4, async read recovery 7, socket scheduling 10, data service 10, session ownership 3, competition context 14, durable history store 13 and deployment contract 3. Required offline lifecycle passed 13 checks plus eight tabs with zero real HTTP/live database mutation attempts. The exact socket replay retained a 24 ms dispatch and 42 ms foreground response with three slower background sockets; these are local synthetic measurements. Tests preserve zero/negative/undated points and database failure behavior. No additional mobile layout or dependency change requires a new presentation/dependency bundle.

Tested SHA256 values: `async_transport.R` = `84538e1920eb6a6e5799d210612346f02c93b2175a29c7212f070a7b0b15c7ad`; `data_service.R` = `eac8c0639f2a49ce967d904cddbef4b76e0eb7bbf0ef7f462f443b86aa880253`; `futmondo_functions.R` = `52e70522b604572c576b350d0620bc94544c16908f973d164303422198309947`. Only those runtime manifest checksums change; inherited unrelated manifest metadata remains outside this task. The final documentation addition has no runtime cost. Authenticated hosted acceptance and real upstream timings remain unverified; no transaction was used for validation.


### LigaRadar pitch / murmuration / radar loader — 2026-11-15 UTC

Added a browser-only 12-second loading loop: pitch markings flow into a starling-like cloud, gather into radar rings, and reveal 22 decorative moving player dots before returning to the pitch. The existing launch preloader and authenticated league/first-page loading use the shared component. Loading text is visible and accessible; readiness never waits for a full cycle. Settled error, partial, empty and retry feedback retain their existing contracts. Dark/light colors reuse the app tokens; reduced motion, missing canvas and context loss show a static SVG.

Performance review: one shared requestAnimationFrame scheduler draws at most 30 FPS, with 320 particles on desktop or 180 for narrow/compact scenes, and pixel density at most 1.5. Geometry/palette reuse avoids per-frame DOM reads. Hidden, offscreen, zero-size, removed and reduced-motion components stop drawing; detached nodes release observers/listeners. DOM discovery scans known loading hosts rather than unrelated nested tables. No dependency, API/DB request, server timer, recurring reactive output or acquisition/persistence behavior is added. Existing successful factual observations retain their database-backed reuse. Runtime JS/CSS plus the R helper are 5,621 bytes gzipped in the local measurement, below the 20 KiB budget.

Final native Chromium visual checks passed at 1440/390/320px in light/dark, inspecting all five loop phases and screenshots. Control response heartbeats were 6.6/8.1/8.8ms; p95 drawing callback times were 0.70/0.60/0.60ms normally and 2.80/1.70/2.20ms at 4x CPU throttling. Coverage includes live theme changes, bounded pixel density, mobile overflow, zero-width hiding, context loss/restoration, hidden ancestors/document visibility, live reduced-motion changes, 20 asynchronous remounts without duplicate rendering, removal, and initial static fallbacks. The isolated visual fixture attempted zero external requests. Visibility/context loss are simulated browser events; these local callback measurements do not establish physical mobile GPU cost or hosted latency.

Both the current workspace and the isolated task-only release passed existing progressive-loading checks (16), deployment-contract checks (3), and the required offline lifecycle (13 checks plus eight tabs, zero HTTP/live database mutations). Existing empty synthetic Plotly warnings remain. Manifest changes include only the six task-related runtime entries; packages and unrelated checksums are preserved. Documentation adds no runtime work. Real-account authentication, production deployment and hosted rendering are not asserted by local offline checks.

Actual app startup acceptance passed at 1440 and 320px using the exact task-only release: the retained launch overlay stops drawing after dismissal; league and market states show the compact scene; readiness removes it; partial/failed states retain Retry and empty states settle without a graphic. The fixture reports zero real HTTP attempts. The browser fixture's login-form visibility value disappears on the client despite the server emitting it; independent inspection reproduced this with the original HEAD UI/server and spinner. Startup acceptance therefore sends synthetic credentials through the native Shiny login handler, without bypassing authentication/access guards, and does not claim click-through login-form validation or repair of that pre-existing behavior. The unchanged local remembered-login script is supplied by an exact browser route because app-object fixtures do not mount the application's www directory. Screenshots of the actual loading states were inspected.

Final compact-size polish scales particle/stroke widths up to 2x so small scenes remain legible while the 420px scene is unchanged. The final visual suite and task-only full-app suite passed again with these exact assets. One concurrent full-app run missed the transient league-loading text; an isolated retry passed with unchanged files, so that failed timing run is not counted as a pass. No production or data behavior was changed to accommodate the fixture.


### Avg N × Trend in millions

Today and Players now label the product **Avg N × Trend (€M)** and display millions with exactly three decimals. Actual widget callbacks passed `2.500`, `0.000`, `-1.500`, rounding, invalid blanks and unchanged numeric-data checks on both pages. Existing average/ratio formatting remains unchanged. `test/test_player_points_ratios.R` passed all four groups; `test/test_player_metric_colors.R` passed 10 checks plus its client checks; `test/test_shiny_simulation.R` passed 13 checks plus eight tabs with zero real HTTP/live database mutations. Independent review confirmed display-only scaling preserves sorting and source-wide color ranges.

Performance review: one constant-time division per rendered metric cell; no new requests, database work, cache entries, invalidations, reactive updates, timers, row scans or retained memory. Existing wrapped headers and horizontal scrolling remain; shorter million values reduce typical text width. No dependency change; only the player-table runtime manifest checksum changes. Documentation has no runtime effect. Physical mobile/browser and hosted rendering were not checked. Unrelated working-tree edits are preserved and excluded from this commit.


### LigaRadar app branding

App-owned visible copy now uses LigaRadar across Welcome/login feedback, loading, offers/direct sales, lineup, notifications, administration, worker consent and error disclosures. The existing Futmondo credential notice is unchanged. Display projections also cover saved system bidders, offer tooltips and transfer counterparties; source IDs, API URLs, raw observations, ledger arithmetic and manual-action controls retain their contracts.

Passed offline checks: login feedback 5 server plus 5 Chromium browser checks; Today transfers 4 groups; received offers 8; Admin settings 6; lineup module 28; offer-error translations; direct sales 6; squad prices 7; transfer metadata/display; award scoring 5; remembered login 21; required app lifecycle 13 checks plus eight tabs with zero real HTTP/live database mutations. Actual notification rendering retains canonical filter values while showing LigaRadar. Additional display fixtures verified both offer tooltips, previously saved system names, preserved raw IDs/names, rival/unknown identity and unchanged finance balances. Remembered-login testing initially lacked `sodium` on the default library path; it passed using the existing `/tmp/futmondo-login-r-library` installation. No application dependency was added.

Performance review: text substitutions add no requests, database work, cache entries/invalidation, reactive updates, timers or retained state. Offer-label projection is constant work per existing cell; the finance view maps its two existing counterparty vectors without changing the shared ledger. The existing 1,000-transfer projection fixture took 0.042 seconds and produced 122.0 KiB locally; this is not a hosted latency benchmark. Existing responsive layouts and wrapping remain. Runtime manifest changes are limited to task files; internal docs/tests remain excluded from deployment. Independent review covers source-contract separation. Full remembered-login/lineup browser suites, physical mobile appearance and authenticated hosted deployment were not checked. Unrelated local edits remain outside this commit.

Deployment contract passed all three checks; all 18 changed packaged entries match their exact staged source checksums. Unrelated source and manifest edits remain uncommitted.


### Lineup note visibility

The Best XI summary and subsequent save reminder now render directly without disclosure icons. Static refresh help is removed, while the button retains its label/tooltip and real refresh/load errors retain their existing reporting. Full score coverage is hidden; missing player averages still produce coverage information. Complete finite averages, including zero/negative scores, do not hide history acquisition or database-save failures. Existing pending calculation, persistence and manual-save behavior is unchanged.

Verification passed: `Rscript test/test_lineup_module.R`, including incomplete coverage/recovery and complete-data read/save failure recovery; `PLAYWRIGHT_BROWSERS_PATH=/tmp/futmondo-browsers node test/test_lineup_errors_browser.cjs` at 390px and 1440px in light/dark themes; `Rscript test/test_deployment_contract.R` (three checks). Browser assertions establish visibility and ordering without icon clicks, absent complete-coverage/static-help notes, retained calendar errors, signed scores and durable snapshot reuse on reload. The mobile dark screenshot was inspected. The first browser attempt used the default empty browser cache; using the existing installed Chromium cache resolved this environment-only issue without a dependency change.

Performance review: only bounded local score-vector checks and smaller status markup; no additional API/database calls, cache entries/invalidation, reactive sources, timers or retained state. Optimize preserved request/write counts, fresh saved snapshots were reused on reload, idle pitch redraws were zero, heartbeat responses were 62–80 ms, and all four viewports/themes had no horizontal overflow or JavaScript errors. No score transformation changed. Independent review passed; this module-only presentation change does not require the shared/startup lifecycle harness. No live accounts/transactions, hosted deployment or physical-device verification were performed. Only this module's manifest checksum changes; unrelated working-tree/manifest edits are preserved. Documentation has no runtime effect.


### Readable notification dates

Removed the source-name prefix from notification cards and formatted UTC dates as **12 Nov 2026, 12:34 UTC**. The same formatter handles last-refresh notices. Complete timestamp validation prevents the shared parser's permissive fallback from turning malformed times into midnight. Valid original ISO timestamps remain in the HTML `datetime` attribute; source values, sorting inputs and durable observations are unchanged. Missing/malformed values stay unavailable.

`test/test_notification_datetime.R` passed known-answer UTC/offset/day-rollover/missing/malformed cases and actual rendered-header/source/escaping checks; player-details checks passed 5; final offline lifecycle passed 13 checks plus eight tabs with zero HTTP/live database mutations; deployment checks passed 3. Independent review confirmed the source filter and acknowledgement/actions are unchanged. Performance: one formatting pass per existing render, 1,000 labels in 0.031 seconds locally; no extra requests, DB work, cache entries, timers, reactive inputs, CSS or dependencies. Existing responsive wrapping is retained, with shorter headers. Only the notification-module manifest checksum changes. Browser/mobile screenshots and authenticated hosted appearance were not checked. Unrelated local changes remain excluded from the task commit.


### Remove generic cached-amount copy

Removed the two requested explanatory sentences from `round_finance_display()` and the header disclosure. Complete stale observations now add no generic message; actual loading, failed reads, missing fields and unverified balances retain their existing feedback. Stale flags, neutral solvency state, exact financial amounts, source clocks, durable database reuse and manual-action gates are unchanged.

Passed: debt/countdown 14 checks; clause-observation contract 6 groups; display-observation 4 groups; banner finance 14; required offline lifecycle 13 checks plus eight tabs; deployment 3. The actual banner fixture confirms both strings are absent after restoring expired database snapshots, with source time and amounts intact. Independent review confirmed the narrow copy removal. Performance: no added reads/writes, requests, caches, timers or reactive dependencies; one fewer paragraph is rendered. Twenty identical observation updates caused zero cash/header/readiness redraws in 0.287 seconds locally. No dependencies or layout changes; only two runtime manifest checksums change. Browser/mobile and authenticated hosted appearance were not checked.


### Collapsed notification filters

`notifications_UI()` now wraps its existing controls in a native `shinydashboardPlus::box()` titled Filters with `collapsible=TRUE` and `collapsed=TRUE`. Read-status/source/event/current-league inputs and Refresh retain their defaults, IDs and behavior. Cards and status remain visible outside the box.

Passed existing notification datetime/rendered-output checks, player-details checks (5), and deployment checks (3). A synthetic dashboard using the real UI/package/CSS passed at 320, 390 and 1440px: collapsed default, expansion/collapse, all five controls within the box, retained changed selections, visible inbox, no horizontal overflow, JavaScript errors or external requests. An initial rapid-toggle probe raced the existing slide animation; waiting for native animation completion passed without product changes. Authenticated hosted and physical-device checks were not performed. The full lifecycle was not rerun for this localized static wrapper; notification server behavior is unchanged.

Performance review: one static wrapper and existing client collapse animation; no custom handler, data/API/database request, cache, persistence setting, observer, server timer or recurring redraw is added. Responsive Bootstrap columns remain, with no CSS/dependency change. Only the notification-module manifest checksum changes. Unrelated working/staged changes are excluded using an isolated index.


### Center dialog dismissal and shorten picker help

The fixed bottom-right Refresh button shared space with Bootstrap's right-aligned dialog footer. Shared CSS now centers footer actions and puts direct Close/Cancel buttons on their own centered row, retaining native dismissal and spacing when another action precedes or follows them. Removed only “Missing scores are shown as —.” from the lineup picker help; unknown values still render as dashes, and round-window help remains.

Passed: required offline lifecycle 13 checks plus eight tabs (zero HTTP/live database mutation attempts); deployment contract 3; existing lineup browser suite at 390/1440px. That older browser fixture needed its mock acquisition signature updated for `is_current`, and its warning assertion updated for the already-collapsed information disclosure; both setup mismatches were repaired before the successful rerun. The initial browser launch used a missing default cache; rerunning with the existing `/tmp/futmondo-browsers` installation required no dependency changes.

A temporary Chromium geometry check rendered the actual lineup picker with the app's floating Refresh markup at 320/360/390/1440px. Occupied/empty pickers and synthetic single-Close/Cancel-first footers were centered within one pixel, did not intersect Refresh, remained clickable and dismissible, and had no modal horizontal overflow or JavaScript errors. The requested sentence was absent. The 320px screenshot was inspected. No additional source/database/submission calls occurred while opening, closing and resizing these dialogs. Authenticated hosted and physical-device appearance remain unverified.

Performance review: static CSS and shorter help text only; no added request, database work, cache, persistence behavior, reactive dependency, timer or retained memory. The existing browser suite measured warm picker opening at 105 ms on phone and 92 ms on desktop locally; these are synthetic timings, not production latency. Documentation and fixture repairs have no production runtime cost. Only the two changed runtime manifest checksums are updated; unrelated working-tree/index edits are excluded from the task commit.


### Rotating 3D pitch / rising-player loading stage — 2026-11-15 UTC

The 15-second loop now tilts/yaws the pitch into perspective, raises 22 anchored player silhouettes, and moves the existing particles from those silhouettes into the flock before the radar sequence. A shallow edge, subtle ground shadows and consistent team colors make the depth and transformation readable at both hero and compact sizes. Static reduced-motion/canvas fallback and all startup readiness behavior remain unchanged; the longer artistic sequence never holds loading open.

The native Canvas 2D implementation computes camera trigonometry once per active frame and reuses projected-coordinate slots. It precomputes particle-to-player/silhouette assignments and keeps one 22-slot lift array per instance. Particle counts stay at 320 or 180, frame rate at 30 FPS and pixel density at 1.5. No library, WebGL context, model, texture, network asset, API/DB work, server timer, reactive frame update or persistence behavior is added. Existing factual data retains durable database reuse. Hidden, removed, zero-size and reduced-motion components retain the existing stopped-rendering behavior. Documentation has no runtime effect.

Independent geometry review found exact flat-coordinate identity, in-scene bounds across the projected stage, a perspective denominator above 482, and raised heads visibly above their ground anchors. Review removed abrupt team-color changes; the final loop also fixes time-varying flat-particle orientation so the returning frame matches the initial one.

The final Chromium suite passed eight sampled phases in light/dark at 1440, 390 and 320px, with hero and compact screenshots inspected. Independent canvas traces verify known flat corners, rotated/foreshortened pitch geometry, 22 heads paired with ground anchors, positive elevation, and finite in-bounds projected marks. The full drawing trace and exact bitmap match across the 15-second loop seam. Existing checks for theme switching, reduced motion, unavailable/lost canvas, hidden/zero-size/removal cleanup and 20 remounts passed, with no horizontal overflow, JavaScript errors or external requests. An initial assertion sampled yaw near its intentional zero crossing; using the dedicated tilted stage corrected that test without changing the camera trajectory.

Final local measurements at 1440/390/320px: response heartbeat 13.8/9.0/10.6ms; all-phase draw callback p95 1.10/0.70/0.70ms; 3D-stage p95 1.10/0.80/0.80ms normally and 3.70/3.00/2.50ms with 4x CPU throttling. Measurements include both hero and compact drawing plus test instrumentation, and do not establish physical-device GPU cost or hosted latency. Combined JavaScript/CSS/R helper gzip is 6,958 bytes, versus 5,621 before this stage, within the 20 KiB budget.

The required offline lifecycle passed 13 checks plus eight tabs with zero real HTTP/live database mutations; deployment checks passed three. Existing empty-fixture Plotly warnings remain. Only the renderer's runtime manifest checksum changes; packages and other file entries are preserved. Startup/authentication interfaces and server loading states are unchanged, so the existing full-app click-through fixture was not rerun for this drawing-only extension. The previous login-visibility fixture limitation remains documented and is not claimed fixed. Real accounts, transactions, production rollout and hosted/physical-device appearance were not used for validation.


### Futbolín loading figures and team tones — 2026-11-15 UTC

The rising figures now use a filled molded shirt/stem/kicking-foot contour, round head and a small shaded shirt face. The two teams use cached green/gold tones from the existing text/warning theme tokens, preserving their identities through the figures, morph particles and radar dots; the radar sweep retains its cyan accent. The same silhouette supplies morph coordinates. Independent review caught an omitted foot endpoint in sparse sampling; the final mapping explicitly reserves both broad-foot corners, verified for all 22 actors at both 180/320-particle budgets.

Desktop and compact previews in both themes were inspected: the broad-foot figurines and team distinction remain readable. No graphics dependency, texture, model, external asset, per-frame color parsing or DOM read is added. The 15-second timeline, 30 FPS cap, 1.5 pixel-density cap, existing particle counts and visibility/reduced-motion cleanup are unchanged. Added contour/shading work is bounded to 22 figures; silhouette sampling and palette reads remain outside the frame loop. No API/database work, cache/persistence behavior, server timer, reactive data output or manual-action behavior changes. Existing factual observations retain their durable database reuse; documentation adds no runtime work.

Final exact-release Chromium checks passed eight phases in light/dark at 1440, 390 and 320px. Canvas traces verify 22 substantial filled bodies with broad lower feet, eleven heads/bodies per team tone, matching particle/radar colors, anchored rising geometry and exact full-trace/bitmap continuity across the loop seam. Existing theme, mobile overflow, reduced-motion, missing/lost canvas, hidden/zero-size/removal and 20-remount checks passed. Final hero/compact screenshots were inspected. No external requests or browser errors occurred.

Local measurements at 1440/390/320px: response heartbeat 13.5/8.6/8.4ms; all-phase drawing callback p95 1.00/0.80/0.70ms; 3D-stage p95 1.00/0.90/0.70ms normally and 3.50/2.90/2.90ms at 4x CPU throttling. These include hero/compact drawing and test instrumentation; physical-device GPU cost and hosted latency are not established. Combined JS/CSS/R-helper gzip is 7,177 bytes, 219 bytes above the previous 6,958-byte version, within the 20 KiB budget.

Required offline lifecycle passed 13 checks plus eight tabs with zero real HTTP/live database mutations; deployment contract passed three. Existing synthetic Plotly warnings remain. Only the renderer's runtime manifest checksum changes, preserving packages and unrelated entries. Full-app click-through was not rerun for this draw-only change; the previously documented login-visibility fixture limitation is not claimed fixed. No real accounts, transactions or production/physical-device acceptance were used. Other working/staged and concurrent branch changes remain outside the task commit.


## Login feature comparison tiers — 2026-11-14

Implemented an eight-item public inventory: three Essentials exploration workflows and five Advanced lineup/ranking/finance workflows. `login_feature_tiers()` owns the runtime copy and `login_features_UI(id)` renders static, semantic sections outside the authentication status output. The two equal desktop columns start at 992px and stack below that. The list stays mounted through validation errors, successful authentication, navigation and logout. Each item explanation starts collapsed in a native `details`/`summary`; titles toggle it with click/tap or Enter/Space and a chevron indicates expansion. No server output or client event handler runs on disclosure toggles. Existing app access and official manual-action rules are unchanged; no commercial copy, pricing or feature gates were added.

The maintained [official comparison](feature_comparison.md) records dated source review, official equivalents, implementation references and limits. Futmondo already documents suggested lineups, averages, historical cards and market actions; only specific added workflows are described. The owner’s league does not show suggested lineups. The comparison acknowledges league configuration while leaving the unverified reason (administrator setting, mode or entitlement) unresolved. No live account was used to test official availability. `Agents.md` now requires this comparison and both inventory updates for new/expanded capabilities.

Verification: `Rscript test/test_login_feedback.R --browser` passed 5 server checks and 5 existing click/keyboard/duplicate-submission browser checks; the default Playwright browser binary was absent, so a temporary `/tmp` adapter selected the already installed Chromium 1248 without repository/dependency changes. `Rscript test/test_login_restore_visibility.R` passed cookie probing, completion, restore, expiry, denial and disabled-setting visibility. Required `Rscript test/test_shiny_simulation.R` passed 13 checks plus 8 tabs, with zero external HTTP/live database mutation attempts. A temporary browser script against the existing HTTP-blocked `test/test_browser_app.R` passed 320/390/992/1440px in light/dark themes: all eight explanations initially hidden, title-click and Enter/Space toggles, rotating chevrons, 3/5 tier assignment, stacked/two-column geometry, no page overflow while collapsed or expanded, no commercial wording or browser errors, and preserved catalog DOM/disclosure state across validation, login, navigation and logout. Initial scratch navigation assertions ran before startup navigation settled; explicit Shiny connection/authentication/idle synchronization resolved the harness timing. Settled collapsed 1440px dark and expanded 390px light screenshots were visually inspected. The lifecycle and existing login browser checks were rerun after the disclosure change. Scoped task diff/whitespace review passed.

Performance review: only eight constant items and two static sections are rendered once per UI construction. Collapsed descriptions reduce initial layout height; native disclosure toggles lay out only the selected content locally. Authentication status updates do not rerender the inventory. Added CSS is scoped and layout is responsive; there are no listeners, reactive dependencies, timers, acquisition/optimizer work, API/database calls, cache invalidations, new persistence or growing buffers. Official reference links initiate no application request. Existing factual results continue to use their established durable database observations; this task adds no result cache. Documentation/rule changes have no runtime impact. No dependency, environment, schema or manifest-dependency change. Verification is synthetic/local; hosted rollout and authenticated official configuration inspection remain unverified.


## Championship empty asking prices and clause dates — 2026-11-14

Root cause: captured `/5/league/championshipplayers` responses carry catalog
valuation/clauses but no asking price. `unify_columns()` consequently created an
all-missing `effective_market_price`; the championship module never read/joined
`/1/market/players`, whose `price` is the actual asking price. The market capture
contains 163 unique player IDs with prices. The newer championship capture has
520 rows, with three `clause.transferDate` values, all on `transferred=TRUE`
players. Parsing preserves all three exactly. The default Active Clause Only
filter correctly excludes these players; absent dates on other rows are expected.
The raw date column also lacked its own label/formatter.

The fix projects asking prices from the existing state-only shared market
adapter by exact player identity, independently of catalog loading, without
substituting valuation/clause prices or manufacturing dates. Missing dates render
as Not reported, and supplied dates use the existing timezone formatter. Pending
transfer dates remain visible when the active-clause filter is disabled. Market
loading/failure/stale feedback remains separate from usable catalog data.
Existing DB worker snapshots retain account/league/team/competition/season/scoring
scope, source clocks and completeness. There is no new cache, durable write,
acknowledgement shortcut, schema or dependency.

Verification: `Rscript test/test_championship_columns.R` passes ten groups covering
exact ID joins, zero row multiplication, known asking prices, unknown/empty/stale/
partial/duplicate evidence, known-answer date formatting versus lock expiry,
actual captured date preservation, typed persisted-snapshot restoration and the
real market wrapper reusing that snapshot with zero source requests. Real shared
adapters with a delayed synthetic market promise prove catalog access and an
input/output heartbeat remain responsive, warm reactive access makes no added
requests, league changes clear old prices, failures retain catalog data and safe
retry feedback, and explicit Refresh recovers with one market request per scope/
refresh. No real account, live database write or transaction was used.

Adjacent checks passed: player-table filters 9, player metric colors 10, nested
player fields 5 (including warm/restart reuse), deployment contract 3; required
`Rscript test/test_shiny_simulation.R` passed 13 checks plus eight tabs with zero
external HTTP/live DB mutation attempts. Existing synthetic source diagnostic and
Plotly warnings remain; they do not fail the harness. Initial focused harness
iterations needed an eager table consumer and HTML/dependency-aware assertions;
loading feedback now uses a plain status element because the existing pending
information helper intentionally returns no markup.

Performance review: one additional shared market dependency for this page; a cold
or invalid source can require one market acquisition, while valid persisted and
warm resources are reused. No player-by-player request, new polling/timer, queue,
service, history fetch or DB schema/index work. The projection uses a bounded ID
match over the existing catalog/market data; 100 joins of 527 players took
0.014–0.040 seconds locally across repeated runs, with a 112,136-byte projected frame. Actual timings vary
and exclude hosted network/DB latency. Existing table equality logic prevents
unchanged payload redraws; the small status element/disclosure uses responsive
existing styles and introduces no fixed-width control. Mobile/browser appearance,
production DB acknowledgement and authenticated hosted acceptance were not tested
in this task. The exact task-only staged application independently passes the
focused, filter, deployment and required lifecycle checks. Scoped runtime manifest
checksums are updated without changing
packages or unrelated entries. Existing working-tree/index edits are excluded
from the task commit. Local validation and branch delivery do not establish
production rollout.


### Pinned player-table crests and positions — 2026-11-14

The shared player tables now display the source club crest in its own 36px
pinned column, immediately before the scrolling Name cell. The boundary has no
border, matching opaque row backgrounds and compact spacing. Position is also
pinned; as Name scrolls away it settles beside the crest. Native Reactable sticky
offsets cover headers/filters and column resizing. The optional `separate_crest`
column-definition flag defaults to the original combined identity renderer for
Best XI cards. Club/source-logo fallback, unknown crests, escaping, original row
IDs, filters, numeric values, row selection and player-card actions are retained.

Passed: `test/test_player_name_crest.R` (6 checks),
`test/test_squad_table_prices.R` (7), `test/test_player_table_filters.R` (9), and
`test/test_shiny_simulation.R` (13 checks plus eight tabs; zero external HTTP/live
database mutation attempts). The synthetic real-module fixture
`test/test_player_table_sticky_browser.R` plus `test/test_player_table_sticky.cjs`
passed Chromium at 320/390/1440px in both themes: crest and Position remain
visible at the right scroll limit, names scroll away, headers/filter cells stay
aligned, the initial crest/name boundary has no border and identical backgrounds,
stripes/departure tints remain opaque, rerendered tables retain pinning, clicking
a pinned crest opens the player card, and desktop Position resizing retains its
offset. No page overflow, visible output errors or JavaScript errors occurred.
Initial light-mobile and scrolled dark-mobile screenshots were inspected.
Heartbeat responses were 122–259ms locally. The first browser invocation used a
mismatched existing browser cache; using `/tmp/futmondo-browsers` resolved it
without a dependency change.

`test/test_best_xi_crests.R` passes its identity/source-logo/escaping/missing-logo
checks but fails its existing loading-notice assertion. Repeating it with the
saved pre-task `get_reactable_columns_for_players()` produces the same failure:
the existing `fm_info(..., pending=TRUE)` loading notice is suppressed. This task
does not change that module or loading behavior; the unrelated fixture mismatch
remains. Do not count that suite as passing.

Performance review: reuse the existing `team` and `logo` payload fields without
adding data fields or acquisitions. One previously hidden field becomes a narrow
cell per rendered row; crest images are moved rather than duplicated and remain
lazy/async. Native CSS sticky positioning adds no scroll listeners, timers,
reactive sources, server calculations, request/database volume, cache entries,
invalidation or retained state. Existing durable observations and context/freshness
contracts are unchanged. DOM work remains bounded by existing pagination; mobile
retains a 148px pinned area at default widths, leaving the remaining table viewport
scrollable. Existing row-background painting is scoped to shared player tables.
No dependency/environment/schema change or data transformation. Documentation has
no runtime effect. Browser checks use synthetic image responses and rows; real
accounts, transactions, production deployment, physical devices and hosted
performance were not exercised. Only task changes and their runtime checksums
belong in the delivery commit; unrelated working changes are preserved.


### Compact pinned recent average — 2026-11-15

Avg Last N now starts at 84px, down from 110px, and uses native left pinning beside
the 36px crest. Position has no sticky setting and scrolls with Name. The header
shows **Avg** above **last N** to remain readable at the narrower width; the column
remains resizable. Numeric sorting, one-decimal badges, missing values, source
score coverage, tooltip details and the existing 1–20 round window are unchanged.
The borderless crest/name join and existing opaque row-state colors are retained.

Passed: player-name/crest contracts (6); recent average (6 base checks plus 7
additional known-answer/display/source-window checks); recent-point hover (6);
deployment (3); required offline lifecycle (13 plus eight tabs, with zero external
HTTP/live database mutation attempts). The updated synthetic browser suite passed
at 320/390/1440px in both themes: Avg Last N is 84px, crest/average/header/filter
remain aligned at the right scroll limit, Name and Position scroll away, changing
N to 20 and back preserves pinning/compact defaults, the initial crest/name join
is seamless, row backgrounds stay opaque, player cards still open from the crest,
and desktop average resizing retains its sticky offset. No page overflow,
JavaScript errors or visible output errors. Phone heartbeat responses were 260ms.
Mobile dark screenshots for N5 and N20 were inspected.

Performance review: the default pinned area shrinks from 148px to 120px; the
existing cells, native sticky positioning and pagination are reused. No added
requests, database reads/writes, cache entries/invalidation, reactive sources,
timers, scroll handlers, retained state, dependencies or schema changes. Existing
durable source observations and freshness/context isolation are unchanged. This
is presentation only; no score/history transformation changed. The same focused
known-answer fixtures still cover signed/zero/unknown points. Documentation has
no runtime effect. Local synthetic checks do not establish hosted performance,
physical-device rendering or production deployment; no real accounts or
transactions were used. Only the two changed runtime checksums belong in this
task's manifest update; unrelated working changes remain excluded from delivery.


### Today heading information icon removal — 2026-11-15

Removed the introductory information icon and its “Available market players and
offers, current league balances and completed transfers.” tooltip in `today_UI()`.
Focused offline rendering passed with `fm_info()` configured to fail if called:
heading, market table placeholder, cash/valuation and recent-transfer outputs
remain present. Deployment contract: 3 passed / 0 failed. Updated only the
Today module checksum; no dependency changes.

Performance review: one less static tooltip/icon; no new API or database calls,
cache reads/writes/invalidation, blocking work, reactive updates, timers or retained
state. Existing persisted observations are unaffected. Removing static markup
reduces rendering work on desktop and mobile and adds no width constraints.
Documentation has no runtime effect. This is a single-screen presentation edit;
no shared infrastructure, startup or cross-cutting UI behavior changed. No live
accounts, transactions, browser/device verification or deployment performed.


### Login coach status unavailable recovery — 2026-11-15

Reproduced the pre-task controller with a synthetic thirty-second-old shared
observation: it set `Coach status unavailable` and retained that message even
after a fresh hired response arrived. The captured official market information
query supplies `configuration.enableCoach=automatic` and `staff.coach=0`, then
`1` after hiring; only those relevant fields/query shape were inspected, without
logging private bodies or replaying an authenticated request.

Fixed the terminal unknown latch, added a bounded asynchronous safety read when
shared evidence is unknown, and a bounded post-action confirmation fallback when
worker replay returns old evidence. Kept the twenty-second action freshness
window, explicit hired/absent distinction, preference handshake and duplicate
submission guards. Genuine failures retain safe source diagnostics; incomplete
data remains unavailable and cannot authorize a hire.

Passed: `test/test_coach_status_recovery.R` (9), existing
`test/test_admin_awards_coach.R` (11), deployment contract (3), and required
`test/test_shiny_simulation.R` (13 checks plus 8 tabs; zero external HTTP/live
database mutation attempts). Recovery checks include real adapter/native request
serialization, compact durable snapshot write/reuse, foreign context and expiry
rejection, post-action cutoff, failed-save resubmission, pending-read heartbeat,
opt-out/logout/league/team changes, recovery after a failed source, and uncertain
mutation response without a repeated hire. Snapshot boundaries are synthetic;
no live database write or authenticated hire was used.

Performance review: fresh initial shared status has zero additional reads. Each
login can add at most one initial and one confirmation safety acquisition, each
with a one-row scoped database lookup and at most one logical team-information
request (existing idempotent transport permits one bounded transient retry).
Valid database reuse performs zero Futmondo HTTP requests and no duplicate save.
Fresh fallback observations enqueue a small immutable coach snapshot alongside
normal factual recording; rejected/unacknowledged writes receive no saved marker
and can resubmit the same revision. Existing queue bounds/retries apply. Shared
confirmation still invalidates only team information; market/roster/history
acquisition is unaffected. No timers, global polling, growing memory cache, new
dependency/schema, client-side credentials or mobile layout work was added.
Held safety reads allowed event-loop heartbeat callbacks; UI loading is not
blocked on HTTP. Existing server-only restricted evidence view/grants remain
unchanged. Supabase changelog/security documentation was checked on 2026-11-15:
https://supabase.com/changelog.md and
https://supabase.com/docs/guides/api/securing-your-api.md.
Documentation has no runtime effect. Local fixtures establish the recovery/safety
contracts, not production performance, physical-device rendering or the precise
source state of the owner's hosted session. Production rollout is unverified.


### Automatic player-card availability — 2026-11-15

The worker card required an explicit Verify availability click for snapshots
whose transaction deadline had expired. Opening now invokes the same live
factual verifier automatically once per opening/context/Refresh. Valid scoped
database observations are reused, including already decoded snapshots awaiting
reactive publication. The existing native team cache is reused on warm reopening.
The retained control reads Retry availability after failures or subsequent expiry.
No purchase is sent by opening, verification or a retry.

Passed: updated worker-card recovery (8 scenario groups), native live capacity
(4 checks, including delayed history/market and zero-request warm reopening),
legacy capacity completeness (4 groups plus malformed-evidence variants),
existing card capacity recheck (4), native availability freshness (2), market
offer opening (14), deployment contract (3), and the required final
`test/test_shiny_simulation.R` (13 checks plus 8 tabs; no external HTTP or live
database mutation attempts). Recovery covers one pending request, duplicate
inputs, old/new snapshot ordering, identical-context redraws, fresh durable reuse,
network/partial/expired/undated failures without loops, late context results,
Refresh, missing worker publication, worker-mode changes and owned-player skip.
The exact staged module is also checked with the worker/native fixtures so
unrelated working player-card edits cannot explain those results.

The final real-browser fixture passed at 360px and 1440px: opening starts a held
live check without a click; verified actions/editor become usable; failed/partial
checks remain blocked and can retry; missing/pending stored publication does not
delay automatic verification; no overflow or JavaScript errors. Heartbeat
responses were 64ms during the initial held check and 66ms during held stored
publication at both widths. Mobile pending/success screenshots were inspected.

Performance review: at most one opening verification per distinct context/revision
plus explicitly requested retries; no worker-publication or same-context redraw
loop. One cancellable 10ms deferral yields to initial replay; it does not repeat
and is removed on scope/session changes. Valid durable capacity adds zero live
checks; warm native reopening adds zero source requests. The existing composite
retains four parallel prerequisites, bounded summary/freshness repair and strict
15/30/300/20/60-second source deadlines. Its synthetic 31s-history/20s-market
fixture made information=3, roster=2, market=2, pressroom=2 requests once, then
zero additional requests for the verified offer editor and warm reopening. No
additional schema/DB scan/write/cache layer or persistence acknowledgement
dependency was introduced. Existing database snapshots/history and bounded
persistence remain authoritative across sessions/restarts; the card projection
does not assert an unacknowledged write is durable. Memory adds two fixed scope
references and one timer handle, not an expanding cache. No CSS/layout,
dependency/environment, background worker cadence or transaction changes.
Documentation has no runtime effect. Local fixtures do not establish production
latency, physical-device behavior or hosted deployment. No real accounts or
transactions were used. The delivery includes only this task's module patch
and its checksum; unrelated scheduled-clause/ownership edits remain unstaged.


### Zero-centered red/green Avg N × Trend — 2026-11-15

The trend product now uses a symmetric range around zero with magnitude equal
to the largest absolute finite source product. Zero remains neutral, negative
products use red and positive products green, with equal absolute values having
equal mixing intensity. Existing source min/max metadata and other metric
palettes are retained. Its default minimum width shrinks from 145px to 104px
with a three-line header; resizing and raw numeric sorting are preserved.

Passed: `test/test_player_metric_colors.R` (10 plus its generated-client
fixtures), `test/test_player_points_ratios.R` (4 known-answer/display/source
groups), deployment contract (3), and required offline lifecycle
`test/test_shiny_simulation.R` (13 checks plus 8 tabs, no external HTTP/live DB
mutation attempts). Client fixtures cover asymmetric, one-sided, equal, empty,
zero and missing domains, clamp behavior, stable filtered/sorted/paginated
colors, unchanged other metrics and at least 4.5:1 text contrast over three
surface colors. Exact staged-module color and known-answer tests also pass
independently of unrelated working module edits.

The real shared-table browser fixture passed at 320/390/1440px in dark and light
settings: trend width is 104px, zero is neutral, signed values match expected
red/green intensity and retain fixed three-decimal millions, and headers remain
readable. Crest/average pinning, row backgrounds, N changes, native Position
scrolling, rerenders, resizing and selection continue working. No overflow,
JavaScript errors or visible output errors occurred. Heartbeats were 258–277ms.
The 390px dark trend screenshot was inspected. Narrowing reduces total scroll
width, so desktop Position can remain partially visible at the scroll limit;
the fixture verifies its native scroll displacement rather than requiring it
to disappear completely.

Performance review: constant-time signed normalization replaces the existing
color interpolation for this column only. Source range extraction, data/cache
reads, API/DB requests, persisted observations, source timestamps, invalidation,
reactive publication, pagination and memory bounds are unchanged. No extra
observer, timer, retained cache, image request, CSS scope or dependency/schema
change. The column is 41px narrower; its extra header line is bounded static
markup. Existing durable database reuse is unaffected. Documentation has no
runtime effect. This is presentation-only; point/history/product calculations
are unchanged and their independent fixtures still pass. Local synthetic
checks do not establish production latency, physical-device behavior or hosted
deployment. No real accounts or transactions were used. Delivery excludes
unrelated scheduled-clause interface edits and includes only this task's
runtime checksum.


### Phone chart dates and series-axis labels — 2026-11-15

The card's four full-width date ticks crowded narrow plot domains. It now
requests three automatic ticks with DD/MM above the year, retaining horizontal
text and exact source timestamps/values in hover. Green valuation and amber
points ticks/titles now match their lines. Both the R plot adapter and browser
theme updater previously overrode those explicit colors; they now honor declared
axis colors while keeping ordinary axes on their neutral theme defaults. The
independent top Round axis also matches its points series.

Passed: `test/test_player_trend_plot.R` (16), existing
`test/test_plots_module.R` (3), `test/test_rival_summary_layout.R` (12),
deployment contract (3), and required `test/test_shiny_simulation.R` (13 checks
plus 8 tabs; no external HTTP/live DB mutation attempts). Chart known answers
retain every finite source valuation, zero/negative scores, chronological
postponed rounds, explicit finality, original dates and undated-score coverage.
The exact staged module/theme also passes the chart contracts separately from
unrelated working changes.

The actual chart expression passed synthetic browser checks at 320/390/1440px
across combined, valuation-only, dated-points-only and undated-score histories
in both themes (24 combinations). Rendered date-label bounding boxes do not
overlap; tick/title colors equal line colors; uncolored axes retain theme
defaults; all 124 valuations and four scores remain intact. Theme changes
preserve trace data and the zoomed date range. No page overflow, JavaScript
errors or visible output errors; heartbeats were 81/93/95ms respectively.
390px dark/light screenshots were inspected. The dedicated fixture uses the
production plot expression, R adapter and browser theme script with synthetic
history; it does not require accounts, worker tokens or a live database.

A mistakenly invoked legacy `test/test_rivals_plot_df.R` stopped at missing
credentials before login and is not counted as passing. It is a live-account
script; verification continued with the offline rival-summary fixture. No
authenticated request or transaction was performed. Browser setup initially
addressed Selectize as a native select; using its actual selection API resolved
that fixture issue without a production change.

Performance review: fewer date ticks reduce label density; the second year line
is bounded SVG text. Theme changes reuse the existing single layout update,
adding only constant-time color lookups per axis. No new observer, resize
handler, timer, listener, API/database request, source transformation, cache,
invalidation, persistence work or retained state. Existing durable history and
context/source-time/freshness contracts remain unchanged. No dependencies or
environment/schema changes. Documentation has no runtime effect. Local browser
fixtures do not establish hosted latency, physical-device behavior or deployed
acceptance. Delivery includes only this task's changes/checksums; unrelated
scheduled-clause, ownership and line-ending edits remain excluded.


## Scheduled one-shot clause purchases — 2026-11-15

Implemented locally under the explicit scope exception: Championship-card scheduling with per-purchase consent, capped price and offer-slot/fund reservations; separate encrypted purchase access; durable cancellation, attempts and next-login results; and one elected scheduler in the existing supervised Python service, independent of Shiny. All source checks reuse the actual R accounting/parser engine. A lost write acknowledgement remains reserved and is reconciled without another send. Fresh membership, price, transfer state, roster and borrowing limits remain mandatory. Existing factual summaries and persisted transfer events cancel changed ownership or follow lock extensions without adding distant-player acquisition. Proven newer context retires superseded pending jobs.

Executed offline checks: scheduled R projection/consent/lifecycle checks (6 groups), direct manual guards (4), asynchronous manual actions (4), PostgreSQL-compatible PGlite fresh/repeated migration and security/state checks (17), and Python scheduler checks (13). The Python suite runs the real headless R parser against captured-shape synthetic responses: seven cold source reads, then zero additional source reads after helper restart with persisted evidence. A 25-second-old valid membership observation retains its own timestamp without aging fresh financial checks. Explicit native rejection retries are tested only with an artificial code; the production allowlist is empty. SQL checks include 40-year schedules, reservation limits, idempotency, crash recovery, revocation retaining reconciliation-only access, expiry with rejected credentials, immutable publication, source monotonicity, context isolation and server-only RLS/grants.

Existing regression checks passed: Python worker38; offline Shiny lifecycle13 plus eight tabs, with no HTTP/live DB mutation; worker availability recovery; native clause6; configured acquisition rules6; live-capacity freshness; worker provider; UI reliability; manual offer4; deployment3. The manual-offer fixture was updated to account for the already-committed automatic card-opening check while preserving its explicit stale-preflight/context-change assertions. Actual Selected Player browser checks at360/390/1440px cover initially unchecked consent, held acquisition, pending amount/date, card cancellation, logout/login outcomes and acknowledgement with no horizontal overflow. Browser evidence caught a missing schedule revision dependency in the card action observer; the workflow now verifies the cancellation button appears after durable admission.

Performance review: one indexed global idle claim/second (86,400 RPCs/day per elected service), with no source polling for distant jobs. One lazy reusable additional R helper closes after30 idle seconds; source slots and account pacing are shared. Preparation at10 seconds then near2 seconds before opening waits until the published instant; jobs execute serially and network/source/DB delays can miss the opening second. Development measurements: about2.97 seconds cold helper preflight,237,024KiB RSS; idle claim0.426ms average over100 isolated queries;1,000 transfer trigger events164.4ms. These are local measurements, not Contabo capacity or hosted timing. Active browser sessions share one asynchronous DB poll/account every5 seconds; unchanged content does not redraw card actions. Evidence is capped at256 entries/32MiB per account; active jobs at256; outcome reads at100 unread plus100 recent acknowledged successes. Expired/rejected jobs are cleaned without a browser. Uncertain read-only reconciliation backs off to one hour. Held browser verification remained interactive (80–128ms heartbeat in the measured run); async tests separately hold both source verification and the single native write. No dependency changes; scoped manifest source checksums/new R module are updated without replacing unrelated package metadata. Docker excludes credentials, HARs, tests and internal docs.

Limits: no real account, purchase, production migration or deployment was exercised. Apply the additive migration before setting the default-off feature flag. Capture the actual closed-clause rejection using the disabled-by-default account-scoped explicitly confirmed diagnostic, add its exact fixture/mapping, then remove the diagnostic bypass. Hosted VM resources, dispatch lag, real DB/source budgets and controlled restart/execution remain acceptance steps; local implementation does not certify real purchase success. Official comparison and exact settings/interfaces are in `feature_comparison.md`, `scheduled_clauses.md` and `README.md`. Unrelated working-tree/index edits are preserved.


Final exact-staged-artifact verification passed all new suites (R6+4+4, Python13, SQL17), existing worker38, availability recovery/manual offer, native clause6, configured rules6, provider/freshness/UI/deployment checks and the mandatory lifecycle13 plus eight tabs. Actual isolated browser flows passed at360/390/1440px with143/142/144ms held-source heartbeat. All61 bundle checksums match and all78 package records are unchanged. Nine inherited stale checksum entries were reconciled against already committed files; their source contents and unrelated local edits are excluded. The native contract test received only a sanitized local fixture, excluded from the bundle and commit. Static login/comparison copy adds no acquisition or polling, and preserves the existing responsive tier layout.


## Shared roster observation note — 2026-11-15

Moved the repeated roster/transaction timestamps out of Incoming and Leaving
into the existing global Background sync note on My Team, Market (Today), and
All Players. Kept actual panel errors and each player's observation/timing
information. This reorganizes existing information without adding a capability;
no feature-comparison or login-tier expansion is required.

Executed offline verification:

- `test_roster_top_note.R`: three groups exercising the actual root projections,
  actual shared note outputs and real factual adapters against synthetic stored
  snapshots. All three pages retain original timestamps; ten clock publications
  update text without replacing the note (0.708s), with zero I/O, acquisition
  jobs or clause-summary work. League switch, unrelated page, unknown/future
  timestamps, actual worker failures and logout are covered.
- Existing clause observation contract (6 groups), observed reload (4),
  information visibility (3 groups) and player disclosures (5) pass. Ten roster/
  history clock updates preserve both transfer panels (0.370s), with zero source
  requests/database writes; real prerequisite failures remain in both panels.
- Worker status redraw regression (4 groups) passes: twenty unchanged polls
  cause no repeated render/source acquisition (0.344s); account isolation and
  genuine worker status failures/recovery remain visible.
- Synthetic Chromium at 320/390/1440px verifies a single roster note across the
  three pages, expanded note wrapping, unchanged note/offer DOM during clock
  updates, incoming/leaving timing icons, keyboard operation, no horizontal
  overflow, no JavaScript errors and zero source/database calls.
- Required `Rscript test/test_shiny_simulation.R`: 13 checks plus eight tabs
  pass, with no HTTP or live database mutation attempts. Deployment contract:
  3 checks pass. No dependency or manifest dependency changes were required.

Performance review: the note adds two subscriptions to existing shared roster/
transaction resources while Background sync is enabled on these pages. Valid
stored observations replay through the bounded worker store without I/O; cold
reads join the existing asynchronous database snapshot poll and reuse durable
observations, including their original freshness and completeness metadata.
The shared timestamp text is independent of the note and transfer-panel DOM.
No new timer, acquisition schedule, full transfer reconciliation, database write,
manual action, unbounded cache or list rendering is introduced. The known
worker/factual cache limits still apply; large cold snapshot transfer latency on
the deployed VM was not measured. Local fixture/browser checks do not certify
hosted appearance or production deployment. No real account or transaction was
used. Unrelated working-tree documentation and source edits remain excluded.


## Admin loading and boxed settings — 2026-11-15

Root cause reproduced with the actual Admin module: `fm_admin_timezones()`
returned a pending directory promise from its activation observer, holding the
whole Shiny session flush. With a synthetic five-second directory response,
settings appeared after 5,848ms. Returning immediately after installing the
existing completion/error callbacks removes that dependency without changing
the directory query, persisted preferences or failure semantics.

Admin now boxes App access, User timezones, League settings, Database
administration and Round awards. Each league category, including unresolved
reported settings, has a border and consistent label/value columns. Values
wrap on narrow screens; explanations retain native keyboard disclosure. This
is a layout and loading correction, with no new product capability requiring
a feature-comparison or login-tier expansion.

Executed verification:

- Existing `test_admin_league_settings.R` passes all six reported groups.
- `test_timezone_preferences.R` passes all six groups, including persisted
  acknowledgement, authorization/account isolation, duplicate-save protection
  and truthful failed writes; 1,000 parsed timestamp labels took 0.002s.
- Existing Admin awards/coach: 11 groups; initial award catalog: six groups,
  including acknowledged historical results reused in another session.
- Actual Admin browser fixture: 320/390/768/1440px, dark and light (eight
  configurations). Settings appeared in 815–842ms while a five-second timezone
  read remained pending; heartbeat round trips were 39–52ms. Navigation away
  and reopening worked before the response, then the directory/editor loaded.
  All category values share a column; all sections are bordered; disclosures
  work with Enter; no horizontal overflow, JavaScript or Shiny output errors.
  Phone dark and desktop light screenshots were inspected. Synthetic source
  and database responses only; live I/O is blocked in the fixture.
- Required `Rscript test/test_shiny_simulation.R`: 13 checks plus eight tabs
  pass, with zero HTTP/live database mutation attempts.

Performance review: the loader makes the same single bounded asynchronous
directory request per activation/reload, and does not introduce polling,
additional acquisition, database writes, caching or retained state. Existing
durable preferences and shared team/league/award data reuse are preserved.
Static singleton CSS, five section wrappers and bounded disclosure grids add
only layout work; callbacks update the existing outputs. No dependency, schema,
environment or manifest-package change. Documentation has no runtime effect.
Local timings establish responsiveness under the synthetic delay, not
authenticated hosted latency or deployment. Only task-specific source checksums
and documentation additions belong in the commit; unrelated edits remain.


## Stable balances and unchanged player refreshes — 2026-11-15

Reproduced the reported repaint against the committed code. The balance card's
class and full display projection changed on freshness/loading transitions even
with identical amounts; direct scheduled-job dependencies could also invalidate
the whole header for metadata-only polls. Player rows and metric endpoints could
compare unequal solely because native/worker values alternated between integer
and double, causing an unnecessary `updateReactable()`.

`round_finance_card_display()` now projects stable observed amounts independently
of the strict finance state. Freshness, actual read errors, unverified balances
and source timestamps update through the separate status detail. An observed
nonpositive balance stays visibly marked without asserting current kickoff
eligibility. Missing source data still clears known amounts. Rendered scheduled
purchase details are compared before publishing. Player rows/IDs/metric styles/
context/recent-N now publish atomically, with equivalent numeric encodings
normalized only for display. Raw action data, source timestamps, completeness,
expiry and existing manual preflights are unchanged.

Verification:

- New `test_unchanged_refresh_display.R`: eight groups covering ten repeated
  freshness/loading/success cycles, genuine failures, changed/negative/unknown
  amounts, scheduled-job metadata versus changed reservations, equivalent signed/
  zero player snapshots, actual player changes and 524-player refreshes. Both
  unchanged balance and table assertions failed against the pre-fix paths.
- Existing finance banner (14), debt/countdown (14), refresh stability (11), player
  filters (9), metric colors (10), shared roster note (3) and deployment (3) pass. Points
  ratio/trend fixtures and recent-average known answers pass, preserving signed
  scores, zero, missing values, history replacement and in-place history updates.
- Chromium at 320/390/1440px uses the actual header, real player table and typed
  worker snapshot codec. New versions/source clocks, two expiry/recovery cycles
  and a delayed database refresh produce zero header/table recalculation or value
  events and preserve DOM identity/color. Real balance/player changes appear;
  no JavaScript errors or horizontal overflow. Held database heartbeat measures
  215/161/163ms; ordinary heartbeat 62ms. Acquisition enqueue count remains zero.
- The required offline lifecycle passes 13 checks plus eight tabs with no HTTP or
  live DB mutation. Final isolated staged-code checks are recorded below.

Performance review: unchanged data stops before header rendering and the table's
schema hash/widget update work. A 524-player synthetic table handled ten unchanged
publications in 0.228s, retaining one 99.2KiB visible snapshot and sending zero widget
updates. The smaller ten table cycles measured 0.318s; thirty financial state
transitions measured 0.762s. These development measurements include reactive/test
flushes and do not establish Contabo production latency. Projection memory is
bounded by the currently loaded table; numeric normalization is linear in its
cells. Existing DB-persisted worker snapshots and bounded memory reuse remain;
no API/DB request, persistence write, invalidation, timer, dependency or worker
cadence is added. No new feature capability or login inventory change is needed.
Authenticated hosted appearance and deployment are unverified. No real account
or transaction was used; unrelated concurrent/working-tree edits are excluded.


Final isolated task-only staged export: new display regression (8 groups), metric
colors (10), deployment (3), full-server banner finance (14), refresh stability
(11), and required offline lifecycle (13 checks plus eight tabs) all pass. The
isolated 524-player measurement was 0.230s for ten unchanged publications, with
zero widget renders/updates. The export excludes unrelated local and concurrent
staged edits; no source or database service was contacted by these tests.


## Admin configuration freshness and authority — 2026-11-15

Two local causes of the reported warning were reproduced: the root conflated
pending/stale snapshots with an unavailable configuration, and the shared
factual adapter could expire a persisted worker observation at the memory
cache's 20-second TTL despite its later explicit source deadline. The adapter
now respects that deadline; it never extends it or renews the observation.
Missing deadlines retain the ordinary TTL, malformed/expired deadlines stay
stale, and interactive acquisition/manual preflight contracts are unchanged.
Admin receives the existing shared status/error/source/observation metadata.
Pending work follows the existing suppressed-information convention; expired
observations are labeled out of date with their original check time; actual
read failures retain safe diagnostics. An authorized scoped Reload settings
action uses the existing primary-team retry, suppresses pending duplicates
and preserves the database-only browsing boundary for opted-in accounts.
The read-only text uses the owner's requested distinction between league and
LigaRadar administrators and points to the official Futmondo app.

Read-only production investigation: health advisories were empty; the service
heartbeat was current. Stored team observations had 60-second deadlines, with
real expired periods between collector turns. Rival jobs also had explicit
source_rejected failures; those are not evidence that the active own-team
request failed. The active resolved own-team job acknowledged another success
at 2026-11-15 15:16:28 UTC with no source error. No production credentials, raw
private responses, transactions, schema changes or queue writes were used.
The initial project-list connector call failed transiently; scoped metadata
queries worked. These observations do not establish continuous hosted
freshness: genuine collection delays remain truthfully stale, and this task
does not claim to repair every rejected rival endpoint or worker cadence gap.

Executed checks: Admin configuration state/recovery (two groups using the real
root projection and module), existing league settings (six groups), worker
expiry (five groups, including a 21-second-old snapshot still valid until its
original 60-second deadline), database-only GUI reads (two cold sessions,
persistence replay, repeated polls and recovery), source-expiry contracts
(seven), worker provider (eight reported assertions) and coach recovery (nine).
The required offline lifecycle passed 13 checks plus eight tabs with zero
HTTP/live database mutation attempts. Eight Chromium configurations cover
320/390/768/1440px and both themes, the exact administrator explanation,
aligned boxes, disclosure keyboard access, navigation under a five-second
held directory read, and no overflow or output errors. Settings appeared in
817–835ms and held-request heartbeats took 42–54ms.

Performance review: constant-time deadline/metadata checks reuse existing
resource state; no timer, cache, source request, DB write or acquisition job is
added to automatic rendering. Explicit reload triggers one existing scoped
read, coalescing pending work; opted-in sessions reread durable snapshots
without scheduling acquisition. Existing cache/registry/queue limits and
account/league/team/season/scoring isolation remain. Rendering is bounded to
the existing settings rows plus one compact source disclosure. No dependency,
schema or environment changes; documentation has no runtime effect. Source
checksums are updated without unrelated manifest/package changes. Local
checks and branch delivery do not establish deployed appearance or eliminate
genuine external/source latency. Unrelated working changes are preserved.


### Player chart date labels without years — 2026-11-15

Changed only the date tick format from DD/MM plus a second-line year to DD/MM
at the owner's request. Full source dates/years remain in hover details.
Chart contracts pass (16), including a known year in valuation hover text;
the exact staged module passes the same checks. Browser checks at
320/390/1440px across four history modes and both themes (24 combinations)
assert day/month-only ticks without overlap and preserve series-colored axes,
source data and zoom. Deployment contract: 3 passed.

Performance review: fewer displayed characters and no second year line; no
additional source/API/database work, cache/invalidation, persistence, reactive
update, timer, listener or retained state. Existing durable history reuse and
source timestamps are unchanged. This is a local chart-format edit; shared
theme/startup/infrastructure are untouched, so the focused chart/browser checks
are appropriate and the lifecycle harness was not repeated. Documentation has
no runtime effect. Synthetic checks do not verify production deployment or
physical-device behavior; no real accounts or transactions were used.


## Personal timezone ownership and Spain default — 2026-11-15

The owner clarified that users choose their own timezone and an unset choice
means Spain. The former Admin directory/editor and `fm_admin_timezones()` are
removed. `fm_timezone_store()` accepts only personal get/set operations and
rejects foreign accounts even for a LigaRadar administrator before I/O.
`fm_timezone_preferences()` starts one immediate async GET; a missing row uses
Europe/Madrid without an automatic write. Explicit source=user choices are
reused; legacy browser/admin values remain stored but do not define a personal
choice. This avoids retaining an old automatic browser value as if the user
selected it. Explicit saves always use source=user and require a matching
account/zone/provenance acknowledgement. Source instants and DST formatting
are unchanged; no unknown date or score is manufactured.

Automatic browser-timezone publication and the cancellable one-second fallback
are removed. Both loading and saving yield to Shiny; duplicate writes and late
replies after account changes/logout remain guarded. Failed/unacknowledged
saves require authoritative readback before another change. Personal defaults
also feed the existing initial quiet-hours input; already-saved background
scheduling settings remain independent. The database schema and restricted
legacy RPCs remain compatible, with no migration or production rewrite.

Verification: timezone regression passes seven reported groups, including DST/
offset known answers, Spain despite a foreign browser, explicit save/reload,
legacy provenance, self-only admin authorization, held read/write response,
uncertain-save readback and logout isolation. Formatting 1,000 parsed dates
took 0.001–0.002s. Personal Chromium at 320/390/1440px verifies Spain in a Los
Angeles browser despite a legacy auto value, two independent users' saves,
acknowledgement before display, logout/reload persistence, no other-user editor,
no geolocation or overflow, and held-save heartbeats of 86/75/91ms. The 390px
screenshot was inspected. The browser fixture's initial failures were its own
reactive URL access and readiness wait; correcting these required no additional
application behavior change.

Eight Admin width/theme configurations pass with four sections, consistent
setting-value columns and zero timezone-directory reads; settings appeared in
812–834ms and heartbeats took 39–103ms. Existing Admin state/recovery (two),
league settings (six), login access (nine) and coach recovery (nine) pass.
The offline lifecycle passes 13 checks plus eight tabs with zero HTTP/live
database mutation attempts. All verification uses synthetic personal records;
no real account, transaction or live preference write was used.

Performance review: one bounded asynchronous preference GET per login/retry and
one write only for an explicit save; unset defaults do not write. Removing the
Admin directory eliminates its activation/search/pagination reads and controls.
The browser no longer reports timezone on connection/visibility events, and
there is no initial one-second wait, new timer, polling, API acquisition, cache
or queue. Existing bounded timezone choices and responsive controls are reused.
Documented interfaces, setup and source checksums are updated; no dependencies,
schema or environment changes. Documentation has no runtime effect. This is a
correction/removal within existing display preferences, with no new comparison
or login-tier capability. Local checks and branch delivery do not verify hosted
rollout, physical devices or production network latency. Unrelated working-tree
and staged changes are preserved.


## Detected personal timezone default — 2026-11-15

Final owner clarification: detect the browser timezone as the default, then
let each user override it. `fm_timezone_preferences()` keeps its immediate
personal database lookup and adds a guarded detection observer. Until an
explicit source=user choice is acknowledged, the current valid detected IANA
zone supplies the default. Missing/invalid detection falls back to Madrid.
A delayed database choice replaces provisional detection; subsequent browser
changes cannot overwrite the saved choice. Legacy browser/admin rows remain
stored without pinning a default from another device/person. Admin cross-user
controls and authorization remain removed. No default row or source observation
is written automatically; explicit personal choices retain database persistence
and authoritative readback after uncertain saves.

`www/worker-state.js` reports `Intl.DateTimeFormat().resolvedOptions().timeZone`
on connection/visibility changes, without coordinates or geolocation permission.
Equal Shiny inputs do not republish. The existing client countdown is unchanged.
There is no wait for detection or fallback timer; a late detection changes only
the unset session default. Source dates, UTC instants and DST rules are unchanged.

Executed checks: timezone regression passes nine reported groups, including
known-answer DST/offset formatting, detected and late defaults, invalid/missing
detection, delayed saved-read precedence, persistence, self-only authorization,
uncertain writes, duplicate guards and logout/account isolation. One thousand
parsed timestamp labels took 0.001s. Synthetic Chromium at 320/390/1440px
confirms Los Angeles detection, saved Madrid overriding that browser after
reload, two independent account choices, no Admin editor, no geolocation, no
overflow or output errors, and held-save heartbeats of 82/77/86ms. An additional
browser fixture makes detection unavailable and confirms Madrid fallback with
zero default writes. The 390px screenshot was inspected.

The required offline lifecycle passes 13 checks plus eight tabs against the
exact task artifact, with zero HTTP/live database mutation attempts. Deployment
checks pass all three groups and verify exclusions, unchanged dependencies/file
inventories, and the two changed runtime checksums.
No real accounts, transactions, production preference writes or deployment are
used for local verification. This default correction introduces no new product
capability or login-tier entry.

Performance review: one existing bounded asynchronous preference GET per
login/retry and writes only on explicit Save. Detection adds one guarded
observer and bounded connection/visibility input publications, with zero
detection-triggered DB/API operations, timers or polling. Matching values do
not redraw; changed unset defaults update existing account-local outputs.
Existing cache/queue bounds and responsive selector are retained. Saved worker
quiet-hours settings stay independent. No dependency, schema or environment
change; documentation has no runtime effect. Hosted appearance, physical-device
behavior and network latency remain unverified. Unrelated working/index edits
are preserved.


## Visible offer submission errors — 2026-11-15

The market and direct-owner submission handlers already emit readable, persistent
`.fm-offer-error` messages. The shared `www/info-disclosures.js` notification
compactor hid that content inside a closed red information icon. Exempt this
explicit markup and remove the compact layout if Shiny reuses a previously
compact notification ID. Existing translation, safe unknown/uncertain-response
wording, duration=NULL, dismissal, preflights and submission behavior are intact.
No new product capability or login inventory entry is introduced.

Verification: the new synthetic browser regression fails before the fix because
the owner error exists but remains hidden. After the fix, all six Chromium layouts
(320/390/1440px, light/dark) pass immediate market/owner error visibility, no icon,
long-text wrapping/no horizontal overflow, reused notification ID handling,
persistence beyond five seconds and explicit dismissal. Existing disclosure
keyboard/ready/pending/error and replacement behavior still passes. Inspected the
390px offer-error screenshot. The fixture waits for prior dismissal animations
before reusing IDs; activation uses keyboard when test notices cover buttons.
The runner accepts the existing optional FUTMONDO_TEST_CHROMIUM executable override
because its default Playwright browser revision is unavailable locally.

Executed `Rscript test/test_offer_error_messages.R` (all existing translation,
readable-message, unknown-code and uncertain-response assertions pass),
`Rscript test/test_deployment_contract.R` (three groups pass), and
`Rscript test/test_shiny_simulation.R` (13 checks plus eight tabs pass; zero HTTP
or live database mutation attempts). Browser fixture backend I/O remains blocked
and records zero attempts; no external requests or browser errors are observed.
The deployment manifest retains package/file inventories and updates only the
changed script and README checksums relative to its starting working copy.
No dependency, environment setup, schema, source-data or durable cache change.
README documents the optional test browser executable override.

Performance review: one bounded descendant-marker lookup per candidate error
container, using the existing local mutation observer. No added timer, polling,
API/database read/write, cache invalidation, server reactive work or retained
collection; no new disclosure DOM is constructed for offer errors. A 150-node
mutation burst has 10–12ms maximum browser timer gaps, and Shiny heartbeat round
trips take 56–59ms across the six layouts. Mobile text wraps within the existing
viewport-bounded panel. Documentation has no runtime effect. Production rollout,
physical devices and real authenticated offer responses remain unverified; no
real accounts or transactions are used. Unrelated working-tree/index edits are
preserved and excluded from the task commit.


## Permanent Welcome loading and blank League settings — 2026-11-15

Both reported paths were reproduced through the actual root application.
Welcome's readiness projection unconditionally returned false on the login
page even when shared team/market states were both ok; startup consequently
remained loading_today. Separately, `fm_worker_session()` returned its pending
renewal/settings promise through an observer, holding Shiny's whole session
flush. With an opt-in policy and a five-second synthetic renewal, opening
Admin showed zero setting cards and a heartbeat took 3,879ms; after renewal,
the same existing nine-field configuration rendered six groups. The original
standalone module fixture lacked this root worker-controller path.

The root now uses the actual primary readiness/status on Welcome and Today.
The worker's local authorize helper returns immediately after installing its
existing asynchronous callbacks. No read/write is treated as acknowledged
early; failure feedback, self/account checks, consent, policy generation and
durable preferences remain unchanged. Actual market failures become partial/
failed status and scoped retry on Welcome, with unavailable values retained.
No API result, cache, source deadline or worker schedule is changed.

Read-only production metadata showed a current worker heartbeat and recent
successful market/team observations. Existing retained rule records contained
48 scalar configuration keys, matching the adapter's mapping, with 546 own-team
rule observations. Some short-lived snapshots were genuinely expired; that
status remains separate from the reproduced stuck banner/flush. Raw private
worker snapshot access was rejected by automatic approval review; investigation
continued with aggregate metadata and synthetic fixtures, without inspecting
private response bodies, changing production data or accessing real accounts.
No universal source outage is inferred from the UI symptoms.

Executed focused checks: three root Welcome groups (already ready, held
market response, real rejection followed by successful scoped retry); ten
unrelated input publications added zero reads in 0.321s. The actual worker
controller test holds renewal, acknowledges an explicit disable and then
rejects the older policy reply, with no duplicate renewal or live transport.
Existing Admin configuration recovery (two groups), league settings (six),
access control (nine) and database-only worker GUI/replay regressions pass.
Full production UI/root browser checks at 320/390/1440px hold renewal for five
seconds: settings render in 844/843/828ms and heartbeat responses take
373/59/260ms before acknowledgement. All six boxes retain known values;
Welcome settles; one renewal occurs per login; no horizontal overflow, visible
output/JavaScript error or live I/O. The 390px screenshot was inspected.

The exact task artifact passes the required offline lifecycle (13 checks plus
eight tabs, zero HTTP/live database mutations) and all three deployment checks. Two runtime checksums are updated, with unchanged
dependencies and application-bundle inventory. No real transaction, live
preference write, production schema change or deployment is used in tests.

Performance review: primary readiness subscribes to existing shared resource
projections; repeated settled Welcome inputs add no reads. Existing post-startup
consumers can proceed once primary data settles. Promise completion uses the
same bounded transport, account policy and durable database writes as before;
only the UI flush is released. Existing generation checks prevent stale policy
replies from restoring disabled access. No new timer, acquisition job, cache,
queue, source request, dependency or environment variable is introduced.
Rendering cost remains the existing bounded settings rows. Documentation has
no runtime effect. Synthetic timing does not establish Contabo network/capacity
or authenticated hosted acceptance; rollout remains unverified. Unrelated
working-tree/index changes and concurrent task commits are preserved.


## Market-offer percentage buttons — 2026-11-15

Place Market Offer now offers +0.5%, +1%, +2%, +4%, +5% and +10%, matching
sale-listing shortcuts. Each button sets the editable offer from the verified
minimum bid plus the rounded-up euro premium; repeat clicks and manual edits
do not change that base. The existing live currency preview receives the
numeric input's normal change event. Availability, account/league/player
isolation, current funds/capacity validation and explicit one-shot submission
remain unchanged. No source response, stored observation or cache is introduced.
The official-source comparison and its current-UI limits are recorded in
`feature_comparison.md`; this is an editor convenience, not a new login tier.

Executed `Rscript test/test_market_offer_opening.R`: 15 checks pass, including
known amounts for a €150 minimum (€151/152/153/156/158/165), held availability,
zero additional opening reads, rejected/duplicate submits and switched contexts.
Pending shell opening took 50ms in the focused run. Executed
`Rscript test/test_shiny_simulation.R`: 13 checks plus eight tabs pass, with
zero HTTP or live database mutation attempts. Executed
`Rscript test/test_deployment_contract.R`: all three groups pass.

`test/test_market_offer_premium.cjs` uses the existing HTTP-blocked synthetic
root app at 320/390/1440px in dark and light themes. Coverage includes all six
buttons and currency previews, noncompounding repeat clicks, an arbitrary
manually edited amount, cancellation/reopening at the minimum, and modal/page
width bounds. Maximum click-to-preview times per layout were 62–67ms. No offer
is submitted. The 390px screenshots in both themes were inspected. The runner supports the existing FUTMONDO_TEST_CHROMIUM override;
local Chromium is used because the default bundled revision is unavailable.

Performance review: six fixed buttons are rendered once when the existing
modal body becomes ready. Each click changes one numeric input and its existing
currency-preview output; the body does not depend on the edited amount. No new
API/database request, persistence task, invalidation, timer, queue or growing
collection. Existing durable acquisition reuse and source deadlines are
unchanged. Buttons wrap on mobile. Documentation has no runtime effect.
No dependency, environment setup or schema changes; only this task's runtime
module checksum is updated in the deployment manifest. Unrelated working-tree
and index changes are excluded. Production rollout, physical devices and real
authenticated offers remain unverified.

## Consolidated page information panels — 2026-11-15

Implemented and verified locally: one collapsed native panel beside every page
title, plus one per dialog containing passive information. Global finance and
worker notes share the active page's panel. Each source retains its own error
color; aggregate severity clears only after the last failed source recovers.
Empty/pending entries are omitted independently. Inline action validation,
transaction notifications, confirmations and official inbox records remain
visible. Numerical finance fields and source timestamps retain their semantics.

Executed checks:

- `Rscript test/test_page_information.R`: all eleven pages render as a full
  HTML document with dependencies preserved, one matching heading, source labels,
  visible action validation and identical healthy metadata republished after a
  context revision.
- `node test/test_page_information.cjs`: six Chromium layouts (320/390/1440px,
  light/dark), exercising independent colors, multiple errors, recovery while
  collapsed, empty/pending-only scopes, delayed sources beside healthy sources,
  unchanged source-clock DOM, coach-layout updates while collapsed, Retry
  binding across repeated replacements,
  dialog close/reopen and one-click Retry, generated table-error recovery,
  source removal, account/league-style reset, native keyboard controls,
  long diagnostic wrapping and no overflow.
  Zero source/database I/O attempts or external requests; browser mutation gaps
  were 10ms and Shiny heartbeats 61–68ms in the final run.
- `node test/test_page_information_app.cjs` against the existing
  `Rscript test/test_browser_app.R` synthetic server: nine actual app pages at
  320/390/1440px, with exactly one populated panel each, no overflow and no
  browser errors. This verifies UI integration/loading, not live source data.
- Existing `node test/test_info_disclosures.cjs`: six layouts passed, including
  standalone disclosures, empty/pending states, tooltips, source-clock identity,
  keyboard actions and full persistent market/direct-owner offer failures.
- `Rscript test/test_info_visibility.R`, `test/test_player_info_disclosures.R`
  and `test/test_worker_status_redraw.R`: pending/empty/success/failure contracts,
  preserved factual values and no repeated source reads or worker-driven
  redraws on unchanged polls. Final focused measurements: 1,000 nested helper
  renders in 0.206s; 100 offer-panel renders in 0.008s; twenty unchanged worker
  polls in 0.333s with zero repeated output renders or source reads.
- `Rscript test/test_lineup_module.R`, `test/test_market_offer_opening.R` and
  `test/test_ui_reliability.R`: passed. A stale lineup test assertion expected
  pending coverage disclosure text; it failed identically with the original
  HEAD helper/module. Updated that assertion to require its omission while
  retaining the existing visible waiting-action notice and draft checks.
- Required `Rscript test/test_shiny_simulation.R`: 13 lifecycle checks plus
  eight tabs passed, with no HTTP/live database mutations. JavaScript syntax
  and task-scoped whitespace checks passed.
- `Rscript test/test_deployment_contract.R`: three checks passed. Eight
  changed runtime checksums match the exact staged source blobs; development
  files remain excluded. Only task checksum fields are staged; unrelated
  manifest package and source edits remain outside this commit.

The browser executable came from the existing local Chromium cache using
`FUTMONDO_TEST_CHROMIUM`; no dependency installation or deployed dependency
change was required. Inspected the mobile full-app image and mixed-error dark
panel image. The first full-document browser run exposed loss of custom list
classes during heading traversal; fixed it and added the document-rendering
regression above. A synthetic delayed callback is guarded against a closed
session; fixture teardown cannot publish into a destroyed session.

Performance review: no new source/API/database request, acquisition timer,
worker work, persistent cache, schema or reactive input is added. The registry
adopts existing bound nodes once, restores them before owning-output replacement,
coalesces local DOM changes per animation frame and prunes removed scopes and
sources. Source-clock-only changes preserve panel/source identities and open
state. Context revisions republish metadata once per account/league change,
including when its text is identical; they do not change acquisition policy.
No full-document scan occurs on normal updates. Mobile wrapping and the bounded
mutation/heartbeat checks found no responsiveness regression. Reusable factual
results still use acknowledged durable database persistence with original
context/source/freshness/completeness metadata; memory is only an accelerator.

No production deployment, real-account operation, live transaction, real-device
Safari test or hosted acceptance is claimed. Existing unrelated working changes
are preserved. Documentation changes have no runtime performance effect.

## Faster player-card availability across selections — 2026-11-15

Implemented locally for the reported wait on every card. Team verification now
belongs to the root interactive session and is shared across player cards and
offer dialogs. Selected-player summaries and own-team listings use the direct
interactive fallback when worker evidence is missing or expired. History-only
refreshes and metadata redraws do not repeat healthy team checks. Retry checks
only missing/expired sources; transaction confirmations and final independent
manual preflights retain their existing guards.

The existing private evidence table/view stores reusable successful facts with
exact account/league/team/competition/season/scoring/player context, original
source times/deadlines, completeness and integrity metadata. Durable summaries
require returned player identity; team observations require complete transfer
and financial proof. Independent persisted invalidation markers survive process
cache clearing. A failed save remains retryable and is never called durable.
No schema, dependency, environment variable, worker schedule or deployment is
changed. Contract: `player_availability.md`.

Executed checks:

- `Rscript test/test_player_availability_reuse.R`: held requests across player
  switches, warm known-summary/team reuse, obsolete-context rejection, finite
  completion after card closure, one explicit refresh, duplicate retry joining,
  history-refresh isolation, strict expiry, summary-only retry and own-listing
  retry/reuse/20-second expiry. No transactions. Local switch/flush was 0.333s.
- `Rscript test/test_player_availability_store.R`: exact signed-point and asking-
  price round trips; all seven context mismatches; original expiry; integrity,
  secret-field, missing player identity and incomplete-transfer rejection;
  invalidation after memory clearing; failed acknowledgement followed by retry;
  verified empty listings distinct from missing data; session cleanup isolation.
  Known-answer calculated funds retain the original 15-second deadline. A cold
  provider restores through the real cooperative adapter with one synthetic
  database GET and zero source calls. Deferred JSON prepares and restores a valid
  snapshot, with acknowledgement required before marking it durable.
- Actual `test/test_worker_card_availability_browser_app.R` and
  `node test/test_worker_card_availability_browser.cjs` at 360/1440px: final warm
  action readiness was 282/293ms, with zero additional team checks. A held target
  summary bypassed the worker wait and restored actions after completion.
  Original pending, network/partial failure, Retry, offer-editor and no-overflow
  checks pass; pending-request Shiny heartbeats were 63–66ms. These are synthetic
  local measurements, not a production or official-app comparison.
- `Rscript test/test_player_card_during_saves.R`: four checks pass; seven held
  history saves neither delay card opening nor restart availability when their
  acknowledgements arrive. Its mock now accepts/asserts the interactive bypass
  arguments. The browser error-state assertion waits for the complete settled
  DOM instead of inspecting two successive output replacements between frames.
- `Rscript test/test_worker_gui_readonly.R`,
  `test/test_worker_manual_offer.R`,
  `test/test_worker_card_availability_recovery.R`,
  `test/test_card_live_capacity_recovery.R`,
  `test/test_card_availability_freshness.R` and
  `test/test_card_capacity_recheck.R`: worker replay and expiry, no recurring
  source reads from worker publications, shared offer checks, context rejection,
  slow-source freshness repair and strict original deadlines remain covered.
  A two-session owned-card fixture exposed account-wide worker revision signals
  tied to the first session's reactive domain; creating those shared signals
  outside a session domain fixes reads after that session closes.
- Existing market-offer opening, existing-offer controls, configured acquisition
  rules, parallel acquisition, transport priority and worker-status redraw
  checks pass. No transaction retry policy or concurrency budget changes.
- Required `Rscript test/test_shiny_simulation.R`: 13 lifecycle checks plus eight
  tabs pass, with no HTTP or live database mutation attempts. Deployment contract,
  JavaScript syntax, scoped whitespace and staged runtime checksums are checked
  before commit; development files remain excluded from the deployment bundle.

Performance review: no polling or worker-cadence change. Valid worker/native
observations avoid database reads; a new uncached scope makes at most one
bounded database lookup before the existing source acquisition. Pending requests
are joined and warm switches do not dispatch another team adapter. Snapshot
writes occur for successful live observations and relevant invalidations through
the existing bounded persistence queue. Retained provider data is limited to
64 entries, 8 MiB/result and 16 MiB/root session; eviction excludes pending saves
and active consumers. Existing native caches keep separate limits. Timing data
uses fixed labels in the existing 64-sample ring and contains no identifiers.

A representative synthetic summary (76 rounds, 365 price records, 239,928 decoded
bytes) originally took about 196.5ms per synchronous save preparation. The final
finite deferred preparation measured about 446ms total with a 162ms maximum
heartbeat gap and 49 heartbeat ticks, moving preparation after the ready-output
flush and yielding during JSON emission. This reduces uninterrupted work at the
cost of total encoding CPU time. The primitive encoder and parsing still include
bounded synchronous work; this is not a background-process implementation.
Already-durable worker replay skips that preparation entirely. A truly cold
card still depends on required source/database latency and complete transfer
history; no deadline is extended and no missing fact is replaced with zero.

The exact staged snapshot was also checked in an isolated directory: the new
persistence/reuse regressions, three deployment checks and the full offline
lifecycle harness pass independently of unrelated working-tree changes. All
seven staged runtime checksums match the staged source blobs.

No production deployment, real-account operation, live transaction, production
latency measurement or hosted acceptance is claimed. Unrelated working changes
are preserved. Documentation has no runtime performance effect.


## Dani Lorenzo / full player valuation-history repair — 2026-11-15 Pacific

Root causes: the chart assembled only seven-day player-summary prices and saved
current-price observations, ignoring the already requested full-profile curve.
Card Refresh omitted the full profile, and a pending read surviving a previous
opening could coalesce with Refresh before reusing an obsolete completion. The
price DB resource also used the general card context instead of resolved history
context. Read-only hosted metadata found 627 Dani Lorenzo price snapshots from
2026-08-16 through 2026-11-16 UTC, all with unknown competition/season, and a
successful full-profile worker snapshot. Those unscoped historical observations
were retained without relabelling. Automatic approval review rejected export of
the encoded profile payload as private data; no workaround, source account,
transaction, production database write or deployment was used.

The card now consumes context-checked full-profile curves, verifies all exact-date
overlap prices against social/classic values and any explicitly reported divider,
and keeps mismatches/ambiguity truthful. Raw price curves and source metadata
persist via the existing server-only immutable evidence store and acknowledged
queue, with exact context keys, 5,000-row/220-KiB limits and no authentication
material. A scoped latest-view read restores them before the live profile;
shorter responses retain older prices and their original observation clocks.
Refresh evicts both inner and outer profile resources and now includes that
existing full-profile read. No separate source-history endpoint, worker
job/cadence, transaction, schema, dependency or setting is added.

Executed checks:

- `test_player_valuation_history.R`: 14 groups. Known-answer 71 vs seven dates;
  independently captured official price fields; social/classic/divider mapping;
  identity, all seven scope dimensions, JSONB key ordering, signed/unknown
  prices, ambiguous/missing overlap, exact fractional dates, partial/stale data,
  database reuse after memory clearing, shorter responses, actual card arrival,
  refresh and late-completion isolation.
- `test_player_valuation_persistence.R`: real queue/adapter fixture. Identical
  source-time payload retried after HTTP 503, acknowledged only after success,
  duplicate acknowledged write skipped. Cold DB replay used two reads (empty
  current-price history plus one scoped latest-snapshot lookup), zero Futmondo
  reads, and a heartbeat during the injected 50ms total DB delay; local elapsed
  0.44s including adapter initialization/replay.
- `test_progressive_loading.R`: 16 checks, including profile inner-cache eviction,
  outer pending-read refresh, preserved unrelated sources, reopening and loading.
  Updated its fixture to accept the existing availability adapter's keyword
  arguments, the existing hidden pending-detail presentation and shared team
  request lifetime; no production availability policy changed.
- `test_player_trend_plot.R`: 16 checks; `test_player_profile_endpoints.R`: six;
  `test_player_history_incremental.R`: unchanged four-to-two incremental request
  behavior and delayed responsive replay; independent history known-answer pass;
  `test_history_persistence_recovery.R`: 11; `test_competition_history.R`: 15.
- `test_player_trend_browser.cjs`: 320/390/1440px, four history modes and both
  themes passed; no overflowing date labels, source/zoom/color loss, visible
  Shiny/JavaScript errors or unresponsive heartbeat (74–85ms). Used the existing
  local Chromium executable through a temporary launcher adapter because the
  installed Playwright default browser revision was absent; no app dependency
  changed.
- Required `test_shiny_simulation.R`: 13 lifecycle checks and eight tabs passed,
  with zero live HTTP/database mutation attempts.

An additional `test_worker_card_updates.R` run fails its old exact summary-read
count; the same failure is reproduced with the unchanged HEAD player module.
That fixture predates the current shared availability flow. The focused actual
card history/refresh checks above pass; this pre-existing fixture failure is
reported without claiming it passed.

Performance review: history reuses the already requested profile and existing
bounded caches. It adds one limit-one DB snapshot lookup for resolved contexts,
and one bounded immutable write per newly acquired eligible curve; duplicate
payloads coalesce/skip through the existing queue, with failed writes retryable.
A vectorized ISO-date parse keeps the 5,000-row transformation at 0.087–0.099s
locally with about 581 KiB of returned data. The actual source fixture has 71
prices; browser checks retain 124 prices. There is no per-second work, polling,
new listener, broad cache invalidation or duplicate native profile acquisition.
The independent profile still refreshes its current facts; stored historical
replay does not pretend that old biographical data is current. Documentation has
no runtime cost. Manifest entries include the new runtime helper and changed
source hashes while preserving unrelated dependency edits. Hosted latency,
production snapshot-save/readback and Dani's exact post-deployment chart remain
unverified; the automatic-review export rejection limits that final replay.


## Authorized Dani Lorenzo source replay — 2026-11-15 Pacific

The owner explicitly approved locally decoding the saved player-profile snapshot
to verify his exact price history. The initial automatic-review export blocker
reported above is resolved. Only the relevant player's saved profile/summary
snapshots and their source metadata were retrieved; no authentication material
or full private response body was printed or added to the repository. Compressed
payload SHA-256 and decoded byte lengths were checked before the existing safe
worker decoder ran. All source/database network functions were blocked during
local replay. Temporary encoded and decoded private files were removed afterward.

Verified source evidence:

- Full-profile snapshot observed 2026-11-15 08:16:09 UTC: 133 daily prices,
  from 2026-07-06 through 2026-11-15. The source's player, competition and
  season identities all match the saved league summary context.
- Summary observed 2026-11-15 12:20:10 UTC: seven prices, November 9–15.
  All seven exact source timestamps overlap and every price matches the
  profile's social valuation, not its classic valuation.
- `normalize_card_valuation()` returns all 133 values with no diagnostic,
  preserving the complete available curve. Latest source and chart value:
  **10,027,832 EUR**, matching the reported card screenshot.
- The actual `player_trend_plot` expression was built with Plotly using this
  curve. Its trace retains all 133 prices and all expected account-local date
  strings on the Date axis. Stored instants were compared before presentation;
  plot dates use the existing account-timezone civil-string formatter. Local
  Plotly build took **0.106s**. No runtime changes were needed after this replay.
- The price-only persistence projection passes returned-row completeness and
  size checks: 133 rows and **17,322 JSON bytes**, below the 220-KiB limit.
  Existing database snapshots supplied this inspection; no new Futmondo request,
  production database write, transaction or source job was performed.

This establishes that Dani Lorenzo's missing earlier curve was available in the
saved source and is included by the corrected code. It does not certify every
possible season date, a production write acknowledgement or a deployed-browser
render. Hosted visual acceptance remains open. This follow-up changes only
verification documentation, with no runtime performance effect; no additional
application tests are required for the documentation edit. The earlier focused
regressions, asynchronous persistence/replay and lifecycle results remain in
force.


## Player availability reused from table/session — 2026-11-15 Pacific

The owner clarified that selecting a player must reuse recent table facts and
successful checks already made in the session. The reported sequence was a
roughly 30-second wait, brief apparent success, then Retry and “expired before
completed.” Code review reproduced an expiry-driven repeat check after a late
summary/reopening, replacement risk from unusable stored publications, and a
worker-mode observer that could miss the first expired live completion. The
owner's private request timing was not captured, so no particular source is
asserted to account for their entire 30 seconds.

Implemented behavior and source integrity:

- Recent explicit table ownership/listing facts (under 60 seconds) admit card
  controls without a team-capacity request or waiting for historical points.
  Market/catalog joins retain the older source clock and completeness. Unknown,
  old, partial and future observations cannot qualify.
- A team/target acquisition that was valid on completion receives a scoped
  `session_verified` display marker. Source expiry, worker publications and
  reopening/switching no longer repeat successful checks or turn them into a
  Retry loop. Context/action invalidation and explicit Refresh remain separate.
- The marker admits editor entry only. No preflight uses a past/fabricated clock,
  and original financial source deadlines are unchanged. An expired market
  editor joins one current asynchronous verification and keeps Submit disabled
  until it succeeds. Every transaction retains its final current-state preflight,
  immutable context, duplicate protection and uncertain-response reconciliation.
- Real initial failures retain Retry. An initial completion that never yielded
  usable facts can receive one bounded repair; failed explicit retries and
  ordinary worker expiry cannot become an acquisition loop.
- Cold acquisition takes short-lived funds/offers after history/roster/transfer
  candidates. Existing source/database caches are reused first. Original source
  clocks, exact scopes, successful database snapshots, acknowledgement handling
  and bounded retry queues remain intact. A session marker is not durable
  authorization and cannot make an expired database observation current in a
  later session. No schema, dependency, environment variable or timer is added.
- Points are attributed to Futmondo. Both “About these points” and the redundant
  “Player history” source label are absent from that disclosure. Other source
  headings, points and finality remain unchanged. This corrects an existing
  workflow/presentation, without adding a product capability or login-tier entry.

Validation:

- Eight availability scripts pass: completion-window/session/table/strict-submit
  reproduction, shared reuse, durable store replay, late-summary reuse, worker
  recovery, actual-card/native recovery, slow-history freshness and alternating
  source-delay limits. The new reproduction covers 90-second expiry/reopening,
  zero table-triggered capacity reads while history is held, explicit editor
  verification, failed final purchase preflight, old/future/unknown join clocks
  and bounded failure repair. No account or transaction is used.
- Seven additional suites pass: market-offer opening, acquisition availability,
  configured acquisition rules, championship columns (11 groups, including the
  actual join's clocks/completeness), capacity completeness, trend plots (16)
  and deployment exclusions (3). Persistence replay still uses one database GET
  and zero source requests; failed acknowledgements remain retryable.
- The consolidated-information browser suite passes six layouts (320/390/1440px,
  both themes), including an explicitly unlabelled Futmondo points note and
  retained Retry bindings. Heartbeats: 52–67ms; browser scheduling gaps: 10ms;
  zero source/database operations from disclosure interaction.
- The actual-card browser checks at 360/1440px pass held-source responsiveness,
  session expiry with zero new card acquisitions, current editor checks with
  Submit disabled, recovery, context changes, and no page errors/overflow.
- Required offline lifecycle: 13 checks plus eight tabs pass, with no live HTTP
  or database mutation attempts.

Performance review: a 31-second synthetic history read now requires one balance,
one market, two roster and two pressroom requests (six total), compared with
eight previously; the fresh and session-reused card paths add zero team reads.
A 20-second final market delay still repairs expired funds, preserving the
original ten-second remaining roster deadline; pathological alternating delays
stop at the existing finite limit. Session state adds only scalar markers to the
existing bounded registry. Joined timestamp/completeness metadata adds no I/O;
100 joins of 527 rows took 0.015s locally, retaining about 110 KiB. Deferred
persistence replay yielded with a measured 31ms maximum heartbeat gap. No extra
poll, redraw clock, unbounded cache, background job or automatic transaction is
introduced. Documentation-only additions have no runtime cost.

One additional legacy `test_player_card_loading_work.R` run fails its initial
exact-six-read assertion. The identical failure is reproduced with the unchanged
HEAD selected-player module; its mock adapter predates the shared availability
contract. It is not reported as passing. Current held-source card/browser,
scoped reuse and lifecycle checks above verify the affected behavior directly.
Production deployment, authenticated request timing and hosted acceptance of
Dani Lorenzo remain unverified; these local results do not certify them.


## Mobile upper-half cloud, upward dissolution and goal-facing figures — 2026-11-15

The fullscreen launch on portrait viewports up to 767px now reserves the upper `50dvh` (`50vh` fallback), accounts for safe areas, and places the brand/status immediately below. Canvas sizing measures both dimensions, preserves uniform pitch/figure scaling and gives the mobile flock approximately 90% width with a fuller vertical envelope. Desktop, landscape and compact banners retain their dimensions. Across all sizes, green/gold futbolín figures face opposite goals along the pitch length. Their shirts span pitch Y, with shallow depth along X, shaded side faces, forward kicking feet and nose cues. Per-player camera-eye selection displays the correct front/back face. Cached volume faces clip by local Z as figures dissolve from head to foot; preassigned particles rise along curved paths into the sky from 4.5–6.2 seconds. The camera eases to rest at takeoff. Fit-aware pitch/shadow positioning and radar geometry prevent clipping in short portrait scenes. Reduced-motion CSS immediately overrides the drawing marker, including after rotation. Public helper and startup/data interfaces are unchanged.

Final `node test/test_loading_visual.cjs` passed ten phases in dark/light at 1440/390/320px, 22 filled figures with eleven per team, shoulders aligned with the pitch lateral vanishing point, opposite goal-directed toes/noses in hero/compact/fullscreen scenes, head-first upward release, matching particle/radar tones and exact geometry/bitmap loop continuity. Its realistic fullscreen waiter fixture passed 320×568, 390×844 and 430×932 in both themes at simulated DPR 3 (capped to 1.5), including later radar/return stages and the seam. Short portrait 767×768 and 430×600, viewport-height changes, landscape/portrait transitions, immediate static fallback, context loss, hidden documents/ancestors, zero size, 20 remounts and cleanup passed. No browser errors or external requests occurred. Hero, compact, phone, light/dark and short-portrait screenshots were inspected. Independent renderer review sampled 1,001 frames through the full loop at eight display profiles, including particle/head radii, and found no geometry overflow.

| Local instrumented measurement | 1440px | 390px | 320px |
| --- | ---: | ---: | ---: |
| Control response | 4.0 ms | 3.4 ms | 2.0 ms |
| All-phase draw callback p95 | 1.10 ms | 0.90 ms | 1.00 ms |
| 3D/dissolve callback p95 | 1.20 ms | 1.00 ms | 1.10 ms |
| 3D/dissolve p95 at 4× CPU throttling | 4.60 ms | 4.60 ms | 4.30 ms |

Fullscreen phone callback p95 was 0.90–1.00 ms; the 390px 4× CPU check measured 3.30 ms over 132 stage samples. Combined JS/CSS/R-helper gzip is 9,110 bytes (previously 7,177), within the existing 20 KiB budget. Drawing remains capped at 30 FPS, with 180 particles on mobile/compact and 320 otherwise, DPR at most 1.5 and one shared scheduler. Camera fitting adds only bounded corner projections; body faces are cached once and local-height clipping allocates no intermediate arrays. Release times and particle pools are reused, including through height-only resizes. Larger backing stores increase bounded canvas memory/fill work; no per-frame DOM measurement, growing buffers, network assets, dependencies, server timers, reactive data outputs, acquisition, database work or cache invalidation is added. Existing factual data persistence/reuse is unaffected. Documentation has no runtime effect.

The isolated release passed `node test/test_loading_visual.cjs --full-app` at 1440/320px: actual launch geometry and dismissal, stopped hidden animation, leagues → market → ready, retained partial/failed Retry controls, settled empty feedback and zero real HTTP attempts. The existing app-object fixture still drives synthetic credentials through Shiny; this does not claim to fix its previously documented login-form visibility limitation. An initial full-app assertion used the obsolete `#startup_status` location for settled feedback; the current disclosure registry moves that content into `.fm-info-panel`. Updating the test to open the actual panel resolved the failure without changing production status behavior.

Required `Rscript test/test_shiny_simulation.R` passed 13 checks plus eight tabs with no real HTTP/live database mutation attempts. Three deployment-contract checks passed; existing synthetic Plotly warnings remain. The isolated release excludes unrelated working/staged changes; standalone fixture runtime inputs match it (theme file differs only in line endings). Only the two loader runtime manifest checksums change; package/dependency entries are preserved. This is a cosmetic refinement, not a new capability for the official-feature comparison/login tiers. These are local synthetic measurements, not hosted latency, physical-phone/GPU or deployment acceptance; no real account or transaction was used.


## Target-only card selection after the remaining 32-second wait — 2026-11-15 Pacific

The previous implementation did not eliminate the reported path. A reproducible
remaining defect rejected a 120-second-old catalog because the card imposed a
60-second cutoff, although that source's policy is 300 seconds. The rejection
started full team capacity, including balances/roster/transfer history. Earlier
fresh-row browser fixtures bypassed that branch and therefore did not establish
that Dani Lorenzo's hosted wait was resolved. The user's actual private 32-second
request timeline was not captured; no particular production request is claimed
to have consumed the whole interval.

Current behavior:

- Catalog and market observations carry separate original deadlines through the
  actual price join, enrichment, filters and selected snapshot. Explicit stricter
  worker/database deadlines win. Market/Today endpoint rows retain listing proof
  and their own source lifetime; no freshness is borrowed from a newer catalog.
- Selecting a player never starts new team-capacity/roster/funds/transfer-history
  work. It reuses recent target facts or acquires the stale target's existing
  summary. Profile/points/history requests proceed independently of purchase
  capacity. Newer authoritative summaries update actual owner/listing controls;
  missing source fields and wrong IDs cannot establish availability. Captured
  asking prices stay distinct from catalog valuation and negative points remain.
- Successful display checks survive source expiry/reopening through a bounded
  root-session registry (64 rows, 4 MiB, 128 KiB per row), with account, league,
  team, player, competition, season, scoring, Refresh and invalidation isolation.
  The original API facts remain durable in the existing successful catalog,
  market and summary snapshots. Session display markers are not persisted as
  authorization, and do not make old data current after another session/restart.
- Financial work starts only on explicit editor demand. `ensure()` first attaches
  a dormant entry so a fresh result is reused instead of forcing a second read.
  The market editor requires both target and financial proof; even fresh funds
  cannot enable Submit before target facts settle. A newly owned/off-market
  target ends pending verification with submission disabled, even if funds are
  still held. Independent final preflights/context/duplicate guards remain.
- Remove the outer whole-card financial repair/retry loop. The native engine's
  finite source freshness repairs remain unchanged. Failed financial completion
  is truthful in the editor and explicit reopening is the retry path. Card Retry
  refreshes only player facts. No timer, background service, schema, package,
  environment setting or product capability/login-tier expansion is added.

Executed verification:

- Eleven selection/provider/loading suites pass: `test_player_selection_availability`,
  `test_player_availability_reuse`, `test_player_availability_store`,
  `test_availability_completion_window`, `test_card_capacity_recheck`,
  `test_card_live_capacity_recovery`, `test_card_availability_freshness`,
  `test_live_capacity_freshness`, `test_worker_card_availability_recovery`,
  `test_player_card_loading_work` (7 checks) and `test_player_card_during_saves`
  (4 checks), all `.R`. These include the real table-to-card selection,
  two-minute catalog/independent market clocks, 32-second held source, stale
  target-only acquisition, newer summary replacement, expired/failed states,
  90-second reopen reuse, late-response isolation and seven pending save ACKs.
- Eight related suites pass: market-offer opening (18 checks, including funds
  before target and a target becoming owned), acquisition availability,
  configured acquisition rules, championship columns (11 groups), capacity
  completeness/legacy codec, trend plots (16), Today availability (3), and
  deployment exclusions (3). The old loading mock's missing `worker_bypass`
  parameter/exact pre-defer count is updated to the current contract; its
  responsiveness/header/source-isolation checks now run and pass.
- Three existing official captured summaries independently verify the target
  ownership/computer/listing/`p` asking-price mapping, preserving signed points.
  Saved requests are never replayed. Persistence replay retains its one scoped
  cooperative DB GET / zero source acquisition behavior, failed ACK retry,
  immutable original deadlines and context rejection.
- Actual Reactable row click -> card -> market editor browser checks pass at
  390/1440px. The phone keeps all optional/history responses held for a real
  **32 seconds**: availability stays ready with **zero** financial acquisitions.
  Reopening reuses facts, an expired catalog refreshes only the selected target,
  and explicit editor entry starts one financial check with Submit disabled
  until successful. No JavaScript errors or horizontal overflow. Final measured
  card readiness after a visible row click: **1,840ms / 1,113ms**; held-request
  heartbeats: **48ms / 51ms**. These are local synthetic timings, not Contabo or
  authenticated Futmondo latency measurements.
- Required offline lifecycle: **13 checks plus eight tabs pass**, with no live
  HTTP/database mutation attempts. No real account or transaction is used.

Performance review: card selection removes the financial composite rather than
masking its pending state. Fresh table selection adds zero capacity/roster/funds/
transfer reads, while a stale target shares one summary demand with history.
Market/Today projections add no I/O: 100 x 527 rows measured 0.006s. Independent
catalog-market joins measured 0.031s for 100 x 527 rows, retaining 113,096 bytes.
The held-source loading fixture measured 1,794ms to target readiness and 2,335ms
to optional completion, with two initial/target header updates and **zero**
optional action rebuilds. Existing source request limits, DB queues and retry
budgets remain bounded. There is no polling, growing per-player worker/session,
broad periodic invalidation or mobile rendering loop. Documentation has no
runtime effect; build fingerprints run only during image construction.

Docker now generates code-only `/player-card-runtime.json` fingerprints for five
relevant source files. `test_player_card_runtime.py` passes the exact build
command in an isolated temporary directory, verifies all hashes/allowed fields
and excludes any account/environment data. This fixes the earlier static
release-metadata ambiguity; it does not deploy an image or certify a private
account flow. No image build or production migration is claimed here.

Production source rollout is now verified after push: the public
`https://ligaradar.com/player-card-runtime.json?commit=c63b9bb` returned behavior
`target-only-selection` and all five SHA-256 fingerprints exactly matched the
committed source blobs in `c63b9bb`. This verifies the rebuilt code-only image
metadata, rather than the old static release label. No account, session token,
private response or transaction was accessed. This follow-up edits verification
documentation only and has no runtime/performance effect; the executed application
checks above are unchanged.

The prior incident remains distinct from deployment/source proof. Dani Lorenzo's
actual authenticated selection timing has not been inspected in this environment.
Hosted account acceptance remains open until that evidence exists; public code
fingerprints and synthetic browser timings alone do not certify a private flow.


## Dani Lorenzo Retry and vertical month labels — 2026-11-15 Pacific

The screenshot identifies a remaining field-contract defect, not a failed source
request or financial timeout. Read-only scoped database inspection located Dani
Lorenzo's successfully persisted interactive summary observed at
**2026-11-16 04:19:12 UTC**, immediately before the screenshot. Its completeness
was `complete` and acquisition status was `ok`. The actual response contains
**`data.computer=TRUE` and a published `market.p`, but no `championship.owner`**.
The earlier worker summary has the same omitted-owner shape. The previous
projection required that owner object and returned NULL. Earlier captured/test
summaries supplied an owner object and missed this real source variant.

The corrected projection accepts the explicit data-level System/free-agent flag
when the owner object is absent. It still rejects missing/false/NA/textual flags,
wrong player IDs and unavailable market fields. The original source deadlines,
asking price, negative/zero/missing scores, account/league/season/scoring scope,
durable acquisition/persistence/ACK gates and final financial checks are intact.
No unknown ownership or amount becomes zero. This fixes the cause of Retry after
a successful Dani-shaped acquisition rather than hiding a legitimate failure.

Actual-source replay:

- Read only the matching player's factual summary in the existing private worker
  and availability stores. No authentication/session-token tables were read;
  no source API request, production write or transaction was made.
- Validate the persisted availability payload's SHA-256 and **19,412** decoded
  byte length, then use the existing safe primitive decoder. The earlier worker
  body also matched its expected 21,444-byte decoded length.
- Before the code change, the real payload matched the requested player and
  `status=ok` but `player_card_summary_row()` returned NULL. After the change it
  returns **Free Agent / Listed on Market**, preserves the exact source asking
  price and original observation/expiry, and preserves supplied negative points.
  Projection replay took about **61ms** locally including first-call compilation.
- Network/database functions were blocked during the post-change replay. Only
  structural diagnostics/booleans were printed; full private responses were not
  printed or committed. Encoded payloads stayed in transient tool/process memory,
  were cleared after replay and were not written to fixture files.

The date axis now uses `%d %b` (for example `01 Nov`) and `tickangle=90`, retaining
three sparse automatic ticks, full hover dates/years and all source observations.
Round-only axes retain numeric labels rather than inventing calendar months.
A 20px Date-title standoff and 95px bottom margin avoid collision/clipping. An
initial geometry check caught overlap in the narrow points-only mode; the spacing
was corrected and every mode/theme/width now passes. The browser check also waits
for the actual requested trace names/paint instead of confusing same-count
valuation-only and points-only transitions.

Executed checks:

- Focused target/table regression, shared session reuse, durable store replay,
  market-offer opening (18 protections), trend plot contracts (16) and deployment
  exclusions (3) pass. The minimized source-shape regression uses a synthetic ID
  and asking price, preserving privacy while testing the exact missing-owner form.
- The actual Reactable -> modal browser flow passes at 390/1440px, including the
  Dani-shaped System response: it enables Make Market Offer and removes Retry
  after the target source settles, with no financial acquisition. Explicit editor
  entry still keeps Submit disabled until financial proof succeeds. The phone
  again holds optional/history responses for a real 32 seconds. No JS errors or
  page overflow; card readiness 1,801/1,047ms and heartbeat 48/49ms locally.
- Actual Plotly expression/browser checks pass at 320/390/1440px across combined,
  valuation-only, points-only and undated modes and both themes. Labels are
  vertical, use abbreviated months, stay inside the plot, do not overlap Date or
  one another, and preserve colors, all series values, zoom and exact hover data.
  Final chart heartbeats: 77–79ms; no JS/Shiny errors or horizontal overflow.
- Required offline lifecycle: 13 checks plus eight tabs pass, with zero live
  HTTP/database mutation attempts. No real account/transaction is used in tests;
  actual factual evidence is inspected separately through the readonly replay.

Performance review: the parser adds one constant-time explicit-flag check to the
existing single-player projection. It adds no API/DB read, poll, retry, queue or
cache. Existing successful persisted summaries remain reusable and failed writes
remain retryable. The plot changes only layout fields and three tick labels;
no points/valuation transformation or source request is added. Extra bottom space
slightly reduces the drawable plot height and is verified on narrow screens.
Documentation changes have no runtime effect. No dependency, environment setting,
new product capability/login entry or schema migration is required.

After push, `https://ligaradar.com/player-card-runtime.json?commit=ed74995`
returned all five source fingerprints matching the committed blobs, including
the corrected selected-player parser and date-axis layout. Production source
rollout is verified. This follow-up is documentation-only with no runtime or
performance effect. An authenticated private browser interaction is not claimed
by the read-only source replay, public fingerprint check or synthetic tests.


### November 16: confirmed zero departures lost when entering Lineup

The new actual-root/real-adapter regression first failed on Today → My Team → Lineup with the exact reported message: `Clause transfers could not be verified. Roster is refreshing. Transaction history is refreshing.` My Team had already rendered `Leaving players (0)`. Both consumers shared a factual-reader registry key, but the first source-time-note adapter owned an activity predicate excluding Lineup. Navigation deactivated the shared entries and reset their state to loading. The root now supplies one explicit roster/history reader pair with activity covering all consumers and the same global/clause refresh signature. Standalone transfer sources retain their existing defaults. No warning suppression, fabricated empty result or validation bypass is used.

Executed checks:

- `test/test_lineup_departure_handoff.R`: three entry orders, repeated page handoffs, league changes, delayed saved-data arrival, partial evidence, logout and account replacement pass. Ten timestamp-only publications per sequence cause zero eligibility redraws or source/database requests (0.614–0.629s locally); warm navigation uses zero I/O or persistence writes.
- `test/test_lineup_departure_browser.cjs`: real Lineup/root wiring, decoded synthetic durable roster/history snapshots and page information panel pass at 390/1440px. Confirmed zero clears the warning, explicit refresh reaches the shared database readers, source timestamps remain unchanged, unsaved drafts survive navigation/refresh, no Futmondo mutation or unexpected I/O occurs, and there is no horizontal overflow/browser error. Heartbeat response: 89/104ms. Inspected the mobile screenshot.
- Existing source-note, clause-observation contract, observed cold reload and complete Lineup module regressions pass, including genuine departures, unknown evidence, kickoff boundaries and final asynchronous save validation.
- Required `Rscript test/test_shiny_simulation.R`: 13 checks plus eight tabs pass with zero live HTTP/database mutations. Existing synthetic Plotly warnings remain.
- `python3 test/test_player_card_runtime.py` and `Rscript test/test_deployment_contract.R` pass. The additional build-only `/lineup-departure-runtime.json` contains exactly the root server/shared transfer module SHA-256 hashes and a fixed behavior label, enabling code rollout verification without account information or application polling.

Performance review: two existing shared prerequisite readers replace competing owners; the bounded registry/cache and two-summary queue remain unchanged. Worker navigation reuses durable observations with zero source requests or GUI persistence writes. Explicit Refresh now reaches the intended shared prerequisite invalidation/read instead of inheriting the note's unrelated refresh predicate. Normal database polling/worker schedules and action preflights are unchanged; no timer, background acquisition, per-player read fan-out or dependency is added. Successful observations keep their original context, expiry/completeness and source timestamps; failed persistence remains retryable. The build marker adds two small file hashes during image construction and no application request or reactive work. Documentation has no runtime effect.

Production read-only aggregate metadata showed successful current-context roster/history snapshots; no database or acquisition repair was needed to reproduce and fix this reader-lifetime defect. Browser/database replay checks are synthetic and do not access the user's private browser session or submit a live lineup. Application build/rollout and authenticated acceptance are distinct from these completed local checks.

After push, `https://ligaradar.com/lineup-departure-runtime.json?commit=50302b9` returned behavior `shared-departure-prerequisites` and both SHA-256 values exactly matching the committed `server.R` and shared transfer module. The automatic production source rollout is verified. Authenticated user-session acceptance remains separate. This follow-up records deployment evidence only and has no runtime or performance effect; no additional performance test is required.


## Multiple position filters and debounce — 2026-11-16

Implemented in the shared table component for every existing Position filter.
All/concrete selection normalization is immediate in Selectize; the server
debounces the actual position filter for 300 ms. Several positions match either
primary or secondary role once per row; missing roles remain visible under All.

Verification (synthetic data, no account access or live transactions):

- `Rscript test/test_player_table_filters.R`: 13 checks pass, including both
  default-All layouts, known-answer role unions, unknown roles, the exact 300 ms
  idle boundary, rapid-selection coalescing and zero extra enrichment. A 524-row
  multi-position pass averaged 0.686 ms over 1,000 repetitions locally.
- Existing player metric colors (10), squad/own-team counts (8) and departure
  highlighting (9) checks pass. Their mock clocks now advance through debounce.
- `test/test_position_filters_browser.cjs` against its offline R fixture passes
  both layouts at 320/390/1440 px, dark/light themes: mutual exclusion, successive
  selections, primary/secondary union, debounce, clearing, reset and independent
  table state. No page/control overflow, browser errors or visible Shiny errors;
  heartbeat responses 63–93 ms. Inspected the mobile screenshots.
- `Rscript test/test_shiny_simulation.R`: 13 checks plus eight tabs pass, with
  no live HTTP/database mutation attempts. Existing synthetic Plotly warnings
  remain. No dependency change or manifest regeneration is required.

Performance review: position changes reuse enriched observations and existing
durable database-backed resources. No API/database fan-out, persistence change,
source invalidation, repeated history computation or polling is added. One
bounded debounce per table coalesces redraws; filtering remains linear in loaded
rows and the selection has at most four concrete roles. Native responsive chip
wrapping handles mobile layouts. Documentation/login wording has no data-path
effect. Browser/lifecycle checks establish local behavior; deployment and
authenticated production acceptance were not performed in this task.

## Player chart stability — 2026-11-16 Pacific

`Rscript test/test_player_trend_plot.R` passed 16 checks, including line styles and known point/value fixtures. `Rscript test/test_player_chart_stability.R` passed the offline same-player update and progressive loading checks. The existing `test/test_progressive_loading.R` fails its pre-existing five-request expectation: current behavior starts four requests, with capacity deferred; a diagnostic run past that assertion also found the later capacity fixture assumes a request that is no longer started. No real account was used. Mobile background/foreground behavior and deployed responsiveness were not measured.

Performance review: preserving the chart and publishing only changed chart inputs reduces widget remounts and Plotly redraws. This adds one bounded reactive snapshot per open card and equality comparisons over existing history/match data; it adds no API or database requests, timers, persistence changes, or background work. Existing durable history reuse and scoped invalidation remain in place. Actual changed history may still redraw the chart.


## Filter label icons — 2026-11-16

Added decorative icons to the existing shared filter labels, including up/down
arrows beside Min Trend in both layouts. The visible wording and input-label
associations remain intact. No new capability or login inventory item is added.

Verification: the existing offline position-filter browser suite passes both
layouts at 320/390/1440 px in dark/light themes, preserving selection, debounce,
clear/reset, isolation and responsive wrapping without page/control overflow or
browser/Shiny errors. A separate browser inspection confirms that the actual
Font Awesome up/down glyph renders, is `aria-hidden`, preserves the accessible
name **Min Trend (M)** and focuses the numeric input when its label is clicked.
The 390 px screenshot was inspected. The required
`Rscript test/test_shiny_simulation.R` passes 13 checks plus eight tabs with no
live HTTP/database mutation attempts; existing synthetic Plotly warnings remain.

Performance review: static label markup adds at most twelve small icon elements
per table and reuses the existing icon dependency. No API/database requests,
cache/persistence changes, observers, timers, repeated filtering, invalidation,
background work or new assets are introduced. Existing mobile wrapping handles
the labels. No additional performance benchmark is needed for these static
elements; documentation has no runtime effect. Verification is local; no
authenticated production acceptance was performed.


## Scheduled-clause production SQL application — 2026-11-16

The owner explicitly requested applying the existing migration. Matched the connected project `ozhnextrcxxksswpnglt` to its prior documented worker migrations, confirmed all eight prerequisite worker/transaction tables and no clause tables, and applied the unchanged reviewed script. Supabase migration history confirms `20261116161538`, `scheduled_clause_purchases`; source SHA256 `fc1f7b61ac53e2a62484aa8aeb42809d7617e2e546822c243a84a89c5209a91e`.

Post-application catalog assertions passed: four tables have RLS and no anon/authenticated read/write privileges; all 23 functions use SECURITY INVOKER, restricted `public, pg_temp` search paths, no anon/authenticated EXECUTE, and service-role EXECUTE; all 11 indexes are valid; the authoritative transfer trigger is enabled. A service-role `clause_read()` smoke check for a synthetic absent account passed inside an explicitly rolled-back transaction. No real account, Futmondo API operation or purchase was used, and no deployment setting was changed.

Security and performance advisors were captured before and after. No new WARN/ERROR findings were introduced. The four new [RLS-without-policy INFO entries](https://supabase.com/docs/guides/database/database-linter?lint=0008_rls_enabled_no_policy) are intentional: these tables deny browser roles and are accessed only by the bypass-RLS server role. Six new [unused-index INFO entries](https://supabase.com/docs/guides/database/database-linter?lint=0005_unused_index) concern the newly installed queue indexes, which are retained for their defined lookup/recovery paths. Existing unrelated advisor findings remain unchanged.

Performance review: production `EXPLAIN (ANALYZE,BUFFERS)` of the empty due-job lookup selected `clause_jobs_deadline`, read one shared buffer hit, wrote none, and measured 0.129 ms execution plus 2.804 ms planning. This verifies installation/index use, not loaded queue throughput, host capacity or execution lag. No source collection, new schedules or timers were started by the migration verification. The documentation-only delivery has no additional runtime request, cache or rendering cost; no dependency/source change requires manifest regeneration or another lifecycle run. The earlier isolated SQL17, Python13, worker38, lifecycle13+eight tabs and browser checks remain the implementation evidence. Feature activation/redeployment, native rejection capture/mapping, temporary-diagnostic removal and controlled hosted execution/restart/resource acceptance remain open.


## Database-first login and automatic factual sync — 2026-11-16

Root causes addressed: browsing could select direct acquisition when worker access/configuration was absent; newly demanded database resources waited for the next poll; league preference loading added a startup request; token renewal reset queued/running jobs; card availability could acquire on opening; refreshing briefly replaced saved content with loading; worker source handling repeated guarded database round trips. The coach safety read also wrote an unrelated player-availability invalidation epoch before an action. That failure reproduced against the preserved pre-task code; read preflight now invalidates only its local cache, while post-action invalidation remains.

Implementation and interfaces: [database-first sync](database_first_sync.md). Login connects encrypted factual access automatically and acknowledges it independently of display reads. Logout retains collection; Disconnect clears factual access and signs out. No additional workers, new dependencies or concurrency increases. Source validity, account/league/season/scoring isolation, known-zero/unknown distinctions, ledger calculations and manual action authority are retained.

Executed synthetic checks (no real account authentication, source API operations or transactions):

- `test/test_database_first_sync.R`: saved balance served in exactly two database reads while connection acknowledgement is held; 1.15 s in the measured cold-session run (includes settling overhead, not network). Zero source requests. Unchanged Refresh paints zero times, changed balance paints once; logout leaves sync running; missing card data never falls back to acquisition. Missing collector configuration retains database display mode.
- `test/test_worker_renewal_flush.R`: one connection per login, held acknowledgement/input response, disconnect beating late renewal, direct account-switch isolation, ordinary logout and synchronous encryption failure recovery.
- Provider, GUI-readonly, live-update, status-redraw, context-handoff and replay-render regressions pass. `test_worker_performance.R` restores a known 5,001-entry ledger in 0.143 s (467.7 KiB JSON / 54.5 KiB gzip).
- `test/test_worker_render_browser.cjs`: actual Today cards/shared header/table at 320/390/1440 px. Metadata updates, expiry/recovery and delayed unchanged Refresh keep nodes/table mounted; changed balance/player facts paint; source clocks advance; zero acquisition jobs or horizontal overflow. Input during an 800 ms held database response: 177/144/145 ms. Subsequent heartbeat: 63/62/63 ms. These three samples are not a hosted p95 measurement.
- `test/test_worker_browser.cjs`: automatic connection, no enable/quiet-hours controls, saved league exclusions, unsaved-edit preservation across polls, Disconnect/signout, clock and no overflow at 360/390/1440 px; heartbeat 67/67/66 ms. The original fixture expected visible text inside closed status disclosures; it now checks their text contract without forcing expansion.
- Worker Python suite: 40 passed in 29.741 s. Includes real headless R parser reuse, durable checkpoints across a fresh worker, three RPCs for an uncached source exchange/one for checkpoint reuse, bounded chunk publication and old-credential 401 isolation.
- Isolated PGlite `test/test_database_first_sync.cjs`: full schema plus idempotent migration, unchanged leases/checkpoints/run IDs on repeat login, read-only expired-data bootstrap, server-only RPCs, disconnect/late renewal, preserved snapshots and league exclusions. All six query families progress with 527 bulk player jobs; six claims took 43 ms locally.
- Manual offer, coach status (9 groups), worker lineup evidence, worker finance known-answer fixture, scheduled-clause async action and scheduled manual-action checks pass. Required `test/test_shiny_simulation.R` passes 13 checks plus eight tabs with zero live HTTP/database mutation attempts. Existing synthetic Plotly warnings remain.

Performance review: startup uses one bootstrap plus one selected-core read, without waiting for connection acknowledgement or a five-second timer. Immediate reads are coalesced and regular polling remains five seconds/visible session, sharing account state. Factual API calls from ordinary production browsing are zero; manual checks remain explicit. Source exchange removes two database network round trips; checkpoints/results remain durable and contexts unchanged. Publication groups at most four 128 KiB chunks per request, still bounded and guarded. The claim function sorts cheap candidates before expensive context/peer predicates. No additional source concurrency, timers, worker processes or cache capacity. Equal facts avoid reactive redraws, including mobile tables; display memory retains existing bounded caches and buffers. Documentation/static login disclosure add no requests. Manifest changes only update affected code checksums, with internal documentation, tests, scripts, captures and credentials excluded.

Production database rollout: applied source `20261116183421_database_first_sync.sql` as Supabase migration **20261116213815**, `database_first_sync`, to the configured project. Source SHA256: `89fa9e98921bece6d8281955404925bcf75181b6f7bb07a6582440dfeccf4c5f`. All nine new/replaced functions are invokers with restricted search paths, server-role EXECUTE and no anon/authenticated EXECUTE. Enabled accounts have no quiet window. A read-only production diagnostic returned one stored league resource plus its saved preference (3,535 bytes), with no encrypted credential exposed. `EXPLAIN (ANALYZE,BUFFERS)` for bootstrap measured 11.057 ms execution/0.264 ms planning, 1,620 shared hits and zero reads, dirtied or written buffers; this is database execution, not browser/network latency. No real token was rotated and no source request or purchase was made by these diagnostics.

Pre/post advisors are unchanged: no new security or performance finding. Existing [GraphQL visibility warnings](https://supabase.com/docs/guides/database/database-linter?lint=0026_pg_graphql_anon_table_exposed) on `futmondo_players`, [security-definer execution warnings](https://supabase.com/docs/guides/database/database-linter?lint=0028_anon_security_definer_function_executable) on `rls_auto_enable`, the duplicate player index and informational private-table RLS/index notices are outside this change. Health advisory was empty.

Limits: existing production jobs report native rejections for team information, money movements and round ranking, also recorded in partial finance diagnostics. This delivery does not establish why each source rejected its exact query or certify complete rival accounting. Those requests/responses must be captured and mapped before claiming those acquisition defects fixed; partial/missing values remain explicit. Finance source timestamps can be the oldest contributing observation, so their age alone is not proof of a stalled worker. Authentication-to-primary-card latency, p95 input latency, first-account acquisition and multi-account VM CPU/RSS/queue lag require hosted acceptance after deployment. Local timings and database diagnostics do not certify those targets.

Final delivery verification used an isolated copy of the exact staged tree, excluding unrelated working-tree changes. Bootstrap/error/cancellation, renewal/disconnect, manual offer, coach, finance, SQL and the required lifecycle all passed there. The R worker contract passed 20 groups with the existing `/tmp/futmondo-r-test-library` Sodium dependency; the initial default-library invocation lacked Sodium and did not pass. R/Python fixture interoperability and all 40 Python tests then passed (26.438 s). Staged dashboard browser checks passed at all three widths with held-read heartbeat 177/109/146 ms; settings/disconnect checks passed with 66/67/67 ms heartbeat. Changed source checksums match staged manifest entries; excluded development/private artifacts remain excluded. No package installation or dependency change was needed.


## Player card: database observations and obsolete Retry — 2026-11-16

The reported card combined two independent state bugs. `assemble_player_card_history()` propagated a saved summary's expired freshness, but the UI unconditionally translated `stale` into **refresh failed**, without an error. The target display path also required the 60-second action-validity check even though worker summaries run at five-minute/hourly cadences. Synthetic restoration of a successful two-minute-old snapshot reproduced both, with no failed request. The card's production read path was already database-only; its old Retry control still exposed a separate manual acquisition path.

The corrected display path validates successful persisted ownership/listing evidence, then uses the existing explicit field/identity parser without renewing source timestamps, stale flags or action deadlines. It reuses the same bounded context-isolated database snapshots/session display projections. New saved observations update the open card; missing/partial/failed data is not invented or promoted. Production browsing has no **Retry availability** button, and obsolete button events cannot query Futmondo. Only explicit transaction entry starts current target/funds checks; Submit stays blocked until both complete. History now carries actual resource errors separately from age; only actual failures receive error severity. Contracts: [player availability](player_availability.md), [player card](selected_player_module.md).

Executed verification, using synthetic data and no real account, source operation or transaction:

- `test/test_player_card_database_display.R`: real snapshot codec; expired success versus partial/failure/future/mismatched fields; original timestamps/deadlines and zero/negative points; zero source calls on opening/reopening/Refresh/obsolete Retry; automatic newer-fact updates; clearing session/decoded caches then restoring database bytes; league/player isolation; real read-error visibility; own listings; separate held target/funds checks before Submit.
- `test/test_worker_manual_offer.R` passes its four groups, `test/test_market_offer_opening.R` passes 18 groups, and actual selection/captured source-field replay passes. The delayed-funds/card freshness test passes with the expected one information, one market, two roster and two history reads, all explicitly triggered by editor entry.
- `test/test_player_availability_store.R` passes durable replay, exact account/league/team/competition/season/scoring/player isolation, failure/retry acknowledgement, source deadline preservation and cooperative serialization. `test/test_player_availability_reuse.R` passes native/offline session reuse and manual intent routing. Two pre-existing fixture problems were corrected: the freshness test tried to open a market offer for an explicitly unlisted player (the untouched pre-task implementation failed identically), and the native reuse test selected obsolete worker-on-opening acquisition and did not map the current `intent` argument. Production database behavior is separately asserted by the new regression.
- `test/test_database_first_sync.R` passes: two-read saved bootstrap while enrollment is held, unchanged Refresh without repaint, missing-card database-only behavior, logout and cancellation/error cases. `test/test_player_valuation_history.R` passes 14 groups including durable profile reuse, exact context isolation and the captured historical-price mapping.
- Required offline lifecycle passes 13 checks plus eight tabs, with zero live HTTP/database mutation attempts. The deployment contract passes three checks; no dependency change requires manifest regeneration.
- A local Chromium smoke fixture built from the same synthetic database-card setup passes at 390px and 1440px: no Retry control or false refresh failure, zero source requests, enabled editor entry, responsive input, and no horizontal overflow or JavaScript error. Early-input samples varied from 8 to 716 ms while the initial chart rendered, including a 623 ms sample under simultaneous R-test load. A paired run with the same isolated fixture measured baseline early/settled input at 681/23 ms (phone), 17/25 ms (desktop), versus changed 102/25 ms and 20/9 ms. The initial-chart delay also occurs before this change; these synthetic samples do not establish a hosted p95 target.

Performance review: the new metadata validator took 0.026 seconds for 1,000 calls. It adds no I/O, timers, cache entries, worker jobs or polling, and preserves existing bounded storage/replay. Only target fields are projected; history/price transforms and chart projections are unchanged. Ordinary browsing still makes zero source calls, while explicit editor entry joins/reuses current target/funds requests. Removing a browsing button reduces DOM work; replacement text uses existing wrapping/mobile styles. A real read error updates status without changing the chart's factual projection. No persistence schema, background cadence, dependency, environment setting or feature-tier expansion is introduced.

Delivery scope/limits: only the task delta is staged; unrelated ownership-label, test, documentation and manifest edits are preserved. The two changed runtime checksums match staged bytes. Six unrelated checksum mismatches already present in HEAD (`futmondo_functions.R`, Market, Players in Championship, Players in Teams, Players Table and Today) are left unchanged. Local behavior and source delivery do not establish authenticated hosted rollout, production worker health, missing first-time acquisition or complete historical coverage. Those remain separate acceptance/evidence checks; this task does not claim an underlying native API rejection was repaired.

Final isolated verification: the exact staged code passes the new database-card regression, worker manual-offer checks, corrected 31-second freshness fixture, native reuse, deployment contract and required lifecycle (13 checks/eight tabs). Phone/desktop browser assertions also pass against that staged tree. Documentation-only verification notes add no runtime cost.


## Classification bonus columns waiting on a saved loading flag — 2026-11-16

Read-only production diagnosis (2026-11-17 UTC) found a 1,787,174-byte decoded
finance snapshot with `loading=TRUE`, 45 pending tasks and no known MVP/Best XI
round components. Its source timestamp was 2026-11-16 05:50:44 UTC. A separate
compact calculation saved at 2026-11-17 04:29:24 UTC already retained later
component coverage. The production worker reported rejected historical-ranking
and private team-information/money-movement requests. The ranking getter still
sent a calendar number even though the established Classification contract uses
the opaque source round ID under `roundNumber`.

Classification now reads the latest compact calculation directly through one
asynchronous scoped database query, excluding transaction archives/chunks and
detailed cache decoding. It reuses the original per-round ledger projection and
does not acquire Futmondo data. The historical finance getter's query is fixed;
its existing evidence persistence/signature reuse remains intact. Missing
completed-round amounts remain NA, and pending rounds remain excluded. The
calculation's saved time is visible and original per-team source times survive.

Executed synthetic checks:

- `test/test_classification_database_bonuses.R`: held database read with usable
  standings/input changes; one query; known sums 30/10/20/7; true zero and unknown
  amounts; zero extra reads on range changes; DB-only Refresh; retained values on
  DB error; memory-cleared database reuse; minute-bounded active-page rereads;
  missing-row completion; old-account response rejection; scope/hash validation.
- `test/test_rival_saved_bonus_recovery.R`: six groups passed, including the
  exact corrected query, closed participant validation, known-answer rewards,
  persistence and subsequent reuse with no additional ranking request. The old
  fixture needed explicit acquisition mode and recognition of the first
  unknown-to-known scoring-context cache transition; no production guard was
  relaxed to make the test pass.
- `test/test_rival_bonus_snapshot.R`: four groups passed, covering the existing
  full/compact ledger contracts, zero/NA preservation, pending settlement and
  malformed projections.
- `test/test_classification_partial_bonuses.R`: completed/pending selections,
  independent component coverage, stored ledger restoration and range reuse
  passed, including the underlying running-balance and classification fixtures.
- `test/test_shiny_simulation.R`: 13 checks plus eight tabs passed; no source
  HTTP or live database mutations. Existing fixture warnings remain visible.

Performance review: the production read-only SELECT used the existing evidence
primary-key index and returned one row in 32.794 ms database execution (29.959 ms
planning; excludes REST/network/rendering). The compact compressed data field
was about 7.5 KB, versus about 410 KB for the inspected worker envelope. One
hundred local synthetic reads/decodes/projections took 0.435 s; 100 bonus range
projections over 10 teams × 38 rounds took 0.298 s. No source requests, writes,
transaction reconstruction, new polling timers, worker lanes or mobile layout
work are introduced. Shared memory remains bounded, and persisted snapshots
remain the cross-session source. The existing status tick adds at most one
compact DB read per minute while the page is active, plus explicit Refresh.

Limits: no real account was used in tests and no production data was mutated.
The exact SQL projection was measured through the connected database tool; a
separate local REST diagnostic could not run because this workspace has no
server database credentials. No production source acquisition/backfill was
triggered here. The newer stored calculation still lacks round-8 MVP coverage,
Best XI historical ownership/eligibility for multiple rounds, and position
reward coverage; corrected worker acquisition and end-to-end completeness are
not yet certified. Desktop/mobile layout is unchanged; no new browser run or
hosted latency claim is made. There is no new capability or schema/dependency
change. Unrelated working-tree and staged changes are preserved.

Final task-only checkout verification also passed the database display regression,
all six historical recovery groups, deployment's three groups, and the offline
lifecycle's 13 checks plus eight tabs. The existing classification loading/settings
regression passed in the workspace. Only the three changed runtime checksums were
updated in the manifest; no dependency regeneration was needed.


## Random club loading colors and compact shoes — 2026-11-17

The loader selects two clubs once per page load from Real Madrid, Barcelona, Atlético Madrid, Real Betis, Inter, AC Milan, Dortmund, Juventus, Manchester City and Liverpool. The first choice is uniform; its opponent comes from a different primary-color family. The same pair is reused by launch and compact scenes through every loop, remount and theme change. `data-fm-teams` records the two space-separated IDs; selection is local and is not persisted. Classic base/stripe palettes cover both visible shirt planes, matching dissolve particles and radar fills/outlines. White and pale kits have contrast edges; black stripe particles have a team-colored rim. Shoes now span -1.6/+3.2 in local X, 6.4 across Y and 3 high (previously -2.5/+6, 10 and 5), with corresponding silhouette samples and smaller ground shadows. Opposing-goal orientation, the upper-half phone composition, readiness-driven dismissal and 15-second loop remain intact.

The final focused browser suite passed ten phases at 1440/390/320px in dark/light, complete 180/320-particle pools, eleven figures per club, shirt accents, goal-facing heads/toes, short-shoe depth/width ratios, head-first upward dissolution and exact geometry/bitmap loop seams. Fullscreen 320×568, 390×844 and 430×932, short portrait 767×768/430×600, viewport-height/rotation changes, reduced motion, canvas loss/unavailability, hidden states, zero size, 20 remounts and cleanup passed. All ten clubs were selected with deterministic fixture inputs; Liverpool/City, City/Juventus and Juventus/Dortmund received additional dark/light figure/flock/radar and identity checks. Fixture RNG replacement is limited to the loader's synchronous initialization and then restored. Independent source review checked all 76 eligible ordered pairs, two random choices per document and unchanged identities across theme changes. Hero, compact and alternate-palette screenshots were inspected. There were no browser errors or external requests.

| Local instrumented measurement | 1440px | 390px | 320px |
| --- | ---: | ---: | ---: |
| Control response | 9.4 ms | 10.1 ms | 8.8 ms |
| All-phase draw callback p95 | 1.40 ms | 1.10 ms | 1.20 ms |
| 3D/dissolve callback p95 | 1.50 ms | 1.20 ms | 1.40 ms |
| 3D/dissolve p95 at 4× CPU | 5.30 ms | 5.20 ms | 6.70 ms |

Fullscreen phone draw p95 was 0.90–1.20 ms; the 390px 4× sample measured 5.90 ms over 132 stage samples. The highest-outline combination, Juventus/Dortmund in light mode with 320 hero plus 180 compact particles, measured 6.70 ms p95 over 126 stage samples at 4× CPU; its captured frame retained all 320 hero particles. Combined JS/CSS/R-helper gzip is 10,075 bytes (previously 9,110), within the 20 KiB budget. The fixed ten-entry catalog, two selections, mesh/panel setup and theme palette resolution stay outside the frame loop. Extra work is bounded shirt fills and contrast strokes; particle counts, 30 FPS cap, DPR ≤1.5, shared scheduler, visibility suspension and canvas dimensions remain unchanged. No per-frame DOM reads or growing state, source/database calls, server timers, reactive data work, cache invalidation, dependencies or network assets are added. Existing durable factual observation reuse is unaffected; documentation adds no runtime cost.

The isolated task release passed `Rscript test/test_shiny_simulation.R` (13 checks plus eight tabs, zero real HTTP/live database mutation attempts) and `Rscript test/test_deployment_contract.R` (three checks). Existing synthetic Plotly warnings remain. Browser runtime inputs match the isolated release after normalizing the pre-existing theme file line endings. Only the renderer checksum changes in the manifest; dependency/package entries and unrelated working/staged changes are preserved. The full-app login/browser fixture was not rerun for this drawing-only refinement; no server startup contract changed. This is a cosmetic refinement with no new official-feature comparison or login-tier capability. Measurements and screenshots are local synthetic evidence, not hosted/physical-device or GPU acceptance; no real accounts or transactions were used.


## Startup database admission check — 2026-11-17

Implemented a read-only database readiness gate before all login. One GET validates the existing application_access main row; either boolean policy is healthy, while missing/malformed/failed responses block login. Disabled initial markup, a separate readiness message and server-side guards cover manual events, Enter, administrators and saved restoration. Failure is an always-visible alert with Retry; saved-cookie routing is constructed only after a successful check.

Verification used a clean HEAD-based candidate with only task changes, preserving unrelated working edits. Results:

- `test_database_probe.R`: 7 groups passed (query/header/timeout contract, either policy, configuration, shape/error cases, responsiveness, deadline and late completion). Dispatch of the deliberately delayed promise took 0.0009 seconds; a heartbeat ran before completion.
- `test_database_login_gate.R`: 5 groups passed, including synthetic login attempts while pending/failed, admin blocking, duplicate retries, no automatic credential submission, remembered startup ordering, invalid probe results and session closure.
- `test_login_feedback.R --browser`: 5 server and 7 Chromium checks passed; idle authentication messages cannot override the DB gate. `test_login_restore_visibility.R` passed; `test_app_access.R` passed 9 checks; `test_remember_login.R` passed 21 checks. Existing tests inject explicit healthy probes instead of bypassing production gating.
- `test_shiny_simulation.R`: 13 checks plus 8 tabs passed; zero live HTTP/database mutations. Expected synthetic-history diagnostics and existing empty-plot warnings remain.
- `test_remember_login_browser.cjs`: 9 HTTPS browser checks passed using synthetic accounts, temporary storage and the actual dashboard shell. At 390px, a two-second startup failure kept the alert visible and login disabled, made no saved-cookie request before readiness, and processed a heartbeat in 67ms. Retry created the real saved-login endpoint, processed an absent cookie, enabled login and completed cookie issuance. Restoration/retry, browser/server restart and worker-path changes, logout and persistence failure also passed, including desktop view. Inspected the mobile outage screenshot: alert/retry are prominent, controls wrap and there is no horizontal overflow. Existing feedback assertions now inspect the current shared page information panel.
- Deployment contract: 3 checks passed; only changed runtime checksums were refreshed, with no added package/schema/asset dependency.

Performance/security review: one small indexed database read per new browser session and per explicit retry; no duplicate outstanding request, background polling, scan, factual API call or database write. Existing post-auth non-admin policy reads remain fresh. Cooperative transport retains its eight-second request timeout; the new ten-second overall deadline includes queue waiting. Timers cancel at settlement, scoped cancellation leaves unrelated requests alone, and closed sessions ignore callbacks. Health is transient operational state, not factual API caching; durable observation persistence is unchanged. Server keys/backend responses never enter UI, cookies, URLs or diagnostic output.

Verification limits: the workspace does not provide DB configuration, so the attempted operational read could not run; live hosted availability and deployment were not verified. No authenticated real account or transaction was used. `test_progressive_loading.R` stops before its login block on an existing player-card assertion (`length(calls) == 5L`); a separate clean archive of baseline 7549fef reproduces exactly that failure. Its isolated, unchanged pending-login/logout checks passed 2/2 against the candidate. No unrelated player-card behavior was changed.

## Configurable worker cadence — 2026-11-18

Local implementation and isolated checks are recorded in [worker cadence](worker_cadence.md). Production job inventory timed out; deployment verification remains pending. This does not mark the Round awards defect fixed.

## Worker storage cleanup — 2026-11-18

See [worker storage cleanup](worker_storage_cleanup.md): explicit production compaction reclaimed about 1.19 GB; bounded cleanup and cadence migrations were acknowledged. Synthetic checks and offline lifecycle passed. Worker deployment verification remains separate; total database size still exceeds the free allowance.

## History/index storage verification — 2026-11-18

[Storage review](worker_storage_cleanup.md#historyindex-storage-review--2026-11-18) records nine concurrent index rebuilds with all history retained, database reduction to 1.203 GB, valid indexes and verified automatic cleanup progress. The 500 MB target is not met; historical normalization remains unimplemented. No runtime source changes in this task.

## Lossless membership normalization — 2026-11-18

[Membership storage](membership_storage.md) documents normalized facts plus exact observation timestamps, compatibility interfaces, full transactional equality verification and production migration. Membership storage fell from 146.4 MB to 41.1 MB without historical row loss; database is about 1.10 GB. SQL/R fixtures, 40 worker tests, 14 context groups and offline Shiny lifecycle passed. Other history normalization remains outside this implemented step.


### History confirmation periods — 2026-11-18

Local endpoint migration fixtures pass full retained-field equality, IDs,
microseconds, nullable legacy rows, negative/zero values, JSON null/empty values,
A/B/A, late changes, historical cutoffs, context isolation and denied public access.
Each 200-confirmation stress fixture adds one endpoint and zero states; final
isolated times 148/125/123 ms for price/match/finance, not production benchmarks.
Shared direct/deferred routing preserves retry acknowledgments and null payloads.
R envelope/membership, 11 recovery groups, continuation bounds, 40 Python worker
tests and full offline Shiny lifecycle (13 checks/eight tabs) pass.

Performance review: source request counts, timers, UI redraws and cache policy are
unchanged. Repeated facts reuse durable states; endpoint lists grow with changes,
not poll count. Writes scan indexed entity-local endpoints; latest views use
indexed newest-event lookup. Bulk copy refreshes statistics; rollout is bounded
per domain (5s lock wait, 120s statement, 150s transaction). Historical confirmation
precision intentionally changes: an as-of read returns an actual retained earlier
confirmation, never a future one. Discarded intermediate times are unrecoverable.

Production migration did not commit; all three normalized timestamp tables were
absent on final check. Total size 1,101,221,011 bytes. Cleanup last success was
18:55:07 UTC; no claim of current operation. Health advisors found no issues;
security advisors and database connections timed out. Production migration,
indexed latency/storage measurements, advancing cleanup counters and adapter
deployment remain outstanding. Local commit is held from push because push
automatically deploys adapters requiring these uninstalled RPCs.


### Production history retry — 2026-11-18

Price normalization reached PostgreSQL but timed out in CREATE TABLE AS
(SQLSTATE 57014, 20:01:13 UTC). Confirmed rolled back: no price timestamp table
and no active migration. Last measured size 1,101,245,587 bytes. EXPLAIN identified
a full-history index scan/incremental sort. Bulk extraction now uses sequential
reads, typed comparisons, bounded 64 MB work_mem and no parallel workers for that
step only; transaction-local settings reset afterward. Runtime writer/read
functions and app request volume are unchanged. All endpoint/isolation fixtures
pass; 200-repeat synthetic measurements are 57/48/43 ms, not production timings.
Read-only production probes exceeded 45 seconds; revised migration initialization
and subsequent status connection timed out. Production migration/deployment and
below-500-MB verification remain blocked; push is held to avoid premature deploy.

Offline Shiny lifecycle rerun passed: 13 checks/eight tabs plus full server
login/navigation/refresh/logout checks.


### Resumable history normalization — 2026-11-18 production completion

All three cutovers acknowledged after per-identity bidirectional retained-row
equality checks and zero pending work. Verified source/endpoint counts:
price 444,610/77,298; match 196,129/49,042; finance 188,505/35,875. The final source
counts matched the verification ledger before cutover. Source writes during the
copy invalidate verification and requeue both old/new identities as appropriate.

Tests: resumable fixture passes discovery pagination, behind-cursor insertion,
rollback/resume, insert/update/delete tracking, A/B/A, legacy null keys, large JSON
payloads, premature-switch refusal, exact independent-JavaScript endpoint results
and public denial. Existing normalized-history/writer fixtures pass. Offline Shiny
lifecycle passes 13 checks/eight tabs. Concurrent database-session races were not
simulated in PGlite; lock ordering and trigger behavior were reviewed against
PostgreSQL 17 documentation. No real-account test transactions were used.

Performance: identity batches bounded to 200 entities and a 5–10s between-identity
budget, normally with a 25s statement limit. One 67MB/305-observation finance
identity needed 60s after materializing row hashes. Full equality remains mandatory.
Temporary data is scoped to a batch; source writes to its locked identities can
wait/retry. The tracker is removed at cutover. No extra API acquisition, UI work,
timers or caches were introduced. Indexed sampled production reads executed in
7.468/79.778/224.207 ms (price/match/finance), with no temporary spill; these are not
latency percentiles. All indexes are valid/ready.

Nine existing indexes were concurrently rebuilt with unchanged definitions and
rows. Database size is 495,479,955 bytes versus 1,101,270,163 before this rollout.
Exact relation/index measurements are in docs/history_batch_migration.md. The
500-million-byte target is met, with limited growth headroom. RLS notices on new
private tables are expected; unrelated pre-existing advisor warnings are recorded
with remediation references in that document. Cleanup status and worker heartbeat
were stale before push; automatic cleanup requires post-deployment verification.


Post-push verification: `f21f9fe` and earlier held adapter commits reached
`origin/2.0`. At 22:58:57 UTC there were zero active worker leaders; last cleanup
success remained 18:55:07 UTC (6,054 chunks / 2,980 checkpoints cumulatively).
Owner confirmed production is intentionally stopped. Therefore a current worker
write, deployed container version and automatic cleanup cannot yet be verified.
Resume those checks after owner restart; no manual cleanup or restart was used
to mask this limit. Final delivery-note edits have no runtime/performance impact.


### Whole-cluster headroom correction and compaction — 2026-11-18

The earlier 495,479,955-byte size was application-only. PostgreSQL template
databases add 15,082,526 bytes, explaining the dashboard's 0.511 GB; the earlier
quota-completion claim was premature. Verified cluster size after stopped-app
physical compaction is 347,508,913 bytes (application: 332,426,387), leaving
152,491,087 bytes under 500,000,000.

Sequential VACUUM FULL ANALYZE rebuilt worker_chunks, worker_jobs,
worker_checkpoints and worker_snapshots. All pre/post full-row fingerprints and
counts matched (4,922 / 1,889 / 711 / 4,640 rows). No historical table was modified;
all indexes are valid/ready and zero active worker leaders remain. Measured
base64-to-binary potential was only about 8.6 MB; types/precision/contracts were
retained while physical reclamation saved about 163 MB.

Performance review: exclusive maintenance locks were confined to the confirmed
stopped-production window. Smaller physical relations and refreshed statistics;
no app request/cache/timer/rendering or dependency changes. No automatic VACUUM
FULL was added. Row integrity, index health and whole-cluster size were verified;
no runtime code changes warranted rerunning app tests. Automatic cleanup still
requires a running worker and remains an explicit post-restart check.


## Column-type audit — 2026-11-18

Completed read-only live schema/value review; see [column-type audit](database_column_types.md). Whole cluster measured 347,549,873 bytes. Binary chunks offer approximately 8.55 MB of content savings, binary signatures 1.18 MB; no schema migration was performed. Preserve decimal averages, opaque IDs and existing time/amount precision. Documentation-only change has no runtime performance impact; app tests not required. Automatic cleanup verification still awaits the owner restarting production.


## Worker error and interval diagnostics — 2026-11-18

Implemented bounded safe startup/leadership, database status/SQLSTATE/transport and Futmondo rejection diagnostics. See [investigation](worker_error_diagnostics.md). Historical log window had zero quota HTTP 402s, five 500s; sampled PostgreSQL failures were query cancellations during earlier migration activity. Current policy remains 1800 seconds with no active leader; authenticated source reproduction and 45-minute production acknowledgement remain unverified. No extra API/database requests, retries, persistence, timers or UI work; diagnostic map bounded to 128 entries and repeated database errors throttled to 60 seconds. Eight new diagnostics tests, 40 existing worker tests, the updated native-rejection test and offline lifecycle (13 checks/eight tabs) pass. No schema or historical data changes; underlying source failures are not declared fixed.


## Endpoint traffic and bandwidth reductions — 2026-11-18 (Pacific)

Measured the 10,242-request window by endpoint; missing Content-Length prevents a complete historical byte ranking. Fixed live safeupdate rejection of interval changes: production now acknowledges 2700 seconds and cleanup resumes. Applied guarded checkpoint-ack and conditional-read RPCs; Python reuses only DB-confirmed unchanged chunks in an account-scoped bounded cache, and idle purchase UI polling falls from 12/minute to 1/minute. Added bounded aggregate body-byte telemetry without requests. See [egress review](worker_egress_review.md) for counts, contracts, measured savings, performance and quota limits. Six Python egress checks, SQL egress/cadence checks, eight diagnostic checks, 40 factual-worker and 13 clause-worker checks, R idle/action and scheduled-clause checks, and offline lifecycle (13 checks/eight tabs) pass. Legacy worker_schema suite has an identical baseline one-job assertion failure; no claim it passed. Monthly quota compliance and comparable post-deployment steady-state traffic remain unverified.


## Correlated Futmondo rejections — 2026-11-18 (Pacific)

Expanded source rejection diagnostics from two named codes to bounded machine codes, with keyed account/league/team/request/job/run references, per-process failure counts and gaps, and team context/argument comparisons. See [contract and limits](futmondo_rejection_diagnostics.md). Ten diagnostics checks, 40 worker tests and offline Shiny lifecycle (13 checks/eight tabs) pass. Performance review: failure-only bounded HMAC/counter work, maximum 512 entries, no additional requests/retries/timers or UI changes; no private payloads/IDs/credentials logged. Standalone team jobs and the combined finance handler are both possible callers; actual rejection cause and any acquisition fix await fresh production code/correlation evidence. No unsupported empty-success fallback or retry suppression was introduced.


## Financial evidence payload reuse — 2026-11-18 (Pacific)

Implemented current metadata reads plus account/state-scoped immutable payload reuse in Python; see [contract and measurements](finance_evidence_egress.md). Existing R helper cache was process-local and full financial-evidence reads bypassed the earlier snapshot cache. Live 581-row sample: 7,369,223 bytes full JSON versus 187,434 metadata bytes; metadata query 97.284 ms with no disk reads. New service-only view applied as 20261119004554, privileges verified; no evidence/timestamps/history changed. Six evidence Python tests and SQL reconstruction/private-access fixture pass, along with six egress tests, ten diagnostic checks, 40 worker and 13 clause-worker checks, and offline lifecycle (13 checks/eight tabs). Performance: one metadata GET for warm pages, bounded 128-ID fetches for missing payloads, 16 MiB/4,096-entry cache; no new source requests/timers/UI changes. Cold reads can use more DB requests; warm reads avoid large unchanged payload transfers. Monthly quota compliance and steady-state production bytes remain to be measured after deployment.


## Lineup bench constraint, picker priority and player status — 2026-11-18

Implemented the optional, initially unchecked full outfield bench requirement,
bench/unused-squad/starter picker ordering, and observed injury/doubt/suspension
icons on pitch/bench portraits and picker rows. Existing real-match team crests
were already present and were reverified. Contract and dated official comparison:
[lineup module](lineup_module.md), [feature comparison](feature_comparison.md).

Executed successfully with synthetic/offline data:

- `Rscript test/test_lineup_runtime.R`: 16 groups, including a known-answer
  4-3-3 optimum of 127 that must become a 4-4-2 optimum of 119 to leave DEF/MID/FWD
  cover; off-mode equivalence, impossible/disabled benches, distinct substitutes,
  missing scores, injuries, departures and multiposition restrictions.
- `Rscript test/test_lineup_independent.R`: existing exhaustive XI/bench checks
  plus twelve independent feasible-XI oracle cases across score orderings,
  multiposition and injury settings; negative and zero scores stay intact.
- `Rscript test/test_lineup_module.R`: all groups passed, including checkbox
  default/tooltip, no recalculation on toggling, stale-setting warning, pending
  cancellation, infeasible-draft retention, picker order after unsaved swaps,
  portrait/picker status labels, unchanged request counters and existing
  conflict/readback/context/departure save safeguards.
- `Rscript test/test_lineup_store.R`: source status and crest fields survive
  normalized persistence/hydration, with context isolation, credential exclusion
  and retryable persistence errors preserved.
- `Rscript test/test_lineup_fixtures.R` and
  `Rscript test/test_award_fixture_calendar.R`: source logo mapping, actual worker
  codec restart reuse, legacy fixtures, original clocks, context isolation,
  calendar evidence reuse and malformed-evidence reacquisition passed.
- `Rscript test/test_lineup_browser.R` with
  `PLAYWRIGHT_BROWSERS_PATH=/tmp/futmondo-browsers FUTMONDO_EDITOR_TEST_PORT=8543 node test/test_lineup_browser.cjs`:
  320, 390 and 1440px passed, covering the unchecked usable checkbox/tooltip,
  explicit constrained calculation, live picker membership/order, status icons
  and accessible tooltips, twenty fixture crests, no overflow, dense/no-forward
  formations, immediate picker interaction, manual edits and one synthetic
  verified save. Source/database counters stayed unchanged for local interactions.
  Inspected the 390px screenshot. Fixture data and intercepted image assets are
  synthetic; this was not an authenticated account test.
- `Rscript test/test_shiny_simulation.R`: 13 lifecycle checks plus eight tabs,
  including no live HTTP/database mutations, passed after the final UI changes.
  Existing Plotly empty-trace warnings remained non-failing.
- Scoped `git diff --check` passed. No dependencies changed, so no deployment
  manifest regeneration was needed.

Performance review: twenty 100-player/all-formation runs averaged 41.1 ms with
coverage off and 64.0 ms with it required; independent warm optimization took
37 ms. Enabled coverage uses at most 3,360 lattice states, with about 2.6 MiB for
100-player reconstruction matrices, released after calculation. Warm picker
opens measured 108/41/57 ms at 320/390/1440px in the final browser run. The added
membership key keeps the existing sort complexity; fixed-size font icons and
wrapping labels avoid new image transfers and overflow. No new API/database
requests, timers, recurring work, cache, persistence format or source acquisition
were added. Shared durable score/roster/calendar observations remain the source
of data across sessions and restarts. Calendar parsing measured 313 ms for 380
matches in the worker; existing local fixture selection averaged 17 ms.

Official public code and captured status codes were inspected without logging
credentials, account/player details or private response bodies. Public Real
Madrid and Barcelona crest assets returned HTTP 200 PNGs. No authenticated
`league/matches` response existed in the available captures; deployed match-logo
coverage, deployment and hosted visual acceptance remain unverified. This task
does not claim to have fixed a demonstrated production crest acquisition defect.


## Admin worker activity by account and route — 2026-11-18 (Pacific)

Implemented the admin-only route matrix, shared-process totals, durable sampled
activity and visible-page asynchronous polling. See [monitor contract](admin_worker_monitor.md).
The current account comes from the authenticated token; no browser-supplied scope,
credentials, raw account IDs, descriptors, bodies or backend exceptions are displayed.

Executed successfully with isolated/synthetic data:

- `test/test_admin_worker_monitor.R`: eight focused groups, plus two actual SQL
  JSON projection fixtures. Covers independent authorization, strict projection,
  own/other route counts, positive/zero/stale styling, unavailable states, serialized
  refreshes, visible-page polling, cancellation and late replies, and the overall
  20-second deadline including queued work.
- `node test/test_admin_worker_monitor.cjs`: nine SQL groups, including old process
  records, expired leadership/leases, ordered activity updates and same-sequence
  heartbeat freshness, owner/run/generation changes, disabled/excluded/blocked
  account contexts, browser-role denial, safe sentinels and row limits. SQL and
  Python allowlists match all 85 routes. The migration is appended unchanged to
  the fresh-install schema.
- Python worker activity/core/diagnostic/cleanup/cadence suites: 61 tests passed
  in 30.002 seconds. Six new activity tests verify reuse of existing guards,
  allowlisted payloads, delayed phase visibility and final cleanup.
- `test/test_admin_worker_monitor_browser_app.R` with the matching Playwright
  check: six viewport/theme combinations (320/390/1440px, dark/light), plus hidden
  browser, navigation and admin-loss checks. Correct route counts (2/1 and 0/1),
  computed positive/zero/stale colors, wrapping, no horizontal overflow, stopped
  real 15-second polls and rejected late/forged refreshes. A delayed two-second
  response kept heartbeat interactions responsive at 84–154 ms. The table precedes
  collapsed worker details, whose keyboard toggle also passed. Screenshots inspected. All
  transport/source/database calls are synthetic or forbidden in this fixture.
- Existing Admin browser fixture: eight viewport/theme combinations (320, 390,
  768, 1440px), all five sections, settings/disclosures/navigation, no browser or
  Shiny errors and no timezone-directory reads. Settings rendered in 820–834 ms;
  heartbeat responses took 38–87 ms. Existing Admin configuration (two groups),
  league-settings (six groups) and awards/coach (11 groups) checks passed.
- `Rscript test/test_shiny_simulation.R`: 13 offline lifecycle checks plus eight
  tabs passed on the isolated task candidate; no live HTTP/database mutation.
  Existing non-failing Plotly empty-trace warnings remain.
- `Rscript test/test_deployment_contract.R`: three groups passed. New runtime
  module and changed R checksums are included; credentials, tests, internal docs
  and development scripts remain excluded. No dependencies changed.

Performance review: worker activity adds bounded JSON to the same existing guard
requests and row update, with no additional source/DB call or timer. Per-process
activity is limited to the two existing lanes; the database retains one <=2 KiB
sample per job, not an event stream. The admin view performs at most four metadata
reads/minute per visible session plus serialized explicit refreshes; hidden or
unauthorized views do none. Metadata projection avoids factual/checkpoint payloads
and uses a partial owned-job index. A PGlite fixture with 10,000 idle jobs, 132
active jobs and 67 recent workers executed in 11.446 ms; bounded response was
43,524 bytes (normal two-job response 1,576 bytes). Rendering is bounded to 64
workers/128 jobs, with details collapsed. Short requests can finish between
samples; the UI reports latest sampled activity, not an instantaneous wire trace.

Production migration `20261119030721_admin_worker_monitor` was applied before
releasing code. Read-only verification at 2026-11-19 03:07 UTC found one live
worker/two active jobs, ten retained old worker records and zero expired leases;
old workers correctly reported unknown endpoint activity. The monitor query took
9.921 ms with zero disk reads; the partial index is valid and worker_jobs RLS
remains enabled. All three new functions are security-invoker, denied to anon and
authenticated roles and granted to service_role. Security/performance advisor
findings are unchanged from before the migration; health advisor is clear.
No account/job/history contents were changed by verification. Hosted UI acceptance
and updated-worker telemetry remain separate deployment checks; no claim of a
complete HTTP trace or resolution of unrelated source-data failures is made.


## Opposing match-crest placement — 2026-11-18

The away crest now follows its club name visually, placing both match crests at
opposite outside edges. One local CSS ordering rule changes presentation only;
existing accessible club labels, 24px images, lazy loading and failed-image
handling remain intact. The existing browser geometry assertion now verifies
both outer-edge positions and name/image separation.

`test/test_lineup_browser.R` plus `test/test_lineup_browser.cjs` passed at
320/390/1440px using the installed Playwright browser and local fixture port
8544; inspected the 320px screenshot. Long names wrap without overflow, and
existing edit/optimize/save flows and request counters pass. Scoped whitespace
checks passed. Performance review: unchanged node/image counts, layout
complexity, API/database traffic, cache reuse, reactive work, timers and memory;
only flex-item order changes. No additional runtime performance test, dependency
or full lifecycle run is required for this module-local CSS adjustment. No
production deployment or authenticated hosted acceptance is claimed.


## Doubtful-only lineup option — 2026-11-18

Changed the checkbox and optimizer argument to `include_doubtful`, off by default.
A shared lineup-only status predicate always rejects injury, long-term injury,
suspension, unavailability and explicit `available = FALSE`; only source
`doubt`/`doubtful` can be admitted by the checkbox. Explicit exclusions/departures
still take precedence. The complete-score preflight uses the same predicate, so
missing excluded-player scores do not block an otherwise valid calculation.

Executed checks:

- `Rscript test/test_lineup_runtime.R`: 17 groups passed, including every hard
  status in both checkbox states, starter/bench doubt inclusion, full-coverage
  infeasibility, explicit exclusion precedence and existing score calculations.
- `Rscript test/test_lineup_independent.R`: exhaustive XI/bench oracle and twelve
  constrained cases passed with independent status filtering, permanent injury
  exclusion and optional doubt; signed/zero scores remain unchanged.
- `Rscript test/test_lineup_module.R`: all groups passed, including default/label,
  matching history requirements, cancellation before late publication, no
  automatic recalculation, hard exclusions despite missing histories, and
  existing departure/context/conflict/readback checks.
- Offline `test/test_lineup_browser.R` plus `test/test_lineup_browser.cjs` on port
  8545 with the installed Playwright browser passed at 320/390/1440px. Default
  optimization excludes the doubtful defender/forward; opting in admits the
  doubtful defender while the highest-scoring injured player stays excluded;
  opting out and explicitly optimizing removes doubtful selections. Existing
  order, icons, opposite fixture crests, manual editing and synthetic save flows
  pass; local interactions leave API/database counters unchanged.
- Scoped whitespace checks passed. No shared infrastructure, startup or global
  availability predicate changed; the focused suites cover this lineup policy.

Performance review: twenty 100-player/all-formation runs averaged 41.3 ms with
bench coverage off and 63.0 ms with it required; the independent warm case took
36 ms. Picker opens measured 39/72/42 ms at 320/390/1440px. Eligibility adds one
linear status-vector check; the bounded optimizer, rendering and memory limits
remain unchanged. There are no new requests, timers, recurring work, result
caches or persistence formats. Existing durable roster/status/history snapshots
retain their account/league/season/scoring context and source clocks. No new
dependencies or environment settings; manifest regeneration is unnecessary.
All execution was local/offline with synthetic accounts. Deployment and hosted
acceptance remain unverified.


## Loading animation pacing — 2026-11-18

The existing canvas loop now runs for 19.7 seconds. The table-football figures
rise more slowly and remain fully raised for about 1.5 seconds. After the
upward dissolve, the formed flock moves for 4.3 seconds before radar assembly.

Browser checks passed at 320, 390 and 1440 px, plus three fullscreen phone
sizes, in both themes. Tests verified the fully formed cloud keeps moving late
in its stage, the upward dissolve, exact loop seam, and alternate club kits.
The offline Shiny lifecycle passed 13 checks and eight tabs. All three
deployment manifest checks passed.

The loop spends more time drawing the 3D stage. The 30 FPS cap and particle
budget are unchanged. Browser p95 3D drawing took 1.3–1.5 ms at standard sizes
and at most 6.7 ms at 4× CPU throttling, including the outlined kit fixture.
Timing is local to the renderer and adds no requests, persistent state, or
delay to application readiness.

## Missing Lineup fixture crests — 2026-11-18

Read-only production diagnostics found a successful, context-matched calendar
with 38 rounds / 380 matches. Round 13 selected ten games correctly; the actual
Shiny render also produced ten cards after synthetic identity substitution. The
user confirmed that reloading restored the section and supplied a screenshot
showing missing crests. Every persisted calendar logo field was empty (760/760).
The existing club snapshot held twenty logos and exactly matched all twenty
next-round club IDs. Fixed pure-display replay resolved twenty of twenty crests,
without acquiring from Futmondo or changing any database rows. Public client
contract and hash are recorded in `lineup_module.md`.

The fix uses the existing shared `get_real_clubs` database resource only when a
selected fixture lacks its own logo. It joins by source ID, preserves direct
logos and match names, rejects foreign/ambiguous/unverified observations, and
keeps separate original catalogue and calendar clocks. Games stay visible while
crests load; the draft and manual-action protections are unchanged.

Validation:

- `test/test_lineup_fixture_crests.R`: actual worker codec and factual adapter;
  synthetic delayed database reads, two sessions with decoded/process caches
  cleared, exact-ID matching despite different names/order, version-1 calendars,
  direct-logo precedence, duplicate IDs, scope isolation, failure/partial/future
  rejection, stale timestamps and zero source calls. Each cold session uses one
  coalesced saved-catalogue read; additional flushes add no reads or writes.
  Two local publication samples were 236/117 ms, including test flush overhead.
- `test/test_lineup_fixture_crests_browser.R` and `.cjs`: 320/390/1440px, light
  and dark; ten visible games before catalogue publication, twenty loaded image
  elements afterward, opposing crest placement, unchanged draft/read-write
  counters, no overflow or browser errors. Images use intercepted synthetic
  assets; this does not measure CDN/mobile-network reliability. Screenshots
  inspected. Initial test setup issues (pending-output waiting and theme
  selector) were corrected before final verification.
- Existing fixture and worker-calendar checks passed (including eight calendar
  groups), as did the Lineup edit/save module suite. The 380-fixture parser took
  326 ms; existing selection/validation averaged 18 ms. Required offline
  lifecycle passed 13 checks plus eight tabs with zero real HTTP/database
  mutations; existing synthetic warnings remained visible.

Performance review: the ID join handles at most 50 selected matches against a
2,000-row bounded catalogue (observed size: twenty clubs / 2,073 encoded bytes).
It adds one cold database resource interest and reuses the existing bounded
shared cache; it adds no source request, write, timer or worker job. Both input
observations remain durable and reusable after restart. Direct-logo calendars
skip the catalogue read entirely. Only the fixture output depends on catalogue
publication; the pitch/draft stays mounted. Two lazy images per match were
already part of the UI contract. This is an existing feature repair, so feature
comparison/login tiers are unchanged. Hosted authenticated rendering and
physical-phone latency remain separate from these local checks.

Final isolated delivery checks passed against the exact prepared task tree:
crest adapter/codec, fixture/calendar, Lineup edit/save, deployment exclusions
and offline lifecycle (13 checks/eight tabs), plus all six responsive browser
combinations. Isolated publication samples were 219/112 ms; 380-match selection
averaged 17 ms. All twenty observed public CDN crest URLs returned HTTP 200.
Only the two changed runtime checksums/fingerprints are included; unrelated
working changes remain excluded. No dependency/schema change or production
data mutation was needed.

Deployment verification: runtime fix `44ef1cb` was committed and pushed to
`origin/2.0`. At 2026-11-19 04:34:46 UTC, the public
`https://ligaradar.com/release-info.json?lineup-crests=44ef1cb-check2` marker
matched both committed Lineup module/runtime SHA-256 values. Automatic source
rollout is verified; authenticated post-deployment rendering on the user's
phone remains separate. This verification note has no runtime/performance effect.


## Player-card match history — 2026-11-19

- The authenticated player-match capture reproduces the conflicting enclosing
  season label and the matching summary's number-only history. A synthetic
  calendar tests exact ID repair, missing kickoff recovery, duplicate and
  mismatched IDs, wrong account/championship/competition/season, and an undated
  score with incomplete schedule evidence.
- `test_player_card_database_display.R` restores encoded worker snapshots
  through the real card loader and asynchronous adapter. It verifies that the
  fixture pairs with its negative score, a zero score stays visible, the two
  dates have distinct provenance, and Refresh reuses persisted data. The
  existing award-calendar test verifies database acknowledgement and reuse of
  the underlying source snapshot without another bulk request.
- Trend, profile, recent-round association, competition-context and chart
  stability checks passed. The offline lifecycle passed 13 checks and 8 tabs.
  A dedicated Chromium test of the actual card and disclosure registry using
  those persisted synthetic fixtures passed at 360, 390 and 1440 px. Each
  viewport showed the paired score and match, one history notice and no modal
  overflow.
- The supplemental full-app competition browser fixture timed out at its
  market-table startup selector before opening a card. The legacy progressive
  test expected five startup reads and failed the same assertion with the
  pre-change player module. Authenticated hosted verification, including exact
  match-ID overlap in the reported league's saved calendar, remains pending.
- The display adds one league-scoped interest in the existing batched worker
  snapshot read if the calendar is cold. Refresh and player switching reuse
  that calendar. The shared reactive computes one association for recent
  rounds and chart dates. Chart updates depend on visible point/date inputs,
  and the mobile dialog renders one history diagnostic instead of a row per
  gap. There are no additional source calls, database writes, worker jobs,
  polling timers or dependencies. A local CPU check ran 100 warm projections
  of 380 calendar fixtures and 12 scores in 2.185 seconds total.

## 2026-11-20 UI wording, light-mode pitch, market funds filter, league formations

- Formations: `lineup_available_formations()` now honors league `configuration.allowExtraStrategies` (official client bundle main.dart.js, inspected 2026-11-20: league admin toggle stored next to `allowPlayerMultiposition`). When TRUE, `pro` formations are unlocked for all teams. Code-level evidence only; not yet confirmed with a live league response. Test added in `test/test_lineup_runtime.R`.
- Market (Today) adds a "Value within available funds" filter using the already-displayed `spendable_budget` (`round_finance_display()`); unknown funds show no rows. No new API/DB requests.
- Rivals: selected row highlighted with an accent bar; the details section shows the rival name as a title. Removed redundant info texts; reworded transfer-method notice. Squad-capacity info now shows only the effective-count detail. "Pending offer confirmations" renamed "Offers awaiting confirmation".
- Light mode: brighter pitch grass and higher-contrast player labels and bench (CSS only).
- Login Essentials: added "Dates and times in your timezone".
- Performance review: CSS/text changes and one reactive derived from existing in-memory states; no added requests, timers or caches. Checks: `test/test_lineup_runtime.R` (17 passed), `test/test_shiny_simulation.R` (passed).
- Not changed: empty round awards ("Saved coverage could not be fully read") and empty history plots are data-read failures (`rival_finance_evidence_latest` pagination incomplete; `user_team_history` returning no rows); require investigating the production database.

## 2026-11-20 History context fix (plots / round awards)

- Diagnosis (read-only, production DB): 60,994 `user_team_history` rows for league 5b55f9d7… were stored as `competition_id/season = unknown`; 285 under `504e…/2025-2026`. Cause: writes and reads resolved context from 5-minute in-memory metadata and fell back to `unknown` after expiry or without an account in scope.
- Fix: `league_history_scope()` (competition_context.R) resolves the stable identity from persistence context → account memory (stale allowed) → single agreeing memory entry → latest complete `league_context_observations` row (cached per league, 300 s known / 60 s unknown, max 256 entries). `league_persistence_scope()` uses it.
- Writes: `supabase_post()` refuses `user_team_history`, `player_history`, `player_daily_snapshots` when competition/season are unknown (returns FALSE, logs `context_unknown`; data is re-acquired by the next sync).
- Reads: `get_league_standings_history()` / `get_league_finances_history()` return an empty frame tagged `context_unknown` instead of querying `unknown`. Plots show "season could not be resolved" or a partial-history notice when the row cap is hit. Award evidence reads fall back to the stable identity only when live metadata is unknown; the awards message distinguishes an unresolved season.
- Live check: cold cache resolves from database, standings history returns 285 rows / 10 teams, complete.
- Tests: new `test/test_history_scope_context.R` (6 passed); competition_context, competition_history, history_persistence(+recovery), league_history_incremental, rival_history_context, normalized_history_writes, player_history_incremental, worker_cold_context, worker_context_handoff, rival_persistent_cache, shiny_simulation pass. `test_live_read_context` and `test_round_history_recovery` fail identically on the pre-change baseline.
- Performance: at most one small DB lookup per league per 5 minutes when memory context is absent; no source requests, timers or unbounded caches.
- Pending: `scripts/migrations/DRAFT_20261120_relabel_unknown_team_history.sql` is a reviewed-only draft (ROLLBACK by default); not applied. Historical `unknown` player_history/daily snapshots not addressed.
- Migration applied 2026-11-20 (owner request): season start confirmed as 2026-07-30T12:58:37Z (opening `budget` movement, 300,000,000). 59,682 rows relabelled to `504e…/2025-2026` (120 batches, 0 failures); 1,310 NULL-`observed_at` rows and 2 colliding duplicates remain `unknown`. Labelled total now 59,967.
- Follow-up found: standings history for this league now exceeds the 20,000-row read cap, so plots show the partial-history notice (first rows from 2026-09-14 only). Needs server-side downsampling or a raised/cursor-based cap.
- Daily standings history (2026-11-20): `scripts/migrations/20261120_team_history_daily.sql` adds `league_team_history_daily(championship, competition, season)` (last observation per team per UTC day, service-role only) plus a supporting index. `get_league_standings_history()` calls it via `read_league_team_history_daily()` (paged 1,000, cap 20,000 ≈ 10 teams × 5 years) and falls back to the raw read when the function is absent (HTTP 404). Not yet applied: no direct Postgres access from this environment. Test: `test_history_scope_context.R` 7 passed; offline lifecycle passed. Performance: ~10 teams × ~115 days ≈ 1,150 rows instead of 59,967; 2 requests instead of 20+.
- Daily history function applied by the owner via the SQL Editor (2026-11-20). Live check: `get_league_standings_history()` returns 462 rows (10 teams, 48 days, one row per team-day, no missing points/values), status ok, 1.6 s, 2026-09-14 → 2026-11-20. Both plots build 10 traces with no partial notice. Recorded history starts 2026-09-14 (nothing earlier exists); 10 gaps of 2–5 days are days with no observation and are not filled. Known cosmetic issues: Set2 palette repeats colours beyond 8 teams; a big.mark/decimal.mark warning in hover formatting.
- Plots palette (2026-11-20): `plots_team_colors()` gives up to 12 distinct colour-blind-aware hues (HCL beyond 12), keyed by sorted team name so a team keeps its colour across Standings, Squad values and Rank progression. Hover numbers now use `.` thousands / `,` decimals (no warning). Live: 10 traces, 10 distinct colours. Round awards: saved-evidence read now resolves `504e…/2025-2026` from the database and completes (581 items, 12 award snapshots), so the "Saved coverage could not be fully read" condition is no longer met; not observed in a logged-in UI.
- Round awards (2026-11-20): all 12 published rounds (classification evidence for rounds 1–12) have a saved, final 11-player selection; none missing, so Admin retrieval is not needed. The card still showed nothing because `award_saved_rows()` compared stored `competition_id` with live 5-minute metadata, which returns `unknown` after expiry; all 12 were rejected. It now falls back to `league_history_scope()`, still matching exactly. Live: 12 rounds accepted. Award suites unchanged vs baseline (`test_award_initial_catalog`, `test_award_scoring_contract` already fail before this change); test_history_scope_context 8 passed; offline lifecycle passed.
- Award test fixtures (2026-11-20): `test_award_initial_catalog` and `test_award_scoring_contract` passed before 76dec9b and failed from it on. Since that commit, display reads go through saved observations whenever the factual worker is configured (`fm_worker_configured()` is TRUE unless offline), so the fixtures' direct-source HTTP mocks were never reached ("Database access is not configured"). The fixtures now set `fm_worker_configured<-function()FALSE` to keep exercising the direct path; both pass (6 and 5 checks). Behaviour unchanged.
- Stable competition sweep: added `league_stable_competition()` / `league_stable_season()` (live metadata first; `league_history_scope()` only replaces `unknown`). Switched `rival_finance_evidence()` stamping, `read_rival_finance_evidence()`, `build_rival_cash_dataset()` evidence filter, `award_saved_rows()`, `get_mvp_saved_catalog()`, award source state, `read_player_match_history()`, locker-transfer evidence season, MVP/round-lineup/classification-ranking resource contexts. Parsing of source-provided seasons (history_repository, player tables/cards, normalize_league_rules) left unchanged. `league_history_scope()` skips its DB lookup inside asynchronous adapters (they carry their own context).
- Tests: test_history_scope_context 9 passed; 112 related suites run with and without live credentials. Without credentials, 20 fail identically at 7003860 (pre-existing); none newly failing. With `.Renviron` loaded, 4 suites (classification_live, locker_async, pressroom_incremental, live_read_context) fail at HEAD and after alike — tests pick up real credentials; run tests with `R_ENVIRON_USER=/dev/null` or without `.Renviron`. Live: stable context resolves from DB; 12 award rounds; 462 daily history rows.
- Correction (2026-11-20 triage): the earlier "none newly failing" claim was wrong; it compared against 7003860, which already contained 312d571. `test_worker_gui_readonly` and `test_rival_saved_bonus_recovery` pass at 312d571^ and fail from 312d571 (3/3 runs each). Both fixtures use no resolved league competition/season; the new guards refuse unknown-context history and rival-evidence reads. 16 other failures predate this session (first bad commits 2026-11-03 to 2026-11-18, mostly 76dec9b and 2b502ee); `test_locker_transfers` and `test_worker_contract` (missing `sodium` package locally) never passed at their last edit.
- Test repair (2026-11-20): 11 of 20 failing suites fixed; 9/112 still fail (best_xi_crests, classification_cold, read_diagnostics, rival_inactive_participants, rival_notification_dates, rival_point_settlements, player_valuation_persistence, locker_transfers, worker_contract). Causes fixed: (a) worker-first display reads since 76dec9b: fixtures that test direct acquisition set `fm_worker_configured<-function()FALSE` (acquisition_availability, read_request_contracts, rival_cash_reconciliation); fixtures of the saved path now recognise `intent="manual_action"` and answer the database "read" RPC with nothing newer (capacity_observation_completeness, card_live_capacity_recovery; card player row gets an observation time); (b) login gate since 2b502ee: settle the startup DB check before login (banner_finance, refresh_stability); (c) heartbeat threshold 60 -> 180 s in 0ca8ffa (worker_status_redraw); (d) player_availability.R not sourced (ui_reliability); (e) removed squad-capacity sentence (banner_finance). My 312d571 regressions fixed by allowing "unknown" as an archive scope for history and rival-evidence reads (writes still refuse unknown; the daily RPC is used only with a resolved season). Offline lifecycle passes.


## League refresh after manual transactions — 2026-11-22

The floating button previously reloaded persisted observations without advancing worker jobs. Confirmed transfers similarly invalidated local reads, so the dashboard could continue showing the old budget, ownership or pending transfers until the worker's routine cadence. The new service-role RPC advances the affected league jobs, and the worker checks source data, publishes acknowledged snapshots and updates the existing Shiny subscribers.

The PGlite migration check exercised authorization, scope, active leases, backoff, duplicate clicks, prioritization and request consumption. A six-job request with 5,000 unrelated jobs took 4.2–6.0 ms across the isolated checks. The worker tests verify that a fresh request reaches the mocked Futmondo source even with a saved checkpoint, that its retries reuse the new checkpoint, and that balance snapshots older than the request are skipped. The focused Shiny checks verify one scheduling RPC, persistent prior display, changed-version rendering and removal of verified incoming players from the available list. The final offline lifecycle passed 14 checks and eight tabs, including no HTTP or live database mutation attempts. The Python worker suite passed 43 tests, refresh stability passed 11 checks, clause exercise passed six checks, and deployment passed three checks with matching scoped checksums.

Performance review: one asynchronous queue call per explicit action, no new browser poll or redraw timer, no global refresh revision for queued trades, and no new concurrency. Existing active work and source backoff are preserved. A forced catalog run uses one existing-snapshot read to avoid rediscovering unchanged players. Roster/history follow-up uses bounded, deduplicated summary jobs. New snapshots use the existing context, bounded cache, completeness and clock metadata. Isolated timings exclude live worker dispatch, internet latency, mobile browser rendering and production database load. Hosted end-to-end acceptance remains separate.


Production Supabase applied the migration as `20261123045201_worker_league_refresh` (2026-11-23 UTC, November 22 Pacific). Preflight matched the installed claim function to the expected predecessor. The migration used a five-second lock timeout and made schema changes without backfilling job rows. Post-migration checks confirmed the nullable column, enabled trigger, claim priority, service-only invoker grants and a recent worker heartbeat. The REST RPC returned HTTP 200 with zero queued jobs for a synthetic unmatched account. The security advisor reported no findings for the new migration objects. These checks establish the database rollout; they do not establish an authenticated Futmondo acquisition or hosted end-to-end timing. This documentation update has no runtime cost.


## Welcome page league refresh entry

Rendered the Welcome feature inventory and verified that the League refresh title and description are absent while Essentials and Advanced still render. The offline Shiny lifecycle passed 14 checks and eight tabs. Removing one static disclosure reduces initial markup and has no effect on refresh requests, database reads, caching, timers or reactive work.

## Completed market bid slot reconciliation — 2026-11-22 Pacific

The reported €3,210,413 hold was an accepted manual market bid. A later stored transfer matched the winning team, price and System/Market seller, and a newer balance was present. The old reconciler only checked whether the bid was still active. The app now matches a versioned pressroom purchase and newer balance directly, while live readback handles both applied bids and completed purchases. The legacy transaction row did not supply authoritative season metadata and was not used to release the live reservation.

The isolated R and PostgreSQL checks cover the matching purchase, stale balances, foreign scope, incorrect prices, and a repeated call. Manual-action tests still cover one submission and uncertain outcomes. The offline Shiny lifecycle passed 14 checks and eight tabs. Production migration `20261123050735_reconcile_completed_market_bids` used a five-second lock timeout. Post-migration checks verified the new restricted invoker function, an empty probe that affected zero jobs, and the updated live reconciler. LigaRadar served the matching release marker and both changed runtime hashes. A scoped repair decoded the exact-scope saved Futmondo pressroom snapshot, verified the purchase and newer stored balance, and called the same reconciliation RPC. It released one reservation; a separate read confirmed the slot and reserved amount were both zero. No Futmondo request or new offer was sent during the repair. Authenticated browser rendering was not exercised.

Performance review: no additional source requests, timers, or subscriptions outside the existing active pages. With a pending manual bid, the observer uses two stored versions already read by the page and performs one bounded, deduplicated async database write per qualifying version pair. Successful repairs reuse the existing clause poll and team lock. The database looks up both snapshot versions by account/key and updates jobs by primary key. No dependencies or settings change.


## Maximum clause input — 2026-11-22

Reproduced with offline player rows and Chromium. The former numeric input changed `135,0` to `1350` and displayed the €328M row. An incomplete number also reached Shiny as a blank input and removed the cap. The plain `135` entry already applied the correct limit in the fixture.

The shared player table filter suite passed 16 groups and UI reliability passed, including the maximum and verified-funds intersection. Chromium checks passed at 320, 390 and 1440 pixels in both themes. The €135M boundary was retained and €328M was excluded. Decimal points, the M suffix, an invalid entry and Clear Filters were exercised. The offline Shiny lifecycle harness passed 14 checks and 8 tabs with no HTTP or live database mutation attempts. Existing empty Plotly trace warnings were present. The hosted account was outside the verification scope.

The performance review found no new acquisition, database reads or writes, cache invalidation, polling timers or growing state. The filter reuses enriched rows and skips price comparisons when both limits are off. A 524-row synthetic comparison over 10,000 runs took 0.235 ms per pass with the prior cap logic and 0.085 ms with the updated path, including parsing. The six browser runs published filtered rows in 296–314 ms with the existing Shiny input cadence.

## Disconnect dialog — 2026-11-22

Chromium verified the hosted retry, final-message and recovery states with the upstream Shiny Server DOM reproduced in the offline app. A real local Shiny WebSocket close and reopen verified the direct-server path. The Refresh page button reloaded the same URL, including its query and fragment. At 360 px in both themes, the illustration and button fit, keyboard focus stayed in the dialog, and the previously focused control regained focus on recovery. The offline Shiny lifecycle harness passed 14 checks and 8 tabs with no HTTP or live database mutation attempts. Hosted behavior was checked through the fixture and upstream source. The production container was outside the verification scope.

The existing fullscreen loader browser suite passed both themes at 320, 390 and 430 px, plus landscape, reduced motion and 4× CPU throttling checks.

The performance review found about 3.5 kB of inline browser code and styles plus one static SVG. The closed dialog occupies no layout space. The browser test confirmed zero dialog asset requests and no loader canvas. A scoped observer watches the hosted reconnect message. The dialog adds no polling, Shiny reactive work, source acquisition, database operations or cache invalidation. A user-initiated reload follows normal page startup.

Refresh button follow-up. Chromium confirmed the amber color on the next animation frame in both themes and the subsequent same-URL reload. One 180 ms timer runs per activation and repeated clicks are ignored while it is pending. The feedback adds no background timers, API or database calls, cache effects or server-side redraws.

Radar sweep follow-up — 2026-11-23. Chromium verified one four-second CSS rotation centered at midfield, a fixed pitch at 360 px, a still sweep under reduced motion and no animation after recovery. The existing button feedback and reload checks passed. The loader fallback retained its original static beam. The offline lifecycle harness passed 14 checks and 8 tabs. Only the small SVG beam group animates while the dialog is open. The sweep adds no canvas, JavaScript animation loop, asset request, API or database work, cache effects or Shiny redraws. Physical-device frame timings were outside this check.

## Immediate manual offer controls — 2026-11-22 Pacific

The held-source Shiny fixture rendered the full market editor in 11 ms, with zero Futmondo preflight requests and zero database admission requests before confirmation. Each confirmed offer sent one native request. Duplicate and stale confirmations did not dispatch. Simulated rejection and lost acknowledgement each requested one factual refresh for the captured league, team and player. A held database settlement did not delay the confirmed UI result.

The isolated Chromium run at 320 px and 1440 px measured the initial market action 3–4 ms after the popup appeared. Across market, direct-owner and clause cases, the subsequent editor/confirmation appeared in 112–1,037 ms. The fixture also covered one-second held responses, visible waiting status and locked controls, native rejection, successful confirmations, idle Escape cancellation, pending Escape handling and no horizontal overflow. These browser timings include local fixture startup and automation overhead.

The database guard adds one bounded, indexed admission RPC before the offer when scheduling is enabled. Its existing account/job bounds and team lock limit the work. Settlement and factual refresh proceed asynchronously. The card reuses scoped durable worker results with original deadlines. There are no added polling loops or repeated acquisition reads. Rendering an empty history chart no longer loads Plotly ahead of actions. The offline tests covered loaded chart stability, account/league/player isolation, native capacity reads for other actions, reservation enforcement in isolated PostgreSQL, and the Shiny lifecycle (14 checks plus 8 tabs).

The broad deterministic suite reproduces one pre-existing sealed own-bid capacity fixture failure on the unchanged branch. Browser and database verification used synthetic data. The hosted migration, deployment, real Futmondo latency and real transactions were outside this verification.

## Active manual offer reconciliation — 2026-11-22 Pacific

A reported 38,016,571 EUR manual market bid returned `api.general.ok` and
was recorded as accepted. The exact-scope team market subsequently contained
the own bid at that amount, and the newer balance had sufficient withheld
funds to cover it. The saved-data reconciler only handled completed transfers.
Until the separate reservation settled, the summary could charge the amount
and occupy a squad slot through both the active offer and the manual hold.

The focused R fixture verifies exact own-bid matching, stale and ambiguous
evidence, accepted and unconfirmed wording, and retry after a version race.
The isolated PostgreSQL check verifies service-only execution, exact scope,
version and source times, and one-time release. The completed-market-bid R
and PostgreSQL checks, banner finance (18), unchanged-refresh display, and
offline Shiny lifecycle (14 checks and eight tabs) passed. The balance summary
browser fixture passed at 320, 390 and 1440 px in both themes, with navigation
under 45 ms in that fixture and zero repeated shared reads or live I/O.

With no pending manual actions the observer exits before reading source states.
While reconciling it reuses already loaded market, balance and optional
pressroom snapshots, scans a bounded batch of up to 64 reservations, and
coalesces matching writes per snapshot pair. The indexed database version
lookups and primary-key job update run in one asynchronous RPC. The existing
poll handles result delivery and retries. The old broad invalidation is
skipped when the same saved observations already proved the active offer.
There are no new source requests, timers or dependencies.

Production migration `20261123072054_reconcile_active_manual_offers` was
applied and the invoker grant verified. A scoped repair verified the latest
persisted market and balance digests, found the exact active offer, called the
same RPC and released one duplicate cash hold and slot. A separate read
confirmed both reservation values were zero. The existing Futmondo bid was
still active in the evidence used for repair. No transaction was submitted.
Hosted authenticated rendering remains outside the verification.

## Locked-clause diagnostic preflight message — 2026-11-22 Pacific

The owner’s screenshot shows a local acquisition preflight stop. The decision precedes reservation admission and the Futmondo purchase request. The screenshot cannot establish which preflight condition failed. The temporary test now shows the available local reason in a persistent notification when preflight or verification stops a request.

The synthetic 360 px Chromium check confirmed the actual test button and confirmation show a spendable-funds block before dispatch. The existing scheduling flows passed at 360, 390 and 1440 px. The asynchronous purchase checks and the offline Shiny lifecycle (14 checks and eight tabs) passed. No real account or transaction was used. The hosted preflight cause and the native closed-clause response remain unverified.

The local block uses one notification in place of two and reuses the existing preflight result. It adds no acquisition, database operations, cache invalidation, polling, timers or reactive reads.

## Scheduled clause activation and diagnostic retirement — 2026-11-22 Pacific

The owner's second screenshot shows `answer.code = api.error.not_found` on the dedicated purchase route for a player whose clause was reported locked. The official client bundle and two earlier successful captures confirm the route, authenticated envelope and field mapping. The public client presents the returned code through its shared response handler. The available evidence does not distinguish a temporary lock from other reasons the purchase target might be unavailable. The code remains a terminal rejection and the temporary confirmation and date bypass are removed. See [the dated source contract](scheduled_clauses.md#locked-clause-rejection-evidence-and-rollout).

Production was already enabled. Two read-only PostgreSQL statistics samples recorded 379,656 and 379,678 PostgREST `clause_claim` calls, 28.7 seconds apart. This confirms that the elected worker was actively checking for due jobs. No production flag, job, credential or consent was modified, and no real purchase or purchase request was submitted for verification.

The 14 isolated Python worker tests include the reconstructed native rejection and assert one POST, failed settlement and no rescheduling. Manual and asynchronous R checks confirmed that fresh future or unknown opening facts block dispatch. The scheduling R fixtures and Chromium checks at 360, 390 and 1440 px passed. Browsers verified the retired button stays absent with the old setting present, the scheduling dialog explains the rejection policy, and normal consent and cancellation still work. The offline Shiny lifecycle passed 14 checks and eight tabs.

Performance review found no additional requests, cache reads or invalidation, writes, timers or scheduler polling. The card drops one reactive flag and the diagnostic path drops its optional live read. Held fixture checks let browser heartbeat updates render in 92–146 ms. The existing isolated worker cold helper took 3.02 seconds and included seven preflight reads, with zero extra reads on the tested restart. These local timings do not measure production opening lag, resource headroom or a hosted purchase.

## Staged incoming lineups — 2026-11-23

- The offline lineup, lineup runtime, worker calendar, incoming-transfer and clause-persistence suites passed. The Shiny lifecycle harness passed 14 checks and eight tabs. Known-answer fixtures covered pre-kickoff filtering, already-owned players, simultaneous arrivals and unavailable players. The legacy lineup schema fixture had an obsolete upsert key on the baseline. The corrected fixture uses the established competition-scoped key.
- The isolated PostgreSQL migration and role tests covered repeatable installation, account isolation, kickoff gating, single dispatch under recovered leases, cancellation, and credential cleanup. The due lookup took 0.096 ms on the small fixture. This is a local query measurement, with hosted database throughput unmeasured.
- The browser fixture passed at 390 and 1440 px. It checked grayscale and full-color images, stage selection, required approvals, a declined cancellation, and one manual save after confirmation. Preview rendering took 236 and 233 ms respectively in the final fixture run, with zero additional lineup source or snapshot calls before confirmation. Calculating three stages across all formations for 18 owned and two incoming players took a median 22 ms over 20 local runs. The existing 100-player calculation remained at 40.8 ms mean for XI and bench, and 64.2 ms with required coverage.
- The Python scheduler exercised the real R bridge with mock HTTP. A pending signing required five source reads and zero writes. Its next attempt reused all three calendar responses, rechecked live lineup, roster, information and formation rules, and submitted once. A dropped response and helper restart required one lineup readback, with no second submission. An external edit was detected with one lineup read and persisted calendar reuse. The existing clause scheduler passed all 14 checks.
- The elected worker adds an indexed due lookup every five idle seconds when the feature is enabled. The existing account and connection limits govern HTTP. The helper closes after 30 idle seconds. Status polling runs only while Lineup is open. Worker checkpoints retain one scoped record per route and expire through hourly cleanup. Mobile layout and request counts were checked in the offline fixture. Hosted worker timing, actual VM memory under concurrent work, production migration, and signed-in Futmondo submission remain unverified.

### Welcome page visibility for incoming lineups

The full Welcome markup lists **Lineups for incoming signings** exactly once under Advanced with the scheduling flag unset, disabled, and enabled. Login feedback passed all five checks. The broader Welcome loading fixture failed its synthetic login assertion on both the committed baseline and this change. The inventory change renders one static disclosure in the existing responsive grid and introduces no API calls, database reads, timers, or worker activity.

### Production migration for scheduled lineups

Supabase project `ozhnextrcxxksswpnglt` recorded migration `20261123154622` as `scheduled_lineups`. The deployed repository migration has SHA-256 `4aa3d824df00784a6c09ac2d2c09825e9dc963cdd2b233ad5eff2ae39f63b8d4`. All three tables have row-level security and deny browser reads. Ten security-invoker functions allow server execution only, and all eight indexes are valid. A read-only service-role smoke test passed with a synthetic absent account. The empty due-job lookup used `lineup_schedule_due` and took 0.061 ms. This measurement covers the empty lookup. Security advisor reports the intentional server-only tables as informational findings because there are no browser policies. Worker rollout and live lineup submission remain unverified.

### Default incoming lineup previews after rollout

The production Welcome markup already listed incoming lineups while the production Lineup markup omitted the control. Supabase statement statistics showed no service-role lineup claims and existing purchase claims. The Lineup code was gating the preview on worker activation and initialized inclusion unchecked. The checkbox and preview now work with the worker flag off and default to including verified pre-kickoff signings. The arrival list uses the same squad and transfer observations as the optimizer, and shows the round-start result. Approvals and automatic saves still require the Dokploy flag.

The offline Lineup and optimizer checks passed. The Shiny lifecycle passed 14 checks and eight tabs. Browser checks at 390 and 1440 px passed with scheduling enabled and disabled, including a verified arrival selected in the XI, a post-kickoff signing excluded, and no source/snapshot calls during preview. Observed local mobile/desktop preview timings were 842/227 ms with scheduling enabled and 829/216 ms with scheduling disabled. The status list adds a bounded render from existing reactive data, with no new source or database calls, background work or timers. The local three-stage fixture took 22 ms median. The reported league's selection requires signed-in validation with its own recorded squad and round data.

## Browser favicon — 2026-11-23

Added a square field-and-radar SVG and a 16/32/48 px ICO fallback in the
shared application head. Chromium verified both declarations in the actual
rendered UI, HTTP 200 responses and the correct image content types (862 bytes
SVG; 15,086 bytes ICO). The existing offline browser fixture does not expose
`www` automatically; an isolated wrapper enabled that static folder with
fallthrough routing for the asset check. The rendered SVG preview was inspected.
ImageMagick confirmed all three ICO sizes. The offline Shiny lifecycle passed
14 checks and eight tabs; no real account or transaction was used.

Performance review: at most two small same-origin static asset requests on
initial load, reusable by normal browser caching. No API or database work,
reactive redraws, timers, blocking server operations or dependencies are added.
The icon does not affect mobile layout and contains no animation. Hosted
browser-tab appearance and deployment remain unverified.

### Coverage-aware reoptimization and schedule replacement

Optimize previously cleared the visible plan before calculating and treated an infeasible early stage as failure for the whole round. The staged preview now retains its previous result on failure. If the round-start squad can meet full coverage, the new plan omits early stages until enough eligible players arrive. The first save uses that arrival time. Local edits and recalculation leave the existing plan active. Confirming a replacement cancels and inserts under one team lock.

The known-answer fixture passed for coverage becoming feasible only after the second signing and for coverage remaining infeasible at kickoff. At 390 and 1440 px, the browser retained the signing in the new preview, deferred the first save, kept the old plan after declining the popup, and replaced it after confirmation. The existing browser run passed manual-save warning and cancellation at both widths. The isolated SQL tests covered exact consent, stale baselines, in-flight dispatch conflicts, one deferred stage, and duplicate requests. The Lineup module suite and Shiny lifecycle passed, including 14 lifecycle checks and eight tabs.

No extra source acquisition or polling was added. Preview work remains one local probe per stage and the three-stage fixture measured 22 ms median. The replacement costs one indexed team admission per click or confirmed retry. The manual save guard also checks for persisted schedules when scheduling is paused and the worker is configured. Local browser counters showed no new source reads or saves during preview and replacement. Production worker latency under load and native submission remain unverified.

Applied `20261123170000_replace_lineup_schedules.sql` as `20261123164123` in production, SHA-256 `f55984d1e6a995c08a60fb65ac7fb969df4bc5196c431d21703d887ccfdbc885`. The security-invoker RPC is executable by the service role and denied to both browser roles. A synthetic invalid admission was rejected without inserting a plan. Before the rollout, production statistics showed worker claims and status reads, with zero recorded admissions and no durable plans.


## Round-based league plots — 2026-11-23

Root-cause trace: Standings used `get_league_standings_history()` → persisted
`user_team_history`/daily rollup → `plots_history(points)` → `recorded_at` x axis.
It plotted every saved cumulative snapshot, including repeated totals and later
revisions. The old plot therefore represented changes between observations,
not per-round history. The screenshot does not prove why a particular native
score was revised. No source-account data was queried or rewritten for this task.

Standings and rank now use the existing worker-backed, database-persisted team
round histories and catalog identities, with a shared cumulative projection.
Numeric subrounds remain distinct; opaque IDs map through the saved catalog.
Duplicate team/round observations resolve once, negative/zero scores survive,
and catalog gaps or absent teams never become false zero totals. Missing
coverage leaves an explicit gap/status. The rank range filters after accumulation.
Other changes cover date-only valuation ticks/hover, a wider financial-metric
column, removal of the single-round inspector, muted blue bars, ownership crests
and counts, toolbar headroom, and descriptions for all five plots.

Executed validation:

- `Rscript test/test_plots_module.R`: eight regression groups, including
  independently specified known answers for totals, ranks, subround ordering,
  duplicate revision, negative/zero points, missing-round coverage and absent
  teams; plus date axes, hover, ownership crests/counts/colors and UI controls.
- `test/test_plots_browser_app.R` + `test/test_plots_browser.cjs`: passed all five
  chart tabs at 320, 390 and 1440 px in light and dark themes. Verified round
  labels and numeric values, hidden inspector, valuation formatting, desktop
  metric width, no horizontal page overflow, count labels, blue bars, three
  fixture crest images at readable width and toolbar below the tab bar.
  Changing the rank range caused no additional saved-data read calls. Inspected
  the final phone/light and desktop/dark ownership screenshots. Crest fixtures
  deliberately use synthetic images, not real account content.
- `Rscript test/test_shiny_simulation.R`: 14 checks and eight tabs passed on the
  initial and final runs. No real HTTP or live database
  mutations were attempted.

Performance review: 100 cumulative projections of 20 teams × 38 rounds averaged
7.35 ms locally; input 30,056 bytes, output 30,832 bytes. One cumulative sum per
team replaces repeated prefix grouping in rank progression. Both charts reuse
one reactive result; range changes only filter already computed rows. Standings
now renders one marker per round/subround rather than every timestamped
observation. The saved catalog adds one shared context-scoped snapshot read on
round-data invalidation, not an HTTP request; team-round reads were already
required by rank progression. No new DB writes, schema, acquisition routes,
background jobs, dependencies or timers were added. Existing durable snapshots,
context isolation, completeness/freshness state and worker cadence remain in use.
Crests reuse exact-ID source logos already present in the saved player snapshot;
at most one existing static image per club is requested by the browser. The
resize listener is replaced on rerender and relayouts only when dimensions differ.
Its initial readable-size correction and no-overflow behavior passed mobile
checks. Hosted throughput, real-account historical score revisions and deployed
appearance remain unverified; this is a local implementation and push.


## Standings blank/loading and responsiveness — 2026-11-23

The reported phone screenshot shows the new description with no standings
curve. The owner also reports unresponsiveness on desktop and phone. Tracing the
changed code reproduced a serial loading barrier: only the catalog registered
initially; resolving it registered team A; resolving A registered team B. Any
pending or failed dependency aborted the complete plot reactive. The prior
browser fixture replaced bound readers with immediately available getters, so
its passing result did not exercise this failing path.

The fix captures each read independently, registers all team histories together,
and shows verified curves without inventing the missing team's scores. Loading
status remains visible and recovers when saved data arrives. Failures retain safe
resource references. Ranks are withheld until every team's selected cumulative
coverage is known. Display projections suppress chart redraws and range resets
when worker publications change metadata only. Existing DB snapshots, contextual
isolation, persistence and bounded read batching are reused; source worker
cadence and HTTP lanes are unchanged.

Executed checks:

- `Rscript test/test_plots_loading.R`: four regression groups passed using real
  shared adapters, including catalog plus all teams registering while pending,
  numeric rounds displaying before the ID catalog, a rejected team preserving
  the other curves, unknown/zero/negative score handling and local range reuse.
- `Rscript test/test_plots_module.R`: all eight existing score/layout groups
  passed, with the incomplete-league rank expectation corrected to unknown.
- `test/test_plots_loading_browser_app.R` +
  `test/test_plots_loading_browser.cjs`: phone 390 px and desktop 1440 px passed
  using actual worker descriptors, compressed snapshot restore and full page
  scripts. Nine verified curves appeared while the first team remained held;
  all ten appeared after release, and loading status cleared. Three newer saved
  versions with identical scores caused zero chart publications. The publication
  counter was verified to be active. Twenty source-crest fixture images rendered,
  the page did not overflow, and input heartbeats remained responsive.
  Final measured heartbeats were 18/26/27/77/809/57 ms on phone and
  20/26/28/44/963/68 ms on desktop. One heartbeat in each run overlapped a
  publication; these are local fixture timings, not a hosted latency guarantee.
  Idle browser task work was 2.4/2.6 ms over 1.2 seconds, with bounded DOM size,
  zero page errors and zero Futmondo source calls.
- Existing `test/test_plots_browser.cjs`: all five tabs passed again at 320,
  390 and 1440 px in both light and dark themes, including no overflow, correct
  labels, crests/colors and range-read reuse.
- `Rscript test/test_shiny_simulation.R`: final run passed 14 checks and eight
  tabs, including no HTTP/live database mutation attempts.

Performance review: 100 projections of 20 teams × 38 rounds averaged 8.17 ms,
with 30,056-byte input and 30,832-byte output. The complete-coverage rank guard
adds a bounded finite-value check per round. Registering independent interests
allows one existing coalesced snapshot read instead of a chain of team waits;
it does not increase source concurrency. Data-only status outputs stay active,
while inactive charts remain suspended. Identical facts do not republish chart
widgets, and identical round domains do not reset slider selection. No new
polling, timer, dependency, database write, migration or retained cache is added.

Read-only production metadata found successful catalog/team-history snapshots
and no acquisition errors for these handlers, so unavailable source history was
not established. Public-client inspection of the cached official bundle on
2026-11-23 reconfirmed `/1/userteam/rounds` with `championshipId` and `userteamId`;
`ay6` maps each response record through `c5Z`, which reads `id`, `number`,
`status`, `points` and `negative`. Bundle source:
https://app.futmondo.com/main.dart.js; SHA-256
`08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c`.
This is parser evidence, not authenticated UI verification. Automatic review
blocked exporting private compressed response payloads; permission for exact
hosted-response inspection was requested separately. The reported hosted
freeze has not been independently reproduced. The local loading defect and
redraw behavior are verified; hosted acceptance remains outstanding.


## Approved saved-round response review — 2026-11-23

After the owner explicitly approved opening one complete saved team-round
response privately, retrieved exactly one most-recent successful
`get_user_team_rounds` snapshot. No account context, descriptors, directories,
catalogs, authentication or other teams' response bodies were exported. Verified
the compressed SHA-256 and decoded byte length before using the existing worker
codec. No private body, actual source identifiers, team names or score values
were printed or added to the repository.

The response is an unnamed list of 13 records. Each record has the native
`id`, `negative`, `number`, `points`, `status` fields. The inspected field
types are character, logical, integer, integer and character, respectively.
All 13 round numbers and scores are finite, covering rounds 1–13. The existing
classification parser retains all 13, and the cumulative projection retains
13 known results. This response provides no evidence of absent scores or a
parser/response-shape mismatch.

Verified the actual approved response through synthetic account/league/team
identities with the real compressed worker restore, factual adapter, module,
standings renderer and rank-range filter. Standings produced one 13-point curve;
the selected round range produced five rank points. No live Futmondo or database
read was needed during this replay. The snapshot itself was not modified or
republished. Ten local projection/build runs took 16.5 ms median and 98.0 ms
maximum. These are single-response local measurements, not hosted browser or
multi-user latency measurements.

The raw and decoded temporary response files were removed after the check.
This review supports the existing loading-path fix; one successful response does
not establish complete coverage for every league participant or reproduce the
reported signed-in production freeze. Hosted acceptance remains outstanding.

Delivery/performance review: documentation-only changes. No runtime code,
source request volume, DB schema/writes, cache invalidation, timers, UI redraws,
dependencies or deployment configuration changed. The targeted actual-response
replay is the applicable verification; the application lifecycle was not repeated
for this documentation-only record.


## Desktop points-axis crowding — 2026-11-23

The desktop screenshot showed an unreadable points axis and a dense green block
over much of the chart. Reproduction exposed a rendering defect in the
round-based chart implementation: the conditional `dtick = NULL` entry became
`dtick: []` in the browser layout. Plotly used a unit step, creating a score
label and gridline at every integer instead of choosing a suitable interval.

Before correction, a small fixture with a maximum cumulative score of 105
produced 119 y-axis labels, visibly overlapping across the chart height.
A pre-fix browser check using league-scale totals did not settle within its
30-second wait. This failure is recorded rather than reported as a successful
timing measurement. The excess SVG work provides a concrete rendering-related
explanation for the reported browser pauses; a signed-in hosted freeze was not
independently captured.

The points branch now omits `dtick` completely and requests automatic ticks
with `nticks = 6`. Rank still uses step 1 and reversed order. The saved-data
loading path, actual scores, cumulative transformations, unknown values and
round/subround labels are unchanged. No shared theme or stylesheet changes were
needed; unrelated working-tree edits to `theme.R` were preserved.

Verification:

- `Rscript test/test_plots_module.R`: nine groups passed, including a
  >1,000-point fixture asserting no points `dtick` key, automatic ticks and
  retained integer rank steps.
- `Rscript test/test_plots_loading.R`: delayed/failed saved-history cases
  passed, preserving known curves, unknown coverage and local range reuse.
- Strengthened `test/test_plots_loading_browser.cjs`: 390 and 1440 px, light
  and dark, passed with ten distinct league-scale score curves. Checks inspect
  real rendered SVG labels: 2–8 ticks, at most eight gridlines, no overlapping
  label rectangles, and full chart opacity. Held-history recovery, zero redraws
  for three unchanged-data publications, 20 crests, input responsiveness,
  bounded idle DOM/CPU, no horizontal overflow and zero source HTTP also passed.
  Final phone heartbeats were 48/21/76/93/810/64 ms (light) and
  47/17/27/93/1058/56 ms (dark). Desktop heartbeats were
  48/25/27/1010/10/67 ms (light) and 60/27/26/77/976/57 ms (dark).
  Occasional roughly one-second responses overlapped synthetic snapshot
  publications; these are local timings, not a hosted performance guarantee.
- The corrected desktop measurement at 1,190 points produced three tick labels
  and two gridlines, with 385 chart SVG/DOM descendants and full opacity.
  Fixture readiness took 4.674 seconds including held/asynchronous saved-read
  startup; total browser task time was 1.287 seconds, including 0.549 seconds of
  script and 0.034 seconds of layout. The pre-fix high-scale check timed out, so
  no numerical speedup ratio is claimed.
- Inspected the final full-page desktop dark screenshot and chart-only preview.
  Lines and axis values are readable, and the dense overlay is absent. The
  full-page snapshot retains a centered Round title.
- `Rscript test/test_shiny_simulation.R`: 14 checks plus eight tabs passed,
  including no HTTP/live database mutation attempts.

Performance review: point-axis tick/grid generation is bounded by the automatic
tick hint rather than growing with the numerical score range. During the final
idle browser checks, task work was 2.1–2.3 ms over 1.2 seconds with bounded DOM
size. Server calculations, API/DB read counts, durable snapshot reuse and
invalidation, worker lanes, timers, queues, cache size, dependencies and memory
retention are unchanged. The responsive SVG label checks cover phone and desktop.
No credentials, raw private response bodies or real accounts were used.
Hosted release/appearance acceptance remains unverified.

### Purchased-clause badge — 2026-11-23 Pacific

The scoped, read-only production summary for the reported card contained the
past `clause.date`, `transferred: true`, and a future `transferDate`. Its saved
owner matched the card. The previous badge picked any formatted opening when
the purchase action was unavailable, which showed an expired lock date even
though Futmondo had reported a completed purchase with delivery pending. The
new badge displays the reported purchase and the separately reported delivery
time in the user's display timezone.

- `Rscript test/test_player_card_database_display.R` passed saved-response
  checks for open and future-locked clauses, an expired opening with unknown
  transfer status, and purchases with and without a future transfer time.
- `Rscript test/test_scheduled_clauses.R` passed eligibility, reservation,
  explicit consent, deduplication, and outcome checks with artificial records.
- `Rscript test/test_shiny_simulation.R` passed 14 checks and eight tabs.

Performance review: the existing card badge formats one additional timestamp
only for a reported purchase. The text wraps in the existing card container.
No extra API or database requests, new cache entries or invalidations, blocking
I/O, timers, reactive dependencies, or additional widgets are introduced. The
saved-card regression issued zero source reads while rendering these states.
The production diagnosis read only a scoped snapshot. No transaction was sent.
Hosted release rendering remains unverified.

## Round-start lineup submission · 2026-11-23

The offline lineup scheduling fixture passed kickoff filtering, simultaneous arrivals, unavailable signings, immediate submission, and a selected substitute arriving after full coverage was already possible. A later unused signing did not delay the save. The 18-owned, two-incoming fixture across all formations took 8 ms median over 20 runs. Planning now makes one optimizer call per request and queues one submission. It reuses existing roster and history reads, status polling, and durable plan storage.

Chromium passed the approval, replacement, cancellation and scheduling-disabled cases at 390 and 1440 px using synthetic fixtures. The offline Shiny lifecycle passed 14 checks and eight tabs. Production submission and real account data were outside these checks.

## Pitch review and dated Save confirmation · 2026-11-23

The lineup module checks passed. Chromium at 390 and 1440 px verified incoming and owned player styling, the confirmation timestamp against the latest selected signing, cancellation, replacement, disabled scheduling and immediate manual saving for an all-owned XI and bench despite a pending signing. The offline Shiny lifecycle passed 14 checks and eight tabs.

Review reuses the pitch and drops the second player-by-player rendering pass. Opening and cancelling the dialog uses local plan state. Fixture read and write counters stayed flat through preview and scheduling, with the existing admission RPC stubbed. In the scheduling-enabled browser fixture the pitch appeared in 821 ms on mobile and 212 ms on desktop. These are local fixture timings. Hosted scheduling was not exercised.

## Save button visibility in the lineup editor

The reported mobile screen omitted Save above Formation. In the previous version the button existed only after the save_control output rendered. The local prior-version fixture did render it after opening the Lineup tab, so the exact hosted trigger remains unverified. The revised page includes the button in its initial HTML, with its eligibility updated independently of the auxiliary UI output.

Chromium checks at 390 and 1440 px entered Lineup from another tab and verified initial markup, button identity across optimization, scheduled confirmation, cancellation, replacement, disabled scheduling and the all-owned manual save. The lineup module checks passed. The offline lifecycle passed 14 checks and eight tabs. This change shares one reactive validation result and updates the existing button in place. It adds no source requests, database operations or timers. Pitch preview in the scheduling-enabled fixture took 825 ms on mobile and 221 ms on desktop.

## Primary-position substitutes · 2026-11-23

The independent exhaustive oracle and targeted MID/FWD case passed with primary-only reserves. All 17 lineup runtime groups, scheduling and module checks passed, as did the offline lifecycle (14 checks and eight tabs). Twenty synthetic 100-player runs across all formations averaged 39.5 ms without required coverage and 70.2 ms with it. The pitch reads primary positions once for its three indicators. No additional source reads, database operations or timers. The account's live XI was not reproduced.

## Editable optimized lineups and expected XI points · 2026-11-23

The lineup module and scheduling checks passed. Known-answer fixtures covered an empty XI (−55), a negative-scoring starter with ten empty positions (−53), missing starter history and exclusion of bench points. Chromium at 390 and 1440 px edited an optimized bench before and after schedule admission, reopened the persisted lineup, exchanged an incoming keeper for an owned player, checked the resulting points difference, and confirmed replacement or cancellation. The scheduling-disabled pitch and all-owned save also passed. The offline lifecycle passed 14 checks and eight tabs.

The total matches at most eleven IDs against the existing score table. Editing a projected lineup recalculates the due time from stored transfer rows and does not run the optimizer. Browser read/write counters stayed flat during local selection changes. The scheduling-enabled fixture rendered its preview in 848 ms on mobile and 219 ms on desktop. Live account behavior and deployment were not verified.

## Incoming players in the regular lineup editor · 2026-11-23

Chromium at 390 and 1440 px verified that an eligible incoming player appears in the normal picker with optimizer inclusion switched off, while a signing after kickoff is excluded. Reloading a session with a queued lineup restored its incoming starter to the editable pitch without a separate edit action. The dated approval, replacement, disabled scheduling and manual-save cancellation checks passed. Lineup module checks and the offline Shiny lifecycle (14 checks and eight tabs) passed.

Restoration uses the existing context-scoped database schedule read and transfer observations. It adds no polling, source requests or database writes. The picker and history reader share the projected roster. Excluding arrivals from optimization can still require reading their stored history for the manual picker. Local edits stayed within fixture read/write counts. The scheduling-enabled fixture took 838 ms on mobile and 220 ms on desktop to display the optimized pitch. Hosted deployment and real account behavior were not verified.

## Plots information messages

The page information panel collected both round status outputs, including the one from the hidden chart tab. Chromium at 390 and 1440 px in both themes verified a single missing-score warning on Standings and Rank progression and its removal on Money movements, where the transfer loading message remained visible. The Plots module and delayed-read checks passed, as did the offline Shiny lifecycle with 14 checks and eight tabs.

The selected-chart status replaces five outputs and reuses the existing plot and status reactives. It adds no source requests, database reads, cache invalidations or timers. Chromium confirmed that unchanged saved facts did not redraw Standings and that transfer grouping still reused the existing reads. Input heartbeats stayed below 1.16 seconds and the idle crest chart used at most 2.1 ms of browser main-thread time in the 1.2-second sample. Checks used local synthetic snapshots.

## Welcome page Essentials copy

The focused render check and Chromium at 320 and 1440 px verified the shortened Squad status description and the new clause-transfer timing entry under Essentials. Both disclosures expanded, the tiers stacked on mobile and appeared side by side on desktop, and neither viewport overflowed horizontally. The offline Shiny lifecycle passed 14 checks and eight tabs.

The addition renders one static disclosure on Welcome. Opening it uses native browser behavior and adds no Shiny events, source or database requests, polling, cache entries or reactive updates. Verification used local fixtures.

## LigaRadar header navigation

Chromium at 390 and 1440 px confirmed that clicking the title or pressing Enter on it opens Welcome while preserving the signed-in session, with no document reload or horizontal overflow. The offline Shiny lifecycle passed 14 checks and eight tabs. The existing Notifications bell was unavailable in this fixture after opening Welcome, with identical output before and after the change.

The new header link sends one Shiny event per activation and uses the existing Welcome tab rendering. It adds no source requests, database operations, cache invalidations, timers or background work. Welcome remains outside the saved-page list. Verification used the local synthetic fixture.

## Single-player loading animation · 2026-11-23

Local Chromium captures verified the full formation, the isolated player and the head-first release into a moving flock. With 4× CPU throttling, the compact player head radius grew from 4.1 to 22.3 scene units on both 320 and 1440 px viewports. The intermediate frame measured 15.5 and 14.5 units respectively. Stage draw time at the 95th percentile was 4.5 ms and 4.7 ms respectively for the hero and compact loaders combined. During release alone, the corresponding times were 3.2 ms and 3.7 ms. The offline Shiny lifecycle passed 14 checks and eight tabs.

The renderer reuses the existing 180 or 320 particles and the shared 30 FPS clock. It skips particle drawing during the solid-formation hold and skips the other 21 figures once the camera settles. The animation runs entirely on the client. Verification used local Chromium emulation.

## Queued lineup wording · 2026-11-23

Chromium scheduling tests at 390 and 1440 px verified a single full-lineup message with the approved submission time and timezone, including the two-signing fixture and schedule replacement. The status renderer now reads the time directly from the persisted stage, eliminating roster and transfer name lookups and their reactive invalidations. Submission, ownership checks, database work and polling cadence are unchanged. Verification used offline fixtures.

## Half-orbit before the loading close-up · 2026-11-23

The focused Chromium camera check passed on a desktop hero, fullscreen phone and compact phone loader. It sampled the side-on view halfway around the pitch, both teams from the opposite touchline, the midpoint of the slower zoom and the single-player release. With 4× CPU throttling, the 95th-percentile draw time measured 4.4 ms at 1440 px, 4.1 ms at 390 px and 3.8 ms at 320 px. The longer loop spends six more seconds drawing per cycle while loading is active, using the same frame cap, particle pool and visibility controls. The camera adds no network or database work. The offline Shiny lifecycle passed 14 checks and eight tabs. Verification was local.

The squad summary reuses existing counts and reservations. The change adds scalar arithmetic and skips empty disclosure markup with no additional reads, timers or reactive outputs.
Offline verification passed on 2026-11-23. Banner finance passed 18 checks and the Shiny lifecycle passed 14 checks plus eight tabs. The unchanged-refresh and active-offer reconciliation suites passed, including scheduled and manual-only cases.

## League transfer activity histogram · 2026-11-23

The focused Plots checks passed with known amounts at local midnight, month and
Monday boundaries, and across the Madrid daylight-saving change. They cover a
recorded zero, unknown dates and amounts, true-empty input, and the rendered
bar values, widths and hover labels. The saved-round loading regression passed. The offline Shiny lifecycle passed 14 checks and eight tabs.

Chromium passed at 390 and 1440 px in light and dark mode with the actual
worker adapter and snapshot codec. It recovered from a withheld transfer
snapshot, verified exact totals in all three groupings and detected no extra
worker database reads on period changes, source HTTP or horizontal overflow.

Cash & transfers and Money movements share one scoped pressroom read. The
histogram starts from existing worker snapshots and sends grouped bars to the
browser. Period switches reuse the parsed rows. The chart adds no database
writes, acquisition jobs or application timers. The existing worker poll and
cache bounds still apply.

For synthetic histories of 1,000 and 10,000 transfers, median date and amount
projection took 6.5 and 58.5 ms and retained 16.8 and 157.5 KiB, respectively.
For 10,000 transfers, grouping took 9 ms daily, 6 ms weekly and 23 ms monthly.
Plotly object building took 12–13 ms. The resulting 417, 60 and 14 bars used
about 100, 19 and 6.5 KB of uncompressed widget JSON, respectively. These are
local measurements, with the weekly view the default. Hosted account data
and deployment were outside local verification.

## Saved date and amount projection for Money movements · 2026-11-23

The report included `FM-UNKNOWN-000002`. That reference is process-local.
A read-only production probe found a complete Pressroom baseline and a current
worker snapshot with 1,273 usable transfers. The stored worker snapshot
decoded and grouped successfully through the chart code. The exact-scope
baseline also decoded all 1,273 rows in a 0.62 s database-and-validation
probe. The separate normalized transaction table lacked matching season
coverage and reconciled clause rows for the affected championship. Its
raw count was insufficient to verify a complete replacement.

The histogram reuses current session data and otherwise asks the existing
worker database read for the date-and-amount snapshot. A confirmed miss
supports older stored histories through the existing full read and audited
baseline. An unavailable primary worker read with a persisted baseline was
reproduced in the offline adapter harness and recovered with a stale notice.
No authenticated hosted browser session or corresponding server log was
available to identify which component originally issued the reference.

Replaying the production snapshot showed identical chart inputs after
projection. The typed JSON shrank from 721,557 to 45,647 bytes. Base64
snapshot chunks shrank from about 176.5 KB to 17.6 KB, a 90% reduction.
Projection and compression took 26 ms in the replay. The worker publishes
one extra small derived snapshot per existing Pressroom acquisition. Browsing
makes no source calls, reuses the normal DB poll and shared cache, and makes no
new reads on period changes. Existing snapshots without the new projection
can temporarily require the legacy fallback. No schema or cadence change.

Focused Plots, worker-provider, context-handoff and baseline-store checks
passed. Chromium at 390 and 1440 px in both themes verified pending recovery,
all three groupings, a cold read without the full Pressroom and zero extra
reads on period changes. The offline Shiny lifecycle passed 14 checks and
eight tabs. The Python worker suite passed 72 tests and the R worker contract
passed 20 checks. Production publication and signed-in rendering remain
unverified.

## Classification bonus evidence and ranking reuse · 2026-11-24

The calculation shown in the report was the finance snapshot saved at
13:39 UTC. A read-only production check found an active factual worker,
nine closed team histories, a persisted nine-team ranking with native status
`V`, and a final 11-player selection for each of rounds 4–9. All nine historical
scores matched across the two sources in those rounds. The tenth team had
an empty history. The older finance loader skipped these ranking observations.
Its fallback participant proof also discarded all affected rounds when a
recorded position amount differed from the configured calculation in a
different round.

Recorded position payments agree with the configured nine-team allocation for
rounds 4–7. Round 9 differs by €666,667 and its earliest saved ranking came
after payment, so the payment-time ranking is unverified. An unrelated round-12
comparison also found a payment discrepancy and a score disagreement between
sources. No final-category payment was saved for round 8. The earlier
postponement record and six independently confirmed award eligibility records
are in the unresolved archive scope. A read-only repair preflight verified all
seven against the active scoring context and final selections, with zero
target conflicts. Automatic approval review blocked publishing those seven
because the report asked for investigation. The repair script is staged for
explicit approval. The source queries here were read-only.

The worker now reuses the standing observations and validates recorded position
payments round by round when using the native ranking. The ledger still needs
verified historical ownership and eligibility for several Best XI recipients.
A recorded combined payment is kept intact until its components reconcile.
The latest source request investigated separately was the upcoming round 14,
which had no ranking snapshot and 94 native rejections when sampled. The
dated calendar already indicated a future kickoff. Tests reproduced the
persisted JSONB ordering defect that prevented using that calendar. The
published current round remains usable from the database.

Verification used synthetic or persisted-field projections with no real
account HTTP in tests. Saved bonus recovery (seven cases), native standings,
future and award calendars, compact bonus snapshots, partial Classification,
combined payments, the reference payment gate, ownership (ten cases),
context isolation (nine cases) and the deployment contract passed. The
offline Shiny lifecycle passed 14 checks and eight tabs. The pre-existing
inactive-participant test's last integrated reward assertion fails identically
against the parent source. Its first five cases pass. The production worker's
next settled snapshot and signed-in mobile rendering still need verification.

Performance review compared equivalent synthetic ledger preparations across
13 historical rounds and nine participants with position settlements. Mean
local time was 201 ms before and 223 ms with reconciliation of the saved
rankings across 25 runs. The extra work runs in the factual worker using
existing scoped evidence, and skips redundant source ranking or calendar
reads when matching records exist. The preparation key causes one recompute
while the existing durable bonus snapshot remains readable. The compact
database-view check measured 0.351 s for 100 local read/decode/projections.
A separate 100-range UI projection benchmark took 0.400 s with no range
change I/O. There are no additional browsing requests, polling timers,
database migrations, cache capacity or widget changes. Local timings
exclude network, production worker load and mobile hardware.

### Approved production evidence repair · 2026-11-24

The user authorized retrying the seven-record repair. The committed script
rechecked one exact account, league, competition, season and scoring context,
found seven eligible records with zero conflicts, and received a successful
database acknowledgement. A separate readback matched all seven prior payloads
and observation timestamps exactly. The active season now contains the
confirmed MVP ownership facts for rounds 4 and 8, the pending final-settlement
fact for round 8, and four unowned Best XI selections for round 11. The saved
lineup records identify one owner each for the other seven round-11
selections. The SQL coverage comparison still found open ownership gaps in
rounds 4 and 8 within the checked evidence.

The last worker run had completed before the repair. The post-repair probe
still found the compact calculation observed at 14:21 UTC. The next finance
job was due at 15:47 UTC under the existing 45-minute cadence. The next
publication and signed-in Classification display remain unverified.

The focused offline settlement, classification bonuses and compact snapshot
checks passed. Production readback used 1,848 shared buffer hits with no disk
reads and took 198 ms database execution plus 49 ms planning. This was a
one-time write of seven scoped facts and bounded read-only verification.
The repair adds no recurring source or database requests, UI redraws or
worker schedule changes. This follow-up changes documentation only and has
no additional runtime performance impact.

## Continuous banking flight · 2026-11-24

The former orbit recalculated vertical framing from the rotating pitch bounds. Its yaw and zoom also eased to zero speed at the same intermediate timestamp. The revised shot uses a stable framing reference and one flight parameter for eye distance, heading, bank and aim. Across 726 poses at each of three scene heights, the eye distance and vertical offset decreased monotonically and the pitch and player projections stayed finite.

Local Chromium verified motion on both sides of the former pause and the transition from the centered midfielder into flock particles. At 4× CPU throttling the 95th-percentile draw time for the existing renderer was 4.6 ms on the 1440 px desktop layout, 3.9 ms on the 390 px fullscreen layout and 4.2 ms on the 320 px compact layout. The particle counts, loop duration and frame cap are unchanged. The change adds no network or database work. The offline Shiny lifecycle passed 14 checks and eight tabs.

## Round 9 settlement comparison and historical ownership dates · 2026-11-24

The source recorded a round 9 position credit of €3,333,333 on October 20.
Both saved rankings have nine participants and the reference team at 72 points.
In the earlier archived ranking the team was 5th, producing €3,333,333
under the recorded €30 million lowest-scores-first pool. In the later ranking
the team was 6th, producing €4,000,000. The user confirmed the current
official 6th-place result and the team above with 74 points. That team
had 69 points in the earlier archive. Their expected credits are €4,000,000
using the earlier standings and €3,333,333 using the current standings.
The earliest saved ranking and team histories postdate payment, and no
contemporaneous settings or ranking snapshot was recovered. The archives
support a standings revision as the explanation. A second recorded credit
or a contemporaneous official record would corroborate the settlement basis.
The financial ledger keeps the recorded credit. Futmondo documents that
post-close score corrections can affect standings without recalculating
bonuses. [Scoring guide](https://help.futmondo.com/article/204-modo-prensa-futmondo-liga-ea-sports).
[Automatic bonus guide](https://help.futmondo.com/article/142-configura-las-primas-automaticas-en-un-campeonato-social).

Read-only production evidence had official kickoff dates for rounds 4, 5, 6,
7 and 9 in the normalized fixture calendar while the dedicated ownership
calendar lacked them. Round 4's saved owner history supports Robert Navarro
being unowned through its September 4–8 interval. Manu Sánchez and Ochieng
lack a saved transition before kickoff. The changed reader reuses scoped
settled fixture dates and handles nested empty JSON placeholders in mixed
evidence pages. The wall-time guard also accepts observations that postdate
the worker's oldest contributing source clock. The proof still requires an
unchanged, uniquely mapped owner or an explicit System state covering
the full interval. No production records were written. The newly calculated
hosted amounts remain unverified.

Offline verification passed the 12 ownership checks, fixture calendar,
pending settlement, ranking recovery, database-only bonus projection and
deployment contract. The synthetic fixture exercised mixed JSON round-trip,
foreign scope, wrong or conflicting round identities and dates, an older
worker clock, and observation-time invalidation of the existing preparation
cache. The offline Shiny lifecycle passed 14 checks and eight tabs.

The revised interval preparation averaged 60.19 ms versus 13.56 ms across
75 repetitions with 13 settled rounds in a 380-fixture synthetic calendar.
The existing worker calculation bears the extra validation. The source is
already in the scoped database read, and only relevant settled fixtures are
validated. Usable saved histories are reused. Unresolved holders can use the
existing bounded worker acquisition queue once their interval is supported.
Browsing adds no source or database requests, cache capacity, timers or
redraws. The comparison excludes network and production worker load.

## Player captures and remaining bonus evidence · 2026-11-24

The two supplied official-client captures were inspected locally. Manu Sánchez's
capture contains team, roster and market reads. It contains no player-summary
response. His earliest stored ownership transition is after the round 4
settlement. Ochieng's `/1/player/summary` returns two owner transitions. Its
earliest is September 8 at 9:21 p.m. Pacific, after the round 4 morning payout.
A source-identified System sale was recorded after settlement. The market
archive had no matching source-identified sale before payout for either player.
These observations leave the September 4–8 ownership interval open for both.
The authenticated capture also corroborated the two read contracts in
[the API inventory](har_api_reference.md#player-season-and-competition-statistics--2026-11-24-pacific).

The user-confirmed current round 9 standings were replayed with the shared
ledger formula. The recorded €30 million pool, paid lowest-scores-first across
nine teams, yields €4,000,000 for current 6th place. The actual €3,333,333
credit matches the earlier 5th-place snapshot. A second dated settlement
receipt or contemporaneous standings would help establish the payment-time
allocation for rivals.

The source-level production audit found evidence of the selected MVP's owner
or System status for each round 4–9. The reference team's combined award
receipts in rounds 4 and 7 still need separate attribution. Round 8 has a
verified pending final-settlement record and zero observed position or combined
award credits. The remaining Best XI ownership gaps by settled round are:

| Round | Players whose historical ownership still needs evidence |
| --- | --- |
| 4 | Manu Sánchez, Ochieng |
| 5 | Álvaro Carreras, Gulácsi, Juan Iglesias |
| 6 | Aramburu, Brugué, Grimaldo, Ionuț Radu, Jonathan David, Le Normand |
| 7 | Pablo Durán, Pedrosa, Román |
| 9 | Dani Requena, Hugo Novoa, Kubo, Maffeo, Pablo Martínez |

All eleven round 11 selections now have lineup or confirmed ownership
evidence in the source records. These are source-level checks of scoped
persisted evidence. Coverage on the live Classification screen depends on
the published calculation.

Verification checked the two HTTP 200 request and response shapes against the
public client, retained the raw captures outside Git, and reproduced the
position amounts in the existing R allocator. Performance review found no
runtime effect from the documentation. The investigation required one-time
local capture parsing and scoped read-only database comparisons.

## Reference cash and combined award reconciliation · 2026-11-24

A scoped, authorized read of the official `/1/userteam/moneymovements` and
`/1/userteam/information` routes returned HTTP 200 for both. The official
client uses the authenticated championship and user-team query for each,
with `type="market"` for information. The existing movement normalizer
accepted 231 records and produced 231 unique identities, including one
opening allocation. The ledger summed to €161,786,854, exactly matching
`answer.budget` from team information. The latest scoped production evidence
contains the same record count, opening allocation, movement sum and team cash.
Opening €300,000,000 plus €113,070,000 in awards and −€251,283,146 in net
transfers reproduces the observed cash. [Official Futmondo web bundle](https://app.futmondo.com/main.dart.js).

The own-team round 9 observed credits are €5,040,000 for 72 points at
€70,000 per point and €3,333,333 for position. The older nine-participant
ranking has descending scores 101, 93, 83, 77, 72, 69, 56, 56 and 54.
The existing allocator reproduces the observed position credit at 5th using
the verified €30 million lowest-scores-first pool. The two tied 56-point
teams would receive €5,000,000 each. This archived ranking explains both
recorded own-team credits. Contemporaneous ranking or other managers'
receipts are still needed to corroborate rival position amounts.
[Futmondo bonus guide](https://help.futmondo.com/article/142-configura-las-primas-automaticas-en-un-campeonato-social).
[Futmondo tie guide](https://help.futmondo.com/article/134-en-caso-de-empate-a-puntos-entre-dos-usuarios-cual-prevalece-en-la-clasificacion).

The reference team's round 4 credits were €6,230,000 for points, €2,666,667
for position and €500,000 as native `dreamTeam`. Valles was a verified
eligible Best XI player for the reference team and Boyé was the MVP for
another eligible team. The recorded amount is entirely the €500,000 XI
credit under the observed rules. In round 7 the point credit was €5,390,000,
the position credit was €2,666,667 and the native combined award was
€1,500,000. Verified eligible Best XI player and MVP Raphinha accounts for
€500,000 Best XI and €1,000,000 MVP. These are reconciliations of the two
specific receipts. Other award players still need dated ownership evidence
for league-wide attribution. The app's automatic classification scope and
complete-ownership gate remain documented in the cash accounting contract.

The supplied Manu Sánchez screenshot and summary corroborate three owner
transitions. The first appears September 17 Pacific, after the September
4–8 round 4 ownership interval. The earliest available Ochieng transition
also comes after payout. Both players' earlier ownership remains unresolved.
An explicit System ownership interval can establish no league recipient.

No raw credential, financial response or player response was retained in
the repository. Verification used two authorized source reads, the existing
R parser and formula, and scoped database aggregates. This follow-up changes
documentation only and has no runtime performance effect.

## Best XI ownership sweep and Kubo evidence · 2026-11-24

The 19 requested unresolved player-round entries in rounds 4–7 and 9 were
resolved to unique player IDs in final award selections. The official
authenticated `POST /1/player/summary` returned HTTP 200 and the matching
player for each request. Ten histories started after kickoff, three had
ownership changes inside the award window, and five returned empty history
arrays. Those 18 still need ownership or award-cutoff evidence. A scoped
transfer lookup corroborated the known changes without supplying the missing
cutoff or earlier owner. Round 8 is still awaiting closure. The round 9
position discrepancy concerns the rival allocations. The account holder's
recorded credit and cash already reconcile independently.

Kubo's history was the remaining case. A follow-up field inspection found
an explicit later loan alongside earlier ordinary transfers. The official
response shows Futmondo held Kubo from the September 14 sale through the
October 16–20 round 9 award window. The normalized transfer records agree
with that sale within one second and show no conflicting move through
settlement. The old parser dropped the entire response when it encountered
the November loan. With the user's approval, the guarded SQL repair added
one scoped, verified unowned fact. The database acknowledged the write.
Independent readback matched the saved fact exactly and found zero
conflicts. The next published calculation and signed-in display remain
separate verification steps.

The revised parser retains loan-marked transitions in version-2 histories.
A later loan preserves a proven earlier interval. A loan as its starting
state or an intervening transition leaves eligibility unresolved. Version-1
histories and existing durable reuse remain supported. The same factual worker
can acquire and persist these observations through its existing bounded
queue and checked database write path. The user-authorized one-time sweep
did not change the normal source acquisition schedule.

The offline ownership suite passed 13 cases, including a mixed version
JSON round-trip with a later loan, an in-window loan, a loan at the opening
boundary, and the known-award ledger total. Ranking recovery, pending
settlement, database-only Classification, deployment and the offline Shiny
lifecycle passed. The parser averaged 4.34 ms before, 4.45 ms with an
ordinary history and 4.41 ms with a loan, over 200 runs of eight transitions
each. There are no new recurring requests, timers, UI redraws or cache
bounds. Only loan-containing histories add the flag to durable observations.
The authorized evidence repair was a single scoped write, with readback.

### Ownership history boundary check · 2026-11-24

Both supplied Kubo responses match the same league, user-team and player
scope. Their eight `owners` entries are identical and confirm the previously
saved Market interval through round 9. Neither supplies a history coverage
boundary. The last transition is the later loan already covered by the parser
fix. The scoped league's saved rule records assign zero opening players, in
agreement with the earlier captured opening rosters.

The first listed transition follows kickoff for ten unresolved players.
Recorded transfers involve five of those players during round 6's award
window. The saved closed lineups and benches did not supply an earlier
ownership observation for any of the ten. Starting empty supports tracing
Market ownership from opening, provided the archive covers all relevant
ownership-changing events through settlement.

An opening-period official Pressroom capture reached the empty terminal
page, but its newest event was August 14, before the affected award windows.
The current database has an exact-scope saved Pressroom baseline. The 1,246
source-identified rows in the separately queryable transfer index lack
resolved competition and season context. Those indexed rows cannot certify
a complete scoped ownership trace for the ten. No additional no-owner cases
were assigned. This review used sanitized local capture checks and read-only
metadata and lineup comparisons. The documentation has no runtime effect.

### Round 6 and 8 postponed-match notices · 2026-11-24

Futmondo's round 6 notice identifies the first lock on September 3 at
21:00 Spain time. Leagues allowing changes had windows on September 15
from 10:00 to 19:00 and October 21 from 10:00 to 20:00. The original
starting balance and captain carried forward. The notice scheduled final
position and Best XI credits for October 22. The reference team's
saved movements corroborate the three point installments (`6`, `6.1`,
`6.2`) and one position credit (`6.2`). [Futmondo's round 6 notice](https://magazine.futmondo.com/partido-adelantado-jornada-6-liga-ea-sports-en-futmondo-2/).

For Social leagues with the postponed lineup lock enabled, aligned
players stay in the original XI even if they change hands. Where changes
are enabled and Futmondo opens a window, managers choose from their
squads at reopening. A replacement can only score if his club still has
its round fixture to play. The scoped `/1/userteam/information` captures
record `configuration.blc=true` on August 26 and September 14, 16 and
20, with the same value in the November 24 saved rules. The captures
bracket the September 15 window. October 21 has no direct setting
observation. [Futmondo's postponed-match guide](https://help.futmondo.com/article/228-procedimiento-en-partidos-aplazados).

Six one-time `/2/player/matches` reads each returned HTTP 200 and a single
round 6 fixture matching the saved official calendar. Aramburu and
Ionuț Radu played in the September 3 Real Sociedad–Celta match. Grimaldo,
Jonathan David and Le Normand played on September 16 in Atlético–Osasuna.
Brugué played in the final October 21 match, Levante–Athletic. The published
lineup rules and award payment dates leave open which dated roster
determines Best XI and MVP recipients when a player changes hands during
the round. The six cases remain unresolved. A
recipient-level award record for a player whose ownership differs between
the possible cutoffs, or an official clarification, could establish the
rule. [Futmondo's bonus guide](https://help.futmondo.com/article/142-configura-las-primas-automaticas-en-un-campeonato-social).

Round 8 has nine final fixtures in the saved calendar and one suspended
fixture, Rayo–Athletic. The reference team's only round 8 credit is for
points. LaLiga lists the resumption for December 9 at 20:00 Spain time.
No fixture-specific Futmondo notice was located as of this review. The
round remains pending for final awards. [LaLiga's round 8 schedule](https://www.laliga.com/laliga-easports/resultados/2026-27/jornada-8).

Verification used scoped read-only database observations, local captures
and the six player fixture reads with at most two in flight. No historical
eligibility was assigned. This documentation-only change has no runtime
impact on requests, storage, calculations or rendering.

### Brugué round 6 lineup and payout cross-check · 2026-11-24

The final Best XI included Brugué. The earliest returned ownership
transition was a September 13 UTC purchase from the Market, after the
September 3 lock. The market index corroborates the seller
and effective timestamp. This supports Market ownership at kickoff
without establishing that the earlier transfer archive is complete.
Two scoped locker records distinguish the September 25 and October 18
clause purchase times from their September 28 and October 22 deliveries.
The reference team held Brugué at the October 21 fixture and at the final
position settlement at 13:47 UTC on October 22. The onward transfer
delivered at 22:05 UTC.

Two authenticated `POST /1/userteam/roundlineup` reads for the earliest
recorded buyer and the reference team returned HTTP 200 and closed
lineups. Both had eleven starters, four bench entries and `negative=false`.
Brugué was absent from both and from all nine saved closed round 6
lineups. As controls, the same saved lookup found both the reference team's
previously reconciled round 4 award recipient Valles and round 7
recipient Raphinha in their respective starting XIs.

The reference team's official movement history contains three round 6
point credits and a position credit, with no Best XI or MVP credit.
Owning Brugué at the individual kickoff and at settlement was insufficient
for this payment. The result fits an original roster cutoff or an
eligibility condition tied to the historical lineup. It leaves the general
prize rule and the original owner unverified, so no league-wide recipient
fact was assigned. [Futmondo's bonus guide](https://help.futmondo.com/article/142-configura-las-primas-automaticas-en-un-campeonato-social).

The check reused scoped database observations and made two one-time
official lineup reads. It changes documentation only and adds no runtime
requests, database work, calculation changes or redraws.

### Manu Sánchez and Ochieng round 4 ownership resolved · 2026-11-24

The supplied ownership histories and saved official transfer records
agree on the first Market purchases. Ochieng first joined a manager's
squad on September 8 at 21:21 Pacific and Manu Sánchez on September 17
at 21:21 Pacific. Round 4 opened September 4 at noon Pacific and the
reference Best XI payment arrived September 8 at 01:58 Pacific.

A one-time scoped `POST /1/locker/pressroom` backfill started at the
ObjectId upper boundary of the later verified first purchase. It followed
the existing same-second overlap rule and reached the empty terminal
page on the seventh request. The 658 unique deliveries covered the
start of the league through both first purchases. Neither player had
any earlier delivery, and both first deliveries came from Futmondo.
The empty opening squads were independently established in the league
capture. Together these facts verify Market ownership for both players
through the entire round 4 award interval.

The supported normalized finance writer appended two league-, season-,
round- and player-scoped verified unowned facts with the source dates,
archive coverage and first delivery timestamps. The database acknowledged
both. A separate readback confirmed both statuses and zero contradictory
verified owners. The saved calculation was older than the new evidence.
The signed-in display after recalculation remains unverified. The two
round 4 gaps are resolved in source evidence, leaving 16 of the original
18 post-Kubo Best XI cases unresolved, including Brugué.

The offline ownership and database-backed Classification checks passed.
The audit made seven sequential source reads and one scoped batch write.
Existing evidence reuse supplies the two small records on the next
calculation. There is no new runtime endpoint, recurring request, redraw
or cache requirement.

### Field participation for Best XI and MVP · 2026-11-24

The account holder confirmed that the award player must have taken the
field. Based on the final `/1/userteam/roundlineup` field, which includes
incoming substitutes, all five recorded combined or Best XI credits in
rounds 1–13 match the reference team's observed award tariffs. Round 6
has nine corroborated participants, nine final lineups and zero Brugué
field appearances. Each of the other fifteen formerly outstanding Best
XI selections in rounds 5, 6, 7 and 9 likewise had zero field appearances
in nine saved final lineups for its round. These checks use the
league-confirmed rule. Futmondo's public bonus guide gives no specific
field condition for other leagues. [Futmondo bonus guide](https://help.futmondo.com/article/142-configura-las-primas-automaticas-en-un-campeonato-social).

One scoped write saved the policy and Brugué's no-recipient fact
through the normalized finance writer. Independent production readback
found both and no contradictory verified recipient. The ledger now
calculates remaining field-based eligibility from the existing final
lineups. The hosted calculation and signed-in display after the code
update remain unverified. See [the accounting contract](rival_cash_accounting.md#league-confirmed-field-eligibility--2026-11-24).

The focused Classification and ownership regressions, database-backed
Classification suite and offline Shiny lifecycle passed. Cold historical
preparation for thirteen rounds and nine teams averaged about 0.43 s
with either path. The calculation adds no source or database reads,
redraws or timers. It reuses the existing lineup storage and skips
award ownership reads for the confirmed league. Adding the scoped
policy changes the existing calculation input hash.

## Coach checkbox on Lineup — 2026-11-24

The full offline browser fixture verified the default checkbox state, saved
opt-out applied at sign-in while Lineup was hidden, the control absent from
Today, the disabled status in Lineup's page information panel, and both toggle
values written to the existing browser key. The nine-page browser check passed
at 320, 390 and 1440 px with no horizontal overflow or browser errors. The
offline lifecycle passed 14 checks and eight tabs with no HTTP or live database
mutation attempts. The coach suites passed 11 and nine checks, the Lineup
module suite passed, and all three deployment checks and changed file hashes
passed.

The move uses the same browser storage key, sign-in observers, shared team
state and coach request budget. The status render follows Lineup visibility,
with the existing responsive layout and page information panel. API and
database request volume, snapshot reuse and invalidation, timers, and server
memory are unaffected by the move. The checks used offline fixtures.

## Sortable received offers — 2026-11-24

The existing My Team offer panel now shows sortable player, amount, profit and
bidder columns. Profit uses the same recorded roster acquisition cost as the
squad table. The browser sorts by unrounded euro values and places unavailable
values last. Player cells use the Lineup label and existing roster club
metadata. Each button still sends the exact bid identity, including after a
sort. The existing saved-observation status and clock remain in the panel.

The 24-offer rendering fixture had a median server render time of 78 ms, up
from 48.5 ms, across 20 local renders of each version. Markup grew from 9.6 to
24.9 KiB with the widget and crest definitions. Reactable paginates the visible
rows and sorts in the browser. Ten timestamp-only publications produced zero
offer-panel renders and zero acquisition or persistence attempts. The table
shares the existing roster read, which used three cold source reads, zero
additional panel reads and zero warm reads. The added browser images use the
existing crest source with lazy loading. The change adds no timers, database
work or cache invalidation.

Offline Chromium passed at 320, 390 and 1440 px. It verified numeric sorting
in both directions, exact selection after sorting, compact names, crests,
literal bidder text, no page overflow and retained sorting after the source
clock advanced. Heartbeats took 59–61 ms, with zero source or database calls.
The received-offer, squad-price, disclosure and observation suites passed.
The full offline lifecycle passed 14 checks plus eight tabs. Deployment
checks and the three updated file hashes passed. These checks used synthetic
offers and crests.

## Standings latency and historical participation — 2026-11-25

The reported chart showed intact curves with a missing-scores warning.
Read-only production aggregates confirmed a ten-team directory and ten saved
team histories. Nine histories contained a finite, closed score for every
round from 1 to 13. The tenth history was explicitly empty. Saved native
Classification rankings listed the same nine participants and matched all
117 team/round scores, including rounds whose native ranking status was
`V`. The previous plot treated the directory as the historical participant
set and used an unknown score for the extra team in every round. This produced
the warning and withheld the participating teams' ranks. The database checks
returned aggregate coverage and match counts. They made no writes and exported
no response bodies, team identifiers or individual scores.

The signed-in team's read now starts with the catalog and directory. Saved
rankings are requested together only for published rounds with unresolved
coverage after team history settles. They use the same database-first worker
descriptors, compression, context and freshness metadata used by
Classification. Verified closed rankings supply missing observed scores and
historical participation. A mismatched, incomplete or uncorroborated
observation leaves a truthful gap. Existing saved source data supplies the
recovery, with zero additional Futmondo acquisition requests or DB writes.

Complete team histories need no additional database read. Leagues with gaps
can use one additional coalesced read for the affected saved rankings when
cold. This runs after the first curves are available. Squad-valuation history
is deferred while Standings is active. Cached observations are reused through
the existing bounded worker and session caches. Fact projections keep
metadata-only updates from rebuilding team round rows or redrawing charts.
No new timer, schema, job, endpoint or dependency was added.

A known-answer benchmark used 20 teams and 38 rounds. Identical points and
ranks were confirmed in the complete case. Across five batches of 100 runs,
median projection cost went from 6.99 ms to 1.88 ms. The new projection with
one team absent and all 38 rankings supplied took 2.33 ms. The browser tests
measure synthetic saved-read latency and input responsiveness. Hosted
first-curve timing and signed-in rollout remain unverified.

The real adapter and compressed-snapshot browser fixture passed at 390 and
1440 px in light and dark themes. The signed-in team's curve appeared before
the other team histories, then the closed rankings resolved the nonparticipant
without creating a missing-score warning. The first curve appeared 0.98–2.51 s
after opening the local fixture. Input heartbeats took 11–779 ms, including
bursts that republished all saved histories and rankings. Those bursts produced
zero unchanged Standings redraws. Idle browser CPU was 1.6–1.9 ms over 1.2 s,
with no page overflow, JavaScript errors or source HTTP. The ordinary browser
chart suite also passed in both themes at 320, 390 and 1440 px.

The first recovery implementation took about 3 s to respond during a
synthetic simultaneous publication. Reusing each round's reactive reader and
avoiding reprocessing unchanged team histories brought the same test below
800 ms. The final R checks passed all 11 plot groups, six loading/recovery
groups, three deployment checks and the module checksum. The offline Shiny
lifecycle passed 14 checks and eight tabs with zero live HTTP or database
mutation attempts. It selects each plot's subtab before checking its output.

## Compact clause transfer groups in My Team

The Incoming and Leaving panels each use one heading per populated timing
group. Focused fixtures passed 25 incoming, 24 outgoing, and nine departure
highlight checks. They cover kickoff boundaries, missing schedules and dates,
and retained transfer and source details. The offline lifecycle passed 14
checks and eight tabs. Chromium at 320, 360, 390, and 1440 px verified group
order and membership, player links, keyboard access to timing details, and
page width. The fixture recorded zero source or database calls.

Grouping is a linear pass over the published transfer rows during panel
rendering. It reuses the current transfer and round data, resource
subscriptions, and cache and invalidation paths. The renderer introduces no
blocking work, timers, acquisition or persistence. With four synthetic
transfers in each panel and disclosures closed, the before-and-after Chromium
fixture measured combined panel heights of 1233 and 1061 px at 320 px, 1140 and
920 px at 390 px, and 424 and 342 px at 1440 px. The shared headings and group
wrappers added four DOM elements overall. Hosted rendering remains unverified.

The follow-up styles each timing group with a distinct full-width heading band
and 12 px between groups. Chromium verified both themes at 320, 360, 390,
and 1440 px, including heading colors and spacing, player-link placement,
open/closed timing details, keyboard access, and horizontal containment.
Measured heading contrast was at least 5.12:1. A panel with three populated
groups gained about 46 px of height over the previous styling at 390 and
1440 px. The styling uses the existing heading elements and adds one timing
attribute per group. The data, cache, invalidation, and refresh paths are reused.
The browser fixture recorded zero source and database calls. The 25 incoming
and 24 outgoing clause checks passed, as did the offline lifecycle with 14 checks
and eight tabs. Hosted rendering remains unverified.

## Compact received-offer rows — 2026-11-25

My Team uses borderless player buttons with a keyboard focus outline and
labels system bidders **Liga Radar**. Headers, names, amounts, profit and
bidder labels stay on one line. Long values use an ellipsis with the full
text in a tooltip. Narrow layouts scroll within the table.

Offline Chromium verified 33 px rows at 320, 360, 390 and 1440 px. At each
width the full fixture amount of 25.271.066 € and loss of -29.64 M€ fit their
columns alongside Liga Radar. The check covered long rival labels, horizontal
containment, numeric sorting, keyboard selection of the exact bid after
sorting, and timestamp updates without replacing the table. Heartbeat
responses took 60–63 ms with zero source or database calls.

The compact styles apply only to the offers table. The renderer adds a
tooltip to each displayed metric and bidder. The existing roster reads,
persisted observations, cache reuse, timers, and reactive dependencies are
unaffected. The received-offers checks passed all 10 groups, the offline
Shiny lifecycle passed 14 checks and eight tabs, and the deployment contract
passed all three checks. Browser checks used synthetic offers and crests.
Hosted rollout and physical-phone appearance were outside this verification.

## Team value and daily trend — 2026-11-25

The summary prefers the same saved Futmondo squad value that Today’s Market
previously displayed. The trend and direction counts use the signed daily
price changes on the saved roster. The previous My Team value card waited on
the enriched roster and incoming-transfer state. Its replacement projects
directly from the existing team and roster states and leaves market listing
and arrival updates to their own panels. There are no added API or database
reads or writes, cache entries, invalidations, timers or network assets.
The 24-player calculation took a median 30 microseconds over five batches
of 1,000 calls. The card stores only the display amounts, counts and statuses.

Known-answer fixtures verified the official value and complete-roster
fallback, positive, negative and flat changes, a net-zero trend, verified
empty rosters, and missing, partial and stale observations. The focused
My Team, Today cash and received-offer checks passed. The full offline Shiny
lifecycle passed 14 checks and eight tabs, including the moved value and the
€170,000 fixture trend. Synthetic Chromium checks passed in both themes at
320, 390 and 1440 px. The card showed the two stacked arrows without overflow,
updated for changed trends and retained its DOM when only the observation
clock changed. Hosted rollout and signed-in official UI were outside
this verification.

## Round award player statistics — 2026-11-25

The eight Round awards checks passed, including independently calculated
appearance and MVP counts across rounds with repeated or absent display
numbers, players sharing a name, and a renamed player. The fixtures excluded
provisional, incomplete, conflicting, foreign and unacknowledged selections.
Different seasons and scoring settings could not contribute to a combined
ranking. A partial observation retained its explanation, and a future-only
catalog displayed the empty state.

Offline Chromium exercised the full module at 320, 390 and 1440 px across light
and dark themes. In the 38-round fixture the first player made the Best XI
38 times and won 12 MVPs. Both charts, numeric sorting, player search, round
share and narrow-screen containment passed. Chart columns stacked on phones.
Opening Statistics calculated it once and added zero source or database
reads. Tab changes preserved the search and reused the calculation. A separate
38-round benchmark with the maximum 418 distinct winners measured a median
1.59 ms for aggregation and 22.60 ms to produce the charts and paginated table
markup. The feature adds no timers, persistence writes or cache entries.

The three Best XI crest groups, ten MVP runtime groups, three deployment
checks and offline lifecycle (14 checks plus eight tabs) passed. The crest
fixture now reflects the existing pending-state behavior, which keeps the
lineup visible during a catalog read and displays an error if that read fails.
The login inventory adds one static entry. Signed-in production data and hosted
rollout were outside the checks.

## Round award club statistics (2026-11-26)

Offline Chromium at 320 and 1440 px opened Statistics first in player mode. Switching by mouse and keyboard changed both charts and the searchable table. With 22 selections across two confirmed rounds, the clubs had 12 and 10 Best XI appearances, one MVP each and 100% round share. Opening By round showed both XIs. Returning to Statistics preserved the grouping and search. Both widths fit without horizontal page overflow.

Offline fixtures covered transfers, multiple club selections in one round, missing and ambiguous club identities, and duplicate club names with distinct source IDs. The persisted award snapshot restored the official club ID alongside the name and crest. In the local successful Dream Team capture, all eleven selected players had a club ID, a team slug and a crest matching the club directory. The award, MVP, crest, saved-catalog, scoring and login inventory checks passed locally. The offline Shiny lifecycle passed 14 checks and rendered eight tabs.

On a synthetic 38-round season with 418 Best XI appearances, 160 players and 20 clubs, the first Shiny switch to club rankings took 222 ms and six subsequent switches took 38–41 ms. The module reused one award subscription and made zero catalog reads with this fully attributed fixture. Grouping with catalog enrichment took a median 5 ms across 41 runs. The switch adds no timers, source requests or writes. Browser and Shiny timings used synthetic accounts. Hosted deployment was outside verification.

## Round award crests and positions — 2026-11-25

Offline Chromium verified both leader charts and the paginated table at 320,
390 and 1440 px. The fixture covered a transferred player, duplicate names
with different player IDs, a long name, multi-position awards, missing
metadata and catalog recovery. Crests and positions stayed attached to the
correct player after searches, numerical sorting in both directions and
pagination. Switching tabs retained the search and added no catalog or award
reads. The browser used synthetic assets and records. Hosted rollout remains
unverified.

The 38-round benchmark with 418 distinct winners and 19 clubs took a median
2.93 ms to aggregate and 38.00 ms to generate the charts and table, compared
with 1.59 ms and 22.60 ms before adding player identities. Rendering labels
only for the visible table page kept the uncompressed output to 74.7 KB.
The catalog and awards use existing persisted observations and the existing
refresh cadence. All historical roles and club pairs round-trip through the
existing full-XI evidence record. One synthetic eleven added 704 uncompressed
JSON bytes for its club names and logo references, with the same database
write count.

Eleven MVP groups, eight round award groups, the crest display checks and
all three deployment checks passed. The offline lifecycle passed 14 checks
plus eight tabs. The fixed-schema legacy writer was checked independently
from the full-XI JSON persistence path.
## Scheduled lineup custom scoring — 2026-11-25

The production schedule passed ownership after all four arrivals, but repeated pre-dispatch attempts ended with `source_unavailable`. The official and approved formations both used 4-5-1. Saved source evidence showed an empty lineup `pointSystem` and matching custom settings in the active-championship response. Offline replay through the actual R bridge reproduced `Lineup scoring context is unavailable`. The bridge's JSON simplification changed the fingerprint of nested persisted settings.

The worker now uses the active-championship evidence from its existing round check and retains the exact nested database payload for fallback. The Python scheduler and R bridge passed three synthetic submission cases with explicit, calendar-derived and persisted scoring. Each case used the same Futmondo request counts with a single simulated save, restart readback, and no blind resend. The calendar case made zero scoring database reads. The fallback reused the existing indexed lookup. The focused lineup snapshot test passed 10 groups and measured 3.05 ms for explicit scoring and 4.07 ms for resolving blank scoring over 100 normalizations each. The offline Shiny lifecycle passed 14 checks plus eight tabs.

The change adds no timer, UI render, source request or additional database write. It reuses the acknowledged calendar evidence and does a bounded normalization when the lineup selector is empty. Nested database rows use list columns, avoiding another JSON round trip. The tests used synthetic submissions.

After rollout, the replacement worker picked up the existing approved job after its prior lease expired. The worker recorded dispatch at 23:24:43 UTC and confirmed Futmondo readback at 23:24:45 UTC. The job settled as saved and its persisted official lineup matched the scheduled formation, starters, bench and captain. This documentation update has no runtime performance effect.

## Advanced and resumed rounds (2026-11-26)

Futmondo published the Round 23 notice on November 25. The latest persisted LaLiga calendar in a read-only production check, observed November 26 at 13:22 UTC, held ten distinct matches under one round ID. Two kickoffs were on December 2 at 18:00 and 20:30 UTC, before the December 4 start of Round 15. The other eight carried the February 7 date. The publisher describes that weekend and has yet to confirm the precise February reopening time. A November 14 calendar still placed all ten games in February. The current stored calendar incorporates the rescheduling without a manual backfill. [Futmondo’s notice](https://magazine.futmondo.com/partidos-adelantados-de-la-jornada-23-de-laliga-ea-sports-funcionamiento-y-preguntas-frecuentes-en-futmondo/) · [LaLiga announcement](https://www.laliga.com/es-US/noticias/horarios-de-los-partidos-adelantados-de-la-jornada-23-de-laliga-ea-sports)

The offline standings reproduction had J14 completed, J15 playing, and J23 partly played. Two exact ranking reads produced 0 for J15 and −2 for J23. The combined range was −2, the round buttons exposed only 14, 15 and 23, and changing selections made zero further ranking reads. The live, cold, preset, loading and partial-bonus classification tests passed. The cold transport fixture now selects the acquisition adapter and supplies the required competition scope for its legacy snapshot. Its missing-score assertion checks the displayed numeric value. Existing settlement checks exercised separate point payments and final bonus categories.

The manual transport fixture checked an exact J23 marker, a J15 readback mismatch and reopened lineup restrictions. The Python scheduler and actual R bridge exercised three scoring configurations, ownership arrival, external edits, a dropped reply, process restart, a changed marker and a previously started matchday. Each configuration sent exactly one synthetic write. The original and resumed windows share the official round ID and have separate draft deadlines. The official source was never used for a test transaction.

The 380-fixture local calendar projection took 45 ms once, used about 5.6 KB, and selected the warm upcoming and latest groups in 0.3 ms. For 527 players, 20 changes of the recent-score window took 1.317 s on an ordinary schedule and 1.357 s on a split schedule with zero source requests. The worker still caps match-detail acquisition at six due matches per sweep. Manual preflight adds two bounded parallel factual reads for the marker and target-round matches. Readback adds one marker read. Scheduled preflight checks the same marker and only the approved round’s matches. Joining statuses, long fixture gaps and kickoff chronology took 14.45 ms per 38-round, 380-fixture worker calendar in a 40-iteration test. The previous status-only lookup took 1.67 ms. None of these timings measure hosted latency.

A same-scope read-only production check of the earlier split Round 6 found three distinct point installments with labels `6`, `6.1` and `6.2`, and one position payment with label `6.2`. The query returned only types, round labels and counts. A synthetic J23 replay began with a €20 December cash movement before any team-round history was available. Replaying the movement retained €20. Once the independently published phase scores and opening eligibility matched the reference payment, the rival with four points at €10 per point showed €40. The February records produced €60 in actual payments (€20 + €40) and €80 for the rival with eight cumulative eligible points. The rival's prior €40 was replaced. The Classification round and range checks included zero and negative point credits, final bonuses pending through the break, snapshot restoration and final totals without duplication. The compact ledger projection took 0.36 s for 100 changes of the standings selection across 10 teams and 38 rounds, with zero requests. Chromium exercised two early fixtures, 11 starter slots and a player card at 390 and 1440 pixels.

A completed, capped ten-fixture response with all games final invalidated an XI observed in December. The award card kept the old selection provisional until a post-final XI was available. Joining 418 award rows across 38 rounds took 1.36 ms per pass over 100 iterations, with zero I/O. For leagues with every final-category rate explicitly zero, the fully played fixture set closed the round without waiting for a payment the rules could never generate. A cold 380-fixture settlement projection took 128 ms on average over ten runs. Cached projections reuse the result until the relevant fixture, evidence or payment timestamp. Two simulated source failures past the calendar TTL retained a previously verified partial round at its original observation time. The next successful acquisition recognized closure. Testing the opening window against the ten fixtures averaged 1.32 ms over 300 explicit saves with zero additional requests. The offline Shiny lifecycle passed all 14 checks and eight tabs without live transactions.

The approved `20261126135250_split_round_job_cadence` migration was applied to production. A readback verified the new routine body, scoped job lock, fixed search path, invoker rights and service-role-only execution among application roles. The migration installs a function and performs no fixture, balance or queue rewrites. The full-schema PostgreSQL test exercised actual enqueue calls for all three affected job types, repeat discoveries, out-of-order calendar revisions, resumption, pending retries and outstanding leases. With 10 teams and the observed 45-minute ordinary floor, 12 quiet-round jobs could otherwise make up to 384 scheduled checks per day. The daily schedule allows 12, excluding retries and priority requests. Actual December/February traffic and account-specific payments have yet to occur.

At 14:07 UTC on November 26, the public Dokploy build served the `award_runtime.R` and `server.R` SHA-256 fingerprints from pushed application commit `4e4d388`. This verifies the deployed application revision. Acceptance against a real J23 lineup or payment still depends on Futmondo opening and settling each phase. This rollout note changes documentation only. [Award build marker](https://ligaradar.com/release-info.json) · [Application build marker](https://ligaradar.com/lineup-departure-runtime.json)

## Loading animation figurines (2026-11-26)

The local Chromium camera test counted 22 round heads and zero nose triangles at 1440, 390 and 320 pixels, including the compact banner. Both teams’ shoes pointed toward their opposing goals. Removing the nose saves three projections and one filled path per visible head per frame. With 4× CPU throttling, 95th-percentile draw times were 3.6, 3.2 and 3.1 ms at those widths. The change is confined to canvas drawing and adds no API or database work.
The offline Shiny lifecycle also passed all 14 checks and eight tabs.

## Live-round bench substitutions · 2026-11-26

- The focused R fixtures verify distinct zero-based bench and pitch indexes, allowed secondary roles, round and team identity, missing or conflicting fixture evidence, zero points, and kickoff on either side. Changed slots, a closed round, and an expired transport guard prevent dispatch. A native rejection and the two-slot readback are covered.
- The offline Chromium flow passed at 390 and 1440 px. Opening the current pitch, selecting two players and displaying the confirmation made zero substitution-service requests. A double click and a simulated timeout after acceptance produced one write. The initial readback failed, then **Check official lineup** verified both slots. A rejected database save was retried independently. That scenario used five factual requests in total (three preflight reads and two readbacks), one substitution request and two database write attempts. The second browser session restored the acknowledged snapshot and made the reverse swap from Classification. This used three preflight reads, one readback, one write and one successful database save. Each browser session made one cold database evidence read across the two views. The delayed write used a 350 ms asynchronous fixture.
- The existing lineup module checks passed, along with all 17 Classification lineup checks, all seven Classification context checks, ten async transport priority checks and three deployment-contract checks. The offline Shiny lifecycle passed 14 checks across eight tabs.
- The picker checks at most eleven starters against four bench slots. A visible pitch schedules its next lock update at the earliest relevant deadline. Both views reuse the same verified snapshot. Source acquisition cadence, worker lanes and the database schema retain their existing configuration.
- Verification used synthetic accounts, a synthetic storage boundary and local browser sessions. Production deployment, live Futmondo write acceptance, and the availability of confirmed external starting XIs were outside these checks.

## La Liga club lineup checker · 2026-11-26

The real R helper and Python provider adapter exchanged synthetic fixtures through the production worker protocol. A complete sheet with 22 unique starters identified a fantasy starter, a benched player and an omitted roster member. An unresolved name stayed unknown. The first fixture used four mocked provider reads, one for the date, one for the match and one squad per club. The repeated fixture and the durable snapshot replay made zero additional provider reads. The identity checks covered accents, duplicate full names, shirt numbers, conflicting birth dates and expired club membership. Incomplete XIs, missing fixtures, authorization failure, free-credit exhaustion and rate limits produced no nonstarter warnings.

On a 380-fixture season calendar outside the observation window, 20 sweeps averaged about 13 ms each with zero roster, profile or provider reads. A supported league uses the existing worker lanes, a 20-second bound per fixture and up to two fixtures per sweep. An ordinary league sync at 45-minute cadence retained its interval in the full-schema PostgreSQL test, while the XI job honored its three-minute deadline and its one-minute minimum. In the offline browser, both pitches displayed the stored badges at 390 and 1440 px. Their manual confirmation checks and source-write counts were unchanged.

The free provider contract was checked against StrikerAPI's public documentation. Authenticated provider timing and matchday quota usage still need verification from the deployed worker. [Lineup contract](https://strikerapi.com/docs/get-lineups) · [Provider timing](https://strikerapi.com/docs/timing-and-rights)

Production Supabase recorded `20261127044336_club_lineup_cadence`. A readback with the live 2,700-second base returned 180 seconds for a due external lineup and 2,700 for ordinary match details. The authenticated browser role had no execute permission on the scheduling routine. The Python suite passed 49 checks including the real helper bridge, the worker contract passed 20 groups and the offline Shiny lifecycle passed 14 checks across eight tabs. Verified badge behavior used synthetic team sheets.

## Manual club lineup checks · 2026-11-26

The full-schema PostgreSQL check verified that initial discovery, a generic refresh, expiry and completion leave the XI job parked. One explicit fixture request woke it. A duplicate tap was coalesced, a wrong lease could not consume it, the current lease consumed it once, and a later check of another fixture required a new request. The broker also binds provider access to the selected round, match and kickoff. Production readback after applying `20261127045106_manual_club_lineup_checks` confirmed that the guard trigger was enabled, browser roles lacked direct RPC execution privileges, and there were zero pending requests. No live provider check was triggered during verification.

Both current-round pitches used the shared persisted result in Chromium at 390 and 1440 px. The fixture counter stayed at zero through first render and became one after an explicit tap. Switching pitch, confirming a substitution and reopening a session reused that result. Button eligibility uses the current calendar and a local display timer. It creates no external request. The earlier per-league acquisition sweep has been removed. A fresh lookup may read a fixture date, the chosen game and its two club squads. A previously acknowledged, fully resolved XI returns directly from storage. A simulated outage on the second squad lookup preserved the first roster and the announced XI. After a worker restart, the next explicit check needed only the missing squad.

## Fixture limits and verified leagues · 2026-11-26

Full-schema PostgreSQL checks verified the two-check ceiling across resource keys, the 15-minute cooldown, the final 30-minute retry window, request expiry, budget contention at the 500-credit boundary, refunds and expiry of older reservations. One synthetic account held memberships in three supported competitions and an unmapped competition. SQL admitted the same synthetic match ID separately in each supported league, rejected mismatched membership and rejected the unmapped ID. Browser roles lack direct access. Production readback confirmed both fixture tables have a competition ID and recorded only the owner-reported two-credit standings call. Migration versions are `20261127050858` and `20261127051417`.

The Python worker and XI suites passed 54 tests. Three simulated competitions used identical provider IDs, date and kickoff. All four cold reads used the correct league, subsequent reads were cached, and an unknown competition used zero provider and budget RPCs. A failed refund write preserved the successful response and the worker retried the accounting adjustment before another paid read. The R quiet-calendar check averaged 12.5 ms across 380 fixtures with zero source reads. No new timer or network read runs on pitch navigation. A real outbound request adds one serialized database reservation across worker lanes and a settlement write only for an adjusted charge. The 31-day ledger is pruned on reservation. Database transactions finish before the upstream request.

The substitution and XI R checks passed. Both pitch views passed in Chromium at 390 and 1440 px, and the offline Shiny lifecycle passed 14 checks across eight tabs. All three deployment-manifest checks passed. The Futmondo competition IDs came from existing league catalog responses, and synthetic fixtures supplied the announced XIs. Verification used zero StrikerAPI credits. Real pre-kickoff availability on the configured key remains unverified. [Provider timing](https://strikerapi.com/docs/timing-and-rights)

The public `get_lineups` catalog entry explicitly states that the pre-publication response has an empty lineup and costs zero. An offline check with a previously matched game ID received that response, returned pending and released the five-credit reservation without a charge header. Reading it again from cache produced no database or provider calls. [Public endpoint catalog](https://mcp.strikerapi.com/v1/tools)

All nine focused XI worker tests passed after adding the empty-result contract. The public Dokploy app served the three verified competition mappings from commit `93edfb7` on 2026-11-27 UTC.

## Shared club lineup responses · 2026-11-26

Across fresh worker instances, two users both checked one game and obtained the same source observation time with independent fantasy player IDs. A third check for another game on the same league date retrieved only that game's team sheet. The four initial mocked reads plus that single additional read included exactly one `list_games`. The second game's unpublished result then replayed with zero source reads in another worker. An unacknowledged database save was retried without repeating acquisition.

Full-schema PostgreSQL checks covered lease contention, expiry, idempotent writes, superseded writers, the two-minute pending lifetime, date-list reuse with acquisition disabled, competition isolation and restricted access. The worker and XI suites passed 57 tests. After tightening daily schedule validation, all 11 focused XI tests passed again. The offline Shiny lifecycle passed 14 checks plus eight tabs. A quiet 380-fixture calendar averaged 12.7 ms with zero reads. Browsing adds no traffic. A worker memory hit avoids the database. A durable hit needs one indexed lookup. A fresh response adds one acknowledged database publication and the claim transaction includes the existing credit reservation. No paid calls were made during verification.

Production schema readback confirmed migration `20261127054006_shared_club_lineup_responses`, zero initial public responses, no browser-role table access and two previously recorded credits. No production fixture was requested for verification.

## Home notification count · 2026-11-26

Offline Chromium checks at 320, 390 and 1440 pixels verified that the Home badge fits the button, matches the header dot color, exposes “1 unread” in the accessible name and opens the inbox. A synthetic Shiny check changed the unread count to 42, zero, unknown and 3. Both indicators updated from a single adapter read per change. Only the badge output subscribes within the Home grid. The change adds no timer, source request or database query. The offline Shiny lifecycle passed 14 checks and eight tabs.

### Trends round 8 coverage (2026-11-26)

A scoped, read-only replay of persisted worker observations reproduced round 8 as the sole unresolved round. Nine of ten directory teams appeared in the round ranking. All nine scores matched independently closed team histories. The remaining team had a successfully saved empty history. Futmondo marked the published stage closed while the fixture calendar still marked the round partially played. The chart had skipped the ranking before comparing the two sources.

With stage reconciliation, each of the nine teams' cumulative totals matched an independent sum of its saved scores through round 13. Every published round had comparable ranks. The unlisted team had no plotted points or positions. The read used the production snapshot checksum and decoder. The existing observations contained the required evidence. Verification covered local code against those saved observations.

The 13 chart checks include known answers for zero, negative scores, an unlisted team joining later and conflicting stage evidence. The delayed-read suite and the offline Shiny lifecycle suite passed (14 checks and eight tabs). The affected league needs one additional existing ranking snapshot in the batch of gap lookups. Both charts reuse the result. Browsing adds no Futmondo requests, writes, timers or database schema changes.

The compressed-snapshot browser fixture also exercised the postponed round at 390 px and 1440 px in both themes. All four runs displayed the expected cumulative series and ranks, removed the missing-score notice, and recorded zero source calls. Replaying identical facts caused zero extra Standings chart publications. First curve times ranged from 0.96 to 2.51 seconds locally.

## My Team departure date heading · 2026-11-26

The offline My Team widget rendered **Leaving on**. All eight squad table checks and 12 championship column checks passed. The heading is set once per table render and adds no row processing, reactive dependency or data request.

## Player table row height · 2026-11-26

At a 390 px viewport, the same 12 synthetic player rows measured 46.19, 57.28 or 62.28 px with the original CSS. Wrapped names and a two-line offer increased the height through the combined outer and inner cell padding. With the new spacing every row measured 46 px. The shared-table Chromium suite passed at 320, 390 and 1440 px in both themes. It checked that both position badges fit side by side, both offer lines fit vertically, and scrolling, column resizing, row selection and data updates still worked.

The layout uses CSS on existing table body elements. It adds no DOM nodes, reactive work, timers, API calls or database reads. The fixture heartbeat took 259–263 ms across the six browser runs. The offline Shiny lifecycle passed 14 checks and eight tabs. Verification used local synthetic data.

## Loading pitch soccer ball · 2026-11-26

Offline Chromium verified two seeded opening positions on both the full-size and compact pitch. At 320, 390 and 1440 px in both themes, the ball stayed connected to eight progressively fainter trail segments, disappeared with the pitch and returned at the loop seam. Reduced motion used the static illustration with zero animated draws.

The ball shares the existing 30 fps renderer, computes a fixed-length trail from scene time and adds no timers, image downloads, API calls or database work. With the full-size and compact loaders visible together, p95 draw time was 1.1–1.3 ms. The pitch stage measured 2.8–3.4 ms at simulated 4× CPU slowdown. Hidden and removed scenes stopped drawing. The offline Shiny lifecycle passed 14 checks and eight tabs.

## Ground passes in the loading animation · 2026-11-26

The previous ball was nearly twice a player's head radius and followed an independent curve across an unfilled pitch. The new pass uses the figurines’ existing boot positions and pitch projection. Offline Chromium verified two seeded starts and a reception beside a shoe, a foreshortened shadow at the trail endpoint, and players drawn on either side of the ball in depth. The 320, 390 and 1440 px runs passed in both themes, including the loop seam. Reduced motion kept the static illustration.

The renderer stores ten pass targets per page and reuses one ball state per canvas. Four trail segments replace eight. The two depth groups each scan the 22 existing players and draw each figure once in total. The change adds no requests or timers. Local p95 draw time with both loaders visible was 1.1–1.3 ms, matching the previous measurement. At simulated 4× CPU slowdown the pitch stage measured 3.0–3.5 ms. The offline Shiny lifecycle passed 14 checks and eight tabs.
