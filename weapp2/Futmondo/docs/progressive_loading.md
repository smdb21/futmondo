# Progressive login, Today, and player cards

## Pitch, murmuration, and radar loading visual

`fm_loading_visual(id, compact = FALSE)` in `loading_visual.R` returns a decorative,
theme-aware scene with a static SVG fallback and a browser canvas. The launch
preloader uses a 420px scene on desktop and landscape. On portrait phones up to
767px wide, the fullscreen launch scene fills the upper half of the visible
viewport, with the brand and loading text directly below. Authenticated `loading_leagues` and
`loading_today` states use its compact 168px variant with visible, accessible
status text. Error, empty, partial, and retry feedback keep their existing
behavior. The visual follows actual readiness and never delays completion to
finish an animation. It does not represent real player positions or progress.

The local `www/loading-visual.js` and `www/loading-visual.css` assets animate a
15-second loop: the flat pitch tilts and rotates into perspective, 22 table-football
figures face their opposing goals, rise from the surface, dissolve from head to
foot, and release particles upward into a flowing flock. This dissolve applies
to every loader size.
The flock gathers into radar rings and reveals moving player dots before
returning to the flat pitch. The component uses the existing `--fm-*` colors and updates on light/dark changes. Its canvas is
hidden from assistive technology; the separate text supplies the loading status.
Reduced-motion preferences or unavailable canvas rendering retain the static
pitch/radar illustration.

The added 3D stage uses a small perspective projection in Canvas 2D rather than
a separate graphics engine: the pitch rotates from 1.2–3.2 seconds, players rise
from 2.2–4.5 seconds, and their silhouette particles disperse into the flock from
4.5–6.2 seconds. Each particle keeps its body position until release, initially
travels upward on a curved path, then joins the cloud above the pitch. The
remaining figure is clipped from the head down in sync with release; the pitch
and ground shadows fade during departure. Radar assembly starts at 8 seconds,
player detection at 10, and the return to the pitch at 13. It uses the existing particle budget, local theme
colors and compact-size stroke scaling; no models, textures or network assets
are required. The extended loop never prolongs loading or changes readiness.

The private `cameraAt(t, instance)` helper computes the view once per frame,
fitting the board and shadow within short portrait canvases. Camera rotation
eases to rest at takeoff, so particles leave stable body positions. `project()`
treats the existing pitch coordinates as the ground plane and positive Z as
height, reusing the camera's `px`, `py` and perspective-scale slots for projected
points. `raisedBoard()` draws the shallow edge and shadow; `risingPlayers()`
draws 22 anchored foosball figures with round heads, solid shirts, rigid lower
bodies and broad kicking feet, using cached formation positions and a bounded
lift array. Green figures face +X toward the right goal; gold figures face -X
toward the left. Shirt width lies across pitch Y, with shallow depth along X,
forward kicking feet and small nose cues. Cached front/back/side/top faces provide
volume; the camera eye position selects the visible face for each player.
`playerFace()` clips these faces at the descending local-Z dissolve boundary
without allocating intermediate vertices. `makeParticles()` assigns each existing
particle a player and a sample of the same goal-facing 3D silhouette and a release time when the particle
budget changes, retaining both foot corners even at the smaller particle budget.
Viewport-height changes reuse this pool. `radar()` and its particle targets share
a fit factor for unusually short portrait scenes so the rings and crosshairs
remain visible. `readPalette()` caches green and warm-gold team tones from the existing text/warning theme
tokens. These tones stay consistent through the figures, morph particles and
radar player dots, while the radar beam uses the existing accent color.

The mobile layout is scoped to `.waiter-overlay .fm-loading-launch`, including
safe-area padding and a `50dvh` drawing height (`50vh` fallback). Its inherited
`--fm-loading-mobile: 1` flag selects the fuller sky composition. The renderer
measures both dimensions on resize and uses uniform scaling with extra vertical
space; pitch geometry and molded figures are never stretched. The mobile flock
spans approximately 90% of the drawing width. The existing compact banner and
desktop/landscape layout retain their dimensions. Static SVG fallback also fits
inside this region without stretching; reduced-motion CSS takes precedence over
the drawing marker immediately. Browser viewport-height changes reuse the
existing resize observer; there is no new polling or per-frame DOM measurement.

Rendering is browser-only, with one shared animation scheduler capped at 30 FPS,
bounded particle counts (320, or 180 for phone/compact scenes), and pixel density capped
at 1.5. Geometry and palette values are reused between frames. Hidden documents,
hidden ancestors (including the retained waiter overlay), and removed components
stop drawing; dynamic mounts cannot create duplicate animation instances. Lost
canvas contexts restore the SVG until rendering is available again. Initial
markup, direct insertion of a component, and the existing startup/waiter hosts
are discovered automatically. A future container that inserts nested component
markup should declare `data-fm-loading-host`; unrelated table subtrees are not
rescanned. No server animation timer, reactive frame updates, source/DB request, persistence,
new dependency, or configuration is introduced. Existing factual observations
continue through the application's durable database-backed resource layer.

The focused offline browser fixture and measured rendering results are recorded
in `data_review_results.md`; production rendering remains a separate acceptance
step.

## Progressive resource loading

Authentication uses `fm_async_login(user_name, password, owner)` and the existing cooperative curl pool. It makes one uncached request, never retries authentication, clears password references after dispatch, and ignores replies belonging to a logged-out or superseded attempt. Pending submissions are disabled in the browser and guarded on the server. Login and shared-resource observers attach asynchronous completion handlers, then return `NULL`; returning the pending promise would make Shiny hold the session output flush and hide progressive loading feedback.

The session's `startup_stage` reactive records `signed_out`, `authenticating`, `loading_leagues`, `loading_today`, `ready`, `partial`, `empty`, or `failed`. Immediate browser feedback covers authentication. A global status covers authenticated loading, changing to a truthful slow-source notice after five seconds. Its single timer is cancelled on session end and superseding context/refresh; a closed-session guard prevents late writes. Navigation is available after league selection. Failed startup retries refresh only unavailable primary resources.

Today fetches team information and market independently. Cash uses `budget`; observed team value uses `teamValue`/`team_value`. Neither waits for affordability, roster, pressroom, lineup, historical averages, or notification count. Optional work begins after both primary requests settle, including an empty or failed result. Each failed primary resource has its own retry. A missing amount is not zero.

`factual_read_adapter(..., state_only=TRUE)` exposes the existing shared state without raising a loading validation. It retains scoped keys, TTL checks, bounded registries, pending coalescing, and refresh behavior. Existing callers retain the validating value interface. `async_adapter_fetch(..., is_current)` checks ownership before every replay/read; obsolete work stops before another paginated request. Already-issued shared HTTP is not cancelled merely because one card closes.

Cards render the captured clicked row's identity and statistics without re-evaluating optional table enrichment. Plotly output/binding construction starts only after the browser acknowledges that identity has painted (two animation frames after its output update), so initial widget loading cannot hold the first useful card display. The acknowledgment checks player identity to reject obsolete events; it creates no timer polling. Summary, scoring lineup, stored points, stored values, profile, matches, acquisition capacity, and own listings resolve independently. Stored points wait for summary/scoring context; live summary points can display before database reads finish. `assemble_player_card_history` performs pure reconciliation using the existing history rules. Observation persistence waits for the stored-points read to settle so comparisons use available previous records.

The player-box header now depends only on the selected row. Action controls subscribe to summary and verified capacity/listing state, independently of history, valuation, profile and scoring completion. Team lineup and own-listing resources exclude the selected player from their context, so switching cards shares an already pending team read while preserving account/league/team/session isolation. A held-source regression reduced header updates from five to one and optional action reconstructions from three to zero, with seven source reads in both cases. Existing acquisition freshness and manual-action checks remain in place.

The HTTP scheduler keeps its four-connection limit and reserves one of those connections for interactive work: at most three normal requests can be active. Historical lineup reads now have interactive priority for both their database and source stages. Eligible normal requests retain FIFO ordering and the existing four-interactive-dispatch fairness rule. This trades background concurrency from four to three so slow backfills cannot occupy every connection. Admission remains bounded at 32 requests; a full admission budget still reports its existing read-limit failure. In a real loopback test, a foreground request completed in 42 ms while three 400 ms background requests remained active, with no duplicate requests and a peak of four connections. These are synthetic timings, not hosted latency guarantees.

The lineup read also opts into progressive saved-data publication. After the existing database GET returns a validated expired snapshot, the shared state publishes it as loading/stale while the same acquisition continues to the source request. The preview retains its original observation timestamp and source deadline; it is never saved as a fresh acquisition. Fresh saved snapshots still skip the source, and successful fresh results replace the preview and use the existing persistence path. A relay keeps same-context refreshes subscribed to their coalesced pending read; settlement retires that relay and late callbacks cannot replace a later generation. Account, league, team, round and session guards remain in force. With an identical 500 ms synthetic source delay, first saved-lineup availability improved from 1,091 ms to 453 ms; fresh completion stayed about 1.1 seconds, with one database read, one source read and one save in both runs. Cold lineups without a usable saved snapshot still depend on source response time.

Verification for these loading changes: transport-priority checks (10), progressive saved-lineup checks (6), refresh/coalescing checks (2), card-work checks (7), existing progressive loading (16), card availability/rechecks and existing-offer controls passed. An isolated release excluding unrelated working-tree edits passed the shared-cache checks (10), lineup expiry, card-work and pending-save checks, deployment checks, and full offline lifecycle (13 checks plus eight tabs). Targeted Chromium checks at 1440/390/360 px showed player identity in 403/439/364 ms and zero added history/profile reads on warm reopen, with all external HTTP blocked. This targeted run adapted the existing browser fixture to select the named player independently of sorting, accept the intentionally collapsed recent-round panel, and omit its unrelated transfer-loading precondition. The unmodified all-up browser script failed those fixture preconditions and is not reported as passing. Production network timings and hosted rollout were not verified. Sol implemented; Luna investigated scheduling and independently reviewed generation/fairness guards; the primary agent reviewed the final changes and isolated release. No schema, dependencies or source-timeout changes were made.

Card rendering no longer runs transaction preflights or synchronous listing reads. Passive acquisition eligibility uses asynchronous capacity and verified summary state. Controls remain disabled while evidence loads. Actual manual actions retain their existing final preflights and uncertain-response protections.

Ordinary opens reuse fresh resources. Refresh history invalidates only selected summary/history/matches and its team's scoring lineup, including the real inner adapter cache keys. Profile, acquisition, and listings remain reusable. Closing, switching players, league changes, and logout invalidate consumers and prevent obsolete replay/pagination.

## Recent rounds

`build_player_recent_rounds(points, matches, context, limit=5, current_player_responses=FALSE)` is pure and returns round groups plus unassociated match rows. It preserves true zero, negative, and unavailable points. Exact match/round identifiers can associate records unless explicit season, competition, or round evidence contradicts them. For current responses fetched for the same selected player, the card opts into round-number matching for live Futmondo points when optional source identifiers are absent. Explicit season/competition or identifier conflicts still block that fallback. Saved-only history retains the stricter requirement for source-provided matching season and competition. Selected fantasy-league IDs and dates never manufacture real competition/season context. Conflicting duplicate match IDs remain unassociated; identical duplicate fixtures are removed. Multiple fixtures within a round share its one total.

The card displays one wrapping Recent rounds section above the profile, covering up to 50 available point rounds. This section is collapsed by default using native details/summary controls; clicking or keyboard-activating the heading reveals its content without another request. A reactive rebuild resets the disclosure to collapsed. Association is performed before the display limit, so older paired matches are not mislabeled as unassociated. Known associations show points, result, and date in the same box; unverified matches remain visible with an explicit association-unavailable label. Current player responses do not need optional season/competition or stable identifiers to display their corresponding round together; saved-only or contradictory records may remain unassociated. Repeated provisional badges were replaced by one expandable explanation that source finality is not always provided. Per-player explicit logical true/false flags produce final/provisional status; missing, malformed, or conflicting evidence stays unknown through reconciliation and persistence. Round completion cannot finalize points. Older saved provisional labels have uncertain provenance and are not bulk rewritten. Unknown or nonpositive height/weight is omitted.

## Performance review and verification

No dependencies, database queries/schema, connection limits, polling loops, or unattended automation were added. Primary content is dispatched first; optional reads reuse existing bounded shared caches. Each card uses eight fixed resource slots and the existing four-connection transport pool. Browser-only dismissal handlers are namespaced and replaced on reopen. Timing metrics retain at most 64 samples with fixed stage labels and elapsed seconds; they contain no credentials or account/player identifiers. Server timings describe scheduling/acquisition completion, while browser acceptance checks measure visible rendering. `testServer` state checks alone cannot establish that a real browser receives intermediate output flushes.

Local focused checks: 15 progressive delayed-resource groups passed; five login feedback checks passed; three Today amount/cache checks passed; six profile endpoint checks passed; UI reliability checks passed; four real local HTTP async checks passed, including dispatch at 0.0258 seconds, heartbeat before delayed response, and zero warm composite-card requests. The vectorized recent-round builder averaged 0.00355 seconds for 1,000 fixture rows across 100 local runs. Fixtures exercise identity before source completion, partial sections, warm reopening, scoped history context, obsolete consumers, duplicate login protection, logout during authentication, primary prioritization, and failed-only retry. Plotly empty-fixture warnings are expected. The mandatory offline lifecycle passed 12 checks and all nine tabs with zero HTTP/live database mutation attempts, including prior-session callback cleanup and simulated post-login navigation. Independent point/fixture association and browser acceptance results are recorded in the integrated review.

Finality regression checks use synthetic fields to verify conservative parser behavior; those checks do not prove that a live API supplies those flags. The finality helper averaged 0.0000281 seconds per synthetic point over 10,000 runs and adds no requests, caches, or timers. Focused contracts (13), data service (10), observation recording (8), history persistence (7), trend plot (10), async transport (4), independent history, and the mandatory 12-check/nine-tab lifecycle passed after unknown-state propagation. No live login, transaction, hosted cold-start benchmark, or production latency guarantee is implied by synthetic results. External source latency remains visible through section loading and safe error states.


## Stable tables and explicit refresh (2026-10-31)

The banner clock updates once per second using the already-loaded round and finance states. It does not invalidate or poll team information, market listings, rounds or player histories. Global Refresh, a targeted retry, or a confirmed relevant transaction still invalidates the existing shared resources. Navigation reuses the root primary snapshot; page-owned factual reads retain their cache check on resume.

Player tables mount on account/league/team, visible column schema, or requested-N changes. Published average changes use `updateReactable` for data only and a JavaScript cell renderer; integer and double numeric storage share a schema. Other visible field changes regenerate the widget because existing R cell formatters produce static rendered content; this prevents stale prices, offers or player details. The current page is retained within its valid range, selection is remapped using player IDs, and source timestamps/hidden coverage-only changes do not send an update. Search and browser filters remain attached to the same widget. History progress is a separate output and can legitimately advance while a one-time backfill is running; it does not rebuild the player table. Inactive `recent_player_points_source` clients do not evaluate their player-row source, so hidden pages cannot demand or restart history acquisition. Existing two-read concurrency, five-second history publication, queue/memory bounds and manual transaction preflights remain in effect.


## Welcome readiness and Background sync flush isolation — 2026-11-15

`server.R` now evaluates `startup_primary_ready` and `startup_stage` from the
existing Today team/market resource projections on both Welcome (`login`) and
Today. Previously Welcome unconditionally returned false and continuously
showed “Loading today's market…”, including after successful data retrieval.
The notice now clears when both primary results settle, and actual partial/
failed results retain their existing error/retry behavior. Other page gates,
context changes and Today’s settled-context latch remain unchanged. This reuses
shared database-backed observations and does not acquire a second copy.

The local `authorize()` helper inside `fm_worker_session()` starts its existing
async worker RPC and returns `invisible(NULL)`. Its callers are observers;
returning the promise made Shiny wait for credential renewal/settings writes
before flushing unrelated UI. Acknowledgement/error callbacks still update
policy and feedback, with the existing account and generation guards. This
restores rendering/navigation while a request is pending rather than treating
an unacknowledged write as successful. No source acquisition or refresh cadence
is changed. Test fixtures block live I/O and retain original persisted-source
freshness semantics. See `test_welcome_primary_loading.R`,
`test_worker_renewal_flush.R` and `test_startup_admin_browser_app.R` / `.cjs`.
