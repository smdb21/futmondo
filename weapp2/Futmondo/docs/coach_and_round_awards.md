# Login coach and Admin round awards

Local implementation, 2026-10-31. Hosted deployment and authenticated historical-round responses remain separate verification steps. The supplied successful dreamteam response is from an unfinished round.

## Login coach

Today contains **Automatically hire coach**, enabled for a new preference. `coach_on_login_source(session, login, championship, user_team, team_state, retry_team)` owns the login action; Today only resolves/presents its preference and status. The account/championship preference is remembered in this browser's localStorage. It does not travel between browsers or devices. The server waits for the browser's context-keyed preference response before acting. Unreadable storage disables the action for that login; failed writes are disclosed beside the control.

The action is restricted to the own team and the league selected at login. Refresh, timers, navigation and switching leagues never create a second attempt. Fresh shared `get_user_team_info` data must report `configuration.enableCoach = "automatic"` and `staff.coach = 0`; a missing/unknown/stale/failed field cannot authorize hiring. `staff.coach = 1` needs no request. Disabling the checkbox prevents an unsubmitted action.

`build_hire_coach_payload()` produces the captured `POST /1/userteam/hirecoach` query `{championshipId, userteamId, roundId: null, type: "coach"}`. `fm_async_hire_coach()` submits once through cooperative `fm_async_http`, never the replaying read adapter. The captured embedded coach value remains zero even on success, so both a response and an ambiguous network failure trigger one fresh shared team-information read. Only `staff.coach = 1` confirms completion. Failure, uncertainty and logout never replay the mutation.

The source uses a per-login latch and discards obsolete callbacks after logout/account/league changes. A bounded process-local account/league/team guard keeps 128 identities for five minutes, preventing concurrent tabs in the same worker from using a stale absence response to submit duplicate actions. Futmondo exposes no verified idempotency key in the capture: cross-worker/host duplicate prevention cannot be guaranteed. There is no polling loop, stored session, schedule, or trading automation.

Normal request budget: reuse initial shared information; zero mutations for an existing coach/opt-out/unknown mode; otherwise one hire and one confirmation read. Only the team/staff/balance resource is invalidated.

## Award source and Admin controls

`normalize_official_dreamteam(answer, championship_id, reference)` accepts exactly eleven unique selected players and exactly one MVP ID matching those players. It preserves source zero/negative/missing points, role/selected position, formation, pointSystem/custom metadata and request round identity. Missing display numbers remain unknown and do not prevent immutable JSON storage. The answer's existence never proves that the round is finished.

`award_runtime.R` supplies:

- `award_selection_valid(rows)`: complete-XI validation used at retrieval, saving and reuse.
- `award_scoring_version(point_system, custom)`: stable scoring fingerprint, or explicit `unknown`.
- `award_expected_scoring(login, championship_id)`: reuses the cached active championship's `ps`/`pointSystem` and `custom`, where present. Known contradictory/unknown saved scoring is excluded when the expected context is known; no calendar or extra lineup request supplies missing context.
- `award_saved_rows(snapshots, championship_id, season, expected_scoring, account_id)`: validates the private envelope, decodes full snapshots and preserves their explicit finality. Previous snapshots without finality metadata are unknown, even if legacy rows hardcoded `is_finished = TRUE`.
- `get_mvp_saved_catalog(login, championship_id)`: one bounded account/league/season evidence read plus shared round discovery. Known and unknown seasons remain separate; the configured/observed season helper is used, never the date.
- `award_snapshot_evidence(...)` and `persist_official_award_snapshot(..., on_complete)`: immutable whole-selection JSON revisions and acknowledged asynchronous writes through the existing bounded persistence queue.
- `award_catalog_numbers(rounds, saved)` reuses unique scoped saved round numbers; `award_round_labels(numbers, ids)` preserves distinct unknown-ID labels.
- `get_award_round_reference(login, championship_id, reference)` checks explicit schedule identity during Admin acquisition only.
- `award_coverage(rounds, rows, failures)`: locally joins catalog and saved/live selection state.
- `shared_mvp_source(session, login, championship, active, refresh)`: one root-session state shared by Admin, Rivals and Round MVPs. Its returned reactive has explicit `retrieve(mode, round_id)` and `retry_save()` attributes.

Opening a consuming page reads saved coverage; it never starts a missing-round backfill. Admin offers **Retrieve missing round awards**, **Refresh provisional or changed rounds**, **Refresh selected round**, and **Retry failed saves**. Compact `/2/league/list` `future`/`next` statuses are not reliable finality or eligibility evidence: the supplied coach capture labels its explicitly numbered current round 12 as `future`. Such IDs remain eligible for explicit lookup with unknown finality. Unknown display numbers use distinct official-ID labels, never `Round NA`.

Explicit acquisition uses cached `/1/match/list` queries `{championshipId,userteamId,roundId}` to obtain missing round numbers. The returned `answer.round._id` must equal the requested ID before using its positive integer `number`; list position never supplies a number. Scoped saved snapshot numbers are reused when unique and consistent. A verified number later than the sole current-number anchor is **Not due** and avoids a dreamteam query. Other IDs can be queried even if their display metadata remains unavailable. Only positive completion evidence establishes finality; a schedule, a score, and a published XI cannot establish credited payment. Current rounds with an explicit unfinished flag remain provisional. Absent finality evidence is **Finality unknown**, not a provisional label on every past round. Refreshing provisional/selected selections first refreshes the catalog so a later completed-round flag can replace an earlier provisional observation.

Coverage distinguishes selection presence, finality, MVP, last fetched time, save status and errors. Progress separates fetched, confirmed-saved, failed and pending rounds. A queued write is **Saving**, never **Saved**. Only its database success callback confirms storage. A failed write retains usable live data and an explicit save retry; retrying that write submits the same immutable revision without another Futmondo request. A malformed/empty/error response does not erase a previously valid selection. Revisions replace the complete membership for a round instead of accumulating players or bonuses.

At most two round requests run concurrently under the existing global four-connection transport pool, with write backpressure and a 120-round explicit batch cap. The root evidence read remains bounded at 20,000 latest facts. Tables/progress publish at completion or at most once per two seconds, not per player. Leaving Admin stops launching its queued requests even if Rivals remains a display consumer; existing responses may finish saving. Logout/context changes discard stale callbacks. Returning to Admin requires a new explicit click to resume missing acquisition.

## Rival estimates and limits

Rival award estimates consume only positively finished selections, explicit historical player eligibility and configured rewards. Current/provisional/unknown-finality selections remain visible in Admin but cannot become credited XI/MVP income. Each full-XI reward and separate MVP reward is counted once; identified recorded payments replace their matching estimate. A whole-selection correction removes prior recipients. Unclassified income prevents overlapping inferred reward additions. Saved selection coverage alone never establishes complete payment or eligibility coverage or a verified cash balance.

The existing `rival_finance_evidence` JSON object and latest view already support formation, scoring and finality metadata; no schema migration is needed. Credentials and private HAR files are excluded from Git/deployment. Nightmare-team responses are not used for rewards.

The currently supplied captures do not establish historical rival lineup eligibility, complete payout metadata, ranking allocation details, or a successful completed-round dreamteam response. The implemented Admin path makes the per-round query and retains failures honestly. The capture proves only the current `roundId:null` match-list response; historical non-null round-ID acceptance is tested with fixtures and remains unverified against the live account.

## Checks

`test/test_admin_awards_coach.R` covers complete-XI identity/finality, zero points, unfinalized cash exclusion, captured coach envelope, scoped preferences, opt-out, unknown/stale/unsupported coach states, duplicate latches/cooldown, logout, explicit bounded acquisition, future-round exclusion, save acknowledgement/failure/retry, and account/league context replacement. Existing focused MVP and rival-finance fixtures cover revisions, payment replacement and eligibility arithmetic. The cross-cutting offline lifecycle remains mandatory. See `data_review_results.md` for exact executed results and performance measurements.
