# Worker display and round-history recovery

The November 14 report showed round-history progress falling from 432/527 to 14/527 and Today blinking every few seconds after database-only acquisition was deployed. Production aggregate checks found 527 successful history markers and 6134 numbered score rows for 527 players in the authoritative competition/season/scoring context. This was a reuse/display defect, not a missing 513-player source backlog.

## History reads

`new_shared_round_points_controller()` now uses the league database dataset directly when worker mode is enabled. Successful saved checks, including explicitly empty histories, remain reusable. A newer catalog's unnumbered fitness vector cannot demote acknowledged numbered history or start 527 individual snapshot waiters. Missing scores remain missing in averages; completion means the player's stored history/check was loaded, not that every round has a numeric score.

The existing worker status heartbeat drives one asynchronous shared dataset read at most every 60 seconds while incomplete and every 300 seconds once settled. The reader retains its bounded incremental baseline and correction overlap. There is no additional timer, source request, GUI persistence write or acquisition scheduling. Manual Refresh requests one database reread. Failed reads retain displayed facts with an error; partial reads merge observations without deleting prior rows. Complete reads can replace the baseline, including authoritative deletions. Newly stored negative scores and corrections flow into averages.

In worker database mode, `resource_context()` can use the last observed account/league identity after its freshness deadline. `league_context_metadata(allow_stale=TRUE)` includes the recorded scoring identity, while the default fresh lookup still rejects expired evidence. Compact membership responses retain previous scoring evidence with its original independent deadline. Explicit unknown scopes, different accounts/leagues, live action checks, and newly observed season/scoring changes retain their isolation. No expiry or source timestamp is renewed by reuse.

## Stable display

`resource_display_source(session, read)` publishes a presentation projection only when its value changes. The data layer still receives source timestamps, failures, freshness transitions and new versions. Today cash/valuation cards, transfer rows, readiness flags, league/team IDs, the financial heading and position counts use the projection. Real value changes and validation/context failures still update the affected output. The worker banner keeps its container mounted and updates its status/check-time text separately.

This addresses new successful observations with unchanged facts, which the earlier unchanged-version poll test did not cover. The countdown's server-time markup is no longer regenerated merely because an unrelated source clock advances.

## Discovery and redundant jobs

The worker bridge has a separate `discovery_resource` message for a narrow allowlist of saved membership, roster, round-catalog and transaction dependencies. Successful expired observations may identify work to schedule, with exact account/league/context checks and unchanged timestamps. They cannot satisfy ordinary fresh reads or manual preflights, and are not added to the parent's source-response timestamp collection.

`20261114135558_worker_summary_job_retirement.sql` retires only idle legacy player-summary jobs whose acknowledged replacement exists in the currently verified context. Unknown-context duplicates require exact arguments and all non-context dimensions; blank-team duplicates require one unambiguous nonblank-team replacement. Ambiguous team scopes and running jobs remain. The migration changes job metadata only and preserves snapshots, chunks and normalized history. Canonical discovery and the database-only GUI do not recreate these legacy descriptors, so a recurring cleanup scan is unnecessary.

No packages, environment variables, service lanes or source rate limits change. Verification results and production rollout status are recorded in `data_review_results.md`.

The migration was applied as `20261114140350_worker_summary_job_retirement`: 1,326 obsolete jobs retired, 567 summary jobs retained, all 1,340 summary snapshots and 527 current-context history checks preserved. Local browser tests reproduce the old blinking and verify unchanged observations no longer replace those outputs; authenticated hosted acceptance remains separate.


## Saved transfer observations and the cached financial label

The later November 14 report showed successful incoming/leaving schedules alongside an orange roster-expired error. Production metadata confirmed successful own-roster and balance acquisitions; the observations were roughly 101–158 seconds old with a 60-second worker deadline. Recurrence is measured from completion and validity from source observation, so expiry alone is expected between runs and does not prove a failed acquisition. Source failures remain separate. No deadline or scheduling interval is extended to remove a warning.

`incoming_clause_source()` consumes successful saved roster/history observations as an explicit display-only `observed` state, retaining original source times and exact account/league/team/competition/season/scoring isolation. Partial data, failed reads, malformed identities/timestamps and missing summaries remain incomplete or errors. `reconcile_pending_clause_transfers(observed_only=TRUE)` never grants current acquisition capacity: action IDs stay empty, counts unavailable and strict status unavailable. Its separate `observed_outgoing_status` describes only whether historical departure details can be displayed. A summary older than the transaction cannot resolve that later transfer; elapsed dates await newer source confirmation.

The panels show roster/transaction observation times separately from the record display. Financial `(cached)` continues to mark amounts past their freshness deadline, with an explicit explanation that they are the last saved amounts and may have changed. Balance and offer observation times update in their own text output. Actual financial read failures take precedence over the routine saved-data explanation. Manual transaction preflights, source timestamps, numeric values and unknown-versus-zero behavior are unchanged. Successful worker projections are already durable and are not written back by the GUI.

No database migration, environment variable or dependency is needed for this display change. See the appended review results for verification and deployment limits.

The accompanying scheduling correction is `20261114144534_worker_shorter_cadence.sql`, applied in production as `20261114144934_worker_shorter_cadence`. A successful idle summary promoted from hourly to five-minute collection now receives a due time based on its previous completion and the new interval. Equal/slower rediscovery, failed/active work and source freshness remain unchanged. A guarded one-off repair covers already-promoted current own-team summaries with successful exact-scope market evidence. The generic display path still cannot schedule acquisition.
