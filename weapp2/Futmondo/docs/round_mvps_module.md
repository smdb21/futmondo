# Round awards module

## Reading saved awards

In the deployed app, `shared_mvp_source()` opens the persisted `get_mvp_saved_catalog` snapshot for the signed-in account, championship, competition, season and scoring version. An expired league metadata observation can still identify its saved selections. If any scope field is missing, the reader uses the session’s resolved league context only when the account, championship and all known fields agree. The page waits for the competition and season before requesting awards. When the league omits scoring metadata, the existing worker reader can serve the explicit unknown-scoring scope or verify a redirect to its canonical scoring version.

The active award views share one subscription to that catalog. A new stored version updates the round cards and Statistics together. An incomplete evidence read shows its coverage warning alongside the selections that were returned. The factual worker handles settlement rechecks during collection.

Position labels in the official Best XI use the existing background pills: green GK, orange DEF, blue MID and red FWD. Unknown roles stay explicit plain text. This presentation reuses the shared badge CSS without adding source requests or changing award data.

`round_mvps_UI(id)` renders **Round awards**, with a **By round** tab showing the MVP and full official Best XI for each published selection, newest first. Each lineup shows roles and points and highlights the MVP. Unknown scores remain unavailable; zero and negative scores are preserved. `round_mvps_Server(id, is_module_active, login_token, championship_id, refresh_trigger)` consumes the same root-session `shared_mvp_source` as Admin and Rivals. Page navigation loads saved full-XI snapshots; it never triggers a historical backfill.

`round_mvp_rows(rows, championship_id)` finds the `is_mvp` player in each published selection. The round cards can therefore include a provisional selection or one whose finality is unknown. `round_best_xi_rows()` selects the same round by immutable ID (falling back to the display number only for legacy rows without IDs), checks eleven unique players and one MVP, and orders source roles as GK/DEF/MID/FWD using the existing role mapping. Internal module, tab and output IDs remain unchanged.

Performance: lineups reuse the existing shared award snapshot and one shared championship player catalog read per render. The existing resource adapter reuses persisted catalog observations before acquisition; there are no per-player requests. All completed rounds remain visible in responsive cards; a 38-round season contains at most 418 lineup rows. A local offline render of 38 rounds with crests took 0.664 seconds; browser and network latency are not included. Focused coverage lives in `test/test_round_awards.R` and `test/test_best_xi_crests.R`.

`normalize_mvp_catalog(active, leagues, championship_id)` matches immutable league/round IDs and source display metadata. It preserves explicit completion evidence; stale compact `future`/`next` statuses stay unknown and do not suppress explicit acquisition. Missing display numbers are enriched from unique saved metadata or identity-checked match-list responses during explicit Admin retrieval; array positions never supply ordinals. Empty/upcoming-only catalogs cannot establish historical absence.

`normalize_official_dreamteam()` validates exactly eleven unique players plus one matching MVP. The request reference supplies the round ID/number/finality, because the captured answer lacks round/finality identity. The successful current-round capture is not proof of settlement. Selected roles/positions, points, formation and scoring metadata are retained.

Use the Admin **Retrieve missing round awards**, refresh, and failed-save retry controls to acquire selections. A fetched round is marked saved only after persistence acknowledgement. Full immutable revisions replace prior membership; legacy hardcoded final flags are downgraded to unknown. Notifications mark rounds for explicit recheck rather than adding money or triggering an unattended backfill.

See [coach_and_round_awards.md](coach_and_round_awards.md) for all new interfaces, controls, persistence, cache and concurrency contracts; [rival_cash_accounting.md](rival_cash_accounting.md) for historical eligibility/payment limits.

Compatibility sync helpers remain available for older direct callers; the interactive Admin path uses bounded cooperative reads and acknowledged immutable evidence writes. `test/test_mvp_runtime.R`, `test/test_admin_awards_coach.R`, and the mandatory offline lifecycle cover the changed behavior.

Best XI player cells reuse the player-table name formatter: the current real-club crest appears before the official award name, with a club tooltip and accessible image label. Player IDs map to one shared championship catalog read per render, using the existing database-backed resource adapter and persistence; there are no per-player requests. Crests load lazily. The awards remain visible while the catalog loads. A failed catalog read exposes its error.

## Player award statistics — 2026-11-25

**Round awards → Statistics** ranks players for the selected championship and season.
A player earns one Best XI appearance per counted round. The official MVP also
earns one MVP win in that round. Round share divides a player's appearances by
the number of confirmed rounds included in the ranking.

`round_award_player_stats(rows, championship_id)` takes the shared view of the
latest persisted award revision for each round. A counted selection must contain
eleven distinct player IDs, one explicitly identified MVP and final completion
evidence. A save in progress or failed is reported separately. The saved reader
attaches the account, league, season and scoring context before projection.
Conflicting seasons or scoring contexts in otherwise eligible rows prevent a
combined ranking. Immutable round IDs distinguish equal or missing display
numbers. Legacy records that lack the round ID column use their recorded
round numbers. Names come from the latest numbered selection with a recorded
name. Identical display names are distinguished by their player IDs.

The summary gives the round count, the number of distinct Best XI players and
the number of distinct MVP winners. Two horizontal bar charts show up to five
players each. Both charts use the same scale from zero to the number of confirmed rounds.
Equal totals sort by player name and then ID. A note identifies any further
players tied at the cutoff. The searchable, sortable table includes every
selected player and shows their appearances, MVP wins and round share.

The Statistics tab renders on demand from the same scoped award snapshot as the
round cards. It uses official award names and shares the cards’ saved player
catalog when historical club or position details need a fallback. Chart labels and counts
are available as text. The charts sit side by side when space permits and
stack on phones. Source coverage messages accompany partial rankings.

### Club crests and positions — 2026-11-25

Both leader charts and the All award winners table show a crest and position
beside each name. The crest comes from the most recent counted selection that
records a club or logo. The club name and logo always come from the same
appearance. The position label lists the distinct playing roles recorded in
counted Best XIs in GK, DEF, MID and FWD order. The official numeric
`position` is a pitch slot and cannot supply a playing role.

`normalize_official_dreamteam()` retains the official player’s `team` and
`logo` as `award_team` and `award_logo` in the full-XI evidence payload.
`rival_award_snapshot_rows()` restores these alongside the existing role
and score. The legacy relational writer projects only its supported columns.
Older saved revisions remain readable. `round_award_player_stats()`
projects the club pair and `award_positions` from eligible selections.

`round_award_player_identities()` matches an older selection to the same player ID
in the saved championship catalog when award metadata is missing. A club name
recorded in the award limits logo fallback to that club. The labels use the
existing club-image mapping when only a club name is available. A missing
historical role can use the catalog’s primary and secondary roles, identified
in the tooltip. Without a recorded club or position, the name and totals stay
visible. An unknown position displays a dash and a tooltip.

The Player column keeps its original text for sorting and searching. Only the visible page constructs table labels.
The charts and table share the resolved club and position fields, and images load lazily. Updating club details reuses the existing
award counts.
