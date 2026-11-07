# Round awards module

Position labels in the official Best XI use the existing background pills: green GK, orange DEF, blue MID and red FWD. Unknown roles stay explicit plain text. This presentation reuses the shared badge CSS without adding source requests or changing award data.

`round_mvps_UI(id)` renders **Round awards**, with the MVP and full official Best XI for each completed round, newest first. Each lineup shows roles and points and highlights the MVP. Unknown scores remain unavailable; zero and negative scores are preserved. `round_mvps_Server(id, is_module_active, login_token, championship_id, refresh_trigger)` consumes the same root-session `shared_mvp_source` as Admin and Rivals. Page navigation loads saved full-XI snapshots; it never triggers a historical backfill.

`round_mvp_rows(rows, championship_id)` filters the single `is_mvp` player from each positively finished round. Provisional or unknown-finality selections remain available for inspection in Admin and do not display as completed awards. `round_best_xi_rows()` selects the same round by immutable ID (falling back to the display number only for legacy rows without IDs), checks eleven unique players and one MVP, and orders source roles as GK/DEF/MID/FWD using the existing role mapping. Internal module, tab and output IDs remain unchanged.

Performance: lineups reuse the existing shared award snapshot and one shared championship player catalog read per render. The existing resource adapter reuses persisted catalog observations before acquisition; there are no per-player requests. All completed rounds remain visible in responsive cards; a 38-round season contains at most 418 lineup rows. A local offline render of 38 rounds with crests took 0.664 seconds; browser and network latency are not included. Focused coverage lives in `test/test_round_awards.R` and `test/test_best_xi_crests.R`.

`normalize_mvp_catalog(active, leagues, championship_id)` matches immutable league/round IDs and source display metadata. It preserves explicit completion evidence; stale compact `future`/`next` statuses stay unknown and do not suppress explicit acquisition. Missing display numbers are enriched from unique saved metadata or identity-checked match-list responses during explicit Admin retrieval; array positions never supply ordinals. Empty/upcoming-only catalogs cannot establish historical absence.

`normalize_official_dreamteam()` validates exactly eleven unique players plus one matching MVP. The request reference supplies the round ID/number/finality, because the captured answer lacks round/finality identity. The successful current-round capture is not proof of settlement. Selected roles/positions, points, formation and scoring metadata are retained.

Use the Admin **Retrieve missing round awards**, refresh, and failed-save retry controls to acquire selections. A fetched round is marked saved only after persistence acknowledgement. Full immutable revisions replace prior membership; legacy hardcoded final flags are downgraded to unknown. Notifications mark rounds for explicit recheck rather than adding money or triggering an unattended backfill.

See [coach_and_round_awards.md](coach_and_round_awards.md) for all new interfaces, controls, persistence, cache and concurrency contracts; [rival_cash_accounting.md](rival_cash_accounting.md) for historical eligibility/payment limits.

Compatibility sync helpers remain available for older direct callers; the interactive Admin path uses bounded cooperative reads and acknowledged immutable evidence writes. `test/test_mvp_runtime.R`, `test/test_admin_awards_coach.R`, and the mandatory offline lifecycle cover the changed behavior.

Best XI player cells reuse the player-table name formatter: the current real-club crest appears before the official award name, with a club tooltip and accessible image label. Player IDs map to one shared championship catalog read per render, using the existing database-backed resource adapter and persistence; there are no per-player requests. Crests load lazily. Loading or failed catalog reads keep the awards visible and show the catalog status.
