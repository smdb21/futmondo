# Player card

The selected-player module displays actual player identity, current values and availability, profile/match data, price history, and round points. It preserves explicit manual bid, sale, offer, and release-clause handlers with their current-state checks.

## Shared history

Chart and recent-points consumers use one shared reactive history result. Round scores are reconciled within player, championship, season, scoring version, and round context. A partial stored result must not suppress available API rounds. The current source/revision contract is implemented in the shared data layer.

Zero and negative scores are valid. A known round does not need a timestamp to be plotted by round number. Provisional/unknown finality remains visible as such; unavailable scores and participation do not become zero. Conflicting or unknown season contexts cannot silently merge with a known season.

Cumulative price-snapshot totals never become per-round scores. Price charts use actual observed values only; no synthetic trend or default historical value fills gaps.

## Interaction

Reopening the same fresh player shares cached resources. Refresh updates the card resources; changing account/league/player rejects old results and clears pending confirmations. Notification, market, roster, and catalog entry paths use the same card behavior.

`selected_player_UI(id)` creates the view and `selected_player_Server()` binds the supplied reactive player/context inputs. The optional `refresh_trigger` reactive integrates the application Refresh; the card also has a dedicated **Refresh history** button. The shared repository API and limits are documented in [data service](data_service.md).

Focused checks cover history reconciliation, date-free round scores, partial sources, notification entry, and manual transaction preflights.

## Acquisition availability (2026-11-01)

The non-owned player card now shows one **Checking availability…** status while its team checks run. A partial check identifies the missing prerequisite in plain English (roster, league limit, current cash, reserved offer funds, pending clauses or outgoing offers); it no longer exposes only `status: partial`. These messages do not relax the checks or hide missing expected data. The app's existing Refresh evicts capacity as well as its source caches, so a recently cached failed check can actually recover immediately.

Capacity is a team resource, independent of the selected player. `get_team_acquisition_capacity(login, championship_id, user_team_id)` caches the shared roster/funds/commitment calculation and a small internal market-bid projection. The existing `get_acquisition_capacity(..., target_player_id=NULL)` interface performs a local target lookup and removes the internal projection before returning. Switching between two other players preserves an already-running or completed team check; account/league/team changes still invalidate it. Action handlers independently call the existing preflight helper before opening/submitting a transaction; the passive display cannot authorize a submission.

The roster loader accepts optional `facts_only=FALSE`. Capacity requests only verified player IDs/values, avoiding club enrichment, incoming roster offers and listing-presentation requests. Full and minimal results have separate caches and share the same raw roster payload; either order can reuse the one roster request. Original observation times bound derived-cache validity to 30 seconds. A capacity snapshot inherits the earliest source expiry and an absolute maximum of 15 seconds; parsing it again does not extend the source observation. The pure preflight accepts optional `now` for deterministic expiry checks and rejects an expired snapshot even if fetching consumed its lifetime. Unknown/stale/partial amounts or roster identities remain blocked.

Pending clause reconciliation shares one latest-event scan. It parses full ISO timestamps including fractions/time zones and treats missing counterparties as System. This fixes phantom missing-counterparty rows and selecting an older same-day trade from a newest-first feed, which previously created false pending candidates and unnecessary individual summary requests. Unrelated players' malformed dates do not enter this team's scan; unverified relevant dates or summaries remain explicit defects. Saved HAR replay succeeds through the real asynchronous adapter. The screenshot's particular partial prerequisite has not been reproduced from those captures; hosted runtime verification remains necessary before claiming that live incident resolved.
