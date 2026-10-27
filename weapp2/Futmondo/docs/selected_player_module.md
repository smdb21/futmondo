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
