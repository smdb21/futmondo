# Multiple leagues per account

A championship is one independent fantasy game. The authenticated user's active championship list supplies the league selector; `server.R` derives the selected championship ID and that league's user-team ID from the same membership object. Both `id` and `_id` are supported. Switching is in-session and does not require another login. Data loaders, model inputs, detail selections and action checks use this context.

| Data | Identity/scope |
|---|---|
| Futmondo account/session | `user_id`; one account may belong to many championships |
| Shared player/club catalog | global immutable ID; identity/bio fields only |
| User team | immutable Futmondo team ID plus `championship_id` |
| Team/price/performance history | `championship_id` plus team/player and observation time |
| Match points | player + championship + season + scoring version + round + observed time |
| Rules/bonuses | championship + scoring version + observed time, raw configuration JSON |
| Auctions and observed bids | championship + auction ID; bids additionally bidder ID |
| Listings/relistings | championship + listing identity + observation time |
| Forecasts | account + championship + model/cutoff/scenario + season/scoring context |
| Saved transfer scenarios | account + championship + own team + name |
| Insights alerts | account + championship + event identity |
| Automation policies/jobs | account + championship + own team + policy/job identity |

The private-history migration backfills team-history league IDs from known team ownership and rejects new mismatched writes. Orphaned legacy history remains available for audit and is excluded from league-specific reads. Player positions and ratings are captured per league alongside value/ownership rather than overwriting a shared player row. Existing legacy catalog columns are retained for compatibility, not used as league observations.

Models reject ambiguous mixed-league inputs; explicit context can select one partition. Point histories also partition by season and scoring version. Different media weights, captain/multiposition settings and financial bonuses are preserved, not averaged across leagues. Missing settings are unknown. Unknown seasons are not guessed from timestamps or a boolean `started` field; verified per-league overrides are documented in README.

API caches use account-prefixed keys that include the championship/team for league data. Notifications are account-wide but filter by league. Links verify membership and bind the correct league/team. Switching leagues clears selected player/action state. Worker policies remain bound to their original league, independently of the league currently open in the browser. Account locks intentionally serialize mutations across all leagues for that account.

The service role accesses private PostgreSQL tables; browser/anonymous roles do not. Application handlers check authenticated identity and league membership rather than trusting a client-supplied league ID. `user_id` is not inferred from a rival's `user_team_id`.

See the offline lifecycle and prediction context regression tests for two leagues with overlapping real players and different balances/rules. PostgreSQL migration behavior still requires staging execution before production rollout.
