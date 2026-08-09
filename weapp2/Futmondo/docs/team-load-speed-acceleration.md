# Team Load Speed Acceleration

## Summary
Removed blocking Supabase HTTP syncs from the reactive computation path and reduced the login debounce delay to accelerate team load speed.

## Changes
- **Debounce reduction**: `debounce(1000)` -> `debounce(50)` on `login_token_RV` (line 5).
- **Deferred championship sync**: `championship_RV` now returns the championship object immediately; `sync_championship_to_supabase()` runs via `on.exit(add = TRUE)` after the reactive value is returned.
- **Deferred user teams sync**: `user_teams_RV` now returns the teams object immediately; `sync_user_teams_to_supabase()` and `log_user_team_history()` run via `on.exit(add = TRUE)` after the reactive value is returned.

## Verification
`Rscript -e "parse('server.R')"` parses successfully with no syntax errors.