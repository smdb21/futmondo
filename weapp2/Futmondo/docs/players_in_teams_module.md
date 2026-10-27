# Your Team

Your Team displays the actual roster, observed player details, and explicit listing controls. Player selection opens the shared player card. Bulk listing remains a deliberate manual operation and must validate the current account, league, team, and market state.

The module no longer contains lineup forecasts, optimized XI selection, transfer simulations, generated trading advice, or automatic coach preferences.

`players_in_teams_UI(id)` creates the view. `players_in_teams_Server()` receives the current reactive authentication/league/team context and returns selected-player state. Context changes discard pending selection/action state. Shared resource invalidation refreshes affected data after acknowledged transactions.
