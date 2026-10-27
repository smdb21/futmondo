# Account and league session state

The root server owns the authenticated account and selected league/team. Modules consume that shared context. Each context change starts a new resource generation so a late result cannot overwrite the current view.

Cache identity includes the dimensions affecting a resource's content or access. Private resources must never fall back to a global unscoped key. Logout cancels session-owned work and clears private state.

Player cards, official notifications, and manual confirmations use the same active context. A saved player row from a different league must not become an actionable current selection.

Cross-context fixtures use overlapping player/round IDs to verify that identical identifiers do not transfer points, cash, or action state between leagues.
