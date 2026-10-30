# Today

Today summarizes observed cash and spending capacity, roster/market state, and recent transfers. It shares the same resource observations as the corresponding detailed screens.

The view contains no generated buy/sell advice, target bids, predicted points, or model scores. Missing financial evidence is shown as unavailable rather than replaced with guessed amounts. Manual actions remain in their explicit player/market controls.

`today_UI(id)` creates the namespaced screen. `today_Server()` consumes reactive login, league/team context and refresh state. Hidden views must not start unrelated collection work.

The application server passes `primary_sources` to `today_Server(..., primary_sources=NULL)`: `{team_state,market_state,retry_team,retry_market}`. The first two are reactive resource-state functions; the last two retry only the corresponding read. Today and the global round banner therefore use the exact same account/league/team data and pending request. The optional argument defaults to standalone module-owned reads for isolated consumers/tests. Global and confirmed-market refreshes invalidate the shared states; one active-session 60-second version refresh serves both primary fields and the banner. Optional transfers still wait until both primary reads settle.

Banner display finances use team information plus normalized own outgoing bids from these states, independently of full acquisition capacity. Missing held funds or market evidence leaves the projected balance unknown. These display calculations never replace the final manual-action preflight.
