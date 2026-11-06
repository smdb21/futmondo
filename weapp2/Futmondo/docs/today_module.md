# Today's market

Today's market summarizes observed cash and spending capacity, roster/market state, and recent transfers. It shares the same resource observations as the corresponding detailed screens.

The view contains no generated buy/sell advice, target bids, predicted points, or model scores. Missing financial evidence is shown as unavailable rather than replaced with guessed amounts. Manual actions remain in their explicit player/market controls.

`today_UI(id)` creates the namespaced screen. `today_Server()` consumes reactive login, league/team context and refresh state. Hidden views must not start unrelated collection work.

The application server passes `primary_sources` to `today_Server(..., primary_sources=NULL)`: `{team_state,market_state,retry_team,retry_market}`. The first two are reactive resource-state functions; the last two retry only the corresponding read. Today and the global round banner therefore use the exact same account/league/team data and pending request. The optional argument defaults to standalone module-owned reads for isolated consumers/tests. Global and confirmed-market refreshes invalidate the shared states; one active-session 60-second version refresh serves both primary fields and the banner. Optional transfers still wait until both primary reads settle.

Banner display finances use team information plus normalized own outgoing bids from these states, independently of full acquisition capacity. Missing held funds or market evidence leaves the projected balance unknown. These display calculations never replace the final manual-action preflight.


The optional `coach_source` argument supplies Today’s login-only default-enabled coach checkbox and status. The root owns action safety; the module owns a context-keyed localStorage preference handshake and persistence warnings. Initial primary-settled state is latched per account/league/team, so coach confirmation refreshes team/staff/balance without suspending player-table enrichment or reopening startup UI. See [coach_and_round_awards.md](coach_and_round_awards.md).

## Market presentation (2026-11-01)

The page heading is **Today's market** and its description identifies available market players/offers alongside the existing balances and completed transfers. Its market table starts with **Free Agents Only** checked. This is a table-specific initial setting; users can uncheck it or use Clear Filters to see owned listings too. The navigation label is **Today's market**. The team page appears in navigation as **My team**. No retrieval, financial calculation, action or refresh behavior changed.

## Recent transfer presentation (2026-11-01)

The latest 20 transfers show Date (`DD/MM/YYYY HH:MM`, UTC like the rival ledger), Name, Price, Buyer, Seller and Type. Names use the existing league directory with captured party names as fallback; system counterparties read Futmondo. Prices use dot thousands separators and no decimals. Historical club name/slug from captured `_playerTeam` is retained for the crest beside the player name; already loaded market metadata is a fallback. All text is HTML-escaped before the table permits generated image markup.

Transfer types reuse explicit classifications already published by the same account/league/team's rival-finance ledger. Today does not start expensive finance retrieval or subscribe to its progress updates. A system side identifies a Futmondo transfer; confirmed methods distinguish clause buyouts and ordinary rival buy/sell. The captured pressroom feed has no clause flag, so unmatched rival transfers say `Between rivals · method unconfirmed`. No method is inferred from price, current clause value or missing bids.
