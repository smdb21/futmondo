# League finances and observed transfer history

The authoritative API financial snapshot is `get_financial_snapshot(login,championship_id,user_team_id)`. Its signed cash, withheld funds, outgoing commitments and API bidding ceiling are described in [data contracts](data_contracts.md). There is no assumed 300M starting balance and no substitution of an estimated balance for zero, debt or unavailable cash.

`calculate_league_finances(login,championship_id,user_teams_df,initial_budget=NA_real_)` returns `team_finances` and `all_purchases`. Configuration comes from each league response: initial budget and point bonuses retain their configured amounts. Season standings cannot establish which per-round ranking/MVP/dream-team bonuses were paid, so reconstructed ranking income is unavailable. Budget is the explicit observed team budget or NA. Transfer-only changes are labeled estimates; they do not include unknown rewards or starting allocations.

`get_user_team_moneymovements(login,championship_id,user_team_id)` reads official financial events. Restricted access can fall back to observed pressroom transfers, retaining unknown running cash rather than inventing transaction dates or evenly spreading season points into rewards. Missing buyer/seller IDs mean Futmondo System, including sales to the computer.

`get_championship_pressroom(login,championship_id)` returns source event ID, timestamp, player, buyer/seller identities, price and nested bid evidence. Account/league-scoped cache metadata distinguishes empty, partial and unavailable history. Pagination has a bounded page limit. History completeness is not inferred from a failed page.

`evaluate_acquisition_preflight(capacity,mode,amount=NULL,existing_bid_amount=NULL)` verifies snapshot status/freshness, roster slots, pending acquisitions, spendable funds and a supplied API bidding limit. Modifications charge only the positive difference from a verified existing own bid. Incoming offers are never outgoing commitments. Worker preflight adds deadlines, policy ceilings and positive-cash checks.

`sync_pressroom_transactions_to_supabase(pressroom_df,championship_id)` upserts by source event identity. Reviewed migrations archive exact duplicates and adopt a uniquely matching legacy row; ambiguous matches retain provenance. Team histories explicitly store championship IDs, and prices/ownership/points remain league-scoped.

```r
snapshot <- get_financial_snapshot(auth, league_id, own_team_id)
capacity <- get_acquisition_capacity(auth, league_id, own_team_id, player_id)
check <- evaluate_acquisition_preflight(capacity, "bid", amount=1000000)
```

See `test_data_contracts.R`, `test_history_persistence.R`, `test_deterministic_fixes.R` and `test_ui_reliability.R` for signed/unknown balances, financial caps, rival history and source-event regression coverage.
