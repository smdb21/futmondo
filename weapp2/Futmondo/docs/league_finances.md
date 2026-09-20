# League finances and observed transfer history

The authoritative API financial snapshot is `get_financial_snapshot(login,championship_id,user_team_id)`. Its signed cash, withheld funds, outgoing commitments and API bidding ceiling are described in [data contracts](data_contracts.md). There is no assumed 300M starting balance and no substitution of an estimated balance for zero, debt or unavailable cash.

## MVP contributions

`calculate_mvp_bonuses(official, championship_id, eligibility=data.frame(), rewards=data.frame(), payments=data.frame(), coverage_complete=FALSE)` is a pure attribution helper in `mvp_runtime.R`. Official records contain `championship_id`, `round_id`, `player_id`, `is_mvp`. Evidence frames carry the same championship/round identity:

- Eligibility: `player_id`, `team_id`, `eligible`, `verified`. These flags require historical recipient and applicable lineup eligibility evidence; today's roster is never evidence.
- Rewards: `amount`, `historical`. A false historical flag explicitly labels a current-configuration assumption. Only a historical zero proves a disabled award.
- Payments: `player_id`, `team_id`, `amount`, `bonus_type="mvp"`. Explicit payments supersede inferred amounts. Duplicate event evidence is counted once; contradictory payments remain unresolved.

Returns `{rows, complete, reason}`. Each row includes event identities, recipient `team_id`, `amount`, `status` (`recorded`, `estimated`, `disabled`, `unresolved`), `source`, and `reason`. Missing amounts are `NA`, not zero. Championship filtering prevents cross-league attribution.

`get_mvp_payment_evidence(login, championship_id, team_ids)` reads cached team movements and accepts only explicit `bonus_type`, `round_id`, `player_id`, and `money` fields. Generic `bonus` categories or free-text descriptions cannot establish an MVP event. The currently documented movement contract does not guarantee these identity fields; when absent, attribution stays unresolved. Historical lineup eligibility and per-round reward contracts are likewise not established by the available API evidence, so the live adapter does not fabricate them. The pure helper supports verified evidence when supplied.

`add_mvp_finance_estimates(finances, attribution)` returns the original team-finance frame extended with `mvp_recorded`, `mvp_estimated`, `mvp_unresolved`, `mvp_status`, and `known_funds_subtotal`. The subtotal is initial budget minus purchases plus sales, point income and attributable MVP income; it is **not verified cash** and may omit other income/history. Original `budget` values are unchanged. Rivals displays both known income subtotals and the reasons for incomplete coverage, recomputing when the shared MVP source or Refresh changes.

```r
awards <- calculate_mvp_bonuses(official, "league-1", payments=payments,
  coverage_complete=TRUE)
display <- add_mvp_finance_estimates(team_finances, awards)
```

Verification: `Rscript test/test_mvp_runtime.R` covers changed owners, reward assumptions, disabled bonuses, duplicate payments, league isolation and unchanged authoritative cash.

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
