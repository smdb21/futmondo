# Temporary debt and next-round countdown

Futmondo permits the team balance to be negative between rounds down to 50% of the current team value. The spending allowance is calculated as `cash + 0.5 * team_value - max(withheld, outgoing_bid_commitments)`. The API-provided bid ceiling, roster capacity and per-action limits remain additional caps. Reaching the exact debt floor is allowed; crossing it is blocked before an API mutation.

This temporary borrowing rule does not make a negative balance safe at kickoff. The balance must be strictly positive when the next round begins or the team does not score points. The application therefore keeps a countdown bar at the top of every page. It shows the next round number and remaining days, hours, minutes and seconds, plus current **Balance**, **Active offers** (their held euro total and, when available, their count), **After offers** (cash less active outgoing offers), and **Can spend** (verified debt-aware headroom). Round solvency uses the projected balance after the held outgoing-offer reservation. Futmondo's `withheld` field and the observed active offers describe the same reservation when they match, so the calculation uses the larger of the two rather than subtracting either amount twice. The bar shows `After offers` as the kickoff balance; zero or debt shows the amount that must be restored before kickoff. Missing finance or schedule data is shown as unavailable rather than guessed.

`acquisition_headroom(cash,team_value,withheld=0,commitments=0,debt_fraction=0.5)` returns `spendable_budget`, `debt_limit`, `minimum_balance`, `projected_committed_balance`, and `reserved_amount`. Invalid or missing values return `NA` fields. `next_round_context(rounds,now=Sys.time())` accepts normalized `round_number` and `begin_process` columns and returns the earliest future round. `format_round_countdown(starts_at,now=Sys.time())` returns a fixed `Dd HHh MMm SSs` string.

```r
headroom <- acquisition_headroom(cash=-1000000,team_value=20000000)
stopifnot(headroom$minimum_balance == -10000000,
          headroom$spendable_budget == 9000000)
```

Run `Rscript test/test_debt_and_round_countdown.R`. The focused test covers the exact debt boundary, commitments, missing inputs, roster-value fallback, future-round selection, countdown formatting and responsive global UI contract.

When no future round start exists, `current_round_context(rounds, now)` selects the latest round whose `begin_process` has passed and whose normalized `is_finished` flag is false. The top bar then displays `Round N — In progress`. If no future or active round can be verified, it displays `Round schedule unavailable`.
