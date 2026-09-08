# Background automation

Automation is optional and starts in observation (`shadow`) mode. The application stores an encrypted API session and explicit trading policies. A separate process schedules and validates jobs while Shiny is closed. No live action is enabled merely by creating a policy.

**Current limitation:** the shared league adapter does not yet verify the complete round deadline/solvency contract or all legal formation/club restrictions. Live execution therefore remains unavailable for these leagues, including after fourteen observation days. Lineup submission is not exposed because its write contract is unverified. Models are advisory until chronological validation has been reviewed; the UI cannot mark a model validated.

## Policy and session contracts

`automation_actions()` returns supported action strings: `bid`, `modify_bid`, `cancel_bid`, `list`, `delist`, `accept_bid`, `reject_bid`, `clause`. Normal-market bid actions require a verified system listing (`computer=true`, `type=normal`, `isClause=false`). Offer acceptance targets the best available offer at or above the floor; rejection targets an offer below it. Clause prices are refreshed and constrained by the acquisition ceiling.

`validate_automation_policy(policy, now=Sys.time())` returns `list(ok, errors)`. A policy requires:

```r
policy <- list(
  name = "Approved targets", actions = list("bid"),
  allowed_player_ids = list("player-id"), amount = 1000000,
  max_per_action = 1200000, total_spending_limit = 3000000,
  minimum_sale_price = 900000, execution_ratio = 0.95,
  model_driven = FALSE, expires_at = "2026-10-01T23:59:59Z"
)
validate_automation_policy(policy)
```

Limits are nonnegative euros. A positive amount is required for an acquisition. `amount` is the explicit bid/listing price; `minimum_sale_price` controls offer selection and listing/acceptance floors. `model_driven=TRUE` is supported only for bids and requires a user-declared executable resale/value ratio in `(0,1]`. That ratio is an assumption, not a calibrated sale-price estimate.

`total_spending_limit` is a **cumulative acquisition authorization ceiling**. Successful initial bids consume their full amount and modifications consume only verified increases; cancellation does not restore this ceiling. This conservative accounting prevents repeated bidding from recycling authorization. Pending and uncertain jobs reserve their full amount. Separately, fresh account cash/withheld/outgoing commitments limit what can actually be spent. Modify actions receive credit only for a currently verified own bid. Missing spending history blocks execution.

Policy rows add `id`, `user_id`, `championship_id`, `user_team_id`, `enabled`, `mode`, `shadow_started_at`, `model_validated`, `expires_at`, `next_run_at`, `pause_reason`, and `last_error`. Policies default to a 21-day expiry in the UI. Policy content is immutable through the UI; changed limits or targets require a new observation policy.

`automation_key(value=Sys.getenv("AUTOMATION_SESSION_KEY"))` decodes exactly 32 base64-encoded random bytes, returning raw bytes or throwing an error. Never print its return value. `encrypt_automation_session(login, expires_at, key=automation_key())` accepts a valid named login vector (`token`, `userid`, optional `user_name`) and returns a JSON-compatible envelope `{version,iv,ciphertext,mac}`. `decrypt_automation_session(envelope,key,now)` authenticates, decrypts, checks expiry, and returns the original named vector or throws. Encryption and MAC keys are derived separately; neither sessions nor keys are logged. The UI sets session expiry to seven days. Reconnection is required when that session or the upstream API token expires; no password is stored by this feature.

## Validation and execution

`validate_automation_action(job, policy_row, financial, roster, now, shadow_days, target)` returns `{ok,reason,spending_charge}`. `target` is the verified preflight response, including `previous_amount` for a modification. Financial snapshots must be available, no more than thirty seconds old, and provide `legal_bid_limit`, cash, spendable budget, roster capacity, and complete outgoing commitments. Acquisitions preserve positive cash, the API bid limit, per-action/collective limits and roster slots. An accepted sale requires a legal remaining XI using verified `financial$lineup_rules`, including activated multiposition and club/formation restrictions. Injury status is not itself a legal-registration exclusion (`exclude_unavailable=FALSE`). There is no minimum projected-points preservation rule.

`verify_automation_target(job, login, services=automation_services(), now)` returns `{ok,reason,...}`. It checks immutable player identity, current championship ownership, listing expiry, the same planned auction, matching own bid IDs for changes/cancellations, current offer IDs/prices, own listing state, and clause protection/transfer flags. Unknown API shapes fail closed. Listing/auction state must be verified even in observation mode.

`automation_shadow_days(history, policy, now)` returns the number of distinct UTC completion dates for successful, fully verified shadow checks from that policy/account. Fourteen dates alone are insufficient: fourteen days must have elapsed since `shadow_started_at`. Blocked jobs, future observations, other accounts/policies, and unverified observations do not count.

`run_automation_job(job, now=Sys.time(), services=NULL)` returns a result with `status`, `reason` or `code`, and the verified preflight details when available. It decrypts the matching account session, verifies league membership, clears only that account's cache, verifies the dynamic target and financial/roster state, loads spending history, and validates policy gates. Shadow results have `preflight_verified=TRUE` but do not invoke the execution fence or an account write. Live jobs additionally require verified deadline/solvency rules, observation history, and model validation where applicable.

A job has this minimum payload:

```r
job <- list(id="job-id", policy_id="policy-id", lease_token="claimed-token",
  user_id="account-id", championship_id="league-id", user_team_id="team-id",
  action_type="bid", payload=list(player_id="player-id", player_slug="slug",
    amount=1000000, listing_expires_at="2026-09-10T03:50:00Z", model_driven=FALSE))
# Run only with injected offline services in tests, or after an atomic worker claim:
# result <- run_automation_job(job, services=fake_services)
```

`automation_services()` supplies the account-scoped API reads, row readers, clock, cache invalidation, final fence, and execution dispatcher; `services` is injectable for deterministic tests. `read_automation_rows(table, filters)` returns a list of row objects; an unavailable database response has `attr(result,"status")="unavailable"`, distinct from an empty result.

`execute_automation_action(job, login)` dispatches exactly one existing API operation. Logical `TRUE` from legacy modify/cancel adapters is success. Logical `FALSE`, malformed acknowledgements, missing operation codes, caught clause transport errors, and thrown timeouts are **uncertain**, not proof of rejection. An explicit `api.*` rejection code is failed. No action is retried by this dispatcher.

## Worker, leases, and recovery

From the project root:

```sh
Rscript scripts/automation_worker.R --once
Rscript scripts/automation_worker.R
```

Run the persistent form under an external service manager. It evaluates due policies every five minutes, drains up to 100 jobs per pass, and waits thirty seconds between passes. Apply the reviewed SQL migration before deploying the worker; it is not applied by Shiny or by tests. Deployment/key/service configuration belongs in the root README.

`plan_automation_jobs(policy, login, now)` returns job rows for approved targets/actions. Bid-model plans use a seven-day conservative resale forecast and the win curve, bounded by declared/account/API limits. IDs derive from policy, player, action and UTC date, so each target/action has at most one scheduled attempt per day. Blocked jobs are visible and are reconsidered on a later date; they do not become silent immediate retries.

`schedule_automation_policies(now)` collects observations, plans and inserts jobs, updates next-run times, and pauses expired/mismatched sessions. Planning/storage errors are surfaced in `last_error`. `insert_automation_job(job)` uses PostgREST `resolution=ignore-duplicates` with the unique `idempotency_key`; duplicate scheduling cannot overwrite a claimed/succeeded/uncertain status. No read-then-upsert race is used.

`automation_rpc(name,payload)` is the guarded server-only RPC transport, returning decoded JSON or `NULL` on failure. Its callers are:

- `claim_automation_job()`: returns one claimed job, or `NULL`. PostgreSQL `FOR UPDATE SKIP LOCKED` plus an account-primary-key lock serializes every league/policy for that account. A random lease token identifies ownership.
- `begin_automation_execution(job,observation)`: returns a boolean after atomically verifying the same unexpired preflight lease, current enabled/live policy, account/league/team, and expiry. It stores preflight evidence and pins the account lock to infinity **before** the external request.
- `finish_automation_job(job,result)`: returns a boolean. Finalization must match both job and lock lease tokens. Confirmed terminal results release the account lock; uncertain results retain it and pause every policy for the account. Failure to save finalization retains the lock.

A two-minute preflight lease never authorizes a second trade after an unknown execution. Expired preflights and interrupted executions become uncertain; an uncertain account stays locked until conclusive reconciliation. A worker returning late cannot mutate after a failed final fence, and a stale token cannot finish another worker's job. The database functions are executable only by the server service role.

`reconcile_automation_job(job, login, services=NULL)` performs read-only confirmation and returns `succeeded` or `uncertain`. It currently confirms positive matching own bids on the same auction, or a matching owned listing/price. It does **not** infer cancellation, rejection, delisting, or transfer success from a missing bid/offer or empty roster response. Those cases remain paused until stronger event-level evidence is available; there is no automatic replay or unsafe “clear reservation” button. This is an explicit recovery limitation.

The inbox exposes the uncertain action, confirmation result, policy pause/error details, and execution reason. Successful reconciliation releases reservations but leaves policies paused until the user resumes them. `record_automation_alert(job,result)` writes an idempotent account-scoped Insights alert for failed, blocked, or uncertain execution and returns an invisible logical result; it never sends email or external messages.

`automation_UI(id)` returns responsive Shiny tags. `automation_Server(id,is_module_active,login_token,championship_id,user_team_id,refresh_trigger=NULL)` registers the session/policy/history/reconciliation handlers. All write handlers verify the current account, and policy updates also restrict the current league and team. Logout/account changes clear cached module state. Observation policies can be resumed only when no unresolved account action remains.

## Verification

Run only the focused offline checks for automation edits:

```sh
Rscript --vanilla test/test_automation_runtime.R
```

Tests cover encryption/tampering/expiry; account isolation; cash and API limits; complete commitments; modification deltas; legal XI preservation; target identity/expiry/ownership/offer changes; logical and uncertain API responses; the fourteen-day gate; shadow isolation; the final lease fence; and read-only reconciliation. Migration assertions verify lease retention, token fencing and atomic insertion contracts. Tests do not call live APIs, create background policies, write to Supabase, or apply migrations. PostgreSQL concurrency behavior still requires a staging database migration/integration check before deployment.
