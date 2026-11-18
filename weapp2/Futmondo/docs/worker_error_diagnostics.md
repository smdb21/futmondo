# Worker restart investigation — 2026-11-18

## Evidence and remaining limits

The supplied container log covers 18:22–18:52 UTC, including successful bounded
cleanup, Futmondo money-movement rejections, a database 500 at 18:49:21, and a
container shutdown. The shutdown cause is not present. These timestamps predate
the final history cutovers (price 21:36, match 22:37, finance 22:52 UTC); they must
not be treated as a fresh verification of the final schema/deployment.

Read-only database inspection at 23:36 UTC found no active leader, a last job start
of 21:16:53, last successful cleanup of 18:55:07, no current lock waiters, and a
published base interval of 1800 seconds. The full cluster was 347,656,369 bytes.
There is no evidence yet that a worker with the owner's intended 45-minute setting
has successfully published its policy. Python reads the runtime environment,
not R's .Renviron. The existing s6 launcher uses with-contenv and does not strip
this setting. A synthetic worker with value 45 publishes 2700 seconds correctly.

Narrow Supabase log queries (18:48:50–18:49:30 and 20:30:30–20:33 UTC, with narrower
follow-ups inside the first window) returned only aggregate metadata, endpoint
names, SQLSTATEs and extracted function/relation names. No raw SQL, request filters,
credentials or private payloads were emitted. The 18:49 error was HTTP 500 from
worker_write_fact with PostgreSQL SQLSTATE 57014 (query cancellation), involving
an INSERT into player_membership_observations. Membership migration was recorded
at 18:48:17. This overlap is consistent with migration-time interruption/locking,
but does not by itself prove the cancellation's cause. Current worker_write_fact
routes membership and all three normalized histories to their dedicated writers.
The later window also contained 57014 errors in worker_claim, worker_read,
worker_checkpoint and worker_cleanup_batch; historical errors alone do not prove
these still fail after the final migration and compaction. No automatic retry or
statement-timeout settings were changed on this evidence.

Nine money-movement jobs have source_rejected and no successful checked_at; nine
team-information jobs also have source_rejected. Context/argument team IDs match
for those jobs. The worker intentionally does not checkpoint application errors,
so last_error cannot reconstruct the original rejection body/code. Source errors
remain distinct from empty successful history. No authenticated live source probe
was possible in this checkout: the worker encryption key/runtime credentials are
not available here and no deployed leader is active. Deployment diagnostics are
needed for fresh evidence; the source failures are not declared fixed.

## Official client contract

Inspected https://app.futmondo.com/main.dart.js on 2026-11-18, SHA-256
08202076259d1678c7557478771d3aa38b5099f42044630569c704fdb7a7cd1c.
The official mx.axr money-movement call uses /1/userteam/moneymovements with
championshipId and userteamId; Qr uses /1/userteam/information with those fields
and type=market. The shared aE/crH helper sends the header token/userid, query and
answer envelope and unwraps response.answer. It treats api.error.general as an
error/null. The money-movement parser aOh reads _id, concept, type, category,
money and date. Existing R adapters match these routes/query fields and existing
captured HAR evidence. Public code proves the request contract, not permission
to read another team's finances or the correctness of a particular live ID.
No endpoint or parser change was justified by this inspection.

## Added diagnostics

- worker_config once per process run: effective base_interval_minutes,
  interval_source=environment/default, lane count.
- worker_schedule after successful maintenance on leadership/admission changes:
  leader, admitted, and the process's configured interval. A leader=true line
  establishes acknowledgement of the policy RPC, unlike startup alone.
- worker_database at the HTTP boundary, including failures passed through the R
  helper: fixed allowlisted endpoint, method, HTTP status, SQLSTATE/PostgREST code
  or enumerated transport category. Unknown endpoints/codes are redacted. No SQL
  messages, hints, details, query strings, tokens or row contents are logged.
  Identical tuples are limited to one line per 60 seconds per process; the
  suppression map holds at most 128 entries.
- worker_source for HTTP-200 application rejections: allowlisted source route,
  status and known public code (api.error.general / api.error.already_bid), or
  unclassified. Arbitrary error strings/bodies are never logged. Existing worker_job
  lines retain handler/lane and retry behavior; errors still remain retryable.

After redeployment, capture worker_config, worker_schedule, worker_source,
worker_database and adjacent worker_job lines. A correct custom startup reads
base_interval_minutes=45 interval_source=environment; successful elected
maintenance reads leader=True and base_interval_minutes=45. An unclassified
source rejection requires further targeted diagnostics, not invented data.

## Verification and performance

Eight synthetic diagnostics tests pass: redaction, unchanged failure codes,
duplicate-log suppression, no extra successful-request calls, custom/default
configuration, acknowledged 2700-second maintenance safe rejection codes, and HTTP-402 quota restriction classification.
All 40 existing Python worker tests pass. The modified native-rejection test was
also rerun individually after adding its log assertions and passed. A module-style
unittest invocation initially failed because test is not an importable package;
the file-based invocation passed. Offline Shiny lifecycle passed all 13 checks and eight tabs.

No extra HTTP/database/source requests, retries, timers, durable data, dependencies
or UI redraws are introduced. Success paths add no database log line; startup and
leadership logs are infrequent. Failure parsing uses already-received responses;
logging/suppression storage is bounded. Historical data, cache persistence, failure
acknowledgement and account isolation are unchanged. Runtime cadence/egress and
new failure reproduction still require the deployed worker.

Supabase health advisors returned no findings. Existing security/performance
advisories include the separate futmondo_players table (GraphQL access and duplicate
indexes), rls_auto_enable execution grants, server-only RLS tables with no policies,
and unindexed foreign keys. They do not establish the cause of these errors and
were not changed in this diagnostic task. References:
[GraphQL exposure](https://supabase.com/docs/guides/database/database-linter?lint=0026_pg_graphql_anon_table_exposed),
[function grants](https://supabase.com/docs/guides/database/database-linter?lint=0028_anon_security_definer_function_executable),
[duplicate indexes](https://supabase.com/docs/guides/database/database-linter?lint=0009_duplicate_index).

## Quota restriction check

The owner specifically raised quota restrictions. An aggregate of edge responses
for the supplied 18:22–18:52:15 UTC log window found 8,983 HTTP 200, 1,215 HTTP 201,
39 HTTP 204 and five HTTP 500 responses, with zero HTTP 402. This is project-wide
traffic (including maintenance), not an app-only request-rate measurement. It shows
API reads/writes were being served during that window, not a blanket 402 block.
It does not establish the present API gateway restriction state. Supabase may also
pause projects or make databases read-only; a successful management SQL read alone
is not proof that application writes are permitted. See the official
[billing FAQ](https://supabase.com/docs/guides/platform/billing-faq#how-is-the-fair-use-policy-applied).
The new diagnostics preserve database_402 distinctly from database_500 and expose
SQLSTATE 25006 if a write fails due to a read-only transaction. No quota restrictions
are bypassed. Fresh deployed-worker acknowledgement remains necessary.

Current SQL inspection also found transaction_read_only=off, default_transaction_read_only=off and no recovery mode. The only configured read-only override belongs to the explicitly read-only supabase_read_only_user role. This does not test the application API gateway or perform a production write.
