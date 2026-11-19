# Admin worker activity

The **Admin → Worker activity** section reports the shared factual acquisition
workers and their latest sampled requests. The request table groups by request
type, method and route, with **My account** and **Other accounts** columns.
Positive counts are green and zero counts neutral. Processing/waiting jobs and
jobs without a fresh endpoint report are counted separately. Shared worker totals
are independent of account columns: one process can serve both groups.

This is operational telemetry for factual workers, not an inventory of Shiny
sessions, every operating-system process or scheduled purchase execution. It does
not launch jobs, acquire Futmondo data, alter cadence or perform transactions.

## Authorization and loading

`admin_worker_monitor.R` supplies:

- `fm_admin_worker_store(token, owner)`: checks the authenticated administrator
  and calls the server-only `worker_admin_monitor(p_account)` RPC. The current
  account comes from the authenticated token, never a browser-supplied account ID.
- `fm_admin_worker_snapshot(value)`: validates a bounded response and projects
  only safe operational fields, discarding unrecognized payload fields.
- `fm_admin_worker_monitor(session, login_token, active, store, visible)`: one
  asynchronous read at a time, every 15 seconds while an authorized admin views
  the visible Admin page. A refresh button uses the same serialization. The
  overall timeout is 20 seconds, including transport queue time.
- `fm_admin_worker_monitor_UI(state, ns)`: responsive metrics, route matrix and
  collapsed worker/job details. Read failures remain unavailable or visibly stale; they
  never become zero workers. A failed refresh removes green request highlights.

Navigation away, a hidden browser, logout, account change or session end cancels
the monitor's transport owner and clears state. Token/generation checks reject
late callbacks. The store, controller and completion callback independently check
authorization. SQL functions use `SECURITY INVOKER`; execute privileges belong
only to `service_role`. Browser `anon`/`authenticated` roles cannot read telemetry
or invoke the monitor. Database credentials remain on the R server.

## Durable activity and response contract

Apply `scripts/migrations/20261119025754_admin_worker_monitor.sql` before deploying
the updated Python worker; fresh installations include it in `scripts/schema.sql`.
The Supabase CLI generated the migration filename. No dependency or environment
variable is added. Existing `worker_guard()` remains compatible with old workers.

`worker_jobs.activity` and `activity_at` retain only the latest bounded sample per
job. `Worker.set_activity()`/`activity_scope()` track phase changes in the existing
two reusable lanes. Existing pre-dispatch and 15-second lease guards call
`worker_guard_activity(account, key, owner, generation, checkpoint, activity)`.
It performs one guarded update for the existing lease/checkpoint plus telemetry;
there is no additional worker HTTP request or sampling timer.

The activity object contains `phase`, an allowlisted `route` and HTTP `method`
(both nullable), UTC `started_at`, monotonic `sequence` and current `run_id`.
Phases are `starting`, `processing`, `source_wait`, `source_request`, `database`
and `publishing`. SQL validates exact keys, routes and scalar shapes, then stores
the owning lane. Later sequences replace earlier reports; repeated sequences
refresh a continuing phase. Older sequences cannot overwrite new activity.
Account generation, owner, job run, lease, leader, league blocking and financial
context checks remain authoritative. Lane completion/cancellation clears the
in-memory activity. One sample per retained job is bounded by the 2 KiB constraint;
no event history is accumulated.

`worker_admin_monitor()` returns:

- `checked_at`, heartbeat timeout (180 seconds) and sample interval (15 seconds).
- `summary`: live/working workers, running jobs, old heartbeat records and expired
  job leases. Old records are explicitly excluded from live counts.
- `workers`: at most 64 safe process identifiers, status, heartbeat/leader lease,
  process-lifetime request/completion counters and active-job count.
- `current` and `others`: exact active-job totals and up to 128 job rows combined,
  prioritizing the current account. Rows contain only short job/account references,
  worker/lane, handler, job/lease times and safe activity fields.
- `limits`: explicit worker/job truncation flags. Route counts cover displayed
  fresh samples; summary totals include all matching metadata.

An active job requires an unexpired job lease, a worker heartbeat within 180
seconds and current leadership. A usable activity report must match the current
owner/run. Reports older than 30 seconds do not contribute to route counts.
The sampled endpoint may have completed since its report; short database calls
can finish between samples. Some database bridges report a logical table while
the store internally reads metadata/payload endpoints. This is latest reported
activity, not a complete wire trace or a guarantee that a request remains in
flight. Missing telemetry from older workers is explicitly unknown.

No credentials, headers, query parameters, request/response bodies, raw account
IDs, user emails, descriptors, checkpoints or backend errors enter the view.
Anonymous account references are only for distinguishing rows in job details.

## Performance and checks

The monitor performs at most four small metadata RPC reads per minute per visible
admin session, plus serialized explicit refreshes. It never scans factual
payloads. A partial index on owned job leases supports active/expired metadata
lookups. Worker telemetry adds a small JSON object to existing guard calls and
one row update; process state is limited to the existing lanes. UI work is bounded
by 64 worker cards and 128 job rows, with worker and job details collapsed.

Focused checks are `test/test_admin_worker_monitor.R`,
`test/test_admin_worker_monitor.cjs`, `test/test_worker_activity.py`, and the
synthetic browser fixture/check `test/test_admin_worker_monitor_browser_app.R`
and `test/test_admin_worker_monitor_browser.cjs`. The shared lifecycle and existing
worker/admin suites remain applicable. Executed measurements and deployment
limits are recorded in [data review results](data_review_results.md).
