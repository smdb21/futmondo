# Configurable acquisition cadence (2026-11-18)

`FUTMONDO_WORKER_BASE_INTERVAL_MINUTES` is owned by the Python worker, defaults
to 30 for missing/empty values, and accepts positive integer minutes up to
35791394. Invalid values fail startup. Restart the worker after changes.

The elected worker publishes policy through `worker_maintenance(owner,
base_interval,requests,jobs)`. The server-only policy table and each job's
`native_interval_seconds` retain the distinction between resource cadence and
the effective minimum. Existing successful idle jobs are deterministically spread
across their effective interval on migration/configuration changes. Running
leases, checkpoints, failure retry times and snapshots are preserved. Discovery
uses native intervals to avoid repeatedly rewriting floored jobs. Publication
no longer prefetches early based on source validity. Source timestamps and stale
states remain truthful; source validity is not extended by this setting.

Successful `worker_connect` calls invoke an account-locked five-minute coalesced
login refresh of current resources (native interval at most five minutes,
excluding player summaries/profiles/matches). Hourly/daily histories are not
requeued. Running jobs and failed jobs retain their existing progress/backoff.
The guarded claim path continues to reject blocked or excluded scopes. New
resources remain immediately discoverable under the same interval floor.

Maintenance is one RPC every 60 seconds, renewing leadership for 180 seconds.
Active-job guards remain 15 seconds. Idle factual lanes check every 30 seconds.
Visible sessions check durable versions every 15 seconds, coalesced across
sessions for 14 seconds. Scheduled purchase dispatch is unchanged.

Performance: maintenance falls from 17,280 to 1,440 requests/day per instance;
idle queue polling falls sixfold. One-minute collection jobs have 30-fold fewer
scheduled runs at the default, excluding login, retries and initial discovery.
These are code-derived rates, not production measurements. No new timers,
source requests from browsing, or unbounded memory caches are added.

Validation: synthetic SQL covers default flooring, configuration increases and
decreases, login refresh and coalescing. Python configuration checks cover
missing/empty/custom/invalid values. The existing 40 worker checks and offline
Shiny lifecycle (13 checks plus eight tabs) pass. Production health reads
succeeded but aggregated job inventory timed out; end-to-end refresh latency,
production workload reduction and the Round awards failure remain unverified.

Production migration attempt failed before acknowledgement while initializing
Supabase migration history: connection timeout. The application branch auto-deploys
on push, so push/deployment is held until the required RPC migration succeeds.

Update 2026-11-18: after explicitly authorized chunk compaction, the cadence and
bounded-cleanup migrations succeeded. The earlier migration blocker is resolved;
see worker_storage_cleanup.md for measurements and deployment limits.

## Split matchdays

`award_calendar_gaps()` records a long break only for an official round with complete fixture identities, kickoff times and statuses. At least one fixture must be final. The earliest remaining kickoff must be at least seven days after the last finished fixture's kickoff. Worker standings, Dream Team display and team-lineup jobs switch to daily cadence starting 24 hours after that earlier kickoff and return to their live cadence 24 hours before the next kickoff. This leaves time to observe the first phase's published scores and payments. The ordinary job floor still applies.

The existing private numbered-calendar observation stores each proven interval under `split_gaps[round_id]` as epoch-second `slow_from` and `resume_at` values. A newer calendar replaces the interval when dates move. `worker_enqueue_batch()` checks the latest stored calendar when one of the three round job types would switch cadence. It checks the official round ID, account, championship, source competition and season through the existing indexed evidence lookup. Ordinary rediscovery keeps the same job keys and performs no additional calendar lookup when intervals agree. A running lease or pending retry retains its due time.
