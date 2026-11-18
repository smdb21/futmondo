# Database storage headroom — 2026-11-18

Production remains intentionally stopped. Physical compaction reduced the full
Postgres cluster from **510,562,481 to 347,508,913 bytes** (347.5 decimal MB), leaving
152,491,087 bytes of headroom under 500,000,000 bytes. The application database is
332,426,387 bytes; the two system/template databases total 15,082,526 bytes.

The earlier 495,479,955-byte measurement covered only the application database.
It did not establish that the Supabase project was below quota: adding template0
and template1 yielded 510,562,481 bytes, matching the dashboard's 0.511 GB. That
quota-completion claim is superseded by this whole-cluster verification.

Use this metric, without filtering out template databases:

```sql
SELECT sum(pg_database_size(oid)) AS total_database_bytes FROM pg_database;
```

Reference: https://supabase.com/docs/guides/platform/database-size

## Maintenance and unchanged data

Preflight found zero active worker leaders and no other active client queries.
Worker chunks contained 34,186,484 bytes of live stored payloads in 171,180,032
allocated bytes; jobs/checkpoints also retained substantial space from earlier
updates and cleanup. With production stopped and the owner's authorization to
reduce storage without losing useful history, each table below received a separate
`VACUUM (FULL, ANALYZE)` operation. These preserve live rows and rebuild physical
storage; they do not delete observations or alter retention.

| Table | Rows retained | Before bytes | After bytes |
|---|---:|---:|---:|
| worker_chunks | 4,922 | 171,180,032 | 39,936,000 |
| worker_jobs | 1,889 | 16,433,152 | 2,818,048 |
| worker_checkpoints | 711 | 19,136,512 | 2,809,856 |
| worker_snapshots | 4,640 | 10,690,560 | 8,790,016 |

Before/after checks compared exact row counts and an order-independent fingerprint
of every full row: MD5 of the sorted concatenation of per-row MD5(to_jsonb(row)::text).
All four counts/fingerprints matched. No private row bodies were printed. There
were zero invalid/unready indexes afterward and zero active worker leaders.
Historical tables and all their observation values were untouched in this step.

## Column types and performance

The chunk payload column contains base64-encoded compressed data: 34,182,684 text
bytes versus 25,632,049 decoded binary bytes. A binary representation could save
about 8.6 MB before table/index effects, but would require read/write compatibility
changes. Physical compaction reclaimed about 163 MB with identical data and no
column-type change, so changing types was unnecessary for this target. No ID
ranges, timestamp precision, numerical precision or payload structure was changed.

VACUUM FULL takes an exclusive table lock and rewrites storage, so it was executed
only during the confirmed stopped-production window, sequentially. It is not
added to automatic cleanup. Normal cleanup and vacuum make freed pages reusable;
a physical rewrite returns accumulated free space to the filesystem. Actual new
history can still grow, so headroom must be monitored using the cluster total.

Application queries, API requests, caches, timers and rendering are unchanged;
smaller physical relations reduce the allocated working set. ANALYZE refreshed
planner statistics. No application runtime code/dependencies changed, so app tests
were not rerun for this maintenance/documentation task. Database row fingerprints,
counts, index validity and cluster size supplied the relevant verification. Live
worker/automatic-cleanup verification still awaits the owner's production restart.
