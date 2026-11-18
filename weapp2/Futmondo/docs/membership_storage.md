# Lossless membership history normalization — 2026-11-18

Membership facts previously repeated player, competition, season, club, effective
date and source strings for every observation. Production inspection found about
389,000 observations of 527 distinct factual states. Observation times are useful
evidence and are retained individually, including repeated states, out-of-order
arrival and A/B/A club transitions. Other historical datasets are not changed.

## Interfaces and isolation

`membership_entities` stores player/competition/season/nullable-club identity.
`membership_states` stores effective date and source once per entity/state.
`membership_times` stores each entity, exact timestamptz observation and state ID.
Its entity/time key preserves the original uniqueness semantics, including
NULL-club identity; the composite FK prevents a state's use by another entity.

The security-invoker `player_membership_observations` view reconstructs the seven
original columns. Scoped filters, ordering and historical cutoff queries keep the
same interface. New tables use RLS and server-only grants; anon/authenticated
cannot read or write. No authentication data is stored here.

The server-only `membership_write(p_payload,p_ignore)` RPC accepts the original
row or row-array payload. Duplicate-ignore retains the existing row; merge updates
the observation's state. It validates fields/types through explicit allowed fields
and original constraints. Transaction advisory locks serialize same-identity
inserts/corrections. Errors roll back the call and remain retryable. Workers and
scheduled-clause factual writers route membership rows through this function
AFTER existing lease, account, context and field guards. Other writes are unchanged.
The R direct writer uses the RPC, preserving JSON nulls through its envelope.

## Migration and performance

The migration locks the original table, builds normalized relations, and verifies
bidirectional EXCEPT ALL equality over EVERY original field/observation before
dropping legacy storage. Any mismatch/error rolls back all changes. No timestamp
rounding, inferred dates, historical cutoffs or retention deletions are used.
Concurrent writes wait for the transaction. A database migration precedes the R
adapter rollout; worker RPC callers remain compatible through the replaced SQL
functions. Legacy direct PostgREST writes to the now-read-only view require the
updated adapter; ordinary production acquisition uses the worker RPC.

Reads use small identity/state joins and an entity/time primary index. Writes
perform indexed state lookups and keep timestamps as compact numeric references;
new source requests are not added. No new dependencies or caches are introduced.
Large scans still cost work proportional to observation count; normalization is
not a license for unbounded full-history reads. Unused states from corrections or
ignored alternative payloads are harmless small records, not discarded history.

Validation: synthetic migration reconstructs all original rows exactly, including
null clubs, repeated A/B/A states and competition isolation. Tests cover duplicate
ignore/merge, late arrival, invalid fields/times and public-role denial. The direct
HTTP adapter fixture verifies null-preserving RPC envelopes and unchanged normal
writes. All 40 worker tests, 14 competition-context groups and offline Shiny
lifecycle (13 checks/eight tabs) pass. Production measurements follow below.

## Production verification

The normalize_membership_history migration was acknowledged on 2026-11-18.
The transaction's full bidirectional equality check passed before legacy removal.
Post-migration inventory: 388,784 observations, 527 states and 527 identities;
combined normalized relations/indexes 41,066,496 bytes versus 146,407,424 bytes
before (105,340,928 bytes reclaimed). Database size measured 1,100,565,651 bytes.
Acquisition continued around the migration, so preflight row counts differ from
the locked migration snapshot; equality is established by the transaction itself.
All normalized indexes are valid/ready. A latest-100 history read for one sampled
player used identity and entity/time indexes, 38 buffer hits, no disk reads or
temporary spill, and 17.236 ms execution time. This is a focused measurement, not
a production latency percentile. At 18:50:33 UTC, latest membership observation
was 18:49:07 and automatic cleanup success 18:49:44.

This implements normalization for membership history only. Player valuation,
match and financial evidence records retain their existing representation. Total
usage remains above 500 MB; no data-loss claim or quota-resolution claim is made.
