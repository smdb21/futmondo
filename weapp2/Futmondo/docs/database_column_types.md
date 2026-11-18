# Column type audit — 2026-11-18

Read-only production audit while the owner keeps production stopped. No column,
writer, retention rule or historical value was changed. Whole-cluster size at the
final read was 347,549,873 bytes (347.55 decimal MB), leaving 152.45 MB below
500,000,000 bytes. This is a point-in-time measurement, not a future growth cap.

## Findings

The public ordinary-table inventory contains 481 columns: 209 text, 100
timestamptz, 56 bigint, 40 integer, 27 boolean, 21 jsonb, 20 unconstrained numeric,
five uuid, two date, and one numeric(5,2). Views and internal schemas are excluded.
No floating-point, timezone-less timestamp, varchar or padded char columns were
found in that inventory. This does not establish every column's semantic contract.

| Candidate | Measured potential content saving | Assessment |
|---|---:|---|
| worker_chunks.data: base64 text to bytea | 8,550,635 bytes | Best candidate; requires coordinated read/write migration |
| Three history-state signature columns: hex text to bytea | 1,182,704 bytes | Lossless representation change, but all hash lookups/writers need adaptation |
| membership_times entity_id/state_id: bigint to integer | 3,115,016 bytes | Theoretical field-width saving; requires range, sequence and foreign-key review |

These are content/field-width differences, not measured post-migration relation
savings. Tuple alignment, TOAST compression, indexes and rewrite overhead affect
actual disk use. They must not be added up as guaranteed reclaimed space.

Chunks currently hold 34,182,684 bytes of base64 text, representing 25,632,049
binary bytes. worker/main.py pack_snapshot/unpack_snapshot explicitly encode and
decode base64; SQL snapshot readers aggregate c.data as JSON strings and writers
insert chunks directly. A bare ALTER COLUMN would break that contract. A future
migration must preserve the wire format or update every consumer, verify exact
round trips/checksums, and measure both storage and request CPU/payload costs.
Authentication token columns are outside this proposal.

All 73,919 inspected history signatures are valid 32-character lowercase hex:
2,365,408 text bytes versus 1,182,704 binary bytes. SQL currently generates and
compares md5 text. Binary signatures must retain full factual equality checks;
they remain lookup accelerators, never the sole identity test.

The membership reference estimate is 389,377 rows times two fields times four
bytes. Small present identifiers alone do not justify narrowing sequence-backed
keys. This audit did not establish a long-term int32 range contract.

## Types to retain

- Exact numeric averages: 14,809 avg_points and 14,806 avg_last_five values have
  fractional parts. Converting to integers loses data; floating point changes
  exactness. Arbitrary numeric precision/scale caps may round future values.
- Match points and rounds were integral in the current sample (20,026 and 14,383
  populated values), but current data alone does not prove all scoring/source
  contracts are integral. minutes_raw has no populated values and gives no
  evidence for choosing a narrower type.
- bigint monetary values: current maximum player value is 137,030,578 and maximum
  team value is 821,225,861. These fit int32 today, but this does not establish safe
  future bounds. Preserve existing exact amounts and negative values.
- Opaque source IDs: all 1,113 player IDs are 24-character hexadecimal strings,
  not UUID representations. Do not cast them to UUID or invent a new identity.
- text: replacing it with varchar(n) does not reduce ordinary value storage.
  Padded char(n) can waste space. Add length constraints only for proven contracts.
- timestamptz and jsonb: preserve observation timing and structured evidence.
  Large evidence payloads are not evidence of an incorrect column type.

Recommendation: retain current types for now. There is already approximately
152 MB of quota headroom; binary chunk storage is the first worthwhile type
migration if further reduction is requested. Neither type changes nor cleanup
can guarantee a permanent size ceiling while genuinely new history is retained.

## Verification and performance

Used live catalog/statistics metadata and aggregate value scans; no private row
payloads or credentials were printed, and no Supabase log queries were issued.
Statistics-based width estimates exclude some TOAST costs and were not treated
as physical savings. Reviewed Python snapshot serialization and SQL hash/read
contracts. No production restart or cleanup verification occurred: those still
require an active deployed worker after the owner's restart.

This task changes documentation only. Runtime request volume, caching, timers,
UI responsiveness and memory are unchanged; application tests are not required.
Any later type migration needs focused round-trip, persistence, equality and
performance checks before deployment.

References: PostgreSQL [character types](https://www.postgresql.org/docs/17/datatype-character.html),
[numeric types](https://www.postgresql.org/docs/17/datatype-numeric.html), and
Supabase [database size](https://supabase.com/docs/guides/platform/database-size).
