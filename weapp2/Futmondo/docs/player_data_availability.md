# Player data availability

`get_players_from_team()`, `get_market_players()`, `get_championship_players()`,
and `get_my_market_players()` tolerate null and empty optional player objects.
`normalize_player_fields()` removes empty fields from named JSON objects before
row construction. It preserves scalar zero, negative values, booleans, empty
strings, and numeric strings. Missing `value`, `change`, and `points` remain
numeric `NA` so a complete column of unknown values cannot break downstream
sorting or change calculations. Other unavailable optional fields are absent
or become `NA` when rows are combined; no missing price becomes zero.

Observation arrays retain their null entries. For example, fitness `[10, null]`
remains incomplete and cannot produce a total or recent average from only the
known score. The existing observed-average transformation is unchanged.

An empty roster `market` object, an empty `bids` array, and null market metadata
leave the player in the roster. Missing incoming offer amounts remain `NA`;
when a nested set contains an unknown amount, the best offer remains unverified.
A known outgoing bid ID with no price still makes acquisition commitments
incomplete, keeps spendable funds unavailable, and fails the existing manual
acquisition preflight.

`enrich_player_clubs()` treats the existing club lookup as optional enrichment
for roster and catalog rows. A failed lookup leaves valid player rows and their
source team names/logos intact. Successful lookup values fill missing names and
logos without replacing source metadata or multiplying rows. No club request is
needed when player rows have no `teamId`.

Null, missing, malformed, API-error, and failed roster/market/offer/listing reads
are unavailable rather than a successful empty result. An explicit empty list
still returns an empty data frame. The shared cache retains its existing stale
fallback and account/team isolation behavior. No transaction, retry, or manual
action authorization logic changed.

## Offline verification and performance review

From the application directory:

- `Rscript --vanilla test/test_player_data_availability.R`: **11 groups passed,
  0 failed**. Fixtures pass synthetic JSON through the actual HTTP response,
  roster, market, own-listing, offer, and catalog loader paths. Finance summary
  prerequisites are deterministic stubs; acquisition capacity uses the actual
  parsed roster and market rows. A known-answer fixture retains cash `-100`,
  outgoing commitments `300`, and spendable funds `4600`.
- `Rscript --vanilla test/test_player_rating_inputs.R`: **4 groups passed,
  0 failed**, including observed recent-average calculations and cache reuse.

Cold roster plus market loading uses the existing five reads: roster, clubs,
roster bids, own listings, and market. Three repeated roster/market/capacity
accesses add **0 reads**, **0 parser/normalization calls**, and no change in cache
entries or retained bytes. A roster surviving failed optional club enrichment
also serves the next access from cache without another lookup.

Normalization is bounded by the already-fetched player fields and runs only
when the existing cache loader runs. Club enrichment uses one ID match per row
and cannot increase the roster length. There are no new HTTP/database requests,
retries, caches, invalidations, background work, timers, or reactive consumers.
Rendering and mobile layout are unchanged; additional work is limited to small
in-memory transformations of the same response. No responsiveness or live
network timing measurements were performed for this localized change.

These synthetic fixtures reproduce confirmed parser/enrichment defects. No
private live response was captured, so they do not establish the exact payload
or transport failure that triggered the reported account symptoms. No live
accounts, transactions, or deployment were exercised. The primary integrated
review records application lifecycle checks separately.

## Scalar request contracts and safe failure references

Seven existing read loaders now serialize their request bodies with
`auto_unbox = TRUE`: clubs, active championships, roster, market, player catalog,
lineup, and league teams. Token, account/league/team IDs, request type, and
boolean flags are JSON scalars rather than one-element arrays. This matches the
documented API contract and existing team-information request format; it does not add fields or requests. The
exact rejection behind the hosted unavailable views remains unconfirmed until
new runtime evidence is available.

`resource_failure()` carries only an allowlisted category (`http`, `network`,
`parser`, `schema`, `read_budget`, or `unknown`), known resource/function base,
optional numeric HTTP status, and a short `FM-*` reference. Transport errors use
explicit categories; untyped R errors use conservative pattern classification.
Cache and async wrappers preserve this safe cause rather than replacing it
with a generic exception. Raw messages, URLs, query strings, credentials, IDs,
response bodies, calls, and stacks are absent from the diagnostic condition,
log line, and unavailable UI text. The original error is not retained. The legacy synchronous HTTP wrapper
still discards its original HTTP/network cause before this boundary; explicit
status/category retention applies to the asynchronous UI transport.

`resource_log_failure()` emits one bounded line per failure source/category
within an async acquisition. An adapter-local registry holds at most 32 safe
entries, matching the existing read budget, and suppresses repeated logs when
replay encounters the same optional failure. There is no global deduplication
history. Warm reads and reactive rendering do not log failures. References use
a single bounded process counter rather than private identifiers.

A transient diagnostic condition lets an outer finance/cache result retain
one safe `fetch_error` when an optional market failure is caught internally.
Stale results retain their original source observation time and safe failure;
existing spendable-funds invalidation remains in place. The factual read UI
shows a short source/connection category, reference, and refresh instruction
when no data is available. Successful and partial rows remain available.

Pressroom retains its completion summary and no longer prints per-page cursor
IDs. Its local catch preserves the safe original category/reference. Repeated
loader messages during cooperative replay do not by themselves imply repeated
HTTP requests.

Focused checks for this follow-up, using only synthetic transports:

- `Rscript --vanilla test/test_read_diagnostics.R`: **6 passed, 0 failed**.
  Covers sanitization, nested caught failures, stale and unavailable states,
  quiet warm reads/rendering, replay deduplication, and actual unavailable UI
  validation text.
- `Rscript --vanilla test/test_read_request_contracts.R`: **4 passed, 0 failed**.
  Exercises the actual sync loaders and async adapter, asserting scalar strings
  and booleans for all seven corrected request paths. Roster plus market still
  uses **5 cold reads and 0 warm reads**. A three-page pressroom fixture sends
  each cursor exactly once, uses **3 cold reads and 0 warm reads**, and prints no
  cursor IDs. A synthetic HTTP 503 retains one reference and emits one safe log.
  The existing catalog numeric-column detection emits coercion warnings for
  this small fixture; no request or assertion fails.

Performance review: no additional HTTP/database calls, retries, timers,
reactive consumers, or UI structures are introduced. Failure classification
and formatting run only on failure/state creation; log deduplication scans at
most 32 entries per acquisition. One safe diagnostic may accompany an existing
cached value, within the unchanged 32 MiB/256-entry limits. Healthy warm reads
perform no additional fetching or parsing. The short UI text uses existing
validation containers; no new browser/mobile layout check was run. Central integration checks and transport
heartbeat measurements are recorded separately by the primary review. No live
accounts, transactions, or hosted network timings were exercised here.
