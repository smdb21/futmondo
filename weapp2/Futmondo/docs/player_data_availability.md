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
