# Historical player references

The optional internal `deferred=FALSE` argument is set to TRUE by the queue; it executes the ordered writes directly and restores the previous persistence context afterward.

`sync_pressroom_transactions_to_supabase(pressroom_df, championship_id)` now inserts historical player identities before saving transfers. The feed supplies `player_id` and optional `player_name`, alongside the existing transaction fields. Duplicate player IDs are collapsed, preferring a known name. Missing names use the observed ID; unknown slugs are empty strings to satisfy the existing NOT NULL column without inventing a Futmondo URL. No club, rating, or ownership is inferred.

Parent batches use `supabase_post_direct("players", rows, conflict="id", ignore_duplicates=TRUE)`. This optional boolean argument defaults to FALSE, preserving normal upsert behavior. TRUE sends PostgREST's `resolution=ignore-duplicates`, so concurrent or existing catalog records are never overwritten by historical metadata. The transport returns an HTTP status or NULL; only numeric 2xx parent results permit transaction writes. Missing IDs and rejected/unconfirmed parent writes return invisible FALSE and record a persistence failure. Interactive callers queue the entire operation so parents precede transfers in the session worker.

Usage: `sync_pressroom_transactions_to_supabase(pressroom_df, championship_id)` in the existing observation collector. No schema migration is required. Deploy the changed app, then use Refresh to retry the historical transactions. This repairs missing player references; missing championship or manager references remain separate errors.

Verification: `Rscript --vanilla test/test_pressroom_player_parents.R` uses a mocked HTTP transport and catalog to check ordering, metadata preservation, repeat ingestion, failed writes, and missing identities. It never contacts Supabase.
