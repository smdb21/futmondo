# Finance worker progress and resolved context

The initial worker integration made finance output depend on the entire historical scan and every prepared child resource completing. It discarded the accounting controller's existing intermediate publications. Production also retained an unresolved current-finance job alongside the resolved job for the same account/league/team; both lanes acquired/replayed the same source history. The user saw an indefinite-looking spinner despite many acknowledged source responses. An old run later failed with generic `resource_parse`; that old diagnostic does not establish its exact exception.

## Context ownership and durable evidence

`worker_finance_context_matches(descriptor, membership)` checks only the current `fm_worker_finances` pipeline against source-verified account membership: competition and season must be known and match; known scoring must match; the reference team must be explicit. Other factual/historical resource handlers keep their existing contracts. Admission, claim and lease continuation use this pure JSON predicate. `fm_worker_finance_source()` waits for resolved competition/season before requesting a finance job.

Apply `scripts/migrations/20261112035000_resolved_finance_jobs.sql` and then `scripts/migrations/20261112035500_finance_progress_publication.sql` before deploying the matching R/Python changes. It pauses ineligible current-finance jobs (`due_at=infinity`, `context_unresolved`, no lease) without deleting or rewriting their observations, snapshots or checkpoints. Unknown archives are never aliased to the current known season/scoring scope. Existing blocked-sport and account/league/generation/leader guards remain. The new predicate is SECURITY INVOKER and executable only by the server role.

Production migration is recorded as `20261112035051_resolved_finance_jobs` (source SHA-256 `9ad41b439fcf6368db5d73e4b2c10b8a784c5c713922363013211966d16fff25`). Verification found the unresolved duplicate inactive with all 131 response checkpoints retained; the resolved job continued. This is recovery of existing opted-in factual acquisition, without transactions, new consent, authentication changes or data deletion.

## Incremental finance publication

The direct accounting controller `shared_rival_finance_source(..., refresh=NULL, on_update=NULL)` accepts an optional callback receiving the same factual calculation value it publishes internally. Ordinary UI callers leave it NULL. Only the private headless R helper installs the persistence callback; encoding and database waits stay outside Shiny's serving process.

`fm_worker_headless_finances(env, auth, context, on_update=NULL)` forwards the callback. The actual helper sender emits a progress request only when the existing accounting result contains at least one finite estimated balance and no calculation error. It copies the controller value, preserves its ledger/confidence/missing-data/source times, and marks it loading while the remaining scan or resource saves continue. Unknown values remain unknown; no alternate accounting formula or zero-filling is introduced.

The private JSON pipe adds:

```json
{"type":"progress","result":{"value":"<existing typed codec node>","observed_at":"<source UTC>","checked_at":"<UTC>","status":"partial","completeness":"partial","ready":false}}
```

The actual `value` is a typed object, not a string. Python restricts this message to finance jobs, forces partial status/completeness, and uses the existing `worker_publish_child` RPC with the exact parent descriptor/key. Chunks and manifest must be acknowledged before returning `{ok:true,published:true}`. This updates the visible durable finance resource without settling the parent job or releasing its lease. Its context and source age remain intact, and existing UI polling reads only the changed version. Failed writes remain retryable; they do not count as a successful preview. Cached valid snapshots continue to be reused across sessions/restarts.

Both sides cap ordinary publications to once per 15 seconds and suppress unchanged helper output. One prepared (`ready=true`) preview may bypass that interval so users need not wait for all child resources to persist. It still remains partial and loading: `ready` is a controller milestone, not proof of full historical coverage or job completion. Only final parent publication settles the job. The per-lane cadence/ready markers reset for the next job. Stale previews do not automatically requeue an already-running resource; explicit manual Refresh still records a subsequent run.

## Saved award reuse and finality

The helper reuses scoped persisted Best XI/MVP selections only when all 11 identities are valid, finality is explicit, persistence is acknowledged, available season/scoring fields match the current context, checks are less than one day old, and no later settlement requires rechecking. Missing, malformed, foreign, provisional, unknown-finality, expired or settlement-flagged selections still acquire and persist source evidence. A source-confirmed finished round is passed to the existing official-selection parser as finished, rather than dropping that catalog evidence at the helper boundary. This retains the existing finality/settlement accounting contracts and daily correction audits.

`scripts/migrations/20261112035500_finance_progress_publication.sql` adds a nullable publication `run_id` to snapshot metadata and preserves the existing guarded publisher signatures. A current finance aggregate can replace its own loading preview when discovering older input moves its conservative source time backward; source dates are not renewed to force ordering. Independent child observations still retain the newer-source guard across different runs. This avoids a final result being discarded while its job incorrectly appears finished. Production recorded `20261112041332_finance_progress_publication`, source SHA-256 `0555cd85f1b84622188a6d7758f6c8d0c0620ac63fdb014a690f23f7ab0e954e`.

The existing ten-minute controller wait now reports the fixed safe `finance_timeout` code when that actual condition occurs. Arbitrary exception messages remain private. This diagnostic is not used as a substitute for acquisition or correctness checks.

## Verification and performance

Tests cover the actual accounting controller and helper progress envelopes, the known ledger 100 minus a Market purchase of 10 equals 90, early usable output, award reuse/rechecks, private protocol acknowledgement, forced partial metadata, cadence plus one ready preview, failed-write retry, SQL lease preservation/chunk checks, context retirement without losing archives, and explicit refresh behavior. Existing lifecycle and persistence/security tests also run; actual results are recorded in `data_review_results.md`.

No source HTTP budget, worker count, timer, unattended activity or transaction scope is expanded. Context matching uses the account row already joined/read by the queue functions. Removing duplicate acquisitions and reusing saved final awards reduces source/database work. Incremental publication adds bounded chunk/manifest writes (up to four ordinary previews per minute plus one ready preview per job); each retains existing row/byte limits and cleanup rules. Unreferenced preview chunks use existing cleanup. Encoding/comparison runs only at the cadence gate in the private helper; ordinary serving-process callbacks add only the NULL-function check. The 5,001-entry ledger restore measured 0.190 seconds locally. Hosted CPU, checkpoint volume and preview storage throughput still require measurement; no VM sizing or full balance reconciliation is inferred from synthetic checks.

Dokploy auto-deploys pushes to the application branch, as confirmed by the owner. The database gate is verified live; progress publication and final finance completion require acceptance after the new image/helper starts. A successful preview never certifies complete historical accounting, and source gaps remain labeled by the existing engine.

## Cold-helper database lookup follow-up

A helper restart previously lost process-local league metadata before its first history lookup, even though its admitted descriptor already carried a verified competition/season/scoring identity. Private helper adapters now bind missing identity defaults to that exact account/league descriptor. Explicit unknown archive arguments and foreign account/league requests are unchanged. No observed metadata row, timestamp, expiry or completeness is manufactured. Cached saved-award catalogs whose returned season contradicts the requested scope are reacquired.

The private database bridge also now returns an empty data frame for a genuine successful empty row array. Previously JSON `[]` became an R list; paginated history code expects a data frame and could fail before saved evidence was reused. Missing/null/malformed row envelopes still fail rather than becoming fabricated empty data. Nonempty responses must decode to frames. This follow-up changes no database schema, source request budget, dependencies or serving-process code; it avoids erroneous unknown-scope lookups, parser failures and unnecessary reacquisition. The cold-helper regression exercises actual archive query construction, explicit unknown/foreign isolation, absence of fabricated metadata, wrong-season cache rejection and malformed-response failure. Finance/controller and Python bridge regressions also pass.

Production acceptance observed an actual durable partial finance snapshot at `2026-11-12 04:30:36 UTC` after deployment of the progress change. This confirms publication of usable calculations (the sender rejects directory-only/no-finite-balance output); it does not certify final historical completion or every balance's reconciliation. The cold-start follow-up requires its own automatic image deployment.
