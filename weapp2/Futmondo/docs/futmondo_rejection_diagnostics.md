# Futmondo rejection correlation — 2026-11-18 (Pacific)

The previous source logger recognized only api.error.general and
api.error.already_bid. Thus error=true with another code (including the captured
api.error.invalid shape in docs/har_api_reference.md) produced unclassified.
HTTP 200 only establishes transport success; these responses remain failed source
observations, never empty successful history.

source_rejection_detail now accepts bounded, dotted api machine codes containing
lowercase letters/underscores (maximum 120 characters, bounded segments). It reads
only the code/error slots on the top-level object and answer, or their direct
string representation. Free-form messages, numeric identifiers, uppercase/arbitrary
strings, oversized values and multiline content remain withheld as unclassified.
No response bodies, tokens, financial amounts, player/team names or raw IDs are
printed. If a response has error=true without a supported code, error_flag=True
makes that limitation explicit; no reason is invented.

Each worker_source rejection line now includes:

- detail: the supported exact machine code; error_flag: explicit boolean marker.
- handler: the parent worker handler, from a constrained machine-name format.
- team_ref: keyed reference scoped to account, championship and queried team.
  The same team across information/moneymovements routes has the same reference.
- request_ref: account/route/query reference, excluding the authentication header.
- job_ref / run_ref: parent job and execution references. A different job_ref with
  the same request_ref identifies overlapping callers; the same job_ref with a
  new run_ref identifies another execution of that job.
- process_count and gap_seconds: rejection count and elapsed monotonic time for
  that request in this process. First occurrence has gap_seconds=-1. These count
  failures, not successes or suppressed requests. Counters reset on restart or
  eviction; at most 512 request references are retained (oldest evicted first).
- job_attempt: queue attempt count where available, otherwise -1.
- team_context / team_argument: yes/no/missing comparisons with the parent job's
  declared team. A mismatch is evidence to investigate, not proof of a defect:
  a finance parent legitimately queries multiple rival teams.

References use HMAC-SHA256 with the existing server worker encryption key, a
purpose-specific namespace and 16 hexadecimal characters. They are stable across
restarts with that key and cannot be obtained by hashing guessed IDs without it.
Rotating the server key changes references. They are pseudonymous diagnostic
labels, not database keys; no key material or authentication tokens enter logs.

## Findings and limits

worker/helper.R creates individual information/moneymovements jobs for discovered
teams. fm_worker_finances also uses those factual resources. This establishes
multiple possible callers, but previous logs did not correlate which caller/team
was rejected. Existing request fields match the official client inspection recorded
in worker_error_diagnostics.md. That does not prove current permission or team-ID
validity. No new endpoint, retries, suppression, access bypass or empty fallback
was introduced. The rejection cause remains unresolved until fresh production
lines reveal the native code and correlated request. Successful results and history
continue through the existing durable persistence path; failures are not stored as
successful observations.

## Verification and performance

Ten diagnostics tests cover supported exact codes, free-form/ID/injection rejection,
private-field redaction, cross-account isolation, stable restart references,
same-team/different-parent correlation, mismatched team flags and the 512-entry
bound, as well as existing database/config diagnostics. Existing worker and offline
lifecycle results are recorded in the review tracker. No real source requests or
transactions are used by tests.

The change adds bounded HMAC/counter work only when a source rejection occurs.
It adds no API/DB reads, retries, timers, success log lines or UI redraws. Existing
one-line rejection logs gain fixed metadata; no tracebacks or full payloads are
added. This is diagnostic implementation, not a claimed fix of the source rejection.
