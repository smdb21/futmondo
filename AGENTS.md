# Futmondo project requirements

## Data completeness and root-cause fixes

- Treat expected application data as deterministic and retrievable or reconstructible from authoritative sources. An unexpected missing value, partial result, or unavailable message is a defect to investigate, not an acceptable finished implementation.
- Trace missing data through the request, response contract, parser, database persistence, cache, calculation and UI. Identify and fix the cause; do not stop at changing the error text, adding a generic refresh instruction, or hiding the affected feature.
- Use captured requests/responses and existing source evidence to establish the correct query and field mapping. Retrieve and persist missing historical data when the source supports it, then reuse stored data efficiently.
- For rival balances, reconcile opening cash, transfers and configured round rewards, including the available Best XI/MVP data and historical eligibility. Use one ledger for estimates, cash movements and totals; investigate discrepancies.
- Do not invent values, treat unknown data as zero, or bypass acquisition checks to remove an unavailable message. Transient failures must remain truthful and actionable while their cause is investigated. If completion depends on genuinely inaccessible evidence or an external failure, state the specific blocker and the work needed to resolve it; do not declare the underlying defect fixed.
- Verify fixes against the failing path with focused regression checks when warranted, including whether successful retrieval is stored and reused.

## Performance review

- Every modification must receive a proportionate performance-impact review to keep the app smooth. Consider request counts, repeated reads, cache reuse, database work, reactive redraws, blocking work and memory. Measure changed expensive paths when warranted and document material limitations.
