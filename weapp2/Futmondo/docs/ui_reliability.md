# UI data reliability

Every retained view must distinguish loading, empty, partial, stale, and unavailable data. Render actual observations and show missing evidence rather than fabricating chart points or balances.

Selected-player state belongs to the current account, championship, team, and player. Discard obsolete async results and pending confirmations after context changes. Notification player links use the same shared card path.

Manual transactions require explicit confirmation and fresh validation. Acknowledged operations invalidate affected resources; uncertain responses require reconciliation.

The countdown changes locally without per-second network retrieval. Inactive views do not trigger broad data collection. Verify the active session stays responsive while requests are delayed.

Use the offline lifecycle harness and responsive browser checks for cross-cutting changes. Actual results are recorded in [review results](data_review_results.md).
