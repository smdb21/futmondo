# Data loading and responsiveness

The current work is defined by the [data redesign plan](data_architecture_redesign_plan.md). The previous use of `on.exit()` or timer callbacks did not establish non-blocking network behavior: synchronous HTTP still occupies the Shiny process while it executes.

Measure data reuse, request counts, memory bounds, and delayed-request responsiveness through the shared resource layer. Cache hits do not prove that cold requests or persistence callbacks remain responsive. Executed results belong in [review results](data_review_results.md).
