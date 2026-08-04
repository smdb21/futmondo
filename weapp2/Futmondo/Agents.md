# AI Agent Development Rules - Futmondo Insights

This document establishes critical guidelines and guardrails for all AI subagents, sidekicks, and developers making additions to the Futmondo Insights R Shiny project.

---

## 📋 1. Mandatory Documentation Rule

Every developer and AI agent making modifications or additions to the codebase **MUST** adhere to the following documentation constraints:

### A. Feature & Code Additions
* Any new R function, module, API connector, or database integration **must** be documented in a corresponding `.md` file inside the `docs/` directory.
* The documentation must outline the functional parameters, payload shapes, return types, and code usage examples.

### B. General & Configuration Additions
* Any global changes, new R package dependencies, environment variable additions, or system-level setup guides **must** be documented directly inside the root `README.md` file under appropriate sections.

---

## 🛠️ 2. Architectural Guardrails
* **No Blocking Syncs**: Always wrap Supabase or external API network writes defensively in `tryCatch()` blocks to prevent user thread blocking or parent server crashes.
* **Caching Respect**: All new API fetching handlers must hook into `get_cached_data()` to ensure sub-millisecond tab switches and protect accounts from API rate-limiting.
* **Mobile-First CSS**: Avoid adding fixed-width elements. Ensure all columns, modals, and container paddings leverage standard Bootstrap fluid structures or responsive media queries in `www/custom_style.css`.