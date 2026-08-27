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

---

## 📦 3. Package Dependencies & Deployment

* **Manifest Creation**: Whenever a new R library dependency is added to the project, you **MUST** regenerate the deployment manifest file in the project root by running:
  ```R
  rsconnect::writeManifest()
  ```
  This is required to ensure that hosting services (such as shinyapps.io, RStudio Connect, or Posit Connect) correctly identify and install the required packages.

---

## 4. Domain & Transaction Rules

* **Futmondo System as Counterparty**: In Futmondo transactions (pressroom feed, market movements, transaction logs), if `buyer_team_id` / `_buyer` ID is missing, `NULL`, or empty (`""`), the buyer is the **Futmondo System / Market** (e.g., player was sold to the market/computer). Likewise, if `seller_team_id` / `_seller` ID is missing, `NULL`, or empty (`""`), the seller is the **Futmondo System / Market** (e.g., player was bought from the market/computer).
  - Agents and functions must treat missing/empty counterparty IDs as the Futmondo system, not as corrupted or missing data.
  - When displaying or reporting transactions, label an empty buyer/seller as `"Futmondo / Mercado"` or `"Futmondo (System)"`.

---

## 5. Project Scope & Legacy Isolation Rule

* **Authoritative Root**: The ONLY authoritative codebase for this project is `/home/rstudio/workspace/futmondo/weapp2/Futmondo/`.
* **Disregard Parent / Legacy Files**: Any file or directory outside `/home/rstudio/workspace/futmondo/weapp2/Futmondo/` (such as root workspace files, `../../src/`, `../../webapp/`, `../../jornada*.txt`, `../../saladeprensa*.txt`, etc.) belongs to deprecated legacy iterations, Java/R prototypes, or previous experiments. AI agents and developers must NEVER read, reference, edit, or rely on any files outside this project directory.

---

## 6. Roadmap Phase Tracking Rule

* **Tag Completed Roadmap Phases**: Every time an AI agent, developer, or subagent finishes implementing a milestone or phase from the product roadmap (`docs/v3_roadmap.md`), the agent **MUST** update `docs/v3_roadmap.md` to tag that phase as `[COMPLETED]` / `[IMPLEMENTED]` with a status checkmark, completion date, and short summary of delivered components.

---

## 7. Scoped Verification & Testing Rule

* **Focused Tests for Every Change**: Whenever a feature, module, UI component, algorithm, or server handler is added or modified, developers and AI agents **MUST** update or add an appropriate focused automated test covering the modified behavior (inside `test/`, e.g. `test/test_shiny_simulation.R` or a dedicated script for the affected component). No change may ship with zero test coverage.
* **Smallest Relevant Test by Default**: For routine changes, run only the smallest directly relevant test script(s) that exercise the modified code. Do not run the full Shiny simulation suite by default.
* **Full Suite Only When Warranted**: Running `Rscript test/test_shiny_simulation.R` (the full Shiny simulation suite) is required only for significant or cross-cutting changes, application-startup changes, shared infrastructure changes, or major UI/server flow changes, or when explicitly requested by the user.
* **Report Test Selection & Results**: Developers and AI agents **MUST** report which test script(s) were selected and their actual results (pass/fail output) as part of the task report.
* **Fix Failures Before Completion**: Any failed focused test must be fixed and re-run until it passes before the task is considered complete.