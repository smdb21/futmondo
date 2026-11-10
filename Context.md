# Project context

## 2026-11-09 — Partial classification bonus availability

Author: Codex (primary implementation/review, Luna independent validation).

Partial standings no longer lose all completed-round bonuses when an ongoing round is selected. Points include the selected live round; bonuses use the selected completed rounds with an explicit pending-round notice. A live-only selection shows Pending, and unfinished finance acquisition shows Loading…. Unknown completed-round evidence remains unknown. Current calendar playing status takes precedence over stale completion flags.

The projection reuses the existing persisted ledger metadata, preserving payment/eligibility rules without additional API/database requests or writes. Run `Rscript test/test_classification_partial_bonuses.R` from `weapp2/Futmondo/`. The focused test covers actual accounting known answers, durable JSON reuse, incomplete evidence, stale flags, reactive range selection and read counts. Existing live and deployment checks passed. The older cold test still expects warning text previously removed from the UI and fails identically on baseline; full browser/production checks were not run.

Related files: [classification module](weapp2/Futmondo/Modules/Classification_Module.R), [regression](weapp2/Futmondo/test/test_classification_partial_bonuses.R), [behavior and performance notes](weapp2/Futmondo/docs/classification_module.md#partial-range-bonus-availability-2026-11-09).

## 2026-11-09 — General tab label

Author: Codex; no delegation. Renamed the Partial classifications tab to **General** and added “Change slider to see partial classifications.” above its slider. The internal tab value stays `partial`, preserving range selection and saved preferences. Updated existing label assertions. This text-only change adds no requests, database operations or reactive calculations. R files formatted with `styler`; rendered-UI checks passed. Full browser and statistical checks deliberately skipped for this text change. See the [classification module](weapp2/Futmondo/Modules/Classification_Module.R) and [documentation](weapp2/Futmondo/docs/classification_module.md).
