# Terminal theme

The header's **Theme** selector offers **System**, **Light**, and **Dark**. System is the default and follows live device color-scheme changes. The choice is saved in this browser's local storage, survives application restarts and redeployments, and synchronizes across tabs. If browser storage is blocked, selection still works for the current page. Preferences are device-specific.

The application shares light and dark color tokens through `theme.R` and `www/custom_style.css`. Tables, plots, inputs, notifications, loading overlays, and manual-action controls use those tokens. `www/theme-switch.js` resolves the preference before the body paints. Reactable uses CSS variables; Plotly receives resolved colors on initial rendering and theme changes, preserving data and zoom. Keep sports pitch and position/metric badge colors meaningful in both themes.

Theme changes run entirely in the browser: no API requests, database writes, server reactive invalidation, or data reload. Existing plots receive one layout update per effective palette change; no polling or DOM observer is needed.

Maintain readable contrast, visible keyboard focus, touch-friendly actions, and responsive layouts at desktop and narrow mobile widths. Hidden or removed recommendation widgets must not dictate new layout behavior.

The isolated browser fixture checks retained tabs and responsive overflow without accessing external services. Actual screenshots/check results belong in [review results](data_review_results.md).

Run `Rscript test/test_theme_switch_app.R`, then `node test/test_theme_switch.cjs` for the focused desktop/mobile browser regression (synthetic account and offline data). It covers all three modes, system changes, explicit overrides, reload persistence, cross-tab synchronization, table/plot colors, preserved chart data/zoom, responsive controls, and blocked-storage fallback.
