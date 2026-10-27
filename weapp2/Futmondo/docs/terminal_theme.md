# Terminal theme

The application shares dark backgrounds and green/neutral text tokens through `theme.R` and `www/custom_style.css`. Tables, plots, inputs, notifications, and manual-action controls should use those tokens.

Maintain readable contrast, visible keyboard focus, touch-friendly actions, and responsive layouts at desktop and narrow mobile widths. Hidden or removed recommendation widgets must not dictate new layout behavior.

The isolated browser fixture checks retained tabs and responsive overflow without accessing external services. Actual screenshots/check results belong in [review results](data_review_results.md).
