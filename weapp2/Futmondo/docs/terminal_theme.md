# Readable terminal theme

Today recommendation cards use a dark, bordered heuristic label instead of a pale pill. Action buttons have a 44px minimum touch height, consistent icon/text spacing and visible keyboard focus. The label moves below the description on narrow screens. These styles use the shared tokens and preserve existing action handlers.

The Today market radar uses `.fis-score-badge` for descriptive FIS values. It is a compact square-cornered dark badge with a fixed readable width, tabular digits and tier-coloured borders/text: green (80+), amber (65–79.9), muted green (below 65). It deliberately does not use pale rounded pills.

Implemented 2026-09-08. The existing Shiny application uses near-black backgrounds, green text, system monospace fonts, tabular numbers, amber warnings and red errors/losses. Photographs are unchanged. No external font, CRT animation, scanlines or glow is required.

`theme.R` is sourced by `data_contracts.R` before UI construction. `fm_theme_tokens()` takes no arguments and returns named CSS strings (`bg`, `surface`, `text`, `muted`, `border`, `warning`, `danger`, `font`). `fm_theme_css()` returns the corresponding `:root` CSS custom properties. `www/custom_style.css` handles responsive navigation, login, cards, inbox, tables, dropdowns, dialogs, focus and loading states.

`fm_plot_layout(p, ...)` accepts a Plotly object and ordinary layout arguments, returning a themed Plotly object; background, typography, axes, annotations and hover labels use shared tokens. `fm_reactable_theme(...)` returns a Reactable theme with shared typography, surfaces and selection colours; legacy arguments are accepted for call compatibility but the shared palette takes precedence.

```r
tags$style(HTML(fm_theme_css()))
fm_plot_layout(plotly::plot_ly(x=1:3, y=c(3,2,4), type='scatter', mode='lines'))
reactable::reactable(data.frame(points=c(0,10)), theme=fm_reactable_theme())
```

`test/test_review_completion.R` checks tokens, text contrast and widget configuration. `test/test_browser_theme.cjs` checks the synthetic offline application at 1440, 390 and 360 pixels across nine tabs. The fixture uses actual application modules and CSS, but does not establish production deployment or real-data completeness.

The stylesheet is embedded with `shiny::includeCSS()` in the production UI. This prevents a cached older `custom_style.css` response, or missing static-resource route, from leaving the page, header, login cards and inputs white. Browser tests use the production UI without a supplementary fixture stylesheet and assert dark surfaces before login. Restart the application after updating the stylesheet so new pages include its current contents.
