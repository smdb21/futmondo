# Offline browser fixture for the real loading markup and optional full startup.
# Rscript test/test_loading_visual_app.R [--full-app]
if ("--full-app" %in% commandArgs(trailingOnly = TRUE)) {
  for (expr in parse("test/test_progressive_browser_app.R")) {
    if (is.call(expr) && identical(expr[[1L]], quote(shiny::runApp))) break
    eval(expr, envir = .GlobalEnv)
  }
  loading_fixture_server <- browser_server
  browser_server <- function(input, output, session) {
    loading_fixture_server(input, output, session)
    shiny::observeEvent(input$fixture_startup_stage, {
      if (input$fixture_startup_stage %in% c("ready", "partial", "failed", "empty"))
        session$userData$startup_stage(input$fixture_startup_stage)
    })
  }
  # Hold each startup phase long enough to inspect its visible animation.
  loading_fixture_async <- async_adapter_fetch
  async_adapter_fetch <- function(fn, args = list(), context, owner = context$session_id,
                                  is_current = function() TRUE) {
    if (is.character(fn) && fn %in% c("get_active_championships", "get_market_players")) {
      return(promises::then(fixture_delay(NULL, 2), function(unused)
        loading_fixture_async(fn, args, context, owner, is_current)))
    }
    loading_fixture_async(fn, args, context, owner, is_current)
  }
  shiny::runApp(shiny::shinyApp(ui = browser_ui, server = browser_server),
    host = "127.0.0.1", port = 38768, launch.browser = FALSE)
} else {
  source("theme.R")
  source("loading_visual.R")
  library(shiny)
  fixture_ui <- fluidPage(
    tags$head(
      tags$style(HTML(fm_theme_css())),
      includeCSS("www/loading-visual.css"),
      includeScript("www/theme-switch.js"),
      includeScript("www/loading-visual.js"),
      tags$style(HTML("body {background:var(--fm-bg);color:var(--fm-text);margin:0;} .fixture-wrap {max-width:680px;margin:0 auto;padding:16px;} .fixture-controls {display:flex;flex-wrap:wrap;gap:8px;margin:16px 0;} .fixture-controls button {color:var(--fm-text);background:var(--fm-surface);border:1px solid var(--fm-border);padding:8px;} .fixture-controls input {color:var(--fm-text);background:var(--fm-surface);max-width:100%;} .fixture-late {margin-top:24px;}"))
    ),
    tags$main(class = "fixture-wrap",
      tags$div(id = "fixture-parent", class = "fm-loading-launch", `data-fm-loading-host` = "true",
        fm_loading_visual("fixture-hero"),
        tags$div(class = "fm-loading-brand", "LigaRadar"),
        tags$p(class = "fm-loading-caption", role = "status", `aria-live` = "polite", "Loading…")),
      tags$div(class = "fixture-controls",
        tags$button(id = "fixture-light", type = "button", onclick = "document.documentElement.dataset.fmTheme='light'", "Light"),
        tags$button(id = "fixture-dark", type = "button", onclick = "document.documentElement.dataset.fmTheme='dark'", "Dark"),
        tags$button(id = "fixture-ping", type = "button", onclick = "this.dataset.clicks=Number(this.dataset.clicks||0)+1;this.textContent='Clicked '+this.dataset.clicks", "Check response"),
        tags$input(id = "fixture-input", type = "text", `aria-label` = "Responsive input", placeholder = "Type while loading")),
      tags$div(id = "fixture-compact-parent", class = "fm-loading-status",
        fm_loading_visual("fixture-compact", compact = TRUE),
        tags$p(role = "status", `aria-live` = "polite", "Loading today's market…")),
      tags$div(class = "fixture-late", textOutput("fixture_late"))
    )
  )
  fixture_server <- function(input, output, session) {
    ready <- reactiveVal(FALSE)
    later::later(function() if (!session$isClosed()) ready(TRUE), 20)
    output$fixture_late <- renderText(if (ready()) "Synthetic request complete" else "Synthetic request pending")
  }
  shiny::runApp(shinyApp(ui = fixture_ui, server = fixture_server),
    host = "127.0.0.1", port = 38767, launch.browser = FALSE)
}
