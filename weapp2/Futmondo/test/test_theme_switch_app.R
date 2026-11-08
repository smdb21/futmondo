# Reuse the offline application fixture with deterministic browser preferences.
for (expr in parse('test/test_browser_app.R')) {
  if (is.call(expr) && identical(expr[[1L]], quote(shiny::runApp))) break
  eval(expr, envir = .GlobalEnv)
}
Sys.setenv(admin = "offline@example.invalid") # Verify placement below the conditional Admin item.
# No configured database is needed to exercise a browser-local theme preference.
fm_page_store <- function(...) promises::promise_resolve(NULL)
shiny::runApp(shiny::shinyApp(ui = browser_ui, server = app_server),
  host = '127.0.0.1', port = 38766, launch.browser = FALSE)
