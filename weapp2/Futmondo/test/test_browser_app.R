# Synthetic, HTTP-blocked app used only by test_browser_theme.cjs.
expressions <- parse('test/test_application_offline.R')
for(expr in expressions) {
  if(is.call(expr) && identical(expr[[1]],as.name('app_check')) &&
     identical(expr[[2]],'full server login, navigation, refresh and logout')) break
  eval(expr,envir=.GlobalEnv)
}
# Supply a mixed squad and synthetic transaction responses for the bulk listing test.
if ("--bulk-market" %in% commandArgs(trailingOnly = TRUE)) {
  app_roster$clause_date <- format(Sys.time() + c(1, 1, rep(-1, 15)) * 86400,
    "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  get_my_market_players <- function(...) app_roster[2, , drop = FALSE]
  put_player_on_market <- function(login, championship_id, team_id, player_id, price) {
    stopifnot(identical(player_id, "p1"), price == 4050000)
    list(success = TRUE)
  }
  update_player_market_listing <- function(login, championship_id, team_id, player_id, price) {
    stopifnot(identical(player_id, "p2"), price == 4050000)
    list(success = TRUE)
  }
}
browser_ui <- source('ui.R',local=.GlobalEnv)$value
shiny::runApp(shiny::shinyApp(ui=browser_ui,server=app_server),host='127.0.0.1',port=38765,launch.browser=FALSE)
