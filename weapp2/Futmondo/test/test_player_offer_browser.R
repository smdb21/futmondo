options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_clause_enabled <- function() FALSE
get_sb_url <- function() ''
get_sb_key <- function() ''
record_acquired_resource <- function(...) invisible(TRUE)
log_market_transaction <- function(...) invisible(TRUE)
invalidate_manual_resources <- function(...) invisible(NULL)
fm_worker_refresh_league <- function(...) invisible(FALSE)
requests <- 0L
async_adapter_fetch <- function(fn, ...) {
  stopifnot(!fn %in% c('get_team_acquisition_capacity', 'get_scheduled_clause_check'))
  promises::promise(function(resolve, reject) NULL)
}
fm_async_http <- function(url, method, body, ...) {
  stopifnot(method == 'POST', url %in% c(BID_URL, ROSTER_CLAUSE_URL))
  requests <<- requests + 1L
  price <- jsonlite::fromJSON(body)$query$price
  promises::promise(function(resolve, reject) later::later(function() {
    code <- if (price == 999) 'api.market.max_number_players_in_roster' else API_CODE_OK
    resolve(list(status_code = 200L,
      content = charToRaw(jsonlite::toJSON(list(answer = list(code = code)), auto_unbox = TRUE))))
  }, 1))
}
row <- data.frame(id = 'player', name = 'Fixture Player', slug = 'fixture', role = 'defensa', team = 'Club',
  value = 200, price = 150, change = 0, points = 10, user_team_id = '', market_inMarket = TRUE)
rival <- observed_data(transform(row, id = 'rival-player', user_team_id = 'rival', userTeam = 'Rival',
  market_inMarket = FALSE, price = NA_real_, clause_price = 400, clause_transferred = FALSE,
  clause_date = '2026-01-01T00:00:00Z'))
capacity <- function(...) list(status = 'ok', valid_until = as.numeric(Sys.time()) + 15,
  roster = list(status = 'ok', count = 10, effective_count = 10, cap = 20),
  funds = list(spendable_budget = 1000), outstanding = list(count = 0))
ui <- fluidPage(tags$head(tags$style(HTML(fm_theme_css())), includeCSS('www/custom_style.css')),
  actionButton('open_market', 'Open market player'), actionButton('open_rival', 'Open rival player'),
  tags$script(HTML("$(document).on('hidden.bs.modal', '#shiny-modal', function(){Shiny.setInputValue('fixture_closed', Math.random(), {priority:'event'});});")))
server <- function(input, output, session) {
  selected <- reactiveVal(NULL)
  selected_player_Server('card', selected, reactive(c(userid = 'browser-offer', token = 'synthetic')),
    reactive('league'), reactive('mine'), capacity_fetcher = capacity)
  open <- function(player) {
    showModal(modalDialog(selected_player_UI('card', player, 'mine'), size = 'l'))
    session$onFlushed(function() selected(player), once = TRUE)
  }
  observeEvent(input$fixture_closed, selected(NULL))
  observeEvent(input$open_market, open(row))
  observeEvent(input$open_rival, open(rival))

}
shiny::runApp(shiny::shinyApp(ui, server), host = '127.0.0.1', port = 3895, launch.browser = FALSE)
