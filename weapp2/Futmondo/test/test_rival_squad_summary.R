# Real Rivals selection and player table with synthetic, deferred roster reads.
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(source("global.R"))
for (method in c("GET", "POST", "PUT", "PATCH", "DELETE")) {
  suppressMessages(trace(method, where = asNamespace("httr"), print = FALSE,
    tracer = quote(stop("Unexpected HTTP in rival squad regression"))))
}
auth <- c(userid = "rival-roster-fixture", token = "synthetic")
teams <- data.frame(teamid = c("own", "mixed", "partial", "empty"), teamname = LETTERS[1:4],
  points = 1:4, teamValue = c(1e6, 24e6, 7e6, NA_real_))
finances <- build_rival_cash_dataset(teams, "league", rules = list(initial_budget = 10e6),
  account_id = auth[["userid"]])
bind_factual_reads <- function(...) NULL
shared_rival_finance_source <- function(...) reactive(finances)
shared_mvp_source <- function(...) reactive(list())
get_user_team_info <- function(...) list()
get_championship_pressroom <- get_league_standings_history <- function(...) data.frame()
get_financial_snapshot <- function(...) list()
calculate_league_finances <- function(...) finances
recent_player_points_source <- function(...) list(history = function() data.frame(), status = function() NULL)
selected_player_Server <- function(...) NULL

reads <- character()
publish <- list()
async_adapter_fetch <- function(fn, args, context, ...) {
  stopifnot(identical(fn, "get_players_from_team"), context$user_team_id == args$user_team_id)
  id <- args$user_team_id
  reads <<- c(reads, id)
  promises::promise(function(resolve, reject) publish[[id]] <<- resolve)
}
settle <- function(session) for (i in seq_len(8L)) { later::run_now(0); session$flushReact() }
roster <- data.frame(id = paste0("p", 1:4), name = LETTERS[1:4], role = "defensa",
  points = 1:4, value = 5e6, change = c(40000, 20000, -90000, 0))
clear_api_cache()
shiny::testServer(rivals_Server, args = list(is_module_active = function() TRUE,
  login_token = function() auth, championship_id = function() "league", user_team_id = function() "own",
  user_teams_RV = function() teams), {
  select <- function(id) session$setInputs(rival_team_selection = list(id = id,
    context = list(user = unname(auth["userid"]), championship = "league", team = "own")))
  settle(session)
  select("mixed")
  session$setInputs(rival_tabs = "history")
  settle(session)
  stopifnot(!length(reads))
  html <- output$scouted_rival_details_ui$html
  stopifnot(regexpr("rival_team_value_box", html) > 0,
    regexpr("rival_team_value_box", html) < regexpr("rival_squad_table-players_table", html),
    !grepl("player-filter-disclosure", html, fixed = TRUE))
  session$setInputs(rival_tabs = "roster")
  settle(session)
  summary <- rival_team_value_display()
  stopifnot(identical(reads, "mixed"), summary$value == 24e6, summary$trend_status == "loading",
    is.na(summary$up), is.na(summary$down))
  publish$mixed(observed_data(roster, status = "stale"))
  settle(session)
  summary <- rival_team_value_display()
  widget <- jsonlite::fromJSON(output[["rival_squad_table-players_table"]], simplifyVector = FALSE)$x$tag$attribs
  stopifnot(summary$value == 24e6, summary$trend == -30000, summary$up == 2L, summary$down == 1L,
    summary$trend_status == "stale", grepl("Last saved trend", output$rival_team_value_box$html, fixed = TRUE),
    identical(rival_players_table_RV()$id, roster$id), identical(unlist(widget$data$name), roster$name),
    !isTRUE(widget$filterable), !isTRUE(widget$searchable),
    identical(reads, "mixed"))
  select("partial")
  settle(session)
  stopifnot(identical(reads, "mixed"))
  session$setInputs(rival_tabs = "history")
  session$setInputs(rival_tabs = "roster")
  settle(session)
  stopifnot(rival_team_value_display()$value == 7e6, is.na(rival_team_value_display()$trend))
  partial <- roster[2, , drop = FALSE]
  attr(partial, "worker_completeness") <- "partial"
  publish$partial(observed_data(partial))
  settle(session)
  summary <- rival_team_value_display()
  stopifnot(summary$trend_status == "unavailable", is.na(summary$up), is.na(summary$down))
  select("empty")
  session$setInputs(rival_tabs = "history")
  session$setInputs(rival_tabs = "roster")
  settle(session)
  publish$empty(observed_data(roster[FALSE, , drop = FALSE], status = "empty"))
  settle(session)
  summary <- rival_team_value_display()
  stopifnot(summary$value == 0, summary$trend == 0, summary$up == 0L, summary$down == 0L)
  select("mixed")
  session$setInputs(rival_tabs = "history")
  session$setInputs(rival_tabs = "roster")
  settle(session)
  stopifnot(rival_team_value_display()$trend == -30000, identical(reads, c("mixed", "partial", "empty")))
})
cat("PASS rival roster totals, trend, filter-free table, partial/empty states and team switching.",
    "Three roster reads for three opened squads; history and repeat selection added none.\n")
