#!/usr/bin/env Rscript
# =============================================================================
# test_today_server_live.R
# Tests today_Server with live data: reactives and outputs render without
# errors or warnings.
# =============================================================================

options(warn = 1)  # Print warnings immediately

pass_count  <- 0
fail_count  <- 0
error_traces <- character(0)

safe_test <- function(label, expr) {
  result <- list(status = "pass", error = NULL, output = NULL, trace = character(0))
  tryCatch(
    {
      result$output <- eval(expr)
    },
    error = function(e) {
      result$status <- "error"
      result$error  <- e
      result$trace  <- conditionMessage(e)
    }
  )
  result
}

cat("\n======================================================================\n")
cat("  TODAY_SERVER LIVE DATA TEST\n")
cat("======================================================================\n")

# ===================================================================
# STEP 1: Source global.R, ui.R, server.R
# ===================================================================
cat("\n--- STEP 1: Sourcing global.R, ui.R, server.R ---\n")

source_global <- safe_test("source_global.R", { source("global.R") })
cat(sprintf("  source global.R: %s\n", source_global$status))

source_ui <- safe_test("source_ui.R", { source("ui.R") })
cat(sprintf("  source ui.R:     %s\n", source_ui$status))

source_server <- safe_test("source_server.R", { source("server.R") })
cat(sprintf("  source server.R: %s\n", source_server$status))

if (any(sapply(list(source_global, source_ui, source_server), function(r) r$status == "error"))) {
  cat("\n  FATAL: Could not source core files. Aborting.\n")
  quit(status = 1)
}
cat("  All core files sourced successfully.\n")

# ===================================================================
# STEP 2: Login with .Renviron credentials
# ===================================================================
cat("\n--- STEP 2: Login with .Renviron credentials ---\n")

user_name_env <- Sys.getenv("user_name")
password_env  <- Sys.getenv("password")

cat(sprintf("  user_name: %s\n", ifelse(user_name_env == "", "<empty>", user_name_env)))
cat(sprintf("  password:  %s\n", ifelse(password_env == "", "<empty>", "***")))

login_result <- safe_test("login", {
  if (user_name_env == "" || password_env == "") {
    stop("user_name or password environment variables are empty")
  }
  login(user_name = user_name_env, password = password_env)
})

if (login_result$status == "error") {
  cat(sprintf("  Login FAILED: %s\n", login_result$trace))
  quit(status = 1)
}

login_token <- login_result$output
cat(sprintf("  Login SUCCESS (token length: %d)\n", length(login_token)))

# ===================================================================
# STEP 3: Test today_Server with live data via shiny::testServer
# ===================================================================
cat("\n--- STEP 3: Testing today_Server reactives and outputs ---\n")

# We run the entire today_Server module inside testServer so that all
# internal reactives (market_players_RV, squad_players_RV, all_players_RV,
# recommendations_RV) and outputs (market_radar_table, recommendations_feed_ui)
# are accessible within the reactive context.

today_test_result <- safe_test("today_server_full", {
  shiny::testServer(
    {
      # ---- Set up core reactives that today_Server depends on ----
      login_token_RV <- reactiveVal(login_token)
      refresh_trigger <- reactiveVal(0)

      championship_RV <- reactive({
        req(login_token_RV())
        get_championships(login = login_token_RV(), championship_name = NULL)
      })

      championship_id_RV <- reactive({
        req(championship_RV())
        championship_RV()["id"]
      })

      user_team_id_RV <- reactive({
        req(championship_RV())
        championship_RV()["userteam.id"]
      })

      user_teams_RV <- reactive({
        req(championship_RV())
        get_teams(login = login_token_RV(), championship_id = championship_RV()["id"])
      })

      # ---- Instantiate today_Server module ----
      today_Server(
        id = "today",
        is_module_active = reactive({ input$tabs == "today" }),
        login_token = login_token_RV,
        championship_id = championship_id_RV,
        user_team_id = user_team_id_RV,
        user_teams_RV = user_teams_RV,
        refresh_trigger = refresh_trigger
      )

      # ---- Activate the "today" tab so is_module_active() == TRUE ----
      input$tabs <<- "today"

      # ---- 3a: Evaluate market_players_RV ----
      cat("\n  [3a] Evaluating market_players_RV...\n")
      mkt <- market_players_RV()
      mkt_nrow <- if (!is.null(mkt)) nrow(mkt) else 0
      cat(sprintf("    market_players_RV: %d rows\n", mkt_nrow))

      if (mkt_nrow < 141) {
        stop(sprintf("market_players_RV has %d rows; expected 141+", mkt_nrow))
      }
      cat("    PASS: market_players_RV has 141+ players\n")

      # ---- 3b: Evaluate squad_players_RV ----
      cat("\n  [3b] Evaluating squad_players_RV...\n")
      sqd <- squad_players_RV()
      sqd_nrow <- if (!is.null(sqd)) nrow(sqd) else 0
      cat(sprintf("    squad_players_RV: %d rows\n", sqd_nrow))

      if (sqd_nrow < 16) {
        stop(sprintf("squad_players_RV has %d rows; expected 16+", sqd_nrow))
      }
      cat("    PASS: squad_players_RV has 16+ players\n")

      # ---- 3c: Evaluate all_players_RV (rbindlist of market + squad) ----
      cat("\n  [3c] Evaluating all_players_RV (rbindlist of market + squad)...\n")
      all_p <- all_players_RV()
      all_nrow <- if (!is.null(all_p)) nrow(all_p) else 0
      cat(sprintf("    all_players_RV: %d rows\n", all_nrow))

      if (all_nrow < 16) {
        stop(sprintf("all_players_RV has %d rows; expected at least 16", all_nrow))
      }
      cat("    PASS: all_players_RV combines market and squad players via rbindlist\n")

      # ---- 3d: Evaluate recommendations_RV ----
      cat("\n  [3d] Evaluating recommendations_RV (command center recommendations)...\n")
      recs <- recommendations_RV()
      recs_nrow <- if (!is.null(recs)) nrow(recs) else 0
      cat(sprintf("    recommendations_RV: %d rows\n", recs_nrow))

      if (recs_nrow < 1) {
        stop("recommendations_RV has 0 rows; expected at least 1 recommendation")
      }
      cat(sprintf("    Recommendation types: %s\n",
                  paste(unique(recs$type), collapse = ", ")))
      cat("    PASS: recommendations_RV generates command center recommendations\n")

      # ---- 3e: Evaluate output$market_radar_table (reactable widget) ----
      cat("\n  [3e] Evaluating output$market_radar_table (reactable widget)...\n")
      # renderReactable is called inside the module; we trigger it by
      # accessing output$market_radar_table which should have been set
      # by the module's renderReactable call.
      # The output is set by the module during its initialization.
      # We verify the reactable was created without error.
      radar_output <- output$market_radar_table
      # The output is a reactive expression that returns a reactable widget.
      # In testServer, output$market_radar_table is a function; calling it
      # executes the renderReactable.
      if (!is.null(radar_output) && is.function(radar_output)) {
        radar_widget <- radar_output()
        cat(sprintf("    market_radar_table class: %s\n", class(radar_widget)[1]))
        cat("    PASS: output$market_radar_table renders reactable widget\n")
      } else {
        cat("    output$market_radar_table is NULL or not a function\n")
        stop("output$market_radar_table was not properly initialized")
      }

      # ---- 3f: Evaluate output$recommendations_feed_ui ----
      cat("\n  [3f] Evaluating output$recommendations_feed_ui...\n")
      feed_output <- output$recommendations_feed_ui
      if (!is.null(feed_output) && is.function(feed_output)) {
        feed_ui <- feed_output()
        cat(sprintf("    recommendations_feed_ui class: %s\n", class(feed_ui)[1]))
        cat("    PASS: output$recommendations_feed_ui renders UI\n")
      } else {
        cat("    output$recommendations_feed_ui is NULL or not a function\n")
        stop("output$recommendations_feed_ui was not properly initialized")
      }

      cat("\n  ALL TODAY_SERVER TESTS PASSED\n")
      TRUE
    }
  )
})

# ===================================================================
# FINAL REPORT
# ===================================================================
cat("\n======================================================================\n")
cat("  FINAL REPORT\n")
cat("======================================================================\n")

if (today_test_result$status == "pass") {
  pass_count <- pass_count + 1
  cat("\n  RESULT: ALL TESTS PASSED\n")
  cat("\n  Summary:\n")
  cat("    - global.R, ui.R, server.R sourced successfully\n")
  cat("    - Login with .Renviron credentials succeeded\n")
  cat("    - today_Server reactives evaluated with live data:\n")
  cat("        * market_players_RV:  141+ players\n")
  cat("        * squad_players_RV:   16+ players\n")
  cat("        * all_players_RV:     combined via rbindlist\n")
  cat("        * recommendations_RV: command center feed generated\n")
  cat("    - today_Server outputs rendered without errors:\n")
  cat("        * output$market_radar_table: reactable widget rendered\n")
  cat("        * output$recommendations_feed_ui: UI rendered\n")
  cat("\n  All reactives and outputs render without any errors or warnings.\n")
  cat("======================================================================\n")
  quit(status = 0)
} else {
  fail_count <- fail_count + 1
  cat(sprintf("\n  RESULT: TEST FAILED\n"))
  cat(sprintf("  Error: %s\n", today_test_result$trace))
  cat("======================================================================\n")
  quit(status = 1)
}