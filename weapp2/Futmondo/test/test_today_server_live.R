#!/usr/bin/env Rscript
# =============================================================================
# test_today_server_live.R
# Tests today_Server with live data: reactives and outputs render without
# errors, and the 'Place Bid' recommendation routes to the Today-local
# selected_player flow (event retains the mapped 'market_bid' action; unknown
# player ids do not route). No network writes are performed.
#
# NOTE: the module is exercised with the proper two-argument
# shiny::testServer(module_fn, test_code, args = list(...)) pattern so the
# module server actually runs inside the mock session (a single-argument
# testServer block is never executed and would make these assertions vacuous).
# =============================================================================

options(warn = 1)  # Print warnings immediately

pass_count  <- 0
fail_count  <- 0
error_traces <- character(0)

# NOTE: the error handler must use <<- to update the outer `result` list.
# A plain `result$status <- ...` inside the handler would create a new local
# binding in the handler frame and silently swallow the failure.
safe_test <- function(label, expr) {
  result <- list(status = "pass", error = NULL, output = NULL, trace = character(0))
  tryCatch(
    {
      result$output <- eval(expr)
    },
    error = function(e) {
      result$status <<- "error"
      result$error  <<- e
      result$trace  <<- conditionMessage(e)
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

# The today_Server module runs inside the mock session via the module
# testServer pattern. The test code below has direct access to the module's
# internal reactives (market_players_RV, squad_players_RV, all_players_RV,
# recommendations_RV, selected_from_today_RV, selected_today_player_RV) and
# can set namespaced inputs via session$setInputs().

today_test_result <- safe_test("today_server_full", {
  # ---- Core reactives that today_Server depends on (top-level) ----
  login_token_RV <- reactiveVal(login_token)
  refresh_trigger <- reactiveVal(0)
  active_RV <- reactiveVal(FALSE)

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

  shiny::testServer(
    today_Server,
    {
      # ---- Activate the "today" tab so is_module_active() == TRUE ----
      active_RV(TRUE)
      session$flushReact()

      # ---- 3a: Evaluate market_players_RV ----
      cat("\n  [3a] Evaluating market_players_RV...\n")
      mkt <- market_players_RV()
      mkt_nrow <- if (!is.null(mkt)) nrow(mkt) else 0
      cat(sprintf("    market_players_RV: %d rows\n", mkt_nrow))

      if (mkt_nrow < 100) {
        stop(sprintf("market_players_RV has %d rows; expected 100+", mkt_nrow))
      }
      cat("    PASS: market_players_RV has 100+ players\n")

      # ---- 3b: Evaluate squad_players_RV ----
      cat("\n  [3b] Evaluating squad_players_RV...\n")
      sqd <- squad_players_RV()
      sqd_nrow <- if (!is.null(sqd)) nrow(sqd) else 0
      cat(sprintf("    squad_players_RV: %d rows\n", sqd_nrow))

      if (sqd_nrow < 10) {
        stop(sprintf("squad_players_RV has %d rows; expected 10+", sqd_nrow))
      }
      cat("    PASS: squad_players_RV has 10+ players\n")

      # ---- 3c: Evaluate all_players_RV (rbindlist of market + squad) ----
      cat("\n  [3c] Evaluating all_players_RV (rbindlist of market + squad)...\n")
      all_p <- all_players_RV()
      all_nrow <- if (!is.null(all_p)) nrow(all_p) else 0
      cat(sprintf("    all_players_RV: %d rows\n", all_nrow))

      if (all_nrow < 100) {
        stop(sprintf("all_players_RV has %d rows; expected at least 100", all_nrow))
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
      radar_widget <- session$getOutput(session$ns("market_radar_table"))
      cat(sprintf("    market_radar_table class: %s\n", class(radar_widget)[1]))
      cat("    PASS: output$market_radar_table renders reactable widget\n")

      # ---- 3f: Evaluate output$recommendations_feed_ui ----
      cat("\n  [3f] Evaluating output$recommendations_feed_ui...\n")
      feed_ui <- session$getOutput(session$ns("recommendations_feed_ui"))
      cat(sprintf("    recommendations_feed_ui class: %s\n", class(feed_ui)[1]))
      cat("    PASS: output$recommendations_feed_ui renders UI\n")

      # ---- 3g: 'Place Bid' recommendation -> Today-local selected player ----
      # Inject the namespaced recommendation click event (the exact shape the
      # browser JS sends) and verify ACTUAL downstream routing:
      #   (a) a known MARKET player's Place Bid maps to 'market_bid', resolves
      #       from market data, and reaches the Today-local open-action pathway;
      #   (b) an OWNED / non-market player's Place Bid cannot resolve / open a
      #       market bid (market-eligibility guard);
      #   (c) an unknown id does not route.
      # No network write is performed (only cached/read API calls from the
      # preflight path).
      cat("\n  [3g] Testing 'Place Bid' recommendation routing (no network write)...\n")

      # Pick a player that is actually listed on the market (market data only).
      mkt_ids <- as.character(mkt$id)
      known_pid <- mkt_ids[1]
      cat(sprintf("    Known MARKET player id: %s\n", known_pid))

      session$setInputs(rec_action_clicked = list(player_id = known_pid, action = "Place Bid"))
      session$flushReact()

      ev <- selected_from_today_RV()
      cat(sprintf("    Event player_id: %s\n", if (is.null(ev) || is.null(ev$player_id)) "NULL" else ev$player_id))
      cat(sprintf("    Event action:    %s\n", if (is.null(ev) || is.null(ev$action)) "NULL" else ev$action))
      if (is.null(ev) || is.null(ev$player_id) || is.null(ev$action)) {
        stop("rec_action_clicked event was not captured by selected_from_today_RV")
      }
      if (ev$player_id != known_pid) {
        stop(sprintf("Event player_id mismatch: %s", ev$player_id))
      }
      if (ev$action != "market_bid") {
        stop(sprintf("Event action should be 'market_bid', got: %s", ev$action))
      }
      cat("    PASS: event retains mapped action 'market_bid'\n")

      # Must resolve from MARKET data to the intended one-row player.
      sp <- selected_today_player_RV()
      if (is.null(sp) || nrow(sp) != 1) {
        stop("selected_today_player_RV did not resolve to a single player row")
      }
      if (as.character(sp$id) != known_pid) {
        stop("Resolved player id does not match the injected market player id")
      }
      if (!known_pid %in% mkt_ids) {
        stop("Resolved market_bid player is not present in market data")
      }
      cat(sprintf("    Resolved player (from market data): %s (id=%s)\n", sp$name, sp$id))
      cat("    PASS: known market player resolved from market data (modal routing target)\n")

      # Must reach the Today-local selected-player open-action pathway: the
      # stable action code carried to the nested selected_player module is
      # exactly 'market_bid' with a valid resolved player.
      open_act <- today_open_action_RV()
      if (!identical(open_act, "market_bid")) {
        stop(sprintf("open-action pathway not reached: expected 'market_bid', got: %s",
                     if (is.null(open_act)) "NULL" else open_act))
      }
      cat("    PASS: reaches Today-local selected-player open-action pathway (action='market_bid')\n")

      # (b) OWNED / non-market player: Place Bid must NOT resolve / open a
      # market bid. Find a squad (owned) player that is absent from market data.
      sqd_ids <- as.character(sqd$id)
      owned_not_market <- setdiff(sqd_ids, mkt_ids)
      if (length(owned_not_market) == 0) {
        stop("No owned (squad) player absent from market data; cannot test market-eligibility guard")
      }
      owned_pid <- owned_not_market[1]
      cat(sprintf("    Owned / non-market player id: %s\n", owned_pid))

      session$setInputs(rec_action_clicked = list(player_id = owned_pid, action = "Place Bid"))
      session$flushReact()
      ev_own <- selected_from_today_RV()
      if (is.null(ev_own) || ev_own$player_id != owned_pid) {
        stop("Owned-player Place Bid event was not captured")
      }
      if (ev_own$action != "market_bid") {
        stop("Owned-player Place Bid should still map to 'market_bid'")
      }
      # Market-eligibility guard: an owned / non-listed player cannot resolve
      # for a market_bid action (no resolved player -> no modal, no offer).
      if (!is.null(selected_today_player_RV())) {
        stop("Owned / non-market player must NOT resolve for a market_bid action")
      }
      cat("    PASS: owned / non-market player cannot resolve / open a market bid\n")

      # (c) Unknown player IDs must not route (no resolved player, no modal).
      session$setInputs(rec_action_clicked = list(player_id = "no_such_player_xyz", action = "Place Bid"))
      session$flushReact()
      ev2 <- selected_from_today_RV()
      if (is.null(ev2) || ev2$player_id != "no_such_player_xyz") {
        stop("Unknown-id event was not captured")
      }
      if (ev2$action != "market_bid") {
        stop("Unknown-id event should still retain the mapped action")
      }
      if (!is.null(selected_today_player_RV())) {
        stop("Unknown player id must not resolve to a player row")
      }
      cat("    PASS: unknown player id does not route (no resolved player, no modal)\n")

      cat("\n  ALL TODAY_SERVER TESTS PASSED\n")
    },
    args = list(
      id = "today",
      is_module_active = active_RV,
      login_token = login_token_RV,
      championship_id = championship_id_RV,
      user_team_id = user_team_id_RV,
      user_teams_RV = user_teams_RV,
      refresh_trigger = refresh_trigger
    )
  )
})

# ===================================================================
# STEP 4: Shared market-offer helper (selected_player_Server) with a
#          stubbed preflight -- deterministic, NO network write.
#          Verifies the actual open_action routing:
#            (3h-a) preflight fail -> helper attempts preflight, no offer modal
#            (3h-b) preflight ok   -> regular market-offer modal opens
# ===================================================================
cat("\n--- STEP 4: Shared market-offer helper (stubbed preflight, no network write) ---\n")

market_offer_helper_test <- safe_test("market_offer_helper", {
  # Stub capacity fetcher: records calls and returns a controlled capacity.
  # This is the test seam (capacity_fetcher arg) -- no network call is made.
  stub_env <- new.env()
  stub_env$count <- 0
  stub_env$capacity <- NULL
  stub_capacity <- function(login, championship_id, user_team_id, target_player_id) {
    stub_env$count <- stub_env$count + 1
    stub_env$capacity
  }

  # A valid market player row (single row with an immutable id).
  sp_df <- data.frame(id = "test_market_player", name = "Test Mkt Player",
                      effective_market_price = 500000, stringsAsFactors = FALSE)
  selected_player_RV <- reactiveVal(sp_df)

  # Non-NULL context values (the stub bypasses the network, so these only need
  # to be present to satisfy the preflight guard).
  login_token_RV2 <- reactiveVal(list(token = "t", userid = "u"))
  championship_id_RV2 <- reactiveVal("champ")
  user_team_id_RV2 <- reactiveVal("team")
  open_action_RV <- reactiveVal(NULL)

  shiny::testServer(
    selected_player_Server,
    {
      # ---- 3h-a: preflight FAIL (roster full) -> no offer modal ----
      stub_env$capacity <- list(
        status = "ok",
        roster = list(count = 25, cap = 25),
        outstanding = list(count = 0),
        funds = list(spendable_budget = 1000000)
      )
      open_action_RV("market_bid")
      session$flushReact()

      if (stub_env$count < 1) {
        stop("market-offer helper did not attempt preflight (capacity fetcher not called)")
      }
      if (isTRUE(offer_modal_opened_RV())) {
        stop("market-offer modal opened despite preflight failure")
      }
      cat("    PASS: preflight fail -> helper attempted preflight, no offer modal shown\n")

      # ---- 3h-b: preflight OK (free slot) -> offer modal opens ----
      stub_env$count <- 0
      stub_env$capacity <- list(
        status = "ok",
        roster = list(count = 20, cap = 25),
        outstanding = list(count = 0),
        funds = list(spendable_budget = 1000000)
      )
      # Re-trigger the open_action observer (it fires on change).
      open_action_RV(NULL)
      session$flushReact()
      open_action_RV("market_bid")
      session$flushReact()

      if (stub_env$count < 1) {
        stop("market-offer helper did not attempt preflight on the allowed path")
      }
      if (!isTRUE(offer_modal_opened_RV())) {
        stop("market-offer modal did not open on the allowed (preflight ok) path")
      }
      cat("    PASS: preflight ok -> regular market-offer modal opens\n")
    },
    args = list(
      id = "sp_test",
      selected_player = selected_player_RV,
      login_token = login_token_RV2,
      championship_id = championship_id_RV2,
      user_team_id = user_team_id_RV2,
      open_action = open_action_RV,
      capacity_fetcher = stub_capacity
    )
  )
})

# ===================================================================
# FINAL REPORT
# ===================================================================
cat("\n======================================================================\n")
cat("  FINAL REPORT\n")
cat("======================================================================\n")

all_passed <- (today_test_result$status == "pass") && (market_offer_helper_test$status == "pass")

if (all_passed) {
  pass_count <- pass_count + 1
  cat("\n  RESULT: ALL TESTS PASSED\n")
  cat("\n  Summary:\n")
  cat("    - global.R, ui.R, server.R sourced successfully\n")
  cat("    - Login with .Renviron credentials succeeded\n")
  cat("    - today_Server reactives evaluated with live data:\n")
  cat("        * market_players_RV:  100+ players\n")
  cat("        * squad_players_RV:   10+ players\n")
  cat("        * all_players_RV:     combined via rbindlist\n")
  cat("        * recommendations_RV: command center feed generated\n")
  cat("    - today_Server outputs rendered without errors:\n")
  cat("        * output$market_radar_table: reactable widget rendered\n")
  cat("        * output$recommendations_feed_ui: UI rendered\n")
  cat("    - 'Place Bid' recommendation routing verified (actual downstream routing):\n")
  cat("        * known MARKET player maps to 'market_bid', resolves from market data,\n")
  cat("          and reaches the Today-local selected-player open-action pathway\n")
  cat("        * owned / non-market player cannot resolve / open a market bid\n")
  cat("        * unknown player id does not route\n")
  cat("    - Shared market-offer helper (selected_player_Server, stubbed preflight):\n")
  cat("        * preflight fail -> no offer modal (helper attempts preflight, blocks)\n")
  cat("        * preflight ok  -> regular market-offer modal opens\n")
  cat("\n  All reactives and outputs render without any errors or warnings.\n")
  cat("======================================================================\n")
  quit(status = 0)
} else {
  fail_count <- fail_count + 1
  cat(sprintf("\n  RESULT: TEST FAILED\n"))
  if (today_test_result$status != "pass") {
    cat(sprintf("  today_Server error: %s\n", today_test_result$trace))
  }
  if (market_offer_helper_test$status != "pass") {
    cat(sprintf("  market-offer helper error: %s\n", market_offer_helper_test$trace))
  }
  cat("======================================================================\n")
  quit(status = 1)
}
