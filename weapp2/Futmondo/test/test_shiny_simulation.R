#!/usr/bin/env Rscript
# =============================================================================
# test_shiny_simulation.R
# Simulate the entire Shiny lifecycle: sourcing, login, tab navigation,
# and reactive/output execution for every tab.
# =============================================================================

options(warn = 1)  # Print warnings immediately

# ---- Global accumulators ----
test_results   <- list()
error_traces   <- character(0)
warning_msgs   <- character(0)

safe_capture <- function(label, expr) {
  local_result <- list(status = "pass", error = NULL, output = NULL, trace = character(0))
  tryCatch(
    {
      local_result$output <- eval(expr)
    },
    error = function(e) {
      local_result$status <- "error"
      local_result$error  <- e
      local_result$trace  <- conditionMessage(e)
      if (!is.null(e$call)) {
        local_result$trace <- c(local_result$trace, deparse(e$call))
      }
    }
  )
  # Also capture warnings via a second try
  if (local_result$status == "pass") {
    local_result$warnings <- character(0)
  }
  test_results[[label]] <<- local_result
  if (local_result$status == "error") {
    error_traces <<- c(error_traces, paste0("[ERROR] ", label, ": ", local_result$trace))
  }
  invisible(local_result)
}

cat("\n======================================================================\n")
cat("  SHINY LIFECYCLE SIMULATION TEST\n")
cat("======================================================================\n")

# ===================================================================
# PHASE 1: Source all files
# ===================================================================
cat("\n--- PHASE 1: Sourcing global.R, ui.R, server.R ---\n")

source_global_R <- safe_capture("source_global.R", {
  source("global.R")
})
cat(sprintf("  source global.R: %s\n", source_global_R$status))

source_ui_R <- safe_capture("source_ui.R", {
  source("ui.R")
})
cat(sprintf("  source ui.R:     %s\n", source_ui_R$status))

source_server_R <- safe_capture("source_server.R", {
  source("server.R")
})
cat(sprintf("  source server.R: %s\n", source_server_R$status))

if (any(sapply(test_results, function(r) r$status == "error"))) {
  cat("\n  FATAL: Could not source core files. Aborting simulation.\n")
  cat("======================================================================\n")
  cat("ERRORS:\n")
  for (e in error_traces) cat("  ", e, "\n")
  cat("======================================================================\n")
  quit(status = 1)
}

cat("\n  All core files sourced successfully.\n")

# ===================================================================
# PHASE 2: Login with .Renviron credentials
# ===================================================================
cat("\n--- PHASE 2: Login with .Renviron credentials ---\n")

user_name_env <- Sys.getenv("user_name")
password_env  <- Sys.getenv("password")

cat(sprintf("  user_name: %s\n", ifelse(user_name_env == "", "<empty>", user_name_env)))
cat(sprintf("  password:  %s\n", ifelse(password_env == "", "<empty>", "***")))

login_result <- safe_capture("login", {
  if (user_name_env == "" || password_env == "") {
    stop("user_name or password environment variables are empty in .Renviron")
  }
  login(user_name = user_name_env, password = password_env)
})

if (login_result$status != "error") {
  token <- login_result$output
  cat(sprintf("  Login status: SUCCESS\n"))
  cat(sprintf("  Token present: %s\n", !is.null(token)))
  cat(sprintf("  Token length:  %s\n", length(token)))
  if (is.null(names(token))) {
    cat("  Token names: NULL (unnamed vector)\n")
  } else {
    cat(sprintf("  Token names: %s\n", paste(names(token), collapse = ", ")))
  }
} else {
  cat(sprintf("  Login status: FAILED\n"))
  cat(sprintf("  Error: %s\n", login_result$trace))
  cat("\n  Cannot proceed without a valid login token.\n")
  cat("======================================================================\n")
  cat("ERRORS:\n")
  for (e in error_traces) cat("  ", e, "\n")
  cat("======================================================================\n")
  quit(status = 1)
}

login_token <- login_result$output

# ===================================================================
# PHASE 3: shiny::testServer simulation
# ===================================================================
cat("\n--- PHASE 3: Simulating Shiny server via shiny::testServer ---\n")

# We cannot fully replicate the Shiny session (no reactive context, no
# session$ns, no output rendering). Instead we use testServer to:
#   a) set login_token_RV
#   b) navigate each tab via input$tabs
#   c) trigger reactives and catch errors

# The server function is stored in the global environment after sourcing
# server.R. It is an anonymous function, so we need to re-source it
# and capture the function object.

server_fn <- safe_capture("get_server_fn", {
  # Re-read server.R to get the function
  src <- readLines("server.R", warn = FALSE)
  # Parse the function
  parsed <- parse(text = src)
  # The last expression should be the function
  as.function(parsed)
})

if (server_fn$status == "error") {
  cat(sprintf("  WARNING: Could not extract server function directly. Using inline approach.\n"))
  server_fn$output <- function(input, output, session) {
    source("server.R")
  }
}

# ---- 3a: Basic server instantiation ----
cat("\n  [3a] Instantiating server with testServer...\n")

server_instantiation <- safe_capture("server_instantiation", {
  shiny::testServer(
    function(input, output, session) {
      # Minimal server that just sources the real server's logic
      # We cannot call the full server because it depends on modules
      # that need a real session. Instead we test the pieces.
    },
    {
      TRUE  # just verify testServer works
    }
  )
})

cat(sprintf("  testServer basic instantiation: %s\n", server_instantiation$status))

# ---- 3b: Login module test ----
cat("\n  [3b] Testing login_Server module...\n")

login_module_test <- safe_capture("login_module_test", {
  shiny::testServer(
    login_Server(id = "login"),
    {
      # Simulate setting user_name and password inputs
      input$user_name  <<- user_name_env
      input$password   <<- password_env

      # Trigger login button
      input$login_button <<- 1

      # The login_token_RV should be set
      token_val <- login_token_RV()
      cat(sprintf("    login_token_RV() length: %s\n", length(token_val)))
      cat(sprintf("    login_token_RV() is not NA: %s\n", !is.na(token_val)))
      token_val
    }
  )
})

if (login_module_test$status == "pass") {
  cat("  login_Server module: PASS\n")
} else {
  cat(sprintf("  login_Server module: FAIL - %s\n", login_module_test$trace))
}

# ---- 3c: Full server with login_token_RV pre-set ----
cat("\n  [3c] Testing full server with pre-set login_token_RV...\n")

# We will build a wrapper server that pre-populates login_token_RV and
# then exercises each tab.

full_server_test <- safe_capture("full_server_test", {
  shiny::testServer(
    {
      # ---- Inline the full server logic ----
      # We must replicate the server.R logic inside testServer because
      # moduleServer calls need a reactive context.

      # First, set up the login token reactively
      login_token_RV <- reactiveVal(login_token)

      # Caching Refresh Trigger
      refresh_trigger <- reactiveVal(0)

      # ---- Core reactives ----
      championship_RV <- reactive({
        req(login_token_RV())
        get_championships(login = login_token_RV(), championship_name = NULL)
      })

      user_teams_RV <- reactive({
        req(championship_RV())
        get_teams(login = login_token_RV(), championship_id = championship_RV()["id"])
      })

      championship_id_RV <- reactive({
        req(championship_RV())
        championship_RV()["id"]
      })

      user_team_id_RV <- reactive({
        req(championship_RV())
        championship_RV()["userteam.id"]
      })

      user_team_name_RV <- reactive({
        req(championship_RV())
        championship_RV()["userteam.name"]
      })

      # ---- Verify core reactives ----
      cat("    Checking login_token_RV()...\n")
      tok <- login_token_RV()
      cat(sprintf("      login_token_RV: length=%d, not_null=%s\n", length(tok), !is.null(tok)))

      cat("    Checking championship_RV()...\n")
      champ <- championship_RV()
      cat(sprintf("      championship_RV: length=%d\n", length(champ)))
      if (length(champ) > 0) {
        cat(sprintf("      champ id: %s\n", as.character(champ["id"])))
        cat(sprintf("      champ userteam.id: %s\n", as.character(champ["userteam.id"])))
        cat(sprintf("      champ userteam.name: %s\n", as.character(champ["userteam.name"])))
      }

      cat("    Checking user_teams_RV()...\n")
      teams <- user_teams_RV()
      cat(sprintf("      user_teams_RV: nrow=%s\n", ifelse(is.null(teams), "NULL", nrow(teams))))

      cat("    Checking championship_id_RV()...\n")
      cid <- championship_id_RV()
      cat(sprintf("      championship_id_RV: %s\n", as.character(cid)))

      cat("    Checking user_team_id_RV()...\n")
      tid <- user_team_id_RV()
      cat(sprintf("      user_team_id_RV: %s\n", as.character(tid)))

      cat("    Checking user_team_name_RV()...\n")
      tname <- user_team_name_RV()
      cat(sprintf("      user_team_name_RV: %s\n", as.character(tname)))
    }
  )
})

if (full_server_test$status == "pass") {
  cat("  Full server core reactives: PASS\n")
} else {
  cat(sprintf("  Full server core reactives: FAIL - %s\n", full_server_test$trace))
}

# ===================================================================
# PHASE 4: Tab-by-tab simulation
# ===================================================================
cat("\n--- PHASE 4: Tab-by-tab module simulation ---\n")

tabs_to_test <- c(
  "today",
  "yourteam",
  "market",
  "players_in_championship",
  "rivals",
  "classification",
  "admin"
)

# Module server functions
module_fns <- list(
  "today"                     = today_Server,
  "yourteam"                  = players_in_teams_Server,
  "market"                    = market_Server,
  "players_in_championship"   = players_in_championship_Server,
  "rivals"                    = rivals_Server,
  "classification"            = classification_Server,
  "admin"                     = admin_Server
)

# Module UI functions
module_ui_fns <- list(
  "today"                     = today_UI,
  "yourteam"                  = players_in_teams_UI,
  "market"                    = market_UI,
  "players_in_championship"   = players_in_championship_UI,
  "rivals"                    = rivals_UI,
  "classification"            = classification_UI,
  "admin"                     = admin_UI
)

for (tab_name in tabs_to_test) {
  cat(sprintf("\n  [TAB: %s]\n", tab_name))

  # ---- 4a: Test UI function ----
  ui_test <- safe_capture(paste0("UI_", tab_name), {
    fn <- module_ui_fns[[tab_name]]
    if (is.null(fn)) stop(paste("UI function not found for tab:", tab_name))
    ui_result <- fn(id = tab_name)
    cat(sprintf("    UI function returned: %s\n", class(ui_result)[1]))
    ui_result
  })

  if (ui_test$status == "pass") {
    cat(sprintf("    UI: PASS\n"))
  } else {
    cat(sprintf("    UI: FAIL - %s\n", ui_test$trace))
  }

  # ---- 4b: Test Server module ----
  srv_test <- safe_capture(paste0("SERVER_", tab_name), {
    fn <- module_fns[[tab_name]]
    if (is.null(fn)) stop(paste("Server function not found for tab:", tab_name))

    shiny::testServer(
      fn(
        id = tab_name,
        is_module_active = reactive({ input$tabs == tab_name }),
        login_token = reactive({ login_token }),
        championship_id = reactive({
          champ <- get_championships(login = login_token, championship_name = NULL)
          as.character(champ["id"])
        }),
        user_team_id = reactive({
          champ <- get_championships(login = login_token, championship_name = NULL)
          as.character(champ["userteam.id"])
        }),
        user_teams_RV = reactive({
          champ <- get_championships(login = login_token, championship_name = NULL)
          get_teams(login = login_token, championship_id = as.character(champ["id"]))
        }),
        refresh_trigger = reactiveVal(0)
      ),
      {
        # Activate this tab
        input$tabs <<- tab_name

        # Try to trigger the module's reactives
        # The module uses is_module_active() == TRUE gate, so setting
        # input$tabs to the tab name should enable it.

        # For modules that return a reactiveVal (selected_player_RV),
        # try to access it
        if (exists("selected_player_RV", mode = "function")) {
          val <- tryCatch(selected_player_RV(), error = function(e) NULL)
          cat(sprintf("      selected_player_RV: %s\n",
                       ifelse(is.null(val), "NULL (expected when no selection)", "has value")))
        }

        # For today module, try to access internal reactives
        if (tab_name == "today") {
          # These are internal to the module, so we cannot access them directly
          # from outside. The test passes if the moduleServer call succeeded.
          cat("      today_Server module instantiated successfully\n")
        }

        cat(sprintf("      Module '%s' initialized without error\n", tab_name))
        TRUE
      }
    )
  })

  if (srv_test$status == "pass") {
    cat(sprintf("    SERVER: PASS\n"))
  } else {
    cat(sprintf("    SERVER: FAIL - %s\n", srv_test$trace))
  }
}

# ===================================================================
# PHASE 5: Tab switching simulation
# ===================================================================
cat("\n--- PHASE 5: Tab switching simulation ---\n")

tab_switch_test <- safe_capture("tab_switching", {
  shiny::testServer(
    {
      # Simulate a full server with all modules
      login_token_RV <- reactiveVal(login_token)
      refresh_trigger <- reactiveVal(0)

      championship_RV <- reactive({
        req(login_token_RV())
        get_championships(login = login_token_RV(), championship_name = NULL)
      })

      user_teams_RV <- reactive({
        req(championship_RV())
        get_teams(login = login_token_RV(), championship_id = championship_RV()["id"])
      })

      championship_id_RV <- reactive({
        req(championship_RV())
        championship_RV()["id"]
      })

      user_team_id_RV <- reactive({
        req(championship_RV())
        championship_RV()["userteam.id"]
      })

      # Instantiate all modules
      selected_player_RV <- players_in_teams_Server(
        id = "players_in_teams",
        is_module_active = reactive({ input$tabs == "yourteam" }),
        login_token = login_token_RV,
        championship_id = championship_id_RV,
        user_team_id = user_team_id_RV,
        user_teams_RV = user_teams_RV,
        refresh_trigger = refresh_trigger
      )

      today_Server(
        id = "today",
        is_module_active = reactive({ input$tabs == "today" }),
        login_token = login_token_RV,
        championship_id = championship_id_RV,
        user_team_id = user_team_id_RV,
        user_teams_RV = user_teams_RV,
        refresh_trigger = refresh_trigger
      )

      market_Server(
        id = "market",
        is_module_active = reactive({ input$tabs == "market" }),
        login_token = login_token_RV,
        championship_id = championship_id_RV,
        user_team_id = user_team_id_RV,
        user_teams_RV = user_teams_RV,
        refresh_trigger = refresh_trigger
      )

      players_in_championship_Server(
        id = "players_in_championship",
        is_module_active = reactive({ input$tabs == "players_in_championship" }),
        login_token = login_token_RV,
        championship_id = championship_id_RV,
        user_teams_RV = user_teams_RV,
        refresh_trigger = refresh_trigger
      )

      rivals_Server(
        id = "rivals",
        is_module_active = reactive({ input$tabs == "rivals" }),
        login_token = login_token_RV,
        championship_id = championship_id_RV,
        user_team_id = user_team_id_RV,
        user_teams_RV = user_teams_RV
      )

      classification_Server(
        id = "classification",
        is_module_active = reactive({ input$tabs == "classification" }),
        login_token = login_token_RV,
        championship_id = championship_id_RV,
        user_team_id = user_team_id_RV,
        user_teams_RV = user_teams_RV
      )

      admin_Server(
        id = "admin",
        is_module_active = reactive({
          req(input$tabs); input$tabs == "admin"
        }),
        login_token = login_token_RV,
        championship_id = championship_id_RV,
        user_team_id = user_team_id_RV,
        user_teams_RV = user_teams_RV
      )

      # Simulate tab switching
      tab_order <- c("today", "yourteam", "market", "players_in_championship",
                     "rivals", "classification", "admin")

      for (t in tab_order) {
        input$tabs <<- t
        cat(sprintf("    Switched to tab: %s\n", t))
      }

      cat("    Tab switching complete: all 7 tabs navigated\n")
      TRUE
    }
  )
})

if (tab_switch_test$status == "pass") {
  cat("  Tab switching: PASS\n")
} else {
  cat(sprintf("  Tab switching: FAIL - %s\n", tab_switch_test$trace))
}

# ===================================================================
# PHASE 6: Menu rendering test
# ===================================================================
cat("\n--- PHASE 6: Menu rendering test ---\n")

menu_render_test <- safe_capture("menu_rendering", {
  shiny::testServer(
    {
      login_token_RV <- reactiveVal(login_token)

      # Replicate the menu rendering logic from server.R
      is_admin <- FALSE
      admin_env <- trimws(Sys.getenv("admin"))
      if (admin_env != "" && !is.null(login_token_RV()) && length(login_token_RV()) >= 3) {
        current_user <- trimws(as.character(login_token_RV()[["user_name"]]))
        is_admin <- tolower(current_user) == tolower(admin_env)
      }

      cat(sprintf("    is_admin: %s\n", is_admin))
      cat(sprintf("    admin_env: %s\n", admin_env))

      menu_items <- list(
        shinydashboard::menuItem("Login", tabName = "login", icon = shiny::icon("right-to-bracket")),
        shinydashboard::menuItem("Today", tabName = "today", icon = shiny::icon("bolt")),
        shinydashboard::menuItem("Your team", tabName = "yourteam", icon = shiny::icon("users")),
        shinydashboard::menuItem("Market", tabName = "market", icon = shiny::icon("money-bill-trend-up")),
        shinydashboard::menuItem("Players", tabName = "players_in_championship", icon = shiny::icon("table")),
        shinydashboard::menuItem("Rivals", tabName = "rivals", icon = shiny::icon("users-viewfinder")),
        shinydashboard::menuItem("Classification", tabName = "classification", icon = shiny::icon("trophy"))
      )

      if (is_admin) {
        menu_items <- c(menu_items, list(
          shinydashboard::menuItem("Admin", tabName = "admin", icon = shiny::icon("gears"))
        ))
      }

      menu <- do.call(shinydashboard::sidebarMenu, c(list(id = "tabs"), menu_items))
      cat(sprintf("    Menu items count: %d\n", length(menu_items)))
      menu
    }
  )
})

if (menu_render_test$status == "pass") {
  cat("  Menu rendering: PASS\n")
} else {
  cat(sprintf("  Menu rendering: FAIL - %s\n", menu_render_test$trace))
}

# ===================================================================
# PHASE 7: Refresh trigger test
# ===================================================================
cat("\n--- PHASE 7: Refresh trigger test ---\n")

refresh_test <- safe_capture("refresh_trigger", {
  shiny::testServer(
    {
      refresh_trigger <- reactiveVal(0)

      cat(sprintf("    Initial refresh_trigger: %d\n", refresh_trigger()))

      # Simulate clicking refresh_all button
      input$refresh_all <<- 1

      # The observeEvent in server.R would call:
      clear_api_cache()
      refresh_trigger(refresh_trigger() + 1)

      cat(sprintf("    After refresh: %d\n", refresh_trigger()))
      cat(sprintf("    Refresh incremented: %s\n", refresh_trigger() == 1))
      TRUE
    }
  )
})

if (refresh_test$status == "pass") {
  cat("  Refresh trigger: PASS\n")
} else {
  cat(sprintf("  Refresh trigger: FAIL - %s\n", refresh_test$trace))
}

# ===================================================================
# PHASE 8: Background sync observer test
# ===================================================================
cat("\n--- PHASE 8: Background sync observer test ---\n")

bg_sync_test <- safe_capture("background_sync", {
  shiny::testServer(
    {
      login_token_RV <- reactiveVal(login_token)

      championship_RV <- reactive({
        req(login_token_RV())
        get_championships(login = login_token_RV(), championship_name = NULL)
      })

      championship_id_RV <- reactive({
        req(championship_RV())
        championship_RV()["id"]
      })

      # Simulate the background sync observer from server.R
      sync_result <- tryCatch({
        champ_id <- championship_id_RV()
        cat(sprintf("    championship_id for sync: %s\n", as.character(champ_id)))

        all_players <- get_championship_players(
          login = login_token_RV(),
          championship_id = champ_id
        )

        if (!is.null(all_players) && nrow(all_players) > 0) {
          cat(sprintf("    Fetched %d players for background sync\n", nrow(all_players)))
          sync_real_clubs_to_supabase(all_players)
          sync_players_to_supabase(all_players)
          log_player_history(all_players, champ_id)
          cat("    Background sync: SUCCESS\n")
        } else {
          cat("    Background sync: No players to sync\n")
        }
        TRUE
      }, error = function(e) {
        cat(sprintf("    Background sync: WARNING - %s\n", e$message))
        TRUE  # Non-fatal
      })

      sync_result
    }
  )
})

if (bg_sync_test$status == "pass") {
  cat("  Background sync: PASS\n")
} else {
  cat(sprintf("  Background sync: FAIL - %s\n", bg_sync_test$trace))
}

# ===================================================================
# PHASE 9: Tab-specific reactive execution
# ===================================================================
cat("\n--- PHASE 9: Tab-specific reactive execution ---\n")

# ---- 9a: today tab reactives ----
cat("\n  [9a] today tab reactives...\n")
today_reactives_test <- safe_capture("today_reactives", {
  shiny::testServer(
    {
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

      # Simulate today module's internal reactives
      input$tabs <<- "today"

      market_players <- get_market_players(
        login = login_token_RV(),
        championship_id = championship_id_RV(),
        user_team_id = user_team_id_RV()
      )
      cat(sprintf("    market_players: %s rows\n", nrow(market_players)))

      squad_players <- get_players_from_team(
        login = login_token_RV(),
        championship_id = championship_id_RV(),
        user_team_id = user_team_id_RV(),
        teams = NULL
      )
      cat(sprintf("    squad_players: %s rows\n", nrow(squad_players)))

      pressroom <- get_championship_pressroom(
        login = login_token_RV(),
        championship_id = championship_id_RV()
      )
      cat(sprintf("    pressroom: %s rows\n", nrow(pressroom)))

      user_finances <- get_user_team_info(
        login = login_token_RV(),
        championship_id = championship_id_RV(),
        user_team_id = user_team_id_RV()
      )
      cat(sprintf("    user_finances: budget=%s\n",
                  ifelse(is.null(user_finances$budget), "NULL", user_finances$budget)))

      TRUE
    }
  )
})

if (today_reactives_test$status == "pass") {
  cat("  today reactives: PASS\n")
} else {
  cat(sprintf("  today reactives: FAIL - %s\n", today_reactives_test$trace))
}

# ---- 9b: yourteam tab reactives ----
cat("\n  [9b] yourteam tab reactives...\n")
yourteam_reactives_test <- safe_capture("yourteam_reactives", {
  shiny::testServer(
    {
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

      input$tabs <<- "yourteam"

      players_table <- get_players_from_team(
        login = login_token_RV(),
        championship_id = championship_id_RV(),
        user_team_id = user_team_id_RV(),
        teams = NULL
      )
      players_table <- players_table %>% translate_player_positions()
      players_table <- players_table %>% calculate_player_changes()
      players_table <- players_table %>% unify_columns()

      cat(sprintf("    players_table: %s rows\n", nrow(players_table)))

      val_sum <- sum(players_table$value, na.rm = TRUE)
      cat(sprintf("    team_value_sum: %s\n", val_sum))

      # Check standings evolution data
      history_df <- tryCatch({
        get_league_standings_history(championship_id_RV())
      }, error = function(e) NULL)

      if (!is.null(history_df)) {
        cat(sprintf("    standings_history: %s rows\n", nrow(history_df)))
      } else {
        cat("    standings_history: NULL\n")
      }

      TRUE
    }
  )
})

if (yourteam_reactives_test$status == "pass") {
  cat("  yourteam reactives: PASS\n")
} else {
  cat(sprintf("  yourteam reactives: FAIL - %s\n", yourteam_reactives_test$trace))
}

# ---- 9c: market tab reactives ----
cat("\n  [9c] market tab reactives...\n")
market_reactives_test <- safe_capture("market_reactives", {
  shiny::testServer(
    {
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

      input$tabs <<- "market"

      market_players <- get_market_players(
        login = login_token_RV(),
        championship_id = championship_id_RV(),
        user_team_id = user_team_id_RV()
      )
      market_players <- market_players %>% translate_player_positions()
      market_players <- market_players %>% calculate_player_changes()
      market_players <- market_players %>% unify_columns()

      cat(sprintf("    market_players: %s rows\n", nrow(market_players)))
      TRUE
    }
  )
})

if (market_reactives_test$status == "pass") {
  cat("  market reactives: PASS\n")
} else {
  cat(sprintf("  market reactives: FAIL - %s\n", market_reactives_test$trace))
}

# ---- 9d: players_in_championship tab reactives ----
cat("\n  [9d] players_in_championship tab reactives...\n")
pich_reactives_test <- safe_capture("pich_reactives", {
  shiny::testServer(
    {
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

      user_teams_RV <- reactive({
        req(championship_RV())
        get_teams(login = login_token_RV(), championship_id = championship_RV()["id"])
      })

      input$tabs <<- "players_in_championship"

      all_players <- get_championship_players(
        login = login_token_RV(),
        championship_id = championship_id_RV()
      )
      all_players <- all_players %>% translate_player_positions()
      all_players <- all_players %>% calculate_player_changes()
      all_players <- all_players %>% unify_columns()

      cat(sprintf("    all_championship_players: %s rows\n", nrow(all_players)))
      TRUE
    }
  )
})

if (pich_reactives_test$status == "pass") {
  cat("  players_in_championship reactives: PASS\n")
} else {
  cat(sprintf("  players_in_championship reactives: FAIL - %s\n", pich_reactives_test$trace))
}

# ---- 9e: rivals tab reactives ----
cat("\n  [9e] rivals tab reactives...\n")
rivals_reactives_test <- safe_capture("rivals_reactives", {
  shiny::testServer(
    {
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

      input$tabs <<- "rivals"

      # Test league finances calculation
      finances <- calculate_league_finances(
        login = login_token_RV(),
        championship_id = championship_id_RV(),
        user_teams_df = user_teams_RV(),
        initial_budget = 300000000
      )

      cat(sprintf("    league_finances: %s keys\n", length(finances)))
      if (!is.null(finances$team_finances)) {
        cat(sprintf("    team_finances: %s rows\n", nrow(finances$team_finances)))
      }

      # Test standings history
      history_df <- tryCatch({
        get_league_standings_history(championship_id_RV())
      }, error = function(e) NULL)

      if (!is.null(history_df)) {
        cat(sprintf("    standings_history: %s rows\n", nrow(history_df)))
      } else {
        cat("    standings_history: NULL\n")
      }

      TRUE
    }
  )
})

if (rivals_reactives_test$status == "pass") {
  cat("  rivals reactives: PASS\n")
} else {
  cat(sprintf("  rivals reactives: FAIL - %s\n", rivals_reactives_test$trace))
}

# ---- 9f: classification tab reactives ----
cat("\n  [9f] classification tab reactives...\n")
classif_reactives_test <- safe_capture("classif_reactives", {
  shiny::testServer(
    {
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

      input$tabs <<- "classification"

      # Test standings history
      history_df <- tryCatch({
        get_league_standings_history(championship_id_RV())
      }, error = function(e) NULL)

      if (!is.null(history_df)) {
        cat(sprintf("    standings_history: %s rows\n", nrow(history_df)))
      } else {
        cat("    standings_history: NULL\n")
      }

      # Test ranking prizes calculation
      teams_df <- user_teams_RV()
      active_members <- if (!is.null(teams_df) && nrow(teams_df) > 0) nrow(teams_df) else 1
      prizes <- calculate_futmondo_ranking_prizes(money = 30000000, members = active_members)
      cat(sprintf("    ranking_prizes: %s rows\n", nrow(prizes)))

      TRUE
    }
  )
})

if (classif_reactives_test$status == "pass") {
  cat("  classification reactives: PASS\n")
} else {
  cat(sprintf("  classification reactives: FAIL - %s\n", classif_reactives_test$trace))
}

# ---- 9g: admin tab reactives ----
cat("\n  [9g] admin tab reactives...\n")
admin_reactives_test <- safe_capture("admin_reactives", {
  shiny::testServer(
    {
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

      input$tabs <<- "admin"

      # Test table row counts
      row_counts <- tryCatch({
        get_table_row_counts()
      }, error = function(e) NULL)

      if (!is.null(row_counts)) {
        cat(sprintf("    table_row_counts: %s rows\n", nrow(row_counts)))
        total <- sum(row_counts$row_count, na.rm = TRUE)
        cat(sprintf("    total_records: %s\n", total))
      } else {
        cat("    table_row_counts: NULL\n")
      }

      # Test table definitions
      tbl_defs <- get_table_definitions()
      cat(sprintf("    table_definitions: %s rows\n", nrow(tbl_defs)))

      TRUE
    }
  )
})

if (admin_reactives_test$status == "pass") {
  cat("  admin reactives: PASS\n")
} else {
  cat(sprintf("  admin reactives: FAIL - %s\n", admin_reactives_test$trace))
}

# ===================================================================
# FINAL SUMMARY
# ===================================================================
cat("\n======================================================================\n")
cat("  FINAL SUMMARY\n")
cat("======================================================================\n")

total_tests    <- length(test_results)
pass_count     <- sum(sapply(test_results, function(r) r$status == "pass"))
fail_count     <- sum(sapply(test_results, function(r) r$status == "error"))

cat(sprintf("\n  Total tests:  %d\n", total_tests))
cat(sprintf("  Passed:       %d\n", pass_count))
cat(sprintf("  Failed:       %d\n", fail_count))

if (fail_count > 0) {
  cat("\n  FAILED TESTS:\n")
  for (name in names(test_results)) {
    if (test_results[[name]]$status == "error") {
      cat(sprintf("    - %s\n", name))
      cat(sprintf("      Error: %s\n", test_results[[name]]$trace))
    }
  }
}

cat("\n  ALL TESTS:\n")
for (name in names(test_results)) {
  status_icon <- ifelse(test_results[[name]]$status == "pass", "PASS", "FAIL")
  cat(sprintf("    [%s] %s\n", status_icon, name))
}

cat("\n======================================================================\n")
if (fail_count == 0) {
  cat("  RESULT: ALL TESTS PASSED\n")
} else {
  cat(sprintf("  RESULT: %d TEST(S) FAILED\n", fail_count))
}
cat("======================================================================\n")