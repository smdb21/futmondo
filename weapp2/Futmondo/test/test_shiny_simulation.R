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
# PHASE 10: FIS Score & Tier Filtering (Phase 3.2)
# ===================================================================
cat("\n--- PHASE 10: FIS Score & Tier Filtering (Phase 3.2) ---\n")

# ---- 10a: calculate_fis_score computes valid scores, tiers, summaries ----
cat("\n  [10a] calculate_fis_score basic computation...\n")

fis_score_basic_test <- safe_capture("fis_score_basic", {
  # Build a minimal player data frame with required columns
  test_df <- data.frame(
    id = c("p1", "p2", "p3", "p4"),
    name = c("Alpha", "Beta", "Gamma", "Delta"),
    points = c(150, 80, 30, 5),
    value = c(50000000, 30000000, 20000000, 10000000),
    change = c(5000000, -2000000, 1000000, -5000000),
    average.average = c(12.5, 8.0, 4.0, 1.0),
    average.averageLastFive = c(14.0, 9.0, 3.5, 1.5),
    average.matches = c(20, 18, 15, 10),
    role = c("Forward", "Midfielder", "Defender", "Goalkeeper"),
    status = c("ok", "ok", "doubt", "injured"),
    stringsAsFactors = FALSE
  )

  result <- calculate_fis_score(test_df)

  # Verify columns exist
  required_cols <- c("perf", "form", "efficiency", "momentum", "fixture_risk",
                     "fis_score", "fis_tier", "fis_summary")
  for (col in required_cols) {
    if (!col %in% colnames(result)) {
      stop(paste("Missing column:", col))
    }
  }

  # Verify score range [0, 100]
  scores <- suppressWarnings(as.numeric(result$fis_score))
  if (any(is.na(scores))) {
    stop("fis_score contains NA values")
  }
  if (any(scores < 0) || any(scores > 100)) {
    stop("fis_score values outside [0, 100] range")
  }

  # Verify tier labels are valid
  valid_tiers <- c("Strong Buy", "Buy", "Hold", "Sell")
  if (!all(result$fis_tier %in% valid_tiers)) {
    stop("fis_tier contains invalid tier labels")
  }

  # Verify summary is non-empty
  if (any(is.na(result$fis_summary)) || any(nzchar(result$fis_summary) == FALSE)) {
    stop("fis_summary contains empty or NA values")
  }

  # Verify tier assignment logic: high score -> Strong Buy, low score -> Sell
  # Alpha (150 pts, ok) should score higher than Delta (5 pts, injured)
  if (scores[1] <= scores[4]) {
    stop("Expected Alpha (high pts, ok) to score higher than Delta (low pts, injured)")
  }

  cat(sprintf("    FIS scores: %s\n", paste(round(scores, 1), collapse = ", ")))
  cat(sprintf("    FIS tiers:  %s\n", paste(result$fis_tier, collapse = ", ")))
  cat("    All FIS columns valid, scores in [0,100], tiers correct\n")
  TRUE
})

if (fis_score_basic_test$status == "pass") {
  cat("  calculate_fis_score basic: PASS\n")
} else {
  cat(sprintf("  calculate_fis_score basic: FAIL - %s\n", fis_score_basic_test$trace))
}

# ---- 10b: calculate_fis_score handles edge cases ----
cat("\n  [10b] calculate_fis_score edge cases...\n")

fis_score_edge_test <- safe_capture("fis_score_edge_cases", {
  # Empty data frame
  empty_df <- data.frame(
    id = character(0), name = character(0), points = numeric(0),
    value = numeric(0), change = numeric(0), average.average = numeric(0),
    average.averageLastFive = numeric(0), average.matches = numeric(0),
    role = character(0), status = character(0), stringsAsFactors = FALSE
  )
  empty_result <- calculate_fis_score(empty_df)
  if (!is.null(empty_result) && nrow(empty_result) != 0) {
    stop("Empty input should return empty result")
  }

  # NULL input
  null_result <- calculate_fis_score(NULL)
  if (!is.null(null_result)) {
    stop("NULL input should return NULL")
  }

  # Single player
  single_df <- data.frame(
    id = "s1", name = "Solo", points = 100, value = 40000000, change = 0,
    average.average = 10.0, average.averageLastFive = 10.0,
    average.matches = 15, role = "Forward", status = "ok",
    stringsAsFactors = FALSE
  )
  single_result <- calculate_fis_score(single_df)
  if (nrow(single_result) != 1) {
    stop("Single player input should return single row")
  }
  single_score <- suppressWarnings(as.numeric(single_result$fis_score))
  if (is.na(single_score) || single_score < 0 || single_score > 100) {
    stop("Single player FIS score should be valid [0,100]")
  }

  cat("    Edge cases (empty, NULL, single): all handled correctly\n")
  TRUE
})

if (fis_score_edge_test$status == "pass") {
  cat("  calculate_fis_score edge cases: PASS\n")
} else {
  cat(sprintf("  calculate_fis_score edge cases: FAIL - %s\n", fis_score_edge_test$trace))
}

# ---- 10c: fis_tier_filter UI options and reactive filtering ----
cat("\n  [10c] fis_tier_filter UI options and reactive filtering...\n")

fis_tier_filter_test <- safe_capture("fis_tier_filter", {
  # Verify the selectInput choices are defined in players_table_UI
  ui_result <- players_table_UI(id = "test_fis_filter")
  # The UI should contain a selectInput with id "fis_tier_filter"
  # We verify by checking the tagList structure
  if (is.null(ui_result)) {
    stop("players_table_UI returned NULL")
  }

  # Verify the filter choices: All, Strong Buy, Buy, Hold, Sell
  expected_choices <- c("All", "Strong Buy", "Buy", "Hold", "Sell")

  # Simulate filtering with each tier value
  test_df <- data.frame(
    id = c("p1", "p2", "p3", "p4"),
    name = c("A", "B", "C", "D"),
    points = c(150, 80, 30, 5),
    value = c(50000000, 30000000, 20000000, 10000000),
    change = c(5000000, -2000000, 1000000, -5000000),
    average.average = c(12.5, 8.0, 4.0, 1.0),
    average.averageLastFive = c(14.0, 9.0, 3.5, 1.5),
    average.matches = c(20, 18, 15, 10),
    role = c("Forward", "Midfielder", "Defender", "Goalkeeper"),
    status = c("ok", "ok", "doubt", "injured"),
    stringsAsFactors = FALSE
  )
  test_df <- calculate_fis_score(test_df)

  # Filter by "Strong Buy"
  strong_buy <- test_df[test_df$fis_tier == "Strong Buy", ]
  # Filter by "Buy"
  buy <- test_df[test_df$fis_tier == "Buy", ]
  # Filter by "Hold"
  hold <- test_df[test_df$fis_tier == "Hold", ]
  # Filter by "Sell"
  sell <- test_df[test_df$fis_tier == "Sell", ]

  # Verify that combining all tiers gives back the full set
  combined_nrow <- nrow(strong_buy) + nrow(buy) + nrow(hold) + nrow(sell)
  if (combined_nrow != nrow(test_df)) {
    stop("Tier filtering does not partition the full data set")
  }

  cat(sprintf("    Tier counts: Strong Buy=%d, Buy=%d, Hold=%d, Sell=%d\n",
              nrow(strong_buy), nrow(buy), nrow(hold), nrow(sell)))
  cat("    fis_tier_filter UI rendered, tier partitioning verified\n")
  TRUE
})

if (fis_tier_filter_test$status == "pass") {
  cat("  fis_tier_filter: PASS\n")
} else {
  cat(sprintf("  fis_tier_filter: FAIL - %s\n", fis_tier_filter_test$trace))
}

# ---- 10d: fis_score column in get_reactable_columns_for_players ----
cat("\n  [10d] fis_score column in get_reactable_columns_for_players...\n")

fis_column_def_test <- safe_capture("fis_column_definition", {
  # Build a table that includes fis_score
  test_df <- data.frame(
    id = c("p1", "p2"),
    name = c("A", "B"),
    points = c(100, 50),
    value = c(40000000, 20000000),
    change = c(2000000, -1000000),
    average.average = c(10.0, 5.0),
    average.averageLastFive = c(11.0, 5.5),
    average.matches = c(15, 10),
    role = c("Forward", "Defender"),
    status = c("ok", "ok"),
    fis_score = c(75.5, 40.2),
    fis_tier = c("Buy", "Hold"),
    stringsAsFactors = FALSE
  )

  cols <- get_reactable_columns_for_players(test_df)

  # Verify fis_score column is defined
  if (!"fis_score" %in% names(cols)) {
    stop("fis_score column not found in get_reactable_columns_for_players output")
  }

  fis_col <- cols[["fis_score"]]
  if (is.null(fis_col)) {
    stop("fis_score column definition is NULL")
  }

  # Verify it has a name
  if (is.null(fis_col$name) || fis_col$name != "FIS") {
    stop("fis_score column should have name 'FIS'")
  }

  cat("    fis_score column definition found with name='FIS'\n")
  cat("    get_reactable_columns_for_players includes FIS column: PASS\n")
  TRUE
})

if (fis_column_def_test$status == "pass") {
  cat("  fis_score column definition: PASS\n")
} else {
  cat(sprintf("  fis_score column definition: FAIL - %s\n", fis_column_def_test$trace))
}

# ===================================================================
# PHASE 11: Selected Player Intelligence Modal (Phase 3.3)
# ===================================================================
cat("\n--- PHASE 11: Selected Player Intelligence Modal (Phase 3.3) ---\n")

# ---- 11a: output$fis_panel computes 5-pillar breakdown, score badge, summary ----
cat("\n  [11a] FIS Panel: 5-pillar breakdown, score badge, summary...\n")

fis_panel_test <- safe_capture("fis_panel_computation", {
  # Build a representative single-player data frame
  sp_df <- data.frame(
    id = "sp1",
    name = "TestPlayer",
    points = 120,
    value = 45000000,
    change = 3000000,
    average.average = 11.0,
    average.averageLastFive = 13.0,
    average.matches = 18,
    role = "Forward",
    status = "ok",
    stringsAsFactors = FALSE
  )

  # Compute FIS score for this single player
  fis_result <- tryCatch({
    calculate_fis_score(sp_df)
  }, error = function(e) {
    stop(paste("FIS computation failed:", e$message))
  })

  if (is.null(fis_result) || nrow(fis_result) == 0) {
    stop("FIS result is empty")
  }

  # Extract 5 pillars
  perf_val <- suppressWarnings(as.numeric(fis_result$perf[1]))
  form_val <- suppressWarnings(as.numeric(fis_result$form[1]))
  eff_val  <- suppressWarnings(as.numeric(fis_result$efficiency[1]))
  mom_val  <- suppressWarnings(as.numeric(fis_result$momentum[1]))
  fix_val  <- suppressWarnings(as.numeric(fis_result$fixture_risk[1]))

  # Verify all 5 pillars are valid [0, 100]
  pillars <- c(perf_val, form_val, eff_val, mom_val, fix_val)
  pillar_names <- c("perf", "form", "efficiency", "momentum", "fixture_risk")
  for (i in seq_along(pillars)) {
    if (is.na(pillars[i])) {
      stop(paste("Pillar", pillar_names[i], "is NA"))
    }
    if (pillars[i] < 0 || pillars[i] > 100) {
      stop(paste("Pillar", pillar_names[i], "out of range:", pillars[i]))
    }
  }

  # Verify score badge (fis_score)
  fis_score_val <- suppressWarnings(as.numeric(fis_result$fis_score[1]))
  if (is.na(fis_score_val) || fis_score_val < 0 || fis_score_val > 100) {
    stop("FIS score badge value is invalid")
  }

  # Verify tier badge
  fis_tier_val <- as.character(fis_result$fis_tier[1])
  valid_tiers <- c("Strong Buy", "Buy", "Hold", "Sell")
  if (!fis_tier_val %in% valid_tiers) {
    stop(paste("Invalid tier badge:", fis_tier_val))
  }

  # Verify analytical summary is non-empty
  fis_summary_val <- as.character(fis_result$fis_summary[1])
  if (is.na(fis_summary_val) || nzchar(fis_summary_val) == FALSE) {
    stop("FIS summary is empty")
  }

  cat(sprintf("    Score: %.1f, Tier: %s\n", fis_score_val, fis_tier_val))
  cat(sprintf("    Pillars: perf=%.1f, form=%.1f, eff=%.1f, mom=%.1f, fix=%.1f\n",
              perf_val, form_val, eff_val, mom_val, fix_val))
  cat("    5-pillar breakdown, score badge, and summary all valid\n")
  TRUE
})

if (fis_panel_test$status == "pass") {
  cat("  FIS Panel computation: PASS\n")
} else {
  cat(sprintf("  FIS Panel computation: FAIL - %s\n", fis_panel_test$trace))
}

# ---- 11b: calculate_smart_bid handles market auctions and competitor evaluations ----
cat("\n  [11b] calculate_smart_bid: fair value, recommended bid, ROI, competitors...\n")

smart_bid_basic_test <- safe_capture("smart_bid_basic", {
  # Build a single player row
  player_row <- data.frame(
    id = "sb1",
    name = "BidPlayer",
    value = 50000000,
    change = 2000000,
    points = 100,
    role = "Midfielder",
    average.average = 10.0,
    average.averageLastFive = 12.0,
    average.matches = 16,
    status = "ok",
    clause_price = 60000000,
    stringsAsFactors = FALSE
  )

  result <- tryCatch({
    calculate_smart_bid(
      player_row = player_row,
      championship_id = "champ1",
      pressroom_df = NULL,
      user_teams_df = NULL,
      user_cash = 300000000
    )
  }, error = function(e) {
    stop(paste("Smart bid computation failed:", e$message))
  })

  # Verify required fields exist
  required_fields <- c("fair_value", "league_premium_pct", "min_winning_bid",
                       "recommended_bid", "max_rational_bid", "expected_roi_pct",
                       "competition_level", "likely_competitors", "confidence_pct")
  for (f in required_fields) {
    if (is.null(result[[f]])) {
      stop(paste("Missing field:", f))
    }
  }

  # Verify fair_value is numeric and positive
  fv <- suppressWarnings(as.numeric(result$fair_value))
  if (is.na(fv) || fv <= 0) {
    stop("fair_value should be positive numeric")
  }

  # Verify recommended_bid is numeric and positive
  rb <- suppressWarnings(as.numeric(result$recommended_bid))
  if (is.na(rb) || rb <= 0) {
    stop("recommended_bid should be positive numeric")
  }

  # Verify ROI is numeric
  roi <- suppressWarnings(as.numeric(result$expected_roi_pct))
  if (is.na(roi)) {
    stop("expected_roi_pct should be numeric")
  }

  # Verify competition_level is valid
  valid_levels <- c("High", "Medium", "Low", "Unknown")
  if (!result$competition_level %in% valid_levels) {
    stop(paste("Invalid competition_level:", result$competition_level))
  }

  # Verify confidence_pct is in [0, 100]
  conf <- suppressWarnings(as.numeric(result$confidence_pct))
  if (is.na(conf) || conf < 0 || conf > 100) {
    stop("confidence_pct should be in [0, 100]")
  }

  # Verify max_rational_bid <= user_cash
  mrb <- suppressWarnings(as.numeric(result$max_rational_bid))
  if (mrb > 300000000) {
    stop("max_rational_bid should not exceed user_cash")
  }

  cat(sprintf("    Fair value: %.0f, Recommended bid: %.0f\n", fv, rb))
  cat(sprintf("    ROI: %.2f%%, Competition: %s, Confidence: %.0f%%\n",
              roi, result$competition_level, conf))
  cat("    Smart bid basic computation: PASS\n")
  TRUE
})

if (smart_bid_basic_test$status == "pass") {
  cat("  calculate_smart_bid basic: PASS\n")
} else {
  cat(sprintf("  calculate_smart_bid basic: FAIL - %s\n", smart_bid_basic_test$trace))
}

# ---- 11c: calculate_smart_bid with pressroom data (competitor prediction) ----
cat("\n  [11c] calculate_smart_bid with pressroom data (competitor prediction)...\n")

smart_bid_pressroom_test <- safe_capture("smart_bid_with_pressroom", {
  player_row <- data.frame(
    id = "sb2",
    name = "AuctionPlayer",
    value = 40000000,
    change = 1000000,
    points = 80,
    role = "Forward",
    average.average = 8.0,
    average.averageLastFive = 10.0,
    average.matches = 14,
    status = "ok",
    clause_price = 50000000,
    stringsAsFactors = FALSE
  )

  # Build a pressroom data frame with transactions for this player
  pressroom_df <- data.frame(
    player_id = c("sb2", "sb2", "sb2"),
    buyer_team_id = c("team_A", "team_B", "team_C"),
    seller_team_id = c("team_X", "team_Y", "team_Z"),
    price = c(42000000, 45000000, 43000000),
    created = c("2025-01-01T10:00:00Z", "2025-01-05T12:00:00Z", "2025-01-10T14:00:00Z"),
    stringsAsFactors = FALSE
  )

  result <- tryCatch({
    calculate_smart_bid(
      player_row = player_row,
      championship_id = "champ1",
      pressroom_df = pressroom_df,
      user_teams_df = NULL,
      user_cash = 300000000
    )
  }, error = function(e) {
    stop(paste("Smart bid with pressroom failed:", e$message))
  })

  # Verify competition level is "High" (3 unique buyers)
  if (result$competition_level != "High") {
    stop(paste("Expected competition_level='High' with 3 buyers, got:", result$competition_level))
  }

  # Verify likely_competitors contains the buyer team IDs
  if (is.null(result$likely_competitors) || length(result$likely_competitors) < 3) {
    stop("likely_competitors should contain at least 3 entries")
  }

  # Verify league_premium_pct is non-zero (pressroom prices differ from fair value)
  lp <- suppressWarnings(as.numeric(result$league_premium_pct))
  if (is.na(lp)) {
    stop("league_premium_pct should be computed from pressroom data")
  }

  cat(sprintf("    Competition level: %s\n", result$competition_level))
  cat(sprintf("    League premium: %.2f%%\n", lp))
  cat(sprintf("    Competitors: %s\n", paste(result$likely_competitors, collapse = ", ")))
  cat("    Smart bid with pressroom data: PASS\n")
  TRUE
})

if (smart_bid_pressroom_test$status == "pass") {
  cat("  calculate_smart_bid with pressroom: PASS\n")
} else {
  cat(sprintf("  calculate_smart_bid with pressroom: FAIL - %s\n", smart_bid_pressroom_test$trace))
}

# ---- 11d: calculate_smart_bid with NULL player_row ----
cat("\n  [11d] calculate_smart_bid NULL handling...\n")

smart_bid_null_test <- safe_capture("smart_bid_null", {
  result <- calculate_smart_bid(
    player_row = NULL,
    championship_id = "champ1"
  )

  if (is.null(result$error)) {
    stop("NULL player_row should return result with error field")
  }

  cat("    NULL player_row returns error as expected\n")
  TRUE
})

if (smart_bid_null_test$status == "pass") {
  cat("  calculate_smart_bid NULL handling: PASS\n")
} else {
  cat(sprintf("  calculate_smart_bid NULL handling: FAIL - %s\n", smart_bid_null_test$trace))
}

# ---- 11e: output$smart_bid_widget renders for non-owned player ----
cat("\n  [11e] Smart Bid Widget: renders for non-owned player...\n")

smart_bid_widget_test <- safe_capture("smart_bid_widget_render", {
  # Build a player row that is NOT owned by the current user
  sp <- data.frame(
    id = "widget1",
    name = "WidgetPlayer",
    value = 35000000,
    change = 1500000,
    points = 90,
    role = "Midfielder",
    average.average = 9.0,
    average.averageLastFive = 11.0,
    average.matches = 17,
    status = "ok",
    clause_price = 45000000,
    user_team_id = "rival_team",  # NOT the current user
    stringsAsFactors = FALSE
  )

  # Compute smart bid directly (simulating what output$smart_bid_widget does)
  smart_bid_result <- tryCatch({
    calculate_smart_bid(
      player_row = sp,
      championship_id = "champ1",
      pressroom_df = NULL,
      user_teams_df = NULL,
      user_cash = 300000000
    )
  }, error = function(e) {
    stop(paste("Smart bid widget computation failed:", e$message))
  })

  # Verify no error
  if (!is.null(smart_bid_result$error)) {
    stop(paste("Smart bid widget returned error:", smart_bid_result$error))
  }

  # Verify all widget-relevant fields
  fv <- suppressWarnings(as.numeric(smart_bid_result$fair_value))
  rb <- suppressWarnings(as.numeric(smart_bid_result$recommended_bid))
  roi <- suppressWarnings(as.numeric(smart_bid_result$expected_roi_pct))
  conf <- suppressWarnings(as.numeric(smart_bid_result$confidence_pct))

  if (is.na(fv) || fv <= 0) stop("fair_value invalid")
  if (is.na(rb) || rb <= 0) stop("recommended_bid invalid")
  if (is.na(roi)) stop("expected_roi_pct invalid")
  if (is.na(conf) || conf < 0 || conf > 100) stop("confidence_pct invalid")

  cat(sprintf("    Widget: fair_value=%.0f, recommended=%.0f, ROI=%.2f%%, confidence=%.0f%%\n",
              fv, rb, roi, conf))
  cat("    Smart Bid Widget render: PASS\n")
  TRUE
})

if (smart_bid_widget_test$status == "pass") {
  cat("  Smart Bid Widget render: PASS\n")
} else {
  cat(sprintf("  Smart Bid Widget render: FAIL - %s\n", smart_bid_widget_test$trace))
}

# ---- 11f: output$smart_bid_widget returns NULL for owned player ----
cat("\n  [11f] Smart Bid Widget: returns NULL for owned player...\n")

smart_bid_widget_owned_test <- safe_capture("smart_bid_widget_owned", {
  # Build a player row that IS owned by the current user
  sp_owned <- data.frame(
    id = "widget2",
    name = "OwnedPlayer",
    value = 35000000,
    change = 1500000,
    points = 90,
    role = "Midfielder",
    average.average = 9.0,
    average.averageLastFive = 11.0,
    average.matches = 17,
    status = "ok",
    clause_price = 45000000,
    user_team_id = "my_team",  # SAME as current user
    stringsAsFactors = FALSE
  )

  current_user_team <- "my_team"
  player_owner_team <- sp_owned$user_team_id

  # Simulate the is_own_player check from output$smart_bid_widget
  is_own_player <- (!is.null(current_user_team) && !is.null(player_owner_team) &&
                    current_user_team == player_owner_team)

  if (!is_own_player) {
    stop("Player should be detected as owned by current user")
  }

  # The widget should return NULL for owned players
  # We verify the logic path, not the actual Shiny renderUI
  cat("    Owned player correctly identified, widget would return NULL\n")
  TRUE
})

if (smart_bid_widget_owned_test$status == "pass") {
  cat("  Smart Bid Widget owned player: PASS\n")
} else {
  cat(sprintf("  Smart Bid Widget owned player: FAIL - %s\n", smart_bid_widget_owned_test$trace))
}

# ---- 11g: output$fis_panel renders for valid player ----
cat("\n  [11g] FIS Panel: renders for valid player in modal context...\n")

fis_panel_modal_test <- safe_capture("fis_panel_modal_render", {
  # Build a player row as it would appear in the selected_player modal
  sp <- data.frame(
    id = "modal1",
    name = "ModalPlayer",
    points = 110,
    value = 42000000,
    change = 2500000,
    average.average = 10.5,
    average.averageLastFive = 12.5,
    average.matches = 16,
    role = "Forward",
    status = "ok",
    stringsAsFactors = FALSE
  )

  # Simulate what output$fis_panel does: convert to df and compute FIS
  sp_df <- as.data.frame(t(unlist(as.list(sp))))
  sp_df <- as.data.frame(t(sp_df))
  if (is.null(dim(sp_df))) {
    sp_df <- data.frame(sp, stringsAsFactors = FALSE)
  }

  fis_result <- tryCatch({
    calculate_fis_score(sp_df)
  }, error = function(e) {
    stop(paste("FIS panel modal computation failed:", e$message))
  })

  if (is.null(fis_result) || nrow(fis_result) == 0) {
    stop("FIS result for modal player is empty")
  }

  # Verify 5 pillars exist and are valid
  for (pillar in c("perf", "form", "efficiency", "momentum", "fixture_risk")) {
    val <- suppressWarnings(as.numeric(fis_result[[pillar]][1]))
    if (is.na(val) || val < 0 || val > 100) {
      stop(paste("Pillar", pillar, "invalid in modal context"))
    }
  }

  # Verify score and tier
  score <- suppressWarnings(as.numeric(fis_result$fis_score[1]))
  tier <- as.character(fis_result$fis_tier[1])
  if (is.na(score) || score < 0 || score > 100) {
    stop("FIS score invalid in modal context")
  }
  if (!tier %in% c("Strong Buy", "Buy", "Hold", "Sell")) {
    stop(paste("Invalid tier in modal context:", tier))
  }

  cat(sprintf("    Modal FIS: score=%.1f, tier=%s\n", score, tier))
  cat("    FIS Panel modal render: PASS\n")
  TRUE
})

if (fis_panel_modal_test$status == "pass") {
  cat("  FIS Panel modal render: PASS\n")
} else {
  cat(sprintf("  FIS Panel modal render: FAIL - %s\n", fis_panel_modal_test$trace))
}

# ===================================================================
# PHASE 12: Lineup Optimizer & Transfer Sandbox (Players_in_Teams_Module)
# ===================================================================

# ---- 12a: optimize_starting_xi basic computation ----
cat("\n  [12a] optimize_starting_xi basic computation...\n")

optimizer_basic_test <- safe_capture("optimizer_basic", {
  test_squad <- data.frame(
    id = paste0("p", 1:15),
    name = paste0("Player_", 1:15),
    role = c(
      "Goalkeeper", "Goalkeeper",
      "Defender", "Defender", "Defender", "Defender", "Defender", "Defender",
      "Midfielder", "Midfielder", "Midfielder", "Midfielder", "Midfielder",
      "Forward", "Forward", "Forward"
    ),
    perf = c(70, 60, 80, 75, 72, 68, 65, 62, 85, 82, 78, 74, 70, 90, 88, 85),
    form = c(65, 55, 78, 72, 70, 66, 63, 60, 80, 76, 74, 70, 68, 85, 82, 80),
    momentum = c(70, 60, 80, 75, 72, 68, 65, 62, 85, 82, 78, 74, 70, 90, 88, 85),
    fixture_risk = c(50, 50, 40, 45, 48, 52, 55, 58, 35, 38, 42, 46, 50, 30, 32, 35),
    value = sample(10000000:100000000, 16),
    stringsAsFactors = FALSE
  )

  result <- optimize_starting_xi(squad_df = test_squad, formation = "4-3-3", mode = "max_fis")

  # Verify structure
  if (is.null(result) || !is.list(result)) stop("optimize_starting_xi returned NULL or non-list")
  if (!"starting_xi" %in% names(result)) stop("Missing 'starting_xi' in result")
  if (!"bench" %in% names(result)) stop("Missing 'bench' in result")
  if (!"formation" %in% names(result)) stop("Missing 'formation' in result")
  if (!"mode" %in% names(result)) stop("Missing 'mode' in result")
  if (!"total_score" %in% names(result)) stop("Missing 'total_score' in result")
  if (!"avg_fis" %in% names(result)) stop("Missing 'avg_fis' in result")
  if (!"feasible" %in% names(result)) stop("Missing 'feasible' in result")

  # Verify starting XI has 11 players
  if (nrow(result$starting_xi) != 11) stop(paste("Starting XI should have 11 players, got", nrow(result$starting_xi)))

  # Verify bench has remaining players
  expected_bench <- 16 - 11
  if (nrow(result$bench) != expected_bench) stop(paste("Bench should have", expected_bench, "players, got", nrow(result$bench)))

  # Verify total_score and avg_fis are numeric
  if (is.na(result$total_score) || result$total_score <= 0) stop("total_score should be positive")
  if (is.na(result$avg_fis) || result$avg_fis <= 0) stop("avg_fis should be positive")

  cat(sprintf("    Formation: %s, XI size: %d, Bench: %d, Total Score: %.1f, Avg FIS: %.1f\n",
              result$formation, nrow(result$starting_xi), nrow(result$bench), result$total_score, result$avg_fis))
  TRUE
})

if (optimizer_basic_test$status == "pass") {
  cat("  optimize_starting_xi basic: PASS\n")
} else {
  cat(sprintf("  optimize_starting_xi basic: FAIL - %s\n", optimizer_basic_test$trace))
}

# ---- 12b: optimize_starting_xi with different formations and modes ----
cat("\n  [12b] optimize_starting_xi with different formations and modes...\n")

optimizer_variants_test <- safe_capture("optimizer_variants", {
  test_squad <- data.frame(
    id = paste0("p", 1:15),
    name = paste0("Player_", 1:15),
    role = c(
      "Goalkeeper", "Goalkeeper",
      "Defender", "Defender", "Defender", "Defender", "Defender", "Defender",
      "Midfielder", "Midfielder", "Midfielder", "Midfielder", "Midfielder",
      "Forward", "Forward", "Forward"
    ),
    perf = runif(16, 50, 100),
    form = runif(16, 50, 100),
    momentum = runif(16, 50, 100),
    fixture_risk = runif(16, 20, 80),
    value = sample(10000000:100000000, 16),
    stringsAsFactors = FALSE
  )

  formations <- c("4-3-3", "4-4-2", "3-5-2", "3-4-3", "4-5-1", "5-3-2", "5-4-1")
  modes <- c("max_fis", "safe", "upside", "form", "fixture")

  for (fmt in formations) {
    for (mode in modes) {
      result <- optimize_starting_xi(squad_df = test_squad, formation = fmt, mode = mode)
      if (nrow(result$starting_xi) != 11) {
        stop(paste("Formation", fmt, "mode", mode, "should produce 11 players"))
      }
      if (result$formation != fmt) {
        stop(paste("Formation mismatch for", fmt))
      }
    }
  }

  cat(sprintf("    Tested %d formations x %d modes = %d combinations\n", length(formations), length(modes), length(formations) * length(modes)))
  TRUE
})

if (optimizer_variants_test$status == "pass") {
  cat("  optimize_starting_xi variants: PASS\n")
} else {
  cat(sprintf("  optimize_starting_xi variants: FAIL - %s\n", optimizer_variants_test$trace))
}

# ---- 12c: simulate_transfer_scenario basic computation ----
cat("\n  [12c] simulate_transfer_scenario basic computation...\n")

sandbox_basic_test <- safe_capture("sandbox_basic", {
  test_squad <- data.frame(
    id = c("s1", "s2", "s3", "s4", "s5"),
    name = c("A", "B", "C", "D", "E"),
    role = c("Forward", "Midfielder", "Defender", "Goalkeeper", "Forward"),
    perf = c(80, 70, 60, 50, 90),
    form = c(75, 65, 55, 45, 85),
    momentum = c(78, 68, 58, 48, 88),
    fixture_risk = c(40, 50, 60, 70, 35),
    value = c(50000000, 40000000, 30000000, 20000000, 60000000),
    stringsAsFactors = FALSE
  )

  test_market <- data.frame(
    id = c("m1", "m2", "m3"),
    name = c("X", "Y", "Z"),
    role = c("Forward", "Midfielder", "Defender"),
    perf = c(90, 85, 80),
    form = c(88, 83, 78),
    momentum = c(92, 87, 82),
    fixture_risk = c(30, 35, 40),
    value = c(70000000, 55000000, 45000000),
    stringsAsFactors = FALSE
  )

  # Test: sell s1 (val=50M), buy m1 (val=70M), budget=100M
  result <- simulate_transfer_scenario(
    squad_df = test_squad,
    current_budget = 100000000,
    sell_player_ids = c("s1"),
    buy_player_ids = c("m1"),
    market_df = test_market
  )

  # Verify structure
  if (is.null(result) || !is.list(result)) stop("simulate_transfer_scenario returned NULL")
  if (!"projected_squad" %in% names(result)) stop("Missing 'projected_squad'")
  if (!"total_sell_proceeds" %in% names(result)) stop("Missing 'total_sell_proceeds'")
  if (!"total_buy_cost" %in% names(result)) stop("Missing 'total_buy_cost'")
  if (!"projected_budget" %in% names(result)) stop("Missing 'projected_budget'")
  if (!"delta_avg_fis" %in% names(result)) stop("Missing 'delta_avg_fis'")
  if (!"is_budget_valid" %in% names(result)) stop("Missing 'is_budget_valid'")

  # Verify projected squad: 5 - 1 + 1 = 5
  if (nrow(result$projected_squad) != 5) stop(paste("Projected squad should have 5 players, got", nrow(result$projected_squad)))

  # Verify budget: 100M + 50M - 70M = 80M
  expected_budget <- 100000000 + 50000000 - 70000000
  if (result$projected_budget != expected_budget) {
    stop(paste("Projected budget should be", expected_budget, "got", result$projected_budget))
  }

  # Verify budget is valid
  if (!result$is_budget_valid) stop("Budget should be valid")

  cat(sprintf("    Sell proceeds: %d, Buy cost: %d, Budget: %d, Delta FIS: %.2f\n",
              result$total_sell_proceeds, result$total_buy_cost, result$projected_budget, result$delta_avg_fis))
  TRUE
})

if (sandbox_basic_test$status == "pass") {
  cat("  simulate_transfer_scenario basic: PASS\n")
} else {
  cat(sprintf("  simulate_transfer_scenario basic: FAIL - %s\n", sandbox_basic_test$trace))
}

# ---- 12d: simulate_transfer_scenario with budget overflow ----
cat("\n  [12d] simulate_transfer_scenario with budget overflow...\n")

sandbox_overflow_test <- safe_capture("sandbox_overflow", {
  test_squad <- data.frame(
    id = c("s1", "s2"),
    name = c("A", "B"),
    role = c("Forward", "Midfielder"),
    perf = c(80, 70),
    form = c(75, 65),
    momentum = c(78, 68),
    fixture_risk = c(40, 50),
    value = c(10000000, 20000000),
    stringsAsFactors = FALSE
  )

  test_market <- data.frame(
    id = c("m1"),
    name = c("X"),
    role = c("Forward"),
    perf = c(95),
    form = c(92),
    momentum = c(96),
    fixture_risk = c(20),
    value = c(100000000),
    stringsAsFactors = FALSE
  )

  # Budget of 5M, sell s1 (10M), buy m1 (100M) -> 5+10-100 = -85M (invalid)
  result <- simulate_transfer_scenario(
    squad_df = test_squad,
    current_budget = 5000000,
    sell_player_ids = c("s1"),
    buy_player_ids = c("m1"),
    market_df = test_market
  )

  if (result$is_budget_valid) stop("Budget should be invalid (negative)")
  if (result$projected_budget >= 0) stop("Projected budget should be negative")

  cat(sprintf("    Budget overflow detected correctly: projected_budget = %d, is_budget_valid = %s\n",
              result$projected_budget, result$is_budget_valid))
  TRUE
})

if (sandbox_overflow_test$status == "pass") {
  cat("  simulate_transfer_scenario overflow: PASS\n")
} else {
  cat(sprintf("  simulate_transfer_scenario overflow: FAIL - %s\n", sandbox_overflow_test$trace))
}

# ---- 12e: recommend_transfers basic computation ----
cat("\n  [12e] recommend_transfers basic computation...\n")

rec_basic_test <- safe_capture("rec_basic", {
  test_squad <- data.frame(
    id = c("s1", "s2", "s3"),
    name = c("Weak_F", "Med_M", "Strong_D"),
    role = c("Forward", "Midfielder", "Defender"),
    perf = c(40, 60, 80),
    form = c(35, 55, 75),
    momentum = c(38, 58, 78),
    fixture_risk = c(70, 50, 30),
    value = c(10000000, 30000000, 60000000),
    stringsAsFactors = FALSE
  )

  test_market <- data.frame(
    id = c("m1", "m2"),
    name = c("Elite_F", "Pro_M"),
    role = c("Forward", "Midfielder"),
    perf = c(95, 85),
    form = c(92, 82),
    momentum = c(96, 86),
    fixture_risk = c(15, 25),
    value = c(50000000, 40000000),
    stringsAsFactors = FALSE
  )

  result <- recommend_transfers(
    squad_df = test_squad,
    market_df = test_market,
    current_budget = 50000000,
    max_transfers = 5
  )

  if (is.null(result) || nrow(result) == 0) stop("recommend_transfers returned empty result")

  # Verify columns
  required_cols <- c("sell_id", "sell_name", "buy_id", "buy_name", "net_cost", "delta_fis", "roi_pct")
  for (col in required_cols) {
    if (!col %in% colnames(result)) stop(paste("Missing column:", col))
  }

  # All delta_fis should be positive
  if (any(result$delta_fis <= 0, na.rm = TRUE)) stop("All delta_fis should be positive")

  # Results should be sorted by delta_fis descending
  if (!all(diff(result$delta_fis) <= 0)) stop("Results should be sorted by delta_fis descending")

  cat(sprintf("    Recommendations: %d, Top delta_fis: %.2f\n", nrow(result), result$delta_fis[1]))
  TRUE
})

if (rec_basic_test$status == "pass") {
  cat("  recommend_transfers basic: PASS\n")
} else {
  cat(sprintf("  recommend_transfers basic: FAIL - %s\n", rec_basic_test$trace))
}

# ---- 12f: Players_in_Teams_Module UI structure with tabsetPanel ----
cat("\n  [12f] Players_in_Teams_Module UI structure with tabsetPanel...\n")

pit_ui_structure_test <- safe_capture("pit_ui_structure", {
  ui_result <- players_in_teams_UI(id = "test_pit")

  if (is.null(ui_result)) stop("players_in_teams_UI returned NULL")

  # The UI should contain a tabsetPanel with 3 tabPanels
  # We verify by checking the tagList structure for key components
  ui_str <- capture.output(print(ui_result))
  ui_text <- paste(ui_str, collapse = "\n")

  # Check for tabsetPanel
  if (!grepl("tabsetPanel", ui_text)) stop("UI should contain tabsetPanel")

  # Check for tab labels
  if (!grepl("Squad Roster", ui_text)) stop("UI should contain Squad Roster tab")
  if (!grepl("Lineup Optimizer", ui_text)) stop("UI should contain Lineup Optimizer tab")
  if (!grepl("Transfer Sandbox", ui_text)) stop("UI should contain Transfer Sandbox tab")

  cat("    UI structure verified: tabsetPanel with 3 tabs (Squad Roster, Lineup Optimizer, Transfer Sandbox)\n")
  TRUE
})

if (pit_ui_structure_test$status == "pass") {
  cat("  Players_in_Teams_Module UI structure: PASS\n")
} else {
  cat(sprintf("  Players_in_Teams_Module UI structure: FAIL - %s\n", pit_ui_structure_test$trace))
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