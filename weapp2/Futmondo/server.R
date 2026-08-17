

function(input, output, session) {
  login_token_RV <- login_Server(id = "login") %>%
    debounce(50)
  
  # Caching Refresh Trigger ----
  refresh_trigger <- reactiveVal(0)
  
  observeEvent(input$refresh_all, {
    clear_api_cache()
    refresh_trigger(refresh_trigger() + 1)
  })
  
  # reactives ----
  ## championship_RV ----
  championship_RV <- reactive({
    req(login_token_RV())
    refresh_trigger() # Dependency to trigger re-fetch on refresh
    championship <- get_championships(login = login_token_RV(), championship_name = NULL)

    # Defer Supabase sync so UI receives championship object instantly
    on.exit({
      tryCatch({
        sync_championship_to_supabase(championship)
      }, error = function(e) {
        print(paste0("[Supabase] Championship sync warning: ", e$message))
      })
    }, add = TRUE)

    return(championship)
  })
  ## user_teams_RV ----
  user_teams_RV <- reactive({
    req(championship_RV())
    refresh_trigger() # Dependency to trigger re-fetch on refresh
    teams <- get_teams(login = login_token_RV(), championship_id = championship_RV()["id"])

    champ_id <- championship_RV()["id"]
    # Defer Supabase sync so UI receives user teams instantly
    on.exit({
      tryCatch({
        sync_user_teams_to_supabase(teams, champ_id)
        log_user_team_history(teams)
      }, error = function(e) {
        print(paste0("[Supabase] Standings sync warning: ", e$message))
      })
    }, add = TRUE)

    return(teams)
  })
  
  ## championship_id_RV ----
  championship_id_RV <- reactive({
    req(championship_RV())
    championship <- championship_RV()
    championship_id <- championship["id"]
    return(championship_id)
  })
  
  ## user_team_id_RV ----
  user_team_id_RV <- reactive({
    req(championship_RV())
    
    championship <- championship_RV()
    userteam_id <- championship["userteam.id"]
    return(userteam_id)
  })
  
  ## user_team_name_RV ----
  user_team_name_RV <- reactive({
    req(championship_RV())
    championship <- championship_RV()
    user_team_name <- championship["userteam.name"]
    return(user_team_name)
  })

  # Background Sync: Full Player Catalog Snapshot (Zero Data Loss) ----
  observe({
    req(login_token_RV())
    req(championship_id_RV())

    # Run defensively in the background
    tryCatch({
      print("[Supabase] Initiating background full-catalog snapshot to prevent data loss...")
      all_players <- get_championship_players(login = login_token_RV(), championship_id = championship_id_RV())

      if (!is.null(all_players) && nrow(all_players) > 0) {
        sync_real_clubs_to_supabase(all_players)
        sync_players_to_supabase(all_players)
        log_player_history(all_players, championship_id_RV())
        print("[Supabase] Success: Full player catalog snapshot successfully logged to history!")
      }
    }, error = function(e) {
      print(paste0("[Supabase] Full-catalog sync warning: ", e$message))
    })
  })

selected_player_RV <- players_in_teams_Server(id = "players_in_teams",
                                                 is_module_active = reactive({
                                                   input$tabs == "yourteam"
                                                 }),
                                                 login_token = login_token_RV,
                                                 championship_id = championship_id_RV,
                                                 user_team_id = user_team_id_RV,
                                                 user_teams_RV = user_teams_RV,
                                                 refresh_trigger = refresh_trigger)

  today_Server(id = "today",
               is_module_active = reactive({
                 input$tabs == "today"
               }),
               login_token = login_token_RV,
               championship_id = championship_id_RV,
               user_team_id = user_team_id_RV,
               user_teams_RV = user_teams_RV,
               refresh_trigger = refresh_trigger)

  market_Server(id = "market",
                is_module_active = reactive({
                  input$tabs == "market"
                }),
                login_token = login_token_RV, 
                championship_id = championship_id_RV, 
                user_team_id = user_team_id_RV, 
                user_teams_RV = user_teams_RV,
                refresh_trigger = refresh_trigger)

players_in_championship_Server(id = "players_in_championship", 
                                  is_module_active = reactive({
                                    input$tabs == "players_in_championship"  
                                  }),
                                  login_token = login_token_RV, 
                                  championship_id = championship_id_RV, 
                                  user_teams_RV = user_teams_RV,
                                  refresh_trigger = refresh_trigger)

  rivals_Server(id = "rivals",
                is_module_active = reactive({
                  input$tabs == "rivals"
                }),
                login_token = login_token_RV,
                championship_id = championship_id_RV,
                user_team_id = user_team_id_RV,
                user_teams_RV = user_teams_RV)

  classification_Server(id = "classification",
                        is_module_active = reactive({
                          input$tabs == "classification"
                        }),
                        login_token = login_token_RV,
                        championship_id = championship_id_RV,
                        user_team_id = user_team_id_RV,
                        user_teams_RV = user_teams_RV)

  admin_Server(id = "admin",
               is_module_active = reactive({
                 req(input$tabs); input$tabs == "admin"
               }),
               login_token = login_token_RV,
               championship_id = championship_id_RV,
               user_team_id = user_team_id_RV,
               user_teams_RV = user_teams_RV)
  # observers ----
  ## observe user_team_id_RV()
observeEvent(login_token_RV(),
                {
                  req(login_token_RV())
                  updateTabsetPanel(inputId = "tabs", selected = "today")
                },
                ignoreNULL = T
   )
  
  # renders----
  ## render menu ----
  output$menu <- shinydashboard::renderMenu({
    # Check if logged-in user matches the admin env var (case-insensitive, trimmed)
    is_admin <- FALSE
    admin_env <- trimws(Sys.getenv("admin"))
    if (admin_env != "" && !is.null(login_token_RV()) && length(login_token_RV()) >= 3) {
      current_user <- trimws(as.character(login_token_RV()[["user_name"]]))
      is_admin <- tolower(current_user) == tolower(admin_env)
    }

    # Standard menu items
    menu_items <- list(
      shinydashboard::menuItem("Login", tabName = "login", icon = icon("right-to-bracket")),
      shinydashboard::menuItem("Today", tabName = "today", icon = icon("bolt")),
      shinydashboard::menuItem("Your team", tabName = "yourteam", icon = icon("users")),
      shinydashboard::menuItem("Market", tabName = "market", icon = icon("money-bill-trend-up")),
      shinydashboard::menuItem("Players", tabName = "players_in_championship", icon = icon("table")),
      shinydashboard::menuItem("Rivals", tabName = "rivals", icon = icon("users-viewfinder")),
      shinydashboard::menuItem("Classification", tabName = "classification", icon = icon("trophy"))
    )

    # Append Admin menu item as the last item if user is admin
    if (is_admin) {
      menu_items <- c(menu_items, list(
        shinydashboard::menuItem("Admin", tabName = "admin", icon = icon("gears"))
      ))
    }

    do.call(shinydashboard::sidebarMenu, c(list(id = "tabs"), menu_items))
  })
}
