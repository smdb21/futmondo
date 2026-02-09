
function(input, output, session) {
  login_token_RV <- login_Server(id = "login") %>%
    debounce(1000)

  # observers ----


  # reactives ----
  ## championship_RV ----
  championship_RV <- reactive({
    req( login_token_RV())
    championship <- get_championships(login = login_token_RV(), championship_name = NULL)#"OHY CAMPEÓN ")
    return(championship)
  })
  ## teams_RV ----
  teams_RV <- reactive({
    browser()
    req(championship_RV())
    teams <- get_teams(login = login_token_RV(), championship_id = championship_RV()["id"])
    return(teams)
  })

  ## championship_id_RV ----
  championship_id_RV <- reactive({
    req(championship_RV())
    championship <- championship_RV()
    championship_id <- championship['id']
    return(championship_id)
  })

  ## championship_id_RV ----
  user_team_id_RV <- reactive({
    req(championship_RV())

    championship <- championship_RV()
    userteam_id <- championship['userteam.id']
    return(userteam_id)
  })

  ## user_team_name_RV ----
  user_team_name_RV <- reactive({
    req(championship_RV())
    championship <- championship_RV()
    user_team_name <- championship['userteam.name']
    return(user_team_name)
  })

  selected_player_RV <- players_in_teams_Server(id = "players_in_teams", login_token = login_token_RV, championship_id = championship_id_RV, user_team_id = user_team_id_RV)

  selected_player_Server(id = "selected_player", login_token = login_token_RV, selected_player = selected_player_RV)

  market_Server(id = "market", login_token = login_token_RV, championship_id = championship_id_RV, user_team_id = user_team_id_RV)
  
  players_in_championship_Server(id = "players_in_championship", login_token = login_token_RV, championship_id = championship_id_RV)
  # observers ----
  ## observe user_team_id_RV()
  observeEvent(login_token_RV(),
    {
      req(login_token_RV())
      updateTabsetPanel(inputId = "tabs", selected = "yourteam")
    },
    ignoreNULL = T
  )
  # observe selected_player_RV() to open popup with           selected_player_UI(id = "selected_player")
  observeEvent(selected_player_RV(),
    {
      req(selected_player_RV())
      showModal(modalDialog(
        title = "Selected player",
        selected_player_UI(id = "selected_player"),
        easyClose = TRUE,
        size = "l"
      ))
    },
    ignoreNULL = TRUE
  )
  # renders----
  ## render menu ----
  output$menu <- shinydashboard::renderMenu({
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Login", tabName = "login", icon = icon("right-to-bracket")),
      shinydashboard::menuItem("Your team", tabName = "yourteam", icon = icon("users")),
      shinydashboard::menuItem("Market", tabName = "market", icon = icon("money-bill-trend-up")),
      shinydashboard::menuItem("Players", tabName = "players_in_championship", icon = icon("table"))
    )
  })
}
