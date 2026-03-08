library(reactable)

selected_player_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # shinydashboardPlus::box(
    #   width = 12,
    #   solidHeader = TRUE,
    #   reactableOutput(ns("selected_player_table"))
    # ),
    fluidRow(
      column(
        3,
        uiOutput(ns("team"))
      )
    ),
    userBox(
      id = ns("selected_player_box"),
      width = 12,
      title = userDescription(
        title = "Nadia Carmichael",
        subtitle = "lead Developer",
        type = 2,
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
      ),
      status = "primary",
      gradient = TRUE,
      background = "light-blue",
      boxToolSize = "xl",
      collapsible = FALSE,
      footer = fluidRow(
        column(4, uiOutput(ns("player_points_description_box"))),
        column(4, uiOutput(ns("player_last_points_description_box"))),
        column(4, uiOutput(ns("player_value_description_box")))
      )
    )
  )
}


selected_player_Server <- function(id, selected_player) {
  moduleServer(id, function(input, output, session) {
    observeEvent(
      {
        selected_player()
      },
      {
        # update selected_player_box
        selected_player <- selected_player()
        req(selected_player)
        print(paste0("Selected player: ", selected_player$name))
        player_name <- selected_player$name
        role_text <- selected_player$role
        if (!is.na(selected_player$role2) && selected_player$role2 != "") {
          role_text <- paste(role_text, selected_player$role2, sep = ", ")
        }

        shinydashboardPlus::updateBox(
          id = "selected_player_box",
          action = "update",
          options = list(
            title = shinydashboardPlus::userDescription(
              title = player_name,
              subtitle = role_text,
              type = 1,
              image = paste0(PHOTO_URL, "/", selected_player$photo)
            ),
            status = "red",
            background = NULL,
            width = 12
          )
        )
      }
    )
    ## render team ----
    output$team <- renderUI({
      selected_player <- selected_player()
      req(selected_player)
      if ("team" %in% colnames(selected_player)) {
        team <- selected_player$team

        team_image_name <- get_team_image_name(team)
        if (!is.null(team)) {
          team_image <- img(src = paste0(TEAM_LOGO_URL, team_image_name, ".png"), style = "height: 24px;", alt = team)

          ret <- tagList(
            team_image,
            team
          )
          return(ret)
        }
      }
      return(NULL)
    })
    ## render player_points_description_box ----
    output$player_points_description_box <- renderUI({
      selected_player <- selected_player()
      req(selected_player)
      ret <- tagList()
      points <- selected_player$points

      block <- descriptionBlock(
        header = points,
        number = NULL, # points,
        numberColor = "black",
        text = "Total Points"
      )
      ret <- tagList(
        ret,
        block
      )
      return(ret)
    })
    ## render player_last_points_description_box ----
    output$player_last_points_description_box <- renderUI({
      selected_player <- selected_player()
      req(selected_player)
      ret <- tagList()
      total_last_points <- selected_player$average.total
      avg_last_points <- selected_player$average.averageLastFive

      block <- descriptionBlock(
        header = paste0(total_last_points, " (Avg: ", avg_last_points, ")"),
        number = NULL,
        numberColor = "black",
        text = "Last 5 matches"
      )
      ret <- tagList(
        ret,
        block
      )
      return(ret)
    })
    ## render player_value_description_box ----
    output$player_value_description_box <- renderUI({
      selected_player <- selected_player()
      req(selected_player)
      value <- selected_player$value
      change <- selected_player$change
      change_pct <- selected_player$change_by_value * 100
      if (change > 0) {
        icon <- icon("caret-up")
        number_color <- "green"
      } else if (change < 0) {
        icon <- icon("caret-down")
        number_color <- "red"
      } else {
        icon <- NULL
        number_color <- "black"
      }
      descriptionBlock(
        header = format_currency(value),
        number = paste0(add_sign(format_currency(change)), " (", round(change_pct, 2), "%)"),
        numberColor = number_color,
        numberIcon = icon,
        text = "Value"
      )
    })

    ## render selected_player_table ----
    output$selected_player_table <- renderReactable({
      selected_player <- selected_player()
      req(selected_player)
      selected_player <- selected_player %>%
        dplyr::select(photo, name, role, role2, points, value)
      reactable(selected_player,
        searchable = TRUE,
        defaultPageSize = 20,
        pagination = TRUE,
        striped = TRUE,
        bordered = TRUE,
        highlight = TRUE,
        compact = TRUE,
        fullWidth = FALSE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(5, 10, 20, 50),
        showPagination = TRUE,
        columns = list(
          photo = colDef(
            name = "Photo",
            cell = function(img, index) {
              player_name <- selected_player[index, "name"]
              image <- img(src = paste0(PHOTO_URL, "/", img), style = "height: 24px;", alt = player_name)
              tagList(
                div(style = "display: inline-block; width: 45px;", image)
              )
            }
          )
        )
      )
    })
  })
}
add_sign <- function(x) {
  if (x > 0) {
    return(paste0("+", x))
  } else {
    return(as.character(x))
  }
}