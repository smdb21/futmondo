library(reactable)

selected_player_UI <- function(id) {
  ns <- NS(id)
  tagList(
    userBox(
      id = ns("selected_player_box"),
      width = 12,
      title = userDescription(
        title = "Player Name",
        subtitle = "Position & Team",
        type = 1,
        image = "https://static01.mondocore.com/futmondo/img/faces/64/null.png"
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
        selected_player <- selected_player()
        req(selected_player)
        print(paste0("Selected player: ", selected_player$name))
        player_name <- selected_player$name
        role_text <- selected_player$role
        if (!is.na(selected_player$role2) && selected_player$role2 != "") {
          role_text <- paste(role_text, selected_player$role2, sep = ", ")
        }

        # Dynamically build integrated team logo emblem & name
        team_logo <- NULL
        if ("team" %in% colnames(selected_player) && !is.na(selected_player$team) && selected_player$team != "") {
          team_image_name <- get_team_image_name(selected_player$team)
          team_logo <- shiny::tags$div(
            style = "margin-top: 6px; display: flex; align-items: center; gap: 8px; font-weight: 500; font-size: 13px; color: #cbd5e1;",
            img(src = paste0(TEAM_LOGO_URL, team_image_name, ".png"), style = "height: 18px; width: auto; object-fit: contain; background: transparent;", alt = selected_player$team),
            selected_player$team
          )
        }

        sub_title_markup <- tagList(
          shiny::tags$span(style = "display: block; font-weight: 600; font-size: 13px; color: #94a3b8; text-transform: uppercase; letter-spacing: 0.5px;", role_text),
          team_logo
        )

        shinydashboardPlus::updateBox(
          id = "selected_player_box",
          action = "update",
          options = list(
            title = shinydashboardPlus::userDescription(
              title = player_name,
              subtitle = sub_title_markup,
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

    ## render player_points_description_box ----
    output$player_points_description_box <- renderUI({
      selected_player <- selected_player()
      req(selected_player)
      ret <- tagList()
      points <- selected_player$points
      
      clean_points <- if (is.null(points) || is.na(points) || points == "NaN" || points == "") {
        "0"
      } else {
        as.character(points)
      }

      block <- descriptionBlock(
        header = clean_points,
        number = NULL,
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

      clean_total <- if (is.null(total_last_points) || is.na(total_last_points) || total_last_points == "NaN" || total_last_points == "") {
        "0"
      } else {
        as.character(total_last_points)
      }

      # Handle NaN / "NaN" / NA in averages safely
      if (is.null(avg_last_points) || is.na(avg_last_points) || avg_last_points == "NaN" || avg_last_points == "") {
        header_text <- clean_total
      } else {
        header_text <- paste0(clean_total, " (Avg: ", round(as.numeric(avg_last_points), 1), ")")
      }

      block <- descriptionBlock(
        header = header_text,
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
        number_color = "green"
      } else if (change < 0) {
        icon <- icon("caret-down")
        number_color = "red"
      } else {
        icon <- NULL
        number_color = "black"
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