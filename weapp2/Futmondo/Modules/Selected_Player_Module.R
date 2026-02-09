library(reactable)

selected_player_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # shinydashboardPlus::box(
    #   width = 12,
    #   solidHeader = TRUE,
    #   reactableOutput(ns("selected_player_table"))
    # ),
    userBox(
      id = ns("selected_player_box"),
      width = 12,
      title = shinydashboardPlus::userDescription(
        title = "Selected player",
        subtitle = "Select a player from the table to see details",
        type = 2,
        # image = "https://www.futmondo.com/img/players/100000000"
        image = "https://adminlte.io/themes/AdminLTE/dist/img/user7-128x128.jpg",
        
      ),
      status = "primary",
      gradient = TRUE,
      background = "light-blue",
      boxToolSize = "xl",
      "Some text here",
      footer = "Footer here"
    )
  )
}


selected_player_Server <- function(id, login_token, selected_player) {
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
        shinydashboardPlus::updateBox(
          id = "selected_player_box",
          action = "update",
          options = list(
            title = shinydashboardPlus::userDescription(
              title = player_name,
              subtitle = paste(selected_player$role, selected_player$role2, sep = ", "),
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
    output$selected_player_table <- renderReactable({
      req(login_token())

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
