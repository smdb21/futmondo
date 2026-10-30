library(reactable)

players_table_UI <- function(id, box_title = NULL,
                             solidHeader = TRUE,
                             status = "primary",
                             filter_by_position = TRUE,
                             filter_by_team = TRUE,
                             filter_by_value = TRUE,
                             filter_by_change_value = TRUE,
                             default_minimum_change_value = NA,
                             filter_by_active_clause = TRUE,
                             filter_by_max_clause_value = FALSE,
                             filter_by_clause_under_available_funds = FALSE,
                             filter_by_is_favorite = TRUE,
                             filter_by_is_from_futmondo = TRUE,
filter_by_players_with_bid = FALSE,
                             show_position_breakdown = FALSE,
                             hide_bid_column = FALSE) {
  ns <- NS(id)
  tagList(
    shinydashboardPlus::box(
      width = 12,
      solidHeader = solidHeader,
      status = status,
      title = box_title,

      # Top Filter Bar Grid
      if (show_position_breakdown) {
        fluidRow(
          column(width = 7,
                 fluidRow(
                   style = "background: var(--fm-surface); padding: 15px; border-radius: 8px; margin: 0 0 20px 0; border: 1px solid #e2e8f0;",
if (filter_by_position) {
                      column(width = 4, div(title = "Filter players by primary or secondary position (Goalkeeper, Defender, Midfielder, Forward)", selectInput(inputId = ns("position_filter"), label = "Position", choices = c("All", "Goalkeeper", "Defender", "Midfielder", "Forward"), selected = "All", width = "100%")))
                    },
                    if (filter_by_team) {
                      column(width = 4, div(title = "Filter players by current user team owner or free agents", selectInput(inputId = ns("team_filter"), label = "User Team Owner", choices = c("All"), width = "100%")))
                    },
                    if (filter_by_value) {
                      tagList(
                        column(width = 4, div(title = "Filter players by minimum market valuation in millions of EUR", numericInput(inputId = ns("min_value_filter"), label = "Min Val (M)", min = 0, max = 1000, value = 0, step = 10, width = "100%"))),
                        column(width = 4, div(title = "Filter players by maximum market valuation in millions of EUR", numericInput(inputId = ns("max_value_filter"), label = "Max Val (M)", min = 0, max = 1000, value = 1000, step = 10, width = "100%")))
                      )
                    },
                    if (filter_by_change_value) {
                      column(width = 4, div(title = "Filter players by minimum 24-hour market trend in millions of EUR", numericInput(inputId = ns("change_value_filter"), label = "Min Trend (M)", min = 0, max = 1, value = default_minimum_change_value, step = 0.05, width = "100%")))
                    }
                 )
          ),
          column(width = 5,
                 uiOutput(ns("position_breakdown_ui"))
          )
        )
      } else {
        fluidRow(
          style = "background: var(--fm-surface); padding: 15px; border-radius: 8px; margin: 0 0 20px 0; border: 1px solid #e2e8f0;",
          if (filter_by_position) {
            column(width = 3, div(title = "Filter players by primary or secondary position (Goalkeeper, Defender, Midfielder, Forward)", selectInput(inputId = ns("position_filter"), label = "Position", choices = c("All", "Goalkeeper", "Defender", "Midfielder", "Forward"), selected = "All", width = "100%")))
          },
          if (filter_by_team) {
            column(width = 3, div(title = "Filter players by current user team owner or free agents", selectInput(inputId = ns("team_filter"), label = "User Team Owner", choices = c("All"), width = "100%")))
          },
          if (filter_by_value) {
            tagList(
              column(width = 2, div(title = "Filter players by minimum market valuation in millions of EUR", numericInput(inputId = ns("min_value_filter"), label = "Min Val (M)", min = 0, max = 1000, value = 0, step = 10, width = "100%"))),
              column(width = 2, div(title = "Filter players by maximum market valuation in millions of EUR", numericInput(inputId = ns("max_value_filter"), label = "Max Val (M)", min = 0, max = 1000, value = 1000, step = 10, width = "100%")))
            )
          },
          if (filter_by_change_value) {
            column(width = 2, div(title = "Filter players by minimum 24-hour market trend in millions of EUR", numericInput(inputId = ns("change_value_filter"), label = "Min Trend (M)", min = 0, max = 1, value = default_minimum_change_value, step = 0.05, width = "100%")))
          }
        )
      },

      # Checkboxes Inline Grid
      if (filter_by_active_clause || filter_by_max_clause_value || filter_by_clause_under_available_funds || filter_by_is_favorite || filter_by_is_from_futmondo || filter_by_players_with_bid) {
        fluidRow(
          style = "padding: 0 15px; margin-bottom: 20px; display: flex; flex-wrap: wrap; gap: 20px; align-items: center;",
          if (filter_by_active_clause) {
            div(checkboxInput(inputId = ns("active_clause_filter"), label = "Active Clause Only", value = FALSE), style = "font-weight: 500;", title = "Show only players whose buyout clause is currently payable (lock period has expired)")
          },
          if (filter_by_max_clause_value) {
            div(numericInput(inputId = ns("max_clause_value_filter"), label = "Max Clause (M)", min = 0,
              max = 1000, value = 1000, step = 0.5, width = "160px"), style = "font-weight: 500;",
              title = "Show only players with a positive release clause at or below this amount")
          },
          if (filter_by_clause_under_available_funds) {
            div(checkboxInput(inputId = ns("clause_under_available_funds_filter"),
              label = "Clause within available funds", value = FALSE), style = "font-weight: 500;",
              title = "Show only release clauses at or below your verified debt-aware spending capacity")
          },
          if (filter_by_is_favorite) {
            div(checkboxInput(inputId = ns("is_favorite_filter"), label = "Favorites Only", value = FALSE), style = "font-weight: 500;", title = "Show only players you have starred as favorite")
          },
          if (filter_by_is_from_futmondo) {
            div(checkboxInput(inputId = ns("is_from_futmondo_filter"), label = "Free Agents Only", value = FALSE), style = "font-weight: 500;", title = "Show only unowned players without a user team (free agents / market)")
          },
          if (filter_by_players_with_bid) {
            tagList(
              div(checkboxInput(inputId = ns("players_you_bid_filter"), label = "Your Bids Only", value = FALSE), style = "font-weight: 500;", title = "Show only players you have placed an active bid on"),
              div(checkboxInput(inputId = ns("players_with_bid_filter"), label = "Bidded Only", value = FALSE), style = "font-weight: 500;", title = "Show only players with active bids from any league team")
            )
          },
          div(actionButton(ns("btn_clear_filters"), label = "Clear Filters", icon = icon("eraser"), class = "btn btn-sm btn-outline-secondary"), style = "margin-left: 10px;", title = "Reset all table filters to default values")
        )
      },

      fluidRow(
        class = "player-average-control",
        column(width=12,
          numericInput(ns("recent_games_n"),"Average points: latest rounds",
            min=1,max=20,value=5,step=1,width="180px"),
          div(class="player-round-progress",role="status",`aria-live`="polite",`aria-atomic`="true",
            textOutput(ns("recent_points_status")))
        )
      ),

      # Table Container
      div(
        reactableOutput(ns("players_table")),
        style = "overflow-x: auto; font-size:85%; width: 100%;"
      )
    )
  )
}


# Normalize player ownership for the User Team Owner filter. League teams are
# matched by immutable ID; human-readable names are display-only.
players_table_normalize_owner <- function(players_df, teams_df = NULL) {
  if (!is.data.frame(players_df)) return(players_df)
  n <- nrow(players_df)
  text_col <- function(candidates) {
    # Full-catalog and market rows can carry both a normalized field and its
    # Futmondo alias. Prefer the first populated value per row: an empty
    # normalized field must not hide a populated alias such as userteamId.
    keys <- candidates[candidates %in% names(players_df)]
    value <- rep("", n)
    for (key in keys) {
      candidate <- trimws(as.character(players_df[[key]]))
      candidate[is.na(candidate)] <- ""
      use_candidate <- !nzchar(value) & nzchar(candidate)
      value[use_candidate] <- candidate[use_candidate]
    }
    value
  }
  owner_id <- text_col(c("owner_team_id","user_team_id","userteamId","userTeamId",
    "userTeam._id","userTeam.id","userteam._id","userteam.id"))
  owner_name <- text_col(c("userTeam","userTeam.name","userteam.name","user_team_name","teamname"))
  if (is.data.frame(teams_df) && nrow(teams_df)) {
    id_key <- c("teamid","id","_id")[c("teamid","id","_id") %in% names(teams_df)][1]
    name_key <- c("teamname","name")[c("teamname","name") %in% names(teams_df)][1]
    if (!is.na(id_key) && !is.na(name_key)) {
      ids <- trimws(as.character(teams_df[[id_key]]))
      names <- trimws(as.character(teams_df[[name_key]]))
      valid <- !is.na(ids) & nzchar(ids) & !is.na(names) & nzchar(names)
      lookup <- setNames(names[valid],ids[valid])
      mapped <- unname(lookup[owner_id])
      use_mapped <- !is.na(mapped) & nzchar(mapped)
      owner_name[!nzchar(owner_name) & use_mapped] <- mapped[!nzchar(owner_name) & use_mapped]
    }
  }
  players_df$owner_team_id <- owner_id
  players_df$user_team_id <- owner_id
  players_df$userTeam <- owner_name
  players_df
}

players_table_owner_choices <- function(teams_df) {
  choices <- c("All" = "All", "Free agents" = "Free")
  if (!is.data.frame(teams_df) || !nrow(teams_df)) return(choices)
  id_key <- c("teamid","id","_id")[c("teamid","id","_id") %in% names(teams_df)][1]
  name_key <- c("teamname","name")[c("teamname","name") %in% names(teams_df)][1]
  if (is.na(id_key) || is.na(name_key)) return(choices)
  ids <- trimws(as.character(teams_df[[id_key]])); names <- trimws(as.character(teams_df[[name_key]]))
  keep <- !is.na(ids) & nzchar(ids) & !is.na(names) & nzchar(names)
  labels <- names[keep]; values <- paste0("team:",ids[keep])
  labels <- labels[!duplicated(values)]; values <- values[!duplicated(values)]
  c(choices,stats::setNames(values,labels))
}

players_table_Server <- function(id, players_table_RV, user_teams_RV, login_token = NULL, championship_id = NULL, user_team_id = NULL, available_funds_RV = NULL, hide_bid_column = FALSE, refresh_trigger = NULL, is_module_active = function() TRUE, optional_ready = function() TRUE) {
  moduleServer(id, function(input, output, session) {
    bind_factual_reads(c("get_finished_rounds"),session,login_token,championship_id,user_team_id,is_active=is_module_active,refresh_trigger=refresh_trigger)

    ns <- session$ns
    table_refresh_trigger <- reactiveVal(0)
    selected_player_id_RV <- reactiveVal(NULL)
    selected_player_snapshot_RV <- reactiveVal(NULL)
    selected_player_context_RV <- reactiveVal(NULL)
    selected_player_ready_RV <- reactiveVal(FALSE)
    player_selection_event_RV <- reactiveVal(0L)
    table_context_RV <- reactive({
      value <- function(x) tryCatch(if(is.function(x)) x() else x,error=function(e)NULL)
      auth <- value(login_token)
      list(user=if(valid_login(auth))fm_scalar(auth[["userid"]],"") else "",
        championship=fm_scalar(value(championship_id),""),team=fm_scalar(value(user_team_id),""))
    })
    observeEvent(table_context_RV(), {
      selected_player_ready_RV(FALSE)
      selected_player_id_RV(NULL);selected_player_snapshot_RV(NULL);selected_player_context_RV(NULL)
      tryCatch(updateReactable("players_table",selected=NA_integer_,session=session),error=function(e)NULL)
      removeModal()
    },ignoreNULL=FALSE,priority=100)

    # All market mutations invalidate the authoritative snapshot. Listing a
    # player, cancelling our bid, and rejecting another team's bid are distinct.
    handle_bid_updated <- function(player_id = NULL, new_bid_price = NULL,
                                   is_cancel = FALSE, action_type = "refresh") {
      table_refresh_trigger(table_refresh_trigger() + 1L)
      if (!is.null(refresh_trigger) && is.function(refresh_trigger)) {
        refresh_trigger(isolate(refresh_trigger()) + 1L)
      }
    }

    # Safe date parsing helper
    parse_safe_datetime <- function(date_vec) {
      if (is.null(date_vec) || length(date_vec) == 0) return(as.POSIXct(character(0)))
      date_str <- as.character(date_vec)
      clean_str <- gsub("T", " ", date_str)
      clean_str <- gsub("Z", "", clean_str)
      clean_str <- gsub("\\..*", "", clean_str)
      parsed <- suppressWarnings(as.POSIXct(clean_str, format = "%Y-%m-%d %H:%M:%S"))
      na_idx <- is.na(parsed)
      if (any(na_idx)) {
        parsed[na_idx] <- suppressWarnings(as.POSIXct(clean_str[na_idx], format = "%Y-%m-%d"))
      }
      return(parsed)
    }

    # observers ----
    # observe user_teams_RV to update market_player_team_filter ----
    observeEvent({ req(is_module_active(),optional_ready()); user_teams_RV() }, {
      req(is_module_active())
      teams <- user_teams_RV()
      req(teams)
      updateSelectInput(session, inputId = "team_filter",
        choices = players_table_owner_choices(teams), selected = "All")
    })

    # observe clear filters button ----
    observeEvent(input$btn_clear_filters, {
      tryCatch({
        updateSelectInput(session, inputId = "position_filter", selected = "All")
        updateSelectInput(session, inputId = "team_filter", selected = "All")
        updateNumericInput(session, inputId = "min_value_filter", value = 0)
        updateNumericInput(session, inputId = "max_value_filter", value = 1000)
        updateNumericInput(session, inputId = "change_value_filter", value = NA)
        updateCheckboxInput(session, inputId = "active_clause_filter", value = FALSE)
        updateNumericInput(session, inputId = "max_clause_value_filter", value = 1000)
        updateCheckboxInput(session, inputId = "clause_under_available_funds_filter", value = FALSE)
        updateCheckboxInput(session, inputId = "is_favorite_filter", value = FALSE)
        updateCheckboxInput(session, inputId = "is_from_futmondo_filter", value = FALSE)
        updateCheckboxInput(session, inputId = "players_you_bid_filter", value = FALSE)
        updateCheckboxInput(session, inputId = "players_with_bid_filter", value = FALSE)
        updateNumericInput(session, inputId = "recent_games_n", value = 5)
      }, error = function(e) {
        warning(paste0("[Players Table] Clear filters error: ", e$message))
      })
    })
    
    # Open from an explicit click event, not from identity changes. The table
    # selection is cleared after opening so the same row can emit another
    # selection event after the modal is closed.
    observeEvent(
      player_selection_event_RV(),
      {
        selection_event <- isolate(player_selection_event_RV())
        showModal(modalDialog(
          # title = "Selected player",
          selected_player_UI(id = ns("selected_player")),
          tags$script(HTML(paste0("$(document).off(\"hidden.bs.modal.fm",gsub("[^a-zA-Z0-9]","",ns("")),"\",\"#shiny-modal\").on(\"hidden.bs.modal.fm",gsub("[^a-zA-Z0-9]","",ns("")),"\",\"#shiny-modal\",function(){Shiny.setInputValue(\"",ns("card_closed"),"\",Math.random(),{priority:\"event\"});});"))),
          footer = div(style = "text-align: center; width: 100%;",
                       modalButton("Close")),
          easyClose = TRUE,
          size = "l"
        ))
        # Send the modal shell before enabling the selected-player reactive.
        # This lets the browser paint the card before synchronous API/history
        # work in the child module begins.
        session$onFlushed(function() {
          if (identical(isolate(player_selection_event_RV()), selection_event)) {
            selected_player_ready_RV(TRUE)
          }
        }, once = TRUE)
        tryCatch(updateReactable("players_table",selected=NA_integer_,session=session),error=function(e)NULL)
      },
      ignoreInit = TRUE
    )
    observeEvent(input$card_closed,{selected_player_ready_RV(FALSE)})
    # Modules ----
    selected_player_Server(
      id = "selected_player",
      selected_player = selected_player_RV,
      login_token = login_token,
      championship_id = championship_id,
      user_team_id = user_team_id,
      refresh_trigger = refresh_trigger,
      on_bid_updated = handle_bid_updated
    )
    # reactives ----
    # Capture identity when the user selects a row. A refresh can reorder rows;
    # the same numeric index must never silently switch the action target.
    observeEvent(getReactableState(outputId = "players_table", name = "selected", session = session), {
      selected_idx <- getReactableState(outputId = "players_table", name = "selected", session = session)
      rows <- players_table_filtered_RV()
      req(length(selected_idx) == 1L, is.data.frame(rows), selected_idx >= 1L, selected_idx <= nrow(rows))
      selected_player_ready_RV(FALSE)
      selected_player_context_RV(table_context_RV())
      selected_player_snapshot_RV(rows[selected_idx,,drop=FALSE])
      selected_player_id_RV(as.character(rows$id[selected_idx]))
      player_selection_event_RV(isolate(player_selection_event_RV())+1L)
    }, ignoreNULL = TRUE)
    selected_player_RV<-reactive({
      if(!isTRUE(selected_player_ready_RV())||!identical(selected_player_context_RV(),table_context_RV()))return(NULL)
      player_id<-selected_player_id_RV();if(is.null(player_id))return(NULL)
      saved<-selected_player_snapshot_RV()
      if(is.data.frame(saved)&&nrow(saved)==1L&&identical(as.character(saved$id),player_id))saved else NULL
    })

    recent_points <- recent_player_points_source(session,login_token,championship_id,user_team_id,
      players_table_RV,is_active=function()isTRUE(is_module_active())&&isTRUE(optional_ready()),
      refresh_trigger=refresh_trigger)

    ## players_table_filtered_RV ----
    players_table_filtered_RV <- reactive({
      req(is_module_active())
      table_refresh_trigger()
      if (is.function(refresh_trigger)) refresh_trigger()
      players_table <- players_table_RV()
      if (is.null(players_table) || nrow(players_table) == 0) {
        return(NULL)
      }

      recent_n <- suppressWarnings(as.integer(input$recent_games_n)[1])
      if (!is.finite(recent_n)) recent_n <- 5L
      recent_n <- max(1L,min(20L,recent_n))
      history <- if(isTRUE(optional_ready()))tryCatch(recent_points$history(),error=function(e)data.frame())else data.frame()
      players_table <- players_table_normalize_owner(players_table, if(isTRUE(optional_ready()))tryCatch(user_teams_RV(), error=function(e)NULL)else NULL)
      players_table <- add_recent_points_average(players_table,history,recent_n)

      if (!is.null(input$position_filter)) {
        if (input$position_filter != "All") {
          players_table <- players_table %>%
            dplyr::filter(role == input$position_filter | role2 == input$position_filter)
        }
      }
      if (!is.null(input$team_filter)) {
        if (input$team_filter != "All") {
          if (input$team_filter == "Free") {
            players_table <- players_table %>%
              dplyr::filter(is.na(owner_team_id) | !nzchar(trimws(as.character(owner_team_id))))
          } else if (startsWith(as.character(input$team_filter), "team:")) {
            selected_owner <- sub("^team:", "", as.character(input$team_filter))
            players_table <- players_table %>%
              dplyr::filter(trimws(as.character(owner_team_id)) == selected_owner)
          } else {
            # Compatibility with saved sessions that used a display name.
            players_table <- players_table %>%
              dplyr::filter(userTeam == input$team_filter)
          }
        }
      }
      if (!is.null(input$min_value_filter)) {
        players_table <- players_table %>%
          dplyr::filter(value >= input$min_value_filter * 1000000)
      }
      if (!is.null(input$max_value_filter)) {
        players_table <- players_table %>%
          dplyr::filter(value <= input$max_value_filter * 1000000)
      }
      clause_values <- if ("clause_price" %in% names(players_table)) {
        suppressWarnings(as.numeric(as.character(players_table$clause_price)))
      } else rep(NA_real_, nrow(players_table))
      if (!is.null(input$max_clause_value_filter) && is.finite(input$max_clause_value_filter)) {
        maximum_clause <- as.numeric(input$max_clause_value_filter) * 1000000
        players_table <- players_table[is.finite(clause_values) & clause_values > 0 & clause_values <= maximum_clause, , drop = FALSE]
        clause_values <- clause_values[is.finite(clause_values) & clause_values > 0 & clause_values <= maximum_clause]
      }
      if (!is.null(input$clause_under_available_funds_filter) && isTRUE(input$clause_under_available_funds_filter)) {
        funds <- tryCatch({
          if (is.null(available_funds_RV)) NA_real_ else if (is.reactive(available_funds_RV) || is.function(available_funds_RV)) available_funds_RV() else available_funds_RV
        }, error = function(e) NA_real_)
        funds <- suppressWarnings(as.numeric(funds)[1])
        if (!is.finite(funds) || funds < 0) {
          # Do not guess affordability when the verified financial snapshot is unavailable.
          players_table <- players_table[0, , drop = FALSE]
        } else {
          clause_values <- if ("clause_price" %in% names(players_table)) suppressWarnings(as.numeric(as.character(players_table$clause_price))) else rep(NA_real_, nrow(players_table))
          players_table <- players_table[is.finite(clause_values) & clause_values > 0 & clause_values <= funds, , drop = FALSE]
        }
      }
      if (!is.null(input$active_clause_filter)) {
        if (input$active_clause_filter) {
          now_time <- Sys.time()
          players_table <- players_table %>%
            dplyr::filter(
              isClause & (
                is.na(clause_date) | clause_date == "" | parse_safe_datetime(clause_date) <= now_time
              )
            )
        }
      }
      # is_favorite_filter
      if (!is.null(input$is_favorite_filter)) {
        if (input$is_favorite_filter) {
          players_table <- players_table %>%
            dplyr::filter(fav == TRUE)
        }
      }
      if (!is.null(input$is_from_futmondo_filter)) {
        if (input$is_from_futmondo_filter) {
          if ("computer" %in% colnames(players_table)) {
            players_table <- players_table %>%
              dplyr::filter(computer == TRUE)
          } else {
            warning("is_from_futmondo_filter is active but 'computer' column is missing from players_table.")
          }
        }
      }
      if (!is.null(input$change_value_filter) && !is.na(input$change_value_filter)) {
        players_table <- players_table %>%
          dplyr::filter(change >= input$change_value_filter * 1000000)
      }
      if (!is.null(input$players_you_bid_filter)) {
        if (input$players_you_bid_filter) {
          players_table <- players_table %>%
            dplyr::filter(bid_price > 0)
        }
      }
      if (!is.null(input$players_with_bid_filter)) {
        if (input$players_with_bid_filter) {
          players_table <- players_table %>%
            # Number of Bids > 0
            dplyr::filter(numberOfBids > 0)
        }
      }
      return(players_table)
    })
    
    output$recent_points_status <- renderText({
      req(is_module_active(),optional_ready())
      status <- tryCatch(recent_points$status(),error=function(e)NULL)
      if(is.null(status))return("Loading round history…")
      messages <- character()
      if(status$remaining+status$pending>0L)messages <- c(messages,paste0("Loading round history: ",
        status$total-status$remaining-status$pending,"/",status$total," players."))
      if(status$saving>0L)messages <- c(messages,paste0("Saving history: ",status$saving," pending."))
      if(!is.null(status$save_error))messages <- c(messages,status$save_error)
      if(status$failed>0L)messages <- c(messages,paste0(status$failed," player histories unavailable. Refresh to retry."))
      if(!status$matrix_complete)messages <- c(messages,"Saved round history is incomplete.")
      paste(messages,collapse=" ")
    })

    # renders ----
    ## render players_table ----
    output$players_table <- renderReactable({
      req(players_table_filtered_RV())
      cols_to_hide <- c(cfg_player_columns_to_hide, "data_coverage", "observed_at",
        "recent_points_count","recent_points_required","recent_points_status")
      if (isTRUE(hide_bid_column)) {
        cols_to_hide <- c(cols_to_hide, "bid_price")
      }
      coverage_rows <- players_table_filtered_RV()
      players_table <- coverage_rows %>%
        reorder_player_table_columns() %>%
        dplyr::select(!any_of(cols_to_hide))
      
      print(paste0(nrow(players_table), " players in table"))
      # players_table <- players_table %>%
      #   dplyr::select(
      #     any_of(starts_with("change")), any_of(starts_with("market_")), any_of(starts_with("bid_price")), any_of(starts_with("clause_")), any_of(c("name", "role", "role2", "points", "value", "status", "team", "rating", "change", "average", "total")))
      #
      table_columns <- get_reactable_columns_for_players(players_table)
      recent_n <- suppressWarnings(as.integer(input$recent_games_n)[1])
      if (!is.finite(recent_n)) recent_n <- 5L
      table_columns[["recent_points_avg"]] <- colDef(
        name=paste0("Avg Last ",max(1L,min(20L,recent_n))),align="center",
        cell=function(value,index) {
          if(length(value)==1L&&is.finite(value)){
            if(identical(coverage_rows$recent_points_status[index],"reported"))
              return(span(round(value,1)," (reported)",title="Futmondo-reported last-five average"))
            return(round(value,1))
          }
          paste0(coverage_rows$recent_points_count[index],"/",coverage_rows$recent_points_required[index]," rounds")
        })
      reactable(players_table,
                columns = table_columns,
                searchable = TRUE,
                filterable = TRUE,
                resizable = TRUE,
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
                selection = "single",
                borderless = TRUE,
                onClick = "select",
                theme = fm_reactable_theme(
                  rowSelectedStyle = list(backgroundColor = "#eee", boxShadow = "inset 2px 0 0 0 #ffa62d")
                )
      )
    })
    
    # Position Breakdown UI Renderer
    output$position_breakdown_ui <- renderUI({
      df <- players_table_RV()
      if (is.null(df) || nrow(df) == 0) return(NULL)

      combined_roles <- paste(
        if ("role" %in% colnames(df)) ifelse(is.na(df$role), "", as.character(df$role)) else "",
        if ("role2" %in% colnames(df)) ifelse(is.na(df$role2), "", as.character(df$role2)) else "",
        sep = " "
      )

      gk <- sum(grepl("portero|goalkeeper|^1$|\\bpor\\b|\\bgkp\\b|\\bgk\\b", combined_roles, ignore.case = TRUE), na.rm = TRUE)
      df_cnt <- sum(grepl("defensa|defender|^2$|\\bdef\\b|\\bdf\\b", combined_roles, ignore.case = TRUE), na.rm = TRUE)
      md <- sum(grepl("centrocampista|midfielder|mediocentro|^3$|\\bmed\\b|\\bcen\\b|\\bmid\\b|\\bmf\\b|\\bmc\\b", combined_roles, ignore.case = TRUE), na.rm = TRUE)
      fw <- sum(grepl("delantero|forward|extremo|atacante|^4$|\\bdel\\b|\\bfwd\\b|\\bfw\\b|\\batt\\b", combined_roles, ignore.case = TRUE), na.rm = TRUE)
      total_squad <- nrow(df) # Exact actual player count (distinct players)

      div(
        class = "squad-breakdown-card",
        div(class = "squad-breakdown-title",
            span(icon("users"), " Squad Position Breakdown"),
            span(class = "badge", style = "background-color: var(--fm-surface); color: var(--fm-text); font-size: 11px; padding: 3px 8px;", paste0("Total: ", total_squad, " Players"))
        ),
        div(style = "display: flex; gap: 8px; flex-wrap: wrap; justify-content: space-between; margin-top: 8px;",
            span(class = "badge-gk squad-pos-badge", paste0("Goalkeepers: ", gk)),
            span(class = "badge-df squad-pos-badge", paste0("Defenders: ", df_cnt)),
            span(class = "badge-md squad-pos-badge", paste0("Midfielders: ", md)),
            span(class = "badge-fw squad-pos-badge", paste0("Forwards: ", fw))
        )
      )
    })

    return(selected_player_RV)
  })
}

# Exact N consecutive source round numbers, with explicit coverage.

add_recent_points_average <- function(players_df,history_df,n=5L) {
  if(!is.data.frame(players_df))return(players_df)
  n <- suppressWarnings(as.integer(n)[1]);if(!length(n)||!is.finite(n))n <- 5L
  n <- max(1L,min(20L,n));count <- nrow(players_df)
  players_df$recent_points_avg <- rep(NA_real_,count)
  players_df$recent_points_count <- integer(count)
  players_df$recent_points_required <- rep(n,count)
  players_df$recent_points_status <- rep("missing",count)
  if(count&&"id"%in%names(players_df)&&is.data.frame(history_df)&&nrow(history_df)&&
      all(c("player_id","round","points")%in%names(history_df))){
    source_latest_round <- attr(history_df,"latest_round")
    history_df$round <- suppressWarnings(as.numeric(as.character(history_df$round)))
    history_df$points <- suppressWarnings(as.numeric(as.character(history_df$points)))
    history_df <- history_df[is.finite(history_df$round)&history_df$round>=1&
      history_df$round==floor(history_df$round),,drop=FALSE]
    if(nrow(history_df)){
      latest <- source_latest_round %||% max(history_df$round);wanted <- seq.int(latest-n+1L,latest)
      by_player <- split(history_df,as.character(history_df$player_id))
      for(i in seq_len(count)){
        rows <- by_player[[as.character(players_df$id[i])]]
        if(is.null(rows))next
        rows <- rows[!duplicated(rows$round,fromLast=TRUE),,drop=FALSE]
        values <- rows$points[match(wanted,rows$round)]
        coverage <- sum(is.finite(values));players_df$recent_points_count[i] <- coverage
        if(coverage==n){players_df$recent_points_avg[i] <- mean(values);players_df$recent_points_status[i] <- "complete"}
        else if(coverage)players_df$recent_points_status[i] <- "partial"
      }
    }
  }
  if(n==5L&&"average.averageLastFive"%in%names(players_df)){
    fallback <- suppressWarnings(as.numeric(as.character(players_df$average.averageLastFive)))
    use <- !is.finite(players_df$recent_points_avg)&is.finite(fallback)
    players_df$recent_points_avg[use] <- fallback[use];players_df$recent_points_status[use] <- "reported"
  }
  players_df
}

# Retrieve source scores through the existing factual-read adapter.
get_player_recent_points <- function(login,championship_id,user_team_id,player_id,scoring_version="unknown") {
  summary <- get_player_summary(login,championship_id,user_team_id,player_id)
  if(is.null(summary))stop("Player round points unavailable")
  season <- league_season_context(championship_id,summary$championship$season %||% summary$championship$seasonId)
  ctx <- resource_context(login,championship_id,user_team_id,player_id,season,scoring_version)
  observed <- normalize_player_match_observations(summary,player_id,championship_id,
    season=season,scoring_version=scoring_version,observed_at=attr(summary,"observed_at") %||% Sys.time())
  list(context=ctx,observations=observed,points=reconcile_player_points(data.frame(),observed,ctx),
    previous_points=data.frame(),status=if(nrow(observed))"partial"else "empty")
}

# Reuse recently observed histories only inside their exact source context.
# A live probe establishes the current round; gaps never become zero or complete.
recent_points_saved_players <- function(stored,context,acquired,now=Sys.time()) {
  stored <- history_context_filter(stored,context)
  if(!is.data.frame(stored)||!nrow(stored)||!all(c("player_id","round","observed_at")%in%names(stored)))return(character())
  probes <- Filter(function(x)!is.null(x$data)&&!identical(x$freshness,"stale"),acquired)
  if(!length(probes))return(character())
  probe_rounds <- unlist(lapply(probes,function(x){
    rows <- history_context_filter(x$data$observations,context)
    suppressWarnings(as.numeric(rows$round))
  }),use.names=FALSE)
  probe_rounds <- probe_rounds[is.finite(probe_rounds)&probe_rounds>=1]
  if(!length(probe_rounds))return(character())
  latest_probe <- max(probe_rounds)
  stored_rounds <- suppressWarnings(as.numeric(stored$round))
  valid <- is.finite(stored_rounds)&stored_rounds>=1
  if(!any(valid))return(character())
  lifetime <- if(context$season!="unknown"&&context$scoring_version!="unknown")24*3600 else 3600
  age <- as.numeric(difftime(now,fm_time(stored$observed_at),units="secs"))
  fresh <- is.finite(age)&age>=-60&age<lifetime
  reusable <- unique(as.character(stored$player_id[fresh&valid]))
  # A partial league fill must not hide players still missing the probe round.
  # A returned NULL score is an observation; it remains unavailable for averages.
  intersect(reusable,as.character(stored$player_id[valid&stored_rounds==latest_probe]))
}

# Acquisition advances independently of table rendering. Only table-player
# changes or a new league-round anchor publish at most one snapshot per 5 s.
recent_player_points_source <- function(session,login_token,championship_id,user_team_id,
  players_RV,is_active=function()TRUE,refresh_trigger=NULL,publish_interval=5) {
  value <- function(x)if(is.function(x))x()else x
  owner <- reactive({
    auth <- value(login_token)
    list(account=if(valid_login(auth))fm_scalar(auth[["userid"]],"")else "",
      league=fm_scalar(value(championship_id),""),team=fm_scalar(value(user_team_id),""),
      active=isTRUE(is_active()),refresh=if(is.function(refresh_trigger))refresh_trigger()else 0L)
  })
  matrix_refresh <- reactiveVal(0L)
  matrix_read <- factual_read_adapter(session,"read_league_round_points",login_token,
    championship_id,user_team_id,is_active,function()paste(if(is.function(refresh_trigger))refresh_trigger()else 0L,
      matrix_refresh(),sep=":"),state_only=TRUE)
  lineup_read <- factual_read_adapter(session,"get_lineup_from_team",login_token,
    championship_id,user_team_id,is_active,refresh_trigger,state_only=TRUE)
  catalog_read <- factual_read_adapter(session,"get_championship_players",login_token,
    championship_id,user_team_id,is_active,refresh_trigger,state_only=TRUE)
  states <- reactive({
    scope <- owner();req(scope$active,nzchar(scope$account),nzchar(scope$league),nzchar(scope$team))
    auth <- value(login_token)
    list(matrix=matrix_read(championship_id=scope$league),
      lineup=lineup_read(login=auth,championship_id=scope$league,user_team_id=scope$team),
      catalog=catalog_read(login=auth,championship_id=scope$league))
  })
  sources <- reactiveVal(list());pending <- reactiveVal(list());generation <- 0L;closed <- FALSE
  source_season <- reactiveVal("unknown");history <- reactiveVal(data.frame())
  force_current_table <- reactiveVal(FALSE)
  publish_tick <- reactiveVal(0L);publish_timer <- NULL;last_signature <- NULL;last_base <- NULL
  last_published <- -Inf
  matrix_refreshed <- FALSE;persistence_attempted <- FALSE;previous_owner <- NULL
  cancel_publication <- function(){if(is.function(publish_timer))publish_timer();publish_timer <<- NULL}
  session$onSessionEnded(function(){closed <<- TRUE;generation <<- generation+1L;cancel_publication()})
  observeEvent(owner(),{
    scope <- owner()
    same_scope <- !is.null(previous_owner)&&identical(scope$account,previous_owner$account)&&
      identical(scope$league,previous_owner$league)&&identical(scope$team,previous_owner$team)
    refreshed <- same_scope&&!identical(scope$refresh,previous_owner$refresh)
    if(!is.null(previous_owner)&&length(isolate(pending()))&&nzchar(previous_owner$account)){
      for(player in names(isolate(pending())))invalidate_resources(resource_context(
        account_id=previous_owner$account,championship_id=previous_owner$league,
        user_team_id=previous_owner$team,player_id=player,session_id=session$token),resources="player_summary_points")
    }
    if(refreshed&&nzchar(scope$account))invalidate_resources(resource_context(account_id=scope$account,
      championship_id=scope$league,user_team_id=scope$team),pattern="^player_summary_points$|^player_summary_")
    previous_owner <<- scope
    generation <<- generation+1L;pending(list());cancel_publication()
    if(!same_scope||refreshed){
      sources(list());source_season("unknown");history(data.frame())
      force_current_table(refreshed)
      last_signature <<- NULL;last_base <<- NULL;last_published <<- -Inf
      matrix_refreshed <<- FALSE;persistence_attempted <<- FALSE
    }
  },ignoreNULL=FALSE,priority=90)
  context <- reactive({
    scope <- owner();req(nzchar(scope$account),nzchar(scope$league),nzchar(scope$team))
    season <- source_season();if(identical(season,"unknown"))season <- league_season_context(scope$league)
    lineup <- tryCatch(states()$lineup$data,error=function(e)NULL)
    resource_context(value(login_token),scope$league,scope$team,season=season,
      scoring_version=normalize_league_rules(list(),lineup,scope$league)$scoring_version,session_id=session$token)
  })
  table_ids <- reactive({
    rows <- tryCatch(players_RV(),error=function(e)NULL)
    result <- if(is.data.frame(rows)&&"id"%in%names(rows))as.character(rows$id)else character()
    unique(result[!is.na(result)&nzchar(result)])
  })
  ids <- reactive({
    catalog <- tryCatch(states()$catalog$data,error=function(e)NULL)
    catalog_ids <- if(is.data.frame(catalog)&&"id"%in%names(catalog))as.character(catalog$id)else character()
    result <- unique(c(table_ids(),catalog_ids))
    head(result[!is.na(result)&nzchar(result)],2000L)
  })
  stored <- reactive(history_context_filter(tryCatch(states()$matrix$data,error=function(e)NULL),context()))
  reusable <- reactive({
    saved_ids <- recent_points_saved_players(stored(),context(),sources())
    # Refresh retries the current table, without replaying the other league players.
    if(force_current_table())setdiff(saved_ids,table_ids())else saved_ids
  })
  needed <- reactive({
    setdiff(ids(),c(names(sources()),names(pending()),reusable()))
  })
  observe({
    scope <- owner();if(closed||!scope$active||!nzchar(scope$account))return()
    state <- states()
    if(any(vapply(state,function(x)identical(x$status,"loading"),logical(1))))return()
    ctx <- context();needed_ids <- needed();busy <- pending()
    # Write completion signals resume acquisition; no queue polling timer.
    .persistence_state$revision()
    saving <- background_sync_status(scope$account)
    queued <- c(.persistence_state$queue,if(!is.null(.persistence_state$active))list(.persistence_state$active)else list())
    bytes <- sum(vapply(queued,function(job)as.numeric(job$bytes %||% 0),numeric(1)))
    if(!length(needed_ids)&&!length(busy)&&saving$pending==0L&&persistence_attempted&&!matrix_refreshed){
      matrix_refreshed <<- TRUE;matrix_refresh(isolate(matrix_refresh())+1L)
    }
    if(saving$pending>=20L||length(queued)>=190L||bytes>=14*1024^2)return()
    slots <- max(0L,2L-length(busy));if(!slots||!length(needed_ids))return()
    for(player in head(needed_ids,slots))local({
      player_id <- player;attempt_generation <- generation;fetch_ctx <- ctx;fetch_ctx$player_id <- player_id
      auth <- value(login_token)
      args <- list(login=auth,championship_id=fetch_ctx$championship_id,user_team_id=fetch_ctx$user_team_id,
        player_id=player_id,scoring_version=fetch_ctx$scoring_version)
      current <- function()isTRUE(isolate(!closed&&isTRUE(is_active())&&identical(generation,attempt_generation)))
      busy <- isolate(pending());busy[[player_id]] <- attempt_generation;pending(busy)
      promise <- get_cached_resource_async("player_summary_points",fetch_ctx,function()
        async_adapter_fetch("get_player_recent_points",args,context=fetch_ctx,is_current=current))
      promises::then(promise,function(result){
        if(closed)return(NULL)
        busy <- isolate(pending())
        if(identical(busy[[player_id]],attempt_generation)){busy[[player_id]] <- NULL;pending(busy)}
        if(!current())return(NULL)
        if(!is.null(result$data)&&!identical(result$freshness,"stale")){
          season <- result$data$context$season %||% "unknown"
          if(!identical(season,"unknown"))source_season(season)
        }
        acquired <- isolate(sources());acquired[[player_id]] <- result;sources(acquired)
        if(!is.null(result$data)&&!identical(result$freshness,"stale")){
          payload <- result$data;payload$context$session_id <- session$token
          saved <- history_context_filter(isolate(states())$matrix$data,payload$context)
          payload$previous_points <- reconcile_player_points(saved,data.frame(),payload$context)
          if(nrow(payload$points))persistence_attempted <<- TRUE
          tryCatch(record_acquired_resource("player_card_history",payload,payload$context),error=function(e)NULL)
        }
        NULL
      })
    })
  })
  observe({
    scope <- owner();if(closed||!scope$active||!nzchar(scope$account))return()
    state <- states();if(any(vapply(state,function(x)identical(x$status,"loading"),logical(1))))return()
    ctx <- context();saved <- stored();visible <- table_ids();acquired <- sources();publish_tick()
    scoped_live <- lapply(acquired,function(x)history_context_filter(x$data$observations,ctx))
    live_rounds <- unlist(lapply(scoped_live,function(x)suppressWarnings(as.numeric(x$round))),use.names=FALSE)
    rounds <- c(suppressWarnings(as.numeric(saved$round)),live_rounds)
    rounds <- rounds[is.finite(rounds)&rounds>=1];latest <- if(length(rounds))max(rounds)else NULL
    base <- list(context=ctx[c("account_id","championship_id","user_team_id","season","scoring_version")],
      ids=visible,stored=saved)
    visible_sources <- names(Filter(function(x)is.data.frame(x)&&nrow(x)>0L,scoped_live[intersect(visible,names(scoped_live))]))
    signature <- list(base=base,acquired=visible_sources,latest=latest)
    if(identical(signature,last_signature))return()
    new_base <- !identical(base,last_base)
    visible_complete <- all(visible%in%c(names(acquired),reusable()))
    complete <- visible_complete||(!length(needed())&&!length(pending()))
    elapsed <- as.numeric(Sys.time())-last_published
    if(!new_base&&!complete&&elapsed<publish_interval){
      if(is.null(publish_timer)){
        attempt_generation <- generation
        publish_timer <<- later::later(function(){
          publish_timer <<- NULL
          if(!closed&&identical(generation,attempt_generation)&&isTRUE(isolate(is_active())))
            shiny::isolate(publish_tick(publish_tick()+1L))
        },max(0,publish_interval-elapsed))
      }
      return()
    }
    cancel_publication()
    by_player <- if(nrow(saved))split(saved,as.character(saved$player_id))else list()
    rows <- lapply(visible,function(player){
      player_ctx <- ctx;player_ctx$player_id <- player
      points <- reconcile_player_points(by_player[[player]],scoped_live[[player]],player_ctx)
      if(!nrow(points))return(NULL)
      data.frame(player_id=player,round=points$round_number,points=points$points,
        season=rep(ctx$season,nrow(points)),scoring_version=rep(ctx$scoring_version,nrow(points)))
    })
    result <- dplyr::bind_rows(rows);if(!is.null(latest))attr(result,"latest_round") <- latest
    last_signature <<- signature;last_base <<- base;last_published <<- as.numeric(Sys.time())
    history(result)
  },priority=-10)
  status <- reactive({
    all_ids <- ids();acquired <- sources();.persistence_state$revision()
    saving <- background_sync_status(owner()$account)
    list(total=length(all_ids),remaining=length(needed()),attempted=length(acquired),pending=length(pending()),
      saving=saving$pending,save_error=saving$error,
      failed=sum(vapply(acquired,function(x)is.null(x$data),logical(1))),
      matrix_complete=isTRUE(attr(tryCatch(states()$matrix$data,error=function(e)NULL),"complete")))
  })
  list(history=history,status=status,states=states,sources=sources,pending=pending)
}


# Shared display contract: preserve signed balances and distinguish missing data.
ui_financial_amount <- function(value) {
  amount <- suppressWarnings(as.numeric(value))
  if (length(amount) != 1L || !is.finite(amount)) return("Unavailable")
  format_table_currency(amount)
}
