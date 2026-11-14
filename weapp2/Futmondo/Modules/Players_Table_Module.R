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
                             hide_bid_column = FALSE,
                             default_active_clause = FALSE,
                             default_free_agents = FALSE) {
  ns <- NS(id)
  checkbox_filters <-
      if (filter_by_active_clause || filter_by_max_clause_value || filter_by_clause_under_available_funds || filter_by_is_favorite || filter_by_is_from_futmondo || filter_by_players_with_bid) {
        fluidRow(
          class = "player-checkbox-filters",
          if (filter_by_active_clause) {
            div(checkboxInput(inputId = ns("active_clause_filter"), label = "Active Clause Only", value = isTRUE(default_active_clause)), style = "font-weight: 500;", title = "Show only players whose buyout clause is currently payable (lock period has expired)")
          },
          if (filter_by_max_clause_value) {
            div(numericInput(inputId = ns("max_clause_value_filter"), label = "Max Clause (M)", min = 0,
              value = NA_real_, step = 0.5, width = "160px"), style = "font-weight: 500;",
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
            div(checkboxInput(inputId = ns("is_from_futmondo_filter"), label = "Free Agents Only", value = isTRUE(default_free_agents)), style = "font-weight: 500;", title = "Show only unowned players without a user team (free agents / market)")
          },
          if (filter_by_players_with_bid) {
            tagList(
              div(checkboxInput(inputId = ns("players_you_bid_filter"), label = "Your Bids Only", value = FALSE), style = "font-weight: 500;", title = "Show only players you have placed an active bid on"),
              div(checkboxInput(inputId = ns("players_with_bid_filter"), label = "Bidded Only", value = FALSE), style = "font-weight: 500;", title = "Show only players with active bids from any league team")
            )
          },
          div(actionButton(ns("btn_clear_filters"), label = "Clear Filters", icon = icon("eraser"), class = "btn btn-sm btn-outline-secondary"), class = "player-clear-filters", title = "Reset all table filters to default values")
        )
      }
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
                   class = "player-filter-panel",
                   style = "background: var(--fm-surface); padding: 15px; border-radius: 8px; margin: 0 0 20px 0; border: 1px solid #e2e8f0;",
                    if (filter_by_position) {
                      column(width = if(!filter_by_team&&!filter_by_value&&!filter_by_change_value)12 else 4, div(title = "Filter players by primary or secondary position (Goalkeeper, Defender, Midfielder, Forward)", selectInput(inputId = ns("position_filter"), label = "Position", choices = c("All", "Goalkeeper", "Defender", "Midfielder", "Forward"), selected = "All", width = "100%")))
                    },
                    if (filter_by_team) {
                      column(width = 4, div(title = "Filter players by current user team owner or free agents", selectInput(inputId = ns("team_filter"), label = "User Team Owner", choices = c("All"), width = "100%")))
                    },
                    if (filter_by_value) {
                      tagList(
                        column(width = 4, div(title = "Filter players by maximum market valuation in millions of EUR", numericInput(inputId = ns("max_value_filter"), label = "Max Val (M)", min = 0, value = NA_real_, step = 10, width = "100%")))
                      )
                    },
                    if (filter_by_change_value) {
                      column(width = 4, div(title = "Filter players by minimum 24-hour market trend in millions of EUR", numericInput(inputId = ns("change_value_filter"), label = "Min Trend (M)", min = 0, max = 1, value = default_minimum_change_value, step = 0.05, width = "100%")))
                    },
                    column(width=12, checkbox_filters)
                 )
          ),
          column(width = 5,
                 uiOutput(ns("position_breakdown_ui"))
          )
        )
      } else {
        fluidRow(
          class = "player-filter-panel",
          style = "background: var(--fm-surface); padding: 15px; border-radius: 8px; margin: 0 0 20px 0; border: 1px solid #e2e8f0;",
          if (filter_by_position) {
            column(width = 3, div(title = "Filter players by primary or secondary position (Goalkeeper, Defender, Midfielder, Forward)", selectInput(inputId = ns("position_filter"), label = "Position", choices = c("All", "Goalkeeper", "Defender", "Midfielder", "Forward"), selected = "All", width = "100%")))
          },
          if (filter_by_team) {
            column(width = 3, div(title = "Filter players by current user team owner or free agents", selectInput(inputId = ns("team_filter"), label = "User Team Owner", choices = c("All"), width = "100%")))
          },
          if (filter_by_value) {
            tagList(
              column(width = 2, div(title = "Filter players by maximum market valuation in millions of EUR", numericInput(inputId = ns("max_value_filter"), label = "Max Val (M)", min = 0, value = NA_real_, step = 10, width = "100%")))
            )
          },
          if (filter_by_change_value) {
            column(width = 2, div(title = "Filter players by minimum 24-hour market trend in millions of EUR", numericInput(inputId = ns("change_value_filter"), label = "Min Trend (M)", min = 0, max = 1, value = default_minimum_change_value, step = 0.05, width = "100%")))
          },
          column(width=12, checkbox_filters)
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
  # A populated owner ID/name takes precedence over a conflicting System flag.
  # Without owner fields, only an explicit computer=TRUE establishes a free agent.
  owner_id_supplied<-any(c("owner_team_id","user_team_id","userteamId","userTeamId",
    "userTeam._id","userTeam.id","userteam._id","userteam.id")%in%names(players_df))
  computer<-if("computer"%in%names(players_df))suppressWarnings(as.logical(as.character(players_df$computer)))else rep(NA,n)
  players_df$is_free_agent<-!nzchar(owner_id)&!nzchar(owner_name)&
    (computer%in%TRUE | (is.na(computer)&owner_id_supplied))
  players_df$owner_team_id <- owner_id
  players_df$user_team_id <- owner_id
  players_df$userTeam <- owner_name
  players_df
}

# Endpoint-optional filter fields remain typed missing, never fabricated zero.
players_table_filter_fields <- function(rows) {
  if(!is.data.frame(rows))return(rows)
  n<-nrow(rows)
  for(field in c("value","change","bid_price","numberOfBids","clause_price"))
    rows[[field]]<-if(field%in%names(rows))suppressWarnings(as.numeric(as.character(rows[[field]])))else rep(NA_real_,n)
  for(field in c("role","role2","clause_date"))
    rows[[field]]<-if(field%in%names(rows))as.character(rows[[field]])else rep(NA_character_,n)
  for(field in c("fav","isClause","clause_transferred"))
    rows[[field]]<-if(field%in%names(rows))suppressWarnings(as.logical(as.character(rows[[field]])))else rep(NA,n)
  rows
}
players_table_active_clause <- function(rows,now=Sys.time()) {
  field<-function(key,default)if(key%in%names(rows))rows[[key]]else rep(default,nrow(rows))
  price<-suppressWarnings(as.numeric(as.character(field("clause_price",NA_real_))))
  transferred<-suppressWarnings(as.logical(as.character(field("clause_transferred",NA))))
  flag<-suppressWarnings(as.logical(as.character(field("isClause",NA))))
  available<-transferred%in%FALSE | (is.na(transferred)&flag%in%TRUE)
  date<-as.character(field("clause_date",NA_character_));no_lock<-is.na(date)|!nzchar(trimws(date))
  until<-rep(NA_real_,nrow(rows))
  if(any(!no_lock))until[!no_lock]<-as.numeric(fm_time(date[!no_lock]))
  is.finite(price)&price>0&available&(no_lock | (is.finite(until)&until<=as.numeric(now)))
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

# Hide only price columns identical across the full source, including missingness.
players_table_duplicate_price_columns <- function(rows) {
  kept <- character(); duplicates <- character()
  for (column in intersect(c("price", "market_price", "effective_market_price"), names(rows))) {
    same <- vapply(kept, function(other) identical(as.numeric(rows[[column]]), as.numeric(rows[[other]])), logical(1))
    if (any(same)) duplicates <- c(duplicates, column) else kept <- c(kept, column)
  }
  duplicates
}

# One finite range per metric over the loaded source, before any table filters.
players_table_metric_ranges <- function(rows) {
  columns <- intersect(c("recent_points_avg", "recent_points_ratio", "points"), names(rows))
  stats::setNames(lapply(columns, function(column) {
    values <- rows[[column]]
    values <- values[is.finite(values)]
    if (!length(values)) return(list(min=NULL, max=NULL))
    list(min=min(values), max=max(values))
  }), columns)
}

# Reactable supplies row, column and table state. Never scan client-visible rows:
# their search, sorting and pagination must not change the source color scale.
players_table_metric_style <- function() htmlwidgets::JS("function(rowInfo, column, state) {
  var value = rowInfo.values[column.id];
  var ranges = state.meta && state.meta.playerMetricRanges;
  var range = ranges && ranges[column.id];
  if (typeof value !== 'number' || !Number.isFinite(value) || !range ||
      typeof range.min !== 'number' || !Number.isFinite(range.min) ||
      typeof range.max !== 'number' || !Number.isFinite(range.max)) return {};
  var t = range.max === range.min ? 0 : Math.max(0, Math.min(1, (value - range.min) / (range.max - range.min)));
  var surface = getComputedStyle(document.documentElement).getPropertyValue('--fm-surface').trim();
  var hex = surface.match(/^#([0-9a-f]{6})$/i);
  var base = hex ? hex[1].match(/../g).map(function(v) { return parseInt(v, 16); }) : [11, 18, 13];
  var color = base.map(function(v, i) { return v + ([124, 255, 155][i] - v) * t; });
  var luminance = color.map(function(v) { v /= 255; return v <= 0.04045 ? v / 12.92 : Math.pow((v + 0.055) / 1.055, 2.4); })
    .reduce(function(sum, v, i) { return sum + v * [0.2126, 0.7152, 0.0722][i]; }, 0);
  return { '--fm-metric-bg': 'color-mix(in srgb, var(--fm-surface) ' + ((1 - t) * 100) + '%, #7cff9b)',
    '--fm-metric-fg': luminance > 0.179 ? '#000000' : '#ffffff' };
}")

# Color belongs to a child badge, not the cell; numeric source values stay intact.
players_table_metric_cell <- function(digits=1L, title_column=NULL) htmlwidgets::JS(paste0("function(cellInfo) {
  var value = cellInfo.value;
  if (typeof value !== 'number' || !Number.isFinite(value)) return '';
  return React.createElement('span', {className: 'fm-metric-badge'",
    if(!is.null(title_column))paste0(", title: cellInfo.row && cellInfo.row[",jsonlite::toJSON(title_column,auto_unbox=TRUE),"]"),
    "}, String(Number(value.toFixed(",as.integer(digits),"))));
}"))

# Keep the data numeric for sorting; missing averages have an empty cell.
players_table_recent_average_column <- function(n=5L) {
  n <- suppressWarnings(as.integer(n)[1]);if(!is.finite(n))n <- 5L
  reactable::colDef(name=paste0("Avg Last ",max(1L,min(20L,n))),align="center",sortNALast=TRUE,
    minWidth=110, headerClass="fm-metric-header",
    header=htmltools::span("Avg last",htmltools::tags$br(),max(1L,min(20L,n))),
    style=players_table_metric_style(), cell=players_table_metric_cell(1L,"recent_points_details")
  )
}

# Points per million euros; reuse the exact displayed N-round average.
players_table_points_ratio <- function(rows, basis) {
  ratio <- rep(NA_real_, nrow(rows))
  if (all(c("recent_points_avg", basis) %in% names(rows))) {
    average <- suppressWarnings(as.numeric(as.character(rows$recent_points_avg)))
    price <- suppressWarnings(as.numeric(as.character(rows[[basis]])))
    valid <- is.finite(average) & is.finite(price) & price > 0
    ratio[valid] <- average[valid] / (price[valid] / 1e6)
    ratio[!is.finite(ratio)] <- NA_real_
  }
  rows$recent_points_ratio <- ratio
  rows
}

# Unknown configuration leaves the reference column visible; only an explicit
# prohibition hides it. Primary team settings take precedence, as in Admin.
players_table_renewal_disabled <- function(settings) {
  cfg <- utils::modifyList(settings$league$configuration %||% list(),settings$info$configuration %||% list())
  value <- cfg$ddt
  length(value)==1L && is.atomic(value) && !is.na(value) && identical(tolower(as.character(value)),"true")
}

players_table_Server <- function(id, players_table_RV, user_teams_RV, login_token = NULL, championship_id = NULL, user_team_id = NULL, available_funds_RV = NULL, hide_bid_column = FALSE, refresh_trigger = NULL, is_module_active = function() TRUE, optional_ready = function() TRUE, points_ratio_basis = NULL, own_squad = FALSE, league_settings_source = NULL) {
  if (!is.null(points_ratio_basis)) points_ratio_basis <- match.arg(points_ratio_basis, c("value", "clause_price"))
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
      if(exists("league_context_revision",mode="function"))league_context_revision()
      champ<-fm_scalar(value(championship_id),"");team<-fm_scalar(value(user_team_id),"")
      context<-if(valid_login(auth))resource_context(auth,champ,team)else list()
      list(user=if(valid_login(auth))fm_scalar(auth[["userid"]],"") else "",
        championship=champ,team=team,competition=fm_scalar(context$competition_id,"unknown"),
        season=fm_scalar(context$season,"unknown"),scoring=fm_scalar(context$scoring_version,"unknown"))
    })
    selection_scope<-NULL
    observeEvent(table_context_RV(), {
      next_scope<-table_context_RV()
      if(identical(selection_scope,next_scope))return()
      selection_scope<<-next_scope
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
        updateNumericInput(session, inputId = "max_value_filter", value = NA_real_)
        updateNumericInput(session, inputId = "change_value_filter", value = NA)
        updateCheckboxInput(session, inputId = "active_clause_filter", value = FALSE)
        updateNumericInput(session, inputId = "max_clause_value_filter", value = NA_real_)
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
      refresh_trigger=round_history_refresh_trigger(session),required_rounds=function()input$recent_games_n %||% 5L)

    # Enrich once per source/history/N change; filters reuse these numeric values.
    players_table_enriched_RV <- reactive({
      req(is_module_active())
      table_refresh_trigger()
      if (is.function(refresh_trigger)) refresh_trigger()
      players_table <- players_table_RV()
      if (!is.data.frame(players_table))return(NULL)
      if(isTRUE(own_squad))players_table$sale_acquisition_cost<-vapply(seq_len(nrow(players_table)),
        function(i)current_player_acquisition_cost(players_table[i,,drop=FALSE],NULL,NULL),numeric(1))
      source_columns<-names(players_table)
      players_table<-players_table_filter_fields(players_table)
      filter_added_fields<-setdiff(names(players_table),source_columns)

      recent_n <- suppressWarnings(as.integer(input$recent_games_n)[1])
      if (!is.finite(recent_n)) recent_n <- 5L
      recent_n <- max(1L,min(20L,recent_n))
      history <- if(isTRUE(optional_ready()))tryCatch(recent_points$history(),error=function(e)data.frame())else data.frame()
      players_table <- players_table_normalize_owner(players_table, if(isTRUE(optional_ready()))tryCatch(user_teams_RV(), error=function(e)NULL)else NULL)
      players_table <- add_recent_points_average(players_table,history,recent_n)
      if(!is.null(points_ratio_basis))players_table<-players_table_points_ratio(players_table,points_ratio_basis)
      attr(players_table,"filter_added_fields")<-filter_added_fields
      players_table
    })
    table_color_meta <- reactive(list(playerMetricRanges=players_table_metric_ranges(players_table_enriched_RV())))

    ## players_table_filtered_RV ----
    players_table_filtered_RV <- reactive({
      players_table <- players_table_enriched_RV()
      if(is.null(players_table))return(NULL)
      filter_added_fields<-attr(players_table,"filter_added_fields")
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
              dplyr::filter(is_free_agent)
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
      if (!is.null(input$max_value_filter) && is.finite(input$max_value_filter)) {
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
      if(isTRUE(input$active_clause_filter))
        players_table<-players_table[players_table_active_clause(players_table),,drop=FALSE]
      # is_favorite_filter
      if (!is.null(input$is_favorite_filter)) {
        if (input$is_favorite_filter) {
          players_table <- players_table %>%
            dplyr::filter(fav == TRUE)
        }
      }
      if(isTRUE(input$is_from_futmondo_filter))
        players_table<-players_table[players_table$is_free_agent%in%TRUE,,drop=FALSE]
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
      attr(players_table,"filter_added_fields")<-filter_added_fields
      return(players_table)
    })
    
    output$recent_points_status <- renderText({
      req(is_module_active(),optional_ready())
      status <- tryCatch(recent_points$status(),error=function(e)NULL)
      if(is.null(status))return("Loading round history…")
      messages <- character()
      if(status$remaining+status$pending>0L)messages <- c(messages,paste0("Loading round history: ",
        status$completed %||% (status$total-status$remaining-status$pending),"/",status$total," players."))
      if((status$retrying %||% 0L)>0L)messages <- c(messages,paste0("Retrying ",status$retrying," player histories…"))
      if(status$saving>0L)messages <- c(messages,paste0("Saving history: ",status$saving," pending."))
      if(!is.null(status$save_error))messages <- c(messages,status$save_error)
      if(status$failed>0L){
        references<-unique(vapply(status$failure_details %||% list(),function(x)fm_scalar(x$reference,""),character(1)))
        references<-head(references[nzchar(references)],3L)
        messages <- c(messages,paste0(status$failed," player histories unavailable",
          if(length(references))paste0(" (",paste(references,collapse=", "),")")else "",". Refresh to retry."))
      }
      if(length(status$initial_errors %||% list())){
        references<-unique(vapply(status$initial_errors,function(x)fm_scalar(x$reference,""),character(1)))
        messages <- c(messages,paste0("History source unavailable (",paste(references,collapse=", "),")."))
      }
      if(!status$matrix_complete)messages <- c(messages,"Saved round history is incomplete.")
      paste(messages,collapse=" ")
    })

    # History averages render in JavaScript and update in place. Other columns
    # use R formatters, so changes to those values need a fresh widget payload.
    table_display_data <- reactive({
      rows <- players_table_filtered_RV()
      if(is.null(rows))return(NULL)
      cols_to_hide <- c(cfg_player_columns_to_hide,players_table_duplicate_price_columns(players_table_enriched_RV()),attr(rows,"filter_added_fields"),"data_coverage","observed_at","is_free_agent",
        "recent_points_count","recent_points_required","recent_points_status",
        "clause_suggestedClause","fav","owner_team_id")
      if(isTRUE(hide_bid_column))cols_to_hide<-c(cols_to_hide,"bid_price")
      rows<-rows %>% reorder_player_table_columns() %>%
        dplyr::select(!any_of(setdiff(cols_to_hide,"logo"))) %>%
        dplyr::select(any_of(c("name","role","recent_points_avg","recent_points_ratio")),everything())
      # Source timestamps belong to the resource state, not widget equality.
      rows<-as.data.frame(rows)
      attributes(rows)<-attributes(rows)[intersect(c("names","row.names","class"),names(attributes(rows)))]
      rownames(rows)<-NULL
      rows
    })
    table_display_ids<-reactive(as.character(players_table_filtered_RV()$id))
    table_schema<-reactiveVal(NULL)
    rendered_schema<-NULL;rendered_data<-NULL;rendered_ids<-character();rendered_meta<-NULL
    observe({
      rows<-table_display_data();ids<-table_display_ids();meta<-table_color_meta()
      if(is.null(rows)){table_schema(NULL);return()}
      recent_n<-suppressWarnings(as.integer(input$recent_games_n)[1])
      if(!is.finite(recent_n))recent_n<-5L
      schema<-list(context=table_context_RV(),columns=names(rows),
        types=lapply(rows,function(column)if(is.numeric(column))"numeric"else class(column)),recent_n=recent_n,
        formatted=digest::digest(lapply(rows[setdiff(names(rows),c("recent_points_avg","recent_points_ratio","recent_points_details"))],
          function(column)if(is.numeric(column))as.numeric(column)else column)),ids=ids)
      if(!identical(isolate(table_schema()),schema)){table_schema(schema);return()}
      if(identical(rendered_schema,schema)&&(!identical(rendered_data,rows)||!identical(rendered_ids,ids)||!identical(rendered_meta,meta))){
        page<-isolate(getReactableState("players_table",name="page",session=session)) %||% 1L
        page_size<-isolate(getReactableState("players_table",name="pageSize",session=session)) %||% 20L
        page<-max(1L,min(page,max(1L,ceiling(nrow(rows)/page_size))))
        selected<-isolate(getReactableState("players_table",name="selected",session=session))
        # Selection indexes refer to the previous client dataset. Remap by ID.
        selected<-match(rendered_ids[selected],ids)
        if(!length(selected))selected<-NA_integer_
        updateReactable("players_table",data=rows,meta=meta,page=page,selected=selected,session=session)
        rendered_data<<-rows;rendered_ids<<-ids;rendered_meta<<-meta
      }
    },priority=10)
    output$players_table <- renderReactable({
      schema<-table_schema();req(schema)
      players_table<-isolate(table_display_data());req(players_table)
      rendered_schema<<-schema;rendered_data<<-players_table;rendered_ids<<-isolate(table_display_ids());rendered_meta<<-isolate(table_color_meta())
      table_columns <- get_reactable_columns_for_players(players_table,show_sale_profit=own_squad)
      if("points" %in% names(players_table))table_columns[["points"]]<-reactable::colDef(
        name="Points",align="center",sortNALast=TRUE,minWidth=84,resizable=TRUE,
        style=players_table_metric_style(),cell=players_table_metric_cell(0L))
      recent_n <- suppressWarnings(as.integer(input$recent_games_n)[1])
      if (!is.finite(recent_n)) recent_n <- 5L
      table_columns[["recent_points_avg"]] <- players_table_recent_average_column(recent_n)
      table_columns[["recent_points_details"]] <- reactable::colDef(show=FALSE,searchable=FALSE,filterable=FALSE)
      if(!is.null(points_ratio_basis))table_columns[["recent_points_ratio"]]<-reactable::colDef(
        name=paste0("Avg Last ",recent_n," / ",if(points_ratio_basis=="value")"Value"else"Clause"," (€M)"),
        align="center",sortNALast=TRUE,minWidth=145,headerClass="fm-metric-header",
        header=htmltools::span(paste0("Avg ",recent_n),htmltools::tags$br(),
          paste0("/ ",if(points_ratio_basis=="value")"Value"else"Clause"," (€M)")),
        style=players_table_metric_style(),cell=players_table_metric_cell(3L))
      reactable(players_table,
                columns = table_columns,
                meta = rendered_meta,
                defaultSorted = list(recent_points_avg="desc"),
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
  players_df$recent_points_details <- rep("",count)
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
        if(coverage==n){
          players_df$recent_points_avg[i] <- mean(values);players_df$recent_points_status[i] <- "complete"
          players_df$recent_points_details[i] <- paste0("- round ",rev(wanted),": ",rev(values),collapse="\n")
        }
        else if(coverage)players_df$recent_points_status[i] <- "partial"
      }
    }
  }
  # The source already supplies its recent-score sequence, oldest first.
  # It can answer a short window even while numbered history is sparse or its
  # anchor is unresolved. Do not assign round IDs or create acquisition checks.
  if(n<=5L&&"average.fitness"%in%names(players_df)){
    missing <- which(!is.finite(players_df$recent_points_avg))
    for(i in missing){
      values <- recent_points_fitness_values(players_df$average.fitness[i])
      if(length(values)<n)next
      values <- tail(values,n)
      if(!all(is.finite(values)))next
      players_df$recent_points_avg[i] <- mean(values)
      players_df$recent_points_count[i] <- n
      players_df$recent_points_status[i] <- "reported"
      players_df$recent_points_details[i] <- paste0("Round numbers unavailable; latest scores first:\n",
        paste0("- score: ",rev(values),collapse="\n"))
    }
  }
  if(n==5L&&"average.averageLastFive"%in%names(players_df)){
    fallback <- suppressWarnings(as.numeric(as.character(players_df$average.averageLastFive)))
    use <- !is.finite(players_df$recent_points_avg)&is.finite(fallback)
    players_df$recent_points_avg[use] <- fallback[use];players_df$recent_points_status[use] <- "reported"
    players_df$recent_points_details[use] <- "Individual round scores are unavailable in the reported average."
  }
  players_df
}

# Validate the upstream field before treating an empty list as a successful check.
recent_points_summary_valid <- function(summary) {
  if(!is.list(summary)||!"points"%in%names(summary)||isFALSE(summary$points_received)||
     identical(attr(summary,"fetch_status"),"stale"))return(FALSE)
  points <- summary$points
  if(is.null(points))return(TRUE)
  if(is.data.frame(points))points <- split(points,seq_len(nrow(points)))
  if(!is.list(points)||length(points)>500L)return(FALSE)
  all(vapply(points,function(row){
    if(!is.list(row)||!"round"%in%names(row))return(FALSE)
    score<-row$points
    valid_score<-is.null(score)||(is.atomic(score)&&length(score)==1L&&
      (is.na(score)||((is.numeric(score)||is.character(score))&&is.finite(fm_number(score)))))
    valid_score&&length(row$round)==1L&&is.finite(fm_number(row$round))&&fm_number(row$round)>=1&&
      fm_number(row$round)==floor(fm_number(row$round))
  },logical(1)))
}

# Pure projection shared by interactive reads and autonomous summary publication.
# It never acquires data and retains the summary's original observation time.
recent_points_from_summary <- function(summary,context) {
  if(!recent_points_summary_valid(summary))stop(attr(summary,"fetch_error") %||%
    resource_failure(resource="get_player_recent_points",category="schema"))
  ctx<-context
  checked_at <- attr(summary,"observed_at") %||% Sys.time()
  observed <- normalize_player_match_observations(summary,ctx$player_id,ctx$championship_id,
    season=ctx$season,scoring_version=ctx$scoring_version,observed_at=checked_at,competition_id=ctx$competition_id)
  if(nrow(observed)){
    # Numeric ordinals alone cannot disambiguate distinct cup-stage rounds.
    reconcile_player_points(data.frame(),observed,ctx)
    keys<-intersect(c("round","source_round_key"),names(observed))
    observed<-observed[!duplicated(observed[keys],fromLast=TRUE),,drop=FALSE]
  }
  list(context=ctx,observations=observed,checked_at=checked_at,
    points=reconcile_player_points(data.frame(),observed,ctx),
    previous_points=data.frame(),status=if(nrow(observed))"partial"else "empty")
}
get_player_recent_points <- function(login,championship_id,user_team_id,player_id,scoring_version="unknown") {
  summary <- get_player_summary(login,championship_id,user_team_id,player_id)
  if(!recent_points_summary_valid(summary))stop(attr(summary,"fetch_error") %||%
    resource_failure(resource="get_player_recent_points",category="schema"))
  season <- league_season_context(championship_id,summary$championship$season %||% summary$championship$seasonId,account_id=login[["userid"]])
  ctx <- resource_context(login,championship_id,user_team_id,player_id,season,scoring_version)
  recent_points_from_summary(summary,ctx)
}

# Successful acquisition checks and score observations are durable facts. Their
# observation time orders corrections; elapsed cache time never invalidates them.
# A check acknowledges even sparse/empty history, rather than promising appearances.
recent_points_saved_players <- function(stored,context,checks=data.frame(),now=Sys.time()) {
  saved_ids <- function(rows,stamp){
    rows <- history_context_filter(rows,context)
    if(!nrow(rows)||!all(c("player_id",stamp)%in%names(rows)))return(character())
    observed <- fm_time(rows[[stamp]])
    valid <- !is.na(observed)&observed<=now+60&!is.na(rows$player_id)&nzchar(as.character(rows$player_id))
    unique(as.character(rows$player_id[valid]))
  }
  checks <- history_context_filter(checks,context)
  if(nrow(checks)&&all(c("result_status","point_count")%in%names(checks)))
    checks <- checks[checks$result_status%in%c("scores","empty")&is.finite(checks$point_count)&checks$point_count>=0,,drop=FALSE]
  else checks <- data.frame()
  unique(c(saved_ids(checks,"checked_at"),saved_ids(stored,"observed_at")))
}

# Legacy rows and mapped last-five observations may already cover a longer N.
# Missing source rows can warrant one acquisition; a stored successful check
# separately acknowledges any genuine gaps or explicitly unavailable scores.
recent_points_covered_players <- function(stored,context,n=5L) {
  stored <- history_context_filter(stored,context)
  if(!nrow(stored)||!all(c("player_id","round")%in%names(stored)))return(character())
  rounds <- suppressWarnings(as.numeric(stored$round))
  keep <- is.finite(rounds)&rounds>=1&rounds==floor(rounds)
  stored <- stored[keep,,drop=FALSE];rounds <- rounds[keep]
  if(!length(rounds))return(character())
  latest <- max(rounds)
  if(latest<n)return(character())
  wanted <- seq.int(latest-n+1L,latest)
  groups <- split(rounds,as.character(stored$player_id))
  names(groups)[vapply(groups,function(x)all(wanted%in%x),logical(1))]
}

# Fitness is the source's last five consecutive league rounds, oldest first.
# Its round anchor must come from a matching fresh, explicitly labelled source.
recent_points_fitness_values <- function(value) {
  value <- fm_scalar(value,"")
  if(!nzchar(value))return(numeric())
  parts <- strsplit(value,",",fixed=TRUE)[[1]]
  if(endsWith(value,","))parts <- c(parts,"")
  if(!length(parts)||length(parts)>5L)return(numeric())
  values <- suppressWarnings(as.numeric(parts))
  if(any(is.infinite(values))||any(is.na(values)&!trimws(parts)%in%c("","NA","NaN","null","NULL")))return(numeric())
  values
}

recent_points_fitness_rows <- function(players) {
  if(!is.data.frame(players)||!nrow(players)||
     !all(c("id","average.fitness")%in%names(players))||
     identical(attr(players,"fetch_status"),"stale"))return(data.frame())
  players <- head(players,2000L)
  stamp <- if("observed_at"%in%names(players))fm_time(players$observed_at) else
    rep(fm_time(attr(players,"observed_at") %||% NA_character_),nrow(players))
  field <- function(name)if(name%in%names(players))as.character(players[[name]])else rep("",nrow(players))
  result <- data.frame(player_id=as.character(players$id),fitness=as.character(players$average.fitness),
    observed_at=format(stamp,"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC"),
    championship_id=field("championship_id"),competition_id=field("competition_id"),season=field("season"),scoring_version=field("scoring_version"))
  result$snapshot <- paste(result$observed_at,result$competition_id,result$season,result$scoring_version,sep="\034")
  keep <- !is.na(result$player_id)&nzchar(result$player_id)&!is.na(stamp)&
    vapply(result$fitness,function(x)length(recent_points_fitness_values(x))>0L,logical(1))
  result[keep,,drop=FALSE]
}

# Database agreement may avoid work; it never authorizes a new round assignment.
recent_points_fitness_agrees <- function(rows,points,context) {
  points <- history_context_filter(points,context)
  if(!nrow(rows)||!nrow(points))return(character())
  latest <- max(suppressWarnings(as.numeric(points$round)),na.rm=TRUE)
  if(!is.finite(latest))return(character())
  stamp <- as.numeric(fm_time(points$observed_at));stamp[is.na(stamp)] <- -Inf
  points <- points[order(stamp),,drop=FALSE]
  points <- points[!duplicated(points[c("player_id","round")],fromLast=TRUE),,drop=FALSE]
  groups <- split(points,as.character(points$player_id))
  rows$player_id[vapply(seq_len(nrow(rows)),function(i){
    values <- recent_points_fitness_values(rows$fitness[i]);saved <- groups[[rows$player_id[i]]]
    if(is.null(saved)||length(values)!=min(5L,latest))return(FALSE)
    index <- match(seq.int(latest-length(values)+1L,latest),saved$round)
    if(anyNA(index))return(FALSE)
    old <- suppressWarnings(as.numeric(saved$points[index]))
    all((is.na(values)&is.na(old))|(!is.na(values)&!is.na(old)&values==old))
  },logical(1))]
}

# A newer full-history check supersedes an older unlabelled fitness vector.
# Likewise, a complete window observed after that vector must not be rolled back.
recent_points_fitness_superseded <- function(rows,points,checks,context) {
  if(!nrow(rows))return(character())
  points <- history_context_filter(points,context)
  checks <- history_context_filter(checks,context)
  checked <- recent_points_saved_players(data.frame(),context,checks)
  checks <- checks[checks$player_id%in%checked,,drop=FALSE]
  latest <- if(nrow(points))max(suppressWarnings(as.numeric(points$round)),na.rm=TRUE)else NA_real_
  by_player <- if(nrow(points))split(points,as.character(points$player_id))else list()
  rows$player_id[vapply(seq_len(nrow(rows)),function(i){
    observed <- fm_time(rows$observed_at[i])
    player_checks <- checks[checks$player_id==rows$player_id[i],,drop=FALSE]
    if(nrow(player_checks)&&any(fm_time(player_checks$checked_at)>=observed,na.rm=TRUE))return(TRUE)
    saved <- by_player[[rows$player_id[i]]]
    values <- recent_points_fitness_values(rows$fitness[i])
    if(is.null(saved)||!is.finite(latest)||length(values)!=min(5L,latest))return(FALSE)
    saved <- saved[order(fm_time(saved$observed_at)),,drop=FALSE]
    saved <- saved[!duplicated(saved$round,fromLast=TRUE),,drop=FALSE]
    index <- match(seq.int(latest-length(values)+1L,latest),saved$round)
    !anyNA(index)&&all(fm_time(saved$observed_at[index])>=observed,na.rm=FALSE)
  },logical(1))%in%TRUE]
}

recent_points_fitness_anchor <- function(row,payload,context) {
  if(!is.data.frame(row)||nrow(row)!=1L||is.null(payload$context))return(NULL)
  for(field in c("account_id","championship_id","user_team_id","competition_id","season","scoring_version"))
    if(!identical(fm_scalar(payload$context[[field]],"unknown"),fm_scalar(context[[field]],"unknown")))return(NULL)
  if(!identical(as.character(payload$context$player_id),as.character(row$player_id)))return(NULL)
  for(field in c("championship_id","competition_id","season","scoring_version"))
    if(field%in%names(row)&&!is.na(row[[field]])&&nzchar(row[[field]])&&row[[field]]!=context[[field]])return(NULL)
  checked <- fm_time(payload$checked_at)
  if(length(checked)!=1L||is.na(checked)||checked<fm_time(row$observed_at))return(NULL)
  values <- recent_points_fitness_values(row$fitness)
  known <- is.finite(values)
  if(sum(known)<2L||length(unique(values[known]))<2L)return(NULL)
  points <- history_context_filter(payload$observations,payload$context)
  if(!nrow(points)||!all(c("round","points")%in%names(points)))return(NULL)
  rounds <- suppressWarnings(as.numeric(points$round))
  if(any(!is.finite(rounds)|rounds<1|rounds!=floor(rounds))||anyDuplicated(rounds))return(NULL)
  latest <- max(rounds)
  if(length(values)!=min(5L,latest))return(NULL)
  matches <- rounds[vapply(rounds,function(end){
    if(end<length(values))return(FALSE)
    wanted <- seq.int(end-length(values)+1L,end)
    scores <- suppressWarnings(as.numeric(points$points[match(wanted,rounds)]))
    all(is.finite(scores[known]))&&all(scores[known]==values[known])
  },logical(1))]
  if(length(matches)!=1L||matches[1]!=latest)return(NULL)
  list(round=latest,snapshot=row$snapshot,context=context)
}

recent_points_fitness_observations <- function(rows,anchor) {
  if(is.null(anchor)||!nrow(rows))return(data.frame())
  ctx <- anchor$context
  rows <- rows[rows$snapshot==anchor$snapshot,,drop=FALSE]
  for(field in c("championship_id","competition_id","season","scoring_version"))
    if(field%in%names(rows))rows <- rows[is.na(rows[[field]])|!nzchar(rows[[field]])|rows[[field]]==ctx[[field]],,drop=FALSE]
  values <- lapply(rows$fitness,recent_points_fitness_values)
  size <- min(5L,anchor$round)
  keep <- lengths(values)==size
  rows <- rows[keep,,drop=FALSE];values <- values[keep]
  if(!nrow(rows))return(data.frame())
  points <- unlist(values,use.names=FALSE)
  data.frame(player_id=rep(rows$player_id,each=size),championship_id=ctx$championship_id,
    competition_id=fm_scalar(ctx$competition_id,"unknown"),season=ctx$season,scoring_version=ctx$scoring_version,
    round=rep(seq.int(anchor$round-size+1L,anchor$round),nrow(rows)),
    source_round_key=paste0("number:",rep(seq.int(anchor$round-size+1L,anchor$round),nrow(rows))),points=points,
    score_status=ifelse(is.finite(points),"unknown","unavailable"),
    observed_at=rep(rows$observed_at,each=size),occurred_at=NA_character_,round_start_at=NA_character_)
}

recent_points_fitness_changes <- function(observations,existing) {
  if(!nrow(observations)||!is.data.frame(existing)||!nrow(existing))return(observations)
  observations<-recent_points_identity_rows(observations);existing<-recent_points_identity_rows(existing)
  keys <- c("player_id","championship_id","competition_id","season","scoring_version","round","source_round_key")
  stamp <- as.numeric(fm_time(existing$observed_at));stamp[is.na(stamp)] <- -Inf
  existing <- existing[order(stamp),,drop=FALSE];stamp <- sort(stamp)
  keep <- !duplicated(existing[keys],fromLast=TRUE)
  key <- function(rows)do.call(paste,c(rows[keys],sep="\034"))
  index <- match(key(observations),key(existing[keep,,drop=FALSE]))
  previous <- stamp[keep][index]
  old <- suppressWarnings(as.numeric(existing$points[keep][index]))
  same <- (is.na(observations$points)&is.na(old))|
    (is.finite(observations$points)&is.finite(old)&observations$points==old)
  # Keep newer corrections and avoid revisions that only downgrade finality.
  observations[is.na(index)|(!same&(is.na(previous)|as.numeric(fm_time(observations$observed_at))>previous)),,drop=FALSE]
}

# Bound queued retries independently from the retained score matrix.
recent_points_fitness_buffer <- function(rows) {
  if(!nrow(rows))return(rows)
  rows<-recent_points_identity_rows(rows)
  rows <- rows[order(fm_time(rows$observed_at),decreasing=TRUE),,drop=FALSE]
  head(rows[!duplicated(rows[c("player_id","championship_id","competition_id","season","scoring_version","round","source_round_key")]),,drop=FALSE],10000L)
}

recent_points_identity_rows <- function(rows) {
  if(!is.data.frame(rows)||!nrow(rows))return(rows)
  if(!"competition_id"%in%names(rows))rows$competition_id<-"unknown"
  rows$competition_id[is.na(rows$competition_id)|!nzchar(rows$competition_id)]<-"unknown"
  if(!"source_round_key"%in%names(rows)){
    id<-as.character(rows$round_id %||% rep("",nrow(rows)))
    rows$source_round_key<-ifelse(!is.na(id)&nzchar(id),paste0("id:",id),paste0("number:",rows$round))
  }
  rows
}

round_points_client_context <- function(client) {
  context<-if(is.list(client$context))client$context else resource_context(client$auth,client$league,client$team)
  context$competition_id<-fm_scalar(context$competition_id,"unknown")
  context
}
round_points_client_key <- function(client) {
  context<-round_points_client_context(client)
  paste(vapply(c("account_id","championship_id","user_team_id","competition_id","season","scoring_version"),
    function(field)fm_scalar(context[[field]],"unknown"),character(1)),collapse="\034")
}

# A single root-session controller owns the league read and API acquisition.
# Module wrappers below only register visible IDs and join published snapshots.
new_shared_round_points_controller <- function(root,publish_interval=5,retry_delays=c(1,3),retry_budget=32L) {
  stopifnot(is.numeric(retry_delays),length(retry_delays)<=2L,all(is.finite(retry_delays)&retry_delays>=0),
    length(retry_budget)==1L,is.finite(retry_budget),retry_budget>=0,retry_budget<=32L)
  clients <- shiny::reactiveVal(list());revision <- shiny::reactiveVal(0L)
  published <- shiny::reactiveVal(list(key=NULL,points=data.frame(),context=NULL))
  entries <- list();current_key <- NULL;generation <- 0L;closed <- FALSE
  publish_timer <- NULL;last_publish <- -Inf;previous_refresh <- list();pending_refresh <- list()
  retry_timer <- NULL;retry_deadline <- Inf
  touch <- function()shiny::isolate(revision(revision()+1L))
  active_clients <- function()Filter(function(x)isTRUE(x$active)&&valid_login(x$auth)&&
    nzchar(x$league)&&nzchar(x$team),shiny::isolate(clients()))
  key_for <- round_points_client_key
  current_clients <- function(){all<-active_clients();Filter(function(x)identical(key_for(x),current_key),all)}
  current_entry <- function()if(is.null(current_key))NULL else entries[[current_key]]
  trim_entries <- function(){
    weight<-function(entry)nrow(entry$points)+sum(vapply(entry$live,nrow,integer(1)))
    while(length(entries)>2L||sum(vapply(entries,weight,numeric(1)))>50000L){
      victims<-setdiff(names(entries),current_key);if(!length(victims))break
      entries[[victims[[1]]]]<<-NULL
    }
  }
  cancel_timer <- function(){if(is.function(publish_timer))publish_timer();publish_timer<<-NULL}
  cancel_retry <- function(){if(is.function(retry_timer))retry_timer();retry_timer<<-NULL;retry_deadline<<-Inf}
  retry_waiting <- function(entry,player){
    attempt<-entry$attempted[[player]]
    !is.null(attempt)&&!isTRUE(attempt$ok)&&!isTRUE(attempt$terminal)&&
      length(attempt$retry_at)==1L&&is.finite(attempt$retry_at)&&entry$retry_reads<retry_budget
  }
  schedule_retry <- function(entry){
    players<-setdiff(names(entry$attempted),names(entry$pending))
    due<-vapply(players,function(player)if(retry_waiting(entry,player))entry$attempted[[player]]$retry_at else Inf,numeric(1))
    due<-due[is.finite(due)&due>as.numeric(Sys.time())]
    next_deadline<-if(length(due))min(due)else Inf
    if(identical(next_deadline,retry_deadline))return()
    cancel_retry()
    if(!is.finite(next_deadline))return()
    expected<-generation;retry_deadline<<-next_deadline
    retry_timer<<-later::later(function(){
      retry_timer<<-NULL;retry_deadline<<-Inf
      if(!closed&&identical(generation,expected)&&identical(current_key,entry$key)&&length(current_clients()))touch()
    },max(0,next_deadline-as.numeric(Sys.time())))
  }
  clear_pending <- function(entry){
    if(is.null(entry))return()
    for(player in names(entry$pending)){
      ctx<-entry$context;ctx$player_id<-player
      invalidate_resources(ctx,resources="player_summary_points")
    }
    for(snapshot in names(entry$fitness_probes))if(entry$fitness_probes[[snapshot]]%in%names(entry$pending))
      entry$fitness_probe_attempted[[snapshot]]<-NULL
    entry$fitness_dirty<-TRUE
    entry$pending<-list()
    # Cancel bootstrap continuations as well as player reads. A late lineup
    # response must not start a matrix request for a context left behind.
    for(type in names(entry$initial_pending)){
      entry$initial_pending[[type]]<-NULL;entry$loading[[type]]<-FALSE
      entry$initial_errors[[type]]<-resource_failure(resource="database",category="network")
    }
  }
  context_for <- function(entry){ctx<-entry$context;ctx$season<-entry$season;ctx$scoring_version<-entry$scoring;ctx}
  visible_ids <- function()unique(unlist(lapply(current_clients(),`[[`,"ids"),use.names=FALSE))
  catalog_requested <- function()any(vapply(current_clients(),function(client)!identical(client$catalog_scope,FALSE),logical(1)))
  ids_for <- function(entry)head(unique(c(visible_ids(),if(catalog_requested())entry$catalog_ids)),2000L)
  saved_ids_for <- function(entry){
    ctx<-context_for(entry);scope<-ctx[c("championship_id","competition_id","season","scoring_version")];scope$players<-sort(ids_for(entry));now<-as.numeric(Sys.time())
    cache<-entry$saved_ids_cache
    if(is.null(cache)||!identical(cache$scope,scope)||now>=cache$valid_until){
      rows<-history_context_filter(entry$points,ctx);ids<-character();valid_until<-Inf
      if(nrow(rows)&&all(c("player_id","observed_at")%in%names(rows))){
        observed<-as.numeric(fm_time(rows$observed_at))
        valid<-is.finite(observed)&observed<=now+60&!is.na(rows$player_id)&nzchar(as.character(rows$player_id))
        ids<-intersect(unique(as.character(rows$player_id[valid])),scope$players)
        future<-observed[is.finite(observed)&observed>now+60]
        if(length(future))valid_until<-min(future)-60
      }
      entry$saved_ids_cache<-list(scope=scope,ids=ids,valid_until=valid_until)
    }
    entry$saved_ids_cache$ids
  }
  latest_round_for <- function(entry){
    ctx<-context_for(entry);scope<-ctx[c("championship_id","competition_id","season","scoring_version")]
    if(is.null(entry$latest_round_cache)||!identical(entry$latest_round_cache$scope,scope)){
      rounds<-unlist(lapply(c(list(entry$points),entry$live),function(rows)history_context_filter(rows,ctx)$round),use.names=FALSE)
      rounds<-suppressWarnings(as.numeric(rounds));rounds<-rounds[is.finite(rounds)&rounds>=1]
      entry$latest_round_cache<-list(scope=scope,value=if(length(rounds))max(rounds)else 0)
    }
    entry$latest_round_cache$value
  }
  covered_for <- function(entry,n){
    scope<-context_for(entry)[c("championship_id","competition_id","season","scoring_version")];scope$players<-sort(ids_for(entry))
    if(is.null(entry$coverage_cache)||!identical(entry$coverage_cache$scope,scope))
      entry$coverage_cache<-list(scope=scope,by_n=list())
    key<-as.character(n);cached<-entry$coverage_cache$by_n[[key]]
    if(is.null(cached)){
      existing<-dplyr::bind_rows(c(list(entry$points),entry$live))
      cached<-intersect(recent_points_covered_players(existing,context_for(entry),n),scope$players)
      entry$coverage_cache$by_n[[key]]<-cached
      entry$coverage_cache$by_n<-tail(entry$coverage_cache$by_n,20L)
    }
    cached
  }
  update_fitness <- function(entry){
    rows <- dplyr::bind_rows(c(lapply(current_clients(),`[[`,"fitness"),list(entry$catalog_fitness)))
    if(nrow(rows)){
      rows <- rows[order(fm_time(rows$observed_at),decreasing=TRUE),,drop=FALSE]
      rows <- head(rows[!duplicated(rows$player_id),,drop=FALSE],2000L)
      rownames(rows) <- NULL
    }
    if(!identical(rows,entry$fitness)){entry$fitness<-rows;entry$fitness_dirty<-TRUE}
  }
  merge_live <- function(entry,observations){
    if(nrow(observations)){
      entry$coverage_cache<-NULL
      previous<-latest_round_for(entry)
      rounds<-suppressWarnings(as.numeric(history_context_filter(observations,context_for(entry))$round))
      rounds<-rounds[is.finite(rounds)&rounds>=1]
      if(length(rounds)&&max(rounds)>previous){
        entry$latest_round_cache$value<-max(rounds)
        entry$fitness_dirty<-TRUE
      }
    }
    observations<-recent_points_identity_rows(observations)
    groups <- split(observations,as.character(observations$player_id))
    for(player in names(groups)){
      rows <- dplyr::bind_rows(entry$live[[player]],groups[[player]])
      stamp <- as.numeric(fm_time(rows$observed_at));stamp[is.na(stamp)] <- -Inf
      rows <- rows[order(stamp,rows$score_status=="final"),,drop=FALSE]
      entry$live[[player]] <- rows[!duplicated(rows[c("championship_id","competition_id","season","scoring_version","round","source_round_key")],fromLast=TRUE),,drop=FALSE]
    }
  }
  prepare_fitness <- function(entry){
    if(!entry$fitness_dirty)return()
    entry$fitness_dirty <- FALSE
    rows <- entry$fitness;ctx <- context_for(entry)
    entry$fitness_probes <- list();entry$fitness_holding <- character();entry$fitness_ready <- character();entry$fitness_changed <- character()
    if(!nrow(rows))return()
    for(field in c("championship_id","competition_id","season","scoring_version"))
      if(field%in%names(rows))rows <- rows[is.na(rows[[field]])|!nzchar(rows[[field]])|rows[[field]]==ctx[[field]],,drop=FALSE]
    existing <- dplyr::bind_rows(c(list(entry$points),entry$live))
    entry$fitness_ready <- unique(c(recent_points_fitness_agrees(rows,existing,ctx),
      recent_points_fitness_superseded(rows,existing,entry$checks,ctx)))
    entry$fitness_changed <- setdiff(rows$player_id,entry$fitness_ready)
    for(snapshot in unique(rows$snapshot)){
      group <- rows[rows$snapshot==snapshot,,drop=FALSE]
      anchor <- entry$fitness_anchors[[snapshot]]
      if(!is.null(anchor)&&identical(anchor$context,ctx)){
        observations <- recent_points_fitness_observations(group,anchor)
        entry$fitness_ready <- unique(c(entry$fitness_ready,observations$player_id))
        changes <- recent_points_fitness_changes(observations,existing)
        projected <- nrow(entry$points)+sum(vapply(entry$live,nrow,integer(1)))+nrow(changes)
        if(projected>50000L){entry$overflow<-TRUE;next}
        if(nrow(changes)){
          merge_live(entry,changes)
          if(!worker_database_mode(entry))entry$fitness_unsaved <- recent_points_fitness_buffer(dplyr::bind_rows(entry$fitness_unsaved,changes))
          trim_entries()
        }
      }else if(!all(group$player_id%in%entry$fitness_ready)&&
        (is.null(entry$fitness_probe_attempted[[snapshot]])||any(group$player_id%in%names(entry$pending))||
         any(vapply(group$player_id,function(player)retry_waiting(entry,player),logical(1))))){
        candidates <- group[group$player_id%in%entry$fitness_changed&vapply(group$fitness,function(x){v<-recent_points_fitness_values(x);sum(is.finite(v))>=2L&&length(unique(v[is.finite(v)]))>=2L},logical(1)),,drop=FALSE]
        eligible<-vapply(candidates$player_id,function(player){attempt<-entry$attempted[[player]];is.null(attempt)||(!isTRUE(attempt$ok)&&!isTRUE(attempt$terminal))},logical(1))
        candidates<-candidates[eligible,,drop=FALSE]
        if(nrow(candidates)){
          preferred <- candidates$player_id[candidates$player_id%in%visible_ids()]
          player <- c(preferred,candidates$player_id)[1]
          entry$fitness_probes[[snapshot]] <- player
          entry$fitness_holding <- unique(c(entry$fitness_holding,intersect(group$player_id,entry$fitness_changed)))
        }
      }
    }
    entry$fitness_changed <- setdiff(entry$fitness_changed,entry$fitness_ready)
  }
  worker_database_mode<-function(entry)fm_worker_configured()&&isTRUE(fm_worker_mode(entry$context$account_id))
  save_fitness <- function(entry){
    if(worker_database_mode(entry))return(invisible(NULL))
    if(!nrow(entry$fitness_unsaved))return()
    capacity <- max(0L,20L-background_sync_status(entry$context$account_id)$pending)
    for(i in seq_len(min(capacity,ceiling(nrow(entry$fitness_unsaved)/100L))))local({
      queued <- c(.persistence_state$queue,if(!is.null(.persistence_state$active))list(.persistence_state$active)else list())
      bytes <- sum(vapply(queued,function(job)as.numeric(job$bytes %||% 0),numeric(1)))
      if(length(queued)>=190L||bytes+as.numeric(object.size(head(entry$fitness_unsaved,100L)))>=14*1024^2)return()
      chunk <- complete_point_identity(head(entry$fitness_unsaved,100L),context_for(entry))
      entry$fitness_unsaved <- entry$fitness_unsaved[-seq_len(nrow(chunk)),,drop=FALSE]
      entry$saving <- entry$saving+1L;did_ack <- FALSE
      acknowledged <- function(ok){
        if(did_ack)return(NULL)
        did_ack <<- TRUE
        if(closed||!identical(entries[[entry$key]],entry))return(NULL)
        entry$saving <- max(0L,entry$saving-1L)
        if(!isTRUE(ok)){
          entry$fitness_failed <- recent_points_fitness_buffer(dplyr::bind_rows(entry$fitness_failed,chunk))
          entry$save_errors <- unique(c(entry$save_errors,chunk$player_id))
        }else entry$save_errors <- setdiff(entry$save_errors,setdiff(chunk$player_id,entry$fitness_failed$player_id))
        touch();NULL
      }
      ok <- tryCatch(defer_persistence("supabase_post_direct",list("player_match_observations",chunk,
        "player_id,championship_id,competition_id,season,scoring_version,round,source_round_key,observed_at",FALSE),
        context_for(entry),on_complete=acknowledged),error=function(e)FALSE)
      if(!isTRUE(ok))acknowledged(FALSE)
    })
  }
  availability_for <- function(entry){
    candidates<-ids_for(entry);ctx<-context_for(entry)
    checked<-recent_points_saved_players(data.frame(),ctx,entry$checks)
    # An old successful check proves a source read, not coverage for a newly
    # requested wider window. Probe only visible players missing numbered rows.
    requested_missing<-character();demanded<-integer();windows<-list()
    demanding<-Filter(function(x)length(x$ids)>0L,current_clients())
    for(client in demanding){
      covered<-covered_for(entry,client$n)
      if(length(client$window)){
        existing<-history_context_filter(dplyr::bind_rows(c(list(entry$points),entry$live)),ctx)
        by_player<-if(nrow(existing))split(existing,as.character(existing$player_id))else list()
        covered<-client$ids[vapply(client$ids,function(player){
          rows<-by_player[[player]];if(is.null(rows))return(FALSE)
          rows<-rows[!duplicated(rows$round,fromLast=TRUE),,drop=FALSE]
          all(is.finite(rows$points[match(client$window,rows$round)]))
        },logical(1))]
      }
      missing<-setdiff(client$ids,if(client$n>5L||length(client$window))covered else c(checked,covered))
      requested_missing<-unique(c(requested_missing,missing))
      if(length(client$window))for(player in missing)windows[[player]]<-sort(unique(c(windows[[player]],client$window)))
      prior<-demanded[missing];prior[is.na(prior)]<-0L
      demanded[missing]<-pmax(prior,as.integer(client$n))
    }
    reused<-setdiff(unique(c(saved_ids_for(entry),checked,entry$fitness_ready)),
      c(entry$forced,requested_missing,entry$fitness_changed))
    blocked<-names(entry$attempted)[vapply(names(entry$attempted),function(player){
      attempt<-entry$attempted[[player]];need<-demanded[player]
      if(isTRUE(attempt$ok)&&length(need)&&!is.na(need)){
        wanted_window<-windows[[player]]
        if(length(wanted_window)&&!identical(attempt$window,wanted_window))return(FALSE)
        seen<-as.integer(attempt$required_rounds %||% 0L)
        return(seen>5L||seen>=need)
      }
      !retry_waiting(entry,player)||attempt$retry_at>as.numeric(Sys.time())
    },logical(1))]
    normal<-setdiff(candidates,c(blocked,names(entry$pending),reused,entry$fitness_holding))
    probes<-setdiff(unlist(entry$fitness_probes,use.names=FALSE),c(blocked,names(entry$pending)))
    list(needed=unique(c(probes,normal)),reused=reused)
  }
  needed_for <- function(entry)availability_for(entry)$needed
  completed_for <- function(entry){
    if(!is.null(entry$initial_errors$lineup)||!is.null(entry$initial_errors$context))return(character())
    available<-availability_for(entry)
    good<-names(entry$attempted)[vapply(entry$attempted,function(x)isTRUE(x$ok)&&
      identical(x$season,entry$season)&&identical(fm_scalar(x$competition_id,"unknown"),fm_scalar(entry$context$competition_id,"unknown"))&&identical(x$scoring_version,entry$scoring),logical(1))]
    setdiff(unique(c(setdiff(good,available$needed),available$reused)),names(entry$pending))
  }
  failed_for <- function(entry,completed=completed_for(entry)){
    setdiff(names(entry$attempted)[vapply(names(entry$attempted),function(player)
      !isTRUE(entry$attempted[[player]]$ok)&&!retry_waiting(entry,player)&&!player%in%names(entry$pending),logical(1))],completed)
  }
  assemble <- function(entry){
    ctx<-context_for(entry);saved<-history_context_filter(entry$points,ctx)
    live<-lapply(entry$live,function(x)history_context_filter(x,ctx))
    by_player<-if(nrow(saved))split(saved,as.character(saved$player_id))else list()
    ids<-unique(c(names(by_player),names(live)))
    rows<-lapply(ids,function(player){
      player_ctx<-ctx;player_ctx$player_id<-player
      pts<-reconcile_player_points(by_player[[player]],live[[player]],player_ctx)
      if(!nrow(pts))return(NULL)
      data.frame(player_id=player,round=pts$round_number,points=pts$points,
        competition_id=rep(fm_scalar(ctx$competition_id,"unknown"),nrow(pts)),season=rep(paste0(ctx$season),nrow(pts)),scoring_version=rep(paste0(ctx$scoring_version),nrow(pts)),
        source_round_key=pts$source_round_key,round_id=pts$round_id,match_id=pts$match_id,source_round_number=pts$source_round_number)
    })
    result<-head(dplyr::bind_rows(rows),50000L)
    rounds<-suppressWarnings(as.numeric(result$round));rounds<-rounds[is.finite(rounds)&rounds>=1]
    if(length(rounds))attr(result,"latest_round")<-max(rounds)
    result
  }
  publish <- function(entry,force=FALSE){
    if(closed||!identical(entry$key,current_key))return()
    elapsed<-as.numeric(Sys.time())-last_publish
    visible<-visible_ids()
    completed<-completed_for(entry)
    finished<-all(visible%in%c(completed,failed_for(entry,completed)))&&
      !any(visible%in%entry$fitness_holding)
    signature<-list(ids=visible,context=context_for(entry)[c("competition_id","season","scoring_version")])
    just_finished<-finished&&!identical(signature,entry$finished_signature)
    if(!finished)entry$finished_signature<-NULL
    if(!force&&!just_finished&&elapsed<publish_interval){
      if(is.null(publish_timer)){
        expected<-generation
        publish_timer<<-later::later(function(){
          publish_timer<<-NULL
          if(!closed&&identical(generation,expected)&&length(current_clients()))
            shiny::isolate(publish(entry,TRUE))
        },max(0,publish_interval-elapsed))
      }
      return()
    }
    cancel_timer();last_publish<<-as.numeric(Sys.time())
    if(finished)entry$finished_signature<-signature
    entry$published_ids<-visible
    assembled<-tryCatch(assemble(entry),error=function(error){
      entry$initial_errors$matrix<-resource_log_failure(error,"database")
      NULL
    })
    if(is.null(assembled))return(invisible(FALSE))
    snapshot<-list(key=entry$key,points=assembled,context=context_for(entry))
    if(!identical(shiny::isolate(published()),snapshot))published(snapshot)
  }
  adopt_resolved_context <- function(entry,resolved){
    resolved$session_id<-root$token
    old_key<-entry$key;new_key<-round_points_client_key(list(context=resolved))
    entry$context<-resolved;entry$season<-resolved$season;entry$scoring<-resolved$scoring_version
    if(identical(new_key,old_key))return(entry)
    all<-shiny::isolate(clients())
    for(id in names(all))if(valid_login(all[[id]]$auth)&&identical(key_for(all[[id]]),old_key)){
      all[[id]]$context<-resolved;all[[id]]$context$session_id<-""
    }
    prior<-entries[[new_key]]
    entries[[old_key]]<<-NULL
    if(is.null(prior)){entry$key<-new_key;entries[[new_key]]<<-entry;prior<-entry}
    if(identical(current_key,old_key))current_key<<-new_key
    clients(all)
    prior
  }
  # Retain bounded interests after promise settlement so worker publications can
  # repair an initial failure or revise an acquired history without page reload.
  track_worker_read <- function(entry,handler,args,context,type,player=NULL){
    if(!fm_worker_configured()||!startsWith(handler,"get_")||identical(fm_worker_mode(context$account_id),FALSE))return(NULL)
    descriptor<-fm_worker_descriptor(handler,args,context)
    state<-fm_worker_account_state(context$account_id)
    id<-if(is.null(player))paste0("initial:",type)else paste0("player:",player)
    entry$worker_reads[[id]]<-list(descriptor=descriptor,type=type,player=player,
      revision=shiny::isolate(fm_worker_resource_revision(state,descriptor$resource_key)()))
    # Match the worker's 128-resource demand budget; keep bootstrap interests.
    players<-grep("^player:",names(entry$worker_reads),value=TRUE)
    if(length(entry$worker_reads)>128L)entry$worker_reads[head(players,length(entry$worker_reads)-128L)]<-NULL
    live<-function()isTRUE(shiny::isolate(!closed&&identical(current_entry(),entry)&&length(current_clients())>0L))
    attr(live,"fm_worker_interest")<-paste(root$token,"round-points",id,sep=":")
    live
  }
  load_initial <- function(entry,client,types=c("matrix","lineup","catalog")){
    key<-entry$key
    calls<-list(context=list("get_resolved_league_context",list(login=client$auth,championship_id=client$league,user_team_id=client$team)),
      matrix=list("read_league_round_dataset",list(championship_id=client$league,context=context_for(entry))),
      lineup=list("get_lineup_from_team",list(login=client$auth,championship_id=client$league,user_team_id=client$team)),
      catalog=list("get_championship_players",list(login=client$auth,championship_id=client$league)))
    if("context"%in%types)types<-"context"
    if("lineup"%in%types)types<-setdiff(types,"matrix")
    if(!catalog_requested())types<-setdiff(types,"catalog")
    for(kind in intersect(types,names(calls)))local({
      if(isTRUE(entry$loading[[kind]])&&!is.null(entry$initial_pending[[kind]]))return()
      type<-kind;call<-calls[[type]]
      entry$loading[[type]]<-TRUE
      entry$initial_sequence<-entry$initial_sequence+1L;request_token<-entry$initial_sequence
      entry$initial_pending[[type]]<-request_token
      current<-function()!closed&&identical(current_key,key)&&length(current_clients())>0L&&
        identical(entries[[key]],entry)&&identical(entry$initial_pending[[type]],request_token)
      live<-track_worker_read(entry,call[[1]],call[[2]],context_for(entry),type)
      promise<-tryCatch(async_adapter_fetch(call[[1]],call[[2]],context=context_for(entry),is_current=live %||% current),
        error=function(error)promises::promise_reject(error))
      finish<-function(data,error=NULL){
        if(!current())return(NULL)
        entry$loading[[type]]<-FALSE
        entry$initial_pending[[type]]<-NULL
        if(is.null(error))error<-attr(data,"fetch_error")
        # Expired successful snapshots remain source evidence. A captured source
        # failure is still an error; expiry alone must not block every player.
        if(is.null(error)&&isTRUE(attr(data,"worker_completeness")%in%c("partial","unavailable")))
          error<-resource_failure(resource=call[[1]],category="schema")
        if(is.null(error)&&identical(attr(data,"fetch_status"),"stale")){
          # Old bootstrap metadata cannot establish a new season or contradict
          # scoring already resolved for the active source context.
          expected<-context_for(entry)
          if(type=="context"){
            fields<-c("competition_id","season")
            known<-all(vapply(expected[fields],fm_context_known,logical(1)))
            if(!known||!identical(unclass(data)[fields],unclass(expected)[fields]))
              error<-resource_failure(resource=call[[1]],category="schema")
          }
          if(type=="lineup"&&fm_context_known(entry$scoring)&&
             !isTRUE(tryCatch(identical(paste0(normalize_league_rules(list(),data,client$league)$scoring_version),entry$scoring),error=function(e)FALSE)))
            error<-resource_failure(resource=call[[1]],category="schema")
        }
        entry$initial_errors[[type]]<-if(is.null(error))NULL else resource_log_failure(error,call[[1]])
        tryCatch({
        if(type=="context"){
          valid<-is.null(error)&&is.list(data)&&identical(data$account_id,entry$context$account_id)&&
            identical(data$championship_id,client$league)&&identical(data$user_team_id,client$team)&&
            all(vapply(data[c("competition_id","season")],fm_context_known,logical(1)))&&
            all(c("competition_id","season")%in%names(data))
          if(!isTRUE(valid)){
            entry$initial_errors$context<-resource_log_failure(error %||% simpleError("Competition and season are not established"),"database")
            entry$loading[c("matrix","lineup","catalog")]<-FALSE
          }else{
            selected<-adopt_resolved_context(entry,data)
            if(!identical(selected,entry)){
              if(length(selected$initial_errors))load_initial(selected,client,names(selected$initial_errors))
              publish(selected,TRUE)
            }else if(fm_context_known(entry$scoring)){
              # Authenticated source metadata or a still-valid saved context
              # already establishes scoring. Reuse it before acquisition.
              entry$loading[["lineup"]]<-FALSE
              entry$initial_errors$lineup<-NULL
              load_initial(entry,client,c("matrix","catalog"))
            }else load_initial(entry,client,c("lineup","catalog"))
          }
        }
        if(type=="matrix"&&is.list(data)){
          entry$points<-head(recent_points_identity_rows(data$points %||% data.frame()),50000L)
          entry$saved_ids_cache<-NULL;entry$coverage_cache<-NULL;entry$latest_round_cache<-NULL;entry$fitness_dirty<-TRUE
          entry$checks<-head(data$checks %||% data.frame(),2000L)
          entry$matrix_complete<-isTRUE(data$complete)
          trim_entries()
        }
        if(type=="lineup"){
          if(is.null(error)&&!is.null(data)){
            entry$scoring<-paste0(normalize_league_rules(list(),data,client$league)$scoring_version)
            # The first matrix read must carry resolved scoring, never bootstrap
            # unknown values that can fill the row budget with another context.
            if(isTRUE(entry$requires_context)&&!fm_context_known(entry$scoring)){
              entry$initial_errors$lineup<-resource_log_failure(simpleError("Scoring configuration is not established"),"get_lineup_from_team")
              entry$loading[["matrix"]]<-FALSE
            }else load_initial(entry,client,"matrix")
          }else entry$loading[["matrix"]]<-FALSE
        }
        if(type=="catalog"&&is.null(error))entry$catalog_loaded<-TRUE
        if(type=="catalog"&&is.data.frame(data)&&"id"%in%names(data)){
          entry$catalog_loaded<-TRUE
          entry$catalog_ids<-head(unique(as.character(data$id)),2000L)
          entry$catalog_fitness<-recent_points_fitness_rows(data)
        }
        if(!any(entry$loading)&&identical(current_key,key)){
          update_fitness(entry);prepare_fitness(entry);publish(entry,TRUE)
        }
        },error=function(error){entry$initial_errors[[type]]<-resource_log_failure(error,call[[1]])})
        touch()
        NULL
      }
      promises::then(promise,finish,function(error)finish(NULL,error))
    })
    invisible(NULL)
  }
  # Retain bounded failed factual payloads for source-free persistence retry.
  save_round_snapshot<-function(entry,player_id,payload){
    database_only<-worker_database_mode(entry)
    # Only a worker snapshot published after factual rows AND the completion
    # marker were acknowledged can supply that marker locally. Older snapshots
    # remain displayable, but the GUI never invents durability or writes them.
    durable<-isTRUE(attr(payload,"worker_history_persisted"))&&
      isTRUE(attr(payload,"worker_acquisition_status")%in%c("ok","empty","partial"))&&
      is.null(attr(payload,"fetch_error"))
    if(database_only&&!durable)return(invisible(NULL))
    entry$saving<-entry$saving+1L
    stamp<-as.numeric(fm_time(payload$checked_at))
    entry$save_latest[[player_id]]<-max(stamp,entry$save_latest[[player_id]] %||% -Inf)
    if(length(entry$save_latest)>2000L)entry$save_latest<-tail(entry$save_latest,2000L)
    entry$save_retry[[player_id]]<-NULL
    did_ack<-FALSE
    acknowledged<-function(ok){
      if(did_ack)return(NULL)
      did_ack<<-TRUE
      if(closed||!identical(entries[[entry$key]],entry))return(NULL)
      entry$saving<-max(0L,entry$saving-1L)
      if(stamp<(entry$save_latest[[player_id]] %||% stamp)){touch();return(NULL)}
      if(isTRUE(ok)){
        marker<-data.frame(championship_id=payload$context$championship_id,player_id=player_id,
          competition_id=fm_scalar(payload$context$competition_id,"unknown"),season=payload$context$season,scoring_version=payload$context$scoring_version,
          checked_at=format(fm_time(payload$checked_at),"%Y-%m-%dT%H:%M:%OS6Z",tz="UTC"),
          result_status=if(nrow(payload$observations))"scores"else "empty",point_count=nrow(payload$observations),row.names=NULL)
        entry$checks<-dplyr::bind_rows(entry$checks,marker)
        entry$checks<-tail(entry$checks[!duplicated(entry$checks[c("championship_id","player_id","competition_id","season","scoring_version")],fromLast=TRUE),,drop=FALSE],2000L)
        entry$save_errors<-setdiff(entry$save_errors,player_id)
        entry$save_retry[[player_id]]<-NULL
        # A saved marker only changes fitness eligibility when this
        # player still needs it; already resolved vectors stay valid.
        if(player_id%in%entry$fitness_changed)entry$fitness_dirty<-TRUE
      }else {
        entry$save_errors<-unique(c(entry$save_errors,player_id))
        entry$save_retry[[player_id]]<-NULL;entry$save_retry[[player_id]]<-payload
        while(length(entry$save_retry)>25L||sum(vapply(entry$save_retry,function(x)as.numeric(object.size(x)),numeric(1)))>8*1024^2)
          entry$save_retry<-entry$save_retry[-1L]
      }
      touch();NULL
    }
    if(database_only){acknowledged(TRUE);return(invisible(NULL))}
    queued_ok<-tryCatch(persist_recent_points_snapshot(payload,payload$context,acknowledged),error=function(e)FALSE)
    if(!isTRUE(queued_ok)&&entry$saving>0L)acknowledged(FALSE)
    invisible(NULL)
  }
  load_entry <- function(client,key){
    entry<-new.env(parent=emptyenv());entry$key<-key
    entry$context<-round_points_client_context(client);entry$context$session_id<-root$token
    entry$season<-entry$context$season;entry$scoring<-entry$context$scoring_version
    entry$points<-data.frame();entry$checks<-data.frame();entry$catalog_ids<-character();entry$catalog_loaded<-FALSE
    entry$saved_ids_cache<-NULL;entry$coverage_cache<-NULL;entry$latest_round_cache<-NULL
    entry$live<-list();entry$attempted<-list();entry$pending<-list();entry$forced<-character();entry$retry_reads<-0L;entry$request_sequence<-0L
    entry$requires_context<-isTRUE(root$userData$league_context_enabled)
    entry$loading<-c(context=entry$requires_context,matrix=TRUE,lineup=TRUE,catalog=catalog_requested());entry$matrix_complete<-FALSE;entry$initial_errors<-list();entry$initial_pending<-list();entry$initial_sequence<-0L;entry$worker_reads<-list()
    entry$save_errors<-character();entry$save_retry<-list();entry$save_latest<-list();entry$saving<-0L;entry$overflow<-FALSE
    entry$finished_signature<-NULL;entry$published_ids<-character()
    entry$catalog_fitness<-data.frame();entry$fitness<-data.frame();entry$fitness_dirty<-TRUE
    entry$fitness_anchors<-list();entry$fitness_probe_attempted<-list();entry$fitness_probes<-list()
    entry$fitness_ready<-character();entry$fitness_changed<-character();entry$fitness_holding<-character();entry$fitness_unsaved<-data.frame();entry$fitness_failed<-data.frame()
    entries[[key]]<<-entry
    load_initial(entry,client,if(entry$requires_context)"context"else c("lineup","catalog"))
    entry
  }
  root$onSessionEnded(function(){closed<<-TRUE;generation<<-generation+1L;cancel_timer();cancel_retry();entries<<-list()})
  shiny::observe({
    all<-clients();active<-active_clients()
    accounts<-unique(vapply(Filter(function(x)valid_login(x$auth),all),function(x)as.character(x$auth[["userid"]]),character(1)))
    if(!length(accounts)){
      clear_pending(current_entry());entries<<-list();current_key<<-NULL
      generation<<-generation+1L;cancel_timer();cancel_retry();published(list(key=NULL,points=data.frame(),context=NULL));touch();return()
    }
    # Drop every prior account's private data on logout or account replacement.
    for(key in names(entries))if(!entries[[key]]$context$account_id%in%accounts)entries[[key]]<<-NULL
    next_key<-if(length(active))key_for(active[[1]])else NULL
    if(!identical(next_key,current_key)){
      clear_pending(current_entry());generation<<-generation+1L;cancel_timer();cancel_retry();current_key<<-next_key
      if(is.null(next_key)){touch();return()}
      if(is.null(entries[[next_key]]))load_entry(active[[1]],next_key)
      else {
        retained<-entries[[next_key]]
        if(length(retained$initial_errors))load_initial(retained,active[[1]],names(retained$initial_errors))
        publish(retained,TRUE)
      }
      # Enforce the shared row budget again whenever an async load completes.
      trim_entries()
    }
    entry<-current_entry()
    for(id in names(all)){
      client<-all[[id]];previous<-previous_refresh[[id]]
      if(!is.null(previous)&&!identical(previous,client$refresh))pending_refresh[[id]]<<-TRUE
      if(!is.null(entry)&&isTRUE(client$active)&&length(client$ids)&&
         identical(key_for(client),current_key)&&isTRUE(pending_refresh[[id]])){
        wanted<-head(client$ids,2000L)
        replaying<-intersect(wanted,names(entry$save_retry))
        for(player in replaying)save_round_snapshot(entry,player,entry$save_retry[[player]])
        wanted<-setdiff(wanted,replaying)
        entry$forced<-unique(c(entry$forced,wanted))
        if(nrow(entry$fitness_failed)){
          entry$fitness_unsaved<-recent_points_fitness_buffer(dplyr::bind_rows(entry$fitness_unsaved,entry$fitness_failed))
          entry$fitness_failed<-data.frame()
        }
        entry$finished_signature<-NULL;entry$retry_reads<-0L
        for(player in wanted){
          entry$pending[[player]]<-NULL
          entry$attempted[[player]]<-NULL
          ctx<-context_for(entry);ctx$player_id<-player;ctx$session_id<-""
          invalidate_resources(ctx,pattern="^player_summary_points$|^player_summary_")
        }
        if(length(entry$initial_errors))load_initial(entry,client,names(entry$initial_errors))
        pending_refresh[[id]]<<-FALSE
      }
      previous_refresh[[id]]<<-client$refresh
    }
    if(!is.null(entry)&&catalog_requested()&&!isTRUE(entry$catalog_loaded)&&!isTRUE(entry$loading[["catalog"]])&&is.null(entry$initial_errors$catalog))load_initial(entry,active[[1]],"catalog")
    if(!is.null(entry)&&!any(entry$loading)){
      update_fitness(entry);prepare_fitness(entry)
      if(!identical(entry$published_ids,visible_ids()))publish(entry,TRUE)
    }
    touch()
  },priority=100)
  shiny::observe({
    revision();clients()
    if(closed||!length(current_clients()))return()
    entry<-current_entry();if(is.null(entry)||!length(entry$worker_reads))return()
    state<-fm_worker_account_state(entry$context$account_id)
    initial<-character();changed<-FALSE
    for(id in names(entry$worker_reads)){
      read<-entry$worker_reads[[id]]
      version<-fm_worker_resource_revision(state,read$descriptor$resource_key)()
      if(identical(version,read$revision))next
      if(is.null(read$player)){
        if(isTRUE(entry$loading[[read$type]]))next
        saved<-fm_worker_get(entry$context$account_id,read$descriptor$resource_key)$data
        reusable<-is.null(entry$initial_errors[[read$type]])&&!is.null(saved)&&
          is.null(attr(saved,"fetch_error"))&&
          !isTRUE(attr(saved,"worker_completeness")%in%c("partial","unavailable"))
        unchanged<-reusable&&isTRUE(tryCatch(switch(read$type,
          lineup=identical(paste0(normalize_league_rules(list(),saved,entry$context$championship_id)$scoring_version),entry$scoring),
          context=identical(unclass(saved)[fm_worker_fields],unclass(context_for(entry))[fm_worker_fields]),
          catalog=identical(head(unique(as.character(saved$id)),2000L),entry$catalog_ids)&&
            identical(recent_points_fitness_rows(saved),entry$catalog_fitness),FALSE),error=function(e)FALSE))
        if(unchanged){entry$worker_reads[[id]]$revision<-version;next}
        initial<-c(initial,read$type)
      }else{
        player<-read$player
        if(!is.null(entry$pending[[player]]))next
        if(!player%in%ids_for(entry))next
        entry$attempted[[player]]<-NULL
        entry$forced<-unique(c(entry$forced,player))
        ctx<-context_for(entry);ctx$player_id<-player;ctx$session_id<-""
        invalidate_resources(ctx,pattern="^player_summary_points$|^player_summary_")
      }
      entry$worker_reads[[id]]$revision<-version;changed<-TRUE
    }
    if(length(initial))load_initial(entry,current_clients()[[1]],unique(initial))
    if(changed){entry$fitness_dirty<-TRUE;touch()}
  })
  shiny::observe({
    revision();clients()
    if(closed||!length(current_clients()))return()
    entry<-current_entry();if(is.null(entry)||any(entry$loading)||entry$overflow||!is.null(entry$initial_errors$lineup)||!is.null(entry$initial_errors$context))return()
    .persistence_state$revision()
    saving<-background_sync_status(entry$context$account_id)
    queued<-c(.persistence_state$queue,if(!is.null(.persistence_state$active))list(.persistence_state$active)else list())
    bytes<-sum(vapply(queued,function(job)as.numeric(job$bytes %||% 0),numeric(1)))
    schedule_retry(entry)
    if(saving$pending>=20L||length(queued)>=190L||bytes>=14*1024^2)return()
    prepare_fitness(entry);save_fitness(entry)
    needed<-needed_for(entry);slots<-max(0L,2L-length(entry$pending))
    if(!length(needed)||!slots)return()
    if(any(needed%in%visible_ids()))entry$finished_signature<-NULL
    for(player in head(needed,slots))local({
      player_id<-player;expected<-generation;ctx<-context_for(entry);ctx$player_id<-player_id
      auth<-current_clients()[[1]]$auth
      current<-function()isTRUE(shiny::isolate(!closed&&identical(generation,expected)&&
        identical(current_key,entry$key)&&length(current_clients())>0L))
      previous_attempt<-entry$attempted[[player_id]]
      requested_n<-max(c(0L,vapply(current_clients(),function(client)
        if(player_id%in%client$ids)as.integer(client$n)else 0L,integer(1))))
      requested_window<-sort(unique(unlist(lapply(current_clients(),function(client)if(player_id%in%client$ids)client$window else numeric()),use.names=FALSE)))
      new_window<-isTRUE(previous_attempt$ok)&&length(requested_window)&&!identical(previous_attempt$window,requested_window)
      # One short-to-wide upgrade is enough: the source request is N-independent.
      wider<-isTRUE(previous_attempt$ok)&&requested_n>5L&&
        as.integer(previous_attempt$required_rounds %||% 0L)<=5L
      attempt_number<-if(wider||new_window)1L else as.integer(previous_attempt$attempts %||% 0L)+1L
      if(attempt_number>1L&&entry$retry_reads>=retry_budget)return()
      if(attempt_number>1L)entry$retry_reads<-entry$retry_reads+1L
      entry$request_sequence<-entry$request_sequence+1L;request_token<-entry$request_sequence
      entry$pending[[player_id]]<-request_token;settled<-FALSE
      probe_snapshots<-names(Filter(function(id)identical(id,player_id),entry$fitness_probes))
      for(snapshot in probe_snapshots)entry$fitness_probe_attempted[[snapshot]]<-TRUE
      entry$fitness_probe_attempted<-tail(entry$fitness_probe_attempted,32L)
      if(length(probe_snapshots)||attempt_number>1L||wider||new_window){
        fresh_ctx<-ctx;fresh_ctx$session_id<-""
        invalidate_resources(fresh_ctx,pattern="^player_summary_points$|^player_summary_")
      }
      args<-list(login=auth,championship_id=ctx$championship_id,user_team_id=ctx$user_team_id,
        player_id=player_id,scoring_version=ctx$scoring_version)
      failed<-function(error,processing=FALSE){
        if((settled&&!processing)||!current()||(!processing&&!identical(entry$pending[[player_id]],request_token)))return(NULL)
        settled<<-TRUE;entry$pending[[player_id]]<-NULL
        failure<-resource_log_failure(error,"get_player_recent_points")
        transient<-failure$category%in%c("network","read_budget")||
          (identical(failure$category,"http")&&!is.null(failure$http_status)&&
           (failure$http_status%in%c(408L,429L)||failure$http_status>=500L))
        retry<-transient&&attempt_number<=length(retry_delays)&&entry$retry_reads<retry_budget
        entry$attempted[[player_id]]<-list(ok=FALSE,attempts=attempt_number,error=failure,terminal=!retry,
          retry_at=if(retry)as.numeric(Sys.time())+retry_delays[attempt_number]else NULL)
        entry$fitness_dirty<-TRUE
        processing_failed<-function(error){entry$attempted[[player_id]]<-list(ok=FALSE,attempts=attempt_number,
          error=resource_log_failure(error,"get_player_recent_points"),terminal=TRUE,retry_at=NULL);touch();NULL}
        tryCatch(prepare_fitness(entry),error=processing_failed)
        touch();tryCatch(publish(entry),error=processing_failed);NULL
      }
      finish<-function(result){
        if(settled||!current()||!identical(entry$pending[[player_id]],request_token))return(NULL)
        tryCatch({
        # A successful summary can have partial historical coverage. Its
        # labelled rows/check time are validated below; expiry is not a failed
        # acquisition and must not renew those original source timestamps.
        good<-!is.null(result$data)&&is.null(result$error)&&is.null(attr(result$data,"fetch_error"))&&
          isTRUE(result$status%in%c("ok","empty","stale","partial"))
        if(!good)return(failed(result$error %||% resource_failure(resource="get_player_recent_points",category="schema")))
        payload<-result$data
        valid<-is.list(payload)&&is.list(payload$context)&&is.data.frame(payload$observations)&&
          nrow(payload$observations)<=500L&&identical(payload$context$player_id,player_id)&&
          identical(payload$context$championship_id,ctx$championship_id)&&
          identical(payload$context$account_id,ctx$account_id)&&identical(payload$context$user_team_id,ctx$user_team_id)&&
          identical(fm_scalar(payload$context$competition_id,"unknown"),fm_scalar(ctx$competition_id,"unknown"))&&
          identical(payload$context$scoring_version,ctx$scoring_version)&&identical(payload$context$scoring_version,entry$scoring)&&
          length(payload$context$season)==1L&&!is.na(payload$context$season)&&nzchar(payload$context$season)&&
          (identical(entry$season,"unknown")||identical(payload$context$season,entry$season))&&
          length(fm_time(payload$checked_at))==1L&&!is.na(fm_time(payload$checked_at))
        if(isTRUE(valid)&&nrow(payload$observations)){
          observed<-payload$observations
          valid<-all(c("player_id","championship_id","season","scoring_version","round","points","score_status","observed_at")%in%names(observed))&&
            all(history_context_filter(observed,payload$context)$player_id==player_id)&&nrow(history_context_filter(observed,payload$context))==nrow(observed)&&
            all(observed$player_id==player_id&observed$championship_id==ctx$championship_id&
              observed$season==payload$context$season&observed$scoring_version==ctx$scoring_version)
        }
        if(!isTRUE(valid))return(failed(resource_failure(resource="get_player_recent_points",category="schema")))
        if(good){
          payload<-result$data;payload$context$session_id<-root$token
          season<-payload$context$season %||% "unknown"
          if(season!="unknown"&&!identical(season,entry$season)){
            entry$fitness_dirty<-TRUE
            # Earlier unknown-season successes cannot satisfy the discovered season.
            for(previous_player in names(entry$attempted)){
              attempt<-entry$attempted[[previous_player]]
              if(isTRUE(attempt$ok)&&!identical(attempt$season,season))entry$attempted[[previous_player]]<-NULL
            }
          }
          if(season!="unknown")entry$season<-season
          for(snapshot in probe_snapshots){
            evidence<-entry$fitness[entry$fitness$snapshot==snapshot&entry$fitness$player_id==player_id,,drop=FALSE]
            anchor<-recent_points_fitness_anchor(evidence,payload,context_for(entry))
            if(!is.null(anchor))entry$fitness_anchors[[snapshot]]<-anchor
          }
          entry$fitness_anchors<-tail(entry$fitness_anchors,32L)
          projected<-nrow(entry$points)+sum(vapply(entry$live,nrow,integer(1)))+nrow(payload$observations)
          if(projected>50000L){entry$overflow<-TRUE;return(failed(resource_failure(resource="get_player_recent_points",category="schema")))}
          else{
            merge_live(entry,payload$observations)
            trim_entries()
            save_round_snapshot(entry,player_id,payload)
          }
        }
        settled<<-TRUE;entry$pending[[player_id]]<-NULL
        entry$attempted[[player_id]]<-list(ok=TRUE,attempts=attempt_number,season=season,
          competition_id=fm_scalar(ctx$competition_id,"unknown"),scoring_version=ctx$scoring_version,required_rounds=requested_n,window=requested_window)
        if(length(probe_snapshots))entry$fitness_dirty<-TRUE
        prepare_fitness(entry)
        touch();publish(entry);NULL
        },error=function(error)failed(error,processing=settled))
      }
      worker_live<-track_worker_read(entry,"get_player_recent_points",args,ctx,"player",player_id)
      promise<-tryCatch(get_cached_resource_async("player_summary_points",ctx,function()
        async_adapter_fetch("get_player_recent_points",args,context=ctx,is_current=worker_live %||% current)),error=function(error)promises::promise_reject(error))
      promises::then(promise,finish,failed)
    })
    touch()
  })
  status<-shiny::reactive({
    revision();clients();entry<-current_entry()
    if(is.null(entry))return(list(total=0L,remaining=0L,attempted=0L,pending=0L,saving=0L,save_error=NULL,failed=0L,matrix_complete=TRUE))
    ids<-ids_for(entry);complete<-intersect(ids,completed_for(entry));failed<-intersect(ids,failed_for(entry,complete))
    retrying<-names(entry$attempted)[vapply(names(entry$attempted),function(player)
      retry_waiting(entry,player)||(!isTRUE(entry$attempted[[player]]$ok)&&player%in%names(entry$pending)),logical(1))]
    list(total=length(ids),completed=length(complete),
      remaining=length(setdiff(ids,c(complete,failed,names(entry$pending)))),attempted=length(entry$attempted),
      pending=length(entry$pending),retrying=length(setdiff(intersect(ids,retrying),complete)),
      saving=entry$saving+ceiling(nrow(entry$fitness_unsaved)/100L),
      save_error=if(length(entry$save_errors))"Round history could not be saved. Refresh to retry."else NULL,
      failed=length(failed),failure_details=lapply(entry$attempted[failed],function(x)x$error),initial_errors=entry$initial_errors,
      matrix_complete=entry$matrix_complete&&!entry$overflow)
  })
  list(clients=clients,dataset=published,status=status,entry=current_entry,
    sources=shiny::reactive({revision();entry<-current_entry();if(is.null(entry))list()else entry$attempted}),
    pending=shiny::reactive({revision();entry<-current_entry();if(is.null(entry))list()else entry$pending}))
}

recent_player_points_source <- function(session,login_token,championship_id,user_team_id,
  players_RV,is_active=function()TRUE,refresh_trigger=NULL,publish_interval=5,required_rounds=function()5L,
  catalog_scope=TRUE,required_window=NULL) {
  root<-session$rootScope()
  if(is.null(root$userData$shared_round_points))
    root$userData$shared_round_points<-shiny::withReactiveDomain(root,new_shared_round_points_controller(root,publish_interval))
  shared<-root$userData$shared_round_points
  root$userData$round_points_registration<-(root$userData$round_points_registration %||% 0L)+1L
  registration<-paste0(session$ns(""),":",root$userData$round_points_registration)
  value<-function(x)if(is.function(x))x()else x
  owner<-shiny::reactive({
    auth<-value(login_token)
    if(!valid_login(auth))return(list(auth=auth,league="",team="",active=FALSE,ids=character(),fitness=data.frame(),n=5L,refresh=0L))
    if(exists("league_context_revision",mode="function"))league_context_revision()
    active<-isTRUE(is_active())
    # Hidden table clients must not demand a source read or restart acquisition.
    rows<-if(active)tryCatch(players_RV(),error=function(e)NULL)else NULL
    ids<-if(is.data.frame(rows)&&"id"%in%names(rows))as.character(rows$id)else character()
    league<-tryCatch(fm_scalar(value(championship_id),""),error=function(e)"")
    team<-tryCatch(fm_scalar(value(user_team_id),""),error=function(e)"")
    list(auth=auth,league=league,team=team,context=resource_context(auth,league,team),
      active=active,ids=head(unique(ids[!is.na(ids)&nzchar(ids)]),2000L),
      fitness=recent_points_fitness_rows(rows),n=max(1L,min(20L,fm_number(value(required_rounds),5L))),
      catalog_scope=isTRUE(catalog_scope),window=if(is.function(required_window))sort(unique(as.numeric(required_window())))else numeric(),
      refresh=if(is.function(refresh_trigger))refresh_trigger()else 0L)
  })
  shiny::observe({
    client<-owner();all<-shiny::isolate(shared$clients())
    if(!identical(all[[registration]],client)){all[[registration]]<-client;shared$clients(all)}
  },priority=110)
  history<-shiny::reactiveVal(data.frame())
  shiny::observe({
    client<-owner();snapshot<-shared$dataset()
    key<-if(valid_login(client$auth))round_points_client_key(client)else NULL
    rows<-if(identical(key,snapshot$key))snapshot$points else data.frame()
    latest<-attr(rows,"latest_round")
    if(nrow(rows))rows<-rows[rows$player_id%in%client$ids,,drop=FALSE]
    if(!is.null(latest))attr(rows,"latest_round")<-latest
    if(!identical(shiny::isolate(history()),rows))history(rows)
  },priority=-10)
  list(history=history,status=shared$status,states=shiny::reactive(list()),sources=shared$sources,pending=shared$pending)
}


# Shared display contract: preserve signed balances and distinguish missing data.
ui_financial_amount <- function(value) {
  amount <- suppressWarnings(as.numeric(value))
  if (length(amount) != 1L || !is.finite(amount)) return("Unavailable")
  format_table_currency(amount)
}
