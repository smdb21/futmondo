library(reactable)
players_in_teams_UI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("team_value_box")),
    actionButton(ns("btn_put_all_on_market"), "Put All Players on Market", icon=icon("tags")),
    players_table_UI(ns("players_table_in_teams"), box_title="Players in Team",
      filter_by_team=FALSE, filter_by_is_favorite=FALSE, filter_by_is_from_futmondo=FALSE,
      show_position_breakdown=TRUE, hide_bid_column=FALSE))
}
players_in_teams_Server <- function(id, is_module_active, login_token, championship_id, user_team_id, user_teams_RV, refresh_trigger=NULL) {
  moduleServer(id, function(input, output, session) {
    bind_factual_reads(c("get_players_from_team","get_my_market_players"),session,login_token,championship_id,user_team_id,
      is_active=is_module_active,refresh_trigger=refresh_trigger)

    ns <- session$ns
    get_reactive_val <- function(x) if (is.function(x)) x() else x
    current_context <- reactive(list(user=fm_scalar(login_token()[["userid"]]), championship=fm_scalar(championship_id()), team=fm_scalar(user_team_id())))
    bulk_context <- reactiveVal(NULL)
    observeEvent(current_context(), { bulk_context(NULL) }, ignoreNULL=FALSE, priority=100)
    players_table_RV <- reactive({
      req(is_module_active() == TRUE)
      req(login_token())
      req(championship_id())
      req(user_team_id())
      if (!is.null(refresh_trigger)) refresh_trigger() # Cache invalidation dependency
      championship_id <- championship_id()
      user_team_id <- user_team_id()
      players_table <- get_players_from_team(
        login = login_token(),
        championship_id = championship_id,
        user_team_id = user_team_id,
        teams = NULL
      )
      players_table <- preserve_observation_time(players_table)
      players_table <- players_table %>%
        translate_player_positions()
      players_table <- players_table %>%
        calculate_player_changes()
      players_table <- players_table %>%
        unify_columns()

      # Ensure market_inMarket column is always present
      if (!"market_inMarket" %in% colnames(players_table)) {
        players_table$market_inMarket <- FALSE
      }
      # Reconcile only against the authenticated manager's own listings.
      my_mkt_df <- tryCatch({
        get_my_market_players(
          login = login_token(),
          championship_id = championship_id,
          user_team_id = user_team_id
        )
      }, error = function(e) {
        print(paste0("[Market check] Fetch warning: ", e$message))
        NULL
      })
      if (!is.null(my_mkt_df) && "id" %in% colnames(my_mkt_df)) {
        mkt_ids <- as.character(my_mkt_df$id)
        players_table$market_inMarket <- as.character(players_table$id) %in% mkt_ids
      }

      players_table <- preserve_observation_time(players_table)

      return(players_table)
    })

    output$team_value_box <- renderUI({
      req(is_module_active());roster <- players_table_RV()
      values <- suppressWarnings(as.numeric(roster$value))
      total <- if (length(values) && all(is.finite(values))) player_card_money(sum(values)) else "Unavailable"
      shinydashboard::valueBox(total, paste(nrow(roster), "players in your squad"), icon=icon("users"), color="green", width=12)
    })
    # ---- Put All Players on Market Modal ----
    observeEvent(input$btn_put_all_on_market, {
      bulk_context(current_context())
      showModal(modalDialog(
        title = tagList(icon("tags"), " Put All Players on Market"),
        p("Choose how to price every player. Listings are sent one by one and only verified successes are refreshed."),
        radioButtons(ns("bulk_listing_price_mode"), "Listing price", choices = c(
          "Current market value" = "value",
          "Closed clause plus premium" = "closed_clause"
        ), selected = "value"),
        conditionalPanel(
          condition = sprintf("input['%s'] === 'closed_clause'", ns("bulk_listing_price_mode")),
          numericInput(ns("bulk_listing_clause_premium"), "Premium above closed clause (%)", value = 5, min = 0, max = 1000, step = 1)
        ),
        p(style = "color: var(--fm-muted); font-size: 12px;", "The clause option skips players without a valid closed clause. Other users and Futmondo can bid on listed players."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("submit_put_all_on_market"), "Confirm Market Listing", class = "btn btn-offer-money")
        ),
        easyClose = TRUE,
        size = "s"
      ))
    })

    # ---- Submit Put All Players on Market ----
    observeEvent(input$submit_put_all_on_market, {
      expected <- isolate(bulk_context()); bulk_context(NULL)
      if (is.null(expected) || !identical(expected, current_context())) {
        showNotification("Team context changed. Open the listing confirmation again.", type="warning")
        return()
      }
      login <- get_reactive_val(login_token)
      champ_id <- get_reactive_val(championship_id)
      team_id <- get_reactive_val(user_team_id)
      req(login, champ_id, team_id)

      mode <- if (identical(as.character(input$bulk_listing_price_mode), "closed_clause")) "closed_clause" else "value"
      premium <- suppressWarnings(as.numeric(input$bulk_listing_clause_premium))
      if (!is.finite(premium)) premium <- 5
      roster <- tryCatch(players_table_RV(), error = function(e) NULL)
      plan <- bulk_market_listing_plan(roster, mode = mode, clause_premium_pct = premium)
      if (!nrow(plan$entries)) {
        shiny::showNotification(plan$message, type = "warning", duration = 6)
        return()
      }

      outcomes <- lapply(seq_len(nrow(plan$entries)), function(i) {
        player <- plan$entries[i, , drop = FALSE]
        tryCatch({
          if (isTRUE(player$already_listed)) {
            update_player_market_listing(login, champ_id, team_id, player$player_id, player$price)
          } else {
            put_player_on_market(login, champ_id, team_id, player$player_id, player$price)
          }
        }, error = function(e) list(success = FALSE, code = "error", message = conditionMessage(e)))
      })
      succeeded <- vapply(outcomes, function(x) if (is.list(x)) isTRUE(x$success) else isTRUE(x), logical(1))
      success_count <- sum(succeeded)
      failure_count <- length(succeeded) - success_count
      removeModal()

      if (success_count > 0L) {
        invalidate_manual_resources(login,champ_id,team_id)
        if (!is.null(refresh_trigger) && is.function(refresh_trigger)) {
          tryCatch(refresh_trigger(refresh_trigger() + 1), error = function(e) NULL)
        }
      }
      detail <- if (plan$skipped > 0L) paste0(" ", plan$skipped, " player(s) skipped because no eligible price was available.") else ""
      if (failure_count == 0L) {
        shiny::showNotification(paste0(success_count, " player(s) listed successfully.", detail), type = "message", duration = 6)
      } else {
        shiny::showNotification(paste0(success_count, " player(s) listed; ", failure_count, " failed.", detail, " Refresh to review the failed listings."), type = "warning", duration = 8)
      }
    })


    selected <- players_table_Server("players_table_in_teams", players_table_RV, user_teams_RV,
      login_token, championship_id, user_team_id, hide_bid_column=FALSE, refresh_trigger=refresh_trigger,
      is_module_active=is_module_active)
    return(selected)
  })
}
bulk_market_listing_plan <- function(roster, mode = c("value", "closed_clause"), clause_premium_pct = 5, now = Sys.time()) {
  mode <- match.arg(mode)
  empty <- data.frame(player_id = character(), price = numeric(), already_listed = logical(), stringsAsFactors = FALSE)
  if (!is.data.frame(roster) || !nrow(roster) || !"id" %in% names(roster))
    return(list(entries = empty, skipped = 0L, message = "Your squad is unavailable. Refresh before listing players."))
  ids <- trimws(as.character(roster$id))
  listed <- if ("market_inMarket" %in% names(roster)) as.logical(roster$market_inMarket) else rep(FALSE, nrow(roster))
  listed[is.na(listed)] <- FALSE
  premium <- suppressWarnings(as.numeric(clause_premium_pct)[1])
  if (!is.finite(premium) || premium < 0) premium <- 5
  if (mode == "value") {
    prices <- if ("value" %in% names(roster)) suppressWarnings(as.numeric(as.character(roster$value))) else rep(NA_real_, nrow(roster))
    message <- "No players have a valid market value to list."
  } else {
    clauses <- if ("clause_price" %in% names(roster)) suppressWarnings(as.numeric(as.character(roster$clause_price))) else rep(NA_real_, nrow(roster))
    transferred <- if ("clause_transferred" %in% names(roster)) as.logical(roster$clause_transferred) else rep(NA, nrow(roster))
    dates <- if ("clause_date" %in% names(roster)) as.character(roster$clause_date) else rep(NA_character_, nrow(roster))
    parsed <- suppressWarnings(as.POSIXct(gsub("Z$", "", gsub("T", " ", dates)), tz = "UTC"))
    closed <- is.finite(clauses) & clauses > 0 & !is.na(transferred) & !transferred & !is.na(parsed) & parsed > now
    prices <- ifelse(closed, ceiling(clauses * (1 + premium / 100)), NA_real_)
    message <- "No players have a valid closed clause to use for this listing method."
  }
  keep <- !is.na(ids) & nzchar(ids) & is.finite(prices) & prices > 0
  entries <- data.frame(player_id = ids[keep], price = prices[keep], already_listed = listed[keep], stringsAsFactors = FALSE)
  list(entries = entries, skipped = as.integer(sum(!keep)), message = message)
}

get_ordinal_position <- function(position) {
  position <- case_when(
    position == 1 ~ paste0(position, "st"),
    position == 2 ~ paste0(position, "nd"),
    position == 3 ~ paste0(position, "rd"),
    TRUE ~ paste0(position, "th")
  )
  return(position)
}
