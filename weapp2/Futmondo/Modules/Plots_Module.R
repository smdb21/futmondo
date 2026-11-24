# Mobile-first, league-wide chart page. Player-card charts intentionally stay local.
plots_owned_by_real_team <- function(players) {
  empty <- data.frame(real_team_id=character(), real_team=character(), owned_players=integer(), stringsAsFactors=FALSE)
  if (!is.data.frame(players) || !nrow(players)) return(empty)
  pick <- function(keys) { key <- keys[keys %in% names(players)][1]; if (is.na(key)) rep("", nrow(players)) else { x <- trimws(as.character(players[[key]])); x[is.na(x)] <- ""; x } }
  player_id <- pick(c("id", "player_id", "_id"))
  owner_id <- pick(c("owner_team_id", "user_team_id", "userteamId", "userTeamId", "userteamId"))
  real_id <- pick(c("teamId", "team_id", "real_team_id"))
  real_name <- pick(c("team", "real_team", "teamName"))
  keep <- nzchar(player_id) & nzchar(owner_id) & nzchar(real_id) & nzchar(real_name)
  if (!any(keep)) return(empty)
  d <- data.frame(player_id=player_id[keep], real_team_id=real_id[keep], real_team=real_name[keep], stringsAsFactors=FALSE)
  d <- d[!duplicated(d$player_id),,drop=FALSE]
  out <- stats::aggregate(player_id ~ real_team_id + real_team, d, function(x) length(unique(x)))
  names(out)[names(out) == "player_id"] <- "owned_players"
  out[order(-out$owned_players, out$real_team, out$real_team_id),,drop=FALSE]
}

plots_empty <- function(message, error=TRUE) {
  plot <- plotly::plot_ly() %>% fm_plot_layout(xaxis=list(visible=FALSE), yaxis=list(visible=FALSE))
  attr(plot, "fm_info") <- fm_info(message, error=error)
  plot
}

# 12 distinct, colour-blind-aware hues (Okabe-Ito + Tableau extras) for team lines;
# names are sorted so each team keeps its colour across plots.
plots_team_colors <- function(teams) {
  base <- c("#0072B2","#E69F00","#009E73","#CC79A7","#D55E00","#56B4E9",
            "#8C564B","#F0E442","#7F3C8D","#17BECF","#BCBD22","#E15759")
  teams <- sort(unique(as.character(teams)))
  cols <- if (length(teams) > length(base)) grDevices::hcl.colors(length(teams), "Dynamic") else base[seq_along(teams)]
  stats::setNames(cols, teams)
}

plots_history <- function(history, metric, title) {
  if (isTRUE(attr(history, "context_unknown"))) return(plots_empty(paste0(title, " unavailable: the league season could not be resolved"), error=TRUE))
  if (!is.data.frame(history) || !nrow(history) || !metric %in% names(history)) return(plots_empty(paste0(title, " unavailable")))
  partial <- identical(attr(history, "fetch_status"), "partial")
  team <- if ("teamname" %in% names(history)) as.character(history$teamname) else "Team"
  dates <- fm_time(history$recorded_at)
  keep <- is.finite(as.numeric(dates)) & is.finite(suppressWarnings(as.numeric(history[[metric]])))
  if (!any(keep)) return(plots_empty(paste0(title, " unavailable")))
  d <- data.frame(team=team[keep], date=fm_plot_datetime(dates[keep]), display_date=format(dates[keep], "%b-%d", tz=fm_display_timezone()), value=as.numeric(history[[metric]][keep]))
  plotly::plot_ly(d,x=~date,y=~value,color=~team,colors=plots_team_colors(d$team),type="scatter",mode="lines+markers",
    hovertemplate=~paste0("<b>",team,"</b><br>",display_date,"<br>",format(value,big.mark=".",decimal.mark=",",scientific=FALSE),"<extra></extra>")) %>%
    fm_plot_layout(xaxis=list(title="",type="date",tickformat="%b-%d",hoverformat="%b-%d"),yaxis=list(title=title),showlegend=FALSE,margin=list(l=55,r=15,t=45,b=45)) -> plot
  if (partial) attr(plot, "fm_info") <- fm_info("Showing partial history: the saved record limit was reached before all rows were read.", error=TRUE)
  plot
}

# Pressroom history already merges clause payments with their eventual delivery.
plots_transfer_days <- function(history, timezone = fm_display_timezone()) {
  if (!is.data.frame(history) || !all(c("created", "price") %in% names(history))) return(NULL)
  # Reuse the vectorized ISO timestamp parser with its legacy-format fallback.
  dates <- as.Date(player_valuation_time(history$created), tz = timezone)
  amounts <- suppressWarnings(as.numeric(as.character(history$price)))
  keep <- is.finite(as.numeric(dates)) & is.finite(amounts)
  rows <- data.frame(date = dates[keep], amount = amounts[keep])
  attr(rows, "incomplete") <- any(!keep)
  rows
}

plots_transfer_periods <- function(rows, period) {
  start <- switch(period,
    day = rows$date,
    week = rows$date - (as.integer(rows$date) + 3L) %% 7L,
    month = as.Date(format(rows$date, "%Y-%m-01"))
  )
  bars <- rows %>%
    dplyr::group_by(start = start) %>%
    dplyr::summarise(amount = sum(amount), transfers = dplyr::n(), .groups = "drop")
  bars$end <- switch(period,
    day = bars$start + 1L,
    week = bars$start + 7L,
    month = as.Date(format(bars$start + 32L, "%Y-%m-01"))
  )
  bars
}

plots_transfer_chart <- function(bars) {
  # Civil-date coordinates keep the same buckets in browsers in other timezones.
  days <- as.numeric(bars$end - bars$start)
  center <- as.POSIXct(bars$start, tz = "UTC") + days * 43200
  label <- format(bars$start, "%d %b %Y")
  multiple_days <- days > 1
  label[multiple_days] <- paste(label[multiple_days], "–", format(bars$end[multiple_days] - 1L, "%d %b %Y"))
  amounts <- format(bars$amount, big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
  plotly::plot_ly(bars) %>%
    plotly::add_bars(x = format(center, "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
      y = ~amount, width = days * 86400000 * 0.9, marker = list(color = "#608BC1"),
      hovertemplate = paste0("<b>", label, "</b><br>", amounts, " EUR<br>",
        bars$transfers, ifelse(bars$transfers == 1L, " transfer", " transfers"), "<extra></extra>")) %>%
    fm_plot_layout(xaxis = list(type = "date", nticks = 6),
      yaxis = list(title = "Transfer value (EUR)", tickmode = "auto", nticks = 6),
      margin = list(l = 55, r = 15, t = 45, b = 45))
}

# One cumulative result per observed round; missing coverage stays unknown.
# Decimal source round numbers are retained as separate, evenly spaced labels.
plots_round_totals <- function(rows) {
  empty <- data.frame(team_id=character(), team_name=character(), round=numeric(), points=numeric(), rank=numeric())
  if (!is.data.frame(rows) || !nrow(rows)) return(empty)
  teams <- unique(rows[c("team_id", "team_name")])
  catalog_numbers <- attr(rows, "round_numbers")
  rows <- rows[is.finite(rows$round), , drop=FALSE]
  rows <- rows[!duplicated(paste(rows$team_id, rows$round), fromLast=TRUE), , drop=FALSE]
  numbers <- sort(unique(rows$round))
  if (!length(numbers)) return(empty)
  numbers <- sort(unique(c(numbers, catalog_numbers[is.finite(catalog_numbers) & catalog_numbers <= max(numbers)])))
  totals <- dplyr::bind_rows(lapply(seq_len(nrow(teams)), function(i) {
    team <- rows[rows$team_id == teams$team_id[i], , drop=FALSE]
    data.frame(team_id=teams$team_id[i], team_name=teams$team_name[i], round=numbers,
      points=cumsum(team$points[match(numbers, team$round)]))
  }))
  totals$rank <- NA_real_
  for (number in numbers) {
    ix <- which(totals$round == number)
    # A league rank is unknown until every team has comparable coverage.
    if(all(is.finite(totals$points[ix]))) totals$rank[ix] <- rank(-totals$points[ix], ties.method="min")
  }
  totals
}

plots_round_chart <- function(totals, metric="points") {
  if (!nrow(totals)) return(plots_empty("No published round history", error=FALSE))
  labels <- as.character(sort(unique(totals$round)))
  totals$round_label <- as.character(totals$round)
  title <- if (metric == "rank") "Cumulative rank" else "Cumulative points"
  yaxis <- list(title=title, autorange=if(metric == "rank") "reversed" else TRUE)
  if(metric == "rank") {
    yaxis$dtick <- 1
  } else {
    # Omit dtick entirely: a NULL list entry serializes as [] and Plotly then
    # draws a tick/gridline per point, overwhelming the axis at league totals.
    yaxis$tickmode <- "auto"
    yaxis$nticks <- 6
  }
  plot <- plotly::plot_ly(totals, x=~round_label, y=totals[[metric]], color=~team_name,
    colors=plots_team_colors(totals$team_name), type="scatter", mode="lines+markers", connectgaps=FALSE,
    hovertemplate=~paste0("<b>",team_name,"</b><br>Round ",round_label,"<br>",
      title,": ",totals[[metric]],"<extra></extra>")) %>%
    fm_plot_layout(xaxis=list(title="Round", type="category", categoryorder="array", categoryarray=labels),
      yaxis=yaxis, showlegend=FALSE,
      margin=list(l=55,r=15,t=45,b=55))
  if (any(!is.finite(totals[[metric]]))) attr(plot, "fm_info") <- fm_info(
    "Some saved round scores are missing. Lines have gaps where a complete cumulative result cannot be calculated; ranks require scores for every team.", error=TRUE)
  plot
}

plots_owned_chart <- function(players) {
  d <- plots_owned_by_real_team(players)
  if (!nrow(d)) return(plots_empty("Owned-player and real-club data unavailable"))
  clubs <- players_table_clubs(players)$choices
  d$crest <- clubs$crest[match(paste0("club:", d$real_team_id), clubs$value)]
  d <- d[order(d$owned_players, d$real_team), , drop=FALSE]
  d$bar <- seq_len(nrow(d))
  images <- lapply(which(!is.na(d$crest) & nzchar(d$crest)), function(i) list(
    source=d$crest[i], xref="paper", yref="y", x=-0.015, y=d$bar[i],
    sizex=0.065, sizey=0.7, xanchor="right", yanchor="middle", sizing="contain"))
  plotly::plot_ly(d, x=~owned_players, y=~bar, type="bar", orientation="h",
    marker=list(color="#608BC1"), text=~owned_players, textposition="outside", cliponaxis=FALSE,
    hovertemplate=~paste0("<b>",real_team,"</b><br>",owned_players," owned players<extra></extra>")) %>%
    fm_plot_layout(xaxis=list(title="Players owned", dtick=1, range=c(0,max(d$owned_players)*1.16+1)),
      yaxis=list(title="", tickvals=d$bar, ticktext=paste0(d$real_team,"       "),
        range=c(0.3,nrow(d)+0.7), zeroline=FALSE), images=images,
      margin=list(l=180,r=35,t=60,b=50)) %>%
    htmlwidgets::onRender("function(el) {
      // Keep source crests readable when the mobile plotting area is narrow.
      if (el.fmSizeClubCrests) el.removeListener('plotly_afterplot', el.fmSizeClubCrests);
      el.fmSizeClubCrests = function() {
        if (!el._fullLayout || !el._fullLayout._size) return;
        var width = el._fullLayout._size.w;
        if (!(width > 0)) return;
        var updates = {};
        (el.layout.images || []).forEach(function(img, i) {
          if (Math.abs(img.sizex - 22 / width) > 0.000001) updates['images[' + i + '].sizex'] = 22 / width;
          if (Math.abs(img.x + 8 / width) > 0.000001) updates['images[' + i + '].x'] = -8 / width;
        });
        if (Object.keys(updates).length) Plotly.relayout(el, updates);
      };
      el.on('plotly_afterplot', el.fmSizeClubCrests);
      el.fmSizeClubCrests();
    }")
}

plots_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shiny::h2("Plots"), shiny::uiOutput(ns("chart_status")),
    shiny::tabsetPanel(id=ns("plot_tab"), type="pills",
      shiny::tabPanel("Standings", shiny::p("Cumulative fantasy-team points after each published round or subround. Repeated daily snapshots are excluded; missing scores remain gaps."), plotly::plotlyOutput(ns("standings_plot"), height="420px")),
      shiny::tabPanel("Squad values", shiny::p("Saved squad market valuations over time, in euros."), plotly::plotlyOutput(ns("values_plot"), height="420px")),
      shiny::tabPanel("Cash & transfers", shiny::p("Compare transfer-only balance estimates, squad purchase spending or transaction volume for the selected dates. Balance estimates exclude round rewards and other cash adjustments."), shiny::fluidRow(shiny::column(4, shiny::dateRangeInput(ns("cash_dates"),"Date range",format="dd/mm/yyyy")), shiny::column(8, shiny::radioButtons(ns("cash_metric"),"Metric",choices=c("Transfer-only balance estimate"="cash","Squad Purchases"="investment","Transaction Volume"="volume"),inline=FALSE))), plotly::plotlyOutput(ns("cash_plot"), height="420px")),
      shiny::tabPanel("Money movements",
        shiny::p("Total value of league transfers over time. Each player transfer counts once, including market and clause transfers."),
        shiny::radioButtons(ns("transfer_period"), "Group by",
          choices = c("Day" = "day", "Week" = "week", "Month" = "month"), selected = "week", inline = TRUE),
        shiny::uiOutput(ns("transfers_status")),
        plotly::plotlyOutput(ns("transfers_plot"), height = "420px")),
      shiny::tabPanel("Rank progression", shiny::p("Each fantasy team's rank from cumulative published round points; first place is at the top. The range selects which rounds are displayed."), shiny::sliderInput(ns("rank_range"),"Rounds",min=1,max=38,value=c(1,38),step=0.1), plotly::plotlyOutput(ns("rank_plot"), height="420px")),
      shiny::tabPanel("Owned by real team", shiny::p("Currently fantasy-owned players, grouped by their real football club."), plotly::plotlyOutput(ns("owned_club_plot"), height="520px"))
    ))
}

plots_Server <- function(id,is_module_active,login_token,championship_id,user_team_id,user_teams_RV,refresh_trigger=NULL) {
  shiny::moduleServer(id,function(input,output,session) {
    bind_factual_reads(c("get_league_standings_history","get_financial_snapshot","get_championship_pressroom","get_user_team_rounds","get_classification_round_catalog","get_championship_players"),session,login_token,championship_id,user_team_id,is_active=is_module_active,refresh_trigger=refresh_trigger)

    active <- function() isTRUE(is_module_active())
    history <- shiny::reactive({ shiny::req(active(), championship_id()); if(is.function(refresh_trigger)) refresh_trigger(); tryCatch(get_league_standings_history(championship_id()),error=function(e)data.frame()) })
    standings_plot <- shiny::reactive({ plots_round_chart(round_totals(), "points") })
    values_plot <- shiny::reactive({ plots_history(history(),"team_value","Squad valuation (EUR)") })
    shiny::observe({ h<-history(); if(is.data.frame(h)&&nrow(h)&&"recorded_at"%in%names(h)) { d<-as.Date(fm_time(h$recorded_at),tz=fm_display_timezone()); d<-d[!is.na(d)]; if(length(d)) shiny::updateDateRangeInput(session,"cash_dates",start=min(d),end=max(d),min=min(d),max=max(d)) } })
    transfer_history <- shiny::reactive({
      shiny::req(active(), login_token(), championship_id(), user_team_id())
      classification_read_state(function() {
        get_championship_pressroom(login = login_token(), championship_id = championship_id())
      })
    })
    read_transfer_baseline <- factual_read_adapter(session, "read_persisted_history_baseline",
      login_token, championship_id, user_team_id,
      is_active = function() active() && identical(input$plot_tab, "Money movements"),
      refresh_trigger = refresh_trigger, state_only = TRUE)
    transfer_chart_history <- shiny::reactive({
      state <- transfer_history()
      history <- state$data
      if (state$loading || (is.data.frame(history) && all(c("created", "price") %in% names(history)) &&
          !"unavailable" %in% c(attr(history, "fetch_status"), attr(history, "worker_completeness")))) {
        return(state)
      }
      # A worker-read failure need not make a previously audited transfer history unavailable.
      league <- championship_id()
      args <- list(championship_id = league)
      scope <- resource_context(login_token(), league, user_team_id())
      scope <- fm_worker_resolve_context("get_championship_pressroom", args, scope)
      fallback <- read_transfer_baseline(resource = paste0("pressroom_", league), context = scope)
      if (identical(fallback$status, "loading"))
        return(list(data = NULL, loading = TRUE, error = NULL))
      if (is.null(fallback$data)) return(state)
      list(data = observed_data(fallback$data, status = "stale",
        observed_at = attr(fallback$data, "observed_at")), loading = FALSE, error = NULL)
    })
    transfer_days <- shiny::reactive(plots_transfer_days(transfer_chart_history()$data))
    output$transfers_status <- shiny::renderUI({
      shiny::req(input$plot_tab == "Money movements")
      state <- transfer_chart_history()
      if (state$loading) return(shiny::div(class = "fm-plot-loading", role = "status", "Loading saved transfer history."))
      if (!is.null(state$error)) return(fm_info(resource_failure_message(state$error), error = TRUE, inline = TRUE))
      history <- state$data
      status <- attr(history, "fetch_status")
      completeness <- attr(history, "worker_completeness")
      rows <- transfer_days()
      if (is.null(rows) || "unavailable" %in% c(status, completeness))
        return(fm_info("Saved transfer history is unavailable.", error = TRUE, inline = TRUE))
      incomplete <- "partial" %in% c(status, completeness) || isTRUE(attr(rows, "incomplete"))
      shiny::tagList(
        if ("stale" %in% status) fm_info("Showing the last saved transfer history.", error = TRUE, inline = TRUE),
        if (incomplete)
          fm_info("Saved transfer history is incomplete. Totals use the available dates and amounts.", error = TRUE, inline = TRUE),
        if (!nrow(rows) && !incomplete && !"stale" %in% status)
          fm_info("No transfers recorded for this league.", inline = TRUE)
      )
    })
    output$transfers_plot <- plotly::renderPlotly({
      shiny::req(input$plot_tab == "Money movements", input$transfer_period %in% c("day", "week", "month"))
      rows <- transfer_days()
      history <- transfer_chart_history()$data
      shiny::req(!is.null(rows), nrow(rows) > 0L,
        !"unavailable" %in% c(attr(history, "fetch_status"), attr(history, "worker_completeness")))
      plots_transfer_chart(plots_transfer_periods(rows, input$transfer_period))
    })
    cash_plot <- shiny::reactive({
      shiny::req(active(),login_token(),championship_id(),user_team_id(),user_teams_RV()); if(is.function(refresh_trigger)) refresh_trigger()
      teams<-user_teams_RV(); snap<-tryCatch(get_financial_snapshot(login_token(),championship_id(),user_team_id()),error=function(e)NULL)
      initial<-normalize_league_rules(list(configuration=if(is.list(snap))snap$configuration else list()))$initial_budget
      press<-transfer_history()$data
      if(!is.data.frame(press)||!nrow(press)) return(plots_empty("Observed transfer data unavailable"))
      dates<-input$cash_dates; if(is.null(dates)||length(dates)!=2) return(plots_empty("Choose a date range", error=FALSE))
      d<-rivals_buying_power_values(press,teams,metric=input$cash_metric %||% "cash",start_date=fm_local_midnight(dates[1]),end_date=fm_local_midnight(dates[2]+1)-0.000001,initial_budget=initial)
      if(!is.data.frame(d)||!nrow(d)) return(plots_empty("League financial data unavailable"))
      d<-d[order(d$value),,drop=FALSE]; plotly::plot_ly(d,x=~value,y=~reorder(team,value),type="bar",orientation="h",marker=list(color="#608BC1"),hovertemplate=~paste0("<b>",team,"</b><br>",format(value,big.mark=".",decimal.mark=",",scientific=FALSE)," EUR<extra></extra>")) %>% fm_plot_layout(xaxis=list(title="EUR"),yaxis=list(title=""),margin=list(l=115,r=15,t=45,b=45))
    })
    # Catch each pending read independently so one team cannot prevent the
    # other saved histories from being requested or displayed.
    catalog_state <- shiny::reactive({
      shiny::req(active(),login_token(),championship_id())
      if(is.function(refresh_trigger)) refresh_trigger()
      classification_read_state(function() get_classification_round_catalog(login_token(),championship_id()))
    })
    round_state <- shiny::reactive({
      shiny::req(active(),login_token(),championship_id())
      if(is.function(refresh_trigger)) refresh_trigger()
      directory <- classification_read_state(user_teams_RV)
      catalog <- catalog_state()
      teams <- directory$data
      rounds <- catalog$data$rounds %||% data.frame()
      reads <- list(directory,catalog)
      frames <- list(classification_empty_rows())
      if(is.data.frame(teams) && nrow(teams)) {
        id_col <- c("teamid","id")[c("teamid","id") %in% names(teams)][1]
        name_col <- c("teamname","name")[c("teamname","name") %in% names(teams)][1]
        if(!is.na(id_col) && !is.na(name_col)) for(i in seq_len(nrow(teams))) {
          team_id <- as.character(teams[[id_col]][i])
          team_name <- as.character(teams[[name_col]][i])
          result <- classification_read_state(function() get_user_team_rounds(
            login_token(),championship_id(),team_id))
          d <- classification_round_rows(result$data,team_id,team_name,rounds)
          if(!nrow(d)) d <- data.frame(team_id=team_id,team_name=team_name,round=NA_real_,points=NA_real_)
          reads[[length(reads)+1L]] <- result
          frames[[length(frames)+1L]] <- d
        }
      }
      rows <- dplyr::bind_rows(frames)
      attr(rows,"round_numbers") <- rounds$round_number
      list(data=rows,loading=any(vapply(reads,function(x)isTRUE(x$loading),logical(1))),
        errors=Filter(Negate(is.null),lapply(reads,`[[`,"error")))
    })
    rank_rows <- shiny::reactive(round_state()$data)
    # Metadata-only worker publications must not redraw the same chart.
    round_totals <- resource_display_source(session,function() plots_round_totals(rank_rows()))
    round_domain <- resource_display_source(session,function() sort(unique(round_totals()$round)))
    shiny::observe({
      numbers <- round_domain()
      if(length(numbers)) shiny::updateSliderInput(session,"rank_range",min=min(numbers),max=max(numbers),
        value=range(numbers),step=0.1)
    })
    round_status <- shiny::reactive({
      state <- round_state()
      totals <- round_totals()
      shiny::tagList(
        if(state$loading) shiny::div(class="fm-plot-loading",role="status",
          "Loading saved round scores. Verified history appears as it becomes available."),
        if(length(state$errors)) fm_info(paste("Some saved round data could not be read.",
          paste(unique(vapply(state$errors,resource_failure_message,character(1))),collapse=" ")),error=TRUE),
        if(!state$loading && !length(state$errors) && !nrow(totals)) fm_info("No published round history",error=FALSE),
        if(nrow(totals) && any(!is.finite(totals$points))) fm_info(
          "Some saved round scores are missing. Lines have gaps where a complete cumulative result cannot be calculated; ranks require scores for every team.",error=TRUE)
      )
    })
    rank_plot <- shiny::reactive({
      h <- round_totals()
      z <- input$rank_range
      if(length(z)==2) h <- h[h$round>=z[1] & h$round<=z[2],,drop=FALSE]
      plots_round_chart(h, "rank")
    })
    owned_club_plot <- shiny::reactive({
      shiny::req(active(),login_token(),championship_id())
      if(is.function(refresh_trigger)) refresh_trigger()
      plots_owned_chart(get_championship_players(login_token(),championship_id()))
    })
    # One status follows the selected chart. The page information panel moves
    # disclosures out of their tab panes, so hidden panes cannot scope them.
    output$chart_status <- shiny::renderUI({
      shiny::req(active(), input$plot_tab)
      switch(input$plot_tab,
        "Standings" = round_status(),
        "Rank progression" = round_status(),
        "Squad values" = attr(values_plot(), "fm_info"),
        "Cash & transfers" = attr(cash_plot(), "fm_info"),
        "Owned by real team" = attr(owned_club_plot(), "fm_info"))
    })
    plot_sources <- list(standings=standings_plot, values=values_plot, cash=cash_plot,
      rank=rank_plot, owned_club=owned_club_plot)
    for (key in names(plot_sources)) local({
      source <- plot_sources[[key]]
      output[[paste0(key, "_plot")]] <- plotly::renderPlotly(source())
    })
  })
}
