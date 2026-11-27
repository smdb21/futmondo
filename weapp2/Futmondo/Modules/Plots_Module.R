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

# Use a saved official ranking only for the exact catalog round. The native
# P/V codes alone do not establish closure. Independently closed team rows
# can corroborate a historical ranking when the catalog lacks that flag.
plots_closed_stage <- function(rounds) {
  rounds$partially_played %in% TRUE & tolower(rounds$status) %in% "closed"
}

plots_saved_round_ranking <- function(snapshot, round, history, verified_histories = character()) {
  stage <- isTRUE(plots_closed_stage(round))
  if (!stage && (isTRUE(round$partially_played) || identical(round$is_finished, FALSE) ||
      isTRUE(classification_playing_rounds(round)))) return(NULL)
  id <- as.character(round$round_id)
  number <- round$round_number
  if (!is.list(snapshot) || !identical(snapshot$round_id, id) ||
      !isTRUE(snapshot$round_number == number)) return(NULL)
  if (any(c(attr(snapshot, "worker_completeness"), attr(snapshot, "fetch_status")) %in%
      c("partial", "unavailable", "error"))) return(NULL)
  ranking <- snapshot$rows
  if (!is.data.frame(ranking) || !nrow(ranking) || nrow(ranking) > 500L ||
      !all(c("round_id", "round", "team_id", "points") %in% names(ranking)) ||
      !all(ranking$round_id %in% id & ranking$round %in% number) ||
      anyNA(ranking$team_id) || any(!nzchar(ranking$team_id)) || anyDuplicated(ranking$team_id) ||
      !is.numeric(ranking$points) || any(!is.finite(ranking$points))) return(NULL)
  known <- history[history$round_id %in% id & is.finite(history$points), , drop = FALSE]
  index <- match(known$team_id, ranking$team_id)
  if (anyNA(index) || any(known$points != ranking$points[index])) return(NULL)
  status <- mvp_text(snapshot$answer$metadata$status)
  closed <- round_is_finished(list(status = status))
  if (stage) {
    # A closed scoring stage can have a postponed fixture outstanding. Require
    # independent closed scores for every listed participant before using it.
    if (!(closed || status %in% c("P", "V")) || !setequal(known$team_id, ranking$team_id) ||
        !all(vapply(known$status, function(x) round_is_finished(list(status = x)), logical(1)))) return(NULL)
    attr(ranking, "stage_history_teams") <- verified_histories
    return(ranking)
  }
  if (!closed && status %in% c("P", "V")) {
    closed <- isTRUE(round$is_finished) || (nrow(known) > 0L &&
      all(vapply(known$status, function(x) round_is_finished(list(status = x)), logical(1))))
  }
  if (closed) ranking else NULL
}

plots_round_numbers <- function(rows, rounds=NULL) {
  if(!is.null(rounds)){
    published <- classification_available_rounds(list(rounds=rounds),rows,whole_rounds=FALSE)
    return(rev(classification_round_order(published,rounds,by_latest_fixture=FALSE)))
  }
  played <- attr(rows,"played_rounds",exact=TRUE)
  if(!is.null(played))return(played)
  published <- c(rows$round,attr(rows,"finished_rounds",exact=TRUE))
  sort(unique(published[is.finite(published)]))
}

plots_ranking_targets <- function(rows,rounds) {
  unique_ids <- !duplicated(rounds$round_number) & !duplicated(rounds$round_number,fromLast=TRUE)
  published <- rounds[unique_ids & rounds$round_number%in%plots_round_numbers(rows),,drop=FALSE]
  open <- rep(FALSE, nrow(published))
  if ("is_finished" %in% names(published)) open <- published$is_finished %in% FALSE
  if ("partially_played" %in% names(published)) open <- open | published$partially_played %in% TRUE
  if (all(c("status", "partially_played") %in% names(published))) open <- open & !plots_closed_stage(published)
  published <- published[!open, , drop = FALSE]
  if("status"%in%names(published))published <- published[!classification_playing_rounds(published),,drop=FALSE]
  known <- unique(rows[is.finite(rows$round)&is.finite(rows$points),c("team_id","round")])
  covered <- tabulate(match(known$round,published$round_number),nbins=nrow(published))
  targets <- published[covered<length(unique(rows$team_id)),,drop=FALSE]
  targets[, intersect(c("round_id", "round_number", "is_finished", "partially_played", "status"), names(targets)), drop = FALSE]
}

# Numeric subrounds retain distinct labels. Missing observations block later
# totals, while a verified nonparticipant has no point or position for that round.
plots_round_totals <- function(rows, rankings = list()) {
  empty <- data.frame(team_id=character(), team_name=character(), round=numeric(), points=numeric(), rank=numeric())
  if (!is.data.frame(rows) || !nrow(rows)) return(empty)
  teams <- unique(rows[c("team_id", "team_name")])
  numbers <- plots_round_numbers(rows)
  if (!length(numbers)) return(empty)
  rows <- rows[is.finite(rows$round), , drop=FALSE]
  rows <- rows[!duplicated(paste(rows$team_id, rows$round), fromLast=TRUE), , drop=FALSE]
  scores <- matrix(NA_real_, nrow = nrow(teams), ncol = length(numbers))
  present <- matrix(FALSE, nrow = nrow(teams), ncol = length(numbers))
  index <- cbind(match(rows$team_id, teams$team_id), match(rows$round, numbers))
  scores[index] <- rows$points
  present[index] <- TRUE
  absent <- matrix(FALSE, nrow = nrow(teams), ncol = length(numbers))
  other_participants <- logical(length(numbers))
  for (ranking in rankings) {
    col <- match(ranking$round[1], numbers)
    if (is.na(col)) next
    matched <- match(teams$team_id, ranking$team_id)
    known <- is.finite(scores[, col])
    if (any(is.na(matched[known])) || any(scores[known, col] != ranking$points[matched[known]])) next
    scores[!known, col] <- ranking$points[matched[!known]]
    absent[, col] <- !present[, col] & is.na(matched)
    stage_teams <- attr(ranking, "stage_history_teams", exact = TRUE)
    if (!is.null(stage_teams)) absent[, col] <- absent[, col] & teams$team_id %in% stage_teams
    other_participants[col] <- any(!ranking$team_id %in% teams$team_id)
  }
  missing <- !absent & !is.finite(scores)
  contributions <- scores
  contributions[absent] <- 0
  cumulative <- scores
  for (i in seq_len(nrow(teams))) cumulative[i, ] <- cumsum(contributions[i, ])
  cumulative[absent] <- NA_real_
  ranks <- matrix(NA_real_, nrow = nrow(teams), ncol = length(numbers))
  for (col in seq_along(numbers)) {
    eligible <- !absent[, col]
    if (!other_participants[col] && all(is.finite(cumulative[eligible, col]))) {
      ranks[eligible, col] <- rank(-cumulative[eligible, col], ties.method = "min")
    }
  }
  # Coverage details are consumed by the status independently of chart rendering.
  totals <- data.frame(team_id = rep(teams$team_id, each = length(numbers)),
    team_name = rep(teams$team_name, each = length(numbers)), round = rep(numbers, nrow(teams)),
    points = as.vector(t(cumulative)), rank = as.vector(t(ranks)))
  attr(totals, "missing_rounds") <- numbers[colSums(missing) > 0L]
  attr(totals, "other_participants") <- numbers[other_participants]
  totals
}

plots_round_chart <- function(totals, metric="points") {
  if (!nrow(totals)) return(plots_empty("No published round history", error=FALSE))
  labels <- as.character(unique(totals$round))
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
  shiny::tagList(shiny::h2("Trends"), shiny::uiOutput(ns("chart_status")),
    shiny::tabsetPanel(id=ns("plot_tab"), type="pills",
      shiny::tabPanel("Standings",
        shiny::p("Season points accumulated through each published round or subround. Missing scores leave gaps."),
        shiny::fluidRow(
          shiny::column(3, shiny::selectInput(ns("standings_from"), "From round",
            choices = c("First published" = "first"), selectize = FALSE, width = "100%")),
          shiny::column(3, shiny::selectInput(ns("standings_through"), "Through round",
            choices = c("Latest published" = "latest"), selectize = FALSE, width = "100%"))),
        plotly::plotlyOutput(ns("standings_plot"), height = "420px")),
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
    bind_factual_reads(c("get_league_standings_history","get_financial_snapshot","get_championship_pressroom","get_user_team_rounds","get_classification_round_catalog","get_classification_round_ranking","get_championship_players"),session,login_token,championship_id,user_team_id,is_active=is_module_active,refresh_trigger=refresh_trigger)

    active <- function() isTRUE(is_module_active())
    history <- shiny::reactive({
      shiny::req(active(), input$plot_tab %in% c("Squad values", "Cash & transfers"), championship_id())
      if (is.function(refresh_trigger)) refresh_trigger()
      tryCatch(get_league_standings_history(championship_id()), error = function(e) data.frame())
    })
    standings_rows <- resource_display_source(session, function() {
      totals <- round_totals()
      rounds <- round_domain()
      if (length(rounds)) {
        start <- match(input$standings_from %||% "first", rounds, nomatch = 1L)
        end <- match(input$standings_through %||% "latest", rounds, nomatch = length(rounds))
        shiny::validate(shiny::need(start <= end, "Choose an end round at or after the start on the chart."))
        rounds <- rounds[seq.int(start, end)]
      }
      keep <- totals$round %in% rounds
      data.frame(team_name = totals$team_name[keep], round = totals$round[keep], points = totals$points[keep])
    })
    standings_plot <- shiny::reactive({ plots_round_chart(standings_rows(), "points") })
    values_plot <- shiny::reactive({ plots_history(history(),"team_value","Squad valuation (EUR)") })
    cash_date_scope <- NULL
    shiny::observe({
      shiny::req(input$plot_tab == "Cash & transfers")
      h <- history()
      if (!is.data.frame(h) || !nrow(h) || !"recorded_at" %in% names(h)) return()
      dates <- as.Date(fm_time(h$recorded_at), tz = fm_display_timezone())
      dates <- dates[!is.na(dates)]
      if (!length(dates)) return()
      limits <- range(dates)
      scope <- list(login_token()[["userid"]], championship_id(), limits)
      if (identical(scope, cash_date_scope)) return()
      cash_date_scope <<- scope
      shiny::updateDateRangeInput(session, "cash_dates", start = limits[1], end = limits[2],
        min = limits[1], max = limits[2])
    })
    transfer_history <- shiny::reactive({
      shiny::req(active(), login_token(), championship_id(), user_team_id())
      classification_read_state(function() {
        get_championship_pressroom(login = login_token(), championship_id = championship_id())
      })
    })
    read_transfer_amounts <- factual_read_adapter(session, "get_championship_transfer_amounts",
      login_token, championship_id, user_team_id,
      is_active = function() active() && identical(input$plot_tab, "Money movements"),
      refresh_trigger = refresh_trigger, state_only = TRUE)
    read_transfer_baseline <- factual_read_adapter(session, "read_persisted_history_baseline",
      login_token, championship_id, user_team_id,
      is_active = function() active() && identical(input$plot_tab, "Money movements"),
      refresh_trigger = refresh_trigger, state_only = TRUE)
    transfer_chart_history <- shiny::reactive({
      shiny::req(active(), login_token(), championship_id(), user_team_id())
      auth <- login_token()
      if (!isTRUE(fm_worker_mode(auth[["userid"]]))) return(transfer_history())
      league <- championship_id()
      args <- list(championship_id = league)
      scope <- resource_context(auth, league, user_team_id())
      scope <- fm_worker_resolve_context("get_championship_pressroom", args, scope)
      full <- fm_worker_descriptor("get_championship_pressroom", args, scope)
      # Read the already decoded history when another page has loaded it.
      cached <- fm_worker_get(scope$account_id, full$resource_key)
      if (!is.null(cached) && resource_snapshot_unexpired(cached$data)) return(transfer_history())
      amounts <- read_transfer_amounts(login = auth, championship_id = league)
      if (is.data.frame(amounts$data))
        return(list(data = amounts$data, loading = FALSE, error = NULL))
      if (identical(amounts$status, "loading")) {
        if (!is.null(cached)) return(transfer_history())
        return(list(data = NULL, loading = TRUE, error = NULL))
      }
      # Older stored histories may predate the slim projection. Keep the
      # normal snapshot subscription and recover from the audited baseline.
      state <- transfer_history()
      history <- state$data
      if (is.data.frame(history) && all(c("created", "price") %in% names(history)) &&
          !"unavailable" %in% c(attr(history, "fetch_status"), attr(history, "worker_completeness")))
        return(state)
      if (!all(vapply(scope[c("competition_id", "season", "scoring_version")], fm_context_known, logical(1))))
        return(state)
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
    own_round_state <- shiny::reactive({
      shiny::req(active(), login_token(), championship_id(), user_team_id())
      if (is.function(refresh_trigger)) refresh_trigger()
      classification_read_state(function() get_user_team_rounds(login_token(), championship_id(), user_team_id()))
    })
    round_inputs <- resource_display_source(session, function() {
      shiny::req(active(), login_token(), championship_id(), user_team_id())
      if (is.function(refresh_trigger)) refresh_trigger()
      auth <- login_token()
      league <- championship_id()
      own_id <- user_team_id()
      # The signed-in team is already known at login; request its saved history
      # in the first batch with the directory and catalog.
      own <- own_round_state()
      directory <- classification_read_state(user_teams_RV)
      catalog <- catalog_state()
      teams <- directory$data
      rounds <- catalog$data$rounds %||% data.frame()
      reads <- list(directory, catalog)
      histories <- list()
      names_by_team <- character()
      if (is.data.frame(teams) && nrow(teams)) {
        id_col <- intersect(c("teamid", "id"), names(teams))[1]
        name_col <- intersect(c("teamname", "name"), names(teams))[1]
        if (!is.na(id_col) && !is.na(name_col)) for (i in seq_len(nrow(teams))) {
          team_id <- as.character(teams[[id_col]][i])
          if (is.na(team_id) || !nzchar(team_id)) next
          result <- if (identical(team_id, own_id)) own else classification_read_state(function() {
            get_user_team_rounds(auth, league, team_id)
          })
          names_by_team[team_id] <- as.character(teams[[name_col]][i])
          answer <- result$data
          # Keep freshness in the adapters. Top-level snapshot clocks cannot
          # change the round rows consumed by either chart.
          if (is.list(answer)) {
            keep <- intersect(names(attributes(answer)), c("names", "class", "row.names", "valid_response", "worker_completeness"))
            attributes(answer) <- attributes(answer)[keep]
          }
          histories[team_id] <- list(answer)
          reads[[length(reads) + 1L]] <- result
        }
      }
      list(teams = names_by_team, rounds = rounds, histories = histories,
        reads = lapply(reads, function(read) read[c("loading", "error")]))
    })
    round_history <- shiny::reactive({
      inputs <- round_inputs()
      rounds <- inputs$rounds
      frames <- lapply(names(inputs$teams), function(team) {
        name <- inputs$teams[[team]]
        rows <- classification_round_rows(inputs$histories[[team]], team, name, rounds)
        if (nrow(rows)) rows else data.frame(team_id = team, team_name = name, round = NA_real_, points = NA_real_)
      })
      rows <- dplyr::bind_rows(c(list(classification_empty_rows()), frames))
      attr(rows, "played_rounds") <- plots_round_numbers(rows,rounds)
      list(rows = rows, rounds = rounds, reads = inputs$reads, histories = inputs$histories)
    })
    pending_reads <- function(reads) any(vapply(reads, function(x) isTRUE(x$loading), logical(1)))
    ranking_targets <- resource_display_source(session, function() {
      history <- round_history()
      rows <- history$rows
      rounds <- history$rounds
      if (pending_reads(history$reads) || !isTRUE(fm_worker_mode(login_token()[["userid"]])) ||
          !nrow(rows) || !all(c("round_id", "round_number") %in% names(rounds))) return(NULL)
      plots_ranking_targets(rows,rounds)
    })
    ranking_readers <- shiny::reactive({
      targets <- ranking_targets()
      # After team reads settle, request just the affected saved rankings.
      # Reuse a settled reader when another ranking publishes.
      if (is.null(targets) || !nrow(targets)) return(list())
      auth <- login_token()
      league <- championship_id()
      own_id <- user_team_id()
      lapply(seq_len(nrow(targets)), function(i) {
        round <- targets[i, , drop = FALSE]
        list(round = round, read = shiny::reactive(classification_read_state(function() {
          get_classification_round_ranking(auth, league, own_id, as.character(round$round_id), round$round_number)
        })))
      })
    })
    ranking_history <- shiny::reactive({
      histories <- round_history()$histories
      dplyr::bind_rows(lapply(names(histories), function(team) {
        answer <- histories[[team]]
        rival_team_round_rows(answer$rounds %||% answer, team)
      }))
    })
    round_state <- shiny::reactive({
      history <- round_history()
      reads <- history$reads
      rankings <- list()
      readers <- ranking_readers()
      if (length(readers)) {
        native <- ranking_history()
        verified_histories <- names(Filter(function(x) isTRUE(attr(x, "valid_response")) &&
          !any(attr(x, "worker_completeness") %in% c("partial", "unavailable")), history$histories)) %||% character()
        for (reader in readers) {
          read <- reader$read()
          reads[[length(reads) + 1L]] <- read
          ranking <- plots_saved_round_ranking(read$data, reader$round, native, verified_histories)
          if (!is.null(ranking)) rankings[[length(rankings) + 1L]] <- ranking
        }
      }
      list(data = history$rows, rankings = rankings, loading = pending_reads(reads),
        errors = Filter(Negate(is.null), lapply(reads, `[[`, "error")))
    })
    # Metadata-only worker publications must not redraw the same chart.
    round_totals <- resource_display_source(session, function() {
      state <- round_state()
      plots_round_totals(state$data, state$rankings)
    })
    # Keep the chart's kickoff order, including subrounds and fixtures played early.
    round_domain <- resource_display_source(session, function() unique(round_totals()$round))
    shiny::observeEvent(round_domain(), {
      rounds <- as.character(round_domain())
      selected <- input$standings_from
      shiny::updateSelectInput(session, "standings_from", choices = c("First published" = "first", rounds),
        selected = if (length(selected) && selected %in% rounds) selected else "first")
      selected <- input$standings_through
      shiny::updateSelectInput(session, "standings_through", choices = c("Latest published" = "latest", rounds),
        selected = if (length(selected) && selected %in% rounds) selected else "latest")
    })
    shiny::observe({
      numbers <- round_domain()
      if(length(numbers)) shiny::updateSliderInput(session,"rank_range",min=min(numbers),max=max(numbers),
        value=range(numbers),step=0.1)
    })
    round_status <- shiny::reactive({
      state <- round_state()
      totals <- round_totals()
      missing <- attr(totals, "missing_rounds")
      other <- attr(totals, "other_participants")
      shiny::tagList(
        if(state$loading) shiny::div(class="fm-plot-loading",role="status",
          "Loading saved round scores. Verified history appears as it becomes available."),
        if(length(state$errors)) fm_info(paste("Some saved round data could not be read.",
          paste(unique(vapply(state$errors,resource_failure_message,character(1))),collapse=" ")),error=TRUE),
        if(!state$loading && !length(state$errors) && !nrow(totals)) fm_info("No published round history",error=FALSE),
        if (!state$loading && length(missing)) fm_info(paste0(
          "Some saved round scores are missing. The first missing score is in round ", missing[1L],
          ". Affected teams' cumulative results stop at that point."), error = TRUE),
        if (identical(input$plot_tab, "Rank progression") && length(other)) fm_info(
          "Some saved rankings include teams outside the current league directory. Ranks for those rounds need their cumulative scores.", error = TRUE)
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
