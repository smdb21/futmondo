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
    totals$rank[ix] <- rank(-totals$points[ix], ties.method="min", na.last="keep")
  }
  totals
}

plots_round_chart <- function(totals, metric="points") {
  if (!nrow(totals)) return(plots_empty("No published round history", error=FALSE))
  labels <- as.character(sort(unique(totals$round)))
  totals$round_label <- as.character(totals$round)
  title <- if (metric == "rank") "Cumulative rank" else "Cumulative points"
  plot <- plotly::plot_ly(totals, x=~round_label, y=totals[[metric]], color=~team_name,
    colors=plots_team_colors(totals$team_name), type="scatter", mode="lines+markers", connectgaps=FALSE,
    hovertemplate=~paste0("<b>",team_name,"</b><br>Round ",round_label,"<br>",
      title,": ",totals[[metric]],"<extra></extra>")) %>%
    fm_plot_layout(xaxis=list(title="Round", type="category", categoryorder="array", categoryarray=labels),
      yaxis=list(title=title, autorange=if(metric == "rank") "reversed" else TRUE,
        dtick=if(metric == "rank") 1 else NULL), showlegend=FALSE,
      margin=list(l=55,r=15,t=45,b=55))
  if (any(!is.finite(totals[[metric]]))) attr(plot, "fm_info") <- fm_info(
    "Some saved round scores are missing. Lines have gaps where a complete cumulative result cannot be calculated.", error=TRUE)
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
  shiny::tagList(shiny::h2("Plots"), fm_info("League charts optimized for a single full-width view on phones."),
    shiny::tabsetPanel(id=ns("plot_tab"), type="pills",
      shiny::tabPanel("Standings", shiny::p("Cumulative fantasy-team points after each published round or subround. Repeated daily snapshots are excluded; missing scores remain gaps."), shiny::uiOutput(ns("standings_status")), plotly::plotlyOutput(ns("standings_plot"), height="420px")),
      shiny::tabPanel("Squad values", shiny::p("Saved squad market valuations over time, in euros."), shiny::uiOutput(ns("values_status")), plotly::plotlyOutput(ns("values_plot"), height="420px")),
      shiny::tabPanel("Cash & transfers", shiny::p("Compare transfer-only balance estimates, squad purchase spending or transaction volume for the selected dates. Balance estimates exclude round rewards and other cash adjustments."), shiny::fluidRow(shiny::column(4, shiny::dateRangeInput(ns("cash_dates"),"Date range",format="dd/mm/yyyy")), shiny::column(8, shiny::radioButtons(ns("cash_metric"),"Metric",choices=c("Transfer-only balance estimate"="cash","Squad Purchases"="investment","Transaction Volume"="volume"),inline=FALSE))), shiny::uiOutput(ns("cash_status")), plotly::plotlyOutput(ns("cash_plot"), height="420px")),
      shiny::tabPanel("Rank progression", shiny::p("Each fantasy team's rank from cumulative published round points; first place is at the top. The range selects which rounds are displayed."), shiny::sliderInput(ns("rank_range"),"Rounds",min=1,max=38,value=c(1,38),step=0.1), shiny::uiOutput(ns("rank_status")), plotly::plotlyOutput(ns("rank_plot"), height="420px")),
      shiny::tabPanel("Owned by real team", shiny::p("Currently fantasy-owned players, grouped by their real football club."), shiny::uiOutput(ns("owned_club_status")), plotly::plotlyOutput(ns("owned_club_plot"), height="520px"))
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
    cash_plot <- shiny::reactive({
      shiny::req(active(),login_token(),championship_id(),user_team_id(),user_teams_RV()); if(is.function(refresh_trigger)) refresh_trigger()
      teams<-user_teams_RV(); snap<-tryCatch(get_financial_snapshot(login_token(),championship_id(),user_team_id()),error=function(e)NULL)
      initial<-normalize_league_rules(list(configuration=if(is.list(snap))snap$configuration else list()))$initial_budget
      press<-tryCatch(get_championship_pressroom(login_token(),championship_id()),error=function(e)data.frame())
      if(!is.data.frame(press)||!nrow(press)) return(plots_empty("Observed transfer data unavailable"))
      dates<-input$cash_dates; if(is.null(dates)||length(dates)!=2) return(plots_empty("Choose a date range", error=FALSE))
      d<-rivals_buying_power_values(press,teams,metric=input$cash_metric %||% "cash",start_date=fm_local_midnight(dates[1]),end_date=fm_local_midnight(dates[2]+1)-0.000001,initial_budget=initial)
      if(!is.data.frame(d)||!nrow(d)) return(plots_empty("League financial data unavailable"))
      d<-d[order(d$value),,drop=FALSE]; plotly::plot_ly(d,x=~value,y=~reorder(team,value),type="bar",orientation="h",marker=list(color="#608BC1"),hovertemplate=~paste0("<b>",team,"</b><br>",format(value,big.mark=".",decimal.mark=",",scientific=FALSE)," EUR<extra></extra>")) %>% fm_plot_layout(xaxis=list(title="EUR"),yaxis=list(title=""),margin=list(l=115,r=15,t=45,b=45))
    })
    rank_rows <- shiny::reactive({
      shiny::req(active(),login_token(),championship_id(),user_teams_RV())
      if(is.function(refresh_trigger)) refresh_trigger()
      teams <- user_teams_RV()
      id_col <- c("teamid","id")[c("teamid","id") %in% names(teams)][1]
      name_col <- c("teamname","name")[c("teamname","name") %in% names(teams)][1]
      if(is.na(id_col) || is.na(name_col)) return(classification_empty_rows())
      catalog <- get_classification_round_catalog(login_token(), championship_id())
      rounds <- catalog$rounds %||% data.frame()
      rows <- dplyr::bind_rows(c(list(classification_empty_rows()), lapply(seq_len(nrow(teams)),function(i) {
        d <- classification_round_rows(get_user_team_rounds(login_token(),championship_id(),teams[[id_col]][i]),
          teams[[id_col]][i],teams[[name_col]][i],rounds)
        if (!nrow(d)) d <- data.frame(team_id=as.character(teams[[id_col]][i]),
          team_name=as.character(teams[[name_col]][i]),round=NA_real_,points=NA_real_)
        d
      })))
      attr(rows, "round_numbers") <- rounds$round_number
      rows
    })
    round_totals <- shiny::reactive(plots_round_totals(rank_rows()))
    shiny::observe({
      r <- round_totals()
      if(nrow(r)) shiny::updateSliderInput(session,"rank_range",min=min(r$round),max=max(r$round),
        value=range(r$round),step=0.1)
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
    plot_sources <- list(standings=standings_plot, values=values_plot, cash=cash_plot,
      rank=rank_plot, owned_club=owned_club_plot)
    for (key in names(plot_sources)) local({
      source <- plot_sources[[key]]
      output[[paste0(key, "_status")]] <- shiny::renderUI(attr(source(), "fm_info"))
      output[[paste0(key, "_plot")]] <- plotly::renderPlotly(source())
    })
  })
}
