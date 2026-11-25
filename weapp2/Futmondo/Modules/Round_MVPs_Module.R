library(shiny)
library(dplyr)

round_mvp_rows <- function(rows, championship_id = NULL) {
  required <- c("round_number", "player_id", "player_name", "player_role", "points", "is_mvp", "is_finished")
  empty <- data.frame(round_number = numeric(), player_id = character(), player_name = character(),
                      player_role = character(), points = numeric(), stringsAsFactors = FALSE)
  if (is.null(rows) || !is.data.frame(rows) || !nrow(rows) || !all(required %in% names(rows))) return(empty)
  out <- rows
  if (!is.null(championship_id) && "championship_id" %in% names(out)) {
    out <- out[as.character(out$championship_id) %in% as.character(championship_id)[1], , drop = FALSE]
  }
  as_flag <- function(x) tolower(trimws(as.character(x))) %in% c("true", "t", "1")
  columns <- c(required[required != "is_mvp" & required != "is_finished"],intersect("round_id",names(out)))
  out <- out[as_flag(out$is_mvp), columns, drop = FALSE]
  if (!nrow(out)) return(empty)
  out$round_number <- suppressWarnings(as.numeric(out$round_number))
  out$points <- suppressWarnings(as.numeric(out$points))
  # Preserve valid official selections even when the catalog omits display numbers.
  if ("round_id" %in% names(rows)) {
    out <- out[!duplicated(out$round_id),,drop=FALSE]
  } else out <- out[is.finite(out$round_number) & !duplicated(out$round_number),,drop=FALSE]
  out[order(out$round_number, decreasing = TRUE), , drop = FALSE]
}

# Select by immutable round identity: display numbers may be missing or repeated.
round_best_xi_rows <- function(rows, championship_id, round) {
  required <- c("round_number", "player_id", "player_name", "player_role", "points", "is_mvp", "is_finished")
  if (!is.data.frame(rows) || !all(required %in% names(rows))) return(data.frame())
  if ("championship_id" %in% names(rows)) {
    rows <- rows[as.character(rows$championship_id) %in% as.character(championship_id)[1], , drop = FALSE]
  }
  if ("round_id" %in% names(rows) && "round_id" %in% names(round)) {
    rows <- rows[rows$round_id %in% round$round_id, , drop = FALSE]
  } else {
    rows <- rows[rows$round_number %in% round$round_number, , drop = FALSE]
  }
  as_flag <- function(x) tolower(trimws(as.character(x))) %in% c("true", "t", "1")
  rows$is_mvp <- as_flag(rows$is_mvp)
  if (nrow(rows) != 11L || anyNA(rows$player_id) || any(!nzchar(rows$player_id)) ||
      anyDuplicated(rows$player_id) || sum(rows$is_mvp) != 1L) return(data.frame())
  rows$points <- suppressWarnings(as.numeric(rows$points))
  positions <- analytics_player_positions(data.frame(role = rows$player_role))
  rows$display_role <- vapply(seq_len(nrow(rows)), function(i) {
    if (length(positions[[i]])) positions[[i]][1] else mvp_text(rows$player_role[i], "Unavailable")
  }, character(1))
  rows$display_role[!nzchar(rows$display_role)] <- "Unavailable"
  rows[order(match(rows$display_role, c("GK", "DEF", "MID", "FWD")), na.last = TRUE), , drop = FALSE]
}

round_award_card <- function(mvp, xi, players = NULL) {
  # Join by player identity, retaining the official award names and scores.
  identities <- data.frame(name = xi$player_name, team = rep("", nrow(xi)), logo = rep("", nrow(xi)))
  if (is.data.frame(players) && all(c("id", "team") %in% names(players))) {
    index <- match(xi$player_id, as.character(players$id))
    identities$team <- as.character(players$team[index])
    if ("logo" %in% names(players)) identities$logo <- as.character(players$logo[index])
  }
  name_cell <- get_reactable_columns_for_players(identities)$name$cell
  position_classes <- c(GK = "badge-gk", DEF = "badge-df", MID = "badge-md", FWD = "badge-fw")
  tags$article(class = "round-mvp-card",
    tags$div(class = "round-mvp-round", if (is.finite(mvp$round_number))
      paste("Round", format(mvp$round_number, trim = TRUE, scientific = FALSE)) else "Round number unavailable"),
    tags$div(class = "round-mvp-player", tags$span(class = "round-mvp-badge", "MVP"), " ", mvp$player_name),
    tags$div(class = "round-mvp-meta", if (is.finite(mvp$points))
      paste(format(mvp$points, trim = TRUE), "points") else "Points unavailable"),
    tags$h4(class = "round-best-xi-title", "Best XI"),
    if (!nrow(xi)) fm_info("The official Best XI selection is incomplete.", error = TRUE, class = "round-mvps-empty") else
      tags$table(class = "round-best-xi-table", `aria-label` = "Official Best XI",
        tags$thead(tags$tr(tags$th(scope = "col", "Role"), tags$th(scope = "col", "Player"), tags$th(scope = "col", "Points"))),
        tags$tbody(lapply(seq_len(nrow(xi)), function(i) {
          position <- xi$display_role[i]
          position_class <- unname(position_classes[position])
          tags$tr(class = if (xi$is_mvp[i]) "round-best-xi-mvp",
            tags$td(if (is.na(position_class)) position else tags$span(class = position_class, position)),
            tags$td(name_cell(xi$player_name[i], i), if (xi$is_mvp[i]) tagList(" ", tags$span(class = "round-mvp-badge", "MVP"))),
            tags$td(if (is.finite(xi$points[i])) format(xi$points[i], trim = TRUE) else "Unavailable"))
        }))))
}

round_award_crest_urls <- function(logos) {
  keys <- unique(logos)
  vapply(keys, lineup_fixture_crest_url, character(1), USE.NAMES = FALSE)[match(logos, keys)]
}

# Resolve the metadata once per winner for both charts and the paginated table.
round_award_player_identities <- function(players, catalog = NULL) {
  current <- if (is.data.frame(catalog) && "id" %in% names(catalog))
    catalog[match(players$player_id, as.character(catalog$id)), , drop = FALSE] else data.frame()
  field <- function(data, key) {
    if (!key %in% names(data)) return(rep("", nrow(players)))
    value <- trimws(as.character(data[[key]]))
    value[is.na(value)] <- ""
    value
  }
  # Resolve repeated club assets just once. A later club cannot supply the
  # image for a different club named in the award.
  team <- field(players, "award_team")
  crest <- round_award_crest_urls(field(players, "award_logo"))
  current_team <- field(current, "team")
  current_crest <- round_award_crest_urls(field(current, "logo"))
  use_current <- !nzchar(crest) & (!nzchar(team) | tolower(team) == tolower(current_team))
  crest[use_current] <- current_crest[use_current]
  fill_team <- !nzchar(team) & (use_current | crest == current_crest)
  team[fill_team] <- current_team[fill_team]
  missing <- !nzchar(crest) & nzchar(team)
  clubs <- unique(team[missing])
  crest[missing] <- round_award_crest_urls(vapply(clubs, get_team_image_name, character(1)))[match(team[missing], clubs)]

  position <- field(players, "award_positions")
  fill <- !nzchar(position)
  position_title <- rep("Positions in confirmed Best XIs", nrow(players))
  if (any(fill) && nrow(current)) {
    positions <- lineup_positions(current[fill, , drop = FALSE], multiposition = TRUE)
    position[fill] <- vapply(positions, paste, collapse = " / ", FUN.VALUE = character(1))
    position_title[fill] <- "Current catalog positions"
  }
  position_title[!nzchar(position)] <- "Position unavailable"
  position[!nzchar(position)] <- "—"

  players$crest <- crest
  players$club <- ifelse(nzchar(team), team, "Club crest")
  players$position <- position
  players$position_title <- position_title
  players
}

round_award_chart_player <- function(player) {
  tags$span(class = "award-player",
    if (nzchar(player$crest)) tags$img(class = "fm-team-crest", src = player$crest, alt = player$club,
      title = player$club, width = 22, height = 22, loading = "lazy", decoding = "async",
      onerror = "this.style.display='none';"),
    tags$span(class = "award-player-details", tags$span(player$player_name),
      tags$small(class = "award-position", `data-position` = player$position, title = player$position_title, player$position)))
}

# Reactable renders only the visible page. The underlying name remains plain text.
round_award_name_cell <- htmlwidgets::JS("function(cell) {
  const row = cell.row;
  const el = React.createElement;
  const crest = row.crest && el('img', {key: row.crest, className: 'fm-team-crest', src: row.crest, alt: row.club,
    title: row.club, width: 22, height: 22, loading: 'lazy', decoding: 'async',
    onError: function(event) { event.currentTarget.style.display = 'none'; }});
  return el('span', {className: 'award-player'}, crest,
    el('span', {className: 'award-player-details'}, el('span', null, cell.value),
      el('small', {className: 'award-position', 'data-position': row.position, title: row.position_title}, row.position)));
}")

round_award_catalog_notice <- function(catalog) {
  if (!is.null(catalog) && is.null(catalog$data) && !identical(catalog$status, "loading"))
    fm_info(paste("Player team crests:", resource_failure_message(catalog$error)), error = TRUE, class = "round-mvps-empty")
}

round_award_leaders_plot <- function(players, metric, title, rounds) {
  counts <- players[[metric]]
  ranked <- players[order(-counts, players$player_name, players$player_id, method = "radix"), , drop = FALSE]
  ranked <- ranked[ranked[[metric]] > 0L, , drop = FALSE]
  leaders <- head(ranked, 5L)
  values <- leaders[[metric]]
  tied <- sum(ranked[[metric]] == tail(values, 1L)) - sum(values == tail(values, 1L))
  tags$figure(class = paste("award-chart", paste0("award-chart-", metric)),
    tags$figcaption(title),
    tags$ol(lapply(seq_along(values), function(i) {
      tags$li(tags$span(class = "award-chart-name", round_award_chart_player(leaders[i, , drop = FALSE])),
        tags$span(class = "award-chart-count", values[i]),
        tags$span(class = "award-chart-track", `aria-hidden` = "true",
          tags$span(style = sprintf("width: %.2f%%", 100 * values[i] / rounds))))
    })),
    tags$div(class = "award-chart-scale", `aria-hidden` = "true", tags$span("0"),
      tags$span(sprintf(ngettext(rounds, "%d round", "%d rounds"), rounds))),
    if (tied > 0L) tags$p(class = "award-chart-note",
      sprintf(ngettext(tied, "%d more player tied at %d.", "%d more players tied at %d."), tied, tail(values, 1L))))
}

round_award_statistics_view <- function(stats, snapshot, catalog = NULL) {
  reason <- mvp_text(snapshot$reason)
  coverage <- snapshot$coverage
  future_only <- is.data.frame(coverage) && nrow(coverage) > 0L &&
    "required" %in% names(coverage) && all(coverage$required %in% FALSE)
  missing <- if (is.data.frame(coverage) && all(c("required", "selection") %in% names(coverage)))
    sum(coverage$required %in% TRUE & coverage$selection %in% "Missing") else 0L
  if (missing > 0L) {
    reason <- trimws(sub("Some round awards are missing.", "", reason, fixed = TRUE))
  }
  issues <- c(if (missing > 0L) sprintf(
      ngettext(missing, "%d round is missing its award selection.", "%d rounds are missing their award selections."), missing),
    if (nzchar(reason)) reason,
    if (nzchar(stats$scope_issue)) stats$scope_issue,
    if (stats$unsaved_rounds > 0L) sprintf(
      ngettext(stats$unsaved_rounds, "%d selection awaits a confirmed save.", "%d selections await a confirmed save."),
      stats$unsaved_rounds),
    if (stats$unconfirmed_rounds > 0L) sprintf(
      ngettext(stats$unconfirmed_rounds, "%d selection awaits confirmation.", "%d selections await confirmation."),
      stats$unconfirmed_rounds),
    if (stats$incomplete_rounds > 0L) sprintf(
      ngettext(stats$incomplete_rounds, "%d selection is incomplete.", "%d selections are incomplete."),
      stats$incomplete_rounds))
  if (!length(issues) && identical(snapshot$status, "partial") && !future_only)
    issues <- "Coverage of earlier round awards is unconfirmed."
  notice <- if (length(issues)) tags$p(class = "award-coverage", paste(issues, collapse = " "))
  if (!stats$rounds) {
    return(tagList(tags$p(if (future_only && !length(issues)) "No completed round awards yet." else
      "No confirmed round awards are available for these rankings."), notice))
  }

  players <- stats$players
  same_name <- duplicated(players$player_name) | duplicated(players$player_name, fromLast = TRUE)
  players$player_name[same_name] <- paste0(players$player_name[same_name], " (", players$player_id[same_name], ")")
  players <- round_award_player_identities(players, catalog$data)
  players$share <- players$best_xi / stats$rounds
  metric <- function(value, label) tags$div(tags$dt(label), tags$dd(value))
  tagList(
    tags$dl(class = "award-stats-summary",
      metric(stats$rounds, "Confirmed rounds"), metric(nrow(players), "Best XI players"),
      metric(sum(players$mvp > 0L), "MVP winners")),
    notice, round_award_catalog_notice(catalog),
    tags$div(class = "award-charts",
      round_award_leaders_plot(players, "best_xi", "Most Best XI appearances", stats$rounds),
      round_award_leaders_plot(players, "mvp", "Most MVP wins", stats$rounds)),
    tags$h4("All award winners"),
    tags$p(class = "award-table-note", "Round share is the percentage of confirmed rounds in which a player made the Best XI. Each MVP win is also a Best XI appearance."),
    reactable::reactable(players[c("player_name", "best_xi", "mvp", "share", "club", "crest", "position", "position_title")],
      searchable = TRUE, compact = TRUE, striped = TRUE, defaultPageSize = 10L,
      defaultSorted = list(best_xi = "desc", mvp = "desc", player_name = "asc"),
      defaultColDef = reactable::colDef(headerVAlign = "bottom"),
      columns = list(
        player_name = reactable::colDef(name = "Player", minWidth = 95, cell = round_award_name_cell),
        club = reactable::colDef(show = FALSE), crest = reactable::colDef(show = FALSE),
        position = reactable::colDef(show = FALSE), position_title = reactable::colDef(show = FALSE),
        best_xi = reactable::colDef(name = "Best XI", minWidth = 52, align = "right", defaultSortOrder = "desc"),
        mvp = reactable::colDef(name = "MVP", minWidth = 42, align = "right", defaultSortOrder = "desc"),
        share = reactable::colDef(name = "Round share", minWidth = 56, align = "right", defaultSortOrder = "desc",
          format = reactable::colFormat(percent = TRUE, digits = 1))),
      theme = fm_reactable_theme(),
      language = reactable::reactableLang(searchPlaceholder = "Search players", searchLabel = "Search award winners")))
}

round_mvps_UI <- function(id) {
  ns <- NS(id)
  fluidRow(box(width = 12, title = "Round awards", status = "primary", solidHeader = TRUE,
    tabsetPanel(id = ns("award_tab"), type = "pills", selected = "rounds",
      tabPanel("By round", value = "rounds",
        fm_info("Published official MVPs and Best XI selections.", class = "round-mvps-intro"),
        uiOutput(ns("mvp_cards"))),
      tabPanel("Statistics", value = "statistics",
        tags$div(class = "award-statistics", uiOutput(ns("award_statistics")))))))
}

round_mvps_Server <- function(id, is_module_active, login_token, championship_id, refresh_trigger = NULL) {
  moduleServer(id, function(input, output, session) {
    player_catalog_read <- factual_read_adapter(session, "get_championship_players", login_token,
      championship_id, is_active = is_module_active, refresh_trigger = refresh_trigger, state_only = TRUE)
    mvp_snapshot <- shared_mvp_source(session,login_token,championship_id,is_module_active,refresh_trigger)
    player_catalog <- reactive({
      auth <- login_token()
      if (valid_login(auth)) player_catalog_read(auth, championship_id())
    })
    award_stats <- reactive(round_award_player_stats(mvp_snapshot()$rows, championship_id()))
    output$award_statistics <- renderUI({
      req(isTRUE(is_module_active()))
      snapshot <- mvp_snapshot()
      if (isTRUE(snapshot$loading) || identical(snapshot$status, "loading"))
        return(NULL)
      stats <- award_stats()
      players <- stats$players
      catalog <- if (anyNA(players$award_positions) || any(!nzchar(round_award_crest_urls(unique(players$award_logo)))))
        player_catalog()
      round_award_statistics_view(stats, snapshot, catalog)
    })
    output$mvp_cards <- renderUI({
      snapshot <- mvp_snapshot()
      rows <- round_mvp_rows(snapshot$rows, championship_id())
      if(isTRUE(snapshot$loading)||identical(snapshot$status,"loading"))return(NULL)
      notice <- if(nzchar(snapshot$reason)) fm_info(snapshot$reason,
        error = !isTRUE(snapshot$loading) && !identical(snapshot$status, "loading"), class="round-mvps-empty")
      if (!nrow(rows)) {
        if (is.data.frame(snapshot$coverage) && nrow(snapshot$coverage) && !any(snapshot$coverage$required %in% TRUE) && is.null(notice))
          return(fm_info("No completed round awards yet."))
        return(notice %||% fm_info("Round awards are unavailable; the reason could not be determined.", error = TRUE))
      }
      catalog <- player_catalog()
      tagList(notice, round_award_catalog_notice(catalog), tags$div(class = "round-mvps-grid", lapply(seq_len(nrow(rows)), function(i) {
        row <- rows[i, , drop = FALSE]
        round_award_card(row, round_best_xi_rows(snapshot$rows, championship_id(), row), catalog$data)
      })))
    })
  })
}
