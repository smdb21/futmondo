# ============================================================
# Futmondo Today Module - Manager Command Center
# ============================================================
# Provides a daily actionable dashboard with KPIs, FIS-driven
# recommendations, market radar, and recent transfer intelligence.
# ============================================================

library(reactable)
library(dplyr)

# ---- Helper: safe reactive value extractor ----
today_get_reactive_val <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.reactive(x) || is.function(x)) {
    tryCatch(x(), error = function(e) NULL)
  } else {
    x
  }
}

# ---- Pure helper: prepare the market radar display data frame ----
# Coerces FIS to numeric, drops non-finite (NA/Inf/-Inf) rows, sorts by FIS
# descending, keeps the top N, and builds the display columns. This is the
# single source of truth for the radar table, so non-finite FIS values can
# never reach the rendered table (fixes FIS "error effect" rendering).
today_prepare_radar_df <- function(mkt, top_n = 10) {
  empty <- data.frame(
    Player = character(0), Role = character(0), Price = numeric(0),
    FIS = numeric(0), Tier = character(0), PlayerID = character(0),
    stringsAsFactors = FALSE
  )
  if (is.null(mkt) || !is.data.frame(mkt) || nrow(mkt) == 0) return(empty)

  df <- mkt
  if ("fis_score" %in% colnames(df)) {
    df$fis_score <- suppressWarnings(as.numeric(df$fis_score))
    # Drop non-finite FIS values (NA, Inf, -Inf) before sorting/slicing.
    df <- df[is.finite(df$fis_score), , drop = FALSE]
    if (nrow(df) == 0) return(empty)
    df <- df[order(df$fis_score, decreasing = TRUE), , drop = FALSE]
    if (nrow(df) > top_n) df <- df[seq_len(top_n), , drop = FALSE]
  }
  if (nrow(df) == 0) return(empty)

  data.frame(
    Player = if ("name" %in% colnames(df)) as.character(df$name) else rep("Unknown", nrow(df)),
    Role = if ("role" %in% colnames(df)) as.character(df$role) else rep("-", nrow(df)),
    Price = if ("value" %in% colnames(df)) suppressWarnings(as.numeric(df$value)) else rep(0, nrow(df)),
    FIS = if ("fis_score" %in% colnames(df)) round(df$fis_score, 1) else rep(0, nrow(df)),
    Tier = if ("fis_tier" %in% colnames(df)) as.character(df$fis_tier) else rep("-", nrow(df)),
    PlayerID = if ("id" %in% colnames(df)) as.character(df$id) else rep("", nrow(df)),
    stringsAsFactors = FALSE
  )
}

# ---- Pure helper: build the reactable onClick JS for the market radar ----
# Returns an htmlwidgets::JS() function that, on row click, sends the clicked
# row's PlayerID to the namespaced input 'radar_selected_player'.
# (htmlwidgets is a hard reactable dependency, so JS() is always available.)
today_radar_onclick_js <- function(ns) {
  htmlwidgets::JS(paste0(
    "function(rowInfo, column, state) {",
    "  var pid = rowInfo.values.PlayerID;",
    "  if (pid) {",
    "    Shiny.setInputValue('", ns("radar_selected_player"), "', pid, {priority: 'event'});",
    "  }",
    "}"
  ))
}

# ---- Pure helper: escape a value for embedding in a single-quoted JS string ----
# Backslashes are escaped FIRST (so the backslash introduced by the quote
# escape is not itself escaped), then single quotes. The result is safe to
# embed between single quotes in generated browser JS.
today_escape_js_string <- function(x) {
  s <- as.character(x)
  s <- gsub("\\\\", "\\\\\\\\", s)  # \ -> \\
  s <- gsub("'", "\\\\'", s)        # ' -> \'
  s
}

# ---- Pure helper: build the onclick attribute for a recommendation action button ----
# Returns an onclick attribute string that sends the player id + action to the
# namespaced input 'rec_action_clicked'. Values are escaped for backslashes
# AND single quotes (today_escape_js_string) so the generated JS is safe.
today_rec_action_onclick_js <- function(ns, player_id, action_label) {
  pid_js <- today_escape_js_string(player_id)
  act_js <- today_escape_js_string(action_label)
  paste0(
    "Shiny.setInputValue('", ns("rec_action_clicked"),
    "', {player_id: '", pid_js, "', action: '", act_js,
    "'}, {priority: 'event'});"
  )
}

# ---- Pure helper: normalize a recommendation action label to a stable action code ----
# Single source of truth for mapping feed action labels to stable codes:
#   - "Place Bid" (case-insensitive, trimmed) -> "market_bid"
#   - "Exercise Clause" (case-insensitive, trimmed) -> "clause_buyout"
#   - already-stable codes ("market_bid", "clause_buyout", "view") pass through unchanged
#   - anything else -> "view"
# No other code may infer action intent from raw labels.
today_normalize_action <- function(action_label) {
  a <- if (is.null(action_label)) "" else trimws(tolower(as.character(action_label)))
  if (is.na(a)) a <- ""
  if (a == "place bid") return("market_bid")
  if (a == "exercise clause") return("clause_buyout")
  if (a %in% c("market_bid", "clause_buyout", "view")) return(a)
  "view"
}

# ---- Pure helper: resolve a player id against a players data frame ----
# Matches on the immutable `id` column and returns the single matching row as
# a 1-row data frame, or NULL when the id is missing/unknown or the data frame
# is NULL/empty/lacks an id column.
today_resolve_player <- function(player_id, players_df) {
  if (is.null(player_id) || !nzchar(as.character(player_id))) return(NULL)
  if (is.null(players_df) || !is.data.frame(players_df) || nrow(players_df) == 0) return(NULL)
  if (!("id" %in% colnames(players_df))) return(NULL)
  idx <- which(as.character(players_df$id) == as.character(player_id))
  if (length(idx) == 0) return(NULL)
  players_df[idx[1], , drop = FALSE]
}

# ---- Pure helper: action-aware player resolution ----
# For a "market_bid" action the player must resolve from the CURRENT FILTERED
# MARKET CANDIDATES only (market_candidates_RV), so a stale / non-listed /
# hidden-owner player can never open a market offer from the feed. For a
# "clause_buyout" action the player must resolve from the CURRENT OPEN CLAUSE
# CANDIDATES only (clause_candidates_RV), so a stale / locked clause can never
# open a buyout from the feed. For any other action (e.g. "view") the player
# resolves from the combined all-players data (all_players_RV). Returns a
# 1-row data frame or NULL. Eligibility is decided purely by data presence
# (the immutable id being present in the candidate rows), never by
# visual/heuristic market fields.
today_resolve_player_for_action <- function(player_id, action, market_df, all_df, clause_df = NULL) {
  act <- today_normalize_action(action)
  if (act == "market_bid") {
    return(today_resolve_player(player_id, market_df))
  }
  if (act == "clause_buyout") {
    return(today_resolve_player(player_id, clause_df))
  }
  return(today_resolve_player(player_id, all_df))
}

# ---- Pure helper: classify a player row's owner class (immutable IDs only) ----
# Returns one of:
#   "unknown" -- the resolved immutable owner ID equals current_team_id (own-team
#                rows are excluded BEFORE any computer/system classification), or
#                the owner id is missing/NA/empty and computer is not strictly TRUE
#   "system"  -- `computer` is an explicit scalar logical TRUE (strict isTRUE:
#                numeric 1, character values (including "true"), NA, FALSE, and
#                malformed/multiple values are NOT system; no coercion is called)
#   "rival"   -- the immutable owner ID (resolved `owner_team_id`, else
#                `user_team_id`) is non-empty and != current_team_id
# Team names (userTeam, teamname, user, ...) are NEVER used for classification:
# a row with only a name and no resolvable immutable ID is "unknown".
today_classify_owner <- function(player_row, current_team_id) {
  if (is.null(player_row)) return("unknown")
  if (is.data.frame(player_row)) {
    if (nrow(player_row) == 0) return("unknown")
    player_row <- player_row[1, , drop = FALSE]
  }

  # 1) Resolve the immutable owner ID FIRST: a row whose owner is the current
  #    team is "unknown" (excluded) before the computer/system classification,
  #    so computer = TRUE + own-team owner id can never be a candidate.
  owner_id <- NULL
  if ("owner_team_id" %in% names(player_row)) owner_id <- player_row[["owner_team_id"]]
  if (is.null(owner_id) || is.na(owner_id) || trimws(as.character(owner_id)) == "") {
    if ("user_team_id" %in% names(player_row)) owner_id <- player_row[["user_team_id"]]
  }
  owner_chr <- if (is.null(owner_id) || is.na(owner_id)) "" else trimws(as.character(owner_id))

  cur <- if (is.null(current_team_id) || is.na(current_team_id)) "" else trimws(as.character(current_team_id))
  if (owner_chr != "" && owner_chr == cur) return("unknown")

  # 2) Explicit scalar logical TRUE -> system. isTRUE() performs no coercion,
  #    so numeric 1 / "true" / NA / FALSE / multi-value values are not system.
  computer <- if ("computer" %in% names(player_row)) player_row[["computer"]] else NULL
  if (isTRUE(computer)) return("system")

  # 3) Resolved owner id != current team -> rival; otherwise unknown.
  if (owner_chr == "") return("unknown")
  "rival"
}

# ---- Pure helper: resolve immutable owner team IDs for market rows ----
# The market endpoint carries the owner only as a name (`userTeam`). This
# helper resolves the immutable owner ID into an `owner_team_id` column:
#   1. a non-empty immutable `user_team_id` column is kept as-is;
#   2. otherwise, when `teams_df` is supplied, the `userTeam` name is resolved
#      to the immutable team ID via the teams table (name -> teamid join),
#      requiring a UNIQUE name-to-ID match: a name mapping to multiple
#      distinct team IDs is ambiguous and fails closed (stays NA); repeated
#      rows with the SAME team ID are not ambiguous;
#   3. unresolvable rows get NA.
# The name is used ONLY for this ID resolution (a normalization step for the
# optional rival listings); owner classification (today_classify_owner) never
# reads names, and a displayed name alone is never ownership evidence.
today_resolve_market_owner_ids <- function(mkt_df, teams_df = NULL) {
  if (is.null(mkt_df) || !is.data.frame(mkt_df)) return(mkt_df)
  n <- nrow(mkt_df)
  if (n == 0) {
    mkt_df$owner_team_id <- character(0)
    return(mkt_df)
  }

  ids <- rep(NA_character_, n)
  if ("user_team_id" %in% colnames(mkt_df)) {
    direct <- as.character(mkt_df$user_team_id)
    ok <- !is.na(direct) & trimws(direct) != ""
    ids[ok] <- trimws(direct[ok])
  }

  if (!is.null(teams_df) && is.data.frame(teams_df) && nrow(teams_df) > 0) {
    tid_col <- if ("teamid" %in% colnames(teams_df)) "teamid" else if ("id" %in% colnames(teams_df)) "id" else NULL
    name_col <- if ("teamname" %in% colnames(teams_df)) "teamname" else if ("name" %in% colnames(teams_df)) "name" else NULL
      if (!is.null(tid_col) && !is.null(name_col) && "userTeam" %in% colnames(mkt_df)) {
        names_v <- as.character(teams_df[[name_col]])
        tids_v <- as.character(teams_df[[tid_col]])
        keep <- !is.na(names_v) & trimws(names_v) != "" & !is.na(tids_v) & trimws(tids_v) != ""
        if (any(keep)) {
          names_k <- trimws(names_v[keep])
          tids_k <- trimws(tids_v[keep])
          # Fail closed on ambiguity: a name is resolvable only when it maps
          # to exactly ONE distinct immutable team ID. A name mapping to
          # multiple distinct IDs is ambiguous and is left unresolved (NA).
          id_counts <- tapply(tids_k, names_k, function(x) length(unique(x)))
          unique_names <- names(id_counts[id_counts == 1])
          lookup <- stats::setNames(
            tids_k[names_k %in% unique_names],
            names_k[names_k %in% unique_names]
          )
          lookup <- lookup[!duplicated(names(lookup))]  # first match wins
          row_names <- trimws(as.character(mkt_df$userTeam))
          matched <- lookup[row_names]
          fill <- is.na(ids) & !is.na(matched)
          ids[fill] <- matched[fill]
        }
      }
  }

  mkt_df$owner_team_id <- ids
  mkt_df
}

# ---- Pure helper: default/opt-in market candidate filtering ----
# Default (include_rival = FALSE): keeps ONLY "system" rows (computer is an
# explicit scalar logical TRUE) -- explicit Futmondo/system market listings.
# Rival-owned and unknown-owner listings are hidden by default.
# Opt-in (include_rival = TRUE): keeps "system" + "rival" rows (rival =
# resolvable immutable owner id != current team). "unknown" rows are ALWAYS
# excluded, in both modes -- including rows whose owner id equals the current
# team (they are "unknown" before the system classification applies).
# Malformed input (NULL / not a data frame / zero rows) yields an empty
# (0-row) data frame -- never an error.
today_filter_market_candidates <- function(mkt_df, current_team_id, include_rival = FALSE, teams_df = NULL) {
  empty <- data.frame()
  if (is.null(mkt_df) || !is.data.frame(mkt_df) || nrow(mkt_df) == 0) return(empty)

  df <- today_resolve_market_owner_ids(mkt_df, teams_df)
  keep <- vapply(seq_len(nrow(df)), function(i) {
    cls <- today_classify_owner(df[i, , drop = FALSE], current_team_id)
    cls == "system" || (isTRUE(include_rival) && cls == "rival")
  }, logical(1))
  df[keep, , drop = FALSE]
}

# ---- Pure helper: strict open release-clause check ----
# A clause is OPEN for buyout only when ALL of the following hold:
#   1. `clause_price` is finite and > 0;
#   2. `clause_transferred` is EXPLICITLY FALSE (NA / missing / TRUE -> not open);
#   3. `clause_date` is present, parseable, and <= `now`.
# Fails closed: any missing/ambiguous field makes the clause NOT open.
today_is_clause_open <- function(player_row, now = Sys.time()) {
  if (is.null(player_row)) return(FALSE)
  if (is.data.frame(player_row)) {
    if (nrow(player_row) == 0) return(FALSE)
    player_row <- player_row[1, , drop = FALSE]
  }

  cp <- if ("clause_price" %in% names(player_row)) suppressWarnings(as.numeric(player_row[["clause_price"]])) else NA_real_
  if (!is.finite(cp) || cp <= 0) return(FALSE)

  tr <- if ("clause_transferred" %in% names(player_row)) player_row[["clause_transferred"]] else NULL
  if (is.null(tr) || is.na(tr)) return(FALSE)
  if (!is.logical(tr)) tr <- suppressWarnings(as.logical(tr))
  if (is.na(tr) || isTRUE(tr)) return(FALSE)

  cd <- if ("clause_date" %in% names(player_row)) player_row[["clause_date"]] else NULL
  if (is.null(cd) || is.na(cd) || !nzchar(trimws(as.character(cd)))) return(FALSE)
  cd_chr <- trimws(as.character(cd))
  # Parse defensively: ISO-8601 with T/Z first, then a plain "YYYY-mm-dd
  # HH:MM:SS" fallback. Unparseable dates (error or NA) -> not open.
  dt <- tryCatch(
    suppressWarnings(as.POSIXct(cd_chr, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")),
    error = function(e) as.POSIXct(NA)
  )
  if (is.na(dt)) {
    dt <- tryCatch(
      suppressWarnings(as.POSIXct(gsub("Z", "", gsub("T", " ", cd_chr)), tz = "UTC")),
      error = function(e) as.POSIXct(NA)
    )
  }
  if (is.na(dt)) return(FALSE)
  dt <= now
}

# ---- Pure helper: strict rival clause candidate filtering ----
# From a combined rival-roster data frame, keeps only rows that:
#   - belong to a rival (immutable `user_team_id` non-empty and != current
#     team; the current team is always excluded), and
#   - have a strict open release clause (today_is_clause_open).
# Malformed input (NULL / not a data frame / zero rows / no id column) yields
# an empty (0-row) data frame -- never an error.
today_filter_clause_candidates <- function(rival_roster_df, current_team_id, now = Sys.time()) {
  empty <- data.frame()
  if (is.null(rival_roster_df) || !is.data.frame(rival_roster_df) || nrow(rival_roster_df) == 0) return(empty)
  if (!("id" %in% colnames(rival_roster_df))) return(empty)

  cur <- if (is.null(current_team_id) || is.na(current_team_id)) "" else trimws(as.character(current_team_id))
  keep <- vapply(seq_len(nrow(rival_roster_df)), function(i) {
    row <- rival_roster_df[i, , drop = FALSE]
    owner_id <- if ("user_team_id" %in% names(row)) row[["user_team_id"]] else NULL
    owner_chr <- if (is.null(owner_id) || is.na(owner_id)) "" else trimws(as.character(owner_id))
    if (owner_chr == "") return(FALSE)
    if (owner_chr == cur) return(FALSE)  # exclude the current team
    today_is_clause_open(row, now = now)
  }, logical(1))
  rival_roster_df[keep, , drop = FALSE]
}


# ============================================================
# today_UI
# ============================================================
today_UI <- function(id) {
  ns <- NS(id)
  tagList(
    # ---- Hero Banner ----
    div(
      style = "background: linear-gradient(135deg, #1e3a5f 0%, #0f2027 100%); color: #fff; padding: 28px 24px; border-radius: 10px; margin-bottom: 20px;",
      fluidRow(
        column(
          width = 12,
          div(
            style = "display: flex; align-items: center; gap: 14px; flex-wrap: wrap;",
            icon("bolt", style = "font-size: 28px; color: #fbbf24;"),
            div(
              style = "flex: 1;",
              h2(
                style = "margin: 0; font-weight: 700; font-size: 22px;",
                "Manager Command Center"
              ),
              p(
                id = ns("today_date_subtitle"),
                style = "margin: 4px 0 0 0; font-size: 13px; color: #94a3b8;",
                "Your daily intelligence briefing"
              )
            )
          )
        )
      )
    ),

    # ---- KPI Value Boxes ----
    fluidRow(
      column(width = 3, uiOutput(ns("kpi_cash_box"))),
      column(width = 3, uiOutput(ns("kpi_valuation_box"))),
      column(width = 3, uiOutput(ns("kpi_opportunities_box"))),
      column(width = 3, uiOutput(ns("kpi_threats_box")))
    ),

    # ---- Main 2-Column Layout ----
    fluidRow(
      # Left Column: Recommendations Feed
      column(
        width = 8,
        box(
          title = tagList(icon("lightbulb"), " What Should I Do Today? (Actionable Manager Feed)"),
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          collapsible = FALSE,
           # Opt-in toggle: by default only explicit Futmondo/system market
           # listings (computer is an explicit scalar logical TRUE) feed the
           # Buy/Place Bid recommendations; checking this also includes
           # rival-owned market listings (unknown-owner listings are always
           # hidden).
          div(
            style = "margin: 0 0 10px 0;",
            checkboxInput(
              inputId = ns("include_rival_listings"),
              label = "Include rival-owned market listings",
              value = FALSE
            )
          ),
          uiOutput(ns("recommendations_feed_ui"))
        )
      ),

      # Right Column: Market Radar + Recent Deals
      column(
        width = 4,
        # Market Radar Table
        box(
          title = tagList(icon("satellite-dish"), " Today's Market Intelligence Radar"),
          width = 12,
          status = "info",
          solidHeader = TRUE,
          collapsible = FALSE,
          reactableOutput(ns("market_radar_table"))
        ),

        # Recent League Transfers
        box(
          title = tagList(icon("newspaper"), " Recent League Transfers"),
          width = 12,
          status = "warning",
          solidHeader = TRUE,
          collapsible = FALSE,
          uiOutput(ns("recent_deals_ui"))
        )
      )
    )
  )
}


# ============================================================
# today_Server
# ============================================================
today_Server <- function(id, is_module_active, login_token, championship_id,
                         user_team_id, user_teams_RV, refresh_trigger = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      # ---- Reactive: Market Players ----
      market_players_RV <- reactive({
        req(is_module_active() == TRUE)
        req(login_token())
        req(championship_id())
        req(user_team_id())
        if (!is.null(refresh_trigger)) refresh_trigger()

        tryCatch({
          df <- get_market_players(
            login = login_token(),
            championship_id = championship_id(),
            user_team_id = user_team_id()
          )
          df <- df %>% translate_player_positions()
          df <- df %>% calculate_player_changes()
          df <- df %>% unify_columns()
          df <- calculate_fis_score(df)
          df
        }, error = function(e) {
          print(paste0("[Today] Error fetching market players: ", e$message))
          data.frame()
        })
      })

      # ---- Reactive: Squad Players ----
      squad_players_RV <- reactive({
        req(is_module_active() == TRUE)
        req(login_token())
        req(championship_id())
        req(user_team_id())
        if (!is.null(refresh_trigger)) refresh_trigger()

        tryCatch({
          df <- get_players_from_team(
            login = login_token(),
            championship_id = championship_id(),
            user_team_id = user_team_id(),
            teams = NULL
          )
          df <- df %>% translate_player_positions()
          df <- df %>% calculate_player_changes()
          df <- df %>% unify_columns()
          df <- calculate_fis_score(df)
          df
        }, error = function(e) {
          print(paste0("[Today] Error fetching squad players: ", e$message))
          data.frame()
        })
      })

      # ---- Reactive: Pressroom Feed ----
      pressroom_RV <- reactive({
        req(is_module_active() == TRUE)
        req(login_token())
        req(championship_id())
        if (!is.null(refresh_trigger)) refresh_trigger()

        tryCatch({
          df <- get_championship_pressroom(
            login = login_token(),
            championship_id = championship_id()
          )
          df
        }, error = function(e) {
          print(paste0("[Today] Error fetching pressroom: ", e$message))
          data.frame()
        })
      })

      # ---- Reactive: User Finances ----
      user_finances_RV <- reactive({
        req(is_module_active() == TRUE)
        req(login_token())
        req(championship_id())
        req(user_team_id())
        if (!is.null(refresh_trigger)) refresh_trigger()

        tryCatch({
          info <- get_user_team_info(
            login = login_token(),
            championship_id = championship_id(),
            user_team_id = user_team_id()
          )
          info
        }, error = function(e) {
          print(paste0("[Today] Error fetching user finances: ", e$message))
          NULL
        })
      })

      # ---- Reactive: Combined Players (for FIS + recommendations) ----
      all_players_RV <- reactive({
        mkt <- market_players_RV()
        sqd <- squad_players_RV()

        if (is.null(mkt) && is.null(sqd)) return(data.frame())
        if (is.null(mkt) && !is.null(sqd)) return(sqd)
        if (!is.null(mkt) && is.null(sqd)) return(mkt)

        # Combine, deduplicate by id
        combined <- data.table::rbindlist(list(as.data.frame(mkt), as.data.frame(sqd)), fill = TRUE) %>% as.data.frame()
        if (nrow(combined) > 0 && "id" %in% colnames(combined)) {
          combined <- combined %>% dplyr::distinct(id, .keep_all = TRUE)
        }
        # Recalculate FIS on combined set
        if (nrow(combined) > 0) {
          combined <- calculate_fis_score(combined)
        }
        combined
      })

      # ---- Reactive: include rival-owned market listings (opt-in toggle) ----
      # Unchecked by default: Buy/Place Bid recommendations include only
      # explicit Futmondo/system market listings (computer is an explicit
      # scalar logical TRUE).
      include_rival_RV <- reactive({
        req(is_module_active() == TRUE)
        isTRUE(input$include_rival_listings)
      })

      # ---- Reactive: filtered market candidates (default: system only) ----
      # Single source of truth for what a "market_bid" action may resolve
      # against. Default keeps only "system" rows; the opt-in toggle also
      # admits "rival" rows; "unknown" rows are always excluded.
      market_candidates_RV <- reactive({
        req(is_module_active() == TRUE)
        req(user_team_id())
        if (!is.null(refresh_trigger)) refresh_trigger()
        today_filter_market_candidates(
          market_players_RV(),
          current_team_id = user_team_id(),
          include_rival = include_rival_RV(),
          teams_df = user_teams_RV()
        )
      })

      # ---- Reactive: combined rival rosters (cached per-team calls) ----
      # One cached get_players_from_team() call per rival team (all user teams
      # except the current team). Malformed teams data (NULL / not a data
      # frame / no rows / no immutable id column) yields an empty data frame,
      # which in turn yields empty clause candidates -- never an error.
      rival_roster_RV <- reactive({
        req(is_module_active() == TRUE)
        req(login_token())
        req(championship_id())
        req(user_team_id())
        if (!is.null(refresh_trigger)) refresh_trigger()

        teams <- user_teams_RV()
        if (is.null(teams) || !is.data.frame(teams) || nrow(teams) == 0) return(data.frame())
        id_col <- if ("teamid" %in% colnames(teams)) "teamid" else if ("id" %in% colnames(teams)) "id" else NULL
        if (is.null(id_col)) return(data.frame())
        team_ids <- as.character(teams[[id_col]])
        team_ids <- team_ids[!is.na(team_ids) & nzchar(trimws(team_ids))]
        team_ids <- setdiff(team_ids, trimws(as.character(user_team_id())))  # exclude current team
        if (length(team_ids) == 0) return(data.frame())

        rosters <- lapply(team_ids, function(tid) {
          tryCatch({
            get_players_from_team(
              login = login_token(),
              championship_id = championship_id(),
              user_team_id = tid
            )
          }, error = function(e) {
            print(paste0("[Today] Error fetching rival roster for team ", tid, ": ", e$message))
            NULL
          })
        })
        rosters <- Filter(function(df) !is.null(df) && is.data.frame(df) && nrow(df) > 0, rosters)
        if (length(rosters) == 0) return(data.frame())

        combined <- data.table::rbindlist(rosters, fill = TRUE) %>% as.data.frame()
        if ("id" %in% colnames(combined)) {
          combined <- combined %>% dplyr::distinct(id, .keep_all = TRUE)
        }
        combined
      })

      # ---- Reactive: strict open rival clause candidates ----
      # Single source of truth for what a "clause_buyout" action may resolve
      # against: rival-owned players with a strict open release clause
      # (finite positive price, explicitly FALSE transferred, parseable
      # clause_date <= now). The current team is excluded.
      clause_candidates_RV <- reactive({
        req(is_module_active() == TRUE)
        req(user_team_id())
        today_filter_clause_candidates(rival_roster_RV(), user_team_id())
      })

      # ---- Reactive: Recommendations Feed ----
      # The feed is generated EXCLUSIVELY from the candidate reactives for the
      # Buy (market_bid) and Clause (clause_buyout) sections: candidates are
      # always supplied (possibly 0-row), so the legacy players_df-based
      # Buy/Clause behavior is never used by Today.
      recommendations_RV <- reactive({
        all_p <- all_players_RV()
        prs <- pressroom_RV()
        ut <- user_teams_RV()
        mkt_cand <- market_candidates_RV()
        clause_cand <- clause_candidates_RV()

        if ((is.null(all_p) || nrow(all_p) == 0) && nrow(mkt_cand) == 0 && nrow(clause_cand) == 0) {
          return(data.frame(
            type = character(0), title = character(0), description = character(0),
            confidence_pct = numeric(0), action_label = character(0),
            action_code = character(0), player_id = character(0), stringsAsFactors = FALSE
          ))
        }

        tryCatch({
          generate_command_center_feed(
            login = login_token(),
            championship_id = championship_id(),
            user_team_id = user_team_id(),
            user_teams_df = ut,
            players_df = all_p,
            pressroom_df = prs,
            market_candidates = mkt_cand,
            clause_candidates = clause_cand
          )
        }, error = function(e) {
          print(paste0("[Today] Error generating recommendations: ", e$message))
          data.frame(
            type = character(0), title = character(0), description = character(0),
            confidence_pct = numeric(0), action_label = character(0),
            action_code = character(0), player_id = character(0), stringsAsFactors = FALSE
          )
        })
      })

      # ============================================================
      # Renders
      # ============================================================

      # ---- Date Subtitle ----
      output$today_date_subtitle <- renderText({
        req(is_module_active() == TRUE)
        paste0(format(Sys.Date(), "%A, %B %d, %Y"), " | Your daily intelligence briefing")
      })

      # ---- KPI: Available Liquid Cash ----
      output$kpi_cash_box <- renderUI({
        req(is_module_active() == TRUE)

        fin <- user_finances_RV()
        sqd <- squad_players_RV()

        liquid_cash_val <- 300000000
        if (!is.null(fin) && !is.null(fin$budget) && is.numeric(fin$budget) && fin$budget > 0) {
          liquid_cash_val <- fin$budget
        } else if (!is.null(sqd) && nrow(sqd) > 0 && "buyPrice" %in% colnames(sqd)) {
          total_spent <- sum(suppressWarnings(as.numeric(sqd$buyPrice)), na.rm = TRUE)
          liquid_cash_val <- 300000000 - total_spent
        }

        value_text <- format_table_currency(liquid_cash_val)

        div(
          style = "background: linear-gradient(135deg, #059669 0%, #047857 100%); color: #fff; padding: 18px 16px; border-radius: 10px; margin-bottom: 10px;",
          icon("sack-dollar", style = "font-size: 20px; margin-bottom: 8px; color: #a7f3d0;"),
          br(),
          div(
            style = "font-size: 20px; font-weight: 700;",
            value_text
          ),
          div(
            style = "font-size: 11px; color: #a7f3d0; margin-top: 4px; text-transform: uppercase; letter-spacing: 0.5px;",
            "Available Liquid Cash"
          )
        )
      })

      # ---- KPI: Squad Market Valuation ----
      output$kpi_valuation_box <- renderUI({
        req(is_module_active() == TRUE)

        sqd <- squad_players_RV()
        val_sum <- if (!is.null(sqd) && nrow(sqd) > 0 && "value" %in% colnames(sqd)) {
          sum(suppressWarnings(as.numeric(sqd$value)), na.rm = TRUE)
        } else {
          0
        }

        value_text <- format_table_currency(val_sum)

        div(
          style = "background: linear-gradient(135deg, #2563eb 0%, #1d4ed8 100%); color: #fff; padding: 18px 16px; border-radius: 10px; margin-bottom: 10px;",
          icon("chart-line", style = "font-size: 20px; margin-bottom: 8px; color: #bfdbfe;"),
          br(),
          div(
            style = "font-size: 20px; font-weight: 700;",
            value_text
          ),
          div(
            style = "font-size: 11px; color: #bfdbfe; margin-top: 4px; text-transform: uppercase; letter-spacing: 0.5px;",
            "Squad Market Valuation"
          )
        )
      })

      # ---- KPI: Active Market Opportunities ----
      output$kpi_opportunities_box <- renderUI({
        req(is_module_active() == TRUE)

        mkt <- market_players_RV()
        high_fis_count <- 0
        if (!is.null(mkt) && nrow(mkt) > 0 && "fis_tier" %in% colnames(mkt)) {
          high_fis_count <- sum(mkt$fis_tier %in% c("Strong Buy", "Buy"), na.rm = TRUE)
        }

        div(
          style = "background: linear-gradient(135deg, #d97706 0%, #b45309 100%); color: #fff; padding: 18px 16px; border-radius: 10px; margin-bottom: 10px;",
          icon("magnifying-glass-chart", style = "font-size: 20px; margin-bottom: 8px; color: #fde68a;"),
          br(),
          div(
            style = "font-size: 20px; font-weight: 700;",
            high_fis_count
          ),
          div(
            style = "font-size: 11px; color: #fde68a; margin-top: 4px; text-transform: uppercase; letter-spacing: 0.5px;",
            "Active Market Opportunities"
          )
        )
      })

      # ---- KPI: Clause Threat Radar ----
      output$kpi_threats_box <- renderUI({
        req(is_module_active() == TRUE)

        sqd <- squad_players_RV()
        threats_count <- 0
        if (!is.null(sqd) && nrow(sqd) > 0) {
          # Count starters (in dream team or high points) with vulnerable clauses
          # A clause is "vulnerable" if clause_price exists and is relatively low vs value
          if ("clause_price" %in% colnames(sqd) && "value" %in% colnames(sqd)) {
            for (i in seq_len(nrow(sqd))) {
              cp <- suppressWarnings(as.numeric(sqd$clause_price[i]))
              v  <- suppressWarnings(as.numeric(sqd$value[i]))
              pts <- suppressWarnings(as.numeric(sqd$points[i]))
              # Vulnerable: clause exists, clause < 80% of value, and player has decent points
              if (!is.na(cp) && cp > 0 && !is.na(v) && v > 0 && cp < v * 0.8 && !is.na(pts) && pts > 0) {
                threats_count <- threats_count + 1
              }
            }
          }
        }

        status_color <- if (threats_count > 2) "#ef4444" else if (threats_count > 0) "#f59e0b" else "#10b981"

        div(
          style = paste0("background: linear-gradient(135deg, ", status_color, " 0%, ", status_color, " 100%); color: #fff; padding: 18px 16px; border-radius: 10px; margin-bottom: 10px;"),
          icon("shield-halved", style = "font-size: 20px; margin-bottom: 8px; color: rgba(255,255,255,0.7);"),
          br(),
          div(
            style = "font-size: 20px; font-weight: 700;",
            threats_count
          ),
          div(
            style = "font-size: 11px; color: rgba(255,255,255,0.8); margin-top: 4px; text-transform: uppercase; letter-spacing: 0.5px;",
            "Clause Threat Radar"
          )
        )
      })

      # ---- Recommendations Feed ----
      output$recommendations_feed_ui <- renderUI({
        req(is_module_active() == TRUE)
        recs <- recommendations_RV()

        if (is.null(recs) || nrow(recs) == 0) {
          return(
            div(
              style = "padding: 24px; text-align: center; color: #64748b;",
              icon("circle-info", style = "font-size: 24px; margin-bottom: 8px;"),
              br(),
              p("No actionable recommendations at this time. Check back later or refresh data.")
            )
          )
        }

        cards <- lapply(seq_len(nrow(recs)), function(i) {
          r <- recs[i, ]
          rec_type <- if (!is.null(r$type) && !is.na(r$type)) as.character(r$type) else "Hold"
          title_text <- if (!is.null(r$title) && !is.na(r$title)) as.character(r$title) else "Recommendation"
          desc_text <- if (!is.null(r$description) && !is.na(r$description)) as.character(r$description) else ""
          conf_raw <- suppressWarnings(as.numeric(r$confidence_pct))
          conf_pct <- if (!is.null(conf_raw) && !is.na(conf_raw)) round(conf_raw, 0) else 50
          action_label <- if (!is.null(r$action_label) && !is.na(r$action_label)) as.character(r$action_label) else "View"
          pid <- if (!is.null(r$player_id) && !is.na(r$player_id)) as.character(r$player_id) else ""
          # The feed emits a stable action_code ("market_bid" / "clause_buyout"
          # / "view"); the button sends that code directly (the observer
          # normalizes it, and stable codes pass through unchanged). Falls back
          # to label normalization only when the column is absent.
          action_code_raw <- if (!is.null(r$action_code) && !is.na(r$action_code)) as.character(r$action_code) else ""
          action_code <- if (nzchar(action_code_raw)) action_code_raw else today_normalize_action(action_label)

          # Color coding by type
          type_icon <- switch(
            rec_type,
            "Buy"    = icon("arrow-down", style = "color: #10b981;"),
            "Sell"   = icon("arrow-up", style = "color: #ef4444;"),
            "Bid"    = icon("hand-holding-dollar", style = "color: #f59e0b;"),
            "Clause" = icon("bolt", style = "color: #8b5cf6;"),
            "Hold"   = icon("hand", style = "color: #6b7280;"),
            icon("circle-info", style = "color: #6b7280;")
          )

          type_badge_color <- switch(
            rec_type,
            "Buy"    = "background-color: #d1fae5; color: #065f46; border-color: #a7f3d0;",
            "Sell"   = "background-color: #fee2e2; color: #991b1b; border-color: #fca5a5;",
            "Bid"    = "background-color: #fef3c7; color: #92400e; border-color: #fde68a;",
            "Clause" = "background-color: #ede9fe; color: #5b21b6; border-color: #c4b5fd;",
            "Hold"   = "background-color: #f3f4f6; color: #374151; border-color: #d1d5db;",
            "background-color: #f3f4f6; color: #374151; border-color: #d1d5db;"
          )

          conf_color <- if (conf_pct >= 80) "#10b981" else if (conf_pct >= 60) "#f59e0b" else "#ef4444"

          # Determine if action button should be shown
          show_action_btn <- rec_type %in% c("Buy", "Bid", "Clause")

          div(
            style = "border: 1px solid #e2e8f0; border-radius: 10px; padding: 16px; margin-bottom: 12px; background: #fff;",
            fluidRow(
              # Icon + Title
              column(
                width = 10,
                div(
                  style = "display: flex; align-items: center; gap: 10px;",
                  div(style = "font-size: 18px;", type_icon),
                  div(
                    style = "flex: 1;",
                    div(
                      style = "font-weight: 700; font-size: 14px; color: #0f172a;",
                      title_text
                    ),
                    div(
                      style = "font-size: 12px; color: #64748b; margin-top: 4px;",
                      desc_text
                    )
                  )
                )
              ),
              # Confidence badge
              column(
                width = 2,
                div(
                  style = paste0("text-align: right; display: flex; align-items: center; justify-content: flex-end;"),
                  div(
                    style = paste0("display: inline-block; padding: 4px 10px; border-radius: 20px; font-size: 11px; font-weight: 700; background-color: ", if (conf_pct >= 80) "#d1fae5" else if (conf_pct >= 60) "#fef3c7" else "#fee2e2", "; color: ", conf_color, ";"),
                    paste0("Confidence: ", conf_pct, "%")
                  )
                )
              )
            ),
            # Type badge + Action button row
            fluidRow(
              column(
                width = 12,
                div(
                  style = "margin-top: 10px; display: flex; align-items: center; justify-content: space-between; flex-wrap: wrap; gap: 8px;",
                  div(
                    style = paste0("display: inline-block; padding: 4px 12px; border-radius: 6px; font-size: 11px; font-weight: 600; border: 1px solid; ", type_badge_color),
                    rec_type
                  ),
                    if (show_action_btn) {
                      actionButton(
                        inputId = ns(paste0("rec_action_", pid)),
                        label = tagList(icon("arrow-right"), action_label),
                        class = "btn btn-sm btn-primary",
                        style = "font-size: 11px; padding: 4px 12px;",
                        onclick = today_rec_action_onclick_js(ns, pid, action_code)
                      )
                    } else {
                      NULL
                    }
                )
              )
            )
          )
        })

        do.call(tagList, cards)
      })

      # ---- Market Radar Table ----
      output$market_radar_table <- renderReactable({
        req(is_module_active() == TRUE)
        mkt <- market_players_RV()

        # Single source of truth: coerce/filter non-finite FIS, sort, top 10.
        display_df <- today_prepare_radar_df(mkt, top_n = 10)

        if (nrow(display_df) == 0) {
          return(
            reactable(
              data.frame(
                Player = character(0),
                Role = character(0),
                Price = numeric(0),
                FIS = numeric(0),
                Tier = character(0)
              ),
 outlined = FALSE,
                bordered = FALSE,
                compact = TRUE
            )
          )
        }

        reactable(
          display_df,
 outlined = FALSE,
            bordered = FALSE,
            compact = TRUE,
            highlight = TRUE,
            onClick = today_radar_onclick_js(ns),
          columns = list(
            Player = colDef(
              name = "Player",
              minWidth = 100,
              cell = function(value) {
                div(
                  style = "font-weight: 600; font-size: 12px; color: #0f172a;",
                  value
                )
              }
            ),
            Role = colDef(
              name = "Role",
              minWidth = 60,
              cell = function(value) {
                div(
                  style = "font-size: 11px; color: #64748b;",
                  value
                )
              }
            ),
            Price = colDef(
              name = "Price",
              align = "right",
              minWidth = 90,
              cell = function(value) {
                div(
                  style = "font-size: 12px; font-weight: 600; color: #059669;",
                  format_table_currency(value)
                )
              }
            ),
            FIS = colDef(
              name = "FIS",
              align = "center",
              minWidth = 55,
              cell = function(value) {
                v <- suppressWarnings(as.numeric(value))
                if (is.na(v) || !is.finite(v)) v <- 0
                badge_color <- if (v >= 80) "#10b981" else if (v >= 65) "#f59e0b" else "#6b7280"
                div(
                  style = paste0("display: inline-block; padding: 2px 8px; border-radius: 10px; font-size: 11px; font-weight: 700; background-color: ", if (v >= 80) "#d1fae5" else if (v >= 65) "#fef3c7" else "#f3f4f6", "; color: ", badge_color, ";"),
                  v
                )
              }
            ),
            Tier = colDef(
              name = "Tier",
              minWidth = 75,
              cell = function(value) {
                tier_color <- switch(
                  as.character(value),
                  "Strong Buy" = "#10b981",
                  "Buy" = "#f59e0b",
                  "Hold" = "#6b7280",
                  "Sell" = "#ef4444",
                  "#6b7280"
                )
                div(
                  style = paste0("font-size: 10px; font-weight: 600; color: ", tier_color, "; text-transform: uppercase;"),
                  value
                )
              }
            ),
            PlayerID = colDef(show = FALSE)
          )
        )
      })

      # ---- Recent League Transfers ----
      output$recent_deals_ui <- renderUI({
        req(is_module_active() == TRUE)
        prs <- pressroom_RV()

        if (is.null(prs) || nrow(prs) == 0) {
          return(
            div(
              style = "padding: 16px; text-align: center; color: #64748b; font-size: 12px;",
              icon("newspaper", style = "font-size: 16px; margin-bottom: 4px;"),
              br(),
              "No recent transfer data available."
            )
          )
        }

        # Take the most recent 6 high-impact deals
        recent <- head(prs, 6)

        deals <- lapply(seq_len(nrow(recent)), function(i) {
          d <- recent[i, ]

          player_name <- if (!is.null(d$player_name) && nzchar(as.character(d$player_name))) as.character(d$player_name) else "Unknown Player"
          buyer_name <- if (!is.null(d$buyer_team_name) && nzchar(as.character(d$buyer_team_name))) as.character(d$buyer_team_name) else "Futmondo / Mercado"
          seller_name <- if (!is.null(d$seller_team_name) && nzchar(as.character(d$seller_team_name))) as.character(d$seller_team_name) else "Futmondo / Mercado"
          price_val <- suppressWarnings(as.numeric(d$price))
          created_str <- if (!is.null(d$created) && nzchar(as.character(d$created))) as.character(d$created) else ""

          # Parse date for display
          display_date <- ""
          if (created_str != "") {
            parsed_dt <- suppressWarnings(as.POSIXct(created_str, tz = "UTC"))
            if (!is.na(parsed_dt)) {
              display_date <- format(parsed_dt, "%d-%m-%Y %H:%M")
            } else {
              display_date <- created_str
            }
          }

          price_display <- if (!is.na(price_val) && price_val > 0) format_table_currency(price_val) else "-"

          div(
            style = "border-bottom: 1px solid #f1f5f9; padding: 10px 0;",
            div(
              style = "display: flex; align-items: center; gap: 8px;",
              icon("exchange-alt", style = "font-size: 12px; color: #6b7280;"),
              div(
                style = "flex: 1; font-size: 12px;",
                div(
                  style = "font-weight: 600; color: #0f172a;",
                  player_name
                ),
                div(
                  style = "font-size: 11px; color: #64748b;",
                  paste0(buyer_name, " <- ", seller_name)
                )
              ),
              div(
                style = "text-align: right;",
                div(
                  style = "font-weight: 700; font-size: 12px; color: #059669;",
                  price_display
                ),
                div(
                  style = "font-size: 10px; color: #94a3b8;",
                  display_date
                )
              )
            )
          )
        })

        do.call(tagList, deals)
      })

      # ---- Selection events (browser JS -> namespaced inputs) ----
      # Both the market radar row click and the recommendation action buttons
      # produce a selection event: list(player_id = <string>, action = <stable
      # action code>). The event is stored in selected_from_today_RV so the
      # mapped intent (e.g. "market_bid" for a "Place Bid" click) is preserved
      # end-to-end.
      selected_from_today_RV <- reactiveVal(NULL)

      # ---- Market Radar row selection (browser JS -> namespaced input) ----
      # The reactable onClick (today_radar_onclick_js) sends the clicked row's
      # PlayerID to input$radar_selected_player. Radar selection is a plain
      # "view" action.
      observeEvent(input$radar_selected_player, {
        req(is_module_active() == TRUE)
        pid <- input$radar_selected_player
        if (!is.null(pid) && nzchar(as.character(pid))) {
          selected_from_today_RV(list(player_id = as.character(pid), action = "view"))
        }
      }, ignoreNULL = TRUE)

      # ---- Recommendation action buttons (browser JS -> namespaced input) ----
      # Each action button's onclick (today_rec_action_onclick_js) sends
      # {player_id, action} to input$rec_action_clicked. A single observer
      # handles all dynamically-created buttons reliably (no per-button
      # observers, no input-existence false positives). The feed emits a
      # stable action_code ("market_bid" / "clause_buyout" / "view") that the
      # button sends directly; today_normalize_action is the single place
      # where label matching happens (stable codes pass through unchanged,
      # legacy labels are mapped -- no intent inferred from button presence).
      observeEvent(input$rec_action_clicked, {
        req(is_module_active() == TRUE)
        sel <- input$rec_action_clicked
        if (is.null(sel)) return()
        pid <- if (is.list(sel)) sel$player_id else sel
        act <- if (is.list(sel)) sel$action else NULL
        if (!is.null(pid) && nzchar(as.character(pid))) {
          selected_from_today_RV(list(
            player_id = as.character(pid),
            action = today_normalize_action(act)
          ))
        }
      }, ignoreNULL = TRUE)

      # ---- Resolve the selected player by immutable id, action-aware ----
      # A "market_bid" event resolves ONLY from the CURRENT FILTERED MARKET
      # CANDIDATES (market_candidates_RV: system listings by default, rival
      # listings when opted in, unknown-owner always hidden), so a stale /
      # non-listed / hidden-owner player can never open a market offer from
      # the feed. A "clause_buyout" event resolves ONLY from the CURRENT OPEN
      # CLAUSE CANDIDATES (clause_candidates_RV), so a stale / locked clause
      # can never open a buyout from the feed. Any other action resolves from
      # all_players_RV(). Yields the one-row player data frame for the current
      # event, or NULL when the event is absent or its id is unknown/stale
      # (or not present in the candidate set for the action).
      selected_today_player_RV <- reactive({
        ev <- selected_from_today_RV()
        if (is.null(ev)) return(NULL)
        today_resolve_player_for_action(
          ev$player_id,
          ev$action,
          market_candidates_RV(),
          all_players_RV(),
          clause_df = clause_candidates_RV()
        )
      })

      # ---- Stable action code for the nested selected_player module ----
      today_open_action_RV <- reactive({
        ev <- selected_from_today_RV()
        if (is.null(ev) || is.null(ev$action)) return(NULL)
        as.character(ev$action)
      })

      # ---- Show the Today-local selected player modal when the event is valid ----
      # Consumes the SAME action-aware resolved reactive (selected_today_player_RV)
      # -- it does NOT re-resolve against all_players_RV() here, which would let an
      # owned / non-listed player open a market offer from the feed. When the id is
      # absent from the appropriate candidate set for the action, a clear warning is
      # shown and NO modal (player, market-offer, or clause-buyout) is opened.
      observeEvent(selected_from_today_RV(), {
        ev <- selected_from_today_RV()
        if (is.null(ev)) return()
        pid <- if (!is.null(ev$player_id)) as.character(ev$player_id) else ""
        if (!nzchar(pid)) return()

        resolved <- selected_today_player_RV()
        if (is.null(resolved)) {
          # Stale / unknown input is rejected: no modal (player or
          # market-offer or clause-buyout) is opened.
          act <- today_normalize_action(ev$action)
          if (act == "market_bid") {
            shiny::showNotification(
              "This player is not currently listed on the market (the market data may be stale or still loading). Please refresh and try again.",
              type = "warning", duration = 5
            )
          } else if (act == "clause_buyout") {
            shiny::showNotification(
              "This player's release clause is no longer available (the clause data may be stale or still loading). Please refresh and try again.",
              type = "warning", duration = 5
            )
          } else {
            shiny::showNotification(
              "Selected player could not be found in the current data. Please refresh and try again.",
              type = "warning", duration = 5
            )
          }
          return()
        }

        showModal(modalDialog(
          selected_player_UI(id = ns("selected_player")),
          footer = div(style = "text-align: center; width: 100%;", modalButton("Close")),
          easyClose = TRUE,
          size = "l"
        ))
      }, ignoreNULL = TRUE)

      # ---- Nested selected_player module (Today-local) ----
      # The resolved one-row player drives the module. on_bid_updated only
      # invalidates/recomputes Today data (cache refresh trigger); Today itself
      # never performs writes. open_action carries the stable action code so
      # the module can open its existing market-offer modal for "market_bid"
      # events (Today's "Place Bid" recommendation) and its existing
      # clause-buyout confirmation modal for "clause_buyout" events (Today's
      # "Exercise Clause" recommendation; clause_price is executed, never a
      # comparison price).
      selected_player_Server(
        id = "selected_player",
        selected_player = selected_today_player_RV,
        login_token = login_token,
        championship_id = championship_id,
        user_team_id = user_team_id,
        on_bid_updated = function(...) {
          if (!is.null(refresh_trigger)) {
            tryCatch(refresh_trigger(refresh_trigger() + 1), error = function(e) NULL)
          }
        },
        open_action = today_open_action_RV
      )

      return(selected_from_today_RV)
    }
  )
}