library(httr)
library(jsonlite)
library(dplyr)

# Retrieve Config variables from environment
SB_URL <- Sys.getenv("supabase_project_url")
SB_KEY <- Sys.getenv("supabase_secret_key")

# Dynamic retrieval helpers -- re-check the environment each call so that
# a later readRenviron() in global.R is respected.
get_sb_url <- function() {
  url <- Sys.getenv("supabase_project_url")
  if (is.null(url) || url == "") SB_URL else url
}
get_sb_key <- function() {
  key <- Sys.getenv("supabase_secret_key")
  if (is.null(key) || key == "") SB_KEY else key
}

supabase_post <- function(table_name, payload) {
  # Defensive check for loaded credentials
  sb_url <- get_sb_url()
  sb_key <- get_sb_key()
  if (is.null(sb_url) || sb_url == "" || is.null(sb_key) || sb_key == "") {
    print("[Supabase] Skipping sync: Supabase environment credentials are not loaded in .Renviron.")
    return(NULL)
  }
  
  if (is.null(payload) || (is.data.frame(payload) && nrow(payload) == 0) || (is.list(payload) && length(payload) == 0)) {
    return(NULL)
  }
  
  url <- paste0(sb_url, "/rest/v1/", table_name)
  
  headers <- c(
    "apikey" = sb_key,
    "Authorization" = paste("Bearer", sb_key),
    "Content-Type" = "application/json",
    "Prefer" = "resolution=merge-duplicates" # Upsert on PK matching
  )
  
  # Perform request defensively
  tryCatch({
    response <- POST(url, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
    code <- status_code(response)
    if (code >= 200 && code < 300) {
      print(paste0("[Supabase] Successfully synced data to table: ", table_name, " (HTTP ", code, ")"))
    } else {
      print(paste0("[Supabase] Warning: Received HTTP code ", code, " from table: ", table_name))
    }
    return(code)
  }, error = function(e) {
    print(paste0("[Supabase] Connection error during post to table ", table_name, ": ", e$message))
    return(NULL)
  })
}

sync_championship_to_supabase <- function(championship) {
  if (is.null(championship) || length(championship) == 0) return()
  
  payload <- list(
    id = as.character(championship["id"]),
    name = as.character(championship["name"]),
    mode = as.character(championship["mode"]),
    sport = as.character(championship["sport"])
  )
  
  supabase_post("championships", payload)
}

sync_real_clubs_to_supabase <- function(clubs_df) {
  if (is.null(clubs_df) || nrow(clubs_df) == 0) return()
  
  # Expected columns: teamId, team, logo
  if (!"teamId" %in% colnames(clubs_df)) return()
  
  payload <- clubs_df %>%
    dplyr::select(id = teamId, name = team, logo) %>%
    dplyr::distinct(id, .keep_all = TRUE)
  
  supabase_post("real_clubs", payload)
}

sync_players_to_supabase <- function(players_df) {
  if (is.null(players_df) || nrow(players_df) == 0) return()
  
  # Ensure minimum expected columns are present
  required <- c("id", "name", "slug")
  if (!all(required %in% colnames(players_df))) return()
  
  payload <- data.frame(
    id = as.character(players_df$id),
    name = as.character(players_df$name),
    slug = as.character(players_df$slug),
    role = if ("role" %in% colnames(players_df)) as.character(players_df$role) else NA_character_,
    role2 = if ("role2" %in% colnames(players_df)) as.character(players_df$role2) else NA_character_,
    photo = if ("photo" %in% colnames(players_df)) as.character(players_df$photo) else NA_character_,
    real_club_id = if ("teamId" %in% colnames(players_df)) as.character(players_df$teamId) else NA_character_,
    status = if ("status" %in% colnames(players_df)) as.character(players_df$status) else NA_character_,
    rating = if ("rating" %in% colnames(players_df)) as.integer(players_df$rating) else NA_integer_,
    stringsAsFactors = FALSE
  )
  
  # Dedup
  payload <- payload %>% dplyr::distinct(id, .keep_all = TRUE)
  
  supabase_post("players", payload)
}

sync_user_teams_to_supabase <- function(teams_df, championship_id) {
  if (is.null(teams_df) || nrow(teams_df) == 0) return()
  
  # Standardize column naming
  team_ids <- if ("teamid" %in% colnames(teams_df)) teams_df$teamid else if ("id" %in% colnames(teams_df)) teams_df$id else return()
  team_names <- if ("teamname" %in% colnames(teams_df)) teams_df$teamname else if ("name" %in% colnames(teams_df)) teams_df$name else "Unknown Team"
  
  payload <- data.frame(
    id = as.character(team_ids),
    championship_id = as.character(championship_id),
    name = as.character(team_names),
    budget = if ("budget" %in% colnames(teams_df)) as.numeric(teams_df$budget) else 0,
    points = if ("points" %in% colnames(teams_df)) as.integer(teams_df$points) else 0,
    position = if ("position" %in% colnames(teams_df)) as.integer(teams_df$position) else NA_integer_,
    team_value = if ("team_value" %in% colnames(teams_df)) as.numeric(teams_df$team_value) else if ("teamValue" %in% colnames(teams_df)) as.numeric(teams_df$teamValue) else 0,
    is_active = if ("is_active" %in% colnames(teams_df)) as.logical(teams_df$is_active) else TRUE,
    stringsAsFactors = FALSE
  )

  payload <- payload %>% dplyr::distinct(id, .keep_all = TRUE)

  supabase_post("user_teams", payload)
}

log_user_team_history <- function(teams_df, round_number = NULL) {
  if (is.null(teams_df) || nrow(teams_df) == 0) return()
  
  team_ids <- if ("teamid" %in% colnames(teams_df)) teams_df$teamid else if ("id" %in% colnames(teams_df)) teams_df$id else return()
  active_cnt <- length(unique(team_ids))
  
  payload <- data.frame(
    user_team_id = as.character(team_ids),
    points = if ("points" %in% colnames(teams_df)) as.integer(teams_df$points) else 0,
    budget = if ("budget" %in% colnames(teams_df)) as.numeric(teams_df$budget) else 0,
    position = if ("position" %in% colnames(teams_df)) as.integer(teams_df$position) else NA_integer_,
    team_value = if ("team_value" %in% colnames(teams_df)) as.numeric(teams_df$team_value) else if ("teamValue" %in% colnames(teams_df)) as.numeric(teams_df$teamValue) else 0,
    round_number = if (!is.null(round_number)) as.integer(round_number) else if ("round_number" %in% colnames(teams_df)) as.integer(teams_df$round_number) else NA_integer_,
    active_teams_count = as.integer(active_cnt),
    stringsAsFactors = FALSE
  )

  supabase_post("user_team_history", payload)
}

log_player_history <- function(players_df, championship_id) {
  if (is.null(players_df) || nrow(players_df) == 0) return()
  if (!"id" %in% colnames(players_df)) return()
  
  payload <- data.frame(
    player_id = as.character(players_df$id),
    championship_id = as.character(championship_id),
    value = if ("value" %in% colnames(players_df)) as.numeric(players_df$value) else 0,
    change = if ("change" %in% colnames(players_df)) as.numeric(players_df$change) else 0,
    points = if ("points" %in% colnames(players_df)) as.integer(players_df$points) else 0,
    avg_points = if ("average.average" %in% colnames(players_df)) as.numeric(players_df$average.average) else NA_real_,
    avg_last_five = if ("average.averageLastFive" %in% colnames(players_df)) as.numeric(players_df$average.averageLastFive) else NA_real_,
    matches = if ("average.matches" %in% colnames(players_df)) as.integer(players_df$average.matches) else 0,
    stringsAsFactors = FALSE
  )
  
  supabase_post("player_history", payload)
}

log_market_transaction <- function(player_id, championship_id, buyer_team_id, seller_team_id = NULL, price, is_clause = FALSE) {
  payload <- list(
    player_id = as.character(player_id),
    championship_id = as.character(championship_id),
    buyer_team_id = if (!is.null(buyer_team_id)) as.character(buyer_team_id) else NULL,
    seller_team_id = if (!is.null(seller_team_id)) as.character(seller_team_id) else NULL,
    price = as.numeric(price),
    is_clause = as.logical(is_clause)
  )
  
  supabase_post("market_transactions", payload)
}

supabase_get <- function(table_name, query_params = list()) {
  # Defensive check for loaded credentials
  sb_url <- get_sb_url()
  sb_key <- get_sb_key()
  if (is.null(sb_url) || sb_url == "" || is.null(sb_key) || sb_key == "") {
    return(NULL)
  }
  
  url <- paste0(sb_url, "/rest/v1/", table_name)
  
  headers <- c(
    "apikey" = sb_key,
    "Authorization" = paste("Bearer", sb_key),
    "Accept" = "application/json"
  )
  
  tryCatch({
    response <- GET(url, query = query_params, add_headers(.headers = headers))
    code <- status_code(response)
    if (code >= 200 && code < 300) {
      data <- fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
      return(as.data.frame(data))
    } else {
      print(paste0("[Supabase] Warning: Received GET HTTP code ", code, " from table: ", table_name))
      return(NULL)
    }
  }, error = function(e) {
    print(paste0("[Supabase] GET Connection error for table ", table_name, ": ", e$message))
    return(NULL)
  })
}

get_player_historical_data <- function(player_id, championship_id) {
  query <- list(
    player_id = paste0("eq.", player_id),
    championship_id = paste0("eq.", championship_id),
    select = "value,change,points,recorded_at",
    order = "recorded_at.asc"
  )
  supabase_get("player_history", query)
}

get_league_standings_history <- function(championship_id) {
  query <- list(
    select = "points,budget,position,team_value,recorded_at,user_teams!inner(name,championship_id)",
    "user_teams.championship_id" = paste0("eq.", championship_id),
    order = "recorded_at.asc"
  )
  
  df <- supabase_get("user_team_history", query)
  if (!is.null(df) && nrow(df) > 0 && "user_teams" %in% colnames(df)) {
    if (is.list(df$user_teams) || is.data.frame(df$user_teams)) {
      df$teamname <- df$user_teams$name
    } else {
      df$teamname <- "Unknown Team"
    }
    df$user_teams <- NULL
  }
  return(df)
}

get_user_teams_finances <- function(championship_id) {
  query <- list(
    championship_id = paste0("eq.", championship_id),
    select = "id,name,budget,points,position"
  )
  supabase_get("user_teams", query)
}

get_league_finances_history <- function(championship_id) {
  query <- list(
    select = "budget,team_value,points,position,recorded_at,user_teams!inner(name,championship_id)",
    "user_teams.championship_id" = paste0("eq.", championship_id),
    order = "recorded_at.asc"
  )

  df <- supabase_get("user_team_history", query)
  if (!is.null(df) && nrow(df) > 0 && "user_teams" %in% colnames(df)) {
    if (is.list(df$user_teams) || is.data.frame(df$user_teams)) {
      df$teamname <- df$user_teams$name
    } else {
      df$teamname <- "Unknown Team"
    }
    df$user_teams <- NULL
  }
  return(df)
}

sync_pressroom_transactions_to_supabase <- function(pressroom_df, championship_id) {
  if (is.null(pressroom_df) || nrow(pressroom_df) == 0) return()

  required_cols <- c("player_id", "buyer_team_id", "seller_team_id", "price", "created")
  if (!all(required_cols %in% colnames(pressroom_df))) return()

  payload <- data.frame(
    player_id = as.character(pressroom_df$player_id),
    championship_id = as.character(championship_id),
    buyer_team_id = as.character(pressroom_df$buyer_team_id),
    seller_team_id = as.character(pressroom_df$seller_team_id),
    price = as.numeric(pressroom_df$price),
    created_at = as.character(pressroom_df$created),
    stringsAsFactors = FALSE
  )

  # Deduplicate by player_id + championship_id + buyer_team_id + price to avoid duplicate syncs
  payload <- payload %>% dplyr::distinct(player_id, championship_id, buyer_team_id, seller_team_id, price, created_at, .keep_all = TRUE)

  # Batch large payloads to avoid payload-size limits on the Supabase REST endpoint
  batch_size <- 500
  n <- nrow(payload)
  print(paste0("[Supabase] Syncing ", n, " pressroom transactions to market_transactions (batch size: ", batch_size, ")."))

  tryCatch({
    for (start_idx in seq(1, n, by = batch_size)) {
      end_idx <- min(start_idx + batch_size - 1, n)
      batch <- payload[start_idx:end_idx, , drop = FALSE]
      supabase_post("market_transactions", batch)
    }
  }, error = function(e) {
    print(paste0("[Supabase] Error during pressroom transaction sync: ", e$message))
  })
}

get_pressroom_transactions_from_supabase <- function(championship_id) {
  if (is.null(championship_id) || championship_id == "") return(NULL)

  query <- list(
    championship_id = paste0("eq.", championship_id),
    select = "player_id,buyer_team_id,seller_team_id,price,created_at"
  )

  tryCatch({
    df <- supabase_get("market_transactions", query)
    return(df)
  }, error = function(e) {
    print(paste0("[Supabase] Error fetching pressroom transactions: ", e$message))
    return(NULL)
  })
}

# ============================================================
# Database Reset and Initialization Functions
# ============================================================

supabase_delete <- function(table_name, filter = "id=neq.00000000-0000-0000-0000-000000000000") {
  sb_url <- get_sb_url()
  sb_key <- get_sb_key()
  if (is.null(sb_url) || sb_url == "" || is.null(sb_key) || sb_key == "") {
    return(list(status = "skipped", reason = "credentials not loaded"))
  }

  url <- paste0(sb_url, "/rest/v1/", table_name)

  headers <- c(
    "apikey" = sb_key,
    "Authorization" = paste("Bearer", sb_key),
    "Accept" = "application/json",
    "Prefer" = "return=minimal, count=exact"
  )

  # Parse filter string "col=op.value" into a named list for the query parameter
  filter_parts <- strsplit(filter, "=", fixed = TRUE)[[1]]
  filter_list <- setNames(list(filter_parts[2]), filter_parts[1])

  tryCatch({
    response <- httr::DELETE(url, query = filter_list, add_headers(.headers = headers))
    code <- status_code(response)
    if (code >= 200 && code < 300) {
      return(list(status = "deleted", http_code = code))
    } else {
      return(list(status = "error", http_code = code))
    }
  }, error = function(e) {
    return(list(status = "error", reason = e$message))
  })
}

supabase_delete_all <- function(table_name) {
  bigint_pk_tables <- c("user_team_history", "player_history", "market_transactions")
  text_pk_tables <- c("championships", "real_clubs", "players", "user_teams")

  if (table_name %in% bigint_pk_tables) {
    supabase_delete(table_name, filter = "id=gte.0")
  } else if (table_name %in% text_pk_tables) {
    supabase_delete(table_name, filter = "id=neq.00000000-0000-0000-0000-000000000000")
  } else {
    print(paste0("[Supabase] Unknown PK type for table: ", table_name))
    return(list(status = "error", reason = "unknown PK type"))
  }
}

supabase_reset_database <- function(force = FALSE) {
  if (!force) {
    print("[Supabase] Reset cancelled: set force = TRUE to proceed.")
    return(list())
  }

  reset_order <- c(
    "market_transactions",
    "player_history",
    "user_team_history",
    "user_teams",
    "players",
    "real_clubs",
    "championships"
  )

  results <- list()

  for (tbl in reset_order) {
    cat(paste0("[Reset] Deleting all rows from: ", tbl, " ... "))
    res <- supabase_delete_all(tbl)
    results[[tbl]] <- paste0(res$status,
                              if (!is.null(res$http_code)) paste0(" (HTTP ", res$http_code, ")") else "",
                              if (!is.null(res$reason)) paste0(": ", res$reason) else "")
    cat(results[[tbl]], "\n")
  }

  return(results)
}

# ============================================================
# Row Count Helper
# ============================================================

get_table_row_counts <- function() {
  sb_url <- get_sb_url()
  sb_key <- get_sb_key()
  if (is.null(sb_url) || sb_url == "" || is.null(sb_key) || sb_key == "") {
    warning("[Row Counts] Supabase credentials not loaded in .Renviron. Returning empty result.")
    return(data.frame(table_name = character(), row_count = integer(), stringsAsFactors = FALSE))
  }

  tables <- c(
    "championships",
    "real_clubs",
    "players",
    "user_teams",
    "user_team_history",
    "player_history",
    "market_transactions"
  )

  results <- vector("list", length(tables))
  names(results) <- tables

  for (tbl in tables) {
    url <- paste0(sb_url, "/rest/v1/", tbl)

    headers <- c(
      "apikey" = sb_key,
      "Authorization" = paste("Bearer", sb_key),
      "Accept" = "application/json",
      "Prefer" = "count=exact",
      "Range-Unit" = "items",
      "Range" = "0-0"
    )

    tryCatch({
      response <- GET(url, query = list(select = "id", limit = "0"), add_headers(.headers = headers))
      code <- status_code(response)

      if (code >= 200 && code < 300) {
        content_range <- response$headers[["content-range"]]
        if (!is.null(content_range) && length(content_range) > 0) {
          # content-range looks like "0-0/999" or "items 0-0/999"
          parts <- strsplit(content_range, "/")[[1]]
          total <- as.integer(trimws(parts[length(parts)]))
        } else {
          # Fallback: if no content-range, try counting via a minimal select
          body_text <- httr::content(response, as = "text", encoding = "UTF-8")
          total <- 0L
        }
        results[[tbl]] <- total
      } else {
        results[[tbl]] <- NA_integer_
      }
    }, error = function(e) {
      results[[tbl]] <- NA_integer_
    })
  }

  df <- data.frame(
    table_name = names(results),
    row_count = unlist(results),
    stringsAsFactors = FALSE
  )

  return(df)
}


init_supabase_db <- function(verbose = FALSE) {
  sb_url <- get_sb_url()
  sb_key <- get_sb_key()
  if (is.null(sb_url) || sb_url == "" || is.null(sb_key) || sb_key == "") {
    warning("[Init] Supabase credentials not loaded in .Renviron. Skipping database verification.")
    return(FALSE)
  }

  required_tables <- c(
    "championships",
    "real_clubs",
    "players",
    "user_teams",
    "user_team_history",
    "player_history",
    "market_transactions"
  )

  all_ok <- TRUE

  for (tbl in required_tables) {
    url <- paste0(sb_url, "/rest/v1/", tbl)

    headers <- c(
      "apikey" = sb_key,
      "Authorization" = paste("Bearer", sb_key),
      "Accept" = "application/json"
    )

    tryCatch({
      response <- GET(url, query = list(select = "id", limit = "1"), add_headers(.headers = headers))
      code <- status_code(response)
      if (code == 200) {
        if (verbose) cat(paste0("[Init] Table OK: ", tbl, " (HTTP 200)\n"))
      } else {
        all_ok <- FALSE
        warning(paste0("[Init] Table check failed for '", tbl, "': HTTP ", code))
        if (verbose) cat(paste0("[Init] Table FAIL: ", tbl, " (HTTP ", code, ")\n"))
      }
    }, error = function(e) {
      all_ok <<- FALSE
      warning(paste0("[Init] Table check error for '", tbl, "': ", e$message))
      if (verbose) cat(paste0("[Init] Table ERROR: ", tbl, " (", e$message, ")\n"))
    })
  }

  return(all_ok)
}