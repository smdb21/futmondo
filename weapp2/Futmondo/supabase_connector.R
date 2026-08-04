library(httr)
library(jsonlite)
library(dplyr)

# Retrieve Config variables from environment
SB_URL <- Sys.getenv("supabase_project_url")
SB_KEY <- Sys.getenv("supabase_secret_key")

supabase_post <- function(table_name, payload) {
  # Defensive check for loaded credentials
  if (is.null(SB_URL) || SB_URL == "" || is.null(SB_KEY) || SB_KEY == "") {
    print("[Supabase] Skipping sync: Supabase environment credentials are not loaded in .Renviron.")
    return(NULL)
  }
  
  if (is.null(payload) || (is.data.frame(payload) && nrow(payload) == 0) || (is.list(payload) && length(payload) == 0)) {
    return(NULL)
  }
  
  url <- paste0(SB_URL, "/rest/v1/", table_name)
  
  headers <- c(
    "apikey" = SB_KEY,
    "Authorization" = paste("Bearer", SB_KEY),
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
    team_value = if ("teamValue" %in% colnames(teams_df)) as.numeric(teams_df$teamValue) else 0,
    stringsAsFactors = FALSE
  )

  payload <- payload %>% dplyr::distinct(id, .keep_all = TRUE)

  supabase_post("user_teams", payload)
}

log_user_team_history <- function(teams_df) {
  if (is.null(teams_df) || nrow(teams_df) == 0) return()
  
  team_ids <- if ("teamid" %in% colnames(teams_df)) teams_df$teamid else if ("id" %in% colnames(teams_df)) teams_df$id else return()
  
  payload <- data.frame(
    user_team_id = as.character(team_ids),
    points = if ("points" %in% colnames(teams_df)) as.integer(teams_df$points) else 0,
    budget = if ("budget" %in% colnames(teams_df)) as.numeric(teams_df$budget) else 0,
    position = if ("position" %in% colnames(teams_df)) as.integer(teams_df$position) else NA_integer_,
    team_value = if ("teamValue" %in% colnames(teams_df)) as.numeric(teams_df$teamValue) else 0,
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
  if (is.null(SB_URL) || SB_URL == "" || is.null(SB_KEY) || SB_KEY == "") {
    return(NULL)
  }
  
  url <- paste0(SB_URL, "/rest/v1/", table_name)
  
  headers <- c(
    "apikey" = SB_KEY,
    "Authorization" = paste("Bearer", SB_KEY),
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
    select = "points,budget,position,recorded_at,user_teams!inner(name,championship_id)",
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