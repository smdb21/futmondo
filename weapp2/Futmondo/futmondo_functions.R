LOGIN_URL <- "https://api.futmondo.com/5/login/with_mail"
ACTIVE_CHAMPIONSHIPS_URL <- "https://api.futmondo.com/2/user/activechampionships"
TEAMS_URL <- "https://api.futmondo.com/2/championship/teams"
ROSTER_URL <- "https://api.futmondo.com/1/userteam/roster"
BID_URL <- "https://api.futmondo.com/1/market/bid"
MARKET_URL <- "https://api.futmondo.com/1/market/players"
PLAYER_SUMMARY_URL <- "https://api.futmondo.com/1/player/summary"
MODIFY_BID_URL <- "https://api.futmondo.com/5/market/modifybid"
CANCEL_BID_URL <- "https://api.futmondo.com/1/market/cancelbid"
API_CODE_OK <- "api.general.ok"
PHOTO_URL <- "https://static01.mondocore.com/futmondo/img/faces/64"
LINEUP_URL <- "https://api.futmondo.com/1/userteam/lineup"
CHAMPIONSHIP_PLAYERS <- "https://api.futmondo.com/5/league/championshipplayers"
TEAM_LOGO_URL <- "https://static02.mondocore.com/futmondo/img/teams/64/"
library(httr)
library(dplyr)
library(jsonlite)
library(data.table)

# Global Cache Environment and Utilities
api_cache_env <- new.env(parent = emptyenv())

get_cached_data <- function(key, expr, timeout_sec = 300) {
  now <- Sys.time()
  cached <- api_cache_env[[key]]
  
  if (!is.null(cached) && (as.numeric(now - cached$time, units = "secs") < timeout_sec)) {
    print(paste0("[CACHE] Serving local cache for: ", key))
    return(cached$data)
  }
  
  print(paste0("[API] Fetching fresh data for: ", key))
  data <- force(expr)
  api_cache_env[[key]] <- list(data = data, time = now)
  return(data)
}

clear_api_cache <- function() {
  print("[CACHE] Clearing all entries...")
  rm(list = ls(envir = api_cache_env), envir = api_cache_env)
}

get_real_clubs <- function(login, championship_id) {
  payload <- list(
    header = list(
      token = login[["token"]],
      userid = login[["userid"]]
    ),
    query = list(
      championshipId = championship_id
    ),
    answer = list()
  )

  headers <- c(
    "Content-Type" = "application/json; charset=utf-8"
  )

  cache_key <- paste0("real_clubs_", championship_id)
  get_cached_data(cache_key, {
    print("Getting real clubs in the league")
    response <- POST("https://api.futmondo.com/1/league/championshipteams", body = toJSON(payload), add_headers(.headers = headers))
    clubs <- httr::content(response)$answer
    
    if (is.null(clubs) || length(clubs) == 0) {
      return(data.frame(
        teamId = character(0),
        team = character(0),
        logo = character(0),
        stringsAsFactors = FALSE
      ))
    }
    
    lapply(clubs, FUN = function(club) {
      data.frame(
        teamId = club$id,
        team = club$name,
        logo = club$logo,
        stringsAsFactors = FALSE
      )
    }) %>% bind_rows()
  })
}

login <- function(user_name = "smdb21@msn.com", password = "pepito21") {
  if (is.null(user_name)) {
    user_name <- Sys.getenv("user_name")
  }
  # URL and payload data
  payload <- list(
    header = list(
      token = "null",
      userid = ""
    ),
    query = list(
      mail = user_name,
      pwd = password
    )
  )

  # Sending the POST request
  response <- POST(LOGIN_URL, body = payload, encode = "json") %>%
    httr::content()

  token <- response$answer$mobile$token
  userid <- response$answer$mobile$userid
  if (is.null(userid) || is.null(token)) {
    warning("Login failed.")
    stop("Login failed")
  }
  # Printing the response
  print(response)
  return(c("token" = token, "userid" = userid, "user_name" = user_name))
}

get_championships <- function(login, championship_name = NULL) {
  cache_key <- paste0("championships_", login[["userid"]])
  get_cached_data(cache_key, {
    payload <- list(
      header = list(
        token = login[["token"]],
        userid = login[["userid"]]
      ),
      query = list(
        excludeGeneral = FALSE,
        includeProphets = TRUE
      ),
      answer = list()
    )

    # Adding headers
    headers <- c(
      "Content-Type" = "application/json; charset=utf-8"
    )

    # Sending the POST request
    print("Getting championships")
    response <- POST(ACTIVE_CHAMPIONSHIPS_URL, body = toJSON(payload), add_headers(.headers = headers))
    ret <- httr::content(response)$answer
    ret <- ret[["championships"]]
    if (!is.null(championship_name)) {
      ret <- ret[sapply(ret, FUN = function(championship) {
        championship$name == championship_name
      })]
      if (length(ret) == 0) {
        print("No championships found")
        return(NULL)
      }
    }
    ret <- ret %>% unlist()
    ret
  })
}


get_players_from_team <- function(login, championship_id, user_team_id, teams = NULL) {
  cache_key <- paste0("roster_", championship_id, "_", user_team_id)
  get_cached_data(cache_key, {
    payload <- list(
      header = list(
        token = login[["token"]],
        userid = login[["userid"]]
      ),
      query = list(
        championshipId = championship_id,
        userteamId = user_team_id
      ),
      answer = list()
    )

    # Adding headers
    headers <- c(
      "Content-Type" = "application/json; charset=utf-8"
    )

    # Sending the POST request
    print("Getting players from team")

    response <- POST(ROSTER_URL, body = toJSON(payload), add_headers(.headers = headers))
    roster <- httr::content(response)
    roster <- roster[["answer"]]
    if (is.null(roster) || length(roster) == 0) {
      print("User team roster is empty.")
      empty_df <- data.frame(
        id = character(0), 
        name = character(0), 
        role = character(0), 
        role2 = character(0),
        value = numeric(0), 
        change = numeric(0), 
        points = numeric(0),
        clause_price = numeric(0), 
        isClause = logical(0),
        change_by_value = numeric(0), 
        championship_id = character(0),
        user_team_id = character(0),
        fav = logical(0),
        bid_price = numeric(0),
        numberOfBids = numeric(0),
        userTeam = character(0),
        stringsAsFactors = FALSE
      )
      return(empty_df)
    }
    roster <- lapply(roster, FUN = function(player) {
      print(player$name)
      average <- player$average
      fitness <- average$fitness %>% unlist()
      if (length(fitness) > 0) { # because before starting the season this is empty
        fitness <- fitness %>% paste0(collapse = ",")
        names(fitness) <- "average.fitness"
        # split in numbers
        last_points <- fitness %>%
          strsplit(",") %>%
          unlist() %>%
          as.numeric()
        total_last_points <- sum(last_points)
        average_last_points <- mean(last_points)
        fitness <- c(fitness, total_last_points, average_last_points)
        names(fitness) <- c("average.fitness", "average.total", "average.averageLastFive")
      }
      average <- average[-which(names(average) == "fitness")]
      clause <- player$clause
      names(clause) <- paste0("clause_", names(clause))
      # remove average element from player list
      player <- player[-which(names(player) == "average")]
      # remove clause element from player list
      player <- player[-which(names(player) == "clause")]
      ## market ----
      market <- player$market
      ### bids ----
      bids <- NULL
      if (is.list(market)) {
        if ("bids" %in% names(market)) {
          bids <- market$bids
          if (length(bids) > 0) {
            bids <- lapply(bids, FUN = function(bid) {
              data.frame(bid_price = bid$price, bid_user = bid$userTeam[["name"]])
            }) %>% rbindlist(fill = T)
            # make it a single row
            bids <- bids %>%
              dplyr::mutate(
                bid_price = paste0(bid_price, collapse = ","),
                bid_user = paste0(bid_user, collapse = ",")
              )
          }
          market <- market[-which(names(market) == "bids")]
        }
        names(market) <- paste0("market_", names(market))
      }
      # remove market element from player list
      player <- player[-which(names(player) == "market")]
      player <- c(player, average, fitness, clause, market)

      if (!is.null(bids)) {
        player <- c(player, bids)
      }
      player
    }) %>% rbindlist(fill = T)

    # any column that can be transformed to numeric, do it
    roster <- roster %>% as.data.frame()
    # make numbers as numbers
    roster <- type.convert(roster, as.is = TRUE)

    # add championship_id, user_team_id
    roster$championship_id <- championship_id
    roster$user_team_id <- user_team_id
    if (!is.null(teams)) {
      roster <- roster %>%
        dplyr::left_join(teams %>% dplyr::distinct(id, teamname), by = c("user_team_id" = "id"))
    }
    
    # Join real-world team name and logo
    clubs <- get_real_clubs(login, championship_id)
    if (!is.null(clubs) && nrow(clubs) > 0 && "teamId" %in% colnames(roster)) {
      roster <- roster %>% dplyr::select(!any_of(c("team", "logo")))
      roster <- roster %>% dplyr::left_join(clubs, by = "teamId")
    }
    
    roster
  })
}

# Function to check if a column is character and convert to numeric
convert_to_numeric <- function(x) {
  if (is.character(x)) {
    # Check if all elements can be safely converted to numeric
    # If all values are just numbers (or NA), convert them
    if (all(!is.na(as.numeric(x)))) {
      return(as.numeric(x))
    } else {
      warning(paste("Column", deparse(substitute(x)), "contains non-numeric characters and was not converted."))
      return(x) # Return the original column if it contains non-numeric strings
    }
  } else {
    return(x)
  }
}


get_market_players <- function(login, championship_id, user_team_id) {
  cache_key <- paste0("market_", championship_id, "_", user_team_id)
  get_cached_data(cache_key, {
    payload <- list(
      header = list(
        token = login[["token"]],
        userid = login[["userid"]]
      ),
      query = list(
        championshipId = championship_id,
        userteamId = user_team_id,
        type = "market"
      ),
      answer = list()
    )

    # Adding headers
    headers <- c(
      "Content-Type" = "application/json; charset=utf-8"
    )

    # Sending the POST request
    print("Getting players in the market")
    response <- POST(MARKET_URL, body = toJSON(payload), add_headers(.headers = headers))
    players <- httr::content(response)$answer
    if (is.null(players)) {
      html_error <- httr::content(response)
      # show error:
      stop(paste0("Error in request: ", html_error))
    }
    ret <- lapply(players, FUN = function(player) {
      player <- parse_player_json(player = player)
      player
    }) %>% rbindlist(fill = T)
    if (NA %in% names(ret)) {
      # remove that column
      ret <- ret %>% as.data.frame()
      ret <- ret[, which(is.na(names(ret)) == FALSE)]
    }
    ret <- ret %>% dplyr::arrange(desc(change))
    ret
  })
}

get_championship_players <- function(login, championship_id) {
  cache_key <- paste0("championshipplayers_", championship_id)
  get_cached_data(cache_key, {
    payload <- list(
      header = list(
        token = login[["token"]],
        userid = login[["userid"]]
      ),
      query = list(
        championshipId = championship_id
      ),
      answer = list()
    )

    # Adding headers
    headers <- c(
      "Content-Type" = "application/json; charset=utf-8"
    )

    # Sending the POST request
    print("Getting championship players")
    response <- POST(CHAMPIONSHIP_PLAYERS, body = toJSON(payload), add_headers(.headers = headers))
    players <- httr::content(response)$answer$players
    ret <- lapply(players, FUN = function(player) {
      player <- parse_player_json(player = player)
      player
    }) %>% rbindlist(fill = T)
    # any column that can be transformed to numeric, do it
    ret <- ret %>% as.data.frame()
    numeric_cols <- sapply(ret, FUN = function(col) {
      all(!is.na(as.numeric(as.character(col))))
    })
    ret[, numeric_cols] <- lapply(ret[, numeric_cols], FUN = function(col) {
      as.numeric(as.character(col))
    })
    
    # Join real-world team name and logo
    clubs <- get_real_clubs(login, championship_id)
    if (!is.null(clubs) && nrow(clubs) > 0 && "teamId" %in% colnames(ret)) {
      ret <- ret %>% dplyr::select(!any_of(c("team", "logo")))
      ret <- ret %>% dplyr::left_join(clubs, by = "teamId")
    }
    
    ret <- ret %>% dplyr::arrange(name)
    ret
  })
}


parse_player_json <- function(player) {
  # player <- assign_names_recursive(lst = player, parent_name = "")
  if (player$name == "Lucas Beltrán") {
    # browser()
  }
  average <- player$average
  average <- remove_json_children(json = average, element_name = "fitness", collapse_children = TRUE)
  # remove total because it is already in player as player$points
  # instead, add the points in average.fitness
  average$total <- average$fitness %>%
    strsplit(",") %>%
    unlist() %>%
    as.numeric() %>%
    sum()
  names(average) <- paste0("average.", names(average))
  # remove average element from player list
  player <- player[-which(names(player) == "average")]
  player <- c(player, average)
  # clause
  if ("clause" %in% names(player)) {
    clause <- player$clause
    names(clause) <- paste0("clause_", names(clause))
    # remove clause element from player list
    player <- player[-which(names(player) == "clause")]
    player <- c(player, clause)
  }
  if ("total" %in% names(player)) {
    warning("Found 'total' directly in player object. Handling it.")
    player <- remove_json_children(json = player, element_name = "total")
  }
  if ("bid" %in% names(player)) {
    bid <- player$bid
    names(bid) <- paste0("bid_", names(bid))
    # remove bid element from player list
    player <- player[-which(names(player) == "bid")]
    player <- c(player, bid)
  }
  ret <- data.frame(value = player)
  colnames(ret) <- names(player)

  return(ret)
}


remove_json_children <- function(json, element_name, collapse_children = FALSE) {
  children <- json[[element_name]]
  if (is.null(children)) {
    return(json)
  }
  if (collapse_children) {
    children <- children %>%
      unlist() %>%
      paste0(., collapse = ",")
    names(children) <- element_name
  } else {
    names(children) <- paste0(element_name, ".", names(children))
  }
  json <- json[-which(names(json) == element_name)]
  json <- c(json, children)
  return(json)
}

translate_player_positions <- function(players_df) {
  if (is.null(players_df) || nrow(players_df) == 0) return(players_df)

  position_map <- data.frame(
    role_to = c("Goalkeeper", "Defender", "Midfielder", "Forward"),
    role_from = c("portero", "defensa", "centrocampista", "delantero"),
    stringsAsFactors = FALSE
  )

  if ("role" %in% colnames(players_df)) {
    translated_role <- position_map$role_to[match(players_df$role, position_map$role_from)]
    players_df$role <- ifelse(!is.na(translated_role), translated_role, players_df$role)
  }

  if ("role2" %in% colnames(players_df)) {
    translated_role2 <- position_map$role_to[match(players_df$role2, position_map$role_from)]
    players_df$role2 <- ifelse(!is.na(translated_role2), translated_role2, players_df$role2)

    # Mutate role = paste0(role, ", ", role2) when secondary position exists
    has_role2 <- !is.na(players_df$role2) & players_df$role2 != "" & players_df$role2 != "NA" & players_df$role2 != players_df$role
    players_df$role <- ifelse(has_role2, paste0(players_df$role, ", ", players_df$role2), players_df$role)
  }

  return(players_df)
}

calculate_player_changes <- function(players_df) {
  players_df <- players_df %>%
    dplyr::mutate(change_by_value = change / value) %>%
    dplyr::relocate(change_by_value, .after = change)
  return(players_df)
}

unify_columns <- function(players_df) {
  if (is.null(players_df) || nrow(players_df) == 0) return(players_df)

  # Normalize market price column across roster (market_price) and market endpoints (price)
  if (!"effective_market_price" %in% colnames(players_df)) {
    if ("market_price" %in% colnames(players_df)) {
      players_df$effective_market_price <- suppressWarnings(as.numeric(players_df$market_price))
    } else if ("price" %in% colnames(players_df)) {
      players_df$effective_market_price <- suppressWarnings(as.numeric(players_df$price))
    } else {
      players_df$effective_market_price <- NA_real_
    }
  }

  # Compute active clause availability
  clause_p <- if ("clause_price" %in% colnames(players_df)) suppressWarnings(as.numeric(players_df$clause_price)) else NA_real_
  clause_t <- if ("clause_transferred" %in% colnames(players_df)) as.logical(players_df$clause_transferred) else FALSE
  clause_t[is.na(clause_t)] <- FALSE

  players_df$isClause <- !is.na(clause_p) & clause_p > 0 & !clause_t

  return(players_df)
}
get_lineup_from_team <- function(login, championship_id, user_team_id) {
  payload <- list(
    header = list(
      token = login[["token"]],
      userid = login[["userid"]]
    ),
    query = list(
      championshipId = championship_id,
      userteamId = user_team_id
    ),
    answer = list()
  )

  # Adding headers
  headers <- c(
    "Content-Type" = "application/json; charset=utf-8"
  )

  # Sending the POST request
  print("Getting lineup from team")

  response <- POST(LINEUP_URL, body = toJSON(payload), add_headers(.headers = headers))
  lineup <- httr::content(response)
  lineup <- lineup[["answer"]]
  lineup_config <- lineup
  lineup_config$players <- NULL
  lineup_config$bench <- NULL
  lineup_config$custom <- NULL # to be parsed in the future
  lineup_config <- as.data.frame(t(unlist(lineup_config)))
  players_list <- lineup$players
  players <- lapply(players_list, FUN = function(player) {
    player <- unlist(player) %>%
      t() %>%
      as.data.frame()
  }) %>%
    bind_rows() %>%
    dplyr::arrange(position)

  budget <- lineup$budget
  bench_list <- lineup$bench
  bench <- lapply(bench_list$players, FUN = function(player) {
    player <- unlist(player) %>%
      t() %>%
      as.data.frame()
  }) %>%
    bind_rows() %>%
    dplyr::arrange(position)
  bench_config <- bench_list
  bench_config$players <- NULL
  bench_config <- as.data.frame(t(unlist(bench_config)))

  lineup <- list(
    lineup_config = lineup_config,
    players = players,
    budget = budget,
    bench = list(
      config = bench_config,
      players = bench
    )
  )
  return(lineup)
}

# Vectorized Date Formatter to DD-MM-YY HH:MM
format_table_date <- function(value) {
  if (is.null(value)) return("")
  if (all(is.na(value)) || all(value == "")) return(rep("", length(value)))
  
  posix_time <- tryCatch({
    as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  }, error = function(e) {
    as.POSIXct(value, tz = "UTC")
  })
  
  formatted <- format(posix_time, "%d-%m-%y %H:%M")
  formatted[is.na(posix_time)] <- ""
  return(formatted)
}

# Vectorized Currency Formatter in Euros
format_table_currency <- function(value) {
  if (is.null(value)) return("")
  if (!is.numeric(value)) {
    value <- suppressWarnings(as.numeric(value))
  }
  if (all(is.na(value))) return(rep("", length(value)))
  
  formatted <- scales::label_currency(prefix = "", suffix = "\u00a0\u20ac", big.mark = ".", decimal.mark = ",")(value)
  formatted[is.na(value)] <- ""
  return(formatted)
}

reorder_player_table_columns <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)

  df_is_dt <- data.table::is.data.table(df)
  df_df <- as.data.frame(df)

  desired_order <- c(
    "name", "role", "team", "numberOfBids", "bid_price", "bid_user",
    "points", "price", "market_price", "effective_market_price",
    "change", "change_by_value", "value",
    "clause_price", "clause_suggestedClause", "clause_date",
    "creationDate", "expirationDate", "userTeam"
  )

  existing_cols <- colnames(df_df)
  present_desired <- intersect(desired_order, existing_cols)
  remaining_cols <- setdiff(existing_cols, present_desired)

  final_cols <- c(present_desired, remaining_cols)
  res <- df_df[, final_cols, drop = FALSE]
  if (df_is_dt) res <- data.table::as.data.table(res)
  return(res)
}

get_reactable_columns_for_players <- function(table) {
  columns <- list()

  # Bids Column ----
  if ("numberOfBids" %in% colnames(table)) {
    columns[["numberOfBids"]] <- colDef(
      name = "Bids",
      align = "center"
    )
  }

  # Your Bid Column ----
  if ("bid_price" %in% colnames(table)) {
    columns[["bid_price"]] <- colDef(
      name = "Your Bid",
      align = "right",
      cell = function(value) {
        if (is.na(value) || is.null(value) || value == "" || value == 0) return("")
        num_val <- suppressWarnings(as.numeric(value))
        formatted <- format_table_currency(num_val)
        shiny::tags$span(class = "badge-active-bid", formatted)
      }
    )
  }

  # Points Column ----
  if ("points" %in% colnames(table)) {
    columns[["points"]] <- colDef(
      name = "Points",
      align = "center"
    )
  }

  # Market Price Columns ----
  if ("price" %in% colnames(table)) {
    columns[["price"]] <- colDef(
      name = "Market Price",
      align = "right",
      cell = function(value) {
        format_table_currency(value)
      }
    )
  }

  if ("market_price" %in% colnames(table)) {
    columns[["market_price"]] <- colDef(
      name = "Market Price",
      align = "right",
      cell = function(value) {
        format_table_currency(value)
      }
    )
  }

  # Trend Columns ----
  if ("change" %in% colnames(table)) {
    columns[["change"]] <- colDef(
      name = "Trend",
      align = "right",
      cell = function(value) {
        if (is.na(value) || !is.numeric(value)) return("")
        color_class <- if (value > 0) "value-positive" else if (value < 0) "value-negative" else ""
        sign_prefix <- if (value > 0) "+" else ""
        formatted <- scales::label_currency(prefix = sign_prefix, suffix = "\u00a0\u20ac", big.mark = ".", decimal.mark = ",")(value)
        shiny::tags$span(class = color_class, style = "white-space: nowrap !important;", formatted)
      }
    )
  }

  if ("change_by_value" %in% colnames(table)) {
    columns[["change_by_value"]] <- colDef(
      name = "Trend (%)",
      align = "right",
      cell = function(value) {
        if (is.na(value) || !is.numeric(value)) return("")
        color_class <- if (value > 0) "value-positive" else if (value < 0) "value-negative" else ""
        sign_prefix <- if (value > 0) "+" else ""
        formatted <- paste0(sign_prefix, round(value * 100, 2), "\u00a0%")
        shiny::tags$span(class = color_class, style = "white-space: nowrap !important;", formatted)
      }
    )
  }

  # Valuation Column ----
  if ("value" %in% colnames(table)) {
    columns[["value"]] <- colDef(
      name = "Valuation",
      align = "right",
      width = 120,
      cell = function(value) {
        format_table_currency(value)
      }
    )
  }

  # Clause Columns ----
  if ("clause_price" %in% colnames(table)) {
    columns[["clause_price"]] <- colDef(
      name = "Clause Price",
      align = "right",
      cell = function(value) {
        format_table_currency(value)
      }
    )
  }

  if ("clause_suggestedClause" %in% colnames(table)) {
    columns[["clause_suggestedClause"]] <- colDef(
      name = "Suggested Clause",
      align = "right",
      cell = function(value) {
        format_table_currency(value)
      }
    )
  }

  # Core Date Columns ----
  if ("creationDate" %in% colnames(table)) {
    columns[["creationDate"]] <- colDef(
      name = "Availability Start",
      align = "center",
      cell = function(value) {
        format_table_date(value)
      }
    )
  }

  if ("expirationDate" %in% colnames(table)) {
    columns[["expirationDate"]] <- colDef(
      name = "Availability End",
      align = "center",
      cell = function(value) {
        format_table_date(value)
      }
    )
  }

  if ("clause_date" %in% colnames(table)) {
    columns[["clause_date"]] <- colDef(
      name = "Clause Expiration",
      align = "center",
      cell = function(value) {
        format_table_date(value)
      }
    )
  }

  # Identity & Status Columns ----
  if ("userTeam" %in% colnames(table)) {
    columns[["userTeam"]] <- colDef(
      name = "User",
      align = "left"
    )
  }

  if ("bid_user" %in% colnames(table)) {
    columns[["bid_user"]] <- colDef(
      name = "Bidder",
      align = "left"
    )
  }

  if ("role" %in% colnames(table)) {
    columns[["role"]] <- colDef(
      name = "Position",
      align = "center",
      cell = function(value) {
        if (is.null(value) || is.na(value) || value == "" || value == "NA") return("")

        # Split multiple positions by comma or slash
        roles <- unlist(strsplit(as.character(value), "[,/]+"))

        badges <- lapply(roles, function(pos) {
          pos_clean <- trimws(pos)
          if (pos_clean == "" || pos_clean == "NA") return(NULL)

          class_name <- if (pos_clean %in% c("Goalkeeper", "portero", "GK", "P")) {
            "badge-gk"
          } else if (pos_clean %in% c("Defender", "defensa", "DF", "D")) {
            "badge-df"
          } else if (pos_clean %in% c("Midfielder", "centrocampista", "MD", "M")) {
            "badge-md"
          } else if (pos_clean %in% c("Forward", "delantero", "FW", "F")) {
            "badge-fw"
          } else {
            "badge-df"
          }

          display_name <- if (pos_clean %in% c("portero", "GK", "P")) "Goalkeeper" else if (pos_clean %in% c("defensa", "DF", "D")) "Defender" else if (pos_clean %in% c("centrocampista", "MD", "M")) "Midfielder" else if (pos_clean %in% c("delantero", "FW", "F")) "Forward" else pos_clean

          shiny::tags$span(class = class_name, display_name)
        })

        badges <- badges[!sapply(badges, is.null)]

        if (length(badges) == 0) {
          ""
        } else if (length(badges) == 1) {
          badges[[1]]
        } else {
          shiny::tags$div(
            style = "display: flex; justify-content: center; align-items: center; gap: 4px; flex-wrap: nowrap;",
            badges
          )
        }
      }
    )
  }

  if ("role2" %in% colnames(table)) {
    columns[["role2"]] <- colDef(show = FALSE)
  }

  if ("market_inMarket" %in% colnames(table)) {
    columns[["market_inMarket"]] <- colDef(
      name = "Listed",
      align = "center",
      cell = function(value) {
        if (is.null(value) || is.na(value)) return("")
        if (value) {
          shiny::tags$span(style = "color: #10b981; font-weight: 600;", "YES")
        } else {
          shiny::tags$span(style = "color: #94a3b8;", "NO")
        }
      }
    )
  }

  if ("status" %in% colnames(table)) {
    columns[["status"]] <- colDef(
      name = "Status",
      align = "center",
      cell = function(value) {
        if (is.na(value) || value == "") {
          return(shiny::tags$span(style = "color: #10b981; font-weight: 500;", shiny::icon("circle-check"), " Fit"))
        }
        val_lower <- tolower(value)
        if (val_lower == "ok") {
          shiny::tags$span(style = "color: #10b981; font-weight: 500;", shiny::icon("circle-check"), " Fit")
        } else if (val_lower == "doubt") {
          shiny::tags$span(style = "color: #f59e0b; font-weight: 500;", shiny::icon("triangle-exclamation"), " Doubt")
        } else if (val_lower == "injured") {
          shiny::tags$span(style = "color: #ef4444; font-weight: 500;", shiny::icon("circle-minus"), " Injured")
        } else if (val_lower == "injured2") {
          shiny::tags$span(style = "color: #b91c1c; font-weight: 500;", shiny::icon("hospital"), " Long-term")
        } else if (val_lower == "redcard") {
          shiny::tags$span(style = "color: #ef4444; font-weight: 500;", shiny::icon("square"), " Suspended")
        } else {
          shiny::tags$span(style = "color: #10b981; font-weight: 500;", shiny::icon("circle-check"), " Fit")
        }
      }
    )
  }

  # Averages / Points Columns ----
  if ("average.average" %in% colnames(table)) {
    columns[["average.average"]] <- colDef(
      name = "Avg Points",
      align = "center",
      cell = function(value) {
        if (is.null(value) || is.na(value) || value == "NaN" || value == "") return("-")
        round(as.numeric(value), 1)
      }
    )
  }

  if ("average.homeAverage" %in% colnames(table)) {
    columns[["average.homeAverage"]] <- colDef(
      name = "Home Avg",
      align = "center",
      cell = function(value) {
        if (is.null(value) || is.na(value) || value == "NaN" || value == "") return("-")
        round(as.numeric(value), 1)
      }
    )
  }

  if ("average.awayAverage" %in% colnames(table)) {
    columns[["average.awayAverage"]] <- colDef(
      name = "Away Avg",
      align = "center",
      cell = function(value) {
        if (is.null(value) || is.na(value) || value == "NaN" || value == "") return("-")
        round(as.numeric(value), 1)
      }
    )
  }

  if ("average.averageLastFive" %in% colnames(table)) {
    columns[["average.averageLastFive"]] <- colDef(
      name = "Avg Last 5",
      align = "center",
      cell = function(value) {
        if (is.null(value) || is.na(value) || value == "NaN" || value == "") return("-")
        round(as.numeric(value), 1)
      }
    )
  }

  if ("average.matches" %in% colnames(table)) {
    columns[["average.matches"]] <- colDef(
      name = "Played",
      align = "center"
    )
  }

  if ("average.total" %in% colnames(table)) {
    columns[["average.total"]] <- colDef(
      name = "Last 5 Points",
      align = "center"
    )
  }

  if ("buyPrice" %in% colnames(table)) {
    columns[["buyPrice"]] <- colDef(
      name = "Acquisition Price",
      align = "right",
      cell = function(value) {
        format_table_currency(value)
      }
    )
  }

  if ("clause_ratio" %in% colnames(table)) {
    columns[["clause_ratio"]] <- colDef(
      name = "Scout Indicator",
      align = "center",
      cell = function(value) {
        if (is.na(value) || !is.numeric(value)) return("")
        if (value < 1.1) {
          shiny::tags$span(class = "badge-md", style = "font-weight: 700; font-size: 11px; padding: 2px 8px;", "STEAL")
        } else if (value < 1.3) {
          shiny::tags$span(style = "color: #f59e0b; font-weight: 600;", "GOOD VALUE")
        } else {
          shiny::tags$span(style = "color: #94a3b8;", "OVERPRICED")
        }
      }
    )
  }

  return(columns)
}


get_teams <- function(login, championship_id) {
  cache_key <- paste0("teams_", championship_id)
  get_cached_data(cache_key, {
    payload <- list(
      header = list(
        token = login[["token"]],
        userid = login[["userid"]]
      ),
      query = list(
        championshipId = championship_id
      ),
      answer = list()
    )

    # Adding headers
    headers <- c(
      "Content-Type" = "application/json; charset=utf-8"
    )

    # Sending the POST request
    print("Getting teams")
    response <- POST(TEAMS_URL, body = toJSON(payload), add_headers(.headers = headers))
    ans <- httr::content(response)$answer
    
    # Support both nested 'teams' object or direct array format safely
    if (is.list(ans) && "teams" %in% names(ans)) {
      teams <- ans$teams
    } else {
      teams <- ans
    }
    
    if (is.null(teams) || length(teams) == 0) {
      warning("No user teams retrieved from championship.")
      return(data.frame(
        teamid = character(0), teamname = character(0), points = numeric(0),
        name = character(0), stringsAsFactors = FALSE
      ))
    }
    
    ret <- lapply(teams, FUN = function(team) {
      unlist(team) %>%
        t() %>%
        as.data.frame()
    }) %>% bind_rows()
    
    # Ensure expected columns are present
    if (nrow(ret) > 0) {
      if (!"teamid" %in% colnames(ret) && "id" %in% colnames(ret)) {
        ret$teamid <- ret$id
      }
      if (!"teamname" %in% colnames(ret) && "name" %in% colnames(ret)) {
        ret$teamname <- ret$name
      }
      if (!"points" %in% colnames(ret)) {
        ret$points <- 0
      }
    }
    
    print(paste0(nrow(ret), " teams retrieved"))
    ret
  })
}


buy_clause <- function(login, championship_id, team_id, player_id, player_slug, price, isClause = TRUE) {
  payload <- list(
    header = list(
      token = login[["token"]],
      userid = login[["userid"]]
    ),
    query = list(
      championshipId = championship_id,
      userteamId = team_id,
      player_id = player_id,
      player_slug = player_slug,
      price = price,
      isClause = isClause
    ),
    answer = list()
  )

  # Adding headers
  headers <- c(
    "Content-Type" = "application/json; charset=utf-8"
  )

  # Sending the POST request
  print("Sending bid/clause purchase request")
  response <- POST(BID_URL, body = toJSON(payload), add_headers(.headers = headers))
  operation_code <- httr::content(response)$answer$code
  return(operation_code == API_CODE_OK)
}

get_user_team_info <- function(login, championship_id, user_team_id) {
  cache_key <- paste0("team_info_", championship_id, "_", user_team_id)
  get_cached_data(cache_key, {
    payload <- list(
      header = list(
        token = login[["token"]],
        userid = login[["userid"]]
      ),
      query = list(
        championshipId = championship_id,
        userteamId = user_team_id,
        type = "market"
      ),
      answer = list()
    )

    headers <- c("Content-Type" = "application/json; charset=utf-8")
    url <- "https://api.futmondo.com/1/userteam/information"

    print("Getting team info details")
    response <- POST(url, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
    ans <- httr::content(response)

    if (!is.null(ans) && "answer" %in% names(ans)) {
      return(ans[["answer"]])
    } else {
      return(NULL)
    }
  })
}

get_team_image_name <- function(team) {
  team_image_name <- gsub(" ", "-", tolower(team))
  team_image_name <- gsub("r\\.", "real", tolower(team_image_name))
  return(team_image_name)
}

get_user_team_moneymovements <- function(login, championship_id, user_team_id) {
  cache_key <- paste0("moneymovements_", championship_id, "_", user_team_id)
  get_cached_data(cache_key, {
    payload <- list(
      header = list(
        token = login[["token"]],
        userid = login[["userid"]]
      ),
      query = list(
        championshipId = championship_id,
        userteamId = user_team_id
      ),
      answer = list()
    )
    headers <- c("Content-Type" = "application/json; charset=utf-8")
    url <- "https://api.futmondo.com/1/userteam/moneymovements"
    print("Getting team money movements")
    response <- POST(url, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
    ans <- httr::content(response)
    if (!is.null(ans) && "answer" %in% names(ans) && is.list(ans$answer)) {
      movements <- ans$answer
      if (length(movements) == 0) {
        return(data.frame(
          id = character(0), concept = character(0), type = character(0),
          category = character(0), money = numeric(0), date = character(0),
          stringsAsFactors = FALSE
        ))
      }
      df <- lapply(movements, FUN = function(m) {
        data.frame(
          id = if (!is.null(m[["_id"]])) as.character(m[["_id"]]) else NA_character_,
          concept = if (!is.null(m[["concept"]])) as.character(m[["concept"]]) else "",
          type = if (!is.null(m[["type"]])) as.character(m[["type"]]) else "",
          category = if (!is.null(m[["category"]])) as.character(m[["category"]]) else "",
          money = if (!is.null(m[["money"]])) as.numeric(m[["money"]]) else 0,
          date = if (!is.null(m[["date"]])) as.character(m[["date"]]) else "",
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows()
      return(df)
    }
    return(data.frame(
      id = character(0), concept = character(0), type = character(0),
      category = character(0), money = numeric(0), date = character(0),
      stringsAsFactors = FALSE
    ))
  })
}

calculate_league_finances <- function(login, championship_id, user_teams_df, initial_budget = 300000000) {
  cache_key <- paste0("league_finances_calc_", championship_id)
  get_cached_data(cache_key, {
    if (is.null(user_teams_df) || nrow(user_teams_df) == 0) {
      return(list(
        team_finances = data.frame(
          teamid = character(0), teamname = character(0), initial_budget = numeric(0),
          total_spent = numeric(0), budget = numeric(0), team_value = numeric(0),
          net_profit_loss = numeric(0), squad_size = numeric(0), points = numeric(0),
          point_bonus = numeric(0), stringsAsFactors = FALSE
        ),
        all_purchases = data.frame()
      ))
    }
    
    finances_list <- list()
    purchases_list <- list()
    
    for (i in seq_len(nrow(user_teams_df))) {
      row <- user_teams_df[i, ]
      tid <- if ("teamid" %in% colnames(row)) row$teamid else row$id
      tname <- if ("teamname" %in% colnames(row)) row$teamname else row$name
      tpoints <- if ("points" %in% colnames(row)) as.numeric(row$points) else 0
      
      # Fetch squad roster for this user team
      roster <- tryCatch({
        get_players_from_team(login = login, championship_id = championship_id, user_team_id = tid, teams = user_teams_df)
      }, error = function(e) {
        print(paste0("[Finances] Error fetching roster for team ", tname, ": ", e$message))
        NULL
      })
      
      total_spent <- 0
      team_value <- 0
      squad_size <- 0
      
      if (!is.null(roster) && nrow(roster) > 0) {
        squad_size <- nrow(roster)
        if ("buyPrice" %in% colnames(roster)) {
          total_spent <- sum(suppressWarnings(as.numeric(roster$buyPrice)), na.rm = TRUE)
        }
        if ("value" %in% colnames(roster)) {
          team_value <- sum(suppressWarnings(as.numeric(roster$value)), na.rm = TRUE)
        }
        
        # Build purchase breakdown for this team
        roster_purchases <- roster
        roster_purchases$owner_teamid <- tid
        roster_purchases$owner_teamname <- tname
        if (!"buyPrice" %in% colnames(roster_purchases)) roster_purchases$buyPrice <- 0
        if (!"value" %in% colnames(roster_purchases)) roster_purchases$value <- 0
        roster_purchases$net_gain_loss <- roster_purchases$value - roster_purchases$buyPrice
        
        # Standardize column data types across all team rosters to prevent bind_rows type mismatch
        char_cols <- c("id", "slug", "name", "team", "role", "role2", "photo", "teamId", "status", "owner_teamid", "owner_teamname", "logo")
        for (col in colnames(roster_purchases)) {
          if (col %in% char_cols) {
            roster_purchases[[col]] <- as.character(roster_purchases[[col]])
          } else if (is.numeric(roster_purchases[[col]]) || is.integer(roster_purchases[[col]])) {
            roster_purchases[[col]] <- as.numeric(roster_purchases[[col]])
          }
        }
        purchases_list[[length(purchases_list) + 1]] <- roster_purchases
      }
      
      # Point bonus calculation (70000 EUR per point)
      point_bonus <- tpoints * 70000
      
      # Money Left = Initial Budget - Total Spent + Point Bonus
      calc_money_left <- initial_budget - total_spent + point_bonus
      
      # If get_user_team_info provides an explicit budget for this team, use it if valid
      actual_info <- tryCatch({
        get_user_team_info(login = login, championship_id = championship_id, user_team_id = tid)
      }, error = function(e) NULL)
      
      final_budget <- calc_money_left
      if (!is.null(actual_info) && !is.null(actual_info$budget) && is.numeric(actual_info$budget) && actual_info$budget > 0) {
        final_budget <- actual_info$budget
      }
      
      if (!is.null(actual_info) && !is.null(actual_info$teamValue) && is.numeric(actual_info$teamValue) && actual_info$teamValue > 0) {
        team_value <- actual_info$teamValue
      }
      
      finances_list[[length(finances_list) + 1]] <- data.frame(
        teamid = as.character(tid),
        teamname = as.character(tname),
        initial_budget = as.numeric(initial_budget),
        total_spent = as.numeric(total_spent),
        budget = as.numeric(final_budget),
        team_value = as.numeric(team_value),
        net_profit_loss = as.numeric(team_value - total_spent),
        squad_size = as.numeric(squad_size),
        points = as.numeric(tpoints),
        point_bonus = as.numeric(point_bonus),
        stringsAsFactors = FALSE
      )
    }
    
    finances_df <- bind_rows(finances_list)
    purchases_df <- if (length(purchases_list) > 0) {
      data.table::rbindlist(purchases_list, fill = TRUE) %>% as.data.frame()
    } else {
      data.frame()
    }
    
    # Sync calculated financial standings to Supabase
    tryCatch({
      sync_user_teams_to_supabase(finances_df, championship_id)
      log_user_team_history(finances_df)
    }, error = function(e) {
      print(paste0("[Finances] Supabase sync warning: ", e$message))
    })
    
    return(list(
      team_finances = finances_df,
      all_purchases = purchases_df
    ))
  }, timeout_sec = 300)
}

get_player_summary <- function(login, championship_id, user_team_id = NULL, player_id = NULL) {
  if (is.null(login) || is.null(championship_id) || is.null(player_id)) return(NULL)
  
  cache_key <- paste0("player_summary_", championship_id, "_", player_id)
  get_cached_data(cache_key, {
    payload <- list(
      header = list(
        token = login[["token"]],
        userid = login[["userid"]]
      ),
      query = list(
        championshipId = championship_id,
        userteamId = if (!is.null(user_team_id)) user_team_id else "",
        playerId = player_id
      ),
      answer = list()
    )
    headers <- c("Content-Type" = "application/json; charset=utf-8")
    
    print(paste0("[API] Fetching player summary for: ", player_id))
    response <- POST(PLAYER_SUMMARY_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
    ans <- httr::content(response)
    
    if (!is.null(ans) && "answer" %in% names(ans) && is.list(ans$answer)) {
      ans_data <- ans$answer
      
      my_bid_id <- NULL
      my_bid_price <- NULL
      
      # Extract bids array from market or top level
      bids_arr <- NULL
      if ("market" %in% names(ans_data) && is.list(ans_data$market) && "bids" %in% names(ans_data$market)) {
        bids_arr <- ans_data$market$bids
      } else if ("bids" %in% names(ans_data)) {
        bids_arr <- ans_data$bids
      }
      
      if (!is.null(bids_arr) && is.list(bids_arr) && length(bids_arr) > 0) {
        for (b in bids_arr) {
          if (is.list(b) && !is.null(b[["id"]]) && !is.null(b[["price"]])) {
            my_bid_id <- as.character(b[["id"]])
            my_bid_price <- suppressWarnings(as.numeric(b[["price"]]))
          }
        }
      }
      
      return(list(
        data = if ("data" %in% names(ans_data)) ans_data$data else NULL,
        prices = if ("prices" %in% names(ans_data)) ans_data$prices else list(),
        bids = bids_arr,
        my_bid_id = my_bid_id,
        my_bid_price = my_bid_price
      ))
    }
    return(NULL)
  }, timeout_sec = 60)
}

modify_bid <- function(login, championship_id, team_id, player_id, bid_id, new_price) {
  payload <- list(
    header = list(
      token = login[["token"]],
      userid = login[["userid"]]
    ),
    query = list(
      championshipId = championship_id,
      userteamId = team_id,
      price = as.numeric(new_price),
      rounds = NULL,
      player_id = player_id,
      bid = bid_id
    ),
    answer = list()
  )
  headers <- c("Content-Type" = "application/json; charset=utf-8")
  print(paste0("[API] Sending modify bid request for bid: ", bid_id, " new price: ", new_price))
  response <- POST(MODIFY_BID_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
  ans <- httr::content(response)
  operation_code <- if (!is.null(ans) && "answer" %in% names(ans) && "code" %in% names(ans$answer)) ans$answer$code else ""
  return(operation_code == API_CODE_OK)
}

cancel_bid <- function(login, championship_id, team_id, bid_id) {
  payload <- list(
    header = list(
      token = login[["token"]],
      userid = login[["userid"]]
    ),
    query = list(
      championshipId = championship_id,
      userteamId = team_id,
      bid = bid_id
    ),
    answer = list()
  )
  headers <- c("Content-Type" = "application/json; charset=utf-8")
  print(paste0("[API] Sending cancel bid request for bid: ", bid_id))
  response <- POST(CANCEL_BID_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
  ans <- httr::content(response)
  operation_code <- if (!is.null(ans) && "answer" %in% names(ans) && "code" %in% names(ans$answer)) ans$answer$code else ""
  return(operation_code == API_CODE_OK)
}
