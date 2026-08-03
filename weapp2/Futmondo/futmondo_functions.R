LOGIN_URL <- "https://api.futmondo.com/5/login/with_mail"
ACTIVE_CHAMPIONSHIPS_URL <- "https://api.futmondo.com/2/user/activechampionships"
TEAMS_URL <- "https://api.futmondo.com/2/championship/teams"
ROSTER_URL <- "https://api.futmondo.com/1/userteam/roster"
CLAUSULA_URL <- "https://api.futmondo.com/1/market/bid"
MARKET_URL <- "https://api.futmondo.com/1/market/players"
PLAYER_SUMMARY_URL <- "https://api.futmondo.com/1/player/summary"
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
  position_map <- data.frame(
    role_to = c("Goalkeeper", "Defender", "Midfielder", "Forward"),
    role_from = c("portero", "defensa", "centrocampista", "delantero")
  )
  tryCatch(
    {
      players_df$role <- position_map$role_to[match(players_df$role, position_map$role_from)]
    },
    warning = function(w) {
      print(paste0("Warning in translating player positions: ", w))
    },
    error = function(e) {
      warning(paste0("Error in translating player positions: ", e))
    }
  )
  if ("role2" %in% colnames(players_df)) {
    players_df$role2 <- position_map$role_to[match(players_df$role2, position_map$role_from)]
  }
  return(players_df)
}

calculate_player_changes <- function(players_df) {
  players_df <- players_df %>%
    dplyr::mutate(change_by_value = change / value)
  return(players_df)
}

unify_columns <- function(players_df) {
  if (!"isClause" %in% colnames(players_df)) {
    players_df <- players_df %>%
      dplyr::mutate(isClause = !is.na(clause_price) & clause_transferred == FALSE)
  }
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

# Standard Date Formatter to DD-MM-YY HH:MM
format_table_date <- function(value) {
  if (is.null(value) || is.na(value) || value == "") return("")
  posix_time <- tryCatch({
    as.POSIXct(value, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  }, error = function(e) {
    as.POSIXct(value, tz = "UTC")
  })
  return(format(posix_time, "%d-%m-%y %H:%M"))
}

# Standard Currency Formatter in Euros
format_table_currency <- function(value) {
  if (is.null(value) || is.na(value) || !is.numeric(value)) return("")
  scales::label_currency(prefix = "", suffix = " EUR", big.mark = ".", decimal.mark = ",")(value)
}

get_reactable_columns_for_players <- function(table) {
  columns <- list()
  
  # Core Price/Valuation Columns ----
  if ("change" %in% colnames(table)) {
    columns[["change"]] <- colDef(
      name = "Trend",
      align = "right",
      cell = function(value) {
        if (is.na(value) || !is.numeric(value)) return("")
        color_class <- if (value > 0) "value-positive" else if (value < 0) "value-negative" else ""
        sign_prefix <- if (value > 0) "+" else ""
        formatted <- scales::label_currency(prefix = sign_prefix, suffix = " EUR", big.mark = ".", decimal.mark = ",")(value)
        shiny::tags$span(class = color_class, formatted)
      }
    )
  }
  
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
  
  if ("change_by_value" %in% colnames(table)) {
    columns[["change_by_value"]] <- colDef(
      name = "Trend (%)",
      align = "right",
      cell = function(value) {
        if (is.na(value) || !is.numeric(value)) return("")
        color_class <- if (value > 0) "value-positive" else if (value < 0) "value-negative" else ""
        sign_prefix <- if (value > 0) "+" else ""
        formatted <- paste0(sign_prefix, round(value * 100, 2), " %")
        shiny::tags$span(class = color_class, formatted)
      }
    )
  }
  
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
  
  if ("bid_price" %in% colnames(table)) {
    columns[["bid_price"]] <- colDef(
      name = "Your Bid",
      align = "right",
      cell = function(value) {
        format_table_currency(value)
      }
    )
  }
  
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
  
  if ("numberOfBids" %in% colnames(table)) {
    columns[["numberOfBids"]] <- colDef(
      name = "Bids",
      align = "center"
    )
  }
  
  if ("role" %in% colnames(table)) {
    columns[["role"]] <- colDef(
      name = "Position",
      align = "center",
      cell = function(value) {
        if (is.na(value) || value == "") return("")
        class_name <- if (value == "Goalkeeper") {
          "badge-gk"
        } else if (value == "Defender") {
          "badge-df"
        } else if (value == "Midfielder") {
          "badge-md"
        } else if (value == "Forward") {
          "badge-fw"
        } else {
          "badge-df"
        }
        shiny::tags$span(class = class_name, value)
      }
    )
  }

  if ("role2" %in% colnames(table)) {
    columns[["role2"]] <- colDef(
      name = "Secondary Position",
      align = "center",
      cell = function(value) {
        if (is.na(value) || value == "") return("")
        class_name <- if (value == "Goalkeeper") {
          "badge-gk"
        } else if (value == "Defender") {
          "badge-df"
        } else if (value == "Midfielder") {
          "badge-md"
        } else if (value == "Forward") {
          "badge-fw"
        } else {
          "badge-df"
        }
        shiny::tags$span(class = class_name, value)
      }
    )
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
  response <- POST(CLAUSULA_URL, body = toJSON(payload), add_headers(.headers = headers))
  operation_code <- httr::content(response)$answer$code
  return(operation_code == API_CODE_OK)
}

get_team_image_name <- function(team) {
  team_image_name <- gsub(" ", "-", tolower(team))
  team_image_name <- gsub("r\\.", "real", tolower(team_image_name))
  return(team_image_name)
}
