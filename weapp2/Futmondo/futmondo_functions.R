LOGIN_URL <- "https://api.futmondo.com/5/login/with_mail"
ACTIVE_CHAMPIONSHIPS_URL <- "https://api.futmondo.com/2/user/activechampionships"
TEAMS_URL <- "https://api.futmondo.com/2/championship/teams"
ROSTER_URL <- "https://api.futmondo.com/1/userteam/roster"
CLAUSULA_URL <- "https://api.futmondo.com/1/market/rosterclause"
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


  return(ret)
}


get_players_from_team <- function(login, championship_id, user_team_id, teams = NULL) {
  payload <- list(
    header = list(
      token = login[["token"]],
      userid = login[["userid"]]
    ),
    query = list(
      championshipId = championship_id,
      userteamId = user_team_id # ,
      # playerId = player_id
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
  return(roster)
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
  return(ret)
}

get_championship_players <- function(login, championship_id) {
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
  ret <- ret %>% dplyr::arrange(name)
  return(ret)
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
    browser()
    # deal with total
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
      print(paste0("Error in translating player positions: ", e))
      browser()
    }
  )
  if ("role2" %in% colnames(players_df)) {
    players_df$role2 <- position_map$role_to[match(players_df$role2, position_map$role_from)]
  }
  return(players_df)
}

calculate_player_changes <- function(players_df) {
  players_df <- players_df %>%
    dplyr::mutate(change_by_value = change / value * 100)
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

get_reactable_columns_for_players <- function(table) {
  columns <- list()
  if ("change" %in% colnames(table)) {
    change_col <- colDef(
      name = "Change",
      format = colFormat(currency = "EUR", separators = TRUE),
      # cell = function(value) {
      #   color <- ifelse(value > 0, "green", ifelse(value < 0, "red", "black"))
      #   span(style = paste("color:", color), value)
      # },
      align = "right"
    )
    columns[["change"]] <- change_col
  }
  if ("value" %in% colnames(table)) {
    value_col <- colDef(
      name = "Value",
      format = colFormat(currency = "EUR", separators = TRUE),
      align = "right",
      # make it wider
      width = 150
    )
    columns[["value"]] <- value_col
  }
  if ("change_by_value" %in% colnames(table)) {
    change_by_value_col <- colDef(
      name = "Change (%)",
      format = colFormat(digits = 1, percent = TRUE),
      align = "right"
    )
    columns[["change_by_value"]] <- change_by_value_col
  }
  if ("numberOfBids" %in% colnames(table)) {
    numberOfBids_col <- colDef(
      name = "Number of Bids",
      align = "right"
    )
    columns[["numberOfBids"]] <- numberOfBids_col
  }
  if ("role" %in% colnames(table)) {
    role_col <- colDef(
      name = "Position",
      align = "center"
    )
    columns[["role"]] <- role_col
  }
  if ("role2" %in% colnames(table)) {
    role2_col <- colDef(
      name = "Secondary Position",
      align = "center"
    )
    columns[["role2"]] <- role2_col
  }
  if ("market_price" %in% colnames(table)) {
    market_price_col <- colDef(
      name = "Market Price",
      format = colFormat(currency = "EUR", separators = TRUE),
      align = "right"
    )
    columns[["market_price"]] <- market_price_col
  }
  if ("bid_price" %in% colnames(table)) {
    bid_price_col <- colDef(
      name = "Bid Price",
      format = colFormat(currency = "EUR", separators = TRUE),
      align = "right"
    )
    columns[["bid_price"]] <- bid_price_col
  }
  if ("clause_price" %in% colnames(table)) {
    clause_price_col <- colDef(
      name = "Clause Price",
      format = colFormat(currency = "EUR", separators = TRUE),
      align = "right"
    )
    columns[["clause_price"]] <- clause_price_col
  }
  if ("clause_suggestedClause" %in% colnames(table)) {
    clause_col <- colDef(
      name = "Suggested Clause",
      format = colFormat(currency = "EUR", separators = TRUE),
      align = "right"
    )
    columns[["clause_suggestedClause"]] <- clause_col
  }
  if ("market_inMarket" %in% colnames(table)) {
    inMarket_col <- colDef(
      name = "In Market",
      align = "center"
    )
    columns[["market_inMarket"]] <- inMarket_col
  }
  if ("clause_date" %in% colnames(table)) {
    clause_date_col <- colDef(
      name = "Clause Date",
      align = "center",
      format = colFormat(date = TRUE)
    )
    columns[["clause_date"]] <- clause_date_col
  }
  return(columns)
}


get_teams <- function(login, championship_id) {
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
  teams <- httr::content(response)$answer$teams
  ret <- lapply(teams, FUN = function(team) {
    unlist(team) %>%
      t() %>%
      as.data.frame()
  }) %>% bind_rows()
  print(paste0(nrow(ret), " teams retrieved"))
  return(ret)
}


buy_clause <- function(login, championship_id, team_id, player_id, player_slug, price) {
  # {
  #   "header": {
  #     "token": "bf2d_ab97838fac849d1ed759e14ce440d637",
  #     "userid": "5b55fb19be298c4b5913fc44"
  #   },
  #   "query": {
  #     "championshipId": "5b55f9d767214483120b87cf",
  #     "userteamId": "5b55fbcbb78eda1f7593cb7f",
  #     "player_slug": "67245413",
  #     "player_id": "5fbe33c11fd5fa0e8491689f",
  #     "price": 6300065
  #   },
  #   "answer": {}
  # }
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
      price = price
    ),
    answer = list()
  )

  # Adding headers
  headers <- c(
    "Content-Type" = "application/json; charset=utf-8"
  )

  # Sending the POST request
  print("Getting players from team")
  response <- POST(CLAUSULA_URL, body = toJSON(payload), add_headers(.headers = headers))
  operation_code <- httr::content(response)$answer$code
  return(operation_code == API_CODE_OK)
}

get_team_image_name <- function(team) {
  team_image_name <- gsub(" ", "-", tolower(team))
  team_image_name <- gsub("r\\.", "real", tolower(team_image_name))
  return(team_image_name)
}
