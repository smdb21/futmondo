LOGIN_URL <- "https://api.futmondo.com/5/login/with_mail"
ACTIVE_CHAMPIONSHIPS_URL <- "https://api.futmondo.com/2/user/activechampionships"
TEAMS_URL <- "https://api.futmondo.com/2/championship/teams"
ROSTER_URL <- "https://api.futmondo.com/1/userteam/roster"
CLAUSULA_URL <- "https://api.futmondo.com/1/market/rosterclause"
MARKET_URL <- "https://api.futmondo.com/1/market/players"
PLAYER_SUMMARY_URL <- "https://api.futmondo.com/1/player/summary"
LINEUP_URL <- "https://api.futmondo.com/1/userteam/lineup"
CHAMPIONSHIP_PLAYERS <- "https://api.futmondo.com/5/league/championshipplayers"

API_CODE_OK <- "api.general.ok"
library(httr)
library(dplyr)
library(jsonlite)
library(data.table)
if (file.exists(".Renviron")) {
  readRenviron(".")
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
  response <- POST(LOGIN_URL, body = payload, encode = "json") %>% content()
  
  token <- response$answer$mobile$token
  userid <- response$answer$mobile$userid
  # Printing the response
  print(response)
  return(c("token" = token, "userid" = userid))
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
  ret <- content(response)$answer
  ret <- ret[["championships"]]
  if (!is.null(championship_name)) {
    ret <- ret[sapply(ret, FUN = function(championship) {
      championship$name == championship_name
    })]
    if (length(ret) == 0) {
      print("No championships found")
      return(NULL)
    }
    ret <- ret %>% unlist()
  }
  
  return(ret)
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
  teams <- content(response)$answer$teams
  ret <- lapply(teams, FUN = function(team) {
    unlist(team) %>%
      t() %>%
      as.data.frame()
  }) %>% bind_rows()
  print(paste0(nrow(ret), " teams retrieved"))
  return(ret)
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
  lineup <- content(response)
  lineup <- lineup[["answer"]]
  lineup_config <- lineup
  lineup_config$players <- NULL
  lineup_config$bench <- NULL
  lineup_config$custom<-NULL # to be parsed in the future
  lineup_config <- as.data.frame(t(unlist(lineup_config)))
  players_list <- lineup$players
  players <- lapply(players_list, FUN = function(player) {
    player <- unlist(player) %>%
      t() %>%
      as.data.frame()
  }) %>% bind_rows() %>%
    dplyr::arrange(position)
  
  budget <- lineup$budget
  bench_list <- lineup$bench
  bench <- lapply(bench_list$players, FUN = function(player) {
    player <- unlist(player) %>%
      t() %>%
      as.data.frame()
  }) %>% bind_rows() %>%
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
  roster <- content(response)
  roster <- roster[["answer"]]
  roster <- lapply(roster, FUN = function(player) {
    print(player$name)
    average <- player$average
    fitness <- average$fitness %>% unlist()
    if (length(fitness) > 0) { # because before starting the season this is empty
      names(fitness) <- paste("fitness", seq(1:length(fitness)))
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
    if ("bids" %in% names(market)) {
      bids <- market$bids
      if (length(bids) > 0) {
        bids <- lapply(bids, FUN = function(bid) {
          data.frame(bid_price = bid$price, bid_user = bid$userTeam[['name']])
          
        }) %>% rbindlist(fill = T)
        # make it a single row
        bids <- bids %>%
          dplyr::mutate(bid_price = paste0(bid_price, collapse = ","),
                        bid_user = paste0(bid_user, collapse = ","))
      }
      market <- market[-which(names(market) == "bids")]
    }
    names(market) <- paste0("market_", names(market))
    # remove market element from player list
    player <- player[-which(names(player) == "market")]
    
    player <- c(player, average, fitness, clause, market)
    
    if (!is.null(bids)) {
    player <- c(player, bids)
    }
    player
  }) %>% rbindlist(fill = T)
  
  # add championship_id, user_team_id
  roster$championship_id <- championship_id
  roster$user_team_id <- user_team_id
  if (!is.null(teams)) {
    roster <- roster %>%
      dplyr::left_join(teams %>% dplyr::distinct(id, teamname), by = c("user_team_id" = "id"))
  }
  return(roster)
}

comprar_clausula <- function(login, championship_id, team_id, player_id, player_slug, price) {
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
  operation_code <- content(response)$answer$code
  return(operation_code == API_CODE_OK)
}

# get players in the market
get_players_in_market <- function(login, championship_id, user_team_id) {
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
  players <- content(response)$answer
  if (is.null(players)) {
    html_error <- content(response)
    # show error:
    stop(paste0("Error in request: ", html_error))
  }
  ret <- lapply(players, FUN = function(player) {
    player <- parse_player_json(player = player)
    player
  }) %>% rbindlist(fill = T)
  
  ret <- ret %>% dplyr::arrange(desc(change))
  return(ret)
}
parse_player_json <- function(player) {
  # player <- assign_names_recursive(lst = player, parent_name = "")
  average <- player$average
  average <- remove_json_children(json = average, element_name = "fitness", collapse_children = TRUE)
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

get_player_summary <- function(login, championship_id, player_id, merge_with_points = FALSE, merge_with_prices = FALSE) {
  # either one or the other
  if (merge_with_prices && merge_with_points) {
    stop("You can't merge with points and prices at the same time")
  }
  payload <- list(
    header = list(
      token = login[["token"]],
      userid = login[["userid"]]
    ),
    query = list(
      championshipId = championship_id,
      playerId = player_id
    ),
    answer = list()
  )
  
  # Adding headers
  headers <- c(
    "Content-Type" = "application/json; charset=utf-8"
  )
  
  # Sending the POST request
  response <- POST(PLAYER_SUMMARY_URL, body = toJSON(payload), add_headers(.headers = headers))
  answer <- content(response)$answer
  # player data has general information about the player
  player <- parse_player_json(player = answer$data)
  if (merge_with_prices) {
    print(paste0("Getting player summary from '", unique(player$name), "' and merging with evolution of prices"))
    # answer$prices has the evolution of prices
    prices <- answer$prices %>% rbindlist(fill = T)
    colnames(prices) <- paste0("prices.", colnames(prices))
    prices$id <- player$id
    player <- player %>% dplyr::left_join(prices, by = "id")
  } else if (merge_with_points) {
    print(paste0("Getting player summary from '", unique(player$name), "' and merging with evolution of points"))
    # answer$points has the evolution of points
    points <- answer$points %>% rbindlist(fill = T)
    colnames(points) <- paste0("points.", colnames(points))
    points$id <- player$id
    player <- player %>% dplyr::left_join(points, by = "id")
  } else {
    print(paste0("Getting player summary from '", unique(player$name), "'"))
  }
  
  
  return(player)
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
  browser()
  players <- content(response)$answer$players
  player <- players[[2]]
  ret <- lapply(players, FUN = function(player) {
    player <- parse_player_json(player = player)
    player
  }) %>% rbindlist(fill = T)
  
  ret <- ret %>% dplyr::arrange(name)
  return(ret)
}

login <- login()
championship <- get_championships(login, "OHY CAMPEÓN ")

championship_id <- championship["id"]
championship_name <- championship["name"]
userteam <- championship["userteam.id"]
teams <- get_teams(login = login, championship_id = championship_id)
team <- teams %>% dplyr::filter(grepl("Salva", name))
roster <- get_players_from_team(login = login, championship_id = championship_id, user_team_id = team$id, teams = teams)
lineup <- get_lineup_from_team(login = login, championship_id = championship_id, user_team_id = team$id)
library(tictoc)
tic()
all_players <- apply(teams, MARGIN = 1, FUN = function(team) {
  roster <- get_players_from_team(login = login, championship_id = championship_id, user_team_id = team["id"], teams = teams)
}) %>% rbindlist(fill = T)
toc()


market <- get_players_in_market(login = login, championship_id = championship_id, user_team_id = team$id)
player <- get_player_summary(login = login, championship_id = championship_id, player_id = market$id[1], merge_with_points = FALSE, merge_with_prices = TRUE)

all_players <- get_championship_players(login = login, championship_id = championship_id)
# format date
player$prices.date <- as.Date(player$prices.date, format = "%Y-%m-%d")
player %>%
  plotly::plot_ly(x = ~ player$prices.date, y = ~ player$prices.price, type = "scatter", mode = "lines+markers", name = "lines+markers") %>%
  plotly::layout(title = "Evolution of player price", xaxis = list(title = "Date"), yaxis = list(title = "Price"))
