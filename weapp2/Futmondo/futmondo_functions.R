source("data_contracts.R")
LOGIN_URL <- "https://api.futmondo.com/5/login/with_mail"
ACTIVE_CHAMPIONSHIPS_URL <- "https://api.futmondo.com/2/user/activechampionships"
TEAMS_URL <- "https://api.futmondo.com/2/championship/teams"
ROSTER_URL <- "https://api.futmondo.com/1/userteam/roster"
DREAMTEAM_URL <- "https://api.futmondo.com/1/userteam/dreamteam"
ROUNDS_URL <- "https://api.futmondo.com/1/userteam/rounds"
BID_URL <- "https://api.futmondo.com/1/market/bid"
MARKET_URL <- "https://api.futmondo.com/1/market/players"
PLAYER_SUMMARY_URL <- "https://api.futmondo.com/1/player/summary"
MODIFY_BID_URL <- "https://api.futmondo.com/5/market/modifybid"
PRESSROOM_URL <- "https://api.futmondo.com/1/locker/pressroom"
CANCEL_BID_URL <- "https://api.futmondo.com/1/market/cancelbid"
PUT_ON_MARKET_URL <- "https://api.futmondo.com/1/market/putonmarket"
CANCEL_SELL_URL <- "https://api.futmondo.com/1/market/cancelsell"
PUT_ALL_ON_MARKET_URL <- "https://api.futmondo.com/5/market/putallonmarket"
MY_PLAYERS_URL <- "https://api.futmondo.com/1/market/myplayers"
ROSTER_BIDS_URL <- "https://api.futmondo.com/1/market/rosterbids"
ROSTER_CLAUSE_URL <- "https://api.futmondo.com/1/market/rosterclause"
ACCEPT_BID_URL <- "https://api.futmondo.com/1/market/acceptbid"
REJECT_BID_URL <- "https://api.futmondo.com/1/market/rejectbid"
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

api_cache_key <- function(key, login = NULL, scope = NULL) {
  if (is.null(scope) && valid_login(login)) scope <- as.character(login[["userid"]])
  if (is.null(scope) && requireNamespace("shiny", quietly = TRUE)) {
    domain <- shiny::getDefaultReactiveDomain()
    if (!is.null(domain)) scope <- domain$userData$futmondo_user_id
  }
  if (is.null(scope) || !nzchar(scope)) key else paste0("account:", scope, ":", key)
}

get_cached_data <- function(key, expr, timeout_sec = 300) {
  caller <- parent.frame()
  auth <- get0("login", envir = caller, inherits = FALSE)
  key <- api_cache_key(key, auth)
  now <- Sys.time()
  cached <- api_cache_env[[key]]
  if (!is.null(cached) && as.numeric(difftime(now, cached$time, units = "secs")) < timeout_sec)
    return(cached$data)
  tryCatch({
    data <- force(expr)
    if (is.null(data)) stop("Data unavailable")
    data <- observed_data(data, status=attr(data,"fetch_status"),
      observed_at=attr(data,"observed_at") %||% now)
    api_cache_env[[key]] <- list(data = data, time = now)
    data
  }, error = function(e) {
    if (!is.null(cached)) {
      fallback <- observed_data(cached$data,status="stale",
        observed_at=attr(cached$data,"observed_at") %||% cached$time)
      if(is.list(fallback) && !is.data.frame(fallback) && identical(fallback$status,"ok")) {
        fallback$status <- "partial"
        if("spendable_budget" %in% names(fallback)) fallback$spendable_budget <- NA_real_
        if(is.list(fallback$funds)) fallback$funds$spendable_budget <- NA_real_
      }
      return(fallback)
    }
    stop(structure(list(message = "Requested data is unavailable. Please retry.", call = NULL),
                   class = c("futmondo_unavailable", "error", "condition")))
  })
}

clear_api_cache <- function(user_id = NULL) {
  if (is.null(user_id) && requireNamespace("shiny", quietly = TRUE)) {
    domain <- shiny::getDefaultReactiveDomain()
    if (!is.null(domain)) user_id <- domain$userData$futmondo_user_id
  }
  keys <- ls(envir = api_cache_env)
  if (!is.null(user_id)) keys <- keys[startsWith(keys, paste0("account:", user_id, ":"))]
  if (length(keys)) rm(list = keys, envir = api_cache_env)
  invisible(TRUE)
}

# Only read endpoints may retry. Mutations require reconciliation after an
# uncertain response, so a network timeout must never replay a trade.
futmondo_post <- function(url, ...) {
  if (isTRUE(getOption("futmondo.offline", FALSE))) stop("Network disabled in offline mode")
  read_routes <- c("information", "activechampionships", "teams", "roster", "rounds",
    "dreamteam", "nightmareteam", "lineup", "championshipplayers", "championshipteams",
    "players", "summary", "pressroom", "moneymovements", "rosterbids", "myplayers",
    "list", "unread", "getdtconfig")
  route <- tail(strsplit(url, "/", fixed = TRUE)[[1]], 1)
  attempts <- if (route %in% read_routes) 2L else 1L
  for (i in seq_len(attempts)) {
    response <- tryCatch(POST(url, ..., httr::timeout(15),
      httr::config(connecttimeout = 5)), error = function(e) NULL)
    if (!is.null(response)) {
      code <- httr::status_code(response)
      if (code >= 200 && code < 300) return(response)
      if (!(code %in% c(429L, 502L, 503L, 504L))) break
    }
  }
  stop("Futmondo request unavailable; no operation has been retried unless it was a read.")
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
    response <- futmondo_post("https://api.futmondo.com/1/league/championshipteams", body = toJSON(payload), add_headers(.headers = headers))
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

login <- function(user_name = NULL, password = NULL) {
  if (is.null(user_name) || is.null(password) || !nzchar(trimws(user_name)) || !nzchar(password))
    stop("Enter your email and password.")
  payload <- list(header = list(token = "null", userid = ""),
                  query = list(mail = trimws(user_name), pwd = password))
  response <- futmondo_post(LOGIN_URL, body = payload, encode = "json")
  answer <- httr::content(response)$answer$mobile
  result <- c(token = answer$token, userid = answer$userid, user_name = trimws(user_name))
  if (!valid_login(result)) stop("Authentication failed.")
  result
}

get_championships <- function(login, championship_name = NULL) {
  cache_key <- paste0("championships_", login[["userid"]], "_", championship_name %||% "all")
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
    response <- futmondo_post(ACTIVE_CHAMPIONSHIPS_URL, body = toJSON(payload), add_headers(.headers = headers))
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


get_finished_rounds <- function(login, championship_id) {
  tryCatch({
    if (is.null(login) || is.null(championship_id)) {
      return(data.frame(
        round_id = character(0), round_number = numeric(0),
        begin_process = character(0), is_finished = logical(0),
        stringsAsFactors = FALSE
      ))
    }

    cache_key <- paste0("finished_rounds_", championship_id)
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

      headers <- c("Content-Type" = "application/json; charset=utf-8")

      print(paste0("[API] Fetching active championships for rounds (championship: ", championship_id, ")"))
      response <- futmondo_post(ACTIVE_CHAMPIONSHIPS_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
      ans <- httr::content(response)

      if (is.null(ans) || !("answer" %in% names(ans)) || is.null(ans$answer) || !("championships" %in% names(ans$answer))) {
        print("[Rounds] No championships data in response.")
        return(data.frame(
          round_id = character(0), round_number = numeric(0),
          begin_process = character(0), is_finished = logical(0),
          stringsAsFactors = FALSE
        ))
      }

      championships <- ans$answer$championships
      champ <- NULL
      for (c_item in championships) {
        if (is.list(c_item) && !is.null(c_item[["id"]] %||% c_item[["_id"]]) && as.character(c_item[["id"]] %||% c_item[["_id"]]) == as.character(championship_id)) {
          champ <- c_item
          break
        }
      }

      if (is.null(champ)) {
        print(paste0("[Rounds] Championship ", championship_id, " not found in active championships."))
        return(data.frame(
          round_id = character(0), round_number = numeric(0),
          begin_process = character(0), is_finished = logical(0),
          stringsAsFactors = FALSE
        ))
      }

      # Extract rounds from ans$answer$rounds
      rounds <- if (!is.null(ans$answer) && "rounds" %in% names(ans$answer)) ans$answer$rounds else NULL

      # If ans$answer$rounds is empty, fallback to champ$rounds
      if (is.null(rounds) || length(rounds) == 0) {
        rounds <- if (!is.null(champ) && "rounds" %in% names(champ)) champ$rounds else NULL
      }

      # If championship has a league ID, filter rounds by matching round$championshipId to champ$league
      if (!is.null(rounds) && length(rounds) > 0 && !is.null(champ) && !is.null(champ[["league"]])) {
        league_id <- as.character(champ[["league"]])
        rounds <- Filter(function(r) {
          if (!is.null(r[["championshipId"]])) {
            as.character(r[["championshipId"]]) == league_id
          } else {
            FALSE
          }
        }, rounds)
      }

      if (is.null(rounds) || length(rounds) == 0) return(data.frame(
        round_id = character(), round_number = numeric(), begin_process = character(),
        is_finished = logical(), stringsAsFactors = FALSE))

      now <- Sys.time()
      rounds_df <- lapply(rounds, FUN = function(r) {
        r_id <- if (!is.null(r[["_id"]])) as.character(r[["_id"]]) else if (!is.null(r[["id"]])) as.character(r[["id"]]) else ""
        r_num <- if (!is.null(r[["number"]])) as.numeric(r[["number"]]) else 1
        begin_proc <- if (!is.null(r[["beginProcess"]])) as.character(r[["beginProcess"]]) else ""

        is_fin <- isTRUE(r$isFinished) || isTRUE(r$finished) ||
          (!is.null(r$status) && r$status %in% c("finished", "closed", "final"))

        data.frame(
          round_id = r_id,
          round_number = r_num,
          begin_process = begin_proc,
          is_finished = is_fin,
          stringsAsFactors = FALSE
        )
      }) %>% bind_rows()

      print(paste0("[Rounds] Found ", nrow(rounds_df), " rounds, ", sum(rounds_df$is_finished), " finished."))
      rounds_df
    })
  }, error = function(e) {
    print(paste0("[Rounds] Error: ", e$message))
    data.frame(
      round_id = character(0), round_number = numeric(0),
      begin_process = character(0), is_finished = logical(0),
      stringsAsFactors = FALSE
    )
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

    response <- futmondo_post(ROSTER_URL, body = toJSON(payload), add_headers(.headers = headers))
    roster <- httr::content(response)
    roster <- roster[["answer"]]
    if (is.list(roster) && (!is.null(roster$code) || isTRUE(roster$error))) stop("Roster unavailable")
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
      average <- normalize_player_average(player$average)
      clause <- player$clause
      if (length(clause)) names(clause) <- paste0("clause_", names(clause))
      player$average <- NULL
      player$clause <- NULL
      ## market ----
      market <- player$market
      ### bids ----
      bids <- NULL
      if (is.list(market)) {
        if ("bids" %in% names(market)) {
          bids <- market$bids
          if (length(bids) > 0) {
            bids_df <- lapply(bids, FUN = function(bid) {
              b_price <- if (!is.null(bid$price)) suppressWarnings(as.numeric(bid$price)) else 0
              b_user <- if (!is.null(bid$userTeam) && is.list(bid$userTeam) && !is.null(bid$userTeam[["name"]]) && bid$userTeam[["name"]] != "") as.character(bid$userTeam[["name"]]) else "Futmondo"
              b_id <- if (!is.null(bid[["id"]])) as.character(bid[["id"]]) else if (!is.null(bid[["_id"]])) as.character(bid[["_id"]]) else ""
              data.frame(bid_price = b_price, bid_user = b_user, bid_id = b_id, stringsAsFactors = FALSE)
            }) %>% rbindlist(fill = TRUE) %>% as.data.frame()

            if (nrow(bids_df) > 0) {
              max_idx <- which.max(bids_df$bid_price)
              bids <- list(
                bid_price = bids_df$bid_price[max_idx],
                bid_user = bids_df$bid_user[max_idx],
                bid_id = bids_df$bid_id[max_idx]
              )
            } else {
              bids <- NULL
            }
          }
          market <- market[-which(names(market) == "bids")]
        }
        names(market) <- paste0("market_", names(market))
      }
      # remove market element from player list
      player$market <- NULL
      player <- c(player, average, clause, market)

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

    # Join active roster bids if present
    roster_bids_df <- tryCatch({
      get_roster_bids(login = login, championship_id = championship_id, user_team_id = user_team_id)
    }, error = function(e) NULL)

    if (!is.null(roster_bids_df) && nrow(roster_bids_df) > 0 && "id" %in% colnames(roster_bids_df) && "id" %in% colnames(roster)) {
      roster <- roster %>% dplyr::select(!any_of(c("bid_price", "bid_user", "bid_id")))
      roster <- roster %>% dplyr::left_join(roster_bids_df, by = "id")
    }

    # Join market listed status if present
    my_mkt_df <- tryCatch({
      get_my_market_players(login = login, championship_id = championship_id, user_team_id = user_team_id)
    }, error = function(e) NULL)

    if (!is.null(my_mkt_df) && nrow(my_mkt_df) > 0 && "id" %in% colnames(my_mkt_df) && "id" %in% colnames(roster)) {
      mkt_ids <- as.character(my_mkt_df$id)
      roster$market_inMarket <- as.character(roster$id) %in% mkt_ids

      if ("price" %in% colnames(my_mkt_df)) {
        mkt_prices <- my_mkt_df %>% dplyr::select(id, market_asking_price = price) %>% dplyr::distinct(id, .keep_all = TRUE)
        roster <- roster %>% dplyr::left_join(mkt_prices, by = "id")
        if (!"effective_market_price" %in% colnames(roster)) roster$effective_market_price <- NA_real_
        roster$effective_market_price <- ifelse(!is.na(roster$market_asking_price) & roster$market_asking_price > 0, roster$market_asking_price, roster$effective_market_price)
      }
    }

    roster
  }, timeout_sec = 30)
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
    response <- futmondo_post(MARKET_URL, body = toJSON(payload), add_headers(.headers = headers))
    players <- httr::content(response)$answer
    if (!is.list(players) || !is.null(players$code)) stop("Market data unavailable")
    if (!length(players)) return(data.frame())
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
    response <- futmondo_post(CHAMPIONSHIP_PLAYERS, body = toJSON(payload), add_headers(.headers = headers))
    players <- httr::content(response)$answer$players
    if (!is.list(players)) stop("Player catalog unavailable")
    if (!length(players)) return(data.frame())
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


# Flatten observed point statistics into the same columns for every endpoint.
normalize_player_average <- function(average) {
  if (!is.list(average) || !length(average)) return(list())
  fitness <- average$fitness
  average$fitness <- NULL
  if (!is.null(fitness)) {
    scores <- vapply(as.list(fitness), fm_number, numeric(1))
    average$fitness <- paste(scores, collapse = ",")
    if (length(scores) && all(is.finite(scores))) {
      average$total <- sum(scores)
      if (!is.finite(fm_number(average$averageLastFive))) average$averageLastFive <- mean(scores)
    }
  }
  names(average) <- paste0("average.", names(average))
  average
}

parse_player_json <- function(player) {
  average <- normalize_player_average(player$average)
  player$average <- NULL
  player <- c(player, average)
  # clause
  if ("clause" %in% names(player)) {
    clause <- player$clause
    if (length(clause)) names(clause) <- paste0("clause_", names(clause))
    player$clause <- NULL
    player <- c(player, clause)
  }
  if ("total" %in% names(player)) {
    warning("Found 'total' directly in player object. Handling it.")
    player <- remove_json_children(json = player, element_name = "total")
  }
  if ("bid" %in% names(player)) {
    bid <- player$bid
    if (length(bid)) names(bid) <- paste0("bid_", names(bid))
    player$bid <- NULL
    player <- c(player, bid)
  }
  player <- player[!is.na(names(player)) & names(player) != ""]
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

  if ("role" %in% names(players_df) && !"primary_role" %in% names(players_df)) players_df$primary_role <- players_df$role
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
  get_cached_data(paste0("lineup_", championship_id, "_", user_team_id), {
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

  response <- futmondo_post(LINEUP_URL, body = toJSON(payload), add_headers(.headers = headers))
  lineup <- httr::content(response)
  lineup <- lineup[["answer"]]
  if (!is.list(lineup) || !is.null(lineup$code)) stop("Lineup unavailable")
  parse_slots <- function(items) {
    if (is.null(items) || !length(items)) return(data.frame(id=character(),position=character()))
    d <- dplyr::bind_rows(lapply(items,function(p)as.data.frame(as.list(unlist(p)),stringsAsFactors=FALSE)))
    if ("position" %in% names(d)) d <- d[order(suppressWarnings(as.numeric(d$position))),,drop=FALSE]
    d
  }
  config <- lineup[setdiff(names(lineup),c("players","bench","custom"))]
  bench_config <- lineup$bench %||% list()
  bench_players <- parse_slots(bench_config$players); bench_config$players <- NULL
  lineup <- list(lineup_config=config,custom=lineup$custom,multiposition=lineup$multiposition,
    pointSystem=lineup$pointSystem,players=parse_slots(lineup$players),budget=lineup$budget,
    bench=list(config=bench_config,players=bench_players))
  return(lineup)
  }, timeout_sec = 30)
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
    "name", "role", "team", "market_inMarket", "bid_price", "bid_user", "numberOfBids",
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

  # Your Bid / Received Offer Column ----
  if ("bid_price" %in% colnames(table)) {
    columns[["bid_price"]] <- colDef(
      name = "Received Offer",
      align = "right",
      cell = function(value, index) {
        if (is.na(value) || is.null(value) || value == "" || value == 0) return("")
        num_val <- suppressWarnings(as.numeric(value))
        formatted <- format_table_currency(num_val)

        bidder <- if ("bid_user" %in% colnames(table) && !is.na(table$bid_user[index]) && as.character(table$bid_user[index]) != "") {
          as.character(table$bid_user[index])
        } else {
          "Futmondo"
        }

        shiny::tags$span(
          class = "badge-active-bid",
          title = paste0("Offer from ", bidder),
          formatted
        )
      }
    )
  }

  if ("bid_user" %in% colnames(table)) {
    columns[["bid_user"]] <- colDef(show = FALSE)
  }

  # My Bid Price Column ----
  if ("my_bid_price" %in% colnames(table)) {
    columns[["my_bid_price"]] <- colDef(
      name = "My Offer",
      align = "right",
      cell = function(value) {
        if (is.na(value) || is.null(value) || value <= 0) return("")
        shiny::tags$span(
          class = "badge-my-bid",
          style = "background: var(--fm-surface); color: var(--fm-text); font-weight: 600; padding: 3px 8px; border-radius: 12px;",
          format_table_currency(value)
        )
      }
    )
  }

  # Highest Other Bid Price Column ----
  if ("highest_other_bid_price" %in% colnames(table) || "other_bid_price" %in% colnames(table)) {
    bid_col_name <- if ("highest_other_bid_price" %in% colnames(table)) "highest_other_bid_price" else "other_bid_price"
    columns[[bid_col_name]] <- colDef(
      name = "Offers by Others",
      align = "right",
      cell = function(value, index) {
        if (is.na(value) || is.null(value) || value <= 0) return("")
        bidder <- if ("other_bid_user" %in% colnames(table) && !is.na(table$other_bid_user[index]) && nzchar(as.character(table$other_bid_user[index]))) {
          as.character(table$other_bid_user[index])
        } else {
          "Futmondo"
        }
        shiny::tags$span(
          class = "badge-active-bid",
          title = paste0("Offer from ", bidder),
          format_table_currency(value)
        )
      }
    )
  }

  if ("other_bid_user" %in% colnames(table)) {
    columns[["other_bid_user"]] <- colDef(show = FALSE)
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

  if ("effective_market_price" %in% colnames(table)) {
    columns[["effective_market_price"]] <- colDef(
      name = "Effective Market Price",
      align = "right",
      cell = function(value) {
        if (is.null(value) || is.na(value) || value == 0) return("")
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

  if ("role" %in% colnames(table)) {
    columns[["role"]] <- colDef(
      name = "Position",
      align = "center",
      minWidth = 170,
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

  if ("V1" %in% colnames(table)) {
    columns[["V1"]] <- colDef(show = FALSE)
  }

  if ("market_inMarket" %in% colnames(table)) {
    columns[["market_inMarket"]] <- colDef(
      name = "In market",
      align = "right",
      minWidth = 150,
      cell = function(value, index) {
        if (is.null(value) || is.na(value) || !isTRUE(as.logical(value))) return("")

        asking_price <- NA_real_
        if ("effective_market_price" %in% colnames(table)) {
          asking_price <- suppressWarnings(as.numeric(table$effective_market_price[index]))
        } else if ("market_price" %in% colnames(table)) {
          asking_price <- suppressWarnings(as.numeric(table$market_price[index]))
        } else if ("price" %in% colnames(table)) {
          asking_price <- suppressWarnings(as.numeric(table$price[index]))
        }

        if (!is.na(asking_price) && asking_price > 0) {
          shiny::tags$span(class = "badge-market-listed", format_table_currency(asking_price))
        } else {
          shiny::tags$span(class = "badge-market-listed", "YES")
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
          return(shiny::tags$span(style = "color: var(--fm-text); font-weight: 500;", shiny::icon("circle-check"), " Fit"))
        }
        val_lower <- tolower(value)
        if (val_lower == "ok") {
          shiny::tags$span(style = "color: var(--fm-text); font-weight: 500;", shiny::icon("circle-check"), " Fit")
        } else if (val_lower == "doubt") {
          shiny::tags$span(style = "color: var(--fm-warning); font-weight: 500;", shiny::icon("triangle-exclamation"), " Doubt")
        } else if (val_lower == "injured") {
          shiny::tags$span(style = "color: var(--fm-danger); font-weight: 500;", shiny::icon("circle-minus"), " Injured")
        } else if (val_lower == "injured2") {
          shiny::tags$span(style = "color: var(--fm-text); font-weight: 500;", shiny::icon("hospital"), " Long-term")
        } else if (val_lower == "redcard") {
          shiny::tags$span(style = "color: var(--fm-danger); font-weight: 500;", shiny::icon("square"), " Suspended")
        } else {
          shiny::tags$span(style = "color: var(--fm-text); font-weight: 500;", shiny::icon("circle-check"), " Fit")
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
          shiny::tags$span(style = "color: var(--fm-warning); font-weight: 600;", "GOOD VALUE")
        } else {
          shiny::tags$span(style = "color: var(--fm-text);", "OVERPRICED")
        }
      }
    )
  }

  # FIS Score Column ----
  if ("fis_score" %in% colnames(table)) {
    columns[["fis_score"]] <- colDef(
      name = "FIS",
      align = "center",
      minWidth = 140,
      cell = function(value, index) {
        if (is.na(value) || is.null(value)) return("")

        tier <- if ("fis_tier" %in% colnames(table) && !is.na(table$fis_tier[index])) {
          as.character(table$fis_tier[index])
        } else {
          # Derive tier from score if fis_tier column is missing
          if (value >= 80) "Strong Buy"
          else if (value >= 65) "Buy"
          else if (value >= 45) "Hold"
          else "Sell"
        }

        # Color mapping per tier
        bg_color <- if (tier == "Strong Buy") {
          "#dcfce7"
        } else if (tier == "Buy") {
          "#e0f2fe"
        } else if (tier == "Hold") {
          "#fef3c7"
        } else {
          "#fee2e2"
        }

        text_color <- if (tier == "Strong Buy") {
          "#166534"
        } else if (tier == "Buy") {
          "#0369a1"
        } else if (tier == "Hold") {
          "#92400e"
        } else {
          "#991b1b"
        }

        # Build tooltip from fis_summary if available
        tooltip_text <- ""
        if ("fis_summary" %in% colnames(table) && !is.na(table$fis_summary[index]) && nzchar(as.character(table$fis_summary[index]))) {
          tooltip_text <- as.character(table$fis_summary[index])
        }

        shiny::tags$span(
          class = "badge-fis-score",
          title = tooltip_text,
          style = paste0(
            "background: ", bg_color, "; color: ", text_color, "; font-weight: 700; font-size: 11px; padding: 3px 8px; border-radius: 12px; white-space: nowrap; display: inline-block;"
          ),
          shiny::tags$span(
            style = "margin-right: 4px;",
            round(value, 1)
          ),
          shiny::tags$span(
            style = "font-weight: 600;",
            tier
          )
        )
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
    response <- futmondo_post(TEAMS_URL, body = toJSON(payload), add_headers(.headers = headers))
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
      championshipId = as.character(championship_id),
      userteamId = as.character(team_id),
      player_id = as.character(player_id),
      player_slug = as.character(player_slug),
      price = as.numeric(price),
      isClause = as.logical(isClause)
    ),
    answer = list()
  )

  headers <- c(
    "Content-Type" = "application/json; charset=utf-8"
  )

  print(paste0("[API] Sending bid/clause request for player: ", player_id, " price: ", price, " isClause: ", isClause))
  response <- futmondo_post(BID_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
  ans <- httr::content(response)

  operation_code <- if (!is.null(ans) && "answer" %in% names(ans) && "code" %in% names(ans$answer)) ans$answer$code else ""
  err_msg <- if (!is.null(ans) && "answer" %in% names(ans) && "msg" %in% names(ans$answer)) ans$answer$msg else if (!is.null(ans) && "answer" %in% names(ans) && "message" %in% names(ans$answer)) ans$answer$message else operation_code

  is_success <- (operation_code == API_CODE_OK)

  if (!is_success) {
    print(paste0("[API] Bid request failed. Code: ", operation_code, " Msg: ", err_msg))
  } else {
    print("[API] Bid request succeeded (api.general.ok)")
  }

  return(list(
    success = is_success,
    code = operation_code,
    message = err_msg
  ))
}

# ============================================================
# Roster Clause Buyout (dedicated endpoint)
# ============================================================
# Builds the exact JSON payload for the dedicated roster clause
# buyout endpoint. The payload serializes exactly:
#   header{token, userid},
#   query{championshipId, userteamId, player_slug, player_id, price},
#   answer{}
# NOTE: isClause is intentionally NOT part of this payload (the
# endpoint itself implies a clause purchase).
build_roster_clause_payload <- function(login, championship_id, team_id, player_id, player_slug, price) {
  list(
    header = list(
      token = login[["token"]],
      userid = login[["userid"]]
    ),
    query = list(
      championshipId = as.character(championship_id),
      userteamId = as.character(team_id),
      player_slug = as.character(player_slug),
      player_id = as.character(player_id),
      price = as.numeric(price)
    ),
    answer = list()
  )
}

# Executes a release-clause buyout via POST /1/market/rosterclause.
# The entire request is wrapped in tryCatch() so a network failure can
# never block the user thread or crash the parent server.
#
# Parameters:
#   login           -- login token vector (token, userid, user_name)
#   championship_id -- character championship ID
#   team_id         -- character, the buying (logged-in) team ID
#   player_id       -- character player ID
#   player_slug     -- character player slug
#   price           -- numeric clause price in EUR
#   url             -- endpoint URL (default ROSTER_CLAUSE_URL); overridable
#                      for tests/mocks
#
# Returns:
#   list(success = logical, code = character, message = character)
buy_roster_clause <- function(login, championship_id, team_id, player_id, player_slug, price,
                              url = ROSTER_CLAUSE_URL) {
  tryCatch({
    payload <- build_roster_clause_payload(
      login = login,
      championship_id = championship_id,
      team_id = team_id,
      player_id = player_id,
      player_slug = player_slug,
      price = price
    )

    headers <- c("Content-Type" = "application/json; charset=utf-8")

    print(paste0("[API] Sending roster clause buyout for player: ", player_id,
                 " price: ", price))
    response <- futmondo_post(url, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
    ans <- httr::content(response)

    operation_code <- if (!is.null(ans) && "answer" %in% names(ans) && "code" %in% names(ans$answer)) ans$answer$code else ""
    err_msg <- if (!is.null(ans) && "answer" %in% names(ans) && "msg" %in% names(ans$answer)) ans$answer$msg else if (!is.null(ans) && "answer" %in% names(ans) && "message" %in% names(ans$answer)) ans$answer$message else operation_code

    is_success <- (operation_code == API_CODE_OK)

    if (!is_success) {
      print(paste0("[API] Roster clause request failed. Code: ", operation_code, " Msg: ", err_msg))
    } else {
      print("[API] Roster clause request succeeded (api.general.ok)")
    }

    list(success = is_success, code = operation_code, message = err_msg)
  }, error = function(e) {
    print(paste0("[API] Roster clause request error: ", e$message))
    list(success = FALSE, code = "error", message = e$message)
  })
}

# ============================================================
# Acquisition Capacity (verified roster + funds + outstanding bids)
# ============================================================
# Extracts the immutable bidder team ID from a bid object. Used to
# normalize "my own" active bids by team ID (never by name or list order).
extract_bidder_team_id <- function(bid) {
  if (!is.list(bid)) return(NA_character_)
  ut <- bid[["userTeam"]]
  if (is.list(ut)) {
    if (!is.null(ut[["_id"]])) return(as.character(ut[["_id"]]))
    if (!is.null(ut[["id"]])) return(as.character(ut[["id"]]))
  }
  for (k in c("userteamId", "userTeamId", "userteam_id", "userTeam_id", "bidder_team_id")) {
    if (!is.null(bid[[k]])) return(as.character(bid[[k]]))
  }
  NA_character_
}

# Computes a verified acquisition-capacity snapshot for the logged-in team:
# roster count/cap, verified spendable funds, and outstanding active bids.
# Cached per (userid, championship, team, target player).
#
# Parameters:
#   login             -- login token vector (token, userid, user_name)
#   championship_id   -- character championship ID
#   user_team_id      -- character, the logged-in team ID
#   target_player_id  -- optional character player ID; when provided, the
#                        target block reports that player's own bid, the
#                        highest competing bid, and the bid count.
#
# Returns a structured list:
#   status      -- "ok" | "partial" | "unavailable". Status is "ok" only when
#                  roster cap, roster count, reported budget, outstanding bids,
#                  AND a valid (finite, non-negative) withheld are all present.
#                  A missing / non-finite / negative withheld makes funds
#                  verification incomplete -> status "partial" (fail closed; we
#                  never assume withheld = 0).
#   roster      -- list(count, cap, remaining_slots)
#   funds       -- list(reported_budget, withheld, team_value, debt_limit,
#                  minimum_balance, spendable_budget). Spendable funds include
#                  temporary borrowing down to -50% of verified team value.
#                  withheld is not valid (funds verification incomplete).
#   outstanding -- list(offers, count, total_amount, completeness)
#   target      -- list(my_bid_id, my_bid_amount, highest_bid, bid_count)
#   diagnostics -- character vector of data-availability notes
get_acquisition_capacity <- function(login, championship_id, user_team_id, target_player_id = NULL) {
  empty_target <- list(my_bid_id = NULL, my_bid_amount = NA_real_,
                       highest_bid = NA_real_, bid_count = NA_integer_)

  if (is.null(login) || is.null(championship_id) || is.null(user_team_id) ||
      is.null(login[["userid"]])) {
    return(list(
      status = "unavailable",
      roster = list(count = NA_integer_, cap = NA_integer_, remaining_slots = NA_integer_),
      funds = list(reported_budget = NA_real_, withheld = NA_real_, team_value = NA_real_,
        debt_limit = NA_real_, minimum_balance = NA_real_, projected_committed_balance = NA_real_,
        api_bid_limit = NA_real_, spendable_budget = NA_real_),
      outstanding = list(offers = NA_integer_, count = NA_integer_,
                         total_amount = NA_real_, completeness = "unavailable"),
      target = empty_target,
      diagnostics = c("login, championship_id, or user_team_id is missing")
    ))
  }

  target_key <- if (is.null(target_player_id) || !nzchar(as.character(target_player_id))) {
    "all"
  } else {
    as.character(target_player_id)
  }

  cache_key <- paste0("acq_capacity_", login[["userid"]], "_", championship_id,
                      "_", user_team_id, "_", target_key)

  get_cached_data(cache_key, {
    diagnostics <- character(0)

    # ---- 1. Team info: roster cap, reported budget, withheld ----
    cap_val <- NA_integer_
    budget_val <- NA_real_
    team_value_val <- NA_real_
    withheld_val <- NA_real_
    withheld_valid <- FALSE
    info <- tryCatch(
      get_user_team_info(login = login, championship_id = championship_id, user_team_id = user_team_id),
      error = function(e) NULL
    )
    if (is.null(info)) {
      diagnostics <- c(diagnostics, "team info unavailable")
    } else {
      cfg <- if (is.list(info)) info[["configuration"]] else NULL
      if (!is.null(cfg) && !is.null(cfg[["maxPlayersInRoster"]])) {
        cap_val <- suppressWarnings(as.integer(cfg[["maxPlayersInRoster"]]))
      }
      if (!is.null(info[["budget"]])) budget_val <- suppressWarnings(as.numeric(info[["budget"]]))
      if (!is.null(info[["teamValue"]])) team_value_val <- suppressWarnings(as.numeric(info[["teamValue"]]))
      if (!is.null(info[["withheld"]])) withheld_val <- suppressWarnings(as.numeric(info[["withheld"]]))
      if (is.na(cap_val)) diagnostics <- c(diagnostics, "maxPlayersInRoster unavailable")
      if (is.na(budget_val)) diagnostics <- c(diagnostics, "reported budget unavailable")
      # withheld is only valid when it is a finite, non-negative number. A
      # missing / non-finite / negative withheld makes funds verification
      # INCOMPLETE (we fail closed rather than assuming 0).
      withheld_valid <- !is.na(withheld_val) && is.finite(withheld_val) && withheld_val >= 0
      if (!withheld_valid) {
        if (is.null(info[["withheld"]])) {
          diagnostics <- c(diagnostics, "withheld unavailable (funds verification incomplete)")
        } else if (is.na(withheld_val) || !is.finite(withheld_val)) {
          diagnostics <- c(diagnostics, "withheld non-finite (funds verification incomplete)")
        } else {
          diagnostics <- c(diagnostics, "withheld negative (funds verification incomplete)")
        }
      }
    }

    # ---- 2. Roster count ----
    roster_count <- NA_integer_
    roster_df <- tryCatch(
      get_players_from_team(login = login, championship_id = championship_id,
                            user_team_id = user_team_id, teams = NULL),
      error = function(e) NULL
    )
    if (is.null(roster_df)) {
      diagnostics <- c(diagnostics, "roster fetch failed")
    } else {
      roster_count <- as.integer(nrow(roster_df))
      if (!is.finite(team_value_val) || team_value_val < 0) {
        if (nrow(roster_df) && "value" %in% names(roster_df)) {
          roster_values <- suppressWarnings(as.numeric(roster_df$value))
          if (length(roster_values) == nrow(roster_df) && all(is.finite(roster_values) & roster_values >= 0))
            team_value_val <- sum(roster_values)
        }
      }
    }

    if (!is.finite(team_value_val) || team_value_val < 0)
      diagnostics <- c(diagnostics, "team value unavailable (debt limit unverified)")
    # Own bids come from market listings. Roster bids are incoming offers.
    bids_complete <- FALSE; bid_count <- NA_integer_; bid_total <- NA_real_
    market <- tryCatch(get_market_players(login, championship_id, user_team_id), error=function(e) NULL)
    target <- empty_target
    if (is.data.frame(market) && !identical(attr(market,"fetch_status"),"stale")) {
      bids_complete <- TRUE
      own <- if ("bid_id" %in% names(market)) which(!is.na(market$bid_id) & nzchar(as.character(market$bid_id))) else integer()
      my_bids <- market[own,,drop=FALSE]
      if (nrow(my_bids)) my_bids <- my_bids[!duplicated(my_bids$bid_id),,drop=FALSE]
      prices <- if (nrow(my_bids) && "bid_price" %in% names(my_bids)) suppressWarnings(as.numeric(my_bids$bid_price)) else numeric()
      bids_complete <- !nrow(my_bids) || (length(prices)==nrow(my_bids) && all(is.finite(prices) & prices>0))
      bid_count <- nrow(my_bids)
      bid_total <- if (bids_complete) sum(prices) else NA_real_
      if (target_key!="all" && "id" %in% names(my_bids)) {
        row <- my_bids[as.character(my_bids$id)==target_key,,drop=FALSE]
        if (nrow(row)==1L) {
          target$my_bid_id <- as.character(row$bid_id)
          target$my_bid_amount <- fm_number(row$bid_price)
        }
      }
      # Normal auctions are sealed: no rival amount can be inferred here.
      if (target_key!="all" && "id" %in% names(market)) {
        row <- market[as.character(market$id)==target_key,,drop=FALSE]
        if (nrow(row)==1L) target$bid_count <- fm_number(row$numberOfBids)
      }
    }
    if (!bids_complete) diagnostics <- c(diagnostics,"outgoing market commitments unavailable")
    if (identical(attr(info,"fetch_status"),"stale") || identical(attr(roster_df,"fetch_status"),"stale"))
      bids_complete <- FALSE

    # ---- 5. Status + spendable funds ----
    # Bids may temporarily use credit down to half the verified team value.
    borrowing <- acquisition_headroom(budget_val, team_value_val, withheld_val, bid_total)
    spendable <- if (bids_complete && withheld_valid) borrowing$spendable_budget else NA_real_

    required_available <- sum(
      !is.na(cap_val),
      !is.na(roster_count),
      !is.na(budget_val),
      is.finite(team_value_val) && team_value_val >= 0,
      bids_complete
    )
    if (required_available == 0) {
      status <- "unavailable"
    } else if (required_available < 5) {
      status <- "partial"
    } else if (!withheld_valid) {
      # All structural data is present, but funds cannot be verified (withheld
      # missing / non-finite / negative). Fail closed: do not assume withheld = 0.
      status <- "partial"
    } else {
      status <- "ok"
    }

    remaining_slots <- if (!is.na(roster_count) && !is.na(cap_val)) as.integer(cap_val - roster_count) else NA_integer_

    list(
      status = status,
      roster = list(
        count = roster_count,
        cap = cap_val,
        remaining_slots = remaining_slots
      ),
      funds = list(
        reported_budget = budget_val,
        withheld = withheld_val,
        team_value = team_value_val,
        debt_limit = borrowing$debt_limit,
        minimum_balance = borrowing$minimum_balance,
        projected_committed_balance = borrowing$projected_committed_balance,
        api_bid_limit = fm_number(info$maxBid),
        spendable_budget = spendable
      ),
      outstanding = list(
        offers = bid_count,
        count = bid_count,
        total_amount = bid_total,
        completeness = if (bids_complete) "complete" else "partial"
      ),
      target = target,
      diagnostics = diagnostics
    )
  }, timeout_sec = 15)
}

# ============================================================
# Acquisition Preflight Decision (pure, deterministic)
# ============================================================
# Evaluates whether an acquisition action is allowed given a verified
# capacity snapshot. Pure function (no I/O) so it is unit-testable.
#
# Parameters:
#   capacity            -- list from get_acquisition_capacity()
#   mode                -- "bid" | "offer" | "clause" | "modify"
#   amount              -- numeric amount to spend (NULL when not yet known,
#                          e.g. at modal-open time)
#   existing_bid_amount -- numeric, the user's current bid amount on the
#                          target (used by "modify" to compute the delta)
#   minimum_bid         -- optional current market/listing minimum for bid/modify
#
# Returns:
#   list(ok = logical, reason = "ok"|"unavailable"|"capacity"|"funds"|"minimum_bid",
#        message = character)
#
# Rules:
#   - Fail closed: if capacity is NULL or status != "ok" (partial/unavailable),
#     reason is "unavailable" (verification could not be confirmed).
#   - New offers ("bid"/"offer"): rejected when roster_count + outstanding_count
#     >= cap (reason "capacity").
#   - Clause buyout ("clause"): rejected when roster_count >= cap.
#   - Bid modification ("modify"): does NOT consume another slot; requires a
#     verifiable existing own bid, else "unavailable".
#   - Funds: when amount is known, the required spend (amount, or the positive
#     delta for "modify") must not exceed verified spendable funds, else
#     "funds".
evaluate_acquisition_preflight <- function(capacity, mode, amount = NULL, existing_bid_amount = NULL, minimum_bid = NULL) {
  unavailable <- function(msg) list(ok = FALSE, reason = "unavailable", message = msg)
  capacity_fail <- function(msg) list(ok = FALSE, reason = "capacity", message = msg)
  funds_fail <- function(msg) list(ok = FALSE, reason = "funds", message = msg)
  ok_result <- list(ok = TRUE, reason = "ok", message = NULL)

  if (is.null(capacity) || !is.list(capacity)) {
    return(unavailable("Acquisition verification unavailable. Please refresh and try again."))
  }
  if (!identical(capacity$status, "ok") || identical(attr(capacity,"fetch_status"),"stale")) {
    return(unavailable(paste0(
      "Acquisition verification unavailable (status: ", capacity$status,
      "). Please refresh and try again."
    )))
  }

  roster_count <- capacity$roster$count
  cap_val <- capacity$roster$cap
  outstanding_count <- capacity$outstanding$count
  spendable <- capacity$funds$spendable_budget

  if(length(spendable)!=1L || !is.finite(spendable)) return(unavailable("Spendable funds are unverified."))
  if (mode %in% c("bid", "modify") && !is.null(minimum_bid)) {
    if (!is.numeric(minimum_bid) || length(minimum_bid) != 1L ||
        !is.finite(minimum_bid) || minimum_bid <= 0) {
      return(unavailable("The current minimum market bid is unavailable. Please refresh the player."))
    }
    if (!is.null(amount) && (!is.numeric(amount) || length(amount) != 1L ||
        !is.finite(amount) || amount < minimum_bid)) {
      return(list(ok = FALSE, reason = "minimum_bid",
        message = paste0("Your bid must be at least ", format(minimum_bid, scientific = FALSE, trim = TRUE),
                         " EUR (the current market/listing minimum).")))
    }
  }
  # ---- Mode-specific capacity rules ----
  if (identical(mode, "modify")) {
    # Modification of an existing bid does not consume another slot, but we
    # must be able to verify the existing own bid (fail closed otherwise).
    if (is.null(existing_bid_amount) || is.na(existing_bid_amount) || !is.finite(existing_bid_amount) ||
        existing_bid_amount <= 0) {
      return(unavailable("Could not verify your existing bid on this player. Please refresh and try again."))
    }
  } else if (identical(mode, "clause")) {
    if (!is.na(roster_count) && !is.na(cap_val) && roster_count >= cap_val) {
      return(capacity_fail(paste0(
        "Roster is full (", roster_count, " of ", cap_val,
        " slots). Free a slot before buying a release clause."
      )))
    }
  } else {
    # New offers: "bid" and "offer"
    if (!is.na(roster_count) && !is.na(cap_val) &&
        !is.na(outstanding_count) && (roster_count + outstanding_count) >= cap_val) {
      return(capacity_fail(paste0(
        "Roster capacity reached (", roster_count, " players + ", outstanding_count,
        " pending offer(s) = cap ", cap_val, "). Free a slot before placing a new offer."
      )))
    }
  }

  # ---- Funds check (only when the amount is known) ----
  if (!is.null(amount) && is.numeric(amount) && length(amount) == 1 && is.finite(amount)) {
    if("api_bid_limit" %in% names(capacity$funds)) {
      api_limit <- fm_number(capacity$funds$api_bid_limit)
      if(!is.finite(api_limit)) return(unavailable("API bidding limit is unverified."))
      if(amount>api_limit) return(funds_fail("Amount exceeds the API bidding limit."))
    }
    required_spend <- amount
    if (identical(mode, "modify") && !is.null(existing_bid_amount) &&
        is.numeric(existing_bid_amount) && is.finite(existing_bid_amount)) {
      # Only the positive delta versus the existing bid consumes new funds.
      required_spend <- max(0, amount - existing_bid_amount)
    }
    if (required_spend > 0 && !is.na(spendable) && is.finite(spendable) &&
        required_spend > spendable) {
      return(funds_fail(paste0(
        "Amount exceeds verified spendable funds (", round(spendable),
        " EUR available). Lower the amount or free up funds."
      )))
    }
  }

  ok_result
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
    response <- futmondo_post(url, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
    ans <- httr::content(response)

    if (!is.null(ans) && "answer" %in% names(ans)) {
      return(ans[["answer"]])
    } else {
      return(NULL)
    }
  }, timeout_sec = 15)
}

get_team_image_name <- function(team, logo = NULL) {
  # If authoritative logo filename is provided directly from API/DB
  if (!is.null(logo) && length(logo) > 0 && !is.na(logo[1]) && nzchar(as.character(logo[1]))) {
    clean_logo <- gsub("\\.png$", "", as.character(logo[1]), ignore.case = TRUE)
    return(clean_logo)
  }

  if (is.null(team) || length(team) == 0 || is.na(team[1]) || !nzchar(as.character(team[1]))) {
    return("")
  }

  # Normalize team name: strip accents and special characters
  t_name <- tolower(trimws(as.character(team[1])))
  t_name <- gsub("[áàäâ]", "a", t_name)
  t_name <- gsub("[éèëê]", "e", t_name)
  t_name <- gsub("[íìïî]", "i", t_name)
  t_name <- gsub("[óòöô]", "o", t_name)
  t_name <- gsub("[úùüû]", "u", t_name)
  t_name <- gsub("[ñ]", "n", t_name)
  t_name <- gsub("[^a-z0-9 ]", "", t_name)
  t_name <- trimws(t_name)

  # Authoritative hardcoded lookup dictionary for all Spanish clubs
  club_dict <- c(
    "alaves"                  = "deportivo-alaves",
    "deportivo alaves"        = "deportivo-alaves",
    "athletic de bilbao"      = "athletic-de-bilbao",
    "athletic club"           = "athletic-de-bilbao",
    "athletic bilbao"         = "athletic-de-bilbao",
    "athletic"                = "athletic-de-bilbao",
    "atletico de madrid"      = "atletico-de-madrid",
    "atletico madrid"         = "atletico-de-madrid",
    "atletico"                = "atletico-de-madrid",
    "barcelona"               = "barcelona",
    "fc barcelona"            = "barcelona",
    "betis"                   = "betis",
    "real betis"              = "betis",
    "real betis balompie"     = "betis",
    "celta de vigo"           = "celta-de-vigo",
    "celta vigo"              = "celta-de-vigo",
    "celta"                   = "celta-de-vigo",
    "deportivo de la coruna"  = "deportivo-de-la-coruna",
    "deportivo la coruna"     = "deportivo-de-la-coruna",
    "deportivo"               = "deportivo-de-la-coruna",
    "elche"                   = "elche",
    "elche cf"                = "elche",
    "espanyol"                = "espanyol",
    "rcd espanyol"            = "espanyol",
    "getafe"                  = "getafe",
    "getafe cf"               = "getafe",
    "girona"                  = "girona",
    "girona fc"               = "girona",
    "las palmas"              = "las-palmas",
    "ud las palmas"           = "las-palmas",
    "leganes"                 = "leganes",
    "cd leganes"              = "leganes",
    "levante"                 = "levante",
    "levante ud"              = "levante",
    "malaga"                  = "malaga",
    "malaga cf"               = "malaga",
    "mallorca"                = "mallorca",
    "rcd mallorca"            = "mallorca",
    "osasuna"                 = "osasuna",
    "ca osasuna"              = "osasuna",
    "racing"                  = "racing-santander",
    "racing santander"        = "racing-santander",
    "racing de santander"     = "racing-santander",
    "rayo vallecano"          = "rayo-vallecano",
    "rayo"                    = "rayo-vallecano",
    "real madrid"             = "real-madrid",
    "real sociedad"           = "real-sociedad",
    "sevilla"                 = "sevilla",
    "sevilla fc"              = "sevilla",
    "valencia"                = "valencia",
    "valencia cf"             = "valencia",
    "valladolid"              = "real-valladolid",
    "real valladolid"         = "real-valladolid",
    "villarreal"              = "villarreal",
    "villarreal cf"           = "villarreal"
  )

  if (t_name %in% names(club_dict)) {
    return(club_dict[[t_name]])
  }

  # Generic fallback: lowercase with hyphens
  fallback_name <- gsub(" ", "-", t_name)
  fallback_name <- gsub("^r-", "real-", fallback_name)
  return(fallback_name)
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
    response <- futmondo_post(url, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
    status <- httr::status_code(response)
    if (status != 200L) {
      print(paste0("[get_user_team_moneymovements] HTTP status: ", status))
      return(data.frame(
        id = character(0), concept = character(0), type = character(0),
        category = character(0), money = numeric(0), date = character(0),
        stringsAsFactors = FALSE
      ))
    }
    ans <- httr::content(response)
    if (!is.null(ans) && "answer" %in% names(ans) && is.list(ans$answer)) {
      raw_ans <- ans$answer

      # Detect error responses: error=TRUE, or a non-ok code without an "answer" key
      if (isTRUE(raw_ans$error) || (!is.null(raw_ans$code) && raw_ans$code != API_CODE_OK && !("answer" %in% names(raw_ans)))) {
        return(data.frame(
          id = character(0), concept = character(0), type = character(0),
          category = character(0), money = numeric(0), date = character(0),
          stringsAsFactors = FALSE
        ))
      }

      # The API may return the movements array directly under ans$answer,
      # or nested under ans$answer$answer. Detect which shape we have.
      if (is.list(raw_ans) && "answer" %in% names(raw_ans) && is.list(raw_ans$answer)) {
        movements <- raw_ans$answer
      } else {
        movements <- raw_ans
      }

      # If movements looks like a single movement object (scalar fields like _id, date, money
      # alongside error/code/msg), it is an error response -- return empty.
      if (is.list(movements) && length(movements) > 0) {
        has_scalar_error_field <- isTRUE(movements$error) ||
          (!is.null(movements$code) && movements$code != API_CODE_OK)
        has_scalar_msg <- !is.null(movements$msg) && is.character(movements$msg) && length(movements$msg) == 1
        has_movement_field <- !is.null(movements[["_id"]]) || !is.null(movements$date) || !is.null(movements$money)
        # If it has error-like scalar fields AND no nested array structure, treat as error
        if ((has_scalar_error_field || has_scalar_msg) && !is.list(movements[[1]])) {
          return(data.frame(
            id = character(0), concept = character(0), type = character(0),
            category = character(0), money = numeric(0), date = character(0),
            stringsAsFactors = FALSE
          ))
        }
      }

      if (is.null(movements) || length(movements) == 0) {
        return(data.frame(
          id = character(0), concept = character(0), type = character(0),
          category = character(0), money = numeric(0), date = character(0),
          stringsAsFactors = FALSE
        ))
      }
      get_val <- function(obj, key, default = "") {
        if (is.null(obj)) return(default)
        nm <- names(obj)
        if (is.null(nm) || !key %in% nm) return(default)
        val <- obj[[key]]
        if (is.null(val)) return(default)
        return(val)
      }
      df <- lapply(movements, FUN = function(m) {
        m <- as.list(m)
        data.frame(
          id = if (is.null(get_val(m, "_id"))) NA_character_ else as.character(get_val(m, "_id")),
          concept = get_val(m, "concept", ""),
          type = get_val(m, "type", ""),
          category = get_val(m, "category", ""),
          money = if (is.null(get_val(m, "money", NULL))) 0 else as.numeric(get_val(m, "money")),
          date = get_val(m, "date", ""),
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

get_championship_pressroom <- function(login, championship_id) {
  if (is.null(login) || is.null(championship_id)) {
    return(data.frame(
      id = character(0), created = character(0), player_id = character(0),
      player_name = character(0), buyer_team_id = character(0),
      buyer_team_name = character(0), seller_team_id = character(0),
      seller_team_name = character(0), price = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  cache_key <- paste0("pressroom_", championship_id)
  get_cached_data(cache_key, {
    headers <- c("Content-Type" = "application/json; charset=utf-8")

    empty_df <- data.frame(
      id = character(0), created = character(0), player_id = character(0),
      player_name = character(0), buyer_team_id = character(0),
      buyer_team_name = character(0), seller_team_id = character(0),
      seller_team_name = character(0), price = numeric(0),
      stringsAsFactors = FALSE
    )

    get_val <- function(obj, key, default = "") {
      if (is.null(obj)) return(default)
      nm <- names(obj)
      if (is.null(nm) || !key %in% nm) return(default)
      val <- obj[[key]]
      if (is.null(val)) return(default)
      return(val)
    }

    parse_news_item <- function(item) {
      item <- as.list(item)

      # Extract player info
      player_obj <- get_val(item, "_player", NULL)
      p_id <- if (!is.null(player_obj)) as.character(get_val(player_obj, "_id", "")) else ""
      p_name <- if (!is.null(player_obj)) as.character(get_val(player_obj, "name", "")) else ""

      # Extract buyer info
      buyer_obj <- get_val(item, "_buyer", NULL)
      b_id <- if (!is.null(buyer_obj)) as.character(get_val(buyer_obj, "_id", "")) else ""
      b_name <- if (!is.null(buyer_obj)) as.character(get_val(buyer_obj, "name", "")) else ""

      # Extract seller info
      seller_obj <- get_val(item, "_seller", NULL)
      s_id <- if (!is.null(seller_obj)) as.character(get_val(seller_obj, "_id", "")) else ""
      s_name <- if (!is.null(seller_obj)) as.character(get_val(seller_obj, "name", "")) else ""

      # Extract price
      price_val <- get_val(item, "price", 0)
      price_num <- suppressWarnings(as.numeric(price_val))
      if (is.na(price_num)) price_num <- 0

      data.frame(
        id = as.character(get_val(item, "_id", "")),
        created = as.character(get_val(item, "created", "")),
        player_id = p_id,
        player_name = p_name,
        buyer_team_id = b_id,
        buyer_team_name = if (b_id == "") "Futmondo / Mercado" else b_name,
        seller_team_id = s_id,
        seller_team_name = if (s_id == "") "Futmondo / Mercado" else s_name,
        price = price_num,
        bids = I(list(get_val(item, "bids", list()))),
        bids_available = "bids" %in% names(item),
        stringsAsFactors = FALSE
      )
    }

    tryCatch({
      # Cursor-based pagination loop to fetch 100% of historical pressroom transactions
      cursor <- ""
      all_news <- list()
      page_count <- 0
      max_pages <- 25

      while (page_count < max_pages) {
        page_count <- page_count + 1
        print(paste0("[API] Fetching pressroom feed - page ", page_count, ", cursor: ", if (cursor == "") "(initial)" else cursor))

        payload <- list(
          header = list(
            token = login[["token"]],
            userid = login[["userid"]]
          ),
          query = list(
            championshipId = as.character(championship_id),
            from = cursor
          ),
          answer = list()
        )

        response <- futmondo_post(PRESSROOM_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
        ans <- httr::content(response)

        if (is.null(ans) || !("answer" %in% names(ans)) || is.null(ans$answer) || !("news" %in% names(ans$answer))) {
          stop("Pressroom response unavailable")
        }

        news <- ans$answer$news
        if (is.null(news) || length(news) == 0) {
          print("[Pressroom] Empty news array, all pages fetched.")
          break
        }

        # Append news items to the accumulator
        for (item in news) {
          all_news[[length(all_news) + 1]] <- item
        }

        # Get the last item's _id to use as the cursor for the next page
        last_item <- news[[length(news)]]
        last_id <- get_val(as.list(last_item), "_id", "")

        # If last_id is empty or same as current cursor, we've reached the end
        if (last_id == "" || last_id == cursor) {
          print("[Pressroom] Reached end of pagination (no new cursor).")
          break
        }

        cursor <- last_id
      }

      print(paste0("[Pressroom] Pagination complete: ", length(all_news), " items across ", page_count, " pages."))

      if (length(all_news) == 0) {
        return(empty_df)
      }

      df <- lapply(all_news, FUN = function(item) {
        parse_news_item(item)
      }) %>% bind_rows()

      # Deduplicate across cursor page boundaries
      df <- df %>%
        dplyr::filter(!is.na(id) & nzchar(id)) %>%
        dplyr::distinct(id, .keep_all = TRUE) %>%
        dplyr::arrange(desc(created))

      return(observed_data(df,status=if(page_count>=max_pages) "partial" else "ok"))
    }, error = function(e) {
      stop("Pressroom data unavailable")
    })
  }, timeout_sec = 300)
}

calculate_league_finances <- function(login, championship_id, user_teams_df, initial_budget = NA_real_) {
  cache_key <- paste0("league_finances_calc_", championship_id)
  get_cached_data(cache_key, {
    if (is.null(user_teams_df) || nrow(user_teams_df) == 0) {
      return(list(
        team_finances = data.frame(
          teamid = character(0), teamname = character(0), initial_budget = numeric(0),
          total_spent = numeric(0), total_sales = numeric(0), budget = numeric(0),
          team_value = numeric(0), net_profit_loss = numeric(0), squad_size = numeric(0),
          points = numeric(0), point_bonus = numeric(0), ranking_prize = numeric(0),
          stringsAsFactors = FALSE
        ),
        all_purchases = data.frame()
      ))
    }

    finances_list <- list()
    purchases_list <- list()

    # Fetch pressroom transaction history for the championship
    pressroom_df <- tryCatch({
      get_championship_pressroom(login = login, championship_id = championship_id)
    }, error = function(e) {
      print(paste0("[Finances] Error fetching pressroom: ", e$message))
      data.frame(
        id = character(0), created = character(0), player_id = character(0),
        player_name = character(0), buyer_team_id = character(0),
        buyer_team_name = character(0), seller_team_id = character(0),
        seller_team_name = character(0), price = numeric(0),
        stringsAsFactors = FALSE
      )
    })

    # Sync pressroom transactions to Supabase
    tryCatch({
      sync_pressroom_transactions_to_supabase(pressroom_df, championship_id)
    }, error = function(e) {
      print(paste0("[Finances] Supabase pressroom sync warning: ", e$message))
    })

    # Fetch all championship players in 1 bulk call for fast roster grouping
    all_players <- tryCatch({
      get_championship_players(login = login, championship_id = championship_id)
    }, error = function(e) {
      print(paste0("[Finances] Error fetching all championship players: ", e$message))
      data.frame()
    })

    # Determine finished rounds to decide whether to apply ranking prizes
    finished_rounds_df <- tryCatch({
      get_finished_rounds(login = login, championship_id = championship_id)
    }, error = function(e) {
      print(paste0("[Finances] Error fetching finished rounds: ", e$message))
      data.frame(
        round_id = character(0), round_number = numeric(0),
        begin_process = character(0), is_finished = logical(0),
        stringsAsFactors = FALSE
      )
    })
    has_finished_rounds <- !is.null(finished_rounds_df) && nrow(finished_rounds_df) > 0 && any(finished_rounds_df$is_finished, na.rm = TRUE)

    # Sort teams by points descending to determine ranking
    teams_sorted <- user_teams_df[order(-as.numeric(user_teams_df$points)), ]
    team_rank <- seq_len(nrow(teams_sorted))
    team_rank_map <- setNames(team_rank, if ("teamid" %in% colnames(teams_sorted)) as.character(teams_sorted$teamid) else as.character(teams_sorted$id))

    for (i in seq_len(nrow(user_teams_df))) {
      row <- user_teams_df[i, ]
      tid <- if ("teamid" %in% colnames(row)) row$teamid else row$id
      tname <- if ("teamname" %in% colnames(row)) row$teamname else row$name
      tpoints <- if ("points" %in% colnames(row)) as.numeric(row$points) else 0

      # Filter roster from bulk all_players
      roster <- if (!is.null(all_players) && nrow(all_players) > 0 && "userteamId" %in% colnames(all_players)) {
        all_players %>% dplyr::filter(!is.na(userteamId) & as.character(userteamId) == as.character(tid))
      } else {
        data.frame()
      }

       total_spent_from_roster <- 0
       total_spent <- 0
       team_value <- 0
       squad_size <- 0

       if (!is.null(roster) && nrow(roster) > 0) {
        squad_size <- nrow(roster)
        total_spent_from_roster <- sum(suppressWarnings(as.numeric(roster$buyPrice)), na.rm = TRUE)
        team_value <- sum(suppressWarnings(as.numeric(roster$value)), na.rm = TRUE)

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

      # Calculate pressroom purchases and sales for this team
      pressroom_purchases <- 0
      pressroom_sales <- 0
      if (!is.null(pressroom_df) && nrow(pressroom_df) > 0) {
        pressroom_purchases <- sum(suppressWarnings(as.numeric(pressroom_df$price[pressroom_df$buyer_team_id == tid])), na.rm = TRUE)
        pressroom_sales <- sum(suppressWarnings(as.numeric(pressroom_df$price[pressroom_df$seller_team_id == tid])), na.rm = TRUE)
      }

# Use pressroom purchases as total_spent if available, otherwise fall back to roster buyPrice
       total_spent_val <- if (pressroom_purchases > 0) pressroom_purchases else total_spent_from_roster
      total_sales_val <- pressroom_sales

      actual_info <- tryCatch(get_user_team_info(login, championship_id, tid), error=function(e)NULL)
      rules <- normalize_league_rules(actual_info)
      initial_budget <- rules$initial_budget
      point_bonus <- as.numeric(tpoints) * rules$money_per_point
      # Season standings cannot reconstruct paid per-round ranking prizes.
      ranking_prize <- NA_real_
      final_budget <- NA_real_ # reconstructed estimates are not verified cash
      if (!is.null(actual_info) && !is.null(actual_info$budget) && is.numeric(actual_info$budget) && is.finite(actual_info$budget)) {
        final_budget <- actual_info$budget
      }

      if (!is.null(actual_info) && !is.null(actual_info$teamValue) && is.numeric(actual_info$teamValue) && is.finite(actual_info$teamValue)) {
        team_value <- actual_info$teamValue
      }

      finances_list[[length(finances_list) + 1]] <- data.frame(
        teamid = as.character(tid),
        teamname = as.character(tname),
        initial_budget = as.numeric(initial_budget),
        total_spent = as.numeric(total_spent_val),
        total_sales = as.numeric(total_sales_val),
        budget = as.numeric(final_budget),
        team_value = as.numeric(team_value),
        net_profit_loss = as.numeric(team_value + total_sales_val - total_spent_val),
        squad_size = as.numeric(squad_size),
        points = as.numeric(tpoints),
        point_bonus = as.numeric(point_bonus),
        ranking_prize = as.numeric(ranking_prize),
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
      log_user_team_history(finances_df,championship_id=championship_id)
    }, error = function(e) {
      print(paste0("[Finances] Supabase sync warning: ", e$message))
    })

    return(list(
      team_finances = finances_df,
      all_purchases = purchases_df
    ))
  }, timeout_sec = 60)
}

get_player_summary <- function(login, championship_id, user_team_id = NULL, player_id = NULL) {
  if (is.null(login) || is.null(championship_id) || is.null(player_id)) return(NULL)

  cache_user_id <- if (!is.null(login[["userid"]]) && nzchar(as.character(login[["userid"]]))) as.character(login[["userid"]]) else "anonymous"
  cache_team_id <- if (!is.null(user_team_id) && nzchar(as.character(user_team_id))) as.character(user_team_id) else "none"
  cache_key <- paste0("player_summary_", cache_user_id, "_", championship_id, "_", cache_team_id, "_", player_id)
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
    response <- futmondo_post(PLAYER_SUMMARY_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
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

      # Only expose my_bid_id/my_bid_price when a bid's immutable team ID
      # matches user_team_id AND that bid carries a non-empty ID. We never take
      # the "last bid" blindly -- it may be a rival's bid.
      if (!is.null(bids_arr) && is.list(bids_arr) && length(bids_arr) > 0 &&
          !is.null(user_team_id) && nzchar(as.character(user_team_id))) {
        for (b in bids_arr) {
          if (is.list(b) && !is.null(b[["id"]]) && !is.null(b[["price"]])) {
            bid_id <- as.character(b[["id"]])
            btid <- extract_bidder_team_id(b)
            if (!is.na(btid) && btid == as.character(user_team_id) && nzchar(bid_id)) {
              my_bid_id <- bid_id
              my_bid_price <- suppressWarnings(as.numeric(b[["price"]]))
              break
            }
          }
        }
      }
      
      return(list(
        data = if ("data" %in% names(ans_data)) ans_data$data else NULL,
        prices = if ("prices" %in% names(ans_data)) ans_data$prices else list(),
        points = ans_data$points %||% list(),
        match = ans_data$match %||% NULL,
        championship = ans_data$championship %||% list(),
        owners = ans_data$owners %||% list(),
        market = ans_data$market %||% list(),
        numberOfBids = ans_data$numberOfBids %||% NA_integer_,
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
  response <- futmondo_post(MODIFY_BID_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
  ans <- httr::content(response)
  operation_code <- if (!is.null(ans) && "answer" %in% names(ans) && "code" %in% names(ans$answer)) ans$answer$code else ""
  return(structure(operation_code == API_CODE_OK, api_code = operation_code))
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
  response <- futmondo_post(CANCEL_BID_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
  ans <- httr::content(response)
  operation_code <- if (!is.null(ans) && "answer" %in% names(ans) && "code" %in% names(ans$answer)) ans$answer$code else ""
  return(structure(operation_code == API_CODE_OK, api_code = operation_code))
}

get_user_team_rounds <- function(login, championship_id, user_team_id) {
  if (is.null(login) || is.null(championship_id) || is.null(user_team_id)) return(NULL)
  
  cache_key <- paste0("rounds_", championship_id, "_", user_team_id)
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
    print(paste0("[API] Fetching rounds for team: ", user_team_id))
    response <- futmondo_post(ROUNDS_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
    ans <- httr::content(response)
    if (!is.null(ans) && "answer" %in% names(ans) && is.list(ans$answer)) {
      return(ans$answer)
    }
    return(list())
  })
}

get_round_dreamteam <- function(login, championship_id, round_number) {
  if (is.null(login) || is.null(championship_id) || is.null(round_number)) return(NULL)
  
  cache_key <- paste0("dreamteam_", championship_id, "_", round_number)
  get_cached_data(cache_key, {
    payload <- list(
      header = list(
        token = login[["token"]],
        userid = login[["userid"]]
      ),
      query = list(
        championshipId = championship_id,
        type = "dreamteam",
        round = round_number
      ),
      answer = list()
    )
    headers <- c("Content-Type" = "application/json; charset=utf-8")
    print(paste0("[API] Fetching dreamteam for round: ", round_number))
    response <- futmondo_post(DREAMTEAM_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
    ans <- httr::content(response)
    if (!is.null(ans) && "answer" %in% names(ans) && is.list(ans$answer)) {
      return(ans$answer)
    }
    return(list())
  })
}

calculate_futmondo_ranking_prizes <- function(money = 30000000, members = 1) {
  if (is.null(members) || is.na(members) || members <= 0) members <- 1
  if (is.null(money) || is.na(money) || money <= 0) money <- 0

  total_pct <- sum(seq_len(members))
  ranks <- seq_len(members)
  ratios <- (members - ranks + 1) / total_pct
  prizes <- round(money * ratios)

  data.frame(
    rank = ranks,
    ratio = ratios,
    prize = prizes,
    stringsAsFactors = FALSE
  )
}

put_player_on_market <- function(login, championship_id, team_id, player_id, price) {
  payload <- list(
    header = list(
      token = login[["token"]],
      userid = login[["userid"]]
    ),
    query = list(
      championshipId = as.character(championship_id),
      userteamId = as.character(team_id),
      price = as.numeric(price),
      player_id = as.character(player_id),
      isClause = NA,
      mode = NA,
      toLoan = NA
    ),
    answer = list()
  )
  headers <- c("Content-Type" = "application/json; charset=utf-8")
  print(paste0("[API] Putting player on market: ", player_id, " price: ", price))
  response <- futmondo_post(PUT_ON_MARKET_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
  ans <- httr::content(response)

  operation_code <- if (!is.null(ans) && "answer" %in% names(ans) && "code" %in% names(ans$answer)) ans$answer$code else ""
  err_msg <- if (!is.null(ans) && "answer" %in% names(ans) && "msg" %in% names(ans$answer)) ans$answer$msg else if (!is.null(ans) && "answer" %in% names(ans) && "message" %in% names(ans$answer)) ans$answer$message else operation_code

  is_success <- (operation_code == API_CODE_OK)
  return(list(
    success = is_success,
    code = operation_code,
    message = err_msg
  ))
}

# Updates an existing listing by withdrawing it first, then creating the
# replacement at the requested price. Each API write is contained so callers
# receive a verified outcome instead of an unhandled transport error.
update_player_market_listing <- function(login, championship_id, team_id, player_id, price) {
  tryCatch({
    withdrawn <- cancel_player_sell(login, championship_id, team_id, player_id)
    withdrawn_ok <- if (is.list(withdrawn)) isTRUE(withdrawn$success) else isTRUE(withdrawn)
    if (!withdrawn_ok) {
      message <- if (is.list(withdrawn) && !is.null(withdrawn$message) && nzchar(as.character(withdrawn$message))) withdrawn$message else "Could not remove the existing listing."
      return(list(success = FALSE, code = if (is.list(withdrawn)) withdrawn$code %||% "" else "", message = message))
    }
    listed <- put_player_on_market(login, championship_id, team_id, player_id, price)
    listed_ok <- if (is.list(listed)) isTRUE(listed$success) else isTRUE(listed)
    if (!listed_ok) {
      message <- if (is.list(listed) && !is.null(listed$message) && nzchar(as.character(listed$message))) listed$message else "The old listing was removed but the new price could not be listed."
      return(list(success = FALSE, code = if (is.list(listed)) listed$code %||% "" else "", message = message))
    }
    if (is.list(listed)) listed else list(success = TRUE, code = "", message = "")
  }, error = function(e) list(success = FALSE, code = "error", message = conditionMessage(e)))
}

cancel_player_sell <- function(login, championship_id, team_id, player_id) {
  payload <- list(
    header = list(
      token = login[["token"]],
      userid = login[["userid"]]
    ),
    query = list(
      championshipId = as.character(championship_id),
      userteamId = as.character(team_id),
      player_id = as.character(player_id)
    ),
    answer = list()
  )
  headers <- c("Content-Type" = "application/json; charset=utf-8")
  print(paste0("[API] Cancelling player sell for: ", player_id))
  response <- futmondo_post(CANCEL_SELL_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
  ans <- httr::content(response)

  operation_code <- if (!is.null(ans) && "answer" %in% names(ans) && "code" %in% names(ans$answer)) ans$answer$code else ""
  err_msg <- if (!is.null(ans) && "answer" %in% names(ans) && "msg" %in% names(ans$answer)) ans$answer$msg else if (!is.null(ans) && "answer" %in% names(ans) && "message" %in% names(ans$answer)) ans$answer$message else operation_code

  is_success <- (operation_code == API_CODE_OK)
  return(list(
    success = is_success,
    code = operation_code,
    message = err_msg
  ))
}

put_all_on_market <- function(login, championship_id, team_id) {
  payload <- list(
    header = list(
      token = login[["token"]],
      userid = login[["userid"]]
    ),
    query = list(
      championshipId = as.character(championship_id),
      userteamId = as.character(team_id)
    ),
    answer = list()
  )
  headers <- c("Content-Type" = "application/json; charset=utf-8")
  print(paste0("[API] Putting ALL players on market for team: ", team_id))
  response <- futmondo_post(PUT_ALL_ON_MARKET_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
  ans <- httr::content(response)

  operation_code <- if (!is.null(ans) && "answer" %in% names(ans) && "code" %in% names(ans$answer)) ans$answer$code else ""
  err_msg <- if (!is.null(ans) && "answer" %in% names(ans) && "msg" %in% names(ans$answer)) ans$answer$msg else if (!is.null(ans) && "answer" %in% names(ans) && "message" %in% names(ans$answer)) ans$answer$message else operation_code

  is_success <- (operation_code == API_CODE_OK)
  return(list(
    success = is_success,
    code = operation_code,
    message = err_msg
  ))
}

get_roster_bids <- function(login, championship_id, user_team_id) {
  if (is.null(login) || is.null(championship_id) || is.null(user_team_id)) return(data.frame())

  cache_key <- paste0("roster_bids_", championship_id, "_", user_team_id)
  get_cached_data(cache_key, {
    payload <- list(
      header = list(
        token = login[["token"]],
        userid = login[["userid"]]
      ),
      query = list(
        championshipId = as.character(championship_id),
        userteamId = as.character(user_team_id),
        type = "roster"
      ),
      answer = list()
    )
    headers <- c("Content-Type" = "application/json; charset=utf-8")
    print("[API] Fetching roster bids for team (type=roster)")
    response <- futmondo_post(ROSTER_BIDS_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
    ans <- httr::content(response)

    if (!is.null(ans) && "answer" %in% names(ans) && is.list(ans$answer)) {
      items <- ans$answer
      if (!is.null(items$code) || isTRUE(items$error)) stop("Offer data unavailable")
      if (length(items) == 0) return(data.frame())

      bids_list <- list()
      for (item in items) {
        if (!is.list(item)) next

        # Resolve player ID
        p_id <- NULL
        if ("player_id" %in% names(item) && !is.null(item[["player_id"]])) {
          p_id <- as.character(item[["player_id"]])
        } else if ("player" %in% names(item)) {
          if (is.character(item[["player"]]) || is.numeric(item[["player"]])) {
            p_id <- as.character(item[["player"]])
          } else if (is.list(item[["player"]])) {
            if (!is.null(item[["player"]][["_id"]])) {
              p_id <- as.character(item[["player"]][["_id"]])
            } else if (!is.null(item[["player"]][["id"]])) {
              p_id <- as.character(item[["player"]][["id"]])
            }
          }
        } else if ("id" %in% names(item) && !is.null(item[["id"]])) {
          p_id <- as.character(item[["id"]])
        }

        # Check for nested bids array or direct bid object
        bids_arr <- NULL
        if ("bids" %in% names(item) && is.list(item$bids)) {
          bids_arr <- item$bids
        } else if ("market" %in% names(item) && is.list(item$market) && "bids" %in% names(item$market)) {
          bids_arr <- item$market$bids
        }

        if (!is.null(p_id) && !is.null(bids_arr) && length(bids_arr) > 0) {
          b_df <- lapply(bids_arr, FUN = function(b) {
            b_price <- if (!is.null(b[["price"]])) suppressWarnings(as.numeric(b[["price"]])) else 0
            b_user <- if (!is.null(b[["userTeam"]]) && is.list(b[["userTeam"]]) && !is.null(b[["userTeam"]][["name"]]) && as.character(b[["userTeam"]][["name"]]) != "") as.character(b[["userTeam"]][["name"]]) else "Futmondo"
            b_id <- if (!is.null(b[["id"]])) as.character(b[["id"]]) else if (!is.null(b[["_id"]])) as.character(b[["_id"]]) else ""
            b_bidder <- extract_bidder_team_id(b)
            data.frame(id = p_id, bid_price = b_price, bid_user = b_user, bid_id = b_id,
                       bidder_team_id = b_bidder, stringsAsFactors = FALSE)
          }) %>% rbindlist(fill = TRUE) %>% as.data.frame()

          if (nrow(b_df) > 0) {
            max_idx <- which.max(b_df$bid_price)
            bids_list[[length(bids_list) + 1]] <- b_df[max_idx, ]
          }
        } else if (!is.null(p_id) && "price" %in% names(item) && !is.null(item[["price"]])) {
          # Direct bid object format
          b_price <- suppressWarnings(as.numeric(item[["price"]]))
          b_user <- if ("userTeam" %in% names(item) && is.list(item[["userTeam"]]) && !is.null(item[["userTeam"]][["name"]]) && as.character(item[["userTeam"]][["name"]]) != "") as.character(item[["userTeam"]][["name"]]) else "Futmondo"
          b_id <- if ("_id" %in% names(item)) as.character(item[["_id"]]) else if ("id" %in% names(item)) as.character(item[["id"]]) else ""
          b_bidder <- extract_bidder_team_id(item)
          bids_list[[length(bids_list) + 1]] <- data.frame(id = p_id, bid_price = b_price, bid_user = b_user, bid_id = b_id,
                                                            bidder_team_id = b_bidder, stringsAsFactors = FALSE)
        }
      }

      if (length(bids_list) > 0) {
        ret_df <- bind_rows(bids_list)
        # Deduplicate per player ID taking highest bid
        ret_df <- ret_df %>%
          dplyr::group_by(id) %>%
dplyr::slice_max(order_by = bid_price, n = 1, with_ties = FALSE) %>%
           dplyr::ungroup() %>%
           as.data.frame()
         return(ret_df)
       }
     }
     return(data.frame())
  }, timeout_sec = 15)
}

get_my_market_players <- function(login, championship_id, user_team_id) {
  if (is.null(login) || is.null(championship_id) || is.null(user_team_id)) return(NULL)

  cache_key <- paste0("my_market_players_", championship_id, "_", user_team_id)
  get_cached_data(cache_key, {
    payload <- list(
      header = list(
        token = login[["token"]],
        userid = login[["userid"]]
      ),
      query = list(
        championshipId = as.character(championship_id),
        userteamId = as.character(user_team_id),
        type = "market"
      ),
      answer = list()
    )
    headers <- c("Content-Type" = "application/json; charset=utf-8")
    print("[API] Fetching user's listed market players")
    response <- futmondo_post(MY_PLAYERS_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
    ans <- httr::content(response)

    if (!is.null(ans) && "answer" %in% names(ans) && is.list(ans$answer)) {
      players <- ans$answer
      if (length(players) == 0) return(data.frame())
      ret <- lapply(players, FUN = function(player) {
        parse_player_json(player = player)
      }) %>% rbindlist(fill = TRUE) %>% as.data.frame()
      return(ret)
    }
    return(data.frame())
  }, timeout_sec = 60)
}

accept_bid <- function(login, championship_id, team_id, player_id, bid_id) {
  payload <- list(
    header = list(
      token = login[["token"]],
      userid = login[["userid"]]
    ),
    query = list(
      championshipId = as.character(championship_id),
      userteamId = as.character(team_id),
      bid = as.character(bid_id),
      player_id = as.character(player_id)
    ),
    answer = list()
  )
  headers <- c("Content-Type" = "application/json; charset=utf-8")
  print(paste0("[API] Accepting offer bid: ", bid_id, " for player: ", player_id))
  response <- futmondo_post(ACCEPT_BID_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
  ans <- httr::content(response)

  operation_code <- if (!is.null(ans) && "answer" %in% names(ans) && "code" %in% names(ans$answer)) ans$answer$code else ""
  err_msg <- if (!is.null(ans) && "answer" %in% names(ans) && "msg" %in% names(ans$answer)) ans$answer$msg else if (!is.null(ans) && "answer" %in% names(ans) && "message" %in% names(ans$answer)) ans$answer$message else operation_code

  is_success <- (operation_code == API_CODE_OK)
  return(list(
    success = is_success,
    code = operation_code,
    message = err_msg
  ))
}

reject_bid <- function(login, championship_id, team_id, player_id, bid_id) {
  payload <- list(
    header = list(
      token = login[["token"]],
      userid = login[["userid"]]
    ),
    query = list(
      championshipId = as.character(championship_id),
      userteamId = as.character(team_id),
      bid = as.character(bid_id),
      player_id = as.character(player_id)
    ),
    answer = list()
  )
  headers <- c("Content-Type" = "application/json; charset=utf-8")
  print(paste0("[API] Rejecting offer bid: ", bid_id, " for player: ", player_id))
  response <- futmondo_post(REJECT_BID_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
  ans <- httr::content(response)

  operation_code <- if (!is.null(ans) && "answer" %in% names(ans) && "code" %in% names(ans$answer)) ans$answer$code else ""
  err_msg <- if (!is.null(ans) && "answer" %in% names(ans) && "msg" %in% names(ans$answer)) ans$answer$msg else if (!is.null(ans) && "answer" %in% names(ans) && "message" %in% names(ans$answer)) ans$answer$message else operation_code

  is_success <- (operation_code == API_CODE_OK)
  return(list(
    success = is_success,
    code = operation_code,
    message = err_msg
  ))
}

# Shared account-aware finance and notification contracts (docs/data_contracts.md).
get_financial_snapshot <- function(login, championship_id, user_team_id) {
  get_cached_data(paste0("financial_snapshot_", championship_id, "_", user_team_id), {
    capacity <- get_acquisition_capacity(login, championship_id, user_team_id)
    info <- tryCatch(get_user_team_info(login, championship_id, user_team_id), error = function(e) NULL)
    status <- if (!is.null(info)) capacity$status else "unavailable"
    stale <- identical(attr(info,"fetch_status"),"stale") || identical(attr(capacity,"fetch_status"),"stale")
    if (stale) status <- "partial"
    cash <- fm_number(info$budget); withheld <- fm_number(info$withheld)
    limit <- fm_number(capacity$funds$api_bid_limit)
    pending <- fm_number(capacity$outstanding$total_amount)
    complete <- identical(capacity$outstanding$completeness,"complete")
    team_value <- fm_number(capacity$funds$team_value)
    spendable <- if (identical(status,"ok") && complete) fm_number(capacity$funds$spendable_budget) else NA_real_
    debt_limit <- fm_number(capacity$funds$debt_limit)
    minimum_balance <- fm_number(capacity$funds$minimum_balance)
    projected_balance <- fm_number(capacity$funds$projected_committed_balance)
    lineup <- tryCatch(get_lineup_from_team(login,championship_id,user_team_id),error=function(e)NULL)
    rules <- normalize_league_rules(info,lineup,championship_id)
    observed_at <- min(attr(info,"observed_at") %||% Sys.time(), attr(capacity,"observed_at") %||% Sys.time())
    list(status = status, cash = cash, withheld = withheld,
      spendable_budget = spendable, legal_bid_limit = limit,
      debt_limit = debt_limit, minimum_balance = minimum_balance,
      projected_committed_balance = projected_balance,
      roster_count = capacity$roster$count, roster_cap = capacity$roster$cap,
      commitments = capacity$outstanding, observed_at = observed_at,
      configuration = info$configuration %||% list(),
      team_value = team_value, rules = rules, lineup_rules = rules)
  }, timeout_sec = 15)
}

get_active_championships <- function(login) {
  get_cached_data("active_championship_objects", {
    payload <- list(header = list(token = login[["token"]], userid = login[["userid"]]),
      query = list(excludeGeneral = FALSE, includeProphets = TRUE), answer = list())
    response <- futmondo_post(ACTIVE_CHAMPIONSHIPS_URL,
      body = jsonlite::toJSON(payload, auto_unbox = TRUE),
      httr::content_type_json())
    answer <- httr::content(response)$answer
    if (!is.list(answer$championships)) stop("Championships unavailable")
    answer$championships <- lapply(answer$championships,function(champ) {
      champ$id <- champ$id %||% champ$`_id`
      if(is.list(champ$userteam)) champ$userteam$id <- champ$userteam$id %||% champ$userteam$`_id`
      champ
    })
    answer
  }, timeout_sec = 60)
}

notification_request <- function(login, action, query = "") {
  if (!valid_login(login)) return(fetch_result(status = "unavailable"))
  tryCatch({
    response <- futmondo_post(paste0("https://api.futmondo.com/1/notification/", action),
      body = jsonlite::toJSON(list(header = list(token = login[["token"]],
        userid = login[["userid"]]), query = query, answer = list()), auto_unbox = TRUE),
      httr::content_type_json())
    answer <- httr::content(response)$answer
    if (is.null(answer) || (is.list(answer) && isTRUE(answer$error))) stop("Unavailable")
    fetch_result(answer)
  }, error = function(e) fetch_result(status = "unavailable", error = "Notifications unavailable"))
}

normalize_notifications <- function(items) {
  empty <- data.frame(id = character(), created_at = character(), updated_at = character(),
    type = character(), action = character(), is_read = logical(), player_id = character(),
    player_name = character(), subject = character(), championship_id = character(),
    league_name = character(), source = character(), message = character(), stringsAsFactors = FALSE)
  if (is.null(items) || !length(items)) return(empty)
  dplyr::bind_rows(c(list(empty), lapply(items, function(n) {
    action <- fm_scalar(n$action, "unknown")
    text <- switch(action, acceptBid = "Offer accepted", rejectBid = "Offer rejected",
                   paste(fm_scalar(n$type, "Notification"), action))
    data.frame(id = fm_scalar(n$`_id`, ""), created_at = fm_scalar(n$created),
      updated_at = fm_scalar(n$updated), type = fm_scalar(n$type, "unknown"),
      action = action, is_read = isTRUE(n$readed), player_id = fm_scalar(n$directObject$id, ""),
      player_name = fm_scalar(n$directObject$name, ""), subject = fm_scalar(n$subject$name, ""),
      championship_id = fm_scalar(n$context$id, ""), league_name = fm_scalar(n$context$name, ""),
      source = "Futmondo", message = text, stringsAsFactors = FALSE)
  }))) %>% dplyr::distinct(id, .keep_all = TRUE)
}

get_notifications <- function(login) {
  tryCatch(get_cached_data("notifications_list", {
    result <- notification_request(login, "list")
    if (result$status != "ok") stop("Unavailable")
    if(!is.list(result$data$items)) stop("Notification list unavailable")
    normalize_notifications(result$data$items)
  }, timeout_sec = 30), error = function(e) NULL)
}

get_notification_unread <- function(login) {
  tryCatch(get_cached_data("notifications_unread", {
    result <- notification_request(login, "unread")
    count <- fm_number(result$data)
    if (result$status != "ok" || !is.finite(count) || count < 0) stop("Unavailable")
    as.integer(count)
  }, timeout_sec = 55), error = function(e) NA_integer_)
}

mark_notification_read <- function(login, id) {
  if (!nzchar(fm_scalar(id, ""))) return(FALSE)
  result <- notification_request(login, "markreaded", list(id = as.character(id)))
  ok <- identical(result$status, "ok") && identical(result$data$code, "notification.markReaded.ok")
  if (ok) {
    for (key in c("notifications_list", "notifications_unread")) {
      scoped <- api_cache_key(key, login)
      if (exists(scoped, api_cache_env, inherits = FALSE)) rm(list = scoped, envir = api_cache_env)
    }
  }
  ok
}
