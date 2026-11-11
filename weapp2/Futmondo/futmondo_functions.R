source("data_contracts.R")
source("locker_transfers.R")
LOGIN_URL <- "https://api.futmondo.com/5/login/with_mail"
ACTIVE_CHAMPIONSHIPS_URL <- "https://api.futmondo.com/2/user/activechampionships"
TEAMS_URL <- "https://api.futmondo.com/2/championship/teams"
ROSTER_URL <- "https://api.futmondo.com/1/userteam/roster"
DREAMTEAM_URL <- "https://api.futmondo.com/1/userteam/dreamteam"
ROUNDS_URL <- "https://api.futmondo.com/1/userteam/rounds"
BID_URL <- "https://api.futmondo.com/1/market/bid"
MARKET_URL <- "https://api.futmondo.com/1/market/players"
PLAYER_SUMMARY_URL <- "https://api.futmondo.com/1/player/summary"
PLAYER_FULL_PROFILE_URL <- "https://api.futmondo.com/1/player/fullprofile"
PLAYER_MATCHES_URL <- "https://api.futmondo.com/2/player/matches"
MODIFY_BID_URL <- "https://api.futmondo.com/5/market/modifybid"
PRESSROOM_URL <- "https://api.futmondo.com/1/locker/pressroom"
CANCEL_BID_URL <- "https://api.futmondo.com/1/market/cancelbid"
PUT_ON_MARKET_URL <- "https://api.futmondo.com/1/market/putonmarket"
CANCEL_SELL_URL <- "https://api.futmondo.com/1/market/cancelsell"
DIRECT_SELL_URL <- "https://api.futmondo.com/1/market/directsell"
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
source("mvp_runtime.R")
source("rival_finance_runtime.R")
source("rival_ownership_runtime.R")
source("round_settlement_runtime.R")

source("data_service.R")
source("async_transport.R")
source("coach_runtime.R")

# Only read endpoints may retry. Mutations require reconciliation after an
# uncertain response, so a network timeout must never replay a trade.
futmondo_post <- function(url, ...) {
  if (isTRUE(getOption("futmondo.offline", FALSE))) stop("Network disabled in offline mode")
  read_routes <- c("information", "activechampionships", "teams", "roster", "rounds",
    "dreamteam", "nightmareteam", "roundlineup", "lineup", "championshipplayers", "championshipteams",
    "players", "summary", "fullprofile", "matches", "pressroom", "moneymovements", "rosterbids", "myplayers",
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
    response <- futmondo_post("https://api.futmondo.com/1/league/championshipteams", body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
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
  }, context=resource_context(login=login,championship_id=championship_id))
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
    response <- futmondo_post(ACTIVE_CHAMPIONSHIPS_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
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
  }, context=resource_context(login=login))
}


empty_finished_rounds <- function() data.frame(round_id=character(), round_number=numeric(), begin_process=character(), is_finished=logical(), stringsAsFactors=FALSE)

fm_round_flag <- function(x) {
  if (is.logical(x)) return(length(x) == 1L && isTRUE(x))
  if (is.numeric(x)) return(length(x) == 1L && is.finite(x) && x != 0)
  length(x) == 1L && tolower(trimws(as.character(x))) %in% c("true", "t", "1", "yes", "y")
}
round_is_finished <- function(r) {
  flags <- c("isFinished","finished","isClosed","closed","isCompleted","completed","finalized")
  if (is.list(r) && any(vapply(flags, function(n) !is.null(r[[n]]) && fm_round_flag(r[[n]]), logical(1)))) return(TRUE)
  is.list(r) && tolower(trimws(as.character(r[["status"]] %||% ""))) %in% c("done","finished","closed","final","completed","complete","ended")
}
round_reference <- function(round_id, round_number=NA_real_) {
  id <- as.character(round_id %||% "")[1]; if (is.na(id) || !nzchar(id)) return(NULL)
  list(round_id=id, round_number=suppressWarnings(as.numeric(round_number)[1]))
}
empty_round_catalog <- function(status="unavailable") list(status=status, rounds=empty_finished_rounds())

get_finished_rounds <- function(login, championship_id) {
  catalog <- get_award_round_catalog(login, championship_id)
  rounds <- catalog$rounds
  attr(rounds,"catalog_status") <- catalog$status
  attr(rounds,"catalog_complete") <- isTRUE(catalog$complete)
  attr(rounds,"catalog_reason") <- catalog$reason %||% ""
  attr(rounds,"observed_at") <- attr(catalog,"observed_at")
  attr(rounds,"valid_until") <- attr(catalog,"valid_until")
  rounds
}

acquisition_data_unverified <- function(data) {
  isTRUE(any(attr(data,"fetch_status")%in%c("stale","partial","unavailable")))
}

acquisition_source_deadline <- function(data,ttl,fallback=Sys.time()) {
  stamp<-fm_time(attr(data,"observed_at") %||% fallback)
  if(length(stamp)!=1L||is.na(stamp))stamp<-fm_time(fallback)
  as.numeric(stamp)+ttl
}

roster_capacity_facts <- function(roster) {
  if(!is.data.frame(roster)||!"id"%in%names(roster))stop("Roster identities unavailable")
  ids<-as.character(roster$id)
  if(anyNA(ids)||any(!nzchar(ids))||anyDuplicated(ids))stop("Roster identities unavailable")
  out<-data.frame(id=ids,value=if("value"%in%names(roster))suppressWarnings(as.numeric(roster$value))else rep(NA_real_,length(ids)),stringsAsFactors=FALSE)
  for(field in c("fetch_status","observed_at","fetched_at","fetch_error"))attr(out,field)<-attr(roster,field)
  out
}

# Keep every received bid while legacy card/table fields retain one highest offer.
received_offers_empty <- function() data.frame(id=character(),bid_price=numeric(),
  bid_user=character(),bid_id=character(),bidder_team_id=character(),bid_source=character(),stringsAsFactors=FALSE)

normalize_received_offer <- function(bid,player_id) {
  if(!is.list(bid))stop("Received offer data unavailable")
  team <- bid[["userTeam"]]
  bidder_id <- fm_scalar(extract_bidder_team_id(bid),"")
  name <- if(is.list(team))fm_scalar(team$name,"")else ""
  slug <- if(is.list(team))fm_scalar(team$slug,"")else ""
  source <- if(nzchar(bidder_id)||nzchar(name)||nzchar(slug))"rival"else
    if(is.null(team)||!length(team)||(is.list(team)&&all(vapply(team,function(value)
      is.null(value)||!length(value)||(length(value)==1L&&(is.na(value)||identical(value,""))),logical(1)))))"system"else "unknown"
  user <- if(source=="system")"Futmondo"else if(nzchar(name))name else
    if(nzchar(slug))slug else if(nzchar(bidder_id))bidder_id else "Unknown bidder"
  data.frame(id=fm_scalar(player_id,""),bid_price=fm_number(bid$price),bid_user=user,
    bid_id=fm_scalar(bid$id %||% bid$`_id`,""),bidder_team_id=bidder_id,bid_source=source,stringsAsFactors=FALSE)
}

merge_received_offers <- function(...) {
  rows <- dplyr::bind_rows(list(...))
  if(!nrow(rows))return(received_offers_empty())
  # A supplemental response may repeat or revise a bid already in the roster.
  keys <- ifelse(nzchar(rows$bid_id),paste(rows$id,rows$bid_id,sep=":"),
    paste(rows$id,rows$bid_price,rows$bid_user,rows$bidder_team_id,rows$bid_source,sep=":"))
  rows <- rows[!duplicated(keys,fromLast=TRUE),,drop=FALSE]
  rownames(rows)<-NULL;rows
}

highest_received_offers <- function(rows) {
  if(!nrow(rows))return(rows)
  rows %>% dplyr::group_by(id) %>%
    dplyr::slice_max(order_by=ifelse(is.na(bid_price),Inf,bid_price),n=1,with_ties=FALSE) %>%
    dplyr::ungroup() %>% as.data.frame()
}

get_players_from_team <- function(login, championship_id, user_team_id, teams = NULL, facts_only = FALSE) {
  ctx<-resource_context(login=login,championship_id=championship_id,user_team_id=user_team_id)
  if(isTRUE(facts_only)){
    # A fresh full roster already contains these facts. Never let a minimal
    # snapshot occupy the full presentation cache or inherit stale identities.
    full<-api_cache_env[[api_cache_key(paste0("roster_",championship_id,"_",user_team_id),context=ctx)]]
    if(is.data.frame(full$data)&&!acquisition_data_unverified(full$data)&&
       as.numeric(Sys.time())<acquisition_source_deadline(full$data,30,full$time))return(roster_capacity_facts(full$data))
  }
  cache_key <- paste0(if(isTRUE(facts_only))"roster_facts_"else"roster_", championship_id, "_", user_team_id)
  # Parsing/presentation never restarts the raw observation's freshness clock.
  for(name in c(cache_key,paste0("roster_payload_",championship_id,"_",user_team_id))){
    key<-api_cache_key(name,context=ctx);cached<-api_cache_env[[key]]
    if(!is.null(cached$data)&&as.numeric(Sys.time())>=acquisition_source_deadline(cached$data,30,cached$time))rm(list=key,envir=api_cache_env)
  }
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

    roster<-get_cached_data(paste0("roster_payload_",championship_id,"_",user_team_id),{
      response <- futmondo_post(ROSTER_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
      answer<-httr::content(response)[["answer"]]
      if (!is.list(answer) || !is.null(answer$code) || isTRUE(answer$error)) stop("Roster unavailable")
      answer
    },timeout_sec=30,context=ctx)
    roster_metadata<-lapply(c("fetch_status","observed_at","fetched_at","fetch_error"),function(field)attr(roster,field));names(roster_metadata)<-c("fetch_status","observed_at","fetched_at","fetch_error")
    if(isTRUE(facts_only)){
      facts<-data.frame(id=vapply(roster,function(player)fm_scalar(player$id,""),character(1)),
        value=vapply(roster,function(player)fm_number(player$value),numeric(1)),stringsAsFactors=FALSE)
      for(field in c("fetch_status","observed_at","fetched_at","fetch_error"))attr(facts,field)<-attr(roster,field)
      return(roster_capacity_facts(facts))
    }
    if (length(roster) == 0) {
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
      for(field in names(roster_metadata))attr(empty_df,field)<-roster_metadata[[field]]
      return(empty_df)
    }
    received_offers <- dplyr::bind_rows(lapply(roster,function(player) {
      bids <- if(is.list(player$market))player$market$bids else NULL
      dplyr::bind_rows(lapply(bids,normalize_received_offer,player_id=player$id %||% player$`_id`))
    }))
    if(!nrow(received_offers))received_offers<-received_offers_empty()
    roster <- lapply(roster, FUN = function(player) {
      player <- normalize_player_fields(player)
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
            bids_df <- highest_received_offers(dplyr::bind_rows(lapply(bids,normalize_received_offer,player_id=player$id)))
            bids <- if(nrow(bids_df))as.list(bids_df[1,c("bid_price","bid_user","bid_id"),drop=FALSE])else NULL
          } else bids <- NULL
          market <- market[-which(names(market) == "bids")]
        }
        if (length(market)) names(market) <- paste0("market_", names(market))
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
    for (field in c("value", "change", "points")) {
      if (all(is.na(roster[[field]]))) roster[[field]] <- rep(NA_real_, nrow(roster))
    }

    # add championship_id, user_team_id
    roster$championship_id <- championship_id
    roster$user_team_id <- user_team_id
    if (!is.null(teams)) {
      roster <- roster %>%
        dplyr::left_join(teams %>% dplyr::distinct(id, teamname), by = c("user_team_id" = "id"))
    }
    
    # Join real-world team name and logo
    roster <- enrich_player_clubs(roster, login, championship_id)

    # Join active roster bids if present
    offers_failure <- NULL
    roster_bids_df <- tryCatch({
      get_roster_bids(login = login, championship_id = championship_id, user_team_id = user_team_id)
    }, error = function(e) {offers_failure <<- resource_failure(e,"get_roster_bids");NULL})
    if(!is.null(roster_bids_df))received_offers<-merge_received_offers(received_offers,
      attr(roster_bids_df,"received_offers") %||% roster_bids_df)

    if(nrow(received_offers)&&"id"%in%names(roster)) {
      roster <- roster %>% dplyr::select(!any_of(c("bid_price","bid_user","bid_id","bidder_team_id","bid_source")))
      roster <- roster %>% dplyr::left_join(highest_received_offers(received_offers),by="id")
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

    for(field in names(roster_metadata))attr(roster,field)<-roster_metadata[[field]]
    attr(roster,"received_offers")<-received_offers
    attr(roster,"received_offers_status")<-if(!is.null(offers_failure)||
      !is.null(attr(roster_bids_df,"fetch_error")))"partial"else
      if(identical(attr(roster_bids_df,"fetch_status"),"stale"))"stale"else "ok"
    attr(roster,"received_offers_error")<-offers_failure %||% attr(roster_bids_df,"fetch_error")
    offer_stamps<-c(fm_time(roster_metadata$observed_at),fm_time(attr(roster_bids_df,"observed_at")))
    offer_stamps<-offer_stamps[!is.na(offer_stamps)]
    attr(roster,"received_offers_observed_at")<-if(length(offer_stamps))min(offer_stamps)else NULL
    roster
  }, timeout_sec = 30, context=resource_context(login=login,championship_id=championship_id,user_team_id=user_team_id))
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
    response <- futmondo_post(MARKET_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
    players <- httr::content(response)$answer
    if (!is.list(players) || !is.null(players$code) || isTRUE(players$error)) stop("Market data unavailable")
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
    # Explicit absent-own-bid columns distinguish a valid market with no own
    # bids from an incomplete/malformed frame at display consumers.
    if (!"bid_id" %in% names(ret)) ret$bid_id <- rep(NA_character_, nrow(ret))
    if (!"bid_price" %in% names(ret)) ret$bid_price <- rep(NA_real_, nrow(ret))
    ret <- ret %>% dplyr::arrange(desc(change))
    ret
  }, context=resource_context(login=login,championship_id=championship_id,user_team_id=user_team_id))
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
    response <- futmondo_post(CHAMPIONSHIP_PLAYERS, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
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
    ret <- enrich_player_clubs(ret, login, championship_id)
    
    ret <- ret %>% dplyr::arrange(name)
    ret
  }, context=resource_context(login=login,championship_id=championship_id))
}


# Remove absent optional object fields without compacting observation arrays.
# In particular, a null fitness entry must remain unknown, not shorten history.
normalize_player_fields <- function(player) {
  if (!is.list(player) || !length(player)) stop("Player data unavailable")
  compact <- function(fields) {
    if (!is.list(fields) || is.null(names(fields))) return(fields)
    fields <- lapply(fields, compact)
    fields[lengths(fields) > 0L]
  }
  player <- compact(player)
  for (field in c("value", "change", "points")) {
    if (!length(player[[field]])) player[[field]] <- NA_real_
  }
  player
}

# Club names/logos are optional enrichment; source player metadata takes priority.
enrich_player_clubs <- function(players, login, championship_id) {
  if (!"teamId" %in% names(players)) return(players)
  clubs <- tryCatch(get_real_clubs(login, championship_id), error = function(e) NULL)
  if (!is.data.frame(clubs) || !nrow(clubs) || !"teamId" %in% names(clubs)) return(players)
  rows <- match(as.character(players$teamId), as.character(clubs$teamId))
  for (field in intersect(c("team", "logo"), names(clubs))) {
    values <- clubs[[field]][rows]
    if (!field %in% names(players)) {
      players[[field]] <- values
    } else {
      missing <- is.na(players[[field]]) | !nzchar(as.character(players[[field]]))
      players[[field]][missing] <- values[missing]
    }
  }
  players
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
  player <- normalize_player_fields(player)
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

  response <- futmondo_post(LINEUP_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
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
  }, timeout_sec = 30, context=resource_context(login=login,championship_id=championship_id,user_team_id=user_team_id))
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
    "name", "role", "team", "selling_by", "market_inMarket", "bid_price", "bid_user", "numberOfBids",
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

# Asking prices/offers stay numeric; this formatter adds a display-only P/L.
format_table_sale_price <- function(value, acquisition_cost, multiline = FALSE) {
  amount <- suppressWarnings(as.numeric(as.character(value)))
  cost <- suppressWarnings(as.numeric(as.character(acquisition_cost)))
  if(length(amount)!=1L || !is.finite(amount) || amount<=0)return("")
  price <- format_table_currency(amount)
  profit <- if(length(cost)!=1L || !is.finite(cost) || cost<0) {
    "(P/L unavailable)"
  } else {
    millions <- round((amount-cost)/1e6,2)
    if(millions==0)millions<-0
    paste0("(",if(millions>0)"+"else"",sprintf("%.2f",millions)," M\u20ac)")
  }
  if(isTRUE(multiline))return(shiny::tags$span(class="fm-sale-price",
    shiny::tags$span(price),shiny::tags$span(profit)))
  paste(price,profit)
}

get_reactable_columns_for_players <- function(table, show_sale_profit = FALSE) {
  columns <- list()
  sale_price <- function(value,index) {
    if(!isTRUE(show_sale_profit))return(format_table_currency(value))
    cost <- if("sale_acquisition_cost"%in%names(table))table$sale_acquisition_cost[index]else
      current_player_acquisition_cost(table[index,,drop=FALSE],NULL,NULL)
    format_table_sale_price(value,cost,multiline=TRUE)
  }

  for(field in intersect(c("bid_source","bidder_team_id","sale_acquisition_cost"),names(table)))columns[[field]]<-colDef(show=FALSE)

  # Keep names as plain sortable/filterable values and show the club crest beside them.
  # Club metadata stays in the payload for this formatter, without a separate Team column.
  if ("logo" %in% names(table)) columns[["logo"]] <- colDef(show=FALSE)
  if ("team" %in% names(table)) columns[["team"]] <- colDef(show=FALSE)
  if ("name" %in% names(table)) columns[["name"]] <- colDef(
    name="Name", minWidth=180,
    cell=function(value, index) {
      if(is.null(value) || is.na(value) || !nzchar(value)) return("")
      club <- if("team" %in% names(table)) as.character(table$team[index]) else ""
      if(is.na(club)) club <- ""
      logo <- if("logo" %in% names(table)) table$logo[index] else NULL
      image_name <- get_team_image_name(club, logo=logo)
      crest <- if(nzchar(image_name)) shiny::tags$img(
        src=paste0(TEAM_LOGO_URL,image_name,".png"),alt=club,width=22,height=22,
        loading="lazy",decoding="async",class="fm-team-crest",
        onerror="this.style.display='none';") else NULL
      shiny::tags$span(class="fm-team-label",title=if(nzchar(club))club else NULL,
        style="max-width: 100%;",crest,
        shiny::tags$span(style="min-width: 0; white-space: normal; overflow-wrap: anywhere;",value))
    }
  )

  # Bids Column ----
  if ("numberOfBids" %in% colnames(table)) {
    columns[["numberOfBids"]] <- colDef(
      name = "Bids",
      align = "center",
      minWidth = 64, resizable = TRUE
    )
  }

  # Your Bid / Received Offer Column ----
  if ("bid_price" %in% colnames(table)) {
    columns[["bid_price"]] <- colDef(
      name = "Offer",
      align = "right",
      minWidth = 180,
      cell = function(value, index) {
        if (is.na(value) || is.null(value) || value == "" || value == 0) return("")
        num_val <- suppressWarnings(as.numeric(value))
        formatted <- sale_price(num_val,index)

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
      minWidth = 180,
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
      minWidth = 180,
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
      align = "center",
      minWidth = 84, resizable = TRUE
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

  for (position_column in intersect(c("role", "primary_role", "secondary_role"), colnames(table))) {
    columns[[position_column]] <- colDef(
      name = switch(position_column, primary_role="Primary position", secondary_role="Secondary position", "Position"),
      align = "center",
      minWidth = 112, resizable = TRUE,
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

          short_name <- switch(display_name, Goalkeeper="GK", Defender="DEF", Midfielder="MID", Forward="FWD", display_name)
          shiny::tags$span(class = class_name, title = display_name, short_name)
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
  if ("average.fitness" %in% names(table)) {
    columns[["average.fitness"]] <- colDef(name="Latest 5",align="center")
  }

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
      name = "Sum last 5",
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

  if("clause_ratio"%in%colnames(table))columns[["clause_ratio"]]<-colDef(name="Clause / market value",align="center",cell=function(value){number<-suppressWarnings(as.numeric(value));if(length(number)!=1L||!is.finite(number))"Unavailable"else sprintf("%.2fx",number)})

  if("selling_by"%in%names(table)) {
    columns[["selling_by"]]<-colDef(name="Selling by",align="right",minWidth=150,
      cell=function(value,index)sale_price(value,index))
    for(field in intersect(c("market_inMarket","price","market_price","effective_market_price","market_asking_price"),names(table)))
      columns[[field]]<-colDef(show=FALSE)
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
    response <- futmondo_post(TEAMS_URL, body = toJSON(payload, auto_unbox = TRUE), add_headers(.headers = headers))
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
        ret$points <- NA_real_
      }
    }
    
    print(paste0(nrow(ret), " teams retrieved"))
    ret
  }, context=resource_context(login=login,championship_id=championship_id))
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
    # A named empty list encodes as the required JSON object {}; list() becomes [].
    answer = setNames(list(), character())
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

    list(success = is_success, code = operation_code,
         message = clause_buyout_error_message(operation_code, err_msg))
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

capacity_roster_message <- function(roster, commitments=0L) {
  n <- function(x,default=NA_integer_) { v<-suppressWarnings(as.integer(x)); if(length(v)==1L&&!is.na(v)) v else default }
  raw<-n(roster$count); out<-n(roster$pending_outbound_count,0L); incoming<-n(roster$pending_inbound_count,0L)
  effective<-n(roster$effective_count); cap<-n(roster$cap); commitments<-n(commitments,0L)
  if(anyNA(c(raw,effective,cap))) return("Roster capacity cannot be verified. Refresh the data before buying another player.")
  paste0("Roster capacity reached: ",effective," effective players (",raw," current - ",out,
    " pending departure",if(out==1L) "" else "s"," + ",incoming," pending arrival",if(incoming==1L) "" else "s",
    ")",if(commitments>0) paste0(" + ",commitments," active offer",if(commitments==1L) "" else "s") else "",
    ", maximum ",cap,". Sell a player first before making another purchase.")
}

clause_buyout_error_message <- function(code,message=NULL,roster=NULL) {
  code <- if(length(code)==1L&&!is.na(code)) trimws(as.character(code)) else ""
  if(identical(code,"api.market.max_number_players_in_roster")) {
    if(is.list(roster)) return(capacity_roster_message(roster))
    return("Your effective roster is at the league maximum. Pending clause arrivals also consume roster capacity. Sell a player first, then refresh before exercising another clause.")
  }
  if(length(message)==1L&&!is.na(message)&&nzchar(as.character(message))) as.character(message) else if(nzchar(code)) code else "Please refresh and try again."
}

pending_clause_candidates <- function(roster_df,pressroom_df,user_team_id,previous_records=NULL,previous_outgoing_records=NULL) {
  if(!is.data.frame(roster_df)||!"id"%in%names(roster_df)||!is.data.frame(pressroom_df)||
      !all(c("player_id","buyer_team_id","seller_team_id","created")%in%names(pressroom_df)))return(NULL)
  roster_ids<-as.character(roster_df$id)
  if(anyNA(roster_ids)||any(!nzchar(roster_ids))||anyDuplicated(roster_ids))return(NULL)
  tx<-pressroom_df
  relevant<-unique(tx$player_id[tx$buyer_team_id%in%user_team_id|tx$seller_team_id%in%user_team_id])
  tx<-tx[tx$player_id%in%relevant,,drop=FALSE]
  if(anyNA(tx$player_id)||any(!nzchar(as.character(tx$player_id))))return(NULL)
  if(nrow(tx)){
    when<-fm_time(tx$created)
    if(anyNA(when))return(NULL)
    tx<-tx[order(when,seq_len(nrow(tx))),,drop=FALSE]
    tx<-tx[!duplicated(as.character(tx$player_id),fromLast=TRUE),,drop=FALSE]
  }
  # Missing counterparty IDs mean System, and must never create NA rows.
  mask<-(tx$buyer_team_id%in%user_team_id & !tx$player_id%in%roster_ids)|
    (tx$seller_team_id%in%user_team_id & tx$player_id%in%roster_ids)
  candidates<-tx[mask,,drop=FALSE]
  # History may be truncated or temporarily omit a previously confirmed buy.
  # Keep it until a fresh roster/summary or a newer sale confirms resolution.
  if(is.data.frame(previous_records)&&nrow(previous_records)) {
    for(i in seq_len(nrow(previous_records))) {
      old<-previous_records[i,,drop=FALSE];pid<-as.character(old$player_id)
      if(pid%in%c(roster_ids,as.character(candidates$player_id)))next
      latest<-tx[tx$player_id%in%pid,,drop=FALSE]
      sold_after<-nrow(latest)&&latest$seller_team_id%in%user_team_id&&
        isTRUE(fm_time(latest$created)>=old$purchased_at)
      if(sold_after)next
      extra<-pressroom_df[NA_integer_,,drop=FALSE]
      extra$player_id<-pid;extra$buyer_team_id<-user_team_id;extra$seller_team_id<-old$seller_team_id
      extra$created<-format(old$purchased_at,"%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
      if("player_name"%in%names(extra))extra$player_name<-old$player_name
      if("id"%in%names(extra))extra$id<-paste0("pending:",pid)
      candidates<-rbind(candidates,extra)
    }
  }
  # A temporarily omitted sale remains pending while a fresh roster still owns
  # the player. A newer purchase back into this team retires the earlier sale.
  if(is.data.frame(previous_outgoing_records)&&nrow(previous_outgoing_records)) {
    for(i in seq_len(nrow(previous_outgoing_records))) {
      old<-previous_outgoing_records[i,,drop=FALSE];pid<-as.character(old$player_id)
      if(!pid%in%roster_ids||pid%in%as.character(candidates$player_id))next
      latest<-tx[tx$player_id%in%pid,,drop=FALSE]
      bought_after<-nrow(latest)&&latest$buyer_team_id%in%user_team_id&&
        isTRUE(fm_time(latest$created)>=old$purchased_at)
      if(bought_after)next
      extra<-pressroom_df[NA_integer_,,drop=FALSE]
      extra$player_id<-pid;extra$buyer_team_id<-old$buyer_team_id;extra$seller_team_id<-user_team_id
      extra$created<-format(old$purchased_at,"%Y-%m-%dT%H:%M:%OSZ",tz="UTC")
      if("player_name"%in%names(extra))extra$player_name<-old$player_name
      if("id"%in%names(extra))extra$id<-paste0("pending-out:",pid)
      if("is_clause"%in%names(extra))extra$is_clause<-TRUE
      candidates<-rbind(candidates,extra)
    }
  }
  candidates
}

incoming_clause_empty <- function() {
  data.frame(player_id=character(), player_name=character(), seller_team_id=character(),
    purchased_at=as.POSIXct(character(),tz="UTC"), joins_at=as.POSIXct(character(),tz="UTC"),
    observed_at=as.POSIXct(character(),tz="UTC"), role=character(), team=character(),logo=character(),
    timing_error=character(), overdue=logical(),stringsAsFactors=FALSE)
}

outgoing_clause_empty <- function() {
  data.frame(player_id=character(),player_name=character(),seller_team_id=character(),buyer_team_id=character(),
    purchased_at=as.POSIXct(character(),tz="UTC"),leaves_at=as.POSIXct(character(),tz="UTC"),
    observed_at=as.POSIXct(character(),tz="UTC"),role=character(),team=character(),logo=character(),
    timing_error=character(),overdue=logical(),stringsAsFactors=FALSE)
}

reconcile_pending_clause_transfers <- function(roster_df,pressroom_df,summaries,user_team_id,now=Sys.time(),previous_records=NULL,previous_outgoing_records=NULL) {
  records<-incoming_clause_empty();outgoing_records<-outgoing_clause_empty();outgoing_verified<-FALSE
  result <- function(status="unavailable",outbound=character(),incoming=character()) list(status=status,
    pending_outbound_ids=unique(outbound),pending_inbound_ids=unique(incoming),
    pending_outbound_count=if(status=="ok")length(unique(outbound))else NA_integer_,
    pending_inbound_count=if(status=="ok")length(unique(incoming))else NA_integer_,
    incoming_records=records,outgoing_records=outgoing_records,
    outgoing_status=if(outgoing_verified)"ok"else"partial")
  team<-fm_scalar(user_team_id,"");if(!nzchar(team))return(result())
  candidate<-if(acquisition_data_unverified(roster_df)||acquisition_data_unverified(pressroom_df))NULL else pending_clause_candidates(roster_df,pressroom_df,team,previous_records,previous_outgoing_records)
  if(is.null(candidate)) {
    if(is.data.frame(previous_records))records<-previous_records
    if(is.data.frame(previous_outgoing_records))outgoing_records<-previous_outgoing_records
    return(result())
  }
  roster_ids<-as.character(roster_df$id);outgoing_verified<-TRUE
  if(!nrow(candidate))return(result("ok"))
  outbound<-incoming<-character();verified<-TRUE
  for(i in seq_len(nrow(candidate))) {
    tx<-candidate[i,,drop=FALSE];pid<-as.character(tx$player_id);summary<-summaries[[pid]]
    incoming_purchase<-tx$buyer_team_id%in%team&&!pid%in%roster_ids
    # A summary for a different player cannot certify transfers or capacity.
    source_id<-if(is.list(summary)&&is.list(summary$data))fm_scalar(summary$data$id %||% summary$data$`_id`,"")else ""
    summary_matches<-!nzchar(source_id)||identical(source_id,pid)
    fresh<-is.list(summary)&&length(summary)>0L&&!acquisition_data_unverified(summary)&&summary_matches
    championship<-if(fresh&&is.list(summary$championship))summary$championship else list()
    clause<-championship$clause
    owner<-if(is.list(championship$owner))
      fm_scalar(championship$owner$`_id` %||% championship$owner$id,"")else ""
    transferred<-is.list(clause)&&identical(clause$transferred,TRUE)
    transfer_at<-if(is.list(clause))fm_time(clause$transferDate %||% NA_character_)else as.POSIXct(NA,tz="UTC")
    valid_time<-length(transfer_at)==1L&&!is.na(transfer_at)
    # Capacity remains fail-closed on ambiguous, stale or overdue observations.
    usable<-fresh&&transferred&&valid_time&&transfer_at>now&&nzchar(owner)
    if(usable&&tx$seller_team_id%in%team&&pid%in%roster_ids&&owner==team)outbound<-c(outbound,pid)
    else if(usable&&incoming_purchase&&owner!=team)incoming<-c(incoming,pid)
    else verified<-FALSE
    outgoing_sale<-tx$seller_team_id%in%team&&pid%in%roster_ids
    if(outgoing_sale) {
      old_out<-if(is.data.frame(previous_outgoing_records)&&"player_id"%in%names(previous_outgoing_records))
        previous_outgoing_records[previous_outgoing_records$player_id%in%pid,,drop=FALSE]else outgoing_clause_empty()
      # Profile IDs, when supplied, must match the requested player before any
      # departure details can be attached. Missing summary data remains unknown.
      out_fresh<-fresh
      out_transferred<-out_fresh&&transferred
      out_valid_time<-out_fresh&&valid_time
      # Ordinary sales must not become clause departures. An explicit source
      # cancellation or ownership change resolves even a previously confirmed row.
      resolved_out<-out_fresh&&((nzchar(owner)&&owner!=team)||
        (is.list(clause)&&identical(clause$transferred,FALSE)))
      if(!resolved_out&&!(out_fresh&&nzchar(owner)&&owner==team&&out_transferred&&out_valid_time&&transfer_at>now))outgoing_verified<-FALSE
      clause_evidence<-out_transferred||isTRUE(tx$is_clause)||nrow(old_out)>0L
      if(!resolved_out&&clause_evidence) {
        data<-if(out_fresh&&is.list(summary$data))summary$data else list()
        field<-function(name,fallback="")fm_scalar(data[[name]],fm_scalar(tx[[name]],fallback))
        stamp<-attr(summary,"observed_at") %||% as.POSIXct(NA,tz="UTC")
        issue<-if(!summary_matches)"Departure verification failed: the player summary identified a different player."else
          if(!out_fresh)"Departure verification failed: the player summary could not be retrieved."else
          if(!nzchar(owner))"Departure verification failed: the player summary omitted the current owner."else
          if(!transferred)"Departure verification failed: the player summary omitted the pending clause status."else
          if(!valid_time)"Departure time missing or malformed in Futmondo's transferDate."else ""
        departure<-if(out_valid_time)transfer_at else if(nrow(old_out))old_out$leaves_at[1]else as.POSIXct(NA,tz="UTC")
        outgoing_records<-rbind(outgoing_records,data.frame(player_id=pid,
          player_name=field("name",fm_scalar(tx$player_name,if(nrow(old_out))old_out$player_name[1]else pid)),
          seller_team_id=team,buyer_team_id=fm_scalar(tx$buyer_team_id,""),purchased_at=fm_time(tx$created),leaves_at=departure,
          observed_at=fm_time(stamp),role=field("role",if(nrow(old_out))old_out$role[1]else ""),
          team=field("team",if(nrow(old_out))old_out$team[1]else ""),logo=field("logo",if(nrow(old_out))old_out$logo[1]else ""),
          timing_error=issue,overdue=length(departure)==1L&&!is.na(departure)&&departure<=now,stringsAsFactors=FALSE))
      }
    }
    if(!incoming_purchase)next
    # Only a fresh, explicit source result confirms arrival/cancellation. A
    # passed transferDate by itself never removes a confirmed incoming player.
    resolved<-fresh&&nzchar(owner)&&(owner==team||
      (is.list(clause)&&identical(clause$transferred,FALSE)))
    if(resolved)next
    old<-if(is.data.frame(previous_records)&&"player_id"%in%names(previous_records))
      previous_records[previous_records$player_id%in%pid,,drop=FALSE]else incoming_clause_empty()
    data<-if(fresh&&is.list(summary$data))summary$data else list()
    field<-function(name,fallback="")fm_scalar(data[[name]],fm_scalar(tx[[name]],fallback))
    stamp<-attr(summary,"observed_at") %||% as.POSIXct(NA,tz="UTC")
    issue<-if(!fresh)"Arrival verification failed: the player summary could not be retrieved."else
      if(!nzchar(owner))"Arrival verification failed: the player summary omitted the current owner."else
      if(!transferred)"Arrival verification failed: the player summary omitted the pending clause status."else
      if(!valid_time)"Arrival time missing or malformed in Futmondo's transferDate."else ""
    arrival<-if(valid_time)transfer_at else if(nrow(old))old$joins_at[1]else as.POSIXct(NA,tz="UTC")
    records<-rbind(records,data.frame(player_id=pid,player_name=field("name",fm_scalar(tx$player_name,pid)),
      seller_team_id=fm_scalar(tx$seller_team_id,""),purchased_at=fm_time(tx$created),joins_at=arrival,
      observed_at=fm_time(stamp),role=field("role",if(nrow(old))old$role[1]else ""),
      team=field("team",if(nrow(old))old$team[1]else ""),logo=field("logo",if(nrow(old))old$logo[1]else ""),
      timing_error=issue,overdue=length(arrival)==1L&&!is.na(arrival)&&arrival<=now,stringsAsFactors=FALSE))
  }
  if(nrow(records))records<-records[order(records$joins_at,records$player_id,na.last=TRUE),,drop=FALSE]
  if(nrow(outgoing_records))outgoing_records<-outgoing_records[order(outgoing_records$leaves_at,outgoing_records$player_id,na.last=TRUE),,drop=FALSE]
  result(if(verified)"ok"else"unavailable",outbound,incoming)
}

# These are absolute source timestamps, formatted only at the UI boundary.
incoming_clause_time <- function(value) {
  stamp<-fm_time(value)
  if(length(stamp)!=1L||is.na(stamp))return("")
  format(stamp,"%d %b %Y, %H:%M %Z",tz="Europe/Madrid")
}
incoming_clause_round_label <- function(joins_at,next_round) {
  stamp<-fm_time(joins_at)
  if(length(stamp)!=1L||is.na(stamp)||!isTRUE(next_round$available))return("")
  delta<-as.numeric(difftime(stamp,next_round$starts_at,units="secs"))
  if(!is.finite(delta))return("")
  if(delta<0)"Before round starts"else if(delta>0)"After round starts"else "At round start"
}

# Pure calculation shared by synchronous manual checks and asynchronous display.
calculate_team_acquisition_capacity <- function(info,roster_df,pressroom,market,summaries,user_team_id,now=Sys.time()) {
  empty_target <- list(my_bid_id=NULL,my_bid_amount=NA_real_,highest_bid=NA_real_,bid_count=NA_integer_)
    diagnostics <- character(0)

    # ---- 1. Team info: roster cap, reported budget, withheld ----
    cap_val <- NA_integer_
    budget_val <- NA_real_
    team_value_val <- NA_real_
    withheld_val <- NA_real_
    debt_fraction <- NA_real_
    withheld_valid <- FALSE
    if (is.null(info)) {
      diagnostics <- c(diagnostics, "team info unavailable")
    } else {
      cfg <- if (is.list(info)) info[["configuration"]] else NULL
      debt_fraction <- fm_number(cfg$mnmp)
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
    if (is.null(roster_df)) {
      diagnostics <- c(diagnostics, "roster fetch failed")
    } else if(!"id"%in%names(roster_df)||anyNA(roster_df$id)||any(!nzchar(as.character(roster_df$id)))||anyDuplicated(roster_df$id)){
      diagnostics<-c(diagnostics,"roster identities unavailable")
      roster_df<-NULL
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

    # ---- 3. Delayed clause transfers ----
    transfer_state <- list(status="unavailable",pending_outbound_ids=character(),pending_inbound_ids=character(),
      pending_outbound_count=NA_integer_,pending_inbound_count=NA_integer_)
    if(is.data.frame(roster_df)&&is.data.frame(pressroom)&&!acquisition_data_unverified(pressroom)) {
      transfer_state<-reconcile_pending_clause_transfers(roster_df,pressroom,summaries,user_team_id,now=now)
    }
    if(!identical(transfer_state$status,"ok")) diagnostics<-c(diagnostics,"pending clause transfers unavailable")

    if (!is.finite(team_value_val) || team_value_val < 0)
      diagnostics <- c(diagnostics, "team value unavailable (debt limit unverified)")
    # Own bids come from market listings. Roster bids are incoming offers.
    bids_complete <- FALSE; bid_count <- NA_integer_; bid_total <- NA_real_
    target <- empty_target
    target_rows<-data.frame()
    if (is.data.frame(market) && !acquisition_data_unverified(market)) {
      bids_complete <- TRUE
      own <- if ("bid_id" %in% names(market)) which(!is.na(market$bid_id) & nzchar(as.character(market$bid_id))) else integer()
      my_bids <- market[own,,drop=FALSE]
      if (nrow(my_bids)) my_bids <- my_bids[!duplicated(my_bids$bid_id),,drop=FALSE]
      prices <- if (nrow(my_bids) && "bid_price" %in% names(my_bids)) suppressWarnings(as.numeric(my_bids$bid_price)) else numeric()
      bids_complete <- !nrow(my_bids) || (length(prices)==nrow(my_bids) && all(is.finite(prices) & prices>0))
      bid_count <- nrow(my_bids)
      bid_total <- if (bids_complete) sum(prices) else NA_real_
      target_rows<-as.data.frame(market)[,intersect(c("id","bid_id","bid_price","numberOfBids"),names(market)),drop=FALSE]
    }
    if (!bids_complete) diagnostics <- c(diagnostics,"outgoing market commitments unavailable")
    if(acquisition_data_unverified(info)||acquisition_data_unverified(roster_df)){
      bids_complete<-FALSE
      diagnostics<-c(diagnostics,"team or roster data is stale or incomplete")
    }

    # ---- 5. Status + spendable funds ----
    # A missing league percentage never becomes an assumed borrowing allowance.
    if(!is.finite(debt_fraction)||debt_fraction<0)
      diagnostics<-c(diagnostics,"league borrowing percentage unavailable")
    borrowing <- acquisition_headroom(budget_val, team_value_val, withheld_val, bid_total, debt_fraction)
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
    } else if (!withheld_valid || !is.finite(borrowing$spendable_budget)) {
      # Structural data alone cannot establish funds when held funds or the
      # configured borrowing allowance are missing/invalid.
      status <- "partial"
    } else {
      status <- "ok"
    }

    effective_count <- if(!is.na(roster_count)&&identical(transfer_state$status,"ok"))
      as.integer(roster_count-transfer_state$pending_outbound_count+transfer_state$pending_inbound_count) else NA_integer_
    remaining_slots <- if (!is.na(roster_count) && !is.na(cap_val)) as.integer(cap_val-roster_count) else NA_integer_
    effective_remaining <- if(!is.na(effective_count)&&!is.na(cap_val)) as.integer(cap_val-effective_count) else NA_integer_

    list(
      status = status,
      roster = list(
        status=transfer_state$status,
        count = roster_count,
        raw_count=roster_count,
        cap = cap_val,
        remaining_slots = remaining_slots,
        effective_count=effective_count,
        effective_remaining_slots=effective_remaining,
        pending_outbound_ids=transfer_state$pending_outbound_ids,
        pending_outbound_count=transfer_state$pending_outbound_count,
        pending_inbound_ids=transfer_state$pending_inbound_ids,
        pending_inbound_count=transfer_state$pending_inbound_count
      ),
      rules = list(minimum_bid_at_value = info$configuration$mbmv %||% NA),
      funds = list(
        debt_fraction = debt_fraction,
        reported_budget = budget_val,
        withheld = withheld_val,
        team_value = team_value_val,
        debt_limit = borrowing$debt_limit,
        minimum_balance = borrowing$minimum_balance,
        projected_committed_balance = borrowing$projected_committed_balance,
        reserved_amount = borrowing$reserved_amount,
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
      valid_until = min(as.numeric(now)+15,acquisition_source_deadline(info,15),acquisition_source_deadline(roster_df,30),
        acquisition_source_deadline(market,20),acquisition_source_deadline(pressroom,300),
        if(length(summaries))min(vapply(summaries,acquisition_source_deadline,numeric(1),ttl=60))else Inf),
      target_rows = target_rows,
      own_bid_valid_until = if (is.data.frame(market) && !acquisition_data_unverified(market))
        min(as.numeric(now)+20, acquisition_source_deadline(market,20)) else NA_real_,
      diagnostics = unique(diagnostics)
    )
}

# Computes a verified acquisition-capacity snapshot for the logged-in team:
# roster count/cap, verified spendable funds, and outstanding active bids.
# Cached per (userid, championship, team); target projection performs no reads.
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
#                  AND valid withheld and configured borrowing fraction are present.
#                  A missing / non-finite / negative withheld makes funds
#                  verification incomplete -> status "partial" (fail closed; we
#                  never assume withheld = 0).
#   roster      -- list(count, cap, remaining_slots)
#   funds       -- list(reported_budget, withheld, team_value, debt_limit,
#                  minimum_balance, spendable_budget). Spendable funds include
#                  temporary borrowing using configuration$mnmp and verified team value.
#   rules       -- list(minimum_bid_at_value), from configuration$mbmv.
#   outstanding -- list(offers, count, total_amount, completeness)
#   target      -- list(my_bid_id, my_bid_amount, highest_bid, bid_count)
#   diagnostics -- character vector of data-availability notes
get_team_acquisition_capacity <- function(login, championship_id, user_team_id) {
  empty_target <- list(my_bid_id = NULL, my_bid_amount = NA_real_,
                       highest_bid = NA_real_, bid_count = NA_integer_)

  if (is.null(login) || is.null(championship_id) || is.null(user_team_id) ||
      is.null(login[["userid"]])) {
    return(list(
      status = "unavailable",
      roster = list(status="unavailable",count = NA_integer_, raw_count=NA_integer_,cap = NA_integer_,
        remaining_slots = NA_integer_,effective_count=NA_integer_,effective_remaining_slots=NA_integer_,
        pending_outbound_ids=character(),pending_outbound_count=NA_integer_,
        pending_inbound_ids=character(),pending_inbound_count=NA_integer_),
      funds = list(reported_budget = NA_real_, withheld = NA_real_, team_value = NA_real_,
        debt_limit = NA_real_, minimum_balance = NA_real_, projected_committed_balance = NA_real_,
        api_bid_limit = NA_real_, spendable_budget = NA_real_),
      outstanding = list(offers = NA_integer_, count = NA_integer_,
                         total_amount = NA_real_, completeness = "unavailable"),
      target = empty_target,
      diagnostics = c("login, championship_id, or user_team_id is missing")
    ))
  }

  cache_key <- paste0("acq_capacity_",login[["userid"]],"_",championship_id,"_",user_team_id)
  key<-api_cache_key(cache_key,context=resource_context(login,championship_id,user_team_id));cached<-api_cache_env[[key]]
  if(!is.null(cached$data$valid_until)&&as.numeric(Sys.time())>=cached$data$valid_until)rm(list=key,envir=api_cache_env)

  get_cached_data(cache_key, {
    info<-tryCatch(get_user_team_info(login,championship_id,user_team_id),error=function(e)NULL)
    roster<-tryCatch(get_players_from_team(login,championship_id,user_team_id,facts_only=TRUE),error=function(e)NULL)
    history<-tryCatch(get_championship_pressroom(login,championship_id),error=function(e)NULL)
    candidates<-if(is.data.frame(roster)&&is.data.frame(history)&&!acquisition_data_unverified(history))
      pending_clause_candidates(roster,history,user_team_id) else NULL
    ids<-if(is.data.frame(candidates))unique(as.character(candidates$player_id))else character()
    summaries<-setNames(lapply(ids,function(id)tryCatch(get_player_summary(login,championship_id,user_team_id,id),error=function(e)NULL)),ids)
    market<-tryCatch(get_market_players(login,championship_id,user_team_id),error=function(e)NULL)
    calculate_team_acquisition_capacity(info,roster,history,market,summaries,user_team_id)
  },timeout_sec=15,context=resource_context(login,championship_id,user_team_id))
}

# Independent prerequisites run together; each source owns its actual read clock.
# A slow history acquisition cannot renew an old balance or hide a changed roster.
get_team_acquisition_capacity_async <- function(login,championship_id,user_team_id,context,
  is_current=function()TRUE,on_progress=NULL,priority="interactive",owner=context$session_id) {
  base<-resource_context(login,championship_id,user_team_id)
  if(!identical(base$account_id,context$account_id)||!identical(base$championship_id,context$championship_id)||
     !identical(base$user_team_id,context$user_team_id))
    return(promises::promise_reject(resource_failure(resource="get_team_acquisition_capacity",category="schema")))
  current<-function()isTRUE(tryCatch(is_current(),error=function(e)FALSE))
  cancelled<-function()resource_failure(structure(list(message="Session request cancelled",call=NULL),class=c("fm_async_cancelled","error","condition")))
  if(!current())return(promises::promise_reject(cancelled()))
  cache_name<-paste0("acq_capacity_",login[["userid"]],"_",championship_id,"_",user_team_id)
  cache_key<-api_cache_key(cache_name,context=base)
  resource_cache_prune();cached<-api_cache_env[[cache_key]]
  if(!is.null(cached$data)&&identical(cached$data$status,"ok")&&identical(cached$data$roster$status,"ok")&&
     !acquisition_data_unverified(cached$data)&&"valid_until"%in%names(cached$data)&&resource_snapshot_unexpired(cached$data))
    return(promises::promise_resolve(cached$data))
  state<-new.env(parent=emptyenv());state$sources<-list();state$summaries<-list();state$errors<-list()
  args<-list(login=login,championship_id=championship_id,user_team_id=user_team_id)
  specs<-list(
    info=list(fn="get_user_team_info",args=args,ttl=15,resources=paste0("team_info_",championship_id,"_",user_team_id)),
    roster=list(fn="get_players_from_team",args=c(args,list(facts_only=TRUE)),ttl=30,
      resources=paste0(c("roster_","roster_facts_","roster_payload_"),championship_id,"_",user_team_id)),
    history=list(fn="get_championship_pressroom",args=args[c("login","championship_id")],ttl=300,resources=paste0("pressroom_",championship_id)),
    market=list(fn="get_market_players",args=args,ttl=20,resources=paste0("market_",championship_id,"_",user_team_id)))
  expired<-function(data,ttl,margin=0)is.null(data)||as.numeric(Sys.time())+margin>=acquisition_source_deadline(data,ttl)
  progress<-function(stage){if(is.function(on_progress))tryCatch(on_progress(stage),error=function(e)NULL);invisible(NULL)}
  invalidate<-function(spec){
    scope<-if(identical(spec$fn,"get_championship_pressroom"))resource_context(login,championship_id)else base
    if(!is.null(spec$args$player_id))scope$player_id<-spec$args$player_id
    invalidate_resources(scope,resources=spec$resources)
  }
  # Every completion, including synchronous exceptions and cancellation, settles
  # one slot. Full source failures remain values for the existing truthful gate.
  batch<-function(jobs,width=4L){
    if(!length(jobs))return(promises::promise_resolve(list()))
    promises::promise(function(resolve,reject){
      next_job<-1L;active<-0L;settled<-FALSE;out<-list()
      fail<-function(error){if(!settled){settled<<-TRUE;reject(error)};invisible(NULL)}
      pump<-function(){tryCatch({
        if(settled)return(invisible(NULL))
        if(!current())return(fail(cancelled()))
        if(next_job>length(jobs)&&active==0L){settled<<-TRUE;resolve(out);return(invisible(NULL))}
        while(active<width&&next_job<=length(jobs))local({
          index<-next_job;next_job<<-next_job+1L;active<<-active+1L
          name<-names(jobs)[index];spec<-jobs[[index]]
          ctx<-context;ctx$player_id<-spec$args$player_id %||% ""
          finish<-function(data=NULL,error=NULL){
            if(settled)return(NULL)
            if(!current())return(fail(cancelled()))
            if(inherits(error,"fm_async_cancelled"))return(fail(error))
            out[name]<<-list(list(data=data,error=if(is.null(error))attr(data,"fetch_error")else resource_failure(error,spec$fn)))
            active<<-active-1L;pump();NULL
          }
          request<-tryCatch(async_adapter_fetch(spec$fn,spec$args,context=ctx,is_current=current,priority=priority,owner=owner),
            error=function(error)promises::promise_reject(error))
          promises::then(request,function(data)finish(data),function(error)finish(error=error))
        })
        invisible(NULL)
      },error=fail)}
      pump()
    })
  }
  store_sources<-function(results){
    if(!current())stop(cancelled())
    for(name in names(results)){
      state$sources[name]<-list(results[[name]]$data)
      state$errors[name]<-list(results[[name]]$error)
    }
    NULL
  }
  candidate_ids<-function(){
    roster<-state$sources$roster;history<-state$sources$history
    candidates<-if(is.data.frame(roster)&&is.data.frame(history)&&!acquisition_data_unverified(roster)&&!acquisition_data_unverified(history))
      pending_clause_candidates(roster,history,user_team_id)else NULL
    ids<-if(is.data.frame(candidates))unique(as.character(candidates$player_id))else character()
    if(length(ids)>32L)stop(resource_failure(resource="get_team_acquisition_capacity",category="read_budget"))
    ids
  }
  load_summaries<-function(refresh_expired=FALSE){
    if(!current())stop(cancelled())
    ids<-candidate_ids()
    state$summaries<-state$summaries[intersect(names(state$summaries),ids)]
    summary_errors<-names(state$errors)[startsWith(names(state$errors),"summary:")]
    state$errors<-state$errors[setdiff(names(state$errors),setdiff(summary_errors,paste0("summary:",ids)))]
    jobs<-list()
    for(id in ids){
      data<-state$summaries[[id]]
      # A failed acquisition is not an expired observation. Its transport has
      # already applied bounded recovery; the freshness pass must not retry it.
      if(id%in%names(state$summaries)&&is.null(data))next
      if(!is.null(data)&&(!refresh_expired||!expired(data,60,2)))next
      spec<-list(fn="get_player_summary",args=c(args,list(player_id=id)),
        resources=paste0("player_summary_",login[["userid"]],"_",championship_id,"_",user_team_id,"_",id))
      if(refresh_expired)invalidate(spec)
      jobs[[id]]<-spec
    }
    if(length(jobs))progress("pending_transfers")
    promises::then(batch(jobs,2L),function(results){
      if(!current())stop(cancelled())
      for(id in names(results)){
        state$summaries[id]<-list(results[[id]]$data)
        state$errors[paste0("summary:",id)]<-list(results[[id]]$error)
      }
      NULL
    })
  }
  finish<-function(){
    if(!current())stop(cancelled())
    now<-Sys.time()
    data<-calculate_team_acquisition_capacity(state$sources$info,state$sources$roster,state$sources$history,
      state$sources$market,state$summaries,user_team_id,now)
    if(!is.finite(data$valid_until)||data$valid_until<=as.numeric(now))data$status<-"partial"
    errors<-Filter(Negate(is.null),state$errors)
    if(length(errors))attr(data,"fetch_error")<-resource_failure(errors[[1]])
    progress("complete")
    resource_store(cache_key,data,base,cache_name,ttl=15)
  }
  progress("prerequisites")
  initial<-promises::then(batch(specs,4L),function(results){store_sources(results);load_summaries()})
  promises::then(initial,function(unused){
    if(!current())stop(cancelled())
    # One bounded freshness pass, with candidate discovery repeated after any
    # roster/history change. New arrivals never inherit another player's check.
    names<-names(specs)[vapply(names(specs),function(name)!is.null(state$sources[[name]])&&expired(state$sources[[name]],specs[[name]]$ttl,2),logical(1))]
    jobs<-specs[names]
    for(spec in jobs)invalidate(spec)
    if(length(jobs))progress("refreshing_facts")
    promises::then(batch(jobs,4L),function(results){
      store_sources(results)
      promises::then(load_summaries(TRUE),function(unused)finish())
    })
  })
}

# Existing interface: projecting a different target never repeats team checks.
get_acquisition_capacity <- function(login,championship_id,user_team_id,target_player_id=NULL) {
  out<-get_team_acquisition_capacity(login,championship_id,user_team_id)
  rows<-out$target_rows;pid<-fm_scalar(target_player_id,"")
  if(nzchar(pid)&&is.data.frame(rows)&&"id"%in%names(rows)){
    row<-rows[rows$id%in%pid,,drop=FALSE]
    if(nrow(row)==1L){
      if("bid_id"%in%names(row)&&nzchar(fm_scalar(row$bid_id,""))){out$target$my_bid_id<-as.character(row$bid_id);out$target$my_bid_amount<-fm_number(row$bid_price)}
      if("numberOfBids"%in%names(row))out$target$bid_count<-fm_number(row$numberOfBids)
    }
  }
  out$target_rows<-NULL
  out
}

acquisition_unavailable_message <- function(capacity) {
  notes<-if(is.list(capacity))as.character(capacity$diagnostics)else character()
  mapping<-c("team info unavailable"="Your team balance and league rules did not load",
    "maxPlayersInRoster unavailable"="The league roster limit did not load",
    "reported budget unavailable"="Your current cash balance did not load",
    "league borrowing percentage unavailable"="The league borrowing allowance could not be verified",
    "roster fetch failed"="Your roster did not load", "team or roster data is stale or incomplete"="Your team or roster data needs a fresh read", "roster identities unavailable"="Your roster player IDs could not be verified",
    "pending clause transfers unavailable"="Pending clause transfers could not be verified",
    "team value unavailable (debt limit unverified)"="Your squad value is missing, so the borrowing limit cannot be verified",
    "outgoing market commitments unavailable"="Your outgoing offers could not be verified")
  messages<-unname(mapping[intersect(notes,names(mapping))])
  if(any(grepl("^withheld",notes)))messages<-c(messages,"Reserved funds for active offers could not be verified")
  if(identical(attr(capacity,"fetch_status"),"stale"))messages<-c(messages,"Some checks are using old data")
  if(!length(messages))messages<-"The current team, funds and outgoing offers could not be verified"
  paste0("Purchases are paused: ",paste(unique(messages),collapse="; "),". Refresh to retry.")
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
evaluate_acquisition_preflight <- function(capacity, mode, amount = NULL, existing_bid_amount = NULL, minimum_bid = NULL, now = Sys.time()) {
  unavailable <- function(msg) list(ok = FALSE, reason = "unavailable", message = msg)
  capacity_fail <- function(msg) list(ok = FALSE, reason = "capacity", message = msg)
  funds_fail <- function(msg) list(ok = FALSE, reason = "funds", message = msg)
  ok_result <- list(ok = TRUE, reason = "ok", message = NULL)

  if (is.null(capacity) || !is.list(capacity)) {
    return(unavailable("Acquisition verification unavailable. Please refresh and try again."))
  }
  if("valid_until"%in%names(capacity) && (length(capacity$valid_until)!=1L||!is.finite(capacity$valid_until)||capacity$valid_until<=as.numeric(now)))
    return(unavailable("Availability checks expired before they completed. Refresh to verify your current roster, funds and offers."))
  if (!identical(capacity$status, "ok") || acquisition_data_unverified(capacity)) {
    return(unavailable(acquisition_unavailable_message(capacity)))
  }

  has_effective_contract <- "effective_count" %in% names(capacity$roster)
  roster_count <- if (has_effective_contract) capacity$roster$effective_count else capacity$roster$count
  roster_status <- if ("status" %in% names(capacity$roster)) capacity$roster$status else "ok"
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
  } else if(!identical(roster_status,"ok")||length(roster_count)!=1L||!is.finite(roster_count)||
      length(cap_val)!=1L||!is.finite(cap_val)) {
    return(unavailable("Roster capacity cannot be verified because pending clause transfers are unavailable. Refresh before buying another player."))
  } else if (identical(mode, "clause")) {
    if (!is.na(roster_count) && !is.na(cap_val) && roster_count >= cap_val) {
      return(capacity_fail(capacity_roster_message(capacity$roster)))
    }
  } else {
    # New offers: "bid" and "offer"
    if (!is.na(roster_count) && !is.na(cap_val) &&
        !is.na(outstanding_count) && (roster_count + outstanding_count) >= cap_val) {
      return(capacity_fail(capacity_roster_message(capacity$roster,outstanding_count)))
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
  }, timeout_sec = 15, context=resource_context(login=login,championship_id=championship_id,user_team_id=user_team_id))
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
    if (status != 200L) return(normalize_money_movements(NULL))
    ans <- httr::content(response)
    if (!is.list(ans) || !"answer" %in% names(ans)) return(normalize_money_movements(NULL))
    normalize_money_movements(ans$answer)
  }, context=resource_context(login=login,championship_id=championship_id,user_team_id=user_team_id))
}

# Complete transaction snapshots outlive the short display cache. Account/league
# keys isolate private history; size, count and idle age bound retained memory.
.transaction_history_baselines <- new.env(parent=emptyenv())
transaction_history_prune <- function(now=Sys.time()) {
  for(key in ls(.transaction_history_baselines)) {
    item <- .transaction_history_baselines[[key]]
    if(as.numeric(difftime(now,item$accessed_at,units="days"))>30)
      rm(list=key,envir=.transaction_history_baselines)
  }
  repeat {
    keys <- ls(.transaction_history_baselines)
    items <- mget(keys,.transaction_history_baselines)
    bytes <- sum(vapply(items,function(item)item$bytes,numeric(1)))
    if(length(keys)<=32L&&bytes<=16*1024^2)break
    oldest <- which.min(vapply(items,function(item)as.numeric(item$accessed_at),numeric(1)))
    rm(list=keys[oldest],envir=.transaction_history_baselines)
  }
  invisible(TRUE)
}
transaction_history_context <- function(context) {
  if(identical(fm_scalar(context$season,"unknown"),"unknown"))
    context$season <- league_season_context(context$championship_id,account_id=context$account_id)
  context
}
read_completed_history_baseline <- function(resource,context) {
  context <- transaction_history_context(context)
  transaction_history_prune()
  key <- api_cache_key(resource,context=context)
  item <- .transaction_history_baselines[[key]]
  if(!is.null(item)) {
    item$accessed_at <- Sys.time();.transaction_history_baselines[[key]] <- item
    return(item$data)
  }
  if(exists("read_persisted_history_baseline",mode="function")) {
    saved <- read_persisted_history_baseline(resource,context)
    if(!is.null(saved)&&isTRUE(write_completed_history_baseline(resource,saved,context)))return(saved)
  }
  NULL
}
write_completed_history_baseline <- function(resource,data,context) {
  context <- transaction_history_context(context)
  stamp <- fm_time(attr(data,"full_history_at"))
  if(!is.data.frame(data)||!identical(attr(data,"fetch_status"),"ok")||
     length(stamp)!=1L||is.na(stamp))return(invisible(FALSE))
  bytes <- as.numeric(object.size(data))
  if(bytes>16*1024^2)return(invisible(FALSE))
  .transaction_history_baselines[[api_cache_key(resource,context=context)]] <-
    list(data=data,accessed_at=Sys.time(),bytes=bytes)
  transaction_history_prune();invisible(TRUE)
}
clear_completed_history_baselines <- function(user_id=NULL,session_id=NULL) {
  # These account-scoped snapshots are shared by concurrent browser sessions.
  if(nzchar(fm_scalar(session_id,"")))return(invisible(TRUE))
  keys <- ls(.transaction_history_baselines)
  if(!is.null(user_id))keys <- keys[startsWith(keys,paste0("account:",user_id,":"))]
  if(length(keys))rm(list=keys,envir=.transaction_history_baselines)
  invisible(TRUE)
}
transaction_history_window <- function(previous,full_audit=FALSE) {
  audited <- fm_time(attr(previous,"full_history_at"))
  latest <- fm_time(attr(previous,"source_latest_at"))
  incremental <- !isTRUE(full_audit)&&is.data.frame(previous)&&identical(attr(previous,"fetch_status"),"ok")&&
    length(audited)==1L&&!is.na(audited)&&
    length(latest)==1L&&!is.na(latest)
  list(incremental=incremental,cutoff=if(incremental)latest-86400 else NULL,audited_at=audited)
}
transaction_history_passed_window <- function(news,cutoff) {
  if(is.null(cutoff)||!length(news))return(FALSE)
  times <- fm_time(vapply(news,function(item)fm_scalar(item$created,""),character(1)))
  # Finish only after the entire overlap boundary second. Pages are ordered by
  # created, while the API cursor uses ObjectId; boundary writers can differ.
  all(!is.na(times))&&all(diff(as.numeric(times))<=0)&&
    as.numeric(tail(times,1L))<floor(as.numeric(cutoff))
}
transaction_history_latest <- function(news,previous=NULL) {
  stamps <- c(as.numeric(fm_time(attr(previous,"source_latest_at"))),
    as.numeric(fm_time(vapply(news,function(item)fm_scalar(item$created,""),character(1)))))
  stamps <- stamps[is.finite(stamps)]
  if(length(stamps))as.POSIXct(max(stamps),origin="1970-01-01",tz="UTC")else NULL
}
transaction_history_delta_ids <- function(data,previous) {
  if(!is.data.frame(previous))return(NULL)
  if(!nrow(data))return(character())
  old_index <- match(data$id,previous$id)
  fields <- sort(union(names(data),names(previous)))
  canonical <- function(value) {
    if(is.list(value))return(lapply(value,canonical))
    if(length(value)==1L&&is.atomic(value)&&is.na(value))return(NA)
    if(is.integer(value))value <- as.numeric(value)
    unname(value)
  }
  row <- function(frame,index)lapply(fields,function(field) {
    if(!field%in%names(frame))return(NA)
    canonical(frame[[field]][[index]])
  })
  changed <- vapply(seq_len(nrow(data)),function(i)
    is.na(old_index[i])||!identical(row(data,i),row(previous,old_index[i])),logical(1))
  as.character(data$id[changed])
}

get_championship_pressroom <- function(login, championship_id, full_audit=FALSE) {
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
  context <- resource_context(login=login,championship_id=championship_id)
  get_cached_data(cache_key, {
    previous <- read_completed_history_baseline(cache_key,context)
    window <- transaction_history_window(previous,full_audit)
    incremental <- window$incremental
    locker<-get_locker_clause_payments(login,championship_id,full_audit=full_audit)
    if(isTRUE(attr(locker,"fetch_status")%in%c("stale","partial","unavailable")))
      stop(resource_failure(attr(locker,"fetch_error"),"pressroom"))
    teams<-if(nrow(locker))get_teams(login,championship_id)else data.frame()
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
      price_num <- fm_number(get_val(item, "price", NULL))
      source_id <- fm_scalar(get_val(item, "_id", ""), "")
      source_date <- fm_scalar(get_val(item, "created", ""), "")
      if(!nzchar(source_id)||is.na(fm_time(source_date))||!is.finite(price_num))
        stop("Pressroom transaction identity, date or amount unavailable")

      data.frame(
        id = source_id,
        created = source_date,
        player_id = p_id,
        player_name = p_name,
        player_team_name = as.character(get_val(get_val(item, "_playerTeam", NULL), "name", "")),
        player_team_slug = as.character(get_val(get_val(item, "_playerTeam", NULL), "slug", "")),
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
      seen_cursors <- character()
      complete <- FALSE
      full_scan <- FALSE

      repeat {
        page_count <- page_count + 1

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
          complete <- TRUE
          full_scan <- TRUE
          print("[Pressroom] Empty news array, all pages fetched.")
          break
        }

        all_news <- c(all_news,news)

        # Refresh from the source head through one day before the newest saved
        # source event. A known first page does not prove the overlap is covered.
        if(incremental&&transaction_history_passed_window(news,window$cutoff)) {
          complete<-TRUE;break
        }

        # Get the last item's _id to use as the cursor for the next page
        last_item <- news[[length(news)]]
        last_id <- get_val(as.list(last_item), "_id", "")

        # The API sorts by created but filters its cursor by Mongo ObjectId.
        # IDs from different writers are not ordered within the same second.
        # Replay the entire boundary second before advancing past it, otherwise
        # purchases with an earlier created time and larger ID are skipped.
        next_cursor <- last_id
        if (grepl("^[0-9a-f]{24}$",last_id)) {
          boundary <- substr(last_id,1L,8L)
          upper <- paste0(boundary,"ffffffffffffffff")
          if (identical(cursor,upper)) {
            # A short page establishes that the boundary second is exhausted.
            # A saturated single-second page cannot prove complete coverage.
            if (length(news)>=150L) break
            next_cursor <- paste0(boundary,"0000000000000000")
          } else next_cursor <- upper
        }
        if (next_cursor == "" || next_cursor == cursor || next_cursor %in% seen_cursors) {
          print("[Pressroom] Reached end of pagination (no new cursor).")
          break
        }

        seen_cursors <- c(seen_cursors,cursor)
        cursor <- next_cursor
      }

      print(paste0("[Pressroom] Pagination complete: ", length(all_news), " items across ", page_count, " pages."))

      df <- if(!length(all_news))empty_df else lapply(all_news, FUN = function(item) {
        parse_news_item(item)
      }) %>% bind_rows()

      if(incremental&&!full_scan){
        old<-previous[!startsWith(previous$id,"locker:"),,drop=FALSE]
        df<-dplyr::bind_rows(df,old)
      }
      # Deduplicate across cursor page boundaries
      df <- df %>%
        dplyr::filter(!is.na(id) & nzchar(id)) %>%
        dplyr::distinct(id, .keep_all = TRUE) %>%
        dplyr::arrange(desc(created))

      if(nrow(locker)){
        locker<-locker_resolve_parties(locker,df,teams,previous=previous)
        if(any(!nzchar(locker$player_id)))locker<-locker_resolve_parties(locker,df,teams,get_championship_players(login,championship_id),previous=previous)
        df<-merge_locker_transfers(df,locker)
      }
      attr(df,"full_history_at") <- if(complete) {
        if(incremental&&!full_scan)window$audited_at else Sys.time()
      } else NULL
      attr(df,"source_latest_at") <- transaction_history_latest(all_news,if(incremental&&!full_scan)previous else NULL)
      attr(df,"history_delta_ids") <- transaction_history_delta_ids(df,previous)
      df <- observed_data(df,status=if(complete) "ok" else "partial")
      if(complete)write_completed_history_baseline(cache_key,df,context)
      return(df)
    }, error = function(e) {
      stop(resource_failure(e, "pressroom"))
    })
  }, timeout_sec = if(isTRUE(full_audit))0 else 300, context=resource_context(login=login,championship_id=championship_id))
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


    }, error = function(e) {
      print(paste0("[Finances] Supabase sync warning: ", e$message))
    })

    return(list(
      team_finances = finances_df,
      all_purchases = purchases_df
    ))
  }, timeout_sec = 60, context=resource_context(login=login,championship_id=championship_id))
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
      
      result<-list(
        data = if ("data" %in% names(ans_data)) ans_data$data else NULL,
        prices = if ("prices" %in% names(ans_data)) ans_data$prices else list(),
        points = ans_data$points %||% list(),
        points_received = "points" %in% names(ans_data),
        match = ans_data$match %||% NULL,
        championship = ans_data$championship %||% list(),
        history_complete = isTRUE(ans_data$historyComplete),
        owners = ans_data$owners %||% list(),
        owners_received = "owners" %in% names(ans_data),
        market = ans_data$market %||% list(),
        numberOfBids = ans_data$numberOfBids %||% NA_integer_,
        bids = bids_arr,
        my_bid_id = my_bid_id,
        my_bid_price = my_bid_price
      )
      return(resolve_summary_round_labels(login,championship_id,result))
    }
    return(NULL)
  }, timeout_sec = 60, context=resource_context(login=login,championship_id=championship_id,user_team_id=user_team_id,player_id=player_id))
}

# Read a player endpoint that is scoped only by player ID.  These endpoints
# contain real-competition profile/match data and must not replace the selected
# championship's points from get_player_summary().
get_player_profile_endpoint <- function(login, player_id, endpoint, cache_prefix) {
  if (!valid_login(login) || is.null(player_id) || !length(player_id) ||
      !nzchar(as.character(player_id)[1])) return(NULL)
  cache_key <- paste0(cache_prefix, "_", as.character(player_id)[1])
  get_cached_data(cache_key, {
    payload <- list(
      header = list(token = login[["token"]], userid = login[["userid"]]),
      query = list(playerId = as.character(player_id)[1]), answer = list()
    )
    response <- futmondo_post(endpoint,
      body = toJSON(payload, auto_unbox = TRUE),
      add_headers(.headers = c("Content-Type" = "application/json; charset=utf-8")))
    raw_body <- httr::content(response, as = "text", encoding = "UTF-8")
    decoded <- tryCatch(jsonlite::fromJSON(raw_body, simplifyVector = FALSE), error = function(e) NULL)
    answer <- if (is.list(decoded)) decoded$answer else NULL
    if (is.null(answer) || !is.list(answer)) stop("Player profile data is unavailable.")
    returned_id<-fm_scalar(answer$player$id %||% answer$player$`_id`,"")
    if(nzchar(returned_id)&&!identical(returned_id,as.character(player_id)[1]))stop("Player profile identity mismatch")
    answer
  }, timeout_sec = 300, context=resource_context(login=login,player_id=player_id))
}

# Fetch Futmondo's real-competition player profile, including independent
# social/classic valuation history. Returns the raw answer payload for
# normalize_player_full_profile().
get_player_full_profile <- function(login, player_id) {
  get_player_profile_endpoint(login, player_id, PLAYER_FULL_PROFILE_URL, "player_full_profile")
}

# Fetch Futmondo's completed match metadata for a player. Returns the raw
# answer payload for normalize_player_matches().
get_player_matches <- function(login, player_id) {
  get_player_profile_endpoint(login, player_id, PLAYER_MATCHES_URL, "player_matches")
}

normalize_player_full_profile <- function(answer) {
  empty_prices <- data.frame(recorded_at=character(), social=numeric(), classic=numeric(), stringsAsFactors=FALSE)
  if (!is.list(answer)) answer <- list()
  profile <- answer$profile %||% list()
  nationality <- if (is.list(profile$nationality)) profile$nationality$main %||% "" else profile$nationality %||% ""
  normalized_profile <- list(
    date_of_birth = fm_scalar(profile$dob, ""), nationality = fm_scalar(nationality, ""),
    height_cm = fm_number(profile$height), weight_kg = fm_number(profile$weight),
    preferred_foot = fm_scalar(profile$foot, ""), shirt_number = fm_number(profile$number)
  )
  prices <- answer$prices
  if (is.null(prices) || !length(prices)) return(list(profile=normalized_profile, price_history=empty_prices))
  if (!is.data.frame(prices)) prices <- dplyr::bind_rows(prices)
  if (!nrow(prices)) return(list(profile=normalized_profile, price_history=empty_prices))
  out <- data.frame(
    recorded_at=if ("date" %in% names(prices)) as.character(prices$date) else NA_character_,
    social=if ("social" %in% names(prices)) suppressWarnings(as.numeric(prices$social)) else NA_real_,
    classic=if ("classic" %in% names(prices)) suppressWarnings(as.numeric(prices$classic)) else NA_real_,
    stringsAsFactors=FALSE
  )
  out <- out[!is.na(out$recorded_at) & nzchar(out$recorded_at),,drop=FALSE]
  list(profile=normalized_profile, price_history=out)
}

normalize_player_matches <- function(answer) {
  empty <- data.frame(round=numeric(), occurred_at=character(), home=character(), away=character(), home_score=numeric(), away_score=numeric(), status=character(), match_id=character(), round_id=character(), season=character(), competition_id=character(), stringsAsFactors=FALSE)
  matches <- if (is.list(answer)) answer$matches else NULL
  if (is.null(matches) || !length(matches)) return(empty)
  if (is.data.frame(matches)) matches <- split(matches, seq_len(nrow(matches)))
  out <- lapply(matches, function(match) {
    if (!is.list(match)) return(NULL)
    home <- match$h %||% list(); away <- match$a %||% list(); info <- match$info %||% list()
    data.frame(round=fm_number(match$r), occurred_at=fm_scalar(info$date, ""),
      home=fm_scalar(home$name, "Home team"), away=fm_scalar(away$name, "Away team"),
      home_score=fm_number(home$score), away_score=fm_number(away$score),
      status=fm_scalar(match$st, ""),
       match_id=fm_scalar(match$id %||% match$`_id` %||% match$matchId, ""),
       round_id=fm_scalar(match$roundId, ""),
       season=fm_scalar(match$season %||% match$seasonId %||% answer$season %||% answer$seasonId %||% answer$league$season,"unknown"),
       competition_id=fm_scalar(match$competitionId %||% answer$competitionId %||% answer$league$`_id` %||% answer$league$id,"unknown"),stringsAsFactors=FALSE)
  })
  out <- Filter(Negate(is.null), out)
  if (!length(out)) return(empty)
  frame <- dplyr::bind_rows(out)
  frame[order(frame$round, decreasing=TRUE, na.last=TRUE),,drop=FALSE]
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
    if (httr::status_code(response)==200L && is.list(ans) && !isTRUE(ans$error) &&
        (is.null(ans$code) || identical(ans$code,API_CODE_OK)) && "answer" %in% names(ans) &&
        is.list(ans$answer) && !isTRUE(ans$answer$error) && is.null(ans$answer$code)) {
      # An explicit successful empty list can replace a reset season snapshot;
      # a malformed/error response must never masquerade as empty history.
      return(structure(ans$answer,valid_response=TRUE))
    }
    stop("Team round history unavailable")
  }, context=resource_context(login=login,championship_id=championship_id,user_team_id=user_team_id))
}

# Display observations have their own namespace: provisional ranking rows must
# never become the closed-round eligibility evidence consumed by cash ledgers.
get_classification_round_catalog <- function(login,championship_id) {
  catalog <- get_award_round_catalog(login,championship_id)
  if(nzchar(mvp_text(catalog$league_id))){
    metadata <- get_award_round_metadata(login,championship_id,catalog$league_id)
    if(identical(metadata$status,"ok")&&is.data.frame(metadata$rows)&&nrow(catalog$rounds)){
      for(i in seq_len(nrow(catalog$rounds))){
        detail <- metadata$rows[metadata$rows$round_id%in%catalog$rounds$round_id[i],,drop=FALSE]
        if(nrow(detail)==1L&&nzchar(mvp_text(detail$status)))catalog$rounds$status[i]<-detail$status
      }
    }
  }
  catalog
}

classification_ranking_not_started <- function(answer) {
  tolower(mvp_text(answer$metadata$status))%in%c("next","future","pending","scheduled","not_started","upcoming")
}

classification_ranking_rows <- function(answer, round_id, round_number) {
  empty <- data.frame(team_id=character(),team_name=character(),round_id=character(),
    round=numeric(),points=numeric(),players_left=numeric(),score_status=character(),stringsAsFactors=FALSE)
  if(!is.list(answer)||isTRUE(answer$error)||!is.null(answer$code)||!is.list(answer$ranking)||classification_ranking_not_started(answer))return(empty)
  records <- mvp_records(answer$ranking)
  if(!length(records)||length(records)>500L)return(empty)
  status <- if(round_is_finished(answer$metadata %||% list()))"final"else"provisional"
  rows <- dplyr::bind_rows(lapply(records,function(row){
    if(!is.list(row))return(NULL)
    # The official ranking card displays live directly; starting and bench
    # are separate fields. Missing/invalid counts are never inferred from points.
    remaining <- if(is.list(row$live) || is.logical(row$live))NA_real_ else fm_number(row$live)
    if(is.finite(remaining) && (remaining<0 || remaining!=floor(remaining)))remaining <- NA_real_
    data.frame(team_id=mvp_text(row$id),team_name=mvp_text(row$name),round_id=as.character(round_id),
      round=as.numeric(round_number),points=fm_number(row$points),players_left=remaining,
      score_status=status,stringsAsFactors=FALSE)
  }))
  if(nrow(rows)!=length(records)||any(!nzchar(rows$team_id))||anyDuplicated(rows$team_id))return(empty)
  rows
}

get_classification_round_ranking <- function(login, championship_id, user_team_id, round_id, round_number) {
  if(!nzchar(mvp_text(round_id))||!is.finite(fm_number(round_number)))stop("Round identity unavailable")
  context <- resource_context(login,championship_id,user_team_id,season=league_season_context(championship_id))
  resource <- paste0("classification_ranking_",championship_id,"_",round_id)
  get_cached_data(resource, {
    saved <- tryCatch(read_rival_finance_evidence(context$account_id,championship_id,context$season),error=function(e)NULL)
    snapshots <- Filter(function(row){
      p <- row$payload
      identical(row$evidence_type,"eligibility")&&is.list(p)&&
        identical(p$kind,"classification_round_ranking")&&identical(mvp_text(p$round_id),as.character(round_id))&&
        identical(fm_number(p$round_number),as.numeric(round_number))&&
        identical(mvp_text(row$account_id),context$account_id)&&identical(mvp_text(row$championship_id),context$championship_id)&&
        identical(mvp_text(row$season,"unknown"),context$season)
    },saved$evidence %||% list())
    if(length(snapshots)){
      stamps <- vapply(snapshots,function(row)as.numeric(fm_time(row$observed_at)),numeric(1))
      index <- order(stamps,na.last=NA,decreasing=TRUE)
      if(length(index)){
        snapshot <- snapshots[[index[1]]];p <- snapshot$payload
        rows <- classification_ranking_rows(p$answer,round_id,round_number)
        deadline <- stamps[index[1]]+60
        if(nrow(rows)&&as.numeric(Sys.time())<deadline){
          result <- list(rows=rows,round_id=round_id,round_number=round_number,answer=p$answer,valid_until=deadline,from_saved=TRUE)
          attr(result,"observed_at") <- fm_time(snapshot$observed_at)
          attr(result,"source") <- "saved Futmondo round ranking"
          return(result)
        }
      }
    }
    payload <- list(header=list(token=login[["token"]],userid=login[["userid"]]),
      # The native client sends the opaque round ID under the misleading
      # roundNumber key. A calendar number returns answer="api.error.general".
      query=list(championshipId=championship_id,roundNumber=as.character(round_id),userteamId=user_team_id),answer=list())
    response <- futmondo_post("https://api.futmondo.com/1/ranking/round",body=toJSON(payload,auto_unbox=TRUE),
      add_headers(.headers=c("Content-Type"="application/json; charset=utf-8")))
    body <- httr::content(response)
    if(httr::status_code(response)!=200L||!is.list(body)||isTRUE(body$error)||
       (!is.null(body$code)&&!identical(body$code,API_CODE_OK)))stop("Round standings unavailable")
    if(is.list(body$answer)&&classification_ranking_not_started(body$answer))
      return(list(rows=data.frame(),round_id=round_id,round_number=round_number,not_started=TRUE,valid_until=as.numeric(Sys.time())+60))
    rows <- classification_ranking_rows(body$answer,round_id,round_number)
    if(!nrow(rows))stop("Round standings unavailable")
    # Retain only score fields, excluding profile/contact/actor payloads.
    answer <- list(metadata=list(status=mvp_text(body$answer$metadata$status)),
      ranking=lapply(seq_len(nrow(rows)),function(i)list(id=rows$team_id[i],name=rows$team_name[i],
        points=rows$points[i],live=rows$players_left[i])))
    stamp <- Sys.time()
    result <- list(rows=rows,round_id=round_id,round_number=round_number,answer=answer,valid_until=as.numeric(stamp)+60)
    attr(result,"observed_at") <- stamp
    result
  },timeout_sec=60,context=context)
}

build_round_dreamteam_payload <- function(login, championship_id, reference) list(header=list(token=login[["token"]], userid=login[["userid"]]), query=list(championshipId=championship_id,type="dreamteam",round=reference$round_id),answer=list())
get_round_dreamteam <- function(login, championship_id, reference) { ref<-if(is.list(reference)) round_reference(reference$round_id,reference$round_number) else round_reference(reference); if(is.null(ref)) return(NULL); get_cached_data(paste0("dreamteam_",championship_id,"_",ref$round_id), { ans<-httr::content(futmondo_post(DREAMTEAM_URL,body=toJSON(build_round_dreamteam_payload(login,championship_id,ref),auto_unbox=TRUE),add_headers(.headers=c("Content-Type"="application/json; charset=utf-8")))); if(is.list(ans$answer)) ans$answer else list() }, context=resource_context(login=login,championship_id=championship_id)) }
official_round_mvp_row <- function(answer, championship_id, reference) {
  result <- normalize_official_dreamteam(answer,championship_id,reference)
  if(result$status!="ok") return(data.frame())
  result$rows[result$rows$is_mvp %in% TRUE,,drop=FALSE]
}
get_official_round_mvps <- function(login, championship_id, rounds) {
  done <- rounds[rounds$is_finished %in% TRUE,,drop=FALSE]
  if(!nrow(done)) return(mvp_result("unsupported",reason="No verified completed rounds were supplied; historical coverage is unknown."))
  results <- lapply(seq_len(nrow(done)),function(i) get_official_dreamteam_result(login,championship_id,round_reference(done$round_id[i],done$round_number[i])))
  rows <- dplyr::bind_rows(lapply(results,function(x) if(x$status=="ok") x$rows[x$rows$is_mvp %in% TRUE,,drop=FALSE] else NULL))
  ok <- all(vapply(results,function(x)identical(x$status,"ok"),logical(1)))
  mvp_result(if(ok) "ok" else "partial",rows,paste(unique(vapply(results,function(x)x$reason,character(1))),collapse=" "),complete=ok)
}

calculate_futmondo_ranking_prizes <- function(money, members = 1) {
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

build_direct_sell_payload <- function(login, championship_id, team_id, player_id) {
  list(header=list(token=login[["token"]],userid=login[["userid"]]),
    query=list(championshipId=as.character(championship_id),userteamId=as.character(team_id),
      player_id=as.character(player_id)), answer=setNames(list(),character()))
}

direct_sale_preflight <- function(login, championship_id, team_id, player_id) {
  denied <- function(code,message) list(ok=FALSE,code=code,message=message)
  ids <- list(championship_id,team_id,player_id)
  if(!valid_login(login) || any(!vapply(ids,function(x) is.atomic(x) && length(x)==1L && !is.na(x) && nzchar(trimws(as.character(x))),logical(1))))
    return(denied("invalid_context","Log in and select an owned player before selling."))
  # A sale must recheck live ownership; a cached card cannot authorize a write.
  invalidate_resources(resource_context(login,championship_id,team_id,player_id),pattern="^player_summary_")
  summary <- tryCatch(get_player_summary(login,championship_id,team_id,player_id),error=function(e)NULL)
  if(!is.list(summary) || !is.list(summary$data) || !is.list(summary$championship) || identical(attr(summary,"fetch_status"),"stale") ||
     !identical(fm_scalar(summary$data$id %||% summary$data$`_id`,""),as.character(player_id)))
    return(denied("unavailable","Current player ownership could not be verified. Refresh before selling."))
  owner <- summary$championship$owner
  owner_id <- if(is.list(owner)) fm_scalar(owner$`_id` %||% owner$id,"") else ""
  if(!identical(owner_id,as.character(team_id))) return(denied("not_owner","This player no longer belongs to your team."))
  if(!is.list(summary$market)) return(denied("unavailable","Current market status could not be verified."))
  # The captured direct-sale flow first cancels an existing listing. Never
  # withdraw a listing implicitly as part of this action.
  if(any(c("pr","p","vom","pom") %in% names(summary$market)))
    return(denied("listed","Remove this player's market listing before selling directly to Futmondo."))
  if(!all(c("bids","direct") %in% names(summary$market)))
    return(denied("unavailable","The returned market status does not establish that direct selling is available. Refresh before selling."))
  list(ok=TRUE,code="ok",message="",value=fm_number(summary$data$value))
}

direct_sell_player <- function(login, championship_id, team_id, player_id) {
  tryCatch({
    check <- direct_sale_preflight(login,championship_id,team_id,player_id)
    if(!isTRUE(check$ok)) return(list(success=FALSE,uncertain=FALSE,code=check$code,message=check$message))
    response <- futmondo_post(DIRECT_SELL_URL,
      body=toJSON(build_direct_sell_payload(login,championship_id,team_id,player_id),auto_unbox=TRUE),
      add_headers(.headers=c("Content-Type"="application/json; charset=utf-8")))
    answer <- httr::content(response)$answer
    code <- if(is.list(answer)) fm_scalar(answer$code,"") else ""
    if(!grepl("^[A-Za-z0-9_.-]{0,100}$",code)) code <- ""
    http_ok <- httr::status_code(response) %in% 200:299
    ok <- http_ok && identical(code,API_CODE_OK) && !isTRUE(answer$error)
    rejected <- http_ok && nzchar(code) && !identical(code,API_CODE_OK)
    list(success=ok,uncertain=!ok && !rejected,code=code,
      message=if(ok) "Player sold to Futmondo / Mercado." else if(rejected) paste0("Futmondo rejected the sale (",code,").") else "The sale could not be confirmed. Refresh your roster and funds before trying again.")
  },error=function(e) list(success=FALSE,uncertain=TRUE,code="unconfirmed",
    message="The sale could not be confirmed. Refresh your roster and funds before trying again."))
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
  context<-resource_context(login=login,championship_id=championship_id,user_team_id=user_team_id)
  # A restored database observation retains its original 15-second deadline.
  key<-api_cache_key(cache_key,context=context);cached<-api_cache_env[[key]]
  if(!is.null(cached$data)) {
    stamp<-fm_time(attr(cached$data,"observed_at"))
    if(length(stamp)!=1L||is.na(stamp)||as.numeric(difftime(Sys.time(),stamp,units="secs"))>=15)
      rm(list=key,envir=api_cache_env)
  }
  get_cached_data(cache_key, {
    if(exists("read_saved_roster_offers",mode="function")) {
      saved<-read_saved_roster_offers(context)
      if(!is.null(saved))return(saved)
    }
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
    if (!is.list(ans$answer)) stop("Offer data unavailable")

    if (!is.null(ans) && "answer" %in% names(ans) && is.list(ans$answer)) {
      items <- ans$answer
      if (!is.null(items$code) || isTRUE(items$error)) stop("Offer data unavailable")
      if(length(items)==0) {
        empty<-received_offers_empty();attr(empty,"received_offers")<-received_offers_empty();return(empty)
      }

      bids_list <- list()
      for(item in items) {
        if(!is.list(item))stop("Offer data unavailable")
        player <- item$player
        p_id <- fm_scalar(item$player_id %||% if(is.list(player))player$`_id` %||% player$id else player %||% item$id,"")
        bids <- if(is.list(item$bids))item$bids else if(is.list(item$market))item$market$bids else NULL
        if(!nzchar(p_id))stop("Offer player identity unavailable")
        if(length(bids))bids_list<-c(bids_list,lapply(bids,normalize_received_offer,player_id=p_id))else
          if("price"%in%names(item))bids_list[[length(bids_list)+1L]]<-normalize_received_offer(item,p_id)
      }
      all_offers <- merge_received_offers(dplyr::bind_rows(bids_list))
      ret_df <- highest_received_offers(all_offers)
      attr(ret_df,"received_offers")<-all_offers
      return(ret_df)
     }
     return(data.frame())
  }, timeout_sec = 15, context=resource_context(login=login,championship_id=championship_id,user_team_id=user_team_id))
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
    if (!is.list(ans$answer) || !is.null(ans$answer$code) || isTRUE(ans$answer$error)) stop("Listed players unavailable")

    if (!is.null(ans) && "answer" %in% names(ans) && is.list(ans$answer)) {
      players <- ans$answer
      if (length(players) == 0) return(data.frame())
      ret <- lapply(players, FUN = function(player) {
        parse_player_json(player = player)
      }) %>% rbindlist(fill = TRUE) %>% as.data.frame()
      return(ret)
    }
    return(data.frame())
  }, timeout_sec = 60, context=resource_context(login=login,championship_id=championship_id,user_team_id=user_team_id))
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
    reserved_amount <- fm_number(capacity$funds$reserved_amount)
    lineup <- tryCatch(get_lineup_from_team(login,championship_id,user_team_id),error=function(e)NULL)
    rules <- normalize_league_rules(info,lineup,championship_id)
    observed_at <- min(attr(info,"observed_at") %||% Sys.time(), attr(capacity,"observed_at") %||% Sys.time())
    list(status = status, cash = cash, withheld = withheld,
      spendable_budget = spendable, legal_bid_limit = limit,
      debt_limit = debt_limit, minimum_balance = minimum_balance,
      projected_committed_balance = projected_balance, reserved_amount = reserved_amount,
      roster_count = capacity$roster$effective_count, roster_raw_count=capacity$roster$raw_count,
      roster_cap = capacity$roster$cap, roster_capacity=capacity$roster,
      pending_clause_inbound=capacity$roster$pending_inbound_count,
      pending_clause_outbound=capacity$roster$pending_outbound_count,
      commitments = capacity$outstanding, observed_at = observed_at,
      configuration = info$configuration %||% list(),
      team_value = team_value, rules = rules, lineup_rules = rules)
  }, timeout_sec = 15, context=resource_context(login=login,championship_id=championship_id,user_team_id=user_team_id))
}

get_active_championships <- function(login) {
  result<-get_cached_data("active_championship_objects", {
    payload <- list(header = list(token = login[["token"]], userid = login[["userid"]]),
      query = list(excludeGeneral = FALSE, includeProphets = TRUE), answer = list())
    response <- futmondo_post(ACTIVE_CHAMPIONSHIPS_URL,
      body = jsonlite::toJSON(payload, auto_unbox = TRUE),
      httr::content_type_json())
    answer <- httr::content(response)$answer
    if(!is.list(answer)||!"championships"%in%names(answer)||(!is.null(answer$championships)&&!is.list(answer$championships)))stop("Championships unavailable")
    if(is.null(answer$championships))answer$championships<-list()
    answer$championships <- lapply(answer$championships,function(champ) {
      champ$id <- champ$id %||% champ$`_id`
      if(is.list(champ$userteam)) champ$userteam$id <- champ$userteam$id %||% champ$userteam$`_id`
      champ
    })
    answer
  }, timeout_sec = 60, context=resource_context(login=login))
  publish_active_league_contexts(login,result)
  result
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
  }, timeout_sec = 30, context=resource_context(login=login)), error = function(e) NULL)
}

get_notification_unread <- function(login) {
  tryCatch(get_cached_data("notifications_unread", {
    result <- notification_request(login, "unread")
    count <- fm_number(result$data)
    if (result$status != "ok" || !is.finite(count) || count < 0) stop("Unavailable")
    as.integer(count)
  }, timeout_sec = 55, context=resource_context(login=login)), error = function(e) NA_integer_)
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
