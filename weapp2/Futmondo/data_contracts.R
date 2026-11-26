# Shared, explicit contracts for observed data. See docs/data_contracts.md.
source("theme.R")

# Compact, client-only disclosure. Source/diagnostic text is retained verbatim;
# native details handles keyboard access without a reactive input or request.
# Recursively inspect tag/list children, rather than treating an empty wrapper
# or a not-yet-bound Shiny output as readable information.
fm_info_content <- function(x) {
  if(is.null(x))return(c(content=FALSE,dynamic=FALSE))
  if(inherits(x,"shiny.tag")) {
    cls<-fm_scalar(x$attribs$class,"")
    dynamic<-grepl("shiny-(text|html)-output",cls)
    visual<-x$name%in%c("img","svg","canvas","button","select","textarea")||
      (identical(x$name,"input")&&!identical(x$attribs$type,"hidden"))
    children<-fm_info_content(x$children)
    return(c(content=visual||children[["content"]],dynamic=dynamic||children[["dynamic"]]))
  }
  if(is.list(x)) {
    states<-lapply(x,fm_info_content)
    return(c(content=any(vapply(states,function(s)s[["content"]],logical(1))),
      dynamic=any(vapply(states,function(s)s[["dynamic"]],logical(1)))))
  }
  text<-as.character(x);text[is.na(text)]<-""
  text<-gsub("<[^>]*>|&nbsp;|[[:space:]\u00a0\u200b]","",text,perl=TRUE)
  c(content=any(nzchar(text)),dynamic=FALSE)
}
fm_info <- function(..., error = FALSE, class = NULL, pending = FALSE, source = NULL, inline = FALSE) {
  if(isTRUE(pending))return(NULL)
  content<-list(...);state<-fm_info_content(content)
  if(!state[["content"]]&&!state[["dynamic"]])return(NULL)
  if(isTRUE(inline))return(shiny::tags$div(class=paste("fm-info-inline",if(isTRUE(error))"fm-info-inline-error",class),
    role=if(isTRUE(error))"alert"else"status",content))
  domain<-shiny::getDefaultReactiveDomain()
  root<-if(!is.null(domain)&&is.function(domain$rootScope))domain$rootScope()else domain
  revision<-if(!is.null(root)&&is.function(root$userData$information_context_revision))root$userData$information_context_revision()else NULL
  shiny::tags$details(class = paste(c("fm-info", if (isTRUE(error)) "fm-info-error",
      if(state[["dynamic"]]||!state[["content"]])"fm-info-empty",class), collapse = " "),
    `data-fm-info-source` = source, `data-fm-info-context` = revision,
    shiny::tags$summary(`aria-label` = if (isTRUE(error)) "Error details" else "Information",
      shiny::tags$span(class = "fm-info-icon", `aria-hidden` = "true", "i")),
    shiny::tags$div(class = "fm-info-body", content))
}
# One persistent disclosure beside each page heading. Source bodies are adopted
# by the browser registry without copying Shiny inputs or output bindings.
fm_info_page_UI <- function(title, ...) {
  omit_heading <- function(x) {
    if(inherits(x,"shiny.tag")) {
      if(identical(x$name,"h2") && identical(paste(unlist(x$children),collapse=""),title))return(NULL)
      x$children<-omit_heading(x$children)
    } else if(is.list(x)&&(!is.object(x)||inherits(x,"shiny.tag.list")))x[]<-lapply(x,omit_heading)
    x
  }
  shiny::tags$div(class="fm-information-scope",`data-fm-info-page`="true",
    shiny::tags$div(class="fm-information-heading",shiny::tags$h2(title),
      shiny::tags$details(class="fm-info fm-info-panel",hidden="hidden",
        shiny::tags$summary(`aria-label`="Page information",title="Page information",
          shiny::tags$span(class="fm-info-icon",`aria-hidden`="true","i")),
        shiny::tags$div(class="fm-info-body"))),
    omit_heading(list(...)))
}

preserve_observation_time <- function(data, source = data) {
  if (!is.data.frame(data)) return(data)
  stamp <- attr(source, "observed_at")
  if (!"observed_at" %in% names(data)) data$observed_at <- rep(
    if (is.null(stamp)) NA_character_ else format(fm_time(stamp), "%Y-%m-%dT%H:%M:%OSZ", tz="UTC"), nrow(data))
  attr(data, "fetch_status") <- attr(source, "fetch_status")
  data
}
fm_scalar <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) != 1L || is.na(x)) return(default)
  as.character(x)
}

fm_number <- function(x, default = NA_real_) {
  n <- suppressWarnings(as.numeric(x))
  if (length(n) != 1L || !is.finite(n)) default else n
}

fm_time <- function(x) {
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = "UTC"))
  if (is.numeric(x)) return(as.POSIXct(ifelse(abs(x)>1e11,x/1000,x),origin="1970-01-01",tz="UTC"))
  values <- vapply(as.character(x), function(value) {
    if (is.na(value) || !nzchar(value)) return(NA_real_)
    value <- sub("Z$", "+0000", value)
    value <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", value)
    for (fmt in c("%Y-%m-%dT%H:%M:%OS%z", "%Y-%m-%d %H:%M:%OS%z",
                  "%Y-%m-%dT%H:%M:%OS", "%Y-%m-%d %H:%M:%OS", "%Y-%m-%d")) {
      parsed <- suppressWarnings(as.POSIXct(value, format=fmt, tz="UTC"))
      if (!is.na(parsed)) return(as.numeric(parsed))
    }
    NA_real_
  }, numeric(1))
  as.POSIXct(values, origin="1970-01-01", tz="UTC")
}

# Temporary borrowing follows configuration$mnmp, with no assumed percentage.
# `withheld` is the API's reservation for active outgoing offers. When the
# market endpoint reports the same offers, both fields describe the same held
# money and must not be subtracted twice. This is acquisition headroom only:
# cash must be positive when a round begins.
acquisition_headroom <- function(cash, team_value, withheld = 0,
                                 commitments = 0, debt_fraction = NA_real_) {
  values <- vapply(list(cash, team_value, withheld, commitments, debt_fraction),
    fm_number, numeric(1))
  names(values) <- c("cash", "team_value", "withheld", "commitments", "debt_fraction")
  if (any(!is.finite(values)) || values["team_value"] < 0 ||
      values["withheld"] < 0 || values["commitments"] < 0 ||
      values["debt_fraction"] < 0) {
    return(list(spendable_budget = NA_real_, debt_limit = NA_real_,
      minimum_balance = NA_real_, projected_committed_balance = NA_real_, reserved_amount = NA_real_))
  }
  debt_limit <- values["team_value"] * values["debt_fraction"]
  reserved_amount <- max(values["withheld"], values["commitments"])
  committed_balance <- values["cash"] - reserved_amount
  list(spendable_budget = max(0, committed_balance + debt_limit),
    debt_limit = debt_limit, minimum_balance = -debt_limit,
    projected_committed_balance = committed_balance, reserved_amount = reserved_amount)
}

# Display-only finances use the same independently loaded team/market states as
# Today. Manual acquisitions still require the full capacity/preflight contract.
round_finance_display <- function(team_state, market_state) {
  fresh <- function(state) is.list(state) && isTRUE(state$status %in% c("ok", "empty")) &&
    is.null(state$error) && is.null(attr(state$data,"fetch_error")) &&
    !isTRUE(attr(state$data,"worker_completeness")%in%c("partial","unavailable")) &&
    !identical(state$freshness, "stale") && !identical(attr(state$data, "fetch_status"), "stale")
  loading <- function(state) is.list(state) && identical(state$status, "loading")
  stale_state <- function(state) identical(state$freshness, "stale") || identical(state$status, "stale") ||
    identical(attr(state$data, "fetch_status"), "stale")
  # Expiry changes freshness, not the factual contents of a successful snapshot.
  # Partial/failed acquisitions are never evidence of a complete offer total.
  usable <- function(state) !is.null(state$data) &&
    isTRUE(state$status %in% c("ok", "empty", "stale", "loading")) &&
    isTRUE((attr(state$data, "fetch_status") %||% "ok") %in% c("ok", "empty", "stale")) &&
    !isTRUE(attr(state$data, "worker_completeness") %in% c("partial", "unavailable"))
  team <- team_state$data
  cash <- fm_number(team$budget)
  withheld <- fm_number(team$withheld)
  team_value <- fm_number(team$teamValue %||% team$team_value)
  market <- market_state$data
  commitments <- NA_real_; offer_count <- NA_integer_
  # A successful empty market records zero outgoing offers at its source time.
  # Missing/failed responses do not. bid_id identifies own outgoing bids.
  if (usable(market_state) && is.data.frame(market) && (!nrow(market) || "bid_id" %in% names(market))) {
    own <- if ("bid_id" %in% names(market)) which(!is.na(market$bid_id) & nzchar(as.character(market$bid_id))) else integer()
    bids <- market[own, , drop=FALSE]
    if (nrow(bids)) bids <- bids[!duplicated(bids$bid_id), , drop=FALSE]
    prices <- if (nrow(bids) && "bid_price" %in% names(bids)) suppressWarnings(as.numeric(bids$bid_price)) else numeric()
    if (!nrow(bids) || (length(prices) == nrow(bids) && all(is.finite(prices) & prices > 0))) {
      commitments <- sum(prices); offer_count <- nrow(bids)
    }
  }
  after_offers_balance <- if (usable(team_state) && is.finite(cash) && is.finite(commitments)) cash - commitments else NA_real_
  projected_committed_balance <- spendable <- NA_real_
  if (usable(team_state) && is.finite(cash) && is.finite(withheld) && withheld >= 0 && is.finite(commitments)) {
    projected_committed_balance <- cash - max(withheld, commitments)
    spendable <- acquisition_headroom(cash, team_value, withheld, commitments,
      debt_fraction=fm_number(team$configuration$mnmp))$spendable_budget
    api_limit<-fm_number(team$maxBid)
    if(is.finite(spendable)&&is.finite(api_limit))spendable<-min(spendable,max(0,api_limit))
  }
  confirmed_problem <- fresh(team_state) &&
    ((is.finite(cash) && cash <= 0) || (fresh(market_state) && is.finite(projected_committed_balance) && projected_committed_balance <= 0))
  class <- if (confirmed_problem) "round-countdown-warning" else
    if (fresh(team_state) && fresh(market_state) && is.finite(projected_committed_balance) && projected_committed_balance > 0) "round-countdown-solvent" else "round-countdown-unavailable"
  team_stale <- stale_state(team_state)
  market_stale <- stale_state(market_state)
  stale <- team_stale || market_stale
  message <- if (confirmed_problem) {
    balance <- if (fresh(market_state) && is.finite(projected_committed_balance)) projected_committed_balance else cash
    paste0("Restore at least ", format(round(abs(balance) + 1), big.mark=".", decimal.mark=",", scientific=FALSE),
      " EUR before kickoff to score points", if (!fresh(market_state) || !is.finite(projected_committed_balance)) "; active offers still need verification" else "")
  } else if (loading(team_state)) "Loading balance…" else if (loading(market_state)) "Loading active offers…" else
    if (!is.null(team_state$error) || !is.null(market_state$error)) "Financial data could not load. Please try again." else
    if (!is.finite(cash)) "Balance unavailable — cannot verify kickoff balance" else
    if (!is.finite(commitments)) "Active offers unavailable — cannot verify kickoff balance" else
    if (!is.finite(withheld) || withheld < 0) "Held funds unavailable — cannot verify kickoff balance" else
    if (!is.finite(spendable)) "Spending allowance unavailable" else NULL
  list(cash=cash, cash_loading=loading(team_state), cash_stale=team_stale && is.finite(cash),
    cash_unverified=!fresh(team_state) && !team_stale && is.finite(cash),
    commitments=commitments, active_offer_count=offer_count, offers_loading=loading(market_state),
    offers_stale=market_stale && is.finite(commitments),
    calculations_stale=stale && is.finite(projected_committed_balance),
    after_offers_balance=after_offers_balance, projected_committed_balance=projected_committed_balance, spendable_budget=spendable,
    class=class, message=message)
}

# Presentation of observed amounts is independent of action freshness. Keep the
# strict finance contract above; source/error/expiry feedback has its own output.
round_finance_card_display <- function(finance) {
  fields<-c("cash","commitments","active_offer_count","after_offers_balance","spendable_budget")
  card<-finance[intersect(fields,names(finance))]
  card$cash_loading<-!is.finite(finance$cash)&&isTRUE(finance$cash_loading)
  card$offers_loading<-!is.finite(finance$commitments)&&isTRUE(finance$offers_loading)
  balances<-c(finance$cash,finance$after_offers_balance)
  negative<-any(is.finite(balances)&balances<=0)
  card$class<-if(negative)"round-countdown-warning"else
    if(any(is.finite(c(finance$cash,finance$commitments))))"round-countdown-observed"else "round-countdown-unavailable"
  card
}

# Display existing ledger values only; missing data never becomes a zero.
# A NULL card is a pending projection; an empty card is a settled failure.
round_finance_amounts_UI <- function(finance = NULL) {
  pending <- is.null(finance)
  money <- function(x, loading = FALSE) {
    value <- fm_number(x)
    if (is.finite(value)) paste0(format(round(value), big.mark=".", decimal.mark=",", scientific=FALSE), " EUR")
    else if (isTRUE(loading)) "Loading…" else "Unavailable"
  }
  cash_loading <- pending || isTRUE(finance$cash_loading)
  offers_loading <- pending || isTRUE(finance$offers_loading)
  count <- fm_number(finance$active_offer_count)
  offers <- money(finance$commitments, offers_loading)
  if (is.finite(fm_number(finance$commitments)) && is.finite(count))
    offers <- paste0(offers, " (", as.integer(count), ")")
  shiny::tags$div(class=paste("round-countdown-finance", finance$class %||% "round-countdown-unavailable"),
    shiny::tags$span(class="round-countdown-finance-item", paste0("Balance: ", money(finance$cash, cash_loading))),
    shiny::tags$span(class="round-countdown-finance-item", paste0("Active offers: ", offers)),
    shiny::tags$span(class="round-countdown-finance-item", paste0("After offers: ", money(finance$after_offers_balance, cash_loading || offers_loading))),
    shiny::tags$span(class="round-countdown-finance-item", paste0("Can spend: ", money(finance$spendable_budget, cash_loading || offers_loading))))
}

# Descriptive projection of the same identity/offer-slot ledger as preflights.
# It never authorizes a transaction or assumes a missing count/limit is zero.
observed_squad_count <- function(roster_state) {
  roster<-roster_state$data
  if(!is.null(roster_state$error)||isTRUE(roster_state$status%in%c("partial","unavailable")))roster<-NULL
  empty<-list(status="ok",outgoing_status="ok",records=data.frame(player_id=character()),
    outgoing_records=data.frame(player_id=character()))
  team_effective_squad_count(roster,empty)$count
}

account_squad_display <- function(roster_state, transfers, team_state, active_offers, reserved_slots) {
  roster<-roster_state$data
  if(!is.null(roster_state$error)||isTRUE(roster_state$status%in%c("partial","unavailable")))roster<-NULL
  actual<-observed_squad_count(roster_state)
  effective<-team_effective_squad_count(roster,transfers)
  cap<-fm_number(team_state$data$configuration$maxPlayersInRoster)
  if(is.null(team_state$data)||isTRUE(team_state$status%in%c("partial","unavailable"))||
     any(attr(team_state$data,"fetch_status")%in%c("partial","unavailable"))||
     any(attr(team_state$data,"worker_completeness")%in%c("partial","unavailable","incomplete"))||
     (!is.null(attr(team_state$data,"worker_acquisition_status"))&&
      !isTRUE(attr(team_state$data,"worker_acquisition_status")%in%c("ok","empty")))||
     !is.finite(cap)||cap<=0||cap!=floor(cap))cap<-NA_real_
  slots<-vapply(list(active_offers,reserved_slots),fm_number,numeric(1))
  slots_known<-all(is.finite(slots)&slots>=0&slots==floor(slots))
  reserved<-if(slots_known)sum(slots)else NA_real_
  full<-is.finite(effective$count)&&is.finite(cap)&&
    (effective$count>=cap||(slots_known&&effective$count+reserved>=cap))
  loading<-isTRUE(roster_state$status=="loading")||isTRUE(team_state$status=="loading")||
    identical(effective$status,"loading")
  observed<-identical(effective$status,"observed")||identical(roster_state$freshness,"stale")||
    identical(team_state$freshness,"stale")||!is.null(team_state$error)
  list(actual=actual,effective=effective$count,cap=cap,reserved=reserved,
    active_offers=slots[[1]],purchase_reservations=slots[[2]],full=full,
    status=if(loading)"loading"else if(!all(is.finite(c(actual,effective$count,cap,reserved))))"unavailable"else if(observed)"observed"else"ok",
    detail=effective$detail,
    roster_observed_at=roster_state$observed_at,team_observed_at=team_state$observed_at)
}

account_squad_UI <- function(state = NULL) {
  pending<-is.null(state)||identical(state$status,"loading")
  count<-function(x)if(is.finite(fm_number(x)))as.character(as.integer(x))else if(pending)"Loading…"else"Unavailable"
  remaining<-fm_number(state$cap)-fm_number(state$effective)-fm_number(state$reserved)
  capacity<-if(isTRUE(state$full)){
    shiny::tags$strong(class="fm-account-squad-full", "No squad slots free for new offers.")
  }else if(is.finite(remaining)){
    shiny::tags$span(sprintf("You can buy up to %d more %s.",as.integer(remaining),
      if(remaining==1)"player"else"players"))
  }else{
    shiny::tags$span(paste0("Remaining squad slots: ",count(remaining)))
  }
  offer_slots <- fm_number(state$active_offers)
  purchase_slots <- fm_number(state$purchase_reservations)
  shiny::tags$div(class="fm-account-squad",
    shiny::tags$span(paste0("Players: ",count(state$actual))),
    shiny::tags$span(paste0("Effective players: ",count(state$effective)," / ",count(state$cap))),
    if (is.finite(offer_slots) && offer_slots > 0)
      shiny::tags$span(sprintf("Slots used by active offers: %d", as.integer(offer_slots))),
    if (is.finite(purchase_slots) && purchase_slots > 0)
      shiny::tags$span(sprintf("Slots reserved for pending actions: %d", as.integer(purchase_slots))),
    capacity,
    fm_info(state$detail,
      shiny::textOutput("account_squad_checked",inline=TRUE),
      source="Squad capacity",pending=pending,error=identical(state$status,"unavailable")))
}

next_round_context <- function(rounds, now = Sys.time()) {
  unavailable <- list(available = FALSE, round_number = NA_real_, starts_at = as.POSIXct(NA))
  if (!is.data.frame(rounds) || !nrow(rounds) ||
      !all(c("round_number", "begin_process") %in% names(rounds))) return(unavailable)
  starts <- fm_time(rounds$begin_process)
  current <- fm_time(now)
  candidates <- which(!is.na(starts) & starts > current)
  if (!length(candidates)) return(unavailable)
  index <- candidates[which.min(starts[candidates])]
  list(available = TRUE, round_number = fm_number(rounds$round_number[index]),
    starts_at = starts[index])
}

# Return the latest started round that the source still marks unfinished.
current_round_context <- function(rounds, now = Sys.time()) {
  unavailable <- list(available = FALSE, round_number = NA_real_, starts_at = as.POSIXct(NA))
  if (!is.data.frame(rounds) || !nrow(rounds) ||
      !all(c("round_number", "begin_process", "is_finished") %in% names(rounds))) return(unavailable)
  starts <- fm_time(rounds$begin_process)
  current <- fm_time(now)
  unfinished <- !is.na(rounds$is_finished) & !as.logical(rounds$is_finished)
  candidates <- which(!is.na(starts) & starts <= current & unfinished)
  if (!length(candidates)) return(unavailable)
  index <- candidates[which.max(starts[candidates])]
  list(available = TRUE, round_number = fm_number(rounds$round_number[index]),
    starts_at = starts[index])
}

format_round_countdown <- function(starts_at, now = Sys.time()) {
  seconds <- floor(as.numeric(difftime(fm_time(starts_at), fm_time(now), units = "secs")))
  if (length(seconds) != 1L || !is.finite(seconds)) return("Start time unavailable")
  seconds <- max(0, seconds)
  days <- seconds %/% 86400; seconds <- seconds %% 86400
  hours <- seconds %/% 3600; seconds <- seconds %% 3600
  minutes <- seconds %/% 60; seconds <- seconds %% 60
  sprintf("%dd %02dh %02dm %02ds", days, hours, minutes, seconds)
}
fetch_result <- function(data = NULL, status = "ok", source = "futmondo",
                         observed_at = Sys.time(), error = NULL) {
  stopifnot(status %in% c("ok", "empty", "stale", "partial", "unavailable"))
  list(data = data, status = status, source = source,
       observed_at = observed_at, error = error)
}

observed_data <- function(data, source = "futmondo", status = NULL,
                          observed_at = Sys.time()) {
  if (is.null(data)) return(NULL)
  if (is.null(status)) status <- if (length(data) == 0L ||
    (is.data.frame(data) && nrow(data) == 0L)) "empty" else "ok"
  attr(data, "fetch_status") <- status
  attr(data, "observed_at") <- observed_at
  attr(data, "source") <- source
  data
}

as_fetch_result <- function(data, source = "futmondo") {
  if (is.null(data)) return(fetch_result(status = "unavailable", source = source))
  fetch_result(data, attr(data, "fetch_status") %||% "ok",
               attr(data, "source") %||% source,
               attr(data, "observed_at") %||% Sys.time())
}

`%||%` <- function(x, y) if (is.null(x)) y else x

valid_login <- function(token) {
  candidate <- is.atomic(token) || is.list(token)
  isTRUE(candidate) && !is.null(names(token)) &&
    all(c("token", "userid") %in% names(token)) &&
    isTRUE(nzchar(fm_scalar(token[["token"]], ""))) &&
    isTRUE(nzchar(fm_scalar(token[["userid"]], "")))
}

is_authorized_admin <- function(token, admin = Sys.getenv("admin")) {
  valid_login(token) && "user_name" %in% names(token) && nzchar(trimws(admin)) &&
    identical(tolower(trimws(fm_scalar(token[["user_name"]], ""))),
              tolower(trimws(admin)))
}

normalize_bid_observations <- function(pressroom_df, championship_id,
                                       observed_at = Sys.time()) {
  empty <- data.frame(auction_id = character(), championship_id = character(),
    player_id = character(), bidder_id = character(), bidder_name = character(),
    amount = numeric(), is_winner = logical(), source_bid_id = character(),
    settled_at = character(), observed_at = character(), stringsAsFactors = FALSE)
  if (is.null(pressroom_df) || !nrow(pressroom_df)) return(empty)
  rows <- list()
  for (i in seq_len(nrow(pressroom_df))) {
    r <- pressroom_df[i, , drop = FALSE]
    # Missing counterparties represent the system. Only purchases FROM the
    # system are comparable auctions; private sales/clauses stay in the ledger.
    seller <- fm_scalar(r$seller_team_id, "")
    winner <- fm_scalar(r$buyer_team_id, "")
    if (nzchar(seller) || !nzchar(winner) || !nzchar(fm_scalar(r$id, ""))) next
    append_bid <- function(id, name, amount, won, bid_id = "") {
      if (!nzchar(id) || !is.finite(amount) || amount <= 0) return(NULL)
      data.frame(auction_id = fm_scalar(r$id), championship_id = as.character(championship_id),
        player_id = fm_scalar(r$player_id), bidder_id = id, bidder_name = name,
        amount = amount, is_winner = won, source_bid_id = bid_id,
        settled_at = fm_scalar(r$created),
        observed_at = format(observed_at, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
        stringsAsFactors = FALSE)
    }
    rows[[length(rows) + 1L]] <- append_bid(winner, fm_scalar(r$buyer_team_name, ""),
                                            fm_number(r$price), TRUE)
    bids <- if ("bids" %in% names(r)) r$bids[[1]] else list()
    if (is.data.frame(bids)) bids <- split(bids, seq_len(nrow(bids)))
    for (b in bids) {
      id <- fm_scalar(b$u$`_id` %||% b$u$id, "")
      if (identical(id, winner)) next # settlement price wins over duplicate history
      rows[[length(rows) + 1L]] <- append_bid(id, fm_scalar(b$u$name, ""),
        fm_number(b$bid), FALSE, fm_scalar(b$`_id`, ""))
    }
  }
  out <- dplyr::bind_rows(c(list(empty), rows))
  if (!nrow(out)) return(empty)
  out <- out[order(-out$amount), , drop = FALSE]
  out[!duplicated(out[c("championship_id", "auction_id", "bidder_id")]), , drop = FALSE]
}

normalize_auction_observations <- function(pressroom_df, championship_id,
                                           observed_at = Sys.time()) {
  if (is.null(pressroom_df) || !nrow(pressroom_df)) return(data.frame())
  d <- pressroom_df
  keep <- (is.na(d$seller_team_id) | d$seller_team_id == "") &
    !is.na(d$buyer_team_id) & d$buyer_team_id != ""
  d <- d[keep, , drop = FALSE]
  if (!nrow(d)) return(data.frame())
  data.frame(auction_id = as.character(d$id), championship_id = as.character(championship_id),
    player_id = as.character(d$player_id), winner_id = as.character(d$buyer_team_id),
    winning_amount = as.numeric(d$price), settled_at = as.character(d$created),
    observed_at = format(observed_at, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    outcome = "sold", visibility_complete = FALSE, stringsAsFactors = FALSE)
}

# Only explicit per-player logical flags establish finality. Missing, malformed
# or contradictory evidence is unknown; round/match status cannot fill this gap.
player_points_finality <- function(point) {
  fields<-intersect(c("isFinished","finished"),names(point))
  if(!length(fields))return("unknown")
  values<-lapply(fields,function(field)point[[field]])
  recognized<-vapply(values,function(value)is.logical(value)&&length(value)==1L&&!is.na(value),logical(1))
  if(!all(recognized))return("unknown")
  values<-unique(unlist(values,use.names=FALSE))
  if(length(values)!=1L)return("unknown")
  if(values)"final"else "provisional"
}

normalize_player_match_observations <- function(summary, player_id, championship_id,
                                                 scoring_version = "unknown",
                                                 season = "unknown", observed_at = Sys.time(), rounds = NULL, competition_id = "unknown") {
  points <- summary$points
  if (is.null(points) || !length(points)) return(data.frame())
  if (is.data.frame(points)) points <- split(points, seq_len(nrow(points)))
  # Historical round numbers do not establish per-player score finality.
  dplyr::bind_rows(lapply(points, function(p) {
    round <- fm_number(p$round)
    if (!is.finite(round)) return(NULL)
    pts <- fm_number(p$points)
    source_match <- if(is.list(p$match))p$match else list()
    timing <- if(is.data.frame(rounds) && all(c('round_number','round_start_at') %in% names(rounds)))
      rounds[rounds$round_number==round,,drop=FALSE] else data.frame()
    occurred <- if(nrow(timing)==1L) fm_scalar(timing$round_start_at) else NA_character_
    # minutesPlayed/initialLineUp semantics are not yet verified: retain raw
    # observations but do not manufacture participation or calibrated minutes.
    data.frame(player_id = as.character(player_id), championship_id = as.character(championship_id),
      season = season, scoring_version = scoring_version, round = round, source_round_number=fm_number(p$source_round_number %||% p$round), points = pts,
      match_id=fm_scalar(p$matchId %||% source_match$id %||% source_match$`_id`,""),
      round_id=fm_scalar(p$roundId,""),
      competition_id=fm_scalar(p$competitionId %||% summary$championship$competitionId,competition_id),
      source_round_key=if(nzchar(fm_scalar(p$roundId,"")))paste0("id:",fm_scalar(p$roundId,""))else paste0("number:",round),
      score_status = if (!is.finite(pts)) "unavailable" else player_points_finality(p),
      is_home = if (is.logical(p$isHomeTeam)) p$isHomeTeam else NA,
      minutes_raw = fm_number(p$minutesPlayed),
      start_raw = if (is.logical(p$initialLineUp)) p$initialLineUp else NA,
      participation = NA, round_start_at=occurred, occurred_at=occurred,
      observed_at = format(observed_at, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
      stringsAsFactors = FALSE)
  }))
}

# Object order is not scoring; preserve arrays, NULLs, and exact values.
fm_canonical_scoring <- function(value) {
  if(is.data.frame(value))value<-lapply(seq_len(nrow(value)),function(i)lapply(value,function(column)if(is.data.frame(column))as.list(column[i,,drop=FALSE])else if(is.list(column))column[[i]]else column[i]))
  if(!is.list(value))return(value)
  value<-lapply(value,fm_canonical_scoring);keys<-names(value)
  if(!is.null(keys)&&!anyNA(keys)&&all(nzchar(keys))&&!anyDuplicated(keys))value<-value[order(keys,method="radix")]
  value
}
fm_scoring_version <- function(point_system=NULL,custom=NULL) {
  if(!is.atomic(point_system)||length(point_system)!=1L||is.na(point_system)||!nzchar(as.character(point_system))||
     (identical(as.character(point_system),"custom")&&is.null(custom)))return("unknown")
  value<-fm_canonical_scoring(list(point_system=point_system,custom=custom))
  unclass(as.character(openssl::sha256(charToRaw(jsonlite::toJSON(value,auto_unbox=TRUE,null="null",digits=NA)))))
}
league_season_context <- function(championship_id, observed_season=NULL,
                                  configured=Sys.getenv("FUTMONDO_SEASON_CONTEXTS","{}"),account_id=NULL) {
  observed<-fm_scalar(observed_season,"")
  metadata<-if(exists("league_context_metadata",mode="function"))league_context_metadata(championship_id,account_id)else NULL
  season<-fm_scalar(metadata$season,"unknown")
  if(nzchar(observed)&&observed!="unknown"){
    if(season!="unknown"&&!identical(observed,season))stop("Conflicting authoritative season metadata")
    return(observed)
  }
  if(season!="unknown")return(season)
  mapping<-tryCatch(jsonlite::fromJSON(configured,simplifyVector=FALSE),error=function(e)list())
  id<-fm_scalar(championship_id,"")
  if(!nzchar(id)||!is.list(mapping))return("unknown")
  fm_scalar(mapping[[id]],"unknown")
}

normalize_league_rules <- function(info, lineup = NULL, championship_id=NULL) {
  cfg <- info$configuration %||% list()
  scoring <- list(point_system=lineup$pointSystem,custom=lineup$custom)
  version <- fm_scoring_version(lineup$pointSystem,lineup$custom)
  list(championship_id=fm_scalar(championship_id %||% info$championshipId),
    season=league_season_context(championship_id %||% info$championshipId,info$season %||% info$seasonId),
    scoring_version=if(is.null(lineup)) "unknown" else version,
    configuration = cfg, initial_budget = fm_number(cfg$budget),
    money_per_point = fm_number(cfg$moneyPerPoint),
    ranking_pool = fm_number(cfg$moneyPerRanking), ranking_mode = fm_scalar(cfg$rankingMode),
    mvp_reward = fm_number(cfg$mvpPlayer), dreamteam_reward = fm_number(cfg$dreamTeamPlayer),
    roster_cap = fm_number(cfg$maxPlayersInRoster),
    captain_enabled = isTRUE(cfg$cpt), captain_multiplier = 2,
    multiposition = isTRUE(lineup$multiposition),
    bench_enabled = isTRUE(lineup$bench$config$enabled),
    bench_mode = fm_scalar(lineup$bench$config$mode, "unknown"),
    bench_size = if(isTRUE(lineup$bench$config$enabled)) 4L else NA_integer_,
    club_limit = NA_integer_, club_limit_status = "unknown", formations = NULL,
    verified = FALSE, # Full formation/club-limit contract still requires verification.
    capabilities_verified = !is.null(lineup) && !identical(attr(lineup,"fetch_status"),"stale"),
    market_mode = fm_scalar(cfg$mtype, "unknown"),
    scoring = lineup$custom %||% list(), point_system = lineup$pointSystem %||% NULL,
    # Unknown deadline/solvency boundary must block unattended mutations.
    deadline = NA_character_, solvency_verified = FALSE)
}

source("competition_context.R")
