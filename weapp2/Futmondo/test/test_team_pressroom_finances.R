#!/usr/bin/env Rscript

# ============================================================================
# test/test_team_pressroom_finances.R
#
# Validates pressroom transaction history against the API-reported budget.
#
# Steps:
#   1. Read .Renviron credentials (user_name, password)
#   2. Source futmondo_functions.R
#   3. Login via login()
#   4. Retrieve active championship and user team ID via get_championships()
#   5. Fetch ALL pressroom transactions from PRESSROOM_URL with cursor pagination
#   6. Filter transactions involving the user's own team (buyer or seller)
#   7. Identify today's transactions and extract the latest transaction ID
#   8. Calculate Money Left = Initial Budget - Purchases + Sales
#      Also query /1/userteam/information to compare with API-reported budget
#   9. Display a clear, formatted summary
# ============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
})

# Helper: format integer/numeric with thousands separator (dot)
fmt_num <- function(x) {
  if (is.na(x)) return("N/A")
  formatC(abs(x), format = "f", digits = 0, big.mark = ".", decimal.mark = ",")
}

# ---- Source the main functions file (project root is parent of test/) -------
source("futmondo_functions.R")

# ---- Constants ---------------------------------------------------------------
INITIAL_BUDGET <- 300000000L

cat("\n")
cat("=================================================================\n")
cat("  Futmondo Pressroom Finances Validation\n")
cat("=================================================================\n\n")

# =============================================================================
# Step 1: Read credentials from .Renviron
# =============================================================================
cat("[1/9] Reading credentials from .Renviron...\n")
user_name <- Sys.getenv("user_name")
password  <- Sys.getenv("password")

if (user_name == "" || password == "") {
  stop("Missing credentials in .Renviron (user_name or password).")
}
cat(sprintf("    user_name: %s\n", user_name))

# =============================================================================
# Step 2: Login
# =============================================================================
cat("\n[2/9] Logging in to Futmondo API...\n")
login_result <- login(user_name = user_name, password = password)
token  <- login_result["token"]
userid <- login_result["userid"]
cat(sprintf("    userid: %s\n", userid))

# =============================================================================
# Step 3: Active championship & user team ID
# =============================================================================
cat("\n[3/9] Getting active championship and user team ID...\n")

# Clear cache so we get fresh data for this test run
clear_api_cache()

championships <- get_championships(login_result)

# get_championships() returns an unlisted named vector.
# Names are flat: "id", "name", "userteam.id", "userteam.name", etc.
champ_id       <- NULL
user_team_id   <- NULL

for (nm in names(championships)) {
  # Championship ID -- the top-level "id" field (not "userteam.id")
  if (is.null(champ_id) && nm == "id") {
    champ_id <- championships[nm]
  }
  # User team ID -- "userteam.id" (or "userTeamId" variant)
  if (is.null(user_team_id) && grepl("userteam\\.id$", nm, ignore.case = TRUE)) {
    user_team_id <- championships[nm]
  } else if (is.null(user_team_id) && grepl("userTeamId$", nm, ignore.case = TRUE)) {
    user_team_id <- championships[nm]
  }
}

cat(sprintf("    championship_id : %s\n", champ_id))
cat(sprintf("    user_team_id    : %s\n", if (!is.null(user_team_id) && user_team_id != "") user_team_id else "(not found in championships)"))

if (is.null(champ_id) || champ_id == "") {
  stop("Could not extract championship_id from get_championships().")
}

# If user_team_id is not in the championships data, fall back to get_teams()
if (is.null(user_team_id) || user_team_id == "") {
  cat("    user_team_id not in championships data -- falling back to get_teams()...\n")
  teams_df <- get_teams(login_result, champ_id)

  if (nrow(teams_df) == 0) {
    stop("No teams found in get_teams().")
  }

  # Try to find the team belonging to the logged-in user
  owner_col <- intersect(c("owner", "userid", "user_id", "userId"), colnames(teams_df))
  if (length(owner_col) > 0) {
    match_row <- teams_df[teams_df[[owner_col[1]]] == userid, ]
    if (nrow(match_row) > 0) {
      user_team_id <- match_row$teamid[1]
    } else {
      user_team_id <- teams_df$teamid[1]
    }
  } else {
    # Fallback: take the first team
    user_team_id <- teams_df$teamid[1]
  }
  cat(sprintf("    resolved user_team_id: %s\n", user_team_id))
}

# =============================================================================
# Step 4: Fetch ALL pressroom transactions (cursor pagination)
# =============================================================================
cat("\n[4/9] Fetching all pressroom transactions (cursor pagination)...\n")

all_transactions <- list()
cursor <- ""
page_count <- 0
max_pages <- 50

headers <- c("Content-Type" = "application/json; charset=utf-8")

while (page_count < max_pages) {
  page_count <- page_count + 1
  cursor_display <- if (cursor == "") "(initial)" else cursor
  cat(sprintf("    Page %d (cursor: %s)...\n", page_count, cursor_display))

  payload <- list(
    header = list(
      token  = token,
      userid = userid
    ),
    query = list(
      championshipId = as.character(champ_id),
      from = cursor
    ),
    answer = list()
  )

  response <- POST(PRESSROOM_URL,
                   body = toJSON(payload, auto_unbox = TRUE),
                   add_headers(.headers = headers))
  ans <- httr::content(response)

  if (is.null(ans) || !("answer" %in% names(ans)) ||
      is.null(ans$answer) || !("news" %in% names(ans$answer))) {
    cat("    No news data in response, stopping.\n")
    break
  }

  news <- ans$answer$news
  if (is.null(news) || length(news) == 0) {
    cat("    Empty news array, all pages fetched.\n")
    break
  }

  for (item in news) {
    all_transactions[[length(all_transactions) + 1]] <- item
  }

  # Get the last item's _id as the cursor for the next page
  last_item <- news[[length(news)]]
  last_id <- if (!is.null(last_item[["_id"]])) as.character(last_item[["_id"]]) else ""

  if (last_id == "" || last_id == cursor) {
    cat("    Reached end of pagination.\n")
    break
  }

  cursor <- last_id
}

cat(sprintf("    Total transactions fetched: %d across %d pages.\n",
            length(all_transactions), page_count))

# =============================================================================
# Step 5: Parse transactions into a data frame
# =============================================================================
cat("\n[5/9] Parsing transactions into a data frame...\n")

get_val <- function(obj, key, default = "") {
  if (is.null(obj)) return(default)
  nm <- names(obj)
  if (is.null(nm) || !key %in% nm) return(default)
  val <- obj[[key]]
  if (is.null(val)) return(default)
  return(val)
}

parse_item <- function(item) {
  item <- as.list(item)

  player_obj <- get_val(item, "_player", NULL)
  p_id       <- if (!is.null(player_obj)) as.character(get_val(player_obj, "_id", "")) else ""
  p_name     <- if (!is.null(player_obj)) as.character(get_val(player_obj, "name", "")) else ""

  buyer_obj  <- get_val(item, "_buyer", NULL)
  b_id       <- if (!is.null(buyer_obj)) as.character(get_val(buyer_obj, "_id", "")) else ""
  b_name     <- if (!is.null(buyer_obj)) as.character(get_val(buyer_obj, "name", "")) else ""

  seller_obj <- get_val(item, "_seller", NULL)
  s_id       <- if (!is.null(seller_obj)) as.character(get_val(seller_obj, "_id", "")) else ""
  s_name     <- if (!is.null(seller_obj)) as.character(get_val(seller_obj, "name", "")) else ""

  price_val  <- get_val(item, "price", 0)
  price_num  <- suppressWarnings(as.numeric(price_val))
  if (is.na(price_num)) price_num <- 0

  data.frame(
    id               = as.character(get_val(item, "_id", "")),
    created          = as.character(get_val(item, "created", "")),
    player_id        = p_id,
    player_name      = p_name,
    buyer_team_id    = b_id,
    buyer_team_name  = b_name,
    seller_team_id   = s_id,
    seller_team_name = s_name,
    price            = price_num,
    stringsAsFactors = FALSE
  )
}

df <- do.call(rbind, lapply(all_transactions, parse_item))
cat(sprintf("    Parsed %d transactions.\n", nrow(df)))

# =============================================================================
# Step 6: Filter transactions involving the user's own team
# =============================================================================
cat("\n[6/9] Filtering transactions involving user's team (", user_team_id, ")...\n")

my_txns <- df[df$buyer_team_id == user_team_id | df$seller_team_id == user_team_id, ]
cat(sprintf("    Transactions involving user's team: %d\n", nrow(my_txns)))

# =============================================================================
# Step 7: Identify today's transactions, most recent overall, most recent for user team
# =============================================================================
cat("\n[7/9] Identifying today's transactions, most recent overall, most recent for user team...\n")

today_str <- format(Sys.Date(), "%Y-%m-%d")
cat(sprintf("    Today (local): %s\n", today_str))

# --- Most recent transaction overall in the pressroom feed ---
# The API returns newest-first, so df[1,] is the top of the feed.
# This _id is the cursor for future `from` polling.
most_recent_overall <- df[1, ]
cat(sprintf("    Most recent overall in feed: ID=%s  created=%s\n",
            most_recent_overall$id, most_recent_overall$created))

# --- Most recent transaction for the user's team ---
if (nrow(my_txns) > 0) {
  my_txns_sorted <- my_txns[order(my_txns$created, decreasing = TRUE), ]
  most_recent_myteam <- my_txns_sorted[1, ]
  cat(sprintf("    Most recent for user's team: ID=%s  created=%s\n",
              most_recent_myteam$id, most_recent_myteam$created))
} else {
  most_recent_myteam <- NULL
  cat("    No transactions found for user's team.\n")
}

# --- Today's transactions for the user's team ---
my_txns$txn_date <- substr(my_txns$created, 1, 10)  # "YYYY-MM-DD"
today_txns <- my_txns[my_txns$txn_date == today_str, ]
cat(sprintf("    Transactions of today (user's team): %d\n", nrow(today_txns)))

# Sort today's transactions by created timestamp (descending) to get the latest
if (nrow(today_txns) > 0) {
  today_txns <- today_txns[order(today_txns$created, decreasing = TRUE), ]
  latest_txn_id <- today_txns$id[1]
  cat(sprintf("    Latest transaction ID of today: %s\n", latest_txn_id))
} else {
  latest_txn_id <- NA_character_
  cat("    No transactions today.\n")
}

# =============================================================================
# Step 8: Calculate Money Left
# =============================================================================
cat("\n[8/9] Calculating Money Left...\n")

# Purchases = transactions where the user's team is the buyer
purchases <- my_txns[my_txns$buyer_team_id == user_team_id, ]
total_purchases <- sum(purchases$price, na.rm = TRUE)

# Sales = transactions where the user's team is the seller
sales <- my_txns[my_txns$seller_team_id == user_team_id, ]
total_sales <- sum(sales$price, na.rm = TRUE)

# Money Left = Initial Budget - Total Purchases + Total Sales
calc_money_left <- INITIAL_BUDGET - total_purchases + total_sales

cat(sprintf("    Initial Budget:          %s EUR\n", fmt_num(INITIAL_BUDGET)))
cat(sprintf("    Total Purchases (out):   %s EUR\n", fmt_num(total_purchases)))
cat(sprintf("    Total Sales (in):        %s EUR\n", fmt_num(total_sales)))
cat(sprintf("    Calculated Money Left:   %s EUR\n", fmt_num(calc_money_left)))

# Query /1/userteam/information for the API-reported budget
cat("\n    Querying /1/userteam/information for API-reported budget...\n")
api_info <- get_user_team_info(login_result, champ_id, user_team_id)
api_budget <- if (!is.null(api_info) && !is.null(api_info$budget)) as.numeric(api_info$budget) else NA

cat(sprintf("    API-reported budget:     %s EUR\n", fmt_num(api_budget)))

if (!is.na(api_budget)) {
  diff_val <- abs(calc_money_left - api_budget)
  cat(sprintf("    Difference:              %s EUR\n", fmt_num(diff_val)))
  if (diff_val == 0) {
    cat("    >> MATCH: Calculated budget matches API budget.\n")
  } else {
    cat(sprintf("    >> MISMATCH: Calculated budget differs from API by %s EUR.\n", fmt_num(diff_val)))
  }
}

# =============================================================================
# Step 9: Formatted summary
# =============================================================================
cat("\n")
cat("=================================================================\n")
cat("           PRESSROOM FINANCES SUMMARY\n")
cat("=================================================================\n")
cat(sprintf("  User:              %s\n", user_name))
cat(sprintf("  User Team ID:      %s\n", user_team_id))
cat(sprintf("  Championship ID:   %s\n", champ_id))
cat(sprintf("  Today's Date:      %s\n", today_str))
cat("-----------------------------------------------------------------\n")
cat(sprintf("  Initial Budget:    %s EUR\n", fmt_num(INITIAL_BUDGET)))
cat(sprintf("  Total Purchases:   %s EUR\n", fmt_num(total_purchases)))
cat(sprintf("  Total Sales:       %s EUR\n", fmt_num(total_sales)))
cat(sprintf("  Calculated Left:   %s EUR\n", fmt_num(calc_money_left)))
if (!is.na(api_budget)) {
  cat(sprintf("  API Budget:        %s EUR\n", fmt_num(api_budget)))
}
cat("-----------------------------------------------------------------\n")
cat(sprintf("  Today's Txns:      %d\n", nrow(today_txns)))
cat(sprintf("  Latest Txn ID:     %s\n", latest_txn_id))
cat("=================================================================\n")

# --- Most recent transaction overall in the pressroom feed ---
# This is the cursor (_id) for future `from` polling
cat("\n--- Most Recent Transaction Overall (Feed Cursor) ---\n")
cat(sprintf("  _id:               %s\n", most_recent_overall$id))
cat(sprintf("  created:           %s\n", most_recent_overall$created))
cat(sprintf("  player:            %s\n", most_recent_overall$player_name))
cat(sprintf("  buyer (team):      %s (id=%s)\n", most_recent_overall$buyer_team_name, most_recent_overall$buyer_team_id))
cat(sprintf("  seller (team):     %s (id=%s)\n", most_recent_overall$seller_team_name, most_recent_overall$seller_team_id))
cat(sprintf("  price:             %s EUR\n", fmt_num(most_recent_overall$price)))
cat(sprintf("  >> Use this _id as `from` cursor for next poll: %s\n", most_recent_overall$id))

# --- Most recent transaction for the user's team ---
cat("\n--- Most Recent Transaction for User's Team ---\n")
if (!is.null(most_recent_myteam)) {
  dir_label <- if (most_recent_myteam$buyer_team_id == user_team_id) "BUY" else "SELL"
  cat(sprintf("  _id:               %s\n", most_recent_myteam$id))
  cat(sprintf("  created:           %s\n", most_recent_myteam$created))
  cat(sprintf("  direction:         %s\n", dir_label))
  cat(sprintf("  player:            %s\n", most_recent_myteam$player_name))
  counterparty_name <- if (dir_label == "BUY") most_recent_myteam$seller_team_name else most_recent_myteam$buyer_team_name
  counterparty_id   <- if (dir_label == "BUY") most_recent_myteam$seller_team_id else most_recent_myteam$buyer_team_id
  cat(sprintf("  counterparty:      %s (id=%s)\n", counterparty_name, counterparty_id))
  cat(sprintf("  price:             %s EUR\n", fmt_num(most_recent_myteam$price)))
} else {
  cat("  (none)\n")
}

# Detail of today's transactions
cat("\n--- Today's Transactions Detail (User's Team) ---\n")
if (nrow(today_txns) > 0) {
  for (i in seq_len(nrow(today_txns))) {
    row <- today_txns[i, ]
    direction <- if (row$buyer_team_id == user_team_id) "BUY" else "SELL"
    counterparty <- if (direction == "BUY") row$seller_team_name else row$buyer_team_name
    cat(sprintf("  [%s] %-4s %-25s | %s | %s EUR | ID: %s\n",
                substr(row$created, 1, 19), direction, row$player_name,
                counterparty, fmt_num(row$price), row$id))
  }
} else {
  cat("  (no transactions today)\n")
}

cat("\nDone.\n")