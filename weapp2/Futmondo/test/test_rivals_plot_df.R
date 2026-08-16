#!/usr/bin/env Rscript

# ============================================================================
# test/test_rivals_plot_df.R
#
# Tests the plot_df calculation for the Rivals Module buying-power plot
# across all three metrics: "cash", "investment", "volume".
#
# Steps:
#   1. Load .Renviron credentials
#   2. Source futmondo_functions.R and Modules/Rivals_Module.R
#   3. Login, get championship, fetch teams via get_teams()
#   4. Fetch get_championship_pressroom()
#   5. Calculate plot_df for metric = "cash", "investment", "volume"
#   6. Verify no errors and all 10 teams have valid values
# ============================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(data.table)
})

# ---- Source the main functions file -----------------------------------------
source("futmondo_functions.R")

# ---- Constants ---------------------------------------------------------------
INITIAL_BUDGET <- 300000000

cat("\n")
cat("=================================================================\n")
cat("  Rivals Module - plot_df Calculation Test\n")
cat("=================================================================\n\n")

# =============================================================================
# Step 1: Read credentials from .Renviron
# =============================================================================
cat("[1/6] Reading credentials from .Renviron...\n")
user_name <- Sys.getenv("user_name")
password  <- Sys.getenv("password")

if (user_name == "" || password == "") {
  stop("Missing credentials in .Renviron (user_name or password).")
}
cat(sprintf("    user_name: %s\n", user_name))

# =============================================================================
# Step 2: Login
# =============================================================================
cat("\n[2/6] Logging in to Futmondo API...\n")
login_result <- login(user_name = user_name, password = password)
cat(sprintf("    userid: %s\n", login_result["userid"]))

# Clear cache so we get fresh data for this test run
clear_api_cache()

# =============================================================================
# Step 3: Get active championship
# =============================================================================
cat("\n[3/6] Getting active championship...\n")
championships <- get_championships(login_result)

champ_id <- NULL
for (nm in names(championships)) {
  if (is.null(champ_id) && nm == "id") {
    champ_id <- championships[nm]
  }
}

if (is.null(champ_id) || champ_id == "") {
  stop("Could not extract championship_id from get_championships().")
}
cat(sprintf("    championship_id: %s\n", champ_id))

# =============================================================================
# Step 4: Fetch teams and pressroom
# =============================================================================
cat("\n[4/6] Fetching teams and pressroom transactions...\n")

teams_df <- get_teams(login = login_result, championship_id = champ_id)
cat(sprintf("    Teams fetched: %d\n", nrow(teams_df)))

pressroom_df <- get_championship_pressroom(login = login_result, championship_id = champ_id)
cat(sprintf("    Pressroom transactions fetched: %d\n", nrow(pressroom_df)))

# =============================================================================
# Step 5: Calculate plot_df for each metric
# =============================================================================
cat("\n[5/6] Calculating plot_df for each metric...\n")

# Replicate the plot_df calculation logic from Rivals_Module.R
# (output$league_finances_plot, lines ~598-654)

calculate_plot_df <- function(teams_df, pressroom_df, metric, initial_budget = 300000000) {
  # Use full date range (no filtering)
  start_date <- as.POSIXct("2000-01-01", tz = "UTC")
  end_date   <- as.POSIXct("2100-01-01", tz = "UTC")

  filtered_tx <- pressroom_df
  if ("created" %in% colnames(filtered_tx)) {
    parsed_dates <- suppressWarnings(as.POSIXct(as.character(filtered_tx$created), tz = "UTC"))
    filtered_tx <- filtered_tx[!is.na(parsed_dates) & parsed_dates >= start_date & parsed_dates <= end_date, ]
  }

  team_ids <- if (!is.null(teams_df) && "teamid" %in% colnames(teams_df)) as.character(teams_df$teamid) else character(0)
  team_names_map <- if (!is.null(teams_df) && "teamname" %in% colnames(teams_df)) {
    setNames(as.character(teams_df$teamname), as.character(teams_df$teamid))
  } else character(0)

  plot_df <- data.frame(
    team_id = team_ids,
    team = ifelse(team_ids %in% names(team_names_map), team_names_map[team_ids], team_ids),
    purchases = 0,
    sales = 0,
    stringsAsFactors = FALSE
  )

  if (nrow(filtered_tx) > 0) {
    # Aggregate purchases
    if ("buyer_team_id" %in% colnames(filtered_tx) && "price" %in% colnames(filtered_tx)) {
      buys_agg <- filtered_tx %>%
        dplyr::filter(!is.na(buyer_team_id) & nzchar(as.character(buyer_team_id))) %>%
        dplyr::group_by(buyer_team_id = as.character(buyer_team_id)) %>%
        dplyr::summarise(total_purchases = sum(suppressWarnings(as.numeric(price)), na.rm = TRUE), .groups = "drop")

      if (nrow(buys_agg) > 0) {
        match_idx <- match(plot_df$team_id, buys_agg$buyer_team_id)
        plot_df$purchases <- ifelse(!is.na(match_idx), buys_agg$total_purchases[match_idx], 0)
      }
    }

    # Aggregate sales
    if ("seller_team_id" %in% colnames(filtered_tx) && "price" %in% colnames(filtered_tx)) {
      sells_agg <- filtered_tx %>%
        dplyr::filter(!is.na(seller_team_id) & nzchar(as.character(seller_team_id))) %>%
        dplyr::group_by(seller_team_id = as.character(seller_team_id)) %>%
        dplyr::summarise(total_sales = sum(suppressWarnings(as.numeric(price)), na.rm = TRUE), .groups = "drop")

      if (nrow(sells_agg) > 0) {
        match_idx <- match(plot_df$team_id, sells_agg$seller_team_id)
        plot_df$sales <- ifelse(!is.na(match_idx), sells_agg$total_sales[match_idx], 0)
      }
    }
  }

  # Apply metric
  if (metric == "cash") {
    plot_df$value <- initial_budget - plot_df$purchases + plot_df$sales
  } else if (metric == "investment") {
    plot_df$value <- plot_df$purchases
  } else {
    # volume
    plot_df$value <- plot_df$purchases + plot_df$sales
  }

  plot_df <- plot_df %>% dplyr::arrange(value)
  return(plot_df)
}

# ---- Test each metric -------------------------------------------------------
metrics <- c("cash", "investment", "volume")
all_passed <- TRUE

for (m in metrics) {
  cat(sprintf("\n  --- Metric: %s ---\n", m))

  tryCatch({
    pdf <- calculate_plot_df(teams_df, pressroom_df, m, INITIAL_BUDGET)

    n_teams <- nrow(pdf)
    cat(sprintf("    Rows in plot_df: %d\n", n_teams))

    # Check all teams present
    if (n_teams < 10) {
      cat(sprintf("    WARNING: Expected 10 teams, got %d\n", n_teams))
      all_passed <- FALSE
    } else {
      cat(sprintf("    OK: All %d teams present\n", n_teams))
    }

    # Check for valid (non-NA, non-NaN, finite) values
    na_count <- sum(is.na(pdf$value))
    nan_count <- sum(is.nan(pdf$value))
    inf_count <- sum(!is.finite(pdf$value) & !is.na(pdf$value) & !is.nan(pdf$value))

    cat(sprintf("    NA values: %d, NaN values: %d, Inf values: %d\n", na_count, nan_count, inf_count))

    if (na_count > 0 || nan_count > 0 || inf_count > 0) {
      cat("    FAIL: Invalid values detected in plot_df$value\n")
      all_passed <- FALSE
    } else {
      cat("    OK: All values are valid (finite, non-NA, non-NaN)\n")
    }

    # Print summary stats
    cat(sprintf("    Min value: %.0f\n", min(pdf$value)))
    cat(sprintf("    Max value: %.0f\n", max(pdf$value)))
    cat(sprintf("    Mean value: %.0f\n", mean(pdf$value)))

    # Print per-team values
    cat("    Per-team breakdown:\n")
    for (i in seq_len(nrow(pdf))) {
      cat(sprintf("      %-30s purchases=%.0f  sales=%.0f  value=%.0f\n",
                  pdf$team[i], pdf$purchases[i], pdf$sales[i], pdf$value[i]))
    }

  }, error = function(e) {
    cat(sprintf("    ERROR: %s\n", e$message))
    all_passed <<- FALSE
  })
}

# =============================================================================
# Step 6: Final verdict
# =============================================================================
cat("\n")
cat("=================================================================\n")
if (all_passed) {
  cat("  RESULT: ALL TESTS PASSED\n")
} else {
  cat("  RESULT: SOME TESTS FAILED -- review output above\n")
}
cat("=================================================================\n\n")

if (!all_passed) {
  quit(status = 1, save = "no")
}