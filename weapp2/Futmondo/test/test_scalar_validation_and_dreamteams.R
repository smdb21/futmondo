#!/usr/bin/env Rscript
# =============================================================================
# Test script: scalar validation logic + sync_all_championship_dreamteams
# =============================================================================

# Source the necessary files
source("futmondo_functions.R")
source("supabase_connector.R")

cat("\n======================================================================\n")
cat("TEST 1: Scalar validation logic (named token vector)\n")
cat("======================================================================\n")

# The validation logic from Admin_Module.R lines 493-496:
check_valid_token <- function(tok) {
  !is.null(tok) && length(tok) >= 1 && (
    (!is.null(names(tok)) && !is.null(tok[["token"]]) && nzchar(tok[["token"]])) ||
    (is.null(names(tok)) && nzchar(tok[1]))
  )
}

# --- 1a: Valid token ---
tok <- c(token = "abc", userid = "123", user_name = "test@example.com")
result <- check_valid_token(tok)
cat(sprintf("tok <- c(token='abc', userid='123', user_name='test@example.com')\n"))
cat(sprintf("  Result: %s\n", result))
cat(sprintf("  Expected: TRUE\n"))
cat(sprintf("  PASS: %s\n\n", ifelse(result == TRUE, "YES", "NO")))

# --- 1b: NULL ---
tok <- NULL
result <- check_valid_token(tok)
cat(sprintf("tok <- NULL\n"))
cat(sprintf("  Result: %s\n", result))
cat(sprintf("  Expected: FALSE\n"))
cat(sprintf("  PASS: %s\n\n", ifelse(result == FALSE, "YES", "NO")))

# --- 1c: character(0) ---
tok <- character(0)
result <- check_valid_token(tok)
cat(sprintf("tok <- character(0)\n"))
cat(sprintf("  Result: %s\n", result))
cat(sprintf("  Expected: FALSE\n"))
cat(sprintf("  PASS: %s\n\n", ifelse(result == FALSE, "YES", "NO")))

# --- 1d: c(token = "") ---
tok <- c(token = "")
result <- check_valid_token(tok)
cat(sprintf("tok <- c(token = '')\n"))
cat(sprintf("  Result: %s\n", result))
cat(sprintf("  Expected: FALSE\n"))
cat(sprintf("  PASS: %s\n\n", ifelse(result == FALSE, "YES", "NO")))

# --- 1e: Extra edge cases ---
# Empty string token with other fields
tok <- c(token = "", userid = "123", user_name = "test@example.com")
result <- check_valid_token(tok)
cat(sprintf("tok <- c(token='', userid='123', user_name='test@example.com')\n"))
cat(sprintf("  Result: %s\n", result))
cat(sprintf("  Expected: FALSE (empty token)\n"))
cat(sprintf("  PASS: %s\n\n", ifelse(result == FALSE, "YES", "NO")))

# Unnamed vector with valid first element
tok <- c("abc", "123", "test@example.com")
result <- check_valid_token(tok)
cat(sprintf("tok <- c('abc', '123', 'test@example.com')  (unnamed)\n"))
cat(sprintf("  Result: %s\n", result))
cat(sprintf("  Expected: TRUE\n"))
cat(sprintf("  PASS: %s\n\n", ifelse(result == TRUE, "YES", "NO")))

# Unnamed vector with empty first element
tok <- c("", "123", "test@example.com")
result <- check_valid_token(tok)
cat(sprintf("tok <- c('', '123', 'test@example.com')  (unnamed, empty first)\n"))
cat(sprintf("  Result: %s\n", result))
cat(sprintf("  Expected: FALSE\n"))
cat(sprintf("  PASS: %s\n\n", ifelse(result == FALSE, "YES", "NO")))

cat("\n======================================================================\n")
cat("TEST 2: sync_all_championship_dreamteams with real login\n")
cat("======================================================================\n")

# Load credentials from .Renviron
user_name <- Sys.getenv("user_name")
password  <- Sys.getenv("password")

if (user_name == "" || password == "") {
  cat("WARNING: user_name or password env vars are empty. Skipping real login test.\n")
} else {
  cat(sprintf("Logging in as: %s\n", user_name))

  login_result <- tryCatch({
    login(user_name = user_name, password = password)
  }, error = function(e) {
    cat(sprintf("LOGIN ERROR: %s\n", e$message))
    return(NULL)
  })

  if (!is.null(login_result)) {
    cat(sprintf("Login successful. Token: %s, Userid: %s\n",
                login_result[["token"]], login_result[["userid"]]))

    # Get a championship to test with
    champ_data <- tryCatch({
      get_championships(login = login_result, championship_name = NULL)
    }, error = function(e) {
      cat(sprintf("ERROR getting championships: %s\n", e$message))
      return(NULL)
    })

    if (!is.null(champ_data) && length(champ_data) > 0) {
      # Extract first championship id
      champ_id <- as.character(champ_data["id"])
      if (is.na(champ_id) || champ_id == "") {
        # Try to find any id field
        id_fields <- grep("id", names(champ_data), value = TRUE)
        if (length(id_fields) > 0) {
          champ_id <- as.character(champ_data[id_fields[1]])
        }
      }

      cat(sprintf("Testing sync_all_championship_dreamteams with championship_id: %s\n", champ_id))

      dreamteam_result <- tryCatch({
        sync_all_championship_dreamteams(login = login_result, championship_id = champ_id, verbose = TRUE)
      }, error = function(e) {
        cat(sprintf("ERROR in sync_all_championship_dreamteams: %s\n", e$message))
        return(NULL)
      })

      if (!is.null(dreamteam_result)) {
        cat(sprintf("\n--- Result ---\n"))
        cat(sprintf("  status:       %s\n", dreamteam_result[["status"]]))
        cat(sprintf("  total_rounds: %s\n", dreamteam_result[["total_rounds"]]))
        cat(sprintf("  total_players: %s\n", dreamteam_result[["total_players"]]))

        # Check expected structure
        has_status       <- !is.null(dreamteam_result[["status"]])
        has_total_rounds <- !is.null(dreamteam_result[["total_rounds"]])
        has_total_players <- !is.null(dreamteam_result[["total_players"]])

        cat(sprintf("\n  Structure check: status=%s, total_rounds=%s, total_players=%s\n",
                    has_status, has_total_rounds, has_total_players))

        # The function returns list(status="ok", total_rounds=0, total_players=0) when no finished rounds
        # or list(status="ok", total_rounds=N, total_players=M) when there are rounds
        is_ok_status <- dreamteam_result[["status"]] %in% c("ok", "skipped", "error")
        cat(sprintf("  Status is valid ('ok'/'skipped'/'error'): %s\n", is_ok_status))

        cat(sprintf("\n  PASS (returns gracefully): %s\n",
                    ifelse(has_status && has_total_rounds && has_total_players && is_ok_status, "YES", "NO")))
      } else {
        cat("FAIL: sync_all_championship_dreamteams returned NULL\n")
      }
    } else {
      cat("No championship data found. Skipping dreamteam sync test.\n")
    }
  } else {
    cat("Login failed. Skipping dreamteam sync test.\n")
  }
}

cat("\n======================================================================\n")
cat("TEST 3: sync_all_championship_dreamteams with NULL login (unit test)\n")
cat("======================================================================\n")

result_null_login <- sync_all_championship_dreamteams(login = NULL, championship_id = NULL, verbose = TRUE)
cat(sprintf("  status:       %s\n", result_null_login[["status"]]))
cat(sprintf("  total_rounds: %s\n", result_null_login[["total_rounds"]]))
cat(sprintf("  total_players: %s\n", result_null_login[["total_players"]]))
cat(sprintf("  Expected: status='skipped', total_rounds=0, total_players=0\n"))
cat(sprintf("  PASS: %s\n\n",
            ifelse(result_null_login[["status"]] == "skipped" &&
                   result_null_login[["total_rounds"]] == 0 &&
                   result_null_login[["total_players"]] == 0, "YES", "NO")))

cat("\n======================================================================\n")
cat("ALL TESTS COMPLETE\n")
cat("======================================================================\n")