# populate_db.R -- Populate all Supabase tables from the Futmondo API
#
# Usage:
#   Rscript scripts/populate_db.R
#
# Reads credentials from .Renviron (user_name, password), logs in,
# retrieves the active championship ID, populates all 7 tables,
# and prints the resulting row counts.

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
})

# ---- Load credentials from .Renviron ----------------------------------------
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

# ---- Source project files ---------------------------------------------------
source("futmondo_functions.R")
source("supabase_connector.R")

cat("\n")
cat("=================================================================\n")
cat("  Futmondo Database Population\n")
cat("=================================================================\n\n")

# =============================================================================
# Step 1: Read credentials from .Renviron
# =============================================================================
cat("[1/5] Reading credentials from .Renviron...\n")
user_name <- Sys.getenv("user_name")
password  <- Sys.getenv("password")

if (user_name == "" || password == "") {
  stop("Missing credentials in .Renviron (user_name or password).")
}
cat(sprintf("    user_name: %s\n", user_name))

# =============================================================================
# Step 2: Login
# =============================================================================
cat("\n[2/5] Logging in to Futmondo API...\n")
login_result <- login(user_name = user_name, password = password)
token  <- login_result["token"]
userid <- login_result["userid"]
cat(sprintf("    userid: %s\n", userid))

# =============================================================================
# Step 3: Retrieve active championship ID
# =============================================================================
cat("\n[3/5] Getting active championship ID...\n")
clear_api_cache()

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
# Step 4: Populate entire database
# =============================================================================
cat("\n[4/5] Populating entire database...\n")
populate_entire_database(
  login = login_result,
  championship_id = champ_id,
  verbose = TRUE
)

# =============================================================================
# Step 5: Display row counts for all 7 tables
# =============================================================================
cat("\n[5/5] Fetching row counts for all tables...\n\n")
counts <- get_table_row_counts()

cat("=================================================================\n")
cat("  ROW COUNTS AFTER POPULATION\n")
cat("=================================================================\n\n")

for (i in seq_len(nrow(counts))) {
  tbl_name <- counts$table_name[i]
  row_count <- counts$row_count[i]
  if (is.na(row_count)) {
    cat(sprintf("  %-25s %s\n", tbl_name, "N/A"))
  } else {
    cat(sprintf("  %-25s %s\n", tbl_name,
                formatC(row_count, format = "f", big.mark = ",", digits = 0)))
  }
}

total <- sum(counts$row_count, na.rm = TRUE)
if (any(is.na(counts$row_count))) {
  total_str <- "N/A"
} else {
  total_str <- formatC(total, format = "f", big.mark = ",", digits = 0)
}

cat("\n  -----------------------------------------\n")
cat(sprintf("  %-25s %s\n", "GRAND TOTAL", total_str))
cat("\n=================================================================\n\n")