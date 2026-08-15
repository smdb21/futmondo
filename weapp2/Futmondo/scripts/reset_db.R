# reset_db.R -- Reset all Supabase tables to empty state
#
# Usage:
#   Rscript scripts/reset_db.R --force
#
# Without --force, the script will prompt for confirmation in interactive mode.

# Load credentials from .Renviron if present
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

source("supabase_connector.R")

# Parse command-line arguments
args <- commandArgs(trailingOnly = TRUE)
force_flag <- "--force" %in% args

# If not forced and running interactively, ask for confirmation
if (!force_flag && interactive()) {
  cat("\nWARNING: This will delete ALL data from every Supabase table.\n")
  cat("This action cannot be undone.\n\n")
  response <- readline(prompt = "Are you sure you want to proceed? [y/N] ")
  if (tolower(trimws(response)) != "y") {
    cat("\nAborted. No changes were made.\n")
    quit(status = 0)
  }
} else if (!force_flag && !interactive()) {
  cat("\nERROR: Non-interactive mode requires --force flag to proceed.\n")
  cat("Usage: Rscript scripts/reset_db.R --force\n\n")
  quit(status = 1)
}

cat("\n[Reset] Starting database reset...\n")
result <- supabase_reset_database(force = TRUE)

cat("\n")
cat("====================================\n")
cat("  DATABASE RESET SUMMARY\n")
cat("====================================\n")

for (tbl in names(result)) {
  status <- result[[tbl]]
  cat(sprintf("  %-25s %s\n", tbl, status))
}

cat("====================================\n")
cat("\n")