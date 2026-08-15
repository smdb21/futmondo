# init_db.R -- Verify Supabase database schema on startup
#
# Usage:
#   Rscript scripts/init_db.R

# Load credentials from .Renviron if present
if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

source("supabase_connector.R")

cat("\n[Init] Verifying Supabase database schema...\n\n")
result <- init_supabase_db(verbose = TRUE)

cat("\n")
cat("====================================\n")
cat("  DATABASE INIT STATUS\n")
cat("====================================\n")

if (is.logical(result) && length(result) == 1) {
  if (result) {
    cat("\n  All tables verified successfully.\n\n")
  } else {
    cat("\n  WARNING: One or more tables could not be verified.\n")
    cat("  Check the warnings above for details.\n")
    cat("  If tables are missing, run the DDL in scripts/schema.sql\n")
    cat("  via the Supabase SQL Editor.\n\n")
  }
} else if (is.list(result)) {
  for (tbl in names(result)) {
    status <- result[[tbl]]
    cat(sprintf("  %-25s %s\n", tbl, status))
  }
  cat("\n")
}

cat("====================================\n")
cat("\n")