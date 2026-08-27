#!/usr/bin/env Rscript
# =============================================================================
# test_supabase_schema_helpers.R
# Standalone, OFFLINE regression test for the Supabase schema helpers:
#   - supabase_primary_key() selects team_id for manager_dna_profiles, id otherwise
#   - supabase_delete_all() uses <primary key>=not.is.null (mocked, no HTTP)
#   - docs/database_schema.md contains the three ALTER upgrade statements and fields
#   - docs/supabase_migration.sql is gone
#   - connector init/count queries use supabase_primary_key(tbl) (static parse)
#   - get_table_row_counts() records each query$select (mocked HTTP, offline)
#   - init_supabase_db() returns FALSE + warns on a manager-DNA 400 (mocked HTTP)
#
# No login / network required. Run with:  Rscript test/test_supabase_schema_helpers.R
# =============================================================================

# Ensure we run from the project root regardless of caller cwd.
if (!file.exists("supabase_connector.R")) {
  test_dir <- normalizePath("test", mustWork = FALSE)
  setwd(file.path(dirname(test_dir), ".."))
}

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
})

# ---- Result accumulator ----
sh_results <- list()
sh_record <- function(label, expr) {
  res <- tryCatch(
    { expr; list(status = "pass", trace = character(0)) },
    error = function(e) list(status = "error", trace = conditionMessage(e))
  )
  sh_results[[label]] <<- res
  if (res$status == "pass") {
    cat(sprintf("  [PASS] %s\n", label))
  } else {
    cat(sprintf("  [FAIL] %s -- %s\n", label, res$trace))
  }
  invisible(res)
}

cat("\n======================================================================\n")
cat("  SUPABASE SCHEMA HELPERS TESTS (offline)\n")
cat("======================================================================\n")

# ---- Source the code under test ----
sh_record("source_supabase_connector", { source("supabase_connector.R") })

all_sourced <- !any(sapply(sh_results, function(r) r$status == "error"))

if (!all_sourced) {
  cat("\n  FATAL: could not source supabase_connector.R. Aborting.\n")
  quit(status = 1)
}

if (all_sourced) {

# =============================================================================
# 1. Primary key helper
# =============================================================================
cat("\n--- supabase_primary_key ---\n")

sh_record("primary_key_manager_dna_is_team_id", {
  stopifnot(identical(supabase_primary_key("manager_dna_profiles"), "team_id"))
})

sh_record("primary_key_players_is_id", {
  stopifnot(identical(supabase_primary_key("players"), "id"))
})

sh_record("primary_key_all_other_tables_are_id", {
  others <- setdiff(supabase_known_tables(), "manager_dna_profiles")
  stopifnot(length(others) == 11L)
  stopifnot(all(vapply(others, function(t) identical(supabase_primary_key(t), "id"), logical(1))))
})

# =============================================================================
# 2. delete-all filters (mocked supabase_delete, no HTTP)
# =============================================================================
cat("\n--- supabase_delete_all filters (mocked) ---\n")

# Capture the original supabase_delete global binding (existence + value)
# before shadowing it, so it can be restored exactly after this suite.
sh_delete_saved <- new.env()
sh_delete_saved$existed <- exists("supabase_delete", envir = globalenv(), inherits = FALSE)
sh_delete_saved$value <- if (sh_delete_saved$existed) get("supabase_delete", envir = globalenv(), inherits = FALSE) else NULL

# Restoration helper: put the saved value back when the binding originally
# existed, otherwise remove the shadowing binding if it is still present.
sh_restore_supabase_delete <- function() {
  if (sh_delete_saved$existed) {
    assign("supabase_delete", sh_delete_saved$value, envir = globalenv())
  } else if (exists("supabase_delete", envir = globalenv(), inherits = FALSE)) {
    rm(list = "supabase_delete", envir = globalenv())
  }
}

captured <- new.env()
captured$table <- NULL
captured$filter <- NULL
# Mock: capture the filter instead of issuing a DELETE request.
supabase_delete <- function(table_name, filter = "id=neq.00000000-0000-0000-0000-000000000000") {
  captured$table <<- table_name
  captured$filter <<- filter
  list(status = "deleted", http_code = 204L)
}

sh_record("delete_all_manager_dna_uses_team_id_not_is_null", {
  res <- supabase_delete_all("manager_dna_profiles")
  stopifnot(res$status == "deleted")
  stopifnot(identical(captured$table, "manager_dna_profiles"))
  stopifnot(identical(captured$filter, "team_id=not.is.null"))
})

sh_record("delete_all_players_uses_id_not_is_null", {
  res <- supabase_delete_all("players")
  stopifnot(res$status == "deleted")
  stopifnot(identical(captured$table, "players"))
  stopifnot(identical(captured$filter, "id=not.is.null"))
})

sh_record("delete_all_unknown_table_rejected_without_delete", {
  captured$table <- NULL
  captured$filter <- NULL
  res <- supabase_delete_all("not_a_real_table")
  stopifnot(res$status == "error", res$reason == "unknown table")
  stopifnot(is.null(captured$table), is.null(captured$filter))
})

# Restore the real supabase_delete before the dynamic mock sections below.
sh_restore_supabase_delete()

# =============================================================================
# 3. Static docs assertions
# =============================================================================
cat("\n--- docs (static) ---\n")

sh_record("schema_md_contains_three_alter_statements", {
  txt <- paste(readLines("docs/database_schema.md", warn = FALSE), collapse = "\n")
  stopifnot(grepl("ALTER TABLE user_teams ADD COLUMN IF NOT EXISTS is_active BOOLEAN DEFAULT TRUE;", txt, fixed = TRUE))
  stopifnot(grepl("ALTER TABLE user_team_history ADD COLUMN IF NOT EXISTS round_number INTEGER;", txt, fixed = TRUE))
  stopifnot(grepl("ALTER TABLE user_team_history ADD COLUMN IF NOT EXISTS active_teams_count INTEGER;", txt, fixed = TRUE))
})

sh_record("schema_md_ddl_contains_new_fields", {
  txt <- paste(readLines("docs/database_schema.md", warn = FALSE), collapse = "\n")
  stopifnot(grepl("is_active BOOLEAN DEFAULT TRUE", txt, fixed = TRUE))
  stopifnot(grepl("round_number INTEGER", txt, fixed = TRUE))
  stopifnot(grepl("active_teams_count INTEGER", txt, fixed = TRUE))
})

sh_record("supabase_migration_sql_deleted", {
  stopifnot(!file.exists("docs/supabase_migration.sql"))
})

# =============================================================================
# 4. Connector init/count queries use supabase_primary_key(tbl)
# =============================================================================
cat("\n--- connector (static parse) ---\n")

sh_record("connector_parses_and_queries_use_primary_key_helper", {
  conn_text <- paste(readLines("supabase_connector.R", warn = FALSE), collapse = "\n")
  parse(text = conn_text)  # must be valid R
  # get_table_row_counts probe
  stopifnot(grepl('query = list(select = supabase_primary_key(tbl), limit = "0")', conn_text, fixed = TRUE))
  # init_supabase_db probe
  stopifnot(grepl('query = list(select = supabase_primary_key(tbl), limit = "1")', conn_text, fixed = TRUE))
})

# =============================================================================
# 5. Dynamic offline mocks: get_table_row_counts() query construction
# =============================================================================
cat("\n--- get_table_row_counts (mocked HTTP, offline) ---\n")

# ---- Offline HTTP mock machinery ----
# The connector resolves unqualified GET / status_code / add_headers through
# globalenv, and calls httr::content qualified. We therefore (a) shadow the
# three unqualified bindings in globalenv and (b) swap httr::content via
# assignInNamespace. The originals are saved first and restored after every
# dynamic test, so no live request can ever be issued. The fake base URL uses
# the reserved .invalid TLD as defense in depth.
sh_http <- new.env()
sh_http$base_url <- "https://offline-mock.supabase.invalid"
sh_http$defaults <- list(code = 200L, body = "[]", content_range = "items 0-0/0")
sh_http$overrides <- list()
sh_http$requests <- list()

sh_http$record <- function(url, query) {
  tbl <- sub(paste0(sh_http$base_url, "/rest/v1/"), "", url, fixed = TRUE)
  sh_http$requests <- c(sh_http$requests, list(list(table = tbl, url = url, query = query)))
  cfg <- sh_http$overrides[[tbl]]
  if (is.null(cfg)) cfg <- sh_http$defaults
  headers <- list()
  if (!is.null(cfg$content_range)) headers[["content-range"]] <- cfg$content_range
  list(url = url, code = cfg$code, headers = headers, body = cfg$body)
}

sh_mock_GET <- function(url, query = list(), ...) {
  sh_http$record(url, query)
}

sh_mock_status_code <- function(response, ...) {
  response$code
}

sh_mock_add_headers <- function(request = NULL, .headers = NULL, ...) {
  if (missing(request)) request <- list(url = NULL, headers = list())
  if (!is.null(.headers)) request$headers <- c(request$headers, .headers)
  request
}

sh_mock_content <- function(x, as = "parsed", encoding = "unknown", ...) {
  if (as == "text") return(x$body)
  jsonlite::fromJSON(x$body)
}

sh_save_env <- function() {
  sh_http$orig_env_url <- Sys.getenv("supabase_project_url", unset = NA)
  sh_http$orig_env_key <- Sys.getenv("supabase_secret_key", unset = NA)
}

sh_set_test_env <- function() {
  Sys.setenv(
    supabase_project_url = sh_http$base_url,
    supabase_secret_key = "offline-test-key"
  )
}

sh_restore_env <- function() {
  if (is.na(sh_http$orig_env_url)) Sys.unsetenv("supabase_project_url")
  else Sys.setenv(supabase_project_url = sh_http$orig_env_url)
  if (is.na(sh_http$orig_env_key)) Sys.unsetenv("supabase_secret_key")
  else Sys.setenv(supabase_secret_key = sh_http$orig_env_key)
}

sh_install_mocks <- function() {
  # Record, separately for each unqualified binding, whether it existed in
  # globalenv and its original value (never assume it is the httr one).
  sh_http$orig_GET_existed <- exists("GET", envir = globalenv(), inherits = FALSE)
  sh_http$orig_GET <- if (sh_http$orig_GET_existed) get("GET", envir = globalenv(), inherits = FALSE) else NULL
  sh_http$orig_status_code_existed <- exists("status_code", envir = globalenv(), inherits = FALSE)
  sh_http$orig_status_code <- if (sh_http$orig_status_code_existed) get("status_code", envir = globalenv(), inherits = FALSE) else NULL
  sh_http$orig_add_headers_existed <- exists("add_headers", envir = globalenv(), inherits = FALSE)
  sh_http$orig_add_headers <- if (sh_http$orig_add_headers_existed) get("add_headers", envir = globalenv(), inherits = FALSE) else NULL
  sh_http$orig_content <- getFromNamespace("content", "httr")
  assign("GET", sh_mock_GET, envir = globalenv())
  assign("status_code", sh_mock_status_code, envir = globalenv())
  assign("add_headers", sh_mock_add_headers, envir = globalenv())
  assignInNamespace("content", sh_mock_content, ns = "httr")
}

sh_restore_mocks <- function() {
  # Restore the saved original value when the binding originally existed;
  # otherwise remove the mock binding if it is still present.
  if (sh_http$orig_GET_existed) {
    assign("GET", sh_http$orig_GET, envir = globalenv())
  } else if (exists("GET", envir = globalenv(), inherits = FALSE)) {
    rm(list = "GET", envir = globalenv())
  }
  if (sh_http$orig_status_code_existed) {
    assign("status_code", sh_http$orig_status_code, envir = globalenv())
  } else if (exists("status_code", envir = globalenv(), inherits = FALSE)) {
    rm(list = "status_code", envir = globalenv())
  }
  if (sh_http$orig_add_headers_existed) {
    assign("add_headers", sh_http$orig_add_headers, envir = globalenv())
  } else if (exists("add_headers", envir = globalenv(), inherits = FALSE)) {
    rm(list = "add_headers", envir = globalenv())
  }
  assignInNamespace("content", sh_http$orig_content, ns = "httr")
}

sh_record("row_counts_records_select_per_table", {
  sh_save_env()
  sh_set_test_env()
  sh_install_mocks()
  sh_http$requests <- list()
  sh_http$overrides <- list(
    manager_dna_profiles = list(code = 200L, body = "[]", content_range = "items 0-0/42")
  )
  tryCatch({
    df <- get_table_row_counts()
    stopifnot(is.data.frame(df), nrow(df) == 12L)
    stopifnot(length(sh_http$requests) == 12L)
    # Every table's select must match its primary key helper.
    for (i in seq_along(sh_http$requests)) {
      req <- sh_http$requests[[i]]
      expected <- if (req$table == "manager_dna_profiles") "team_id" else "id"
      stopifnot(identical(req$query$select, expected))
      stopifnot(identical(req$query$limit, "0"))
    }
    # manager_dna_profiles specifically selects team_id; a normal table id.
    req_tables <- vapply(sh_http$requests, function(r) r$table, character(1))
    dna_req <- sh_http$requests[[which(req_tables == "manager_dna_profiles")]]
    stopifnot(identical(dna_req$query$select, "team_id"))
    players_req <- sh_http$requests[[which(req_tables == "players")]]
    stopifnot(identical(players_req$query$select, "id"))
    # Count is parsed from the content-range header (42 for manager_dna_profiles).
    stopifnot(df$row_count[df$table_name == "manager_dna_profiles"] == 42L)
  }, finally = {
    try(sh_restore_mocks(), silent = TRUE)
    try(sh_restore_env(), silent = TRUE)
  })
})

# =============================================================================
# 6. Dynamic offline mocks: init_supabase_db() 400 handling + warnings
# =============================================================================
cat("\n--- init_supabase_db (mocked HTTP, offline) ---\n")

sh_record("init_returns_false_and_warns_on_manager_dna_400", {
  sh_save_env()
  sh_set_test_env()
  sh_install_mocks()
  sh_http$requests <- list()
  sh_http$overrides <- list(
    manager_dna_profiles = list(
      code = 400L,
      body = '{"message":"column manager_dna_profiles.id does not exist"}',
      content_range = NULL
    )
  )
  warn_env <- new.env()
  warn_env$messages <- character(0)
  tryCatch({
    init_ok <- withCallingHandlers(
      init_supabase_db(verbose = FALSE),
      warning = function(w) {
        warn_env$messages <- c(warn_env$messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    stopifnot(identical(init_ok, FALSE))
    # All 12 tables checked; manager_dna_profiles queried with team_id + limit 1.
    stopifnot(length(sh_http$requests) == 12L)
    req_tables <- vapply(sh_http$requests, function(r) r$table, character(1))
    dna_req <- sh_http$requests[[which(req_tables == "manager_dna_profiles")]]
    stopifnot(identical(dna_req$query$select, "team_id"))
    stopifnot(identical(dna_req$query$limit, "1"))
    # Exactly one warning, carrying both the HTTP 400 and the response-body message.
    stopifnot(length(warn_env$messages) == 1L)
    w <- warn_env$messages[1]
    stopifnot(grepl("HTTP 400", w, fixed = TRUE))
    stopifnot(grepl("column manager_dna_profiles.id does not exist", w, fixed = TRUE))
    stopifnot(grepl("manager_dna_profiles", w, fixed = TRUE))
  }, finally = {
    try(sh_restore_mocks(), silent = TRUE)
    try(sh_restore_env(), silent = TRUE)
  })
})

# =============================================================================
# 6b. Real supabase_delete() query encoding (mocked DELETE, no HTTP)
# =============================================================================
cat("\n--- supabase_delete encoding (mocked DELETE, offline) ---\n")

sh_record("supabase_delete_encodes_named_query_filters", {
  # Save the original DELETE global binding (existence + value) and the
  # httr namespace binding (the connector calls httr::DELETE qualified, so
  # the namespace swap is what actually intercepts, same pattern as the
  # httr::content swap above).
  delete_saved <- new.env()
  delete_saved$env_existed <- exists("DELETE", envir = globalenv(), inherits = FALSE)
  delete_saved$env_value <- if (delete_saved$env_existed) get("DELETE", envir = globalenv(), inherits = FALSE) else NULL
  delete_saved$ns_value <- getFromNamespace("DELETE", "httr")

  captured_delete <- new.env()
  captured_delete$queries <- list()
  mock_DELETE <- function(url, query = list(), ...) {
    captured_delete$queries <<- c(captured_delete$queries, list(query))
    # Fake 204 response consumable by the real httr::status_code (offline).
    structure(list(status_code = 204L, url = url, headers = list(), content = raw(0)), class = "response")
  }
  assignInNamespace("DELETE", mock_DELETE, ns = "httr")
  assign("DELETE", mock_DELETE, envir = globalenv())

  sh_save_env()
  sh_set_test_env()
  tryCatch({
    res_dna <- supabase_delete("manager_dna_profiles", "team_id=not.is.null")
    stopifnot(res_dna$status == "deleted", res_dna$http_code == 204L)
    res_players <- supabase_delete("players", "id=not.is.null")
    stopifnot(res_players$status == "deleted", res_players$http_code == 204L)
    stopifnot(length(captured_delete$queries) == 2L)
    # The filter string must be parsed into an exactly named query list.
    stopifnot(identical(captured_delete$queries[[1]], list(team_id = "not.is.null")))
    stopifnot(identical(captured_delete$queries[[2]], list(id = "not.is.null")))
  }, finally = {
    # Restore the DELETE bindings (namespace + globalenv existence/value).
    assignInNamespace("DELETE", delete_saved$ns_value, ns = "httr")
    if (delete_saved$env_existed) {
      assign("DELETE", delete_saved$env_value, envir = globalenv())
    } else if (exists("DELETE", envir = globalenv(), inherits = FALSE)) {
      rm(list = "DELETE", envir = globalenv())
    }
    try(sh_restore_env(), silent = TRUE)
  })
})

# =============================================================================
# 7. Restoration verification (mocks + env back to their original state)
# =============================================================================
cat("\n--- restoration verification ---\n")

sh_record("mocks_and_env_restored_after_dynamic_tests", {
  # supabase_delete: original binding presence AND value.
  stopifnot(identical(exists("supabase_delete", envir = globalenv(), inherits = FALSE), sh_delete_saved$existed))
  if (sh_delete_saved$existed) {
    stopifnot(identical(get("supabase_delete", envir = globalenv(), inherits = FALSE), sh_delete_saved$value))
  }
  # GET: original binding presence AND value.
  stopifnot(identical(exists("GET", envir = globalenv(), inherits = FALSE), sh_http$orig_GET_existed))
  if (sh_http$orig_GET_existed) {
    stopifnot(identical(get("GET", envir = globalenv(), inherits = FALSE), sh_http$orig_GET))
  }
  # status_code: original binding presence AND value.
  stopifnot(identical(exists("status_code", envir = globalenv(), inherits = FALSE), sh_http$orig_status_code_existed))
  if (sh_http$orig_status_code_existed) {
    stopifnot(identical(get("status_code", envir = globalenv(), inherits = FALSE), sh_http$orig_status_code))
  }
  # add_headers: original binding presence AND value.
  stopifnot(identical(exists("add_headers", envir = globalenv(), inherits = FALSE), sh_http$orig_add_headers_existed))
  if (sh_http$orig_add_headers_existed) {
    stopifnot(identical(get("add_headers", envir = globalenv(), inherits = FALSE), sh_http$orig_add_headers))
  }
  # Namespace content + environment (retained checks).
  stopifnot(identical(getFromNamespace("content", "httr"), sh_http$orig_content))
  stopifnot(identical(Sys.getenv("supabase_project_url", unset = NA), sh_http$orig_env_url))
  stopifnot(identical(Sys.getenv("supabase_secret_key", unset = NA), sh_http$orig_env_key))
})

}  # end if (all_sourced)

# =============================================================================
# SUMMARY
# =============================================================================
total <- length(sh_results)
passed <- sum(sapply(sh_results, function(r) r$status == "pass"))
failed <- total - passed

cat("\n======================================================================\n")
cat(sprintf("  SUPABASE SCHEMA HELPERS TESTS: %d passed / %d failed / %d total\n", passed, failed, total))
cat("======================================================================\n")

if (failed > 0) {
  cat("  FAILED TESTS:\n")
  for (nm in names(sh_results)) {
    if (sh_results[[nm]]$status == "error") {
      cat(sprintf("    - %s: %s\n", nm, sh_results[[nm]]$trace))
    }
  }
  quit(status = 1)
}

quit(status = 0)
