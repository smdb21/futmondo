#!/usr/bin/env Rscript
# Focused offline regression for the roster-clause request captured in
# app.futmondo.com.clause_exercise.har. Captured credentials are never used.
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source("futmondo_functions.R"))

har_path <- "app.futmondo.com.clause_exercise.har"
if (!file.exists(har_path)) stop("Clause exercise test requires app.futmondo.com.clause_exercise.har")

entries <- jsonlite::fromJSON(har_path, simplifyVector = FALSE)$log$entries
matches <- Filter(function(entry) {
  identical(entry$request$method, "POST") &&
    identical(entry$request$url, ROSTER_CLAUSE_URL)
}, entries)
stopifnot(length(matches) == 1L)
capture <- matches[[1L]]
captured_request <- jsonlite::fromJSON(capture$request$postData$text, simplifyVector = FALSE)
captured_response <- jsonlite::fromJSON(capture$response$content$text, simplifyVector = FALSE)

login <- c(token = "synthetic-token", userid = "synthetic-user")
query <- captured_request$query
payload <- build_roster_clause_payload(login, query$championshipId, query$userteamId,
                                       query$player_id, query$player_slug, query$price)

# Original failure: an unnamed empty R list becomes a JSON array.
old_json <- jsonlite::toJSON(list(answer = list()), auto_unbox = TRUE)
stopifnot(grepl('"answer":\\[\\]', old_json))

# Compare the wire representation with the successful HAR after replacing auth.
expected <- captured_request
expected$header <- list(token = login[["token"]], userid = login[["userid"]])
actual_json <- jsonlite::toJSON(payload, auto_unbox = TRUE)
expected_json <- jsonlite::toJSON(expected, auto_unbox = TRUE)
stopifnot(
  identical(actual_json, expected_json),
  grepl('"answer":\\{\\}', actual_json),
  !("isClause" %in% names(payload$query)),
  identical(names(payload$query),
            c("championshipId", "userteamId", "player_slug", "player_id", "price"))
)

# Stub the success code observed in the HAR; never replay captured credentials.
stopifnot(identical(captured_response$answer$code, API_CODE_OK))
original_post <- futmondo_post
futmondo_post <- function(url, ...) {
  stopifnot(identical(url, ROSTER_CLAUSE_URL))
  body <- jsonlite::toJSON(list(answer = list(code = captured_response$answer$code)),
                           auto_unbox = TRUE)
  structure(list(status_code = 200L,
                 headers = list("Content-Type" = "application/json; charset=utf-8"),
                 content = charToRaw(body), url = url), class = "response")
}
success <- buy_roster_clause(login, query$championshipId, query$userteamId,
                             query$player_id, query$player_slug, query$price)
stopifnot(isTRUE(success$success), identical(success$code, API_CODE_OK))

# Transport failures remain contained by the adapter's defensive tryCatch.
futmondo_post <- function(...) stop("synthetic transport failure")
failure <- buy_roster_clause(login, query$championshipId, query$userteamId,
                             query$player_id, query$player_slug, query$price)
stopifnot(isFALSE(failure$success), identical(failure$code, "error"),
          identical(failure$message, "synthetic transport failure"))
futmondo_post <- original_post

cat("CLAUSE EXERCISE: 5 passed / 0 failed\n")
