#!/usr/bin/env Rscript
# Only synthetic configuration and promises; no database or source API requests.
source("app_access.R")
db_url <- "https://database.invalid"
db_key <- "synthetic-server-key"
get_sb_url <- function() db_url
get_sb_key <- function() db_key
requests <- list()
response <- '[{"admin_only":false}]'
failure <- NULL
resolve_response <- NULL
delayed <- FALSE
cancelled <- character()
fm_transport_cancel <- function(owner) {cancelled <<- c(cancelled,owner); invisible(TRUE)}
fm_async_http <- function(url, method="GET", body=NULL, headers=character(), owner="", timeout=15, ...) {
  requests[[length(requests)+1L]] <<- list(url=url, method=method, body=body,
    headers=headers, owner=owner, timeout=timeout)
  if (!is.null(failure)) return(promises::promise_reject(failure))
  if (delayed) return(promises::promise(function(resolve, reject) resolve_response <<- resolve))
  promises::promise_resolve(list(content=charToRaw(response), status_code=200L))
}
await <- function(p) {
  done <- FALSE; value <- NULL; error <- NULL
  promises::then(p, function(x) {value <<- x; done <<- TRUE; NULL},
    function(e) {error <<- e; done <<- TRUE; NULL})
  deadline <- Sys.time()+2
  while (!done && Sys.time()<deadline) later::run_now(.01)
  stopifnot(done)
  if (!is.null(error)) stop(error)
  value
}
rejects <- function(p) inherits(tryCatch(await(p), error=identity), "error")
n <- 0L
check <- function(name, expr) {force(expr); n <<- n+1L; cat("PASS", name, "\n")}

check("healthy open and restricted policies both satisfy connectivity", {
  for (body in c('[{"admin_only":false}]', '[{"admin_only":true}]')) {
    response <- body
    stopifnot(identical(await(fm_database_probe("startup-fixture")), TRUE))
  }
  stopifnot(length(requests)==2L)
  for (request in requests) {
    parsed <- httr::parse_url(request$url)
    stopifnot(identical(request$method,"GET"), is.null(request$body),
      identical(parsed$path,"rest/v1/application_access"),
      identical(parsed$query,list(id="eq.main",select="admin_only",limit="1")),
      identical(request$headers[["apikey"]],db_key),
      identical(request$headers[["Authorization"]],paste("Bearer",db_key)),
      identical(request$owner,"startup-fixture"), identical(request$timeout,8))
  }
})
check("missing configuration fails without issuing a request", {
  before <- length(requests)
  db_url <- ""; stopifnot(rejects(fm_database_probe()))
  db_url <- "https://database.invalid"; db_key <- ""
  stopifnot(rejects(fm_database_probe()),length(requests)==before)
  db_key <- "synthetic-server-key"
})
check("missing rows and malformed payloads never report a healthy database", {
  for (body in c("[]","null","{}","true","not JSON",
      '[{"admin_only":"false"}]','[{"admin_only":null}]',
      '[{"admin_only":0}]','[{"admin_only":[false,true]}]',
      '[{"admin_only":false},{"admin_only":true}]',
      '{"wrong_shape":{"admin_only":false}}')) {
    response <- body
    stopifnot(rejects(fm_database_probe()))
  }
})
check("HTTP, authorization, network and deadline failures remain failures", {
  for (category in c("HTTP 401","HTTP 403","HTTP 404","HTTP 500","network","timeout")) {
    failure <- simpleError(category)
    stopifnot(rejects(fm_database_probe()))
  }
  failure <- NULL
})
check("delayed database reads return immediately and preserve the event loop", {
  delayed <- TRUE
  heartbeat <- FALSE
  later::later(function() heartbeat <<- TRUE,.001)
  started <- Sys.time()
  probe <- fm_database_probe()
  elapsed <- as.numeric(difftime(Sys.time(),started,units="secs"))
  deadline <- Sys.time()+.25
  while (!heartbeat && Sys.time()<deadline) later::run_now(.005)
  stopifnot(elapsed<.1,heartbeat,is.function(resolve_response))
  resolve_response(list(content=charToRaw('[{"admin_only":false}]'),status_code=200L))
  stopifnot(identical(await(probe),TRUE))
  delayed <- FALSE
  cat(sprintf("MEASURE probe_dispatch_seconds=%.4f heartbeat_before_response=TRUE\n",elapsed))
})
check("overall deadline cancels only the probe and ignores late completion", {
  delayed <- TRUE
  before <- length(cancelled)
  started <- Sys.time()
  probe <- fm_database_probe("database:deadline-fixture",timeout=.02)
  completed <- 0L
  promises::then(probe,function(value) {completed <<- completed+1L; NULL},
    function(error) {completed <<- completed+1L; NULL})
  stopifnot(rejects(probe),length(cancelled)==before+1L,
    identical(tail(cancelled,1),"database:deadline-fixture"),
    as.numeric(difftime(Sys.time(),started,units="secs"))<.5)
  resolve_response(list(content=charToRaw('[{"admin_only":false}]'),status_code=200L))
  later::run_now(.01)
  stopifnot(completed==1L)
  delayed <- FALSE
})
check("successful completion cancels the deadline timer", {
  response <- '[{"admin_only":false}]'
  before <- length(cancelled)
  stopifnot(identical(await(fm_database_probe("database:quick-fixture",timeout=.03)),TRUE))
  deadline <- Sys.time()+.06
  while (Sys.time()<deadline) later::run_now(.01)
  stopifnot(length(cancelled)==before)
})
cat("DATABASE PROBE:",n,"passed / 0 failed\n")
