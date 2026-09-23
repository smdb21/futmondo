# Offline transport fixtures: no credentials or real network calls.
suppressPackageStartupMessages(source('supabase_connector.R'))
options(futmondo.offline = FALSE, futmondo.in_session_persistence = TRUE)
get_sb_url <- function() 'https://fixture.invalid'
get_sb_key <- function() 'fixture'
catalog <- data.frame(id='existing', name='Verified Name', slug='verified-slug')
calls <- character()
parent_status <- 201L
POST <- function(url, query, body, ...) {
  table <- tail(strsplit(url, '/', fixed=TRUE)[[1]], 1)
  calls <<- c(calls, table)
  rows <- jsonlite::fromJSON(body)
  configs <- list(...)
  headers <- unlist(lapply(configs, function(x) x$headers))
  if (table == 'players') {
    stopifnot(query$on_conflict == 'id',
      any(headers == 'resolution=ignore-duplicates'), !anyDuplicated(rows$id))
    if (parent_status >= 200L && parent_status < 300L) {
      catalog <<- rbind(catalog, rows[!rows$id %in% catalog$id,,drop=FALSE])
    }
    status <- parent_status
  } else {
    stopifnot(table == 'market_transactions', all(rows$player_id %in% catalog$id),
      query$on_conflict == 'championship_id,source_event_id',
      any(headers == 'resolution=merge-duplicates'), all(is.na(rows$seller_team_id)))
    status <- 201L
  }
  structure(list(status_code=status, headers=list('content-type'='application/json'),
    content=charToRaw('{"code":"23503"}')), class='response')
}
feed <- data.frame(id=c('event1','event2','event3'),
  player_id=c('existing','5b6e2823a2736a321a4d1003','5b6e2823a2736a321a4d1003'),
  player_name=c('Older Name',NA,'Historical Player'), buyer_team_id='team', seller_team_id='',
  price=100, created=c('2026-09-01','2026-09-02','2026-09-03'))
sync_pressroom_transactions_to_supabase(feed,'league')
stopifnot(identical(calls,c('players','market_transactions')), nrow(catalog)==2L,
  catalog$name[1]=='Verified Name', catalog$slug[1]=='verified-slug',
  catalog$name[2]=='Historical Player', catalog$slug[2]=='')
sync_pressroom_transactions_to_supabase(feed,'league')
stopifnot(nrow(catalog)==2L)
cat('[PASS] Missing historical players are inserted before transfers; existing metadata and repeat ingestion are preserved.\n')
for (status in c(409L,503L)) {
  parent_status <- status; calls <- character()
  stopifnot(identical(sync_pressroom_transactions_to_supabase(feed,'league'),FALSE),
    identical(calls,'players'))
}
fixture_post <- POST
POST <- function(...) stop('simulated transport timeout')
stopifnot(identical(sync_pressroom_transactions_to_supabase(feed,'league'),FALSE))
cat('[PASS] Rejected and unconfirmed parent writes block dependent transfers.\n')
calls <- character(); feed$player_id[1] <- ''
stopifnot(identical(sync_pressroom_transactions_to_supabase(feed,'league'),FALSE),length(calls)==0L)
cat('[PASS] Missing player identities are rejected without fabricating IDs.\n')
POST <- fixture_post; parent_status <- 201L
feed$player_id[1] <- 'existing'
options(futmondo.in_session_persistence = FALSE)
queued <- NULL
defer_persistence <- function(name, args) { queued <<- list(name=name,args=args); TRUE }
session <- shiny::MockShinySession$new()
shiny::withReactiveDomain(session, {
  stopifnot(isTRUE(sync_pressroom_transactions_to_supabase(feed,'league')), length(calls)==0L)
  do.call(queued$name, queued$args)
  stopifnot(identical(calls,c('players','market_transactions')),
    identical(getOption('futmondo.in_session_persistence'),FALSE))
})
session$close()
cat('[PASS] Interactive calls queue one ordered operation and restore the execution context.\n')
