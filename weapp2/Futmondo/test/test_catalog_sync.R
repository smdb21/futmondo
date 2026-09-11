#!/usr/bin/env Rscript
# Offline regression: capture payloads and enforce relevant catalog constraints.
options(futmondo.offline = TRUE)
suppressPackageStartupMessages(source('supabase_connector.R'))
checks <- 0L
check <- function(name, code) {
  force(code); checks <<- checks + 1L
  cat('[PASS]', name, '\n')
}
writes <- list()
club_ids <- character()
supabase_post <- function(table_name, payload, ...) {
  if (table_name == 'real_clubs') {
    stopifnot(!anyNA(payload$id), !anyNA(payload$name), all(nzchar(payload$id)),
              all(nzchar(payload$name)), !anyDuplicated(payload$id))
    club_ids <<- union(club_ids, payload$id)
  }
  if (table_name == 'players') {
    references <- payload$real_club_id[!is.na(payload$real_club_id)]
    stopifnot(all(references %in% club_ids))
  }
  writes[[length(writes) + 1L]] <<- list(table = table_name, rows = payload)
  204L
}

check('an unmatched or unassigned player cannot reject the club batch', {
  players <- data.frame(id = paste0('p', 1:5), name = paste('Player', 1:5),
    slug = paste0('player-', 1:5), teamId = c(' club-1 ', 'unmapped-club', NA, '', '   '),
    team = c('Club One', NA, NA, NA, NA), logo = c('club.png', NA, NA, NA, NA))
  stopifnot(sync_real_clubs_to_supabase(players) == 204L)
  clubs <- tail(writes, 1)[[1]]$rows
  stopifnot(nrow(clubs) == 2L, setequal(clubs$id, c('club-1', 'unmapped-club')),
            clubs$name[clubs$id == 'unmapped-club'] == 'unmapped-club')
  sync_players_to_supabase(players)
  saved <- tail(writes, 1)[[1]]$rows
  stopifnot(identical(saved$real_club_id[1:2], c('club-1', 'unmapped-club')),
            all(is.na(saved$real_club_id[3:5])))
  json <- jsonlite::fromJSON(jsonlite::toJSON(saved, na = 'null'))
  stopifnot(all(is.na(json$real_club_id[3:5])))
})
check('missing join columns are optional and preserve legitimate references', {
  players <- data.frame(id = 'p6', name = 'Player Six', slug = 'player-6', teamId = 'club-6')
  sync_real_clubs_to_supabase(players)
  clubs <- tail(writes, 1)[[1]]$rows
  stopifnot(clubs$id == 'club-6', clubs$name == 'club-6', is.na(clubs$logo))
  sync_players_to_supabase(players)
  stopifnot(tail(writes, 1)[[1]]$rows$real_club_id == 'club-6')
})
check('known names win over incomplete duplicate club enrichment', {
  data <- data.frame(teamId = c('club-7', ' club-7 ', 'club-8'),
    team = c(NA, 'Club Seven', '  Club Eight  '))
  sync_real_clubs_to_supabase(data)
  clubs <- tail(writes, 1)[[1]]$rows
  stopifnot(nrow(clubs) == 2L, clubs$name[clubs$id == 'club-7'] == 'Club Seven',
            clubs$name[clubs$id == 'club-8'] == 'Club Eight', all(is.na(clubs$logo)))
})
check('absent club IDs skip club writes and persist unassigned players as null', {
  before <- length(writes)
  stopifnot(isTRUE(sync_real_clubs_to_supabase(data.frame(teamId = c(NA, '', ' ')))),
            isTRUE(sync_real_clubs_to_supabase(data.frame(id = 'p9'))),
            isTRUE(sync_real_clubs_to_supabase(NULL)))
  stopifnot(length(writes) == before)
  sync_players_to_supabase(data.frame(id = 'p9', name = 'Player Nine', slug = 'player-9'))
  stopifnot(is.na(tail(writes, 1)[[1]]$rows$real_club_id))
})
check('unexpected club write failures are recorded without escaping the worker', {
  old <- supabase_post
  supabase_post <- function(...) stop('synthetic failure')
  options(futmondo.persistence_failures = 0L, futmondo.persistence_issues = list())
  stopifnot(identical(sync_real_clubs_to_supabase(data.frame(teamId = 'club-10')), FALSE),
            getOption('futmondo.persistence_failures') == 1L,
            getOption('futmondo.persistence_issues')[[1]]$table == 'real_clubs')
  supabase_post <- old
})
cat(sprintf('Catalog sync: %d checks passed.\n', checks))
