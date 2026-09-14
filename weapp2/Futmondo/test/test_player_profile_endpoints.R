#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({library(shiny); library(jsonlite); library(dplyr)})
source('futmondo_functions.R')
source('Modules/Selected_Player_Module.R')
checks <- 0L
check <- function(name, expr) { force(expr); checks <<- checks + 1L; cat('[PASS]', name, '\n') }

login <- c(token='token-a', userid='user-a')
profile_answer <- list(
  profile=list(dob='2000-01-02T00:00:00.000Z', nationality=list(main='Spain'), height=180, weight=75, foot='Right', number=9),
  prices=list(list(date='2026-09-01T00:00:00.000Z', social=1000000, classic=900000))
)
matches_answer <- list(matches=list(
  list(r=4, info=list(date='2026-09-01T16:00:00.000Z'), h=list(name='Home',score=2), a=list(name='Away',score=1), st='F'),
  list(r=3, info=list(date='2026-08-25T16:00:00.000Z'), h=list(name='Other',score=0), a=list(name='Home',score=1), st='F')
))
original_post <- futmondo_post
requests <- list()
futmondo_post <- function(url, body, ...) {
  requests[[length(requests)+1L]] <<- list(url=url, body=fromJSON(body, simplifyVector=FALSE))
  payload <- if (identical(url, PLAYER_FULL_PROFILE_URL)) list(answer=profile_answer) else list(answer=matches_answer)
  structure(list(status_code=200L, headers=list(`content-type`='application/json'), all_headers=list(), cookies=data.frame(), content=charToRaw(toJSON(payload, auto_unbox=TRUE)), url=url), class='response')
}

check('profile connector sends a player-only authenticated request and caches it', {
  clear_api_cache(); requests <<- list()
  one <- get_player_full_profile(login, 'player-1')
  two <- get_player_full_profile(login, 'player-1')
  stopifnot(identical(one, two), length(requests)==1L, identical(requests[[1]]$url, PLAYER_FULL_PROFILE_URL),
    identical(requests[[1]]$body$query$playerId, 'player-1'), identical(requests[[1]]$body$header$userid, 'user-a'))
})
check('matches connector has its own cached endpoint and player-only request', {
  clear_api_cache(); requests <<- list()
  out <- get_player_matches(login, 'player-1')
  stopifnot(length(requests)==1L, identical(requests[[1]]$url, PLAYER_MATCHES_URL),
    identical(requests[[1]]$body$query$playerId, 'player-1'), is.list(out), length(out$matches)==2L)
})
check('profile and match payloads normalize to stable card fields', {
  profile <- normalize_player_full_profile(profile_answer)
  matches <- normalize_player_matches(matches_answer)
  stopifnot(identical(profile$profile$nationality, 'Spain'), profile$profile$height_cm==180,
    nrow(profile$price_history)==1L, profile$price_history$social[1]==1000000,
    identical(matches$round, c(4,3)), identical(matches$home, c('Home','Other')), identical(matches$away_score, c(1,1)))
})
check('supplementary scoring modes cannot replace league summary points', {
  summary <- list(match=list(r=list(number=3)), points=list(list(round=1,points=4),list(round=2,points=8)))
  profile <- list(points=list(list(rn=1, po=list(list(mode='press',p=99)))))
  trace <- player_summary_points_trace(summary, NULL)
  stopifnot(identical(trace$points_df$points, c(4,8)), nrow(normalize_player_full_profile(profile)$price_history) == 0L)
})
check('player-card UI registers independent profile and match output', {
  source_text <- paste(readLines('Modules/Selected_Player_Module.R', warn=FALSE), collapse='\n')
  stopifnot(grepl('player_external_details', source_text, fixed=TRUE), grepl('Player profile & matches', source_text, fixed=TRUE))
})
futmondo_post <- original_post
cat(sprintf('Player profile endpoints: %d checks passed.\n', checks))
