#!/usr/bin/env Rscript
# Independent known-answer checks for pairing player points with match results.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(library(dplyr))
source("data_contracts.R")
source("history_repository.R")

check <- function(label, expr) {
  force(expr)
  cat("[PASS]", label, "\n")
}

empty_matches <- data.frame(
  round=numeric(), match_id=character(), round_id=character(), season=character(),
  competition_id=character(), home=character(), away=character(),
  stringsAsFactors=FALSE
)
points <- function(round, score, match_id="", round_id="") data.frame(
  round_number=as.numeric(round), points=as.numeric(score),
  score_status=rep("provisional", length(round)), match_id=as.character(match_id),
  round_id=as.character(round_id), stringsAsFactors=FALSE
)
matches <- function(round, home, match_id="", round_id="", season="S1", competition_id="C1") data.frame(
  round=as.numeric(round), home=as.character(home), away="Away",
  match_id=as.character(match_id), round_id=as.character(round_id),
  season=as.character(season), competition_id=as.character(competition_id),
  stringsAsFactors=FALSE
)
context <- list(season="S1", competition_id="C1")

check("reversed source order still pairs by round and sorts newest first", {
  got <- build_player_recent_rounds(
    points(c(8,10,9), c(0,-2,5)),
    matches(c(9,8,10), c("Home 9","Home 8","Home 10")), context
  )
  rounds <- got$rounds
  stopifnot(identical(vapply(rounds, `[[`, numeric(1), "round_number"), c(10,9,8)))
  stopifnot(identical(vapply(rounds, `[[`, numeric(1), "points"), c(-2,5,0)))
  stopifnot(identical(vapply(rounds, function(x) x$fixtures$home[1], character(1)),
    c("Home 10","Home 9","Home 8")))
})

check("known season or competition conflicts block even matching IDs", {
  got <- build_player_recent_rounds(points(4, 7, "match-4", "round-4"),
    matches(c(4,4), c("Other season","Other competition"),
      match_id=rep("match-4",2), round_id=rep("round-4",2),
      season=c("S2","S1"), competition_id=c("C1","C2")), context)
  stopifnot(nrow(got$rounds[[1]]$fixtures)==0L, nrow(got$unassociated)==2L)
})

check("unknown metadata does not permit a round-number-only association", {
  got <- build_player_recent_rounds(points(6, 3),
    matches(6, "Unknown context", season="unknown", competition_id=""),
    list(season="unknown", competition_id=""))
  stopifnot(nrow(got$rounds[[1]]$fixtures)==0L, nrow(got$unassociated)==1L)
})

check("verified season and competition permit a round fallback", {
  got <- build_player_recent_rounds(points(5, 8),
    matches(c(5,5,5), c("Current","Old season","Other competition"),
      season=c("S1","S2","S1"), competition_id=c("C1","C1","C2")), context)
  stopifnot(identical(got$rounds[[1]]$fixtures$home, "Current"), nrow(got$unassociated)==2L)
})

check("true zero, negative and unavailable points remain distinct", {
  got <- build_player_recent_rounds(points(c(3,2,1), c(NA_real_,-2,0)), empty_matches, context)
  scores <- vapply(got$rounds, `[[`, numeric(1), "points")
  stopifnot(is.na(scores[1]), identical(scores[2:3], c(-2,0)))
})

check("a multi-fixture round displays its total once for the fixture group", {
  got <- build_player_recent_rounds(points(7, 12),
    matches(c(7,7), c("Home A","Home B")), context)
  stopifnot(length(got$rounds)==1L, got$rounds[[1]]$points==12,
    nrow(got$rounds[[1]]$fixtures)==2L, got$rounds[[1]]$fixtures$home[1]=="Home A",
    got$rounds[[1]]$fixtures$home[2]=="Home B")
})

check("duplicate exact IDs on conflicting fixture rounds remain ambiguous", {
  got <- build_player_recent_rounds(points(8, 6, match_id="duplicate-id"),
    matches(c(8,9), c("Round 8","Conflicting round 9"),
      match_id=rep("duplicate-id",2)), context)
  stopifnot(nrow(got$rounds[[1]]$fixtures)==0L,
    all(c("Round 8","Conflicting round 9") %in% got$unassociated$home))
})

check("identical repeated fixture rows are deduplicated before grouping", {
  one <- matches(8, "Round 8", match_id="same-fixture")
  got <- build_player_recent_rounds(points(8, 6, match_id="same-fixture"),
    rbind(one,one), context)
  stopifnot(nrow(got$rounds[[1]]$fixtures)==1L,
    got$rounds[[1]]$fixtures$home[1]=="Round 8",nrow(got$unassociated)==0L)
})

cat("Independent recent-round association checks passed.\n")
