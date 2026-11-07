#!/usr/bin/env Rscript
# Independent small-fixture oracle for the exact starter/bench optimization.
# Run from weapp2/Futmondo with Rscript test/test_lineup_independent.R.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
source('lineup_runtime.R')

canonical <- function(x) {
  aliases <- c('1'='GK','gk'='GK','goalkeeper'='GK','portero'='GK',
    '2'='DEF','df'='DEF','def'='DEF','defender'='DEF','defensa'='DEF',
    '3'='MID','mf'='MID','md'='MID','mid'='MID','midfielder'='MID','centrocampista'='MID',
    '4'='FWD','fw'='FWD','fwd'='FWD','forward'='FWD','delantero'='FWD')
  parts <- trimws(tolower(unlist(strsplit(as.character(x),'[,;/|]+'))))
  unique(unname(aliases[parts[parts %in% names(aliases)]]))
}
oracle_lineup <- function(roster,scores,formations,include_unavailable=FALSE) {
  if(!include_unavailable)roster<-roster[!analytics_known_unavailable(roster),,drop=FALSE]
  score <- setNames(scores$average,scores$id)
  roles <- lapply(seq_len(nrow(roster)),function(i)
    unique(c(canonical(roster$role[i]),canonical(roster$role2[i]))))
  best <- -Inf
  for(strategy in formations) {
    n <- as.integer(strsplit(strategy,'-',fixed=TRUE)[[1]])
    target <- c(DEF=n[1],MID=n[2],FWD=n[3])
    picks <- lapply(names(target),function(role) {
      choices <- which(vapply(roles,function(value)role %in% value,logical(1)))
      if(length(choices)<target[role])return(list())
      combn(choices,target[role],simplify=FALSE)
    })
    if(any(lengths(picks)==0L))next
    gk <- which(vapply(roles,function(value)'GK' %in% value,logical(1)))
    for(d in picks[[1]])for(m in picks[[2]])for(f in picks[[3]])for(g in gk) {
      chosen <- c(d,m,f,g)
      if(anyDuplicated(chosen))next
      total <- sum(score[roster$id[chosen]])
      if(total>best)best<-total
    }
  }
  best
}
brute_bench <- function(roster,scores,size) {
  role <- lapply(seq_len(nrow(roster)),function(i)
    unique(c(canonical(roster$role[i]),canonical(roster$role2[i]))))
  covered_by_distinct_players <- function(index) {
    assign <- function(role_index,used) {
      if(role_index>3L)return(0L)
      eligible<-index[vapply(index,function(i)c('DEF','MID','FWD')[role_index]%in%role[[i]],logical(1))]
      max(c(assign(role_index+1L,used),if(length(eligible[!eligible%in%used]))
        1L+vapply(eligible[!eligible%in%used],function(i)assign(role_index+1L,c(used,i)),integer(1)) else integer()))
    }
    assign(1L,integer())
  }
  choices <- combn(seq_len(nrow(roster)),size,simplify=FALSE)
  totals <- vapply(choices,function(index)sum(scores$average[match(roster$id[index],scores$id)]),numeric(1))
  covered <- vapply(choices,covered_by_distinct_players,integer(1))
  pick <- order(-covered,-totals)[1]
  list(coverage=covered[pick],total=totals[pick],ids=roster$id[choices[[pick]]])
}

# A small but overlapping position pool makes greedy role filling fail on some
# score assignments; the enumerator independently tries every feasible XI.
roster <- data.frame(id=c('gk','d1','d2','d3','d4','m1','m2','m3','m4','f1','f2','f3','f4','dm','mf','injured-forward'),
  role=c('portero',rep('defensa',4),rep('centrocampista',4),rep('delantero',4),'defensa','centrocampista','delantero'),
  role2=c(NA,rep(NA,8),rep(NA,4),'centrocampista','delantero',NA),
  status=c('ok',rep('ok',14),'injured'),stringsAsFactors=FALSE)
score_values <- c(4,9,2,6,3,8,1,7,5,11,2,10,0,-2,12,100)
scores <- data.frame(id=roster$id,average=score_values,coverage=5L,required=5L,status='complete',stringsAsFactors=FALSE)
formations <- c('4-4-2','4-3-3','3-4-3','3-5-2','4-5-1')
current <- list(strategy='4-4-2',starters=c('f1','f2','m1','m2','m3','m4','d1','d2','d3','d4','gk'),bench=rep('',4),captain='d1')
expected <- oracle_lineup(roster,scores,formations)
actual <- lineup_optimize(roster,scores,formations,current=current,include_unavailable=FALSE)
stopifnot(!is.null(actual),actual$total==expected,length(actual$draft$starters)==11L,
  length(unique(actual$draft$starters))==11L,actual$draft$captain%in%actual$draft$starters,
  !'injured-forward'%in%actual$draft$starters)
for(i in seq_along(actual$draft$starters)) {
  id <- actual$draft$starters[i]
  eligible <- unique(c(canonical(roster$role[match(id,roster$id)]),canonical(roster$role2[match(id,roster$id)])))
  stopifnot(lineup_slot_roles(actual$draft$strategy)[i]%in%eligible)
}
included <- lineup_optimize(roster,scores,formations,current=current,include_unavailable=TRUE)
stopifnot(included$total==oracle_lineup(roster,scores,formations,include_unavailable=TRUE),
  'injured-forward'%in%included$draft$starters)
stopifnot(is.null(lineup_optimize(roster,scores,'not-a-formation',current=current)))
cat('PASS exhaustive independent XI oracle across five formations, multiposition roles, signed/zero scores, injury exclusion/override, uniqueness, captain and invalid formation\n')

# Coverage outranks bench points. Four high-scoring defenders must give way to
# distinct midfield/forward cover when four substitutes can cover all roles.
bench_roster <- data.frame(id=c('d1','d2','d3','d4','d5','m1','m2','f1','f2','dm','mf'),
  role=c(rep('defensa',5),rep('centrocampista',2),rep('delantero',2),'defensa','centrocampista'),
  role2=c(rep(NA,9),'centrocampista','delantero'),stringsAsFactors=FALSE)
bench_scores <- data.frame(id=bench_roster$id,average=c(100,90,80,70,60,2,1,0,-1,3,4),stringsAsFactors=FALSE)
bench_expected <- brute_bench(bench_roster,bench_scores,4L)
bench_actual <- lineup_bench(bench_roster,bench_scores,size=4L)
stopifnot(length(bench_actual$ids)==4L,
  length(unique(bench_actual$ids))==4L,
  length(bench_actual$coverage)==bench_expected$coverage,
  bench_actual$total==bench_expected$total,
  identical(as.numeric(bench_scores$average[match(bench_actual$ids,bench_scores$id)]),
    sort(as.numeric(bench_scores$average[match(bench_actual$ids,bench_scores$id)]),decreasing=TRUE)))
cat('PASS independent exhaustive bench oracle; coverage outranks points and order is best to worst\n')

# One common window for all players; a running round is ignored, a missing score
# stays unknown, and a confirmed zero/negative score is retained.
finished <- data.frame(round_number=1:4,is_finished=c(TRUE,TRUE,FALSE,TRUE))
history <- data.frame(player_id=c('a','a','a','b','b','b'),round=c(1,2,3,1,2,4),
  points=c(0,-5,100,-2,2,4))
early_roster <- data.frame(id=c('a','b'),role=c('DEF','FWD'))
window <- lineup_average_scores(early_roster,history,finished,n=5L)
stopifnot(identical(attr(window,'rounds'),c(1,2,4)),
  is.na(window$average[match('a',window$id)]),window$coverage[match('a',window$id)]==2L,
  window$required[match('a',window$id)]==3L,
  isTRUE(all.equal(window$average[match('b',window$id)],4/3)),
  window$status[match('a',window$id)]=='partial')
empty <- lineup_clear(current)
stopifnot(identical(empty$strategy,current$strategy),all(empty$starters==''),
  all(empty$bench==''),identical(empty$captain,''))
cat('PASS completed-round window preserves missingness and negative/zero scores; Clear empties XI/bench/captain and retains formation\n')

# Warm path performance evidence for the bounded position lattice. The fixed
# roster uses 100 synthetic IDs and only factual, local scores.
large <- data.frame(id=sprintf('p%03d',1:100),role=rep(c('GK','DEF','MID','FWD'),length.out=100),
  role2=NA_character_,status='ok',stringsAsFactors=FALSE)
large$role2[c(7,18,29,40,51,62,73,84,95)] <- c('MID','FWD','DEF','MID','FWD','DEF','MID','FWD','DEF')
large_scores <- data.frame(id=large$id,average=seq_len(nrow(large))/3,stringsAsFactors=FALSE)
invisible(lineup_optimize(large,large_scores,lineup_formations()))
elapsed <- system.time(invisible(lineup_optimize(large,large_scores,lineup_formations())))[['elapsed']]
stopifnot(is.finite(elapsed),elapsed<0.1)
cat(sprintf('PASS warm 100-player/all-formations optimizer: %.4f s\n',elapsed))
