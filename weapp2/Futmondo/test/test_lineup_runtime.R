#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
roster<-data.frame(id=paste0('p',1:20),name=paste('Player',1:20),role=c(rep('portero',2),rep('defensa',6),rep('centrocampista',7),rep('delantero',5)),role2='',status='ok',buyPrice=1:20,value=21:40)
score<-data.frame(id=roster$id,average=1:20)
check('cold loading has no players and never manufactures a target round',{
 stopifnot(nrow(lineup_average_scores(roster[FALSE,],data.frame(),data.frame()))==0L,
  is.null(lineup_target(data.frame(round_id=NA_character_,round_number=1,begin_process=format(Sys.time()+86400,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')))))
})
check('every official formation has eleven role-eligible places',{
 for(formation in lineup_formations()){
  result<-lineup_optimize(roster,score,formation)
  stopifnot(result$draft$strategy==formation,length(result$draft$starters)==11L,!anyDuplicated(c(result$draft$starters,result$draft$bench)),
    !length(lineup_validate(result$draft,roster,list(allowed=formation,multiposition=TRUE,bench_enabled=TRUE,captain_enabled=TRUE))))
 }
})
# Eleven equal-scoring players can form 4-3-3 with the dual player up front,
# or 4-4-2 with that same player in midfield. No selection/point tradeoff exists.
preferred_roster<-data.frame(id=c('g',paste0('d',1:4),paste0('m',1:3),'f1','f2','dual'),
 role=c('portero',rep('defensa',4),rep('centrocampista',3),rep('delantero',3)),
 role2=c(rep('',10),'centrocampista'),status='ok',stringsAsFactors=FALSE)
preferred_scores<-data.frame(id=preferred_roster$id,average=1)
assigned_role<-function(draft,id)lineup_slot_roles(draft$strategy)[match(id,draft$starters)]
check('equal-total optimization changes formation to place a dual MID/FWD in midfield',{
 current<-lineup_optimize(preferred_roster,preferred_scores,'4-3-3')$draft
 got<-lineup_optimize(preferred_roster,preferred_scores,c('4-3-3','4-4-2'),current)
 stopifnot(current$strategy=='4-3-3',assigned_role(current,'dual')=='FWD',
  got$draft$strategy=='4-4-2',assigned_role(got$draft,'dual')=='MID',got$total==11,
  setequal(got$draft$starters,current$starters),!anyDuplicated(got$draft$starters))
 shuffled<-lineup_optimize(preferred_roster[11:1,],preferred_scores[11:1,],c('4-4-2','4-3-3'),current)
 stopifnot(identical(got$draft,shuffled$draft))
})
check('midfield priority is optimized within a formation before retaining current starters',{
 rows<-rbind(preferred_roster,data.frame(id='m4',role='centrocampista',role2='',status='ok'))
 values<-data.frame(id=rows$id,average=1)
 current<-lineup_optimize(rows,values,'4-4-2',multiposition=FALSE)$draft
 stopifnot(assigned_role(current,'dual')=='FWD','m4'%in%current$starters)
 got<-lineup_optimize(rows,values,'4-4-2',current)
 stopifnot(got$total==11,assigned_role(got$draft,'dual')=='MID',
  all(c('f1','f2')%in%got$draft$starters),!identical(got$draft$starters,current$starters))
})
check('higher observed total and legal forward fallback outrank midfield preference',{
 rows<-rbind(preferred_roster,data.frame(id='m4',role='centrocampista',role2='',status='ok'))
 values<-data.frame(id=rows$id,average=10);values$average[values$id=='f2']<-0
 got<-lineup_optimize(rows,values,'4-4-2')
 stopifnot(got$total==110,assigned_role(got$draft,'dual')=='FWD',!'f2'%in%got$draft$starters)
})
check('midfield preference respects multiposition capability and formation entitlement',{
 no_multi<-lineup_optimize(preferred_roster,preferred_scores,c('4-4-2','4-3-3'),multiposition=FALSE)
 locked<-lineup_optimize(preferred_roster,preferred_scores,'4-3-3')
 stopifnot(no_multi$draft$strategy=='4-3-3',assigned_role(no_multi$draft,'dual')=='FWD',
  locked$draft$strategy=='4-3-3',assigned_role(locked$draft,'dual')=='FWD',
  is.null(lineup_optimize(preferred_roster,preferred_scores,'4-4-2',multiposition=FALSE)))
})
check('bench covers all three outfield roles before maximizing average and sorts best first',{
 result<-lineup_optimize(roster,score,'4-4-2')
 stopifnot(setequal(result$coverage,c('DEF','MID','FWD')),!any(result$draft$bench%in%roster$id[roster$role=='portero']),
   all(diff(score$average[match(result$draft$bench,score$id)])<=0))
 sparse<-roster[roster$role!='delantero',];b<-lineup_bench(sparse,score,size=4L)
 stopifnot(setequal(b$coverage,c('DEF','MID')),length(b$ids)==4L)
 one<-data.frame(id='multi',role='defensa',role2='delantero')
 stopifnot(length(lineup_bench_coverage(one,'multi'))==1L)
})
coverage_roster<-data.frame(id=c('g',paste0('d',1:5),paste0('m',1:5),paste0('f',1:3)),
 role=c('GK',rep('DEF',5),rep('MID',5),rep('FWD',3)),role2='',status='ok')
coverage_scores<-data.frame(id=coverage_roster$id,average=c(0,rep(10,4),1,rep(10,4),1,20,19,18))
check('required bench coverage changes the optimal formation and leaves three distinct outfield substitutes',{
 allowed<-c('4-3-3','4-4-2')
 ordinary<-lineup_optimize(coverage_roster,coverage_scores,allowed)
 explicit_off<-lineup_optimize(coverage_roster,coverage_scores,allowed,require_bench_coverage=FALSE)
 covered<-lineup_optimize(coverage_roster,coverage_scores,allowed,require_bench_coverage=TRUE)
 stopifnot(identical(ordinary,explicit_off),ordinary$draft$strategy=='4-3-3',ordinary$total==127,
  !'FWD'%in%ordinary$coverage,covered$draft$strategy=='4-4-2',covered$total==119,
  setequal(covered$coverage,c('DEF','MID','FWD')),sum(nzchar(covered$draft$bench))==3L,
  !anyDuplicated(c(covered$draft$starters,covered$draft$bench)),
  !any(covered$draft$bench%in%'g'),sum(coverage_scores$average[match(covered$draft$starters,coverage_scores$id)])==119)
 stopifnot(is.null(lineup_optimize(coverage_roster,coverage_scores,'4-3-3',require_bench_coverage=TRUE)),
  is.null(lineup_optimize(coverage_roster,coverage_scores,allowed,bench_enabled=FALSE,require_bench_coverage=TRUE)))
})
check('required coverage respects unavailable players, departures, missing scores and multiposition permissions',{
 rows<-coverage_roster;rows$status[rows$id=='f3']<-'injured'
 stopifnot(is.null(lineup_optimize(rows,coverage_scores,'4-4-2',require_bench_coverage=TRUE)),
  is.null(lineup_optimize(rows,coverage_scores,'4-4-2',include_doubtful=TRUE,require_bench_coverage=TRUE)),
  is.null(lineup_optimize(rows,coverage_scores,'4-4-2',include_doubtful=TRUE,excluded_ids='f3',require_bench_coverage=TRUE)))
 rows$status[rows$id=='f3']<-'doubt'
 stopifnot(is.null(lineup_optimize(rows,coverage_scores,'4-4-2',require_bench_coverage=TRUE)),
  !is.null(lineup_optimize(rows,coverage_scores,'4-4-2',include_doubtful=TRUE,require_bench_coverage=TRUE)),
  is.null(lineup_optimize(rows,coverage_scores,'4-4-2',include_doubtful=TRUE,excluded_ids='f3',require_bench_coverage=TRUE)))
 missing<-coverage_scores;missing$average[missing$id=='f3']<-NA_real_
 stopifnot(is.null(lineup_optimize(coverage_roster,missing,'4-4-2',require_bench_coverage=TRUE)))
 rows<-coverage_roster;rows$role[rows$id=='m5']<-'DEF';rows$role2[rows$id=='m5']<-'MID'
 stopifnot(!is.null(lineup_optimize(rows,coverage_scores,'4-4-2',require_bench_coverage=TRUE)),
  is.null(lineup_optimize(rows,coverage_scores,'4-4-2',multiposition=FALSE,require_bench_coverage=TRUE)))
 # Even a substitute eligible for every role cannot replace three players.
 rows<-coverage_roster[!coverage_roster$id%in%c('d5','m5'),];rows$role2[rows$id=='f3']<-'DEF/MID'
 stopifnot(is.null(lineup_optimize(rows,coverage_scores,'4-4-2',require_bench_coverage=TRUE)))
})
check('completed-round arithmetic uses the shared exact window, including zero and negative points',{
 rounds<-data.frame(round_number=1:7,is_finished=c(rep(TRUE,6),FALSE))
 history<-data.frame(player_id=rep('p1',7),round=1:7,points=c(100,2,0,-3,4,7,999))
 got<-lineup_average_scores(roster[1:2,],history,rounds,5L)
 stopifnot(got$average[1]==2,got$coverage[1]==5L,is.na(got$average[2]),identical(attr(got,'rounds'),as.numeric(2:6)))
 history$points[3]<-NA_real_;stopifnot(is.na(lineup_average_scores(roster,history,rounds,5L)$average[1]))
 early<-lineup_average_scores(roster,history,rounds[1:2,],5L);stopifnot(early$required[1]==2L,early$average[1]==51)
})
check('clear preserves formation and makes the lineup incomplete without mutation',{
 result<-lineup_optimize(roster,score,'4-4-2')$draft;result$captain<-result$starters[1]
 empty<-lineup_clear(result)
 stopifnot(empty$strategy==result$strategy,all(empty$starters==''),all(empty$bench==''),empty$captain=='',
   length(lineup_validate(empty,roster,list(allowed=lineup_formations(),multiposition=TRUE,bench_enabled=TRUE,captain_enabled=TRUE)))>0)
})
check('official cpt, buyPrice/value and locked fields survive normalized parsing',{
 raw<-list(strategy='4-4-2',multiposition=TRUE,players=list(list(id='p1',position=10,role='portero',cpt=TRUE,buyPrice=4,value=8,locked=TRUE)),bench=list(enabled=TRUE,players=list()))
 parsed<-lineup_parse_official(raw)
 stopifnot(parsed$draft$captain=='p1',parsed$wire$players[[1]]$pr==4,isTRUE(parsed$locked[[1]]$bench==FALSE))
 snapshot<-list(official=parsed$draft,source=list(locked=parsed$locked))
 stopifnot(!length(lineup_locked_changes(snapshot,parsed$draft)),identical(lineup_locked_changes(snapshot,lineup_clear(parsed$draft)),'p1'))
})
check('batch events remove before placing swaps, retain captain changes and never put roles into price',{
 draft<-lineup_optimize(roster,score,'4-4-2')$draft;rules<-list(multiposition=TRUE,captain_enabled=TRUE)
 before<-lineup_modified_wire(draft,roster,rules);changed<-draft;changed$starters[1:2]<-rev(changed$starters[1:2]);changed$captain<-changed$starters[2]
 after<-lineup_modified_wire(changed,roster,rules);events<-lineup_changes(before,after)
 stopifnot(length(events)>=4L,!is.null(events[[1]]$from),is.null(events[[1]]$to),!is.null(events[[3]]$to),
   all(vapply(after$players,function(player)is.numeric(player$pr),logical(1))))
 auth<-c(userid='fixture',token='not-a-real-token');payload<-lineup_build_batch(auth,'league','team',before,after,events)
 stopifnot(identical(payload$query$current,before),identical(payload$query$modified,after),length(payload$query$changes)==length(events))
})
check('formation entitlement never purchases a paid formation',{
 available<-lineup_available_formations(list(strategies=list(list(st='4-4-2',pro=FALSE),list(st='3-6-1',pro=TRUE),list(st='4-6-0',pro=TRUE)),bought=list(list(st='4-6-0',fs=FALSE))),'4-4-2')
 stopifnot(setequal(available,c('4-4-2','4-6-0')))
})
check('injury, suspension and unavailability exclusions cannot be overridden for starters or bench',{
 for(status in c('injured','injured2','redcard','suspended','unavailable'))for(include in c(FALSE,TRUE)){
  rows<-roster;rows$status[rows$id%in%c('p20','p4')]<-status
  rows$available<-rows$id!='p19'
  got<-lineup_optimize(rows,score,'4-4-2',include_doubtful=include,require_bench_coverage=TRUE)
  stopifnot(!is.null(got),!any(c('p20','p19','p4')%in%c(got$draft$starters,got$draft$bench)))
 }
})
check('doubtful players are optional for starters and bench, with explicit exclusions preserved',{
 rows<-roster;rows$status[rows$id=='p20']<-' DOUBT ';rows$status[rows$id=='p4']<-'doubtful'
 for(coverage in c(FALSE,TRUE)){
  excluded<-lineup_optimize(rows,score,'4-4-2',require_bench_coverage=coverage)
  included<-lineup_optimize(rows,score,'4-4-2',include_doubtful=TRUE,require_bench_coverage=coverage)
  stopifnot(!any(c('p20','p4')%in%c(excluded$draft$starters,excluded$draft$bench)),
   'p20'%in%included$draft$starters,'p4'%in%included$draft$bench)
 }
 departed<-lineup_optimize(rows,score,'4-4-2',include_doubtful=TRUE,excluded_ids=c('p20','p4'))
 stopifnot(!any(c('p20','p4')%in%c(departed$draft$starters,departed$draft$bench)))
})
check('departure exclusion always applies to XI and bench, including when doubtful players are enabled',{
 departed<-c('p20','p15','p8');rows<-roster;rows$status[20]<-'injured'
 got<-lineup_optimize(rows,score,lineup_formations(),include_doubtful=TRUE,excluded_ids=departed)
 stopifnot(!any(c(got$draft$starters,got$draft$bench)%in%departed),
  'p19'%in%got$draft$starters,all(c(got$draft$starters,got$draft$bench)%in%rows$id))
})
large<-roster[rep(seq_len(nrow(roster)),5),];large$id<-paste0('p',seq_len(nrow(large)))
large_scores<-data.frame(id=large$id,average=seq_len(nrow(large)))
time<-system.time(for(i in 1:20)lineup_optimize(large,large_scores,lineup_formations()))[['elapsed']]/20
cat(sprintf('PERFORMANCE 100-player/all-formation optimizer + bench: %.1f ms mean (20 runs)\n',1000*time))
stopifnot(time<.1)
coverage_time<-system.time(for(i in 1:20)lineup_optimize(large,large_scores,lineup_formations(),require_bench_coverage=TRUE))[['elapsed']]/20
cat(sprintf('PERFORMANCE required coverage, 100 players/all formations: %.1f ms mean (20 runs)\n',1000*coverage_time))
stopifnot(coverage_time<.2)
cat('LINEUP RUNTIME:',checks,'groups passed / 0 failed\n')
