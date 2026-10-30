# Production pager + union reader, with deterministic capped REST pages.
load_assignment<-function(path,name,env){xs<-Filter(function(x)is.call(x)&&identical(x[[1]],as.name('<-'))&&identical(x[[2]],as.name(name)),as.list(parse(path)));eval(tail(xs,1)[[1]],env)}
e<-new.env(parent=globalenv())
for(name in c('%||%','fm_scalar'))load_assignment('data_contracts.R',name,e)
load_assignment('history_repository.R','history_read_pages',e)
load_assignment('supabase_connector.R','read_league_round_dataset',e)
points<-data.frame(row_kind='point',championship_id='league',player_id=rep(paste0('p',1:3),each=500),season='unknown',scoring_version='A',round=rep(1:500,3),points=rep(c(-2,0,NA_real_,8,5),300),checked_at=NA_character_,result_status=NA_character_,point_count=NA_integer_)
checks<-points[1:4,];checks$row_kind<-'check';checks$player_id<-paste0('p',1:4);checks$round<-NA_integer_;checks$points<-NA_real_;checks$checked_at<-'2026-10-30T12:00:00Z';checks$result_status<-c('scores','scores','scores','empty');checks$point_count<-c(500L,500L,500L,0L)
fixture<-rbind(points,checks);fixture<-fixture[order(fixture$player_id,fixture$season,fixture$scoring_version,fixture$row_kind,fixture$round),];offsets<-integer()
e$supabase_get<-function(table_name,query_params){
 stopifnot(table_name=='player_round_dataset',query_params$championship_id=='eq.league',is.null(query_params$player_id),is.null(query_params$season),is.null(query_params$scoring_version))
 offset<-as.integer(query_params$offset);offsets<<-c(offsets,offset);limit<-min(700L,query_params$limit)
 if(offset>=nrow(fixture))return(fixture[FALSE,])
 fixture[seq.int(offset+1L,min(offset+limit,nrow(fixture))),]
}
x<-e$read_league_round_dataset('league')
stopifnot(nrow(x$points)==1500L,nrow(x$checks)==4L,isTRUE(x$complete),identical(offsets,c(0L,700L,1400L,1504L)),anyNA(x$points$points),any(x$points$points==0,na.rm=TRUE),any(x$points$points<0,na.rm=TRUE),x$checks$point_count[x$checks$player_id=='p4']==0L)
cat('PASS one league batch paginates points and explicit empty checks, preserving NULL/zero/negative values.\n')
y<-e$read_league_round_dataset('league',max_rows=1000L);stopifnot(nrow(y$points)==1000L,!y$complete)
cat('PASS score cap reports partial coverage.\n')
fixture<-fixture[FALSE,];z<-e$read_league_round_dataset('league');stopifnot(nrow(z$points)==0L,nrow(z$checks)==0L,z$complete)
cat('PASS a new league has a successful empty matrix.\n')
e$supabase_get<-function(...)NULL;failed<-tryCatch({e$read_league_round_dataset('league');FALSE},error=function(e)TRUE);stopifnot(failed)
cat('PASS unavailable database is an error, not a successful empty check.\n')
