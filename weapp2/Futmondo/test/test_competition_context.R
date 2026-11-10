#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('background_runtime.R')})
checks<-0L
check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
auth<-c(userid='context-account',token='synthetic')
active<-list(championships=list(list(id='english-game',name='Same name',league='premier',ps='custom',custom=list(stats='ff',media=list(list(media='official',pct=100)))),list(id='european-game',name='Same name',league='europe',ps='stats')),
 leagues=list(list(`_id`='premier',name='English competition',season='S-current'),list(`_id`='europe',name='European competition',season='S-other')))
attr(active,'observed_at')<-Sys.time()-2
check('source league and season resolve separately for each account championship',{
 publish_active_league_contexts(auth,active)
 a<-resource_context(auth,'english-game','team');b<-resource_context(auth,'european-game','team')
 stopifnot(a$competition_id=='premier',a$season=='S-current',b$competition_id=='europe',b$season=='S-other',
   resource_context(account_id='other-account',championship_id='english-game')$season=='unknown')
})
check('explicit unknown archives never become the current known season',{
 old<-resource_context(auth,'english-game',season='unknown',scoring_version='unknown',competition_id='unknown')
 stopifnot(old$season=='unknown',old$scoring_version=='unknown',old$competition_id=='unknown')
})
check('equivalent scoring settings share one fingerprint across every consumer',{
 a<-list(stats='ff',media=list(list(media='official',pct=100)))
 b<-list(media=list(list(pct=100,media='official')),stats='ff')
 v<-fm_scoring_version('custom',a)
 stopifnot(v==fm_scoring_version('custom',b),v==award_scoring_version('custom',b),
   v==normalize_league_rules(list(),list(pointSystem='custom',custom=b))$scoring_version,
   v!=fm_scoring_version('stats',b),fm_scoring_version(NULL,a)=='unknown')
})
check('conflicting season evidence is rejected without borrowing another league',{
 stopifnot(inherits(try(league_season_context('english-game','wrong',account_id=auth[['userid']]),silent=TRUE),'try-error'),
   league_season_context('english-game',account_id=auth[['userid']])=='S-current')
})
check('metadata persistence is immutable scoped normalized data and retryable',{
 writes<-list();defer_persistence<-function(fn,args,context,...){writes[[length(writes)+1L]]<<-args;FALSE}
 ctx<-resource_context(auth,'english-game','team');row<-league_context_metadata('english-game',auth[['userid']])
 stopifnot(!persist_league_context(row,ctx),!persist_league_context(row,ctx),length(writes)==2L,
   writes[[1]][[1]]=='league_context_observations',isTRUE(writes[[1]][[4]]),
   !any(grepl('token|password|header|authorization',names(row$payload),ignore.case=TRUE)))
})
check('valid database metadata restores only an already authenticated exact context',{
 row<-league_context_metadata('english-game',auth[['userid']]);saved<-row
 clear_league_contexts(auth[["userid"]])
 supabase_get<-function(table_name,query_params){stopifnot(table_name=='league_context_observations',query_params$account_id=='eq.context-account',query_params$competition_id=='eq.premier',query_params$season=='eq.S-current');
   out<-as.data.frame(saved[setdiff(names(saved),'payload')],stringsAsFactors=FALSE);out$payload<-I(list(saved$payload));out}
 restored<-get_persisted_league_context(auth[['userid']],'english-game','premier','S-current')
 stopifnot(!is.null(restored),resource_context(auth,'english-game')$season=='S-current')
 bad<-saved;bad$scoring_version<-'not-the-source-config';saved<-bad
 stopifnot(is.null(get_persisted_league_context(auth[['userid']],'english-game','premier','S-current')))
})
check('new compact membership metadata reuses older valid durable scoring',{
 clear_league_contexts(auth[['userid']])
 older<-normalize_active_league_contexts(active,auth[['userid']],Sys.time()-20)[[1]]
 compact<-active;compact$championships[[1]]$ps<-NULL;compact$championships[[1]]$custom<-NULL;attr(compact,'observed_at')<-Sys.time()-1
 publish_active_league_contexts(auth,compact)
 supabase_get<-function(table_name,query_params){
   stopifnot(query_params$scoring_version=='neq.unknown',!is.null(query_params[['payload->>scoring_expires_at']]))
   out<-as.data.frame(older[setdiff(names(older),'payload')],stringsAsFactors=FALSE);out$payload<-I(list(older$payload));out
 }
 restored<-get_persisted_league_context(auth[['userid']],'english-game','premier','S-current')
 stopifnot(!is.null(restored),restored$scoring_version==older$scoring_version,
   league_context_metadata('english-game',auth[['userid']])$scoring_version==older$scoring_version,
   restored$payload$scoring_observed_at==older$payload$scoring_observed_at)
})
check('same ID club changes, return transfers and simultaneous competitions preserve observations',{
 publish_active_league_contexts(auth,active)
 players<-data.frame(id='source-player',teamId='club-A',observed_at=format(Sys.time()-4,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC'))
 ctx<-resource_context(auth,'english-game');before<-normalize_player_memberships(players,ctx)
 players$teamId<-'club-B';players$observed_at<-format(Sys.time()-3,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC');after<-normalize_player_memberships(players,ctx)
 players$teamId<-'club-A';returned<-normalize_player_memberships(players,ctx)
 european<-normalize_player_memberships(players,resource_context(auth,'european-game'))
 stopifnot(before$real_club_id=='club-A',after$real_club_id=='club-B',returned$real_club_id=='club-A',
   before$competition_id=='premier',european$competition_id=='europe',all(is.na(c(before$effective_at,after$effective_at))))
 players$id<-'new-source-id';changed<-normalize_player_memberships(players,ctx)
 stopifnot(changed$player_id!=before$player_id,before$player_id=='source-player')
})
check('nested player endpoint context survives normalization after a real-world move',{
 normalized<-normalize_player_matches(list(player=list(id='source-player'),league=list(`_id`='europe',season='S-other'),
   matches=list(list(id='match',r=1,info=list(date='2026-11-01T00:00:00Z'),h=list(name='H'),a=list(name='A')))))
 stopifnot(normalized$competition_id=='europe',normalized$season=='S-other',normalized$match_id=='match')
})
check('missing or expired source identity cannot create durable membership history',{
 x<-data.frame(id='source-player',teamId='club',observed_at='2026-01-01')
 stopifnot(nrow(normalize_player_memberships(x,resource_context(account_id='unresolved',championship_id='game')))==0L)
 row<-league_context_metadata('english-game',auth[['userid']]);row$observed_at<-format(Sys.time()-600,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC');row$expires_at<-format(Sys.time()-300,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')
 .fm_league_contexts$items[[fm_league_context_key(auth[['userid']],'english-game')]]<-row
 stopifnot(is.null(league_context_metadata('english-game',auth[['userid']])))
})
check('compact metadata cannot renew an older scoring deadline',{
 clear_league_contexts(auth[["userid"]]);publish_active_league_contexts(auth,active)
 row<-league_context_metadata('english-game',auth[['userid']]);deadline<-format(Sys.time()+5,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC')
 row$payload$scoring_expires_at<-deadline;.fm_league_contexts$items[[fm_league_context_key(auth[['userid']],'english-game')]]<-row
 compact<-active;compact$championships[[1]]$ps<-NULL;compact$championships[[1]]$custom<-NULL
 attr(compact,'observed_at')<-Sys.time();publish_active_league_contexts(auth,compact)
 merged<-league_context_metadata('english-game',auth[['userid']]);stopifnot(identical(merged$payload$scoring_expires_at,deadline))
 merged$payload$scoring_expires_at<-format(Sys.time()-1,'%Y-%m-%dT%H:%M:%OS6Z',tz='UTC')
 .fm_league_contexts$items[[fm_league_context_key(auth[['userid']],'english-game')]]<-merged
 stopifnot(league_context_metadata('english-game',auth[['userid']])$scoring_version=='unknown')
})
check('cache eviction never makes an authenticated source-present league inaccessible',{
 answer<-list(championships=lapply(seq_len(130L),function(i)list(id=paste0('game-',i),league=paste0('real-',i),ps='stats')),
   leagues=lapply(seq_len(130L),function(i)list(`_id`=paste0('real-',i),season='source-season')))
 attr(answer,'observed_at')<-Sys.time()
 get_active_championships<-function(...)answer
 resolved<-get_resolved_league_context(auth,'game-1','team')
 stopifnot(resolved$competition_id=='real-1',resolved$season=='source-season',length(.fm_league_contexts$items)<=128L)
 before<-shiny::isolate(league_context_revision());get_resolved_league_context(auth,'game-1','team')
 stopifnot(shiny::isolate(league_context_revision())==before)
})
check('newer source clocks cannot evict a valid context being demanded now',{
 selected<-list(championships=list(list(id='selected-game',league='selected-real',ps='stats')),
   leagues=list(list(`_id`='selected-real',season='selected-season')))
 attr(selected,'observed_at')<-Sys.time()-50
 publish_active_league_contexts(auth,selected)
 for(i in seq_len(128L)){
   other<-list(championships=list(list(id=paste0('other-game-',i),league='other-real',ps='stats')),leagues=list(list(`_id`='other-real',season='other-season')))
   attr(other,'observed_at')<-Sys.time()-10
   publish_active_league_contexts(c(userid=paste0('other-account-',i),token='synthetic'),other)
 }
 get_active_championships<-function(...)selected
 resolved<-get_resolved_league_context(auth,'selected-game','team')
 stopifnot(resolved$competition_id=='selected-real',resolved$season=='selected-season',length(.fm_league_contexts$items)<=128L)
})
check('metadata accelerator has an independent byte budget and retains the newly demanded row',{
 clear_league_contexts(auth[['userid']])
 base<-normalize_active_league_contexts(active,auth[['userid']],Sys.time())[[1]]
 for(i in seq_len(100L)){row<-base;row$championship_id<-paste0('large-game-',i);row$payload$fixture_padding<-paste(rep('x',100000L),collapse='');stopifnot(isTRUE(remember_league_context(row)))}
 stopifnot(length(.fm_league_contexts$items)<=128L,sum(vapply(.fm_league_contexts$items,function(x)as.numeric(object.size(x)),numeric(1)))<=8*1024^2,
   !is.null(league_context_metadata('large-game-100',auth[['userid']])) )
})
cat(sprintf('COMPETITION CONTEXT: %d groups passed / 0 failed\n',checks))
