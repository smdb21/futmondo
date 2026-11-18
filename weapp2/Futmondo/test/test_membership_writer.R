source('supabase_connector.R')
options(futmondo.offline=FALSE)
get_sb_url<-function()'https://fixture.invalid'
get_sb_key<-function()'synthetic'
requests<-list()
POST<-function(url,query,body,...){requests[[length(requests)+1L]]<<-list(url=url,query=query,body=jsonlite::fromJSON(body,simplifyVector=FALSE));structure(list(status_code=200L),class='response')}
row<-data.frame(player_id='p',competition_id='c',season='s',real_club_id=NA_character_,observed_at='2026-01-01T00:00:00Z',effective_at=NA_character_,source='fixture')
stopifnot(supabase_post_direct('player_membership_observations',row,'player_id,competition_id,season,observed_at,real_club_id',TRUE)==200L)
r<-requests[[1]]
stopifnot(r$url=='https://fixture.invalid/rest/v1/rpc/membership_write',is.null(r$query),isTRUE(r$body$p_ignore),length(r$body$p_payload)==1L,is.null(r$body$p_payload[[1]]$real_club_id),r$body$p_payload[[1]]$player_id=='p')
stopifnot(supabase_post_direct('players',list(id='p'),'id',TRUE)==200L)
stopifnot(requests[[2]]$url=='https://fixture.invalid/rest/v1/players',requests[[2]]$query$on_conflict=='id')
cat('PASS normalized membership RPC envelope and unchanged ordinary writes\n')
