source('supabase_connector.R')
row<-list(player_id='p',observed_at='2026-01-01T00:00:00.123456Z',points=-2,unknown=NULL,payload=list(empty=list(),unknown=NULL))
for(table in c('player_history','player_match_observations','rival_finance_evidence')){
 wire<-supabase_write_request(table,row,'id',TRUE)
 decoded<-jsonlite::fromJSON(wire$body,simplifyVector=FALSE)
 stopifnot(startsWith(wire$path,'rpc/'),is.null(wire$conflict),isTRUE(decoded$p_ignore),decoded$p_conflict=='id',decoded$p_payload$points==-2,is.null(decoded$p_payload$unknown),is.null(decoded$p_payload$payload$unknown),length(decoded$p_payload$payload$empty)==0L)
}
wire<-supabase_write_request('player_history',data.frame(player_id='p',value=0,observed_at=NA_character_),NULL,FALSE)
decoded<-jsonlite::fromJSON(wire$body,simplifyVector=FALSE)
stopifnot(is.null(decoded$p_conflict),is.null(decoded$p_payload[[1]]$observed_at),decoded$p_payload[[1]]$value==0)
cat('PASS normalized history write envelopes preserve arrays, timestamps and null conflict\n')
