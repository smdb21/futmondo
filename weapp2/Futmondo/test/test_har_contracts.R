#!/usr/bin/env Rscript
# Replays response bodies only. Credentials and requests in captures are never used.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('futmondo_functions.R'))
http_attempts<-0L
for(method in c('GET','POST','PUT','PATCH','DELETE')) suppressMessages(trace(method,where=asNamespace('httr'),print=FALSE,
  tracer=quote({http_attempts<<-http_attempts+1L;stop('Network forbidden in HAR tests')})))
paths<-c('app.futmondo.com.notifications.har','app.futmondo.com.market.har','app.futmondo.com.pressroom.har','app.futmondo.com.roster_clause.har')
if(!all(file.exists(paths))) stop('HAR contract tests need the local response captures (excluded from deployment).')
entries<-unlist(lapply(paths,function(p)jsonlite::fromJSON(p,simplifyVector=FALSE)$log$entries),recursive=FALSE)
responses<-list()
for(e in entries) {
  url<-sub('\\?.*$','',e$request$url)
  if(!startsWith(url,'https://api.futmondo.com/')) next
  if(is.null(responses[[url]]) && is.character(e$response$content$text)) responses[[url]]<-e$response$content$text
}
futmondo_post<-function(url,...) {
  text<-responses[[url]]
  if(is.null(text)) stop('No response fixture for requested route')
  structure(list(status_code=200L,headers=list('Content-Type'='application/json; charset=utf-8'),
    content=charToRaw(text),url=url),class='response')
}
auth<-c(token='synthetic',userid='synthetic-user')
n<-0L
check<-function(name,code){force(code);n<<-n+1L;cat('PASS',name,'\n')}
check('notification list unread and acknowledgement response contracts',{
 d<-get_notifications(auth);stopifnot(is.data.frame(d),nrow(d)>0,all(c('action','source','is_read')%in%names(d)))
 stopifnot(is.finite(get_notification_unread(auth)),mark_notification_read(auth,'synthetic-id'))
})
check('market listings preserve own bid and deadline fields',{
 d<-get_market_players(auth,'fixture-champ','fixture-team')
 stopifnot(nrow(d)>0,all(c('id','expirationDate','price','computer','bid_id','bid_price')%in%names(d)))
 own<-d[!is.na(d$bid_id)&nzchar(as.character(d$bid_id)),,drop=FALSE]
 stopifnot(all(is.finite(as.numeric(own$bid_price))),all(!is.na(fm_time(d$expirationDate))))
})
check('pressroom preserves source IDs nested bid evidence and system counterparties',{
 d<-get_championship_pressroom(auth,'fixture-champ')
 stopifnot(nrow(d)>0,'bids'%in%names(d),!anyDuplicated(d$id))
 b<-normalize_bid_observations(d,'fixture-champ')
 stopifnot(nrow(b)>0,!anyDuplicated(b[c('championship_id','auction_id','bidder_id')]),any(b$is_winner))
})
check('summary preserves round scores fixtures and ownership context',{
 s<-get_player_summary(auth,'fixture-champ','fixture-team','fixture-player')
 stopifnot(is.list(s),all(c('points','match','championship','owners')%in%names(s)))
})
check('replay made zero external requests',stopifnot(http_attempts==0L))
for(method in c('GET','POST','PUT','PATCH','DELETE')) suppressMessages(untrace(method,where=asNamespace('httr')))
cat(sprintf('HAR CONTRACTS: %d passed / 0 failed\n',n))
