options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
get_cached_data<-function(key,expr,...)force(expr)
get_locker_clause_payments<-function(...)data.frame()
page<-0L
futmondo_post<-function(url,body,...){
 page<<-page+1L
 query<-jsonlite::fromJSON(body)$query
 stopifnot(identical(query$from,if(page==1L)''else paste0('event',page-1L)))
 news<-if(page<=30L)list(list(`_id`=paste0('event',page),created='2026-09-01T10:00:00Z',`_player`=list(`_id`='p',name='Player'),`_buyer`=list(`_id`='a'),price=10))else list()
 structure(list(url="https://fixture.invalid/pressroom",status_code=200L,headers=structure(list('content-type'='application/json'),class='insensitive'),content=charToRaw(jsonlite::toJSON(list(answer=list(news=news)),auto_unbox=TRUE))),class='response')
}
x<-get_championship_pressroom(list(userid='fixture',token='fixture'),'league')
stopifnot(nrow(x)==30,page==31L,identical(attr(x,'fetch_status'),'ok'))
cat('PASS all cursor pages beyond the former cap, source IDs deduplicated, completion established by empty page\n')
# A repeated cursor is incomplete evidence, never a claim of full history.
page<-0L
futmondo_post<-function(...){page<<-page+1L;structure(list(url="https://fixture.invalid/pressroom",status_code=200L,headers=structure(list('content-type'='application/json'),class='insensitive'),content=charToRaw('{"answer":{"news":[{"_id":"same","created":"2026-09-01","price":10}]}}')),class='response')}
x<-get_championship_pressroom(list(userid='fixture',token='fixture'),'league')
stopifnot(page==2,nrow(x)==1,identical(attr(x,'fetch_status'),'partial'))
cat('PASS repeated cursors terminate with explicit partial coverage\n')
