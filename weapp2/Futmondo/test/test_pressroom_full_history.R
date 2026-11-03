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
# Production contract: created-desc ordering, ObjectId cursor filtering. Several
# writers generate IDs in the same second, so ID order differs from time order.
source_rows<-lapply(seq_len(149),function(i)list(`_id`=sprintf('70000002%016x',i),created=sprintf('2026-10-17T04:22:26.%03dZ',1000-i),price=1))
source_rows<-c(source_rows,list(
 list(`_id`='700000010000000000000001',created='2026-10-17T04:22:25.500Z',price=1),
 list(`_id`='700000010000000000000003',created='2026-10-17T04:22:25.490Z',price=1276591),
 list(`_id`='700000010000000000000002',created='2026-10-17T04:22:25.480Z',price=23566956),
 list(`_id`='700000000000000000000001',created='2026-10-17T04:22:24.000Z',price=22999999)))
page<-0L
futmondo_post<-function(url,body,...){
 page<<-page+1L;q<-jsonlite::fromJSON(body)$query
 rows<-Filter(function(x)!nzchar(q$from)||x$`_id`<q$from,source_rows)
 rows<-head(rows,150L)
 fm_httr_response(list(status_code=200L,content=charToRaw(jsonlite::toJSON(list(answer=list(news=rows)),auto_unbox=TRUE)),times=list(total=0)),url)
}
x<-get_championship_pressroom(list(userid='fixture',token='fixture'),'league')
stopifnot(nrow(x)==153L,!anyDuplicated(x$id),sum(x$price)==47843546+150,
 identical(attr(x,'fetch_status'),'ok'),page==4L)
cat('PASS nonmonotonic ObjectIds recover boundary purchases exactly once and reach an empty page\n')
