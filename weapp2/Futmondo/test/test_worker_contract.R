options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
check<-function(ok,label){stopifnot(ok);cat('PASS',label,'\n')}
roundtrip<-function(value)fm_worker_decode(jsonlite::fromJSON(as.character(jsonlite::toJSON(fm_worker_encode(value),auto_unbox=TRUE,na='null',null='null',digits=NA)),simplifyVector=FALSE))
x<-data.frame(id=c('a','b','c'),points=c(0,-2,NA_real_),price=c(2,NA,4),stringsAsFactors=FALSE)
attr(x,'observed_at')<-as.POSIXct('2026-10-01 12:00:00',tz='UTC');attr(x,'complete')<-FALSE
restored<-roundtrip(x)
check(identical(restored,x),'typed frames preserve zero, negative, unknown, time and completeness')
check(identical(roundtrip(list(empty=data.frame(id=character(),value=numeric()),rows=list(),number=NA_real_)),list(empty=data.frame(id=character(),value=numeric()),rows=list(),number=NA_real_)),'empty frames and nested missing values retain types')
check(inherits(try(fm_worker_encode(list(token='secret')),silent=TRUE),'try-error'),'credential-bearing values are rejected')
check(inherits(try(fm_worker_decode(list(kind='call',values=list())),silent=TRUE),'try-error'),'executable values cannot be decoded')
a<-c(userid='u',token='first');b<-c(userid='u',token='second')
ctx<-resource_context(a,'l','t',season='s',scoring_version='v',competition_id='c')
d1<-fm_worker_descriptor('get_player_summary',list(login=a,championship_id='l',user_team_id='t',player_id='p'),ctx)
d2<-fm_worker_descriptor('get_player_summary',list(player_id='p',user_team_id='t',login=b,championship_id='l'),ctx)
check(identical(d1,d2)&&!grepl('first|second|token',fm_worker_json(d1)),'canonical keys are ordered and credential independent')
ctx2<-ctx;ctx2$season<-'other'
check(d1$resource_key!=fm_worker_descriptor('get_player_summary',list(championship_id='l',user_team_id='t',player_id='p'),ctx2)$resource_key,'season context changes resource identity')
# Cross-language descriptor/encryption fixtures contain synthetic tokens only.
writeLines(fm_worker_json(fm_worker_descriptor('get_active_championships',list(),resource_context(account_id='fixture'))),'/tmp/futmondo-worker-descriptor.json')
Sys.setenv(FUTMONDO_WORKER_KEY=paste(rep('ab',32),collapse=''))
sealed<-fm_worker_encrypt(c(userid='fixture',token='synthetic',user_name='fixture@example.invalid'))
writeLines(sealed,'/tmp/futmondo-worker-encrypted.txt')
writeLines(attr(sealed,'credential_id'),'/tmp/futmondo-worker-credential-id.txt')
# Database restoration verifies source age, decompression, integrity and context.
wire<-charToRaw(as.character(jsonlite::toJSON(fm_worker_encode(x),auto_unbox=TRUE,null='null',na='null',digits=NA)))
compressed<-history_baseline_compress(wire)
resource<-list(key=d1$resource_key,manifest=list(account_id='u',resource_key=d1$resource_key,json_bytes=length(wire),parts=1L,
 sha256=paste0(openssl::sha256(compressed)),observed_at='2026-10-01T12:00:00Z',checked_at='2026-10-01T12:00:01Z',valid_until='2026-10-01T12:01:00Z',status='ok'),
 chunks=list(jsonlite::base64_enc(compressed)))
z<-fm_worker_restore(resource,'u')
check(identical(attr(z$data,'fetch_status'),'stale')&&identical(as.numeric(attr(z$data,'observed_at')),as.numeric(attr(x,'observed_at'))),'snapshot hydration cannot renew source freshness')
check(inherits(try(fm_worker_restore(resource,'other'),silent=TRUE),'try-error'),'foreign snapshot restoration is rejected')
resource$manifest$sha256<-paste(rep('0',64),collapse='')
check(inherits(try(fm_worker_restore(resource,'u'),silent=TRUE),'try-error'),'corrupt snapshot restoration is rejected')
check(inherits(try(fm_worker_descriptor('get_rival_finance_saved',list(account_id='foreign'),ctx),silent=TRUE),'try-error'),'arguments cannot replace the authenticated account')
check(identical(roundtrip(ctx),ctx),'the explicit context class survives typed snapshots')
node<-jsonlite::fromJSON(as.character(jsonlite::toJSON(fm_worker_encode(x),auto_unbox=TRUE,null='null',na='null',digits=NA)),simplifyVector=FALSE)
node$columns<-node$columns[rev(names(node$columns))]
check(identical(fm_worker_decode(node),x),'JSONB/Python object-key order cannot reorder dataframe columns')
typed<-fm_worker_descriptor('get_rival_round_ranking_observation',list(source_teams=c('a','b'),round_number=1),ctx)
check(identical(fm_worker_decode(jsonlite::fromJSON(as.character(jsonlite::toJSON(typed$encoded_args,auto_unbox=TRUE,null='null',na='null',digits=NA)),simplifyVector=FALSE))$source_teams,c('a','b')),'source argument vectors retain their types across the Python bridge')
# Removing the only named argument leaves names=character(0), serialized as [].
empty_named<-list(login=a)[FALSE]
check(identical(roundtrip(empty_named),empty_named),'empty named lists survive the JSON bridge')
empty_vector<-setNames(character(),character())
check(identical(roundtrip(empty_vector),empty_vector),'empty named primitive vectors survive the JSON bridge')
for(handler in c('get_active_championships','get_notification_unread')){
 d<-fm_worker_descriptor(handler,list(login=a),resource_context(a))
 decoded<-jsonlite::fromJSON(fm_worker_json(d),simplifyVector=FALSE)
 check(identical(fm_worker_decode(decoded$encoded_args),empty_named),paste(handler,'decodes arguments after authentication removal'))
}
bad<-fm_worker_encode(list(a=1));bad$names<-list()
check(inherits(try(fm_worker_decode(bad),silent=TRUE),'try-error'),'empty names cannot label nonempty values')
market<-data.table::data.table(id='p',points=-2,change=1)
attr(market,'observed_at')<-as.POSIXct('2026-11-11',tz='UTC')
check(identical(roundtrip(market),as.data.frame(market)),'data.table market rows transport as factual frames without internal pointers')
unsupported<-market;class(unsupported)<-c('unsupported',class(unsupported))
check(inherits(try(fm_worker_encode(unsupported),silent=TRUE),'try-error'),'unknown subclasses cannot bypass the worker class allowlist')
cat('WORKER CONTRACT: 20 groups passed\n')
