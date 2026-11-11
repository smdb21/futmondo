options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
# Typical large financial ledger; no live source or database is involved.
rows<-data.frame(id=paste0('tx-',1:5001),team_id='t',player_id=paste0('p',1:5001),
 amount=c(10000,rep(-1,5000)),running_balance=c(10000,10000-(1:5000)),included=TRUE,
 occurred_at=format(Sys.time()-(5001:1),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'),
 source='observed_market_transfer',component='Purchase',stringsAsFactors=FALSE)
wire<-charToRaw(as.character(jsonlite::toJSON(fm_worker_encode(list(cash_ledgers=list(t=rows))),auto_unbox=TRUE,na='null',null='null',digits=NA)))
compressed<-history_baseline_compress(wire)
resource<-list(key='fixture',manifest=list(account_id='u',resource_key='fixture',parts=1L,json_bytes=length(wire),
 sha256=paste0(openssl::sha256(compressed)),observed_at='2026-11-01T00:00:00Z',checked_at='2026-11-01T00:00:00Z',valid_until='2026-11-01T00:01:00Z',status='ok',completeness='complete'),chunks=list(jsonlite::base64_enc(compressed)))
elapsed<-system.time(value<-fm_worker_restore(resource,'u'))[['elapsed']]
stopifnot(nrow(value$data$cash_ledgers$t)==5001L,tail(value$data$cash_ledgers$t$running_balance,1)==5000,elapsed<.5)
cat(sprintf('PASS 5,001-entry worker ledger restore %.3fs; JSON %.1f KiB, gzip %.1f KiB\n',elapsed,length(wire)/1024,length(compressed)/1024))
