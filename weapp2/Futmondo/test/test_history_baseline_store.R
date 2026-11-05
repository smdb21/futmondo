options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source("global.R"))
source("history_baseline_store.R")
checks <- 0L
check <- function(name,expr){force(expr);checks<<-checks+1L;cat("PASS",name,"\n")}
ctx <- resource_context(account_id="fixture-account",championship_id="fixture-league",season="season-A",scoring_version="score-A",session_id="fixture-session")
resource <- "pressroom_fixture-league"
stamp <- as.POSIXct("2026-10-01 12:00:00",tz="UTC")
data <- data.frame(id=c("event-a","event-b"),created=c("2026-10-01T11:00:00Z","2026-09-01T10:00:00Z"),
  player_id=c("p1","p2"),player_name=c("One","Two"),buyer_team_id=c("team",NA_character_),
  buyer_team_name=c("Team","Futmondo / Mercado"),seller_team_id=c("","team"),seller_team_name=c("Futmondo / Mercado","Team"),
  price=c(0,250),bids_available=c(TRUE,FALSE),is_clause=c(FALSE,NA),stringsAsFactors=FALSE)
data$bids <- I(list(list(list(`_id`="bid",u=list(`_id`="bidder",name="Bidder"),bid=100)),list()))
attr(data,"fetch_status") <- "ok";attr(data,"full_history_at") <- stamp
attr(data,"source_latest_at") <- stamp-3600;attr(data,"observed_at") <- stamp
attr(data,"unrelated_private_context") <- list(token="synthetic-secret")
saved_get <- supabase_get;saved_defer <- defer_persistence
queued <- list();database <- NULL;reads <- list()
defer_persistence <- function(fn,args,context,on_complete){
  queued[[length(queued)+1L]] <<- list(fn=fn,args=args,context=context,callback=on_complete)
  TRUE
}
supabase_get <- function(table_name,query_params){
  reads[[length(reads)+1L]] <<- list(table=table_name,query=query_params)
  database
}
check("missing snapshots return a cache miss, with one scoped read",{
  stopifnot(is.null(read_persisted_history_baseline(resource,ctx)),length(reads)==1L,
    identical(reads[[1]]$table,"complete_history_baselines"),identical(reads[[1]]$query$account_id,"eq.fixture-account"),
    identical(reads[[1]]$query$season,"eq.season-A"),identical(reads[[1]]$query$scoring_version,"eq.score-A"),
    identical(reads[[1]]$query$limit,1L))
})
check("complete source snapshot is one atomic queued upsert",{
  stopifnot(isTRUE(persist_completed_history_baseline(resource,data,ctx)),length(queued)==1L,
    identical(queued[[1]]$fn,"supabase_post_direct"),identical(queued[[1]]$args[[1]],"complete_history_baselines"),
    identical(queued[[1]]$args[[3]],"account_id,championship_id,season,scoring_version,resource"))
  payload <- queued[[1]]$args[[2]]
  json <- rawToChar(memDecompress(openssl::base64_decode(payload$payload),"gzip"))
  stopifnot(!grepl("synthetic-secret|fixture-session|token",json),nchar(payload$payload,type="bytes")<=900L*1024L)
})
check("failed writes remain retryable and acknowledgement suppresses unchanged saves",{
  queued[[1]]$callback(FALSE)
  stopifnot(isTRUE(persist_completed_history_baseline(resource,data,ctx)),length(queued)==2L)
  queued[[2]]$callback(TRUE)
  attr(data,"observed_at") <- stamp+1
  stopifnot(isTRUE(persist_completed_history_baseline(resource,data,ctx)),length(queued)==2L)
})
database <- as.data.frame(queued[[2]]$args[[2]],stringsAsFactors=FALSE)
check("cold restore preserves signed/zero values, system counterparties and native bids",{
  rm(list=ls(.history_baseline_ack),envir=.history_baseline_ack)
  restored <- read_persisted_history_baseline(resource,ctx)
  stopifnot(is.data.frame(restored),isTRUE(all.equal(restored$bids,data$bids)),identical(restored$price,c(0,250)),
    identical(restored$buyer_team_id,data$buyer_team_id),identical(restored$seller_team_id,data$seller_team_id),
    identical(attr(restored,"fetch_status"),"ok"),identical(as.numeric(attr(restored,"full_history_at")),as.numeric(stamp)),
    identical(as.numeric(attr(restored,"source_latest_at")),as.numeric(stamp-3600)),
    is.null(attr(restored,"unrelated_private_context")))
  stopifnot(isTRUE(persist_completed_history_baseline(resource,data,ctx)),length(queued)==2L)
})
check("subsecond source timestamps have stable canonical round trips",{
  times<-Sys.time()+seq(0,.999999,length.out=1000L)
  for(i in seq_along(times)){
    canonical<-history_baseline_stamp(times[i])
    stopifnot(identical(history_baseline_stamp(fm_time(canonical)),canonical))
  }
})
check("changed historical data and full-audit timestamp each trigger a save",{
  revised <- data;revised$price[2] <- 200
  stopifnot(isTRUE(persist_completed_history_baseline(resource,revised,ctx)),length(queued)==3L)
  queued[[3]]$callback(TRUE)
  attr(revised,"full_history_at") <- stamp+1
  stopifnot(isTRUE(persist_completed_history_baseline(resource,revised,ctx)),length(queued)==4L)
})
check("partial data, unknown columns, credential-bearing bids and malformed identities are rejected",{
  partial<-data;attr(partial,"fetch_status")<-"partial"
  columns<-data;columns$unexpected<-"value"
  secret<-data;secret$bids[[1]][[1]]$accessToken<-"forbidden"
  duplicate<-data;duplicate$id[2]<-duplicate$id[1]
  missing_time<-data;attr(missing_time,"full_history_at")<-NULL
  stopifnot(!persist_completed_history_baseline(resource,partial,ctx),!persist_completed_history_baseline(resource,columns,ctx),
    !persist_completed_history_baseline(resource,secret,ctx),!persist_completed_history_baseline(resource,duplicate,ctx),
    !persist_completed_history_baseline(resource,missing_time,ctx),length(queued)==4L)
})
check("empty locker baseline retains non-clause source watermark",{
  locker <- data.frame();attr(locker,"fetch_status")<-"ok";attr(locker,"full_history_at")<-stamp
  attr(locker,"source_latest_at")<-stamp-10;attr(locker,"observed_at")<-stamp
  locker_resource <- "pressroom_locker_payments_fixture-league"
  stopifnot(persist_completed_history_baseline(locker_resource,locker,ctx),length(queued)==5L)
  database<-as.data.frame(queued[[5]]$args[[2]],stringsAsFactors=FALSE)
  restored<-read_persisted_history_baseline(locker_resource,ctx)
  stopifnot(is.data.frame(restored),nrow(restored)==0L,ncol(restored)==0L,
    identical(as.numeric(attr(restored,"source_latest_at")),as.numeric(stamp-10)))
})
check("account, league, season, scoring and resource mismatches cannot restore",{
  database<-as.data.frame(queued[[2]]$args[[2]],stringsAsFactors=FALSE)
  for(field in c("account_id","championship_id","season","scoring_version")){
    wrong<-ctx;wrong[[field]]<-"other"
    stopifnot(is.null(read_persisted_history_baseline(resource,wrong)))
  }
  stopifnot(is.null(read_persisted_history_baseline("other",ctx)))
})
check("corrupt and oversize snapshots fall back without claiming coverage",{
  original<-database
  database$payload_sha256<-paste(rep("0",64),collapse="")
  stopifnot(is.null(read_persisted_history_baseline(resource,ctx)))
  database<-original;database$json_bytes<-16L*1024L^2L+1L
  stopifnot(is.null(read_persisted_history_baseline(resource,ctx)))
  oversized<-history_baseline_compress(charToRaw(paste(rep("x",10000L),collapse="")))
  bounded<-history_baseline_decompress(oversized,10L)
  stopifnot(length(bounded)==11L)
  database<-original;database$snapshot_hash<-paste(rep("0",64),collapse="")
  stopifnot(is.null(read_persisted_history_baseline(resource,ctx)))
  original_limit<-.history_baseline_limits$payload_bytes;.history_baseline_limits$payload_bytes<-1L
  changed<-data;changed$price[1]<-7
  stopifnot(!persist_completed_history_baseline(resource,changed,ctx))
  .history_baseline_limits$payload_bytes<-original_limit
  database<-original
})
check("database failures return a cache miss while cooperative replay requests propagate",{
  supabase_get<-function(...)stop("Synthetic database failure")
  stopifnot(is.null(read_persisted_history_baseline(resource,ctx)))
  supabase_get<-function(...)stop(structure(list(message="Awaiting read response"),class=c("fm_async_request","condition")))
  signal<-tryCatch(read_persisted_history_baseline(resource,ctx),fm_async_request=function(e)e)
  stopifnot(inherits(signal,"fm_async_request"))
})
check("queued callbacks retain only scalar acknowledgement metadata",{
  callback_env<-environment(queued[[1]]$callback)
  stopifnot(setequal(ls(callback_env,all.names=TRUE),c("key","fingerprint")),
    identical(parent.env(callback_env),environment(history_baseline_ack_callback)),
    as.numeric(object.size(as.list(callback_env)))<2048L)
})
supabase_get<-saved_get;defer_persistence<-saved_defer

# Restore through the real cooperative read adapter with an entirely synthetic
# delayed database response. It must issue one read and let other work continue.
saved_url<-get_sb_url;saved_key<-get_sb_key;saved_http<-fm_async_read_http
get_sb_url<-function()"https://fixture.invalid";get_sb_key<-function()"synthetic-key"
read_requests<-0L;heartbeat<-FALSE
fm_async_read_http<-function(url,method,body,headers,owner){
  read_requests<<-read_requests+1L
  stopifnot(grepl("/complete_history_baselines?",url,fixed=TRUE),identical(method,"GET"))
  promises::promise(function(resolve,reject)later::later(function()resolve(list(status_code=200L,
    content=charToRaw(jsonlite::toJSON(database,dataframe="rows",auto_unbox=TRUE)),times=list(total=0))),.02))
}
check("cold persisted restoration is one non-blocking request",{
  done<-FALSE;answer<-NULL;failure<-NULL
  later::later(function()heartbeat<<-TRUE,.001)
  promises::then(async_adapter_fetch("read_persisted_history_baseline",list(resource=resource,context=ctx),ctx),
    function(value){answer<<-value;done<<-TRUE},function(error){failure<<-error;done<<-TRUE})
  deadline<-Sys.time()+3
  while(!done&&Sys.time()<deadline)later::run_now(.01)
  stopifnot(done,is.null(failure),heartbeat,read_requests==1L,is.data.frame(answer),nrow(answer)==2L)
})
get_sb_url<-saved_url;get_sb_key<-saved_key;fm_async_read_http<-saved_http

# Synthetic workload: measure snapshot construction/restore size without a network
# or account. This reports CPU/compression cost, not production round-trip latency.
bulk<-data[rep(1:2,1250L),,drop=FALSE];bulk$id<-paste0("event-",seq_len(nrow(bulk)))
attr(bulk,"fetch_status")<-"ok";attr(bulk,"full_history_at")<-stamp
attr(bulk,"source_latest_at")<-stamp-3600;attr(bulk,"observed_at")<-stamp
elapsed<-system.time({snap<-history_baseline_snapshot(resource,bulk,ctx);raw<-charToRaw(history_baseline_json(snap));encoded<-openssl::base64_encode(history_baseline_compress(raw))})[["elapsed"]]
stopifnot(!is.null(snap),nchar(encoded,type="bytes")<=900L*1024L)
cat(sprintf("MEASURE baseline_rows=%d json_bytes=%d encoded_bytes=%d preparation_seconds=%.3f\n",nrow(bulk),length(raw),nchar(encoded,type="bytes"),elapsed))
cat(sprintf("HISTORY BASELINE STORE: %d groups passed / 0 failed\n",checks))
