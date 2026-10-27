#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source('futmondo_functions.R');source('supabase_connector.R');source('factual_helpers.R');source('theme.R')})
checks<-0L
check<-function(name,code){force(code);checks<<-checks+1L;cat('[PASS]',name,'\n')}
now<-as.POSIXct('2026-09-07 12:00:00',tz='UTC')
roster<-data.frame(id=paste0('p',1:11),value=100,points=50,observed_at=now)
check('missing observation times remain unknown and stale data keeps original time',{
  d<-data.frame(id='p',value=100)
  stopifnot(is.na(preserve_observation_time(d)$observed_at))
  attr(d,'observed_at')<-now-86400;attr(d,'fetch_status')<-'stale'
  out<-preserve_observation_time(d)
  stopifnot(fm_time(out$observed_at)==now-86400,attr(out,'fetch_status')=='stale')
})
check('price persistence uses source time and skips stale repeats',{
  saved<-supabase_post;writes<-list();supabase_post<-function(table_name,payload,...) {writes[[length(writes)+1L]]<<-payload;204L}
  d<-roster;d$observed_at<-format(now-86400,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')
  log_player_history(d,'league')
  stopifnot(length(writes)==1L,all(fm_time(writes[[1]]$observed_at)==now-86400))
  attr(d,'fetch_status')<-'stale';log_player_history(d,'league');stopifnot(length(writes)==1L)
  supabase_post<-saved
})
check('past auction replay selects the revision known at its cutoff',{
  saved<-supabase_read_all
  supabase_read_all<-function(...)data.frame(championship_id='c',auction_id='a',amount=c(100,200),
    observed_at=format(now+c(-86400,86400),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'))
  d<-read_observation_revisions('fixture','c',now,c('championship_id','auction_id'))
  stopifnot(nrow(d)==1L,d$amount==100)
  supabase_read_all<-saved
})
check('realized profit pairs ownership cycles and preserves unknown acquisition cost',{
  d<-data.frame(id=1:5,player_id=c('p','p','p','p','q'),price=c(100,120,150,140,50),
    buyer_team_id=c('me','','me','',''),seller_team_id=c('','me','','me','me'),
    created=format(now+1:5,'%Y-%m-%dT%H:%M:%OSZ',tz='UTC'))
  l<-realized_trade_ledger(rbind(d,d),'me')
  stopifnot(nrow(l)==3L,identical(l$realized_profit,c(20,-10,NA_real_)),all(l$counterparty=='Futmondo (System)'))
})
check('terminal theme uses shared accessible foreground and widget backgrounds',{
  t<-fm_theme_tokens();stopifnot(t$bg=='#050805',t$text=='#7CFF9B',grepl('monospace',t$font))
  luminance<-function(hex){c<-grDevices::col2rgb(hex)/255;c<-ifelse(c<=.04045,c/12.92,((c+.055)/1.055)^2.4);sum(c*c(.2126,.7152,.0722))}
  stopifnot((luminance(t$text)+.05)/(luminance(t$surface)+.05)>4.5)
  p<-plotly::plotly_build(fm_plot_layout(plotly::plot_ly(x=1:2,y=1:2,type='scatter',mode='lines')))
  stopifnot(p$x$layout$paper_bgcolor==t$surface,p$x$layout$font$color==t$text)
  stopifnot(fm_reactable_theme()$style$fontFamily==t$font)
  stopifnot(!grepl('fonts.googleapis',paste(readLines('www/custom_style.css'),collapse='')))
})
cat('FACTUAL REVIEW:',checks,'passed / 0 failed\n')
