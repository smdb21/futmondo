#!/usr/bin/env Rscript
# Offline disclosure regression: factual content stays visible; diagnostics require expansion.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(name,code){force(code);checks<<-checks+1L;cat('PASS',name,'\n')}
visible_text<-function(tag) {
  if(inherits(tag,'shiny.tag')) {
    if(identical(tag$name,'details'))return('')
    return(paste(vapply(tag$children,visible_text,character(1)),collapse=' '))
  }
  if(is.list(tag))return(paste(vapply(tag,visible_text,character(1)),collapse=' '))
  paste(as.character(tag),collapse=' ')
}
check('offer failure details are collapsed and red while amounts and exact offer link remain visible',{
  row<-normalize_received_offer(list(id='offer',price=123456,userTeam=list(name='Rival')),'p')
  row$player_name<-'Player';row$acquisition_cost<-100000
  panel<-team_received_offers_panel(list(status='partial',records=row,error='Synthetic source failure'),identity)
  html<-as.character(panel);visible<-visible_text(panel)
  table<-team_received_offers_table(row,identity)$x$tag$attribs
  stopifnot(grepl('fm-info-error',html,fixed=TRUE),grepl('Data unavailable (FM-',html,fixed=TRUE),
    !grepl('Synthetic source failure|verified|Refresh',visible),
    grepl('123.456',table$columns[[2]]$cell[[1]],fixed=TRUE),
    grepl(team_received_offer_input(row),html,fixed=TRUE),!grepl('<details[^>]* open',html))
})
check('transfer panels omit the shared roster note while preserving schedule and empty-list facts',{
  state<-list(status='observed',records=incoming_clause_empty(),error=NULL,observations_only=TRUE)
  panel<-incoming_clause_panel(state,list(available=TRUE,round_number=12,starts_at=fm_time('2026-12-01T12:00:00Z')),
    identity)
  html<-as.character(panel);visible<-visible_text(panel)
  stopifnot(!grepl('Roster observed:|Transactions observed:',html),!grepl('Last observed transfer details.',html,fixed=TRUE),
    !grepl('observed|Saved observations|confirm completion',visible),
    grepl('Round 12 starts',visible,fixed=TRUE),grepl('No incoming clause signings.',visible,fixed=TRUE),
    !grepl('fm-info-error',html,fixed=TRUE))
})
check('unknown transfer schedules keep red diagnostics without asserting an empty verified list',{
  panel<-incoming_clause_panel(list(status='unavailable',records=incoming_clause_empty(),error='Synthetic read failure'),list(available=FALSE),identity)
  html<-as.character(panel);visible<-visible_text(panel)
  stopifnot(grepl('fm-info-error',html,fixed=TRUE),grepl('Synthetic read failure',html,fixed=TRUE),
    !grepl('failure|schedule|No incoming|\\(0\\)',visible))
})
check('chart diagnostics preserve missing-data evidence without adding a fabricated trace',{
  info<-player_trend_information(list(status='empty',points=data.frame(),valuation=data.frame()),data.frame(points=37,value=1234567))
  html<-as.character(info)
  stopifnot(!nzchar(visible_text(info)),grepl('Total points: 37',html,fixed=TRUE),
    grepl('1.234.567',html,fixed=TRUE),grepl('fm-info-error',html,fixed=TRUE))
})
check('loading diagnostics have no icons and rendering performs no source or database reads',{
  reads<-0L
  async_adapter_fetch<-function(...){reads<<-reads+1L;stop('Unexpected acquisition')}
  get_cached_data<-function(...){reads<<-reads+1L;stop('Unexpected API read')}
  supabase_get<-function(...){reads<<-reads+1L;stop('Unexpected DB read')}
  state<-list(status='loading',records=received_offers_empty(),error=NULL)
  elapsed<-system.time(for(i in 1:100)team_received_offers_panel(state,identity))[['elapsed']]
  html<-as.character(team_received_offers_panel(state,identity))
  stopifnot(reads==0L,!grepl('fm-info',html,fixed=TRUE),!grepl('fm-info-error',html,fixed=TRUE))
  cat(sprintf('MEASURE 100 offer-state renders %.3fs; source/database reads=%d\n',elapsed,reads))
})
cat(sprintf('PLAYER INFORMATION DISCLOSURES: %d passed / 0 failed\n',checks))
