#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
checks<-0L
check<-function(name,expr){force(expr);checks<<-checks+1L;cat('PASS',name,'\n')}
M<-1000000
rows<-data.frame(teamid=c('a','b','c','d'),teamname=c('Alpha','Beta','Gamma','Delta'),
  estimated_cash=c(300,290,300,300)*M,initial_budget=rep(300*M,4),
  team_value=c(25,0,0,NA_real_)*M,trading_profit=c(10,-10,0,10)*M,
  cash_activity_complete=c(TRUE,TRUE,TRUE,FALSE),trading_profit_complete=c(TRUE,TRUE,TRUE,FALSE),
  net_gain_estimated=TRUE,estimate_notes='Movement history completeness is unverified.',
  estimate_summary='Historical award eligibility is incomplete.',trading_profit_notes='',
  observed_cash=NA_real_,observed_cash_fresh=FALSE,reconciliation_difference=NA_real_,
  total_spent=30*M,total_sales=40*M,net_profit_loss=c(0,-10,0,0)*M,
  confidence='Estimated',as_of='2026-11-06T12:00:00Z',last_access=NA_character_,points=c(1,-2,0,NA_real_))
rows$player_transactions<-c(12L,2L,0L,NA_integer_)
metrics<-rivals_financial_metrics(rows)
check('independent wealth and completed profit retain positive, negative, zero and unknown',{
  stopifnot(identical(metrics$wealth_gained,c(25,-10,0,NA_real_)*M),
    identical(metrics$trading_profit,c(10,-10,0,NA_real_)*M))
})
check('reported wealth is independently explained by the displayed cash, squad and opening values',{
  screenshot<-rows[1,,drop=FALSE]
  screenshot$estimated_cash<-61495971;screenshot$team_value<-393516996
  stopifnot(rivals_financial_metrics(screenshot)$wealth_gained==155012967)
})
check('missing source flags or inputs cannot create known metrics',{
  for(field in c('estimated_cash','initial_budget','team_value','cash_activity_complete')){
    changed<-rows;changed[[field]]<-NULL
    stopifnot(all(is.na(rivals_financial_metrics(changed)$wealth_gained)))
  }
  for(field in c('trading_profit','trading_profit_complete')){
    changed<-rows;changed[[field]]<-NULL
    stopifnot(all(is.na(rivals_financial_metrics(changed)$trading_profit)))
  }
  stopifnot(nrow(rivals_financial_metrics(data.frame()))==0L)
})
check('the selected card and table share exact values and actual opening cash',{
  for(i in seq_len(nrow(rows))){
    summary<-rivals_summary_values(list(),NULL,rows,rows$teamid[i])
    stopifnot(identical(summary$wealth_gained,metrics$wealth_gained[i]),
      identical(summary$trading_profit,metrics$trading_profit[i]),summary$opening_cash==300*M)
  }
  changed<-rows;changed$initial_budget[1]<-100*M
  stopifnot(rivals_financial_metrics(changed)$wealth_gained[1]==225*M)
})
check('both new table columns are numeric and unknowns sort last',{
  widget<-rivals_finance_table_widget(list(team_finances=rows))
  attrs<-widget$x$tag$attribs
  data<-jsonlite::fromJSON(attrs$data)
  stopifnot(isTRUE(all.equal(data$`Overall wealth gained`,metrics$wealth_gained)),
    isTRUE(all.equal(data$`Trading profit`,metrics$trading_profit)),is.numeric(data$`Trading profit`),
    all(vapply(attrs$columns,function(x)isTRUE(x$sortNALast),logical(1))))
  columns<-vapply(attrs$columns,function(x)x$id,character(1))
  stopifnot(all(c('Overall wealth gained','Trading profit')%in%columns))
  stopifnot(identical(as.numeric(data$`Player transactions`),c(12,2,0,NA_real_)),
    'Player transactions'%in%columns)
})
check('fallback directory keeps both metric columns truthfully unavailable',{
  directory<-data.frame(teamid='a',teamname='Alpha',teamValue=25*M,points=1)
  widget<-rivals_finance_table_widget(list(team_finances=directory))
  attrs<-widget$x$tag$attribs;data<-jsonlite::fromJSON(attrs$data)
  stopifnot(all(c('Overall wealth gained','Trading profit')%in%names(data)),
    all(is.na(data$`Overall wealth gained`)),all(is.na(data$`Trading profit`)),
    all(is.na(data$`Player transactions`)),
    identical(attrs$selection,'single'))
})
check('own team shading follows IDs in full and fallback tables, independently of selection',{
  same_names<-rows[c(2,1,3,4),];same_names$teamname[1:2]<-'Same name'
  for(data in list(same_names,same_names[,c('teamid','teamname','team_value','points')])){
    for(own in c('a','b','','missing')){
      widget<-rivals_finance_table_widget(list(team_finances=data),own_team_id=own,selected_id='c')
      attrs<-widget$x$tag$attribs
      ids<-jsonlite::fromJSON(attrs$data)$team_id_hidden
      marked<-vapply(attrs$rowClass,function(x)identical(x,'fm-own-team-row'),logical(1))
      stopifnot(identical(unname(marked),ids%in%own))
    }
  }
})
summary<-rivals_summary_values(list(),NULL,rows,'a')
check('wealth details show a numeric equation and group raw evidence without IDs',{
  summary$wealth_gain_notes<-paste('A changed round is waiting for its historical lineup to refresh.',
    'Movement history completeness is unverified; secondary transfer feed was not added twice.',
    paste(rep('Round 64a4f7efae633549bd0f038a player 65a14cffd9c62803f70f8509 : historical ownership unavailable.',100),collapse=' '))
  html<-as.character(rivals_calculation_details(summary,NULL,'wealth'))
  stopifnot(grepl('Cash balance',html,fixed=TRUE),grepl('Starting cash (subtract)',html,fixed=TRUE),
    grepl(ui_financial_amount(300*M),html,fixed=TRUE),grepl(ui_financial_amount(25*M),html,fixed=TRUE),
    grepl('eligibility are unverified',html,fixed=TRUE),grepl('not verified as complete',html,fixed=TRUE),
    !grepl('[a-f0-9]{24}',html),nchar(html)<2200)
})
check('unavailable wealth retains both missing inputs and readable source limitations',{
  missing<-rivals_summary_values(list(),NULL,rows,'d')
  html<-as.character(rivals_calculation_details(missing,NULL,'wealth'))
  stopifnot(grepl('Current squad valuation is unavailable',html,fixed=TRUE),
    grepl('Included cash activity contains missing amounts',html,fixed=TRUE),
    grepl('not verified as complete',html,fixed=TRUE))
})
check('profit details reuse cached matched pairs including fees, held purchases excluded',{
  ledger<-data.frame(amount=c(-30,40,-20,-2)*M)
  attr(ledger,'realized_trades')<-list(pairs=data.frame(sell_price=40*M,buy_price=30*M,fees=2*M))
  summary$trading_profit<-8*M
  html<-as.character(rivals_calculation_details(summary,ledger,'trading'))
  stopifnot(grepl(ui_financial_amount(40*M),html,fixed=TRUE),
    grepl(ui_financial_amount(30*M),html,fixed=TRUE),grepl(ui_financial_amount(2*M),html,fixed=TRUE),
    grepl(ui_financial_amount(8*M),html,fixed=TRUE),!grepl(ui_financial_amount(20*M),html,fixed=TRUE))
})
check('unknown matched costs remain unavailable and trade identity keys are readable',{
  summary$trading_profit_complete<-FALSE;summary$trading_profit<-NA_real_
  summary$trading_profit_notes<-'Transfer history for id:65a14cffd9c62803f70f8509 has a sale without a matching purchase or overlapping ownership.'
  html<-as.character(rivals_calculation_details(summary,NULL,'trading'))
  stopifnot(grepl('Unavailable',html,fixed=TRUE),grepl('matching purchase',html,fixed=TRUE),!grepl('[a-f0-9]{24}',html))
})
check('league projection remains small and makes no source or ledger-matching calls',{
  old<-rival_realized_trades
  rival_realized_trades<-function(...)stop('Unexpected trade rematching')
  elapsed<-system.time(for(i in seq_len(1000))rivals_financial_metrics(rows))[['elapsed']]
  rival_realized_trades<-old
  stopifnot(elapsed<5)
  cat(sprintf('MEASURE 1000 four-team projections: %.3fs; output %d bytes; no I/O or rematching\n',elapsed,as.integer(object.size(metrics))))
})
cat(sprintf('Rival summary layout: %d passed / 0 failed\n',checks))
