source('test/test_idless_bonus_payments.R')
suppressPackageStartupMessages({source('background_runtime.R');source('Modules/Players_Table_Module.R');source('Modules/Rivals_Module.R');source('Modules/Classification_Module.R')})
e<-function(type,id,p)rival_finance_evidence('account','league','unknown',type,id,p,stamp)
history<-e('team_rounds','reference',list(team_id='reference',rounds=lapply(1:11,function(i)list(id=paste0('r',i),number=i,points=c(82,68,60,89,90,80,77,60,72,75,105)[i],status='closed',negative=FALSE))))
awards<-data.frame(round_id=c('r1','r4','r7'),round_number=c(1,4,7),player_id=c('p1','p4','p7'),player_name=c('Player 1','Player 4','Player 7'),is_mvp=c(TRUE,FALSE,TRUE),is_finished=TRUE)
eligibility<-data.frame(round_id=awards$round_id,player_id=awards$player_id,team_id='reference',eligible=TRUE,verified=TRUE)
rules<-list(initial_budget=300000000,money_per_point=70000,ranking_pool=30000000,ranking_mode='top',users_to_rank=-1,dreamteam_reward=500000,mvp_reward=1000000)
build_combined<-function(a=awards)build_rival_cash_dataset(data.frame(teamid='reference',teamname='Reference'),'league',c(restored,list(history)),a,
 rules=rules,eligibility=eligibility,coverage_complete=TRUE,account_id='account',reference_team_id='reference')
x<-build_combined();ledger<-x$cash_ledgers$reference
stopifnot(x$team_finances$reconstructed_cash==52129125,x$team_finances$mvp_income==2000000,
 x$team_finances$dreamteam_income==1500000,!any(ledger$estimated & ledger$bonus_type%in%c('mvp','dreamteam')),
 sum(ledger$component=='Best XI/MVP payment')==3,sum(ledger$amount[ledger$component=='Best XI/MVP payment'])==3500000)
inflow<-rivals_inflow_totals(ledger)
stopifnot(inflow$mvp==2000000,inflow$dreamteam==1500000,is.na(inflow$ranking))
h<-data.frame(team_id='reference',team_name='Reference',round=c(1,4,7),points=1)
columns<-classification_bonus_table(data.frame(team_id='reference'),h,x,c(1,11))
stopifnot(columns$mvp_bonus==2000000,columns$best_xi_bonus==1500000)
# Execute the actual renderUI expression with a pending position subtotal.
module_lines<-readLines('Modules/Rivals_Module.R')
start<-grep('output$rival_transactions_tab_ui <- renderUI',module_lines,fixed=TRUE)
end<-grep('# Render Transaction Table',module_lines,fixed=TRUE)
ui_expr<-parse(text=module_lines[start:(end-1)])[[1]][[3]][[2]]
stopifnot(!is.null(ui_expr))
rival_moneymovements_raw_RV<-function()ledger;input<-list(tx_pivot_by_player=FALSE);ns<-identity
html<-htmltools::renderTags(eval(ui_expr))$html
stopifnot(grepl('Financial history',html),inherits(rivals_cash_history_widget(ledger),'reactable'))
# A mismatch preserves actual cash and blocks unverified component attribution.
bad<-awards;bad$is_mvp<-FALSE
failed<-build_combined(bad)
stopifnot(failed$team_finances$reconstructed_cash==52129125,is.na(failed$team_finances$mvp_income),
 is.na(failed$team_finances$dreamteam_income),!any(failed$cash_ledgers$reference$estimated & failed$cash_ledgers$reference$bonus_type=='mvp'))
cat('PASS combined XI/MVP cash, independently validated bonus columns, mismatch handling and actual history render with unknown position totals\n')
