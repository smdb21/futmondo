options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
at<-fm_time('2026-11-19T16:05:05Z')
rows<-data.frame(id='official',player_id='player',buyer_team_id='mine',seller_team_id='',
 price=3210413,created=fm_clause_stamp(at+60),is_clause=NA)
job<-list(id='job',player_id='player',original_owner='',max_price='3210413',dispatched_at=fm_clause_stamp(at))
match_bid<-function(rows,stamp=at+70)fm_clause_completed_purchase_matches(list(job),rows,'mine',stamp,at+65)
stopifnot(length(match_bid(rows))==1L,!length(match_bid(rows,at+50)),
 !length(match_bid(transform(rows,buyer_team_id='other'))),!length(match_bid(transform(rows,price=1))),
 !length(match_bid(transform(rows,created=fm_clause_stamp(at-3)))),
 !length(match_bid(transform(rows,seller_team_id='rival'))),
 !length(match_bid(rbind(rows,transform(rows,id='ambiguous')))))
cat('PASS completed bid survives missing active offer, with matching origin, amount, identity and newer balance required\n')

clause_job <- modifyList(job,list(mode='clause',state='accepted',result_code=API_CODE_OK,
 original_owner='rival',max_price=100000000,actual_price=91657534,reserved_amount=91657534,reserved_slot=1))
clause_history <- transform(rows,seller_team_id='rival',price=91657534)
match_clause <- function(history=clause_history, balance_at=at+70, current_job=clause_job)
 fm_clause_completed_purchase_matches(list(current_job),history,'mine',balance_at,at+65)
stopifnot(length(match_clause())==1L,
 !length(match_clause(balance_at=at+59)),
 !length(match_clause(transform(clause_history,seller_team_id='other'))),
 !length(match_clause(transform(clause_history,is_clause=FALSE))),
 !length(match_clause(rbind(clause_history,transform(clause_history,id='ambiguous')))))
uncertain <- modifyList(clause_job,list(state='uncertain',result_code='purchase_response_uncertain'))
stopifnot(!length(match_clause(current_job=uncertain)),
 length(match_clause(transform(clause_history,is_clause=TRUE),current_job=uncertain))==1L)

# Replay the saved balance with the reported hold before and after settlement.
team <- observed_data(list(budget=48433849,withheld=0,teamValue=387372730,
 configuration=list(mnmp=.5),maxBid=242120214),observed_at=at+70)
base <- round_finance_display(resource_state(team),resource_state(data.frame()))
pending <- fm_clause_finance(base,list(clause_job),team)
amounts <- as.character(round_finance_amounts_UI(round_finance_card_display(pending)))
stopifnot(pending$projected_committed_balance==-43223685,pending$spendable_budget==150462680,
 grepl('After offers: 48.433.849 EUR',amounts,fixed=TRUE),
 !grepl('round-countdown-warning',amounts,fixed=TRUE))
settled <- modifyList(clause_job,list(state='succeeded',reserved_amount=0,reserved_slot=0))
stopifnot(fm_clause_finance(base,list(settled),team)$spendable_budget==242120214,
 fm_clause_reservations(list(settled))$count==0)
cat('PASS manual clause settlement requires matching saved evidence and a newer balance; After offers shows cash with zero offers\n')
