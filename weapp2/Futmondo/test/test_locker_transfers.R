options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
item<-function(id='payment',buyer='Buyer &amp; Co',amount='397.345.103',seller='Seller',player='Player',date='2026-11-02T23:18:30.665Z')list(`_id`=id,created=date,typ='u',styp='clause',txt=paste0('El equipo <strong>',buyer,'</strong> ha pagado <strong>',amount,'</strong> propiedad de <strong>',seller,'</strong> como clausula de <strong>',player,'</strong>'))
raw<-locker_clause_rows(list(item(),item(),list(styp='info',txt='Ignore admin message')))
stopifnot(nrow(raw)==1,raw$price==397345103,raw$buyer_team_name=='Buyer & Co',raw$locker_event_id=='payment')
teams<-data.frame(teamid=c('a','b'),teamname=c('Buyer & Co','Seller'))
feed<-data.frame(id='delivery',created='2026-11-06T05:18:30Z',player_id='p',player_name='Player',player_team_name='Barcelona',player_team_slug='barcelona',buyer_team_id='a',buyer_team_name='Buyer & Co',seller_team_id='b',seller_team_name='Seller',price=397345103)
locker<-locker_resolve_parties(raw,feed,teams)
merged<-merge_locker_transfers(feed,locker)
stopifnot(nrow(merged)==1,merged$is_clause,merged$created==raw$created,merged$delivery_at==feed$created,merged$price==feed$price)
stopifnot(nrow(merge_locker_transfers(merged,locker))==1)
# A second paid clause with no delivery must count now, once on each side.
next_payment<-locker;next_payment$id<-'locker:new';next_payment$locker_event_id<-'new';next_payment$player_id<-'p2';next_payment$player_name<-'Second';next_payment$price<-100
combined<-merge_locker_transfers(feed,rbind(locker,next_payment))
stopifnot(nrow(combined)==2,sum(combined$price)==397345203)
rules<-list(initial_budget=500000000,money_per_point=0,ranking_pool=0,dreamteam_reward=0,mvp_reward=0)
x<-build_rival_cash_dataset(teams,'c',pressroom=combined,rules=rules,account_id='u')
stopifnot(tail(x$cash_ledgers$a$running_balance,1)==102654797,tail(x$cash_ledgers$b$running_balance,1)==897345203)
# Actual money movements are authoritative; never append the same public payment.
e<-rival_finance_evidence('u','c','unknown','movements','a:payment',list(id='payment',team_id='a',type='clause',concept='Player',money=-397345103,date=raw$created))
y<-build_rival_cash_dataset(teams,'c',evidence=list(e),pressroom=merged,rules=rules,account_id='u')
stopifnot(sum(y$cash_ledgers$a$amount[y$cash_ledgers$a$type=='buy'])==-397345103)
ambiguous<-rbind(feed,transform(feed,id='another'));stopifnot(inherits(try(merge_locker_transfers(ambiguous,locker),silent=TRUE),'try-error'))
unknown<-locker_resolve_parties(raw,feed[FALSE,],teams[FALSE,]);stopifnot(inherits(try(merge_locker_transfers(feed,unknown),silent=TRUE),'try-error'))
bad<-item(amount='unknown');stopifnot(inherits(try(locker_clause_rows(list(bad)),silent=TRUE),'try-error'))
cat('PASS exact HTML contract, escaping/entities, duplicate IDs, four-day delivery overlap, pending payment cash, actual-movement precedence and ambiguous/missing identity protection\n')
# Native source date remains precise; user-facing date is plain local time.
ledger<-x$cash_ledgers$a
bonus<-rival_cash_entry('bonus','Point reward',1,'actual',bonus_type='points');bonus$round_number<-1
ledger<-dplyr::bind_rows(ledger,bonus);ledger<-rival_cash_ledger(ledger)
view<-rivals_cash_history_rows(ledger,timezone='America/Los_Angeles')
stopifnot(any(grepl('fm-bonus-icon',view$player)),any(grepl('width:22px;height:22px',view$player,fixed=TRUE)),!any(grepl('PST|PDT|UTC|known payment|estimated date',view$date)))
cat('PASS bonus icon dimensions and plain local payment dates\n')
# Existing evidence storage preserves normalized source facts, not HTML/actor data.
old_persist<-persist_rival_finance_evidence;old_sync<-sync_pressroom_transactions_to_supabase
saved<-NULL;writes<-0L
persist_rival_finance_evidence<-function(evidence,context){saved<<-evidence;TRUE}
sync_pressroom_transactions_to_supabase<-function(...){writes<<-writes+1L;TRUE}
record_acquired_resource('pressroom_c',observed_data(merged),resource_context(account_id='u',championship_id='c'))
stopifnot(length(saved)==1,saved[[1]]$payload$kind=='locker_transfer',!any(c('txt','u','token')%in%names(saved[[1]]$payload$transfer)),writes==1)
old_read<-read_rival_finance_evidence
read_rival_finance_evidence<-function(account_id,championship_id,season){stopifnot(account_id=='u',championship_id=='c');list(evidence=saved,complete=TRUE)}
reused<-get_rival_finance_saved('u','c')$pressroom
stopifnot(nrow(reused)==1,reused$locker_event_id=='payment',nrow(merge_locker_transfers(feed,reused))==1)
read_rival_finance_evidence<-old_read;persist_rival_finance_evidence<-old_persist;sync_pressroom_transactions_to_supabase<-old_sync
cat('PASS normalized clause persistence, exact scoped saved recovery and repeated merge without extra cash\n')
saved[[1]]$payload$transfer$delivery_at<-NULL
stopifnot(locker_saved_transfer_rows(saved)$delivery_at=='')
cat('PASS saved optional JSON null fields retain a complete typed transfer row\n')
