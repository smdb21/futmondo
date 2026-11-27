options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
teams<-data.frame(teamid=c('a','b'),teamname=c('A','B'))
feed<-data.frame(id=c('e1','e2'),created=c('2026-11-01T12:00:00Z','2026-11-01T13:00:00Z'),player_id='p',player_name='Player <one>',player_team_name='Barcelona',player_team_slug='barcelona',buyer_team_id=c('a','b'),seller_team_id=c('','a'),price=c(10,20))
ledger<-rival_cash_ledger(dplyr::bind_rows(rival_cash_entry('opening','Starting cash',100,'rules',type='opening'),rival_cash_entry('pressroom:e1','Player <one> (Purchased)',-10,'pressroom',type='buy',date=feed$created[1],player_id='p'),rival_cash_entry('movement:m2','Player <one>',20,'actual',type='sell',date=feed$created[2]),rival_cash_entry('bonus','Point reward',30,'actual',type='bonus',bonus_type='points')))
ledger$round_number<-NA_real_
actual<-rival_transfer_metadata(ledger,feed,'a',teams)
stopifnot(identical(actual$running_balance,ledger$running_balance),actual$transfer_buyer[actual$id=='pressroom:e1']=='A',actual$transfer_seller[actual$id=='pressroom:e1']=='Futmondo',actual$transfer_buyer[actual$id=='movement:m2']=='B')
view<-rivals_cash_history_rows(actual)
stopifnot(any(grepl('barcelona.png',view$player,fixed=TRUE)),any(grepl('&lt;one&gt;',view$player,fixed=TRUE)),!any(grepl('<one>',view$player,fixed=TRUE)),tail(view$running_balance,1)==140,!is.null(rivals_cash_history_widget(actual)))
# The pivot preserves plain names for sorting and uses the paired historical club.
attr(actual,"realized_trades")<-rival_realized_trades(actual)
buy<-actual$id=="pressroom:e1"
sale<-actual$id=="movement:m2"
actual$transfer_logo[sale]<-"betis"
pivot<-rivals_realized_trade_rows(actual)
stopifnot(pivot$Player=="Player <one>",grepl("barcelona.png",pivot$PlayerCell,fixed=TRUE),
  grepl("&lt;one&gt;",pivot$PlayerCell,fixed=TRUE),pivot$Net_PL==10)
actual$transfer_club[buy]<-NA_character_;actual$transfer_logo[buy]<-NA_character_
stopifnot(grepl("betis.png",rivals_realized_trade_rows(actual)$PlayerCell,fixed=TRUE))
for(index in list(buy,sale)){
  single<-actual[index,,drop=FALSE];attr(single,"realized_trades")<-rival_realized_trades(single)
  cell<-rivals_realized_trade_rows(single)$PlayerCell
  stopifnot(length(cell)==1L,grepl("<img",cell,fixed=TRUE)==any(index & sale))
}
legacy<-actual;legacy$transfer_logo<-legacy$transfer_club<-NULL
stopifnot(rivals_realized_trade_rows(legacy)$PlayerCell=="Player &lt;one&gt;")
batch<-actual[rep(which(buy),4),,drop=FALSE]
batch$id<-c("b1","b2","s1","s2");batch$type<-c("buy","buy","sell","sell")
batch$amount<-c(-10,-20,15,25)
batch$transfer_logo<-c(NA,"barcelona",NA,"betis")
attr(batch,"realized_trades")<-rival_realized_trades(batch)
grouped<-rivals_realized_trade_rows(batch)
stopifnot(nrow(grouped)==1L,grouped$Net_PL==10,grepl("barcelona.png",grouped$PlayerCell,fixed=TRUE))
# Ambiguous equal-name/amount/date events never invent a counterparty.
ambiguous<-rival_transfer_metadata(ledger,rbind(feed,transform(feed[2,],id='e3',buyer_team_id='c')),'a',teams)
stopifnot(ambiguous$transfer_buyer[ambiguous$id=='movement:m2']=='Unknown')
cat('PASS exact transfer metadata, historical crest, escaping, bonuses and unchanged balances; ambiguous matches remain unknown\n')
# Measure matching/projection at a representative long-history size.
big<-feed[rep(1,1000),];big$id<-paste0('e',seq_len(1000));big$created<-format(as.POSIXct('2026-01-01',tz='UTC')+seq_len(1000),'%Y-%m-%dT%H:%M:%SZ',tz='UTC')
entries<-rival_cash_entry(paste0('pressroom:',big$id),big$player_name,-10,'pressroom',type='buy',date=big$created,player_id='p')
entries$round_number<-NA_real_;entries<-rival_cash_ledger(entries)
time<-system.time({full<-rival_transfer_metadata(entries,big,'a',teams);view<-rivals_cash_history_rows(full)})[['elapsed']]
stopifnot(nrow(view)==1000)
cat(sprintf('MEASURE 1000 transfer metadata and display rows: %.3fs; %.1f KiB\n',time,as.numeric(object.size(view))/1024))
