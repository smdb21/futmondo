source('test/test_rival_reference_bonus_gate.R')
x<-build(list(payment('points',20),payment('ranking',20,'8.1')))
b<-x$cash_ledgers$b;a<-x$cash_ledgers$a
view<-rivals_cash_history_rows(b)
stopifnot(grepl('Round total: 5 points',view$concept[view$bonus_type=='points']),
 grepl('Position 2',view$concept[view$bonus_type=='ranking']),
 grepl('Position 1',rivals_cash_history_rows(a)$concept[rivals_cash_history_rows(a)$bonus_type=='ranking']))
# Prize direction does not reverse actual standings; equal scores share rank.
rules$ranking_mode<-'flop'
v<-rivals_cash_history_rows(build(list(payment('ranking',10)))$cash_ledgers$b)
stopifnot(grepl('Position 2',v$concept[v$bonus_type=='ranking']))
rounds[[2]]$payload$rounds[[1]]$points<-10
v<-rivals_cash_history_rows(build(list(payment('ranking',15)))$cash_ledgers$b)
stopifnot(any(grepl('Position 1 (tied)',v$concept,fixed=TRUE)))
# Same instant, different browser zones; local clock follows the event's DST.
b$date[b$bonus_type=='points']<-'2026-11-03T19:00:00Z'
b$timestamp<-fm_time(b$date)
pst<-rivals_cash_history_rows(b,timezone='America/Los_Angeles')
cet<-rivals_cash_history_rows(b,timezone='Europe/Madrid')
stopifnot(pst$date[pst$bonus_type=='points']=='03/11/2026 11:00',cet$date[cet$bonus_type=='points']=='03/11/2026 20:00',
 identical(pst$money,cet$money),identical(pst$running_balance,cet$running_balance),rivals_display_timezone('invalid')=='UTC')
b$date[b$bonus_type=='points']<-'2026-07-03T19:00:00Z';b$timestamp<-fm_time(b$date)
stopifnot(any(rivals_cash_history_rows(b,timezone='America/Los_Angeles')$date=='03/07/2026 12:00'),any(rivals_cash_history_rows(b,timezone='Europe/Madrid')$date=='03/07/2026 21:00'))
cat('PASS round points, actual position, tied standings and browser-local DST labels without cash changes\n')
