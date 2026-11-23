options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
at<-fm_time('2026-11-19T16:05:05Z')
rows<-data.frame(id='official',player_id='player',buyer_team_id='mine',seller_team_id='',
 price=3210413,created=fm_clause_stamp(at+60),is_clause=NA)
job<-list(id='job',player_id='player',original_owner='',max_price='3210413',dispatched_at=fm_clause_stamp(at))
match_bid<-function(rows,stamp=at+70)fm_clause_completed_bid_matches(list(job),rows,'mine',stamp,at+65)
stopifnot(length(match_bid(rows))==1L,!length(match_bid(rows,at+50)),
 !length(match_bid(transform(rows,buyer_team_id='other'))),!length(match_bid(transform(rows,price=1))),
 !length(match_bid(transform(rows,created=fm_clause_stamp(at-3)))),
 !length(match_bid(transform(rows,seller_team_id='rival'))),
 !length(match_bid(rbind(rows,transform(rows,id='ambiguous')))))
cat('PASS completed bid survives missing active offer, with matching origin, amount, identity and newer balance required\n')
