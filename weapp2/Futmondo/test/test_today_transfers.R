#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
rows<-data.frame(created=c('2026-11-01','2026-10-31','2026-10-30'),player_id=c('p1','p2','p3'),
 player_name=c('<script>bad</script>','Player 2','Player 3'),price=c(1234567.8,0,NA),
 buyer_team_id=c('r1','','System'),seller_team_id=c('', 'r2','r3'),
 buyer_team_name=c('Old name','Futmondo / Mercado','System'),seller_team_name=c('Futmondo / Mercado','Captured rival','Other rival'))
teams<-data.frame(id='r1',teamname='Rival <one>')
players<-data.frame(id='p1',team='Barcelona',logo='barcelona.png')
x<-today_transfer_rows(rows,teams,players)
stopifnot(identical(x$Buyer,c('Rival &lt;one&gt;','Futmondo','Futmondo')),
 identical(x$Seller,c('Futmondo','Captured rival','Other rival')),
 identical(x$Price,c('1.234.568 €','0 €','')),
 grepl('&lt;script&gt;',x$Name[1],fixed=TRUE),!grepl('<script>',x$Name[1],fixed=TRUE),
 grepl('barcelona.png',x$Name[1],fixed=TRUE),!grepl('<img',x$Name[2],fixed=TRUE),
 nrow(today_transfer_rows(rows[rep(1,30),],teams,players))==20,
 nrow(today_transfer_rows(data.frame()))==1)
cat('PASS transfer party names/system sides, integer prices, escaped text, optional crests and row limit\n')

rows$buyer_team_id<-c('r1','r1','r1');rows$seller_team_id<-c('r2','r2','r2')
rows$is_clause<-c(TRUE,FALSE,NA)
rows$player_team_slug<-c('real-sociedad','','');rows$player_team_name<-c('Real Sociedad','','')
x<-today_transfer_rows(rows,teams,players)
stopifnot(identical(x$Type,c('Clause buyout','Buy/sell between rivals','Between rivals · method unconfirmed')),
 grepl('real-sociedad.png',x$Name[1],fixed=TRUE),!grepl('barcelona.png',x$Name[1],fixed=TRUE))
cat('PASS evidence-based clause labels and historical club crest preference\n')

rows$created<-c('2026-11-01T13:25:59.123Z','2026-11-01T14:20:03+01:00','2026-10-30T05:01:00Z')
x<-today_transfer_rows(rows,teams,players)
stopifnot(identical(names(x),c('Date','Name','Price','Buyer','Seller','Type')),
 identical(x$Date,c('01/11/2026 13:25','01/11/2026 13:20','30/10/2026 05:01')))
cat('PASS Date/Name headers and timezone-aware minute precision\n')
