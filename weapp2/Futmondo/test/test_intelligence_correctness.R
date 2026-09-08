#!/usr/bin/env Rscript
# Offline semantic regression tests for ratings, legal lineups and transfer decisions.
suppressPackageStartupMessages(source('intelligence_engine.R'))
checks <- 0L
check <- function(name, expr) {
  force(expr); checks <<- checks + 1L; cat('[PASS]', name, '\n')
}
roster <- data.frame(id=paste0('p',1:14), name=paste0('Player ',1:14),
  role=c('Goalkeeper','Goalkeeper',rep('Defender',5),rep('Midfielder',4),rep('Forward',3)),
  status='ok', team=rep(c('Club A','Club B','Club C'),length.out=14), value=1e6,
  average.average=seq_len(14),average.averageLastFive=seq_len(14),average.matches=5,
  points=5*seq_len(14),change=0,stringsAsFactors=FALSE)
forecast <- data.frame(player_id=roster$id,horizon=1,expected_points=seq_len(14),lower=seq_len(14)-2,upper=seq_len(14)+2)
check('rating is identical across cohorts and row orders', {
  all <- calculate_fis_score(roster); single <- calculate_fis_score(roster[6,,drop=FALSE])
  stopifnot(identical(all$fis_score[6],single$fis_score),identical(all$fis_score,rev(calculate_fis_score(roster[14:1,])$fis_score)))
})
check('missing match data remains unavailable', {
  x <- calculate_fis_score(data.frame(id=c('a','b'),name=c('A','B')))
  stopifnot(all(is.na(x$fis_score)),all(x$fis_tier=='Unavailable'),all(x$fis_status=='unavailable'))
})
check('bad and unnormalized weights remain bounded', {
  x <- calculate_fis_score(roster,list(perf=5,form=-2,efficiency=Inf,momentum=NA,fixture_risk=0))
  stopifnot(all(is.finite(x$fis_score)),all(x$fis_score>=0 & x$fis_score<=100))
})
check('fixture and safe modes exclude known injury', {
  x<-roster; x$status[2]<-'injured'
  for(mode in c('fixture','safe','max_fis','upside')) {
    result<-optimize_starting_xi(x,mode=mode,forecast_df=forecast)
    stopifnot(result$feasible,'p1'%in%result$starting_xi$id,!'p2'%in%result$starting_xi$id)
  }
})
check('short squads and absent positions are infeasible', {
  stopifnot(!optimize_starting_xi(roster[1,,drop=FALSE])$feasible,
            !optimize_starting_xi(roster[-c(1,2),])$feasible)
})
check('exact XI agrees with independent position-wise optimum', {
  result<-optimize_starting_xi(roster,forecast_df=forecast)
  expected<-c(2,4:7,9:11,12:14)
  stopifnot(result$feasible,setequal(result$starting_xi$id,paste0('p',expected)),
            result$expected_points==sum(expected),sum(result$formation_counts)==11)
})
check('multiposition player fills required primary position', {
  x<-roster[c(1,3:5,8:10,12:14,11),];x$role[nrow(x)]<-'Defender, Midfielder'
  result<-optimize_starting_xi(x,rules=list(multiposition=TRUE),forecast_df=forecast)
  stopifnot(result$feasible,result$starting_xi$pos_group[result$starting_xi$id=='p11']=='DEF')
})
check('club caps captain lock and formation rules are enforced', {
  result<-optimize_starting_xi(roster,formation='auto',rules=list(formations='4-3-3',club_limit=5,captain_enabled=TRUE,captain_multiplier=2,bench_size=2),forecast_df=forecast,locked_ids='p1')
  stopifnot(result$feasible,'p1'%in%result$starting_xi$id,max(table(result$starting_xi$team))<=5,
            result$captain_id=='p14',length(result$bench_order)==2,
            result$expected_points==sum(result$starting_xi$expected_points)+14,
            !optimize_starting_xi(roster,formation='3-5-2',rules=list(formations='4-3-3'))$feasible,
            !optimize_starting_xi(roster,locked_ids='p1',excluded_ids='p1')$feasible)
})
check('disabled multiposition respects primary role even after display role merging', {
  x<-roster[c(1,3:5,8:10,12:14,11),]
  x$primary_role<-x$role;x$role2<-NA_character_
  x$role[nrow(x)]<-'Defender, Midfielder';x$role2[nrow(x)]<-'DF'
  stopifnot(!optimize_starting_xi(x,forecast_df=forecast)$feasible,
            !optimize_starting_xi(x,rules=list(multiposition=FALSE),forecast_df=forecast)$feasible,
            optimize_starting_xi(x,rules=list(multiposition=TRUE),forecast_df=forecast)$feasible)
  x$primary_role[nrow(x)]<-'DF'
  stopifnot(optimize_starting_xi(x,rules=list(multiposition=FALSE),forecast_df=forecast)$feasible)
})
check('enabled captain defaults to double points and disabled bench stays empty', {
  x<-optimize_starting_xi(roster,rules=list(captain_enabled=TRUE,bench_enabled=FALSE,bench_size=3),forecast_df=forecast)
  stopifnot(x$expected_points==sum(x$starting_xi$expected_points)+14,length(x$bench_order)==0)
  stopifnot(length(optimize_starting_xi(roster,forecast_df=forecast)$bench_order)==0)
})
check('ordered bench excludes injured excluded and positionless reserves', {
  x<-roster;x$status[1]<-'injured';x$role[3]<-'unknown'
  result<-optimize_starting_xi(x,rules=list(bench_enabled=TRUE,bench_size=5),forecast_df=forecast)
  stopifnot(result$feasible,!any(c('p1','p3')%in%result$bench_order),
    all(result$bench_order%in%result$bench$id),length(result$bench_order)==1)
})
check('unknown bench size preserves valid XI without fabricated reserve eligibility', {
  x<-optimize_starting_xi(roster,rules=list(bench_enabled=TRUE,bench_size=NA),forecast_df=forecast)
  stopifnot(x$feasible,length(x$bench_order)==0)
})
check('missing predictive intervals remain unknown and safe ordering uses points before ratings', {
  f<-forecast;f$lower<-f$upper<-NA_real_
  x<-optimize_starting_xi(roster,mode='safe',forecast_df=f)
  stopifnot(x$feasible,is.na(x$lower_points),is.na(x$upper_points),
    setequal(x$starting_xi$id,optimize_starting_xi(roster,forecast_df=forecast)$starting_xi$id))
})
check('explicit unavailable flags cannot enter starting XI or bench', {
  x<-roster;x$available<-TRUE;x$available[1]<-FALSE
  result<-optimize_starting_xi(x,rules=list(bench_enabled=TRUE,bench_size=4),forecast_df=forecast)
  stopifnot(result$feasible,!'p1'%in%result$starting_xi$id,!'p1'%in%result$bench_order)
})
check('duplicate roster IDs fail explicitly', {
  x<-roster;x$id[2]<-x$id[1];stopifnot(!optimize_starting_xi(x)$feasible)
})
market<-roster[14,,drop=FALSE];market$id<-'new';market$name<-'New forward';market$average.average<-30
market$average.averageLastFive<-30;market$points<-150;market$price<-2e6;market$value<-1.5e6;market$extra_market_field<-'listing'
check('heterogeneous transfer frames work and actual asking price is used', {
  result<-simulate_transfer_scenario(roster,1e6,'p12','new',market)
  stopifnot(identical(result$status,'ok'),result$is_lineup_valid,result$projected_budget==0,
            result$total_buy_cost==2e6,nrow(result$projected_squad)==14)
})
check('invalid and missing transfers never return valid empty scenarios', {
  for(result in list(simulate_transfer_scenario(roster,NA),simulate_transfer_scenario(roster,0,buy_player_ids='missing',market_df=market),
      simulate_transfer_scenario(roster,0,sell_player_ids='missing'),simulate_transfer_scenario(roster,0,buy_player_ids='p1',market_df=roster))) {
    stopifnot(!result$is_budget_valid,result$status!='ok',length(result$diagnostics)>0)
  }
})
check('legal XI and zero or negative cash survive scenario evaluation', {
  okay<-simulate_transfer_scenario(roster,0);bad<-simulate_transfer_scenario(roster,-1)
  absent<-simulate_transfer_scenario(roster,0,sell_player_ids=c('p1','p2'))
  stopifnot(okay$is_budget_valid,okay$status=='ok',!bad$is_budget_valid,!absent$is_lineup_valid,absent$status=='invalid')
})
check('transfer recommendations prioritize euro profit and never manufacture ROI', {
  priced<-roster;priced$expected_sale_proceeds<-1e6;priced$executable_sale_price<-1e6
  opportunity<-market;opportunity$expected_sale_proceeds<-5e6
  rec<-recommend_transfers(priced,opportunity,current_budget=2e6)
  stopifnot(nrow(rec)>0,all(rec$expected_profit>0),all(is.na(rec$roi_pct)))
  sole<-priced[-2,];rec2<-recommend_transfers(sole,opportunity,current_budget=2e6)
  stopifnot(!'p1'%in%rec2$sell_id)
})
check('smart bid cannot breach its rational ceiling', {
  p<-roster[1,,drop=FALSE];p$value<-2e6
  result<-calculate_smart_bid(p,'champ',capacity=list(status='ok',funds=list(spendable_budget=20e6)),market_high_bid=5e6)
  stopifnot(result$recommended_bid<=result$max_rational_bid,!result$can_compete,is.na(result$confidence_pct),is.na(result$expected_roi_pct),is.na(result$data_coverage_pct),!result$calibrated)
})
check('sell-only manager profile is valid and has no invented overpayment', {
  tx<-data.frame(id='t',player_id='p',buyer_team_id='',seller_team_id='team',price=1e7,created='2026-08-01T10:00:00Z')
  d<-calculate_manager_dna('team',tx)
  stopifnot(d$total_trades==1,is.na(d$avg_overpayment_pct),!grepl('Error',d$insights))
})
check('holding periods pair repeated ownership cycles chronologically', {
  tx<-data.frame(id=1:4,player_id='p',buyer_team_id=c('team','','team',''),seller_team_id=c('','team','','team'),price=1e6,
    created=c('2026-08-01T10:00:00Z','2026-08-03T10:00:00Z','2026-08-10T10:00:00Z','2026-08-12T10:00:00Z'))
  stopifnot(calculate_manager_dna('team',tx)$avg_holding_days==2)
})
cat('INTELLIGENCE CORRECTNESS:',checks,'passed / 0 failed\n')
