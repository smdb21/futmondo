# Advisory joint plans; never dispatches actions. See docs/portfolio_planner.md.
profit_portfolio_inputs <- function(roster,market,prices,offers,execution,as_of=Sys.time(),context=NULL) {
  all<-dplyr::bind_rows(roster,market)
  all<-all[!duplicated(all$id),,drop=FALSE]
  f<-forecast_resale_values(all,prices,as_of,horizons=7,context=context)
  attach<-function(d) {d$expected_sale_proceeds<-f$expected_value[match(d$id,f$player_id)]*execution$mean;d}
  roster<-attach(roster);market<-attach(market)
  roster$executable_sale_price<-NA_real_
  if(is.data.frame(offers)&&all(c('id','bid_price','bid_id')%in%names(offers))) {
    offers<-offers[is.finite(offers$bid_price)&offers$bid_price>0,,drop=FALSE]
    offers<-offers[order(-offers$bid_price),,drop=FALSE];offers<-offers[!duplicated(offers$id),,drop=FALSE]
    roster$executable_sale_price<-offers$bid_price[match(roster$id,offers$id)]
  }
  sales<-data.frame(player_id=as.character(roster$id),proceeds=roster$executable_sale_price,
    hold_proceeds=roster$expected_sale_proceeds)
  sales<-sales[is.finite(sales$proceeds)&is.finite(sales$hold_proceeds),,drop=FALSE]
  list(roster=roster,market=market,sales=sales)
}

build_profit_candidates <- function(market,model,prices,execution,cash,max_bid,horizon=7,fees=0,as_of=Sys.time(),user_team_id=NULL,context=NULL) {
  if(!is.data.frame(market)||!nrow(market)||!is.finite(execution$mean)||!is.finite(cash)||cash<=0||!is.finite(max_bid))return(data.frame())
  forecasts<-forecast_resale_values(market,prices,as_of,horizons=horizon,context=context)
  rows<-list()
  for(i in seq_len(nrow(market))) {
    p<-market[i,,drop=FALSE];f<-forecasts[forecasts$player_id==p$id,,drop=FALSE]
    minimum<-fm_number(p$effective_market_price %||% p$price, fm_number(p$value))
    ceiling<-min(cash,max_bid)
    if(nrow(f)!=1L||!is.finite(minimum)||minimum<=0||minimum>ceiling||!is.finite(f$expected_value))next
    grid<-unique(round(seq(minimum,ceiling,length.out=41)))
    curve<-predict_auction_win_curve(model,grid,fm_number(p$value),user_id=user_team_id)
    if(!nrow(curve))next
    proceeds<-f$expected_value*execution$mean
    lower<-f$lower_value*execution$lower
    curve$expected_profit<-curve$p_win*(proceeds-curve$bid-fees)
    curve<-curve[is.finite(curve$expected_profit)&curve$expected_profit>0,,drop=FALSE]
    if(!nrow(curve))next
    # A dearer option with no higher expected profit is dominated.
    curve<-curve[order(curve$bid),,drop=FALSE]
    curve<-curve[c(TRUE,diff(cummax(curve$expected_profit))>0),,drop=FALSE]
    curve$player_id<-as.character(p$id);curve$price<-curve$bid
    curve$expected_proceeds<-proceeds;curve$downside_profit<-lower-curve$bid-fees
    curve$horizon<-horizon;curve$offer_observations<-execution$n
    rows[[length(rows)+1L]]<-curve
  }
  dplyr::bind_rows(rows)
}

plan_profit_portfolio <- function(roster,market,candidates,financial,sales=data.frame(),rules=NULL,forecast_df=NULL,beam_width=64L) {
  cash<-fm_number(financial$spendable_budget);cap<-fm_number(financial$roster_cap)
  fail<-function(reason)list(status='unavailable',reason=reason,purchases=data.frame(),sales=data.frame(),expected_profit=NA_real_)
  if(!is.finite(cash)||cash<0||!is.finite(cap)||!is.data.frame(roster))return(fail('Verified cash and roster capacity are required.'))
  rules$exclude_unavailable<-FALSE
  legal<-function(r)optimize_starting_xi(r,formation='auto',rules=rules,forecast_df=forecast_df)
  base<-legal(roster)
  if(!isTRUE(base$feasible))return(fail('A feasible starting XI is required before planning transfers.'))
  states<-list(list(cash=cash,profit=0,downside=0,roster=roster,buy=integer(),sell=integer()))
  trim<-function(states)head(states[order(-vapply(states,function(s)s$profit,numeric(1)),
    -vapply(states,function(s)s$cash,numeric(1)))],beam_width)
  if(is.data.frame(sales)&&all(c('player_id','proceeds','hold_proceeds')%in%names(sales)))for(i in seq_len(nrow(sales))) {
    sale<-sales[i,,drop=FALSE];next_states<-states
    if(!all(is.finite(c(sale$proceeds,sale$hold_proceeds)))||sale$proceeds<0)next
    for(state in states)if(sale$player_id%in%state$roster$id) {
      r<-state$roster[state$roster$id!=sale$player_id,,drop=FALSE]
      if(nrow(r)<11L||!isTRUE(legal(r)$feasible))next
      s<-state;s$roster<-r;s$cash<-s$cash+sale$proceeds;s$profit<-s$profit+sale$proceeds-sale$hold_proceeds
      s$downside<-NA_real_;s$sell<-c(s$sell,i);next_states[[length(next_states)+1L]]<-s
    }
    states<-trim(next_states)
  }
  if(is.data.frame(candidates)&&nrow(candidates))for(id in unique(as.character(candidates$player_id))) {
    options<-which(candidates$player_id==id);next_states<-states
    p<-market[market$id==id,,drop=FALSE];if(nrow(p)!=1L)next
    for(state in states) {
      if(id%in%state$roster$id||nrow(state$roster)>=cap)next
      for(i in options) {
        c<-candidates[i,,drop=FALSE]
        if(!is.finite(c$price)||c$price>state$cash||!is.finite(c$expected_profit)||c$expected_profit<=0)next
        s<-state;s$roster<-dplyr::bind_rows(s$roster,p);s$cash<-s$cash-c$price
        s$profit<-s$profit+c$expected_profit;s$downside<-s$downside+fm_number(c$downside_profit)
        s$buy<-c(s$buy,i);next_states[[length(next_states)+1L]]<-s
      }
    }
    states<-trim(next_states)
  }
  best<-states[[1]];xi<-legal(best$roster)
  buys<-if(length(best$buy))candidates[best$buy,,drop=FALSE] else data.frame()
  sold<-if(length(best$sell))sales[best$sell,,drop=FALSE] else data.frame()
  list(status=if(length(best$buy)||length(best$sell))'scenario' else 'no_bid',
    reason='Bounded joint search; no guarantee of a global optimum.',
    purchases=buys,sales=sold,cash_remaining=best$cash,expected_profit=best$profit,
    downside_profit=best$downside,projected_roster=best$roster,lineup=xi,
    points_change=xi$expected_points-base$expected_points,
    requires_sale_settlement=nrow(sold)>0,execution_available=FALSE,
    search_method='beam_search',beam_width=beam_width)
}

realized_trade_ledger <- function(transactions,user_team_id,season_start=NULL) {
  if(!is.data.frame(transactions)||!all(c('player_id','price','buyer_team_id','seller_team_id','created')%in%names(transactions)))return(data.frame())
  d<-transactions
  if('id'%in%names(d))d<-d[!duplicated(d$id),,drop=FALSE]
  d<-d[order(fm_time(d$created)),,drop=FALSE]
  costs<-list();rows<-list()
  for(i in seq_len(nrow(d))) {
    r<-d[i,,drop=FALSE];id<-as.character(r$player_id);price<-fm_number(r$price)
    if(!is.finite(price)||is.na(fm_time(r$created)))next
    buyer<-fm_scalar(r$buyer_team_id,'');seller<-fm_scalar(r$seller_team_id,'')
    if(buyer==user_team_id)costs[[id]]<-price
    if(seller==user_team_id) {
      cost<-costs[[id]];if(is.null(cost))cost<-NA_real_
      if(is.null(season_start)||fm_time(r$created)>=fm_time(season_start))rows[[length(rows)+1L]]<-data.frame(
        player_id=id,sold_at=as.character(r$created),proceeds=price,cost_basis=cost,
        realized_profit=price-cost,counterparty=if(nzchar(buyer))buyer else 'Futmondo (System)')
      costs[[id]]<-NULL
    }
  }
  dplyr::bind_rows(rows)
}
