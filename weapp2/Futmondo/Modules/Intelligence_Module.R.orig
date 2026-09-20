# Read-only forecasts and rival-bid analysis; trade execution remains in existing action modules.
intelligence_value <- function(x) {
  if (is.function(x)) return(x())
  x
}

# Align observed auction data without inferring unobserved participation.
intelligence_prepare_auctions <- function(auctions, bids, price_history = NULL, championship_id = '') {
  if (!is.data.frame(auctions)) auctions <- data.frame()
  if (!is.data.frame(bids)) bids <- data.frame()
  alias <- function(df, target, alternatives, default = NA) {
    if (!target %in% names(df)) {
      source <- alternatives[alternatives %in% names(df)]
      df[[target]] <- if(length(source)) df[[source[1]]] else rep(default,nrow(df))
    }
    df
  }
  auctions <- alias(auctions,'auction_id',c('event_id','id'),'')
  auctions <- alias(auctions,'settled_at',c('created','transaction_date'))
  auctions <- alias(auctions,'observed_at',c('captured_at'))
  auctions <- alias(auctions,'championship_id',character(),championship_id)
  auctions <- alias(auctions,'visibility',c('bid_visibility'),'unknown')
  auctions <- alias(auctions,'reference_value',character(),NA_real_)
  auctions <- alias(auctions,'reference_observed_at',character())
  if (!'eligible_manager_ids' %in% names(auctions)) auctions$eligible_manager_ids <- I(rep(list(character()),nrow(auctions)))
  bids <- alias(bids,'auction_id',c('event_id'),'')
  bids <- alias(bids,'bidder_id',c('manager_id'),'')
  bids <- alias(bids,'amount',c('price'),NA_real_)
  bids <- alias(bids,'settled_at',c('created'))
  bids <- alias(bids,'observed_at',c('captured_at'))
  bids <- alias(bids,'championship_id',character(),championship_id)
  if (is.data.frame(price_history) && all(c('player_id','value','observed_at') %in% names(price_history)) && 'player_id' %in% names(auctions)) {
    history_time <- as.numeric(analytics_time(price_history$observed_at))
    if (inherits(price_history$observed_at,'POSIXt') || is.numeric(price_history$observed_at)) history_time <- as.numeric(price_history$observed_at)
    capture_time <- if('captured_at'%in%names(price_history)) .pe_time(price_history$captured_at) else history_time
    settlement <- as.numeric(analytics_time(auctions$settled_at))
    if (inherits(auctions$settled_at,'POSIXt') || is.numeric(auctions$settled_at)) settlement <- as.numeric(auctions$settled_at)
    for (i in seq_len(nrow(auctions))) if (!is.finite(suppressWarnings(as.numeric(auctions$reference_value[i])))) {
      same_context <- rep(TRUE,nrow(price_history))
      for(field in c('championship_id','season','scoring_version')) if(field%in%names(price_history)&&field%in%names(auctions)) {
        key<-as.character(auctions[[field]][i])
        same_context<-same_context & !is.na(price_history[[field]]) & !is.na(key) & as.character(price_history[[field]])==key
      }
      idx <- which(same_context & as.character(price_history$player_id)==as.character(auctions$player_id[i]) &
        is.finite(history_time) & history_time < settlement[i] & history_time >= settlement[i]-86400 & is.finite(capture_time) & capture_time < settlement[i] & is.finite(price_history$value) & price_history$value>0)
      if (length(idx)) {
        j <- idx[which.max(history_time[idx])]
        auctions$reference_value[i] <- price_history$value[j]
        auctions$reference_observed_at[i] <- format(as.POSIXct(history_time[j],origin='1970-01-01',tz='UTC'),'%Y-%m-%dT%H:%M:%OSZ',tz='UTC')
      }
    }
  }
  list(auctions=auctions,bids=bids)
}

intelligence_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h2('Intelligence'),
    shiny::p('Compare rival bidding patterns, resale scenarios and the strongest legal lineup.'),
    shiny::fluidRow(
      shiny::column(8,shiny::selectizeInput(ns('player_id'),'Market player',choices=NULL,width='100%')),
      shiny::column(4,shiny::actionButton(ns('reload'),'Refresh analysis',icon=shiny::icon('rotate')))
    ),
    shiny::uiOutput(ns('coverage')),
    shiny::tabsetPanel(
      shiny::tabPanel('Rival bids',
        shiny::p('Historical bids are observed after completion. Future participation and bid amounts are estimates.'),
        shiny::div(style='overflow-x:auto;',shiny::tableOutput(ns('rivals'))),
        plotly::plotlyOutput(ns('win_curve'),height='300px'),shiny::uiOutput(ns('curve_note'))),
      shiny::tabPanel('Profit planner',
        shiny::fluidRow(
          shiny::column(4,shiny::selectInput(ns('horizon'),'Resale horizon',choices=c('1 day'=1,'3 days'=3,'7 days'=7),selected=7)),
          shiny::column(4,shiny::numericInput(ns('resale_percent'),'Assumed resale offer (% of future value)',value=95,min=1,max=100,step=1)),
          shiny::column(4,shiny::numericInput(ns('fees'),'Scenario fees (€)',value=0,min=0,step=1000))
        ),
        shiny::p('The resale percentage is a scenario assumption. A future market value is not a guaranteed sale offer.'),
        shiny::uiOutput(ns('profit_summary')),
        shiny::div(style='overflow-x:auto;',shiny::tableOutput(ns('resale'))),
        shiny::div(style='overflow-x:auto;',shiny::tableOutput(ns('profit_candidates'))),
        shiny::h4('Joint seven-day profit plan'),
        shiny::p('Uses observed market offers. Sale-funded bids are conditional on the sales settling first.'),
        shiny::actionButton(ns('build_plan'),'Compare purchases and sales'),
        shiny::uiOutput(ns('portfolio_summary')),shiny::tableOutput(ns('portfolio_buys')),
        shiny::tableOutput(ns('portfolio_sales')),shiny::tableOutput(ns('realized_profit'))),
      shiny::tabPanel('Model performance',shiny::p('Settled outcomes compared with predictions saved before the event. Models remain advisory.'),
        shiny::tableOutput(ns('model_performance'))),
      shiny::tabPanel('Best XI',
        shiny::p('The lineup uses expected next-round points and verified eligibility. Missing forecasts remain visible.'),
        shiny::uiOutput(ns('lineup_summary')),
        shiny::div(style='overflow-x:auto;',shiny::tableOutput(ns('lineup'))),
        shiny::h4('Advisory bench order'),
        shiny::p('Eligible reserves are ordered by projected points. Substitution outcomes depend on position, eligibility and the league’s automatic-bench rules.'),shiny::div(style='overflow-x:auto;',shiny::tableOutput(ns('bench'))),
        shiny::h4('Next three rounds'),shiny::div(style='overflow-x:auto;',shiny::tableOutput(ns('points_horizons'))))
    )
  )
}

intelligence_Server <- function(id,is_module_active,login_token,championship_id,user_team_id,
                                refresh_trigger=NULL,loaders=NULL) {
  shiny::moduleServer(id,function(input,output,session) {
    get <- function(name,args=list(),default=data.frame()) {
      handler <- if(is.list(loaders)&&is.function(loaders[[name]])) loaders[[name]] else if(exists(name,mode='function')) base::get(name,mode='function',inherits=TRUE) else NULL
      if(is.null(handler)) return(default)
      result<-tryCatch(do.call(handler,args),error=function(e) default)
      if(is.null(result)) default else result
    }
    loaded <- shiny::reactive({
      shiny::req(isTRUE(intelligence_value(is_module_active)))
      login<-intelligence_value(login_token); champ<-intelligence_value(championship_id); team<-intelligence_value(user_team_id)
      shiny::req(login,champ,team)
      input$reload
      if(is.function(refresh_trigger)) refresh_trigger()
      now<-Sys.time(); args<-list(login=login,championship_id=champ,user_team_id=team)
      finance<-get('get_financial_snapshot',args,default=list(status='unavailable'))
      context<-list(championship_id=as.character(champ))
      for(field in c('season','scoring_version')) {
        value<-finance$rules[[field]]
        if(length(value)==1L&&!is.na(value)&&nzchar(as.character(value))) context[[field]]<-as.character(value)
      }
      scope_frame<-function(df,fields=c('championship_id','season','scoring_version'),live=FALSE) {
        if(!is.data.frame(df)) return(data.frame())
        scoped<-.pe_context_data(list(df),context,columns=fields)
        if(live) .pe_add_context(scoped$data[[1]],scoped$context) else scoped$data[[1]]
      }
      market<-scope_frame(get('get_market_players',args),live=TRUE); roster<-scope_frame(get('get_players_from_team',args),live=TRUE)
      for(kind in c('market','roster')) {
        df<-if(kind=='market') market else roster
        if(is.data.frame(df)&&nrow(df)) {
          if(exists('translate_player_positions',mode='function')) df<-translate_player_positions(df)
          if(!'observed_at'%in%names(df)) {
            source_time<-attr(df,'observed_at')
            df$observed_at<-if(length(source_time)==1L&&!is.na(source_time)) source_time else NA_character_
          }
        }
        if(kind=='market') market<-df else roster<-df
      }
      prices<-scope_frame(get('read_player_price_history',list(championship_id=champ)))
      auctions<-get('read_model_auctions',list(championship_id=champ))
      if(!is.data.frame(auctions)||!nrow(auctions)) auctions<-get('read_auction_observations',list(championship_id=champ))
      bids<-get('read_bid_observations',list(championship_id=champ))
      if(!nrow(auctions)) {
        press<-get('get_championship_pressroom',list(login=login,championship_id=champ))
        if(is.data.frame(press)&&nrow(press)&&all(c('seller_team_id','buyer_team_id')%in%names(press))) {
          is_system<-is.na(press$seller_team_id)|!nzchar(as.character(press$seller_team_id))
          auctions<-press[is_system&!is.na(press$buyer_team_id)&nzchar(as.character(press$buyer_team_id)),,drop=FALSE]
          auctions$observed_at<-now
          bids<-get('normalize_bid_observations',list(pressroom_df=auctions,championship_id=champ))
        }
      }
      auctions<-scope_frame(auctions);bids<-scope_frame(bids)
      prepared<-intelligence_prepare_auctions(auctions,bids,prices,as.character(champ))
      teams<-get('get_teams',list(login=login,championship_id=champ))
      model<-tryCatch(fit_rival_bid_model(prepared$auctions,prepared$bids,manager_ids=if(is.data.frame(teams)&&'id'%in%names(teams)) teams$id else NULL,as_of=now,context=context),error=function(e) list(managers=data.frame(),metadata=list(auctions=0,bids=0,premium_samples=0)))
      matches<-scope_frame(get('read_player_match_history',c(list(championship_id=champ,cutoff=now),context[intersect(c('season','scoring_version'),names(context))])))
      account_id<-if('userid'%in%names(login)) as.character(login[['userid']]) else character()
      if(length(account_id)!=1L||is.na(account_id)||!nzchar(account_id)) account_id<-NULL
      sale_history<-get('read_sale_observations',list(user_id=account_id,championship_id=champ))
      offers<-get('get_roster_bids',args)
      list(market=market,roster=roster,finance=finance,prices=prices,model=model,teams=teams,matches=matches,loaded_at=now,
        sale_history=sale_history,offers=offers,
        team_id=as.character(team),account_id=account_id,championship_id=as.character(champ),context=context)
    })
    selection_scope<-shiny::reactiveVal(NULL)
    selected_id<-shiny::reactiveVal(NULL)
    scenario_settings<-shiny::reactiveVal(list(horizon=7,resale_percent=95,fees=0))
    shiny::observe({
      d<-loaded();m<-d$market
      key<-paste(d$account_id,d$championship_id,d$team_id,sep=':')
      changed<-!identical(shiny::isolate(selection_scope()),key)
      current<-shiny::isolate(selected_id())
      choices<-if(is.data.frame(m)&&nrow(m)&&all(c('id','name')%in%names(m))) stats::setNames(as.character(m$id),as.character(m$name)) else character()
      if(changed||is.null(current)||!current%in%choices) current<-if(length(choices)) unname(choices[1]) else NULL
      selection_scope(key);selected_id(current)
      shiny::updateSelectizeInput(session,'player_id',choices=choices,selected=current,server=TRUE)
      if(changed) {
        scenario_settings(list(horizon=7,resale_percent=95,fees=0))
        shiny::updateSelectInput(session,'horizon',selected=7)
        shiny::updateNumericInput(session,'resale_percent',value=95)
        shiny::updateNumericInput(session,'fees',value=0)
      }
    },priority=1000)
    shiny::observeEvent(input$player_id,{
      m<-loaded()$market
      if(is.data.frame(m)&&'id'%in%names(m)&&input$player_id%in%as.character(m$id)) selected_id(as.character(input$player_id))
    },ignoreNULL=TRUE)
    shiny::observeEvent(list(input$horizon,input$resale_percent,input$fees),{
      old<-scenario_settings()
      valid<-function(x,minimum,maximum) length(x)==1L&&is.finite(suppressWarnings(as.numeric(x)))&&as.numeric(x)>=minimum&&as.numeric(x)<=maximum
      if(valid(input$horizon,1,7)&&as.numeric(input$horizon)%in%c(1,3,7)) old$horizon<-as.numeric(input$horizon)
      if(valid(input$resale_percent,1,100)) old$resale_percent<-as.numeric(input$resale_percent)
      if(valid(input$fees,0,Inf)) old$fees<-as.numeric(input$fees)
      scenario_settings(old)
    })
    selected<-shiny::reactive({
      m<-loaded()$market;id<-selected_id()
      if(!is.data.frame(m)||!nrow(m)||is.null(id)) return(data.frame())
      m[as.character(m$id)%in%as.character(id),,drop=FALSE]
    })
    money<-function(x) if(length(x)!=1L||!is.finite(x)) 'Unavailable' else paste0(format(round(x),big.mark=',',scientific=FALSE,trim=TRUE),' €')
    curve<-shiny::reactive({
      d<-loaded();p<-selected()
      if(!nrow(p)||!'value'%in%names(p)||!is.finite(as.numeric(p$value[1]))||p$value[1]<=0) return(data.frame())
      value<-as.numeric(p$value[1]); minimum<-if('price'%in%names(p)&&is.finite(as.numeric(p$price[1]))&&p$price[1]>0) as.numeric(p$price[1]) else value
      grid<-unique(round(seq(minimum,max(minimum,value*1.75),length.out=81)))
      tryCatch(predict_auction_win_curve(d$model,grid,value,user_id=d$team_id),error=function(e) data.frame())
    })
    resale<-shiny::reactive({
      d<-loaded();p<-selected();if(!nrow(p)) return(data.frame())
      ratio<-scenario_settings()$resale_percent/100
      fees<-scenario_settings()$fees
      tryCatch(forecast_resale_values(p,d$prices,as_of=d$loaded_at,horizons=c(1,3,7),execution_ratio=ratio,fees=fees,context=d$context),error=function(e)data.frame())
    })
    profit<-shiny::reactive({
      d<-loaded();r<-resale();c<-curve();h<-scenario_settings()$horizon
      r<-if(nrow(r)&&length(h)==1) r[r$horizon==h,,drop=FALSE] else data.frame()
      cash<-if(identical(d$finance$status,'ok')) d$finance$spendable_budget else NA_real_
      if(length(cash)!=1L||!is.finite(cash)) cash<-NA_real_
      limit<-d$finance$legal_bid_limit;if(length(limit)!=1L||!is.finite(limit)) limit<-cash
      if(!nrow(r)) return(list(action='no_bid',reason='insufficient_history',recommended_bid=0,expected_profit=0,candidates=data.frame()))
      result<-select_profit_bid(c,r$conservative_proceeds[1],spendable_cash=cash,max_bid=limit,
        expected_proceeds=r$expected_value[1]*r$execution_ratio[1]-r$fees[1])
      if(identical(result$action,'bid')) {
        purchase<-selected();purchase$price<-result$recommended_bid
        cap<-d$finance$roster_cap
        if(length(cap)!=1L||!is.finite(cap)) cap<-NA_real_
        basket<-evaluate_trade_basket(d$roster,purchases=purchase,verified_cash=cash,max_roster=cap,
          legal_check=function(players) { rules<-d$finance$lineup_rules;if(is.null(rules)) rules<-list();rules$exclude_unavailable<-FALSE;optimize_starting_xi(players,formation='auto',rules=rules) })
        result$basket<-basket
        if(!isTRUE(basket$valid)) {result$action<-'no_bid';result$recommended_bid<-0;result$expected_profit<-0;result$reason<-'legal_roster_or_capacity_unverified'}
      }
      result
    })
    point_forecasts<-shiny::reactive({
      d<-loaded();r<-d$roster
      if(!is.data.frame(r)||!nrow(r)) return(data.frame())
      forecast_fantasy_points(r,d$matches,as_of=d$loaded_at,horizons=c(1,3),context=d$context)
    })
    lineup<-shiny::reactive({
      d<-loaded();r<-d$roster
      if(!is.data.frame(r)||!nrow(r)) return(list(feasible=FALSE,diagnostics='Roster unavailable.',starting_xi=data.frame()))
      rules<-d$finance$lineup_rules
      optimize_starting_xi(r,formation='auto',mode='expected',forecast_df=point_forecasts(),rules=rules)
    })
    # The persistence provider queues private records; no external API write happens here.
    shiny::observe({
      d<-loaded()
      if(is.null(d$account_id)) return(invisible(NULL))
      persist_rows<-function(rows,type,version) {
        if(!is.data.frame(rows)||!nrow(rows)) return(invisible(NULL))
        for(i in seq_len(nrow(rows))) get('persist_forecast',list(user_id=d$account_id,
          championship_id=d$championship_id,player_id=as.character(rows$player_id[i]),
          model_version=version,forecast_type=type,cutoff=d$loaded_at,horizon=rows$horizon[i],
          prediction=c(as.list(rows[i,,drop=FALSE]),list(league_rules=d$finance$rules)),
          season=d$context$season,scoring_version=d$context$scoring_version),default=FALSE)
      }
      persist_rows(point_forecasts(),'fantasy_points','points-shrinkage-v1')
      persist_rows(resale(),'resale_value','resale-log-return-v1')
      p<-selected();c<-curve()
      if(nrow(p)&&nrow(c)) get('persist_forecast',list(user_id=d$account_id,championship_id=d$championship_id,
        player_id=as.character(p$id[1]),model_version='rival-bids-v1',forecast_type='rival_bid',
        cutoff=d$loaded_at,horizon=1,season=d$context$season,scoring_version=d$context$scoring_version,
        prediction=list(reference_value=p$value[1],curve=c,
          listing_id=if('expirationDate'%in%names(p)&&!is.na(p$expirationDate[1]))paste(p$id[1],p$expirationDate[1],sep=':') else NULL,
          league_rules=d$finance$rules,
          rivals=d$model$managers[d$model$managers$manager_id!=d$team_id,,drop=FALSE],metadata=d$model$metadata)),default=FALSE)
    })
    output$coverage<-shiny::renderUI({
      d<-loaded();meta<-d$model$metadata
      shiny::div(class='well',shiny::strong(paste0('League ',d$championship_id,' · Available cash: ',money(d$finance$spendable_budget))),
        shiny::p(paste(meta$auctions,'historical auctions ·',meta$bids,'observed bids ·',meta$premium_samples,'bids with an earlier value reference.')),
        if(!nrow(d$model$managers)||any(!is.finite(d$model$managers$participation_probability)))
          shiny::p('Win probabilities require verified eligible managers and complete settled-auction visibility. More winning prices alone cannot supply that evidence.'),
        shiny::tags$small(paste('Loaded',format(d$loaded_at,'%Y-%m-%d %H:%M UTC',tz='UTC'),'. Estimates need sufficient visible history; unavailable results are left blank.')))
    })
    output$rivals<-shiny::renderTable({
      d<-loaded();m<-d$model$managers
      if(!is.data.frame(m)||!nrow(m)) return(data.frame(Status='Rival history is not available yet.'))
      m<-m[as.character(m$manager_id)!=d$team_id,,drop=FALSE]
      names<-as.character(m$manager_id);teams<-d$teams
      if(is.data.frame(teams)&&nrow(teams)&&'id'%in%names(teams)) {
        column<-intersect(c('teamname','name'),names(teams))
        if(length(column)) {matched<-as.character(teams[[column[1]]][match(names,as.character(teams$id))]);use<-!is.na(matched)&nzchar(matched);names[use]<-matched[use]}
      }
      p<-selected();value<-if(nrow(p)&&'value'%in%names(p)) suppressWarnings(as.numeric(p$value[1])) else NA_real_
      last_seen<-if('last_observed_at'%in%names(m)) format(as.POSIXct(m$last_observed_at,origin='1970-01-01',tz='UTC'),'%Y-%m-%d %H:%M UTC',tz='UTC') else rep(NA_character_,nrow(m))
      data.frame(Manager=names,'Estimated participation (%)'=round(100*m$participation_probability,1),
        'Conditional median bid (€)'=round(value*m$conditional_bid_ratio_median),
        'Conditional low bid (€)'=round(value*m$conditional_bid_ratio_low),
        'Conditional high bid (€)'=round(value*m$conditional_bid_ratio_high),
        'Observed bids'=m$observed_bids,'Value references'=m$premium_samples,
        'Last observed bid'=last_seen,check.names=FALSE)
    },na='Unavailable',striped=TRUE,bordered=TRUE)
    output$win_curve<-plotly::renderPlotly({
      c<-curve()
      if(!nrow(c)||!any(is.finite(c$p_win))) return(plotly::plot_ly(x=numeric(),y=numeric(),type='scatter',mode='lines')%>%fm_plot_layout(xaxis=list(visible=FALSE),yaxis=list(visible=FALSE),annotations=list(list(text='More complete auction history is needed for win probabilities.',showarrow=FALSE,x=.5,y=.5,xref='paper',yref='paper'))))
      plotly::plot_ly(c,x=~bid/1e6,y=~p_win,type='scatter',mode='lines',name='Estimated win probability')%>%
        plotly::add_ribbons(ymin=~p_win_low,ymax=~p_win_high,name='Model uncertainty',opacity=.15)%>%
        fm_plot_layout(xaxis=list(title='Your bid (€ millions)'),yaxis=list(title='Estimated chance',range=c(0,1),tickformat='.0%'))
    })
    output$curve_note<-shiny::renderUI(shiny::p('The curve combines each rival’s estimated participation and bid range. Its uncertainty is model-based and has not been calibrated against future auctions.'))
    output$profit_summary<-shiny::renderUI({
      p<-profit()
      if(identical(p$action,'bid')) shiny::div(class='well',shiny::strong(paste('Scenario bid:',money(p$recommended_bid))),shiny::p(paste('Expected scenario profit:',money(p$expected_profit),'· Downside:',money(p$downside_profit))),shiny::p('Check the current auction, roster and executable sale terms before placing a bid.'))
      else shiny::div(class='well',shiny::strong('No supported profitable bid in this scenario.'),shiny::p('A recommendation requires available funds, a supported win curve and sufficient resale history.'))
    })
    output$resale<-shiny::renderTable({
      r<-resale();if(!nrow(r)) return(data.frame(Status='Select a player to inspect resale scenarios.'))
      data.frame('Days'=r$horizon,'Expected value (€)'=round(r$expected_value),'Lower scenario (€)'=round(r$lower_value),
        'Upper scenario (€)'=round(r$upper_value),'Conservative proceeds (€)'=round(r$conservative_proceeds),check.names=FALSE)
    },na='Unavailable',digits=0,striped=TRUE)
    output$profit_candidates<-shiny::renderTable({
      c<-profit()$candidates;if(!is.data.frame(c)||!nrow(c)) return(NULL)
      c<-head(c,5);data.frame('Bid (€)'=round(c$bid),'Estimated win (%)'=round(100*c$p_win,1),'Scenario profit (€)'=round(c$expected_profit),check.names=FALSE)
    },striped=TRUE)
    output$lineup_summary<-shiny::renderUI({
      x<-lineup()
      if(!isTRUE(x$feasible)) return(shiny::p(paste(x$diagnostics,collapse=' ')))
      shiny::p(paste0(if(isTRUE(x$legality_verified)) 'Verified XI · ' else 'Provisional XI · ',
        'Formation ',x$formation,' · Expected points: ',if(is.finite(x$expected_points)) round(x$expected_points,1) else 'Unavailable',
        if(is.character(x$captain_id)&&!is.na(x$captain_id)) paste0(' · Captain: ',x$starting_xi$name[match(x$captain_id,x$starting_xi$id)]) else ''))
    })
    output$lineup<-shiny::renderTable({
      x<-lineup();if(!isTRUE(x$feasible)) return(NULL)
      s<-x$starting_xi;data.frame(Player=if('name'%in%names(s))s$name else s$id,Position=s$pos_group,
        'Expected points'=round(s$expected_points,1),'Lower scenario'=round(s$forecast_lower,1),'Upper scenario'=round(s$forecast_upper,1),check.names=FALSE)
    },na='Unavailable',striped=TRUE)
    output$bench<-shiny::renderTable({
      x<-lineup()
      if(!isTRUE(x$feasible)||!length(x$bench_order)) return(data.frame(Status='No verified enabled bench configuration or available reserves.'))
      b<-x$bench[match(x$bench_order,x$bench$id),,drop=FALSE]
      data.frame(Order=seq_len(nrow(b)),Player=if('name'%in%names(b))b$name else b$id,
        'Expected points'=round(b$expected_points,1),check.names=FALSE)
    },na='Unavailable',striped=TRUE)
    output$points_horizons<-shiny::renderTable({
      f<-point_forecasts();if(!nrow(f)) return(NULL)
      f<-f[f$horizon==3,,drop=FALSE];r<-loaded()$roster
      data.frame(Player=if('name'%in%names(r))r$name[match(f$player_id,r$id)] else f$player_id,
        'Expected points (3 rounds)'=round(f$expected_points,1),'Observed rounds'=f$n_observations,
        'Evidence'=f$method,check.names=FALSE)
    },na='Unavailable',striped=TRUE)
    portfolio <- shiny::eventReactive(input$build_plan, {
      d<-loaded();execution<-fit_sale_execution(d$sale_history,d$loaded_at)
      if(!is.finite(execution$mean))return(list(status='unavailable',reason=paste('At least five distinct system offers are needed; observed:',execution$n)))
      inputs<-profit_portfolio_inputs(d$roster,d$market,d$prices,d$offers,execution,d$loaded_at,d$context)
      potential_cash<-fm_number(d$finance$spendable_budget)+sum(inputs$sales$proceeds)
      candidates<-build_profit_candidates(inputs$market,d$model,d$prices,execution,potential_cash,
        fm_number(d$finance$legal_bid_limit),as_of=d$loaded_at,user_team_id=d$team_id,context=d$context)
      finance<-d$finance
      pending_slots<-fm_number(finance$commitments$count)
      if(!is.finite(pending_slots)||!identical(finance$commitments$completeness,'complete'))
        return(list(status='unavailable',reason='Outstanding acquisition commitments are not fully known.'))
      finance$roster_cap<-fm_number(finance$roster_cap)-pending_slots
      plan_profit_portfolio(inputs$roster,inputs$market,candidates,finance,inputs$sales,
        rules=d$finance$lineup_rules,forecast_df=point_forecasts())
    },ignoreInit=TRUE)
    output$portfolio_summary<-shiny::renderUI({
      p<-portfolio()
      if(p$status=='unavailable')return(shiny::p(p$reason))
      shiny::tagList(shiny::p('Expected incremental profit: ',money(p$expected_profit),
        ' · Downside: ',money(p$downside_profit),' · Cash after all wins: ',money(p$cash_remaining)),
        shiny::p('Projected XI change: ',round(p$points_change,1),' points. ',p$reason),
        if(p$requires_sale_settlement)shiny::p('Conditional plan: complete the listed sales before funding these bids.'),
        shiny::p('Advisory plan. League deadline and full eligibility must be verified before execution.'))
    })
    output$portfolio_buys<-shiny::renderTable({
      p<-portfolio();b<-p$purchases;if(!is.data.frame(b)||!nrow(b))return(NULL)
      data.frame(Player=b$player_id,'Bid (€)'=b$price,'Expected profit (€)'=b$expected_profit,'Downside (€)'=b$downside_profit,check.names=FALSE)
    },na='Unavailable',digits=0)
    output$portfolio_sales<-shiny::renderTable({
      s<-portfolio()$sales;if(!is.data.frame(s)||!nrow(s))return(NULL)
      data.frame(Player=s$player_id,'Sale proceeds (€)'=s$proceeds,check.names=FALSE)
    },digits=0)
    output$realized_profit<-shiny::renderTable({
      d<-loaded();tx<-get('get_championship_pressroom',list(login=intelligence_value(login_token),championship_id=d$championship_id))
      ledger<-realized_trade_ledger(tx,d$team_id)
      wealth<-if(is.finite(fm_number(d$finance$cash))&&all(is.finite(d$roster$value))) d$finance$cash+sum(d$roster$value) else NA_real_
      data.frame('Observed wealth (€)'=wealth,'Realized profit, known costs (€)'=if(nrow(ledger))sum(ledger$realized_profit,na.rm=TRUE) else NA_real_,
        'Sales missing acquisition cost'=if(nrow(ledger))sum(is.na(ledger$cost_basis)) else 0,check.names=FALSE)
    },na='Unavailable',digits=0)
    output$model_performance<-shiny::renderTable({
      d<-loaded();rows<-get('supabase_read_all',list(table_name='model_evaluations',query=list(user_id=paste0('eq.',d$account_id),
        championship_id=paste0('eq.',d$championship_id),order='cutoff.desc')))
      if(!nrow(rows))return(data.frame(Status='No evaluated outcomes yet. Predictions remain advisory.'))
      records<-insights_rows(rows);records<-records[!duplicated(vapply(records,function(r)r$model_version,character(1)))]
      dplyr::bind_rows(lapply(records,function(r) {
        metrics<-unlist(r$metrics)
        if(!length(metrics))return(data.frame(Model=r$model_version,Status=r$status,Metric='Evidence unavailable',Value=NA_real_))
        data.frame(Model=r$model_version,Status=r$status,Metric=names(metrics),Value=suppressWarnings(as.numeric(metrics)))
      }))
    },na='Unavailable')
    list(data=loaded,model=shiny::reactive(loaded()$model),curve=curve,profit=profit,lineup=lineup,point_forecasts=point_forecasts)
  })
}
