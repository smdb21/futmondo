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
stopifnot(inflow$mvp==2000000,inflow$dreamteam==1500000,inflow$ranking==32000000)
h<-data.frame(team_id='reference',team_name='Reference',round=c(1,4,7),points=1)
columns<-classification_bonus_table(data.frame(team_id='reference'),h,x,c(1,11))
stopifnot(columns$mvp_bonus==2000000,columns$best_xi_bonus==1500000)
# Exercise the selected rival's actual tab, totals and expandable details.
published <- shiny::reactiveVal(x)
fixture_module <- rivals_Server
environment(fixture_module) <- list2env(list(
  bind_factual_reads = function(...) NULL,
  shared_mvp_source = function(...) shiny::reactive(NULL),
  shared_rival_finance_source = function(...) published,
  get_user_team_info = function(...) list(),
  factual_read_adapter = function(...) function(...) list(status = "ready", data = data.frame()),
  get_league_standings_history = function(...) data.frame(),
  get_financial_snapshot = function(...) list(),
  get_championship_pressroom = function(...) data.frame(),
  players_table_UI = function(...) NULL,
  players_table_Server = function(...) shiny::reactive(NULL)
), parent = environment(rivals_Server))
shiny::testServer(fixture_module, args = list(
  is_module_active = function() TRUE,
  login_token = function() c(userid = "account"),
  championship_id = function() "league",
  user_team_id = function() "reference",
  user_teams_RV = function() x$team_finances
), {
  session$flushReact()
  session$setInputs(rival_team_selection = list(id = "reference", context = list(
    user = "account", championship = "league", team = "reference")))
  shell <- output$scouted_rival_details_ui$html
  page <- xml2::read_html(shell)
  tabs <- xml2::xml_find_all(page, "//ul/li/a[@data-toggle='tab']")
  stopifnot(identical(xml2::xml_attr(tabs, "data-value"), c("history", "roster")),
    xml2::xml_attr(xml2::xml_parent(tabs[1]), "class") == "active")
  anchors <- c("tx_pivot_by_player", "rival_transactions_table", "rival_transactions_tab_ui", "rival_history_breakdown")
  positions <- vapply(anchors, function(name) as.integer(regexpr(session$ns(name), shell, fixed = TRUE)), integer(1))
  stopifnot(all(positions > 0), all(diff(positions) > 0),
    length(xml2::xml_find_all(page, "//details[summary='Income and expense breakdown' and not(@open)]")) == 1)
  totals <- xml2::read_html(output$rival_transactions_tab_ui$html)
  stopifnot(identical(xml2::xml_text(xml2::xml_find_all(totals, "//dt")),
    c("Balance", "Total income", "Total expenses")),
    xml2::xml_text(xml2::xml_find_first(totals, "//dd")) == ui_financial_amount(52129125))
  breakdown <- xml2::read_html(output$rival_history_breakdown$html)
  shown <- setNames(xml2::xml_text(xml2::xml_find_all(breakdown, "//dd")),
                    xml2::xml_text(xml2::xml_find_all(breakdown, "//dt")))
  stopifnot(all(c("Starting cash", "Player sales", "Ordinary sales (identified)", "Clause sales (identified)",
    "Position bonuses", "Point bonuses", "Player purchases", "Ordinary purchases (identified)",
    "Clause purchases (identified)") %in% names(shown)),
    shown[["MVP bonuses"]] == ui_financial_amount(2000000),
    shown[["Best XI bonuses"]] == ui_financial_amount(1500000),
    grepl("Total income − total expenses = balance", xml2::xml_text(breakdown), fixed = TRUE),
    inherits(rivals_cash_history_widget(ledger), "reactable"))
  session$setInputs(rival_tabs = "roster", tx_pivot_by_player = TRUE)
  renamed <- x
  renamed$team_finances$teamname <- "New team name"
  published(renamed)
  session$flushReact()
  stopifnot(identical(shell, output$scouted_rival_details_ui$html),
    identical(output$selected_rival_name, "New team name"))
})
# A mismatch preserves actual cash and blocks unverified component attribution.
bad<-awards;bad$is_mvp<-FALSE
failed<-build_combined(bad)
stopifnot(failed$team_finances$reconstructed_cash==52129125,is.na(failed$team_finances$mvp_income),
 is.na(failed$team_finances$dreamteam_income),!any(failed$cash_ledgers$reference$estimated & failed$cash_ledgers$reference$bonus_type=='mvp'))
cat('PASS combined XI/MVP cash, independently validated bonus columns, mismatch handling and actual history render with unknown position totals\n')
