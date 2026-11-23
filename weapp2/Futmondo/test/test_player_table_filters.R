#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
selected_player_Server<-function(...)NULL
recent_player_points_source<-function(...)list(history=function()data.frame(),status=function()NULL)
n<-0L;check<-function(name,expr){force(expr);n<<-n+1L;cat('PASS',name,'\n')}
settle<-function(session){session$elapse(350);for(i in seq_len(8)){later::run_now(0);session$flushReact()}}
# Native source rows need not contain derived isClause or an optional lock date.
source_rows<-shiny::reactiveVal(data.frame(id=c('free','owned'),name=c('Free','Owned'),role='defensa',value=1e6,
  clause_price=2e6,clause_transferred=FALSE,computer=c(TRUE,FALSE),userteamId=c('','rival')))
shiny::testServer(players_table_Server,args=list(players_table_RV=source_rows,user_teams_RV=shiny::reactive(NULL),optional_ready=function()FALSE),{
 check('schema observer survives both filters with absent clause fields',{
  session$setInputs(active_clause_filter=TRUE,is_from_futmondo_filter=TRUE);settle(session)
  stopifnot(identical(players_table_filtered_RV()$id,'free'),!is.null(output$players_table))
  rows<-source_rows();rows$isClause<-TRUE;source_rows(rows);settle(session)
  stopifnot(identical(players_table_filtered_RV()$id,'free'),!is.null(output$players_table))
 })
 check('optional filters accept absent fields and zero matches render then recover',{
  source_rows(data.frame(id='minimal',name='Minimal',value=100));settle(session)
  session$setInputs(active_clause_filter=TRUE,is_from_futmondo_filter=TRUE,is_favorite_filter=TRUE,
    players_you_bid_filter=TRUE,players_with_bid_filter=TRUE,position_filter='Defender',change_value_filter=0)
  settle(session);stopifnot(nrow(players_table_filtered_RV())==0L,!is.null(output$players_table))
  session$setInputs(active_clause_filter=FALSE,is_from_futmondo_filter=FALSE,is_favorite_filter=FALSE,
    players_you_bid_filter=FALSE,players_with_bid_filter=FALSE,position_filter='All',change_value_filter=NA)
  settle(session);stopifnot(identical(players_table_filtered_RV()$id,'minimal'),!is.null(output$players_table),
    !any(c('role','role2','clause_date','fav','bid_price','numberOfBids')%in%names(table_display_data())))
 })
 check('genuine empty source retains a renderable typed table',{
  source_rows(data.frame(id=character(),name=character(),value=numeric()));settle(session)
  stopifnot(is.data.frame(players_table_filtered_RV()),nrow(players_table_filtered_RV())==0L,!is.null(output$players_table))
 })

})
shiny::testServer(players_table_Server,args=list(players_table_RV=shiny::reactive(data.frame(id=c("low","high"),name=c("Low","High"),value=c(1e6,2e9))),user_teams_RV=shiny::reactive(NULL),optional_ready=function()FALSE),{
 check('blank maximum includes values above 1000M and a supplied cap still filters',{
  session$setInputs(max_value_filter=NA_real_);settle(session)
  stopifnot(nrow(players_table_filtered_RV())==2)
  session$setInputs(max_value_filter=1000);settle(session)
  stopifnot(identical(players_table_filtered_RV()$id,"low"))
  session$setInputs(max_value_filter=NA_real_);settle(session)
  stopifnot(nrow(players_table_filtered_RV())==2)
 })
 check('obsolete minimum valuation inputs cannot hide players',{
  session$setInputs(min_value_filter=3000);settle(session)
  stopifnot(identical(players_table_filtered_RV()$id,c("low","high")))
 })
})
check('Market caps the Valuation column independently of listing and clause prices',local({
  market_rows <- dplyr::bind_rows(lapply(list(
    list(id='under', name='At Valuation Cap', value=5e6, price=8e6, clause=list(price=12e6)),
    list(id='over', name='Above Valuation Cap', value=6e6, price=3e6, clause=list(price=4e6)),
    list(id='unknown', name='Unknown Valuation', price=1e6, clause=list(price=2e6))
  ),parse_player_json))
  market_reads <- 0L
  original_market <- get_market_players
  original_bind <- bind_factual_reads
  get_market_players <<- function(...) {market_reads <<- market_reads+1L; market_rows}
  bind_factual_reads <<- function(...) NULL
  on.exit({
    get_market_players <<- original_market
    bind_factual_reads <<- original_bind
  }, add=TRUE)

  html <- htmltools::renderTags(market_UI('market'))$html
  stopifnot(grepl('Max Value (M)',html,fixed=TRUE),
    !grepl('max_clause_value_filter',html,fixed=TRUE))
  shiny::testServer(market_Server,args=list(is_module_active=function()TRUE,
    login_token=function()NULL,championship_id=function()NULL,user_team_id=function()NULL,
    user_teams_RV=function()NULL),{
    rendered <- function() jsonlite::toJSON(output[['market_players_table-players_table']],auto_unbox=TRUE)
    settle(session)
    stopifnot(all(vapply(market_rows$name,grepl,logical(1),x=rendered(),fixed=TRUE)), market_reads==1L)
    session$setInputs(`market_players_table-max_value_filter`=5);settle(session)
    stopifnot(grepl('At Valuation Cap',rendered(),fixed=TRUE),
      !grepl('Above Valuation Cap|Unknown Valuation',rendered()),market_reads==1L)
    session$setInputs(`market_players_table-max_value_filter`=NA_real_);settle(session)
    stopifnot(all(vapply(market_rows$name,grepl,logical(1),x=rendered(),fixed=TRUE)),market_reads==1L)
  })
}))
check('both table layouts omit the minimum valuation control',{
 for(breakdown in c(FALSE,TRUE)){
  html<-htmltools::renderTags(players_table_UI('filters',show_position_breakdown=breakdown))$html
  stopifnot(!grepl('min_value_filter',html,fixed=TRUE),grepl('max_value_filter',html,fixed=TRUE))
 }
})
check('both layouts provide a default-All multiselect position input',{
 for(breakdown in c(FALSE,TRUE)){
  html<-htmltools::renderTags(players_table_UI('multi',show_position_breakdown=breakdown))$html
  control<-regmatches(html,regexpr('<select[^>]*id="multi-position_filter"[^>]*>',html))
  stopifnot(grepl('multiple',control,fixed=TRUE),grepl('<option value="All" selected>All</option>',html,fixed=TRUE))
 }
})
check('filter labels and Clear Filters place decorative icons after their text in both layouts',{
 for(breakdown in c(FALSE,TRUE)){
  html<-htmltools::renderTags(players_table_UI('icons',show_position_breakdown=breakdown,
   filter_by_own_team=TRUE,filter_by_max_clause_value=TRUE,filter_by_clause_under_available_funds=TRUE,filter_by_players_with_bid=TRUE))$html
  dom<-xml2::read_html(html)
  icons<-xml2::xml_find_all(dom,"//label//i | //button[@id='icons-btn_clear_filters']//i")
  stopifnot(length(icons)==13L,all(xml2::xml_attr(icons,'aria-hidden')=='true'))
  for(icon in icons)stopifnot(!is.na(xml2::xml_text(xml2::xml_find_first(icon,"preceding-sibling::text()[normalize-space()]"))),
   length(xml2::xml_find_all(icon,"following-sibling::text()[normalize-space()]"))==0L)
 }
})
club_rows <- data.frame(
  id = c("z", "a", "legacy", "bn", "b", "missing"),
  name = c("Zeta Player", "Alaves Player", "Legacy Player", "Name-Only Player", "Betis Player", "Unknown Player"),
  teamId = c("z", "a", NA, NA, "b", NA),
  team = c("Zeta FC", "Álaves", " Álaves ", "Celta & Vigo", "Betis", NA),
  logo = c("zeta.png", NA, "alaves", "https://example.invalid/celta.webp", "../invalid.png", NA),
  userteamId = c("rival", "rival", "mine", "", "mine", ""), value = 1e6
)
check("club options sort by name, merge proven legacy matches and preserve crest evidence", {
  clubs <- players_table_clubs(club_rows)
  stopifnot(identical(clubs$keys, c("club:z", "club:a", "club:a", "name:celta & vigo", "club:b", "")),
    identical(clubs$choices$label, c("Álaves", "Betis", "Celta & Vigo", "Zeta FC")),
    identical(clubs$choices$crest, c(paste0(TEAM_LOGO_URL, "alaves.png"), "",
      "https://example.invalid/celta.webp", paste0(TEAM_LOGO_URL, "zeta.png"))))
  ambiguous <- data.frame(teamId = c("first", "second", NA), team = "United", logo = "united")
  stopifnot(identical(players_table_clubs(ambiguous)$keys, c("club:first", "club:second", "name:united")))
  for (breakdown in c(FALSE, TRUE)) {
    ui <- htmltools::renderTags(players_table_UI("club", show_position_breakdown = breakdown))$html
    stopifnot(grepl('id="club-club_filter_ui"', ui, fixed = TRUE))
  }
})
club_source <- reactiveVal(club_rows)
club_league <- reactiveVal("first")
shiny::testServer(players_table_Server, args = list(players_table_RV = club_source,
  championship_id = club_league, user_teams_RV = reactive(NULL), optional_ready = function() FALSE), {
  settle(session)
  messages <- list()
  root <- session$rootScope()
  root$sendInputMessage <- function(inputId, message) messages[[sub("^.*-", "", inputId)]] <<- message
  options <- function() xml2::xml_find_all(xml2::read_html(output$club_filter_ui$html), "//option")
  stopifnot(identical(xml2::xml_text(options()), c("All clubs", "Álaves", "Betis", "Celta & Vigo", "Zeta FC")))
  first_value <- xml2::xml_attr(options()[2], "value")
  session$setInputs(club_filter = first_value)
  settle(session)
  check("club and fantasy owner filters intersect while the clubs stay available", {
    stopifnot(identical(players_table_filtered_RV()$id, c("a", "legacy")), length(options()) == 5L)
    session$setInputs(team_filter = "team:rival")
    settle(session)
    stopifnot(identical(players_table_filtered_RV()$id, "a"))
    before <- output$club_filter_ui$html
    revised <- club_source()
    revised$value <- revised$value + 1e6
    club_source(revised)
    settle(session)
    stopifnot(identical(output$club_filter_ui$html, before), identical(players_table_filtered_RV()$id, "a"))
  })
  check("clear resets club and stale context inputs cannot filter another league", {
    session$setInputs(btn_clear_filters = 1L)
    settle(session)
    stopifnot(any(vapply(messages, function(m) identical(m$value, "All"), logical(1))),
      identical(messages[["club_filter"]]$value, "All"))
    session$setInputs(team_filter = "All")
    club_league("other")
    settle(session)
    stopifnot(identical(players_table_filtered_RV()$id, club_rows$id),
      identical(xml2::xml_attr(options()[1], "selected"), "selected"),
      !identical(xml2::xml_attr(options()[2], "value"), first_value))
    session$setInputs(club_filter = xml2::xml_attr(options()[2], "value"))
    settle(session)
    revised <- club_source()
    revised$team[1] <- "Zeta Athletic"
    club_source(revised)
    settle(session)
    stopifnot(identical(xml2::xml_attr(options()[2], "selected"), "selected"),
      identical(players_table_filtered_RV()$id, c("a", "legacy")))
  })
})

cash_rows <- data.frame(id = c("cheap", "exact", "debt", "missing"), name = c("Cheap", "Exact", "Debt", "Missing"),
  clause_price = c(1e6, 2e6, 2.01e6, NA_real_), value = c(1e6, 2e6, 2.01e6, NA_real_))
cash <- reactiveVal(2e6)
cash_reads <- 0L
shiny::testServer(players_table_Server, args = list(players_table_RV = reactive(cash_rows),
  user_teams_RV = reactive(NULL), optional_ready = function() FALSE,
  available_funds_RV = 5e6, available_cash_RV = function() { cash_reads <<- cash_reads + 1L; cash() }), {
  settle(session)
  check("cash cap includes its exact boundary, excludes borrowing and composes with manual cap", {
    stopifnot(identical(players_table_filtered_RV()$id, cash_rows$id), cash_reads == 0L)
    session$setInputs(clause_under_available_funds_filter = TRUE)
    settle(session)
    stopifnot(identical(players_table_filtered_RV()$id, c("cheap", "exact")), cash_reads == 1L)
    session$setInputs(max_clause_value_filter = "1.5")
    settle(session)
    stopifnot(identical(players_table_filtered_RV()$id, "cheap"), cash_reads == 1L)
  })
  check("zero or negative cash excludes all positive clauses and unknown cash is reported", {
    session$setInputs(max_clause_value_filter = "")
    for (balance in c(0, -1e6, NA_real_)) {
      cash(balance)
      settle(session)
      stopifnot(nrow(players_table_filtered_RV()) == 0L)
    }
    stopifnot(grepl("Current cash balance is unavailable", output$clause_cash_status$html, fixed = TRUE))
    session$setInputs(clause_under_available_funds_filter = FALSE, value_under_available_funds_filter = TRUE)
    settle(session)
    stopifnot(identical(players_table_filtered_RV()$id, c("cheap", "exact", "debt")))
  })
})

position_rows<-data.frame(id=c('gk','def','mid','fwd','dual','unknown'),name=c('Keeper','Defender','Midfielder','Forward','Dual','Unknown'),
 role=c('Goalkeeper','Defender','Midfielder','Forward','Midfielder',NA),role2=c('',NA,'Defender','','Forward',NA),value=1e6)
enrichments<-0L;original_average<-add_recent_points_average
add_recent_points_average<-function(...){enrichments<<-enrichments+1L;original_average(...)}
shiny::testServer(players_table_Server,args=list(players_table_RV=function()position_rows,
 user_teams_RV=function()NULL,optional_ready=function()FALSE),{
 settle(session);baseline<-enrichments
 check('initial selection includes unknown positions and multiple choices match either role without duplicates',{
  stopifnot(identical(players_table_filtered_RV()$id,position_rows$id))
  session$setInputs(position_filter=c('Defender','Midfielder'));settle(session)
  stopifnot(identical(players_table_filtered_RV()$id,c('def','mid','dual')))
  session$setInputs(position_filter=c('Goalkeeper','Forward'));settle(session)
  stopifnot(identical(players_table_filtered_RV()$id,c('gk','fwd','dual')))
 })
 check('rapid selections publish only after 300ms idle and reuse enriched observations',{
  session$setInputs(position_filter='Defender');session$elapse(150)
  stopifnot(identical(players_table_filtered_RV()$id,c('gk','fwd','dual')))
  session$setInputs(position_filter=c('Defender','Forward'));session$elapse(299)
  stopifnot(identical(players_table_filtered_RV()$id,c('gk','fwd','dual')))
  session$elapse(1)
  stopifnot(identical(players_table_filtered_RV()$id,c('def','mid','fwd','dual')),enrichments==baseline)
 })
 check('All and an uninitialized or cleared input include every row',{
  for(selection in list('All',character(),NULL)){
   session$setInputs(position_filter=selection);settle(session)
   stopifnot(identical(players_table_filtered_RV()$id,position_rows$id))
  }
 })
})
add_recent_points_average<-original_average
check('clause cap preserves decimal separators and exact euro boundaries', {
  for (value in list(135, "135", "135,0", "135.0", "135 M"))
    stopifnot(identical(players_table_clause_cap(value), 135e6))
  stopifnot(identical(players_table_clause_cap("1,000001"), 1000001),
    identical(players_table_clause_cap("135,5 M"), 135.5e6),
    identical(players_table_clause_cap("0"), 0),
    identical(players_table_clause_cap("  "), Inf),
    identical(players_table_clause_cap(NULL), Inf))
  for (value in c("135e", "-135", "135,0.0", "135 MM"))
    stopifnot(is.na(players_table_clause_cap(value)))
})
check('clause eligibility uses price, transfer state and UTC lock times',{
 now<-as.POSIXct('2026-11-01 12:00:00',tz='UTC')
 rows<-data.frame(clause_price=c(10,10,10,10,10,0,10,10),clause_transferred=c(FALSE,FALSE,FALSE,FALSE,TRUE,FALSE,NA,NA),
  isClause=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,NA,TRUE),clause_date=c(NA,'','2026-11-01T13:00:00+01:00','2026-11-01T13:00:00Z','','','','bad date'))
 stopifnot(identical(players_table_active_clause(rows,now),c(TRUE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE)))
})
check('owner aliases override flags; unknown owner with computer FALSE is not free',{
 rows<-data.frame(id=letters[1:6],owner_team_id=c('','known','','','',''),userteamId=c('rival','','','','',''),
   userTeam=c('','','Named owner','','',''),computer=c(TRUE,TRUE,TRUE,TRUE,FALSE,NA))
 stopifnot(identical(players_table_normalize_owner(rows)$is_free_agent,c(FALSE,FALSE,FALSE,TRUE,FALSE,TRUE)))
 unknown<-players_table_normalize_owner(data.frame(id=c('u','f'),computer=c(FALSE,TRUE)))
 stopifnot(identical(unknown$is_free_agent,c(FALSE,TRUE)))
})
check('Today and Players checkbox defaults apply only to their tables',{
 checked<-function(html,id){tag<-regmatches(html,regexpr(paste0('<input[^>]*id="',id,'"[^>]*>'),html));length(tag)==1L&&grepl('checked',tag,fixed=TRUE)}
 today<-htmltools::renderTags(today_UI('today'))$html;players<-htmltools::renderTags(players_in_championship_UI('players'))$html
 plain<-htmltools::renderTags(players_table_UI('plain'))$html
 stopifnot(checked(today,'today-market_players-is_from_futmondo_filter'),!checked(today,'today-market_players-active_clause_filter'),
   checked(players,'players-championship_players_table-active_clause_filter'),!checked(players,'players-championship_players_table-is_from_futmondo_filter'),
   !checked(plain,'plain-active_clause_filter'),!checked(plain,'plain-is_from_futmondo_filter'))
})
rows<-data.frame(id=paste0('p',seq_len(524)),name='Player',value=1e6,clause_price=2e6,clause_transferred=FALSE,computer=TRUE)
elapsed<-system.time({normalized<-players_table_filter_fields(players_table_normalize_owner(rows));mask<-players_table_active_clause(normalized)&normalized$is_free_agent})[['elapsed']]
stopifnot(all(mask));cat(sprintf('METRIC 524-row normalization/combined filter: %.3fs; %.1fKiB\n',elapsed,as.numeric(object.size(normalized))/1024))
cat(sprintf('PLAYER TABLE FILTERS: %d passed / 0 failed\n',n))

positions<-c('Defender','Forward');large<-position_rows[rep(seq_len(nrow(position_rows)),length.out=524),]
elapsed<-system.time(for(i in seq_len(1000))filtered<-dplyr::filter(large,role %in% positions | role2 %in% positions))[['elapsed']]
cat(sprintf('METRIC 524-row multiposition filter: %.3fms per pass over 1000 iterations\n',elapsed))
