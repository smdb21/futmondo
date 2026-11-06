library(reactable)
get_table_definitions <- function() {
  tables<-supabase_known_tables()
  data.frame(table_name=tables,description=gsub("_"," ",tables),identity_column=vapply(tables,supabase_primary_key,character(1)),stringsAsFactors=FALSE)
}

# Pure display adapter: only known configuration fields are exposed.
admin_league_settings <- function(league=list(),info=list(),championship_id="") {
 cfg<-utils::modifyList(league$configuration %||% list(),info$configuration %||% list())
 number<-function(x){v<-suppressWarnings(as.numeric(x));if(length(v)==1L&&is.finite(v))v else NA_real_}
 value<-function(x,suffix=""){v<-number(x);if(is.finite(v))paste0(format(v,big.mark=".",decimal.mark=",",scientific=FALSE,trim=TRUE),suffix)else "Not provided"}
 flag<-function(x){
  if(length(x)!=1L||is.list(x)||is.na(x))return("Not provided")
  if(tolower(as.character(x))%in%c("true","false"))if(tolower(as.character(x))=="true")"Yes"else"No"else "Not provided"
 }
 reported<-function(x){
  if(length(x)!=1L||!is.atomic(x)||is.na(x))return("Not provided")
  if(is.logical(x)||tolower(as.character(x))%in%c("true","false"))flag(x)else as.character(x)
 }
 days<-function(x)value(x,if(isTRUE(number(x)==1))" day"else" days")
 percent<-function(x)value(number(x)*100,"%")
 choice<-function(x,choices){key<-as.character(x);if(length(key)==1L&&!is.na(key)&&key%in%names(choices))unname(choices[key])else reported(x)}
 rows<-list();add<-function(group,setting,current,explanation){rows[[length(rows)+1L]]<<-data.frame(group=group,setting=setting,value=current,explanation=explanation,stringsAsFactors=FALSE)}
 opening<-rival_opening_cash(championship_id,list(initial_budget=number(cfg$budget)))
 add("Starting conditions","Starting cash",value(opening$amount," EUR"),"Cash allocated at the start, from the league configuration; this is not the team's current balance.")
 add("Starting conditions","Start with an empty squad",flag(info$initialRosterEmpty %||% league$initialRosterEmpty),"Whether teams begin without assigned players.")
 add("Starting conditions","Initial players",value(cfg$numberOfPlayers),"Number of players configured for the initial squad allocation.")
 add("Round rewards","Per point",value(cfg$moneyPerPoint," EUR"),"Amount per team point in a round. A zero amount disables this reward.")
 add("Round rewards","Ranking prize pool",value(cfg$moneyPerRanking," EUR"),"Total pool distributed by each round's standings, not the season standings.")
 mode<-as.character(cfg$rankingMode %||% "")
 direction<-if(identical(mode,"top"))"Highest scores first"else if(identical(mode,"flop"))"Lowest scores first"else "Not provided / unrecognized"
 add("Round rewards","Ranking direction",direction,"Determines which end of the round standings receives the largest prizes.")
 count<-number(cfg$usersToRank)
 recipients<-if(is.finite(count)&&count==-1)"All eligible teams"else value(count)
 distribution<-if(is.finite(count)&&count==-1)"Weighted shares: N, N−1, …, 1 for N eligible teams."else if(is.finite(count)&&count==3)"Prize shares: 50%, 30%, 20%."else if(is.finite(count)&&count==5)"Prize shares: 40%, 30%, 15%, 10%, 5%."else "Distribution is not recognized from the supplied setting."
 add("Round rewards","Ranking recipients",recipients,paste(distribution,"Tied teams share the prizes for their positions."))
 add("Round rewards","Per Best XI player",value(cfg$dreamTeamPlayer," EUR"),"Bonus for each eligible owned player selected in the round's Best XI.")
 add("Round rewards","MVP bonus",value(cfg$mvpPlayer," EUR"),"Additional bonus for the eligible owner of the round MVP, separate from the Best XI bonus.")
 add("Market","Auction duration",days(cfg$bidDuration),"How long Futmondo market auctions remain open.")
 add("Market","Futmondo market players",value(cfg$marketPlayers),"Number of players configured for Futmondo's market listings.")
 add("Squad and league limits","Maximum squad size",value(cfg$maxPlayersInRoster),"Configured limit on players in one team's squad.")
 add("Squad and league limits","Maximum league teams",value(cfg$maxUserteams),"Configured limit on participating teams.")
 add("Squad and league limits","Full season",flag(cfg$fullSeason),"Whether the championship is configured for the full season.")
 add("Squad and league limits","Configured rounds",value(cfg$numberOfRounds),"Number of rounds configured for this championship.")
 add("Squad and league limits","Remaining rounds",value(cfg$remainingNumberOfRounds),"Remaining rounds reported by the configuration; not inferred from dates.")
 add("Clauses and transfers","Automatic release clauses",flag(cfg$enableAutomaticClauses),"Whether Futmondo's automatic release-clause feature is enabled for the league.")
 add("Clauses and transfers","Clause arrival delay",days(cfg$playerMoveInDays),"Configured daily transfer runs after paying a release clause; zero means immediate. This is not a fixed number of 24-hour periods. The player's reported transfer date determines its exact arrival.")
 add("Clauses and transfers","Minimum holding period",days(cfg$playerRetention),"Days after buying a player before they can be sold.")
 add("Clauses and transfers","Clause protection after signing",days(cfg$enablingClause),"Days after acquiring a player before their release clause becomes available to rivals.")
 add("Clauses and transfers","Loans",flag(cfg$loans),"Whether loans are enabled in the league configuration.")
 add("Market","Markets per day",value(cfg$marketTimes),"Number of Futmondo market cycles per day.")
 add("Round rewards","Extra payment",value(cfg$extraPay),"Reported extra-payment setting. Timing and units are not established by this response.")
 coach<-reported(cfg$enableCoach)
 add("Lineup","Coach mode",if(identical(coach,"automatic"))"Automatic"else coach,"Coach mode reported by Futmondo for this league. This is separate from this app's login-time hiring preference.")
 # Labels, enum values and units verified against Futmondo's public settings UI.
 # See docs/admin_module.md for the response-to-control mapping and limitations.
 add("Starting conditions","Deduct initial squad value",choice(cfg$rtv,c(`true`="Yes",`false`="No",automatic="Automatic",`TRUE`="Yes",`FALSE`="No")),"Whether the initial squad's value is deducted from the starting allocation. Recorded opening cash takes precedence in the ledger.")
 add("Market","Squad value available for bids",percent(cfg$mnmp),"Percentage of squad value added to available cash when Futmondo calculates the maximum bid. Pending commitments may reduce available cash.")
 add("Market","Direct-sale percentage",percent(cfg$dspct),"Percentage used by Futmondo for direct-sale offers. The actual quoted offer remains authoritative.")
 add("Market","Minimum bid is player value",flag(cfg$mbmv),"Whether bids must meet the player's value.")
 add("Market","Players offered for sale",if(isTRUE(number(cfg$mpm)==0))"No limit"else value(cfg$mpm),"Maximum players a team can offer for sale at once; zero means no limit.")
 add("Market","Visible offers",if(isTRUE(number(cfg$vmb)==-1))"All"else value(cfg$vmb),"Number of offers Futmondo displays; -1 means all.")
 add("Market","Show number of bids",flag(cfg$avmb),"Whether users can see how many bids a player has received.")
 add("Market","Market type",choice(cfg$mtype,c(normal="Normal",auction="Auction")),"The league's configured market type.")
 add("Clauses and transfers","Clause price range",choice(cfg$ccr,c(`1`="Cheap",`2`="Intermediate",`3`="Expensive")),"Price range used by Futmondo to set release clauses.")
 add("Clauses and transfers","Daily clause transfer time",choice(cfg$ctt,c(`true`="12:05",`false`="00:05",`TRUE`="12:05",`FALSE`="00:05")),"Time shown by Futmondo for its daily clause transfer run. The response does not establish a timezone; exact arrivals use the supplied transfer timestamp.")
 add("Clauses and transfers","Weekly clauses against a team",if(is.finite(number(cfg$mcpw))&&number(cfg$mcpw)<1)"No limit"else value(cfg$mcpw),"Maximum players rivals can take from one team through clauses each week. Futmondo's settings UI treats values below one as unlimited.")
 add("Clauses and transfers","Prohibit clause blocking",flag(cfg$dbp),"Whether users are prohibited from protecting players by blocking their clauses.")
 add("Clauses and transfers","Maximum blocked players",if(isTRUE(number(cfg$mbp)==0))"No limit"else value(cfg$mbp),"Maximum protected players, including protection after signing. Zero means no limit; the prohibition on blocking is a separate setting.")
 add("Clauses and transfers","Prohibit clause renewal",flag(cfg$ddt),"Whether users are prohibited from increasing release clauses.")
 add("Clauses and transfers","Recover clause investment",if(is.finite(number(cfg$rcp))&&number(cfg$rcp)<=0)"None"else percent(cfg$rcp),"Recoverable share of money invested in increasing clauses. Futmondo displays non-positive settings as no recovery.")
 add("Clauses and transfers","Allow loan cancellation",flag(cfg$acl),"Whether loans can be cancelled.")
 add("Lineup","Captain enabled",flag(cfg$cpt),"Whether the league permits a captain.")
 add("Lineup","Multiposition players",flag(cfg$apm),"Whether players can use the permitted alternative positions.")
 add("Lineup","Extra formations",flag(cfg$aes),"Whether extra formations are enabled.")
 add("Lineup","Lock postponed-match lineups",flag(cfg$blc),"Whether lineups for postponed matches are locked.")
 # Unresolved captured codes remain explicit; never guess meanings or expose
 # arbitrary configuration fields (which could contain private data).
 codes<-c("ac","avub","mdbp","nsnr","mtoffset")
 for(key in intersect(codes,names(cfg)))add("Other reported settings",key,reported(cfg[[key]]),"Reported configuration value; meaning and units have not been established.")
 do.call(rbind,rows)
}


# Compact, keyboard-accessible disclosure rows; no requests or reactive state.
admin_league_settings_UI <- function(rows,stale=FALSE) {
 row_ui<-function(part,i)tags$details(class="fm-admin-setting",
  tags$summary(tags$span(part$setting[i]),tags$strong(part$value[i])),
  tags$p(part$explanation[i]))
 groups<-setdiff(unique(rows$group),"Other reported settings")
 other<-rows[rows$group=="Other reported settings",,drop=FALSE]
 tagList(
  singleton(tags$style(HTML(".fm-admin-settings-card{margin-bottom:12px;padding:12px;border:1px solid var(--fm-border);border-radius:7px;background:var(--fm-surface);min-width:0}.fm-admin-settings-card h3{font-size:15px;margin:0 0 8px}.fm-admin-setting{border-top:1px solid var(--fm-border);padding:6px 0;min-width:0}.fm-admin-setting summary{display:flex;gap:8px;align-items:baseline;cursor:pointer;overflow-wrap:anywhere}.fm-admin-setting summary::before{content:'+';color:var(--fm-muted)}.fm-admin-setting[open] summary::before{content:'−'}.fm-admin-setting summary>span{flex:1;min-width:0}.fm-admin-setting summary>strong{text-align:right;max-width:55%;min-width:0}.fm-admin-setting p{color:var(--fm-muted);font-size:12px;margin:6px 0 2px;overflow-wrap:anywhere}.fm-admin-reported{margin:4px 0 16px}.fm-admin-reported>summary{cursor:pointer}.fm-admin-reported dl{display:grid;grid-template-columns:minmax(0,1fr) minmax(0,1fr);gap:4px 12px;margin:8px 0;max-width:480px}.fm-admin-reported dt,.fm-admin-reported dd{overflow-wrap:anywhere}.fm-admin-reported dd{text-align:right;margin:0}"))),
  tags$p("Current league settings · Read-only. Open a setting for details; change it in Futmondo."),
  if(isTRUE(stale))tags$p(role="status","Showing saved settings while the latest configuration is unavailable."),
  fluidRow(class="fm-admin-settings-grid",lapply(split(groups,ceiling(seq_along(groups)/2)),function(pair){
   column(12,class="col-md-4",lapply(pair,function(group){
    part<-rows[rows$group==group,,drop=FALSE]
    tags$section(class="fm-admin-settings-card",tags$h3(group),
     lapply(seq_len(nrow(part)),function(i)row_ui(part,i)))
   }))
  })),
  if(nrow(other))tags$details(class="fm-admin-reported",tags$summary(paste0("Other reported settings (",nrow(other),")")),
   tags$p("These additional settings retain their reported field names because their meanings and units have not been established."),
   tags$dl(lapply(seq_len(nrow(other)),function(i)tagList(tags$dt(other$setting[i]),tags$dd(other$value[i]))))),
  tags$p(style="font-size:12px;color:var(--fm-muted);","Round rewards exclude teams that started the round in debt. These are current rules; past changes may affect reconstructed balances.")
 )
}

admin_UI <- function(id) {
 ns<-NS(id)
 tagList(h2("League settings"),uiOutput(ns("league_settings")),h2("Database administration"),p("Inspect retained factual tables. Changes require an authorized administrator."),
  actionButton(ns("btn_refresh_stats"),"Refresh table counts"),reactable::reactableOutput(ns("tables_stats_table")),
  actionButton(ns("btn_verify_db"),"Verify database connection"),
  h3("Round awards"),p("Saved selections improve reward estimates. Historical eligibility and payment coverage are checked separately."),
  uiOutput(ns("award_actions")),
  selectInput(ns("award_round"),"Round to refresh",choices=character()),actionButton(ns("btn_refresh_selected_award"),"Refresh selected round"),
  textOutput(ns("awards_progress")),reactable::reactableOutput(ns("awards_coverage")),
  actionButton(ns("btn_reset_db"),"Reset retained database tables",class="btn-danger"))
}
admin_Server <- function(id,is_module_active,login_token,championship_id,user_team_id,user_teams_RV,is_admin=NULL,refresh_trigger=NULL,league_settings_source=NULL) {
 moduleServer(id,function(input,output,session){
  ns<-session$ns
  authorized<-reactive(valid_login(login_token()) && if(is.function(is_admin))isTRUE(is_admin())else if(!is.null(is_admin))isTRUE(is_admin)else is_authorized_admin(login_token()))
  output$league_settings<-renderUI({
   req(is_module_active(),authorized())
   if(!is.function(league_settings_source))return(tags$p("League settings are unavailable."))
   snapshot<-league_settings_source()
   if(!is.list(snapshot))return(tags$p("League settings are unavailable."))
   rows<-admin_league_settings(snapshot$league %||% list(),snapshot$info %||% list(),championship_id())
   admin_league_settings_UI(rows,stale=isTRUE(snapshot$stale))
  })
  context<-reactive(list(user=fm_scalar(login_token()[["userid"]]),league=fm_scalar(championship_id())))
  reset_context<-reactiveVal(NULL);observeEvent(context(),reset_context(NULL),ignoreNULL=FALSE,priority=100)
  row_counts_df<-reactiveVal(NULL)
  load_row_counts<-function(){req(is_module_active(),authorized());row_counts_df(tryCatch(get_table_row_counts(),error=function(e)NULL))}
  observeEvent(input$btn_refresh_stats,load_row_counts())
  output$tables_stats_table<-reactable::renderReactable({
   req(is_module_active(),authorized());definitions<-get_table_definitions();counts<-row_counts_df()
   if(is.data.frame(counts)&&nrow(counts)){
    if(!"table_name"%in%names(counts)&&"table"%in%names(counts))names(counts)[names(counts)=="table"]<-"table_name"
    if("table_name"%in%names(counts))definitions<-dplyr::left_join(definitions,counts,by="table_name")
   };reactable::reactable(definitions,striped=TRUE,compact=TRUE)
  })
  observeEvent(input$btn_verify_db,{req(is_module_active(),authorized());ok<-tryCatch(isTRUE(init_supabase_db(verbose=FALSE)),error=function(e)FALSE);showNotification(if(ok)"Database connection verified."else "Database verification failed.",type=if(ok)"message"else "warning")})
  awards<-shared_mvp_source(session,login_token,championship_id,function()isTRUE(is_module_active())&&isTRUE(authorized()),refresh_trigger)
  observe({req(is_module_active(),authorized());coverage<-awards()$coverage
    choices<-if(is.data.frame(coverage)&&nrow(coverage)){
      coverage<-coverage[coverage$required%in%TRUE&!is.na(coverage$round_id)&nzchar(trimws(coverage$round_id)),,drop=FALSE]
      setNames(coverage$round_id,award_round_labels(coverage$round_number,coverage$round_id))
    }else character()
    selected<-isolate(input$award_round)
    if(length(selected)!=1L||!selected%in%choices)selected<-head(unname(choices),1L)
    updateSelectInput(session,"award_round",choices=choices,selected=selected)
  })
  output$award_actions<-renderUI({req(is_module_active(),authorized());snapshot<-awards();rows<-snapshot$coverage
    if(is.data.frame(rows)&&nrow(rows))rows<-rows[rows$required%in%TRUE,,drop=FALSE]
    eligible<-is.data.frame(rows)&&nrow(rows)&&any(rows$selection=="Missing")
    button<-function(id,label,enabled)actionButton(session$ns(id),label,disabled=!(enabled&&!isTRUE(snapshot$running)&&!isTRUE(snapshot$loading)),style="white-space:normal;max-width:100%;margin-bottom:8px;")
    tagList(button("btn_sync_dreamteams","Retrieve missing round awards",eligible),
      button("btn_refresh_awards","Refresh provisional or changed rounds",is.data.frame(rows)&&nrow(rows)>0L),
      button("btn_retry_award_saves","Retry failed saves",isTRUE(snapshot$unsaved>0L)))
  })
  output$awards_coverage<-reactable::renderReactable({req(is_module_active(),authorized());rows<-awards()$coverage
    if(!is.data.frame(rows)||!nrow(rows))return(reactable::reactable(data.frame(Status="Round catalog unavailable or still loading.")))
    rows<-rows[rows$required%in%TRUE,,drop=FALSE]
    if(!nrow(rows))return(reactable::reactable(data.frame(Status="No completed rounds yet.")))
    reactable::reactable(rows,columns=list(round_id=reactable::colDef(show=FALSE),required=reactable::colDef(show=FALSE),round_number=reactable::colDef(name="Round",cell=function(value,index)award_round_labels(value,rows$round_id[index])),selection=reactable::colDef(name="Selection"),finality=reactable::colDef(name="Finality"),mvp=reactable::colDef(name="MVP"),last_fetched=reactable::colDef(name="Last fetched"),persistence=reactable::colDef(name="Saved status"),issue=reactable::colDef(name="Issue")),defaultSorted=list(round_number="desc"),compact=TRUE,wrap=TRUE)
  })
  output$awards_progress<-renderText({req(is_module_active(),authorized());snapshot<-awards()
    if(isTRUE(snapshot$loading))return("Loading round details…")
    # Keep the actual failure visible above the table on narrow screens.
    issues<-if(is.data.frame(snapshot$coverage)&&all(c("issue","required")%in%names(snapshot$coverage)))snapshot$coverage$issue[snapshot$coverage$required%in%TRUE] else character()
    issues<-issues[!is.na(issues)&nzchar(trimws(issues))]
    counts<-sort(table(issues),decreasing=TRUE)
    details<-if(length(counts))paste0("Failures: ",paste(paste0(head(as.integer(counts),3L)," × ",head(names(counts),3L)),collapse="; "),". ")else ""
    paste0(snapshot$fetched %||% 0," fetched; ",snapshot$saved %||% 0," confirmed saved; ",snapshot$failed %||% 0," failed; ",snapshot$pending %||% 0," pending. ",details,snapshot$reason)
  })
  observeEvent(input$btn_sync_dreamteams,{req(is_module_active(),authorized());attr(awards,"retrieve")("missing")})
  observeEvent(input$btn_refresh_awards,{req(is_module_active(),authorized());attr(awards,"retrieve")("refresh")})
  observeEvent(input$btn_refresh_selected_award,{req(is_module_active(),authorized());attr(awards,"retrieve")("selected",input$award_round)})
  observeEvent(input$btn_retry_award_saves,{req(is_module_active(),authorized());attr(awards,"retry_save")()})
  observeEvent(input$btn_reset_db,{
   req(is_module_active(),authorized());reset_context(context())
   showModal(modalDialog(title="Reset retained database tables?","This removes saved factual records. Confirm only after creating a restorable backup.",footer=tagList(modalButton("Cancel"),actionButton(ns("btn_confirm_reset"),"Confirm reset",class="btn-danger"))))
  })
  observeEvent(input$btn_confirm_reset,{
   req(is_module_active(),authorized());expected<-isolate(reset_context());reset_context(NULL);req(!is.null(expected),identical(expected,context()));removeModal()
   result<-tryCatch(supabase_reset_database(force=TRUE),error=function(e)NULL)
   showNotification(if(is.null(result))"Database reset failed."else "Database reset completed.",type=if(is.null(result))"error"else "message");load_row_counts()
  })
 })
}
