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
 flag<-function(x)if(is.logical(x)&&length(x)==1L&&!is.na(x))if(x)"Yes"else"No"else "Not provided"
 rows<-list();add<-function(group,setting,current,explanation){rows[[length(rows)+1L]]<<-data.frame(group=group,setting=setting,value=current,explanation=explanation,stringsAsFactors=FALSE)}
 opening<-rival_opening_cash(championship_id,list(initial_budget=number(cfg$budget)))
 add("Starting conditions","Starting cash",value(opening$amount," EUR"),if(opening$source=="user confirmed")"User-confirmed starting amount; not returned in the current configuration."else "Cash allocated at the start; this is not the team's current balance.")
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
 add("Market","Auction duration",value(cfg$bidDuration," days"),"How long Futmondo market auctions remain open.")
 add("Market","Futmondo market players",value(cfg$marketPlayers),"Number of players configured for Futmondo's market listings.")
 add("Squad and league limits","Maximum squad size",value(cfg$maxPlayersInRoster),"Configured limit on players in one team's squad.")
 add("Squad and league limits","Maximum league teams",value(cfg$maxUserteams),"Configured limit on participating teams.")
 add("Squad and league limits","Full season",flag(cfg$fullSeason),"Whether the championship is configured for the full season.")
 add("Squad and league limits","Configured rounds",value(cfg$numberOfRounds),"Number of rounds configured for this championship.")
 add("Squad and league limits","Remaining rounds",value(cfg$remainingNumberOfRounds),"Remaining rounds reported by the configuration; not inferred from dates.")
 add("Squad and league limits","Automatic release clauses",flag(cfg$enableAutomaticClauses),"Whether Futmondo's automatic release-clause feature is enabled for the league.")
 do.call(rbind,rows)
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
   tagList(tags$p("Current settings for the selected league. Read-only; use Futmondo to change them."),
    if(isTRUE(snapshot$stale))tags$p(role="status","Showing saved settings while the latest configuration is unavailable."),
    tags$p("Round rewards exclude teams that started the round in debt. These are current rules; past rule changes may affect reconstructed balances."),
    lapply(unique(rows$group),function(group){
     part<-rows[rows$group==group,,drop=FALSE]
     tags$section(tags$h3(group),lapply(seq_len(nrow(part)),function(i)
      tags$div(style="padding:10px 0;border-bottom:1px solid var(--fm-border);overflow-wrap:anywhere;",
       tags$strong(part$setting[i]),tags$div(part$value[i]),
       tags$p(style="color:var(--fm-muted);margin:4px 0;",part$explanation[i]))))
    }))
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
      coverage<-coverage[!is.na(coverage$round_id)&nzchar(trimws(coverage$round_id)),,drop=FALSE]
      setNames(coverage$round_id,award_round_labels(coverage$round_number,coverage$round_id))
    }else character()
    selected<-isolate(input$award_round)
    if(length(selected)!=1L||!selected%in%choices)selected<-head(unname(choices),1L)
    updateSelectInput(session,"award_round",choices=choices,selected=selected)
  })
  output$award_actions<-renderUI({req(is_module_active(),authorized());snapshot<-awards();rows<-snapshot$coverage
    eligible<-is.data.frame(rows)&&nrow(rows)&&any(rows$selection=="Missing")
    button<-function(id,label,enabled)actionButton(session$ns(id),label,disabled=if(enabled&&!isTRUE(snapshot$running)&&!isTRUE(snapshot$loading))NULL else "disabled",style="white-space:normal;max-width:100%;margin-bottom:8px;")
    tagList(button("btn_sync_dreamteams","Retrieve missing round awards",eligible),
      button("btn_refresh_awards","Refresh provisional or changed rounds",is.data.frame(rows)&&nrow(rows)>0L),
      button("btn_retry_award_saves","Retry failed saves",isTRUE(snapshot$unsaved>0L)))
  })
  output$awards_coverage<-reactable::renderReactable({req(is_module_active(),authorized());rows<-awards()$coverage
    if(!is.data.frame(rows)||!nrow(rows))return(reactable::reactable(data.frame(Status="Round catalog unavailable or still loading.")))
    reactable::reactable(rows,columns=list(round_id=reactable::colDef(show=FALSE),required=reactable::colDef(show=FALSE),round_number=reactable::colDef(name="Round",cell=function(value,index)award_round_labels(value,rows$round_id[index])),selection=reactable::colDef(name="Selection"),finality=reactable::colDef(name="Finality"),mvp=reactable::colDef(name="MVP"),last_fetched=reactable::colDef(name="Last fetched"),persistence=reactable::colDef(name="Saved status"),issue=reactable::colDef(name="Issue")),defaultSorted=list(round_number="desc"),compact=TRUE,wrap=TRUE)
  })
  output$awards_progress<-renderText({req(is_module_active(),authorized());snapshot<-awards()
    paste0(snapshot$fetched %||% 0," fetched; ",snapshot$saved %||% 0," confirmed saved; ",snapshot$failed %||% 0," failed; ",snapshot$pending %||% 0," pending. ",snapshot$reason)
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
