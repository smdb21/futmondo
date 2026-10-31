library(reactable)
get_table_definitions <- function() {
  tables<-supabase_known_tables()
  data.frame(table_name=tables,description=gsub("_"," ",tables),identity_column=vapply(tables,supabase_primary_key,character(1)),stringsAsFactors=FALSE)
}

admin_UI <- function(id) {
 ns<-NS(id)
 tagList(h2("Database administration"),p("Inspect retained factual tables. Changes require an authorized administrator."),
  actionButton(ns("btn_refresh_stats"),"Refresh table counts"),reactable::reactableOutput(ns("tables_stats_table")),
  actionButton(ns("btn_verify_db"),"Verify database connection"),
  h3("Round awards"),p("Saved selections improve reward estimates. Historical eligibility and payment coverage are checked separately."),
  uiOutput(ns("award_actions")),
  selectInput(ns("award_round"),"Round to refresh",choices=character()),actionButton(ns("btn_refresh_selected_award"),"Refresh selected round"),
  textOutput(ns("awards_progress")),reactable::reactableOutput(ns("awards_coverage")),
  actionButton(ns("btn_reset_db"),"Reset retained database tables",class="btn-danger"))
}
admin_Server <- function(id,is_module_active,login_token,championship_id,user_team_id,user_teams_RV,is_admin=NULL,refresh_trigger=NULL) {
 moduleServer(id,function(input,output,session){
  ns<-session$ns
  authorized<-reactive(valid_login(login_token()) && if(is.function(is_admin))isTRUE(is_admin())else if(!is.null(is_admin))isTRUE(is_admin)else is_authorized_admin(login_token()))
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
