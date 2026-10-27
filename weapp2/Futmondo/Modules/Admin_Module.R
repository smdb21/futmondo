library(reactable)
get_table_definitions <- function() {
  tables<-supabase_known_tables()
  data.frame(table_name=tables,description=gsub("_"," ",tables),identity_column=vapply(tables,supabase_primary_key,character(1)),stringsAsFactors=FALSE)
}

admin_UI <- function(id) {
 ns<-NS(id)
 tagList(h2("Database administration"),p("Inspect retained factual tables. Changes require an authorized administrator."),
  actionButton(ns("btn_refresh_stats"),"Refresh table counts"),reactable::reactableOutput(ns("tables_stats_table")),
  actionButton(ns("btn_verify_db"),"Verify database connection"),actionButton(ns("btn_sync_dreamteams"),"Save official round MVP results"),
  actionButton(ns("btn_reset_db"),"Reset retained database tables",class="btn-danger"))
}
admin_Server <- function(id,is_module_active,login_token,championship_id,user_team_id,user_teams_RV,is_admin=NULL) {
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
  observeEvent(input$btn_sync_dreamteams,{
   req(is_module_active(),authorized(),championship_id());result<-tryCatch(sync_all_championship_dreamteams(login=login_token(),championship_id=championship_id(),verbose=FALSE),error=function(e)NULL)
   showNotification(if(is.null(result))"Official round results could not be saved."else mvp_sync_message(result),type="message")
  })
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
