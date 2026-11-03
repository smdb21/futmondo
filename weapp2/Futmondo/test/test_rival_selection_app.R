options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
a<-data.frame(teamid=c('a','b'),teamname=c('Alpha','Beta'),points=c(1,2))
d<-build_rival_cash_dataset(a,'fixture',rules=list(initial_budget=100),account_id='fixture')
shinyApp(fluidPage(actionButton('refresh','Update balances'),actionButton('league','Change league'),reactableOutput('tbl'),textOutput('chosen')),
 function(input,output,session){
  data<-reactiveVal(d);chosen<-reactiveVal(NULL);context<-reactiveVal(list(user='fixture',championship='one',team='a'))
  observeEvent(input$pick,{req(identical(input$pick$context,context()));chosen(input$pick$id)})
  observeEvent(input$refresh,{x<-data();x$team_finances<-x$team_finances[2:1,];x$team_finances$estimated_cash<-c(110,90);data(x)})
  observeEvent(input$league,{context(list(user='fixture',championship='two',team='a'));chosen(NULL)})
  output$tbl<-renderReactable(rivals_finance_table_widget(data(),selected_id=isolate(chosen()),selection_input='pick',selection_context=context(),table_id='tbl'))
  output$chosen<-renderText(chosen() %||% 'none')
 }) |> runApp(host='127.0.0.1',port=as.integer(Sys.getenv('FUTMONDO_TEST_PORT','8492')),launch.browser=FALSE)
