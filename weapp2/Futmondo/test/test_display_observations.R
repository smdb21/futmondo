# Source clocks remain current while unchanged visible projections stay stable.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
settle<-function(session)for(i in 1:15){later::run_now(0);session$flushReact()}
team<-function(cash=100,at=Sys.time(),stale=FALSE){
 data<-observed_data(list(budget=cash,withheld=0,teamValue=1000,configuration=list(mnmp=.5)),observed_at=at)
 attr(data,'valid_until')<-as.numeric(at)+60
 resource_state(data,status=if(stale)'stale'else'ok')
}
shiny::testServer(function(input,output,session){
 source<-shiny::reactiveVal(team());valid<-shiny::reactiveVal(TRUE)
 market<-resource_state(observed_data(data.frame(),observed_at=Sys.time()),status='empty')
 stats<-new.env(parent=emptyenv());stats$cash<-0L;stats$finance<-0L;stats$ready<-0L
 cash<-resource_display_source(session,function(){shiny::req(valid());fm_number(source()$data$budget)})
 finance<-resource_display_source(session,function()round_finance_display(source(),market))
 ready<-resource_display_source(session,function()!identical(source()$status,'loading'))
 output$cash<-shiny::renderText({stats$cash<-stats$cash+1L;cash()})
 output$finance<-shiny::renderText({stats$finance<-stats$finance+1L;finance()$message %||% finance()$class})
 output$ready<-shiny::renderText({stats$ready<-stats$ready+1L;ready()})
},{
 settle(session);stopifnot(output$cash=='100',output$ready=='TRUE')
 before<-as.list(stats);stamp<-Sys.time()
 elapsed<-system.time(for(i in 1:20){source(team(at=stamp+i));settle(session)})[['elapsed']]
 stopifnot(identical(as.list(stats),before),identical(as.numeric(source()$observed_at),as.numeric(stamp+20)),
  source()$data$budget==100)
 cat(sprintf('PASS 20 newly observed identical financial values preserve source clocks with zero cash/header/readiness rerenders (%.3fs)\n',elapsed))
 source(team(cash=200,at=stamp+21));settle(session)
 stopifnot(output$cash=='200',stats$cash==before$cash+1L,stats$finance==before$finance+1L,stats$ready==before$ready)
 cat('PASS a real changed balance updates cash and finance once without reopening readiness\n')
 source(team(cash=200,at=stamp+22,stale=TRUE));settle(session)
 stopifnot(stats$cash==before$cash+1L,stats$finance==before$finance+2L,grepl('Cached financial data',output$finance,fixed=TRUE))
 source(team(cash=200,at=stamp+23));settle(session)
 stopifnot(stats$cash==before$cash+1L,stats$finance==before$finance+3L,stats$ready==before$ready)
 cat('PASS actual stale/fresh transitions still update the financial warning without repainting unchanged money\n')
 valid(FALSE);settle(session)
 failure<-tryCatch({output$cash;NULL},error=identity)
 stopifnot(inherits(failure,'shiny.silent.error'))
 valid(TRUE);source(team(cash=300,at=stamp+24));settle(session)
 stopifnot(output$cash=='300')
 cat('PASS context validation clears prior presentation and recovery publishes the new authorized value\n')
})
