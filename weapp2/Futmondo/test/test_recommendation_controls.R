# Render the actual recommendation label/button expressions without API calls.
library(shiny)
source('data_contracts.R')
source('Modules/Today_Module.R')
nodes<-list()
walk<-function(x) {
  if(missing(x))return()
  if(!is.call(x))return()
  if(!is.null(x$class)&&is.character(x$class)&&x$class%in%c('today-heuristic-label','btn btn-primary today-recommendation-action'))nodes[[length(nodes)+1L]]<<-x
  for(child in as.list(x)[-1])walk(child)
}
for(expr in parse('Modules/Today_Module.R'))walk(expr)
stopifnot(length(nodes)==2L)
ns<-NS('today');pid<-'fixture';action_label<-'Accept';action_code<-'market_bid'
controls<-lapply(nodes,eval)
html<-as.character(tagList(tags$style(HTML(fm_theme_css())),includeCSS('www/custom_style.css'),controls))
stopifnot(grepl('today-rec_action_fixture',html,fixed=TRUE),grepl('market_bid',html,fixed=TRUE),!grepl('#f1f5f9',html,fixed=TRUE))
writeLines(html,'/tmp/futmondo-recommendation-controls.html')
cat('PASS actual recommendation controls retain label and action routing\n')
