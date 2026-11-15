#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
# Rendering the whole document catches lost htmlDependency/tagList classes.
page<-source('ui.R')$value
html<-htmltools::renderTags(page)$html
doc<-xml2::read_html(html)
panels<-xml2::xml_find_all(doc,'//details[contains(@class,"fm-info-panel")]')
stopifnot(length(panels)==11L,all(!is.na(xml2::xml_attr(panels,'hidden'))),
 length(xml2::xml_find_all(doc,'//div[@id="shiny-tab-today"]//h2[text()="Today\'s market"]'))==1L)
inline<-as.character(fm_info('Amount is required.',error=TRUE,inline=TRUE))
stopifnot(grepl('role="alert"',inline,fixed=TRUE),!grepl('<summary|<details',inline),
 grepl('data-fm-info-source="Coach"',as.character(fm_info('Already hired.',source='Coach')),fixed=TRUE),
 is.null(fm_info('Waiting',pending=TRUE,inline=TRUE)))
cat('PASS all 11 page panels render with dependencies intact, one heading, explicit source labels and visible action validation\n')
# Identical metadata must republish after account/league reset; opening or source
# clock changes do not increment this revision or trigger data acquisition.
shiny::testServer(function(input,output,session){
 session$userData$information_context_revision<-shiny::reactiveVal(0L)
 output$info<-shiny::renderUI(fm_info('Healthy source.',source='Source'))
 shiny::observeEvent(input$context,session$userData$information_context_revision(input$context))
},{
 session$flushReact();before<-output$info$html
 session$setInputs(context=1L);session$flushReact();after<-output$info$html
 stopifnot(!identical(before,after),grepl('data-fm-info-context="1"',after,fixed=TRUE),
 grepl('Healthy source.',after,fixed=TRUE))
})
cat('PASS unchanged healthy metadata republishes for a new context without acquisition\n')
