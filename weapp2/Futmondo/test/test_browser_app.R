# Synthetic, HTTP-blocked app used only by test_browser_theme.cjs.
expressions <- parse('test/test_application_offline.R')
for(expr in expressions) {
  if(is.call(expr) && identical(expr[[1]],as.name('app_check')) &&
     identical(expr[[2]],'full server login, navigation, refresh and logout')) break
  eval(expr,envir=.GlobalEnv)
}
browser_ui <- source('ui.R',local=.GlobalEnv)$value
shiny::runApp(shiny::shinyApp(ui=browser_ui,server=app_server),host='127.0.0.1',port=38765,launch.browser=FALSE)
