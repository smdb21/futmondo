# Synthetic transport for the real catalog, ranking, bound reads and scheduler.
# Source global.R in offline mode first. No remote reads, credentials or writes.
fixture <- new.env(parent=emptyenv())
fixture$auth <- c(token='synthetic',userid='classification-cold-fixture')
fixture$requests <- list();fixture$pending <- list();fixture$saved <- list();fixture$writes <- 0L
fixture$finance <- shiny::reactiveVal('pending')
fixture$active <- list(championships=list(list(id='league',league='competition',userteam=list(id='a'))),
  rounds=list(list(`_id`='r13',number=13,championshipId='competition')))
fixture$leagues <- list(list(`_id`='competition',rounds=lapply(1:38,function(n)
  list(`_id`=paste0('r',n),status=if(n<12)'done'else if(n==12)'next'else'future'))))
fixture$calendar <- list(rounds=lapply(38:1,function(n)list(`_id`=paste0('r',n),number=n,
  status=if(n<12)'done'else if(n==12)'playing'else'future')))
fixture$ranking <- list(metadata=list(status='playing'),ranking=list(
  list(id='a',name='Alpha',points=26),list(id='b',name='Beta',points=18)))
get_sb_url <- function()''
get_sb_key <- function()''
supabase_post <- function(...)invisible(TRUE)
read_rival_finance_evidence <- function(...)list(evidence=fixture$saved,complete=TRUE)
persist_rival_finance_evidence <- function(evidence,context){
  fixture$saved <- c(fixture$saved,evidence);fixture$writes <- fixture$writes+1L;TRUE
}
shared_mvp_source <- function(...)shiny::reactive(list())
shared_rival_finance_source <- function(...)shiny::reactive({
  state <- fixture$finance()
  if(state=='pending')shiny::validate(shiny::need(FALSE,'Loading financial details…'))
  if(state=='failed')stop('Synthetic finance source unavailable')
  list(loading=FALSE)
})
fm_async_http <- function(url,method='GET',body=NULL,headers=character(),owner='',...){
  stopifnot(method=='POST',startsWith(url,'https://api.futmondo.com/'))
  query <- jsonlite::fromJSON(body,simplifyVector=FALSE)$query
  key <- if(identical(url,ROUNDS_URL))paste0(url,':',query$userteamId)else url
  fixture$requests[[length(fixture$requests)+1L]] <- list(url=url,query=query,key=key)
  promises::promise(function(resolve,reject)fixture$pending[[key]] <- list(resolve=resolve,reject=reject))
}
fixture_reply <- function(key,answer=NULL,error=NULL) {
  request <- fixture$pending[[key]];stopifnot(!is.null(request))
  fixture$pending[[key]] <- NULL
  if(!is.null(error))request$reject(error)else request$resolve(list(status_code=200L,
    content=charToRaw(jsonlite::toJSON(list(answer=answer),auto_unbox=TRUE,null='null',na='null')),
    times=c(total=0)))
}
fixture_reads <- function(url)sum(vapply(fixture$requests,function(x)identical(x$url,url),logical(1)))
fixture_settle <- function(session)for(i in 1:25){later::run_now(0);session$flushReact()}
fixture_server <- function(input,output,session) {
  login <- shiny::reactive(fixture$auth);league <- shiny::reactive('league');team <- shiny::reactive('a')
  directory_read <- factual_read_adapter(session,'get_teams',login,league,team)
  directory <- shiny::reactive(directory_read(login(),'league'))
  primary_read <- factual_read_adapter(session,'get_user_team_info',login,league,team,state_only=TRUE)
  settings <- shiny::reactive({
    state <- primary_read(login(),'league','a')
    list(league=list(),info=state$data %||% list(),status=state$status,error=state$error)
  })
  classification_Server('classification',shiny::reactive(TRUE),login,league,team,directory,
    league_settings_source=settings)
}
options(futmondo.offline=FALSE)
