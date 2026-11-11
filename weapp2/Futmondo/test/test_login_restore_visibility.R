#!/usr/bin/env Rscript
# Synthetic module callbacks: no real credentials, database or HTTP.
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
auth <- c(token='synthetic-token',userid='synthetic-user',user_name='Fixture')
remember_login_enabled <- function() TRUE
remember_login_session <- function(session,on_restore,on_status,on_begin,on_end,on_expire) {
  session$userData$restore_callbacks <- list(begin=on_begin,end=on_end,restore=on_restore,expire=on_expire)
  list(retry=function(...)on_begin(),result=function(...)NULL,logout=function(...)NULL,cancel=function(...)NULL)
}
allowed <- TRUE
fm_app_access <- function(...)promises::promise_resolve(allowed)
settle <- function(session)for(i in seq_len(12L)){later::run_now(0);session$flushReact()}
shiny::testServer(login_Server,args=list(id='login'),{
  settle(session)
  callbacks <- session$userData$restore_callbacks
  stopifnot(identical(output$login_form_visible,'FALSE'),identical(output$signed_in,'FALSE'))
  callbacks$begin();settle(session)
  stopifnot(identical(output$login_form_visible,'FALSE'))
  callbacks$end();settle(session)
  stopifnot(identical(output$login_form_visible,'TRUE'))
  callbacks$begin();callbacks$restore(auth);settle(session);callbacks$end();settle(session)
  stopifnot(identical(output$login_form_visible,'FALSE'),identical(output$signed_in,'TRUE'))
  callbacks$expire();settle(session)
  stopifnot(identical(output$login_form_visible,'TRUE'),identical(output$signed_in,'FALSE'))
  allowed <<- FALSE
  callbacks$begin();callbacks$restore(auth);settle(session);callbacks$end();settle(session)
  stopifnot(identical(output$login_form_visible,'TRUE'),identical(output$signed_in,'FALSE'))
})
remember_login_enabled <- function() FALSE
shiny::testServer(login_Server,args=list(id='login'),{
  settle(session)
  stopifnot(identical(output$login_form_visible,'TRUE'),identical(output$signed_in,'FALSE'))
})
cat('PASS cookie probe, absent/failure completion, restoration, expiry, access denial and disabled configuration visibility\n')
