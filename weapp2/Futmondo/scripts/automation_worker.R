#!/usr/bin/env Rscript
# Launch from the project root: Rscript scripts/automation_worker.R [--once]
# No source files outside this project are used.
if(!file.exists("automation_runtime.R")) stop("Run the worker from the Futmondo project root.")
if(file.exists(".Renviron")) readRenviron(".Renviron")
source("futmondo_functions.R")
source("supabase_connector.R")
source("intelligence_engine.R")
source("prediction_engine.R")
source("automation_runtime.R")
source("insights_runtime.R")
source("portfolio_engine.R")
source("Modules/Notifications_Module.R")
invisible(automation_key()) # validate without displaying its value
once <- "--once" %in% commandArgs(trailingOnly=TRUE)
repeat {
  tryCatch(schedule_observations(),error=function(e)message("Observation schedule unavailable; retrying later."))
  tryCatch(schedule_automation_policies(),error=function(e)message("Policy collection unavailable; retrying later."))
  for(i in seq_len(100L)) {
    job<-claim_automation_job()
    if(is.null(job)) break
    result<-tryCatch(run_automation_job(job),error=function(e)list(status="uncertain",reason="Worker interrupted; reconciliation required"))
    finished <- finish_automation_job(job,result)
    if(!isTRUE(finished)) message("Job finalization unavailable; lease retained for reconciliation.")
    record_automation_alert(job,result)
    message("Job ",job$id,": ",result$status)
  }
  if(once) break
  Sys.sleep(30)
}
