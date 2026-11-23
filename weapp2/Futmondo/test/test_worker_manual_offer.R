options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('global.R'))
fm_worker_configured<-function()TRUE
fm_worker_mode <- function(id) TRUE
auth <- c(userid = 'manual-worker-fixture', token = 'synthetic')
original_async <- async_adapter_fetch
settle <- function() for (i in 1:25) later::run_now(0)
# The top-level live exception must propagate to every nested prerequisite.
async_adapter_fetch<-original_async
get_team_acquisition_capacity_async_original<-attr(get_team_acquisition_capacity_async,"worker_original") %||% get_team_acquisition_capacity_async
received<-NULL
get_team_acquisition_capacity_async<-function(...,worker_bypass=FALSE){received<<-worker_bypass;promises::promise_resolve(list(status='partial'))}
ctx<-resource_context(auth,'league','mine')
result<-NULL
promises::then(async_adapter_fetch('get_team_acquisition_capacity',list(login=auth,championship_id='league',user_team_id='mine'),ctx,worker_bypass=TRUE),function(x)result<<-x)
settle();stopifnot(isTRUE(received),result$status=='partial')
get_team_acquisition_capacity_async<-get_team_acquisition_capacity_async_original
nested<-list()
async_adapter_fetch<-function(fn,args=list(),context,worker_bypass=FALSE,...){
 nested[[length(nested)+1L]]<<-list(fn=fn,bypass=worker_bypass)
 promises::promise_reject(resource_failure(resource=fn,category='network'))
}
result<-NULL
promises::then(get_team_acquisition_capacity_async(auth,'league','mine',ctx,worker_bypass=TRUE),function(x)result<<-x)
settle();stopifnot(length(nested)==4L,all(vapply(nested,`[[`,logical(1),'bypass')),result$status=='unavailable')
cat('PASS explicit live bypass reaches all four prerequisite reads; failed prerequisites remain unavailable without repeating acquisition\n')
