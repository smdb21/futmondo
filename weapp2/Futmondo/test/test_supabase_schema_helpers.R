#!/usr/bin/env Rscript
options(futmondo.offline=TRUE)
suppressPackageStartupMessages(source('supabase_connector.R'))
tables<-supabase_known_tables();stopifnot(length(tables)==15L,!anyDuplicated(tables),!any(grepl('automation|forecast|dna|decision|alert',tables)))
stopifnot(supabase_primary_key('players')=='id',supabase_primary_key('player_match_observations')=='player_id',supabase_primary_key('observation_revisions')=='championship_id')
cat('PASS factual registry and composite identity probes\n')
calls<-list();supabase_delete<-function(table_name,filter){calls[[length(calls)+1L]]<<-list(table=table_name,filter=filter);list(status='deleted')}
stopifnot(supabase_delete_all('player_match_observations')$status=='deleted',calls[[1]]$filter=='player_id=not.is.null')
stopifnot(supabase_delete_all('retired_table')$status=='error',length(calls)==1L)
stopifnot(length(supabase_reset_database(FALSE))==0L,length(calls)==1L)
x<-supabase_reset_database(TRUE);stopifnot(setequal(names(x),tables),tail(names(x),1)=='championships')
cat('PASS deletion allowlist, confirmation gate and child-first reset\n')
get_sb_url<-function()'https://fixture.invalid';get_sb_key<-function()'fixture'
requests<-list();fail_table<-'';missing_count<-FALSE
GET<-function(url,query,...){table<-sub('^.*/','',url);requests[[length(requests)+1L]]<<-list(table=table,query=query);structure(list(status_code=if(table==fail_table)400L else 200L,headers=if(missing_count)list()else list('content-range'='*/7'),content=charToRaw('[]')),class='response')}
x<-get_table_row_counts();stopifnot(nrow(x)==15L,all(x$row_count==7L),all(vapply(requests,function(r)r$query$select==supabase_primary_key(r$table)&&r$query$limit=='0',logical(1))))
missing_count<-TRUE;x<-get_table_row_counts();stopifnot(all(is.na(x$row_count)));missing_count<-FALSE
requests<-list();stopifnot(init_supabase_db(),length(requests)==15L,all(vapply(requests,function(r)r$query$limit=='1',logical(1))))
fail_table<-'player_match_observations';stopifnot(!suppressWarnings(init_supabase_db()),is.na(get_table_row_counts()$row_count[match(fail_table,tables)]))
cat('PASS exact counts, unknown counts, minimal probes and failed-schema reporting\n')
