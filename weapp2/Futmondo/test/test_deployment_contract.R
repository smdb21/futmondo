#!/usr/bin/env Rscript
m<-jsonlite::fromJSON('manifest.json',simplifyVector=FALSE);files<-names(m$files)
private<-'\\.har$|\\.orig$|(^|/)(\\.Renviron([^/]*)?|\\.Rhistory|\\.RData|\\.env([^/]*)?|Agents\\.md|AGENTS\\.md)$|(^|/)(test|docs|scripts|\\.agents|\\.codex)(/|$)'
stopifnot(length(files)>0L,!any(grepl(private,files,ignore.case=TRUE)), !any(basename(files)=='hirecoach'))
cat('PASS deployment excludes credentials, captures, backups and development files\n')
stopifnot(all(c('curl','later','promises','openssl','sodium','httr','jsonlite','shiny')%in%names(m$packages)))
cat('PASS async transport and factual runtime dependencies are recorded\n')
required<-c('global.R','ui.R','server.R','utils.R','theme.R','futmondo_functions.R','supabase_connector.R','data_contracts.R','data_service.R','async_transport.R','remember_login.R','page_preferences.R','app_access.R','lineup_runtime.R','lineup_store.R','lineup_api.R','Modules/Lineup_Module.R','www/remember-login.js','www/theme-switch.js','history_repository.R','history_baseline_store.R','background_runtime.R','mvp_runtime.R','award_runtime.R','coach_runtime.R','rival_finance_runtime.R','rival_calculation_store.R','rival_ownership_runtime.R','round_settlement_runtime.R','factual_helpers.R','player_columns_to_hide.txt',paste0('Modules/',c('Login','Players_Table','Selected_Player','Market','Players_in_Championship','Players_in_Teams','Rivals','Plots','Classification','Round_MVPs','Notifications','Admin','Today'),'_Module.R'))
retired<-c('intelligence_engine.R','prediction_engine.R','automation_runtime.R','portfolio_engine.R','insights_runtime.R','Modules/Intelligence_Module.R','Modules/Automation_Module.R','scripts/automation_worker.R','scripts/populate_db.R')
stopifnot(all(required%in%files),!any(retired%in%files),all(file.exists(files)))
cat('PASS factual files are bundled and retired engines absent\nDEPLOYMENT: 3 passed / 0 failed\n')
