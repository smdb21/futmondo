#!/usr/bin/env Rscript
# Static manifest/package checks; no deployment or authentication.
m<-jsonlite::fromJSON('manifest.json',simplifyVector=FALSE)
files<-names(m$files)
stopifnot(!any(grepl('\\.har$|(^|/)\\.Renviron$',files)))
cat('PASS deployment excludes credentials and private captures\n')
stopifnot(all(c('callr','openssl','later')%in%names(m$packages)))
cat('PASS background/encryption runtime dependencies are recorded\n')
stopifnot(all(c('data_contracts.R','background_runtime.R','automation_runtime.R',
  'prediction_engine.R','theme.R','insights_runtime.R','portfolio_engine.R','Modules/Notifications_Module.R','Modules/Intelligence_Module.R',
  'Modules/Automation_Module.R')%in%files))
cat('PASS application bundle includes new modules and engines\n')
cat('DEPLOYMENT: 3 passed / 0 failed\n')
