#!/usr/bin/env Rscript
options(futmondo.offline = TRUE)
suppressPackageStartupMessages({
  library(shiny); library(shinydashboardPlus); library(reactable); library(htmlwidgets)
  source("Modules/Today_Module.R")
})
recs <- data.frame(type=c("Buy","Sell","Bid","Clause","Hold"), id=1:5)
stopifnot(identical(today_filter_recommendations(recs,c("Buy","Bid"))$type,c("Buy","Bid")),
  nrow(today_filter_recommendations(recs,character()))==0L)
html <- as.character(today_UI("today"))
stopifnot(grepl("today-recommendation_types",html,fixed=TRUE),
  grepl("cart-shopping",html,fixed=TRUE),grepl("hand-holding-dollar",html,fixed=TRUE),
  grepl("Clause",html,fixed=TRUE),grepl("Hold",html,fixed=TRUE))
module <- paste(readLines("Modules/Today_Module.R",warn=FALSE),collapse="\n")
stopifnot(grepl("today-player-card-button",module,fixed=TRUE),
  grepl('title = "Show player card"',module,fixed=TRUE),
  grepl('icon("address-card")',module,fixed=TRUE),
  grepl('today_rec_action_onclick_js(ns, pid, "view")',module,fixed=TRUE),
  grepl('paste0("rec_view_", i, "_", pid)',module,fixed=TRUE))
css <- paste(readLines("www/custom_style.css",warn=FALSE),collapse="\n")
stopifnot(grepl(".today-action-filters",css,fixed=TRUE),grepl("flex-wrap: wrap",css,fixed=TRUE),
  grepl(".today-player-card-button",css,fixed=TRUE))
cat("COMMAND CENTER PRESENTATION: 2 passed / 0 failed\n")
