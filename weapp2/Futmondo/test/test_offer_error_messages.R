options(futmondo.offline=TRUE)
suppressPackageStartupMessages({source("futmondo_functions.R");source("Modules/Selected_Player_Module.R")})
code<-"api.market.max_number_players_in_roster"
x<-offer_error_message(list(code=code,message=code))
stopifnot(grepl("maximum size",x),grepl("sale",x),!grepl("api[.]",x),identical(x,offer_error_message(list(message=code))))
stopifnot(offer_error_message(list(code="api.market.other",message="The owner no longer accepts offers."))=="The owner no longer accepts offers.")
stopifnot(!grepl("api[.]",offer_error_message(list(code="api.market.unknown",message="api.market.unknown"))))
stopifnot(grepl("check your active offers",offer_error_message(NULL)),grepl("check your active offers",offer_error_message(list(code="error",message="transport failed"))))
cat("PASS roster-limit translation, readable server messages, unknown-code fallback and uncertain-response guidance\n")
