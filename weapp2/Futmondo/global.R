if (!isTRUE(getOption("futmondo.offline", FALSE)) && !isTRUE(getOption("futmondo.worker_helper", FALSE)) && file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

source("futmondo_functions.R")
source("factual_helpers.R")
source("supabase_connector.R")
source("background_runtime.R")
source("history_baseline_store.R")
source("clause_transfer_store.R")
source("remember_login.R")
source("page_preferences.R")
source("league_preferences.R")
source("app_access.R")
source("lineup_runtime.R")
source("lineup_store.R")
source("lineup_api.R")
source("Modules/Lineup_Module.R")
source("Modules/Login_Module.R")
source("Modules/Players_in_Teams_Module.R")
source("Modules/Selected_Player_Module.R")
source("Modules/Market_Module.R")
source("Modules/Players_in_Championship_Module.R")
source("Modules/Rivals_Module.R")
source("Modules/Classification_Module.R")
source("Modules/Round_MVPs_Module.R")
source("Modules/Players_Table_Module.R")
source("Modules/Admin_Module.R")
source("Modules/Today_Module.R")
source("Modules/Notifications_Module.R")
source("Modules/Plots_Module.R")
source("utils.R")
library(shiny)
library(shinydashboardPlus)
library(dplyr)



cfg_player_columns_to_hide <- fread(file = "player_columns_to_hide.txt", header = FALSE)[[1]]

source("worker_contract.R")
source("worker_runtime.R")
if (!isTRUE(getOption("futmondo.worker_helper", FALSE))) {
  .fm_worker_finance_original <- shared_rival_finance_source
  shared_rival_finance_source <- fm_worker_finance_source
  fm_worker_wrap_reads()
}
