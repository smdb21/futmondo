if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

source("futmondo_functions.R")
source("supabase_connector.R")
source("Modules/Login_Module.R")
source("Modules/Players_in_Teams_Module.R")
source("Modules/Selected_Player_Module.R")
source("Modules/Market_Module.R")
source("Modules/Players_in_Championship_Module.R")
source("Modules/Rivals_Module.R")
source("Modules/Classification_Module.R")
source("Modules/Players_Table_Module.R")
source("utils.R")
library(shiny)
library(shinydashboardPlus)
library(dplyr)

init_supabase_db(verbose = FALSE)

cfg_player_columns_to_hide <- fread(file = "player_columns_to_hide.txt", header = FALSE)[[1]]

