source("futmondo_functions.R")
source("Modules/Login_Module.R")
source("Modules/Players_in_Teams_Module.R")
source("Modules/Selected_Player_Module.R")
source("Modules/Market_Module.R")
source("Modules/Players_in_Championship_Module.R")

library(shiny)
library(shinydashboardPlus)
library(dplyr)

if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}
