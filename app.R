# Packages ----
f_load_packages = function(){
  library(ggiraph)
  library(ggplot2)
  library(shiny)
  library(magrittr)
  library(dplyr)
  library(tidyr)
  library(shinyWidgets)
  library(bslib)
  library(thematic)
  library(stringr)
  library(shinyhttr)
  library(httr)
  library(inplace)
  library(kableExtra)
  library(golem)
  library(shinyBS)
  library(DT) # pour les data tables interactifs
  library(reactable)
  library(shinyalert) # boîtes dialogue
  library(shinydashboard)
  library(dashboardthemes)
  library(fontawesome)
  #library(shinydashboardPlus)
  print("Packages chargés !")
}
f_load_packages()


folder_path = "C:/Users/a/Documents/R/Projet/TerraformingMars/TM/"

source(paste0(folder_path,"global.R"))
source(paste0(folder_path,"styles/symboleManager.R"))
source(paste0(folder_path,"data.R"))
source(paste0(folder_path,"modules.R"))
source(paste0(folder_path,"ui.R"))
source(paste0(folder_path,"server.R"))

# Launcher ----

shinyApp(ui,server)

