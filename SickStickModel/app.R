library(shiny)
library(shinydashboard)
library(ggpubr)
library(shinythemes)
library(simcausal)


source("ui.R")
source("server.R")
source("helpers.R")
source("model_tab.R")
source("table_tab.R")
source("empirical_tab.R")
source("sidebar_config.R")
source("style.R")

shinyApp(ui = ui, server = server)

# Code for deployment
# library(rsconnect)
# rsconnect::deployApp('~/Documents/SickStick_QSEIR/SickStickModel/')

