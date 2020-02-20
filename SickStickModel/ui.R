library(shinydashboard)

source("model_tab.R")
source("table_tab.R")
source("empirical_tab.R")
source("sidebar_config.R")
source("style.R")

dash_header <- dashboardHeader( 
  title = "SickStick Model"
)

dash_body <- dashboardBody(
  body_style,
  navbarPage(title = NULL,
             model_tab,
             table_tab,
             empirical_tab
  )
)

ui <- dashboardPage(skin = "black",
                    dash_header, 
                    dash_sidebar,
                    dash_body
)
