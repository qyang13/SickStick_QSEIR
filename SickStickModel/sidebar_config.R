library(shinydashboard)
library(shinyWidgets)

nsliders = 12

### Defines simulation input options (left sidebar) #### 

dash_sidebar <- dashboardSidebar(width = 400 ,
    ###########################################################################
    # Set slider color
    setSliderColor(rep("#D9524F", nsliders), seq(1,nsliders)),
    
    ###########################################################################
    # Column container for all the boxes
    column(width = 12, style = "height:90vh; overflow-y: scroll;",
        #######################################################################
        # SickStick Parameters
        br(),
        box(width = NULL, collapsible = T, collapsed = F, 
          title = "SickStick Parameters",
          sliderInput("TP", "True Positive Rate (%):", 
                      min=0, max=100, value=95, step=1),
          sliderInput("TN", "True Negative Rate (%):", 
                      min=0, max=100, value=95, step=1)
        ),
        #######################################################################
        # SickStick Deployment Strategy
        box(width = NULL, collapsible = T, collapsed = F, 
          title = "SickStick Deployment Strategy", 
          
          # KMB: Remove this option for now - not sure if we'll need it
          # sliderInput("DQ", "Consecutive Negative Results in Quarantine (Days):",
          #             min=1, max=10, value=1, step=1),
          selectInput("Stg", "SickStick Deployment Strategy:", choices = list("Everyday, Everyone" = 1, "Some days, Everyone" = 2,
                                                                              "Everyday, Some people" = 3), 
                                                                              selected = 1),
          conditionalPanel(
            condition = "input.Stg == 2",
            sliderInput("SS_stg", "Every # of days:",
                        min=1, max=7, value=5, step=1)),
          #KMB: Remove this option for now - TODO: add later
          conditionalPanel(
            condition = "input.Stg == 3",
            sliderInput("SS_percent", "Percentage of population (%):",
                        min=0, max=100, value=90, step=5))),

        #######################################################################
        #Outbreak Scenario Selection
        box(width = NULL, collapsible = T, collapsed = F, 
            title = "Outbreak Scenario Selection",
            awesomeRadio(
              inputId = "OS",
              label = "",
              choices = list("Common Cold (Ro = 0.5)  " = 1, "Influenza (Ro = 0.9)  " = 2,
                             "Ebola (Ro = 2.0)  " = 3, "COVID-19 (Ro = 3.5)  " = 4, "Measles (Ro = 9.0)  " = 5, "Customize Scenario" =6),
              selected = 1,
              status = "danger",
              checkbox = TRUE
            )
        ),


        #######################################################################
        # Detailed infection parameters configuration
        conditionalPanel(
          condition = "input.OS == 6",
          box(width = NULL, collapsible = T, collapsed = F, 
              title="Infection Configurations", 
              sliderInput("T_max", "Total Time (Months):", 
                          min=0, max=12, value=2, step=1),
              sliderInput("N", "Total Population:", 
                          min=0, max=1000, value=100, step=50),
              sliderInput("R0", "Disease Severity (R0) Value:", 
                          min=0, max=18, value=2.5, step=0.1),
              sliderInput("T_contagious", "Contagious time (Days):", 
                          min=0, max=30, value=7, step=1),
              sliderInput("T_incubate", "Incubation time (Days):", 
                          min=0, max=30, value=2, step=1),
              sliderInput("r_Q", "Percent of those who self-quarantine each day:", 
                            min=0, max=100, value=15, step=1),
              radioButtons("r_RS", "After infection:",
                           c("Remain immune" = -1,
                             "Return to susceptible" = 0)),
              #sliderInput("r_RS", "Disease Reoccurance Rate:", 
              #            min=0, max=100, value=10, step=1)
          )
        ),
        
        actionButton('run', 'Run Simulation', icon("forward"), width = NULL)),
        tags$footer('Contributors: Qing Yang & Kate Bubar', style="font-size:8pt;color:grey;padding-left:10px;")
)
