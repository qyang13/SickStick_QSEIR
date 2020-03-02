library(shinydashboard)
library(flexdashboard)

model_tab <- tabPanel(title="Populational Model", value = "model_tab",
                      h4("Simulatation Parameters"),
                      fluidRow(
                        box(width=4,gaugeOutput("Ro", height = "120px")),
                        box(width=4,gaugeOutput("Sigma", height = "120px")),
                        box(width=4,gaugeOutput("Gamma", height = "120px"))
                      ),
                      hr(),
                      h4("Without SickStick"),
                      fluidRow(column(width=4, plotOutput("graph3", height = 200)),
                               column(width=4, plotOutput("graph1", height = 200)),
                               column(width=2, shinydashboard::valueBoxOutput("qd_without_ss",width = 12)),
                               column(width=2, shinydashboard::valueBoxOutput("sd_without_ss",width = 12))),
                      hr(),
                      h4("With SickStick"),
                      fluidRow(column(width=4, plotOutput("graph4", height = 200)),
                               column(width=4, plotOutput("graph2", height = 200)),
                               column(width=2, shinydashboard::valueBoxOutput("qd_with_ss",width = 12)),
                               column(width=2, shinydashboard::valueBoxOutput("sd_with_ss",width = 12)))
                      )
