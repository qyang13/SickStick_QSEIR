library(shinydashboard)
library(flexdashboard)

model_tab <- tabPanel(title="Populational Model", value = "model_tab",
                      h4("Simulatation Parameters"),
                      fluidRow(
                        box(width=4,gaugeOutput("Ro", height = "120px")),
                        box(width=4,gaugeOutput("Sigma", height = "120px")),
                        box(width=4,gaugeOutput("Gamma", height = "120px"))
                      ),
                      fluidRow(column(width=10,
                                      fluidRow(plotOutput("graph2", height = 300)),
                                      fluidRow(plotOutput("graph1", height = 300))),
                               column(width=2,
                                      br(),br(),br(),br(),
                                      shinydashboard::valueBoxOutput("sd_with_ss",width = 12),
                                      shinydashboard::valueBoxOutput("sd_without_ss",width = 12)))
                      )
