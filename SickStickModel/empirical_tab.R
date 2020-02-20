library(shinydashboard)

ht = 550 
empirical_tab <- tabPanel(title="Empirical Plots",
                          value="empirical_tab",
                          fluidRow(column(width=12, selectInput("select", h4("Select parameter to see how it affects SickStick efficacy"), 
                                      choices = list("Ro Value" = 1, "Population size"=2,  "Recovery Time" = 3,
                                                     "Incubation Time" = 4, "Self-reporting Quarantine Rate" = 5), selected = 1, width = "60%"))),
                          hr(),
                          fluidRow(plotOutput("graph_emp", height = ht))
                          
)
