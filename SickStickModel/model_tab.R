library(shinydashboard)

model_tab <- tabPanel(title="Populational Model", value = "model_tab",
                      fluidRow(column(width = 6,
                                      h3("Total Quarantine Days per Person"),
                                      HTML('<hr>'),
                                      fluidRow(valueBoxOutput("sd_with_ss",width = 6),
                                               valueBoxOutput("sd_without_ss",width = 6))
                                      ),
                               column(width = 6,
                                      h3("Total Days in I (symptomatic and not quarantined)"),
                                      HTML('<hr>'),
                                      fluidRow(valueBoxOutput("ti_with_ss",width = 6),
                                               valueBoxOutput("ti_without_ss",width = 6))
                                      )
                               ),
                       fluidRow(
                         # box(width = 12,
                         #     title = "Population Plot",
                         #     collapsible = F,
                             plotOutput("graph1", height = 500)
                             # )
                         )
                       )


# valueBox(textOutput('Ro'), ("R。"), color = "red",  size = "mini",width = 2),
# br(),
# hr(),