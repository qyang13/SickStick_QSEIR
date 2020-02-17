model_tab <- tabPanel(title="Populational Model", value = "model_tab",
                      fluidRow(column(width = 6,
                                      h3("Total Sick Days per Person"),
                                      HTML('<hr style="color: black;">'),
                                      fluidRow(valueBoxOutput("sd_with_ss"),
                                               valueBoxOutput("sd_without_ss"))
                                      ),
                               column(width = 6,
                                      h3("Total Cases of Infections"),
                                      HTML('<hr style="color: black;">'),
                                      fluidRow(valueBoxOutput("ti_with_ss"),
                                               valueBoxOutput("ti_without_ss"))
                                      )
                               ),
                       fluidRow(
                         box(width = 12,
                             title = "Population Plot",
                             collapsible = F,
                             plotOutput("graph1")
                             )
                         )
                       )


# valueBox(textOutput('Ro'), ("R。"), color = "red",  size = "mini",width = 2),
# br(),
# hr(),