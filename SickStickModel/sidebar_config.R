nsliders = 12


dash_sidebar <- dashboardSidebar(width = 300 , 
                        # Outbreak Scenario Selection
                        # box(title = "Outbreak Scenario Selection", color = "grey", title_side = "top left",
                        #     br(),
                        #     radioButtons("OS", label="",
                        #                  choices = list("Common Cold (Ro = 0.5)  " = 1, "Influenza (Ro = 0.9)  " = 2, 
                        #                                 "Ebola (Ro = 2.0)  " = 3, "2019-CoV (Ro = 3.5)  " = 4, "Measles (Ro = 15.0)  " = 5)),
                        #     br()
                        # )
                        
                        setSliderColor(rep("#DF4577", nsliders), seq(1,nsliders)),
                        column(width = 12, style = "height:800px; overflow-y: scroll;",
                        # SickStick Parameters
                        box(width = NULL, collapsible = T, collapsed = F, 
                          title = "SickStick Parameters",
                          
                          sliderInput("TP", "True Positive Rate (%):", 
                                      min=0, max=100, value=95, step=1),
                          sliderInput("TN", "True Negative Rate (%):", 
                                      min=0, max=100, value=95, step=1)
                        ),
                        # SickStick Deployment Strategy
                        box(width = NULL, collapsible = T, collapsed = F, 
                          title = "SickStick Deployment Strategy", 
                          
                          sliderInput("DQ", "Consecutive Negative Results in Quarantine (Days):",
                                      min=1, max=10, value=1, step=1),
                          selectInput("Stg", "SickStick Deployment Strategy:", choices = list("Everyday, Everyone" = 1, "Some days, Everyone" = 2,
                                                                                              "Everyday, Some people" = 3), selected = 1),
                          conditionalPanel(
                            condition = "input.Stg == 2",
                            sliderInput("Fq", "Every # of days:",
                                        min=1, max=15, value=5, step=1)),
                          conditionalPanel(
                            condition = "input.Stg == 3",
                            sliderInput("Fr", "Percentage of population (%):",
                                        min=0, max=100, value=10, step=1))),
                        
                          box(width = NULL, collapsible = T, collapsed = T, 
                              title="Infection Configurations", 
                              sliderInput("T_max", "Total Time (Months):", 
                                          min=0, max=36, value=12, step=1),
                              sliderInput("N", "Total Population:", 
                                          min=0, max=1000, value=300, step=100),
                              sliderInput("R0", "Disease Severity (Ro Value):", 
                                          min=0, max=20, value=1.5, step=0.1),
                              sliderInput("gamma", "Recovery time (Days):", 
                                          min=0, max=100, value=60, step=1),
                              sliderInput("sigma", "Incubation time (Days):", 
                                          min=0, max=100, value=20, step=1),
                              sliderInput("r_Q", "Sympotom-based self quarantine rate:", 
                                          min=0, max=100, value=50, step=1),
                              sliderInput("r_RS", "Disease Reoccurance Rate:", 
                                          min=0, max=100, value=50, step=1)
                          ),
                    
                        actionButton('run', 'Run Simulation', icon("forward"), width = NULL))
)