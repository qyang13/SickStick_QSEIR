library(shiny)
library(ggplot2)
library(shiny.semantic)
library(semantic.dashboard)
library(ggpubr)
library(DT)
source("helpers.R")

ui <- dashboardPage(
  dashboardHeader(img(src = "logo.png", height= 60, width = 500)),
  dashboardSidebar(side = "left",
                   sidebarMenu(
                     menuItem(tabName = "plot_tab", text = "Simulation & Plot", icon = icon("chart line")),
                     menuItem(tabName = "spec_tab", text = "Simulation Specs", icon = icon("cog")),
                     menuItem(tabName = "table_tab", text = "Data Summary", icon = icon("clipboard")),
                     menuItem(tabName = "spec_tab", text = "Empirical Summary", icon = icon("balance scale")))),
  dashboardBody(
#    chooseSliderSkin(skin = "Modern",color = "#3399FF"),
    tabItems(
      tabItem(tabName = "plot_tab",
              fluidRow(
                        box(title = "Configurations", color = "blue", title_side = "top left",collapsible = F,
                            width = 4,
                            # Outbreak Scenario Selection
                            box(title = "Outbreak Scenario Selection", color = "grey", title_side = "top left",
                                br(),
                                radioButtons("OS", label="",
                                             choices = list("Common Cold (Ro = 0.5)  " = 1, "Influenza (Ro = 0.9)  " = 2, 
                                                            "Ebola (Ro = 2.0)  " = 3, "2019-CoV (Ro = 3.5)  " = 4, "Measles (Ro = 15.0)  " = 5)),
                                br()
                            ),
                            # SickStick Parameters
                            box(
                              title = "SickStick Parameters", color = "grey", title_side = "top left",
      
                              sliderInput("TP", "True Positive Rate (%):", 
                                           min=0, max=100, value=95, step=1),
                              sliderInput("TN", "True Negative Rate (%):", 
                                           min=0, max=100, value=95, step=1)
                            ),
                            # SickStick Deployment Strategy
                            box(
                                title = "SickStick Deployment Strategy", color = "grey", title_side = "top left",
                                
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
                                              min=0, max=100, value=10, step=1))
                            )
                          ),
                        box(
                            title = "Population Change Plot", color = "blue", title_side = "top left",collapsible = F,
                            plotOutput("graph1", height = 640), width = 9),
                        box(
                            title = "Simulation Summary", color = "blue", title_side = "top left",collapsible = F,
                            width = 3,
                            h2("R0:"),
                            valueBox("",1.30, icon("certificate"), color = "red",  size = "small"),
                            br(),
                            hr(),
                            h2("Total Sick Days:"), 
                            valueBox("With SickStick", 20, icon("hospital"), color = "green",  size = "small"),
                            br(),
                            valueBox("Without SickStick", 190, icon("hospital"), color = "grey",  size = "small"),
                            br(),
                            hr(),
                            h2("Total Infected:"), 
                            valueBox("With SickStick", 3, icon("wheelchair"), color = "green", size = "small"),
                            br(),
                            valueBox("Without SickStick", 291, icon("wheelchair"), color = "grey", size = "small"),
                            br()
                          )
                )
                ),
      
      tabItem(tabName = "table_tab",
              fluidRow(
                valueBox("Unread Mail", 144, icon("mail"), color = "blue", width = 6, size = "small"),
                valueBox("Spam", 20, icon("mail"), color = "red", width = 5, size = "small"),
                valueBox("Readed Mail", 666, icon("mail"), color = "green", width = 5, size = "small")
              ),
              fluidRow(
                box(title = "Classic box", color = "blue", ribbon = FALSE,
                    title_side = "top left", width = 14,
                    tags$div(
                      dataTableOutput("mtcars_table")
                      , style = paste0("color:", semantic_palette[["blue"]], ";"))
                )))),
    tabItem(tabName = "spec_tab"),
    tabItem(tabName = "empirical_tab")
  ), theme = "slate"
)

server <- function(input, output) {
  
  dat_nm = reactive({processData(
          0, # TP of SickStick X/100
          1, # TN of SickStick X/100
          2.1, # Infection rate X/100
          0.1, # Recovery rate X/100
          1, # Disease progression rate X/100
          0, # Number of days remained quarantined before clear X
          0.01, # Reverse rate R - S, can also be considered as disease reocurrence rate X/100,
          1,
          0,
          0,
          2
        )
    })
  
  dat_ss = reactive({processData(
    0.90, # TP of SickStick X/100
    0.99, # TN of SickStick X/100
    2.1, # Infection rate X/100
    0.1, # Recovery rate X/100
    1, # Disease progression rate X/100
    0, # Number of days remained quarantined before clear X
    0.01, # Reverse rate R - S, can also be considered as disease reocurrence rate X/100,
    1,
    0,
    0,
    2
  )
  })
  
  
  output$graph1 <- renderPlot({
    dat_nm <- dat_nm()[,1:5]
    dat_ss <- dat_ss()[,1:5]
    

    long_nm <- data.frame(
      Period = rep(1:nrow(dat_nm),5), 
      Population = c(dat_nm[,1], dat_nm[,2], dat_nm[,3], dat_nm[,4], dat_nm[,5]), 
      Indicator=rep(c("Uninfected", "Exposed", "Infected", "Recovered", "Quarantined"), 
                    each=nrow(dat_nm)),
      SickStick=rep(0, 5*nrow(dat_nm)))
    
    
    long_ss <- data.frame(
      Period = rep(1:nrow(dat_ss),5), 
      Population = c(dat_ss[,1], dat_ss[,2], dat_ss[,3], dat_ss[,4], dat_ss[,5]), 
      Indicator=rep(c("Uninfected", "Exposed", "Infected", "Recovered", "Quarantined"), 
                    each=nrow(dat_ss)),
      SickStick=rep(1, 5*nrow(dat_ss)))
    
    long <- rbind(long_nm, long_ss)
    
    labels <- c("0" = "Before SickStick", "1" = "After SickStick")
    
    p <- ggplot(long,
                aes(x=Period, y=Population, group=Indicator))    
    p <- p + geom_line(aes(colour = Indicator), size=2) + 
      ggtitle("") +
      facet_grid(. ~ SickStick, labeller=labeller(SickStick = labels)) +
      labs(x = "Time (days)", y = "Fraction of the population") +
      theme_linedraw() + 
      theme(strip.text.x = element_text(size=20)) + theme(plot.background = element_rect(fill = "#ebebeb"))
    print(p)
  })
  

  
  output$mtcars_table <- renderDataTable(mtcars, options = list(dom = 't'))
  
  output$dropdown <- renderDropdownMenu({
    dropdownMenu(messageItem("User", "Test message", color = "teal", style = "min-width: 200px"),
                 messageItem("Users", "Test message", color = "teal", icon = "users"),
                 messageItem("See this", "Another test", icon = "warning", color = "red"))
  })
}

shinyApp(ui = ui, server = server)
