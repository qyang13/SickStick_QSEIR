library(shiny)
library(ggplot2)
library(shiny.semantic)
library(semantic.dashboard)
library(ggpubr)
library(DT)
source("helpers.R")

ui <- dashboardPage(
  dashboardHeader(img(src = "logo.png", height= 60, width = 500)),
  dashboardSidebar(side = "left",style="overflow-y: scroll",
                   sidebarMenu(
                     menuItem(tabName = "plot_tab", text = "Simulation & Plot", icon = icon("chart line")),
                     #menuItem(tabName = "spec_tab", text = "Simulation Specs", icon = icon("cog")),
                     menuItem(tabName = "table_tab", text = "Data Summary", icon = icon("clipboard")),
                     menuItem(tabName = "empirical_tab", text = "Empirical Summary", icon = icon("balance scale")))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "plot_tab",
              fluidRow(
                box(title = "Configurations", color = "blue", title_side = "top left",collapsible = F, 
                    width = 4,
                    # Outbreak Scenario Selection
                    # box(title = "Outbreak Scenario Selection", color = "grey", title_side = "top left",
                    #     br(),
                    #     radioButtons("OS", label="",
                    #                  choices = list("Common Cold (Ro = 0.5)  " = 1, "Influenza (Ro = 0.9)  " = 2, 
                    #                                 "Ebola (Ro = 2.0)  " = 3, "2019-CoV (Ro = 3.5)  " = 4, "Measles (Ro = 15.0)  " = 5)),
                    #     br()
                    # )
                    box(
                      title="Basic Configurations", color = "grey", title_side = "top left", collapsed=T,
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
                     )
                    ,
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
                  plotOutput("graph1", height = 630), width = 9),
                box(
                  title = "Simulation Summary", color = "blue", title_side = "top left",collapsible = F,
                  width = 3,
                  valueBox(textOutput('Ro'), ("R。"), color = "red",  size = "mini"),
                  br(),
                  hr(),
                  h2("Total Sick Days per Person:"), 
                  valueBox("With SickStick", textOutput('sd_with_ss'), icon("hospital"), color = "green",  size = "mini"),
                  br(),
                  valueBox("Without SickStick", textOutput('sd_without_ss'), icon("hospital"), color = "grey",  size = "mini"),
                  br(),
                  hr(),
                  h2("Total Infections:"), 
                  valueBox("With SickStick", textOutput('ti_with_ss'), icon("heartbeat"), color = "green", size = "mini"),
                  br(),
                  valueBox("Without SickStick", textOutput('ti_without_ss'), icon("heartbeat"), color = "grey", size = "mini"),
                  br()
                )
              )
      ),
      
      tabItem(tabName = "table_tab",
                box(title = "Data Summary for without SickStick", color = "blue", ribbon = FALSE,
                    title_side = "top left", width = 14,
                    tags$div(
                      dataTableOutput("summary_table_nm")
                      , style = paste0("color:", semantic_palette[["blue"]], ";"))
                ),
                box(title = "Data Summary for with SickStick", color = "blue", ribbon = FALSE,
                    title_side = "top left", width = 14,
                    tags$div(
                      dataTableOutput("summary_table_ss")
                      , style = paste0("color:", semantic_palette[["blue"]], ";"))
                )
        ),
      #tabItem(tabName = "spec_tab"),
      tabItem(tabName = "empirical_tab", box(img(src = "todo.jpeg"), width = 12))
      )

  ), theme = "slate"
)

server <- function(input, output) {
  
  
  dat_nm = reactive({runModel(
    input$T_max*30,
    input$N,
    FALSE,
    
    input$TP, # TP of SickStick X/100
    input$TN, # TN of SickStick X/100
    
    input$R0,
    1/input$gamma,
    1/input$sigma,
    input$r_Q/100,
    input$r_RS/100
  )
  })
  
  dat_ss = reactive({runModel(
    input$T_max*30,
    input$N,
    TRUE,
    
    input$TP, # TP of SickStick X/100
    input$TN, # TN of SickStick X/100
    
    input$R0,
    1/input$gamma,
    1/input$sigma,
    input$r_Q/100,
    input$r_RS/100
  )
  })
  output$Ro <- renderText(input$R0)
  output$sd_with_ss <- renderText(as.integer((sum(dat_ss()[,3]+dat_ss()[,8]))/input$N))
  output$sd_without_ss <- renderText(as.integer((sum(dat_nm()[,3]+dat_ss()[,8]))/input$N))
  output$ti_with_ss <- renderText(as.integer(sum(dat_ss()[,3])))
  output$ti_without_ss <- renderText(as.integer(sum(dat_nm()[,3])))
  
  
  
  output$graph1 <- renderPlot({
    dat_nm <- dat_nm()
    dat_ss <- dat_ss()
    
    
    long_nm <- data.frame(
      Period = rep(1:nrow(dat_nm),5), 
      Population = c(dat_nm[,1], dat_nm[,2], dat_nm[,3], dat_nm[,4], dat_nm[,8]), 
      Indicator=rep(c("Uninfected", "Exposed", "Infected", "Recovered", "Quarantined"), 
                    each=nrow(dat_nm)),
      SickStick=rep(0, 5*nrow(dat_nm)))
    
    
    long_ss <- data.frame(
      Period = rep(1:nrow(dat_ss),5), 
      Population = c(dat_ss[,1], dat_ss[,2], dat_ss[,3], dat_ss[,4], dat_ss[,8]), 
      Indicator=rep(c("Uninfected", "Exposed", "Infected", "Recovered", "Quarantined"), 
                    each=nrow(dat_ss)),
      SickStick=rep(1, 5*nrow(dat_ss)))
    
    long <- rbind(long_nm, long_ss)
    
    labels <- c("0" = "Before SickStick", "1" = "After SickStick")
    
    p <- ggplot(long,
                aes(x=Period/30, y=Population/input$N, group=Indicator))    
    p <- p + geom_line(aes(colour = Indicator), size=2) + 
      ggtitle("") +
      facet_grid(. ~ SickStick, labeller=labeller(SickStick = labels)) +
      labs(x = "Time (Months)", y = "Fraction of the population") +
      theme_linedraw() + 
      theme(strip.text.x = element_text(size=20)) + theme(plot.background = element_rect(fill = "#ebebeb"))+ 
      scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
    print(p)
  })
  

output$summary_table_nm <- renderDataTable(dat_nm(), options = list(dom = 't'))
output$summary_table_ss <- renderDataTable(dat_ss(), options = list(dom = 't'))

}

shinyApp(ui = ui, server = server)


# Code for deployment
# library(rsconnect)
# rsconnect::deployApp('~/Documents/SickStick_QSEIR/SickStickModel/')
