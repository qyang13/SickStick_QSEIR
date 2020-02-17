library(shiny)
library(ggplot2)
library(shinydashboard)
library(ggpubr)
library(DT)
library("shinythemes")
source("helpers.R")
source("model_tab.R")
source("table_tab.R")
source("sidebar_config.R")
source("style.R")
library(shinyWidgets)
dash_header <- dashboardHeader(titleWidth='100%', 
                               title = span(tags$img(src = "sickstick_logo.png", style = "height: 100%; opacity: 1")
                               ))
dash_body <- dashboardBody(
  body_style,
  navbarPage(title = NULL,
    model_tab,
    table_tab,
    tabPanel(title="Empirical Plots", value="empirical_tab", box(img(src = "todo.jpeg"), width = 12))
  )
  
)

ui <- dashboardPage(skin = "black",
  dash_header, 
  dash_sidebar,
  dash_body
  )

server <- function(input, output) {
  
  dat_nm = eventReactive(input$run, {runModel(
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
  
  dat_ss = eventReactive(input$run, {runModel(
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
  #"With SickStick", textOutput('sd_with_ss'), icon("hospital"), color = "green",  size = "large"
  # output$value1 <- renderValueBox({
  #   valueBox(
  #     formatC(sales.account$value, format="d", big.mark=',')
  #     ,paste('Top Account:',sales.account$Account)
  #     ,icon = icon("stats",lib='glyphicon')
  #     ,color = "purple") 
  
  output$sd_with_ss <- renderValueBox({valueBox(subtitle='With SickStick',
                                                value=as.integer((sum(dat_ss()[,3]+dat_ss()[,8]))/input$N),
                                                icon=icon("coffee"),
                                                color = "teal")})
  
  output$sd_without_ss <- renderValueBox({valueBox(as.integer((sum(dat_nm()[,3]+dat_ss()[,8]))/input$N),
                                                   'Without SickStick',
                                                   icon=icon("coffee"),
                                                   color = "red")})
  
  output$ti_with_ss <- renderValueBox({valueBox(as.integer(sum(dat_ss()[,3])),
                                                'With SickStick',
                                                icon=icon("hospital"),
                                                color = "teal")})
  
  output$ti_without_ss <- renderValueBox({valueBox(as.integer(sum(dat_nm()[,3])),
                                                   'Without SickStick',
                                                   icon=icon("hospital"),
                                                   color = "red")})
  
  
  
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
  

  output$summary_table_nm <- renderDataTable(dat_nm(), options = list(dom = 't',paging = FALSE))
  output$summary_table_ss <- renderDataTable(dat_ss(), options = list(dom = 't',paging = FALSE))

}

shinyApp(ui = ui, server = server)


# Code for deployment
# library(rsconnect)
# rsconnect::deployApp('~/Documents/SickStick_QSEIR/SickStickModel/')
