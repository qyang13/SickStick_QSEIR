library("shiny")
library("ggplot2")
library("shinythemes")
library("shinyWidgets")
library("semantic.dashboard")
# # Load source R file that contains function definitions
source("helpers.R")

# Define UI ----
ui <- fluidPage(
  # Basic Page Configuration
  title = "SickStick Model Lite",
  theme = "bootstrap.css",
  titlePanel(img(src = "sickstick_logo.png", height = 100, width = 500)), br(), 
  setSliderColor(rep("midnightblue",3), seq(1,3)),
  chooseSliderSkin("Flat"),
  
  # Page Layout
  sidebarLayout(
    position = "left",
    # Side Panel for all the parameters
    sidebarPanel(
                  h3("SickStick Parameters"),
                  sliderInput("TP", "True Positive Rate (%):", 
                              min=0, max=100, value=95, step=1),
                  sliderInput("TN", "True Negative Rate (%):", 
                              min=0, max=100, value=95, step=1),
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
                                            min=0, max=100, value=10, step=1)),
                  br(), hr(),
                  radioButtons("OS", label = "Outbreak Scenario",
                               choices = list("Common Cold (Ro = 0.5)" = 1, "Influenza (Ro = 0.9)" = 2, 
                                              "Ebola (Ro = 2.0)" = 3, "2019-CoV (Ro = 3.5)" = 4, "Measles (Ro = 15.0)" = 5), 
                               selected = 1),
                  br(), hr(),
                  selectInput("AO", label = "Advanced options", choices = list("Show" = 1, "Hide" = 2), selected = 2),
                              conditionalPanel(
                                condition = "input.AO == 1",
                                sliderInput("beta", "Infection rate (%):", 
                                            min=0, max=200, value=100, step=1),
                                sliderInput("sigma", "Disease progression rate (%):", 
                                            min=1, max=200, value=100, step=1), 
                                sliderInput("gamma", "Recovery rate (%):", 
                                            min=1, max=200, value=40, step=1),
                                sliderInput("Rvr", "Infection Re-occurence rate (%):", 
                                            min=0, max=100, value=1, step=1))
    ),
    
    # Main panel displaying results
    mainPanel(
              width = 7,
              tabsetPanel(
                          position = "top",
                          tabPanel("Total Population Kinetics Plot",
                                   plotOutput("graph1", width = "auto"),
                                   hr(),
                                   tableOutput("datatable0")),
                          tabPanel("Data Summary without SickStick", tableOutput("datatable1")),
                          tabPanel("Data Summary with SickStick", tableOutput("datatable2"))))
    ),
    br(), br(), hr(),
  
    # Footer for author information
    tags$footer(
                class = "footer", 
                a("Created by: "),
                a("Qing Yang", 
                  href="mailto:qing.yang@colorado.edu"),
                a(" and "),
                a("Kate Bubar", 
                  href="")
    )
)


# Define server logic ----
server <- function(input, output) {
  # Reactive function that process the user input and carry out calculation
  dat_ss = reactive({processData(
                                input$TP/100, # TP of SickStick X/100
                                input$TN/100, # TN of SickStick X/100
                                input$beta/100, # Infection rate X/100
                                input$gamma/100, # Recovery rate X/100
                                input$sigma/100, # Disease progression rate X/100
                                input$DQ, # Number of days remained quarantined before clear X
                                input$Rvr/100, # Reverse rate R - S, can also be considered as disease reocurrence rate X/100
                                input$Stg,
                                input$Fq,
                                input$Fr/100,
                                input$OS
                              )})
  dat_nm = reactive({processData(
                                0, # TP of SickStick X/100
                                1, # TN of SickStick X/100
                                input$beta/100, # Infection rate X/100
                                input$gamma/100, # Recovery rate X/100
                                input$sigma/100, # Disease progression rate X/100
                                0, # Number of days remained quarantined before clear X
                                input$Rvr/100, # Reverse rate R - S, can also be considered as disease reocurrence rate X/100,
                                1,
                                0,
                                0,
                                input$OS
                              )})
  ro = reactive({rknot(
                        input$ST*30, # Discharge rate (# discharge per day)
                        input$beta, # Probability of disease transmission per contact
                        input$gamma, # Probability of recovery per capita
                        input$sigma # Probability of disease progression
                        #input$OS
                 )})
  
  # Table listing changes over time
  output$datatable0 <- renderTable({
                                    idx <- nrow(dat_nm())
                                    ro <- ro()
                                    SD_nm <- sum(dat_nm()[,6])
                                    SD_ss <- sum(dat_ss()[,6])
                                    df <- data.frame( label=c("Ro Value", 
                                                              "Annual Sick Days Per Person without SickStick",
                                                              "Annual Sick Days Per Person with SickStick"),
                                                      data=c(ro,SD_nm,SD_ss))
                                     
                                    }, bordered = T, rownames = F, colnames = F)
  # Data Summary without SickStick
  output$datatable1 <- renderTable(dat_nm(), bordered = T, rownames = T)
  # Data Summary with SickStick
  output$datatable2 <- renderTable(dat_ss(), bordered = T, rownames = T)
  
  # Graph 1
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
                          SickStick=rep(1, 5*nrow(dat_nm)))
    
    long <- rbind(long_nm, long_ss)
    
    labels <- c("0" = "Before SickStick", "1" = "After SickStick")
    
    p <- ggplot(long,
                aes(x=Period, y=Population, group=Indicator))    
    p <- p + geom_line(aes(colour = Indicator), size=1.5) + 
      ggtitle("") + theme_linedraw() +
      facet_grid(. ~ SickStick, labeller=labeller(SickStick = labels)) +
      theme(strip.text.x = element_text(size=16)) + labs(x = "Time (days)", y = "Fraction of the population")

    print(p)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)


# Code for deployment
# library(rsconnect)
# rsconnect::deployApp('Documents/SickStick_QSEIR/SickStickModel/')


