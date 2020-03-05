library(ggplot2)
library(ggthemes)
library(shinydashboard)
library(flexdashboard)

source("helpers.R")
load(file="emp_res.Rdata")

###############################################################################
# Function to plot empirical plots
plotEmp <- function(df, xlab, cur_settings){
  p <- ggplot(df,
              aes(x=Ro, y=Saved, group=Legend)) + 
    geom_ribbon(aes(ymin=(Saved-CI), ymax=(Saved+CI)), alpha=0.2, fill='#657b83')+
    ggtitle("") +
    geom_line(aes(colour = Legend), size=2) +
    labs(x = xlab, y = "Total Sick Days Saved per Person") +
    geom_hline(yintercept=0, color="grey", linetype="dashed")+
    theme_solarized_2(light=FALSE)+
    scale_colour_solarized('blue')  +
    theme(strip.text.x = element_text(size=20), axis.text=element_text(size=20),
          axis.title=element_text(size=20,face="bold"), legend.text=element_text(size=15), plot.subtitle = element_text(size=15), title=element_text(size=20)) + 
    labs(title = paste("Effect of", xlab, "on SickStick efficacy"),
         subtitle = cur_settings)
  return(p)
}

###############################################################################
# Begin Server Definition
server <- function(input, output) {
  
  #############################################################################
  # Reactive population data update, only when RUN button is clicked
  env = eventReactive(input$run, {
    if (input$OS == 1) {R0 = 0.5; gamma = 1/8; sigma = 1/2; r_Q = 10/100; r_RS = 100/100}
    else if (input$OS == 2) {R0 = 0.9; gamma = 1/10; sigma = 1/10; r_Q = 10/100; r_RS =100/100}
    else if (input$OS == 3) {R0 = 2; gamma = 1/20; sigma = 1/7; r_Q = 10/100; r_RS = 100/100}
    else if (input$OS == 4) {R0 = 3.5; gamma = 1/20; sigma = 1/14; r_Q = 10/100; r_RS = 100/100}
    else if (input$OS == 5) {R0 = 9; gamma = 1/10; sigma = 1/10; r_Q = 10/100; r_RS = 100/100}
    else {R0 = input$R0; gamma = 1/input$gamma; sigma = 1/input$sigma; r_Q = input$r_Q/100; r_RS = input$r_RS/100}
    return(c(R0, gamma, sigma, r_Q, r_RS))
  })
  
  dat_nm = eventReactive(input$run, {
    if (input$OS == 1) {R0 = 0.5; gamma = 1/8; sigma = 1/2; r_Q = 10/100; r_RS = 100/100}
    else if (input$OS == 2) {R0 = 0.9; gamma = 1/10; sigma = 1/10; r_Q = 10/100; r_RS =100/100}
    else if (input$OS == 3) {R0 = 2; gamma = 1/20; sigma = 1/7; r_Q = 10/100; r_RS = 100/100}
    else if (input$OS == 4) {R0 = 3.5; gamma = 1/20; sigma = 1/14; r_Q = 10/100; r_RS = 100/100}
    else if (input$OS == 5) {R0 = 9; gamma = 1/10; sigma = 1/10; r_Q = 10/100; r_RS = 100/100}
    else {R0 = input$R0; gamma = 1/input$gamma; sigma = 1/input$sigma; r_Q = input$r_Q/100; r_RS = input$r_RS/100}
    runMean(
      input$T_max*30, input$N, FALSE,
      input$TP, input$TN, 
      R0, gamma, sigma, r_Q, r_RS)
  })
  dat_ss = eventReactive(input$run, {
    if (input$OS == 1) {R0 = 0.5; gamma = 1/8; sigma = 1/2; r_Q = 10/100; r_RS = 100/100}
    else if (input$OS == 2) {R0 = 0.9; gamma = 1/10; sigma = 1/10; r_Q = 10/100; r_RS =100/100}
    else if (input$OS == 3) {R0 = 2; gamma = 1/20; sigma = 1/7; r_Q = 10/100; r_RS = 100/100}
    else if (input$OS == 4) {R0 = 3.5; gamma = 1/20; sigma = 1/14; r_Q = 10/100; r_RS = 100/100}
    else if (input$OS == 5) {R0 = 9; gamma = 1/10; sigma = 1/10; r_Q = 10/100; r_RS = 100/100}
    else {R0 = input$R0; gamma = 1/input$gamma; sigma = 1/input$sigma; r_Q = input$r_Q/100; r_RS = input$r_RS/100}    
    runMean(
      input$T_max*30, input$N, TRUE,
      input$TP, input$TN, 
      R0, gamma, sigma, r_Q, r_RS)
  })
  
  #############################################################################
  # value box showing current parameters
  output$Ro <- renderGauge({gauge(label='Infection Ro',
                                  value=env()[1],
                                  min=0, max=10, gaugeSectors(danger=c(0,10),colors = c('#D9524F')))})
  
  output$Sigma <- renderGauge({gauge(label='Incubation Time',
                                     symbol=' Days',
                                     value=as.integer(1/env()[3]),
                                     min=0, max=20, gaugeSectors(danger=c(0,20),colors = c('#D9524F')))})
  
  output$Gamma <- renderGauge({gauge(label='Recovery Time',
                                     symbol=' Days',
                                     value=as.integer(1/env()[2]),
                                     min=0, max=20, gaugeSectors(danger=c(0,20),colors = c('#D9524F')))})
  
  #############################################################################
  # Update the display numbers in the value boxes
  output$workdays_saved <- shinydashboard::renderValueBox({shinydashboard::valueBox(subtitle='Work days saved',
                                                                                value=as.integer(sum(dat_nm()[,8]) - sum(dat_ss()[,8])), # /input$N),
                                                                                icon=icon("hospital"),
                                                                                color = "teal")})
  
  output$qd_with_ss <- shinydashboard::renderValueBox({shinydashboard::valueBox(subtitle='Quarantine days',
                                                                                value=as.integer(sum(dat_ss()[,8])), # /input$N),
                                                                                icon=icon("hospital"),
                                                                                color = "teal")})
  
  output$qd_without_ss <- shinydashboard::renderValueBox({shinydashboard::valueBox(value=as.integer(sum(dat_nm()[,8])), #/input$N),
                                                                                   subtitle='Quarantine days',
                                                                                   icon=icon("hospital"),
                                                                                   color = "red")})
  
  output$sd_with_ss <- shinydashboard::renderValueBox({shinydashboard::valueBox(subtitle='Days sick at work',
                                                                                value=as.integer(sum(dat_ss()[,3])), #/input$N),
                                                                                icon=icon("bug"),
                                                                                color = "aqua")})
  
  output$sd_without_ss <- shinydashboard::renderValueBox({shinydashboard::valueBox(value=as.integer((sum(dat_nm()[,3]))), #/input$N),
                                                                                   subtitle='Days sick at work',
                                                                                   icon=icon("bug"),
                                                                                   color = "orange")})
  
  #############################################################################
  # Graph 1&2: Detailed Population graph
  output$graph1 <- renderPlot({
    dat_nm <- dat_nm()
    
    long_nm <- data.frame(
      Period = rep(1:nrow(dat_nm),5), 
      Population = c(dat_nm[,1], dat_nm[,2], dat_nm[,3], dat_nm[,4], dat_nm[,8]), 
      PopCI = c(dat_nm[,9], dat_nm[,10], dat_nm[,11], dat_nm[,12], dat_nm[,16]), 
      Indicator=rep(c("Uninfected", "Exposed", "Infected", "Recovered", "Quarantined"), 
                    each=nrow(dat_nm)))
      
    p <- ggplot(long_nm,
                aes(x=Period/30, y=Population/input$N, group=Indicator)) + 
      geom_line(aes(colour = Indicator), size=2) + 
      ggtitle("") +
      labs(x = "Time (Months)", y = "Fraction of the population") +
      theme_solarized_2(light=FALSE)+
      scale_colour_solarized('blue')+
      geom_ribbon(aes(ymin=(Population-PopCI)/input$N,ymax=(Population+PopCI)/input$N), alpha=0.3, fill='#657b83') +
      theme(strip.text.x = element_text(size=20), axis.text=element_text(size=20),
            axis.title=element_text(size=15,face="bold"), legend.text=element_text(size=15)) + 
      scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
    print(p)
  })  
  
  
  output$graph2 <- renderPlot({
    dat_ss <- dat_ss()
    
    long_ss <- data.frame(
      Period = rep(1:nrow(dat_ss),5), 
      Population = c(dat_ss[,1], dat_ss[,2], dat_ss[,3], dat_ss[,4], dat_ss[,8]), 
      PopCI = c(dat_ss[,9], dat_ss[,10], dat_ss[,11], dat_ss[,12], dat_ss[,16]), 
      Indicator=rep(c("Uninfected", "Exposed", "Infected", "Recovered", "Quarantined"), 
                    each=nrow(dat_ss)))

    p <- ggplot(long_ss,
                aes(x=Period/30, y=Population/input$N, group=Indicator)) + 
      geom_line(aes(colour = Indicator), size=2) + 
      ggtitle("") +
      labs(x = "Time (Months)", y = "Fraction of the population") +
      theme_solarized_2(light=FALSE)+
      scale_colour_solarized('blue')+
      geom_ribbon(aes(ymin=(Population-PopCI)/input$N,ymax=(Population+PopCI)/input$N), alpha=0.3, fill='#657b83') +
      theme(strip.text.x = element_text(size=20), axis.text=element_text(size=20),
            axis.title=element_text(size=15,face="bold"), legend.text=element_text(size=15)) + 
      scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
    print(p)
  })  
  
  #############################################################################
  # Graph 3&4: Less detailed Population graph
  output$graph3 <- renderPlot({
    dat_nm <- dat_nm()
    
    long_nm <- data.frame(
      Period = rep(1:nrow(dat_nm),2), 
      Population = c((dat_nm[,1] +dat_nm[,2]+dat_nm[,3]+dat_nm[,4]), (dat_nm[,8])), 
      PopCI = c(rowMeans(cbind(dat_nm[,9], dat_nm[,10],  dat_nm[,11],dat_nm[,12])), rowMeans(cbind( dat_nm[,16]))), 
      
      Indicator=rep(c("In Training", "Quarantined"), 
                    each=nrow(dat_nm)))
      p <- ggplot(long_nm,
                aes(x=Period/30, y=Population/input$N, group=Indicator)) + 
      geom_line(aes(colour = Indicator), size=2) + 
      ggtitle("") +
      labs(x = "Time (Months)", y = "Fraction of the population") +
      theme_solarized_2(light=FALSE)+
      scale_colour_solarized('blue')+
      geom_ribbon(aes(ymin=(Population-PopCI)/input$N,ymax=(Population+PopCI)/input$N), alpha=0.3, fill='#657b83') +
      theme(strip.text.x = element_text(size=20), axis.text=element_text(size=20),
            axis.title=element_text(size=15,face="bold"), legend.text=element_text(size=15)) + 
      scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
    print(p)
  })
  
  output$graph4 <- renderPlot({
    dat_ss <- dat_ss()
    long_ss <- data.frame(
      Period = rep(1:nrow(dat_ss),2), 
      Population = c((dat_ss[,1] +dat_ss[,2]+dat_ss[,3]+dat_ss[,4]), (dat_ss[,8])), 
      PopCI = c(rowMeans(cbind(dat_ss[,9], dat_ss[,10],  dat_ss[,11],dat_ss[,12])), rowMeans(cbind( dat_ss[,16]))), 
      Indicator=rep(c("In Training", "Quarantined"), 
                    each=nrow(dat_ss)))
    p <- ggplot(long_ss,
                aes(x=Period/30, y=Population/input$N, group=Indicator)) + 
      geom_line(aes(colour = Indicator), size=2) + 
      ggtitle("") +
      labs(x = "Time (Months)", y = "Fraction of the population") +
      theme_solarized_2(light=FALSE)+
      scale_colour_solarized('blue')+
      geom_ribbon(aes(ymin=(Population-PopCI)/input$N,ymax=(Population+PopCI)/input$N), alpha=0.3, fill='#657b83') +
      theme(strip.text.x = element_text(size=20), axis.text=element_text(size=20),
            axis.title=element_text(size=15,face="bold"), legend.text=element_text(size=15)) + 
      scale_x_continuous(breaks = function(x) unique(floor(pretty(seq(0, (max(x) + 1) * 1.1)))))
    print(p)
  })
  #############################################################################
  # Summary data tables for data tab
  output$summary_table_nm <- renderDataTable(dat_nm(), options = list(dom = 't',paging = FALSE))
  output$summary_table_ss <- renderDataTable(dat_ss(), options = list(dom = 't',paging = FALSE))
  
  #############################################################################
  # Empirical plots, calls for the plotEmp function
  selected <- reactive({input$select})
  output$graph_emp <- renderPlot({
    if(selected()==1){plotEmp(ro_sickd, "Ro Value", "N = 100; Incubation Time = 14 days; Recovery Time = 14 days; Self-quarantine rate: 5%")}
    else if(selected()==2){plotEmp(N_sickd, "Total Population", "Ro = 2.5; Incubation Time = 14 days; Recovery Time = 14 days; Self-quarantine rate: 5%")}
    else if(selected()==3){plotEmp(gamma_sickd, "Recovery Time (Days)", "N = 100; Ro = 2.5, Incubation Time = 14 days; Self-quarantine rate: 5%")}
    else if(selected()==4){plotEmp(sigma_sickd, "Incubation Time (Days)", "N = 100; Ro = 2.5, Recovery Time = 14 days; Self-quarantine rate: 5%")}
    else if(selected()==5){plotEmp(rQ_sickd, "Self-quarantine Rate", "N = 100; Ro = 2.5, Incubation Time = 14 days;  Recovery Time = 14 days")}
  })
}