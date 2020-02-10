library(ggplot2)
source("~/Documents/SickStick_QSEIR/SickStickModel_lite/helpers.R")

plot <- function(sickday_ro, xlab){
  num_sim = nrow(sickday_ro)/3
  indicator <- c(rep("TP = 75%", num_sim ), rep("TP = 85%", num_sim ), rep("TP = 95%", num_sim ))
  
  sickday_ro <- data.frame(sickday_ro, Legend=indicator)
  colnames(sickday_ro)=c(xlab, 'Saved', 'Legend')
  
  p <-  ggplot(sickday_ro,
               aes(x=sickday_ro[,1], y=Saved, group=Legend)) + 
    ggtitle("") +
    geom_line(aes(colour = Legend), size=2) +
    labs(x = xlab, y = "Total Sick Days Saved per Person each Year") +
    theme_linedraw() + theme(plot.background = element_rect(fill = "#ebebeb"))
  
  print(p)
}
########################################################

sickday_ro=data.frame()
for (TP in c(75, 85, 95)){
  # Currently Setting TN=TP
  TN = TP
  for (pct in (seq(1, 100)*0.01)) {
    for (beta in (seq(1,20)*0.05)){
    sick_day_nm=processData(
      0, # TP of SickStick X/100
      1, # TN of SickStick X/100
      beta, # Infection rate X/100
      0.1, # Recovery rate
      0.05, # Disease progression rate
      0, # Number of days remained quarantined before clear X
      0.2, # Reverse rate R - S, can also be considered as disease reocurrence rate X/100,
      3,
      0,
      pct)
    
    
    sick_day_ss=processData(
      TP/100, # TP of SickStick X/100
      TN/100, # TN of SickStick X/100
      beta, # Infection rate X/100
      0.1, # Recovery rate
      0.05, # Disease progression rate
      1, # Number of days remained quarantined before clear X
      0.2, # Reverse rate R - S, can also be considered as disease reocurrence rate X/100,
      3,
      0,
      pct )
    
    # mu <- 0.01
    # gamma <- 0.1
    # sigma <- 1
    ro=beta/0.05
    sickday_ro <- rbind(sickday_ro, c(ro, pct, (sick_day_nm-sick_day_ss)))
    

  }}}



Ro=plot(sickday_ro, "Percentage population use SickStick")



library(plotly)

num_sim = nrow(sickday_ro)/3
indicator <- c(rep("TP = 75%", num_sim ), rep("TP = 85%", num_sim ), rep("TP = 95%", num_sim ))
sickday_ro <- data.frame(sickday_ro, Legend=indicator)


colnames(sickday_ro) <- c("ro","percent", "DaysSaved",'tp')
sickday_ro$percent=sickday_ro$percent*100
p <- plot_ly(sickday_ro, x = ~ro, y = ~percent, z = ~DaysSaved, color = ~tp, colors = c('orange', 'lightcoral', 'midnightblue'), alpha=1, size = 1) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Ro Value'),
                      yaxis = list(title = 'Percentage People Using SickStick (%)'),
                      zaxis = list(title = 'Days Saved per person each year'))) 


p


