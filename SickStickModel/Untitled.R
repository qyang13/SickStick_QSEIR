library(ggplot2)
source("Documents/SickStick_QSEIR/SickStickModel/helpers.R")


# 365, # Total time (days)
# 500, # total number of people
# SickStick, # If using sickstick (TRUE or FALSE)
# TP, # TP of SickStick
# TN, # TN of SickStick
# R0, # r0 value for disease of interest
# gamma, # Recovery rate 
# sigma, # incubation rate
# r_Q, # Sympotom-based self quarantine rate
# r_RS # Reverse rate R - S, can also be considered as disease reocurrence rate

for (TP in c(65, 75, 85, 95)){
  for (Ro in seq(0, 30)) {
  
    sick_day_nm=runModel(
      365, # Total time (days)
      500, # total number of people
      F, # If using sickstick (TRUE or FALSE)
      TP, # TP of SickStick
      TN, # TN of SickStick
      R0, # r0 value for disease of interest
      gamma, # Recovery rate
      sigma, # incubation rate
      r_Q, # Sympotom-based self quarantine rate
      r_RS # Reverse rate R - S, can also be considered as disease reocurrence rate
      )
    
    sick_day_ss==runModel(
      365, # Total time (days)
      500, # total number of people
      T, # If using sickstick (TRUE or FALSE)
      TP, # TP of SickStick
      TN, # TN of SickStick
      R0, # r0 value for disease of interest
      gamma, # Recovery rate
      sigma, # incubation rate
      r_Q, # Sympotom-based self quarantine rate
      r_RS # Reverse rate R - S, can also be considered as disease reocurrence rate
    )
  }}
  

  



indicator <- c(rep("TP = 85%", 150 ), rep("TP = 75%", 150 ), rep("TP = 95%", 150 ))

long_95 <- data.frame(long_95[,1:2], Legend=indicator)


p <- ggplot(long_95,
            aes(x=ro, y=sn, group=indicator))    
p <- p  + 
  ggtitle("") +
  geom_line(aes(colour = indicator), size=2) +
  labs(x = "Ro Value", y = "Total Sick Days Saved (w/ - wo/) per Person per Year") +
  theme_linedraw() + theme(plot.background = element_rect(fill = "#ebebeb"))

print(p)

