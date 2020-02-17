library(ggplot2)
source("helpers.R")

########################################################
# Defualt parameter seting
T_max = 365
N = 100
R0 = 2.5 # r0 value for disease of interest
gamma = 1/25 # Recovery rate
sigma = 1/15 # incubation rate
r_Q = 0.5 # Sympotom-based self quarantine rate
r_RS = 0.5 # Reverse rate R - S, can also be considered as disease reocurrence rate
########################################################

sickday_ro=data.frame()
for (TP in c(75, 85, 95)){
  # Currently Setting TN=TP
  TN = TP
  for (Ro in seq(0, 20)) {
    
    
    sd_nm=runModel(
      T_max, # Total time (days)
      N, # total number of people
      FALSE, # If using sickstick (TRUE or FALSE)
      TP, # TP of SickStick
      TN, # TN of SickStick
      Ro, # r0 value for disease of interest
      gamma, # Recovery rate
      sigma, # incubation rate
      r_Q, # Sympotom-based self quarantine rate
      r_RS # Reverse rate R - S, can also be considered as disease reocurrence rate
    )
    
    
    sd_ss=runModel(
      T_max, # Total time (days)
      N, # total number of people
      TRUE, # If using sickstick (TRUE or FALSE)
      TP, # TP of SickStick
      TN, # TN of SickStick
      Ro, # r0 value for disease of interest
      gamma, # Recovery rate
      sigma, # incubation rate
      r_Q, # Sympotom-based self quarantine rate
      r_RS # Reverse rate R - S, can also be considered as disease reocurrence rate
    )
    
    sd_saved=((sum(sd_nm[,3]+sd_nm[,8])) - (sum(sd_ss[,3]+sd_ss[,8])))/N
    sickday_ro <- rbind(sickday_ro, c(Ro,sd_saved))
    print(c('Current Ro: ',Ro, 'Current TP: ', TP))
  }}


data = sickday_ro[,1:2]

num_sim = 21
indicator <- c(rep("TP = 75%", num_sim ), rep("TP = 85%", num_sim ), rep("TP = 95%", num_sim ))
sickday_ro <- data.frame(sickday_ro, Legend=indicator)
colnames(sickday_ro)=c('Ro', 'Saved', 'Legend')

p <-  ggplot(sickday_ro,
            aes(x=Ro, y=Saved, group=Legend)) + 
      ggtitle("") +
      geom_line(aes(colour = Legend), size=2) +
      labs(x = "Ro Value", y = "Total Sick Days Saved (w/ - wo/) per Person per Year") +
      theme_linedraw() + theme(plot.background = element_rect(fill = "#ebebeb"))

print(p)



