library(ggplot2)
source("~/Documents/SickStick_QSEIR/SickStickModel_lite/helpers.R")

sick_day_nm <- 0
snmv <- NULL
rov <- NULL
for (TP in c(75, 85, 95)){
  # Currently Setting TN=TP
  TN = TP
for (beta in (seq(1, 150)*0.01)) {
  
  sick_day_nm=processData(
  0, # TP of SickStick X/100
  1, # TN of SickStick X/100
  beta, # Infection rate X/100
  0.01, # Recovery rate X/100
  1, # Disease progression rate X/100
  0, # Number of days remained quarantined before clear X
  0.01, # Reverse rate R - S, can also be considered as disease reocurrence rate X/100,
  1,
  0,
  0 )
  
  sick_day_ss=processData(
    TP, # TP of SickStick X/100
    TN, # TN of SickStick X/100
    beta, # Infection rate X/100
    0.01, # Recovery rate X/100
    1, # Disease progression rate X/100
    1, # Number of days remained quarantined before clear X
    0.01, # Reverse rate R - S, can also be considered as disease reocurrence rate X/100,
    1,
    0,
    0 )
  
  mu <- 0.01
  gamma <- 0.1
  sigma <- 1
  
  ro = beta*sigma/((gamma+mu)*(sigma+mu))
  rov <- c(rov, ro)
  snmv <- c(snmv, sick_day_ss-sick_day_nm)
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

