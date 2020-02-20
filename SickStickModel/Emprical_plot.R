library(ggplot2)
source("helpers.R")

########################################################
# Defualt parameter seting
T_max = 90
N = 100
SickStick = FALSE
TP = 75
FP = 75
R0 = 2.5 # r0 value for disease of interest
gamma = 1/14 # Recovery rate
sigma = 1/14 # incubation rate
r_Q = 0.05 # Sympotom-based self quarantine rate
r_RS = 0.05 # Reverse rate R - S, can also be considered as disease reocurrence rate
########################################################

# Changing Ro
########################################################
sickday_ro=data.frame()
for (TP in c(75, 85, 95)){
  # Currently Setting TN=TP
  TN = TP
  for (Ro in seq(0, 20)*0.15) {
    
    sd_nm=runMean(
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
    
    
    sd_ss=runMean(
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
    sd_saved_CI=sqrt((sum(sqrt(sd_nm[,11]^2+sd_nm[,16]^2))/T_max)^2 + (sum(sqrt(sd_ss[,11]^2+sd_ss[,16]^2))/T_max)^2)
    sickday_ro <- rbind(sickday_ro, c(Ro,sd_saved,sd_saved_CI))
    print(c('Current Ro: ',Ro, 'Current TP: ', TP))
  }}

ro_sickd = sickday_ro
num_sim = 21
indicator <- c(rep("TP = 75%", num_sim ), rep("TP = 85%", num_sim ), rep("TP = 95%", num_sim ))
ro_sickd <- data.frame(ro_sickd, Legend=indicator)

colnames(ro_sickd)=c('Ro', 'Saved', 'CI', 'Legend')

p_Ro <-  ggplot(ro_sickd,
            aes(x=Ro, y=Saved, group=Legend)) + 
            geom_ribbon(aes(ymin=(Saved-CI), ymax=(Saved+CI)), alpha=0.2, fill='#657b83')+
            ggtitle("") +
            geom_line(aes(colour = Legend), size=2) +
            labs(x = "Ro Value", y = "Total Sick Days Saved (w/ - wo/) per Person") +
            geom_hline(yintercept=0, color="grey", linetype="dashed")+
            theme_solarized_2(light=FALSE)+
            scale_colour_solarized('blue')

print(p_Ro)


# Changing gamma (recovery rate)
########################################################
sickday_ro=data.frame()
for (TP in c(75, 85, 95)){
  # Currently Setting TN=TP
  TN = TP
  for (rr in seq(3, 33)) {
    gamma=1/rr
    sd_nm=runMean(
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
    
    sd_ss=runMean(
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
    sd_saved_CI=sqrt((sum(sqrt(sd_nm[,11]^2+sd_nm[,16]^2))/T_max)^2 + (sum(sqrt(sd_ss[,11]^2+sd_ss[,16]^2))/T_max)^2)
    sickday_ro <- rbind(sickday_ro, c(rr,sd_saved,sd_saved_CI))
    print(c('Current RR: ',rr, 'Current TP: ', TP))
  }}

gamma_sickd = sickday_ro
num_sim = 31
indicator <- c(rep("TP = 75%", num_sim ), rep("TP = 85%", num_sim ), rep("TP = 95%", num_sim ))
gamma_sickd <- data.frame(gamma_sickd, Legend=indicator)

colnames(gamma_sickd)=c('Ro', 'Saved', 'CI', 'Legend')

p_gamma <-  ggplot(gamma_sickd,
                aes(x=Ro, y=Saved, group=Legend)) + 
  geom_ribbon(aes(ymin=(Saved-CI), ymax=(Saved+CI)), alpha=0.2, fill='#657b83')+
  ggtitle("") +
  geom_line(aes(colour = Legend), size=2) +
  labs(x = "Recovery Time (Days)", y = "Total Sick Days Saved (w/ - wo/) per Person") +
  geom_hline(yintercept=0, color="grey", linetype="dashed")+
  theme_solarized_2(light=FALSE)+
  scale_colour_solarized('blue')

print(p_gamma)

# Changing signma (incubation rate)
########################################################
sickday_ro=data.frame()
for (TP in c(75, 85, 95)){
  # Currently Setting TN=TP
  TN = TP
  for (rr in seq(3, 33)) {
    sigma=1/rr
    sd_nm=runMean(
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
    
    sd_ss=runMean(
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
    sd_saved_CI=sqrt((sum(sqrt(sd_nm[,11]^2+sd_nm[,16]^2))/T_max)^2 + (sum(sqrt(sd_ss[,11]^2+sd_ss[,16]^2))/T_max)^2)
    sickday_ro <- rbind(sickday_ro, c(rr,sd_saved,sd_saved_CI))
    print(c('Current RR: ',rr, 'Current TP: ', TP))
  }}

sigma_sickd = sickday_ro
num_sim = 31
indicator <- c(rep("TP = 75%", num_sim ), rep("TP = 85%", num_sim ), rep("TP = 95%", num_sim ))
sigma_sickd <- data.frame(sigma_sickd, Legend=indicator)

colnames(sigma_sickd)=c('Ro', 'Saved', 'CI', 'Legend')

p_sigma <-  ggplot(sigma_sickd,
                   aes(x=Ro, y=Saved, group=Legend)) + 
  geom_ribbon(aes(ymin=(Saved-CI), ymax=(Saved+CI)), alpha=0.2, fill='#657b83')+
  ggtitle("") +
  geom_line(aes(colour = Legend), size=2) +
  labs(x = "Incubation Time (Days)", y = "Total Sick Days Saved (w/ - wo/) per Person") +
  geom_hline(yintercept=0, color="grey", linetype="dashed")+
  theme_solarized_2(light=FALSE)+
  scale_colour_solarized('blue')

print(p_sigma)

# Changing self-reporting rate
########################################################
sickday_ro=data.frame()
for (TP in c(75, 85, 95)){
  # Currently Setting TN=TP
  TN = TP
  for (r_Q in seq(1, 30)*0.02) {
    sd_nm=runMean(
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
    
    sd_ss=runMean(
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
    sd_saved_CI=sqrt((sum(sqrt(sd_nm[,11]^2+sd_nm[,16]^2))/T_max)^2 + (sum(sqrt(sd_ss[,11]^2+sd_ss[,16]^2))/T_max)^2)
    sickday_ro <- rbind(sickday_ro, c(r_Q,sd_saved,sd_saved_CI))
    print(c('Current r_Q: ',r_Q, 'Current TP: ', TP))
  }}

rQ_sickd = sickday_ro
num_sim = 30
indicator <- c(rep("TP = 75%", num_sim ), rep("TP = 85%", num_sim ), rep("TP = 95%", num_sim ))
rQ_sickd <- data.frame(rQ_sickd, Legend=indicator)

colnames(rQ_sickd)=c('Ro', 'Saved', 'CI', 'Legend')

p_rQ <-  ggplot(rQ_sickd,
                   aes(x=Ro*100, y=Saved, group=Legend)) + 
  geom_ribbon(aes(ymin=(Saved-CI), ymax=(Saved+CI)), alpha=0.2, fill='#657b83')+
  ggtitle("") +
  geom_line(aes(colour = Legend), size=2) +
  labs(x = "Self Reporting Rate (%)", y = "Total Sick Days Saved (w/ - wo/) per Person") +
  geom_hline(yintercept=0, color="grey", linetype="dashed")+
  theme_solarized_2(light=FALSE)+
  scale_colour_solarized('blue')

print(p_rQ)

# Changing N 
########################################################
sickday_ro=data.frame()
for (TP in c(75, 85, 95)){
  # Currently Setting TN=TP
  TN = TP
  for (N in seq(1, 10)*200) {
    sd_nm=runMean(
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
    
    sd_ss=runMean(
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
    sd_saved_CI=sqrt((sum(sqrt(sd_nm[,11]^2+sd_nm[,16]^2))/T_max)^2 + (sum(sqrt(sd_ss[,11]^2+sd_ss[,16]^2))/T_max)^2)
    sickday_ro <- rbind(sickday_ro, c(N,sd_saved,sd_saved_CI))
    print(c('Current N: ',N, 'Current TP: ', TP))
  }}

N_sickd = sickday_ro
num_sim = 30
indicator <- c(rep("TP = 75%", num_sim ), rep("TP = 85%", num_sim ), rep("TP = 95%", num_sim ))
N_sickd <- data.frame(N_sickd, Legend=indicator)

colnames(N_sickd)=c('Ro', 'Saved', 'CI', 'Legend')

p_N <-  ggplot(N_sickd,
                aes(x=Ro, y=Saved, group=Legend)) + 
  geom_ribbon(aes(ymin=(Saved-CI), ymax=(Saved+CI)), alpha=0.2, fill='#657b83')+
  ggtitle("") +
  geom_line(aes(colour = Legend), size=2) +
  labs(x = "Incubation Time", y = "Total Sick Days Saved (w/ - wo/) per Person") +
  geom_hline(yintercept=0, color="grey", linetype="dashed")+
  theme_solarized_2(light=FALSE)+
  scale_colour_solarized('blue')

print(p_N)

save(list=c("ro_sickd", "gamma_sickd", "sigma_sickd", "rQ_sickd", "N_sickd"), file="emp_res.Rdata")
