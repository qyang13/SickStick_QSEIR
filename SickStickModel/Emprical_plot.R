library(ggplot2)
source("helpers.R")
library(ggthemes)



# Define, unit time as days
getTmax <- function(
  T_max, # Total time (days)
  N, # total number of people
  SickStick, # If using sickstick (TRUE or FALSE)
  
  TP, # TP of SickStick
  TN, # TN of SickStick
  
  R0, # r0 value for disease of interest
  # beta, # Transmission rate (contact rate * probability of transmission given contact)
  gamma, # Recovery rate 
  sigma, # incubation rate
  r_Q, # Sympotom-based self quarantine rate
  r_RS # Reverse rate R - S, can also be considered as disease reocurrence rate
){
  
  #### Initialization ####
  # Initialize variables
  TP <- TP/100
  TN <- TN/100
  FN <- 1 - TP
  FP <- 1 - TN
  T_recover <- 1/gamma
  T_incubate <- 1/sigma
  beta = R0*gamma
  t = 1
  S <- 1
  E <- 2
  I <- 3
  R <- 4
  QS <- 5
  QE <- 6
  QI <- 7
  
  current_pop <- matrix(0L, nrow = 2, ncol = N)
  rownames(current_pop) <- c("state", "num days in state")
  
  total_pop <- matrix(0L, nrow = T_max, ncol = 7)
  colnames(total_pop) <- c("S", "E", "I", "R", "QS", "QE", "QI")
  
  # Set initial population in current_pop and total_pop
  # KMB: 1% in I0 and 1% in E0, may need to change later
  E0 <- round(N/100)
  I0 <- round(N/100)
  
  current_pop[1, 1:E0] <- E
  current_pop[1, (E0+1):(E0+I0)] <- I
  current_pop[1, -(1:(E0+I0))] <- S
  
  count_states <- table(current_pop[1,])
  total_pop[t, S] <- count_states["1"]
  total_pop[t, E] <- count_states["2"]
  total_pop[t, I] <- count_states["3"]
  
  #### Simulation loop ####
  for (t in 2:T_max) {
    
    # SickStick Phase: ####
    # first compute individuals who move with bernoulli distribution, then update compartments accordingly
    num_to_QI = 0
    if (SickStick == TRUE) {
      
      for (i in 1:N){
        if (current_pop[1,i] == S) {
          move <- rbern(1, FP)
          if (move == 1) current_pop[1,i] = QS
        }
        else if (current_pop[1,i] == E) {
          move <- rbern(1, TP)
          if (move == 1) current_pop[1,i] = QE
        }
        else if (current_pop[1,i] == I) {
          move <- rbern(1, TP)
          if (move == 1) {
            current_pop[1,i] = QI
            num_to_QI = num_to_QI + 1
          } 
        }
        else if (current_pop[1,i] == QS) {
          move <- rbern(1, TP)
          if (move == 1) current_pop[1,i] = S
        }
        else if (current_pop[1,i] == QE) {
          move <- rbern(1, FP)
          if (move == 1) current_pop[1,i] = E
        }
      }
    }
    
    # Normal Disease Dynamics ###
    # first compute if individual moves compartments, then move and update num days in compartment
    # then compute overall number of people in each compartment
    
    for (i in 1:N){
      if (current_pop[1,i] == S){
        rate <- 1 - (1 - beta)^(total_pop[t-1, I] - num_to_QI) # I needs to be current_pop - sickstick quarantine 
        move <- rbern(1, rate) 
        if (move == 1){
          current_pop[1,i] = E
          current_pop[2,i] = 1
        }
      }
      
      else if (current_pop[1,i] == E){
        prob_success <- pnorm(current_pop[2,i], T_incubate, 1) #TODO: update sd based on certain disease
        move <- rbern(1, prob_success)
        if (move == 1){
          current_pop[1,i] = I
          current_pop[2,i] = 1
        } else current_pop[2,i] = current_pop[2,i] + 1
      }
      
      else if (current_pop[1,i] == I){ 
        # first self-quarantine. Then recovery rate. 
        move <- rbern(1, r_Q)
        if (move == 1){
          current_pop[1,i] = QI
          current_pop[2,i] = current_pop[2,i] + 1
        } else {
          prob_success <- pnorm(current_pop[2,i], T_recover, 1) #TODO: update sd based on certain disease
          move <- rbern(1, prob_success)
          if (move == 1){
            current_pop[1,i] = R
            current_pop[2,i] = 1
          } else current_pop[2,i] = current_pop[2,i] + 1
        }
      }
      
      else if (current_pop[1,i] == QE){
        prob_success <- pnorm(current_pop[2,i], T_incubate, 1) #TODO: update sd based on certain disease
        move <- rbern(1, prob_success)
        if (move == 1){
          current_pop[1,i] = QI
          current_pop[2,i] = 1
        } else current_pop[2,i] = current_pop[2,i] + 1
      }
      
      else if (current_pop[1,i] == QI){
        prob_success <- pnorm(current_pop[2,i], T_recover, 1) #TODO: update sd based on certain disease
        move <- rbern(1, prob_success)
        if (move == 1){
          current_pop[1,i] = R
          current_pop[2,i] = 1
        } else current_pop[2,i] = current_pop[2,i] + 1
      }
    }
    
    count_states <- table(current_pop[1,])
    total_pop[t, S] <- count_states["1"]
    total_pop[t, E] <- count_states["2"]
    total_pop[t, I] <- count_states["3"]
    total_pop[t, R] <- count_states["4"]
    total_pop[t, QS] <- count_states["5"]
    total_pop[t, QE] <- count_states["6"]
    total_pop[t, QI] <- count_states["7"]
    
    total_pop[t, is.na(total_pop[t,])] <- 0
    if (total_pop[t, R]==N){
      tmax=t
      break
    }
    
  }
  return(tmax)
}

########################################################
# Defualt parameter seting
T_max = 60
N = 100
SickStick = FALSE
TP = 75
FP = 75
R0 = 2.5 # r0 value for disease of interest
gamma = 1/10 # Recovery rate
sigma = 1/10 # incubation rate
r_Q = 0.05 # Sympotom-based self quarantine rate
r_RS = 0.05 # Reverse rate R - S, can also be considered as disease reocurrence rate

########################################################

# Changing Ro
########################################################
sickday_ro=data.frame()
for (TP in c(75, 85, 95)){
  # Currently Setting TN=TP
  TN = TP
  for (Ro in seq(0, 20)*0.3+0.1) {
    # tmax=getTmax(
    #   T_max, # Total time (days)
    #   N, # total number of people
    #   FALSE, # If using sickstick (TRUE or FALSE)
    #   TP, # TP of SickStick
    #   TN, # TN of SickStick
    #   Ro, # r0 value for disease of interest
    #   gamma, # Recovery rate
    #   sigma, # incubation rate
    #   r_Q, # Sympotom-based self quarantine rate
    #   r_RS # Reverse rate R - S, can also be considered as disease reocurrence rate
    # )
    
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
    sd_saved=((sum(sd_nm[,8])) - (sum(sd_ss[,8])))/N
    sd_saved_CI=sqrt((sum(sd_nm[,16])/T_max)^2 + (sum(sd_ss[,16])/T_max)^2)
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
            geom_hline(yintercept=0, color="grey", linetype="dashed") +
            theme_solarized_2(light=FALSE) +
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
