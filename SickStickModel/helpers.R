library(simcausal)

# Define, unit time as days
runModel <- function(
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
  r_RS, # Reverse rate R - S, can also be considered as disease reocurrence rate
  SS_Stg = 0 # implementation strategy - how many days in between sick Stick use i.e. 0 = use everyday
  
  # KMB 1/31: ignoring this params for now
  # T_Q = 1, # Number of days remained quarantined before clear
  # Fq = 1, # Every # of days 
  # Fr = 1, # Percentage of population (%) to use
  # ST = 10 # Serve time (in months)
){
  
  #### Initialization ####
  # Initialize variables
  TP <- TP/100
  TN <- TN/100
  
  FN <- 1 - TP
  FP <- 1 - TN
  
  T_contagious <- 1/gamma
  T_incubate <- 1/sigma
  
  num_contagious_asym <- 1 # number of days you are contagious before showing symptoms (contagious in compartment E)
  T_in_I <- T_contagious - num_contagious_asym
  
  beta = R0*gamma
  
  t = 1
  
  S  <- 1
  E  <- 2
  I  <- 3
  R  <- 4
  QS <- 5
  QE <- 6
  QI <- 7
  
  current_pop <- matrix(0L, nrow = 2, ncol = N)
  rownames(current_pop) <- c("state", "num days in state")
  
  total_pop <- matrix(0L, nrow = T_max, ncol = 7)
  colnames(total_pop) <- c("S", "E", "I", "R", "QS", "QE", "QI")
  
  # Set initial population in current_pop and total_pop
  # KMB: 2% in I0 and 2% in E0, may need to change later
  E0 <- round(2*N/100)
  I0 <- round(2*N/100)
  
  current_pop[1, 1:E0] <- E
  current_pop[1, (E0+1):(E0+I0)] <- I
  current_pop[1, -(1:(E0+I0))] <- S
  
  count_states <- table(current_pop[1,])
  total_pop[t, S] <- count_states["1"]
  total_pop[t, E] <- count_states["2"]
  total_pop[t, I] <- count_states["3"]
  
  #### KMB: Implement "Serve time" later ####
  #if (ST==0) {mu <- 0} # else {mu <- 1/ST}
  #else {mu <- 0}
  
  # KMB: Implement "Strategy" later
  #if (Stg == 1) {
  #  Fq <- 0
  #  Fr <- 1
  #}
  
  #### Simulation loop ####
  for (t in 2:T_max) {
    
    # SickStick Phase: ####
    # first compute individuals who move with bernoulli distribution, then update compartments accordingly
    num_to_QI = 0
    if (SickStick == TRUE) {
      
      for (i in 1:N){
        if (t %% (SS_Stg + 1) == 0) {
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
    
    # Count number of people that will be infected this day
    transmission_rate <- rnorm(1, beta, 0.05)
    num_E_contag <- 0
    
    # Count number of people in E that are contagious
    for (i in 1:N){
      if (current_pop[1,i] == E && current_pop[2,i] > (T_incubate - num_contagious_asym)) {
        num_E_contag <- num_E_contag + 1
      }
    }
                            
    if (transmission_rate > 0 && transmission_rate < 1) {
      num_to_infect <- rbinom(1, total_pop[t-1, I] - num_to_QI + num_E_contag, transmission_rate*(total_pop[t-1,S])/N)
    }
    else if (transmission_rate > 1) {
      num_to_infect <- 0
      while (transmission_rate > 1) {
        num_to_infect <- num_to_infect + rbinom(1, total_pop[t-1, I] - num_to_QI, 1*(total_pop[t-1,S])/N)
        transmission_rate <- transmission_rate - 1
      }
      num_to_infect <- num_to_infect + rbinom(1, total_pop[t-1, I] - num_to_QI, transmission_rate*(total_pop[t-1,S])/N)
    }
    else {num_to_infect <- 0}
    
    for (i in 1:N){
      if (current_pop[1,i] == S){
        # rate <- 1 - (1 - beta)^(total_pop[t-1, I]/N - num_to_QI) # I needs to be current_pop - sickstick quarantine 
        # move <- rbern(1, rate) 
        # if (move == 1){
        #   current_pop[1,i] = E
        #   current_pop[2,i] = 1
        # }
        if (num_to_infect > 0){
          current_pop[1,i] <- E
          current_pop[2,i] <- 1
          num_to_infect <- num_to_infect - 1
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
          prob_success <- pnorm(current_pop[2,i], T_contagious - num_contagious_asym, 1) #TODO: update sd based on certain disease
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
        prob_success <- pnorm(current_pop[2,i], T_contagious, 1) #TODO: update sd based on certain disease
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
    
    # KMB: 2/24. For now, don't have people move from R -> S
    # # move people from R back to S at the end of the timestep
    # for (i in 1:N) {
    #   if (current_pop[1,i] == R){
    #     current_pop[1,i] = S
    #     current_pop[2,i] = 0
    #   }
    # } 
  }
  
  Q_tot <- total_pop[, "QS"] + total_pop[, "QE"] + total_pop[, "QI"]
  
  total_pop.df <- as.data.frame(total_pop)
  total_pop.df$Q_tot <- Q_tot
  colnames(total_pop.df) <- c("Uninfected", "Exposed", "Infected", "Recovered","QS","QE","QI", "Total Quarantined")
  return(total_pop.df)
}

runMean <- function(
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
) {
  
  num_iterations <- 20
  
  pop_over_time <- array(0L, dim = c(T_max, 8, num_iterations))
  
  # Run simulation # of times in order to compute mean and CI of model outputs
  for (i in 1:num_iterations) {
    temp <- runModel(T_max,N,SickStick, TP, TN, R0, gamma, sigma, r_Q, r_RS)
    temp <- as.matrix(temp)
    pop_over_time[,,i] <- temp
  }
  
  means <- apply(pop_over_time, c(1,2), mean)
  colnames(means) <- c("S", "E", "I", "R", "QS", "QE", "QI", "Qtot")
  
  t_dist <- qt(c(.025, .975), df=(num_iterations - 1)) # t-distribution for alpha = 0.05, n-1 d.f.
  CI_fun <- function(x) t_dist[2]*sd(x)/sqrt(length(x))
  
  CI <- apply(pop_over_time, c(1,2), CI_fun)
  colnames(CI) <- c("S_CI", "E_CI", "I_CI", "R_CI", "QS_CI", "QE_CI", "QI_CI", "Qtot_CI")
  
  # Store mean and CI for each compartment overtime
  means.df <- as.data.frame(list(means, CI))
  means.df$time <- 1:T_max
  
  return(means.df)
}
