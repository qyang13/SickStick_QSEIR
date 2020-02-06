# Define, unit time as days
processData <- function(
                        T_max, # Total time (days)
                        I0, # Initial infected population percentage
                        N, # total number of people
                        
                        TP, # TP of SickStick
                        TN, # TN of SickStick
                        
                        R0, # r0 value for disease of interest
                        beta, # Transmission rate (contact rate * probability of transmission given contact)
                        gamma, # Recovery rate 
                        sigma, # incubation rate
                        r_Q, # Sympotom-based self quarantine rate
                        T_Q, # Number of days remained quarantined before clear
                        r_RS, # Reverse rate R - S, can also be considered as disease reocurrence rate
                        
                        # KMB 1/31: ignoring this params for now
                        Stg, # Strategy
                        Fq, # Every # of days 
                        Fr, # Percentage of population (%) to use
                        ST # Serve time (in months)
                        ){
  
  #### Initialization ####
  # Initialize variables
  TP <- TP/100
  TN <- TN/100
  
  FN <- 1 - TP
  FP <- 1 - TN
  
  T_recover <- 1/gamma
  T_incubate <- 1/sigma
  
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
      
      #V1
      #S_to_QS <- rbinom(1, total_pop[t-1, S], FP)
      #QS_to_S <- rbinom(1, total_pop[t-1, QS], TP)
      
      #E_to_QE <- rbinom(1, total_pop[t-1, E], TP)
      #QE_to_E <- rbinom(1, total_pop[t-1, QE], FP)
      
      #I_to_QI <- rbinom(1, total_pop[t-1, I], TP)
      
      # update current_pop with SickStick quarantines
      #pop[t, S] <- pop[t, S] - S_to_QS + QS_to_S
      #pop[t, E] <- pop[t, E] - E_to_QE + QE_to_E
      #pop[t, I] <- pop[t, I] - I_to_QI
      #pop[t, QS] <- pop[t, QS] + S_to_QS - QS_to_S
      #pop[t, QE] <- pop[t, QE] + E_to_QE - QE_to_E
      #pop[t, QI] <- pop[t, QI] + I_to_QI
    }
    
    # Normal Disease Dynamics ###
    # first compute if individual moves compartments, then move and update num days in compartment
    # then compute overall number of people in each compartment
    
    for (i in 1:N){
      if (current_pop[1,i] == S){
        move <- rbern(1, beta*(total_pop[t-1, I] - num_to_QI)/N) # I needs to be current_pop - sickstick quarantine # TODO: think about this rate calc
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
      
      else if (current_pop[1,i] == R){
        prob_success <- pnorm(current_pop[2,i], 1, 1) #TODO: update sd based on certain disease
        move <- rbern(1, prob_success)
        if (move == 1) {
          current_pop[1,i] = S
          current_pop[2,i] = 0
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
  }
   
  total_pop.df <- as.data.frame(total_pop)
  
  # Old version of the code
  
  # Turn the results into a dataframe , add a column with sickstick flag, 0 means no sickstick
  # pot <- cbind(Sv, Ev, Iv, Rv, Qv, SKv)
  # colnames(pot) <- c("Uninfected", "Exposed", "Infected", "Recovered", "Quarantined", "Total Sick Days")
  # pot 
}


rknot <- function(
  ST, # Discharge rate (# discharge per day)
  beta, # Probability of disease transmission per contact
  gamma, # Probability of recovery per capita
  sigma # Probability of disease progression
){
  mu <- 1/ST
  ro <- beta*sigma/((gamma+mu)*(sigma+mu))
  ro
}