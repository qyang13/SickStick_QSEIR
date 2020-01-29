# Global Variables
ST <- 48    # Average Serving time in month
mu <- 1/ST  # Recruit/discharge rate
Ip <- 0.01  # Initial infected population fraction
Qr <- 0     # Self-report based quarantine rate
Tm <- 365   # Observation time in days



processData <- function(
                        TP, # TP of SickStick
                        TN, # TN of SickStick
                        beta, # Infection rate
                        gamma, # Recovery rate 
                        sigma, # Disease progression rate
                        DQ, # Number of days remained quarantined before clear
                        Rvr, # Reverse rate R - S, can also be considered as disease reocurrence rate
                        Stg, # Strategy
                        Fq, # Every # of days 
                        Fr, # Percentage of population (%) to use
                        OS # Outbreak Scenarios
                        ){
  if (OS==1) {
    beta = 0.4
    gamma = 0.79
    sigma = 1
  }
  
  else if (OS==2) {
    beta = 1.4
    gamma = 1.55
    sigma = 1
  }
  
  else if (OS==5) {
    beta = 1.96
    gamma = 0.13
    sigma = 0.22
  }
  

  # Population initialization
  Sv <- S <- (1-Ip) # Susceptible population
  Ev <- E <- Ip        # Exposed population
  Iv <- I <-      # Infected population
  Rv <- R <- 0        # Recovered population
  QH <- 0          # Healthy quarantined
  QI <- 0       # Infected quarantined
  Qv <- 0
  SKv <- SK <- 0
  
  FN <- 1 - TP
  FP <- 1 - TN
  

  if (ST==0) {mu <- 0} else {mu <- 1/ST}
  
  if (Stg == 1) {
    Fq <- 0
    Fr <- 1
  }
  
  if (Stg != 3) { # Everyday everyone, someday everyone
  # Function definition
  
  dS <- function() mu - beta*S*I - mu*S + Rvr*R + (TN^DQ)*QH 
  
  dS_QH <- function() - FP*S
  
  dQH <- function() FP*S - mu*QH - (TN^DQ)*QH
  
  dE <- function() beta*S*I - sigma*E*FN - mu*E - TP*E
  
  dI <- function() sigma*E*FN - mu*I - gamma*I - TP*I - Qr*I
  
  dQI <- function() TP*E + TP*I + Qr*I - gamma*QI - QI*mu
  
  dR <- function() gamma*I + gamma*QI - R*Rvr - R*mu
  }
  
  else { # Everyday, some people

    dS <- function() mu - beta*S*I - mu*S + Rvr*R + (TN^DQ)*QH 
    
    dS_QH <- function() - FP*S*Fr
    
    dQH <- function() FP*S*Fr - mu*QH - (TN^DQ)*QH
    
    dE <- function() beta*S*I - sigma*E*FN*Fr - mu*E - TP*E*Fr - sigma*E*(1-Fr)
    
    dI <- function() sigma*E*FN*Fr + sigma*E*(1-Fr) - mu*I - gamma*I - TP*I*Fr - Qr*I 
    
    dQI <- function() TP*E*Fr + TP*I*Fr + Qr*I - gamma*QI - QI*mu
    
    dR <- function() gamma*I + gamma*QI - R*Rvr - R*mu
  }

  
  
  if ((Stg == 1) | (Stg == 3)) {
  # Loop through periods
  for (d in 1:Tm) {
    ds = dS()
    de = dE()
    di = dI()
    dr = dR()
    dqi = dQI()
    
    
    S = S + ds
    E = E + de
    I = I + di
    R = R + dr
    QI = QI + dqi
    
    QH = QH + dQH()
    
    S = S + dS_QH()

    SK = QH + QI + I

    
    # Save the changes in vector form
    Sv = c(Sv, S)
    Ev = c(Ev, E)
    Iv = c(Iv, I)
    Rv = c(Rv, R)
    Qv = c(Qv, QH+QI)
    SKv = c(SKv, SK)
  }}
  else if (Stg == 2) {
    tempTP <- TP
    tempFP <- FP
    tempFN <- FN
    # Loop through periods
    for (d in 1:Tm) {
      if (d%%Fq!=0){
        FP <- 0
        TP <- 0
        FN <- 1
      } else {
        FP = tempFP
        TP = tempTP
        FN = tempFN
      }
      
      ds = dS()
      de = dE()
      di = dI()
      dr = dR()
      dqi = dQI()
      
      
      S = S + ds
      E = E + de
      I = I + di
      R = R + dr
      QI = QI + dqi
      
      QH = QH + dQH()
      
      S = S - FP*S
      
      SK = QH + QI + I
      
      
      # Save the changes in vector form
      Sv = c(Sv, S)
      Ev = c(Ev, E)
      Iv = c(Iv, I)
      Rv = c(Rv, R)
      Qv = c(Qv, QH+QI)
      SKv = c(SKv, SK)
    }
  } 
  
  # Turn the results into a datafrome , add a column with sickstick flag, 0 means no sickstick
  pot <- cbind(Sv, Ev, Iv, Rv, Qv, SKv)
  colnames(pot) <- c("Uninfected", "Exposed", "Infected", "Recovered", "Quarantined", "Total Sick Days")
  pot 
}


rknot <- function(
  ST, # Discharge rate (# discharge per day)
  beta, # Probability of disease transmission per contact
  gamma, # Probability of recovery per capita
  sigma # Probability of disease progression
){
  ro <- beta*sigma/((gamma+mu)*(sigma+mu))
  ro
}