library(dplyr)

dataSIR <- function(nContacts, contactInfProb, recoveryRate, nDays){
  
  states <- data.frame(day = 1, susceptible = 999, infected = 1, recovered = 0)
  
  while(max(states$day) < nDays){
    
    currentState <- states %>%  dplyr::filter(day == max(day))
    
    # For each person, there are 3 steps: 1) How many people do you meet? 
    # 2) In expectation, what share of those people are infected? 
    # 3) Given you meet an infected person, will you get infected?
    newInfections <- currentState$infected/(currentState$infected + currentState$susceptible) * nContacts * contactInfProb * currentState$susceptible
    
    newRecoveries <- currentState$infected * recoveryRate
    
    newState <- data.frame(
      day = currentState$day + 1,
      susceptible = currentState$susceptible - newInfections,
      infected = currentState$infected + newInfections - newRecoveries,
      recovered = currentState$recovered + newRecoveries
    )
    
    states <- bind_rows(states, newState)
    
  }
  
  return(states)
  
}