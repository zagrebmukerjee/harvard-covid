
contactTracingWrapper <- function(invariants, contactsToTrace, FPContactsToTrace, stateData){
  
  # invariants <- invariants
  # contactsToTrace <- stateTransferMatrix["asymptomatic","truePositives"] + stateTransferMatrix["asymptomatic","symptomatic"]
  # FPContactsToTrace <- (1- invariants$testPCRSpecificity)/invariants$testingTime * previousOldState$susceptible
  # stateData <- states
  
  
  totalContactsRaw <- contactsToTrace + FPContactsToTrace
  totalContactsAdj <- min(totalContactsRaw, invariants$maxContactsTraced)
  
  contactsToTraceAdj <- totalContactsAdj/totalContactsRaw * contactsToTrace
  FPContactsToTraceAdj <- totalContactsAdj/totalContactsRaw * FPContactsToTrace
  
  ctResult <- contactTracingFunction(
    totalTraceesRaw = contactsToTraceAdj,
    contactTracingLookback = invariants$contactTracingWindow,
    contactTracingDelay = invariants$contactTracingDelay,
    contactTracingResponseRate = invariants$contactTracingResponseRate,
    contactTracingAccuracy = invariants$contactTracingAccuracy,
    avgContacts = invariants$avgNonPodContacts + invariants$podSize - 1,
    contactInfProb = invariants$conditionalInfectionProb,
    testingDelay = invariants$testingTime,
    incubationTime = invariants$incubationTime,
    doubleCountAdjustment = invariants$ctDoubleCountAdjustment,
    allStateData = stateData
  )
  
  FPCTResult <- contactTracingFunction(
    totalTraceesRaw = contactsToTraceAdj,
    contactTracingLookback = invariants$contactTracingWindow,
    contactTracingDelay = invariants$contactTracingDelay,
    contactTracingResponseRate = invariants$contactTracingResponseRate,
    contactTracingAccuracy = 0,
    avgContacts = invariants$avgNonPodContacts + invariants$podSize - 1,
    contactInfProb = invariants$conditionalInfectionProb,
    testingDelay = invariants$testingTime,
    incubationTime = invariants$incubationTime,
    doubleCountAdjustment = invariants$ctDoubleCountAdjustment,
    allStateData = stateData
  )
  
  totalResult <- ctResult + FPCTResult
  return(totalResult)
  
}



contactTracingFunction <- function(
  totalTraceesRaw,
  contactTracingLookback,
  contactTracingDelay,
  contactTracingResponseRate,
  contactTracingAccuracy,
  avgContacts,
  contactInfProb,
  testingDelay,
  incubationTime,
  doubleCountAdjustment,
  allStateData
){
  
  # totalTraceesRaw = contactsToTraceAdj
  # contactTracingLookback = invariants$contactTracingWindow
  # contactTracingDelay = invariants$contactTracingDelay
  # contactTracingResponseRate = invariants$contactTracingResponseRate
  # contactTracingAccuracy = invariants$contactTracingAccuracy
  # avgContacts = invariants$avgNonPodContacts + invariants$podSize - 1
  # contactInfProb = invariants$conditionalInfectionProb
  # testingDelay = invariants$testingTime
  # incubationTime = invariants$incubationTime
  # doubleCountAdjustment = invariants$ctDoubleCountAdjustment
  # allStateData = stateData
  

  expectedInfectiousTime <- ceiling(testingDelay/2 + contactTracingDelay)
  contactsTracedPerDay <- avgContacts * contactTracingResponseRate * doubleCountAdjustment * totalTraceesRaw 
  
  totalNewExposedFound <- 0
  totalRandomExposedFound <- 0
  totalRandomSuscFound <- 0
  totalRandomAsympFound <- 0
  
  totalPopulation <- rowSums(allStateData[1,])
  
  
  totalContactsTraced <- 0
  
  for(ctlb in 1:contactTracingLookback){
    
    ctState <- allStateData[nrow(allStateData)-ctlb,]
    ctSuscShare <- ctState$susceptible/nonIsolatedPopFunction(ctState)
    ctExpShare <- ctState$exposed/nonIsolatedPopFunction(ctState)
    ctAsympShare <- ctState$asymptomatic/nonIsolatedPopFunction(ctState)
    
    if(ctlb <= expectedInfectiousTime) {
      newExposedFound <-totalTraceesRaw* doubleCountAdjustment*contactTracingAccuracy* contactTracingResponseRate* (contactInfProb * avgContacts * ctSuscShare)
    } else {
      newExposedFound <-  0
    }
    
    contactsTracedRandom <- contactsTracedPerDay - newExposedFound
    
    totalNewExposedFound <- totalNewExposedFound + newExposedFound
    totalRandomExposedFound <- totalRandomExposedFound +  (contactsTracedRandom * ctExpShare)
    totalRandomSuscFound <- totalRandomSuscFound + (contactsTracedRandom * ctSuscShare)
    totalRandomAsympFound <- totalRandomAsympFound + (contactsTracedRandom * ctAsympShare)
  }
  
  propExposedBecameAsymp <-  (expectedInfectiousTime-incubationTime)/expectedInfectiousTime
  
  totalNewExposedFound <- totalNewExposedFound * (1- propExposedBecameAsymp)
  totalNewAsympFound <- totalNewExposedFound * propExposedBecameAsymp
  totalExposedFound <- totalRandomExposedFound + totalNewExposedFound
  totalAsympFound <- totalRandomAsympFound + totalNewAsympFound
  
  
  result <- data.frame(
    totalNewExposedFound = totalNewExposedFound,
    totalNewAsympFound = totalNewAsympFound,
    totalExposedFound = totalExposedFound,
    totalSuscFound = totalRandomSuscFound,
    totalAsympFound = totalAsympFound
  )
  
  return(result)
  
}