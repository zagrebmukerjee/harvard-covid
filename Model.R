
# these evolution functions look at the last state(s) and add 1 new state according to rules
# given that the model depends on looking back some amount of time, the first few cycles are specially defined

assertConsistency = function(newState, oldState, thr=1e-8){
  # get only population columns
  newPop = select(newState, -c(cycle, testingCost))
  oldPop = select(oldState, -c(cycle, testingCost))
  # check sum of deltas is 0
  stopifnot(abs(sum(newPop)-sum(oldPop)) < thr)
  # check magnitude of negative deltas is a fraction of previous state (boundedness)
  # stopifnot(
  #   ((newPop-oldPop) / (oldPop + thr)) > -1)
  # safety check, stop if any negative value:
  stopifnot(newPop >= -1e-8)
}

# modification of initial conditions based on serology - assume all tested
serologySetupFunction <- function(states, invariants){
  
  serologyState <- states
  
  # batch test 
  serologyState$recovered <- serologyState$recovered + invariants$testSerSensitivity * serologyState$hiddenRecovered
  serologyState$hiddenRecovered <- serologyState$hiddenRecovered - invariants$testSerSensitivity * serologyState$hiddenRecovered
  
  serologyState$testingCost <- serologyState$testingCost + invariants$testSerCost * rowSums(serologyState)
  
  
  return(serologyState)
  
}


nonIsolatedPopFunction <- function(state){(state$susceptible + state$exposed + state$asymptomatic) }


evolutionFunction <- function(states, invariants, mechanics, exoShockFun, partyFun, ssFun, stateNameVec, initializing){

  # states <- stateDataFrame
  # invariants <- timeInvariants
  # exoShockFun <- testParameters$exogeneousShockFunction
  # partyFun <- partyFun
  # ssFun <- ssFunction
  # mechanics <- testParameters$mechanicsParameters
  # initializing <- FALSE
  # stateNameVec <- colnames(states %>% select(-cycle, -testingCost))
  
  oldState <- states %>%  filter(cycle == max(cycle))
  if(!initializing){previousOldState <- states %>%  filter(cycle == max(cycle) - (invariants$testingLag - 1))}
  
  
  # get shock for this cycle:
  exoShockValue <- exoShockFun(oldState$cycle + 1)
  superSpreaderValue <- ssFun(oldState$cycle + 1)
  if(invariants$parties) {partyValue <- partyFun(oldState$cycle + 1)} else {partyValue <- 0}
  
  
  nStates <- length(stateNameVec)
  asymptomaticShareOfNonIsolated <- oldState$asymptomatic/nonIsolatedPopFunction(oldState)
  
  ################################################
  # progress the model
  ################################################
  
  # how many go where. to ROW from COLUMN - ["susceptible","asymptomatic"] is # people going from susc. to asymp.
  stateTransferMatrix <- matrix(data = rep(0, nStates^2), nrow = nStates, ncol = nStates, dimnames = list(stateNameVec, stateNameVec) )
  
  contactMatrixTransmissionValue <- contactMatrixTransmission(invariants, partyValue)
  
  
  # print(testParameters$timeInvariantParams$asymptomaticTransRate - contactMatrixTransmissionValue)
  stateTransferMatrix["susceptible","exposed"] <- - (contactMatrixTransmissionValue*(asymptomaticShareOfNonIsolated))* oldState$susceptible  - exoShockValue - superSpreaderValue
  stateTransferMatrix["susceptible","falsePositives"] <- if(initializing){0} else {
    - (1- invariants$testPCRSpecificity)/invariants$testingTime * previousOldState$susceptible + # new FPs
      invariants$falsePositiveReturnRate * oldState$falsePositives } # old FPs returning
  
  stateTransferMatrix["exposed","asymptomatic"] <- -invariants$incubationRate*oldState$exposed
  stateTransferMatrix["asymptomatic","symptomatic"] <- -invariants$symptomOnsetRate * oldState$asymptomatic
  stateTransferMatrix["asymptomatic","recovered"] <- -invariants$recoveryRate * oldState$asymptomatic
  stateTransferMatrix["asymptomatic","truePositives"] <- if(initializing){0} else{ 
    -invariants$testPCRSensitivity/invariants$testingTime * previousOldState$asymptomatic}
  
  stateTransferMatrix["immuneFPs","hiddenRecovered"] <- if(initializing){0} else{ 
    ((1 - invariants$testPCRSpecificity)/invariants$testingTime) * previousOldState$hiddenRecovered -
      invariants$falsePositiveReturnRate * oldState$immuneFPs}
  
  stateTransferMatrix["quarantineExp","quarantineAsymp"] <- -invariants$incubationRate*oldState$quarantineExp
  stateTransferMatrix["quarantineAsymp","symptomatic"] <- -invariants$symptomOnsetRate* oldState$quarantineAsymp 
  stateTransferMatrix["quarantineAsymp","truePositives"] <- -invariants$ctQuarReleaseRate* oldState$quarantineAsymp 
  
  stateTransferMatrix["truePositives","recovered"] <- -invariants$recoveryRate * oldState$truePositives 
  stateTransferMatrix["truePositives","symptomatic"] <- -invariants$symptomOnsetRate* oldState$truePositives 
  
  stateTransferMatrix["symptomatic","deaths"] <- -invariants$deathRate * oldState$symptomatic
  stateTransferMatrix["symptomatic","recovered"] <- -invariants$recoveryRate * oldState$symptomatic
  

  if(invariants$runContactTracing && !initializing){ 
  
    # contact tracing with hack to prevent more CT than exist
    stateTransferMatrixTest <- stateTransferMatrix
    stateTransferMatrixTest[lower.tri(stateTransferMatrixTest)] <- -1 *(stateTransferMatrixTest %>%  t())[lower.tri(stateTransferMatrixTest %>%  t())]
    oldStateBase <- oldState %>%  select(-cycle, -testingCost)
    proposedChange <-  rowSums(stateTransferMatrixTest)
    proposedNewState <- oldStateBase + proposedChange
    
    contactsTraced <- contactTracingWrapper(
      invariants = invariants,
      contactsToTrace = -stateTransferMatrix["asymptomatic","truePositives"] - stateTransferMatrix["asymptomatic","symptomatic"],
      FPContactsToTrace = (1- invariants$testPCRSpecificity)/invariants$testingTime * previousOldState$susceptible,
      stateData = states
    )
    
    stateTransferMatrix["susceptible","quarantineSusc"] <- -contactsTraced$totalSuscFound + 
      invariants$ctQuarReleaseRate * oldState$quarantineSusc 
    stateTransferMatrix["exposed","quarantineExp"] <- -min(contactsTraced$totalExposedFound,proposedNewState$exposed)
    stateTransferMatrix["asymptomatic","quarantineAsymp"] <- -min(contactsTraced$totalAsympFound, proposedNewState$asymptomatic)
    
  }

  stateTransferMatrix[lower.tri(stateTransferMatrix)] <- -1 *(stateTransferMatrix %>%  t())[lower.tri(stateTransferMatrix %>%  t())]
  
  # if we are going to make something negative instead normalize the transition to the size of that population
  oldStateBase <- oldState %>%  select(-cycle, -testingCost)
  proposedChange <-  rowSums(stateTransferMatrix)
  proposedNewState <- oldStateBase + proposedChange
  stateTransferMatrixClean <- stateTransferMatrix
  # TODO: this fails with > 1 0
  
  for(i in 1:nStates){
    
      if(proposedNewState[[1,i]] < 0){ 
        
        rescaleFactor <- oldStateBase[[1,i]]/abs(proposedChange[[i]])
        
        stateTransferMatrixClean[i,] <- stateTransferMatrix[i,] *rescaleFactor
        stateTransferMatrixClean[,i] <- stateTransferMatrix[,i] *rescaleFactor
      }
        
  }

  
  finalChange <- rowSums(stateTransferMatrixClean)
  
  newState <- oldStateBase + finalChange
  
  newState$cycle <- oldState$cycle + 1  
  newState <- newState %>%  select(cycle, everything())
  
  ################################################
  # compute some diagnostic numbers
  ################################################
  
  
  newStudentsSymptomatic <- invariants$symptomOnsetRate * oldState$asymptomatic + # symptomatic from asymp
    invariants$symptomOnsetRate* oldState$truePositives # symptomatic from TPs
  
  if(initializing){
    newStudentsEnteringIsolation <- newStudentsSymptomatic
    newStudentsEnteringQuarantine <- 0
    accurateCTs <- 0
    
    newTestCost <- 0
    testPositivity <- NA
    trueTestPositivity <- NA
    
  } else {

    if(invariants$runContactTracing){
      newStudentsEnteringQuarantine <- -stateTransferMatrix["susceptible","quarantineSusc"] - stateTransferMatrix["exposed","quarantineExp"] - stateTransferMatrix["asymptomatic","quarantineAsymp"]  
      accurateCTs <- contactsTraced$totalNewExposedFound + contactsTraced$totalNewAsympFound
    } else {
      newStudentsEnteringQuarantine <- 0
      accurateCTs <- 0}
    
    totalTestsDone <- (previousOldState$susceptible + previousOldState$exposed + previousOldState$asymptomatic + previousOldState$hiddenRecovered)/invariants$testingTime

    truePositiveTests <- invariants$testPCRSensitivity/invariants$testingTime * previousOldState$asymptomatic  # TPs from symptomatic
    falsePositiveTests <- (1- invariants$testPCRSpecificity)/invariants$testingTime * previousOldState$susceptible + # FPs from susceptible 
      ((1 - invariants$testPCRSpecificity)/invariants$testingTime) * previousOldState$hiddenRecovered  # FPs from hiddenRec

    totalPositiveTests <- truePositiveTests + falsePositiveTests
    testPositivity <- totalPositiveTests/totalTestsDone
    trueTestPositivity <- truePositiveTests/totalTestsDone
    
    newStudentsEnteringIsolation <- totalPositiveTests + newStudentsSymptomatic
    
    
    newTestCost <- invariants$testPCRCost * totalTestsDone
    if(invariants$pooledTests){newTestCost <- newTestCost/invariants$podSize}
    
    # conf test cost
    newTestCost <- newTestCost + invariants$testConfCost * newStudentsEnteringIsolation
    
    
  }
  
  newState$testingCost <- oldState$testingCost + newTestCost
  
  

  
  if(newState$cycle %% 24 == 0){print(newState$cycle/mechanics$nCycles)}
  # print(rowSums(oldState %>% select(-cycle, -testingCost)))
  # print(rowSums(newState %>% select(-cycle, -testingCost)))
   
  
  # DEBUG:
  assertConsistency(newState, oldState)


  diagNumbers <- list(
    newStudentsEnteringQuarantine = newStudentsEnteringQuarantine,
    accurateCTs = accurateCTs,
    newStudentsEnteringIsolation = newStudentsEnteringIsolation,
    newStudentsSymptomatic = newStudentsSymptomatic, 
    testPositivity = testPositivity,
    trueTestPositivity = trueTestPositivity)
  
  ################################################
  # outputs
  ################################################
  
  
  return(list(newState = newState, diagNumbers = diagNumbers, stateTransferMatrixClean = stateTransferMatrixClean))
  
  
}

################################################
# main function
################################################

modelRunner <- function(initialState, timeInvariants, exoShockFun, partyFun, ssFunction, mechanics){

  # initialState <- testParameters$stateParams
  # timeInvariants <- testParameters$timeInvariantParams
  # exoShockFun <- testParameters$exogeneousShockFunction
  # partyFun = testParameters$partyFunction
  # ssFunction = testParameters$superSpreaderFunction
  # mechanics <- testParameters$mechanicsParameters
  

  stateNames <- colnames(initialState %>% select(-cycle, -testingCost))
  stateDataFrame <- initialState
  diagnosticMatrix <- list()
  diagnosticNumbers <- list()
  
  if(timeInvariants$runSerologyTest){stateDataFrame <- serologySetupFunction(stateDataFrame, timeInvariants)} 
  
  
  while (max(stateDataFrame$cycle) < timeInvariants$firstTestDelay){
    
    
    step <- evolutionFunction(stateDataFrame, timeInvariants, mechanics, exoShockFun, partyFun, ssFunction, stateNames, initializing = TRUE)
    diagnosticMatrix[[max(stateDataFrame$cycle)+1]] <- step$stateTransferMatrixClean
    diagnosticNumbers[[max(stateDataFrame$cycle)+1]] <- step$diagNumbers
    stateDataFrame <- bind_rows(stateDataFrame, step$newState )
    
  }
  
  while (max(stateDataFrame$cycle) < mechanics$nCycles){
    
    step <- evolutionFunction(stateDataFrame, timeInvariants, mechanics, exoShockFun, partyFun, ssFunction, stateNames, initializing = FALSE)
    diagnosticMatrix[[max(stateDataFrame$cycle)+1]] <- step$stateTransferMatrixClean
    diagnosticNumbers[[max(stateDataFrame$cycle)+1]] <- step$diagNumbers
    stateDataFrame <- bind_rows(stateDataFrame, step$newState )
    
    
  }
  outputData <- stateDataFrame
  
  outputData$population <- rowSums(outputData %>%  select(-cycle, -testingCost))
  
  outputData <- outputData %>%  
    mutate(
      infected = asymptomatic + truePositives + symptomatic,
      bedsUsed = truePositives + falsePositives + symptomatic + immuneFPs,
      quarUsed = quarantineSusc + quarantineExp + quarantineAsymp,
      allRecovered = recovered + hiddenRecovered,
      day = cycle /mechanics$cyclesPerDay)
  
  
  
  
  return(list(outputData = outputData, diagnosticMatrix = diagnosticMatrix, diagnosticNumbers = diagnosticNumbers))
  
}