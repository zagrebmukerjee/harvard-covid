###########################################
## Define shock function
###########################################

exogeneousShockFunction <- function(cycle, shockRate, shockSize){
  # Deterministic shock every shockRate days
  if(((cycle %% shockRate) == 0)&&(cycle != 0)){
    return(shockSize)
  } else { return(0)}
}

exogStochasticShockFunction <- function(cycle, shockRate, shockSize){
  # Poisson with intensity shockSize
  if( (cycle %% shockRate) == 0 ){
    return( rpois(1, shockSize))
  } else { return(0)}
}

exogStochasticContinuousShockFunction <- function(cycle, shockRate, shockSize){
  # Poisson with intensity shockSize, weighted per each step
  return( rpois(1, shockSize)/shockRate)
}

exogConstantShockFunction <- function(cycle, shockRate, shockSize){
  # Poisson with intensity shockSize/shockRate
  # independent of cycle
  shock = shockSize/shockRate
  
  return(shock)
}


partyFunction <- function(cycle, partyRate, partySize){
  # Deterministic shock every shockRate days
  if(((cycle %% partyRate) == 0)&&(cycle != 0)){
    return(partySize)
  } else { return(0)}
}

###########################################
## Collect parameters and specialize shock function
###########################################
parameterSetupFunction <- function(scenarioNumber, testMatrix){
  
  testSubset <- testMatrix[scenarioNumber,]
  
  mechanicsParameters <- list(
    nCycles = testSubset$nCycles, 
    cyclesPerDay = testSubset$cyclesPerDay
  )

  
  ###########################################
  # State Parameters
  ###########################################
  stateParams <- data.frame(
    cycle = 0,
    
    susceptible = testSubset$startingSusceptible,
    exposed = testSubset$startingExposed, 
    asymptomatic = testSubset$startingAsymptomatics,
    
    quarantineSusc = testSubset$startingQuarantineSusc,
    quarantineExp = testSubset$startingQuarantineExp,
    quarantineAsymp = testSubset$startingQuarantineAsymp,
    
    falsePositives = testSubset$startingFPs,
    truePositives = testSubset$startingTPs,
    immuneFPs = testSubset$startingImmuneFPs,
    symptomatic = testSubset$startingSymptomatic,
    
    recovered = testSubset$startingRecovered,
    hiddenRecovered = testSubset$startinghiddenRecovered,
    deaths = testSubset$startingDeaths,
    testingCost = testSubset$startingTestingCost
    
  )
  
  ###########################################
  # Model Parameters
  ###########################################
  
  
  modelParameters <- list(
    infectionRate = testSubset$infectionRate, # aka R0
    conditionalInfectionProb = testSubset$conditionalInfectionProb,
    avgNonPodContacts = testSubset$avgNonPodContacts,
    intraPodInfectionProb = testSubset$intraPodInfectionProb,
    recoveryTime = testSubset$recoveryTime, # in days
    incubationTime = testSubset$incubationTime, # in days
    symptomDevelopmentProportion = testSubset$symptomDevelopmentProportion, # as a percentage
    conditionalMortality = testSubset$conditionalMortality, # as a percentage rate, conditional on symptomatic - .0005 cond. on symptomatic
    exogeneousShockRate =  testSubset$exogeneousShockRate,  # how often (in days) new infections are added  - every nth cycle
    exogeneousShockSize = testSubset$exogeneousShockSize, # how many new infections added each time
    partyRate =  testSubset$partyRate,
    partySize = testSubset$partySize, 
    contactsPerParty = testSubset$contactsPerParty,
    
    testPCRSpecificity = testSubset$testPCRSpecificity, # 1 - false positive rate for PCR
    testPCRSensitivity = testSubset$testPCRSensitivity, # 1 - false negative rate for PCR
    testSerSpecificity = testSubset$testSerSpecificity, # 1 - false positive rate for Serology
    testSerSensitivity = testSubset$testSerSensitivity, # 1 - false negative rate for Serology
    testPCRCost = testSubset$testPCRCost, #cost in dollars
    testSerCost = testSubset$testSerCost, #cost in dollars
    testConfCost = testSubset$testConfCost, #
    
    falsePositiveReturnTime = testSubset$falsePositiveReturnTime, # in days
    testingTime =   testSubset$testingTime, # test every n days
    testingLag = testSubset$testingLag, # time to isolate the positive tests
    firstTestDelay = testSubset$firstTestDelay, # how long till testing starts
    studentPopulation = rowSums(stateParams %>%  select(-cycle, -testingCost)),
    podSize = testSubset$podSize,
    
    maxContactsTraced = testSubset$maxContactsTraced,
    contactTracingAccuracy = testSubset$contactTracingAccuracy,
    contactTracingWindow = testSubset$contactTracingWindow,
    contactTracingDelay = testSubset$contactTracingDelay,
    contactTracingResponseRate = testSubset$contactTracingResponseRate,
    ctDoubleCountAdjustment = testSubset$ctDoubleCountAdjustment,
    ctQuarTime = testSubset$ctQuarTime,
    
    runContactTracing = testSubset$runContactTracing,
    runSerologyTest = testSubset$runSerologyTest,
    pooledTests = testSubset$pooledTests,
    parties = testSubset$parties
    
    
  )
  
  
  
  ###########################################
  # preprocessing of parameters
  ###########################################
  
  

  timeInvariantParams <- modelParameters
  
  # convert times to cycles
  timeInvariantParams$recoveryTimeCycles <- timeInvariantParams$recoveryTime * testSubset$cyclesPerDay
  timeInvariantParams$incubationTimeCycles <- timeInvariantParams$incubationTime * testSubset$cyclesPerDay
  timeInvariantParams$exogeneousShockRate <- timeInvariantParams$exogeneousShockRate * testSubset$cyclesPerDay
  timeInvariantParams$partyRate <- timeInvariantParams$partyRate * testSubset$cyclesPerDay
  timeInvariantParams$falsePositiveReturnTime <- timeInvariantParams$falsePositiveReturnTime * testSubset$cyclesPerDay
  timeInvariantParams$testingTime <- timeInvariantParams$testingTime * testSubset$cyclesPerDay
  timeInvariantParams$avgNonPodContacts <- timeInvariantParams$avgNonPodContacts / testSubset$cyclesPerDay # contacts per cycles
  timeInvariantParams$firstTestDelay <- timeInvariantParams$firstTestDelay * testSubset$cyclesPerDay
  
  timeInvariantParams$maxContactsTraced <- timeInvariantParams$maxContactsTraced/ testSubset$cyclesPerDay
  timeInvariantParams$contactTracingWindow <- timeInvariantParams$contactTracingWindow * testSubset$cyclesPerDay
  timeInvariantParams$contactTracingDelay <- timeInvariantParams$contactTracingDelay * testSubset$cyclesPerDay
  timeInvariantParams$ctQuarTimeCycles <- timeInvariantParams$ctQuarTime * testSubset$cyclesPerDay
  
  
    
  if(timeInvariantParams$pooledTests){timeInvariantParams$testingLag <- timeInvariantParams$testingLag + 1} # 1 day lag in pooled test results
  timeInvariantParams$testingLag <- round(timeInvariantParams$testingLag * testSubset$cyclesPerDay,0)
  
  timeInvariantParams$recoveryRate <- 1/timeInvariantParams$recoveryTimeCycles # rho
  timeInvariantParams$incubationRate <- 1/timeInvariantParams$incubationTimeCycles # from exposed to asymp
  timeInvariantParams$symptomOnsetRate <- timeInvariantParams$recoveryRate * timeInvariantParams$symptomDevelopmentProportion / (1-timeInvariantParams$symptomDevelopmentProportion) # sigma
  timeInvariantParams$podInfectionRate <- timeInvariantParams$recoveryRate * (timeInvariantParams$intraPodInfectionProb/(1 - timeInvariantParams$intraPodInfectionProb))
  
  timeInvariantParams$intraPodContacts <- timeInvariantParams$podInfectionRate/ timeInvariantParams$conditionalInfectionProb
  
  timeInvariantParams$falsePositiveReturnRate <- 1/ timeInvariantParams$falsePositiveReturnTime # mu
  timeInvariantParams$testingRate <- 1/ timeInvariantParams$testingTime # tau
  
  timeInvariantParams$asymptomaticTransRate <- timeInvariantParams$infectionRate * (timeInvariantParams$recoveryRate + timeInvariantParams$symptomOnsetRate) # beta
  timeInvariantParams$hhTransRate <- (timeInvariantParams$hhInfectionRate * (timeInvariantParams$podSize - 1)) * (timeInvariantParams$recoveryRate + timeInvariantParams$symptomOnsetRate) # beta for households
  
  k <- timeInvariantParams$conditionalMortality /(timeInvariantParams$symptomOnsetRate / (timeInvariantParams$symptomOnsetRate + timeInvariantParams$recoveryRate))
  timeInvariantParams$deathRate <- k *  timeInvariantParams$recoveryRate/(1-k)
  
  timeInvariantParams$ctQuarReleaseRate <- 1/timeInvariantParams$ctQuarTimeCycles
  
  
  
  # exogeneous shock function (for the above parameters)
  # takes cycle and returns size of exogeneous shock on that day - how many new asymptomatic


  modelshockFun = function(cycle) exogeneousShockFunction(cycle,
          shockRate=timeInvariantParams$exogeneousShockRate,
          shockSize=timeInvariantParams$exogeneousShockSize)
  
  modelPartyFun <- function(cycle) partyFunction(cycle = cycle, 
                                                 partyRate = timeInvariantParams$partyRate,
                                                 partySize = timeInvariantParams$partySize)
  
  return(list(stateParams = stateParams,
              timeInvariantParams = timeInvariantParams, 
              exogeneousShockFunction = modelshockFun,
              partyFunction = modelPartyFun,
              mechanicsParameters = mechanicsParameters, 
              modelParameters = modelParameters))
  
  
}


