##################################
source("PackageLoad.R")

# load model functions
source("contactMatrixTransmission.R")
source("contactTracing.R")
source("Model.R")

# load parameter setup functions
source("ParameterSetup.R")
source("vizFunction.R")
source("Visualization.R")

library(ggplot2)
library(gridExtra)
library(grid)
##################################

objectiveTS <- read.csv("IllinoisPositivity.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE) %>% 
  mutate(day = 1:length(objectiveTS$Date)) %>% 
  rename(objective = Positivity) 


scenarioNumber <- 1


testsToRun <- read.csv("testListIllinois.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
scenarioList <- read.csv("RunValuesIllinois.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)


allResults <- data.frame(
  r_final = c(),
  finalerror = c(),
  name = c()
)
  

resultList <- list()
chartList <- list()
outputList <- list()
finalChartList <- list()

for(i in 1:nrow(testsToRun)){
  
  t <- i
  testDetails <- testsToRun[t,]
  scenariosToRun_Base <- scenarioList %>% filter(TestName %in% c(testDetails$scenario1, testDetails$scenario2))
  
  ##################################
  # Hill Climbing Setup
  ##################################
  
  r_initial <- 3
  stepVal <- .025
  direction <- -1 # going down or up
  
  nSteps <- 0
  r0ToUse <- r_initial
  previousError <- 9999
  
  
  
  results <- data.frame(
    rvalue = c(),
    step = c(),
    symptomatics = c(),
    isolations = c(),
    error = c()
  )
  allCharts <- list()
  allOutput <- list()
  
  
  
  
  
  while((nSteps < 50)) {
    
  
  
    tic()
    
    scenariosToRun <- scenariosToRun_Base
    scenariosToRun[scenarioNumber, ]$infectionRate <- r0ToUse
    # Overwrite default inputs with inputs from calc
    
    
    testParameters <- parameterSetupFunction(scenarioNumber, scenariosToRun)
    
    # hack for calibration process
    recoveryRate <- testParameters$timeInvariantParams$recoveryRate
    symptomOnsetRate <- testParameters$timeInvariantParams$symptomOnsetRate
    divisor <- recoveryRate + symptomOnsetRate
    contactInfProb <- testParameters$timeInvariantParams$conditionalInfectionProb
    beta <- testParameters$timeInvariantParams$infectionRate * divisor
    testParameters$timeInvariantParams$avgNonPodContacts <- beta/contactInfProb
    
    
    modelOutputs <- modelRunner(
      initialState = testParameters$stateParams,
      timeInvariants = testParameters$timeInvariantParams,
      exoShockFun = testParameters$exogeneousShockFunction,
      partyFun = testParameters$partyFunction,
      ssFun = testParameters$superSpreaderFunction,
      mechanics = testParameters$mechanicsParameters)
    
    outputData <- modelOutputs$outputData
    diagnosticNumbers <- modelOutputs$diagnosticNumbers
    diagnosticMatrix <- modelOutputs$diagnosticMatrix
    
    outputDataArr <- outputData %>%  arrange(cycle)
    ggCharts <- ggPlotChartingFunction(chartData = outputData, chartDiags = diagnosticNumbers,  chartParameters = testParameters, annotations = FALSE)
    
    positivityTimeSeries <- data.frame(
      cycle =outputDataArr$cycle,
      day = outputDataArr$day,
      testPositivity = c(NA, lapply(diagnosticNumbers, function(a){a$testPositivity}) %>%  unlist() ),
      trueTestPositivity = c(NA, lapply(diagnosticNumbers, function(a){a$trueTestPositivity}) %>%  unlist() )
    )
    
    predictionErrorTS <- positivityTimeSeries %>% 
      mutate(day = floor(day)) %>% 
      left_join(objectiveTS, by = c("day")) %>% 
      mutate(error = testPositivity - objective)
    
    totalError <- sqrt(sum((predictionErrorTS$error)^2, na.rm = TRUE))
  
    
    a <- plot_ly(data = predictionErrorTS, x = ~day, y = ~testPositivity, name = "Actual Positivity", 
            type = "scatter", mode = "lines", line = list(color = clrs1[[1]]))  %>%
      add_trace(y = ~objective, name = "Objective", line = list( color = clrs1[[2]]))
    
    
    results <- bind_rows(allResults, data.frame(
      rvalue = r0ToUse,
      step = nSteps,
      symptomatics = (tables$keyResults %>% filter(Name == "Total Students Symptomatic"))$Value,
      isolations = (tables$keyResults %>% filter(Name == "Total Isolation Entries"))$Value,
      error = totalError
    ))
    allCharts[[t]] <- ggCharts$trajectoryGGPlot
    allOutput[[t]] <- outputData

    print(paste0("Case: ", scenariosToRun[1,]$TestName))
    print(paste0("Steps: ", nSteps))
    print(paste0("Prev Error: ", previousError))
    print(paste0("Error: ", totalError))
    print(paste0("R0: ", r0ToUse))
    print(a)
    
    
    if(totalError >= previousError){
      print("end")
      nSteps <- 9999
      
    } else {
      print("proceed")
      r0ToUse <- r0ToUse * (1+direction * stepVal)
      
    }
    
    previousError <- totalError
    nSteps <- nSteps + 1
    toc()
    
    
  
  
  }
    
  
  r_final <- r0ToUse
  resultList <- append(resultList, results)
  chartList <- append(chartList, allCharts)
  outputList <- append(outputList, allOutput)
  
  
  allResults <- bind_rows(allResults, data.frame(
    r_final = r_final,
    finalerror = totalError,
    name = scenariosToRun[1,]$TestName
  ))
  
  finalChartList <- append(finalChartList, a)
  
  print(paste0("Final R0: ", r0ToUse))
  
}


write.csv(allResults, "tmp.csv")
