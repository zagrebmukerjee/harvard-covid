# Instructions:
# Set parameters in RunValues.csv
# Then run this script

source("PackageLoad.R")

campusSIRFunction <- function(
    r0, 
    testPCRSpecificity, 
    testPCRSensitivity, 
    testingTime, 
    commInf, 
    startingAsymptomatics, 
    studentPopulation,
    conditionalInfectionProb,
    totalDays,
    symptomDevelopmentProportion,
    testingCost,
    testConfCost,
    falsePositiveReturnTime,
    podSizeInput,
    podInfectionProbInput,
    partyRateInput,
    partySizeInput,
    partyContactsInput,
    ssDateInput,
    ssSizeInput
  ){
   
  
  # load model functions
  source("contactMatrixTransmission.R")
  source("contactTracing.R")
  source("Model.R")
  
  # load parameter setup functions
  source("ParameterSetup.R")
  source("vizFunction.R")
  source("Visualization.R")
  
  dataLocation <- "RData/"
  if (!dir.exists(dataLocation)){dir.create(dataLocation)} 
  pdfLocation <- "pdfs/"
  if (!dir.exists(pdfLocation)){dir.create(pdfLocation)}
  
  
  testsToRun <- read.csv("testList.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  scenarioList <- read.csv("RunValues.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  
  
  for(t in 1:nrow(testsToRun)){
    
    testDetails <- testsToRun[t,]
    
    scenariosToRun <- scenarioList %>% filter(TestName %in% c(testDetails$scenario1, testDetails$scenario2))
    
    
    for(scenarioNumber in 1:nrow(scenariosToRun)){
      tic()
      
      # Overwrite default inputs with inputs from user
      
      # go from r0 to # contacts
      recoveryRate <- 1/14
      symptomOnsetRate <- recoveryRate*(.3/(1-.3))
      divisor <- recoveryRate + symptomOnsetRate
      contactInfProb <- scenariosToRun[scenarioNumber, ]$conditionalInfectionProb
      beta <- r0 * divisor
      avgContacts <- beta/contactInfProb
      
      scenariosToRun[scenarioNumber, ]$infectionRate <- r0
      scenariosToRun[scenarioNumber, ]$avgNonPodContacts <- avgContacts
      scenariosToRun[scenarioNumber, ]$testingTime <- testingTime
      scenariosToRun[scenarioNumber, ]$testPCRSpecificity <- testPCRSpecificity/100
      scenariosToRun[scenarioNumber, ]$testPCRSensitivity <- testPCRSensitivity/100
      scenariosToRun[scenarioNumber, ]$exogeneousShockSize <- commInf
      scenariosToRun[scenarioNumber, ]$startingAsymptomatics <- startingAsymptomatics
      scenariosToRun[scenarioNumber, ]$startingSusceptible <- studentPopulation - scenariosToRun[scenarioNumber, ]$startingAsymptomatics
      scenariosToRun[scenarioNumber, ]$conditionalInfectionProb <- conditionalInfectionProb/100
      scenariosToRun[scenarioNumber, ]$nCycles <- totalDays * scenariosToRun[scenarioNumber, ]$cyclesPerDay
      scenariosToRun[scenarioNumber, ]$symptomDevelopmentProportion <- symptomDevelopmentProportion/100
      scenariosToRun[scenarioNumber, ]$testPCRCost <- testingCost
      scenariosToRun[scenarioNumber, ]$testConfCost <- testConfCost
      scenariosToRun[scenarioNumber, ]$falsePositiveReturnTime <- falsePositiveReturnTime

      scenariosToRun[scenarioNumber, ]$podSize <- podSizeInput
      scenariosToRun[scenarioNumber, ]$intraPodInfectionProb <- podInfectionProbInput/100
      scenariosToRun[scenarioNumber, ]$partyRate <- partyRateInput
      scenariosToRun[scenarioNumber, ]$partySize <- partySizeInput
      scenariosToRun[scenarioNumber, ]$contactsPerParty <- partyContactsInput
      
      scenariosToRun[scenarioNumber, ]$ssEventDate <- ssDateInput
      scenariosToRun[scenarioNumber, ]$ssEventSize <- ssSizeInput
      
      
      if(partyRateInput > 0){scenariosToRun[scenarioNumber, ]$parties <- TRUE} else {scenariosToRun[scenarioNumber, ]$parties <- FALSE}
      
      testParameters <- parameterSetupFunction(scenarioNumber, scenariosToRun)
      
      print(testParameters$timeInvariantParams$avgNonPodContacts)
      
      
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
      
      totalStudentsEnteringQuarantine <-  Reduce(x =  lapply(diagnosticNumbers, function(a){a$newStudentsEnteringQuarantine}), f = sum)
      totalStudentsEnteringIsolation <-  Reduce(x =  lapply(diagnosticNumbers, function(a){a$newStudentsEnteringIsolation}), f = sum)
      
      #charts <- chartingFunction(chartData = outputData, chartParameters = testParameters, annotations = TRUE)
      tables <- tableFunction(tableData = outputData, tableParameters = testParameters, tableDiags = diagnosticNumbers)
      ggCharts <- ggPlotChartingFunction(chartData = outputData, chartParameters = testParameters, annotations = TRUE)
      
      formattedTable <- formattedResultsTableFunction(tables$keyResults %>%  filter(!(Name %in% c("Accurate Contact Traces", "Total Quarantine Entries")))) 
      chartDisplay <- dashboardChartingFunction(chartData = outputData, chartParameters = testParameters, annotations = TRUE)$dashboard
      positivityChart <- positivityChartingFunction(chartData = outputData, chartDiags = diagnosticNumbers, chartParameters = testParameters)
      formattedParameterTable <- formattedParameterTableFunction(tables$tableParams, testParameters = testParameters)

      output <- list("chart" = chartDisplay, "positivityChart" = positivityChart, "table" = formattedTable, "paramTable" = formattedParameterTable, "reportCharts" = ggCharts)
      toc()
      
      # write.csv(formattedTable, "tmp.csv", row.names = FALSE)
      
      return(output)
      
      
    }
    
    
  }
}