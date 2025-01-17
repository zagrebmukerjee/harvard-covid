# Instructions:
# Set parameters in RunValues.csv
# Then run this script

source("PackageLoad.R")

campusSIRFunction <- function(
    r0, 
    testPCRSpecificity, 
    testPCRSensitivity, 
    testingTime, 
    extInf, 
    startingAsymptomatics, 
    studentPopulation,
    conditionalInfectionProb,
    symptomDevelopmentProportion,
    testingCost,
    testConfCost,
    falsePositiveReturnTime,
    podSizeInput,
    podInfectionProbInput,
    # partyRateInput,
    partySizeInput,
    partyContactsInput,
    ssDateInput,
    ssSizeInput,
    inctime,
    rectime,
    condmort
  ){

  
  testsToRun <- read.csv("testList.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  scenarioList <- read.csv("RunValues.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  
  
  for(t in 1:nrow(testsToRun)){
    
    testDetails <- testsToRun[t,]
    
    scenariosToRun <- scenarioList %>% filter(TestName %in% c(testDetails$scenario1, testDetails$scenario2))
    
    
    for(scenarioNumber in 1:nrow(scenariosToRun)){
      tic()
      
      # Overwrite default inputs with inputs from user
      
      # go from r0 to # contacts
      recoveryRate <- 1/rectime
      symptomOnsetRate <- recoveryRate*((symptomDevelopmentProportion/100)/(1-(symptomDevelopmentProportion/100)))
      divisor <- recoveryRate + symptomOnsetRate
      contactInfProb <- conditionalInfectionProb/100
      beta <- r0 * divisor
      avgContacts <- beta/contactInfProb
      
      scenariosToRun[scenarioNumber, ]$infectionRate <- r0
      scenariosToRun[scenarioNumber, ]$avgNonPodContacts <- avgContacts
      scenariosToRun[scenarioNumber, ]$testingTime <- testingTime
      scenariosToRun[scenarioNumber, ]$testPCRSpecificity <- testPCRSpecificity/100
      scenariosToRun[scenarioNumber, ]$testPCRSensitivity <- testPCRSensitivity/100
      scenariosToRun[scenarioNumber, ]$exogeneousShockSize <- extInf
      scenariosToRun[scenarioNumber, ]$startingAsymptomatics <- startingAsymptomatics
      scenariosToRun[scenarioNumber, ]$startingSusceptible <- studentPopulation - scenariosToRun[scenarioNumber, ]$startingAsymptomatics
      scenariosToRun[scenarioNumber, ]$conditionalInfectionProb <- conditionalInfectionProb/100
      
      scenariosToRun[scenarioNumber, ]$symptomDevelopmentProportion <- symptomDevelopmentProportion/100
      scenariosToRun[scenarioNumber, ]$testPCRCost <- testingCost
      scenariosToRun[scenarioNumber, ]$testConfCost <- testConfCost
      scenariosToRun[scenarioNumber, ]$falsePositiveReturnTime <- falsePositiveReturnTime

      scenariosToRun[scenarioNumber, ]$podSize <- podSizeInput
      scenariosToRun[scenarioNumber, ]$intraPodInfectionProb <- podInfectionProbInput/100
      scenariosToRun[scenarioNumber, ]$partyRate <- 1
      scenariosToRun[scenarioNumber, ]$partySize <- partySizeInput
      scenariosToRun[scenarioNumber, ]$contactsPerParty <- partyContactsInput
      
      scenariosToRun[scenarioNumber, ]$ssEventDate <- ssDateInput
      scenariosToRun[scenarioNumber, ]$ssEventSize <- ssSizeInput
      
      
      scenariosToRun[scenarioNumber, ]$incubationTime <- inctime
      scenariosToRun[scenarioNumber, ]$recoveryTime <- rectime
      scenariosToRun[scenarioNumber, ]$conditionalMortality <- condmort/100
      
      
      
      if(partySizeInput > 0){scenariosToRun[scenarioNumber, ]$parties <- TRUE} else {scenariosToRun[scenarioNumber, ]$parties <- FALSE}
      
      testParameters <- parameterSetupFunction(scenarioNumber, scenariosToRun)
      
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
      ggCharts <- ggPlotChartingFunction(chartData = outputData, chartDiags = diagnosticNumbers,  chartParameters = testParameters, annotations = TRUE)
      
      formattedResultsTable <- formattedResultsTableFunction(tables$keyResults %>%  filter(!(Name %in% c("Accurate Contact Traces", "Total Quarantine Entries")))) 
      chartDisplay <- dashboardChartingFunction(chartData = outputData, chartParameters = testParameters, annotations = TRUE)$dashboard
      positivityChart <- positivityChartingFunction(chartData = outputData, chartDiags = diagnosticNumbers, chartParameters = testParameters)
      formattedParameterTable <- formattedParameterTableFunction(tables$tableParams, testParameters = testParameters)

      outputForDiff <- list(
        testParameters = testParameters,
        outputData = outputData, 
        diagnosticNumbers = diagnosticNumbers,
        rawTables = tables,
        charts = ggCharts,
        table = formattedResultsTable,
        paramTable = formattedParameterTable
      )
      
      dashboardOutput <- list(
        chart = chartDisplay,
        positivityChart = positivityChart,
        table = formattedResultsTable,
        paramTable = formattedParameterTable,
        reportCharts = ggCharts,
        outputForDiff = outputForDiff)
      toc()
      
      # write.csv(formattedTable, "tmp.csv", row.names = FALSE)
      

      return(dashboardOutput)
      
      
    }
    
    
  }
}