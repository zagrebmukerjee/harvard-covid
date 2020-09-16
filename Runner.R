

# Instructions:
# Set parameters in RunValues.csv
# Then run this script

source("PackageLoad.R")

# load model functions
source("contactMatrixTransmission.R")
source("contactTracing.R")
source("Model.R")

## ODE Runner
source("OdeModel.R")

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
    print(scenarioNumber)
    
    testParameters <- parameterSetupFunction(scenarioNumber, scenariosToRun)
    
    ## New runner
    modelOutputs <- modelRunner(
      initialState = testParameters$stateParams,
      timeInvariants = testParameters$timeInvariantParams,
      exoShockFun = testParameters$exogeneousShockFunction,
      partyFun = testParameters$partyFunction,
      mechanics = testParameters$mechanicsParameters)
    
    outputData <- modelOutputs$outputData
    diagnosticNumbers <- modelOutputs$diagnosticNumbers
    diagnosticMatrix <- modelOutputs$diagnosticMatrix
    
    charts <- chartingFunction(chartData = outputData, chartParameters = testParameters, annotations = TRUE)
    ggCharts <- ggPlotChartingFunction(chartData = outputData, chartParameters = testParameters, annotations = TRUE)
    tables <- tableFunction(tableData = outputData, tableParameters = testParameters, tableDiags = diagnosticNumbers)
    
    formattedTable <- formattedResultsTableFunction(tables$keyResults)
    
    outputs <- list(
      testParameters = testParameters,
      outputData = outputData, 
      diagnosticNumbers = diagnosticNumbers,
      diagnosticMatrix = diagnosticMatrix,
      tables = tables,
      charts = charts
    )
    save(outputs, file = paste0(dataLocation,scenariosToRun[scenarioNumber,]$TestName,"Data.Rdata"))
    
  }
  
  
  # vizRunner(generateDiff = testDetails$generateDiff, stitch = testDetails$stitch, testDetails = testDetails, scenariosToRun = scenariosToRun)
  
  # rm(list=c("outputData", "testParameters"))

}