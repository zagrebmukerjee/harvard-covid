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

testsToRun <- read.csv("testListStatic.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
scenarioList <- read.csv("RunValuesStatic.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)

allResults <- data.frame(
  name = c(),
  symptomatics = c(),
  isolations = c(),
  cost = c()
)
allCharts <- list()
allOutput <- list()

for(t in 1:nrow(testsToRun)){
  
  testDetails <- testsToRun[t,]
  
  scenariosToRun <- scenarioList %>% filter(TestName %in% c(testDetails$scenario1, testDetails$scenario2))
  
  
  for(scenarioNumber in 1:nrow(scenariosToRun)){
    tic()
    
    # Overwrite default inputs with inputs from user
    
    # go from r0 to # contacts
    
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
    
    totalStudentsEnteringQuarantine <-  Reduce(x =  lapply(diagnosticNumbers, function(a){a$newStudentsEnteringQuarantine}), f = sum)
    totalStudentsEnteringIsolation <-  Reduce(x =  lapply(diagnosticNumbers, function(a){a$newStudentsEnteringIsolation}), f = sum)
    
    #charts <- chartingFunction(chartData = outputData, chartParameters = testParameters, annotations = TRUE)
    tables <- tableFunction(tableData = outputData, tableParameters = testParameters, tableDiags = diagnosticNumbers)
    ggCharts <- ggPlotChartingFunction(chartData = outputData, chartDiags = diagnosticNumbers,  chartParameters = testParameters, annotations = FALSE, isolations = FALSE)
    
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
    
    
    allResults <- bind_rows(allResults, data.frame(
      name = scenariosToRun[scenarioNumber,]$TestName,
      population = testParameters$timeInvariantParams$studentPopulation,
      symptomatics = (tables$keyResults %>% filter(Name == "Total Students Symptomatic"))$Value,
      isolations = (tables$keyResults %>% filter(Name == "Total Isolation Entries"))$Value,
      cost = (tables$keyResults %>% filter(Name == "Testing Cost"))$Value))
    allCharts[[t]] <- ggCharts$trajectoryGGPlot
    allOutput[[t]] <- outputData
  }
  
  
}


allResults <- allResults %>%  mutate(
  symptomaticsPC = symptomatics/population*1000,
  isolationsPC = isolations/population*1000,
)


g <- ggplotGrob(allCharts[[1]] + 
                  theme(
                    legend.position = "right",
                    legend.text = element_text(size = 12),
                    legend.key.size = unit(.25,"inches")))$grobs
legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
lheight <- sum(legend$height)
lwidth <- sum(legend$width)

allChartsForDisplay <- lapply(allCharts, function(a){
  a + ylab("")+xlab("")+ theme(legend.position = "none")
})


allChartsForDisplay[1:9] <- lapply(allChartsForDisplay[1:9], function(a){
  a + theme(axis.text.x=element_blank())
})

gl <-  c(allChartsForDisplay, ncol = 3, nrow = 4, bottom = "Days", left = "Number of People")


combined <- arrangeGrob(do.call(arrangeGrob, gl),
                        legend,
                        ncol = 2,
                        widths = unit.c(unit(1, "npc") - lwidth, lwidth))


grid.newpage()
grid.draw(combined)
