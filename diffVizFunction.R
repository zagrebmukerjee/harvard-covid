diffVizFunction <- function(controlFile, treatmentFile ){
  controlFile <- "savedData/testControl.rds"
  treatmentFile <- "savedData/testTreatment.rds"
  
  controlData <- readRDS(controlFile)
  treatmentData <- readRDS(treatmentFile)
  
  controlOutput <- controlData$outputData
  treatmentOutput <- treatmentData$outputData
  
  controlParams <- controlData$testParameters
  treatmentParams <- treatmentData$testParameters
  
  controlDiags <- controlData$diagnosticNumbers
  treatmentDiags <- treatmentData$diagnosticNumbers
  
  controlTables <- controlData$rawTables
  treatmentTables <- treatmentData$rawTables
  
  diffOutputData <- treatmentOutput - controlOutput
  diffOutputData$cycle <- treatmentOutput$cycle
  diffOutputData$day <- treatmentOutput$day
  
  ################################################
  # Generate traj chart for dashboard
  ################################################
  xRange <- c(0, max(diffOutputData$day))
  yRangeTraj <- c(
    1.1*min(
      min(diffOutputData$allRecovered),
      min(diffOutputData$infected),
      min(diffOutputData$bedsUsed),
      min(controlOutput$allRecovered),
      min(controlOutput$infected),
      min(controlOutput$bedsUsed),
      min(treatmentOutput$allRecovered),
      min(treatmentOutput$infected),
      min(treatmentOutput$bedsUsed)
    ),
    1.1*max(
      max(diffOutputData$allRecovered),
      max(diffOutputData$infected),
      max(diffOutputData$bedsUsed),
      max(controlOutput$allRecovered),
      max(controlOutput$infected),
      max(controlOutput$bedsUsed),
      max(treatmentOutput$allRecovered),
      max(treatmentOutput$infected),
      max(treatmentOutput$bedsUsed)
    ))
  
  yaxisString <- "Difference in Number of People"
  
  
  diffTrajectoryChart <- plot_ly(data = diffOutputData, x = ~day, y = ~infected, name = "Infected (Symp + ASymp)", 
                             type = "scatter", mode = "lines", line = list(color = clrs1[[1]]))  %>%
    add_trace(y = ~allRecovered, name = "Recovered", line = list( color = clrs1[[2]])) %>%
    add_trace(y = ~bedsUsed, name = "Beds Used", line = list( color = clrs1[[3]])) %>% 
    layout(yaxis = list(title = yaxisString, range = yRangeTraj), xaxis = list(title = "Day", range = xRange), legend = list(x = 0, y = 1.2, orientation = 'h'),
           font = list(size = 11), showlegend = FALSE) 
  
  trajLegends <- c("Infected", "Recovered", "Beds Used")
  n <- length(trajLegends)
  y_annotation <- seq(1, 1-n*.024, length.out = n)
  
  for(i in 1:n){
    diffTrajectoryChart <- diffTrajectoryChart %>%
      add_annotations( text = trajLegends[i], font = list(color = clrs1[i], size = 11), x = .05, y = y_annotation[i],
                       xref = "paper", yref = "paper", showarrow = FALSE, xanchor = 'left')
    
  }
  
  ################################################
  # Generate positivity chart for dashboard
  ################################################
  
  
  chartDataArrC <- controlOutput %>%  arrange(cycle)
  chartDataArrT <- treatmentOutput %>%  arrange(cycle)
  
  positivityTimeSeriesC <- data.frame(
    cycle =chartDataArrC$cycle,
    day = chartDataArrC$day,
    testPositivity = c(NA, lapply(controlDiags, function(a){a$testPositivity}) %>%  unlist() ),
    trueTestPositivity = c(NA, lapply(controlDiags, function(a){a$trueTestPositivity}) %>%  unlist() )
  )
  
  positivityTimeSeriesT <- data.frame(
    cycle =chartDataArrT$cycle,
    day = chartDataArrT$day,
    testPositivity = c(NA, lapply(treatmentDiags, function(a){a$testPositivity}) %>%  unlist() ),
    trueTestPositivity = c(NA, lapply(treatmentDiags, function(a){a$trueTestPositivity}) %>%  unlist() )
  )
  
 
  diffPosData <- positivityTimeSeriesT - positivityTimeSeriesC
  diffPosData$cycle <- treatmentOutput$cycle
  diffPosData$day <- treatmentOutput$day
  
  
  xRange <- c(0, max(diffPosData$day))
  yRangePos <- c(max(-1, 2*min(diffPosData$testPositivity, na.rm = TRUE)), 
                 min(1, 2*max(diffPosData$testPositivity, na.rm = TRUE)))
  
  
  diffPositivityChart <- plot_ly(data = diffPosData, x = ~day, y = ~testPositivity, name = "Test Positivity", 
                             type = "scatter", mode = "lines", line = list(color = clrs1[[1]]))  %>%
    add_trace(y = ~trueTestPositivity, name = "True Test Positivity", line = list( color = clrs1[[2]])) %>%
    layout(yaxis = list(title = "Positivity", range = yRangePos, tickformat = '%'), xaxis = list(title = "Day", range = xRange), legend = list(x = 0, y = 1.2, orientation = 'h'),
           font = list(size = 11), showlegend = FALSE) 
  
  
  posLegends <- c("Total Positive Tests", "True Positive Tests")
  n <- length(posLegends)
  y_annotation <- seq(1, 1-n*.024, length.out = n)
  
  for(i in 1:n){
    diffPositivityChart <- diffPositivityChart %>%
      add_annotations( text = posLegends[i], font = list(color = clrs1[i], size = 11), x = .05, y = y_annotation[i],
                       xref = "paper", yref = "paper", showarrow = FALSE, xanchor = 'left')
    
  }
  
  ################################################
  # Generate diff tables for dashboard
  ################################################
  
  controlResultsTable <- controlTables$keyResults %>%  filter(!(Name %in% c("Accurate Contact Traces", "Total Quarantine Entries")))
  treatmentResultsTable <- treatmentTables$keyResults%>%  filter(!(Name %in% c("Accurate Contact Traces", "Total Quarantine Entries")))
  
  diffResults <- treatmentResultsTable
  diffResults$Value <- treatmentResultsTable$Value - controlResultsTable$Value
  # diffResults$Name <- diffResults$diffName
  # diffResults$Notes <- diffResults$diffNotes
  
  diffResults$TreatmentValue <- treatmentResultsTable$Value
  diffResults$ControlValue <- controlResultsTable$Value
  diffResults$Percent <- 100*((treatmentResultsTable$Value/controlResultsTable$Value)-1)
  
  diffResultsToShow <- diffResults
  
  diffResultsToShow$Value <- mapply(function(s,f){sprintf(f,s)}, diffResults$Value, diffResults$formatString) 
  
  diffResultsToShow$Value[[1]] <- paste0(format(as.numeric(diffResultsToShow$Value[[1]]), big.mark=","),"$")
  diffResultsToShow <- diffResultsToShow %>%  select(Name, Value)
  
  
  ################################################
  # End
  ################################################
  
  return(list(diffTrajectoryChart = diffTrajectoryChart, diffPositivityChart = diffPositivityChart, diffResultsTable = diffResultsToShow))
  
  
}