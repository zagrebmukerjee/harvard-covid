diffVizFunction <- function(controlFile, treatmentFile ){
  # controlFile <- "savedData/testControl.rds"
  # treatmentFile <- "savedData/testTreatment.rds"

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
    add_trace(y = ~bedsUsed, name = "Isolated", line = list( color = clrs1[[3]])) %>% 
    layout(yaxis = list(title = yaxisString, range = yRangeTraj), xaxis = list(title = "Day", range = xRange), legend = list(x = 0, y = 1.2, orientation = 'h'),
           font = list(size = 11), showlegend = FALSE) 
  
  trajLegends <- c("Infected", "Recovered", "Isolated")
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
  yRangePos <- c(max(-1, 2*min(
    min(diffPosData$testPositivity, na.rm = TRUE),
    min(diffPosData$trueTestPositivity, na.rm = TRUE)
  )), min(1, 2*max(
    max(diffPosData$testPositivity, na.rm = TRUE),
    max(diffPosData$trueTestPositivity, na.rm = TRUE)
  )))
  
  
  diffPositivityChart <- plot_ly(data = diffPosData, x = ~day, y = ~testPositivity, name = "Test Positivity", 
                             type = "scatter", mode = "lines", line = list(color = clrs1[[1]]))  %>%
    add_trace(y = ~trueTestPositivity, name = "True Test Positivity", line = list( color = clrs1[[2]])) %>%
    layout(yaxis = list(title = "Difference in Positivity", range = yRangePos, tickformat = '%'), xaxis = list(title = "Day", range = xRange), legend = list(x = 0, y = 1.2, orientation = 'h'),
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
  # Generate diff charts for reports 
  ################################################
  
  # yRangeTraj <- c(1.1*min(
    # min(diffOutputData$allRecovered),min(diffOutputData$infected)), 1.1*max(max(diffOutputData$allRecovered),max(diffOutputData$infected)))
  
  diffChartDataReshape <- diffOutputData %>%  left_join(diffPosData) %>% 
    select(Day = day, 
           Infected = infected,
           Recovered = allRecovered, 
           Isolated = bedsUsed, 
           `Total Positive Tests` = testPositivity , 
           `True Positive Tests` = trueTestPositivity) %>% melt(id = "Day")
  trajDataGG <- diffChartDataReshape %>%  filter(variable %in% c("Infected", "Recovered", "Isolated"))
  posDataGG <- diffChartDataReshape %>%  filter(variable %in% c("Total Positive Tests", "True Positive Tests"))
  
  
  diffTrajectoryGGPlot <- ggplot(data = trajDataGG, aes(x = Day, y = value, colour = variable)) + 
    geom_line(size = 1) +
    scale_colour_manual(values = clrs1[1:3]) + theme_bw() + ylab(yaxisString) +
    scale_x_continuous(limits = xRange, expand = c(0, 0)) + scale_y_continuous(limits = yRangeTraj, expand = c(0,0))  +
    theme(legend.position = c(0.2, 0.9), legend.title = element_blank(), legend.text = element_text(size = 6), legend.key.size = unit(.15,"inches"),
          panel.border = element_blank(),
          axis.title.y = element_text(size = 8, margin = margin(t = 0, b = 0, r = 10, l = 0)),
          axis.title.x = element_text(size = 8, margin = margin(t = 10, b = 0, r = 0, l = 0))) 
  
  
  diffPositivityGGPlot <- ggplot(data = posDataGG, aes(x = Day, y = value, colour = variable)) +
    geom_line(size = 1)  +
    scale_colour_manual(values = clrs1[1:2]) + theme_bw() + ylab("Difference in Positivity") +
    scale_x_continuous(limits = xRange, expand = c(0, 0)) + 
    scale_y_continuous(limits = yRangePos, expand = c(0,0), labels = scales::percent)  +
    theme(legend.position = c(0.25, 0.85), legend.title = element_blank(), legend.text = element_text(size = 6), legend.key.size = unit(.15,"inches"),
          panel.border = element_blank(),
          axis.title.y = element_text(size = 8, margin = margin(t = 0, b = 0, r = 10, l = 0)),
          axis.title.x = element_text(size = 8, margin = margin(t = 10, b = 0, r = 0, l = 0)))
  
  
  ################################################
  # Generate diff tables for dashboard
  ################################################
  
  controlResultsTable <- controlTables$keyResults %>%  filter(!(Name %in% c("Accurate Contact Traces", "Total Quarantine Entries")))
  treatmentResultsTable <- treatmentTables$keyResults%>%  filter(!(Name %in% c("Accurate Contact Traces", "Total Quarantine Entries")))
  
  diffResults <- treatmentResultsTable
  diffResults$Value <- treatmentResultsTable$Value - controlResultsTable$Value

  diffResults$TreatmentValue <- treatmentResultsTable$Value
  diffResults$ControlValue <- controlResultsTable$Value
  diffResults$Percent <- 100*((treatmentResultsTable$Value/controlResultsTable$Value)-1)
  
  diffResultsToShow <- diffResults 
  
  diffResultsToShow$Value <- mapply(function(s,f){sprintf(f,s)}, diffResults$Value, diffResults$formatString) 
  
  diffResultsToShow$Value[[1]] <- paste0(format(as.numeric(diffResultsToShow$Value[[1]]), big.mark=","),"$")
  diffResultsToShow <- diffResultsToShow %>%  select(Name, Value)
  
  ########################
  
  controlParamsTable <- controlTables$tableParams
  treatmentParamsTable <- treatmentTables$tableParams

  diffParams <- treatmentParamsTable
  diffParams$Value <- treatmentParamsTable$Value - controlParamsTable$Value
  

  diffParams$TreatmentValue <- treatmentParamsTable$Value
  diffParams$ControlValue <- controlParamsTable$Value
  diffParams$Percent <- 100*((treatmentParamsTable$Value/controlParamsTable$Value)-1)
  
  
  diffParamsToShow <- diffParams %>%  filter(Value != 0)
  
  diffParamsToShow$Value <- mapply(function(s,f){sprintf(f,s)}, diffParamsToShow$Value, diffParamsToShow$formatString) 
  
  
  
  diffBasicTable <- diffParamsToShow %>%  filter(paramType == "basic")  %>%  select(Name, Value)
  diffTestTable <- diffParamsToShow %>%  filter(paramType == "test") %>% select(Name, Value)
  diffPopsocTable <- diffParamsToShow %>%  filter(paramType == "popsoc") %>% select(Name, Value)
  diffExtraTable <- diffParamsToShow %>%  filter(paramType == "disease") %>% select(Name, Value)
  
  diffParamsTables <- list(
    diffBasicTable = diffBasicTable,
    diffTestTable = diffTestTable,
    diffPopsocTable = diffPopsocTable,
    diffExtraTable = diffExtraTable)
  
  
  
  ################################################
  # repackage original ctrl and treatment for reporting
  ################################################
  controlReportData <- list(
    table = controlData$table,
    paramTable = controlData$paramTable,
    reportCharts = controlData$charts
  )
  
  treatmentReportData <- list(
    table = treatmentData$table,
    paramTable = treatmentData$paramTable,
    reportCharts = treatmentData$charts
  )
  
  
  ################################################
  # Generate automatic text summary
  ################################################
  
  summaryString <- ""
  treatmentString <- ""
  controlString <- ""
  
  
  paramColumnsWithDifference <- (
    diffParamsToShow %>% filter(!(paramType %in% c("noshow")))
  )$shortName
  treatmentParamValues <- treatmentData$rawTables$tableParams %>%  filter(shortName %in% paramColumnsWithDifference)
  controlParamValues <- controlData$rawTables$tableParams %>%  filter(shortName %in% paramColumnsWithDifference)
  

  if(length(paramColumnsWithDifference) != 0){
    
    diffResultsForString <- diffResults %>% 
      filter(Name %in% c(
        "Testing Cost",
        "Total Isolation Entries",
        "Total Students Symptomatic"
      ))
    
    tmpParams <- function(r){
      
      if((r$shortName %in% (c("runSerologyTest", "pooledTests", "runContactTracing", "parties"))) && r$Value == 0){r$textAdd <- paste0("no ", r$textAdd)} 
      formattedValue <- sprintf(r$diffFormat, r$Value)
      stringToShow <- paste0(r$textBefore, ifelse(nchar(r$textBefore) > 0, " ", ""), formattedValue, ifelse(nchar(r$textAdd) > 0, " ", ""), r$textAdd)
      
    }
    
    tmpResults <- function(r){
      
      
      compareStr <- ifelse(r$Value > 0, paste0(r$compareG, " "), ifelse(r$Value < 0, paste0(r$compareL, " "), ""))
      transformedValue <- eval(parse(text = paste0("(", r$diffTransform, ")","(",r$Value,")")))
      formattedValue <- sprintf(r$formatString, transformedValue)
      displayValue <- ifelse(r$Value == 0, r$compareS, formattedValue)
      
      if(r$specialDisplay == "Percent"){
        
        percentCompare <-  ifelse(r$Value > 0, "more", ifelse(r$Value < 0, "less", ""))
        
        r$textAdd <- paste0(r$textAdd, " (", sprintf("%0.0f%%", r$Percent)," ", percentCompare, ")")
        
      }
      if(r$specialDisplay == "ShowBoth"){
        
        treatmentValue <- sprintf(r$formatString, r$TreatmentValue)
        controlValue <- sprintf(r$formatString, r$ControlValue)
        compareStrBoth <- ifelse(r$Value > 0, "increase", ifelse(r$Value < 0, "decrease", ""))
        
        
        r$textAdd <- paste0(r$textAdd," (an ", compareStrBoth, " from ", controlValue, " to ", treatmentValue, ")")
        
      }
      
      
      stringToShow <- paste0(displayValue, " ",compareStr, r$textAdd)
      
    }
    
    
    autoStringsParamsTreatment <- lapply(X = split(treatmentParamValues, seq(nrow(treatmentParamValues))), FUN = tmpParams)
    autoStringsParamsControl <- lapply(X = split(controlParamValues, seq(nrow(controlParamValues))), FUN = tmpParams)
    autoStringsResults <- lapply(X = split(diffResultsForString, seq(nrow(diffResultsForString))), FUN = tmpResults)
    
    treatmentString <- Reduce(x = autoStringsParamsTreatment, f = function(a,b){paste0(a, ", ", b)})
    treatmentString <- paste0("**Treatment**: ", treatmentString)
    treatmentString <- paste0(gsub("\\s+"," ",treatmentString), ".  (Details on Page 2)  \n")
    
    controlString <- Reduce(x = autoStringsParamsControl, f = function(a,b){paste0(a, ", ", b)})
    controlString <- paste0("**Control**: ", controlString)
    controlString <- paste0(gsub("\\s+"," ",controlString), ".  (Details on Page 3)  \n")
    
    print(treatmentString)
    print(controlString)
    
    takeAwayString <- Reduce(x = autoStringsResults, f = function(a,b){paste0(a, ", ", b)})
    takeAwayString <- paste0("**Conclusion**: Changing from Control to Treatment shows: ", takeAwayString)
    takeAwayString <- gsub("\\s+"," ",takeAwayString)

  } else {
    treatmentString <- "No Parameters Changed"
    controlString <- "No Parameters Changed"
    takeAwayString <- "**Conclusion**: No differences between scenarios"
  }
  
  
  autoTextStrings <- list(
    treatmentString = treatmentString,
    controlString = controlString,
    takeAwayString = takeAwayString
  )
  
  ################################################
  # End
  ################################################
  
  return(list(diffTrajectoryChart = diffTrajectoryChart,
              diffPositivityChart = diffPositivityChart,
              diffTrajectoryGGPlot = diffTrajectoryGGPlot,
              diffPositivityGGPlot = diffPositivityGGPlot,
              diffResultsTable = diffResultsToShow,
              diffParamsTables = diffParamsTables,
              controlReportData = controlReportData, 
              treatmentReportData = treatmentReportData,
              autoTextStrings = autoTextStrings))
  
  
}