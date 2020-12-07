
ggPlotChartingFunction <- function(chartData, chartDiags, chartParameters, annotations = TRUE, isolations = TRUE){
  # chartData <- outputData
  # chartParameters <- testParameters
  # annotations <- TRUE
  
  
  chartDataArr <- chartData %>%  arrange(cycle)
  
  positivityTimeSeries <- data.frame(
    cycle =chartDataArr$cycle,
    day = chartDataArr$day,
    testPositivity = c(NA, lapply(chartDiags, function(a){a$testPositivity}) %>%  unlist() ),
    trueTestPositivity = c(NA, lapply(chartDiags, function(a){a$trueTestPositivity}) %>%  unlist() )
  )
  
  
  if(annotations){
    maxInf <- chartData[which.max(chartData$infected),]
    
    maxInfAnnotation <- list(
      x = maxInf$day, y = maxInf$infected, text = paste0("Max Inf.: ", sprintf("%.1f", maxInf$infected)),
      xref = "x", yref = "y", showarrow = TRUE, arrowhead = 1, ax = -40, ay = -20
    )
  }
  
  
  xRange <- c(0, max(chartData$cycle/chartParameters$mechanicsParameters$cyclesPerDay))
  yRangeTraj <- c(1.1*min(min(chartData$allRecovered),min(chartData$infected),0),
                  1.1*max(max(chartData$allRecovered),max(chartData$infected), max(chartData$bedsUsed)))
  yRangePos <- c(0, min(1, 2*max(positivityTimeSeries$testPositivity, na.rm = TRUE)))
  
  yaxisString <- paste0("Number of People (out of ", chartParameters$timeInvariantParams$studentPopulation, ")")
  
  chartDataReshape <- chartData %>%  left_join(positivityTimeSeries) %>% 
    select(Day = day, 
           Infected = infected,
           Recovered = allRecovered, 
           Isolated = bedsUsed, 
           `Total Positive Tests` = testPositivity , 
           `True Positive Tests` = trueTestPositivity) %>% melt(id = "Day")
  trajDataGG <- chartDataReshape %>%  filter(variable %in% c("Infected", "Recovered", "Isolated"))
  posDataGG <- chartDataReshape %>%  filter(variable %in% c("Total Positive Tests", "True Positive Tests"))
  
  if(!isolations){trajDataGG <- trajDataGG %>% filter(variable %in% c("Infected", "Recovered"))}
  
  trajectoryGGPlot <- ggplot(data = trajDataGG, aes(x = Day, y = value, colour = variable)) + 
    geom_line(size = 1) +
    scale_colour_manual(values = clrs1[1:3]) + theme_bw() + ylab(yaxisString) +
    scale_x_continuous(limits = xRange, expand = c(0, 0)) + scale_y_continuous(limits = yRangeTraj, expand = c(0,0))  +
    theme(legend.position = c(0.2, 0.9), legend.title = element_blank(), legend.text = element_text(size = 6), legend.key.size = unit(.15,"inches"),
          panel.border = element_blank(),
          axis.title.y = element_text(size = 8, margin = margin(t = 0, b = 0, r = 10, l = 0)),
          axis.title.x = element_text(size = 8, margin = margin(t = 10, b = 0, r = 0, l = 0))) 
  
  
  positivityGGPlot <- ggplot(data = posDataGG, aes(x = Day, y = value, colour = variable)) +
    geom_line(size = 1)  +
    scale_colour_manual(values = clrs1[1:2]) + theme_bw() + ylab("Positivity") +
    scale_x_continuous(limits = xRange, expand = c(0, 0)) +
    scale_y_continuous(limits = yRangePos, expand = c(0,0), labels = scales::percent)  +
    theme(legend.position = c(0.25, 0.85), legend.title = element_blank(), legend.text = element_text(size = 6), legend.key.size = unit(.15,"inches"),
          panel.border = element_blank(),
          axis.title.y = element_text(size = 8, margin = margin(t = 0, b = 0, r = 10, l = 0)),
          axis.title.x = element_text(size = 8, margin = margin(t = 10, b = 0, r = 0, l = 0)))
  
  if(annotations){
    
    trajectoryGGPlot <- trajectoryGGPlot + annotate(
      geom = "segment",
      xend = maxInf$day, x = maxInf$day - 10,
      yend = maxInf$infected, y = maxInf$infected - yRangeTraj[[2]]/20,
      size = .5, arrow = arrow(angle = 30, length = unit(.1, "inches"))
    ) + annotate(
      geom = "text",
      x = maxInf$day - 10, y = maxInf$infected - 1.4*yRangeTraj[[2]]/20,
      label = maxInfAnnotation$text, size = 2.5
    )
  }
 
  
  return(list(positivityGGPlot =positivityGGPlot, trajectoryGGPlot = trajectoryGGPlot))
   
}

dashboardChartingFunction <- function(chartData, chartParameters, annotations = TRUE){
 
  
  if(annotations){
    maxInf <- chartData[which.max(chartData$infected),]
    
    maxInfAnnotation <- list(
      x = maxInf$day, y = maxInf$infected, text = paste0("Max Inf.: ", sprintf("%.1f", maxInf$infected)),
      xref = "x", yref = "y", showarrow = TRUE, arrowhead = 1, ax = -40, ay = -20
    )
    
    maxQuar <- chartData[which.max(chartData$bedsUsed),]
    
    
    maxQuarAnnotation <- list(
      x = maxQuar$day, y = maxQuar$bedsUsed, text = paste0("Max Quar.: ", sprintf("%.0f", maxQuar$bedsUsed)),
      xref = "x", yref = "y", showarrow = TRUE, arrowhead = 1, ax = 50, ay = -20
    )
  }
  xRange <- c(0, max(chartData$cycle/chartParameters$mechanicsParameters$cyclesPerDay))
  yRangeTraj <- c(1.1*min(min(chartData$allRecovered),min(chartData$infected),0),
                  1.1*max(max(chartData$allRecovered),max(chartData$infected), max(chartData$bedsUsed)))
  
  yaxisString <- paste0("Number of People (out of ", chartParameters$timeInvariantParams$studentPopulation, ")")
  
  ########################
  trajectoryChart <- plot_ly(data = chartData, x = ~day, y = ~infected, name = "Infected (Symp + ASymp)", 
                             type = "scatter", mode = "lines", line = list(color = clrs1[[1]]))  %>%
    add_trace(y = ~allRecovered, name = "Recovered", line = list( color = clrs1[[2]])) %>%
    add_trace(y = ~bedsUsed, name = "Isolated", line = list( color = clrs1[[3]])) %>% 
    layout(yaxis = list(title = yaxisString, range = yRangeTraj), xaxis = list(title = "Day", range = xRange), legend = list(x = 0, y = 1.2, orientation = 'h'),
           annotations = maxInfAnnotation, font = list(size = 11), showlegend = FALSE) 
  
  
  
  
  ########################
  # legends and annotations 
  ########################
  
  trajLegends <- c("Infected", "Recovered", "Isolated")
  n <- length(trajLegends)
  y_annotation <- seq(1, 1-n*.024, length.out = n)
  
  for(i in 1:n){
    trajectoryChart <- trajectoryChart %>%
      add_annotations( text = trajLegends[i], font = list(color = clrs1[i], size = 11), x = .05, y = y_annotation[i],
                       xref = "paper", yref = "paper", showarrow = FALSE, xanchor = 'left')
    
  }
  
  
  return(list(dashboard = trajectoryChart)) 
  
}

positivityChartingFunction <- function(chartData, chartDiags, chartParameters){
  # chartData <- outputData
  # chartParameters <- testParameters
  # chartDiags <- diagnosticNumbers
  # annotations <- TRUE
  
  chartDataArr <- chartData %>%  arrange(cycle)
  
  positivityTimeSeries <- data.frame(
    cycle =chartDataArr$cycle,
    day = chartDataArr$day,
    testPositivity = c(NA, lapply(chartDiags, function(a){a$testPositivity}) %>%  unlist() ),
    trueTestPositivity = c(NA, lapply(chartDiags, function(a){a$trueTestPositivity}) %>%  unlist() )
  )
  
  xRange <- c(0, max(positivityTimeSeries$day))
  yRangePos <- c(0, min(1, 2*max(positivityTimeSeries$testPositivity, na.rm = TRUE)))
  
  
  positivityChart <- plot_ly(data = positivityTimeSeries, x = ~day, y = ~testPositivity, name = "Test Positivity", 
                             type = "scatter", mode = "lines", line = list(color = clrs1[[1]]))  %>%
    add_trace(y = ~trueTestPositivity, name = "True Test Positivity", line = list( color = clrs1[[2]])) %>%
    layout(yaxis = list(title = "Positivity", range = yRangePos, tickformat = '%'), xaxis = list(title = "Day", range = xRange), legend = list(x = 0, y = 1.2, orientation = 'h'),
           font = list(size = 11), showlegend = FALSE) 
  
  
  posLegends <- c("Total Positive Tests", "True Positive Tests")
  n <- length(posLegends)
  y_annotation <- seq(1, 1-n*.024, length.out = n)
  
  for(i in 1:n){
    positivityChart <- positivityChart %>%
      add_annotations( text = posLegends[i], font = list(color = clrs1[i], size = 11), x = .05, y = y_annotation[i],
                       xref = "paper", yref = "paper", showarrow = FALSE, xanchor = 'left')
    
  }
  
 
  return(positivityChart)
   
}


########################
# table
########################

tableFunction <- function(tableData, tableParameters, tableDiags){
  # tableData <- outputData
  # tableParameters <- testParameters
  # tableDiags <- diagnosticNumbers
  
  tableParamsRaw <- tableParameters$modelParameters
  
  
  # percents
  tableParamsRaw$symptomDevelopmentProportion <- tableParamsRaw$symptomDevelopmentProportion * 100
  tableParamsRaw$conditionalMortality <- tableParamsRaw$conditionalMortality * 100
  tableParamsRaw$conditionalInfectionProb <- tableParamsRaw$conditionalInfectionProb * 100
  tableParamsRaw$intraPodInfectionProb <- tableParamsRaw$intraPodInfectionProb * 100
  
  
  tableParamsRaw$testPCRSensitivity <- tableParamsRaw$testPCRSensitivity * 100
  tableParamsRaw$testPCRSpecificity <- tableParamsRaw$testPCRSpecificity * 100
  tableParamsRaw$testSerSensitivity <- tableParamsRaw$testSerSensitivity * 100
  tableParamsRaw$testSerSpecificity <- tableParamsRaw$testSerSpecificity * 100
  
  
  tableParamsRaw$testingTime <- tableParamsRaw$testingTime - 1
  
  
  tableStrings <- read.csv("ParamTableTextAndFormat.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  keyResultStrings <- read.csv("keyResultsTable.csv", fileEncoding="UTF-8-BOM", stringsAsFactors = FALSE)
  # TODO: the testing frequency seem to get assigned to the quarantine delay. 
  # We should try to have a dictionary of the descriptions with variable names as keys,
  # instead of hardcoding the order in the table.
  
  tableParamsMelted <- tableParamsRaw  %>%  melt() %>%  select(shortName = L1, Value = value) 
  
  tableParams <- tableStrings %>%  left_join(tableParamsMelted, by = c("shortName"))
  # tableParams$shortName <- tableParamsRaw$Name
  # tableParams$Value <- tableParamsRaw$Value
  
  totalStudentsEnteringQuarantine <-  Reduce(x =  lapply(tableDiags, function(a){a$newStudentsEnteringQuarantine}), f = sum)
  totalStudentsEnteringIsolation <-  Reduce(x =  lapply(tableDiags, function(a){a$newStudentsEnteringIsolation}), f = sum)
  totalStudentsSymptomatic <-  Reduce(x =  lapply(tableDiags, function(a){a$newStudentsSymptomatic}), f = sum)
  totalAccurateCTs <-  Reduce(x =  lapply(tableDiags, function(a){a$accurateCTs}), f = sum)
  
  keyResults <- keyResultStrings
  keyResults$Value <- as.numeric(lapply(keyResults$Value, function(s){eval(parse(text = s))}))

  return(list(tableParams =tableParams, keyResults = keyResults))
  
}


formattedResultsTableFunction <- function(krtInput){
  
  keyResultsToShow <- krtInput
  
  keyResultsToShow$Value <- mapply(function(s,f){sprintf(f,s)}, krtInput$Value, krtInput$formatString) 
  
  keyResultsToShow$Value[[1]] <- paste0("$", format(as.numeric(keyResultsToShow$Value[[1]]), big.mark=","))
  keyResultsToShow <- keyResultsToShow %>%  select(Name, Value)
  
  
}



formattedParameterTableFunction <- function(rawTableInput, testParameters){
  
  # tableParamsToShow <- tables$tableParams 
  
  tableParamsToShow <- rawTableInput
  
  if(!testParameters$modelParameters$runSerologyTest){tableParamsToShow <- tableParamsToShow %>%  filter(!(shortName %in% c("testSerSensitivity", "testSerSpecificity", "testSerCost", "runSerologyTest")))}
  if(!testParameters$modelParameters$parties){tableParamsToShow <- tableParamsToShow %>%  filter(!(shortName %in% c("partyRate", "partySize", "contactsPerParty", "parties")))}
  if(!testParameters$modelParameters$runContactTracing){
    tableParamsToShow <- tableParamsToShow %>%  filter(!(shortName %in% c("maxContactsTraced", "contactTracingAccuracy", "contactTracingDelay", "contactTracingResponseRate", "ctDoubleCountAdjustment", "ctQuarTime", "runContactTracing")))
  }
  
  if(!testParameters$modelParameters$pooledTests){tableParamsToShow <- tableParamsToShow %>%  filter(!(shortName %in% c("pooledTests")))}
  
  tableParamsToShow$Value <- mapply(function(s,f){sprintf(f,s)}, tableParamsToShow$Value, tableParamsToShow$formatString) 
  
  
  basicTable <- tableParamsToShow %>%  filter(paramType == "basic") %>%  select(Name, Value, Notes)
  testTable <- tableParamsToShow %>%  filter(paramType == "test") %>% select(Name, Value, Notes)
  popsocTable <- tableParamsToShow %>%  filter(paramType == "popsoc") %>% select(Name, Value, Notes)
  extraTable <- tableParamsToShow %>%  filter(paramType == "disease") %>% select(Name, Value, Notes)
  
  
  return(list(basicTable = basicTable, testTable = testTable, popsocTable = popsocTable, extraTable = extraTable))
  
  
}

chartingFunction <- function(chartData, chartParameters, annotations = TRUE){
  
  ########################
  # charts
  ########################
  
  if(annotations){
    maxInf <- chartData[which.max(chartData$infected),]
    
    maxInfAnnotation <- list(
      x = maxInf$day, y = maxInf$infected, text = paste0("Max Inf.: ", sprintf("%.1f", maxInf$infected)),
      xref = "x", yref = "y", showarrow = TRUE, arrowhead = 1, ax = -40, ay = -20
    )
    
    maxQuar <- chartData[which.max(chartData$bedsUsed),]
    
    
    maxQuarAnnotation <- list(
      x = maxQuar$day, y = maxQuar$bedsUsed, text = paste0("Max Quar.: ", sprintf("%.0f", maxQuar$bedsUsed)),
      xref = "x", yref = "y", showarrow = TRUE, arrowhead = 1, ax = 50, ay = -20
    )
  }
  xRange <- c(0, max(chartData$cycle/chartParameters$mechanicsParameters$cyclesPerDay))
  yRangeTraj <- c(1.1*min(min(chartData$allRecovered),min(chartData$infected),0),
                  1.1*max(max(chartData$allRecovered),max(chartData$infected)))
  yRangeOcc <-c(1.1*min(chartData$bedsUsed),
                1.1*max(chartData$bedsUsed))
  
  
  ########################
  trajectoryChart <- plot_ly(data = chartData, x = ~day, y = ~infected, name = "Infected (Symp + ASymp)", 
                             type = "scatter", mode = "lines", line = list(color = clrs1[[1]]))  %>%
    add_trace(y = ~allRecovered, name = "Recovered", line = list( color = clrs1[[2]])) %>% 
    layout(yaxis = list(title = "Number of People", range = yRangeTraj), xaxis = list(title = "Day", range = xRange), legend = list(x = 0, y = 1.2, orientation = 'h'),
           annotations = maxInfAnnotation, font = list(size = 9)) 
  
  
  
  
  occupancyChart <- plot_ly(data = chartData, x = ~day, y = ~truePositives, name = "True Positives (Asymptomatic)",
                            type = "scatter", mode = "none", stackgroup = "one", fillcolor = clrs2[[1]])  %>%
    add_trace(y = ~symptomatic, name = "True Positives (Symptomatic)", fillcolor = clrs2[[2]]) %>%
    add_trace(y = ~falsePositives, name = "False Positives", fillcolor = clrs2[[3]]) %>% 
    add_trace(y = ~immuneFPs, name = "False Positives (Recovered)", fillcolor = clrs2[[4]]) %>% 
    layout(yaxis = list(title = "Number of Beds", range = yRangeOcc), xaxis = list(title = "Day", range = xRange), legend = list(x =0, y = 1.2, orientation = 'h'),
           annotations = maxQuarAnnotation, font = list(size = 9))
  
  ########################
  # legends and annotations 
  ########################
  
  trajLegends <- c("Infected", "Recovered")
  occLegends <- c("True Positive (Asymp)","True Positive (Symp)","False Positive",  "False Positive (Immune)" )
  n <- length(trajLegends)
  n2 <- length(occLegends)
  y_annotation <- seq(1, 1-n*.017, length.out = n)
  y_annotation2 <- seq(0.87,1, length.out = n2)
  
  for(i in 1:n){
    trajectoryChart <- trajectoryChart %>%
      add_annotations( text = trajLegends[i], font = list(color = clrs1[i]), x = .05, y = y_annotation[i], xref = "paper", yref = "paper", showarrow = FALSE, xanchor = 'left')
    
  }
  
  for(i in 1:n2){
    
    occupancyChart <- occupancyChart %>%
      add_annotations( text = occLegends[i], font = list(color = clrs2[i]), x = .7, y = y_annotation2[i], xref = "paper", yref = "paper", showarrow = FALSE, xanchor = 'left')
    
  }
  
  return(list(occupancyChart =occupancyChart, trajectoryChart = trajectoryChart))
  
}
