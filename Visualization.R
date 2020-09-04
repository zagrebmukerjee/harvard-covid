
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
  yRangeOcc <-c(1.1*min(chartData$bedsUsed),
                1.1*max(chartData$bedsUsed))
  
  yaxisString <- paste0("Number of People (out of ", chartParameters$timeInvariantParameters$studentPopulation, ")")
  
  ########################
  trajectoryChart <- plot_ly(data = chartData, x = ~day, y = ~infected, name = "Infected (Symp + ASymp)", 
                             type = "scatter", mode = "lines", line = list(color = clrs1[[1]]))  %>%
    add_trace(y = ~allRecovered, name = "Recovered", line = list( color = clrs1[[2]])) %>%
    add_trace(y = ~bedsUsed, name = "Beds Used", line = list( color = clrs1[[3]])) %>% 
    layout(yaxis = list(title = yaxisString, range = yRangeTraj), xaxis = list(title = "Day", range = xRange), legend = list(x = 0, y = 1.2, orientation = 'h'),
           annotations = maxInfAnnotation, font = list(size = 9)) 
  
  
  
  
  ########################
  # legends and annotations 
  ########################
  
  trajLegends <- c("Infected", "Recovered", "Beds Used")
  n <- length(trajLegends)
  y_annotation <- seq(1, 1-n*.018, length.out = n)
  
  for(i in 1:n){
    trajectoryChart <- trajectoryChart %>%
      add_annotations( text = trajLegends[i], font = list(color = clrs1[i]), x = .05, y = y_annotation[i], xref = "paper", yref = "paper", showarrow = FALSE, xanchor = 'left')
    
  }
  
  
  return(list(dashboard = trajectoryChart)) 
  
}


########################
# table
########################

tableFunction <- function(tableData, tableParameters, tableDiags){
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
  
  tableParamsRaw <- tableParamsRaw  %>%  melt() %>%  select(Name = L1, Value = value) 
  
  tableParams <- tableStrings
  tableParams$shortName <- tableParamsRaw$Name
  tableParams$Value <- tableParamsRaw$Value
  
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
  keyResultsToShow <- keyResultsToShow %>%  select(Name, Value)
  
  
}
