

vizRunner <- function(generateDiff = FALSE, stitch = FALSE, testDetails, scenariosToRun, dataLocation = "RData/", pdfLocation = "pdfs/"){
  
  # generateDiff <- TRUE
  # stitch <- TRUE
  # dataLocation <- "RData/"
  # pdfLocation <- "pdfs/"

  # diff convention = second minus first
  
  

  if(generateDiff && (nrow(scenariosToRun) != 2)) {stop("Diffs currently only supported for 2 tests.")}
    
  allOutputs <- list()
  allPDFs <- list()
  allTests <- list()
  
  for(scenarioNumber in 1:nrow(scenariosToRun)){    
    testName <- scenariosToRun[scenarioNumber,]$TestName
    longName <- scenariosToRun[scenarioNumber,]$LongName
    
    load(paste0(dataLocation, testName, "Data.Rdata"))
    allTests[[scenarioNumber]] <- scenariosToRun[scenarioNumber,]
    allOutputs[[scenarioNumber]] <- outputs
    allPDFs[[scenarioNumber]] <- paste0(pdfLocation,longName,".pdf")
    
    testParameters <- outputs$testParameters
    outputData <- outputs$outputData
    charts <- outputs$charts
    tables <- outputs$tables
    
    
    diffViz <- FALSE
    
    
    render("Visualization.Rmd", output_file =  paste0(pdfLocation,longName,".pdf"), quiet = TRUE)

  }
  
  if(generateDiff){
    
    ## TODO: eliminate code reuse
    
    tables <- list()
    charts <- list()
    
    longName <- testDetails$DiffName

    treatmentResultsTable <- allOutputs[[2]]$tables$keyResults
    controlResultsTable <- allOutputs[[1]]$tables$keyResults
    
    treatmentParamsTable <- allOutputs[[2]]$tables$tableParams
    controlParamsTable <- allOutputs[[1]]$tables$tableParams
    
    
    keyResultsTmp <- treatmentResultsTable
    keyResultsTmp$Value <- treatmentResultsTable$Value - controlResultsTable$Value
    keyResultsTmp$Name <- keyResultsTmp$diffName
    keyResultsTmp$Notes <- keyResultsTmp$diffNotes

    keyResultsTmp$TreatmentValue <- treatmentResultsTable$Value
    keyResultsTmp$ControlValue <- controlResultsTable$Value
    
    
    keyResultsTmp$Percent <- 100*((treatmentResultsTable$Value/controlResultsTable$Value)-1)
    
    tables$keyResults <- keyResultsTmp
    
    tables$tableParams <- treatmentParamsTable
    tables$tableParams$Value <- treatmentParamsTable$Value - controlParamsTable$Value
    
    testOutput <- allOutputs[[2]]$outputData
    controlOutput <- allOutputs[[1]]$outputData
    diffOutputData <- testOutput - controlOutput
    diffOutputData$cycle <- testOutput$cycle
    diffOutputData$day <- testOutput$day
    
    xRange <- c(0, max(diffOutputData$cycle/testParameters$mechanicsParameters$cyclesPerDay))
    yRangeTraj <- c(
      1.1*min(min(testOutput$allRecovered),min(testOutput$infected),
        min(diffOutputData$allRecovered),min(diffOutputData$infected),0),
      1.1*max(max(testOutput$allRecovered),max(testOutput$infected),
              max(diffOutputData$allRecovered),max(diffOutputData$infected)))
    yRangeOcc <-c(1.1*min(min(testOutput$bedsUsed), min(diffOutputData$bedsUsed)),
                  1.1*max(max(testOutput$bedsUsed), max(diffOutputData$bedsUsed)))
    
    trajectoryChart <- plot_ly(data = diffOutputData, x = ~day, y = ~infected, name = "Infected (Symp + ASymp)", 
                               type = "scatter", mode = "lines", line = list(color = clrs1[[1]]))  %>%
      add_trace(y = ~allRecovered, name = "Recovered", line = list( color = clrs1[[2]])) %>% 
      layout(yaxis = list(title = "Number of People", range = yRangeTraj), xaxis = list(title = "Day", range = xRange), legend = list(x = 0, y = 1.2, orientation = 'h'),
             font = list(size = 9)) 
    
    
    
    
    occupancyChart <- plot_ly(data = diffOutputData, x = ~day, y = ~truePositives, name = "True Positives (Asymptomatic)",
                              type = "scatter", mode = "lines", line = list(color = clrs2[[1]]))  %>%
      add_trace(y = ~symptomatic, name = "True Positives (Symptomatic)", line = list(color = clrs2[[2]])) %>%
      add_trace(y = ~falsePositives, name = "False Positives", line = list(color = clrs2[[3]])) %>% 
      add_trace(y = ~immuneFPs, name = "False Positives (Recovered)", line = list(color = clrs2[[4]])) %>% 
      layout(yaxis = list(title = "Number of Beds", range = yRangeOcc), xaxis = list(title = "Day", range = xRange), legend = list(x =0, y = 1.2, orientation = 'h'),
             font = list(size = 9))
    
    ########################
    # legends
    ########################
    
    trajLegends <- c("Infected", "Recovered")
    occLegends <- c("True Positive (Asymp)","True Positive (Symp)","False Positive",  "False Positive (Immune)" )
    n <- length(trajLegends)
    n2 <- length(occLegends)
    y_annotation <- seq(1, 0.9, length.out = n)
    y_annotation2 <- seq(0.87,1, length.out = n2)
    
    for(i in 1:n){
      trajectoryChart <- trajectoryChart %>%
        add_annotations( text = trajLegends[i], font = list(color = clrs1[i]), x = .05, y = y_annotation[i], xref = "paper", yref = "paper", showarrow = FALSE, xanchor = 'left')
      
    }
    
    for(i in 1:n2){
      
      occupancyChart <- occupancyChart %>%
        add_annotations( text = occLegends[i], font = list(color = clrs2[i]), x = .7, y = y_annotation2[i], xref = "paper", yref = "paper", showarrow = FALSE, xanchor = 'left')
      
    }
    
    charts$trajectoryChart <- trajectoryChart
    charts$occupancyChart <- occupancyChart
    
    diffViz <- TRUE
    
    render("Visualization.Rmd", output_file = paste0(pdfLocation,"Comparison.pdf"), quiet = TRUE)
    
    allPDFs <- list(
      allPDFs[[1]],
      allPDFs[[2]],
      paste0(pdfLocation,"Comparison.pdf")
    )
  }
  
  if(stitch){pdf_combine(rev(allPDFs), output = paste0(pdfLocation,testDetails$stitchFileName,".pdf"))}
  
  
}


lightPDFgenerator <- function(outputs){
  
  
  testName <- "Placeholder text shortName"
  longName <- "Placeholder text longName"
  
  testParameters <- outputs$testParameters
  outputData <- outputs$outputData
  charts <- outputs$charts
  tables <- outputs$tables
  
  
  diffViz <- FALSE
  generateDiff <- FALSE
  
  
  render("Visualization.Rmd", output_file =  "modelReport.pdf", quiet = TRUE)
  
  return(NULL)
}

