library(shiny)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(dashboardthemes)
library(rintrojs)
library(plotly)
library(dplyr)
library(DT)
library(gridExtra)
library(knitr)
library(ggplot2)
library(kableExtra)
library(reshape2)
library(rmarkdown)
library(pdftools)
library(tictoc)

source("singleFunction.R")
source("diffVizFunction.R")
source("ui.R")



# load model functions
source("contactMatrixTransmission.R")
source("contactTracing.R")
source("Model.R")

# load parameter setup functions
source("ParameterSetup.R")
source("vizFunction.R")
source("Visualization.R")


server <- function(input, output, session){
   
   
   session.id <- reactive({ as.character(floor(runif(1)*1e20)) })
   
   session$allowReconnect("force") # this will stop it going grey, we hope
   
   fileNameList <- list(ctrl = "", trt = "")
   
   pdfList <- list(ctrl = "", trt = "", csl = "")
   
   ###########################################
   # General stuff
   ###########################################
   
   observeEvent("", {
      showModal(modalDialog(
         includeHTML("introPopup.html"),
         easyClose = TRUE
      ))
   })

   onStop(function(){
      if (file.exists(fileNameList$ctrl)){
         file.remove(fileNameList$ctrl)}
      
      if (file.exists(fileNameList$trt)) {
         file.remove(fileNameList$trt)}
      
      if (file.exists(pdfList$csl)){
         
         file.remove(pdfList$csl)}
      
      if (file.exists(pdfList$ctrl)){
         file.remove(pdfList$ctrl)}
      
      if (file.exists(pdfList$trt)){
         file.remove(pdfList$trt)}
      
   })
   
   
   
   ###########################################
   # Scenario Tab
   ###########################################
   
   # update party slider based on pop
   observeEvent(input$pop, {updateSliderInput(session, "partySizeInput", max  = input$pop)},
                ignoreNULL = FALSE)
   
   observeEvent(input$pop, {updateSliderInput(session, "ssSize", max  = input$pop/5)},
                ignoreNULL = FALSE)
   
   observeEvent(input$days, {updateSliderInput(session, "ssDate", max  = input$days)},
                ignoreNULL = FALSE)
   

   
   # show sidebar if hidden
   observeEvent(input$tabs, {
      
      if(input$tabs  == "Single Scenario") {
         removeClass(selector = "body", class = "sidebar-collapse")
      }
      
   })
   
   funList <- eventReactive(eventExpr = input$recomputeButton, 
                            valueExpr = {campusSIRFunction(
                                  r0 = input$r0,
                                  testPCRSpecificity = input$spec,
                                  testPCRSensitivity = input$sens,
                                  testingTime = input$cad,
                                  commInf = input$comm,
                                  startingAsymptomatics = input$asymp,
                                  studentPopulation = input$pop,
                                  conditionalInfectionProb = input$infectprob,
                                  totalDays = input$days,
                                  symptomDevelopmentProportion = input$devsymp,
                                  testingCost = input$cost,
                                  testConfCost = input$confcost,
                                  falsePositiveReturnTime = input$reldays,
                                  podSizeInput = input$podSizeInput,
                                  podInfectionProbInput = input$podInfectionProbInput,
                                  partyRateInput = input$partyRateInput,
                                  partySizeInput = input$partySizeInput,
                                  partyContactsInput = input$partyContactsInput,
                                  ssDateInput = input$ssDate,
                                  ssSizeInput = input$ssSize)},
                            ignoreNULL = FALSE)
   
   output$plot <- renderPlotly(
      subplot(funList()$chart, funList()$positivityChart, titleX = TRUE, titleY = TRUE, margin = .05) %>% 
         layout(showlegend = FALSE, title = "Disease Trajectory & Positivity Rate") #%>%
         # layout(height = 400, width = 800)
   )
   
   
   output$tabledata <- DT::renderDataTable({
      DT::datatable(
         funList()$table,
         rownames = FALSE,
         colnames = c("", ""),
         options = list(paging = FALSE, searching = FALSE, dom = "t"),
         class = 'order-column cell-border hover',
      )})
   
   output$downloadData <- downloadHandler(
      filename = function(){"modelReport.pdf"},
      content = function(file){
         
         tempReport <- file.path(tempdir(), "LiteReport.Rmd")
         file.copy("LiteReport.Rmd", tempReport, overwrite = TRUE)
         
         funListDownload <- reactive({campusSIRFunction(
            r0 = input$r0,
            testPCRSpecificity = input$spec,
            testPCRSensitivity = input$sens,
            testingTime = input$cad,
            commInf = input$comm,
            startingAsymptomatics = input$asymp,
            studentPopulation = input$pop,
            conditionalInfectionProb = input$infectprob,
            totalDays = input$days,
            symptomDevelopmentProportion = input$devsymp,
            testingCost = input$cost,
            testConfCost = input$confcost,
            falsePositiveReturnTime = input$reldays,
            podSizeInput = input$podSizeInput,
            podInfectionProbInput = input$podInfectionProbInput,
            partyRateInput = input$partyRateInput,
            partySizeInput = input$partySizeInput,
            partyContactsInput = input$partyContactsInput,
            ssDateInput = input$ssDate,
            ssSizeInput = input$ssSize)})
         
         
         # Set up parameters to pass to Rmd document
         params <- list(table = funListDownload()$table,
                        ggCharts = funListDownload()$reportCharts,
                        paramTable = funListDownload()$paramTable)
         
         # Knit the document, passing in the `params` list, and eval it in a
         # child of the global environment (this isolates the code in the document
         # from the code in this app).
         rmarkdown::render(tempReport, output_file = file,
                           params = params,
                           envir = new.env(parent = globalenv())
         )}
   )
   
   observeEvent(input$saveControl, {
      
      if (!dir.exists("savedData/")){dir.create("savedData/")} 
      
      fileNameList$ctrl <<- paste0("savedData/", session.id(), "Control.rds" )
      
      saveRDS(object = funList()$outputForDiff, file = fileNameList$ctrl)
   })
   
   
   observeEvent(input$saveTreatment, {
      
      if (!dir.exists("savedData/")){dir.create("savedData/")} 
      
      fileNameList$trt <<- paste0("savedData/", session.id(), "Treatment.rds")
      
      saveRDS(object = funList()$outputForDiff, file = fileNameList$trt)
      
   })
   
   
   
   observeEvent(input$clearSaves, {
      # print(fileNameList$ctrl)
      # print(fileNameList$ctrl)
      if (file.exists(fileNameList$ctrl)){
         # print("A")
         file.remove(fileNameList$ctrl)}
      if (file.exists(fileNameList$trt)) {
         # print("B")
         file.remove(fileNameList$trt)} 
   })
   
   ###########################################
   # Comparison Tab
   ###########################################
   
   
   observeEvent(input$tabs, {

      if(input$tabs  == "Causal Effect") {
         addClass(selector = "body", class = "sidebar-collapse")
      }
      
   })
   
   
   causalEffectData <- eventReactive(
         eventExpr = {input$tabs  == "Causal Effect"}, 
         valueExpr = {
            validate(need(file.exists(fileNameList$ctrl) && file.exists(fileNameList$trt),
                          "Please Save Scenarios as Control and/or Treatment in the Single Scenario Tab"))
            diffVizFunction(controlFile = fileNameList$ctrl, treatmentFile = fileNameList$trt)
         }
      )
   
   output$downloadComparisonData <- downloadHandler(
      filename = function(){"fullCausalReport.pdf"},
      content = function(file){
         
         if (!dir.exists("pdfs/")){dir.create("pdfs/")} 
         
         
         controlReport <- paste0(session.id(), "Control.pdf")
         treatmentReport <- paste0(session.id(), "Treatment.pdf")
         causalReport <- paste0(session.id(), "Causal.pdf")
         
         pdfList$ctrl <<- paste0("pdfs/", controlReport)
         pdfList$trt <<- paste0("pdfs/", treatmentReport)
         pdfList$csl <<- paste0("pdfs/", causalReport)
         
         tempReport1 <- file.path(tempdir(), "LiteReport.Rmd")
         tempReport2 <- file.path(tempdir(), "LiteReport.Rmd")
         tempReport3 <- file.path(tempdir(), "DiffReport.Rmd")
         
         file.copy("LiteReport.Rmd", tempReport1, overwrite = TRUE)
         file.copy("LiteReport.Rmd", tempReport2, overwrite = TRUE)
         file.copy("DiffReport.Rmd", tempReport3, overwrite = TRUE)
         
         # Set up parameters to pass to Rmd document
         params1 <- list(table = causalEffectData()$controlReportData$table,
                         ggCharts = causalEffectData()$controlReportData$reportCharts,
                         paramTable = causalEffectData()$controlReportData$paramTable)
         
         params2 <- list(table = causalEffectData()$treatmentReportData$table,
                         ggCharts = causalEffectData()$treatmentReportData$reportCharts,
                         paramTable = causalEffectData()$treatmentReportData$paramTable)
         
         params3 <- list(diffResultsTable = causalEffectData()$diffResultsTable,
                         diffParamsTables = causalEffectData()$diffParamsTables,
                         diffTrajectoryGGPlot = causalEffectData()$diffTrajectoryGGPlot,
                         diffOccupancyGGPlot = causalEffectData()$diffOccupancyGGPlot)
         
         rmarkdown::render(tempReport1, output_file = controlReport,
                           output_dir = "pdfs/",
                           params = params1,
                           envir = new.env(parent = globalenv()))       
         
         
         rmarkdown::render(tempReport2, output_file = treatmentReport,
                           output_dir = "pdfs/",
                           params = params2,
                           envir = new.env(parent = globalenv()))       
         
         
         rmarkdown::render(tempReport3, output_file = causalReport,
                           output_dir = "pdfs/",
                           params = params3,
                           envir = new.env(parent = globalenv()))
         
         
         pdf_combine(input = list(
            pdfList$csl,
            pdfList$ctrl,
            pdfList$trt), output = file)
         }
   )
   
   
   
   output$comparisonPlot <- renderPlotly(
      subplot(causalEffectData()$diffTrajectoryChart, causalEffectData()$diffPositivityChart, titleX = TRUE, titleY = TRUE, margin = .05) %>% 
         layout(showlegend = FALSE, title = "Disease Trajectory & Positivity Rate") #%>%
      # layout(height = 400, width = 800)
   )
   
   output$comparisonTabledata <- DT::renderDataTable({
      DT::datatable(
         caption = "Changes in Outcomes:",
         causalEffectData()$diffResultsTable,
         rownames = FALSE,
         colnames = c("", ""),
         options = list(paging = FALSE, searching = FALSE, dom = "t"),
         class = 'order-column cell-border hover',
      )})
   
   
   
   
   
}


shinyApp(ui = ui, server = server)
