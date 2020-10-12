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

source("singleFunction.R")
source("ui.R")


server <- function(input, output, session){
   
   session.id <- reactive({ as.character(floor(runif(1)*1e20)) })
   
   session$allowReconnect("force") # this will stop it going grey, we hope
   
   # update party slider based on pop
   observeEvent(input$pop, {updateSliderInput(session, "partySizeInput", max  = input$pop)},
                ignoreNULL = FALSE)
   
   observeEvent(input$pop, {updateSliderInput(session, "ssSize", max  = input$pop/5)},
                ignoreNULL = FALSE)
   
   observeEvent(input$days, {updateSliderInput(session, "ssDate", max  = input$days)},
                ignoreNULL = FALSE)
   

   
   
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
         
         funList2 <- reactive({campusSIRFunction(
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
         params <- list(table = funList2()$table, ggCharts = funList2()$reportCharts, paramTable = funList2()$paramTable)
         
         # Knit the document, passing in the `params` list, and eval it in a
         # child of the global environment (this isolates the code in the document
         # from the code in this app).
         rmarkdown::render(tempReport, output_file = file,
                           params = params,
                           envir = new.env(parent = globalenv())
         )}
   )
   
   
   observeEvent(input$saveControl, {
      
      controlFilename <<- paste0("savedData/", session.id(), "Control.rds" )
      
      saveRDS(object = funList()$outputForDiff, file = controlFilename)
   })
   
   
   observeEvent(input$saveTreatment, {
      
      treatmentFilename <<- paste0("savedData/", session.id(), "Treatment.rds")
      
      saveRDS(object = funList()$outputForDiff, file = treatmentFilename)
      
   })
   
   observeEvent(input$clearSaves, {
      
      if(exists("controlFilename"))
         if (file.exists(controlFilename)) 
            #Delete file if it exists
            file.remove(controlFilename)    
      
      if(exists("treatmentFilename"))
         if (file.exists(treatmentFilename)) 
            #Delete file if it exists
            file.remove(treatmentFilename)    
   })
   
   
   
}


shinyApp(ui = ui, server = server)