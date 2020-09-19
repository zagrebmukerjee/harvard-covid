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


server <- function(input, output){
   
   # session$allowReconnect("force") # this will stop it going grey, we hope
   
   funList <- reactive({
      campusSIRFunction(
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
         falsePositiveReturnTime = input$reldays
      )})
   
   output$plot <- renderPlotly(
      funList()$chart
   )
   
   output$tabledata <- DT::renderDataTable({
      DT::datatable(
         funList()$table,
         rownames = FALSE,
         options = list(paging = FALSE, searching = FALSE),
         class = 'order-column cell-border hover'
      )})
   
   output$downloadData <- downloadHandler(
      filename = function(){"modelReport.pdf"},
      content = function(file){
         tempReport <- file.path(tempdir(), "LiteReport.Rmd")
         file.copy("LiteReport.Rmd", tempReport, overwrite = TRUE)
         
         # Set up parameters to pass to Rmd document
         params <- list(spec = input$spec*100, table = funList()$table, ggCharts = funList()$reportCharts, paramTable = funList()$paramTable)
         
         # Knit the document, passing in the `params` list, and eval it in a
         # child of the global environment (this isolates the code in the document
         # from the code in this app).
         rmarkdown::render(tempReport, output_file = file,
                           params = params,
                           envir = new.env(parent = globalenv())
         )}
   )
   
}


shinyApp(ui = ui, server = server)