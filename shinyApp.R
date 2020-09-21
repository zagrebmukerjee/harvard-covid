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

source("singleFunction.R")
source("ui.R")


server <- function(input, output){
  
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
  
  # v <- reactiveValues(data = NULL)
  # 
  # observeEvent(input$reset, {
  #   v$data <- NULL
  # })
  
  output$plot <- renderPlotly({
    # if (is.null(v$data)) return()
    isolate(funList()$chart)
  })

  output$tabledata <- DT::renderDataTable({
    DT::datatable(
      funList()$table,
      rownames = FALSE,
      options = list(paging = FALSE, searching = FALSE, dom = 't'),
      class = 'order-column cell-border hover')
  })
  
  output$description <- renderPrint({
    "This dashboard was made by Zagreb Mukerjee and Olivia Fu at the Harvard Institute of Quantitative Social Science. Updated September 2020."
  })

}


shinyApp(ui = ui, server = server)