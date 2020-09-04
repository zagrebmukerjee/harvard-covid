ui <- dashboardPage(
  
  # tags$head(
  #   tags$style("* { font-family: BentonSans Book; }")
  # ),
  
  dashboardHeader(
    title = "Harvard Population COVID-19 Model",
    titleWidth = 350
  ),
  
  dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem(
      "Assumptions",
      tabName = "assumptions",
      startExpanded = TRUE,
      sliderInput(
        inputId = "r0", 
        label = "R0 reproduction number",
        value = 2.5, 
        min = 0, 
        max = 5, 
        step = 0.5
      ),
      bsTooltip(
        "r0", 
        "R0 describes how many new infections patient zero produces over the course of illness.",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "sens", 
        label = "Test sensitivity",
        value = 85, 
        min = 70, 
        max = 100, 
        post = "%"
      ),
      bsTooltip(
        "sens", 
        "Test sensitivity is equal to 1 - false positive rate.",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "spec", 
        label = "Test specificity",
        value = 95, 
        min = 90, 
        max = 100, 
        post = "%"
      ),
      bsTooltip(
        "spec", 
        "Test specificity is equal to 1 - false negative rate.",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "cad", 
        "Testing cadence",
        value = 2, 
        min = 1, 
        max = 7, 
        post = " days"
      ),
      bsTooltip(
        "cad", 
        "This shows a test is performed every X days.",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "comm", 
        label = "External infections per day",
        value = 0.5, 
        min = 0, 
        max = 5, 
        step = 0.5
      ),
      bsTooltip(
        "comm", 
        "External infections are those coming from outside Harvard.",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "asymp", 
        label = "Starting asymptomatic cases",
        value = 3, 
        min = 0, 
        max = 26
      ),
      bsTooltip(
        "asymp", 
        "This is the number of people arriving on campus carrying Covid without symptoms.",
        placement = "bottom", 
        trigger = "hover"
      )
    ),
    
    menuItem(
      "Advanced",
      tabName = "advanced",
      icon = icon("cog"),
      numericInput(
        inputId = "pop",
        label = "Student population",
        value = 1500,
        min = 0,
        max = 100000
      ),
      sliderInput(
        inputId = "infectprob", 
        label = "Probability of infection given contact",
        value = 2.5, 
        min = 0, 
        max = 10, 
        step = 0.5,
        post = "%"
      ),
      sliderInput(
        inputId = "days", 
        label = "Total number of days",
        value = 80, 
        min = 0, 
        max = 200, 
        step = 10,
        post = " days"
      ),
      sliderInput(
        inputId = "devsymp", 
        label = "Proportion developing symptoms",
        value = 30, 
        min = 0, 
        max = 75, 
        step = 5,
        post = "%"
      ),
      sliderInput(
        inputId = "cost", 
        label = "Testing cost",
        value = 25, 
        min = 0, 
        max = 100, 
        step = 5,
        pre = "$"
      ),
      sliderInput(
        inputId = "confcost", 
        label = "Confirmatory test cost",
        value = 50, 
        min = 0, 
        max = 200, 
        step = 10,
        pre = "$"
      ),
      sliderInput(
        inputId = "reldays", 
        label = "Days to release false positive from quarantine",
        value = 1, 
        min = 0, 
        max = 14,
        post = " days"
      )
    ),
    
    menuItem(
      "Download Selection",
      tabName = "download",
      icon = icon("download"),
      textInput(
        inputId = "filename",
        placeholder = "Name download file",
        label = ""
      ),
      downloadButton(
        outputId = "downloadData",
        label = "Save Data",
        icon = icon("download"),
        style = "color: black; margin-left: 15px; margin-bottom: 5px;"
      )
    ),
    
    br(),
    submitButton(
      text = "Compute",
      icon = icon("play-circle")
    )
    
  )),
  
  dashboardBody(
    
    introjsUI(),
    
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    
    # tags$head(tags$style(HTML('
    #   .main-header .logo {
    #     font-family: BentonSans Book;
    #     font-weight: bold;
    #     font-size: 24px;
    #   }
    # '))),
    
    plotlyOutput('plot'),
    br(),
    tabsetPanel(
      id = 'dataset',
      tabPanel("Summary Results", DT::dataTableOutput("tabledata"))
    )
  
))