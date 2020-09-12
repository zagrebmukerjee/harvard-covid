dashboardLogo <- shinyDashboardLogoDIY(
  
  boldText = "Campus Covid-19 Model"
  ,mainText = "by Harvard IQSS"
  ,textSize = 20
  ,badgeText = "v.1"
  ,badgeTextColor = "white"
  ,badgeTextSize = 2
  ,badgeBackColor = "#40E0D0"
  ,badgeBorderRadius = 2
  
)

dashboardTheme <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(255,255,255)"
  
  ### header
  ,logoBackColor = "rgb(23,103,124)"
  
  ,headerButtonBackColor = "rgb(255,255,255)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgb(255,255,255)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = "rgb(255,255,255)"
  
  ,sidebarShadowRadius = ""
  ,sidebarPadding = 10
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarMenuBackColor = "#E6F4F1"
  ,sidebarMenuPadding = 5
  ,sidebarMenuBorderRadius = 20
  
  ,sidebarUserTextColor = "rgb(0,0,0)"
  
  ,sidebarSearchBackColor = "rgb(40,70,115)"
  ,sidebarSearchIconColor = "rgb(0,0,0)"
  ,sidebarSearchBorderColor = "rgb(0,0,0)"
  
  ,sidebarTabTextColor = "rgb(0,0,0)"
  ,sidebarTabTextSize = 16
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0
  
  ,sidebarTabBackColorSelected = "#B4DDD5"
  ,sidebarTabTextColorSelected = "rgb(255,255,255)"
  ,sidebarTabRadiusSelected = "30px"
  
  ,sidebarTabBackColorHover = "#B4DDD5"
  ,sidebarTabTextColorHover = "rgb(0,0,0)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "30px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(255,255,255)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(255,255,255)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(255,255,255)"
  ,buttonTextColor = "rgb(45,45,45)"
  ,buttonBorderColor = "rgb(150,150,150)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(255,255,255)"
  ,buttonTextColorHover = "rgb(0,0,0)"
  ,buttonBorderColorHover = "rgb(150,150,150)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(118,118,118)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(108,108,108)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(255,255,255)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

ui <- dashboardPage(
  
  dashboardHeader(
    title = dashboardLogo,
    titleWidth = 500
  ),
  
  dashboardSidebar(
  width = 350,
  br(),
  introBox(data.step = 3, data.intro = "Compute button",
           div(style="display:inline-block",
               submitButton("COMPUTE",
                            icon("play-circle")
               )
               # bsButton(inputId = "compute",
               #          label = "COMPUTE",
               #          icon = icon("play-circle"),
               #          style = "primary",
               #          size = "large")
           ),
           div(style="display:inline-block",
               bsButton(inputId = "reset", 
                        label = "RESET", 
                        icon = icon("refresh"), 
                        style = "default",
                        # size = "large"
                        )
           )
          ),
  br(),
  sidebarMenu(
    menuItem(
      "Assumptions",
      tabName = "assumptions",
      icon = icon("clipboard-list"),
      startExpanded = TRUE,
      sliderInput(
        inputId = "r0", 
        label = "R0 Reproduction Number",
        value = 2.5, 
        min = 0, 
        max = 5, 
        step = 0.5
      ),
      bsTooltip(
        "r0", 
        "R0 describes how many new infections patient zero produces over the course of their illness.",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "sens", 
        label = "Test Sensitivity",
        value = 85, 
        min = 70, 
        max = 100, 
        post = "%"
      ),
      bsTooltip(
        "sens", 
        "Test sensitivity is equal to 1 - false negative rate.",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "spec", 
        label = "Test Specificity",
        value = 95, 
        min = 90, 
        max = 100, 
        post = "%"
      ),
      bsTooltip(
        "spec", 
        "Test specificity is equal to 1 - false positive rate.",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "cad", 
        "Testing Cadence",
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
        label = "External Infections Per Day",
        value = 0.5, 
        min = 0, 
        max = 5, 
        step = 0.5
      ),
      bsTooltip(
        "comm", 
        "External infections are those coming from outside campus",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "asymp", 
        label = "Starting Asymptomatic Cases",
        value = 3, 
        min = 0, 
        max = 26
      ),
      bsTooltip(
        "asymp", 
        "The number of people arriving on campus carrying Covid without symptoms",
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
        label = "Student Population",
        value = 1500,
        min = 0,
        max = 100000
      ),
      bsTooltip(
        "comm", 
        "How many students are on campus",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "infectprob", 
        label = "Probability of Infection Given Contact",
        value = 2.5, 
        min = 0, 
        max = 10, 
        step = 0.5,
        post = "%"
      ),
      bsTooltip(
        "comm", 
        "The probability of infection given a 15-minute, unmasked, indoors contact between two people",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "days", 
        label = "Total Number of Days",
        value = 80, 
        min = 0, 
        max = 200, 
        step = 10,
        post = " days"
      ),
      bsTooltip(
        "comm", 
        "How many days to run the simulation",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "devsymp", 
        label = "Proportion Developing Symptoms",
        value = 30, 
        min = 0, 
        max = 75, 
        step = 5,
        post = "%"
      ),
      bsTooltip(
        "comm", 
        "What proportion of those infected develop some degree of symptom",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "cost", 
        label = "Cost per Test",
        value = 25, 
        min = 0, 
        max = 100, 
        step = 5,
        pre = "$"
      ),
      bsTooltip(
        "comm", 
        "Cost of administering a PCR test",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "confcost", 
        label = "Confirmatory Test Cost",
        value = 50, 
        min = 0, 
        max = 200, 
        step = 10,
        pre = "$"
      ),
      bsTooltip(
        "comm", 
        "Cost of testing administered on entering isolation",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "reldays", 
        label = "False Positive release time",
        value = 1, 
        min = 0, 
        max = 14,
        post = " days"
      ),
      bsTooltip(
        "comm", 
        "Days it takes to release a false positive from isolation",
        placement = "bottom", 
        trigger = "hover"
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
    )
    
  )),
  
  dashboardBody(
    
  tags$head(
    tags$style(
      "body{
        min-height: 611px;
        height: auto;
        max-width: 1600px;
        margin: auto;
      }"
    )
  ),
  dashboardTheme,
  plotlyOutput('plot'),
  tabsetPanel(
    id = 'dataset',
    tabPanel("Summary Results", DT::dataTableOutput("tabledata"))
  ),
  br(),
  textOutput("description")
  
))