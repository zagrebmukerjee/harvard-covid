###########################################
# LOGO
###########################################
dashboardLogo <- shinyDashboardLogoDIY(
  
  boldText = "CovidU"
  ,mainText = "by Harvard IQSS"
  ,textSize = 20
  ,badgeText = "v.2"
  ,badgeTextColor = "white"
  ,badgeTextSize = 2
  ,badgeBackColor = "#aaaaaa"
  ,badgeBorderRadius = 2
  
)
###########################################
# THEME
###########################################

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
  ,logoBackColor = "#A51C30"
  
  # ,headerButtonBackColor = "rgb(255,255,255)"
  # ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColor = "#A51C30"
  ,headerButtonIconColor = "rgb(210,210,210)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  # ,headerBackColor = "rgb(255,255,255)"
  ,headerBackColor = "#A51C30"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = "rgb(255,255,255)"
  
  ,sidebarShadowRadius = ""
  ,sidebarPadding = 10
  ,sidebarShadowColor = "0px 0px 0px"
  
  ,sidebarMenuBackColor = "#E8E8E8"
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
  
  ,sidebarTabBackColorSelected = "#D3D3D3"
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "30px"
  
  ,sidebarTabBackColorHover = "#D3D3D3"
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

###########################################
# UI
###########################################


ui <- dashboardPage(
  title="CovidU",
  
  dashboardHeader(
    title = dashboardLogo,
    titleWidth = 350,
    dropdownMenu(type = "notifications", 
                 headerText = strong("HELP"), 
                 icon = icon("question"), 
                 badgeStatus = NULL,
                 tags$li(
                   tags$ul(
                     tags$li("Use the buttons on the left to configure a scenario, and then press \"Recompute\" to display the scenario results."),
                     tags$li("After defining a scenario, you can also get a PDF report with more details on the scenario parameters and outcomes."),
                     tags$li("To examine causal effects of changes, you can define Treatment and Control scenarios, save them on the main tab, and then use the \"Causal Effects\" tab to see differences in outcomes."),
                     tags$li("On the Causal Effects tab, you can download a PDF report with details on the treatment scenario, the control scenario, and differences between them.")
                   )
                 )
    ),
    tags$li(
      a(
        strong("ABOUT CovidU"),
        height = 40,
        href = "https://github.com/zagrebmukerjee/harvard-covid/blob/master/README.md",
        title = ""
      ),
      class = "dropdown"
    )
  ),
  ###########################################
  # Sidebar
  ###########################################
  
  dashboardSidebar(
  width = 350,
  br(),
  introBox(data.step = 3, data.intro = "Compute button",
           div(style="display:inline-block",
               actionButton(inputId = "recomputeButton",
                            label = "Recompute",
                            icon("play-circle")
               ),
               bsTooltip(
                 "recomputeButton", 
                 "After defining scenario parameters, press here to compute and display results",
                 placement = "bottom", 
                 trigger = "hover"
               )
               
           ),
           div(style="display:inline-block",
               downloadButton(
                 outputId = "downloadData",
                 label = "Get Detailed Report",
                 icon = icon("download"),
                 style = "color: black; margin-left: 10px;"
               ),
               bsTooltip(
                 "downloadData", 
                 "After defining a scenario, press here to get a detailed PDF report of outcomes  - it will take a few seconds",
                 placement = "bottom", 
                 trigger = "hover"
               )
           )
  ),
  br(),
  sidebarMenu( id = "sidebar",
    menuItem(
      "Basic Assumptions",
      tabName = "assumptions",
      icon = icon("chart-area"),
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
        "How many new infections patient zero produces over the course of their illness",
        placement = "bottom", 
        trigger = "hover"
      ),
     sliderInput(
        inputId = "extInf", 
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
        max = 30
      ),
      bsTooltip(
        "asymp", 
        "The number of people arriving on campus carrying Covid without symptoms",
        placement = "bottom", 
        trigger = "hover"
      ),
     sliderInput(
       inputId = "ssDate", 
       label = "Superspreader Event Date",
       value = 0, 
       min = 0, 
       max = 80
     ),
     bsTooltip(
       "ssDate", 
       "Date on which a superspreader event occurs (0 disables)",
       placement = "bottom", 
       trigger = "hover"
     ),
     sliderInput(
       inputId = "ssSize", 
       label = "Superspreader Event Size",
       value = 0, 
       min = 0, 
       max = 500
     ),
     bsTooltip(
       "ssSize", 
       "Size of a superspreader event if one occurs (0 disables). Capped at 20 percent of total population",
       placement = "bottom", 
       trigger = "hover"
     )
    ),

    menuItem(
      "Testing and  Costs",
      tabName = "costs",
      icon = icon("clipboard-list"),
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
        "Equal to 1 - false negative rate",
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
        "Equal to 1 - false positive rate",
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
        "A test performed every X days",
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
        "cost", 
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
        "confcost", 
        "Cost of testing administered on entering isolation",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "reldays", 
        label = "False Positive Release Time",
        value = 2, 
        min = 0, 
        max = 14,
        post = " days"
      ),
      bsTooltip(
        "reldays", 
        "Days it takes to release a false positive from isolation",
        placement = "bottom", 
        trigger = "hover"
      )
      
    ),
    
    menuItem(
      "Population and Social",
      tabName = "social",
      icon = icon("user-graduate"),
      numericInput(
        inputId = "pop",
        label = "Student Population",
        value = 1500,
        min = 0,
        max = 100000
      ),
      bsTooltip(
        "pop", 
        "How many students are on campus",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "podSizeInput", 
        label = "Average Size of Shared Housing",
        value = 1, 
        min = 1, 
        max = 25, 
        step = 1
      ),
      bsTooltip(
        "podSizeInput", 
        "Number of students living together on average (sharing a bathroom)",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "podInfectionProbInput", 
        label = "Probability of Shared Housing Infection",
        value = 15, 
        min = 0, 
        max = 50, 
        step = 5,
        post = "%"
      ),
      bsTooltip(
        "podInfectionProbInput", 
        "Cumulative additional probability that a sick student infects a healthy one due to sharing living space",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "partySizeInput", 
        label = "Number of Students Attending Social Gatherings per Day (0 Disables)",
        value = 0,
        min = 0,
        max = 1500
      ),
      bsTooltip(
        "partySizeInput", 
        "How many students go to social gatherings",
        placement = "bottom", 
        trigger = "hover"
      ),
      sliderInput(
        inputId = "partyContactsInput", 
        label = "Number of contacts at Social Gatherings",
        value = 20, 
        min = 0, 
        max = 50, 
        step = 1
      ),
      bsTooltip(
        "partyContactsInput", 
        "Average number of contacts made in a social gathering (1 contact = 15 minutes indoors, unmasked)",
        placement = "bottom", 
        trigger = "hover"
      )
      
    ),

    
    menuItem(
      "Disease Assumptions",
      tabName = "assumptions",
      icon = icon("gear"),
      startExpanded = FALSE,
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
        "infectprob", 
        "The probability of infection given a 15-minute, unmasked, indoors contact between two people",
        placement = "bottom", 
        trigger = "hover"
      ),
      
      sliderInput(
        inputId = "inctime", 
        label = "Days of Incubation",
        value = 4, 
        min = 2, 
        max = 8, 
        step = 1
      ),
      bsTooltip(
        "inctime", 
        "Number of days it takes from being infected to being detectable by tests",
        placement = "bottom", 
        trigger = "hover"
      ),
      
      sliderInput(
        inputId = "rectime", 
        label = "Days to Recovery",
        value = 14, 
        min = 5, 
        max = 21, 
        step = 1
      ),
      bsTooltip(
        "rectime", 
        "Number of days it takes to recover after becoming infectious",
        placement = "bottom", 
        trigger = "hover"
      ),
      
      sliderInput(
        inputId = "condmort", 
        label = "Conditional Mortality",
        value = .05, 
        min = 0, 
        max = .1, 
        step = .01,
        post = "%"
      ),
      bsTooltip(
        "condmort", 
        "Probability of death conditional on symptom development (for student population)",
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
        "devsymp", 
        "What proportion of those infected develop some degree of symptoms",
        placement = "bottom", 
        trigger = "hover"
      )
    ),
    
    
   
    menuItem(
      "Save for Causal Effect",
      tabName = "save",
      icon = icon("save"),
      actionButton(inputId = "saveControl",
                   label = "Save As Control",
                   icon("file-export")
                   
      ),
      bsTooltip(
        "saveControl", 
        "Save a Control scenario as a baseline for a causal effect estimate - make sure to recompute first",
        placement = "bottom", 
        trigger = "hover"
      ),
      actionButton(inputId = "saveTreatment",
                   label = "Save As Treatment",
                   icon("file-export")
      ),
      bsTooltip(
        "saveTreatment", 
        "Save a Treatment scenario to estimate the causal effect of a change - make sure to recompute first",
        placement = "bottom", 
        trigger = "hover"
      ),
      actionButton(inputId = "clearSaves",
                   label = "Clear Saved States",
                   icon("trash-alt")
      ),
      bsTooltip(
        "clearSaves", 
        "Delete saved Control and Treatment",
        placement = "bottom", 
        trigger = "hover"
      )
    )
    
  )),
  ###########################################
  # Body
  ###########################################
  
  
  dashboardBody(
    tags$head(
      tags$script(
        src = paste0(
          "https://cdn.jsdelivr.net/npm/js-cookie@rc/",
          "dist/js.cookie.min.js"
        )
      ),
      tags$script(src = "www/script.js")
    ),
    useShinyjs(),
    dashboardTheme,
    tabsetPanel(id = "tabs",
      tabPanel("Single Scenario", 
        fluidRow(
          box(width=15, plotlyOutput('plot') %>%  withSpinner(type = 1, color = "#505050"))) ,
        fluidRow(
          box(width=15, DT::dataTableOutput("tabledata") %>%  withSpinner(type = 1, color = "#505050"))),
        br()
      ),
      tabPanel("Causal Effect",
               br(),
               div(style="display:inline-block",
                   downloadButton(
                     outputId = "downloadComparisonData",
                     label = "Get Causal Effect Report",
                     icon = icon("download"),
                     style = "color: black; margin-left: 10px;"
                   ),
                   bsTooltip(
                     "downloadComparisonData", 
                     "After defining an experiment, press here to get a detailed PDF report on control, treatment, and causal effects - it will take a few seconds",
                     placement = "bottom", 
                     trigger = "hover"
                   )
               ),
               fluidRow(
                 box(width=15, plotlyOutput('comparisonPlot') %>%  withSpinner(type = 1, color = "#505050"))),
               fluidRow(
                 box(width=15, DT::dataTableOutput("comparisonTabledata") %>%  withSpinner(type = 1, color = "#505050")))
               
      )
    )
  )
  
  
)
