
#### Shiny ####
header <- dashboardHeader(title = h4(HTML("A climate-smart<br/>conservation planning framework")), titleWidth = 230) #"A climate-smart conservation planning framework"

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Background", tabName = "background", icon = icon("menu-hamburger", lib = "glyphicon")),
    menuItem("Plot", tabName = "plot", icon = icon("picture", lib = "glyphicon")),
    menuItem("Compare", tabName = "compare", icon = icon("th-large", lib = "glyphicon"))
  )
)

body_intro <- fluidPage(
  h2("Spatial plan: ", style = "font-family: Arial")
)

body_background <- fluidPage(
  titlePanel("Background"),
  tags$div("This Shiny App serves as a visualisation tool for the work by Buenafe et al. (submitted) 
           and allows to explore the proposed climate-smart framework. 
           Here we show how choosing different options of certain climate-smart aspects affect the resulting climate-smart spatial plan by using the Western Pacific as a case study."), 
  tags$br(),
  tags$div("Despite the impacts of climate change on biodiversity, climate change has yet to be fully incorporated into area-based management 
           tools used for biodiversity conservation partly due to the lack of consensus on an appropriate methodology."),
  tags$br(),
  tags$div("To bridge this gap, we propose a climate-smart framework that prioritizes the protection of areas of low climate exposure and 
           high biodiversity retention (i.e., climate refugia), which are identified using climate metrics. The framework guides through the 
           selection of climate projections, the calculation of appropriate climate metrics, and the identification of climate refugia. We provide key recommendations for best practices at each step (Figure 1). Particularly, we explored four key climate-smart aspects:"),
  tags$br(),
  tags$div(
    #tags$ul(
    tags$ol(
      tags$li(tags$b("Climate models")), tags$div("There are dozens of global climate models available, 
                                                    each with somewhat different underlying physical, chemical and biological processes representing
                                                    the carbon cycle. In our case study, we used five climate models (CanESM5, CMCC-ESM2, GFDL-ESM4, IPSL-CM6A-LR, NorESM2-MM)
                                                    as well as the mean of the five models (i.e., ensemble mean). 
                                                    To allow the reader to explore the differences in the models, 
                                                    we show the resulting spatial plans individually for each model", tags$br(),
                                                  tags$b("Recommendation: Incorporate model outputs from an ensemble of models.")),
      tags$br(),
      tags$li(tags$b("Emission scenarios")), tags$div("For future projections, climate models are forced using different emission scenarios. 
                                                        These range from scenarios assuming low emissions of greenhouse gases (SSP1-2.6) 
                                                        to high emission scenarios (e.g. SSP5-8.5).", tags$br(),
                                                      tags$b("Recommendation: Use a range of emission scenarios to incorporate the uncertainty in environmental futures.")),
      tags$br(),
      tags$li(tags$b("Climate metrics")), tags$div("There is a suite of variables available from climate models 
                                                     that impact the abundance and distribution of biodiversity. 
                                                     These projections can be used to calculate climate metrics. 
                                                     These range from metrics of chronic exposure 
                                                     (e.g. rate of change of climate warming, ocean acidification,
                                                     and deoxygenation), to acute exposure 
                                                     (e.g. intensity of frequency of heatwaves), 
                                                     and biodiversity retention (e.g. climate velocity). 
                                                     For a detailed description of the metrics, see main manuscript.", tags$br(),
                                                   tags$b("Recommendation: Carefully choose climate metrics based on the objectives of the conservation exercise.")),
      tags$br(),
      tags$li(tags$b("Approaches to identifying climate refugia")), tags$div("There are several ways of identifying climate refugia, creating climate layers, and including them in a spatial prioritization. Each approach has a trade-off between climate-smart performance and cost (e.g., selected area for protection). Ranked in descending climate-smart performance and cost are the following approaches explored in the manuscript: 1) “percentile” approach; 2) “feature” approach; 3) “climate-priority-area” approach; and 4) “penalty” approach. For a detailed description of the approaches, see main manuscript.", tags$br(),
                                                                             tags$b("Recommendation: Carefully choose the approach for identifying climate refugia based on how important climate change is in the prioritization.")),
    )
    #)
  ),
  tags$br(),
  div(img(src = "Fig2-ClimateSmartFramework.png", height = 500, width = 345),style="text-align: center;", tags$figcaption(tags$em("Figure 1. Climate-smart conservation planning framework"))),
  tags$br(),
  tags$div("All four aspects of climate-smart conservation planning considered led to different spatial plans. The choice of climate metrics and the approach to identifying refugia result in large differences in climate-smart spatial plans, 
           whereas the choice of emission scenarios and climate models have a smaller effect."),
  tags$br(),
  tags$div("The following parts of this Shiny App give the reader the chance to explore the individual climate-smart aspects themselves by creating spatial plans depending on chosen inputs (Plot section). 
           Additionally, to facilitate a more direct comparison of the aspects, we included an option to run two prioritizations with different settings at the same time (Comparison section).")
)


body_plot <- fluidPage(
  #fluidRow("We show plots individually depending on the inputs"),
  fluidRow(),
  fluidRow(box(title = "Input", width = 3, background = "purple",
               selectInput(inputId = "scenario3", label = "Scenario",
                           choices = c("SSP 1-2.6" = "126", "SSP 2-4.5" = "245", "SSP 5-8.5"= "585")),
               selectInput(inputId = "model3", label = "Model",
                           choices = c("Ensemble Mean" = "EM", "CanESM5" = "CanESM5", "CMCC-ESM2" = "CMCC", 
                                       "GFDL-ESM4" = "GFDL", "IPSL-CM6A-LR" = "IPSL", "NorESM2-MM" = "NorESM2")),
               selectInput(inputId = "metric3", label = "Climate metric",
                           choices = c("Warming" = "tos", "Acidification" = "phos", "Deoxygenation" = "o2os", 
                                       "Climate velocity" = "velocity", "MHW Intensity" = "MHW", "Combined Metric" = "CombinedMetric")),
               selectInput(inputId = "approach3", label = "Approach to identifying refugia",
                           choices = c("Percentile" = "Percentile", "Climate priority area" = "ClimatePriorityArea",
                                       "Feature" = "Feature", "Penalty" = "Penalty"))),
           box(actionButton("create2", "Create Plot"),
              title = "Climate-smart spatial plan", color = "purple",
               width = 9, solidHeader = TRUE, #status = "primary",
               shinycssloaders::withSpinner(plotOutput("IndividualPlot")),
              shiny::h4("Climate-smart performance"),
               shinycssloaders::withSpinner(plotOutput("IndividualClimPlot"))
               ))
  
)

body_compare <- fluidPage(
  fluidRow(
    box(title = "Input 1", 
        width = 3, background = "purple",
        selectInput(inputId = "scenario1", label = "Scenario",
                    choices = c("SSP 1-2.6" = "126", "SSP 2-4.5" = "245", "SSP 5-8.5"= "585")),
        selectInput(inputId = "model1", label = "Model",
                    choices = c("Ensemble Mean" = "EM", "CanESM5" = "CanESM5", "CMCC-ESM2" = "CMCC", 
                                "GFDL-ESM4" = "GFDL", "IPSL-CM6A-LR" = "IPSL", "NorESM2-MM" = "NorESM2")),
        selectInput(inputId = "metric1", label = "Climate metric",
                    choices = c("Warming" = "tos", "Acidification" = "phos", "Deoxygenation" = "o2os", 
                                "Climate velocity" = "velocity", "MHW Intensity" = "MHW" , "Combined Metric" = "CombinedMetric")),
        selectInput(inputId = "approach1", label = "Approach to identifying refugia",
                    choices = c("Percentile" = "Percentile", "Climate priority area" = "ClimatePriorityArea",
                                "Feature" = "Feature", "Penalty" = "Penalty"))),
    box(title = "Input 2", 
        width = 3, background = "purple",
        selectInput(inputId = "scenario2", label = "Scenario",
                    choices = c("SSP 1-2.6" = "126", "SSP 2-4.5" = "245", "SSP 5-8.5"= "585")),
        selectInput(inputId = "model2", label = "Model",
                    choices = c("Ensemble Mean" = "EM", "CanESM5" = "CanESM5", "CMCC-ESM2" = "CMCC", 
                                "GFDL-ESM4" = "GFDL", "IPSL-CM6A-LR" = "IPSL", "NorESM2-MM" = "NorESM2")),
        selectInput(inputId = "metric2", label = "Climate metric",
                    choices = c("Warming" = "tos", "Acidification" = "phos", "Deoxygenation" = "o2os", 
                                "Climate velocity" = "velocity", "MHW Intensity" = "MHW", "Combined Metric" = "CombinedMetric")),
        selectInput(inputId = "approach2", label = "Approach to identifying refugia",
                    choices = c("Percentile" = "Percentile", "Climate priority area" = "ClimatePriorityArea",
                                "Feature" = "Feature", "Penalty" = "Penalty"))),
    box(title = "Summary Statistics", width = 4, solidHeader = TRUE, collapsible = TRUE, #status = "primary",
        "Area for Plot 1 (% planning region):", verbatimTextOutput("areaPlot1"), br(),
        "Area for Plot 2 (% planning region):", verbatimTextOutput("areaPlot2"), br(),
        "Cohen's Kappa: ", verbatimTextOutput("Matrix"), br(),
        "Degree of Agreement: ", verbatimTextOutput("CategoryMatrix")
    )
  ),
  fluidRow(
    box(title = "Comparison of Input 1 & 2",
        width = 12, solidHeader = TRUE, #status = "primary",
        actionButton("create", "Create Plot"),
        shinycssloaders::withSpinner(plotOutput("ComparisonPlot")))
  ),
  fluidRow(
    box(title = "Spatial Plan (Input 1)", width = 6, solidHeader = TRUE,#status = "primary",
        shinycssloaders::withSpinner(plotOutput("Plot1")),
        shiny::h4("Climate-smart performance"),
        shinycssloaders::withSpinner(plotOutput("ClimPlotInput1"))),
    box(title = "Spatial Plan (Input 2)", width = 6, solidHeader = TRUE, #status = "primary", 
        shinycssloaders::withSpinner(plotOutput("Plot2")),
        shiny::h4("Climate-smart performance"),
        shinycssloaders::withSpinner(plotOutput("ClimPlotInput2")))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "background",
            body_background),
    tabItem(tabName = "plot",
            body_plot),
    tabItem(tabName = "compare",
            body_compare)
  ))


ui <- dashboardPage(skin = "purple",
                    header,
                    sidebar,
                    body)
