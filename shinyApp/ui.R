
#### Shiny ####
header <- dashboardHeader(title = "Climate-smart Spatial Planning", titleWidth = 310)

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
           and allows to explore the proposed climate-smart framework applied to the Western Pacific. 
           Here we show how choosing different options certain climate-smart aspects affect the resulting climate-smart spatial plan."), 
  tags$br(),
  tags$div("Briefly, despite the impacts of climate change on biodiversity, it has yet to be fully incorporated into area-based management 
           tools used for biodiversity conservation, which is partly due to the lack of consensus on an appropriate methodology."),
  tags$br(),
  tags$div("To bridge this gap, we propose a climate-smart framework that prioritizes the protection of areas of low climate exposure and 
           high biodiversity retention (i.e., climate refugia), which are identified using climate metrics. The framework guides through the 
           selection of climate data, a subsequent calculation of climate metrics and ultimately how these can be used to identify refugia 
           while giving recommendations for best practices at each step (Figure 1). Particularly, we explored four key climate-smart aspects:"),
  tags$br(),
  tags$div(
    #tags$ul(
    tags$ol(
      tags$li(tags$b("Climate models")), tags$div("There are dozens of global climate models available, 
                                                    each with somewhat different underlying physical, chemical and biological processes representing
                                                    the carbon cycle. In our case study, the Western Pacific, 
                                                    we used five climate models (CanESM5, CMCC-ESM2, GFDL-ESM4, IPSL-CM6A-LR, NorESM2-MM)
                                                    as well as the mean of the five models (ensemble mean). 
                                                    To allow the reader to explore the differences in the models, 
                                                    we kept the models individually in this app.", tags$br(),
                                                  tags$b("Recommendation: Incorporate model outputs from an ensemble of models.")),
      tags$br(),
      tags$li(tags$b("Emission scenarios")), tags$div("For future projections, climate models are forced using different emission scenarios. 
                                                        These range from scenarios assuming low emissions of greenhouse gases (SSP1-2.6) 
                                                        to high emission scenarios (e.g. SSP5-8.5).", tags$br(),
                                                      tags$b("Recommendation: Use a range of emission scenarios to incorporate the uncertainty in environmental futures.")),
      tags$br(),
      tags$li(tags$b("Climate metrics")), tags$div("There is a suite of variables available as outputs from climate models 
                                                     that impact the abundance and distribution of biodiversity. 
                                                     These model outputs can be used to calculate climate metrics. 
                                                     These range from metrics of chronic exposure (e.g. rate of change of climate warming, 
                                                     ocean acidification and deoxygenation), to acute exposure (e.g. heatwaves), 
                                                     and biodiversity retention (e.g. climate velocity). 
                                                     For a detailed description of the metrics, see main manuscript.", tags$br(),
                                                   tags$b("Recommendation: Carefully choose climate metrics based on the objectives of the conservation exercise.")),
      tags$br(),
      tags$li(tags$b("Approaches to identifying climate refugia")), tags$div("There are several ways of identifying climate refugia, 
                                                                               creating climate layers, and including them in a spatial prioritization. 
                                                                               Each approach has a trade-off between climate-smart performance and cost 
                                                                               (i.e., selected area for protection). 
                                                                               Ranked in descending climate-smart performance and cost are the following approaches 
                                                                               explored in the manuscript: 1) “percentile” approach; 2) “climate-priority-area” approach; 
                                                                               3) “feature” approach; and 4) “penalty” approach. For a detailed description of the approaches, see main manuscript.", tags$br(),
                                                                             tags$b("Recommendation: Carefully choose the approach for identifying climate refugia based on how important climate change is in the prioritization.")),
    )
    #)
  ),
  tags$br(),
  div(img(src = "Workflow-03.png", height = 500, width = 400),style="text-align: center;", tags$figcaption(tags$em("Figure 1. Climate-smart marine spatial planning framework"))),
  tags$br(),
  tags$div("All four aspects of climate-smart spatial planning considered led to different spatial plans. The choice of climate metrics and the approach to identifying refugia result in large differences in climate-smart spatial plans, 
           whereas the choice of emission scenarios and climate models have a smaller effect"),
  tags$br(),
  tags$div("The following parts of this app give the reader the chance to explore the individual climate-smart aspects themselves by creating spatial plans depending on chosen inputs (Plot section). 
           Additionally, to facilitate a more direct comparison of the aspects, we included an option to run two prioritsations with different settings at the same time (Comparison section).")
)


body_plot <- fluidPage(
  #fluidRow("We show plots individually depending on the inputs"),
  fluidRow(),
  fluidRow(box(title = "Input", width = 3, background = "light-blue",
               selectInput(inputId = "scenario3", label = NULL,
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
           box(title = "Climate-smart spatial plan",
               width = 9, solidHeader = TRUE, status = "primary",
               actionButton("create2", "Create Plot"),
               shinycssloaders::withSpinner(plotOutput("IndividualPlot"))))
  
)

body_compare <- fluidPage(
  fluidRow(
    box(title = "Input 1", 
        width = 3, background = "light-blue",
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
        width = 3, background = "light-blue",
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
    box(title = "Summary Statistics", width = 4,  status = "primary", solidHeader = TRUE, collapsible = TRUE,
        "Area for Plot 1 (% planning region):", verbatimTextOutput("areaPlot1"), br(),
        "Area for Plot 2 (% planning region):", verbatimTextOutput("areaPlot2"), br(),
        "Cohen's Kappa: ", verbatimTextOutput("Matrix"), br(),
        "Degree of Agreement: ", verbatimTextOutput("CategoryMatrix")
    )
  ),
  fluidRow(
    box(title = "Comparison of Input 1 & 2",
        width = 12, solidHeader = TRUE, status = "primary",
        actionButton("create", "Create Plot"),
        shinycssloaders::withSpinner(plotOutput("ComparisonPlot")))
  ),
  fluidRow(
    box(title = "Spatial Plan (Input 1)", width = 6, status = "primary", solidHeader = TRUE,
        shinycssloaders::withSpinner(plotOutput("Plot1"))),
    box(title = "Spatial Plan (Input 2)", width = 6,  status = "primary", solidHeader = TRUE,
        shinycssloaders::withSpinner(plotOutput("Plot2")))
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


ui <- dashboardPage(header,
                    sidebar,
                    body)
