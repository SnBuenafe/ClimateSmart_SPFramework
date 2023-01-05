
server <- function(input, output) {
  solutionPath <- solPath
  fileList <- list.files(solutionPath)#### Comparison Plots ####
  plot1 <- reactive({
    pattern1 <- c(input$scenario1, input$model1, input$metric1, input$approach1)
    solution1 <- apply(outer(fileList, pattern1, str_detect), 1, all) %>% 
      as.numeric()
    x <- which(solution1 == 1)
    plot1 <- readRDS(paste0(solutionPath, fileList[x])) %>% 
      dplyr::select(solution_1) %>% 
      mutate(solution_1 = as.logical(solution_1))
  }) %>% bindEvent(input$create)
  
  output$Plot1 <- renderPlot({
    fSpatPlan_PlotSolution(plot1(), PUs, land) + theme(legend.position = "none")
  })
  
  plot2 <- reactive({
    pattern2 <- c(input$scenario2, input$model2, input$metric2, input$approach2)
    solution2 <- apply(outer(fileList, pattern2, str_detect), 1, all) %>% 
      as.numeric()
    x <- which(solution2 == 1)
    plot2 <- readRDS(paste0(solutionPath, fileList[x])) %>% 
      dplyr::select(solution_1) %>% 
      mutate(solution_1 = as.logical(solution_1))
  }) %>% bindEvent(input$create)
  
  output$Plot2 <- renderPlot({
    fSpatPlan_PlotSolution(plot2(), PUs, land) + theme(legend.position = "none")
  })
  
  data <- reactive({
    pattern1 <- c(input$scenario1, input$model1, input$metric1, input$approach1)
    #pattern1 <- c("126", "CMCC", "tos", "Percentile")
    solution1 <- apply(outer(fileList, pattern1, str_detect), 1, all) %>% 
      as.numeric()
    x <- which(solution1 == 1)
    plot1 <- readRDS(paste0(solutionPath, fileList[x])) %>% dplyr::select(solution_1)
    
    pattern2 <- c(input$scenario2, input$model2, input$metric2, input$approach2)
    #pattern2 <- c("585", "EM", "tos", "Percentile")
    solution2 <- apply(outer(fileList, pattern2, str_detect), 1, all) %>% 
      as.numeric()
    x <- which(solution2 == 1)
    plot2 <- readRDS(paste0(solutionPath, fileList[x])) %>% as_tibble() %>% dplyr::select(solution_1) %>% rename(solution_2 = solution_1)
    
    df <- bind_cols(plot1, plot2) %>% 
      mutate(Combined = solution_1 + solution_2) %>%
      mutate(Compare = case_when(Combined == 2 ~ "Same",
                                 solution_1 == 1 & solution_2 == 0 ~ "Removed (-)",
                                 solution_1 == 0 & solution_2 == 1 ~ "Added (+)"),
             Compare = factor(Compare, levels = c("Added (+)", "Same", "Removed (-)"))) %>%
      drop_na()
  }) %>% 
    bindEvent(input$create)
  
  output$ComparisonPlot <- renderPlot({
    #req(data())
    fSpatPlan_PlotComparison(data(), land)
    #ggplot(data()) + geom_sf(aes(fill = as.factor(Compare)), color = NA, size = 0.01
  })
  
  # Getting summary statistics
  output$areaPlot1 <- renderPrint({
    compute_summary(plot1() %>% mutate(as.numeric(solution_1)))$percent_area
  })
  output$areaPlot2 <- renderPrint({
    compute_summary(plot2() %>% mutate(as.numeric(solution_1)))$percent_area
  })
  cohen <- reactive({
    planList <- list(plot1() %>% st_drop_geometry() %>% dplyr::rename(plot1 = solution_1), plot2() %>% st_drop_geometry() %>% dplyr::rename(plot2 = solution_1)) %>% 
      create_corrmatrix()
    
    round(as.numeric(planList[2,2]), digits = 2)
  })
  
  output$Matrix <- renderPrint({
    cohen()
  })
  
  output$CategoryMatrix <- renderPrint({
    if(cohen() <= 0) {
      category = "none"
    } else if(cohen() > 0 && cohen() <= 0.2) {
      category = "slight"
    } else if(cohen() > 0.2 && cohen() <= 0.4) {
      category = "fair"
    } else if(cohen() > 0.4 && cohen() <= 0.6) {
      category = "moderate"
    } else if(cohen() > 0.6 && cohen() <= 0.8) {
      category = "substantial"
    } else {
      category = "almost perfect"
    }
    category
  })
  
  
  #### Individual plots ####
  dataIndividual <- reactive({
    pattern3 <- c(input$scenario3, input$model3, input$metric3, input$approach3)
    
    solution3 <- apply(outer(fileList, pattern3, str_detect), 1, all) %>% 
      as.numeric()
    x <- which(solution3 == 1)
    plot3 <- readRDS(paste0(solutionPath, fileList[x])) %>% dplyr::select(solution_1, transformed) %>%  mutate(solution_1 = as.logical(solution_1))
    }) %>% 
    bindEvent(input$create2)
  
  #plotting
  output$IndividualClimPlot <- renderPlot({
    create_climKernelDensityPlot(dataIndividual())
    #browser()
    #fSpatPlan_PlotSolution(dataIndividual(), PUs, land)
    
  })
  
  
  output$IndividualPlot <- renderPlot({
    
    fSpatPlan_PlotSolution(dataIndividual(), PUs, land)
    
  })
}

