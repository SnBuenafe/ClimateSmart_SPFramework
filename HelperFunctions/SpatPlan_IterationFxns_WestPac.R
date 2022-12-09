# title: "Helper functions (loops)"
# author: "Tin Buenafe and Sandra Neubert"

# Created loop functions for running iterations in `08_SpatPlan_WestPac_SuppRuns_Iterations.R`

loopthrough_EM_Percentile <- function(solution_list, metric_list, scenario_list) {
  
  i = 1
  for(metric_num in 1:length(metric_list)) {
    for(scenario_num in 1:length(scenario_list)) {
      
      if(scenario_list[scenario_num] == "126") {scenario_object <- "SSP 1-2.6"} else if (scenario_list[scenario_num] == "245") {scenario_object <- "SSP 2-4.5"} else if (scenario_list[scenario_num] == "585") {scenario_object <- "SSP 5-8.5"}

      LoadClimateMetrics(metric = metric_list[metric_num], model = NA, scenario = scenario_object)
      
      if(metric_list[metric_num] == "velocity") {
        metric_df <- paste0("velocity_SSP", scenario_list[scenario_num])
      } else if (str_detect(metric_list[metric_num], pattern = "MHW")) {
        metric_df <- paste0(metric_list[metric_num], "_SSP", scenario_list[scenario_num])
      } else {
        metric_df <- paste0("roc_", metric_list[metric_num], "_SSP", scenario_list[scenario_num])
      }
      
      x = get(metric_df)
      
      aqua_percentile <- create_PercentileLayer(aqua_sf, metric_name = metric_list[metric_num], colname = "transformed", x, PUs)
      
      # Get list of features
      features <- aqua_percentile %>% 
        as_tibble() %>% 
        dplyr::select(-geometry) %>% 
        names()
      
      # Set up the spatial planning problem
      out_sf <- cbind(aqua_percentile, x, UniformCost)
      p <- prioritizr::problem(out_sf, features, "cost") %>%
        add_min_set_objective() %>%
        add_relative_targets(30/35) %>%
        add_binary_decisions() %>%
        add_gurobi_solver(gap = 0, verbose = FALSE)
      
      # Solve the planning problem 
      s <- prioritizr::solve(p)
      saveRDS(s, paste0(output_solutions, solution_list[i], "-EM-Percentile-", metric_list[metric_num], "-SSP", scenario_list[scenario_num], ".rds")) # save solution
      
      # Plot the spatial design
      s_plot <- s %>% 
        mutate(solution_1 = as.logical(solution_1))
      (ggSol <- fSpatPlan_PlotSolution(s_plot, PUs, land) + 
          ggtitle(paste0("Climate-smart design: ", metric_list[metric_num]), 
                  subtitle = paste0("Percentile, SSP", scenario_list[scenario_num])) + 
          theme(axis.text = element_text(size = 25)))
      ggsave(filename = paste0("EM-Percentile-", metric_list[metric_num], "-", scenario_list[scenario_num], ".png"),
             plot = ggSol, width = 21, height = 29.7, dpi = 300,
             path = "Figures/") # save plot
      
      gc()
      rm(list = ls(pattern = metric_df))
      i = i + 1
    }
  }
}

loopthrough_EM_Feature <- function(solution_list, metric_list, scenario_list) {
  
  i = 1
  for(metric_num in 1:length(metric_list)) {
    for(scenario_num in 1:length(scenario_list)) {
      
      if(scenario_list[scenario_num] == "126") {scenario_object <- "SSP 1-2.6"} else if (scenario_list[scenario_num] == "245") {scenario_object <- "SSP 2-4.5"} else if (scenario_list[scenario_num] == "585") {scenario_object <- "SSP 5-8.5"}
      
      if(metric_list[metric_num] == "velocity") {
        metric_df <- paste0("velocity_SSP", scenario_list[scenario_num])
      } else if (str_detect(metric_list[metric_num], pattern = "MHW")) {
        metric_df <- paste0(metric_list[metric_num], "_SSP", scenario_list[scenario_num])
      } else {
        metric_df <- paste0("roc_", metric_list[metric_num], "_SSP", scenario_list[scenario_num])
      }
      
      LoadClimateMetrics(metric = metric_list[metric_num], model = NA, scenario = scenario_object)
      
      x = get(metric_df)
      
      # Prepare climate layer
      ClimateFeature <- create_FeatureLayer(metric_name = metric_list[metric_num], colname = "transformed", x)
      
      # Get list of features and targets
      features <- aqua_sf %>% 
        as_tibble() %>% 
        dplyr::select(-geometry) %>% 
        names()
      features <- append(features, "climate_layer") # add "climate_layer" to features
      targets <- features %>% as_tibble() %>% 
        setNames(., "Species") %>% 
        add_column(target = 0.3) %>% 
        mutate(target = ifelse(str_detect(Species, pattern = "climate_layer"), 30/35, 0.3))
      
      # Set up the spatial planning problem
      out_sf <- cbind(aqua_sf, ClimateFeature, UniformCost)
      p <- prioritizr::problem(out_sf, features, "cost") %>%
        add_min_set_objective() %>%
        add_relative_targets(targets$target) %>% 
        add_binary_decisions() %>%
        add_gurobi_solver(gap = 0, verbose = FALSE)
      
      # Solve the planning problem 
      s <- prioritizr::solve(p)
      saveRDS(s, paste0(output_solutions, paste0(solution_list[i], "-EM-Feature-", metric_list[metric_num], "-SSP", scenario_list[scenario_num], ".rds"))) # save solution
      
      # Plot the spatial design
      s_plot <- s %>% 
        mutate(solution_1 = as.logical(solution_1))
      ggSol <- fSpatPlan_PlotSolution(s_plot, PUs, land) + 
          ggtitle(paste0("Climate-smart design: ", metric_list[metric_num]), 
                                                                    subtitle = paste0("Feature, SSP ", scenario_list[scenario_num])) +
          theme(axis.text = element_text(size = 25))
      ggsave(filename = paste0("EM-Feature-", metric_list[metric_num], "-", scenario_list[scenario_num], ".png"),
             plot = ggSol, width = 21, height = 29.7, dpi = 300,
             path = "Figures/") # save plot
      
      gc()
      rm(list = ls(pattern = metric_df))
      print(i)
      i = i + 1
    }
  }
  
}

loopthrough_EM_Penalty <- function(solution_list, metric_list, scenario_list) {
  
  i = 1
  for(metric_num in 1:length(metric_list)) {
    for(scenario_num in 1:length(scenario_list)) {
      
      if(scenario_list[scenario_num] == "126") {scenario_object <- "SSP 1-2.6"} else if (scenario_list[scenario_num] == "245") {scenario_object <- "SSP 2-4.5"} else if (scenario_list[scenario_num] == "585") {scenario_object <- "SSP 5-8.5"}
      
      # Prepare climate layer
      if(metric_list[metric_num] == "velocity") {
        metric_df <- paste0("velocity_SSP", scenario_list[scenario_num])
      } else if (str_detect(metric_list[metric_num], pattern = "MHW")) {
        metric_df <- paste0(metric_list[metric_num], "_SSP", scenario_list[scenario_num])
      } else {
        metric_df <- paste0("roc_", metric_list[metric_num], "_SSP", scenario_list[scenario_num])
      }
      
      LoadClimateMetrics(metric = metric_list[metric_num], model = NA, scenario = scenario_object)
      
      x = get(metric_df)
      
      # Get scaling
      cost <- UniformCost$cost
      vector <- x %>% as_tibble() %>% dplyr::select(.data[[ "transformed" ]]) %>% pull()
      scaling_PenaltyWarming <- create_Scaling(cost, vector, metric_list[metric_num])
      
      # Get list of features
      features <- aqua_sf %>% 
        as_tibble() %>% 
        dplyr::select(-geometry) %>% 
        names()
      
      # Set up the spatial planning problem
      out_sf <- cbind(aqua_sf, x, UniformCost)
      scaling <- scaling_PenaltyWarming %>% filter(scaling == 30) %>% pull() # get scaling for 30%
      p <- prioritizr::problem(out_sf, features, "cost") %>%
        add_min_set_objective() %>%
        add_relative_targets(0.3) %>%
        add_binary_decisions() %>%
        add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
        add_linear_penalties(scaling, data = "transformed")
      
      # Solve the planning problem 
      s <- prioritizr::solve(p)
      saveRDS(s, paste0(output_solutions, solution_list[i], "-EM-Penalty-", metric_list[metric_num], "-SSP", scenario_list[scenario_num], ".rds")) # save solution
      
      # Plot the spatial design
      s_plot <- s %>% 
        mutate(solution_1 = as.logical(solution_1)) 
      ggSol <- fSpatPlan_PlotSolution(s_plot, PUs, land) + 
          ggtitle(paste0("Climate-smart design: ", metric_list[metric_num]), 
                  subtitle = paste0("Penalty, SSP ", scenario_list[scenario_num])) + 
          theme(axis.text = element_text(size = 25))
      ggsave(filename = paste0("EM-Penalty-", metric_list[metric_num], "-", scenario_list[scenario_num], ".png"),
             plot = ggSol, width = 21, height = 29.7, dpi = 300,
             path = "Figures/") # save
    }
    
    gc()
    rm(list = ls(pattern = metric_df))
    i = i + 1
  }

}

loopthrough_EM_ClimatePriorityArea <- function(solution_list, metric_list, scenario_list) {
  
  i = 1
  
  for(scenario_num in 1:length(scenario_list)) {
    for(metric_num in 1:length(metric_list)) {
      
      if(scenario_list[scenario_num] == "126") {scenario_object <- "SSP 1-2.6"} else if (scenario_list[scenario_num] == "245") {scenario_object <- "SSP 2-4.5"} else if (scenario_list[scenario_num] == "585") {scenario_object <- "SSP 5-8.5"}
      
      if(metric_list[metric_num] == "velocity") {
        metric_df <- paste0("velocity_SSP", scenario_list[scenario_num])
      } else if (str_detect(metric_list[metric_num], pattern = "MHW")) {
        metric_df <- paste0(metric_list[metric_num], "_SSP", scenario_list[scenario_num])
      } else {
        metric_df <- paste0("roc_", metric_list[metric_num], "_SSP", scenario_list[scenario_num])
      }
      
      LoadClimateMetrics(metric = metric_list[metric_num], model = NA, scenario = scenario_object)
      
      x <- get(metric_df)
      
      # 1. Prepare the climate layers and features
      ImptFeat <- create_ImportantFeatureLayer(aqua_sf, metric_list[metric_num], colname = "transformed", x)
      gc() # Clear space
      RepFeat <- create_RepresentationFeature(ImptFeat, aqua_sf)
      gc() # Clear space
      Features <- cbind(ImptFeat, RepFeat) %>% 
        dplyr::select(-geometry.1)
      
      # 2. Get list of features
      features <- Features %>% 
        as_tibble() %>% 
        dplyr::select(-geometry) %>% 
        names()
      
      # 3. Differentiate targets for important features and representative features
      targets <- features %>% as_tibble() %>% 
        setNames(., "Species") %>% 
        add_column(target = 1) %>% 
        mutate(target = ifelse(str_detect(Species, pattern = ".1"), 25/95, 1))
      
      # 4. Set up the spatial planning problem
      out_sf <- cbind(Features, x, UniformCost)
      p <- prioritizr::problem(out_sf, features, "cost") %>%
        add_min_set_objective() %>%
        add_relative_targets(targets$target) %>%
        add_binary_decisions() %>%
        add_gurobi_solver(gap = 0, verbose = FALSE)
      
      # 5. Solve the planning problem 
      s <- prioritizr::solve(p)
      saveRDS(s, paste0(output_solutions, paste0(solution_list[i], "-EM-ClimatePriorityArea-", metric_list[metric_num], "-SSP", scenario_list[scenario_num], ".rds"))) # save solution
      # 6. Plot the spatial design
      s_plot <- s %>% 
        mutate(solution_1 = as.logical(solution_1)) 
      ggSol <- fSpatPlan_PlotSolution(s_plot, PUs, land) + 
          ggtitle(paste0("Climate-smart design: ", metric_list[metric_num]), 
                  subtitle = paste0("Climate Priority Area, SSP ", scenario_list[scenario_num])) + 
          theme(axis.text = element_text(size = 25))
      ggsave(filename = paste0("EM-ClimatePriorityArea-", metric_list[metric_num], "-", scenario_list[scenario_num], ".png"),
             plot = ggSol, width = 21, height = 29.7, dpi = 300,
             path = "Figures/") # save
      
      #clean up environment
      rm(ImptFeat, RepFeat, Features, features, targets)
      rm(list = ls(pattern = metric_df))
      gc()
      i = i + 1
    }
  }
}
  







