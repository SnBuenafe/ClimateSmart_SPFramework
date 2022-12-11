# Load packages
#install.packages("pacman")
#devtools::install_github("JorGarMol/VoCC", dependencies = TRUE, build_vignettes = TRUE)

#install.packages("remotes")
#remotes::install_github("jfq3/ggordiplots", force = TRUE)

#install.packages("/Library/gurobi911/mac64/R/gurobi_9.1-1_R_4.0.2.tgz", repos=NULL)

# devtools::install_github("ropensci/rfishbase")

# install lpsymphony
# BiocManager::install("lpsymphony")

pacman::p_load(sf, terra, tidyverse, rnaturalearth, prioritizr, stars, patchwork, proj4, magrittr, doParallel, ggridges, viridis, vegan, irr, corrplot, VoCC, RColorBrewer, rfishbase)

longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

# Load a climate metric depending on arguments
load_metrics <- function(metric, 
                         model = "ensemble", # default is ensemble mean
                         scenario
) {
  path <- "Output/"
  list <- list.files(path)
  
  param <- c(model, scenario, metric)
  
  file <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
  file <- which(file == 1)
  
  df <- readRDS(paste0(path, list[file]))
  return(df)
}

# Rename and organize metric data.frame for approaches
rename_metric <- function(df) {
  metric_df <- df %>% 
    tibble::as_tibble() %>% 
    dplyr::select(cellID, transformed) %>% 
    dplyr::rename(metric = transformed)
}

# Create a dummy problem for calculating representation of features
call_dummy <- function() {
  out_sf <- cbind(aqua_sf, UniformCost)
  features <- aqua_sf %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    names()
  dummy_problem <- prioritizr::problem(out_sf, features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.3) %>%
    add_binary_decisions() %>%
    add_cbc_solver(gap = 0, verbose = FALSE)
  
  return(dummy_problem)
}

# Arranges the solution to create Cohen's Kappa Correlation Matrix
select_solution <- function(obj, col_name) {
  obj <- obj %>% 
    as_tibble() %>% 
    dplyr::select(solution_1) %>% 
    dplyr::rename(!!sym(col_name) := solution_1)
}

# TODO: Check what happens when there's a metric; Make Kernel for Climate Data
make_kernel <- function(solution, name, group, metric = NA) {
  
  if(is.na(metric)) {
    df <- solution %>% 
      tibble::as_tibble() %>% 
      # dplyr::filter(solution_1 == 1) %>% 
      dplyr::select(solution_1, transformed) %>% # Select only solution and climate metric
      dplyr::rename(!!sym(name) := transformed) %>% # Rename metric
      tidyr::pivot_longer(!!sym(name), names_to = group, values_to = "transformed")
  } else { 
    metric_df <- metric %>% 
      as_tibble()
    
    df <- solution %>% 
      as_tibble() %>%
      left_join(., metric_df) %>% 
      # dplyr::filter(solution_1 == 1) %>% 
      dplyr::select(transformed) %>% 
      dplyr::rename(!!sym(name) := transformed) %>% 
      tidyr::pivot_longer(!!sym(name), names_to = group, values_to = "transformed")
  }
  
  
  return(df)
}

# Make inset histogram
plot_inset <- function(sFreq) {
  
  temp <- sFreq %>% 
    tibble::as_tibble() %>% 
    dplyr::select(selection) %>% # Select the selection frequency column
    dplyr::group_by(selection) %>% 
    dplyr::summarize(total = n()) %>% # Summarize how many planning units have that selection frequency
    dplyr::mutate(proportion = total/nrow(PUs)) %>% 
    #arrange(., desc(selection)) %>%  # commenting this out because want to get areas with exact selection frequencies
    #dplyr::mutate(proportion = ifelse(selection == 0, yes = total/nrow(PUs), no = cumsum(total)/nrow(PUs))) %>% 
    dplyr::arrange(selection) # Ascending value of selection
  
  # Creating the inset plot
  inset <- ggplot(temp, 
                  aes(x = as.factor(selection), 
                      y = proportion, 
                      fill = as.factor(selection))) +
    ggplot2::scale_fill_brewer(name = "Selection Frequency",
                      palette = "PuBu", aesthetics = "fill") +
    ggplot2::geom_col(width = 1, show.legend = FALSE) +
    ggplot2::theme_bw() + 
    ggplot2::xlab(element_blank()) +
    ggplot2::ylab(element_blank()) +
    ggplot2::scale_y_continuous(expand = c(0,0)) +
    ggplot2::labs(title = element_blank()) +
    ggplot2::theme(axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())
  
  return(inset)
}

# Preprocess frequencies of selection to create KD plots of targets
frequency_targets <- function(sFreq, name) {
  
  solution <- list() # empty list
  for(i in 1:(length(name))){
    solution[[i]] <- sFreq %>% 
      tibble::as_tibble() %>% 
      dplyr::filter(selection == i) %>% 
      dplyr::select(selection, cellID) %>% 
      dplyr::left_join(PUs, ., by = "cellID") %>% 
      dplyr::mutate(solution_1 = ifelse(is.na(selection), yes = 0, no = 1))
    
  }
  
  return(solution)
}

# Solve spatial planning problems
solve_SPproblem <- function(p) {
  s <- prioritizr::solve(p) %>% 
    dplyr::select(cellID, solution_1, cost, slpTrends, seTrends, sigTrends, transformed, everything()) # arrange column names
}

# Loading summaries and merging it into one object
load_summary <- function(metric, column) {
  feature <- read_csv(paste0(summary_dir, "Supplement_Feature_Summary.csv")) %>% 
    dplyr::filter(grepl(metric, run)) %>% 
    dplyr::select(!!sym(column), run)
  
  percentile <- read_csv(paste0(summary_dir, "MetricTheme_Summary.csv")) %>% 
    dplyr::filter(grepl(metric, run)) %>% 
    dplyr::select(!!sym(column), run)
  
  penalty <- read_csv(paste0(summary_dir, "Supplement_Penalty_Summary.csv")) %>% 
    dplyr::filter(grepl(metric, run)) %>% 
    dplyr::select(!!sym(column), run)
  
  climatePriorityArea <- read_csv(paste0(summary_dir, "Supplement_ClimatePriorityArea_Summary.csv")) %>% 
    dplyr::filter(grepl(metric, run)) %>% 
    dplyr::select(!!sym(column), run)
  
  climate <- bind_rows(feature, percentile, penalty, climatePriorityArea)
  
  return(climate)
}

# Loading feature representation targets
load_featrep <- function(metric) {
  feature <- read_csv(paste0(summary_dir, "Supplement_Feature_FeatureRepresentation.csv")) %>% 
    dplyr::select(feature, contains(metric))
  
  percentile <- read_csv(paste0(summary_dir, "MetricTheme_FeatureRepresentation.csv")) %>% 
    dplyr::select(feature, contains(metric))
  
  penalty <- read_csv(paste0(summary_dir, "Supplement_Penalty_FeatureRepresentation.csv")) %>% 
    dplyr::select(feature, contains(metric))
  
  climatePriorityArea <- read_csv(paste0(summary_dir, "Supplement_ClimatePriorityArea_FeatureRepresentation.csv")) %>% 
    dplyr::select(feature, contains(metric))
  
  feat_rep <- dplyr::left_join(feature, percentile) %>% 
    dplyr::left_join(., penalty) %>% 
    dplyr::left_join(., climatePriorityArea)
}

# Calculate mean of PUs that were not selected
calculate_meanClimateNotSelected <- function(solution_list, names) {
  tib <- list()
  for(i in 1:length(names)) {
    x <- mean((solution_list[[i]] %>% 
                 dplyr::filter(solution_1 == 0))$transformed)
    tib[[i]] <- c("approach" = names[i], "mean" = x)
  }
  tibble <- dplyr::bind_rows(tib) %>% 
    dplyr::mutate(mean = as.numeric(mean))
}

# Calculate median of PUs that were not selected
calculate_medianClimateNotSelected <- function(solution_list, names) {
  tib <- list()
  for(i in 1:length(names)) {
    x <- median((solution_list[[i]] %>% 
                 dplyr::filter(solution_1 == 0))$transformed)
    tib[[i]] <- c("approach" = names[i], "median" = x)
  }
  tibble <- dplyr::bind_rows(tib) %>% 
    dplyr::mutate(median = as.numeric(median))
}

# Loop through percentile approach
loopthrough_Percentile <- function(solution_list, 
                                   metric_list, 
                                   scenario_list, 
                                   model_list) {
  
  i = 1
  for(scenario_num in 1:length(scenario_list)) {
    for(metric_num in 1:length(metric_list)) {
      for(model_num in 1:length(model_list)) {
        
        scenario_name = str_replace_all(scenario_list[scenario_num], "[^[:digit:]]+", "")
        if(metric_list[metric_num] %in% c("phos", "o2os", "CombinedMetric")) {
          direction = 1 # Higher values are more climate-smart
          percentile = 65 # Upper 65th percentile
        } else {
          direction = -1 # Lower values are more climate-smart
          percentile = 35 # Lower 35th percentile
        }
        
        # Load metric
        metric <- load_metrics(metric = metric_list[metric_num], 
                               model = model_list[model_num], 
                               scenario = scenario_list[scenario_num])
        
        if(metric_list[metric_num] == "CombinedMetric") {
          metric %<>% dplyr::rename(transformed = combined)
        }
        
        # 1. Prepare climate layer
        aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
                                                  percentile = percentile,
                                                  metricDF = rename_metric(metric),
                                                  direction = direction
        )
        
        # 2. Set up features and targets
        features <- aqua_sf %>% 
          tibble::as_tibble() %>% 
          dplyr::select(-geometry, -cellID) %>% 
          names()
        # Using fixed targets of 30
        target_df <- tibble::as_tibble(features) %>% 
          dplyr::rename(feature = value) %>% 
          dplyr::mutate(target = 30)
        targets <- fAssignTargets_Percentile(featuresDF = aqua_sf,
                                             climateSmartDF = aqua_percentile,
                                             targetsDF = target_df)
        
        # 3. Set up the spatial planning problem
        out_sf <- cbind(UniformCost,
                        aqua_percentile %>% 
                          tibble::as_tibble() %>% 
                          dplyr::select(-cellID, -geometry), 
                        metric %>% 
                          tibble::as_tibble() %>% 
                          dplyr::select(-cellID, -geometry)
        )
        p <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
          add_min_set_objective() %>%
          add_relative_targets(targets$target) %>% 
          add_binary_decisions() %>%
          add_cbc_solver(gap = 0.1, verbose = FALSE)
        
        # 4. Solve the planning problem 
        s <- prioritizr::solve(p) %>% 
          dplyr::select(cellID, solution_1, cost, transformed, everything()) # arrange columns
        
        # Save file
        if(model_list[model_num] == "ensemble") {
          saveRDS(s, paste0(solutions_dir, 
                            solution_list[i], 
                            "-EM", 
                            "-Percentile-", 
                            metric_list[metric_num],
                            "-",
                            scenario_name, 
                            ".rds")) # save solution
        } else {
          saveRDS(s, paste0(solutions_dir, 
                            solution_list[i], 
                            "-MM-", 
                            model_list[model_num], 
                            "-Percentile-", 
                            metric_list[metric_num],
                            "-",
                            scenario_name, 
                            ".rds")) # save solution
        }

        
        gc()
        print(paste0("Saved: ", solution_list[i], 
                     "; model: ", model_list[model_num], 
                     "; approach: percentile",
                     "; scenario: ", scenario_list[scenario_num],
                     "; metric: ", metric_list[metric_num])) # Sanity check
        
        i = i + 1
        

        
      }
    }
  }
}

# Loop through feature approach
loopthrough_Feature <- function(solution_list, 
                                metric_list, 
                                scenario_list, 
                                model_list) {
  
  i = 1
  for(scenario_num in 1:length(scenario_list)) {
    for(metric_num in 1:length(metric_list)) {
      for(model_num in 1:length(model_list)) {
        
        scenario_name = str_replace_all(scenario_list[scenario_num], "[^[:digit:]]+", "")
        if(metric_list[metric_num] %in% c("phos", "o2os", "CombinedMetric")) {
          direction = 1 # Higher values are more climate-smart
          percentile = 65 # Upper 65th percentile
        } else {
          direction = -1 # Lower values are more climate-smart
          percentile = 35 # Lower 35th percentile
        }
        
        # Load metric
        metric <- load_metrics(metric = metric_list[metric_num], 
                               model = model_list[model_num], 
                               scenario = scenario_list[scenario_num])
        
        if(metric_list[metric_num] == "CombinedMetric") {
          metric %<>% dplyr::rename(transformed = combined)
        }
        
        # 1. Prepare climate layer
        aqua_feature <- fFeature_CSapproach(featuresDF = aqua_sf, 
                                            percentile = percentile, 
                                            metricDF = rename_metric(metric),
                                            direction = direction # lower values are more climate-smart
        )
        
        # 2. Set up features and targets
        features <- aqua_sf %>% 
          tibble::as_tibble() %>% 
          dplyr::select(-geometry, -cellID) %>% 
          names()
        # Using fixed targets of 30
        target_df <- tibble::as_tibble(features) %>% 
          dplyr::rename(feature = value) %>% 
          dplyr::mutate(target = 30)
        targets <- fAssignTargets_Feature(climateSmartDF = aqua_feature,
                                          refugiaTarget = 30,
                                          targetsDF = target_df)
        
        # 3. Set up the spatial planning problem
        out_sf <- cbind(UniformCost,
                        aqua_feature %>% 
                          tibble::as_tibble() %>% 
                          dplyr::select(-cellID, -geometry), 
                        metric %>% 
                          tibble::as_tibble() %>% 
                          dplyr::select(-cellID, -geometry)
        )
        p <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
          add_min_set_objective() %>%
          add_relative_targets(targets$target) %>% 
          add_binary_decisions() %>%
          add_cbc_solver(gap = 0.1, verbose = FALSE)
        
        # 4. Solve the planning problem 
        s <- prioritizr::solve(p) %>% 
          dplyr::select(cellID, solution_1, cost, transformed, everything()) # arrange columns
        
        # Save file
        if(model_list[model_num] == "ensemble") {
          saveRDS(s, paste0(solutions_dir, 
                            solution_list[i], 
                            "-EM", 
                            "-Feature-", 
                            metric_list[metric_num],
                            "-",
                            scenario_name, 
                            ".rds")) # save solution
        } else {
          saveRDS(s, paste0(solutions_dir, 
                            solution_list[i], 
                            "-MM-", 
                            model_list[model_num], 
                            "-Feature-", 
                            metric_list[metric_num],
                            "-",
                            scenario_name, 
                            ".rds")) # save solution
        }
        
        
        gc()
        print(paste0("Saved: ", solution_list[i], 
                     "; model: ", model_list[model_num], 
                     "; approach: feature",
                     "; scenario: ", scenario_list[scenario_num],
                     "; metric: ", metric_list[metric_num])) # Sanity check
        
        i = i + 1
      }
    }
  }  
}

# Loop through penalty approach
loopthrough_Penalty <- function(solution_list, 
                                metric_list, 
                                scenario_list, 
                                model_list) {
  
  i = 1
  for(scenario_num in 1:length(scenario_list)) {
    for(metric_num in 1:length(metric_list)) {
      for(model_num in 1:length(model_list)) {
        
        scenario_name = str_replace_all(scenario_list[scenario_num], "[^[:digit:]]+", "")
        
        # Load metric
        metric <- load_metrics(metric = metric_list[metric_num], 
                               model = model_list[model_num], 
                               scenario = scenario_list[scenario_num])
        
        if(metric_list[metric_num] == "CombinedMetric") {
          metric %<>% dplyr::rename(transformed = combined)
        }
        
        # 1. Select scaling
        # Again, here, scaling doesn't matter because the cost layer is uniform
        scaling <- 1/median(metric$transformed) # using the median to scale it
        
        # What matters is the sign of the scaling. To penalize high values, scaling should be positive; to penalize low values, scaling should be negative
        
        if(metric_list[metric_num] %in% c("phos", "o2os", "CombinedMetric")) { # Penalize high values, so scaling should be negative
          if (scaling > 0) scaling = -scaling
        } else { # Penalize low values, so scaling should be positive
          if (scaling < 0) scaling = -scaling
        }
        
        # 2. Get list of features
        features <- aqua_sf %>% 
          dplyr::as_tibble() %>% 
          dplyr::select(-geometry, -cellID) %>% 
          names()
        
        # 3. Set up the spatial planning problem
        out_sf <- cbind(UniformCost,
                        aqua_sf %>% 
                          tibble::as_tibble() %>% 
                          dplyr::select(-cellID, -geometry), 
                        metric %>% 
                          tibble::as_tibble() %>% 
                          dplyr::select(-cellID, -geometry)
        )
        p <- prioritizr::problem(out_sf, features, "cost") %>%
          add_min_set_objective() %>%
          add_relative_targets(0.3) %>% # using 30% targets
          add_binary_decisions() %>%
          add_cbc_solver(gap = 0.1, verbose = FALSE) %>% 
          add_linear_penalties(scaling, data = "transformed")
        
        # 4. Solve the planning problem 
        s <- prioritizr::solve(p) %>% 
          dplyr::select(cellID, solution_1, cost, transformed, everything()) # arrange columns
        
        # Save file
        if(model_list[model_num] == "ensemble") {
          saveRDS(s, paste0(solutions_dir, 
                            solution_list[i], 
                            "-EM", 
                            "-Penalty-", 
                            metric_list[metric_num],
                            "-",
                            scenario_name, 
                            ".rds")) # save solution
        } else {
          saveRDS(s, paste0(solutions_dir, 
                            solution_list[i], 
                            "-MM-", 
                            model_list[model_num], 
                            "-Penalty-", 
                            metric_list[metric_num],
                            "-",
                            scenario_name, 
                            ".rds")) # save solution
        }
        
        
        gc()
        print(paste0("Saved: ", solution_list[i], 
                     "; model: ", model_list[model_num], 
                     "; approach: penalty",
                     "; scenario: ", scenario_list[scenario_num],
                     "; metric: ", metric_list[metric_num])) # Sanity check
        
        i = i + 1
      }
    }
  }
}

# Loop through the climate-priority-area approach
loopthrough_CPA <- function(solution_list, 
                            metric_list, 
                            scenario_list, 
                            model_list) {
  
  i = 1
  for(scenario_num in 1:length(scenario_list)) {
    for(metric_num in 1:length(metric_list)) {
      for(model_num in 1:length(model_list)) {
        
        scenario_name = str_replace_all(scenario_list[scenario_num], "[^[:digit:]]+", "")
        if(metric_list[metric_num] %in% c("phos", "o2os", "CombinedMetric")) {
          direction = 1 # Higher values are more climate-smart
          percentile = 95 # Upper 95th percentile
        } else {
          direction = -1 # Lower values are more climate-smart
          percentile = 5 # Lower 5th percentile
        }
        
        # Load metric
        metric <- load_metrics(metric = metric_list[metric_num], 
                               model = model_list[model_num], 
                               scenario = scenario_list[scenario_num])
        
        if(metric_list[metric_num] == "CombinedMetric") {
          metric %<>% dplyr::rename(transformed = combined)
        }
        
        # 1. Prepare the climate layers and features
        aqua_CPA <- fClimatePriorityArea_CSapproach(featuresDF = aqua_sf,
                                                    percentile = percentile,
                                                    metricDF = rename_metric(metric),
                                                    direction = direction
        )
        
        # 2. Set up features and targets
        features <- aqua_sf %>% 
          tibble::as_tibble() %>% 
          dplyr::select(-geometry, -cellID) %>% 
          names()
        # Using fixed targets of 30
        target_df <- tibble::as_tibble(features) %>% 
          dplyr::rename(feature = value) %>% 
          dplyr::mutate(target = 0.3) # this approach needs proportions as targets
        targets <- fAssignTargets_CPA(climateSmartDF = aqua_CPA,
                                      targetsDF = target_df,
                                      refugiaTarget = 1 # 100% protection to the most climate-smart areas
        )
        
        # 3. Set up the spatial planning problem
        out_sf <- cbind(UniformCost,
                        aqua_CPA %>% 
                          tibble::as_tibble() %>% 
                          dplyr::select(-cellID, -geometry), 
                        metric %>% 
                          tibble::as_tibble() %>% 
                          dplyr::select(-cellID, -geometry)
        )
        p <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
          add_min_set_objective() %>%
          add_relative_targets(targets$target) %>% # using 30% targets
          add_binary_decisions() %>%
          add_cbc_solver(gap = 0.1, verbose = FALSE)
        
        # 4. Solve the planning problem 
        s <- prioritizr::solve(p) %>% 
          dplyr::select(cellID, solution_1, cost, transformed, everything()) # arrange columns
        
        # Save file
        if(model_list[model_num] == "ensemble") {
          saveRDS(s, paste0(solutions_dir, 
                            solution_list[i], 
                            "-EM", 
                            "-ClimatePriorityArea-", 
                            metric_list[metric_num],
                            "-",
                            scenario_name, 
                            ".rds")) # save solution
        } else {
          saveRDS(s, paste0(solutions_dir, 
                            solution_list[i], 
                            "-MM-", 
                            model_list[model_num], 
                            "-ClimatePriorityArea-", 
                            metric_list[metric_num],
                            "-",
                            scenario_name, 
                            ".rds")) # save solution
        }
        
        rm(aqua_CPA, targets, metric) # Free up space
        gc()
        print(paste0("Saved: ", solution_list[i], 
                     "; model: ", model_list[model_num], 
                     "; approach: climate-priority-area",
                     "; scenario: ", scenario_list[scenario_num],
                     "; metric: ", metric_list[metric_num])) # Sanity check
        
        i = i + 1
      }
    }
  }
}
