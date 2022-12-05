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
