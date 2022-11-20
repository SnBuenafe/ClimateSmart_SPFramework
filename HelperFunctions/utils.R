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
    add_gurobi_solver(gap = 0, verbose = FALSE)
  
  return(dummy_problem)
}