# TODO: Fix this

# Description:
# Streamlines calculating the summaries of the climate metrics of the spatial designs

fGetClimateSummary <- function(solution_list, # list of solutions
                               climate_layer, # list of climate metric dfs
                               metric, # metric used
                               col_scenario, # vector of scenarios
                               col_approach, # vector of approaches
                               col_run, # vector of tags
                               climateLayer = "multiple" # using different climate layers to get summary?
                               ) {
  
  filter_presence <- function(s, metric) { # filter presences 
    df <- s %>% 
      tibble::as_tibble() %>% 
      dplyr::select(solution_1, geometry) %>% 
      dplyr::left_join(., metric) %>% 
      dplyr::filter(solution_1 == 1)
  }
  
  df <- list() # create empty list
  
  for(i in 1:length(solution_list)) {
    
    # Filter presences in the data.frame
    if(climateLayer == "single") {
      df[[i]] <- filter_presence(solution_list[[i]], 
                                  climate_layer)
    } else {
      df[[i]] <- filter_presence(solution_list[[i]], 
                                  climate_layer[[i]])
    }
    
    if (length(metric) > 1) {
      metric = metric[i]
    } 
    
    # Calculate the mean of the metric
    if (metric == "velocity") {
      df[[i]] %<>% dplyr::summarize(!!sym(paste0("median_", metric)) := median(transformed))
    } else {
      df[[i]] %<>% dplyr::summarize(!!sym(paste0("mean_", metric)) := mean(transformed))
    }
    
    # Attach run and scenario names
    if (length(col_scenario) > 1) {
      df[[i]] %<>% dplyr::mutate(run = col_run[i],
                                 scenario = col_scenario[i])
    } else {
      df[[i]] %<>% dplyr::mutate(run = col_run[i],
                                 scenario = col_scenario)
    }
    
    if (length(col_approach) > 1) {
      df[[i]] %<>% dplyr::mutate(approach = col_approach[i])
    } else {
      df[[i]] %<>% dplyr::mutate(approach = col_approach)
    }
    
  }
  tmp <- do.call(rbind, df)
  
  return(tmp)
}