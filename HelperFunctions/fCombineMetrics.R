# Calculating the combined metric
fCombineMetrics <- function(scenario, model, cCRS) {
  # Load each of the layers per scenario from the outputs
  path <- "Output/"
  list <- list.files(path)
  
  var_list <- c("tos", "phos", "o2os", "velocity", "MHW")
  for(i in 1:length(var_list)) {
    param <- c(model, scenario, var_list[i])
    
    file <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
    file <- which(file == 1)
    
    df <- readRDS(paste0(path, list[file])) %>% 
      tibble::as_tibble() %>% 
      dplyr::select(cellID, transformed, geometry) %>% 
      dplyr::rename(!!sym(var_list[i]) := transformed)
    
    assign(paste0(var_list[i], "_df"), df)
  }
  
  join_df <- list(tos_df, phos_df, o2os_df, velocity_df, MHW_df) %>% 
    purrr::reduce(left_join, by = c("cellID", "geometry")) %>% 
    dplyr::select(cellID, tos, phos, o2os, velocity, MHW, geometry) # arrange columns
  
  # We want the combined metric to be from 0-100 with 0 being the least climate-smart and 100 being the most climate-smart
  # Metrics should be negated if low values represent the more climate-smart areas (i.e., tos, velocity, and MHW)
  combined_metric <- join_df %>% 
    dplyr::mutate(tos_scaled = scales::rescale(-tos, to = c(0, 100)), # scale each metric from 0-100 (with 0 being the least-climate-smart)
                  phos_scaled = scales::rescale(phos, to = c(0, 100)),
                  o2os_scaled = scales::rescale(o2os, to = c(0, 100)),
                  velocity_scaled = scales::rescale(-velocity, to = c(0, 100)),
                  MHW_scaled = scales::rescale(-MHW, to = c(0, 100))
    ) %>%
    dplyr::mutate(combined = (tos_scaled*phos_scaled*o2os_scaled*velocity_scaled*MHW_scaled)^(1/5)) %>% # calculate the combined metric (geometric mean of all scaled metrics)
    dplyr::select(cellID, combined, geometry) %>% # select only the following columns
    sf::st_as_sf(crs = cCRS)
  
  # plot(x = combined_metric$velocity, y = combined_metric$velocity_scaled) # sanity check for the combined metrics
  # plot(x = combined_metric$phos, y = combined_metric$phos_scaled) # sanity check for the combined metrics
  
  return(combined_metric)
}
