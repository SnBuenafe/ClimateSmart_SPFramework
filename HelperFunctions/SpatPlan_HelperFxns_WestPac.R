# title: "Helper functions"
# author: "Tin Buenafe"

# Created Helper Functions to make running prioritizr easier.

# Plot statistics
plot_statistics <- function(summary, col_name, y_axis, theme) {
  
  # Dictate palettes and what to plot
  if (theme == "ensemble"){
    color_legend <- c("#FAF7B7", "#E6C173", "#855600", "#5075BA", "#81B0CC", "#5A9E67")
    string <- "as.factor(run)"
  } 
  else if (theme == "metric") {
    color_legend <- c("#3C6342", "#289E3D", "#E6C173", "#81B0CC", "#855600")
    string <- "as.factor(run)"
  } 
  else if (theme == "LR-approach"){
    color_legend = c("#E6BA7E", "#4D3B2A", "#6984BF", "#2B8142")
    string <- "as.factor(run)"
  } 
  else if (theme == "layer"){
    color_legend = c("LowRegret" = "#4C90F5", "Single" = "#A9C2EB", "Multiple_SSP126" = "#289E3D", "Multiple_SSP245" = "#E6C173", "Multiple_SSP585" = "#855600")
    string <- "as.factor(run)"
  }
  
  plot <- ggplot(data = summary, aes_string(x = string)) +
    geom_bar(aes_string(y = col_name, fill = string), stat = 'identity', position = position_dodge()) +
    scale_fill_manual(name = 'Run',
                      values = color_legend) +
    xlab("Run") +
    ylab(y_axis) +
    theme(legend.position = "bottom") +
    theme_classic()
  
  return(plot)
  
}

# Streamlines the creation of the climate layer for the "feature" approach
create_FeatureLayer <- function(metric_name, colname, metric_df) {
  
  if (metric_name %in% c("tos", "velocity", "MHW_num", "MHW_PeakInt", "MHW_CumInt", "MHW_Dur", "MHW_CumDur", "MHW_SumCumInt")) {
    quantile <- metric_df %>% as_tibble() %>% 
      dplyr::select(!!sym(colname)) %>% 
      dplyr::summarize(quantile = quantile(.data[[ colname ]], 0.35)) %>% 
      pull()
    
    filtered <- metric_df %>% as_tibble() %>% 
      mutate(climate_layer = case_when(!!sym(colname) <= quantile ~ 1,
                                       TRUE ~ 0))   
  } else if (metric_name %in% c("phos", "o2os")) {
    quantile <- metric_df %>% as_tibble() %>% 
      dplyr::select(!!sym(colname)) %>% 
      dplyr::summarize(quantile = quantile(.data[[ colname ]], 0.65)) %>% 
      pull()
    
    filtered <- metric_df %>% as_tibble() %>% 
      mutate(climate_layer = case_when(!!sym(colname) >= quantile ~ 1,
                                       TRUE ~ 0))   
  }
  
  filtered %<>% st_as_sf(sf_column_name = "geometry")
  
  return(filtered)
}

# Streamlines the creation of the climate layer for the "climate priority area" approach: creating climate priority areas features
create_ImportantFeatureLayer <- function(aqua_sf, metric_name, colname, metric_df) {
  
  spp <- aqua_sf %>% as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    names()
  
  metric_df <- metric_df %>% 
    dplyr::mutate(cellID = row_number()) %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  
  # Commence Parallel Loop.
  
  ncores <- detectCores(logical = FALSE) - 1 
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  list <- vector("list", length = length(spp)) # create empty list
  
  aq_tmp <- foreach(i = 1:length(spp), .packages = c('tidyverse', 'sf', 'magrittr')) %dopar% {
    
    # Select 1 species at a time
    df <- aqua_sf %>% 
      as_tibble() %>% 
      dplyr::select(!!sym(spp[i])) %>% 
      dplyr::mutate(cellID = row_number()) %>% 
      left_join(., metric_df, by = "cellID") %>% 
      dplyr::select(-cellID)
    
    if (metric_name %in% c("tos", "velocity", "MHW_num", "MHW_PeakInt", "MHW_CumInt", "MHW_Dur", "MHW_CumDur", "MHW_SumCumInt")) {
      # Get 5th percentile of climate metric under the range of the species
      quantile <- df %>% 
        dplyr::filter(!!sym(spp[i]) == 1) %>%  # filter out those that have biodiversity values
        summarize(quantile = quantile(.data[[ colname ]], 0.05)) %>%  # get 35th percentile of climate metric
        pull()
      
      df %<>% dplyr::mutate(!!sym(spp[i]) := case_when(((!!sym(colname) <= quantile) & !!sym(spp[i]) == 1) ~ 1, TRUE ~ 0))
    } else if (metric_name %in% c("phos", "o2os")) {
      # Get 95th percentile of climate metric under range of the species
      quantile <- df %>% 
        dplyr::filter(!!sym(spp[i]) == 1) %>%  # filter out those that have biodiversity values
        summarize(quantile = quantile(.data[[ colname ]], 0.95)) %>%  # get 65th percentile of climate metric
        pull()
      
      df %<>% dplyr::mutate(!!sym(spp[i]) := case_when(((!!sym(colname) >= quantile) & !!sym(spp[i]) == 1) ~ 1, TRUE ~ 0))
    }
    
    list[[i]] <- df %>% dplyr::select(1) 
    
    #if (metric_name %in% c("tos", "phos", "o2os")) {
    #  list[[i]] <- df %>% dplyr::select(-slpTrends, -seTrends, -sigTrends, -transformed)
    #} else if (metric_name == "velocity") {
    #  list[[i]] <- df %>% dplyr::select(-voccMag, -voccAng, -transformed)
    #}
    
  }
  stopCluster(cl)
  
  aqua_df <- do.call(cbind, aq_tmp) %>% 
    cbind(., PUs) %>% st_as_sf(sf_column_name = "geometry")
  
  return(aqua_df)
  
  
}

# Streamlines the creation of the climate layer for the "climate priority area" approach: getting the non-climate-smart areas
create_RepresentationFeature <- function(df, aqua_sf) {
  
  spp <- aqua_sf %>% as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    names()
  
  impt_df <- df %>% 
    dplyr::mutate(cellID = row_number()) %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  
  # Commence Parallel Loop.
  
  ncores <- detectCores(logical = FALSE) - 1 
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  list <- vector("list", length = length(spp)) # create empty list
  
  aq_tmp <- foreach(i = 1:length(spp), .packages = c('tidyverse', 'sf', 'magrittr')) %dopar% {
    
    # Select 1 species at a time
    df <- aqua_sf %>% 
      as_tibble() %>% 
      dplyr::select(!!sym(spp[i])) %>% 
      dplyr::mutate(cellID = row_number())
    
    df2 <- impt_df %>% 
      dplyr::select(!!sym(spp[i]), cellID) %>% 
      dplyr::rename(V1 := !!sym(spp[i]))
    
    CombinedDf <- left_join(df, df2, by = "cellID") %>% 
      dplyr::mutate(!!sym(spp[i]) := ifelse(V1 == 1, yes = 0, no = .data[[ spp[i] ]])) %>% 
      dplyr::select(-V1, -cellID)
  }
  stopCluster(cl)
  
  aqua_df <- do.call(cbind, aq_tmp) %>% 
    cbind(., PUs) %>% st_as_sf(sf_column_name = "geometry")
}

# Intersect solution with climate layer
#make_intersect <- function(s, metric) {
#  df <- s %>% as_tibble() %>% 
#    dplyr::select(solution_1, geometry) %>% 
#    left_join(., metric) %>% 
#    filter(solution_1 == 1)
#}

# Create penalty scaling values for the "penalty" approach
create_Scaling <- function(cost, climate_metric, metric) {
  # I calculated scaling using this equation:
  # scaling$_ClimateMetric$ $= \frac{(Cost_{Max} - Cost_{Min})}{(ClimateMetric_{Max} - ClimateMetric_{Min})} \cdot (Scaling_{percent})$

  percentage <- c(seq(from  = 20, to = 100, by = 10), seq(from = 120, to = 200, by = 20), 400)
  
  #x = (max(cost) - min(cost)) / (max(climate_metric) - min(climate_metric))
  x = (max(cost)) / (max(climate_metric) - min(climate_metric)) #  Used max cost instead of range of cost because we're using a uniform cost layer
  
  scaling <- tibble(scaling = numeric(), penalty_value = numeric())
  
  if (metric %in% c("tos", "velocity", "MHW_num", "MHW_PeakInt", "MHW_CumInt", "MHW_Dur", "MHW_CumDur", "MHW_SumCumInt")) {
    for (i in 1:length(percentage)) {
      scaling %<>% add_row(scaling = percentage[i], penalty_value = x*percentage[i]/100)
    }
  } else if (metric %in% c("phos", "o2os")) {
    for (i in 1:length(percentage)) {
      scaling %<>% add_row(scaling = percentage[i], penalty_value = -x*percentage[i]/100)
    }
  }

  return(scaling)

}

# Plot features for the workflow figure
plot_AQMFeatures <- function(s1, PlanUnits, world, column){
  gg <- ggplot() + 
    geom_sf(data = s1, aes_string(fill = column), colour = NA, size = 0.1, show.legend = TRUE) +
    #    geom_sf(data = PlanUnits, colour = "lightblue", fill = NA, size = 0.1, show.legend = FALSE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PlanUnits)$xlim, ylim = st_bbox(PlanUnits)$ylim) +
    scale_colour_manual(values = c("TRUE" = "#8856a7",
                                   "FALSE" = "#e0ecf4"),
                        aesthetics = "fill") + 
    theme_bw()
  
}


# Calculating the combined metric
combineMetric <- function(scenario, model) {
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
    sf::st_as_sf(crs = rob_pacific)
  
  # plot(x = combined_metric$velocity, y = combined_metric$velocity_scaled) # sanity check for the combined metrics
  # plot(x = combined_metric$phos, y = combined_metric$phos_scaled) # sanity check for the combined metrics
  
  return(combined_metric)
}
