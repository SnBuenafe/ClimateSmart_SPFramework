# title: "Helper functions"
# author: "Tin Buenafe"

# Created Helper Functions to make running prioritizr easier.

# This function extracts the percentage of each feature that is selected
represent_feature <- function(p, s, col_name) {
  feat_rep <- eval_feature_representation_summary(p, s[, 'solution_1']) %>% 
    dplyr::select(feature, relative_held) %>% 
    mutate(relative_held = relative_held*100) %>% 
    rename(!!sym(col_name) := relative_held)
  
  if(grepl(pattern = "penalty|feature", x = col_name, ignore.case = TRUE)) {
    feat_rep %<>% add_row(feature = "climate_layer", !!sym(col_name) := NA)
  }

  return(feat_rep)
}

# This function computes for some of the summary statistics available for the solution
compute_summary <- function(s, total_area, PU_size, run_name, Cost) {
  
  summary <- s %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    filter(solution_1 == 1) %>% 
    summarize(sum_area = nrow(.) * PU_size, 
              total_cost = sum(!!sym(Cost))) # total area and cost

  summary %<>% mutate(percent_area = sum_area*100/total_area, 
                      num_pu = nrow(s %>% as_tibble() %>% filter(solution_1 == 1)), 
                      run = run_name)
  
  return(summary)
}

# This function arranges the object that is ultimately used to create Cohen's Kappa Correlation Matrix
select_solution <- function(obj, col_name) {
  obj <- obj %>% 
    as_tibble() %>% 
    dplyr::select(solution_1) %>% 
    dplyr::rename(!!sym(col_name) := solution_1)
}

# This function creates Cohen's Kappa Correlation Matrix
create_corrmatrix <- function(list_plans) {
  
  pacman::p_load(irr)

  y = 1
  s_matrix <- list() # empty list
  for(i in 1:length(list_plans)){
    for(j in 1:length(list_plans)){
      kappa_temp <- irr::kappa2(bind_cols(list_plans[[i]], list_plans[[j]]))
      kappa_corrvalue <- kappa_temp$value
      kappa_pvalue <- kappa_temp$p.value
      s_matrix[[y]] <- cbind(colnames(list_plans[[i]]), # first plan
                             colnames(list_plans[[j]]), # second plan
                             kappa_corrvalue, # correlation value
                             kappa_pvalue) # p value
      y = y+1
    }
  }
  
  s_matrix_all <- do.call(rbind, s_matrix) %>% 
    as_tibble()
  colnames(s_matrix_all)[1:2] <- c('plan1','plan2')
  
  matrix <- s_matrix_all %>% 
    as_tibble() %>% 
    dplyr::select(-kappa_pvalue) %>% 
    pivot_wider(names_from = plan2, values_from = kappa_corrvalue) %>% 
    as.matrix()
  
  return(matrix)
}

# This plots the Correlation Matrix.
plot_corrplot <- function(matrix, num) {
  pacman::p_load(corrplot)
  
  # creating corrplot
  rownames(matrix) <- matrix[,1]
  n <- num + 1 # num represents the number of inputted spatial plans
  matrix_f <- matrix[,2:n]
  class(matrix_f) <- "numeric"
  
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  plot <- corrplot(matrix_f, method = "shade", cl.lim = c(-0.2,1), tl.col = "black", addCoef.col = "black",
                   col=col(200), tl.srt=45)
  return(plot)
}

# Plot statistics
plot_statistics <- function(summary, col_name, y_axis, theme) {
  
  # Dictate palettes and what to plot
  if (theme == "ensemble"){
    color_legend <- c("#FAF7B7", "#E6C173", "#855600", "#5075BA", "#81B0CC", "#5A9E67")
    string <- "as.factor(run)"
  } 
  else if (theme == "scenario"){
    color_legend <- c("126" = "#289E3D", "245" = "#E6C173", "585" = "#855600")
    string <- "as.factor(scenario)"
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

# Streamlines the creation of the climate layer for the "percentile" approach
create_PercentileLayer <- function(aqua_sf, metric_name, colname, metric_df, PUs) {
  
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
      
      # Get 35th percentile of climate metric under the range of the species
      quantile <- df %>% 
        dplyr::filter(!!sym(spp[i]) == 1) %>%  # filter out those that have biodiversity values
        summarize(quantile = quantile(.data[[ colname ]], 0.35)) %>%  # get 35th percentile of climate metric
        pull()
      
      df %<>% dplyr::mutate(!!sym(spp[i]) := case_when(((!!sym(colname) <= quantile) & !!sym(spp[i]) == 1) ~ 1, TRUE ~ 0))
      
    } else if (metric_name %in% c("phos", "o2os")) {
      
      # Get 65th percentile of climate metric under range of the species
      quantile <- df %>% 
        dplyr::filter(!!sym(spp[i]) == 1) %>%  # filter out those that have biodiversity values
        summarize(quantile = quantile(.data[[ colname ]], 0.65)) %>%  # get 65th percentile of climate metric
        pull()
      
      df %<>% dplyr::mutate(!!sym(spp[i]) := case_when(((!!sym(colname) >= quantile) & !!sym(spp[i]) == 1) ~ 1, TRUE ~ 0))
    }
    
    list[[i]] <- df %>%  dplyr::select(1) # always just select the species
    
  }
  stopCluster(cl)
  
  aqua_df <- do.call(cbind, aq_tmp) %>% 
    cbind(., PUs) %>% st_as_sf(sf_column_name = "geometry")
  
  return(aqua_df)
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

# Streamlines calculating the summaries of the climate metrics of the spatial designs
get_ClimateSummary <- function(solution_list, climate_layer, metric, col_scenario, col_approach, col_run, climateLayer = "multiple") {
  
  get_mean <- function(s, metric) {
    df <- s %>% as_tibble() %>% 
      dplyr::select(solution_1, geometry) %>% 
      left_join(., metric) %>% 
      filter(solution_1 == 1)
  }

  df <- list() # create empty list
  for(i in 1:length(solution_list)) {
    
    if(climateLayer == "single") {
      df[[i]] <-  get_mean(solution_list[[i]], climate_layer)
    } else {
      df[[i]] <-  get_mean(solution_list[[i]], climate_layer[[i]])
    }
    
    if (length(metric) > 1) {
    metric = metric[i]
    } 
    
    if (metric == "tos") {
      df[[i]] %<>% summarize(mean_climate_warming = mean(transformed))
    } else if (metric == "phos") {
      df[[i]] %<>% summarize(mean_ocean_acidification = mean(transformed))
    } else if (metric == "o2os") {
      df[[i]] %<>% summarize(mean_oxygen_decline = mean(transformed))
    } else if (metric == "velocity") {
      df[[i]] %<>% summarize(median_velocity = median(transformed),
                             mean_log_velocity = mean(log(transformed)))
    } else if (metric == "MHW_SumCumInt") {
      df[[i]] %<>% summarize(mean_sum_cumulative_intensity = mean(transformed))
    }
    
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

# Intersect solution with climate layer
#make_intersect <- function(s, metric) {
#  df <- s %>% as_tibble() %>% 
#    dplyr::select(solution_1, geometry) %>% 
#    left_join(., metric) %>% 
#    filter(solution_1 == 1)
#}

# Create selection frequency plot
plot_SelectionFrequency <- function(data, land) {
  gg <- ggplot() + geom_sf(data = data, aes(fill = as.factor(selection)), color = NA, size = 0.01) +
    geom_sf(data = land, color = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(data)$xlim, ylim = st_bbox(data)$ylim) +
    scale_fill_brewer(name = "Selection Frequency",
                      palette = "PuBu", aesthetics = "fill") +
    theme_bw() +
    labs(subtitle = "Variability in GCMs")
}

# Create low regret areas for specific approach
create_LowRegretSf <- function(solution_list, col_names, PUs, scenario = FALSE) {
   
  df <- list() # empty list
  for (i in 1:length(col_names)) {
    df[[i]] <- solution_list[[i]] %>% dplyr::select(solution_1) %>% 
      rename(!!sym(col_names[i]) := solution_1) %>% 
      as_tibble()
  }
  
  tmp <- df[[1]]
  for (i in 2:length(col_names)) {
    tmp <- tmp %>% 
      left_join(df[[i]], .)
  }
  
  tmp %<>% dplyr::select(-geometry) %>% 
    mutate(selection = rowSums(., na.rm = TRUE)) %>% 
    dplyr::mutate(cellID = row_number())
  
  PUs_temp <- PUs %>% 
    dplyr::mutate(cellID = row_number())
  
  # Create the low-regret sf object
  low_regret <- full_join(tmp, PUs_temp, by = "cellID") %>% 
    st_as_sf(sf_column_name = "geometry") %>% 
    left_join(., UniformCost %>% as_tibble(), by = "geometry") %>% 
    st_as_sf(sf_column_name = "geometry") 
  
  if (isTRUE(scenario)) {
    low_regret %<>% 
      dplyr::mutate(solution_1 = ifelse(selection == 3, 1, 0))
  } else {
    low_regret %<>% 
      dplyr::mutate(solution_1 = ifelse(selection == 4, 1, 0))    
  }

  return(low_regret)
}

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

# Make Kernel for Climate Data
make_kernel <- function(solution, name, group, metric = NA) {
  
  if(is.na(metric)) {
    df <- solution %>% 
      as_tibble() %>% 
     # dplyr::filter(solution_1 == 1) %>% 
      dplyr::select(solution_1, transformed) %>% 
      dplyr::rename(!!sym(name) := transformed) %>% 
      pivot_longer(!!sym(name), names_to = group, values_to = "transformed")
  } else {
    metric_df <- metric %>% 
      as_tibble()
    
    df <- solution %>% 
      as_tibble() %>%
      left_join(., metric_df) %>% 
     # dplyr::filter(solution_1 == 1) %>% 
      dplyr::select(transformed) %>% 
      dplyr::rename(!!sym(name) := transformed) %>% 
      pivot_longer(!!sym(name), names_to = group, values_to = "transformed")
  }
  
  
  return(df)
}

# Make inset histogram
plot_inset <- function(sFreq) {
  
  temp <- sFreq %>% as_tibble %>% 
    dplyr::select(selection) %>% 
    dplyr::group_by(selection) %>% 
    dplyr::summarize(total = n()) %>% 
    dplyr::mutate(proportion = total/nrow(PUs)) %>% 
    #arrange(., desc(selection)) %>%  # commenting this out because want to get areas with exact selection frequencies
    #dplyr::mutate(proportion = ifelse(selection == 0, yes = total/nrow(PUs), no = cumsum(total)/nrow(PUs))) %>% 
    arrange(selection)
  
  inset <- ggplot(temp, aes(x = as.factor(selection), y = proportion, fill = as.factor(selection))) +
    scale_fill_brewer(name = "Selection Frequency",
                      palette = "PuBu", aesthetics = "fill") +
    geom_col(width = 1, show.legend = FALSE) +
    theme_bw() + 
    xlab(element_blank()) +
    ylab(element_blank()) +
    scale_y_continuous(expand = c(0,0)) +
    labs(title = element_blank()) +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  return(inset)
}

# Preprocess frequencies of selection to create KD plots of targets
frequencyTargets <- function(sFreq, name) {
  
  solution <- list() # empty list
  for(i in 1:(length(name))){
    solution[[i]] <- sFreq %>% 
      as_tibble() %>% 
      dplyr::filter(selection == i) %>% 
      dplyr::select(selection, cellID) %>% 
      left_join(PlanUnits, ., by = "cellID") %>% 
      dplyr::mutate(solution_1 = ifelse(is.na(selection), yes = 0, no = 1))
    
  }
  
  return(solution)
}
