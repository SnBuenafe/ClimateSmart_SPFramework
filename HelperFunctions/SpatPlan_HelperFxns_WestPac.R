# Created Helper Functions to make running prioritizr easier.

# This function extracts the percentage of each feature that is selected
represent_feature <- function(p, s, col_name) {
  feat_rep <- eval_feature_representation_summary(p, s[, 'solution_1']) %>% 
    dplyr::select(feature, relative_held) %>% 
    mutate(relative_held = relative_held*100) %>% 
    rename(!!sym(col_name) := relative_held)
  
  #if(str_detect(col_name, regex("percentile", ignore_case = TRUE))) {
  #  feat_rep %<>% mutate(!!sym(col_name) := .data[[ col_name ]] * 0.35)
  #} 
  if(str_detect(col_name, grepl("penalty|feature", ignore_case = TRUE))) {
    feat_rep %<>% add_row(feature = "climate_layer", !!sym(col_name) := NA)
  }# else if(str_detect(col_name, regex("ClimatePriorityArea", ignore_case = FALSE))) {
  #  feat_rep %<>% mutate(!!sym(col_name) := ifelse(str_detect(feature, ".1"), yes = .data[[ col_name ]] * 0.95, no = .data[[ col_name ]] * 0.05))
  #}

  return(feat_rep)
}

# This function computes for some of the summary statistics available for the solution
compute_summary <- function(s, total_area, PU_size, run_name, Cost) {
  
  summary <- s %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    filter(solution_1 == 1) %>% 
    summarize(sum_area = nrow(.) * PU_size, total_cost = sum(!!sym(Cost)))

  summary %<>% mutate(percent_area = sum_area*100/total_area, num_pu = nrow(s %>% as_tibble() %>% 
                                                                              filter(solution_1 == 1)), run = run_name)
  
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
      s_matrix[[y]] <- cbind(colnames(list_plans[[i]]), colnames(list_plans[[j]]), kappa_corrvalue, kappa_pvalue)
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
  if (theme == "ensemble"){
    color_legend <- c("#FAF7B7", "#E6C173", "#855600", "#5075BA", "#81B0CC", "#5A9E67")
    string <- "as.factor(run)"
  } else if (theme == "scenario"){
    color_legend <- c("126" = "#289E3D", "245" = "#E6C173", "585" = "#855600")
    string <- "as.factor(scenario)"
  } else if (theme == "metric") {
    color_legend <- c("#289E3D", "#E6C173", "#81B0CC", "#855600")
    string <- "as.factor(run)"
  } else if (theme == "LR-approach"){
    color_legend = c("#E6BA7E", "#4D3B2A", "#6984BF", "#2B8142")
    string <- "as.factor(run)"
  }
  
  plot <- ggplot(data = summary, aes_string(x = string)) + # TODO: add in aes (later on) group = scenario
    geom_bar(aes_string(y = col_name, fill = string), stat = 'identity', position = position_dodge()) +
    scale_fill_manual(name = 'Run',
                      values = color_legend) +
    xlab("Run") +
    ylab(y_axis) +
    theme(legend.position = "bottom") +
    theme_classic()
  
  return(plot)
  
#  if (color == 1) { # For plotting related to RQ2
#    color_legend <- c("tos" = "#289E3D", "phos" = "#E6C173", "o2os" = "#81B0CC", "velocity" = "#855600")
    
#    summary %<>% dplyr::mutate(approach = case_when(str_detect(run, pattern = "tos") ~ "tos",
#                                                    str_detect(run, pattern = "phos") ~ "phos",
#                                                    str_detect(run, pattern = "o2os") ~ "o2os",
#                                                    str_detect(run, pattern = "velocity") ~ "velocity"))
    
#  } else if (color == 2) { # For plotting related to RQ1
#    color_legend <- c("uninformed" = "#a6611a", "tos" = "#dfc27d")
    
#    summary %<>% dplyr::mutate(approach = case_when(str_detect(run, pattern = "uninformed") ~ "uninformed",
                              #                      str_detect(run, pattern = "tos") ~ "tos"))
#  } else if (color == 3) { # For plotting vs scenarios
#    summary %<>% dplyr::mutate(approach = case_when(run == "percentile_tos_585" ~ "SSP 5-8.5",
#                                                    run == "percentile_tos_126" ~ "SSP 1-2.6",
 #                                                   run == "percentile_tos_245" ~ "SSP 2-4.5"))
  #  
   # color_legend <- c("SSP 1-2.6" = "#289E3D", "SSP 2-4.5" = "#E6C173", "SSP 5-8.5" = "#855600")
#  } else {
 #   color_legend <- c("#289E3D", "#E6C173", "#81B0CC", "#855600", "#5075BA")
  #  }
  
  #plot <- ggplot(data = summary, aes(x = as.factor(approach))) + # TODO: add in aes (later on) group = scenario
   # geom_bar(aes_string(y = col_name, fill = "as.factor(approach)"), stat = 'identity', position = position_dodge()) +
  #  scale_fill_manual(name = 'Run',
   #                   values = color_legend) +
    #xlab("Run") +
    #ylab(y_axis) +
    #theme(legend.position = "bottom") +
    #theme_classic()
  
}

# Plots statistics, comparing them across approaches
plot_ComparisonStatistics <- function(summary, col_name, y_axis) {
    color_legend = c("uninformed" = "#1C2833", "feature" = "#E6BA7E", "percentile" = "#4D3B2A", "penalty" = "#6984BF")

    plot <- ggplot(data = summary, aes(x = as.factor(metric), group = as.factor(approach))) +
      geom_bar(aes_string(y = col_name, fill = "as.factor(approach)"), stat = 'identity', position = position_dodge()) +
      scale_fill_manual(name = 'Approach',
                        values = color_legend) +
      xlab("Climate-smart Metric") +
      ylab(y_axis) +
      theme(legend.position = "bottom") +
      theme_classic()
    
    return(plot)
}

# Plots statistics of low-regret areas
plot_LowRegretStatistics <- function(LowRegret_df, col_name, y_axis) {
  color_legend = c("feature" = "#E6BA7E", "percentile" = "#4D3B2A", "penalty" = "#6984BF")
  
  plot <- ggplot(data = LowRegret_df, aes(x = as.factor(approach))) +
    geom_bar(aes_string(y = col_name, fill = "as.factor(approach)"), stat = "identity", position = position_dodge()) +
    scale_fill_manual(name = "Approach",
                      values = color_legend) +
    xlab("Climate-smart Approach") +
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
  
  ncores <- detectCores() - 1 
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
    
    if (metric_name %in% c("tos", "velocity")) {
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
    
    if (metric_name %in% c("tos", "phos", "o2os")) {
      list[[i]] <- df %>% dplyr::select(-slpTrends, -seTrends, -sigTrends, -transformed)
    } else if (metric_name == "velocity") {
      list[[i]] <- df %>% dplyr::select(-voccMag, -voccAng, -transformed)
    }
    
  }
  stopCluster(cl)
  
  aqua_df <- do.call(cbind, aq_tmp) %>% 
    cbind(., PUs) %>% st_as_sf(sf_column_name = "geometry")
  
  return(aqua_df)
}

# Streamlines the creation of the climate layer for the "feature" approach
create_FeatureLayer <- function(metric_name, colname, metric_df) {
  
  #metric_name = tos, phos, o2os, velocity
  # colname = slpTrends / voccMag
  # metric_df = roc_tos_SSP585, ...
  
  if (metric_name %in% c("tos", "velocity")) {
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

create_ImportantFeatureLayer <- function(aqua_sf, metric_name, colname, metric_df) {
  
  spp <- aqua_sf %>% as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    names()
  
  metric_df <- metric_df %>% 
    dplyr::mutate(cellID = row_number()) %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  
  # Commence Parallel Loop.
  
  ncores <- detectCores() - 1 
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
    
    if (metric_name %in% c("tos", "velocity")) {
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
    
    if (metric_name %in% c("tos", "phos", "o2os")) {
      list[[i]] <- df %>% dplyr::select(-slpTrends, -seTrends, -sigTrends, -transformed)
    } else if (metric_name == "velocity") {
      list[[i]] <- df %>% dplyr::select(-voccMag, -voccAng, -transformed)
    }
    
  }
  stopCluster(cl)
  
  aqua_df <- do.call(cbind, aq_tmp) %>% 
    cbind(., PUs) %>% st_as_sf(sf_column_name = "geometry")
  
  return(aqua_df)
  
  
}
create_RepresentationFeature <- function(df, aqua_sf) {
  
  spp <- aqua_sf %>% as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    names()
  
  impt_df <- df %>% 
    dplyr::mutate(cellID = row_number()) %>% 
    as_tibble() %>% 
    dplyr::select(-geometry)
  
  # Commence Parallel Loop.
  
  ncores <- detectCores() - 1 
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
make_intersect <- function(s, metric) {
  df <- s %>% as_tibble() %>% 
    dplyr::select(solution_1, geometry) %>% 
    left_join(., metric) %>% 
    filter(solution_1 == 1)
}

# Check normality of metric.
check_normality <- function(df, col_name) {
  tmp <- df %>% as_tibble() %>%
    dplyr::select(!!sym(col_name))
  qqnorm(tmp[[ col_name ]])
  qqline(tmp[[ col_name ]])
}

# Plots Low Regret Areas
plot_lowregret <- function(data, land) {
  gg <- ggplot() + geom_sf(data = data, aes(fill = as.factor(selection)), color = NA, size = 0.01) +
    geom_sf(data = land, color = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(data)$xlim, ylim = st_bbox(data)$ylim) +
    scale_fill_brewer(name = "Selection",
                      palette = "OrRd", aesthetics = "fill") +
    theme_bw() +
    labs(subtitle = "Low-Regret Areas")
}

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

  percentage <- seq(from  = 20, to = 100, by = 10)
  
  x = (max(cost)) / (max(climate_metric) - min(climate_metric)) #  Used max cost instead of range of cost because we're using a uniform cost layer
  
  scaling <- tibble(scaling = numeric(), penalty_value = numeric())
  
  if (metric %in% c("tos", "velocity")) {
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

# Creates the summaries of the climate metrics for low-regret areas
lowRegret_ClimateSummary <- function(solution, run, metric, climate, scenario, approach) {
  
  list <- list() # empty list
  
  for(i in 1:length(run)) {
    df <- solution[[i]] %>% 
      dplyr::select(selection, cellID) %>% 
      dplyr::rename(solution_1 = selection)
    
    x <- tibble(run = character()) # empty tibble
    for(j in 1:length(metric)) {
      tmp <- get_ClimateSummary(solution_list = list(df), climate_layer = climate[[j]],
                                metric = metric[j], col_scenario = scenario,
                                col_approach = approach[i], col_run = run[i], 
                                climateLayer = "single")
      x <- left_join(tmp, x)
    }
    
    list[[i]] <- x
  }
  
  complete <- do.call(rbind, list)
  return(complete)
}

# intersection of all low-regret areas
intersect_lowregret <- function(solution, run) {
  
  tibble <- tibble(cellID = integer()) #empty tibble
  for(i in 1:length(run)) {
    df <- solution[[i]] %>% 
      as_tibble() %>% 
      dplyr::select(selection, cellID) %>% 
      dplyr::rename(!!sym(run[i]) := selection)
    tibble <- left_join(df, tibble)
  }
  
  tmp <- PUs %>% 
    dplyr::mutate(cellID = row_number()) %>% 
    as_tibble()
  
  tibble %<>% dplyr::mutate(selection = rowSums(., na.rm = TRUE) - cellID) %>% 
    left_join(., tmp, by = "cellID") %>% 
    dplyr::mutate(cost = PU_size) %>% 
    st_as_sf(sf_column_name = "geometry")
    
  return(tibble)
}

# Compares statistics across approaches and metrics.
plot_ComparisonStatistics <- function(summary, col_name, y_axis) {
  color_legend = c("uninformed" = "#1C2833", "feature" = "#E6BA7E", "percentile" = "#4D3B2A", "penalty" = "#6984BF")
  
  plot <- ggplot(data = summary, aes(x = as.factor(metric), group = as.factor(approach))) +
    geom_bar(aes_string(y = col_name, fill = "as.factor(approach)"), stat = 'identity', position = position_dodge()) +
    scale_fill_manual(name = 'Approach',
                      values = color_legend) +
    xlab("Climate-smart Metric") +
    ylab(y_axis) +
    theme(legend.position = "bottom") +
    theme_classic()
  
  return(plot)
}

# Plot features

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
make_kernel <- function(solution, name, group) {
  df <- solution %>% 
    as_tibble() %>% 
    dplyr::filter(solution_1 == 1) %>% 
    dplyr::select(transformed) %>% 
    dplyr::rename(!!sym(name) := transformed) %>% 
    pivot_longer(!!sym(name), names_to = group, values_to = "transformed")
  
  return(df)
}

