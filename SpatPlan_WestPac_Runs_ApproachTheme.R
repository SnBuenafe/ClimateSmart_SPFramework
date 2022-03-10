
#' ### Compare Low Regret Areas
gg_LowRegretFeature + gg_LowRegretPercentile + gg_LowRegretPenalty + gg_LowRegretImptFeature + plot_layout(guides = "collect")

LowRegret_SummaryAll <- LowRegret_SummaryFeature %>% 
  rbind(., LowRegret_SummaryPercentile) %>% 
  rbind(., LowRegret_SummaryPenalty) %>% 
  rbind(., LowRegret_SummaryImptFeature)

# Kappa
list <- c("LowRegret_Feature_585", "LowRegret_Percentile_585", "LowRegret_Penalty_585", "LowRegret_ImptFeature_585")
object_list <- list() # empty list
solution_list <- list(LowRegret_Feature, LowRegret_Percentile, LowRegret_Penalty, LowRegret_ImptFeature)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

# Check the Climate Metrics of the Low-Regret Areas
LowRegretFeature_df <- LowRegret_Feature %>% 
  as_tibble() %>% 
  dplyr::select(-feature_velocity_585, -feature_o2os_585, -feature_phos_585, -feature_tos_585, -cost) %>%
  dplyr::rename(SelectionFeature = selection) %>% 
  left_join(., roc_tos_SSP585 %>% as_tibble() %>% dplyr::select(slpTrends, geometry), by = "geometry") %>% 
  dplyr::rename(tos = slpTrends, Feature_Solution = solution_1) %>% 
  left_join(., roc_phos_SSP585 %>% as_tibble() %>% dplyr::select(slpTrends, geometry), by = "geometry") %>% 
  dplyr::rename(phos = slpTrends) %>% 
  left_join(., roc_o2os_SSP585 %>% as_tibble() %>% dplyr::select(slpTrends, geometry), by = "geometry") %>% 
  dplyr::rename(o2os = slpTrends) %>% 
  left_join(., velocity_SSP585 %>% as_tibble() %>% dplyr::select(voccMag, geometry), by = "geometry") %>% 
  dplyr::rename(velocity = voccMag)

LowRegretPercentile_df <- LowRegret_Percentile %>% 
  as_tibble() %>% 
  dplyr::select(-percentile_velocity_585, -percentile_o2os_585, -percentile_phos_585, -percentile_tos_585, -cost) %>% 
  dplyr::rename(SelectionPercentile = selection, Percentile_Solution = solution_1)

LowRegretPenalty_df <- LowRegret_Penalty %>% 
  as_tibble() %>% 
  dplyr::select(-penalty_velocity_585, -penalty_o2os_585, -penalty_phos_585, -penalty_tos_585, -cost) %>%
  dplyr::rename(SelectionPenalty = selection, Penalty_Solution = solution_1)

LowRegretImptFeature_df <- LowRegret_ImptFeature %>% 
  as_tibble() %>% 
  dplyr::select(-imptfeature_velocity_585, -imptfeature_o2os_585, -imptfeature_phos_585, -imptfeature_tos_585, -cost) %>% 
  dplyr::rename(SelectionImptFeature = selection, ImptFeature_Solution = solution_1)

LowRegretAll_sf <- left_join(LowRegretFeature_df, LowRegretPercentile_df, by = c("cellID", "geometry")) %>% 
  left_join(., LowRegretPenalty_df, by = c("cellID", "geometry")) %>%
  left_join(., LowRegretImptFeature_df, by = c("cellID", "geometry")) %>% 
  dplyr::mutate(SelectionAll = case_when((SelectionFeature == 4 & SelectionPercentile == 4 & SelectionPenalty == 4 & SelectionImptFeature == 4) ~ 1,
                                         TRUE ~ 0)) %>% 
  st_as_sf(sf_column_name = "geometry")

LowRegret_SummaryAll <- lowRegret_ClimateSummary(df = LowRegretAll_sf, approach_column = "Feature_Solution") %>% 
  rbind(., lowRegret_ClimateSummary(df = LowRegretAll_sf, approach_column = "Percentile_Solution")) %>% 
  rbind(., lowRegret_ClimateSummary(df = LowRegretAll_sf, approach_column = "Penalty_Solution")) %>% 
  rbind(., lowRegret_ClimateSummary(df = LowRegretAll_sf, approach_column = "ImptFeature_Solution")) %>% 
  as_tibble() %>% 
  left_join(., LowRegret_SummaryAll, by = "approach")

# Cost + Area
ggComparison_Cost_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "log10(total_cost)", y_axis = "log10(cost)") + theme(axis.text = element_text(size = 25))
ggComparison_Area_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "percent_area", y_axis = "% area")  + theme(axis.text = element_text(size = 25))
ggComparison_Cost_LowRegret + ggComparison_Area_LowRegret + plot_layout(guides = "collect")
# Climate warming
ggComparison_Warming_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_climate_warming", y_axis = expression('Δ'^"o"*'C yr'^"-1"*''))
ggComparison_LogWarming_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_log_climate_warming", y_axis = expression('log Δ'^"o"*'C yr'^"-1"*'')) + scale_y_reverse()
ggComparison_Warming_LowRegret + ggComparison_LogWarming_LowRegret + plot_layout(guides = "collect")
# Ocean Acidification
ggComparison_OceanAcidification_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_ocean_acidification", y_axis = expression('Δ pH yr'^"-1"*'')) + scale_y_reverse()
ggComparison_LogOceanAcidification_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_log_ocean_acidification", y_axis = expression('log Δ pH yr'^"-1"*'')) + scale_y_reverse()
ggComparison_OceanAcidification_LowRegret + ggComparison_LogOceanAcidification_LowRegret + plot_layout(guides = "collect")
# Oxygen Decline
ggComparison_OxygenDecline_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_oxygen_decline", y_axis = expression('Δ mol m'^"-3"*' yr'^"-1"*'')) + scale_y_reverse()
ggComparison_LogOxygenDecline_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_log_oxygen_decline", y_axis = expression('Δ mol m'^"-3"*' yr'^"-1"*'')) + scale_y_reverse()
ggComparison_OxygenDecline_LowRegret + ggComparison_LogOxygenDecline_LowRegret + plot_layout(guides = "collect")
# Climate Velocity
ggComparison_ClimateVelocity_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "median_velocity", y_axis = expression('km yr'^"-1"*''))
ggComparison_LogClimateVelocity_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_log_velocity", y_axis = expression('log km yr'^"-1"*''))
ggComparison_ClimateVelocity_LowRegret + ggComparison_LogClimateVelocity_LowRegret + plot_layout(guides = "collect")

#' Create Low-Regret Areas across all Low-Regret Spatial Plans for the three approaches
LowRegret_sf <- LowRegretAll_sf %>% 
  dplyr::rename(solution_1 = SelectionAll) %>% 
  dplyr::mutate(solution_1 = as.logical(solution_1))

#' Plot the spatial design
(ggLowRegret_All <- fSpatPlan_PlotSolution(LowRegret_sf, PUs, land) + ggtitle("Low-Regret Areas", subtitle = "Across all approaches") + theme(axis.text = element_text(size = 25)))

#' Check the summary
df <- cbind(LowRegret_sf, UniformCost)
summary_lr <- compute_summary(df, total_area, PU_size, run_name = "LowRegret_All", Cost = "cost")
print(summary_lr)

#### Exploring Different climate scenarios ####

#' SSP 1-2.6; Percentile (Climate Warming)
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those <= 35th percentile.
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP126, PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, roc_tos_SSP126, UniformCost)
p38 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s38 <- prioritizr::solve(p38)

#' 5. Plot the spatial design
s38_plot <- s38 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol38 <- fSpatPlan_PlotSolution(s38_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 1-2.6") + theme(axis.text = element_text(size = 25)))

#' SSP 2-4.5; Percentile (Climate Warming)
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those <= 35th percentile.
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP245, PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, roc_tos_SSP245, UniformCost)
p39 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s39 <- prioritizr::solve(p39)

#' 5. Plot the spatial design
s39_plot <- s39 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol39 <- fSpatPlan_PlotSolution(s39_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 2-4.5") + theme(axis.text = element_text(size = 25)))

# Feature representation
list <- c("percentile_tos_126", "percentile_tos_245")
problem_list <- list(p38, p39)
solution_list <- list(s38, s39)
for (i in 1:length(list)) {
  tmp_df <- represent_feature(problem_list[[i]], solution_list[[i]], list[i])
  feat_rep <- left_join(tmp_df, feat_rep)
}

df <- compute_summary(solution_list[[1]], total_area, PU_size, run_name = list[1], Cost = "cost") %>% 
  rbind(., compute_summary(solution_list[[2]], total_area, PU_size, run_name = list[2], Cost = "cost"))

summary %<>% add_row(df)

#' Get Kappa Correlation Matrix
list <- c("percentile_tos_126", "percentile_tos_245", "percentile_tos_585")
object_list <- list() # empty list
solution_list <- list(s38, s39, s2)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#' ### Low-regret Areas
#' To create low-regret climate-smart design, we should only select areas that have been selected for all climate-smart designs utilizing different climate scenarios
# Select solutions for all climate-smart designs
solution_list <- list(s38, s39, s2)
col_names <- c("penalty_tos_126", "penalty_tos_245", "penalty_tos_585")
LowRegret_ClimateScenario <- create_LowRegretSf(solution_list, col_names, PUs)

(gg_LowRegretClimateScenario <- plot_lowregret(LowRegret_ClimateScenario, land) + theme(axis.text = element_text(size = 25)))

#' Check low-regret summary
LowRegret_SummaryClimateScenario <- compute_summary(LowRegret_ClimateScenario, total_area, PU_size, "low_regret", Cost = "cost") %>% 
  mutate(approach = "percentile", scenario = 126245585)
print(LowRegret_SummaryClimateScenario)

LowRegretSummary <- read_csv("Output/summary/LowRegret_summary_statistics.csv") %>% 
  dplyr::select(-X1) 
LowRegretSummary %<>% 
  add_row(., LowRegret_SummaryClimateScenario)

ggSummary_Area <- plot_statistics(summary %>% filter(run %in% c("percentile_tos_126", "percentile_tos_245", "percentile_tos_585") ), col_name = "percent_area", y_axis = "% area", color = 3) + theme(axis.text = element_text(size = 25))


#### Research Question 4 ####
#' ## Research Question 4
#' ### What are the pros and cons of the different climate-smart marine reserve design approaches explored here?

#' ### Graph summary of plots
df <- summary %>% 
  dplyr::mutate(metric = case_when(str_detect(run, pattern = "tos") ~ "tos",
                                   str_detect(run, pattern = "phos") ~ "phos",
                                   str_detect(run, pattern = "o2os") ~ "o2os",
                                   str_detect(run, pattern = "velocity") ~ "velocity",
                                   str_detect(run, pattern = "uninformed") ~ "uninformed")) %>% 
  dplyr::mutate(approach = ifelse(str_detect(run, pattern = "uninformed"), yes = "uninformed", no = approach))

# Cost + Area
ggComparison_Cost <- plot_ComparisonStatistics(df, col_name = "log10(total_cost)", y_axis = "log10(cost)")
ggComparison_Area <- plot_ComparisonStatistics(df, col_name = "percent_area", y_axis = "% area")
ggComparison_Cost + ggComparison_Area + plot_layout(guides = "collect")
# Climate warming
ggComparison_Warming <- plot_ComparisonStatistics(df, col_name = "mean_climate_warming", y_axis = expression('Δ'^"o"*'C yr'^"-1"*''))
ggComparison_LogWarming <- plot_ComparisonStatistics(df, col_name = "mean_log_climate_warming", y_axis = expression('log Δ'^"o"*'C yr'^"-1"*'')) + scale_y_reverse()
ggComparison_Warming + ggComparison_LogWarming + plot_layout(guides = "collect")
# Ocean Acidification
ggComparison_OceanAcidification <- plot_ComparisonStatistics(df, col_name = "mean_ocean_acidification", y_axis = expression('Δ pH yr'^"-1"*'')) + scale_y_reverse()
ggComparison_LogOceanAcidification <- plot_ComparisonStatistics(df, col_name = "mean_log_ocean_acidification", y_axis = expression('log Δ pH yr'^"-1"*'')) + scale_y_reverse()
ggComparison_OceanAcidification + ggComparison_LogOceanAcidification + plot_layout(guides = "collect")
# Oxygen Decline
ggComparison_OxygenDecline <- plot_ComparisonStatistics(df, col_name = "mean_oxygen_decline", y_axis = expression('Δ mol m'^"-3"*' yr'^"-1"*'')) + scale_y_reverse()
ggComparison_LogOxygenDecline <- plot_ComparisonStatistics(df, col_name = "mean_log_oxygen_decline", y_axis = expression('Δ mol m'^"-3"*' yr'^"-1"*'')) + scale_y_reverse()
ggComparison_OxygenDecline + ggComparison_LogOxygenDecline + plot_layout(guides = "collect")
# Climate Velocity
ggComparison_ClimateVelocity <- plot_ComparisonStatistics(df, col_name = "median_velocity", y_axis = expression('km yr'^"-1"*''))
ggComparison_LogClimateVelocity <- plot_ComparisonStatistics(df, col_name = "mean_log_velocity", y_axis = expression('log km yr'^"-1"*''))
ggComparison_ClimateVelocity + ggComparison_LogClimateVelocity + plot_layout(guides = "collect")

#' ### Compare Low Regret Areas
ggLowRegret_Feature <- plot_lowregret(LowRegret_Feature, land) + labs(subtitle = "Feature, SSP 5-8.5")
ggLowRegret_Percentile <- plot_lowregret(LowRegret_Percentile, land) + labs(subtitle = "Percentile, SSP 5-8.5")
ggLowRegret_Penalty <- plot_lowregret(LowRegret_Penalty, land) + labs(subtitle = "Penalty, SSP 5-8.5")

ggLowRegret_Feature + ggLowRegret_Percentile + ggLowRegret_Penalty + plot_layout(guides = "collect")

LowRegret_SummaryAll <- LowRegret_SummaryFeature %>% 
  rbind(., LowRegret_SummaryPercentile) %>% 
  rbind(., LowRegret_SummaryPenalty)

# Kappa
list <- c("LowRegret_Feature_585", "LowRegret_Percentile_585", "LowRegret_Penalty_585")
object_list <- list() # empty list
solution_list <- list(LowRegret_Feature, LowRegret_Percentile, LowRegret_Penalty)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

# Check the Climate Metrics of the Low-Regret Areas
LowRegretFeature_df <- LowRegret_Feature %>% 
  as_tibble() %>% 
  dplyr::select(-feature_velocity_585, -feature_o2os_585, -feature_phos_585, -feature_tos_585, -cost) %>%
  dplyr::rename(SelectionFeature = selection) %>% 
  left_join(., roc_tos_SSP585 %>% as_tibble() %>% dplyr::select(slpTrends, geometry), by = "geometry") %>% 
  dplyr::rename(tos = slpTrends, Feature_Solution = solution_1) %>% 
  left_join(., roc_phos_SSP585 %>% as_tibble() %>% dplyr::select(slpTrends, geometry), by = "geometry") %>% 
  dplyr::rename(phos = slpTrends) %>% 
  left_join(., roc_o2os_SSP585 %>% as_tibble() %>% dplyr::select(slpTrends, geometry), by = "geometry") %>% 
  dplyr::rename(o2os = slpTrends) %>% 
  left_join(., velocity_SSP585 %>% as_tibble() %>% dplyr::select(voccMag, geometry), by = "geometry") %>% 
  dplyr::rename(velocity = voccMag)
  
LowRegretPercentile_df <- LowRegret_Percentile %>% 
  as_tibble() %>% 
  dplyr::select(-percentile_velocity_585, -percentile_o2os_585, -percentile_phos_585, -percentile_tos_585, -cost) %>% 
  dplyr::rename(SelectionPercentile = selection, Percentile_Solution = solution_1)

LowRegretPenalty_df <- LowRegret_Penalty %>% 
  as_tibble() %>% 
  dplyr::select(-penalty_velocity_585, -penalty_o2os_585, -penalty_phos_585, -penalty_tos_585, -cost) %>%
  dplyr::rename(SelectionPenalty = selection, Penalty_Solution = solution_1)

LowRegretAll_sf <- left_join(LowRegretFeature_df, LowRegretPercentile_df, by = c("cellID", "geometry")) %>% 
  left_join(., LowRegretPenalty_df, by = c("cellID", "geometry")) %>% 
  dplyr::mutate(SelectionAll = case_when((SelectionFeature == 4 & SelectionPercentile == 4 & SelectionPenalty == 4) ~ 1,
                                         TRUE ~ 0)) %>% 
  st_as_sf(sf_column_name = "geometry")

LowRegret_SummaryAll <- lowRegret_ClimateSummary(df = LowRegretAll_sf, approach_column = "Feature_Solution") %>% 
  rbind(., lowRegret_ClimateSummary(df = LowRegretAll_sf, approach_column = "Percentile_Solution")) %>% 
  rbind(., lowRegret_ClimateSummary(df = LowRegretAll_sf, approach_column = "Penalty_Solution")) %>% 
  as_tibble() %>% 
  left_join(., LowRegret_SummaryAll, by = "approach")

# Cost + Area
ggComparison_Cost_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "log10(total_cost)", y_axis = "log10(cost)") + theme(axis.text = element_text(size = 25))
ggComparison_Area_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "percent_area", y_axis = "% area")  + theme(axis.text = element_text(size = 25))
ggComparison_Cost_LowRegret + ggComparison_Area_LowRegret + plot_layout(guides = "collect")
# Climate warming
ggComparison_Warming_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_climate_warming", y_axis = expression('Δ'^"o"*'C yr'^"-1"*''))
ggComparison_LogWarming_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_log_climate_warming", y_axis = expression('log Δ'^"o"*'C yr'^"-1"*'')) + scale_y_reverse()
ggComparison_Warming_LowRegret + ggComparison_LogWarming_LowRegret + plot_layout(guides = "collect")
# Ocean Acidification
ggComparison_OceanAcidification_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_ocean_acidification", y_axis = expression('Δ pH yr'^"-1"*'')) + scale_y_reverse()
ggComparison_LogOceanAcidification_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_log_ocean_acidification", y_axis = expression('log Δ pH yr'^"-1"*'')) + scale_y_reverse()
ggComparison_OceanAcidification_LowRegret + ggComparison_LogOceanAcidification_LowRegret + plot_layout(guides = "collect")
# Oxygen Decline
ggComparison_OxygenDecline_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_oxygen_decline", y_axis = expression('Δ mol m'^"-3"*' yr'^"-1"*'')) + scale_y_reverse()
ggComparison_LogOxygenDecline_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_log_oxygen_decline", y_axis = expression('Δ mol m'^"-3"*' yr'^"-1"*'')) + scale_y_reverse()
ggComparison_OxygenDecline_LowRegret + ggComparison_LogOxygenDecline_LowRegret + plot_layout(guides = "collect")
# Climate Velocity
ggComparison_ClimateVelocity_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "median_velocity", y_axis = expression('km yr'^"-1"*''))
ggComparison_LogClimateVelocity_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_log_velocity", y_axis = expression('log km yr'^"-1"*''))
ggComparison_ClimateVelocity_LowRegret + ggComparison_LogClimateVelocity_LowRegret + plot_layout(guides = "collect")

#' Create Low-Regret Areas across all Low-Regret Spatial Plans for the three approaches
LowRegret_sf <- LowRegretAll_sf %>% 
  dplyr::rename(solution_1 = SelectionAll) %>% 
  dplyr::mutate(solution_1 = as.logical(solution_1))

#' Plot the spatial design
(ggLowRegret_All <- fSpatPlan_PlotSolution(LowRegret_sf, PUs, land) + ggtitle("Low-Regret Areas", subtitle = "Across all approaches") + theme(axis.text = element_text(size = 25)))

#' Check the summary
df <- cbind(LowRegret_sf, UniformCost)
summary_lr <- compute_summary(df, total_area, PU_size, run_name = "LowRegret_All", Cost = "cost")
print(summary_lr)

#### Ensemble variability approach ####
#' ## Ensemble Variability approach

# Temperature
#' Call climate layers to be used here (different for each model)
inpdir <- "Data/Climate/ClimateMetrics_Ensemble/tos/SSP 5-8.5/"
file_list <- list.files(inpdir)

for (i in 1:length(file_list)) {
  cCRS <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  save_name <- unlist(str_split(file_list[i], pattern = "_"))[3]
  
  file <- readRDS(paste0(inpdir, file_list[i]))
  climate_layer <- fSpatPlan_Get_ClimateLayer(ClimateLayer = file, PUs, cCRS, metric = "roc_tos_ensemble")
  
  assign(x = paste0("tos_", save_name), value = climate_layer)
}

#' Save all of them as climate layers
list <- list(tos_CanESM5, `tos_CMCC-ESM2`, `tos_GFDL-ESM4`, `tos_IPSL-CM6A-LR`, `tos_NorESM2-MM`)
name_list <- c("roc_tos_SSP 5-8.5_CanESM5_ensemble.rds", "roc_tos_SSP 5-8.5_CMCC-ESM2_ensemble.rds",
               "roc_tos_SSP 5-8.5_GFDL-ESM4_ensemble.rds", "roc_tos_SSP 5-8.5_IPSL-CM6A-LR_ensemble.rds",
               "roc_tos_SSP 5-8.5_NorESM2-MM_ensemble.rds")

for (i in 1:length(list)){
  saveRDS(list[[i]], file.path("Output",
                           paste(save_name, "PU", paste0(PU_size, "km2"),
                                 name_list[i], sep = "_")))
}

#' If already done the saving above, just call the files
tos_CanESM5 <- readRDS("Output/WestPacific_PU_669.9km2_roc_tos_SSP 5-8.5_CanESM5_ensemble.rds")
`tos_CMCC-ESM2`<- readRDS("Output/WestPacific_PU_669.9km2_roc_tos_SSP 5-8.5_CMCC-ESM2_ensemble.rds")
`tos_GFDL-ESM4` <- readRDS("Output/WestPacific_PU_669.9km2_roc_tos_SSP 5-8.5_GFDL-ESM4_ensemble.rds")
`tos_IPSL-CM6A-LR` <- readRDS("Output/WestPacific_PU_669.9km2_roc_tos_SSP 5-8.5_IPSL-CM6A-LR_ensemble.rds")
`tos_NorESM2-MM` <- readRDS("Output/WestPacific_PU_669.9km2_roc_tos_SSP 5-8.5_NorESM2-MM_ensemble.rds")

#' Start the runs for all models with the following parameters:
#' 1. Models forced under SSP 5-8.5
#' 2. "Percentile" approach

#' ### CanESM5
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those <= median (50th percentile). 
ensemble <- list(`tos_CanESM5`, `tos_CMCC-ESM2`, `tos_GFDL-ESM4`, `tos_IPSL-CM6A-LR`, `tos_NorESM2-MM`)
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "slpTrends", metric_df = ensemble[[1]], PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, tos_CanESM5, UniformCost)
p14 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s14 <- prioritizr::solve(p14)

#' 5. Plot the spatial design
s14_plot <- s14 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol14 <- fSpatPlan_PlotSolution(s14_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5 (GCM: CanESM5)"))

#' ### CMCC-ESM2
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those <= 35th percentile).
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "slpTrends", metric_df = ensemble[[2]], PUs = PUs)

#' 2. Get list of features
# same list of features as above

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, `tos_CMCC-ESM2`, UniformCost)
p15 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s15 <- prioritizr::solve(p15)

#' 5. Plot the spatial design
s15_plot <- s15 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol15 <- fSpatPlan_PlotSolution(s15_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5 (GCM: CMCC-ESM2)"))

#' ### GFDL-ESM4
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those <= 35th percentile).
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "slpTrends", metric_df = ensemble[[3]], PUs = PUs)

#' 2. Get list of features
# same list of features as above

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, `tos_GFDL-ESM4`, UniformCost)
p16 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s16 <- prioritizr::solve(p16)

#' 5. Plot the spatial design
s16_plot <- s16 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol16 <- fSpatPlan_PlotSolution(s16_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5 (GCM: GFDL-ESM4)"))

#' ### IPSL-CM6A-LR
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those <= 35th percentile).
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "slpTrends", metric_df = ensemble[[4]], PUs = PUs)

#' 2. Get list of features
# same list of features as above

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, `tos_IPSL-CM6A-LR`, UniformCost)
p17 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s17 <- prioritizr::solve(p17)

#' 5. Plot the spatial design
s17_plot <- s17 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol17 <- fSpatPlan_PlotSolution(s17_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5 (GCM: IPSL-CM6A-LR)"))

#' ### NorESM2-MM
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those <= median (50th percentile).
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "slpTrends", metric_df = ensemble[[5]], PUs = PUs)

#' 2. Get list of features
# same list of features as above

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, `tos_NorESM2-MM`, UniformCost)
p18 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s18 <- prioritizr::solve(p18)

#' 5. Plot the spatial design
s18_plot <- s18 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol18 <- fSpatPlan_PlotSolution(s18_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5 (GCM: NorESM2-MM)"))

#' Create selection frequency of these
solution_list <- list(s14, s15, s16, s17, s18)
col_names <- c("tos_CanESM5", "tos_CMCC-ESM2", "tos_GFDL-ESM4", "tos_IPSL-CM6A-LR", "tos_NorESM2-MM")
Selection_tosEnsemble_Frequency <- create_LowRegretSf(solution_list, col_names, PUs)

(gg_Selection_tosEnsemble_Frequency <- plot_SelectionFrequency(Selection_tosEnsemble_Frequency, land) + theme(axis.text = element_text(size = 25)))


# Kappa
list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
object_list <- list() # empty list
solution_list <- list(s14, s15, s16, s17, s18)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

## ph (Rate of ocean acidification)

#' Call climate layers to be used here (different for each model)
inpdir <- "Data/Climate/ClimateMetrics_Ensemble/phos/SSP 5-8.5/"
file_list <- list.files(inpdir)

for (i in 1:length(file_list)) {
  cCRS <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  save_name <- unlist(str_split(file_list[i], pattern = "_"))[3]
  
  file <- readRDS(paste0(inpdir, file_list[i]))
  climate_layer <- fSpatPlan_Get_ClimateLayer(ClimateLayer = file, PUs, cCRS, metric = "roc_phos_ensemble")
  
  assign(x = paste0("phos_", save_name), value = climate_layer)
}

#' Save all of them as climate layers
list <- list(phos_CanESM5, `phos_CMCC-ESM2`, `phos_GFDL-ESM4`, `phos_IPSL-CM6A-LR`, `phos_NorESM2-MM`)
name_list <- c("roc_phos_SSP 5-8.5_CanESM5_ensemble.rds", "roc_phos_SSP 5-8.5_CMCC-ESM2_ensemble.rds",
               "roc_phos_SSP 5-8.5_GFDL-ESM4_ensemble.rds", "roc_phos_SSP 5-8.5_IPSL-CM6A-LR_ensemble.rds",
               "roc_phos_SSP 5-8.5_NorESM2-MM_ensemble.rds")

for (i in 1:length(list)){
  saveRDS(list[[i]], file.path("Output",
                               paste(save_name, "PU", paste0(PU_size, "km2"),
                                     name_list[i], sep = "_")))
}

#' If already done the saving above, just call the files
phos_CanESM5 <- readRDS("Output/WestPacific_PU_669.9km2_roc_phos_SSP 5-8.5_CanESM5_ensemble.rds")
`phos_CMCC-ESM2`<- readRDS("Output/WestPacific_PU_669.9km2_roc_phos_SSP 5-8.5_CMCC-ESM2_ensemble.rds")
`phos_GFDL-ESM4` <- readRDS("Output/WestPacific_PU_669.9km2_roc_phos_SSP 5-8.5_GFDL-ESM4_ensemble.rds")
`phos_IPSL-CM6A-LR` <- readRDS("Output/WestPacific_PU_669.9km2_roc_phos_SSP 5-8.5_IPSL-CM6A-LR_ensemble.rds")
`phos_NorESM2-MM` <- readRDS("Output/WestPacific_PU_669.9km2_roc_phos_SSP 5-8.5_NorESM2-MM_ensemble.rds")

#' ### CanESM5
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those >= 65th percentile). 
ensemble <- list(`phos_CanESM5`, `phos_CMCC-ESM2`, `phos_GFDL-ESM4`, `phos_IPSL-CM6A-LR`, `phos_NorESM2-MM`)
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "phos", colname = "slpTrends", metric_df = ensemble[[1]], PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, phos_CanESM5, UniformCost)
p19 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s19 <- prioritizr::solve(p19)

#' 5. Plot the spatial design
s19_plot <- s19 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol19 <- fSpatPlan_PlotSolution(s19_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 5-8.5 (GCM: CanESM5)"))

#' ### CMCC-ESM2
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those >= 65th percentile). 
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "phos", colname = "slpTrends", metric_df = ensemble[[2]], PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, `phos_CMCC-ESM2`, UniformCost)
p20 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s20 <- prioritizr::solve(p20)

#' 5. Plot the spatial design
s20_plot <- s20 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol20 <- fSpatPlan_PlotSolution(s20_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 5-8.5 (GCM: CMCC-ESM2)"))

#' ### GFDL-ESM4
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those >= 65th percentile). 
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "phos", colname = "slpTrends", metric_df = ensemble[[3]], PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, `phos_GFDL-ESM4`, UniformCost)
p21 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s21 <- prioritizr::solve(p21)

#' 5. Plot the spatial design
s21_plot <- s21 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol21 <- fSpatPlan_PlotSolution(s21_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 5-8.5 (GCM: GFDL-ESM4)"))

#' ### IPSL-CM6A-LR
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those >= 65th percentile). 
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "phos", colname = "slpTrends", metric_df = ensemble[[4]], PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, `phos_IPSL-CM6A-LR`, UniformCost)
p22 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s22 <- prioritizr::solve(p22)

#' 5. Plot the spatial design
s22_plot <- s22 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol22 <- fSpatPlan_PlotSolution(s22_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 5-8.5 (GCM: IPSL-CM6A-LR)"))

#' ### NorESM2-MM
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those >= 65th percentile). 
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "phos", colname = "slpTrends", metric_df = ensemble[[5]], PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, `phos_NorESM2-MM`, UniformCost)
p23 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s23 <- prioritizr::solve(p23)

#' 5. Plot the spatial design
s23_plot <- s23 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol23 <- fSpatPlan_PlotSolution(s23_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 5-8.5 (GCM: NorESM2-MM)"))

#' Create selection frequency of these
solution_list <- list(s19, s20, s21, s22, s23)
col_names <- c("phos_CanESM5", "phos_CMCC-ESM2", "phos_GFDL-ESM4", "phos_IPSL-CM6A-LR", "phos_NorESM2-MM")
Selection_phosEnsemble_Frequency <- create_LowRegretSf(solution_list, col_names, PUs)

(gg_Selection_phosEnsemble_Frequency <- plot_SelectionFrequency(Selection_phosEnsemble_Frequency, land) + theme(axis.text = element_text(size = 25)))

# Kappa
list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
object_list <- list() # empty list
solution_list <- list(s19, s20, s21, s22, s23)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

## o2 (Rate of Declining Oxygen Concentration)

#' Call climate layers to be used here (different for each model)
inpdir <- "Data/Climate/ClimateMetrics_Ensemble/o2os/SSP 5-8.5/"
file_list <- list.files(inpdir)

for (i in 1:length(file_list)) {
  cCRS <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  save_name <- unlist(str_split(file_list[i], pattern = "_"))[3]
  
  file <- readRDS(paste0(inpdir, file_list[i]))
  climate_layer <- fSpatPlan_Get_ClimateLayer(ClimateLayer = file, PUs, cCRS, metric = "roc_o2os_ensemble")
  
  assign(x = paste0("o2os_", save_name), value = climate_layer)
}

#' Save all of them as climate layers
list <- list(o2os_CanESM5, `o2os_CMCC-ESM2`, `o2os_GFDL-ESM4`, `o2os_IPSL-CM6A-LR`, `o2os_NorESM2-MM`)
name_list <- c("roc_o2os_SSP 5-8.5_CanESM5_ensemble.rds", "roc_o2os_SSP 5-8.5_CMCC-ESM2_ensemble.rds",
               "roc_o2os_SSP 5-8.5_GFDL-ESM4_ensemble.rds", "roc_o2os_SSP 5-8.5_IPSL-CM6A-LR_ensemble.rds",
               "roc_o2os_SSP 5-8.5_NorESM2-MM_ensemble.rds")

for (i in 1:length(list)){
  save_name = "WestPacific"
  saveRDS(list[[i]], file.path("Output",
                               paste(save_name, "PU", paste0(PU_size, "km2"),
                                     name_list[i], sep = "_")))
}

#' If already done the saving above, just call the files
o2os_CanESM5 <- readRDS("Output/WestPacific_PU_669.9km2_roc_o2os_SSP 5-8.5_CanESM5_ensemble.rds")
`o2os_CMCC-ESM2`<- readRDS("Output/WestPacific_PU_669.9km2_roc_o2os_SSP 5-8.5_CMCC-ESM2_ensemble.rds")
`o2os_GFDL-ESM4` <- readRDS("Output/WestPacific_PU_669.9km2_roc_o2os_SSP 5-8.5_GFDL-ESM4_ensemble.rds")
`o2os_IPSL-CM6A-LR` <- readRDS("Output/WestPacific_PU_669.9km2_roc_o2os_SSP 5-8.5_IPSL-CM6A-LR_ensemble.rds")
`o2os_NorESM2-MM` <- readRDS("Output/WestPacific_PU_669.9km2_roc_o2os_SSP 5-8.5_NorESM2-MM_ensemble.rds")

#' ### CanESM5
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those >= 65th percentile). 
ensemble <- list(`o2os_CanESM5`, `o2os_CMCC-ESM2`, `o2os_GFDL-ESM4`, `o2os_IPSL-CM6A-LR`, `o2os_NorESM2-MM`)
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "slpTrends", metric_df = ensemble[[1]], PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, o2os_CanESM5, UniformCost)
p24 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s24 <- prioritizr::solve(p24)

#' 5. Plot the spatial design
s24_plot <- s24 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol24 <- fSpatPlan_PlotSolution(s24_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Percentile, SSP 5-8.5 (GCM: CanESM5)"))

#' ### CMCC-ESM2
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those >= 65th percentile). 
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "slpTrends", metric_df = ensemble[[2]], PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, `o2os_CMCC-ESM2`, UniformCost)
p25 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s25 <- prioritizr::solve(p25)

#' 5. Plot the spatial design
s25_plot <- s25 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol25 <- fSpatPlan_PlotSolution(s25_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Percentile, SSP 5-8.5 (GCM: CMCC-ESM2)"))

#' ### GFDL-ESM4
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those >= 65th percentile). 
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "slpTrends", metric_df = ensemble[[3]], PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, `o2os_GFDL-ESM4`, UniformCost)
p26 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s26 <- prioritizr::solve(p26)

#' 5. Plot the spatial design
s26_plot <- s26 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol26 <- fSpatPlan_PlotSolution(s26_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Percentile, SSP 5-8.5 (GCM: GFDL-ESM4)"))

#' ### IPSL-CM6A-LR
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those >= 65th percentile). 
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "slpTrends", metric_df = ensemble[[4]], PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, `o2os_IPSL-CM6A-LR`, UniformCost)
p27 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s27 <- prioritizr::solve(p27)

#' 5. Plot the spatial design
s27_plot <- s27 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol27 <- fSpatPlan_PlotSolution(s27_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Percentile, SSP 5-8.5 (GCM: IPSL-CM6A-LR)"))

#' ### NorESM2-MM
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those >= 65th percentile). 
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "slpTrends", metric_df = ensemble[[5]], PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, `o2os_NorESM2-MM`, UniformCost)
p28 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s28 <- prioritizr::solve(p28)

#' 5. Plot the spatial design
s28_plot <- s28 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol28 <- fSpatPlan_PlotSolution(s28_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Percentile, SSP 5-8.5 (GCM: NorESM2-MM)"))

#' Create selection frequency of these
solution_list <- list(s24, s25, s26, s27, s28)
col_names <- c("o2os_CanESM5", "o2os_CMCC-ESM2", "o2os_GFDL-ESM4", "o2os_IPSL-CM6A-LR", "o2os_NorESM2-MM")
Selection_o2osEnsemble_Frequency <- create_LowRegretSf(solution_list, col_names, PUs)

(gg_Selection_o2osEnsemble_Frequency <- plot_SelectionFrequency(Selection_o2osEnsemble_Frequency, land) + theme(axis.text = element_text(size = 25)))

# Kappa
list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
object_list <- list() # empty list
solution_list <- list(s24, s25, s26, s27, s28)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

## velocity

#' Call climate layers to be used here (different for each model)
inpdir <- "Data/Climate/ClimateMetrics_Ensemble/velocity/SSP 5-8.5/"
file_list <- list.files(inpdir)

for (i in 1:length(file_list)) {
  cCRS <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  save_name <- unlist(str_split(file_list[i], pattern = "_"))[2]
  
  file <- readRDS(paste0(inpdir, file_list[i]))
  climate_layer <- fSpatPlan_Get_ClimateLayer(ClimateLayer = file, PUs, cCRS, metric = "velocity_ensemble")
  
  assign(x = paste0("velocity_", save_name), value = climate_layer)
}

#' Save all of them as climate layers
list <- list(velocity_CanESM5, `velocity_CMCC-ESM2`, `velocity_GFDL-ESM4`, `velocity_IPSL-CM6A-LR`, `velocity_NorESM2-MM`)
name_list <- c("velocity_SSP 5-8.5_CanESM5_ensemble.rds", "velocity_SSP 5-8.5_CMCC-ESM2_ensemble.rds",
               "velocity_SSP 5-8.5_GFDL-ESM4_ensemble.rds", "velocity_SSP 5-8.5_IPSL-CM6A-LR_ensemble.rds",
               "velocity_SSP 5-8.5_NorESM2-MM_ensemble.rds")

for (i in 1:length(list)){
  saveRDS(list[[i]], file.path("Output",
                               paste(save_name, "PU", paste0(PU_size, "km2"),
                                     name_list[i], sep = "_")))
}

#' If already done the saving above, just call the files
velocity_CanESM5 <- readRDS("Output/WestPacific_PU_669.9km2_velocity_SSP 5-8.5_CanESM5_ensemble.rds")
`velocity_CMCC-ESM2`<- readRDS("Output/WestPacific_PU_669.9km2_velocity_SSP 5-8.5_CMCC-ESM2_ensemble.rds")
`velocity_GFDL-ESM4` <- readRDS("Output/WestPacific_PU_669.9km2_velocity_SSP 5-8.5_GFDL-ESM4_ensemble.rds")
`velocity_IPSL-CM6A-LR` <- readRDS("Output/WestPacific_PU_669.9km2_velocity_SSP 5-8.5_IPSL-CM6A-LR_ensemble.rds")
`velocity_NorESM2-MM` <- readRDS("Output/WestPacific_PU_669.9km2_velocity_SSP 5-8.5_NorESM2-MM_ensemble.rds")


#' ### CanESM5
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those >= 65th percentile). 
ensemble <- list(`velocity_CanESM5`, `velocity_CMCC-ESM2`, `velocity_GFDL-ESM4`, `velocity_IPSL-CM6A-LR`, `velocity_NorESM2-MM`)
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "voccMag", metric_df = ensemble[[1]], PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, velocity_CanESM5, UniformCost)
p29 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s29 <- prioritizr::solve(p29)

#' 5. Plot the spatial design
s29_plot <- s29 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol29 <- fSpatPlan_PlotSolution(s29_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 5-8.5 (GCM: CanESM5)"))

#' ### CMCC-ESM2
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those >= 65th percentile). 
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "voccMag", metric_df = ensemble[[2]], PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, `velocity_CMCC-ESM2`, UniformCost)
p30 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s30 <- prioritizr::solve(p30)

#' 5. Plot the spatial design
s30_plot <- s30 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol30 <- fSpatPlan_PlotSolution(s30_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 5-8.5 (GCM: CMCC-ESM2)"))

#' ### GFDL-ESM4
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those >= 65th percentile). 
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "voccMag", metric_df = ensemble[[3]], PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, `velocity_GFDL-ESM4`, UniformCost)
p31 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s31 <- prioritizr::solve(p31)

#' 5. Plot the spatial design
s31_plot <- s31 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol31 <- fSpatPlan_PlotSolution(s31_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 5-8.5 (GCM: GFDL-ESM4)"))

#' ### IPSL-CM6A-LR
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those <= 35th percentile). 
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "voccMag", metric_df = ensemble[[4]], PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, `velocity_IPSL-CM6A-LR`, UniformCost)
p32 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s32 <- prioritizr::solve(p32)

#' 5. Plot the spatial design
s32_plot <- s32 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol32 <- fSpatPlan_PlotSolution(s32_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 5-8.5 (GCM: IPSL-CM6A-LR)"))

#' ### NorESM2-MM
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those <= 35th percentile). 
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "voccMag", metric_df = ensemble[[5]], PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, `velocity_NorESM2-MM`, UniformCost)
p33 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s33 <- prioritizr::solve(p33)

#' 5. Plot the spatial design
s33_plot <- s33 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol33 <- fSpatPlan_PlotSolution(s33_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 5-8.5 (GCM: NorESM2-MM)"))

#' Create selection frequency of these
solution_list <- list(s29, s30, s31, s32, s33)
col_names <- c("velocity_CanESM5", "velocity_CMCC-ESM2", "velocity_GFDL-ESM4", "velocity_IPSL-CM6A-LR", "velocity_NorESM2-MM")
Selection_velocityEnsemble_Frequency <- create_LowRegretSf(solution_list, col_names, PUs)

(gg_Selection_velocityEnsemble_Frequency <- plot_SelectionFrequency(Selection_velocityEnsemble_Frequency, land) + theme(axis.text = element_text(size = 25)))

# Kappa
list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
object_list <- list() # empty list
solution_list <- list(s29, s30, s31, s32, s33)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

#### Creating some plots explaining the different approaches ####
aqm_subset1 <- aqua_sf %>% dplyr::select(colnames(aqua_sf)[4289]) %>% 
  dplyr::mutate(Katsuwonus_pelamis = as.logical(Katsuwonus_pelamis))

aqm1_Plot <- plot_AQMFeatures(aqm_subset1, PUs, land, column = "Katsuwonus_pelamis") + ggtitle("Species Distribution #1", subtitle = "Katsuwonus pelamis") + theme(axis.text = element_text(size = 25))

aqm1_percentile <- create_PercentileLayer(aqua_sf = aqm_subset1, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP585, PUs = PUs) %>% 
  dplyr::mutate(Katsuwonus_pelamis = as.logical(Katsuwonus_pelamis))

aqm1_PercentilePlot <- plot_AQMFeatures(aqm1_percentile, PUs, land, column = "Katsuwonus_pelamis") + ggtitle("Species Distribution #1", subtitle = "Katsuwonus pelamis") + theme(axis.text = element_text(size = 25))


aqm_subset2 <- aqua_sf %>% dplyr::select(colnames(aqua_sf)[8199]) %>% 
  dplyr::mutate(Thunnus_orientalis = as.logical(Thunnus_orientalis)) 

aqm2_Plot <- plot_AQMFeatures(aqm_subset2, PUs, land, column = "Thunnus_orientalis") + ggtitle("Species Distribution #1", subtitle = "Thunnus orientalis") + theme(axis.text = element_text(size = 25))

aqm2_percentile <- create_PercentileLayer(aqua_sf = aqm_subset2, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP585, PUs = PUs) %>% 
  dplyr::mutate(Thunnus_orientalis = as.logical(Thunnus_orientalis))

aqm2_PercentilePlot <- plot_AQMFeatures(aqm2_percentile, PUs, land, column = "Thunnus_orientalis") + ggtitle("Species Distribution #1", subtitle = "Thunnus_orientalis") + theme(axis.text = element_text(size = 25))

### Features
gg_roc_tos_SSP585 <- fSpatPlan_PlotClimate(roc_tos_SSP585, land, metric = "roc_tos", from = 0.02, to = 0.05) + theme(axis.text = element_text(size = 25))

ClimateFeature <- create_FeatureLayer(aqua_sf, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP585) %>%
  dplyr::select(climate_layer, geometry) %>% 
  dplyr::mutate(climate_layer = as.logical(climate_layer))

featsub <- plot_AQMFeatures(ClimateFeature, PUs, land, column = "climate_layer") + ggtitle("Low Exposure Areas") + theme(axis.text = element_text(size = 25))

### Important Features
ImptFeat <- create_ImportantFeatureLayer(aqm_subset1, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP585) %>% 
  dplyr::mutate(Katsuwonus_pelamis = as.logical(Katsuwonus_pelamis))
aqm1_5thPercentile <- plot_AQMFeatures(ImptFeat, PUs, land, column = "Katsuwonus_pelamis") + ggtitle("Species Distribution #1", subtitle = "Katsuwonus pelamis") + theme(axis.text = element_text(size = 25))

RepFeat <- create_RepresentationFeature(ImptFeat, aqm_subset1) %>% 
  dplyr::mutate(Katsuwonus_pelamis = as.logical(Katsuwonus_pelamis))
aqm1_95thPercentile <- plot_AQMFeatures(RepFeat, PUs, land, column = "Katsuwonus_pelamis") + ggtitle("Species Distribution #1", subtitle = "Katsuwonus pelamis") + theme(axis.text = element_text(size = 25))

ImptFeat <- create_ImportantFeatureLayer(aqm_subset2, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP585) %>% 
  dplyr::mutate(Thunnus_orientalis = as.logical(Thunnus_orientalis))
aqm2_5thPercentile <- plot_AQMFeatures(ImptFeat, PUs, land, column = "Thunnus_orientalis") + ggtitle("Species Distribution #1", subtitle = "Thunnus orientalis") + theme(axis.text = element_text(size = 25))

RepFeat <- create_RepresentationFeature(ImptFeat, aqm_subset2) %>% 
  dplyr::mutate(Thunnus_orientalis = as.logical(Thunnus_orientalis))
aqm2_95thPercentile <- plot_AQMFeatures(RepFeat, PUs, land, column = "Thunnus_orientalis") + ggtitle("Species Distribution #1", subtitle = "Thunnus orientalis") + theme(axis.text = element_text(size = 25))
