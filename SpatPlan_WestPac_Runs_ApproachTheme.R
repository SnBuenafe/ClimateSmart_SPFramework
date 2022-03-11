# title: "Climate-smart methods paper runs"
# author: "Tin Buenafe"

#### Preliminaries ####
# Description
# This code creates and analyzes spatial designs using the features and the planning region generated from `SpatPlan_Master_WestPac.R`

source("HelperFunctions/SpatPlan_Extras.R") # Load the extras, including functions and libraries
source("HelperFunctions/SpatPlan_HelperFxns_WestPac.R") # Load helper functions written specifically for this spatial planning project
output_solutions <- "Output/solutions/"
output_summary <- "Output/summary/"
output_lowregret <- "Output/lowregret/"

# Load files
source("SpatPlan_Master_Preliminaries.R")
total_area = nrow(PUs) * PU_size

#### Comparing across approaches #####
# Load Low-regret areas
LRFeature <- readRDS("Output/lowregret/s3-EM-LowRegret-Feature-585.rds")
LRPercentile <- readRDS("Output/lowregret/s2-EM-LowRegret-Percentile-585.rds")
LRPenalty <- readRDS("Output/lowregret/s4-EM-LowRegret-Penalty-585.rds")
LRClimatePriorityArea <- readRDS("Output/lowregret/s5-EM-LowRegret-ClimatePriorityArea-585.rds")

# Load summaries
LRFeature_summary <- read_csv("Output/summary/MetricTheme_Feature_LowRegretSummary.csv") %>% 
  dplyr::select(-1)
LRPercentile_summary <- read_csv("Output/summary/MetricTheme_Percentile_LowRegretSummary.csv") %>% 
  dplyr::select(-1)
LRPenalty_summary <- read_csv("Output/summary/MetricTheme_Penalty_LowRegretSummary.csv") %>% 
  dplyr::select(-1)
LRClimatePriorityArea_summary <- read_csv("Output/summary/MetricTheme_ClimatePriorityArea_LowRegretSummary.csv") %>% 
  dplyr::select(-1)

# Compare cost of each
summary <- rbind(LRFeature_summary, LRPercentile_summary, LRPenalty_summary, LRClimatePriorityArea_summary)

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "LR-approach")  + theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-ApproachTheme-Approaches-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# Kappa
solution_list <- list(LRFeature, LRPercentile, LRPenalty, LRClimatePriorityArea)
names <- c("Feature", "Percentile", "Penalty", "ClimatePriorityArea")
object_list <- list() # empty list
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

# Check the Climate Metrics of the Low-Regret Areas
run_list <- c("LRFeature", "LRPercentile", "LRPenalty", "LRClimatePriorityArea")
climateLayer_list <- list(roc_tos_SSP585, roc_phos_SSP585, roc_o2os_SSP585, velocity_SSP585)
metric_list <- c("tos", "phos", "o2os", "velocity")
climate <- lowRegret_ClimateSummary(solution = solution_list, 
                                    run = run_list, metric = metric_list,
                                    climate = climateLayer_list, scenario = "585",
                                    approach = names)
write_csv(climate, paste0(output_summary, "ApproachTheme_Approaches_LowRegretClimateSummary.csv"))

# Get intersection of all low-regret areas
intersection <- intersect_lowregret(solution_list, run_list) %>% 
  dplyr::mutate(solution_1 = ifelse(selection == (length(run_list)*4), yes = 1, no = 0)) #*4 because 4 metrics TODO: Change it to 5 once including marine heatwaves

# Get summary
summary <- compute_summary(intersection, total_area, PU_size, run_name = "LR-Approaches", Cost = "cost")

climate <- list() # empty list
for (i in 1:length(metric_list)) {
  climate[[i]] <- get_ClimateSummary(list(intersection), climateLayer_list[[i]], metric_list[i], col_scenario = "585", col_approach = "LR-Approaches", col_run = "LR-Approaches", climateLayer = "single")
}

climate <- plyr::join_all(climate, by=c("run", "scenario", "approach"), type='left')

summary <- left_join(climate, summary, by = "run")

write.csv(summary, paste0(output_summary, "ApproachTheme_Approaches_LowRegretSummary.csv")) # save

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
