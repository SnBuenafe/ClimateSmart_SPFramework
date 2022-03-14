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

#### Approach: Percentile ####
# Plots for the workflow
sp1 <- aqua_sf %>% dplyr::select(colnames(aqua_sf)[4289]) %>% 
  dplyr::mutate(Katsuwonus_pelamis = as.logical(Katsuwonus_pelamis))

sp1_plot <- plot_AQMFeatures(aqm_subset1, PUs, land, column = "Katsuwonus_pelamis") + ggtitle("Species Distribution #1", subtitle = "Katsuwonus pelamis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp1.png",
      plot = sp1_plot, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

sp1_percentile <- create_PercentileLayer(aqua_sf = sp1, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP585, PUs = PUs) %>% 
  dplyr::mutate(Katsuwonus_pelamis = as.logical(Katsuwonus_pelamis))

sp1_PercentilePlot <- plot_AQMFeatures(aqm1_percentile, PUs, land, column = "Katsuwonus_pelamis") + ggtitle("Species Distribution #1", subtitle = "Katsuwonus pelamis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp1Filtered.png",
      plot = sp1_PercentilePlot, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

sp2 <- aqua_sf %>% dplyr::select(colnames(aqua_sf)[8199]) %>% 
  dplyr::mutate(Thunnus_orientalis = as.logical(Thunnus_orientalis)) 

sp2_plot <- plot_AQMFeatures(sp2, PUs, land, column = "Thunnus_orientalis") + ggtitle("Species Distribution #1", subtitle = "Thunnus orientalis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp2.png",
      plot = sp2_plot, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

sp2_percentile <- create_PercentileLayer(aqua_sf = sp2, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP585, PUs = PUs) %>% 
  dplyr::mutate(Thunnus_orientalis = as.logical(Thunnus_orientalis))

sp2_PercentilePlot <- plot_AQMFeatures(aqm2_percentile, PUs, land, column = "Thunnus_orientalis") + ggtitle("Species Distribution #1", subtitle = "Thunnus_orientalis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp2Filtered.png",
      plot = sp2_PercentilePlot, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

#### Approach: Feature ####
# Plots for the workflow
climateLayer <- create_FeatureLayer(aqua_sf, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP585) %>%
  dplyr::select(climate_layer, geometry) %>% 
  dplyr::mutate(climate_layer = as.logical(climate_layer))

feature <- plot_AQMFeatures(climateLayer, PUs, land, column = "climate_layer") + ggtitle("Low Exposure Areas") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Feature-climateFiltered.png",
      plot = feature, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

#### Approach: Climate Priority Area ####
# Plots for the workflow
sp1_ImportantFeature <- create_ImportantFeatureLayer(sp1, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP585) %>% 
  dplyr::mutate(Katsuwonus_pelamis = as.logical(Katsuwonus_pelamis))

sp1_ImportantFeaturePlot <- plot_AQMFeatures(ImptFeat, PUs, land, column = "Katsuwonus_pelamis") + ggtitle("Species Distribution #1", subtitle = "Katsuwonus pelamis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-ClimatePriorityArea-sp1ImptFeat.png",
      plot = sp1_ImportantFeaturePlot, width = 21, height= 29.7, dpi = 300,
      path = "Figures/") # save plot

sp1_RepresentationFeature <- create_RepresentationFeature(sp1_ImportantFeature, sp1) %>% 
  dplyr::mutate(Katsuwonus_pelamis = as.logical(Katsuwonus_pelamis))

sp1_RepresentationFeaturePlot <- plot_AQMFeatures(sp1_RepresentationFeature, PUs, land, column = "Katsuwonus_pelamis") + ggtitle("Species Distribution #1", subtitle = "Katsuwonus pelamis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-ClimatePriorityArea-sp1RepFeat.png",
      plot = sp2_RepresentationFeaturePlot, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

sp2_ImportantFeature <- create_ImportantFeatureLayer(sp2, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP585) %>% 
  dplyr::mutate(Thunnus_orientalis = as.logical(Thunnus_orientalis))

sp2_ImportantFeaturePlot <- plot_AQMFeatures(sp2_ImportantFeature, PUs, land, column = "Thunnus_orientalis") + ggtitle("Species Distribution #2", subtitle = "Thunnus orientalis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-ClimatePriorityArea-sp2ImptFeat.png",
      plot = sp2_ImportantFeaturePlot, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

sp2_RepresentationFeature <- create_RepresentationFeature(sp2_ImportantFeature, sp2) %>% 
  dplyr::mutate(Thunnus_orientalis = as.logical(Thunnus_orientalis))
  
sp2_RepresentationFeaturePlot <- plot_AQMFeatures(sp2_RepresentationFeature, PUs, land, column = "Thunnus_orientalis") + ggtitle("Species Distribution #2", subtitle = "Thunnus orientalis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-CLimatePriorityArea-sp2RepFeat.png",
      plot = sp2_RepresentationFeaturePlot, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot