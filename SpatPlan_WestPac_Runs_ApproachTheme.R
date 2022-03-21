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

# Load feature representation
LRFeature_featrep <- read_csv("Output/summary/MetricTheme_Feature_FeatureRepresentation.csv") %>% 
  dplyr::select(-1) %>% 
  dplyr::filter(feature != "climate_layer")
LRPercentile_featrep <- read_csv("Output/summary/MetricTheme_Percentile_FeatureRepresentation.csv") %>% dplyr::select(-1)
LRPenalty_featrep <- read_csv("Output/summary/MetricTheme_Penalty_FeatureRepresentation.csv") %>% 
  dplyr::select(-1) %>% 
  dplyr::filter(feature != "climate_layer")
LRClimatePriorityArea_featrep <- read_csv("Output/summary/MetricTheme_ClimatePriorityArea_FeatureRepresentation.csv") %>% dplyr::select(-1)

feat_rep <- left_join(LRFeature_featrep, LRPercentile_featrep, by = "feature") %>% 
  left_join(., LRPenalty_featrep, by = "feature") %>% 
  left_join(., LRClimatePriorityArea_featrep, by = "feature")

# Compare area of each
summary <- rbind(LRFeature_summary, LRPercentile_summary, LRPenalty_summary, LRClimatePriorityArea_summary)

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "LR-approach")  + theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-ApproachTheme-Approaches-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# Kappa
solution_list <- list(LRFeature, LRPercentile, LRPenalty, LRClimatePriorityArea)
names <- c("Feature", "Percentile", "Penalty", "ClimatePriorityArea")
object_list <- list() # empty list
for (i in 1:length(names)) {
  obj <- select_solution(solution_list[[i]], names[i])
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

# Measuring "climate-smart"-edness
# Climate Warming
# Kernel Density Plots
list <- list() # empty list
names <- c("Feature", "Percentile", "Penalty", "Climate Priority Area")
group_name = "approach"
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name, metric = roc_tos_SSP585)
}
df <- do.call(rbind, list)

ggRidge <- ggplot(data = df, aes(x = transformed, y = approach, group = approach, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_viridis_c(name = expression('Δ'^"o"*'C yr'^"-1"*''), option = "C") +
  geom_vline(xintercept = climate$mean_climate_warming,
             linetype = "dashed", color = "tan1", size = 0.5) +
  theme_classic()
ggsave(filename = "ClimateWarmingDist-ApproachTheme-tos.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# Ocean Acidification
# Kernel Density Plots
list <- list() # empty list
names <- c("Feature", "Percentile", "Penalty", "Climate Priority Area")
group_name = "approach"
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name, metric = roc_phos_SSP585)
}
df <- do.call(rbind, list)

ggRidge <- ggplot(data = df, aes(x = transformed, y = approach, group = approach, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 1.5) +
  scale_fill_viridis_c(name = expression('Δ pH yr'^"-1"*''), option = "A") +
  geom_vline(xintercept = climate$mean_ocean_acidification,
             linetype = "dashed", color = "tan1", size = 0.5) +
  theme_classic()
ggsave(filename = "OceanAcidificationDist-ApproachTheme-phos.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# Rate of Declining Oxygen Concentration
# Kernel Density Plots
list <- list() # empty list
names <- c("Feature", "Percentile", "Penalty", "Climate Priority Area")
group_name = "approach"
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name, metric = roc_o2os_SSP585)
}
df <- do.call(rbind, list)

ggRidge <- ggplot(data = df, aes(x = transformed, y = approach, group = approach, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_viridis_c(name = expression('Δ mol m'^"-3"*' yr'^"-1"*''), option = "D") +
  geom_vline(xintercept = climate$mean_oxygen_decline,
             linetype = "dashed", color = "black", size = 0.5) +
  theme_classic()
ggsave(filename = "OxygenDeclineDist-ApproachTheme-o2os.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# Climate velocity
# Kernel Density Plots
list <- list() # empty list
names <- c("Feature", "Percentile", "Penalty", "Climate Priority Area")
group_name = "approach"
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name, metric = velocity_SSP585)
}
df <- do.call(rbind, list)

ggRidge <- ggplot(data = df, aes(x = transformed, y = approach, group = approach, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_distiller(name = expression('km yr'^"-1"*''), palette = "RdYlBu") +
  geom_vline(xintercept = climate$median_velocity,
             linetype = "dashed", color = "khaki3", size = 0.5) +
  theme_classic()
ggsave(filename = "ClimateVelocityDist-ApproachTheme-velocity.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# Targets
# Climate warming
x <- feat_rep %>% 
  dplyr::select(feature, contains("tos")) %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = approach, group = approach, fill = approach),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_ClimatePriorityArea_tos_585` = "#E6BA7E",
                               `EM_Feature_tos_585` = "#4D3B2A",
                               `EM_Penalty_tos_585` = "#6984BF",
                               `EM_Percentile_tos_585` = "#2B8142")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  xlim(c(min(x$percent), NA)) +
  theme_classic()
ggsave(filename = "TargetDist-ApproachTheme-tos.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

# Ocean acidification
x <- feat_rep %>% 
  dplyr::select(feature, contains("phos")) %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = approach, group = approach, fill = approach),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_ClimatePriorityArea_phos_585` = "#E6BA7E",
                               `EM_Feature_phos_585` = "#4D3B2A",
                               `EM_Penalty_phos_585` = "#6984BF",
                               `EM_Percentile_phos_585` = "#2B8142")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  xlim(c(min(x$percent), NA)) +
  theme_classic()
ggsave(filename = "TargetDist-ApproachTheme-phos.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

# Oxygen decline
x <- feat_rep %>% 
  dplyr::select(feature, contains("o2os")) %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = approach, group = approach, fill = approach),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_ClimatePriorityArea_o2os_585` = "#E6BA7E",
                               `EM_Feature_o2os_585` = "#4D3B2A",
                               `EM_Penalty_o2os_585` = "#6984BF",
                               `EM_Percentile_o2os_585` = "#2B8142")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  xlim(c(min(x$percent), NA)) +
  theme_classic()
ggsave(filename = "TargetDist-ApproachTheme-o2os.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

# Climate velocity
x <- feat_rep %>% 
  dplyr::select(feature, contains("velocity")) %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = approach, group = approach, fill = approach),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_ClimatePriorityArea_velocity_585` = "#E6BA7E",
                               `EM_Feature_velocity_585` = "#4D3B2A",
                               `EM_Penalty_velocity_585` = "#6984BF",
                               `EM_Percentile_velocity_585` = "#2B8142")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  xlim(c(min(x$percent), NA)) +
  theme_classic()
ggsave(filename = "TargetDist-ApproachTheme-velocity.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

#### Compare across metrics ####


#### Approach: Percentile ####
# First species: Katsuwonus pelamis
# Plots for the workflow
sp1 <- aqua_sf %>% dplyr::select(colnames(aqua_sf)[4289]) %>% 
  dplyr::mutate(Katsuwonus_pelamis = as.logical(Katsuwonus_pelamis))

sp1_plot <- plot_AQMFeatures(sp1, PUs, land, column = "Katsuwonus_pelamis") + ggtitle("Species Distribution #1", subtitle = "Katsuwonus pelamis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp1.png",
      plot = sp1_plot, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

# For SSP 1-2.6
sp1_percentile <- create_PercentileLayer(aqua_sf = sp1, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP126, PUs = PUs) %>% 
  dplyr::mutate(Katsuwonus_pelamis = as.logical(Katsuwonus_pelamis))

sp1_PercentilePlot <- plot_AQMFeatures(sp1_percentile, PUs, land, column = "Katsuwonus_pelamis") + ggtitle("Species Distribution #1", subtitle = "Katsuwonus pelamis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp1FilteredSSP126.png",
       plot = sp1_PercentilePlot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# For SSP 2-4.5
sp1_percentile <- create_PercentileLayer(aqua_sf = sp1, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP245, PUs = PUs) %>% 
  dplyr::mutate(Katsuwonus_pelamis = as.logical(Katsuwonus_pelamis))

sp1_PercentilePlot <- plot_AQMFeatures(sp1_percentile, PUs, land, column = "Katsuwonus_pelamis") + ggtitle("Species Distribution #1", subtitle = "Katsuwonus pelamis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp1FilteredSSP245.png",
       plot = sp1_PercentilePlot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# For SSP 5-8.5
sp1_percentile <- create_PercentileLayer(aqua_sf = sp1, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP585, PUs = PUs) %>% 
  dplyr::mutate(Katsuwonus_pelamis = as.logical(Katsuwonus_pelamis))

sp1_PercentilePlot <- plot_AQMFeatures(sp1_percentile, PUs, land, column = "Katsuwonus_pelamis") + ggtitle("Species Distribution #1", subtitle = "Katsuwonus pelamis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp1FilteredSSP585.png",
      plot = sp1_PercentilePlot, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

# Second species: Thunnus orientalis
sp2 <- aqua_sf %>% dplyr::select(colnames(aqua_sf)[8199]) %>% 
  dplyr::mutate(Thunnus_orientalis = as.logical(Thunnus_orientalis)) 

sp2_plot <- plot_AQMFeatures(sp2, PUs, land, column = "Thunnus_orientalis") + ggtitle("Species Distribution #1", subtitle = "Thunnus orientalis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp2.png",
      plot = sp2_plot, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

# For SSP 1-2.6
sp2_percentile <- create_PercentileLayer(aqua_sf = sp2, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP126, PUs = PUs) %>% 
  dplyr::mutate(Thunnus_orientalis = as.logical(Thunnus_orientalis))

sp2_PercentilePlot <- plot_AQMFeatures(sp2_percentile, PUs, land, column = "Thunnus_orientalis") + ggtitle("Species Distribution #1", subtitle = "Thunnus_orientalis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp2FilteredSSP126.png",
       plot = sp2_PercentilePlot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# For SSP 2-4.5
sp2_percentile <- create_PercentileLayer(aqua_sf = sp2, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP245, PUs = PUs) %>% 
  dplyr::mutate(Thunnus_orientalis = as.logical(Thunnus_orientalis))

sp2_PercentilePlot <- plot_AQMFeatures(sp2_percentile, PUs, land, column = "Thunnus_orientalis") + ggtitle("Species Distribution #1", subtitle = "Thunnus_orientalis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp2FilteredSSP245.png",
       plot = sp2_PercentilePlot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# For SSP 5-8.5
sp2_percentile <- create_PercentileLayer(aqua_sf = sp2, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP585, PUs = PUs) %>% 
  dplyr::mutate(Thunnus_orientalis = as.logical(Thunnus_orientalis))

sp2_PercentilePlot <- plot_AQMFeatures(sp2_percentile, PUs, land, column = "Thunnus_orientalis") + ggtitle("Species Distribution #1", subtitle = "Thunnus_orientalis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp2FilteredSSP585.png",
      plot = sp2_PercentilePlot, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

# Third species:
sp3 <- aqua_sf %>% dplyr::select(colnames(aqua_sf)[7752]) %>% 
  dplyr::mutate(Stenella_coeruleoalba = as.logical(Stenella_coeruleoalba)) 

sp3_plot <- plot_AQMFeatures(sp3, PUs, land, column = "Stenella_coeruleoalba") + ggtitle("Species Distribution #3", subtitle = "Stenella_coeruleoalba") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp3.png",
       plot = sp3_plot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# For SSP 1-2.6
sp3_percentile <- create_PercentileLayer(aqua_sf = sp3, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP126, PUs = PUs) %>% 
  dplyr::mutate(Stenella_coeruleoalba = as.logical(Stenella_coeruleoalba))

sp3_PercentilePlot <- plot_AQMFeatures(sp3_percentile, PUs, land, column = "Stenella_coeruleoalba") + ggtitle("Species Distribution #1", subtitle = "Stenella_coeruleoalba") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp3FilteredSSP126.png",
       plot = sp3_PercentilePlot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# For SSP 2-4.5
sp3_percentile <- create_PercentileLayer(aqua_sf = sp3, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP245, PUs = PUs) %>% 
  dplyr::mutate(Stenella_coeruleoalba = as.logical(Stenella_coeruleoalba))

sp3_PercentilePlot <- plot_AQMFeatures(sp3_percentile, PUs, land, column = "Stenella_coeruleoalba") + ggtitle("Species Distribution #1", subtitle = "Stenella_coeruleoalba") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp3FilteredSSP245.png",
       plot = sp3_PercentilePlot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# For SSP 5-8.5
sp3_percentile <- create_PercentileLayer(aqua_sf = sp3, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP585, PUs = PUs) %>% 
  dplyr::mutate(Stenella_coeruleoalba = as.logical(Stenella_coeruleoalba))

sp3_PercentilePlot <- plot_AQMFeatures(sp3_percentile, PUs, land, column = "Stenella_coeruleoalba") + ggtitle("Species Distribution #1", subtitle = "Stenella_coeruleoalba") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp3FilteredSSP585.png",
       plot = sp3_PercentilePlot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# Fourth species:
sp4 <- aqua_sf %>% dplyr::select(colnames(aqua_sf)[5300]) %>% 
  dplyr::mutate(Nannobrachium_idostigma = as.logical(Nannobrachium_idostigma)) 

sp4_plot <- plot_AQMFeatures(sp4, PUs, land, column = "Nannobrachium_idostigma") + ggtitle("Species Distribution #4", subtitle = "Nannobrachium_idostigma") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp4.png",
       plot = sp4_plot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# For SSP 1-2.6
sp4_percentile <- create_PercentileLayer(aqua_sf = sp4, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP126, PUs = PUs) %>% 
  dplyr::mutate(Nannobrachium_idostigma = as.logical(Nannobrachium_idostigma))

sp4_PercentilePlot <- plot_AQMFeatures(sp4_percentile, PUs, land, column = "Nannobrachium_idostigma") + ggtitle("Species Distribution #4", subtitle = "Nannobrachium_idostigma") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp4FilteredSSP126.png",
       plot = sp4_PercentilePlot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# For SSP 2-4.5
sp4_percentile <- create_PercentileLayer(aqua_sf = sp4, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP245, PUs = PUs) %>% 
  dplyr::mutate(Nannobrachium_idostigma = as.logical(Nannobrachium_idostigma))

sp4_PercentilePlot <- plot_AQMFeatures(sp4_percentile, PUs, land, column = "Nannobrachium_idostigma") + ggtitle("Species Distribution #4", subtitle = "Nannobrachium_idostigma") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp4FilteredSSP245.png",
       plot = sp4_PercentilePlot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# For SSP 5-8.5
sp4_percentile <- create_PercentileLayer(aqua_sf = sp4, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP585, PUs = PUs) %>% 
  dplyr::mutate(Nannobrachium_idostigma = as.logical(Nannobrachium_idostigma))

sp4_PercentilePlot <- plot_AQMFeatures(sp4_percentile, PUs, land, column = "Nannobrachium_idostigma") + ggtitle("Species Distribution #4", subtitle = "Nannobrachium_idostigma") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Percentile-sp4FilteredSSP585.png",
       plot = sp4_PercentilePlot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Approach: Feature ####
# Plots for the workflow
climateLayer <- create_FeatureLayer(metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP585) %>%
  dplyr::select(climate_layer, geometry) %>% 
  dplyr::mutate(climate_layer = as.logical(climate_layer))

feature <- plot_AQMFeatures(climateLayer, PUs, land, column = "climate_layer") + ggtitle("Low Exposure Areas") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Feature-climateFiltered.png",
      plot = feature, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

#### Approach: Climate Priority Area ####
# Plots for the workflow
sp1_ImportantFeature <- create_ImportantFeatureLayer(sp1, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP585) %>% 
  dplyr::mutate(Katsuwonus_pelamis = as.logical(Katsuwonus_pelamis))

sp1_ImportantFeaturePlot <- plot_AQMFeatures(sp1_ImportantFeature, PUs, land, column = "Katsuwonus_pelamis") + ggtitle("Species Distribution #1", subtitle = "Katsuwonus pelamis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-ClimatePriorityArea-sp1ImptFeat.png",
      plot = sp1_ImportantFeaturePlot, width = 21, height= 29.7, dpi = 300,
      path = "Figures/") # save plot

sp1_RepresentationFeature <- create_RepresentationFeature(sp1_ImportantFeature, sp1) %>% 
  dplyr::mutate(Katsuwonus_pelamis = as.logical(Katsuwonus_pelamis))

sp1_RepresentationFeaturePlot <- plot_AQMFeatures(sp1_RepresentationFeature, PUs, land, column = "Katsuwonus_pelamis") + ggtitle("Species Distribution #1", subtitle = "Katsuwonus pelamis") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-ClimatePriorityArea-sp1RepFeat.png",
      plot = sp1_RepresentationFeaturePlot, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

sp2_ImportantFeature <- create_ImportantFeatureLayer(sp2, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP585) %>% 
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
