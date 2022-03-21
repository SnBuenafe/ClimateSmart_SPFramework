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

#### Main Text: Percentile ####
#### Climate warming ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metric: Rate of Climate Warming (SSP 5-8.5)
# Approach: "Percentile"
# 1. Prepare climate layer
# Retain only planning units of each of the biodiversity features that in intersect with areas of low exposure (<= 35th percentile)
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP585, PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, roc_tos_SSP585, UniformCost)
p2 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>% # using Effective 30% Protection. Since we only retained planning units that intersect with both biodiversity features and areas <= 35th percentile (0.35), by multiplying this by ~0.875 target, we effectively protect only 30%.
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s2 <- prioritizr::solve(p2)
saveRDS(s2, paste0(output_solutions, "s2-EM-Percentile-tos-585.rds")) # save solution
# 5. Plot the spatial design
s2_plot <- s2 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol2 <- fSpatPlan_PlotSolution(s2_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-tos-585.png",
       plot = ggSol2, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Ocean acidification ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metric: Rate of Ocean Acidification (SSP 5-8.5)
# Approach: "Percentile"
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "phos", colname = "transformed", metric_df = roc_phos_SSP585, PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, roc_phos_SSP585, UniformCost)
p3 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s3 <- prioritizr::solve(p3)
saveRDS(s3, paste0(output_solutions, "s3-EM-Percentile-phos-585.rds")) # save solution
# 5. Plot the spatial design
s3_plot <- s3 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol3 <- fSpatPlan_PlotSolution(s3_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-phos-585.png",
       plot = ggSol3, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Declining oxygen concentration ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metrics: Rate of Declining oxygen concentration (SSP 5-8.5)
# Approach: "Percentile"
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "transformed", metric_df = roc_o2os_SSP585, PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, roc_o2os_SSP585, UniformCost)
p4 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s4 <- prioritizr::solve(p4)
saveRDS(s4, paste0(output_solutions, "s4-EM-Percentile-o2os-585.rds")) # save solution
#' 5. Plot the spatial design
s4_plot <- s4 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol4 <- fSpatPlan_PlotSolution(s4_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concetration", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-o2os-585.png",
       plot = ggSol4, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Climate velocity ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metrics: Climate velocity (SSP 5-8.5)
# Approach: "Percentile"
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "transformed", metric_df = velocity_SSP585, PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, velocity_SSP585, UniformCost)
p5 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s5 <- prioritizr::solve(p5)
saveRDS(s5, paste0(output_solutions, "s5-EM-Percentile-velocity-585.rds")) # save solution
# 5. Plot the spatial design
s5_plot <- s5 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol5 <- fSpatPlan_PlotSolution(s5_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-velocity-585.png",
       plot = ggSol5, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Summary ####
# Feature representation
# Load dummy problem, to compare solutions with original distributions and not the filtered distributions
dummy_problem <- readRDS("Output/temp/p10.rds") # "penalty" problem dummy
problem_list <- list(dummy_problem, dummy_problem, dummy_problem, dummy_problem)
solution_list <- list(s2, s3, s4, s5)
names <- c("EM_Percentile_tos_585", "EM_Percentile_phos_585", "EM_Percentile_o2os_585", "EM_Percentile_velocity_585")
feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- represent_feature(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- left_join(df, feat_rep, by = "feature")
}
write.csv(feat_rep, paste0(output_summary, "MetricTheme_Percentile_FeatureRepresentation.csv")) # save

# Kernel distribution plots of targets
x <- feat_rep %>% 
  pivot_longer(!feature, names_to = "metric", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = metric, group = metric, fill = metric),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_Percentile_tos_585` = "#289E3D",
                               `EM_Percentile_phos_585` = "#E6C173",
                               `EM_Percentile_o2os_585` = "#81B0CC",
                               `EM_Percentile_velocity_585` = "#855600")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  xlim(c(30, NA)) +
  theme_classic()
ggsave(filename = "TargetDist-MetricTheme-percentile.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

# Summary
climateLayer_list <- list(roc_tos_SSP585, roc_phos_SSP585, roc_o2os_SSP585, velocity_SSP585)
metric_list <- c("tos", "phos", "o2os", "velocity")
df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}

climate <- list() # empty list
for (i in 1:length(names)) {
  climate[[i]] <- get_ClimateSummary(solution_list, climateLayer_list[[i]], metric_list[i], col_scenario = "585", col_approach = "percentile", col_run = names, climateLayer = "single")
}
climate <- plyr::join_all(climate, by=c("run", "scenario", "approach"), type='left')

summary <- left_join(climate, df, by = "run")

write.csv(summary, paste0(output_summary, "MetricTheme_Percentile_Summary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "metric") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-MetricTheme-Percentile-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# Get Kappa Correlation Matrix
list <- c("tos", "phos", "o2os", "velocity")
object_list <- list() # empty list
solution_list <- list(s2, s3, s4, s5)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#### Low-regret areas ####
s2_LRplot <- create_LowRegretSf(solution_list, names, PUs)
saveRDS(s2_LRplot, paste0(output_lowregret, "s2-EM-LowRegret-Percentile-585.rds")) # save low-regret solution
(ggLowRegret2 <- plot_lowregret(s2_LRplot, land) + theme(axis.text = element_text(size = 25)))
ggsave(filename = "LR-Metric-Percentile.png",
        plot = ggLowRegret2, width = 21, height = 29.7, dpi = 300,
        path = "Figures/") # save plot
# Summary of low-regret
summary <- compute_summary(s2_LRplot, total_area, PU_size, "LR-Percentile-585", Cost = "cost")
write.csv(summary, paste0(output_summary, "MetricTheme_Percentile_LowRegretSummary.csv")) # save

#### Supplementary: Feature ####
#### Climate warming ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metric: Rate of Climate Warming (SSP 5-8.5)
# Approach: "Feature"
# 1. Prepare climate layer
# Climate layer is treated as a feature with its own target.
ClimateFeature <- create_FeatureLayer(metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP585)
# 2. Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
features <- append(features, "climate_layer") # add "climate_layer" to features
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, UniformCost)
p6 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s6 <- prioritizr::solve(p6)
saveRDS(s6, paste0(output_solutions, "s6-EM-Feature-tos-585.rds")) # save solution
# 5. Plot the spatial design
s6_plot <- s6 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol6 <- fSpatPlan_PlotSolution(s6_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Feature, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Feature-tos-585.png",
      plot = ggSol6, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save

#### Ocean acidification ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metric: Rate of Ocean Acidification (SSP 5-8.5)
# Approach: "Feature"
# 1. Prepare climate layer
ClimateFeature <- create_FeatureLayer(metric_name = "phos", colname = "transformed", metric_df = roc_phos_SSP585)
# 2. Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
features <- append(features, "climate_layer") # add "climate_layer" to features
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, UniformCost)
p7 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s7 <- prioritizr::solve(p7)
saveRDS(s7, paste0(output_solutions, "s7-EM-Feature-phos-585.rds")) # save solution
# 5. Plot the spatial design
s7_plot <- s7 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol7 <- fSpatPlan_PlotSolution(s7_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Feature, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Feature-phos-585.png",
       plot = ggSol7, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Declining oxygen concentration ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metrics: Rate of Declining oxygen concentration (SSP 5-8.5)
# Approach: "Feature"
# 1. Prepare climate layer
ClimateFeature <- create_FeatureLayer(metric_name = "o2os", colname = "transformed", metric_df = roc_o2os_SSP585)
# 2. Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
features <- append(features, "climate_layer") # add "climate_layer" to features
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, UniformCost)
p8 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s8 <- prioritizr::solve(p8)
saveRDS(s8, paste0(output_solutions, "s8-EM-Feature-o2os-585.rds")) # save solution
# 5. Plot the spatial design
s8_plot <- s8 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol8 <- fSpatPlan_PlotSolution(s8_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Feature, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Feature-o2os-585.png",
       plot = ggSol8, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Climate velocity ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metrics: Climate velocity (SSP 5-8.5)
# Approach: "Feature"
# 1. Prepare climate layer
ClimateFeature <- create_FeatureLayer(metric_name = "velocity", colname = "transformed", metric_df = velocity_SSP585)
# 2. Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
features <- append(features, "climate_layer") # add "climate_layer" to features
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, UniformCost)
p9 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s9 <- prioritizr::solve(p9)
saveRDS(s9, paste0(output_solutions, "s9-EM-Feature-velocity-585.rds")) # save solution
# 5. Plot the spatial design
s9_plot <- s9 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol9 <- fSpatPlan_PlotSolution(s9_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Feature, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Feature-velocity-585.png",
      plot = ggSol9, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

#### Summary ####
# Feature representation
problem_list <- list(p6, p7, p8, p9)
solution_list <- list(s6, s7, s8, s9)
names <- c("EM_Feature_tos_585", "EM_Feature_phos_585", "EM_Feature_o2os_585", "EM_Feature_velocity_585")
feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- represent_feature(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- left_join(df, feat_rep, by = "feature")
}
write.csv(feat_rep, paste0(output_summary, "MetricTheme_Feature_FeatureRepresentation.csv")) # save

# Kernel distribution plots of targets
x <- feat_rep %>% 
  pivot_longer(!feature, names_to = "metric", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = metric, group = metric, fill = metric),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_Feature_tos_585` = "#289E3D",
                               `EM_Feature_phos_585` = "#E6C173",
                               `EM_Feature_o2os_585` = "#81B0CC",
                               `EM_Feature_velocity_585` = "#855600")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  xlim(min(x$percent), 100) +
  theme_classic()
ggsave(filename = "TargetDist-MetricTheme-feature.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

# Summary
climateLayer_list <- list(roc_tos_SSP585, roc_phos_SSP585, roc_o2os_SSP585, velocity_SSP585)
metric_list <- c("tos", "phos", "o2os", "velocity")
df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}

climate <- list() # empty list
for (i in 1:length(names)) {
  climate[[i]] <- get_ClimateSummary(solution_list, climateLayer_list[[i]], metric_list[i], col_scenario = "585", col_approach = "feature", col_run = names, climateLayer = "single")
}

climate <- plyr::join_all(climate, by=c("run", "scenario", "approach"), type='left')

summary <- left_join(climate, df, by = "run")

write.csv(summary, paste0(output_summary, "MetricTheme_Feature_Summary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "metric") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-MetricTheme-Feature-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# Get Kappa Correlation Matrix
list <- c("tos", "phos", "o2os", "velocity")
object_list <- list() # empty list
solution_list <- list(s6, s7, s8, s9)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#### Low-regret areas ####
s3_LRplot <- create_LowRegretSf(solution_list, names, PUs)
saveRDS(s3_LRplot, paste0(output_lowregret, "s3-EM-LowRegret-Feature-585.rds")) # save low-regret solution
(ggLowRegret3 <- plot_lowregret(s3_LRplot, land) + theme(axis.text = element_text(size = 25)))
ggsave(filename = "LR-Metric-Feature.png",
      plot = ggLowRegret3, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot
# Summary of low-regret
summary <- compute_summary(s3_LRplot, total_area, PU_size, "LR-Feature-585", Cost = "cost")
write.csv(summary, paste0(output_summary, "MetricTheme_Feature_LowRegretSummary.csv")) # save

#### Supplementary: Penalty ####
#### Climate Warming ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metric: Rate of Climate Warming (SSP 5-8.5)
# Approach: "Penalty"
# 1. Prepare climate layer
# Get scaling
scaling_PenaltyWarming <- create_Scaling(UniformCost$cost, roc_tos_SSP585$transformed, "tos")
# 2. Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, roc_tos_SSP585, UniformCost)
scaling <- scaling_PenaltyWarming %>% filter(scaling == 30) %>% pull() # get scaling for 30%
p10 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")
# 4. Solve the planning problem 
s10 <- prioritizr::solve(p10)
saveRDS(s10, paste0(output_solutions, "s10-EM-Penalty-tos-585.rds")) # save solution
# 5. Plot the spatial design
s10_plot <- s10 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol10 <- fSpatPlan_PlotSolution(s10_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Penalty, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Penalty-tos-585.png",
      plot = ggSol10, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save

#### Ocean acidification ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metric: Rate of Ocean Acidiication (SSP 5-8.5)
# Approach: "Penalty"
# 1. Prepare climate layer
scaling_PenaltyAcidification <- create_Scaling(UniformCost$cost, roc_phos_SSP585$transformed, "phos")
# 2. Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, roc_phos_SSP585, UniformCost)
scaling <- scaling_PenaltyAcidification %>% filter(scaling == 30) %>% pull() # get scaling for 30%
p11 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")
# 4. Solve the planning problem 
s11 <- prioritizr::solve(p11)
saveRDS(s11, paste0(output_solutions, "s11-EM-Penalty-phos-585.rds")) # save solution
# 5. Plot the spatial design
s11_plot <- s11 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol11 <- fSpatPlan_PlotSolution(s11_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Penalty, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Penalty-phos-585.png",
      plot = ggSol11, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

#### Declining Oxygen Concentration ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metrics: Rate of declining oxygen concentration (SSP 5-8.5)
# Approach: "Penalty"
# 1. Prepare climate layer
scaling_PenaltyOxygen <- create_Scaling(UniformCost$cost, roc_o2os_SSP585$transformed, "o2os")
# 2. Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, roc_o2os_SSP585, UniformCost)
scaling <- scaling_PenaltyOxygen %>% filter(scaling == 30) %>% pull() # get scaling for 30%
p12 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% # target is 30% for all features.
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")
# 4. Solve the planning problem
s12 <- prioritizr::solve(p12)
saveRDS(s12, paste0(output_solutions, "s12-EM-Penalty-o2os-585.rds")) # save solution
# 5. Plot the spatial design
s12_plot <- s12 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol12 <- fSpatPlan_PlotSolution(s12_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Penalty, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Penalty-o2os-585.png",
      plot = ggSol12, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

#### Climate velocity ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metrics: Climate velocity (SSP 5-8.5)
# Approach: "Penalty"
# 1. Prepare climate layer
scaling_PenaltyVelocity <- create_Scaling(UniformCost$cost, velocity_SSP585$transformed, "velocity")
# 2. Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, velocity_SSP585, UniformCost)
scaling <- scaling_PenaltyVelocity %>% filter(scaling == 30) %>% pull() # get scaling for 30%
p13 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% # target is 30% for all features.
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")
# 4. Solve the planning problem
s13 <- prioritizr::solve(p13)
saveRDS(s13, paste0(output_solutions, "s13-EM-Penalty-velocity-585.rds")) # save solution
# 5. Plot the spatial design
s13_plot <- s13 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol13 <- fSpatPlan_PlotSolution(s13_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Penalty, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Penalty-velocity-585.png",
      plot = ggSol13, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

#### Summary ####
# Feature representation
problem_list <- list(p10, p11, p12, p13)
solution_list <- list(s10, s11, s12, s13)
names <- c("EM_Penalty_tos_585", "EM_Penalty_phos_585", "EM_Penalty_o2os_585", "EM_Penalty_velocity_585")
feat_rep <- tibble(feature = character()) # empty tibble
for (i in 1:length(names)) {
  df <- represent_feature(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- left_join(df, feat_rep, by = "feature")
}
write.csv(feat_rep, paste0(output_summary, "MetricTheme_Penalty_FeatureRepresentation.csv")) # save

# Kernel distribution plots of targets
x <- feat_rep %>% 
  pivot_longer(!feature, names_to = "metric", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature)) %>% 
  dplyr::filter(!is.na(percent))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = metric, group = metric, fill = metric),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_Penalty_tos_585` = "#289E3D",
                               `EM_Penalty_phos_585` = "#E6C173",
                               `EM_Penalty_o2os_585` = "#81B0CC",
                               `EM_Penalty_velocity_585` = "#855600")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  xlim(min(x$percent), 100) +
  theme_classic()
ggsave(filename = "TargetDist-MetricTheme-penalty.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

# Summary
climateLayer_list <- list(roc_tos_SSP585, roc_phos_SSP585, roc_o2os_SSP585, velocity_SSP585)
metric_list <- c("tos", "phos", "o2os", "velocity")
df <- tibble(run = character()) # empty list
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}

climate <- list() # empty list
for (i in 1:length(names)) {
  climate[[i]] <- get_ClimateSummary(solution_list, climateLayer_list[[i]], metric_list[i], col_scenario = "585", col_approach = "penalty", col_run = names, climateLayer = "single")
}

climate <- plyr::join_all(climate, by=c("run", "scenario", "approach"), type='left')

summary <- left_join(climate, df, by = "run")

write.csv(summary, paste0(output_summary, "MetricTheme_Penalty_Summary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "metric") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-MetricTheme-Penalty-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# Get Kappa Correlation Matrix
list <- c("tos", "phos", "o2os", "velocity")
object_list <- list() # empty list
solution_list <- list(s10, s11, s12, s13)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#### Low-regret areas ####
s4_LRplot <- create_LowRegretSf(solution_list, names, PUs)
saveRDS(s4_LRplot, paste0(output_lowregret, "s4-EM-LowRegret-Penalty-585.rds")) # save low-regret solution
(ggLowRegret4 <- plot_lowregret(s4_LRplot, land) + theme(axis.text = element_text(size = 25)))
ggsave(filename = "LR-Metric-Penalty.png",
      plot = ggLowRegret4, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot
# Summary of low-regret
summary <- compute_summary(s4_LRplot, total_area, PU_size, "LR-Penalty-585", Cost = "cost")
write.csv(summary, paste0(output_summary, "MetricTheme_Penalty_LowRegretSummary.csv")) # save

#### Supplementary: Climate priority area ####
#### Climate Warming ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metric: Rate of Climate Warming (SSP 5-8.5)
# Approach: "Climate Priority Area"
# 1. Prepare the climate layers and features
ImptFeat <- create_ImportantFeatureLayer(aqua_sf, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP585)
RepFeat <- create_RepresentationFeature(ImptFeat, aqua_sf)
Features <- cbind(ImptFeat, RepFeat) %>% 
  dplyr::select(-geometry.1)
# 2. Get list of features
features <- Features %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Differentiate targets for important features and representative features
targets <- features %>% as_tibble() %>% 
  setNames(., "Species") %>% 
  add_column(target = 1) %>% 
  mutate(target = ifelse(str_detect(Species, pattern = ".1"), 25/95, 1))
# 4. Set up the spatial planning problem
out_sf <- cbind(Features, roc_tos_SSP585, UniformCost)
p34 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 5. Solve the planning problem 
s34 <- prioritizr::solve(p34)
saveRDS(s34, paste0(output_solutions, "s34-EM-ClimatePriorityArea-tos-585.rds")) # save solution
#' 5. Plot the spatial design
s34_plot <- s34 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol34 <- fSpatPlan_PlotSolution(s34_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Climate Priority Area, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-ClimatePriorityArea-tos-585.png",
      plot = ggSol34, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save

#### Ocean acidification ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metric: Rate of ocean acidification (SSP 5-8.5)
# Approach: "Climate Priority Area"
# 1. Prepare the climate layers and features
ImptFeat <- create_ImportantFeatureLayer(aqua_sf, metric_name = "phos", colname = "transformed", metric_df = roc_phos_SSP585)
RepFeat <- create_RepresentationFeature(ImptFeat, aqua_sf)
Features <- cbind(ImptFeat, RepFeat) %>% 
  dplyr::select(-geometry.1)
# 2. Get list of features
features <- Features %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>%
  names()
# 3. Differentiate targets for important features and representative features
targets <- features %>% as_tibble() %>% 
  setNames(., "Species") %>% 
  add_column(target = 1) %>% 
  mutate(target = ifelse(str_detect(Species, pattern = ".1"), 25/95, 1))
# 4. Set up the spatial planning problem
out_sf <- cbind(Features, roc_phos_SSP585, UniformCost)
p35 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 5. Solve the planning problem 
s35 <- prioritizr::solve(p35)
saveRDS(s35, paste0(output_solutions, "s35-EM-ClimatePriorityArea-phos-585.rds")) # save solution
# 6. Plot the spatial design
s35_plot <- s35 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol35 <- fSpatPlan_PlotSolution(s35_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Climate Priority Area, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-ClimatePriorityArea-phos-585.png",
      plot = ggSol35, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

#### Declining Oxygen Concentration ####
# Parameters:
# Ensemble: Ensemble mean
# Cliamte metrics: Rate of declining oxygen concentration (SSP 5-8.5)
# Approach: "Climate Priority Area"
# 1. Prepare the climate layers and features
ImptFeat <- create_ImportantFeatureLayer(aqua_sf, metric_name = "o2os", colname = "transformed", metric_df = roc_o2os_SSP585)
RepFeat <- create_RepresentationFeature(ImptFeat, aqua_sf)
Features <- cbind(ImptFeat, RepFeat) %>% 
  dplyr::select(-geometry.1)
# 2. Get list of features
features <- Features %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Differentiate targets for important features and representative features
targets <- features %>% as_tibble() %>% 
  setNames(., "Species") %>% 
  add_column(target = 1) %>% 
  mutate(target = ifelse(str_detect(Species, pattern = ".1"), 25/95, 1))
# 4. Set up the spatial planning problem
out_sf <- cbind(Features, roc_o2os_SSP585, UniformCost)
p36 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 5. Solve the planning problem 
s36 <- prioritizr::solve(p36)
saveRDS(s36, paste0(output_solutions, "s36-EM-ClimatePriorityArea-o2os-585.rds")) # save solution
# 6. Plot the spatial design
s36_plot <- s36 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol36 <- fSpatPlan_PlotSolution(s36_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Climate Priority Area, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-ClimatePriorityArea-o2os-585.png",
      plot = ggSol36, width = 21, height = 29.7, dpi = 300,
      path = "Figures/")

#### Climate Velocity ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metrics: Climate velocity (SSP 5-8.5)
# Approach: "Climate priority area"
# 1. Prepare the climate layers and features
ImptFeat <- create_ImportantFeatureLayer(aqua_sf, metric_name = "velocity", colname = "transformed", metric_df = velocity_SSP585)
RepFeat <- create_RepresentationFeature(ImptFeat, aqua_sf)
Features <- cbind(ImptFeat, RepFeat) %>% 
  dplyr::select(-geometry.1)
# 2. Get list of features
features <- Features %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Differentiate targets for important features and representative features
targets <- features %>% as_tibble() %>% 
  setNames(., "Species") %>% 
  add_column(target = 1) %>% 
  mutate(target = ifelse(str_detect(Species, pattern = ".1"), 25/95, 1))
# 4. Set up the spatial planning problem
out_sf <- cbind(Features, velocity_SSP585, UniformCost)
p37 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 5. Solve the planning problem 
s37 <- prioritizr::solve(p37)
saveRDS(s37, paste0(output_solutions, "s37-EM-ClimatePriorityArea-velocity-585.rds")) # save solution
# 6. Plot the spatial design
s37_plot <- s37 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol37 <- fSpatPlan_PlotSolution(s37_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Important Feature, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-ClimatePriorityArea-velocity-585.png",
      plot = ggSol37, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

#### Summary ####
# Feature representation
# Load dummy problem, to compare solutions with original distributions and not the filtered distributions
dummy_problem <- readRDS("Output/temp/p10.rds") # "penalty" problem dummy
problem_list <- list(dummy_problem, dummy_problem, dummy_problem, dummy_problem)
solution_list <- list(s34, s35, s36, s37)

x <- s34 %>% dplyr::select(solution_1, geometry)
eval_feature_representation_summary(dummy_problem, x[,'solution_1'])

names <- c("EM_ClimatePriorityArea_tos_585", "EM_ClimatePriorityArea_phos_585", "EM_ClimatePriorityArea_o2os_585", "EM_ClimatePriorityArea_velocity_585")
feat_rep <- tibble(feature = character()) # empty tibble
for (i in 1:length(names)) {
  df <- represent_feature(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- left_join(df, feat_rep, by = "feature")
}
write.csv(feat_rep, paste0(output_summary, "MetricTheme_ClimatePriorityArea_FeatureRepresentation.csv")) # save

# Kernel distribution plots of targets
x <- feat_rep %>% 
  pivot_longer(!feature, names_to = "metric", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = metric, group = metric, fill = metric),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_ClimatePriorityArea_tos_585` = "#289E3D",
                               `EM_ClimatePriorityArea_phos_585` = "#E6C173",
                               `EM_ClimatePriorityArea_o2os_585` = "#81B0CC",
                               `EM_ClimatePriorityArea_velocity_585` = "#855600")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  xlim(min(x$percent), 100) +
  theme_classic()
ggsave(filename = "TargetDist-MetricTheme-ClimatePriorityArea.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

# Summary
climateLayer_list <- list(roc_tos_SSP585, roc_phos_SSP585, roc_o2os_SSP585, velocity_SSP585)
metric_list <- c("tos", "phos", "o2os", "velocity")
df <- tibble(run = character()) # empty list
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}

climate <- list() # empty list
for (i in 1:length(names)) {
  climate[[i]] <- get_ClimateSummary(solution_list, climateLayer_list[[i]], metric_list[i], col_scenario = "585", col_approach = "ClimatePriorityArea", col_run = names, climateLayer = "single")
}

climate <- plyr::join_all(climate, by=c("run", "scenario", "approach"), type='left')

summary <- left_join(climate, df, by = "run")

write.csv(summary, paste0(output_summary, "MetricTheme_ClimatePriorityArea_Summary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "metric") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-MetricTheme-ClimatePriorityArea-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# Get Kappa Correlation Matrix
list <- c("tos", "phos", "o2os", "velocity")
object_list <- list() # empty list
solution_list <- list(s34, s35, s36, s37)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#### Low-regret Areas ####
s5_LRplot <- create_LowRegretSf(solution_list, names, PUs)
saveRDS(s5_LRplot, paste0(output_lowregret, "s5-EM-LowRegret-ClimatePriorityArea-585.rds")) # save low-regret solution
(ggLowRegret5 <- plot_lowregret(s5_LRplot, land) + theme(axis.text = element_text(size = 25)))
ggsave(filename = "LR-Metric-ClimatePriorityArea.png",
      plot = ggLowRegret5, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot
# Summary of low-regret
summary <- compute_summary(s5_LRplot, total_area, PU_size, "LR-ClimatePriorityArea-585", Cost = "cost")
write.csv(summary, paste0(output_summary, "MetricTheme_ClimatePriorityArea_LowRegretSummary.csv")) # save
