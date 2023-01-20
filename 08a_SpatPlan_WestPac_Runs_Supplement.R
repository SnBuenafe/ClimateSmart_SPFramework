# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

#### Preliminaries ####
# "Metric + Approach themes: Supplementary"
# Explores different approaches of incorporating climate metrics into spatial prioritization
# 8a: Feature approach

# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R") # climate layers are loaded in the script
# Load climate metrics for different metrics (all SSP5-8.5)
metric_list <- c("tos", "phos", "o2os", "velocity", "MHW", "CombinedMetric")
for(metric_num in 1:length(metric_list)) {
  x <- load_metrics(metric = metric_list[metric_num], model = "ensemble", scenario = "SSP 5-8.5")
  assign(paste0(metric_list[metric_num], "_SSP585"), x)
}
CombinedMetric_SSP585 %<>% dplyr::rename(transformed = combined) # rename column name

#########################
###### FEATURE #########
#########################
# ----- A. Climate warming -----
# 1. Prepare climate layer
aqua_feature <- fFeature_CSapproach(featuresDF = aqua_sf, 
                                    percentile = 35, 
                                    metricDF = rename_metric(tos_SSP585),
                                    direction = -1 # lower values are more climate-smart
)

# 2. Set up features and targets
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()
# Using fixed targets of 30
target_df <- tibble::as_tibble(features) %>% 
  dplyr::rename(feature = value) %>% 
  dplyr::mutate(target = 30)
targets <- fAssignTargets_Feature(climateSmartDF = aqua_feature,
                                  refugiaTarget = 30,
                                  targetsDF = target_df)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_feature %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                tos_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p6 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s6 <- solve_SPproblem(p6)
saveRDS(s6, paste0(solutions_dir, "s6-EM-Feature-tos-585.rds")) # save solution

# 5. Plot the spatial design
s6_plot <- s6 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol6 <- fSpatPlan_PlotSolution(s6_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Feature, SSP 5-8.5")
ggsave(filename = "EM-Feature-tos-585.png",
       plot = ggSol6, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

# ----- B. Ocean acidification -----
# 1. Prepare climate layer
aqua_feature <- fFeature_CSapproach(featuresDF = aqua_sf, 
                                    percentile = 65, 
                                    metricDF = rename_metric(phos_SSP585),
                                    direction = 1 # higher values are more climate-smart
)

# 2. Set up features and targets
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()
# Using fixed targets of 30
target_df <- tibble::as_tibble(features) %>% 
  dplyr::rename(feature = value) %>% 
  dplyr::mutate(target = 30)
targets <- fAssignTargets_Feature(climateSmartDF = aqua_feature,
                                  refugiaTarget = 30,
                                  targetsDF = target_df)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_feature %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                phos_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p7 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s7 <- solve_SPproblem(p7)
saveRDS(s7, paste0(solutions_dir, "s7-EM-Feature-phos-585.rds")) # save solution

# 5. Plot the spatial design
s7_plot <- s7 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol7 <- fSpatPlan_PlotSolution(s7_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Feature, SSP 5-8.5")
ggsave(filename = "EM-Feature-phos-585.png",
       plot = ggSol7, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- C. Declining oxygen concentration -----
# 1. Prepare climate layer
aqua_feature <- fFeature_CSapproach(featuresDF = aqua_sf, 
                                    percentile = 65, 
                                    metricDF = rename_metric(o2os_SSP585),
                                    direction = 1 # higher values are more climate-smart
)

# 2. Set up features and targets
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()
# Using fixed targets of 30
target_df <- tibble::as_tibble(features) %>% 
  dplyr::rename(feature = value) %>% 
  dplyr::mutate(target = 30)
targets <- fAssignTargets_Feature(climateSmartDF = aqua_feature,
                                  refugiaTarget = 30,
                                  targetsDF = target_df)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_feature %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                o2os_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p8 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s8 <- solve_SPproblem(p8)
saveRDS(s8, paste0(solutions_dir, "s8-EM-Feature-o2os-585.rds")) # save solution

# 5. Plot the spatial design
s8_plot <- s8 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol8 <- fSpatPlan_PlotSolution(s8_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Feature, SSP 5-8.5")
ggsave(filename = "EM-Feature-o2os-585.png",
       plot = ggSol8, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- D. Climate velocity -----
# 1. Prepare climate layer
aqua_feature <- fFeature_CSapproach(featuresDF = aqua_sf, 
                                    percentile = 35, 
                                    metricDF = rename_metric(velocity_SSP585),
                                    direction = -1 # lower values are more climate-smart
)

# 2. Set up features and targets
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()
# Using fixed targets of 30
target_df <- tibble::as_tibble(features) %>% 
  dplyr::rename(feature = value) %>% 
  dplyr::mutate(target = 30)
targets <- fAssignTargets_Feature(climateSmartDF = aqua_feature,
                                  refugiaTarget = 30,
                                  targetsDF = target_df)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_feature %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                velocity_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p9 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s9 <- prioritizr::solve(p9) %>% 
  dplyr::select(cellID, cost, transformed, everything())
saveRDS(s9, paste0(solutions_dir, "s9-EM-Feature-velocity-585.rds")) # save solution

# 5. Plot the spatial design
s9_plot <- s9 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol9 <- fSpatPlan_PlotSolution(s9_plot, PUs, land) + 
  ggtitle("Climate-smart design: Climate Velocity", subtitle = "Feature, SSP 5-8.5") 
ggsave(filename = "EM-Feature-velocity-585.png",
       plot = ggSol9, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- E. Sum of the cumulative MHW intensity -----
# 1. Prepare climate layer
aqua_feature <- fFeature_CSapproach(featuresDF = aqua_sf, 
                                    percentile = 35, 
                                    metricDF = rename_metric(MHW_SSP585),
                                    direction = -1 # lower values are more climate-smart
)

# 2. Get list of features and set targets
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()
# Using fixed targets of 30
target_df <- tibble::as_tibble(features) %>% 
  dplyr::rename(feature = value) %>% 
  dplyr::mutate(target = 30)
targets <- fAssignTargets_Feature(climateSmartDF = aqua_feature,
                                  refugiaTarget = 30,
                                  targetsDF = target_df)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_feature %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                MHW_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p291 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s291 <- prioritizr::solve(p291) %>% 
  dplyr::select(cellID, cost, transformed, everything())
saveRDS(s291, paste0(solutions_dir, "s291-EM-Feature-MHW-585.rds")) # save solution

# 5. Plot the spatial design
s291_plot <- s291 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol291 <- fSpatPlan_PlotSolution(s291_plot, PUs, land) + 
  ggtitle("Climate-smart design: Sum of Cumulative Intensity", subtitle = "Feature, SSP 5-8.5")
ggsave(filename = "EM-Feature-MHW-585.png",
       plot = ggSol291, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- F. Combined metric -----
# 1. Prepare climate layer
aqua_feature <- fFeature_CSapproach(featuresDF = aqua_sf, 
                                    percentile = 65, 
                                    metricDF = rename_metric(CombinedMetric_SSP585),
                                    direction = 1 # higher values are more climate-smart
)

# 2. Get list of features and set targets
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()
# Using fixed targets of 30
target_df <- tibble::as_tibble(features) %>% 
  dplyr::rename(feature = value) %>% 
  dplyr::mutate(target = 30)
targets <- fAssignTargets_Feature(climateSmartDF = aqua_feature,
                                  refugiaTarget = 30,
                                  targetsDF = target_df)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_feature %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                CombinedMetric_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p363 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s363 <- prioritizr::solve(p363) %>% 
  dplyr::select(cellID, cost, transformed, everything())
saveRDS(s363, paste0(solutions_dir, "s363-EM-Feature-CombinedMetric-585.rds")) # save solution

# 5. Plot the spatial design
s363_plot <- s363 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol363 <- fSpatPlan_PlotSolution(s363_plot, PUs, land) + 
  ggtitle("Climate-smart design: Combined Metric", subtitle = "Feature, SSP 5-8.5")
ggsave(filename = "EM-Feature-CombinedMetric-585.png",
       plot = ggSol363, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#####################################
###### CALCULATE SUMMARIES #########
#####################################

dummy <- call_dummy() # Make a "dummy problem" where the features are the original distributions (and not the filtered distributions)
problem_list <- list(dummy, dummy, dummy, dummy, dummy, dummy)
solution_list <- list(s6, s7, s8, s9, s291, s363)
climate_list <- list(tos_SSP585, phos_SSP585, o2os_SSP585, velocity_SSP585, MHW_SSP585, CombinedMetric_SSP585)
metric_list <- c("tos", "phos", "o2os", "velocity", "MHW", "CombinedMetric")

# ----- FEATURE REPRESENTATION -----
names <- c("EM_Feature_tos_585", "EM_Feature_phos_585", "EM_Feature_o2os_585", "EM_Feature_velocity_585", "EM_Feature_MHW_585", "EM_Feature_CombinedMetric_585")

feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- fFeatureRepresent(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- dplyr::left_join(df, feat_rep, by = "feature") %>% 
    drop_na()
}
write.csv(feat_rep, paste0(summary_dir, "Supplement_Feature_FeatureRepresentation.csv")) # save

# ----- KERNEL DENSITY PLOTS OF TARGETS -----
x <- feat_rep %>% 
  tidyr::pivot_longer(!feature, names_to = "metric", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- fPlot_RidgeTargetMetric(x)

ggsave(filename = "TargetRidge-Supplement-Feature.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

# ----- SUMMARY STATISTICS -----
df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- fComputeSummary(solution_list[[i]], 
                                total_area, 
                                PU_size, 
                                names[i])
  df <- rbind(statistics, df)
}

climate <- list() # empty list
for (i in 1:length(names)) {
  climate[[i]] <- fGetClimateSummary(solution_list, 
                                     climate_list[[i]], 
                                     metric_list[i], 
                                     col_scenario = "585",
                                     col_approach = "feature", 
                                     col_run = names, 
                                     climateLayer = "single"
                                     )
}
climate <- plyr::join_all(climate, by=c("run", "scenario", "approach"), type='left')

summary <- dplyr::left_join(climate, df, by = "run")
write.csv(summary, paste0(summary_dir, "Supplement_Feature_Summary.csv")) # save

ggArea <- fPlot_StatisticsMetric(summary, col_name = "percent_area", y_axis = "% area")
ggsave(filename = "Area-Supplement-Feature.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- KAPPA CORRELATION MATRIX -----
object_list <- list() # empty list
for (i in 1:length(metric_list)) {
  obj <- select_solution(solution_list[[i]], metric_list[i])
  object_list[[i]] <- obj
}

# Save corrplot
file_path_test = "Figures/CorrMatrix-Supplement-Feature.png"
png(height=1200, width=1200, res = 200, file=file_path_test, type = "cairo")

matrix <- fGetCorrMatrix(object_list) %>% 
  fPlot_CorrPlot(., length(object_list))

# Then
dev.off()

# ----- SELECTION FREQUENCY PLOT -----
sFreq <- fGetSelFrequency(solution_list, names, PUs)
saveRDS(sFreq, paste0(lowregret_dir, "sFreq5-EM-Feature-585.rds")) # save low-regret solution

ggFreq <- fPlot_SelFrequency(sFreq, land) + 
  ggtitle("Metric Theme", subtitle = "Feature (SSP 5-8.5)") +
  inset_element(plot_inset(sFreq),
                0.7, 0.7, 0.99, 0.99)
ggsave(filename = "FreqPlot-Supplement-Feature.png",
       plot = ggFreq, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot
