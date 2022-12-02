# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

#### Preliminaries ####
# "Metric + Approach themes: Supplementary"
# Explores different approaches of incorporating climate metrics into spatial prioritization
# 8c: Climate-priority-area approach

# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R") # climate layers are loaded in the script
# Load climate metrics for different metrics (all SSP5-8.5)
metric_list <- c("tos", "phos", "o2os", "velocity", "MHW", "CombinedMetric")
for(metric_num in 1:length(metric_list)) {
  x <- load_metrics(metric = metric_list[metric_num], model = "ensemble", scenario = "SSP 5-8.5")
  assign(paste0(metric_list[metric_num], "_SSP585"), x)
}
CombinedMetric_SSP585 %<>% dplyr::rename(transformed = combined) # rename column name

###############################################
###### CLIMATE PRIORITY AREA APPROACH #########
###############################################
# ----- A. Climate Warming -----
# 1. Prepare the climate layers and features
aqua_CPA <- fClimatePriorityArea_CSapproach(featuresDF = aqua_sf,
                                            percentile = 5, # lowest 5th percentile of warming
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
  dplyr::mutate(target = 0.3) # this approach needs proportions as targets
targets <- fAssignTargets_CPA(climateSmartDF = aqua_CPA,
                              targetsDF = target_df,
                              refugiaTarget = 1 # 100% protection to the most climate-smart areas
)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_CPA %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                tos_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p34 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 5. Solve the planning problem 
s34 <- solve_SPproblem(p34)
saveRDS(s34, paste0(solutions_dir, "s34-EM-ClimatePriorityArea-tos-585.rds")) # save solution

# 6. Plot the spatial design
s34_plot <- s34 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol34 <- fSpatPlan_PlotSolution(s34_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Climate Priority Area, SSP 5-8.5")
ggsave(filename = "EM-ClimatePriorityArea-tos-585.png",
       plot = ggSol34, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

# ----- B. Ocean acidification -----
# 1. Prepare the climate layers and features
aqua_CPA <- fClimatePriorityArea_CSapproach(featuresDF = aqua_sf,
                                            percentile = 95, # Highest 5th percentile of ocean acidification
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
  dplyr::mutate(target = 0.3) # this approach needs proportions as targets
targets <- fAssignTargets_CPA(climateSmartDF = aqua_CPA,
                              targetsDF = target_df,
                              refugiaTarget = 1 # 100% protection to the most climate-smart areas
)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_CPA %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                phos_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p35 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s35 <- solve_SPproblem(p35)
saveRDS(s35, paste0(solutions_dir, "s35-EM-ClimatePriorityArea-phos-585.rds")) # save solution

# 5. Plot the spatial design
s35_plot <- s35 %>%  
  mutate(solution_1 = as.logical(solution_1)) 
ggSol35 <- fSpatPlan_PlotSolution(s35_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Climate Priority Area, SSP 5-8.5")
ggsave(filename = "EM-ClimatePriorityArea-phos-585.png",
       plot = ggSol35, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- C. Declining Oxygen Concentration -----
# 1. Prepare the climate layers and features
aqua_CPA <- fClimatePriorityArea_CSapproach(featuresDF = aqua_sf,
                                            percentile = 95, # Highest 5th percentile of ocean deoxygenation
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
  dplyr::mutate(target = 0.3) # this approach needs proportions as targets
targets <- fAssignTargets_CPA(climateSmartDF = aqua_CPA,
                              targetsDF = target_df,
                              refugiaTarget = 1 # 100% protection to the most climate-smart areas
)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_CPA %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                o2os_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p36 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 5. Solve the planning problem 
s36 <-solve_SPproblem(p36)
saveRDS(s36, paste0(solutions_dir, "s36-EM-ClimatePriorityArea-o2os-585.rds")) # save solution

# 6. Plot the spatial design
s36_plot <- s36 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol36 <- fSpatPlan_PlotSolution(s36_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Climate Priority Area, SSP 5-8.5")
ggsave(filename = "EM-ClimatePriorityArea-o2os-585.png",
       plot = ggSol36, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

# ----- D. Climate Velocity -----
# 1. Prepare the climate layers and features
aqua_CPA <- fClimatePriorityArea_CSapproach(featuresDF = aqua_sf,
                                            percentile = 5, # Lowest 5th percentile of climate velocity
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
  dplyr::mutate(target = 0.3) # this approach needs proportions as targets
targets <- fAssignTargets_CPA(climateSmartDF = aqua_CPA,
                              targetsDF = target_df,
                              refugiaTarget = 1 # 100% protection to the most climate-smart areas
)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_CPA %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                velocity_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p37 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 5. Solve the planning problem 
s37 <- prioritizr::solve(p37) %>% 
  dplyr::select(cellID, cost, transformed, everything())
saveRDS(s37, paste0(solutions_dir, "s37-EM-ClimatePriorityArea-velocity-585.rds")) # save solution

# 6. Plot the spatial design
s37_plot <- s37 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol37 <- fSpatPlan_PlotSolution(s37_plot, PUs, land) + 
  ggtitle("Climate-smart design: Climate Velocity", subtitle = "Important Feature, SSP 5-8.5")
ggsave(filename = "EM-ClimatePriorityArea-velocity-585.png",
       plot = ggSol37, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- E. Sum of the cumulative MHW intensity -----
# 1. Prepare the climate layers and features
aqua_CPA <- fClimatePriorityArea_CSapproach(featuresDF = aqua_sf,
                                            percentile = 5, # Lowest 5th percentile of MHW intensity
                                            metricDF = rename_metric(MHW_SSP585),
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
  dplyr::mutate(target = 0.3) # this approach needs proportions as targets
targets <- fAssignTargets_CPA(climateSmartDF = aqua_CPA,
                              targetsDF = target_df,
                              refugiaTarget = 1 # 100% protection to the most climate-smart areas
)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_CPA %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                MHW_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p293 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s293 <- prioritizr::solve(p293) %>% 
  dplyr::select(cellID, cost, transformed, everything())
saveRDS(s293, paste0(solutions_dir, "s293-EM-ClimatePriorityArea-MHW-585.rds")) # save solution

# 5. Plot the spatial design
s293_plot <- s293 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol293 <- fSpatPlan_PlotSolution(s293_plot, PUs, land) + 
  ggtitle("Climate-smart design: Sum of Cumulative MHW Intensity", subtitle = "Climate Priority Area, SSP 5-8.5")
ggsave(filename = "EM-ClimatePriorityArea-MHW-585.png",
       plot = ggSol293, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- F. Combined metric -----
# 1. Prepare the climate layers and features
aqua_CPA <- fClimatePriorityArea_CSapproach(featuresDF = aqua_sf,
                                            percentile = 95, # Highest 95th percentile of the combined metric
                                            metricDF = rename_metric(CombinedMetric_SSP585),
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
  dplyr::mutate(target = 0.3) # this approach needs proportions as targets
targets <- fAssignTargets_CPA(climateSmartDF = aqua_CPA,
                              targetsDF = target_df,
                              refugiaTarget = 1 # 100% protection to the most climate-smart areas
)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_CPA %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                CombinedMetric_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p365 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s365 <- prioritizr::solve(p365) %>% 
  dplyr::select(cellID, cost, transformed, everything())
saveRDS(s365, paste0(solutions_dir, "s365-EM-ClimatePriorityArea-CombinedMetric-585.rds")) # save solution

# 5. Plot the spatial design
s365_plot <- s365 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol365 <- fSpatPlan_PlotSolution(s365_plot, PUs, land) + 
  ggtitle("Climate-smart design: Combined Metric", subtitle = "Climate Priority Area, SSP 5-8.5")
ggsave(filename = "EM-ClimatePriorityArea-CombinedMetric-585.png",
       plot = ggSol365, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#####################################
###### CALCULATE SUMMARIES #########
#####################################

dummy <- call_dummy() # Make a "dummy problem" where the features are the original distributions (and not the filtered distributions)
problem_list <- list(dummy, dummy, dummy, dummy, dummy, dummy)
solution_list <- list(s34, s35, s36, s37, s293, s365)
climate_list <- list(tos_SSP585, phos_SSP585, o2os_SSP585, velocity_SSP585, MHW_SSP585, CombinedMetric_SSP585)
metric_list <- c("tos", "phos", "o2os", "velocity", "MHW", "CombinedMetric")

# ----- FEATURE REPRESENTATION -----
names <- c("EM_ClimatePriorityArea_tos_585", "EM_ClimatePriorityArea_phos_585", "EM_ClimatePriorityArea_o2os_585", "EM_ClimatePriorityArea_velocity_585", "EM_ClimatePriorityArea_MHW_585", "EM_ClimatePriorityArea_CombinedMetric_585")

feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- fFeatureRepresent(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- dplyr::left_join(df, feat_rep, by = "feature") %>% 
    drop_na()
}
write.csv(feat_rep, paste0(summary_dir, "Supplement_ClimatePriorityArea_FeatureRepresentation.csv")) # save

# ----- KERNEL DENSITY PLOTS OF TARGETS -----
x <- feat_rep %>% 
  tidyr::pivot_longer(!feature, names_to = "metric", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- fPlot_RidgeTargetMetric(x)

ggsave(filename = "TargetRidge-Supplement-ClimatePriorityArea.png",
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
                                     col_approach = "percentile", 
                                     col_run = names, 
                                     climateLayer = "single")
}
climate <- plyr::join_all(climate, by=c("run", "scenario", "approach"), type='left')

summary <- dplyr::left_join(climate, df, by = "run")
write.csv(summary, paste0(summary_dir, "Supplement_ClimatePriorityArea_Summary.csv")) # save

ggArea <- fPlot_StatisticsMetric(summary, col_name = "percent_area", y_axis = "% area")
ggsave(filename = "Area-Supplement-ClimatePriorityArea.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- KAPPA CORRELATION MATRIX -----
object_list <- list() # empty list
for (i in 1:length(metric_list)) {
  obj <- select_solution(solution_list[[i]], metric_list[i])
  object_list[[i]] <- obj
}

# Save corrplot
file_path_test = "Figures/CorrMatrix-Supplement-ClimatePriorityArea.png"
png(height=1200, width=1200, res = 200, file=file_path_test, type = "cairo")

matrix <- fGetCorrMatrix(object_list) %>% 
  fPlot_CorrPlot(., length(object_list))

# Then
dev.off()

# ----- SELECTION FREQUENCY PLOT -----
sFreq <- fGetSelFrequency(solution_list, names, PUs)
saveRDS(sFreq, paste0(lowregret_dir, "sFreq7-EM-Penalty-585.rds")) # save low-regret solution

ggFreq <- fPlot_SelFrequency(sFreq, land) + 
  ggtitle("Metric Theme", subtitle = "Climate priority area (SSP 5-8.5)") +
  inset_element(plot_inset(sFreq), 
                0.7, 0.7, 0.99, 0.99)

ggsave(filename = "FreqPlot-Supplement-ClimatePriorityArea.png",
       plot = ggFreq, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot
