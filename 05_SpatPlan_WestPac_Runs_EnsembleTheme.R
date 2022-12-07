# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

#### Preliminaries ####
# "Ensemble Theme"
# Explores the use of the ensemble mean approach or the multi-model ensemble approach for calculating the climate metrics
# To limit complexity, we used the following parameters for these runs:
# 1. Single emission scenario (SSP 5-8.5)
# 2. Rate of climate warming
# 3. "Percentile" approach: used to incorporate the climate layers into spatial prioritization
# areas that are within the 35th percentile are considered climate refugia/climate-smart
# targets: using Effective 30% Protection
# Since we only retained planning units that intersect with both biodiversity features and areas <= 35th percentile (0.35), by multiplying this by ~0.857 target (30/35), we effectively protect only 30%.

# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R") # climate layers are loaded in the script
tos_SSP585 <- load_metrics(metric = "tos", model = "ensemble", scenario = "SSP 5-8.5") # Load climate metric for ens mean
# Load climate metrics for multi-model ens approach
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
for(model_num in 1:length(model_list)) {
  x <- load_metrics(metric = "tos", model = model_list[model_num], scenario = "SSP 5-8.5")
  assign(paste0("tos_", model_list[model_num], "_SSP585"), x)
}

#################################
###### CLIMATE-UNINFORMED #######
#################################

# 1. Get list of features
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID)

feature_names <- features %>% 
  names()

# 2. Set up the spatial planning problem
out_sf <- cbind(UniformCost, features)
p1 <- prioritizr::problem(out_sf, feature_names, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% # using 30% as the target percentage of protection
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 3. Solve the planning problem 
s1 <- prioritizr::solve(p1) %>% 
  dplyr::select(cellID, solution_1, cost)
saveRDS(s1, paste0(solutions_dir, "s1-uninformed.rds")) # save solution

# 4. Plot the spatial design
s1_plot <- s1 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol1 <- fSpatPlan_PlotSolution(s1_plot, PUs, land) + 
  ggtitle("Climate-uninformed")
ggsave(filename = "Climate_Uninformed.png",
       plot = ggSol1, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# 5. Check summary statistics
# Feature Representation
feat_rep <- fFeatureRepresent(p1, s1, "uninformed")
head(feat_rep)
utils::write.csv(feat_rep, paste0(summary_dir, "Uninformed_FeatureRepresentation.csv")) # save

# Summary
summary <- fComputeSummary(s1, total_area, PU_size, "uninformed")
print(summary)
utils::write.csv(summary, paste0(summary_dir, "Uninformed_Summary.csv")) # save

#####################################
###### ENSEMBLE-MEAN APPROACH #######
#####################################

# 1. Prepare climate layer
aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
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
targets <- fAssignTargets_Percentile(featuresDF = aqua_sf,
                                     climateSmartDF = aqua_percentile,
                                     targetsDF = target_df)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_percentile %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                tos_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p2 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s2 <- solve_SPproblem(p2)
saveRDS(s2, paste0(solutions_dir, "s2-EM-Percentile-tos-585.rds")) # save solution

# 5. Plot the spatial design
s2_plot <- s2 %>% 
  mutate(solution_1 = as.logical(solution_1))
ggSol2 <- fSpatPlan_PlotSolution(s2_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5")
ggsave(filename = "EM-Percentile-tos-585.png",
       plot = ggSol2, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# 6. Check summary statistics
# Feature Representation
dummy <- call_dummy() # Make a "dummy problem" where the features are the original distributions (and not the filtered distributions)
feat_rep <- fFeatureRepresent(dummy, s2, "EM_Percentile_tos_585")
head(feat_rep)

# Summary
summary <- fComputeSummary(s2, total_area, PU_size, "EM_Percentile_tos_585")
print(summary)

# Looking for mean rate of climate warming
climate <- fGetClimateSummary(list(s2), # list of solutions
                              list(tos_SSP585), # list of climate dfs
                              "tos", # metric
                              "585", # scenario
                              "percentile", # CS approach used
                              "EM_Percentile_tos_585" # tag of each row
                              )
summary %<>% 
  dplyr::left_join(., climate, by = c("run"))

###############################################
###### MULTI-MODEL ENSEMBLE APPROACH #########
###############################################

#### CanESM5 #####
# 1. Prepare climate layer
aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
                                          percentile = 35,
                                          metricDF = rename_metric(`tos_CanESM5_SSP585`),
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
targets <- fAssignTargets_Percentile(featuresDF = aqua_sf,
                                     climateSmartDF = aqua_percentile,
                                     targetsDF = target_df)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_percentile %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                `tos_CanESM5_SSP585` %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p14 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s14 <- solve_SPproblem(p14)
saveRDS(s14, paste0(solutions_dir, "s14-MM-CanESM5-Percentile-tos-585.rds")) # save solution

# 5. Plot the spatial design
s14_plot <- s14 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol14 <- fSpatPlan_PlotSolution(s14_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5 (GCM: CanESM5)")
ggsave(filename = "MM-CanESM5-Percentile-tos-585.png",
       plot = ggSol14, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### CMCC-ESM2 #####
# 1. Prepare climate layer
aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
                                          percentile = 35,
                                          metricDF = rename_metric(`tos_CMCC-ESM2_SSP585`),
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
targets <- fAssignTargets_Percentile(featuresDF = aqua_sf,
                                     climateSmartDF = aqua_percentile,
                                     targetsDF = target_df)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_percentile %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                `tos_CMCC-ESM2_SSP585` %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p15 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s15 <- solve_SPproblem(p15)
saveRDS(s15, paste0(solutions_dir, "s15-MM-CMCC_ESM2-Percentile-tos-585.rds")) # save solution

# 5. Plot the spatial design
s15_plot <- s15 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol15 <- fSpatPlan_PlotSolution(s15_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5 (GCM: CMCC-ESM2)")
ggsave(filename = "MM-CMCC_ESM2-Percentile-tos-585.png",
       plot = ggSol15, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### GFDL-ESM4 #####
# 1. Prepare climate layer
aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
                                          percentile = 35,
                                          metricDF = rename_metric(`tos_GFDL-ESM4_SSP585`),
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
targets <- fAssignTargets_Percentile(featuresDF = aqua_sf,
                                     climateSmartDF = aqua_percentile,
                                     targetsDF = target_df)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_percentile %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                `tos_GFDL-ESM4_SSP585` %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p16 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s16 <- solve_SPproblem(p16)
saveRDS(s16, paste0(solutions_dir, "s16-MM-GFDL_ESM4-Percentile-tos-585.rds")) # save solution

# 5. Plot the spatial design
s16_plot <- s16 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol16 <- fSpatPlan_PlotSolution(s16_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5 (GCM: GFDL-ESM4)")
ggsave(filename = "MM-GFDL_ESM4-Percentile-tos-585.png",
       plot = ggSol16, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### IPSL-CM6A-LR ####
# 1. Prepare climate layer
aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
                                          percentile = 35,
                                          metricDF = rename_metric(`tos_IPSL-CM6A-LR_SSP585`),
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
targets <- fAssignTargets_Percentile(featuresDF = aqua_sf,
                                     climateSmartDF = aqua_percentile,
                                     targetsDF = target_df)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_percentile %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                `tos_IPSL-CM6A-LR_SSP585` %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p17 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s17 <- solve_SPproblem(p17)
saveRDS(s17, paste0(solutions_dir, "s17-MM-IPSL_CM6A_LR-Percentile-tos-585.rds")) # save solution

# 5. Plot the spatial design
s17_plot <- s17 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol17 <- fSpatPlan_PlotSolution(s17_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5 (GCM: IPSL-CM6A-LR)")
ggsave(filename = "MM-IPSL_CM6A_LR-Percentile-tos-585.png",
       plot = ggSol17, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### NorESM2-MM ####
# 1. Prepare climate layer
aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
                                          percentile = 35,
                                          metricDF = rename_metric(`tos_NorESM2-MM_SSP585`),
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
targets <- fAssignTargets_Percentile(featuresDF = aqua_sf,
                                     climateSmartDF = aqua_percentile,
                                     targetsDF = target_df)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_percentile %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                `tos_NorESM2-MM_SSP585` %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p18 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s18 <- solve_SPproblem(p18)
saveRDS(s18, paste0(solutions_dir, "s18-MM-NorESM2_MM-Percentile-tos-585.rds")) # save solution

# 5. Plot the spatial design
s18_plot <- s18 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol18 <- fSpatPlan_PlotSolution(s18_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5 (GCM: NorESM2-MM)")
ggsave(filename = "MM-NorESM2_MM-Percentile-tos-585.png",
       plot = ggSol18, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#####################################
###### CALCULATE SUMMARIES #########
#####################################

dummy <- call_dummy() # Make a "dummy problem" where the features are the original distributions (and not the filtered distributions)
problem_list <- list(dummy, dummy, dummy, dummy, dummy, dummy)
solution_list <- list(s2, s14, s15, s16, s17, s18)
climate_list <- list(tos_SSP585, 
                     `tos_CanESM5_SSP585`, 
                     `tos_CMCC-ESM2_SSP585`, 
                     `tos_GFDL-ESM4_SSP585`, 
                     `tos_IPSL-CM6A-LR_SSP585`, 
                     `tos_NorESM2-MM_SSP585`)

# ----- FEATURE REPRESENTATION ----- 
names <- c("MM-CanESM5_Percentile_tos_585", "MM-CMCC-ESM2_Percentile_tos_585", "MM-GFDL-ESM4_Percentile_tos_585", "MM-IPSL-CM6A-LR_Percentile_tos_585", "MM-NorESM2-MM_Percentile_tos_585")

empty_list <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- fFeatureRepresent(problem_list[[i]], solution_list[[i]], names[i])
  empty_list <- dplyr::left_join(df, empty_list, by = "feature")
}
feat_rep %<>% dplyr::left_join(., empty_list)
utils::write.csv(feat_rep, paste0(summary_dir, "EnsembleTheme_FeatureRepresentation.csv")) # save

# ----- KERNEL DENSITY PLOTS OF TARGETS ----- 
x <- feat_rep %>% 
  tidyr::pivot_longer(!feature, names_to = "ensemble", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- fPlot_RidgeTargetEnsemble(x)
ggsave(filename = "TargetRidge-EnsembleTheme.png",
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
# calculate mean/median metric values for all solutions (multi-model ensemble; so, remove the ENSEMBLE-MEAN in the lists)
trunc_solution_list <- list(s14, s15, s16, s17, s18)
trunc_climate_list <- list(`tos_CanESM5_SSP585`, 
                           `tos_CMCC-ESM2_SSP585`, 
                           `tos_GFDL-ESM4_SSP585`, 
                           `tos_IPSL-CM6A-LR_SSP585`, 
                           `tos_NorESM2-MM_SSP585`)

climate <- fGetClimateSummary(trunc_solution_list, # list of solutions
                              trunc_climate_list, # list of climate dfs
                              "tos",  # metric
                              col_scenario = "585", # scenario
                              col_approach = "percentile", # CS approach used
                              col_run = names # tag of each row
                              )

summary <- dplyr::left_join(climate, df, by = "run") %>% 
  rbind(., summary)
utils::write.csv(summary, paste0(summary_dir, "EnsembleTheme_Summary.csv")) # save

ggArea <- fPlot_StatisticsEnsemble(summary, 
                                   col_name = "percent_area", 
                                   y_axis = "% area")
ggsave(filename = "Area-EnsembleTheme.png",
       plot = ggArea, width = 8, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- KAPPA CORRELATION MATRIX -----
names <- c("Ensemble-Mean", "CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
object_list <- list() # empty list
solution_list <- list(s2, s14, s15, s16, s17, s18)
for (i in 1:length(names)) {
  obj <- select_solution(solution_list[[i]], names[i])
  object_list[[i]] <- obj
}

# Save corrplot
file_path_test = "Figures/CorrMatrix-EnsembleTheme.png"
png(height=2400, width=2400, res = 200, file=file_path_test, type = "cairo")

matrix <- fGetCorrMatrix(object_list) %>% 
  fPlot_CorrPlot(., length(object_list))

# Then
dev.off()

# ----- KERNEL DENSITY PLOTS OF CLIMATE METRICS -----
list <- list() # empty list
group_name = "ensemble"
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list) %>% 
  dplyr::mutate(ensemble = fct_relevel(ensemble, names))

ggRidge <- fPlot_RidgeClimateEnsemble(df, climate)

ggsave(filename = "ClimateSmartRidge-EnsembleTheme.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# Calculate the mean of the non-selected-planning units
notSelectedClimate <- calculate_meanClimateNotSelected(solution_list, names) %>% 
  dplyr::rename(mean_tos = mean) %>% 
  dplyr::rename(run = approach)

ggRidge <- fPlot_RidgeClimateEnsemble(df, notSelectedClimate)
ggsave(filename = "ClimateSmartRidge-EnsembleTheme-NotSelected.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# ----- SELECTION FREQUENCY PLOT -----
# Just for the 5 models
sFreq <- fGetSelFrequency(trunc_solution_list, model_list, PUs)
saveRDS(sFreq, paste0(lowregret_dir, "sFreq2-EM-Percentile-tos.rds")) # save solution

ggFreq <- fPlot_SelFrequency(sFreq, land) + 
  ggplot2::ggtitle("Ensemble Theme", 
                  subtitle = "Climate Warming, Percentile (SSP 5-8.5)") + 
  patchwork::inset_element(plot_inset(sFreq), 
                           0.7, 0.7, 0.99, 0.99)
  
ggsave(filename = "FreqPlot-EnsembleTheme.png",
       plot = ggFreq, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- KERNEL DENSITY PLOTS OF TARGETS ACCORDING TO SEL FREQUENCY -----
name <- c("selection_1", "selection_2", "selection_3", "selection_4", "selection_5")
solution <- frequency_targets(sFreq, name)

feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(name)) {
  df <- fFeatureRepresent(dummy, solution[[i]], name[i])
  feat_rep <- dplyr::left_join(df, feat_rep, by = "feature")
}

x <- feat_rep %>% 
  tidyr::pivot_longer(!feature, names_to = "selection", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- fPlot_RidgeSelectionEnsemble(x)

ggsave(filename = "FreqRidge-EnsembleTheme.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot
