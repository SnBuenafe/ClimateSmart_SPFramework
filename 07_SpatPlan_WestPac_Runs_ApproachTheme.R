# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

#### Preliminaries ####
# "Approach theme"
# Explores different approaches of incorporating climate metrics into spatial prioritization
# To limit complexity, we used the following parameters for these runs:
# 1. Single emission scenario (SSP 5-8.5)
# 2. Climate warming
# 3. Ensemble mean approach
# Looking at the following approaches:
# 1. Percentile: filters the lower 35th percentile all species' distributions (lower climate warming) (30% divided by 35th percentile)
# 2. Feature: uses 35th percentile of entire planning region as a feature with its own target (30%)
# 3. Penalty: uses the climate layer as a linear penalty
# 4. Climate priority area: filters the lower 5th percentile, assigns it with a 100% target, and gets the rest of the distirbution and assigns it with a lower target (30% divided by the 95th percentile)

# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R") # climate layers are loaded in the script
tos_SSP585 <- load_metrics(metric = "tos", model = "ensemble", scenario = "SSP 5-8.5") # Load climate metric for ens mean

#########################
###### PERCENTILE #######
#########################
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

#########################
###### FEATURE #######
#########################
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

#########################
###### PENALTY ##########
#########################
# 1. Determine scaling
# Scaling here wouldn't matter because we don't have a cost layer;
# This essentially makes metric a cost layer?
scaling <- 1/median(tos_SSP585$transformed) # using the median to scale it
# scaling <- fPenalty_CSapproach(UniformCost$cost, 
#                                tos_SSP585$transformed, 
#                                direction = -1 # low values are more climate-smart
# )

# 2. Get list of features
features <- aqua_sf %>% 
  dplyr::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_sf %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                tos_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p10 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% # using 30% targets
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")

# 4. Solve the planning problem 
s10 <- solve_SPproblem(p10)
saveRDS(s10, paste0(solutions_dir, "s10-EM-Penalty-tos-585.rds")) # save solution

# 5. Plot the spatial design
s10_plot <- s10 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol10 <- fSpatPlan_PlotSolution(s10_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Penalty, SSP 5-8.5")
ggsave(filename = "EM-Penalty-tos-585.png",
       plot = ggSol10, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

####################################################
########## CLIMATE-PRIORITY-AREA APPROACH ##########
####################################################
# 1. Prepare the climate layers and features
aqua_CPA <- fClimatePriorityArea_CSapproach(featuresDF = aqua_sf,
                                            percentile = 5, # Considering the top 5 percentile of each feature as climate-smart areas
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

#####################################
###### CALCULATE SUMMARIES #########
#####################################

dummy <- call_dummy() # Make a "dummy problem" where the features are the original distributions (and not the filtered distributions)
problem_list <- list(dummy, dummy, dummy, dummy)
solution_list <- list(s6, s2, s34, s10)
approach_list <- c("feature", "percentile", "ClimatePriorityArea", "penalty")

# ----- FEATURE REPRESENTATION -----
names <- c("EM_Feature_tos_585", "EM_Percentile_tos_585", "EM_ClimatePriorityArea_tos_585", "EM_Penalty_tos_585")

feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- fFeatureRepresent(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- dplyr::left_join(df, feat_rep, by = "feature")
}
utils::write.csv(feat_rep, paste0(summary_dir, "ApproachTheme_FeatureRepresentation.csv")) # save

# ----- KERNEL DENSITY PLOTS OF TARGETS -----
rev <- c("EM_Penalty_tos_585", "EM_ClimatePriorityArea_tos_585", "EM_Percentile_tos_585", "EM_Feature_tos_585")
x <- feat_rep %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature)) %>% 
  dplyr::mutate(approach = fct_relevel(approach, rev))

ggRidge <- fPlot_RidgeTargetApproach(x)
ggsave(filename = "TargetRidge-ApproachTheme.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
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

climate <- fGetClimateSummary(solution_list, 
                              tos_SSP585, 
                              "tos", 
                              col_scenario = "585", 
                              col_approach = approach_list, 
                              col_run = names, 
                              climateLayer = "single")

summary <- dplyr::left_join(climate, df, by = "run")
utils::write.csv(summary, paste0(summary_dir, "ApproachTheme_Summary.csv")) # save

ggArea <- fPlot_StatisticsApproach(summary, col_name = "percent_area", y_axis = "% area")
ggsave(filename = "Area-ApproachTheme.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- KAPPA CORRELATION MATRIX -----
object_list <- list() # empty list
for (i in 1:length(names)) {
  obj <- select_solution(solution_list[[i]], names[i])
  object_list[[i]] <- obj
}

# Save corrplot
file_path_test = "Figures/CorrMatrix-ApproachTheme.png"
png(height=1200, width=1200, res = 200, file=file_path_test, type = "cairo")

matrix <- fGetCorrMatrix(object_list) %>% 
  fPlot_CorrPlot(., length(object_list))

# Then
dev.off()

# ----- KERNEL DENSITY PLOTS OF CLIMATE METRICS -----
list <- list() # empty list
group_name = "approach"

for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list) %>% 
  dplyr::mutate(approach = fct_relevel(approach, rev))

ggRidge <- fPlot_RidgeClimateApproach(df, climate)
ggsave(filename = "ClimateSmartRidge-ApproachTheme.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# Calculate the mean of the non-selected-planning units
notSelectedClimate <- calculate_meanClimateNotSelected(solution_list, names) %>% 
  dplyr::rename(mean_tos = mean)

ggRidge <- fPlot_RidgeClimateApproach(df, notSelectedClimate)

ggsave(filename = "ClimateSmartRidge-ApproachTheme-NotSelected.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# ----- SELECTION FREQUENCY PLOT -----
sFreq <- fGetSelFrequency(solution_list, names, PUs)
saveRDS(sFreq, paste0(lowregret_dir, "sFreq4-EM-tos-585.rds")) # save low-regret solution

ggFreq <- fPlot_SelFrequency(sFreq, land) + 
  ggtitle("Approach Theme", subtitle = "Percentile (SSP 5-8.5)") +
  inset_element(plot_inset(sFreq), 
                0.7, 0.7, 0.99, 0.99)

ggsave(filename = "FreqPlot-ApproachTheme.png",
       plot = ggFreq, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- KERNEL DENSITY PLOTS OF TARGETS ACCORDING TO SEL FREQUENCY -----
name <- c("selection_1", "selection_2", "selection_3", "selection_4")
solution <- frequency_targets(sFreq, name)

feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(name)) {
  df <- fFeatureRepresent(dummy, solution[[i]], name[i])
  feat_rep <- dplyr::left_join(df, feat_rep, by = "feature")
}

x <- feat_rep %>% 
  tidyr::pivot_longer(!feature, names_to = "selection", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- fPlot_RidgeSelectionApproach(x)

ggsave(filename = "FreqRidge-ApproachTheme.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

