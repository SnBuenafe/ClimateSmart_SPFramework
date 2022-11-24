# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

#### Preliminaries ####
# "Metric theme"
# Explores the use of different climate metrics
# To limit complexity, we used the following parameters for these runs:
# 1. Single emission scenario (SSP 5-8.5)
# 2. Ensemble mean approach
# 3. "Percentile" approach: used to incorporate the climate layers into spatial prioritization
# areas that are within the 35th percentile are considered climate refugia/climate-smart
# targets: using Effective 30% Protection
# Since we only retained planning units that intersect with both biodiversity features and areas <= 35th percentile (0.35), by multiplying this by ~0.857 target (30/35), we effectively protect only 30%.

# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R") # climate layers are loaded in the script
# Load climate metrics for different metrics (all SSP5-8.5)
metric_list <- c("tos", "phos", "o2os", "velocity", "MHW", "CombinedMetric")
for(metric_num in 1:length(metric_list)) {
  x <- load_metrics(metric = metric_list[metric_num], model = "ensemble", scenario = "SSP 5-8.5")
  assign(paste0(metric_list[metric_num], "_SSP585"), x)
}
CombinedMetric_SSP585 %<>% dplyr::rename(transformed = combined) # rename column name
total_area = nrow(PUs)

##############################
###### CLIMATE WARMING #######
##############################
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

##################################
###### OCEAN ACIDIFICATION #######
##################################
# 1. Prepare climate layer
aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
                                          percentile = 65,
                                          metricDF = rename_metric(phos_SSP585),
                                          direction = 1 # higher values are more climate-smart (less acidification)
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
                phos_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p3 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s3 <- solve_SPproblem(p3)
saveRDS(s3, paste0(solutions_dir, "s3-EM-Percentile-phos-585.rds")) # save solution

# 5. Plot the spatial design
s3_plot <- s3 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol3 <- fSpatPlan_PlotSolution(s3_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 5-8.5")
ggsave(filename = "EM-Percentile-phos-585.png",
       plot = ggSol3, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

##################################
###### OCEAN DEOXYGENATION #######
##################################
# 1. Prepare climate layer
aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
                                          percentile = 65,
                                          metricDF = rename_metric(o2os_SSP585),
                                          direction = 1 # higher values are more climate-smart (more positive)
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
                o2os_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p4 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s4 <- solve_SPproblem(p4)
saveRDS(s4, paste0(solutions_dir, "s4-EM-Percentile-o2os-585.rds")) # save solution

# 5. Plot the spatial design
s4_plot <- s4 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol4 <- fSpatPlan_PlotSolution(s4_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Declining Oxygen Concetration", subtitle = "Percentile, SSP 5-8.5") 
ggsave(filename = "EM-Percentile-o2os-585.png",
       plot = ggSol4, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

###############################
###### CLIMATE VELOCITY #######
###############################
# 1. Prepare climate layer
aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
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
targets <- fAssignTargets_Percentile(featuresDF = aqua_sf,
                                     climateSmartDF = aqua_percentile,
                                     targetsDF = target_df)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_percentile %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                velocity_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)

p5 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s5 <- solve_SPproblem(p5)
saveRDS(s5, paste0(solutions_dir, "s5-EM-Percentile-velocity-585.rds")) # save solution

# 5. Plot the spatial design
s5_plot <- s5 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol5 <- fSpatPlan_PlotSolution(s5_plot, PUs, land) + 
  ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 5-8.5")
ggsave(filename = "EM-Percentile-velocity-585.png",
       plot = ggSol5, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

##############################################
###### SUM OF CUMULATIVE MHW INTENSITY #######
##############################################
# 1. Prepare climate layer
aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
                                          percentile = 35,
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
  dplyr::mutate(target = 30)
targets <- fAssignTargets_Percentile(featuresDF = aqua_sf,
                                     climateSmartDF = aqua_percentile,
                                     targetsDF = target_df)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_percentile %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                MHW_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p290 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s290 <- prioritizr::solve(p290) %>% 
  dplyr::select(cellID, cost, transformed, everything())
saveRDS(s290, paste0(solutions_dir, "s290-EM-Percentile-MHW-585.rds")) # save solution

# 5. Plot the spatial design
s290_plot <- s290 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol290 <- fSpatPlan_PlotSolution(s290_plot, PUs, land) + 
  ggtitle("Climate-smart design: Sum of Cumulative Intensity", subtitle = "Percentile, SSP 5-8.5")
ggsave(filename = "EM-Percentile-MHW-585.png",
       plot = ggSol290, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

######################################
###### COMBINED CLIMATE METRIC #######
######################################
# 1. Prepare climate layer
aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
                                          percentile = 65,
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
  dplyr::mutate(target = 30)
targets <- fAssignTargets_Percentile(featuresDF = aqua_sf,
                                     climateSmartDF = aqua_percentile,
                                     targetsDF = target_df)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_percentile %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                CombinedMetric_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p362 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s362 <- prioritizr::solve(p362) %>% 
  dplyr::select(cellID, cost, transformed, everything())
saveRDS(s362, paste0(solutions_dir, "s362-EM-Percentile-CombinedMetric-585.rds")) # save solution

# 5. Plot the spatial design
s362_plot <- s362 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol362 <- fSpatPlan_PlotSolution(s362_plot, PUs, land) + 
  ggtitle("Climate-smart design: Sum of Cumulative Intensity", subtitle = "Percentile, SSP 5-8.5")
ggsave(filename = "EM-Percentile-CombinedMetric-585.png",
       plot = ggSol362, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#####################################
###### CALCULATE SUMMARIES #########
#####################################

dummy <- call_dummy() # Make a "dummy problem" where the features are the original distributions (and not the filtered distributions)
problem_list <- list(dummy, dummy, dummy, dummy, dummy, dummy)
solution_list <- list(s2, s3, s4, s5, s290, s362)
climate_list <- list(tos_SSP585, phos_SSP585, o2os_SSP585, velocity_SSP585, MHW_SSP585, CombinedMetric_SSP585)

# ----- FEATURE REPRESENTATION -----
names <- c("EM_Percentile_tos_585", "EM_Percentile_phos_585", "EM_Percentile_o2os_585", "EM_Percentile_velocity_585", "EM_Percentile_MHW_585", "EM_Percentile_CombinedMetric_585")

feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- fFeatureRepresent(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- dplyr::left_join(df, feat_rep, by = "feature")
}
write.csv(feat_rep, paste0(summary_dir, "MetricTheme_FeatureRepresentation.csv")) # save

# ----- KERNEL DENSITY PLOTS OF TARGETS -----
x <- feat_rep %>% 
  tidyr::pivot_longer(!feature, names_to = "metric", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- fPlot_RidgeTargetMetric(x)
ggsave(filename = "TargetRidge-MetricTheme.png",
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
                                     climateLayer = "single"
                                     )
}
climate <- plyr::join_all(climate, by=c("run", "scenario", "approach"), type='left')

summary <- dplyr::left_join(climate, df, by = "run")
write.csv(summary, paste0(summary_dir, "MetricTheme_Summary.csv")) # save

ggArea <- fPlot_StatisticsMetric(summary, col_name = "percent_area", y_axis = "% area")
ggsave(filename = "Area-MetricTheme.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- KAPPA CORRELATION MATRIX -----
object_list <- list() # empty list
for (i in 1:length(metric_list)) {
  obj <- select_solution(solution_list[[i]], metric_list[i])
  object_list[[i]] <- obj
}

# Save corrplot
file_path_test = "Figures/CorrMatrix-MetricTheme.png"
png(height=1200, width=1200, res = 200, file=file_path_test, type = "cairo")

matrix <- fGetCorrMatrix(object_list) %>% 
  fPlot_CorrPlot(., length(object_list))

# Then
dev.off()

# ----- SELECTION FREQUENCY PLOT -----
sFreq <- fGetSelFrequency(solution_list, names, PUs)
saveRDS(sFreq, paste0(lowregret_dir, "sFreq3-EM-Percentile-585.rds")) # save low-regret solution

ggFreq <- fPlot_SelFrequency(sFreq, land) + 
  ggtitle("Metric Theme", subtitle = "Percentile (SSP 5-8.5)") +
  inset_element(plot_inset(sFreq), 
                0.7, 0.7, 0.99, 0.99)

ggsave(filename = "FreqPlot-MetricTheme.png",
        plot = ggFreq, width = 21, height = 29.7, dpi = 300,
        path = "Figures/") # save plot

# ----- KERNEL DENSITY PLOTS OF TARGETS ACCORDING TO SEL FREQUENCY -----
name <- c("selection_1", "selection_2", "selection_3", "selection_4", "selection_5", "selection_6")
solution <- frequency_targets(sFreq, name)

feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(name)) {
  df <- fFeatureRepresent(dummy, solution[[i]], name[i])
  feat_rep <- dplyr::left_join(df, feat_rep, by = "feature")
}

x <- feat_rep %>% 
  tidyr::pivot_longer(!feature, names_to = "selection", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- fPlot_RidgeSelectionMetric(x)

ggsave(filename = "FreqRidge-MetricTheme.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot
