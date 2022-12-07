# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

#### Preliminaries ####
# "Scenario Theme"
# Explores the use of single emission scenarios versus multiple emission scenarios
# To limit complexity, we used the following parameters for these runs:
# 1. Ensemble mean approach: used to calculate the metric
# 2. Rate of climate warming
# 3. "Percentile" approach: used to incorporate the climate layers into spatial prioritization
# areas that are within the 35th percentile are considered climate refugia/climate-smart
# targets: using Effective 30% Protection
# Since we only retained planning units that intersect with both biodiversity features and areas <= 35th percentile (0.35), by multiplying this by ~0.857 target (30/35), we effectively protect only 30%.

# Load preliminaries files
source("03_SpatPlan_Master_Preliminaries.R")
scenario_list = c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
for(scenario_num in 1:length(scenario_list)) {
  x <- load_metrics(metric = "tos", model = "ensemble", scenario = scenario_list[scenario_num])
  assign(paste0("tos_", toupper(str_replace_all(scenario_list[scenario_num], "[^[:alnum:]]", ""))), x)
}

################################
###### SOLVE SP PROBLEMS #######
################################

#### SSP 1-2.6 #####
# 1. Prepare climate layer
aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
                                          percentile = 35,
                                          metricDF = rename_metric(tos_SSP126),
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
                tos_SSP126 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
                )
p38 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s38 <- solve_SPproblem(p38)
saveRDS(s38, paste0(solutions_dir, "s38-EM-Percentile-tos-126.rds")) # save solution

# 5. Plot the spatial design
s38_plot <- s38 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol38 <- fSpatPlan_PlotSolution(s38_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 1-2.6")
ggsave(filename = "EM-Percentile-tos-126.png",
       plot = ggSol38, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### SSP 2-4.5 ####
# 1. Prepare climate layer
aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
                                          percentile = 35,
                                          metricDF = rename_metric(tos_SSP245),
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
                tos_SSP245 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p39 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s39 <- solve_SPproblem(p39)
saveRDS(s39, paste0(solutions_dir, "s39-EM-Percentile-tos-245.rds")) # save solution

# 5. Plot the spatial design
s39_plot <- s39 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol39 <- fSpatPlan_PlotSolution(s39_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 2-4.5")
ggsave(filename = "EM-Percentile-tos-245.png",
       plot = ggSol39, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### SSP 5-8.5 ####
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

#####################################
###### CALCULATE SUMMARIES #########
#####################################

dummy <- call_dummy() # Make a "dummy problem" where the features are the original distributions (and not the filtered distributions)
problem_list <- list(dummy, dummy, dummy)
solution_list <- list(s38, s39, s2)
climate_list <- list(tos_SSP126, tos_SSP245, tos_SSP585)

# ----- FEATURE REPRESENTATION -----
names <- c("EM-Percentile-tos-126", "EM-Percentile-tos-245", "EM-Percentile-tos-585")
feat_rep <- tibble(feature = character()) # empty tibble
for (i in 1:length(names)) {
  df <- fFeatureRepresent(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- dplyr::left_join(df, feat_rep, by = "feature")
}
write.csv(feat_rep, paste0(summary_dir, "ScenarioTheme_FeatureRepresentation.csv")) # save

# ----- KERNEL DENSITY PLOTS OF TARGETS -----
x <- feat_rep %>% 
  tidyr::pivot_longer(!feature, names_to = "scenario", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- fPlot_RidgeTargetScenario(x)

ggsave(filename = "TargetRidge-ScenarioTheme.png", # save ridge plot
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# ----- SUMMARY STATISTICS -----
df <- tibble(run = character()) # empty tibble
# calculate % area for all solutions
for(i in 1:length(names)) {
  statistics <- fComputeSummary(solution_list[[i]], 
                                total_area, 
                                PU_size, 
                                names[i])
  df <- rbind(statistics, df)
}
# calculate mean/median metric values for all solutions
climate <- fGetClimateSummary(solution_list, # list of solutions
                              climate_list, # list of climate metric dfs
                              "tos", # metric
                              col_scenario = str_replace_all(scenario_list, "[^[:digit:]]+", ""), # scenarios
                              col_approach = "percentile", # CS approach used
                              col_run = names # tag of each row
                              )

summary <- dplyr::left_join(climate, df, by = "run")
write.csv(summary, paste0(summary_dir, "ScenarioTheme_Summary.csv")) # save

ggArea <- fPlot_StatisticsScenario(summary, 
                                   col_name = "percent_area", 
                                   y_axis = "% area")
ggsave(filename = "Area-ScenarioTheme.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- KAPPA CORRELATION MATRIX -----
object_list <- list() # empty list
for (i in 1:length(scenario_list)) {
  obj <- select_solution(solution_list[[i]], scenario_list[i])
  object_list[[i]] <- obj
}

# Save corrplot
file_path_test = "Figures/CorrMatrix-ScenarioTheme.png"
png(height=1200, width=1200, res = 200, file=file_path_test, type = "cairo")

matrix <- fGetCorrMatrix(object_list) %>% 
    fPlot_CorrPlot(., length(object_list))

# Then
dev.off()

# ----- KERNEL DENSITY PLOTS OF CLIMATE METRICS -----
list <- list() # empty list
group_name = "scenario"
for(i in 1:length(scenario_list)) {
  list[[i]] <- make_kernel(solution_list[[i]], scenario_list[i], group_name)
}
df <- do.call(rbind, list)

ggRidge <- fPlot_RidgeClimateScenario(df, climate)
ggsave(filename = "ClimateSmartRidge-ScenarioTheme.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# Calculate the mean of the non-selected-planning units
notSelectedClimate <- calculate_meanClimateNotSelected(solution_list, scenario_list) %>% 
  dplyr::rename(mean_tos = mean) %>% 
  dplyr::mutate(scenario = str_replace_all(approach, "[^[:digit:]]", ""))

ggRidge <- fPlot_RidgeClimateScenario(df, notSelectedClimate)
ggsave(filename = "ClimateSmartRidge-ScenarioTheme-NotSelected.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot


# ----- SELECTION FREQUENCY PLOT -----
sFreq <- fGetSelFrequency(solution_list, scenario_list, PUs)
saveRDS(sFreq, paste0(lowregret_dir, "sFreq1-EM-Percentile-tos.rds"))

ggFreq <- fPlot_SelFrequency(sFreq, land) + 
  ggplot2::ggtitle("Scenario Theme", 
          subtitle = "Climate Warming, Percentile") + 
  patchwork::inset_element(plot_inset(sFreq), 
                           0.7, 0.7, 0.99, 0.99)
ggsave(filename = "FreqPlot-ScenarioTheme.png",
       plot = ggFreq, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- KERNEL DENSITY PLOTS OF TARGETS ACCORDING TO SEL FREQUENCY -----
name <- c("selection_1", "selection_2", "selection_3")
solution <- frequency_targets(sFreq, name) # prepare the object

feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(name)) {
  df <- fFeatureRepresent(dummy, solution[[i]], name[i])
  feat_rep <- dplyr::left_join(df, feat_rep, by = "feature")
}

x <- feat_rep %>% 
  tidyr::pivot_longer(!feature, names_to = "selection", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- fPlot_RidgeSelectionScenario(x)

ggsave(filename = "FreqRidge-ScenarioTheme.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot
