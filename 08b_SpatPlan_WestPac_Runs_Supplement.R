# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

#### Preliminaries ####
# "Metric + Approach themes: Supplementary"
# Explores different approaches of incorporating climate metrics into spatial prioritization
# 8b: Penalty approach

# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R")
# Load climate metrics for different metrics (all SSP5-8.5)
metric_list <- c("tos", "phos", "o2os", "velocity", "MHW", "CombinedMetric")
for(metric_num in 1:length(metric_list)) {
  x <- load_metrics(metric = metric_list[metric_num], model = "ensemble", scenario = "SSP 5-8.5")
  assign(paste0(metric_list[metric_num], "_SSP585"), x)
}
CombinedMetric_SSP585 %<>% dplyr::rename(transformed = combined) # rename column name

#########################
###### PENALTY #########
#########################
# ----- A. Climate Warming -----
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

# ----- B. Ocean acidification -----
# 1. Determine scaling
# Scaling here wouldn't matter because we don't have a cost layer;
# This essentially makes metric a cost layer?
scaling <- 1/median(phos_SSP585$transformed) # using the median to scale it
# we want it to be negative to penalize lower values (i.e., more acidic locations)
# scaling <- fPenalty_CSapproach(UniformCost$cost, 
#                                phos_SSP585$transformed, 
#                                direction = -1 # low values are more climate-smart
# )

# 2. Get list of features
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_sf %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                phos_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p11 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% # using 30% targets
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")

# 4. Solve the planning problem 
s11 <- solve_SPproblem(p11)
saveRDS(s11, paste0(solutions_dir, "s11-EM-Penalty-phos-585.rds")) # save solution

# 5. Plot the spatial design
s11_plot <- s11 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol11 <- fSpatPlan_PlotSolution(s11_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Penalty, SSP 5-8.5")
ggsave(filename = "EM-Penalty-phos-585.png",
       plot = ggSol11, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- C. Declining Oxygen Concentration -----
# 1. Determine scaling
# Scaling here wouldn't matter because we don't have a cost layer;
# This essentially makes metric a cost layer?
scaling <- 1/median(o2os_SSP585$transformed) # using the median to scale it
# we want it to be negative to penalize lower values (i.e., areas with more deoxygenation)
# scaling <- fPenalty_CSapproach(UniformCost$cost, 
#                                o2os_SSP585$transformed, 
#                                direction = -1 # low values are more climate-smart
# )

# 2. Get list of features
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_sf %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                o2os_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p12 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% # using 30% targets
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")

# 4. Solve the planning problem
s12 <- solve_SPproblem(p12)
saveRDS(s12, paste0(solutions_dir, "s12-EM-Penalty-o2os-585.rds")) # save solution

# 5. Plot the spatial design
s12_plot <- s12 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol12 <- fSpatPlan_PlotSolution(s12_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Penalty, SSP 5-8.5")
ggsave(filename = "EM-Penalty-o2os-585.png",
       plot = ggSol12, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- D. Climate velocity -----
# 1. Determine scaling
# Scaling here wouldn't matter because we don't have a cost layer;
# This essentially makes metric a cost layer?
scaling <- 1/median(velocity_SSP585$transformed) # using the median to scale it
# scaling <- fPenalty_CSapproach(UniformCost$cost, 
#                                velocity_SSP585$transformed, 
#                                direction = -1 # low values are more climate-smart
# )

# 2. Get list of features
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_sf %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                velocity_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p13 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% # using 30% targets
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")

# 4. Solve the planning problem
s13 <- prioritizr::solve(p13) %>% 
  dplyr::select(cellID, cost, transformed, everything())
saveRDS(s13, paste0(solutions_dir, "s13-EM-Penalty-velocity-585.rds")) # save solution

# 5. Plot the spatial design
s13_plot <- s13 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol13 <- fSpatPlan_PlotSolution(s13_plot, PUs, land) + 
  ggtitle("Climate-smart design: Climate Velocity", subtitle = "Penalty, SSP 5-8.5")
ggsave(filename = "EM-Penalty-velocity-585.png",
       plot = ggSol13, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- E. Sum of the cumulative MHW intensity -----
# 1. Determine scaling
# Scaling here wouldn't matter because we don't have a cost layer;
# This essentially makes metric a cost layer?
scaling <- 1/median(MHW_SSP585$transformed) # using the median to scale it
# scaling <- fPenalty_CSapproach(UniformCost$cost, 
#                                velocity_SSP585$transformed, 
#                                direction = -1 # low values are more climate-smart
# )

# 2. Get list of features
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_sf %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                MHW_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p292 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% # using 30% targets
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")

# 4. Solve the planning problem
s292 <- prioritizr::solve(p292) %>% 
  dplyr::select(cellID, cost, transformed, everything())
saveRDS(s292, paste0(solutions_dir , "s292-EM-Penalty-MHW-585.rds")) # save solution

# 5. Plot the spatial design
s292_plot <- s292 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol292 <- fSpatPlan_PlotSolution(s292_plot, PUs, land) + 
  ggtitle("Climate-smart design: Sum of Cumulative Intensity", subtitle = "Penalty, SSP 5-8.5")
ggsave(filename = "EM-Penalty-MHW-585.png",
       plot = ggSol292, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- F. Combined metric -----
# 1. Determine scaling
# Get scaling
# Scaling here wouldn't matter because we don't have a cost layer;
# This essentially makes metric a cost layer?
scaling <- -1/median(CombinedMetric_SSP585$transformed) # using the median to scale it; negative so it would penalize values of low score
# we want it to be negative to penalize lower values (i.e., areas lower climate-smart scores)
# scaling <- fPenalty_CSapproach(UniformCost$cost, 
#                                CombinedMetric_SSP585$transformed, 
#                                direction = 1 # high values are more climate-smart
# )

# 2. Get list of features
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_sf %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                CombinedMetric_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p364 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% # using 30% targets
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")

# 4. Solve the planning problem
s364 <- prioritizr::solve(p364) %>% 
  dplyr::select(cellID, cost, transformed, everything())
saveRDS(s364, paste0(solutions_dir , "s364-EM-Penalty-CombinedMetric-585.rds")) # save solution

# 5. Plot the spatial design
s364_plot <- s364 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol364 <- fSpatPlan_PlotSolution(s364_plot, PUs, land) + 
  ggtitle("Climate-smart design: Combined Metric", subtitle = "Penalty, SSP 5-8.5")
ggsave(filename = "EM-Penalty-CombinedMetric-585.png",
       plot = ggSol364, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#####################################
###### CALCULATE SUMMARIES #########
#####################################

dummy <- call_dummy() # Make a "dummy problem" where the features are the original distributions (and not the filtered distributions)
problem_list <- list(dummy, dummy, dummy, dummy, dummy, dummy)
solution_list <- list(s10, s11, s12, s13, s292, s364)
climate_list <- list(tos_SSP585, phos_SSP585, o2os_SSP585, velocity_SSP585, MHW_SSP585, CombinedMetric_SSP585)
metric_list <- c("tos", "phos", "o2os", "velocity", "MHW", "CombinedMetric")

# ----- FEATURE REPRESENTATION -----
names <- c("EM_Penalty_tos_585", "EM_Penalty_phos_585", "EM_Penalty_o2os_585", "EM_Penalty_velocity_585", "EM_Penalty_MHW_585", "EM_Penalty_CombinedMetric_585")

feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- fFeatureRepresent(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- dplyr::left_join(df, feat_rep, by = "feature") %>% 
    drop_na()
}
write.csv(feat_rep, paste0(summary_dir, "Supplement_Penalty_FeatureRepresentation.csv")) # save

# ----- KERNEL DENSITY PLOTS OF TARGETS -----
x <- feat_rep %>% 
  pivot_longer(!feature, names_to = "metric", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- fPlot_RidgeTargetMetric(x)

ggsave(filename = "TargetRidge-Supplement-Penalty.png",
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
                                     col_approach = "penalty", 
                                     col_run = names, 
                                     climateLayer = "single")
}
climate <- plyr::join_all(climate, by=c("run", "scenario", "approach"), type='left')

summary <- dplyr::left_join(climate, df, by = "run")
write.csv(summary, paste0(summary_dir, "Supplement_Penalty_Summary.csv")) # save

ggArea <-  fPlot_StatisticsMetric(summary, col_name = "percent_area", y_axis = "% area")
ggsave(filename = "Area-Supplement-Penalty.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- KAPPA CORRELATION MATRIX -----
object_list <- list() # empty list
for (i in 1:length(metric_list)) {
  obj <- select_solution(solution_list[[i]], metric_list[i])
  object_list[[i]] <- obj
}

# Save corrplot
file_path_test = "Figures/CorrMatrix-Supplement-Penalty.png"
png(height=1200, width=1200, res = 200, file=file_path_test, type = "cairo")

matrix <- fGetCorrMatrix(object_list) %>% 
  fPlot_CorrPlot(., length(object_list))

# Then
dev.off()

# ----- SELECTION FREQUENCY PLOT -----
sFreq <- fGetSelFrequency(solution_list, names, PUs)
saveRDS(sFreq, paste0(lowregret_dir, "sFreq6-EM-Penalty-585.rds")) # save low-regret solution

ggFreq <- fPlot_SelFrequency(sFreq, land) + 
  ggtitle("Metric Theme", subtitle = "Penalty (SSP 5-8.5)") +
  inset_element(plot_inset(sFreq), 
                0.7, 0.7, 0.99, 0.99)

ggsave(filename = "FreqPlot-Supplement-Penalty.png",
       plot = ggFreq, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot
