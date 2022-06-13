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

# Load functions
source("HelperFunctions/SpatPlan_Extras.R") # Load the extras, including functions and libraries
source("HelperFunctions/SpatPlan_HelperFxns_WestPac.R") # Load helper functions written specifically for this spatial planning project
output_solutions <- "Output/solutions/"
output_summary <- "Output/summary/"
output_lowregret <- "Output/lowregret/"

# Load files
source("03_SpatPlan_Master_Preliminaries.R")
total_area = nrow(PUs) * PU_size

#### Main Text: Percentile ####
# ----- Load climate layers -----
metric_list <- c("tos", "phos", "o2os", "velocity", "MHW_SumCumInt")
for(metric_num in 1:length(metric_list)) {
  LoadClimateMetrics(metric = metric_list[metric_num], model = NA, scenario = "SSP 5-8.5")
}
# ----- Climate warming -----
# 1. Prepare climate layer
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
  add_relative_targets(30/35) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s2 <- prioritizr::solve(p2)
saveRDS(s2, paste0(output_solutions, "s2-EM-Percentile-tos-585.rds")) # save solution
# 5. Plot the spatial design
s2_plot <- s2 %>% 
  mutate(solution_1 = as.logical(solution_1))
ggSol2 <- fSpatPlan_PlotSolution(s2_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25))
ggsave(filename = "EM-Percentile-tos-585.png",
       plot = ggSol2, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- Ocean acidification -----
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
ggSol3 <- fSpatPlan_PlotSolution(s3_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25))
ggsave(filename = "EM-Percentile-phos-585.png",
       plot = ggSol3, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- Declining oxygen concentration -----
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
# 5. Plot the spatial design
s4_plot <- s4 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol4 <- fSpatPlan_PlotSolution(s4_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concetration", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25))
ggsave(filename = "EM-Percentile-o2os-585.png",
       plot = ggSol4, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- Climate velocity -----
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
ggSol5 <- fSpatPlan_PlotSolution(s5_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25))
ggsave(filename = "EM-Percentile-velocity-585.png",
       plot = ggSol5, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- Sum of the cumulative MHW intensity -----
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "MHW_SumCumInt", colname = "transformed", metric_df = MHW_SumCumInt_SSP585, PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, MHW_SumCumInt_SSP585, UniformCost)
p290 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s290 <- prioritizr::solve(p290)
saveRDS(s290, paste0(output_solutions, "s290-EM-Percentile-MHW_SumCumInt-585.rds")) # save solution
# 5. Plot the spatial design
s290_plot <- s290 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol290 <- fSpatPlan_PlotSolution(s290_plot, PUs, land) + ggtitle("Climate-smart design: Sum of Cumulative Intensity", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25))
ggsave(filename = "EM-Percentile-MHW_SumCumInt-585.png",
       plot = ggSol290, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot
#### Summary ####
# Make a "dummy problem" where the features are the original distributions (and not the filtered distributions)
out_sf <- cbind(aqua_sf, UniformCost)
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
dummy_problem <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

problem_list <- list(dummy_problem, dummy_problem, dummy_problem, dummy_problem, dummy_problem)

solution_list <- list(s2, s3, s4, s5, s290)
climateLayer_list <- list(roc_tos_SSP585, roc_phos_SSP585, roc_o2os_SSP585, velocity_SSP585, MHW_SumCumInt_SSP585)
metric_list <- c("tos", "phos", "o2os", "velocity", "MHW_SumCumInt")
# ----- Feature representation -----
names <- c("EM_Percentile_tos_585", "EM_Percentile_phos_585", "EM_Percentile_o2os_585", "EM_Percentile_velocity_585", "EM_Percentile_MHW_SumCumInt_585")
feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- represent_feature(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- left_join(df, feat_rep, by = "feature")
}
write.csv(feat_rep, paste0(output_summary, "MetricTheme_Percentile_FeatureRepresentation.csv")) # save

# ----- Kernel distribution plots of targets -----
x <- feat_rep %>% 
  pivot_longer(!feature, names_to = "metric", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = metric, group = metric, fill = metric),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_Percentile_tos_585` = "#289E3D",
                               `EM_Percentile_phos_585` = "#E6C173",
                               `EM_Percentile_o2os_585` = "#81B0CC",
                               `EM_Percentile_velocity_585` = "#855600",
                               `EM_Percentile_MHW_SumCumInt_585` = "#3C6342")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  xlim(c(30, NA)) +
  theme_classic()
ggsave(filename = "TargetDist-MetricTheme-percentile.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

# ----- Summary statistics -----
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

# ----- Get Kappa Correlation Matrix -----
object_list <- list() # empty list
for (i in 1:length(metric_list)) {
  obj <- select_solution(solution_list[[i]], metric_list[i])
  object_list[[i]] <- obj
}

# Save corrplot
file_path_test = "Figures/MetricTheme_Percentile_CorrelationMatrix.png"
png(height=1200, width=1200, res = 200, file=file_path_test, type = "cairo")

matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list))

# Then
dev.off()

# ----- Create selection frequency plot -----
sFreq <- create_LowRegretSf(solution_list, names, PUs)
saveRDS(sFreq, paste0(output_lowregret, "sFreq3-EM-Percentile-585.rds")) # save low-regret solution

ggFreq <- plot_SelectionFrequency(sFreq, land) + ggtitle("Metric Theme", subtitle = "Percentile (SSP 5-8.5)") + theme(axis.text = element_text(size = 25)) +
  inset_element(plot_inset(sFreq), 0.7, 0.7, 0.99, 0.99)

ggsave(filename = "Freq-Percentile-Ensemble-tos-585.png",
        plot = ggFreq, width = 21, height = 29.7, dpi = 300,
        path = "Figures/") # save plot

# ----- Features according to frequency selection -----
PlanUnits <- PUs %>% 
  dplyr::mutate(cellID = row_number())
name <- c("selection_1", "selection_2", "selection_3", "selection_4", "selection_5")

solution <- frequencyTargets(sFreq, name)

feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(name)) {
  df <- represent_feature(dummy_problem, solution[[i]], name[i])
  feat_rep <- left_join(df, feat_rep, by = "feature")
}

x <- feat_rep %>% 
  pivot_longer(!feature, names_to = "selection", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = selection, group = selection, fill = selection),
                      scale = 5) +
  scale_fill_manual(values = c(selection_1 = "#d0d1e6",
                               selection_2 = "#a6bddb",
                               selection_3 = "#74a9cf",
                               selection_4 = "#2b8cbe",
                               selection_5 = "#045a8d")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  theme_classic()

ggsave(filename = "Freq-Targets-MetricTheme-Percentile-tos.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot
