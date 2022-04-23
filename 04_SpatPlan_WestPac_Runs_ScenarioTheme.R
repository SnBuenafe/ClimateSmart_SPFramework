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

# Load functions
source("HelperFunctions/SpatPlan_Extras.R") # Load the extras, including functions and libraries
source("HelperFunctions/SpatPlan_HelperFxns_WestPac.R") # Load helper functions written specifically for this spatial planning project

output_solutions <- "Output/solutions/"
output_summary <- "Output/summary/"
output_lowregret <- "Output/lowregret/"

# Load files
source("03_SpatPlan_Master_Preliminaries.R")
scenario_list = c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
for(scenario_num in 1:length(scenario_list)) {
  LoadClimateMetrics(metric = "tos", model = NA, scenario = scenario_list[scenario_num])
}
total_area = nrow(PUs) * PU_size

#### SSP 1-2.6 #####
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP126, PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, roc_tos_SSP126, UniformCost)
p38 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s38 <- prioritizr::solve(p38)
saveRDS(s38, paste0(output_solutions, "s38-EM-Percentile-tos-126.rds")) # save solution
# 5. Plot the spatial design
s38_plot <- s38 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol38 <- fSpatPlan_PlotSolution(s38_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 1-2.6") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-tos-126.png",
       plot = ggSol38, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### SSP 2-4.5 ####
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP245, PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, roc_tos_SSP245, UniformCost)
p39 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s39 <- prioritizr::solve(p39)
saveRDS(s39, paste0(output_solutions, "s39-EM-Percentile-tos-245.rds")) # save solution
# 5. Plot the spatial design
s39_plot <- s39 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol39 <- fSpatPlan_PlotSolution(s39_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 2-4.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-tos-245.png",
       plot = ggSol39, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### SSP 5-8.5 ####
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


#### Summaries #####
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
  
problem_list <- list(dummy_problem, dummy_problem, dummy_problem)
solution_list <- list(s38, s39, s2)
climateLayer_list <- list(roc_tos_SSP126, roc_tos_SSP245, roc_tos_SSP585)
scenario_list <- c("126", "245", "585")

# ----- Feature representation -----
names <- c("EM-Percentile-tos-126", "EM-Percentile-tos-245", "EM-Percentile-tos-585")
feat_rep <- tibble(feature = character()) # empty tibble
for (i in 1:length(names)) {
  df <- represent_feature(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- left_join(df, feat_rep, by = "feature")
}
write.csv(feat_rep, paste0(output_summary, "ScenarioTheme_tos_FeatureRepresentation.csv")) # save

# ----- Kernel distribution plots of targets -----
x <- feat_rep %>% 
  pivot_longer(!feature, names_to = "scenario", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = scenario, group = scenario, fill = scenario),
                      scale = 2) +
  scale_fill_manual(values = c(`EM-Percentile-tos-126` = "#289E3D",
                              `EM-Percentile-tos-245` = "#E6C173",
                              `EM-Percentile-tos-585` = "#855600")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  theme_classic()
ggsave(filename = "TargetDist-ScenarioTheme-tos.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# ----- Summary statistics -----
df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}
climate <- get_ClimateSummary(solution_list, climateLayer_list, "tos", col_scenario = scenario_list, col_approach = "percentile", col_run = names)

summary <- left_join(climate, df, by = "run")
write.csv(summary, paste0(output_summary, "ScenarioTheme_tos_Summary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "scenario") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-Percentile-tos.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- Get Kappa Correlation Matrix -----
list <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
object_list <- list() # empty list
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

# ----- Measuring how climate-smart the solution are across scenarios using kernel density plots -----
list <- list() # empty list
names <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
group_name = "scenario"
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list)

ggRidge <- ggplot(data = df, aes(x = transformed, y = scenario, group = scenario, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_viridis_c(name = expression('Δ'^"o"*'C yr'^"-1"*''), option = "C") +
  geom_vline(xintercept=(climate %>% 
                           dplyr::filter(scenario == 126))$mean_climate_warming,
             linetype = "dashed", color = "tan1", size = 0.5) +
  geom_vline(xintercept=(climate %>% 
                           dplyr::filter(scenario == 245))$mean_climate_warming,
             linetype = "dashed", color = "orchid3", size = 0.5) +
  geom_vline(xintercept=(climate %>% 
                           dplyr::filter(scenario == 585))$mean_climate_warming,
             linetype = "dashed", color = "orchid4", size = 0.5) +
  theme_classic()
ggsave(filename = "ClimateWarmingDist-ScenarioTheme-Percentile-tos.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# ----- Create selection frequency plot -----
sFreq <- create_LowRegretSf(solution_list, scenario_list, PUs, scenario = TRUE)
saveRDS(sFreq, paste0(output_lowregret, "sFreq1-EM-Percentile-tos.rds"))

(ggFreq <- plot_SelectionFrequency(sFreq, land) + ggtitle("Scenario Theme", subtitle = "Climate Warming, Percentile") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "Freq-EM-Percentile-Scenario-tos.png",
       plot = ggFreq, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot
