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

# Load functions
source("HelperFunctions/SpatPlan_Extras.R") # Load the extras, including functions and libraries
source("HelperFunctions/SpatPlan_HelperFxns_WestPac.R") # Load helper functions written specifically for this spatial planning project
output_solutions <- "Output/solutions/"
output_summary <- "Output/summary/"
output_lowregret <- "Output/lowregret/"

# Load files
source("03_SpatPlan_Master_Preliminaries.R")
total_area = nrow(PUs) * PU_size
LoadClimateMetrics(metric = "tos", model = NA, scenario = "SSP 5-8.5")

#### Percentile ####
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
(ggSol2 <- fSpatPlan_PlotSolution(s2_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-tos-585.png",
       plot = ggSol2, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Feature ####
# 1. Prepare climate layer
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

#### Penalty ####
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

#### Climate Priority Area ####
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
# 6. Plot the spatial design
s34_plot <- s34 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol34 <- fSpatPlan_PlotSolution(s34_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Climate Priority Area, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-ClimatePriorityArea-tos-585.png",
       plot = ggSol34, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save
rm(list = ls(pattern = "^roc_tos"))

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

problem_list <- list(dummy_problem, dummy_problem, dummy_problem, dummy_problem)

solution_list <- list(s2, s6, s10, s34)
climateLayer_list <- roc_tos_SSP585
metric_list <- "tos"
approach_list <- c("percentile", "feature", "penalty", "ClimatePriorityArea")

# ----- Feature representation -----
names <- c("EM_Percentile_tos_585", "EM_Feature_tos_585", "EM_Penalty_tos_585", "EM_ClimatePriorityArea_tos_585")
feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- represent_feature(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- left_join(df, feat_rep, by = "feature")
}
write.csv(feat_rep, paste0(output_summary, "ApproachTheme_tos_FeatureRepresentation.csv")) # save

# ----- Kernel distribution plots of targets -----
x <- feat_rep %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = approach, group = approach, fill = approach),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_ClimatePriorityArea_tos_585` = "#E6BA7E",
                               `EM_Feature_tos_585` = "#4D3B2A",
                               `EM_Penalty_tos_585` = "#6984BF",
                               `EM_Percentile_tos_585` = "#2B8142")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  xlim(c(30, NA)) +
  theme_classic()
ggsave(filename = "TargetDist-ApproachTheme-tos.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

# ----- Summary statistics -----
df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}

climate <- get_ClimateSummary(solution_list, climateLayer_list, metric_list, col_scenario = "585", col_approach = approach_list, col_run = names, climateLayer = "single")

summary <- left_join(climate, df, by = "run")
write.csv(summary, paste0(output_summary, "ApproachTheme_tos_Summary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "LR-approach") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-ApproachTheme-tos-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- Get Kappa Correlation Matrix -----
object_list <- list() # empty list
for (i in 1:length(names)) {
  obj <- select_solution(solution_list[[i]], names[i])
  object_list[[i]] <- obj
}

# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

# ----- Create selection frequency plot -----
sFreq <- create_LowRegretSf(solution_list, names, PUs)
saveRDS(sFreq, paste0(output_lowregret, "sFreq4-EM-tos-585.rds")) # save low-regret solution
(ggFreq <- plot_SelectionFrequency(sFreq, land) + ggtitle("Approach Theme", subtitle = "Percentile (SSP 5-8.5)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "Freq-Ensemble-tos-585.png",
       plot = ggFreq, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- Measuring how climate-smart solutions are using Kernel Density plots -----
# Kernel Density Plots
list <- list() # empty list
group_name = "approach"
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list)

ggRidge <- ggplot(data = df, aes(x = transformed, y = approach, group = approach, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_viridis_c(name = expression('Δ'^"o"*'C yr'^"-1"*''), option = "C") +
  geom_vline(xintercept = climate$mean_climate_warming,
             linetype = "dashed", color = "tan1", size = 0.5) +
  theme_classic()
ggsave(filename = "ClimateWarmingDist-ApproachTheme-tos.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot
