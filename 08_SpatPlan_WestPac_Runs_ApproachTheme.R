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


# TODO: Delete below

#### Comparing low-regret climate-approach solutions ####
# ----- Load low-regret files -----
# Plans
LRFeature <- readRDS("Output/lowregret/s3-EM-LowRegret-Feature-585.rds")
LRPercentile <- readRDS("Output/lowregret/s2-EM-LowRegret-Percentile-585.rds")
LRPenalty <- readRDS("Output/lowregret/s4-EM-LowRegret-Penalty-585.rds")
LRClimatePriorityArea <- readRDS("Output/lowregret/s5-EM-LowRegret-ClimatePriorityArea-585.rds")
# Summaries
LRFeature_summary <- read_csv("Output/summary/MetricTheme_Feature_LowRegretSummary.csv") %>% 
  dplyr::select(-1)
LRPercentile_summary <- read_csv("Output/summary/MetricTheme_Percentile_LowRegretSummary.csv") %>% 
  dplyr::select(-1)
LRPenalty_summary <- read_csv("Output/summary/MetricTheme_Penalty_LowRegretSummary.csv") %>% 
  dplyr::select(-1)
LRClimatePriorityArea_summary <- read_csv("Output/summary/MetricTheme_ClimatePriorityArea_LowRegretSummary.csv") %>% 
  dplyr::select(-1)
# Feature representation
LRFeature_featrep <- read_csv("Output/summary/MetricTheme_Feature_FeatureRepresentation.csv") %>% 
  dplyr::select(-1) %>% 
  dplyr::filter(feature != "climate_layer")
LRPercentile_featrep <- read_csv("Output/summary/MetricTheme_Percentile_FeatureRepresentation.csv") %>% dplyr::select(-1)
LRPenalty_featrep <- read_csv("Output/summary/MetricTheme_Penalty_FeatureRepresentation.csv") %>% 
  dplyr::select(-1) %>% 
  dplyr::filter(feature != "climate_layer")
LRClimatePriorityArea_featrep <- read_csv("Output/summary/MetricTheme_ClimatePriorityArea_FeatureRepresentation.csv") %>% dplyr::select(-1)

feat_rep <- left_join(LRFeature_featrep, LRPercentile_featrep, by = "feature") %>% 
  left_join(., LRPenalty_featrep, by = "feature") %>% 
  left_join(., LRClimatePriorityArea_featrep, by = "feature")
summary <- rbind(LRFeature_summary, LRPercentile_summary, LRPenalty_summary, LRClimatePriorityArea_summary)

# ----- Summary statistics of low-regret climate-approach solutions -----
solution_list <- list(LRFeature, LRPercentile, LRPenalty, LRClimatePriorityArea)
run_list <- c("LRFeature", "LRPercentile", "LRPenalty", "LRClimatePriorityArea")
# Load climate metrics
metric_list <- c("tos", "phos", "o2os", "velocity", "MHW_SumCumInt")
for(i in 1:length(metric_list)) {
  LoadClimateMetrics(metric_list[i], model = NA, scenario = "SSP 5-8.5")
}
climateLayer_list <- list(roc_tos_SSP585, roc_phos_SSP585, roc_o2os_SSP585, velocity_SSP585, MHW_SumCumInt_SSP585)
# ----- Total area -----
ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "LR-approach")  + theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-ApproachTheme-Approaches-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- Kappa Correlation Matrix -----
object_list <- list() # empty list
for (i in 1:length(run_list)) {
  obj <- select_solution(solution_list[[i]], run_list[i])
  object_list[[i]] <- obj
}

# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

# ----- Check and save the Climate Metrics of the Low-Regret Solutions -----
climate <- lowRegret_ClimateSummary(solution = solution_list, 
                                    run = run_list, metric = metric_list,
                                    climate = climateLayer_list, scenario = "585",
                                    approach = run_list)
write_csv(climate, paste0(output_summary, "ApproachTheme_Approaches_LowRegretClimateSummary.csv"))

# ----- Intersection of all low-regret climate-approach solutions -----
intersection <- intersect_lowregret(solution_list, run_list) %>% 
  dplyr::mutate(solution_1 = ifelse(selection == (length(run_list)*5), yes = 1, no = 0))

summary <- compute_summary(intersection, total_area, PU_size, run_name = "LR-Approaches", Cost = "cost")

climate <- list() # empty list
for (i in 1:length(metric_list)) {
  climate[[i]] <- get_ClimateSummary(list(intersection), climateLayer_list[[i]], metric_list[i], col_scenario = "585", col_approach = "LR-Approaches", col_run = "LR-Approaches", climateLayer = "single")
}

climate <- plyr::join_all(climate, by=c("run", "scenario", "approach"), type='left')
summary <- left_join(climate, summary, by = "run")

write.csv(summary, paste0(output_summary, "ApproachTheme_Approaches_LowRegretSummary.csv")) # save

# ----- Climate-smart metrics of LR areas shown using Kernel Density plots -----
group_name = "approach"
# ----- Climate Warming -----
list <- list() # empty list
for(i in 1:length(run_list)) {
  list[[i]] <- make_kernel(solution_list[[i]], run_list[i], group_name, metric = roc_tos_SSP585)
}
df <- do.call(rbind, list)

ggRidge <- ggplot(data = df, aes(x = transformed, y = approach, group = approach, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_viridis_c(name = expression('Δ'^"o"*'C yr'^"-1"*''), option = "C") +
  geom_vline(xintercept = climate$mean_climate_warming,
             linetype = "dashed", color = "tan1", size = 0.5) +
  theme_classic()
ggsave(filename = "ClimateWarmingDist-ApproachThemeLR-tos.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# ----- Ocean Acidification -----
list <- list() # empty list
for(i in 1:length(run_list)) {
  list[[i]] <- make_kernel(solution_list[[i]], run_list[i], group_name, metric = roc_phos_SSP585)
}
df <- do.call(rbind, list)

ggRidge <- ggplot(data = df, aes(x = transformed, y = approach, group = approach, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 1.5) +
  scale_fill_viridis_c(name = expression('Δ pH yr'^"-1"*''), option = "A") +
  geom_vline(xintercept = climate$mean_ocean_acidification,
             linetype = "dashed", color = "tan1", size = 0.5) +
  theme_classic()
ggsave(filename = "OceanAcidificationDist-ApproachThemeLR-phos.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# ----- Declining Oxygen Concentration -----
list <- list() # empty list
for(i in 1:length(run_list)) {
  list[[i]] <- make_kernel(solution_list[[i]], run_list[i], group_name, metric = roc_o2os_SSP585)
}
df <- do.call(rbind, list)

ggRidge <- ggplot(data = df, aes(x = transformed, y = approach, group = approach, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_viridis_c(name = expression('Δ mol m'^"-3"*' yr'^"-1"*''), option = "D") +
  geom_vline(xintercept = climate$mean_oxygen_decline,
             linetype = "dashed", color = "black", size = 0.5) +
  theme_classic()
ggsave(filename = "OxygenDeclineDist-ApproachThemeLR-o2os.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# ----- Climate velocity -----
list <- list() # empty list
for(i in 1:length(run_list)) {
  list[[i]] <- make_kernel(solution_list[[i]], run_list[i], group_name, metric = velocity_SSP585)
}
df <- do.call(rbind, list)

ggRidge <- ggplot(data = df, aes(x = transformed, y = approach, group = approach, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_distiller(name = expression('km yr'^"-1"*''), palette = "RdYlBu") +
  geom_vline(xintercept = climate$median_velocity,
             linetype = "dashed", color = "khaki3", size = 0.5) +
  theme_classic()
ggsave(filename = "ClimateVelocityDist-ApproachThemeLR-velocity.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# ----- Sum of Cumulative MHW Intensity -----
list <- list() # empty list
for(i in 1:length(run_list)) {
  list[[i]] <- make_kernel(solution_list[[i]], run_list[i], group_name, metric = MHW_SumCumInt_SSP585)
}
df <- do.call(rbind, list)

ggRidge <- ggplot(data = df, aes(x = transformed, y = approach, group = approach, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_viridis_c(name = expression('total degree days'), option = "G") +
  geom_vline(xintercept = climate$mean_sum_cumulative_intensity,
             linetype = "dashed", color = "khaki3", size = 0.5) +
  theme_classic()
ggsave(filename = "MHWSumCumIntDist-ApproachThemeLR-MHW_SumCumInt.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

#### Comparing individual solutions across metrics and approaches (climate-smart aspect) ####
names <- c("Feature", "Percentile", "Penalty", "Climate Priority Area")
group_name = "approach"
# ----- Kernel density plots showing climate warming of solutions created using 'metric: climate warming' -----
solution_list <- list(s6, s2, s10, s34)
list <- list() # empty list
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name, metric = roc_tos_SSP585)
}
df <- do.call(rbind, list)

# Load summaries
feature <- read_csv(paste0(output_summary, "MetricTheme_Feature_Summary.csv")) %>% dplyr::filter(grepl("tos", run)) %>% dplyr::select(mean_climate_warming, run)
percentile <- read_csv(paste0(output_summary, "MetricTheme_Percentile_Summary.csv")) %>% dplyr::filter(grepl("tos", run)) %>% dplyr::select(mean_climate_warming, run)
penalty <- read_csv(paste0(output_summary, "MetricTheme_Penalty_Summary.csv")) %>% dplyr::filter(grepl("tos", run)) %>% dplyr::select(mean_climate_warming, run)
climatePriorityArea <- read_csv(paste0(output_summary, "MetricTheme_ClimatePriorityArea_Summary.csv")) %>% dplyr::filter(grepl("tos", run)) %>% dplyr::select(mean_climate_warming, run)

climate <- bind_rows(feature, percentile, penalty, climatePriorityArea)

ggRidge <- ggplot(data = df, aes(x = transformed, y = approach, group = approach, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_viridis_c(name = expression('Δ'^"o"*'C yr'^"-1"*''), option = "C") +
  geom_vline(xintercept = climate$mean_climate_warming,
             linetype = "dashed", color = "tan1", size = 0.5) +
  theme_classic()
ggsave(filename = "ClimateWarmingDist-ApproachTheme-tos.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# ----- Kernel density plots showing ocean acidification of solutions created using 'metric: ocean acidification' -----
solution_list <- list(s7, s3, s11, s35)
list <- list() # empty list
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name, metric = roc_phos_SSP585)
}
df <- do.call(rbind, list)

feature <- read_csv(paste0(output_summary, "MetricTheme_Feature_Summary.csv")) %>% dplyr::filter(grepl("phos", run)) %>% dplyr::select(mean_ocean_acidification, run)
percentile <- read_csv(paste0(output_summary, "MetricTheme_Percentile_Summary.csv")) %>% dplyr::filter(grepl("phos", run)) %>% dplyr::select(mean_ocean_acidification, run)
penalty <- read_csv(paste0(output_summary, "MetricTheme_Penalty_Summary.csv")) %>% dplyr::filter(grepl("phos", run)) %>% dplyr::select(mean_ocean_acidification, run)
climatePriorityArea <- read_csv(paste0(output_summary, "MetricTheme_ClimatePriorityArea_Summary.csv")) %>% dplyr::filter(grepl("phos", run)) %>% dplyr::select(mean_ocean_acidification, run)

climate <- bind_rows(feature, percentile, penalty, climatePriorityArea)

ggRidge <- ggplot(data = df, aes(x = transformed, y = approach, group = approach, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_viridis_c(name = expression('Δ pH yr'^"-1"*''), option = "A") +
  geom_vline(xintercept = climate$mean_ocean_acidification,
             linetype = "dashed", color = "tan1", size = 0.5) +
  theme_classic()
ggsave(filename = "OceanAcidificationDist-ApproachTheme-phos.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# ----- Kernel density plots showing declining oxygen concentration of solutions created using 'metric: declining oxygen concentration' -----
solution_list <- list(s8, s4, s12, s36)
list <- list() # empty list
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name, metric = roc_o2os_SSP585)
}
df <- do.call(rbind, list)

feature <- read_csv(paste0(output_summary, "MetricTheme_Feature_Summary.csv")) %>% dplyr::filter(grepl("o2os", run)) %>% dplyr::select(mean_oxygen_decline, run)
percentile <- read_csv(paste0(output_summary, "MetricTheme_Percentile_Summary.csv")) %>% dplyr::filter(grepl("o2os", run)) %>% dplyr::select(mean_oxygen_decline, run)
penalty <- read_csv(paste0(output_summary, "MetricTheme_Penalty_Summary.csv")) %>% dplyr::filter(grepl("o2os", run)) %>% dplyr::select(mean_oxygen_decline, run)
climatePriorityArea <- read_csv(paste0(output_summary, "MetricTheme_ClimatePriorityArea_Summary.csv")) %>% dplyr::filter(grepl("o2os", run)) %>% dplyr::select(mean_oxygen_decline, run)

climate <- bind_rows(feature, percentile, penalty, climatePriorityArea)

ggRidge <- ggplot(data = df, aes(x = transformed, y = approach, group = approach, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_viridis_c(name = expression('Δ mol m'^"-3"*' yr'^"-1"*''), option = "D") +
  geom_vline(xintercept = climate$mean_oxygen_decline,
             linetype = "dashed", color = "black", size = 0.5) +
  theme_classic()
ggsave(filename = "OxygenDeclineDist-ApproachTheme-o2os.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# ----- Kernel density plots showing climate velocity of solutions created using 'metric: climate velocity' -----
solution_list <- list(s9, s5, s13, s37)
list <- list() # empty list
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name, metric = velocity_SSP585)
}
df <- do.call(rbind, list)

feature <- read_csv(paste0(output_summary, "MetricTheme_Feature_Summary.csv")) %>% dplyr::filter(grepl("velocity", run)) %>% dplyr::select(median_velocity, run)
percentile <- read_csv(paste0(output_summary, "MetricTheme_Percentile_Summary.csv")) %>% dplyr::filter(grepl("velocity", run)) %>% dplyr::select(median_velocity, run)
penalty <- read_csv(paste0(output_summary, "MetricTheme_Penalty_Summary.csv")) %>% dplyr::filter(grepl("velocity", run)) %>% dplyr::select(median_velocity, run)
climatePriorityArea <- read_csv(paste0(output_summary, "MetricTheme_ClimatePriorityArea_Summary.csv")) %>% dplyr::filter(grepl("velocity", run)) %>% dplyr::select(median_velocity, run)

climate <- bind_rows(feature, percentile, penalty, climatePriorityArea)

ggRidge <- ggplot(data = df, aes(x = transformed, y = approach, group = approach, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_distiller(name = expression('km yr'^"-1"*''), palette = "RdYlBu") +
  geom_vline(xintercept = climate$median_velocity,
             linetype = "dashed", color = "khaki3", size = 0.5) +
  theme_classic()
ggsave(filename = "ClimateVelocityDist-ApproachTheme-velocity.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# ----- Kernel density plots showing sum of cumulative MHW intensity of solutions created using 'metric: Sum of Cumulative MHW Intensity' -----
solution_list <- list(s291, s290, s292, s293)
list <- list() # empty list
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name, metric = MHW_SumCumInt_SSP585)
}
df <- do.call(rbind, list)

feature <- read_csv(paste0(output_summary, "MetricTheme_Feature_Summary.csv")) %>% dplyr::filter(grepl("MHW", run)) %>% dplyr::select(mean_sum_cumulative_intensity, run)
percentile <- read_csv(paste0(output_summary, "MetricTheme_Percentile_Summary.csv")) %>% dplyr::filter(grepl("MHW", run)) %>% dplyr::select(mean_sum_cumulative_intensity, run)
penalty <- read_csv(paste0(output_summary, "MetricTheme_Penalty_Summary.csv")) %>% dplyr::filter(grepl("MHW", run)) %>% dplyr::select(mean_sum_cumulative_intensity, run)
climatePriorityArea <- read_csv(paste0(output_summary, "MetricTheme_ClimatePriorityArea_Summary.csv")) %>% dplyr::filter(grepl("MHW", run)) %>% dplyr::select(mean_sum_cumulative_intensity, run)

climate <- bind_rows(feature, percentile, penalty, climatePriorityArea)

ggRidge <- ggplot(data = df, aes(x = transformed, y = approach, group = approach, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_viridis_c(name = expression('total degree days'), option = "G") +
  geom_vline(xintercept = climate$mean_sum_cumulative_intensity,
             linetype = "dashed", color = "khaki3", size = 0.5) +
  theme_classic()
ggsave(filename = "MHWSumCumIntDist-ApproachTheme-MHW_SumCumInt.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot
#### Comparing individual solutions across metrics and approaches (targets) ####
# ----- Kernel density plots showing the % distribution of all features in solutions created using 'metric: climate warming' -----
x <- feat_rep %>% 
  dplyr::select(feature, contains("tos")) %>% 
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
  xlim(c(min(x$percent), NA)) +
  theme_classic()
ggsave(filename = "TargetDist-ApproachTheme-tos.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

# ----- Kernel density plots showing the % distribution of all features in solutions created using 'metric: ocean acidification' -----
x <- feat_rep %>% 
  dplyr::select(feature, contains("phos")) %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = approach, group = approach, fill = approach),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_ClimatePriorityArea_phos_585` = "#E6BA7E",
                               `EM_Feature_phos_585` = "#4D3B2A",
                               `EM_Penalty_phos_585` = "#6984BF",
                               `EM_Percentile_phos_585` = "#2B8142")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  xlim(c(min(x$percent), NA)) +
  theme_classic()
ggsave(filename = "TargetDist-ApproachTheme-phos.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

# ----- Kernel density plots showing the % distribution of all features in solutions created using 'metric: declining oxygen concentration' -----
x <- feat_rep %>% 
  dplyr::select(feature, contains("o2os")) %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = approach, group = approach, fill = approach),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_ClimatePriorityArea_o2os_585` = "#E6BA7E",
                               `EM_Feature_o2os_585` = "#4D3B2A",
                               `EM_Penalty_o2os_585` = "#6984BF",
                               `EM_Percentile_o2os_585` = "#2B8142")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  xlim(c(min(x$percent), NA)) +
  theme_classic()
ggsave(filename = "TargetDist-ApproachTheme-o2os.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

# ----- Kernel density plots showing the % distribution of all features in solutions created using 'metric: climate velocity' -----
x <- feat_rep %>% 
  dplyr::select(feature, contains("velocity")) %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = approach, group = approach, fill = approach),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_ClimatePriorityArea_velocity_585` = "#E6BA7E",
                               `EM_Feature_velocity_585` = "#4D3B2A",
                               `EM_Penalty_velocity_585` = "#6984BF",
                               `EM_Percentile_velocity_585` = "#2B8142")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  xlim(c(min(x$percent), NA)) +
  theme_classic()
ggsave(filename = "TargetDist-ApproachTheme-velocity.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

# ----- Kernel density plost showing the % distribution of all features in solutions created using 'metric: sum of cumulative MHW intensity -----
x <- feat_rep %>% 
  dplyr::select(feature, contains("MHW")) %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = approach, group = approach, fill = approach),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_ClimatePriorityArea_MHW_SumCumInt_585` = "#E6BA7E",
                               `EM_Feature_MHW_SumCumInt_585` = "#4D3B2A",
                               `EM_Penalty_MHW_SumCumInt_585` = "#6984BF",
                               `EM_Percentile_MHW_SumCumInt_585` = "#2B8142")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  xlim(c(min(x$percent), NA)) +
  theme_classic()
ggsave(filename = "TargetDist-ApproachTheme-MHW_SumCumInt.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot
#### Comparing low-regret climate-metric solutions ####
#### Climate warming ####
# ----- Create low-regret solution -----
solution_list <- list(s2, s6, s10, s34)
names <- c("EM_Percentile_tos_585", "EM_Feature_tos_585", "EM_Penalty_tos_585", "EM_ClimatePriorityArea_tos_585")
s6_LRplot <- create_LowRegretSf(solution_list, names, PUs)
saveRDS(s6_LRplot, paste0(output_lowregret, "s6-EM-LowRegret-tos-585.rds")) # save low-regret solution
(ggLowRegret6 <- plot_lowregret(s6_LRplot, land) + theme(axis.text = element_text(size = 25)))
ggsave(filename = "LR-Approach-tos.png",
       plot = ggLowRegret6, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- Summary statistics -----
df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}

approach_list <- c("percentile", "feature", "penalty", "climate priority area")
climate <- get_ClimateSummary(solution_list, climate_layer = roc_tos_SSP585, metric = "tos", col_scenario = "585", col_approach = approach_list, col_run = names, climateLayer = "single")

summary <- left_join(climate, df, by = "run")
write.csv(summary, paste0(output_summary, "ApproachTheme_tos_Summary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "LR-approach") +
  theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-ApproachTheme-tos-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- Get Kappa Correlation Matrix -----
object_list <- list() # empty list
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], approach_list[i])
  object_list[[i]] <- obj
}

# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#### Ocean acidification ####
# ----- Create low-regret solution -----
solution_list <- list(s3, s7, s11, s35)
names <- c("EM_Percentile_phos_585", "EM_Feature_phos_585", "EM_Penalty_phos_585", "EM_ClimatePriorityArea_phos_585")
s7_LRplot <- create_LowRegretSf(solution_list, names, PUs)
saveRDS(s7_LRplot, paste0(output_lowregret, "s7-EM-LowRegret-phos-585.rds")) # save low-regret solution
(ggLowRegret7 <- plot_lowregret(s7_LRplot, land) + theme(axis.text = element_text(size = 25)))
ggsave(filename = "LR-Approach-phos.png",
       plot = ggLowRegret7, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- Summary statistics -----
df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}

approach_list <- c("percentile", "feature", "penalty", "climate priority area")
climate <- get_ClimateSummary(solution_list, climate_layer = roc_phos_SSP585, metric = "phos", col_scenario = "585", col_approach = approach_list, col_run = names, climateLayer = "single")

summary <- left_join(climate, df, by = "run")

write.csv(summary, paste0(output_summary, "ApproachTheme_phos_Summary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "LR-approach") +
  theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-ApproachTheme-phos-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- Get Kappa Correlation Matrix -----
object_list <- list() # empty list
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], approach_list[i])
  object_list[[i]] <- obj
}

# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#### Declining oxygen concentration ####
# ----- Create low-regret solution -----
solution_list <- list(s4, s8, s12, s36)
names <- c("EM_Percentile_o2os_585", "EM_Feature_o2os_585", "EM_Penalty_o2os_585", "EM_ClimatePriorityArea_o2os_585")
s8_LRplot <- create_LowRegretSf(solution_list, names, PUs)
saveRDS(s8_LRplot, paste0(output_lowregret, "s8-EM-LowRegret-o2os-585.rds")) # save low-regret solution
(ggLowRegret8 <- plot_lowregret(s8_LRplot, land) + theme(axis.text = element_text(size = 25)))
ggsave(filename = "LR-Approach-o2os.png",
       plot = ggLowRegret8, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- Summary statistics -----
df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}

approach_list <- c("percentile", "feature", "penalty", "climate priority area")
climate <- get_ClimateSummary(solution_list, climate_layer = roc_o2os_SSP585, metric = "o2os", col_scenario = "585", col_approach = approach_list, col_run = names, climateLayer = "single")

summary <- left_join(climate, df, by = "run")

write.csv(summary, paste0(output_summary, "ApproachTheme_o2os_Summary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "LR-approach") +
  theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-ApproachTheme-o2os-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- Get Kappa Correlation Matrix -----
object_list <- list() # empty list
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], approach_list[i])
  object_list[[i]] <- obj
}

# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))


#### Climate velocity ####
# ----- Create low-regret solution -----
solution_list <- list(s5, s9, s13, s37)
names <- c("EM_Percentile_velocity_585", "EM_Feature_velocity_585", "EM_Penalty_velocity_585", "EM_ClimatePriorityArea_velocity_585")
s9_LRplot <- create_LowRegretSf(solution_list, names, PUs)
saveRDS(s9_LRplot, paste0(output_lowregret, "s9-EM-LowRegret-velocity-585.rds")) # save low-regret solution
(ggLowRegret9 <- plot_lowregret(s9_LRplot, land) + theme(axis.text = element_text(size = 25)))
ggsave(filename = "LR-Approach-velocity.png",
       plot = ggLowRegret9, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- Summary statistics -----
df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}

approach_list <- c("percentile", "feature", "penalty", "climate priority area")
climate <- get_ClimateSummary(solution_list, climate_layer = velocity_SSP585, metric = "velocity", col_scenario = "585", col_approach = approach_list, col_run = names, climateLayer = "single")

summary <- left_join(climate, df, by = "run")
write.csv(summary, paste0(output_summary, "ApproachTheme_velocity_Summary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "LR-approach") +
  theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-ApproachTheme-velocity-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- Get Kappa Correlation Matrix -----
object_list <- list() # empty list
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], approach_list[i])
  object_list[[i]] <- obj
}

# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#### Sum of Cumulative MHW Intensity ####
# ----- Create low-regret solution -----
solution_list <- list(s290, s291, s292, s293)
names <- c("EM_Percentile_MHW_SumCumInt_585", "EM_Feature_MHW_SumCumInt_585", "EM_Penalty_MHW_SumCumInt_585", "EM_ClimatePriorityArea_MHW_SumCumInt_585")
s10_LRplot <- create_LowRegretSf(solution_list, names, PUs)
saveRDS(s10_LRplot, paste0(output_lowregret, "s10-EM-LowRegret-MHW_SumCumInt-585.rds")) # save low-regret solution
(ggLowRegret10 <- plot_lowregret(s10_LRplot, land) + theme(axis.text = element_text(size = 25)))
ggsave(filename = "LR-Approach-MHW_SumCumInt.png",
       plot = ggLowRegret10, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- Summary statistics -----
df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}

approach_list <- c("percentile", "feature", "penalty", "climate priority area")
climate <- get_ClimateSummary(solution_list, climate_layer = MHW_SumCumInt_SSP585, metric = "MHW_SumCumInt", col_scenario = "585", col_approach = approach_list, col_run = names, climateLayer = "single")

summary <- left_join(climate, df, by = "run")
write.csv(summary, paste0(output_summary, "ApproachTheme_MHW_SumCumInt_Summary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "LR-approach") +
  theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-ApproachTheme-MHW_SumCumInt-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- Get Kappa Correlation Matrix -----
object_list <- list() # empty list
for (i in 1:length(approach_list)) {
  obj <- select_solution(solution_list[[i]], approach_list[i])
  object_list[[i]] <- obj
}

# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))


# ----- Intersection of all low-regret climate-metric solutions -----
run_list <- c("LR-tos", "LR-phos", "LR-o2os", "LR-velocity", "LR-MHWSumCumInt")
solution_list <- list(s6_LRplot, s7_LRplot, s8_LRplot, s9_LRplot, s10_LRplot)
intersection <- intersect_lowregret(solution_list, run_list) %>% 
  dplyr::mutate(solution_1 = ifelse(selection == (length(run_list)*4), yes = 1, no = 0))

#### Summary statistics of low-regret climate-metric solutions ####
# ----- Load Low-regret areas -----
LR_tos <- readRDS("Output/lowregret/s6-EM-LowRegret-tos-585.rds")
LR_phos <- readRDS("Output/lowregret/s7-EM-LowRegret-phos-585.rds")
LR_o2os <- readRDS("Output/lowregret/s8-EM-LowRegret-o2os-585.rds")
LR_velocity <- readRDS("Output/lowregret/s9-EM-LowRegret-velocity-585.rds")
LR_MHW_SumCumInt <- readRDS("Output/lowregret/s10-EM-LowRegret-MHW_SumCumInt-585.rds")

# ----- Create summaries -----
solution_list <- list(LR_tos, LR_phos, LR_o2os, LR_velocity, LR_MHW_SumCumInt)
cols <- c("LR-tos", "LR-phos", "LR-o2os", "LR-velocity", "LR-MHW_SumCumInt")

df <- list() # empty list
for(i in 1:length(cols)) {
  df[[i]] <- compute_summary(solution_list[[i]], total_area, PU_size, cols[i], Cost = "cost")
}
summary <- do.call(rbind, df)
write.csv(summary, paste0(output_summary, "ApproachTheme_Metric_LowRegretSummary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "metric")  + theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-ApproachTheme-Metrics-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- Cohen's Kappa Correlation Matrix -----
object_list <- list() # empty list
for (i in 1:length(cols)) {
  obj <- select_solution(solution_list[[i]], cols[i])
  object_list[[i]] <- obj
}

# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#### Supplementary: Workflow figures for each of the approaches ####
# ----- Percentile approach -----
species <- tibble(code = character(), species = character(), AqmID = integer()) %>% 
  add_row(code = "sp1", species = "Katsuwonus_pelamis", AqmID = 4289) %>% 
  add_row(code = "sp2", species = "Thunnus_orientalis", AqmID = 8199) %>% 
  add_row(code = "sp3", species = "Stenella_coeruleoalba", AqmID = 7752) %>% 
  add_row(code = "sp4", species = "Nannobrachium_idostigma", AqmID = 5300)

workflow_percentile <- function(index, 
                                metric_name,
                                metric_df = NA,
                                scenario = NA # no punctuation
                                ) {
  
  # Plot of original distribution
  sp <- aqua_sf %>% dplyr::select(species$species[index]) %>% dplyr::mutate(!!sym(species$species[index]) := as.logical(!!sym(species$species[index])))
  
  sp_plot <- plot_AQMFeatures(sp, PUs, land, column = species$species[index]) +
    ggtitle("Species Distribution", subtitle = species$species[index]) +
    theme(axis.text = element_text(size = 25))
  
  ggsave(filename = paste0("Workflow-Percentile-", species$code[index], ".png"),
         plot = sp_plot, width = 21, height = 29.7, dpi = 300,
         path = "Figures/") # save plot
  
  if(is.na(scenario)) {break}

  # Plot retained distribution
  spFiltered <- create_PercentileLayer(aqua_sf = sp, metric_name, colname = "transformed", metric_df, PUs) %>% 
    dplyr::mutate(!!sym(species$species[index]) := as.logical(!!sym(species$species[index])))
  
  spFiltered_plot <- plot_AQMFeatures(spFiltered, PUs, land, column = species$species[index]) +
    ggtitle("Species Distribution", subtitle = species$species[index]) +
    theme(axis.text = element_text(size = 25))
  
  ggsave(filename = paste0("Workflow-Percentile-", species$code[index], "Filtered", scenario, ".png"),
         plot = spFiltered, width = 21, height = 29.7, dpi = 300,
         path = "Figures/") # save plot

}

# Create plots for Sp1 and Sp3
workflow_percentile(index = 1, metric_name = "tos", metric_df = NA, scenario = NA)
workflow_percentile(index = 1, metric_name = "tos", metric_df = roc_tos_SSP585, scenario = "SSP585")

workflow_percentile(index = 3, metric_name = "tos", metric_df = NA, scenario = NA)
workflow_percentile(index = 3, metric_name = "tos", metric_df = roc_tos_SSP585, scenario = "SSP585")

# ----- Feature Approach -----
# Plots for the workflow
climateLayer <- create_FeatureLayer(metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP585) %>%
  dplyr::select(climate_layer, geometry) %>% 
  dplyr::mutate(climate_layer = as.logical(climate_layer))

feature <- plot_AQMFeatures(climateLayer, PUs, land, column = "climate_layer") + ggtitle("Low Exposure Areas") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Workflow-Feature-climateFiltered.png",
      plot = feature, width = 21, height = 29.7, dpi = 300,
      path = "Figures/") # save plot

# ----- Climate Priority Area Approach -----
workflow_CPA <- function(index,
                         metric_name,
                         metric_df = NA,
                         scenario = NA # no punctuation
) {
  # Plot of original distribution
  sp <- aqua_sf %>% dplyr::select(species$species[index]) %>% dplyr::mutate(!!sym(species$species[index]) := as.logical(!!sym(species$species[index])))
  
  sp_plot <- plot_AQMFeatures(sp, PUs, land, column = species$species[index]) +
    ggtitle("Species Distribution", subtitle = species$species[index]) +
    theme(axis.text = element_text(size = 25))
  
  ggsave(filename = paste0("Workflow-Percentile-", species$code[index], ".png"),
         plot = sp_plot, width = 21, height = 29.7, dpi = 300,
         path = "Figures/") # save plot
  
  if(is.na(scenario)) {break}
  
  # Plot important feature (5th percentile)
  
  sp_ImptFeat <- create_ImportantFeatureLayer(sp, metric_name, colname = "transformed", metric_df) %>% 
    dplyr::mutate(!!sym(species$species[index]) := as.logical(!!sym(species$species[index])))
  
  sp_ImptFeatplot <- plot_AQMFeatures(sp_ImptFeat, PUs, land, column = species$species[index]) +
    ggtitle("Species Distribution",
            subtitle = species$species[index]) +
    theme(axis.text = element_text(size = 25))
  
  ggsave(filename = paste0("Workflow-ClimatePriorityArea-", species$code[index], "ImptFeat.png"),
         plot = sp_ImptFeatplot, width = 21, height = 29.7, dpi = 300,
         path = "Figures/")
  
  # Plot the rest of the distribution
  
  sp_RepFeat <- create_RepresentationFeature(sp_ImptFeat, sp) %>% 
    dplyr::mutate(!!sym(species$species[index]) := as.logical(!!sym(species$species[index])))
  
  sp_RepFeatplot <- plot_AQMFeatures(sp_RepFeat, PUs, land, column = species$species[index]) +
    ggtitle("Species Distribution",
            subtitle = species$species[index]) +
    theme(axis.text = element_text(size = 25))
  
  ggsave(filename = paste0("Workflow-ClimatePriorityArea-", species$code[index], "RepFeat.png"),
         plot = sp_RepFeatplot, width = 21, height = 29.7, dpi = 300,
         path = "Figures/")
}