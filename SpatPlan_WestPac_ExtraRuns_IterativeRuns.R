# title: "Climate-smart methods paper runs"
# author: "Tin Buenafe"

#### Preliminaries ####
# Description
# This code creates and analyzes spatial designs using the features and the planning region generated from `SpatPlan_Master_WestPac.R`

source("HelperFunctions/SpatPlan_Extras.R") # Load the extras, including functions and libraries
source("HelperFunctions/SpatPlan_HelperFxns_WestPac.R") # Load helper functions written specifically for this spatial planning project
output_solutions <- "Output/solutions/"
output_summary <- "Output/summary/"
output_lowregret <- "Output/lowregret/"

# Load files
source("SpatPlan_Master_Preliminaries.R")
total_area = nrow(PUs) * PU_size

#### Ensemble mean approach ####
#### Percentile approach runs ####
#### Ocean acidification (SSP1-2.6) ####
# Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "phos", colname = "transformed", metric_df = roc_phos_SSP126, PUs = PUs)

# Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, roc_phos_SSP126, UniformCost)
p40 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s40 <- prioritizr::solve(p40)
saveRDS(s40, paste0(output_solutions, "s40-EM-Percentile-phos-126.rds")) # save solution

# Plot the spatial design
s40_plot <- s40 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol40 <- fSpatPlan_PlotSolution(s40_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 1-2.6") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-phos-126.png",
       plot = ggSol40, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Ocean acidification (SSP2-4.5) ####
# Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "phos", colname = "transformed", metric_df = roc_phos_SSP245, PUs = PUs)

# Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, roc_phos_SSP245, UniformCost)
p41 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s41 <- prioritizr::solve(p41)
saveRDS(s41, paste0(output_solutions, "s41-EM-Percentile-phos-245.rds")) # save solution

# Plot the spatial design
s41_plot <- s41 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol41 <- fSpatPlan_PlotSolution(s41_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 2-4.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-phos-245.png",
       plot = ggSol41, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Declining oxygen concentration (SSP1-2.6) ####
# Prepare climate layer
# Retain only planning units of each of the biodiversity features that in intersect with areas of low exposure (>= 65th percentile)
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "transformed", metric_df = roc_o2os_SSP126, PUs = PUs)

# Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, roc_o2os_SSP126, UniformCost)
p42 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s42 <- prioritizr::solve(p42)
saveRDS(s42, paste0(output_solutions, "s42-EM-Percentile-o2os-126.rds")) # save solution

# Plot the spatial design
s42_plot <- s42 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol42 <- fSpatPlan_PlotSolution(s42_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Percentile, SSP 1-2.6") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-o2os-126.png",
       plot = ggSol42, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Declining oxygen concentration (SSP2-4.5) ####
# Prepare climate layer
# Retain only planning units of each of the biodiversity features that in intersect with areas of low exposure (>= 65th percentile)
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "transformed", metric_df = roc_o2os_SSP245, PUs = PUs)

# Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, roc_o2os_SSP245, UniformCost)
p43 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s43 <- prioritizr::solve(p43)
saveRDS(s43, paste0(output_solutions, "s43-EM-Percentile-o2os-245.rds")) # save solution

# Plot the spatial design
s43_plot <- s43 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol43 <- fSpatPlan_PlotSolution(s43_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Percentile, SSP 2-4.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-o2os-245.png",
       plot = ggSol43, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Climate velocity (SSP1-2.6) ####
# Prepare climate layer
# Retain only planning units of each of the biodiversity features that in intersect with areas of low exposure (<= 35th percentile)
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "transformed", metric_df = velocity_SSP126, PUs = PUs)

# Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, velocity_SSP126, UniformCost)
p44 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s44 <- prioritizr::solve(p44)
saveRDS(s44, paste0(output_solutions, "s44-EM-Percentile-velocity-126.rds")) # save solution

# Plot the spatial design
s44_plot <- s44 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol44 <- fSpatPlan_PlotSolution(s44_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 1-2.6") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-velocity-126.png",
       plot = ggSol44, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Climate velocity (SSP2-4.5) ####
# Prepare climate layer
# Retain only planning units of each of the biodiversity features that in intersect with areas of low exposure (<= 35th percentile)
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "transformed", metric_df = velocity_SSP245, PUs = PUs)

# Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, velocity_SSP245, UniformCost)
p45 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s45 <- prioritizr::solve(p45)
saveRDS(s45, paste0(output_solutions, "s45-EM-Percentile-velocity-245.rds")) # save solution

# Plot the spatial design
s45_plot <- s45 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol45 <- fSpatPlan_PlotSolution(s45_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 2-4.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-velocity-245.png",
       plot = ggSol45, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Feature approach runs ####
#### Climate warming (SSP1-2.6) ####
# Prepare climate layer
# Retain only planning units of each of the biodiversity features that in intersect with areas of low exposure (<= 35th percentile)
ClimateFeature <- create_FeatureLayer(metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP126)

# Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
features <- append(features, "climate_layer") # add "climate_layer" to features

# Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, UniformCost)
p46 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s46 <- prioritizr::solve(p46)
saveRDS(s46, paste0(output_solutions, "s46-EM-Feature-tos-126.rds")) # save solution

# Plot the spatial design
s46_plot <- s46 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol46 <- fSpatPlan_PlotSolution(s46_plot, PUs, land) + ggtitle("Climate-smart design: Climate Warming", subtitle = "Feature, SSP 1-2.6") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Feature-tos-126.png",
       plot = ggSol46, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Climate warming (SSP2-4.5) ####
# Prepare climate layer
# Retain only planning units of each of the biodiversity features that in intersect with areas of low exposure (<= 35th percentile)
ClimateFeature <- create_FeatureLayer(metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP245)

# Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
features <- append(features, "climate_layer") # add "climate_layer" to features

# Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, UniformCost)
p47 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s47 <- prioritizr::solve(p47)
saveRDS(s47, paste0(output_solutions, "s47-EM-Feature-tos-245.rds")) # save solution

# Plot the spatial design
s47_plot <- s47 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol47 <- fSpatPlan_PlotSolution(s47_plot, PUs, land) + ggtitle("Climate-smart design: Climate Warming", subtitle = "Feature, SSP 2-4.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Feature-tos-245.png",
       plot = ggSol47, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Ocean Acidification (SSP1-2.6) ####
# Prepare climate layer
# Retain only planning units of each of the biodiversity features that in intersect with areas of low exposure (>= 65th percentile)
ClimateFeature <- create_FeatureLayer(metric_name = "phos", colname = "transformed", metric_df = roc_phos_SSP126)

# Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
features <- append(features, "climate_layer") # add "climate_layer" to features

# Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, UniformCost)
p48 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s48 <- prioritizr::solve(p48)
saveRDS(s48, paste0(output_solutions, "s48-EM-Feature-phos-126.rds")) # save solution

# Plot the spatial design
s48_plot <- s48 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol48 <- fSpatPlan_PlotSolution(s48_plot, PUs, land) + ggtitle("Climate-smart design: Ocean Acidification", subtitle = "Feature, SSP 1-2.6") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Feature-phos-126.png",
       plot = ggSol48, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Ocean Acidification (SSP2-4.5) ####
# Prepare climate layer
# Retain only planning units of each of the biodiversity features that in intersect with areas of low exposure (>= 65th percentile)
ClimateFeature <- create_FeatureLayer(metric_name = "phos", colname = "transformed", metric_df = roc_phos_SSP245)

# Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
features <- append(features, "climate_layer") # add "climate_layer" to features

# Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, UniformCost)
p49 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s49 <- prioritizr::solve(p49)
saveRDS(s49, paste0(output_solutions, "s49-EM-Feature-phos-245.rds")) # save solution

# Plot the spatial design
s49_plot <- s49 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol49 <- fSpatPlan_PlotSolution(s49_plot, PUs, land) + ggtitle("Climate-smart design: Ocean Acidification", subtitle = "Feature, SSP 2-4.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Feature-phos-245.png",
       plot = ggSol49, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Declining Oxygen Concentration (SSP1-2.6) ####
# Prepare climate layer
# Retain only planning units of each of the biodiversity features that in intersect with areas of low exposure (>= 65th percentile)
ClimateFeature <- create_FeatureLayer(metric_name = "o2os", colname = "transformed", metric_df = roc_o2os_SSP126)

# Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
features <- append(features, "climate_layer") # add "climate_layer" to features

# Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, UniformCost)
p50 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s50 <- prioritizr::solve(p50)
saveRDS(s50, paste0(output_solutions, "s50-EM-Feature-o2os-126.rds")) # save solution

# Plot the spatial design
s50_plot <- s50 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol50 <- fSpatPlan_PlotSolution(s50_plot, PUs, land) + ggtitle("Climate-smart design: Declining Oxygen Concentration", subtitle = "Feature, SSP 1-2.6") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Feature-o2os-126.png",
       plot = ggSol50, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Declining Oxygen Concentration (SSP2-4.5) ####
# Prepare climate layer
# Retain only planning units of each of the biodiversity features that in intersect with areas of low exposure (>= 65th percentile)
ClimateFeature <- create_FeatureLayer(metric_name = "o2os", colname = "transformed", metric_df = roc_o2os_SSP245)

# Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
features <- append(features, "climate_layer") # add "climate_layer" to features

# Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, UniformCost)
p51 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s51 <- prioritizr::solve(p51)
saveRDS(s51, paste0(output_solutions, "s51-EM-Feature-o2os-245.rds")) # save solution

# Plot the spatial design
s51_plot <- s51 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol51 <- fSpatPlan_PlotSolution(s51_plot, PUs, land) + ggtitle("Climate-smart design: Declining Oxygen Concentration", subtitle = "Feature, SSP 2-4.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Feature-o2os-245.png",
       plot = ggSol51, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Climate velocity (SSP1-2.6) ####
# Prepare climate layer
# Retain only planning units of each of the biodiversity features that in intersect with areas of low exposure (<=35th percentile)
ClimateFeature <- create_FeatureLayer(metric_name = "velocity", colname = "transformed", metric_df = velocity_SSP126)

# Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
features <- append(features, "climate_layer") # add "climate_layer" to features

# Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, UniformCost)
p52 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s52 <- prioritizr::solve(p52)
saveRDS(s52, paste0(output_solutions, "s52-EM-Feature-velocity-126.rds")) # save solution

# Plot the spatial design
s52_plot <- s52 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol52 <- fSpatPlan_PlotSolution(s52_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Feature, SSP 1-2.6") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Feature-velocity-126.png",
       plot = ggSol52, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Climate velocity (SSP2-4.5) ####
# Prepare climate layer
# Retain only planning units of each of the biodiversity features that in intersect with areas of low exposure (<= 35th percentile)
ClimateFeature <- create_FeatureLayer(metric_name = "velocity", colname = "transformed", metric_df = velocity_SSP245)

# Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
features <- append(features, "climate_layer") # add "climate_layer" to features

# Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, UniformCost)
p53 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s53 <- prioritizr::solve(p53)
saveRDS(s53, paste0(output_solutions, "s53-EM-Feature-velocity-245.rds")) # save solution

# Plot the spatial design
s53_plot <- s53 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol53 <- fSpatPlan_PlotSolution(s53_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Feature, SSP 2-4.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Feature-velocity-245.png",
       plot = ggSol53, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Penalty approach runs ####
#### Climate Warming (SSP1-2.6) ####
# Prepare climate layer
# Get scaling
scaling_PenaltyWarming <- create_Scaling(UniformCost$cost, roc_tos_SSP126$transformed, "tos")

# Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_sf, roc_tos_SSP126, UniformCost)
scaling <- scaling_PenaltyWarming %>% filter(scaling == 30) %>% pull() # get scaling for 30%
p54 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")

# Solve the planning problem 
s54 <- prioritizr::solve(p54)
saveRDS(s54, paste0(output_solutions, "s54-EM-Penalty-tos-126.rds")) # save solution

# Plot the spatial design
s54_plot <- s54 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol54 <- fSpatPlan_PlotSolution(s54_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Penalty, SSP 1-2.6") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Penalty-tos-126.png",
       plot = ggSol54, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

#### Climate Warming (SSP2-4.5) ####
# Prepare climate layer
# Get scaling
scaling_PenaltyWarming <- create_Scaling(UniformCost$cost, roc_tos_SSP245$transformed, "tos")

# Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_sf, roc_tos_SSP245, UniformCost)
scaling <- scaling_PenaltyWarming %>% filter(scaling == 30) %>% pull() # get scaling for 30%
p55 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")

# Solve the planning problem 
s55 <- prioritizr::solve(p55)
saveRDS(s55, paste0(output_solutions, "s55-EM-Penalty-tos-245.rds")) # save solution

# Plot the spatial design
s55_plot <- s55 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol55 <- fSpatPlan_PlotSolution(s55_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Penalty, SSP 2-4.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Penalty-tos-245.png",
       plot = ggSol55, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

#### Ocean Acidification (SSP1-2.6) ####
# Prepare climate layer
# Get scaling
scaling_PenaltyWarming <- create_Scaling(UniformCost$cost, roc_phos_SSP126$transformed, "phos")

# Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_sf, roc_phos_SSP126, UniformCost)
scaling <- scaling_PenaltyWarming %>% filter(scaling == 30) %>% pull() # get scaling for 30%
p56 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")

# Solve the planning problem 
s56 <- prioritizr::solve(p56)
saveRDS(s56, paste0(output_solutions, "s56-EM-Penalty-phos-126.rds")) # save solution

# Plot the spatial design
s56_plot <- s56 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol56 <- fSpatPlan_PlotSolution(s56_plot, PUs, land) + ggtitle("Climate-smart design: Ocean Acidification", subtitle = "Penalty, SSP 1-2.6") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Penalty-phos-126.png",
       plot = ggSol56, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

#### Ocean Acidification (SSP2-4.5) ####
# Prepare climate layer
# Get scaling
scaling_PenaltyWarming <- create_Scaling(UniformCost$cost, roc_phos_SSP245$transformed, "phos")

# Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_sf, roc_phos_SSP245, UniformCost)
scaling <- scaling_PenaltyWarming %>% filter(scaling == 30) %>% pull() # get scaling for 30%
p57 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")

# Solve the planning problem 
s57 <- prioritizr::solve(p57)
saveRDS(s57, paste0(output_solutions, "s57-EM-Penalty-phos-245.rds")) # save solution

# Plot the spatial design
s57_plot <- s57 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol57 <- fSpatPlan_PlotSolution(s57_plot, PUs, land) + ggtitle("Climate-smart design: Ocean Acidification", subtitle = "Penalty, SSP 2-4.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Penalty-phos-245.png",
       plot = ggSol57, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

#### Declining Oxygen Concentration (SSP1-2.6) ####
# Prepare climate layer
# Get scaling
scaling_PenaltyWarming <- create_Scaling(UniformCost$cost, roc_o2os_SSP126$transformed, "o2os")

# Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_sf, roc_o2os_SSP126, UniformCost)
scaling <- scaling_PenaltyWarming %>% filter(scaling == 30) %>% pull() # get scaling for 30%
p58 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")

# Solve the planning problem 
s58 <- prioritizr::solve(p58)
saveRDS(s58, paste0(output_solutions, "s58-EM-Penalty-o2os-126.rds")) # save solution

# Plot the spatial design
s58_plot <- s58 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol58 <- fSpatPlan_PlotSolution(s58_plot, PUs, land) + ggtitle("Climate-smart design: Declining Oxygen Concentration", subtitle = "Penalty, SSP 1-2.6") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Penalty-o2os-126.png",
       plot = ggSol58, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

#### Declining Oxygen Concentration (SSP2-4.5) ####
# Prepare climate layer
# Get scaling
scaling_PenaltyWarming <- create_Scaling(UniformCost$cost, roc_o2os_SSP245$transformed, "o2os")

# Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_sf, roc_o2os_SSP245, UniformCost)
scaling <- scaling_PenaltyWarming %>% filter(scaling == 30) %>% pull() # get scaling for 30%
p59 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")

# Solve the planning problem 
s59 <- prioritizr::solve(p59)
saveRDS(s59, paste0(output_solutions, "s59-EM-Penalty-o2os-245.rds")) # save solution

# Plot the spatial design
s59_plot <- s59 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol59 <- fSpatPlan_PlotSolution(s59_plot, PUs, land) + ggtitle("Climate-smart design: Declining Oxygen Concentration", subtitle = "Penalty, SSP 2-4.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Penalty-o2os-245.png",
       plot = ggSol59, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

#### Velocity (SSP1-2.6) ####
# Prepare climate layer
# Get scaling
scaling_PenaltyWarming <- create_Scaling(UniformCost$cost, velocity_SSP126$transformed, "velocity")

# Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_sf, velocity_SSP126, UniformCost)
scaling <- scaling_PenaltyWarming %>% filter(scaling == 30) %>% pull() # get scaling for 30%
p60 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")

# Solve the planning problem 
s60 <- prioritizr::solve(p60)
saveRDS(s60, paste0(output_solutions, "s60-EM-Penalty-velocity-126.rds")) # save solution

# Plot the spatial design
s60_plot <- s60 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol60 <- fSpatPlan_PlotSolution(s60_plot, PUs, land) + ggtitle("Climate-smart design: Climate velocity", subtitle = "Penalty, SSP 1-2.6") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Penalty-velocity-126.png",
       plot = ggSol60, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

#### Velocity (SSP2-4.5) ####
# Prepare climate layer
# Get scaling
scaling_PenaltyWarming <- create_Scaling(UniformCost$cost, velocity_SSP245$transformed, "velocity")

# Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_sf, velocity_SSP245, UniformCost)
scaling <- scaling_PenaltyWarming %>% filter(scaling == 30) %>% pull() # get scaling for 30%
p61 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "transformed")

# Solve the planning problem 
s61 <- prioritizr::solve(p61)
saveRDS(s61, paste0(output_solutions, "s61-EM-Penalty-velocity-245.rds")) # save solution

# Plot the spatial design
s61_plot <- s61 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol61 <- fSpatPlan_PlotSolution(s61_plot, PUs, land) + ggtitle("Climate-smart design: Climate velocity", subtitle = "Penalty, SSP 2-4.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Penalty-velocity-245.png",
       plot = ggSol61, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

#### Multi-model ensemble approach ####
#### Percentile approach runs ####
#### Climate warming (SSP 1-2.6) ####
# Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = tos_CanESM5_SSP126, PUs = PUs)

# Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, tos_CanESM5_SSP126, UniformCost)
p70 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s70 <- prioritizr::solve(p70)
saveRDS(s70, paste0(output_solutions, "s70-MM-CanESM5-Percentile-tos-126.rds")) # save solution

# Plot the spatial design
s70_plot <- s70 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol70 <- fSpatPlan_PlotSolution(s70_plot, PUs, land) + ggtitle("Climate-smart design: Climate Warming", subtitle = "Percentile, SSP 1-2.6 (GCM: CanESM5)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-CanESM5-Percentile-tos-126.png",
       plot = ggSol70, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = `tos_CMCC-ESM2_SSP126`, PUs = PUs)

# Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `tos_CMCC-ESM2_SSP126`, UniformCost)
p71 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s71 <- prioritizr::solve(p71)
saveRDS(s71, paste0(output_solutions, "s71-MM-CMCC_ESM2-Percentile-tos-126.rds")) # save solution

# Plot the spatial design
s71_plot <- s71 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol71 <- fSpatPlan_PlotSolution(s71_plot, PUs, land) + ggtitle("Climate-smart design: Climate Warming", subtitle = "Percentile, SSP 1-2.6 (GCM: CMCC-ESM2)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-CMCC_ESM2-Percentile-tos-126.png",
       plot = ggSol71, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = `tos_GFDL-ESM4_SSP126`, PUs = PUs)

# Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `tos_GFDL-ESM4_SSP126`, UniformCost)
p72 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s72 <- prioritizr::solve(p72)
saveRDS(s72, paste0(output_solutions, "s72-MM-GFDL-ESM4-Percentile-tos-126.rds")) # save solution

# Plot the spatial design
s72_plot <- s72 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol72 <- fSpatPlan_PlotSolution(s72_plot, PUs, land) + ggtitle("Climate-smart design: Climate Warming", subtitle = "Percentile, SSP 1-2.6 (GCM: GFDL-ESM4)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-GFDL-ESM4-Percentile-tos-126.png",
       plot = ggSol72, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = `tos_IPSL-CM6A-LR_SSP126`, PUs = PUs)

# Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `tos_IPSL-CM6A-LR_SSP126`, UniformCost)
p73 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s73 <- prioritizr::solve(p73)
saveRDS(s73, paste0(output_solutions, "s73-MM-IPSL-CM6A-LR-Percentile-tos-126.rds")) # save solution

# Plot the spatial design
s73_plot <- s73 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol73 <- fSpatPlan_PlotSolution(s73_plot, PUs, land) + ggtitle("Climate-smart design: Climate Warming", subtitle = "Percentile, SSP 1-2.6 (GCM: IPSL-CM6A-LR)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-IPSL-CM6A-LR-Percentile-tos-126.png",
       plot = ggSol73, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = `tos_NorESM2-MM_SSP126`, PUs = PUs)

# Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `tos_NorESM2-MM_SSP126`, UniformCost)
p74 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# Solve the planning problem 
s74 <- prioritizr::solve(p74)
saveRDS(s74, paste0(output_solutions, "s74-MM-NorESM2_MM-Percentile-tos-126.rds")) # save solution

# Plot the spatial design
s74_plot <- s74 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol74 <- fSpatPlan_PlotSolution(s74_plot, PUs, land) + ggtitle("Climate-smart design: Climate Warming", subtitle = "Percentile, SSP 1-2.6 (GCM: NorESM2-MM)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-NorESM2_MM-Percentile-tos-126.png",
       plot = ggSol74, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Climate warming (SSP 2-4.5) ####
solution <- c("s82", "s83", "s84", "s85", "s86")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayer <- list(tos_CanESM5_SSP245, `tos_CMCC-ESM2_SSP245`, `tos_GFDL-ESM4_SSP245`, `tos_IPSL-CM6A-LR_SSP245`, `tos_NorESM2-MM_SSP245`)

for(i in 1:length(solution)) {
  # Prepare climate layer
  aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = climateLayer[[i]], PUs = PUs)
  
  # Get list of features
  features <- aqua_percentile %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    names()
  
  # Set up the spatial planning problem
  out_sf <- cbind(aqua_percentile, climateLayer[[i]], UniformCost)
  p <- prioritizr::problem(out_sf, features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(30/35) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 0, verbose = FALSE)
  
  # Solve the planning problem 
  s <- prioritizr::solve(p)
  saveRDS(s, paste0(output_solutions, solution[i], "-MM-", models[i], "-Percentile-tos-245.rds")) # save solution
  print(paste0("Saved solution: ", models[i]))
  
  # Plot the spatial design
  s_plot <- s %>% 
    mutate(solution_1 = as.logical(solution_1))
  (ggSol <- fSpatPlan_PlotSolution(s_plot, PUs, land) + ggtitle("Climate-smart design: Climate Warming", subtitle = paste0("Percentile, SSP 2-4.5 (GCM:", models[i], ")")) + theme(axis.text = element_text(size = 25)))
  ggsave(filename = paste0("MM-", models[i], "-Percentile-tos-245.png"),
         plot = ggSol, width = 21, height = 29.7, dpi = 300,
         path = "Figures/") # save plot
  
  print(paste0("Saved figure: ", models[i]))
}

#### Ocean acidification (SSP 1-2.6) ####
solution <- c("s62", "s63", "s64", "s65", "s66")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayer <- list(phos_CanESM5_SSP126, `phos_CMCC-ESM2_SSP126`, `phos_GFDL-ESM4_SSP126`, `phos_IPSL-CM6A-LR_SSP126`, `phos_NorESM2-MM_SSP126`)

for(i in 1:length(solution)) {
  # Prepare climate layer
  aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "phos", colname = "transformed", metric_df = climateLayer[[i]], PUs = PUs)
  
  # Get list of features
  features <- aqua_percentile %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    names()
  
  # Set up the spatial planning problem
  out_sf <- cbind(aqua_percentile, climateLayer[[i]], UniformCost)
  p <- prioritizr::problem(out_sf, features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(30/35) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 0, verbose = FALSE)
  
  # Solve the planning problem 
  s <- prioritizr::solve(p)
  saveRDS(s, paste0(output_solutions, solution[i], "-MM-", models[i], "-Percentile-phos-126.rds")) # save solution
  print(paste0("Saved solution: ", models[i]))
  
  # Plot the spatial design
  s_plot <- s %>% 
    mutate(solution_1 = as.logical(solution_1))
  (ggSol <- fSpatPlan_PlotSolution(s_plot, PUs, land) + ggtitle("Climate-smart design: Ocean Acidification", subtitle = paste0("Percentile, SSP 1-2.6 (GCM:", models[i], ")")) + theme(axis.text = element_text(size = 25)))
  ggsave(filename = paste0("MM-", models[i], "-Percentile-phos-126.png"),
         plot = ggSol, width = 21, height = 29.7, dpi = 300,
         path = "Figures/") # save plot
  
  print(paste0("Saved figure: ", models[i]))
}

#### Ocean acidification (SSP 2-4.5) ####
solution <- c("s87", "s88", "s89", "s90", "s91")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayer <- list(phos_CanESM5_SSP245, `phos_CMCC-ESM2_SSP245`, `phos_GFDL-ESM4_SSP245`, `phos_IPSL-CM6A-LR_SSP245`, `phos_NorESM2-MM_SSP245`)

for(i in 1:length(solution)) {
  # Prepare climate layer
  aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "phos", colname = "transformed", metric_df = climateLayer[[i]], PUs = PUs)
  
  # Get list of features
  features <- aqua_percentile %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    names()
  
  # Set up the spatial planning problem
  out_sf <- cbind(aqua_percentile, climateLayer[[i]], UniformCost)
  p <- prioritizr::problem(out_sf, features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(30/35) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 0, verbose = FALSE)
  
  # Solve the planning problem 
  s <- prioritizr::solve(p)
  saveRDS(s, paste0(output_solutions, solution[i], "-MM-", models[i], "-Percentile-phos-245.rds")) # save solution
  print(paste0("Saved solution: ", models[i]))
  
  # Plot the spatial design
  s_plot <- s %>% 
    mutate(solution_1 = as.logical(solution_1))
  (ggSol <- fSpatPlan_PlotSolution(s_plot, PUs, land) + ggtitle("Climate-smart design: Ocean Acidification", subtitle = paste0("Percentile, SSP 2-4.5 (GCM:", models[i], ")")) + theme(axis.text = element_text(size = 25)))
  ggsave(filename = paste0("MM-", models[i], "-Percentile-phos-245.png"),
         plot = ggSol, width = 21, height = 29.7, dpi = 300,
         path = "Figures/") # save plot
  
  print(paste0("Saved figure: ", models[i]))
}

#### Declining oxygen concentration (SSP 1-2.6) ####
solution <- c("s67", "s68", "s69", "s75", "s76")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayer <- list(o2os_CanESM5_SSP126, `o2os_CMCC-ESM2_SSP126`, `o2os_GFDL-ESM4_SSP126`, `o2os_IPSL-CM6A-LR_SSP126`, `o2os_NorESM2-MM_SSP126`)

for(i in 1:length(solution)) {
  # Prepare climate layer
  aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "transformed", metric_df = climateLayer[[i]], PUs = PUs)
  
  # Get list of features
  features <- aqua_percentile %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    names()
  
  # Set up the spatial planning problem
  out_sf <- cbind(aqua_percentile, climateLayer[[i]], UniformCost)
  p <- prioritizr::problem(out_sf, features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(30/35) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 0, verbose = FALSE)
  
  # Solve the planning problem 
  s <- prioritizr::solve(p)
  saveRDS(s, paste0(output_solutions, solution[i], "-MM-", models[i], "-Percentile-o2os-126.rds")) # save solution
  print(paste0("Saved solution: ", models[i]))
  
  # Plot the spatial design
  s_plot <- s %>% 
    mutate(solution_1 = as.logical(solution_1))
  (ggSol <- fSpatPlan_PlotSolution(s_plot, PUs, land) + ggtitle("Climate-smart design: Declining Oxygen Concentration", subtitle = paste0("Percentile, SSP 1-2.6 (GCM:", models[i], ")")) + theme(axis.text = element_text(size = 25)))
  ggsave(filename = paste0("MM-", models[i], "-Percentile-o2os-126.png"),
         plot = ggSol, width = 21, height = 29.7, dpi = 300,
         path = "Figures/") # save plot
  
  print(paste0("Saved figure: ", models[i]))
}
#### Declining oxygen concentration (SSP 2-4.5) ####
solution <- c("s92", "s93", "s94", "s95", "s96")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayer <- list(o2os_CanESM5_SSP245, `o2os_CMCC-ESM2_SSP245`, `o2os_GFDL-ESM4_SSP245`, `o2os_IPSL-CM6A-LR_SSP245`, `o2os_NorESM2-MM_SSP245`)

for(i in 1:length(solution)) {
  # Prepare climate layer
  aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "transformed", metric_df = climateLayer[[i]], PUs = PUs)
  
  # Get list of features
  features <- aqua_percentile %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    names()
  
  # Set up the spatial planning problem
  out_sf <- cbind(aqua_percentile, climateLayer[[i]], UniformCost)
  p <- prioritizr::problem(out_sf, features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(30/35) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 0, verbose = FALSE)
  
  # Solve the planning problem 
  s <- prioritizr::solve(p)
  saveRDS(s, paste0(output_solutions, solution[i], "-MM-", models[i], "-Percentile-o2os-245.rds")) # save solution
  print(paste0("Saved solution: ", models[i]))
  
  # Plot the spatial design
  s_plot <- s %>% 
    mutate(solution_1 = as.logical(solution_1))
  (ggSol <- fSpatPlan_PlotSolution(s_plot, PUs, land) + ggtitle("Climate-smart design: Declining Oxygen Concentration", subtitle = paste0("Percentile, SSP 2-4.5 (GCM:", models[i], ")")) + theme(axis.text = element_text(size = 25)))
  ggsave(filename = paste0("MM-", models[i], "-Percentile-o2os-245.png"),
         plot = ggSol, width = 21, height = 29.7, dpi = 300,
         path = "Figures/") # save plot
  
  print(paste0("Saved figure: ", models[i]))
}
#### Climate velocity (SSP 1-2.6) ####
solution <- c("s77", "s78", "s79", "s80", "s81")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayer <- list(velocity_CanESM5_SSP126, `velocity_CMCC-ESM2_SSP126`, `velocity_GFDL-ESM4_SSP126`, `velocity_IPSL-CM6A-LR_SSP126`, `velocity_NorESM2-MM_SSP126`)

for(i in 1:length(solution)) {
  # Prepare climate layer
  aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "transformed", metric_df = climateLayer[[i]], PUs = PUs)
  
  # Get list of features
  features <- aqua_percentile %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    names()
  
  # Set up the spatial planning problem
  out_sf <- cbind(aqua_percentile, climateLayer[[i]], UniformCost)
  p <- prioritizr::problem(out_sf, features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(30/35) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 0, verbose = FALSE)
  
  # Solve the planning problem 
  s <- prioritizr::solve(p)
  saveRDS(s, paste0(output_solutions, solution[i], "-MM-", models[i], "-Percentile-velocity-126.rds")) # save solution
  print(paste0("Saved solution: ", models[i]))
  
  # Plot the spatial design
  s_plot <- s %>% 
    mutate(solution_1 = as.logical(solution_1))
  (ggSol <- fSpatPlan_PlotSolution(s_plot, PUs, land) + ggtitle("Climate-smart design: Climate velocity", subtitle = paste0("Percentile, SSP 1-2.6 (GCM:", models[i], ")")) + theme(axis.text = element_text(size = 25)))
  ggsave(filename = paste0("MM-", models[i], "-Percentile-velocity-126.png"),
         plot = ggSol, width = 21, height = 29.7, dpi = 300,
         path = "Figures/") # save plot
  
  print(paste0("Saved figure: ", models[i]))
}
#### Climate velocity (SSP 2-4.5) ####
solution <- c("s97", "s98", "s99", "s100", "s101")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayer <- list(velocity_CanESM5_SSP245, `velocity_CMCC-ESM2_SSP245`, `velocity_GFDL-ESM4_SSP245`, `velocity_IPSL-CM6A-LR_SSP245`, `velocity_NorESM2-MM_SSP245`)

for(i in 1:length(solution)) {
  # Prepare climate layer
  aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "transformed", metric_df = climateLayer[[i]], PUs = PUs)
  
  # Get list of features
  features <- aqua_percentile %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    names()
  
  # Set up the spatial planning problem
  out_sf <- cbind(aqua_percentile, climateLayer[[i]], UniformCost)
  p <- prioritizr::problem(out_sf, features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(30/35) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 0, verbose = FALSE)
  
  # Solve the planning problem 
  s <- prioritizr::solve(p)
  saveRDS(s, paste0(output_solutions, solution[i], "-MM-", models[i], "-Percentile-velocity-245.rds")) # save solution
  print(paste0("Saved solution: ", models[i]))
  
  # Plot the spatial design
  s_plot <- s %>% 
    mutate(solution_1 = as.logical(solution_1))
  (ggSol <- fSpatPlan_PlotSolution(s_plot, PUs, land) + ggtitle("Climate-smart design: Climate velocity", subtitle = paste0("Percentile, SSP 2-4.5 (GCM:", models[i], ")")) + theme(axis.text = element_text(size = 25)))
  ggsave(filename = paste0("MM-", models[i], "-Percentile-velocity-245.png"),
         plot = ggSol, width = 21, height = 29.7, dpi = 300,
         path = "Figures/") # save plot
  
  print(paste0("Saved figure: ", models[i]))
}

#### Feature approach runs ####
# TODO: Change this to a more inclusive function later on.
makeIterations_Feature <- function(solutions, # name of solutions
                                   models, # name of models
                                   climateLayers, # list of climate layers
                                   metric,
                                   scenario) {
  
  for(i in 1:length(solutions)) {
    # Prepare climate layer
    ClimateFeature <- create_FeatureLayer(metric_name = metric, colname = "transformed", metric_df = climateLayer[[i]])
    
    # Get list of features
    features <- aqua_sf %>% 
      as_tibble() %>% 
      dplyr::select(-geometry) %>% 
      names()
    features <- append(features, "climate_layer") # add "climate_layer" to features
    
    # Set up the spatial planning problem
    out_sf <- cbind(aqua_sf, ClimateFeature, UniformCost)
    p <- prioritizr::problem(out_sf, features, "cost") %>%
      add_min_set_objective() %>%
      add_relative_targets(0.3) %>% 
      add_binary_decisions() %>%
      add_gurobi_solver(gap = 0, verbose = FALSE)
    
    # Solve the planning problem 
    s <- prioritizr::solve(p)
    saveRDS(s, paste0(output_solutions, solutions[i], "-MM-", models[i], "-Feature-", metric, "-", scenario, ".rds")) # save solution
    print(paste0("Saved solution: ", models[i]))
    
    # Plot the spatial design
    s_plot <- s %>% 
      mutate(solution_1 = as.logical(solution_1)) 
    (ggSol <- fSpatPlan_PlotSolution(s_plot, PUs, land) + ggtitle(paste0("Climate-smart design: ", metric), subtitle = paste0("Feature, SSP ", scenario, " (GCM: ", models[i], ")")) + theme(axis.text = element_text(size = 25)))
    ggsave(filename = paste0("MM-", models[i], "-Feature-", metric, "-", scenario, ".png"),
           plot = ggSol, width = 21, height = 29.7, dpi = 300,
           path = "Figures/") # save
    print(paste0("Saved figure: ", models[i]))
  }
  
}
#### Climate warming (SSP 1-2.6) ####
solutions <- c("s142", "s143", "s144", "s145", "s146")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayer <- list(tos_CanESM5_SSP126, `tos_CMCC-ESM2_SSP126`, `tos_GFDL-ESM4_SSP126`, `tos_IPSL-CM6A-LR_SSP126`, `tos_NorESM2-MM_SSP126`)
metric = "tos"
scenario = "126"

makeIterations_Feature(solutions, models, climateLayer, metric, scenario)
#### Climate warming (SSP 2-4.5) ####
solutions <- c("s162", "s163", "s164", "s165", "s166")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayer <- list(tos_CanESM5_SSP245, `tos_CMCC-ESM2_SSP245`, `tos_GFDL-ESM4_SSP245`, `tos_IPSL-CM6A-LR_SSP245`, `tos_NorESM2-MM_SSP245`)
metric = "tos"
scenario = "245"

makeIterations_Feature(solutions, models, climateLayer, metric, scenario)
#### Climate warming (SSP 5-8.5) ####
solutions <- c("s202", "s203", "s204", "s205", "s206")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayer <- list(tos_CanESM5_SSP585, `tos_CMCC-ESM2_SSP585`, `tos_GFDL-ESM4_SSP585`, `tos_IPSL-CM6A-LR_SSP585`, `tos_NorESM2-MM_SSP585`)
metric = "tos"
scenario = "585"

makeIterations_Feature(solutions, models, climateLayer, metric, scenario)
#### Ocean Acidification (SSP 1-2.6) ####
solutions <- c("s147", "s148", "s149", "s150", "s151")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayer <- list(phos_CanESM5_SSP126, `phos_CMCC-ESM2_SSP126`, `phos_GFDL-ESM4_SSP126`, `phos_IPSL-CM6A-LR_SSP126`, `phos_NorESM2-MM_SSP126`)
metric = "phos"
scenario = "126"

makeIterations_Feature(solutions, models, climateLayer, metric, scenario)
#### Ocean Acidification (SSP 2-4.5) ####
solutions <- c("s167", "s168", "s169", "s170", "s171")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayer <- list(phos_CanESM5_SSP245, `phos_CMCC-ESM2_SSP245`, `phos_GFDL-ESM4_SSP245`, `phos_IPSL-CM6A-LR_SSP245`, `phos_NorESM2-MM_SSP245`)
metric = "phos"
scenario = "126"

makeIterations_Feature(solutions, models, climateLayer, metric, scenario)
#### Ocean Acidification (SSP 5-8.5) ####
solutions <- c("s207", "s208", "s209", "s210", "s211")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayer <- list(phos_CanESM5_SSP585, `phos_CMCC-ESM2_SSP585`, `phos_GFDL-ESM4_SSP585`, `phos_IPSL-CM6A-LR_SSP585`, `phos_NorESM2-MM_SSP585`)
metric = "phos"
scenario = "585"

makeIterations_Feature(solutions, models, climateLayer, metric, scenario)

#### Declining oxygen concentration (SSP 1-2.6) ####
solutions <- c("s152", "s153", "s154", "s155", "s156")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(o2os_CanESM5_SSP126, `o2os_CMCC-ESM2_SSP126`, `o2os_GFDL-ESM4_SSP126`, `o2os_IPSL-CM6A-LR_SSP126`, `o2os_NorESM2-MM_SSP126`)
metric = "o2os"
scenario = "126"

makeIterations_Feature(solutions, models, climateLayers, metric, scenario)
#### Declining oxygen concentration (SSP 2-4.5) ####
solutions <- c("s172", "s173", "s174", "s175", "s176")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(o2os_CanESM5_SSP245, `o2os_CMCC-ESM2_SSP245`, `o2os_GFDL-ESM4_SSP245`, `o2os_IPSL-CM6A-LR_SSP245`, `o2os_NorESM2-MM_SSP245`)
metric = "o2os"
scenario = "245"

makeIterations_Feature(solutions, models, climateLayers, metric, scenario)

#### Declining oxygen concentration (SSP 5-8.5) ####
solutions <- c("s212", "s213", "s214", "s215", "s216")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(o2os_CanESM5_SSP585, `o2os_CMCC-ESM2_SSP585`, `o2os_GFDL-ESM4_SSP585`, `o2os_IPSL-CM6A-LR_SSP585`, `o2os_NorESM2-MM_SSP585`)
metric = "o2os"
scenario = "585"

makeIterations_Feature(solutions, models, climateLayers, metric, scenario)

#### Climate velocity (SSP 1-2.6) ####
solutions <- c("s157", "s158", "s159", "s160", "s161")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(velocity_CanESM5_SSP126, `velocity_CMCC-ESM2_SSP126`, `velocity_GFDL-ESM4_SSP126`, `velocity_IPSL-CM6A-LR_SSP126`, `velocity_NorESM2-MM_SSP126`)
metric = "velocity"
scenario = "126"

makeIterations_Feature(solutions, models, climateLayers, metric, scenario)

#### Climate velocity (SSP 2-4.5) ####
solutions <- c("s177", "s178", "s179", "s180", "s181")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(velocity_CanESM5_SSP245, `velocity_CMCC-ESM2_SSP245`, `velocity_GFDL-ESM4_SSP245`, `velocity_IPSL-CM6A-LR_SSP245`, `velocity_NorESM2-MM_SSP245`)
metric = "velocity"
scenario = "245"

makeIterations_Feature(solutions, models, climateLayers, metric, scenario)

#### Climate velocity (SSP 5-8.5) ####
solutions <- c("s217", "s218", "s219", "s220", "s221")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(velocity_CanESM5_SSP585, `velocity_CMCC-ESM2_SSP585`, `velocity_GFDL-ESM4_SSP585`, `velocity_IPSL-CM6A-LR_SSP585`, `velocity_NorESM2-MM_SSP585`)
metric = "velocity"
scenario = "585"

makeIterations_Feature(solutions, models, climateLayers, metric, scenario)
#### Penalty approach runs ####
# TODO: Change this to a more inclusive function later on.
makeIterations_Penalty <- function(solutions, # name of solutions
                                   models, # name of models
                                   climateLayers, # list of climate layers
                                   metric,
                                   scenario) {
  
  for(i in 1:length(solutions)) {
    # Prepare climate layer
    # Get scaling
    scalingPenalty <- create_Scaling(UniformCost$cost, climateLayers[[i]]$transformed, metric)
    
    # Get list of features
    features <- aqua_sf %>% 
      as_tibble() %>% 
      dplyr::select(-geometry) %>% 
      names()
    
    # Set up the spatial planning problem
    out_sf <- cbind(aqua_sf, climateLayers[[i]], UniformCost)
    scaling <- scalingPenalty %>% filter(scaling == 30) %>% pull() # get scaling for 30%
    p <- prioritizr::problem(out_sf, features, "cost") %>%
      add_min_set_objective() %>%
      add_relative_targets(0.3) %>%
      add_binary_decisions() %>%
      add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
      add_linear_penalties(scaling, data = "transformed")
    
    # Solve the planning problem 
    s <- prioritizr::solve(p)
    saveRDS(s, paste0(output_solutions, solutions[i], "-MM-", models[i], "-Penalty-", metric, "-", scenario, ".rds")) # save solution
    print(paste0("Saved solution: ", models[i]))
    
    # Plot the spatial design
    s_plot <- s %>% 
      mutate(solution_1 = as.logical(solution_1)) 
    (ggSol <- fSpatPlan_PlotSolution(s_plot, PUs, land) + ggtitle("Climate-smart design", subtitle = paste0("Penalty, SSP ", scenario)) + theme(axis.text = element_text(size = 25)))
    ggsave(filename = paste0("MM-", models[i], "-Penalty-", metric, "-", scenario, ".png"),
           plot = ggSol, width = 21, height = 29.7, dpi = 300,
           path = "Figures/") # save
    print(paste0("Saved figure: ", models[i]))
  }
}

#### Climate warming (SSP 1-2.6) ####
solutions <- c("s102", "s103", "s104", "s105", "s106")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(tos_CanESM5_SSP126, `tos_CMCC-ESM2_SSP126`, `tos_GFDL-ESM4_SSP126`, `tos_IPSL-CM6A-LR_SSP126`, `tos_NorESM2-MM_SSP126`)
metric = "tos"
scenario = "126"

makeIterations_Penalty(solutions, models, climateLayers, metric, scenario)
#### Climate warming (SSP 2-4.5) ####
solutions <- c("s122", "s123", "s124", "s125", "s126")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(tos_CanESM5_SSP245, `tos_CMCC-ESM2_SSP245`, `tos_GFDL-ESM4_SSP245`, `tos_IPSL-CM6A-LR_SSP245`, `tos_NorESM2-MM_SSP245`)
metric = "tos"
scenario = "245"

makeIterations_Penalty(solutions, models, climateLayers, metric, scenario)
#### Climate warming (SSP 5-8.5) ####
solutions <- c("s182", "s183", "s184", "s185", "s186")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(tos_CanESM5_SSP585, `tos_CMCC-ESM2_SSP585`, `tos_GFDL-ESM4_SSP585`, `tos_IPSL-CM6A-LR_SSP585`, `tos_NorESM2-MM_SSP585`)
metric = "tos"
scenario = "585"

makeIterations_Penalty(solutions, models, climateLayers, metric, scenario)
#### Ocean acidification (SSP 1-2.6) ####
solutions <- c("s107", "s108", "s109", "s110", "s111")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(phos_CanESM5_SSP126, `phos_CMCC-ESM2_SSP126`, `phos_GFDL-ESM4_SSP126`, `phos_IPSL-CM6A-LR_SSP126`, `phos_NorESM2-MM_SSP126`)
metric = "phos"
scenario = "126"

makeIterations_Penalty(solutions, models, climateLayers, metric, scenario)
#### Ocean acidification (SSP 2-4.5) ####
solutions <- c("s127", "s128", "s129", "s130", "s131")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(phos_CanESM5_SSP245, `phos_CMCC-ESM2_SSP245`, `phos_GFDL-ESM4_SSP245`, `phos_IPSL-CM6A-LR_SSP245`, `phos_NorESM2-MM_SSP245`)
metric = "phos"
scenario = "245"

makeIterations_Penalty(solutions, models, climateLayers, metric, scenario)
#### Ocean acidification (SSP 5-8.5) ####
solutions <- c("s187", "s188", "s189", "s190", "s191")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(phos_CanESM5_SSP585, `phos_CMCC-ESM2_SSP585`, `phos_GFDL-ESM4_SSP585`, `phos_IPSL-CM6A-LR_SSP585`, `phos_NorESM2-MM_SSP585`)
metric = "phos"

makeIterations_Penalty(solutions, models, climateLayers, metric, scenario)
#### Declining oxygen concentration(SSP 1-2.6) ####
solutions <- c("s112", "s113", "s114", "s115", "s116")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(o2os_CanESM5_SSP126, `o2os_CMCC-ESM2_SSP126`, `o2os_GFDL-ESM4_SSP126`, `o2os_IPSL-CM6A-LR_SSP126`, `o2os_NorESM2-MM_SSP126`)
metric = "o2os"
scenario = "126"

makeIterations_Penalty(solutions, models, climateLayers, metric, scenario)

#### Declining oxygen concentration (SSP 2-4.5) ####
solutions <- c("s132", "s133", "s134", "s135", "s136")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(o2os_CanESM5_SSP245, `o2os_CMCC-ESM2_SSP245`, `o2os_GFDL-ESM4_SSP245`, `o2os_IPSL-CM6A-LR_SSP245`, `o2os_NorESM2-MM_SSP245`)
metric = "o2os"
scenario = "245"

makeIterations_Penalty(solutions, models, climateLayers, metric, scenario)

#### Declining oxygen concentration (SSP 5-8.5) ####
solutions <- c("s192", "s193", "s194", "s195", "s196")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(o2os_CanESM5_SSP585, `o2os_CMCC-ESM2_SSP585`, `o2os_GFDL-ESM4_SSP585`, `o2os_IPSL-CM6A-LR_SSP585`, `o2os_NorESM2-MM_SSP585`)
metric = "o2os"
scenario = "585"

makeIterations_Penalty(solutions, models, climateLayers, metric, scenario)

#### Climate velocity (SSP 1-2.6) ####
solutions <- c("s117", "s118", "s119", "s120", "s121")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(velocity_CanESM5_SSP126, `velocity_CMCC-ESM2_SSP126`, `velocity_GFDL-ESM4_SSP126`, `velocity_IPSL-CM6A-LR_SSP126`, `velocity_NorESM2-MM_SSP126`)
metric = "velocity"
scenario = "126"

makeIterations_Penalty(solutions, models, climateLayers, metric, scenario)

#### Climate velocity (SSP 2-4.5) ####
solutions <- c("s137", "s138", "s139", "s140", "s141")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(velocity_CanESM5_SSP245, `velocity_CMCC-ESM2_SSP245`, `velocity_GFDL-ESM4_SSP245`, `velocity_IPSL-CM6A-LR_SSP245`, `velocity_NorESM2-MM_SSP245`)
metric = "velocity"
scenario = "245"

makeIterations_Penalty(solutions, models, climateLayers, metric, scenario)

#### Climate velocity (SSP 5-8.5) ####
solutions <- c("s197", "s198", "s199", "s200", "s201")
models <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
climateLayers <- list(velocity_CanESM5_SSP585, `velocity_CMCC-ESM2_SSP585`, `velocity_GFDL-ESM4_SSP585`, `velocity_IPSL-CM6A-LR_SSP585`, `velocity_NorESM2-MM_SSP585`)
metric = "velocity"
scenario = "585"

makeIterations_Penalty(solutions, models, climateLayers, metric, scenario)
