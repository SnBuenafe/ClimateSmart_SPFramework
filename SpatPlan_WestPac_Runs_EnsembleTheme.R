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

#### Climate-uninformed design ####
# 1. Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 2. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, UniformCost)
p1 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% # using 30% as the target percentage of protection
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 3. Solve the planning problem 
s1 <- prioritizr::solve(p1)
saveRDS(s1, paste0(output_solutions, "s1-uninformed.rds")) # save solution
# 4. Plot the spatial design
s1_plot <- s1 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol1 <- fSpatPlan_PlotSolution(s1_plot, PUs, land) + ggtitle("Climate-uninformed") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "Climate_Uninformed.png",
       plot = ggSol1, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot
# 5. Check summary statistics
# Feature Representation
feat_rep <- represent_feature(p1, s1, "uninformed")
head(feat_rep)
write.csv(feat_rep, paste0(output_summary, "Uninformed_FeatureRepresentation.csv")) # save
# Summary
summary <- compute_summary(s1, total_area, PU_size, "uninformed", Cost = "cost")
print(summary)
write.csv(summary, paste0(output_summary, "Uninformed_Summary.csv")) # save

#### "Ensemble" theme ####
#### "Ensemble mean" approach ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metric: Rate of Climate Warming (SSP 5-8.5)
# Approach: "Percentile"
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
# 6. Check summary statistics
# Feature Representation
feat_rep <- represent_feature(p2, s2, "EM_Percentile_tos_585")
head(feat_rep)

# Summary
summary <- compute_summary(s2, total_area, PU_size, "EM_Percentile_tos_585", Cost = "cost")
print(summary)
# Looking for mean rate of climate warming
climate <- get_ClimateSummary(list(s2), list(roc_tos_SSP585), "tos", "585", "percentile", "EM_Percentile_tos_585")

summary %<>% left_join(., climate, by = c("run"))

#### "Multi-model ensemble" approach ####
# Parameters:
# Ensemble: Multi-model ensemble
# Climate metric: Rate of Climate Warming (SSP 5-8.5)
# Approach: "Percentile"

# A. CanESM5
# 1. Prepare climate layer
# Intersect this with climate layer, select only those <= 35th percentile. 
ensemble <- list(`tos_CanESM5`, `tos_CMCC-ESM2`, `tos_GFDL-ESM4`, `tos_IPSL-CM6A-LR`, `tos_NorESM2-MM`)
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = ensemble[[1]], PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, tos_CanESM5, UniformCost)
p14 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>% # Target should be target percentage divided by the percentile
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s14 <- prioritizr::solve(p14)
saveRDS(s14, paste0(output_solutions, "s14-MM-CanESM5-Percentile-tos-585.rds")) # save solution
# 5. Plot the spatial design
s14_plot <- s14 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol14 <- fSpatPlan_PlotSolution(s14_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5 (GCM: CanESM5)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-CanESM5-Percentile-tos-585.png",
       plot = ggSol14, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# B. CMCC-ESM2
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = ensemble[[2]], PUs = PUs)
# 2. Get list of features: same list of features as above
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `tos_CMCC-ESM2`, UniformCost)
p15 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s15 <- prioritizr::solve(p15)
saveRDS(s15, paste0(output_solutions, "s15-MM-CMCC_ESM2-Percentile-tos-585.rds")) # save solution
# 5. Plot the spatial design
s15_plot <- s15 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol15 <- fSpatPlan_PlotSolution(s15_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5 (GCM: CMCC-ESM2)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-CMCC_ESM2-Percentile-tos-585.png",
       plot = ggSol15, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# C. GFDL-ESM4
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = ensemble[[3]], PUs = PUs)
# 2. Get list of features: same list of features as above
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `tos_GFDL-ESM4`, UniformCost)
p16 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s16 <- prioritizr::solve(p16)
saveRDS(s16, paste0(output_solutions, "s16-MM-GFDL_ESM4-Percentile-tos-585.rds")) # save solution
# 5. Plot the spatial design
s16_plot <- s16 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol16 <- fSpatPlan_PlotSolution(s16_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5 (GCM: GFDL-ESM4)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-GFDL_ESM4-Percentile-tos-585.png",
       plot = ggSol16, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# D. IPSL-CM6A-LR
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = ensemble[[4]], PUs = PUs)
# 2. Get list of features: same list of features as above
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `tos_IPSL-CM6A-LR`, UniformCost)
p17 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s17 <- prioritizr::solve(p17)
saveRDS(s17, paste0(output_solutions, "s17-MM-IPSL_CM6A_LR-Percentile-tos-585.rds")) # save solution
# 5. Plot the spatial design
s17_plot <- s17 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol17 <- fSpatPlan_PlotSolution(s17_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5 (GCM: IPSL-CM6A-LR)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-IPSL_CM6A_LR-Percentile-tos-585.png",
       plot = ggSol17, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# E. NorESM2-MM
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = ensemble[[5]], PUs = PUs)
# 2. Get list of features: same list of features as above
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `tos_NorESM2-MM`, UniformCost)
p18 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s18 <- prioritizr::solve(p18)
saveRDS(s18, paste0(output_solutions, "s18-MM-NorESM2_MM-Percentile-tos-585.rds")) # save solution
# 5. Plot the spatial design
s18_plot <- s18 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol18 <- fSpatPlan_PlotSolution(s18_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5 (GCM: NorESM2-MM)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-NorESM2_MM-Percentile-tos-585.png",
       plot = ggSol18, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# Get summaries of all solutions
# Feature representation
problem_list <- list(p14, p15, p16, p17, p18)
solution_list <- list(s14, s15, s16, s17, s18)
names <- c("MM-CanESM5_Percentile_tos_585", "MM-CMCC-ESM2_Percentile_tos_585", "MM-GFDL-ESM4_Percentile_tos_585", "MM-IPSL-CM6A-LR_Percentile_tos_585", "MM-NorESM2-MM_Percentile_tos_585")
empty_list <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- represent_feature(problem_list[[i]], solution_list[[i]], names[i])
  empty_list <- left_join(df, empty_list, by = "feature")
}
feat_rep %<>% left_join(., empty_list)
write.csv(feat_rep, paste0(output_summary, "EnsembleTheme_tos_FeatureRepresentation.csv")) # save

# Summary
climateLayer_list <- list(`tos_CanESM5`, `tos_CMCC-ESM2`, `tos_GFDL-ESM4`, `tos_IPSL-CM6A-LR`, `tos_NorESM2-MM`)
df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}
climate <- get_ClimateSummary(solution_list, climateLayer_list, metric = "tos", col_scenario = "585", col_approach = "percentile", col_run = names)

summary <- left_join(climate, df, by = "run") %>% 
  rbind(., summary)

write.csv(summary, paste0(output_summary, "EnsembleTheme_tos_Summary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "ensemble") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-EnsembleTheme-Percentile-tos-585.png",
       plot = ggArea, width = 8, height = 5, dpi = 300,
       path = "Figures/") # save plot

# Create selection frequency plot
col_names <- c("tos_CanESM5", "tos_CMCC-ESM2", "tos_GFDL-ESM4", "tos_IPSL-CM6A-LR", "tos_NorESM2-MM")
s1_MMplot <- create_LowRegretSf(solution_list, col_names, PUs)
saveRDS(s1_MMplot, paste0(output_lowregret, "s1-MM-SelectionFrequency-Percentile-tos-585.rds")) # save solution

(ggSelFreq1 <- plot_SelectionFrequency(s1_MMplot, land) + ggtitle("Selection Frequency: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-SelectionFrequency-tos-585.png",
       plot = ggSelFreq1, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# Kappa
list <- c("EnsembleMean", "CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
object_list <- list() # empty list
solution_list <- list(s2, s14, s15, s16, s17, s18)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}
# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#### Supplementary Information: Multi-Model Ensemble Approach #### 

#### "Ensemble mean" approach: Rate of Ocean Acidification ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metric: Rate of Ocean Acidification (SSP 5-8.5)
# Approach: "Percentile"
# 1. Prepare climate layer
# Retain only planning units of each of the biodiversity features that in intersect with areas of low exposure (>= 65th percentile)
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
(ggSol3 <- fSpatPlan_PlotSolution(s3_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-phos-585.png",
       plot = ggSol3, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Multi-Model Ensemble Approach: Rate of Ocean Acidification ####

# Parameters:
# Ensemble: Multi-model ensemble
# Climate metric: Rate of Ocean Acidification (SSP 5-8.5)
# Approach: "Percentile"

# A. CanESM5
# 1. Prepare climate layer
# Intersect this with climate layer, select only those >= 65th percentile. 
ensemble <- list(`phos_CanESM5`, `phos_CMCC-ESM2`, `phos_GFDL-ESM4`, `phos_IPSL-CM6A-LR`, `phos_NorESM2-MM`)
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "phos", colname = "slpTrends", metric_df = ensemble[[1]], PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, phos_CanESM5, UniformCost)
p19 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s19 <- prioritizr::solve(p19)
saveRDS(s19, paste0(output_solutions, "s19-MM-CanESM5-Percentile-phos-585.rds")) # save solution
#' 5. Plot the spatial design
s19_plot <- s19 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol19 <- fSpatPlan_PlotSolution(s19_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 5-8.5 (GCM: CanESM5)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-CanESM5-Percentile-phos-585.png",
       plot = ggSol19, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# B. CMCC-ESM2
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "phos", colname = "slpTrends", metric_df = ensemble[[2]], PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `phos_CMCC-ESM2`, UniformCost)
p20 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s20 <- prioritizr::solve(p20)
saveRDS(s20, paste0(output_solutions, "s20-MM-CMCC_ESM2-Percentile-phos-585.rds")) # save solution
# 5. Plot the spatial design
s20_plot <- s20 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol20 <- fSpatPlan_PlotSolution(s20_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 5-8.5 (GCM: CMCC-ESM2)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-CMCC_ESM2-Percentile-phos-585.png",
       plot = ggSol20, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# C. GFDL-ESM4
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "phos", colname = "slpTrends", metric_df = ensemble[[3]], PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `phos_GFDL-ESM4`, UniformCost)
p21 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s21 <- prioritizr::solve(p21)
saveRDS(s21, paste0(output_solutions, "s21-MM-GFDL_ESM4-Percentile-phos-585.rds")) # save solution
# 5. Plot the spatial design
s21_plot <- s21 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol21 <- fSpatPlan_PlotSolution(s21_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 5-8.5 (GCM: GFDL-ESM4)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-GFDL_ESM4-Percentile-phos-585.png",
       plot = ggSol21, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# D. IPSL-CM6A-LR
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "phos", colname = "slpTrends", metric_df = ensemble[[4]], PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `phos_IPSL-CM6A-LR`, UniformCost)
p22 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s22 <- prioritizr::solve(p22)
saveRDS(s22, paste0(output_solutions, "s22-MM-IPSL_CM6A_LR-Percentile-phos-585.rds")) # save solution
# 5. Plot the spatial design
s22_plot <- s22 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol22 <- fSpatPlan_PlotSolution(s22_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 5-8.5 (GCM: IPSL-CM6A-LR)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-IPSL_CM6A_LR-Percentile-phos-585.png",
       plot = ggSol22, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# E. NorESM2-MM
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "phos", colname = "slpTrends", metric_df = ensemble[[5]], PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `phos_NorESM2-MM`, UniformCost)
p23 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s23 <- prioritizr::solve(p23)
saveRDS(s23, paste0(output_solutions, "s23-MM-NorESM2_MM-Percentile-phos-585.rds")) # save solution
# 5. Plot the spatial design
s23_plot <- s23 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol23 <- fSpatPlan_PlotSolution(s23_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 5-8.5 (GCM: NorESM2-MM)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-NorESM2_MM-Percentile-phos-585.png",
       plot = ggSol23, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# Get summaries of all solutions
# Feature representation
problem_list <- list(p19, p20, p21, p22, p23)
solution_list <- list(s19, s20, s21, s22, s23)
names <- c("MM-CanESM5_Percentile_phos_585", "MM-CMCC-ESM2_Percentile_phos_585", "MM-GFDL-ESM4_Percentile_phos_585", "MM-IPSL-CM6A-LR_Percentile_phos_585", "MM-NorESM2-MM_Percentile_phos_585")
feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- represent_feature(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- left_join(df, feat_rep, by = "feature")
}
write.csv(feat_rep, paste0(output_summary, "EnsembleTheme_phos_FeatureRepresentation.csv")) # save

# Summary
climateLayer_list <- list(`phos_CanESM5`, `phos_CMCC-ESM2`, `phos_GFDL-ESM4`, `phos_IPSL-CM6A-LR`, `phos_NorESM2-MM`)
df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}
climate <- get_ClimateSummary(solution_list, climateLayer_list, "phos", col_scenario = "585", col_approach = "percentile", col_run = names)

summary <- left_join(climate, df, by = "run")

write.csv(summary, paste0(output_summary, "EnsembleTheme_phos_Summary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "ensemble") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-MM-Percentile-phos-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# Create selection frequency plot
col_names <- c("phos_CanESM5", "phos_CMCC-ESM2", "phos_GFDL-ESM4", "phos_IPSL-CM6A-LR", "phos_NorESM2-MM")
s2_MMplot <- create_LowRegretSf(solution_list, col_names, PUs)
saveRDS(s2_MMplot, paste0(output_lowregret, "s2-MM-SelectionFrequency-Percentile-phos-585.rds")) # save solution

(ggSelFreq2 <- plot_SelectionFrequency(s2_MMplot, land) + ggtitle("Selection Frequency: Rate of Ocean Acidification", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-SelectionFrequency-phos-585.png",
       plot = ggSelFreq2, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# Kappa
list <- c("EnsembleMean", "CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
object_list <- list() # empty list
solution_list <- list(s3, s19, s20, s21, s22, s23)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}
# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#### "Ensemble mean" approach: Rate of Declining Oxygen Concentration ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metric: Rate of Declining Oxygen Concentration (SSP 5-8.5)
# Approach: "Percentile"
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "slpTrends", metric_df = roc_o2os_SSP585, PUs = PUs)
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
#' 5. Plot the spatial design
s4_plot <- s4 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol4 <- fSpatPlan_PlotSolution(s4_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concetration", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-o2os-585.png",
       plot = ggSol4, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Multi-Model Ensemble Approach: Rate of Declining Oxygen Concentration ####

# Parameters:
# Ensemble: Multi-model ensemble
# Climate metric: Rate of Declining Oxygen Concentration (SSP 5-8.5)
# Approach: "Percentile"

# A. CanESM5
# 1. Prepare climate layer
# Intersect this with climate layer, select only those >= 65th percentile). 
ensemble <- list(`o2os_CanESM5`, `o2os_CMCC-ESM2`, `o2os_GFDL-ESM4`, `o2os_IPSL-CM6A-LR`, `o2os_NorESM2-MM`)
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "slpTrends", metric_df = ensemble[[1]], PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, o2os_CanESM5, UniformCost)
p24 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s24 <- prioritizr::solve(p24)
saveRDS(s24, paste0(output_solutions, "s24-MM-CanESM5-Percentile-o2os-585.rds")) # save solution
# 5. Plot the spatial design
s24_plot <- s24 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol24 <- fSpatPlan_PlotSolution(s24_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Percentile, SSP 5-8.5 (GCM: CanESM5)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-CanESM5-Percentile-o2os-585.png",
       plot = ggSol24, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# B. CMCC-ESM2
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "slpTrends", metric_df = ensemble[[2]], PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `o2os_CMCC-ESM2`, UniformCost)
p25 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s25 <- prioritizr::solve(p25)
saveRDS(s25, paste0(output_solutions, "s25-MM-CMCC_ESM2-Percentile-o2os-585.rds")) # save solution
# 5. Plot the spatial design
s25_plot <- s25 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol25 <- fSpatPlan_PlotSolution(s25_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Percentile, SSP 5-8.5 (GCM: CMCC-ESM2)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-CMCC_ESM2-Percentile-o2os-585.png",
       plot = ggSol25, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# C. GFDL-ESM4
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "slpTrends", metric_df = ensemble[[3]], PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `o2os_GFDL-ESM4`, UniformCost)
p26 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s26 <- prioritizr::solve(p26)
saveRDS(s26, paste0(output_solutions, "s26-MM-GFDL_ESM4-Percentile-o2os-585.rds")) # save solution
# 5. Plot the spatial design
s26_plot <- s26 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol26 <- fSpatPlan_PlotSolution(s26_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Percentile, SSP 5-8.5 (GCM: GFDL-ESM4)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-GFDL_ESM4-Percentile-o2os-585.png",
       plot = ggSol26, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# D. IPSL-CM6A-LR
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "slpTrends", metric_df = ensemble[[4]], PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `o2os_IPSL-CM6A-LR`, UniformCost)
p27 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s27 <- prioritizr::solve(p27)
saveRDS(s27, paste0(output_solutions, "s27-MM-IPSL_CM6A_LR-Percentile-o2os-585.rds")) # save solution
# 5. Plot the spatial design
s27_plot <- s27 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol27 <- fSpatPlan_PlotSolution(s27_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Percentile, SSP 5-8.5 (GCM: IPSL-CM6A-LR)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-IPSL_CM6A_LR-Percentile-o2os-585.png",
       plot = ggSol27, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# E. NorESM2-MM
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "slpTrends", metric_df = ensemble[[5]], PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `o2os_NorESM2-MM`, UniformCost)
p28 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s28 <- prioritizr::solve(p28)
saveRDS(s28, paste0(output_solutions, "s28-MM-NorESM2_MM-Percentile-o2os-585.rds")) # save solution
# 5. Plot the spatial design
s28_plot <- s28 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol28 <- fSpatPlan_PlotSolution(s28_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Percentile, SSP 5-8.5 (GCM: NorESM2-MM)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-NorESM2_MM-Percentile-o2os-585.png",
       plot = ggSol28, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# Get summaries of all solutions
# Feature representation
problem_list <- list(p24, p25, p26, p27, p28)
solution_list <- list(s24, s25, s26, s27, s28)
names <- c("MM-CanESM5_Percentile_o2os_585", "MM-CMCC-ESM2_Percentile_o2os_585", "MM-GFDL-ESM4_Percentile_o2os_585", "MM-IPSL-CM6A-LR_Percentile_o2os_585", "MM-NorESM2-MM_Percentile_o2os_585")
feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- represent_feature(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- left_join(df, feat_rep, by = "feature")
}
write.csv(feat_rep, paste0(output_summary, "EnsembleTheme_o2os_FeatureRepresentation.csv")) # save

# Summary
climateLayer_list <- list(`o2os_CanESM5`, `o2os_CMCC-ESM2`, `o2os_GFDL-ESM4`, `o2os_IPSL-CM6A-LR`, `o2os_NorESM2-MM`)
df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}
climate <- get_ClimateSummary(solution_list, climateLayer_list, "o2os", col_scenario = "585", col_approach = "percentile", col_run = names)

summary <- left_join(climate, df, by = "run")

write.csv(summary, paste0(output_summary, "EnsembleTheme_o2os_Summary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "ensemble") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-MM-Percentile-o2os-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# Create selection frequency plot
solution_list <- list(s24, s25, s26, s27, s28)
col_names <- c("o2os_CanESM5", "o2os_CMCC-ESM2", "o2os_GFDL-ESM4", "o2os_IPSL-CM6A-LR", "o2os_NorESM2-MM")
s3_MMplot <- create_LowRegretSf(solution_list, col_names, PUs)
saveRDS(s3_MMplot, paste0(output_lowregret, "s3-MM-SelectionFrequency-Percentile-o2os-585.rds")) # save solution

(ggSelFreq3 <- plot_SelectionFrequency(s3_MMplot, land) + ggtitle("Selection Frequency: Rate of Declining Oxygen Concentration", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-SelectionFrequency-o2os-585.png",
       plot = ggSelFreq3, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# Kappa
list <- c("EnsembleMean", "CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
object_list <- list() # empty list
solution_list <- list(s4, s24, s25, s26, s27, s28)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}
# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))



#### "Ensemble mean" approach: Climate Velocity ####
# Parameters:
# Ensemble: Ensemble mean
# Climate metric: Climate Velocity (SSP 5-8.5)
# Approach: "Percentile"
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "voccMag", metric_df = velocity_SSP585, PUs = PUs)
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
(ggSol5 <- fSpatPlan_PlotSolution(s5_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-velocity-585.png",
       plot = ggSol5, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Multi-Model Ensemble Approach: Climate Velocity ####

# Parameters:
# Ensemble: Multi-model ensemble
# Climate metric: Climate Velocity (SSP 5-8.5)
# Approach: "Percentile"

# A. CanESM5
# 1. Prepare climate layer
ensemble <- list(`velocity_CanESM5`, `velocity_CMCC-ESM2`, `velocity_GFDL-ESM4`, `velocity_IPSL-CM6A-LR`, `velocity_NorESM2-MM`)
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "voccMag", metric_df = ensemble[[1]], PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, velocity_CanESM5, UniformCost)
p29 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s29 <- prioritizr::solve(p29)
saveRDS(s29, paste0(output_solutions, "s29-MM-CanESM5-Percentile-velocity-585.rds")) # save solution
# 5. Plot the spatial design
s29_plot <- s29 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol29 <- fSpatPlan_PlotSolution(s29_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 5-8.5 (GCM: CanESM5)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-CanESM5-Percentile-velocity-585.png",
       plot = ggSol29, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# B. CMCC-ESM2
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "voccMag", metric_df = ensemble[[2]], PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `velocity_CMCC-ESM2`, UniformCost)
p30 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s30 <- prioritizr::solve(p30)
saveRDS(s30, paste0(output_solutions, "s30-MM-CMCC_ESM2-Percentile-velocity-585.rds")) # save solution
# 5. Plot the spatial design
s30_plot <- s30 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol30 <- fSpatPlan_PlotSolution(s30_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 5-8.5 (GCM: CMCC-ESM2)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-CMCC_ESM2-Percentile-velocity-585.png",
       plot = ggSol30, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# C. GFDL-ESM4
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "voccMag", metric_df = ensemble[[3]], PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `velocity_GFDL-ESM4`, UniformCost)
p31 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s31 <- prioritizr::solve(p31)
saveRDS(s31, paste0(output_solutions, "s31-MM-GFDL_ESM4-Percentile-velocity-585.rds")) # save solution
# 5. Plot the spatial design
s31_plot <- s31 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol31 <- fSpatPlan_PlotSolution(s31_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 5-8.5 (GCM: GFDL-ESM4)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-GFDL_ESM4-Percentile-velocity-585.png",
       plot = ggSol31, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# D. IPSL-CM6A-LR
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "voccMag", metric_df = ensemble[[4]], PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `velocity_IPSL-CM6A-LR`, UniformCost)
p32 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s32 <- prioritizr::solve(p32)
saveRDS(s32, paste0(output_solutions, "s32-MM-IPSL_CM6A_LR-Percentile-velocity-585.rds")) # save solutions
#' 5. Plot the spatial design
s32_plot <- s32 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol32 <- fSpatPlan_PlotSolution(s32_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 5-8.5 (GCM: IPSL-CM6A-LR)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-IPSL_CM6A_LR-Percentile-velocity-585.png",
       plot = ggSol32, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# E. NorESM2-MM
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "voccMag", metric_df = ensemble[[5]], PUs = PUs)
# 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, `velocity_NorESM2-MM`, UniformCost)
p33 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s33 <- prioritizr::solve(p33)
saveRDS(s33, paste0(output_solutions, "s33-MM-NorESM2_MM-Percentile-velocity-585.rds")) # save solutions
# 5. Plot the spatial design
s33_plot <- s33 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol33 <- fSpatPlan_PlotSolution(s33_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 5-8.5 (GCM: NorESM2-MM)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-NorESM2_MM-Percentile-velocity-585.png",
       plot = ggSol33, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# Get summaries of all solutions
# Feature representation
problem_list <- list(p29, p30, p31, p32, p33)
solution_list <- list(s29, s30, s31, s32, s33)
names <- c("MM-CanESM5_Percentile_velocity_585", "MM-CMCC-ESM2_Percentile_velocity_585", "MM-GFDL-ESM4_Percentile_velocity_585", "MM-IPSL-CM6A-LR_Percentile_velocity_585", "MM-NorESM2-MM_Percentile_velocity_585")
feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- represent_feature(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- left_join(df, feat_rep, by = "feature")
}
write.csv(feat_rep, paste0(output_summary, "EnsembleTheme_velocity_FeatureRepresentation.csv")) # save

# Summary
climateLayer_list <- list(`velocity_CanESM5`, `velocity_CMCC-ESM2`, `velocity_GFDL-ESM4`, `velocity_IPSL-CM6A-LR`, `velocity_NorESM2-MM`)
df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}
climate <- get_ClimateSummary(solution_list, climateLayer_list, "velocity", col_scenario = "585", col_approach = "percentile", col_run = names)

summary <- left_join(climate, df, by = "run")

write.csv(summary, paste0(output_summary, "EnsembleTheme_velocity_Summary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "ensemble") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-MM-Percentile-velocity-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# Create selection frequency plot
solution_list <- list(s29, s30, s31, s32, s33)
col_names <- c("velocity_CanESM5", "o2os_CMCC-ESM2", "o2os_GFDL-ESM4", "o2os_IPSL-CM6A-LR", "o2os_NorESM2-MM")
s4_MMplot <- create_LowRegretSf(solution_list, col_names, PUs)
saveRDS(s4_MMplot, paste0(output_lowregret, "s4-MM-SelectionFrequency-Percentile-velocity-585.rds")) # save solution

(ggSelFreq4 <- plot_SelectionFrequency(s4_MMplot, land) + ggtitle("Selection Frequency: Climate Velocity", subtitle = "Percentile, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "MM-SelectionFrequency-velocity-585.png",
       plot = ggSelFreq4, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# Kappa
list <- c("EnsembleMean", "CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
object_list <- list() # empty list
solution_list <- list(s5, s29, s30, s31, s32, s33)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}
# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#### TODO: Marine Heatwaves ####