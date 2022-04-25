# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

#### Preliminaries ####
# "Ensemble Theme"
# Explores the use of the ensemble mean approach or the multi-model ensemble approach for calculating the climate metrics
# To limit complexity, we used the following parameters for these runs:
# 1. Single emission scenario (SSP 5-8.5)
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
source("03_SpatPlan_Master_Preliminaries.R") # climate layers are loaded in the script
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

#### "Ensemble mean" approach ####
# ----- Load climate layer -----
LoadClimateMetrics(metric = "tos", model = NA, scenario = "SSP 5-8.5")
# ----- Create spatial plan -----
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
# 6. Check summary statistics
# Feature Representation
dummy_problem <- readRDS("Output/temp/p10.rds")
feat_rep <- represent_feature(dummy_problem, s2, "EM_Percentile_tos_585")
head(feat_rep)

# Summary
summary <- compute_summary(s2, total_area, PU_size, "EM_Percentile_tos_585", Cost = "cost")
print(summary)

# Looking for mean rate of climate warming
climate <- get_ClimateSummary(list(s2), list(roc_tos_SSP585), "tos", "585", "percentile", "EM_Percentile_tos_585")
summary %<>% left_join(., climate, by = c("run"))

#### "Multi-model ensemble" approach ####
# ----- Load climate layers -----
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
for(model_num in 1:length(model_list)) {
  LoadClimateMetrics(metric = "tos", model = model_list[model_num], scenario = "SSP 5-8.5")
}
# ----- Create spatial plans -----
# A. CanESM5
# 1. Prepare climate layer
# Intersect this with climate layer, select only those <= 35th percentile. 
ensemble <- list(`tos_CanESM5_SSP585`, `tos_CMCC-ESM2_SSP585`, `tos_GFDL-ESM4_SSP585`, `tos_IPSL-CM6A-LR_SSP585`, `tos_NorESM2-MM_SSP585`)
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

#### Climate warming: ensemble mean vs multi-model ensemble approaches ####
# Make a "dummy problem" where the features are the original distributions (and not the filtered distributions)
dummy_problem = p1
problem_list <- list(dummy_problem, dummy_problem, dummy_problem, dummy_problem, dummy_problem, dummy_problem)

solution_list <- list(s2, s14, s15, s16, s17, s18)
climateLayer_list <- list(roc_tos_SSP585, `tos_CanESM5_SSP585`, `tos_CMCC-ESM2_SSP585`, `tos_GFDL-ESM4_SSP585`, `tos_IPSL-CM6A-LR_SSP585`, `tos_NorESM2-MM_SSP585`)

# ----- Feature representation ----- 
names <- c("MM-CanESM5_Percentile_tos_585", "MM-CMCC-ESM2_Percentile_tos_585", "MM-GFDL-ESM4_Percentile_tos_585", "MM-IPSL-CM6A-LR_Percentile_tos_585", "MM-NorESM2-MM_Percentile_tos_585")
empty_list <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- represent_feature(problem_list[[i]], solution_list[[i]], names[i])
  empty_list <- left_join(df, empty_list, by = "feature")
}
feat_rep %<>% left_join(., empty_list)
write.csv(feat_rep, paste0(output_summary, "EnsembleTheme_tos_FeatureRepresentation.csv")) # save

# ----- Kernel distribution plots of targets ----- 
x <- feat_rep %>% 
  pivot_longer(!feature, names_to = "ensemble", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = ensemble, group = ensemble, fill = ensemble),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_Percentile_tos_585` = "#FAF7B7",
                               `MM-CanESM5_Percentile_tos_585` = "#E6C173",
                               `MM-CMCC-ESM2_Percentile_tos_585` = "#855600",
                               `MM-GFDL-ESM4_Percentile_tos_585` = "#5075BA",
                               `MM-IPSL-CM6A-LR_Percentile_tos_585` = "#81B0CC",
                               `MM-NorESM2-MM_Percentile_tos_585` = "#5A9E67")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  xlim(c(min(x$percent), NA)) +
  theme_classic()
ggsave(filename = "TargetDist-EnsembleTheme-tos.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

# ----- Summary statistics ----- 
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

# ----- Kappa Correlation Matrix -----
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

# ----- Measuring how climate-smart solutions are using Kernel Density plots -----
# Kernel Density Plots
list <- list() # empty list
names <- c("EnsembleMean", "CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
group_name = "ensemble"
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list)

intercept1 <- (climate %>% dplyr::filter(grepl("NorESM2|GFDL", run)))$mean_climate_warming
intercept2 <- (climate %>% dplyr::filter(grepl("EM|CMCC", run)))$mean_climate_warming
intercept3 <- (climate %>% dplyr::filter(grepl("IPSL|CanESM5", run)))$mean_climate_warming
ggRidge <- ggplot(data = df, aes(x = transformed, y = ensemble, group = ensemble, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_viridis_c(name = expression('Δ'^"o"*'C yr'^"-1"*''), option = "C") +
  geom_vline(xintercept = intercept1,
             linetype = "dashed", color = "tan1", size = 0.5) +
  geom_vline(xintercept = intercept2,
             linetype = "dashed", color = "orchid3", size = 0.5) +
  geom_vline(xintercept = intercept3,
             linetype = "dashed", color = "orchid4", size = 0.5) +
  theme_classic()
ggsave(filename = "ClimateWarmingDist-EnsembleTheme-Percentile-tos.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# ----- Create selection frequency for MM -----
col_names <- c("tos_CanESM5", "tos_CMCC-ESM2", "tos_GFDL-ESM4", "tos_IPSL-CM6A-LR", "tos_NorESM2-MM")
sFreq <- create_LowRegretSf(solution_list, col_names, PUs)
saveRDS(sFreq, paste0(output_lowregret, "sFreq2-EM-Percentile-tos.rds")) # save solution

(ggFreq <- plot_SelectionFrequency(sFreq, land) + ggtitle("Ensemble Theme", subtitle = "Climate Warming, Percentile (SSP 5-8.5)") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "Freq-Percentile-Ensemble-tos-585.png",
       plot = ggFreq, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot