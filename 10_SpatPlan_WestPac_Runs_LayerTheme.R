# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

# Disclaimer: This was not part of the paper, as it proved to be too computationally intensive.

#### Preliminaries ####
# "Layer Theme"
# Explores different ways on how to look at data from different climate layers
# 1. "Multiple solutions": looking at individual solutions and treating them separately (results are in 04_SpatPlan_WestPac_Runs_ScenarioTheme.R and are, therefore, not repeated here)
# 2. "Low-regret": overlapping the individual solutions, categorizing areas that were selected in all climate layers as "low-regret"
# 3. "Single solution": incorporating all the different climate layers as inputs and producing one single solution
# To limit complexity, we used the following parameters for these runs:
# 1. Different climate layers = climate warming forced under SSP1-2.6, SSP2-4.5, and SSP5-8.5
# 2. Percentile approach
# 3. Ensemble mean approach

# Load functions
source("HelperFunctions/SpatPlan_Extras.R") # Load the extras, including functions and libraries
source("HelperFunctions/SpatPlan_HelperFxns_WestPac.R") # Load helper functions written specifically for this spatial planning project

output_solutions <- "Output/solutions/"
output_summary <- "Output/summary/"
output_lowregret <- "Output/lowregret/"

# Load files
source("03_SpatPlan_Master_Preliminaries.R")
total_area = nrow(PUs) * PU_size
scenario_list = c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
for(scenario_num in 1:length(scenario_list)) {
  LoadClimateMetrics(metric = "tos", model = NA, scenario = scenario_list[scenario_num])
}
s38 <- readRDS(paste0(output_solutions, "s38-EM-Percentile-tos-126.rds")) # SSP1-2.6 solution
s39 <- readRDS(paste0(output_solutions, "s39-EM-Percentile-tos-245.rds")) # SSP2-4.5 solution
s2 <- readRDS(paste0(output_solutions, "s2-EM-Percentile-tos-585.rds")) # SSP5-8.5 solution
sLR <- readRDS(paste0(output_lowregret, "sFreq1-EM-Percentile-tos.rds")) # SelectionFrequency solution (has low-regret data)
multiple_FeatRep <- read_csv(paste0(output_summary, "ScenarioTheme_tos_FeatureRepresentation.csv")) %>% dplyr::select(-1)

#### Low-regret solution ####
# ----- Plot low-regret solution -----
ggLR <- plot_lowregret(sLR, land) + 
  ggtitle("Layer Theme: Low-Regret", subtitle = "Climate Warming, Percentile") + theme(axis.text = element_text(size = 25))
ggsave(filename = "LR-EM-Percentile-Scenario-tos.png",
       plot = ggLR, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Single solution ####
# ----- Filter distributions: 35th percentile of each layer -----
# SSP1-2.6
aqua_percentile_SSP126 <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP126, PUs = PUs) %>% 
  dplyr::rename_with(~paste0(., "_SSP126"), -c("geometry"))
# SSP2-4.5
aqua_percentile_SSP245 <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP245, PUs = PUs) %>% 
  dplyr::rename_with(~paste0(., "_SSP245"), -c("geometry"))
# SSP5-8.5
aqua_percentile_SSP585 <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP585, PUs = PUs) %>% 
  dplyr::rename_with(~paste0(., "_SSP585"), -c("geometry"))

# ----- Know which areas are selected at least once for all features -----
# Get spp names
spp <- aqua_sf %>% as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

# Combine all filtered distributions into one df
df_SSP126 <- aqua_percentile_SSP126 %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  dplyr::mutate(cellID = row_number())
df_SSP245 <- aqua_percentile_SSP245 %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  dplyr::mutate(cellID = row_number())
df_SSP585 <- aqua_percentile_SSP585 %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  dplyr::mutate(cellID = row_number())

df_combined <- left_join(df_SSP126, df_SSP245, by = "cellID") %>% 
  left_join(., df_SSP585, by = "cellID")

# Loop through all the species
ncores <- detectCores(logical = FALSE) - 1 
cl <- makeCluster(ncores)
registerDoParallel(cl)

tmp <- foreach(i = 1:length(spp), .packages = c('tidyverse', 'sf', 'magrittr')) %dopar% {
  
  col_list <- c(paste0(spp[i], "_SSP126"), paste0(spp[i], "_SSP245"), paste0(spp[i], "_SSP585"))
  
  p <- df_combined %>% subset(., select = col_list) %>% rowwise() %>% 
    dplyr::mutate(!!sym(spp[i]) := sum(c_across(all_of(col_list)))) %>% 
    dplyr::select(!!sym(spp[i]))
}
stopCluster(cl)
  
tmp_df <- do.call(cbind, tmp) %>% 
  cbind(., PUs) %>% st_as_sf(sf_column_name = "geometry")
#saveRDS(tmp_df, "Output/temp/single.rds") # save the object

# ----- Assign targets for each of the features according to filtered distributions' area -----
tmp_target <- tmp_df %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  summarize(across(everything(), ~length(which(. != 0 )))) %>% 
  pivot_longer(everything(.), names_to = "Species", values_to = "Filtered")
  
tmp_aq <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  summarize(across(everything(), ~sum(., is.na(.), 0))) %>% 
  pivot_longer(everything(.), names_to = "Species", values_to = "Original")
  
targets <- left_join(tmp_target, tmp_aq) %>% 
  dplyr::mutate(targets = 0.3/(Filtered/Original))
  
# ----- Prepare layers for setting the spatial planning problem -----
# Feature names:
features <- tmp_df %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
 
# Translating filtered distribution into binary 
mutated <- tmp_df %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  dplyr::mutate(across(everything(), ~ ifelse(. > 0, 1, 0) )) %>% 
  cbind(., PUs) %>% st_as_sf(sf_column_name = "geometry")
  
out_sf <- cbind(mutated, UniformCost)

# ----- Create spatial planning problem -----
p <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$targets) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

# ----- Solve spatial planning problem -----
sSINGLE <- prioritizr::solve(p)
saveRDS(sSINGLE, paste0(output_solutions, "SINGLE-EM-Percentile-tos.rds"))

# ----- Plot spatial design -----
splot <- sSINGLE %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol <- plot_lowregret(splot, land) + 
  ggtitle("Layer Theme: Single Solution", subtitle = "Climate Warming, Percentile") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Single-EM-Percentile-Scenario-tos.png",
       plot = ggSol, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Comparisons between solutions ####
# ----- Feature representation -----
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

# Feature representation of low-regret solution
lowRegret_FeatRep <- represent_feature(dummy_problem, sLR, "LR")

# Feature representation of single solution
single_FeatRep <- represent_feature(dummy_problem, sSINGLE, "Single")

# Join all feat rep
combined_FeatRep <- left_join(multiple_FeatRep, lowRegret_FeatRep) %>% 
  left_join(single_FeatRep)

# ----- Kernel density plot of targets -----
x <- combined_FeatRep %>% 
  pivot_longer(!feature, names_to = "layer", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = layer, group = layer, fill = layer),
                      scale = 2) +
  scale_fill_manual(values = c(`EM-Percentile-tos-126` = "#289E3D",
                               `EM-Percentile-tos-245` = "#E6C173",
                               `EM-Percentile-tos-585` = "#855600",
                               LR = "#4C90F5",
                               Single = "#A9C2EB")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  theme_classic()
ggsave(filename = "TargetDist-LayerTheme-tos.png",
       plot = ggRidge, width = 15, height = 10, dpi = 300,
       path = "Figures/") # save plot

# ----- Summary Statistics -----
solution_list <- list(s38, s39, s2, sLR, sSINGLE)
names <- c("Multiple_SSP126", "Multiple_SSP245", "Multiple_SSP585", "LowRegret", "Single")
climateLayer_list <- list(roc_tos_SSP126, roc_tos_SSP245, roc_tos_SSP585, roc_tos_SSP585, roc_tos_SSP585)
scenario_list <- c("126", "245", "585", "all-585", "all-585")

df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}
climate <- get_ClimateSummary(solution_list, climateLayer_list, "tos", col_scenario = scenario_list, col_approach = "percentile", col_run = names)

summary <- left_join(climate, df, by = "run")
write.csv(summary, paste0(output_summary, "LayerTheme_tos_Summary_585.csv")) # save

ggArea <- plot_statistics(summary, 
                          col_name = "percent_area", y_axis = "% area", theme = "layer") + theme(axis.text = element_text(size = 25))
ggsave(filename = "LayerTheme-Area-Percentile-tos.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- Get Kappa Correlation Matrix -----
list <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5", "LowRegret", "Single")
object_list <- list() # empty list
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

# Save corrplot
file_path_test = "Figures/LayerTheme_CorrelationMatrix.png"
png(height=1200, width=1200, res = 200, file=file_path_test, type = "cairo")

matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list))

# Then
dev.off()
  

# ----- Measuring how climate-smart the solution are across scenarios using kernel density plots -----
# LowRegret and Single solutions would have different climate warming depending on the EMISSION SCENARIO.
# So we are making 3 different plots.

list <- list() # empty list
solution_list <- list(s38, s39, s2, cbind(sLR, roc_tos_SSP585), cbind(sSINGLE, roc_tos_SSP585))
names <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5", "LowRegret", "Single")
group_name = "layer"
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list)

ggRidge <- ggplot(data = df %>% dplyr::filter(!layer %in% c("SSP 1-2.6", "SSP 2-4.5")), aes(x = transformed, y = layer, group = layer, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_viridis_c(name = expression('Δ'^"o"*'C yr'^"-1"*''), option = "C") +
  geom_vline(xintercept=(climate %>% 
                           dplyr::filter(run == "Multiple_SSP585"))$mean_climate_warming,
             linetype = "dashed", color = "orchid4", size = 0.5) +
  geom_vline(xintercept=(climate %>% 
                           dplyr::filter(run == "LowRegret"))$mean_climate_warming,
             linetype = "dashed", color = "orchid4", size = 0.5) +
  geom_vline(xintercept=(climate %>% 
                           dplyr::filter(run == "Single"))$mean_climate_warming,
             linetype = "dashed", color = "orchid4", size = 0.5) +
  theme_classic()
ggsave(filename = "ClimateWarmingDist-LayerTheme-Percentile-tos-585.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# ----- SSP 1-2.6 -----
solution_list <- list(s38, s39, s2, cbind(sLR, roc_tos_SSP126), cbind(sSINGLE, roc_tos_SSP126))
names <- c("Multiple_SSP126", "Multiple_SSP245", "Multiple_SSP585", "LowRegret", "Single")
climateLayer_list <- list(roc_tos_SSP126, roc_tos_SSP245, roc_tos_SSP585, roc_tos_SSP126, roc_tos_SSP126)
scenario_list <- c("126", "245", "585", "all-126", "all-126")

df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}
climate <- get_ClimateSummary(solution_list, climateLayer_list, "tos", col_scenario = scenario_list, col_approach = "percentile", col_run = names)

summary <- left_join(climate, df, by = "run")
write.csv(summary, paste0(output_summary, "LayerTheme_tos_Summary_126.csv")) # save

list <- list() # empty list
names <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5", "LowRegret", "Single")
group_name = "layer"
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list)

ggRidge <- ggplot(data = df %>% dplyr::filter(!layer %in% c("SSP 5-8.5", "SSP 2-4.5")), aes(x = transformed, y = layer, group = layer, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_viridis_c(name = expression('Δ'^"o"*'C yr'^"-1"*''), option = "C") +
  geom_vline(xintercept=(climate %>% 
                           dplyr::filter(run == "Multiple_SSP126"))$mean_climate_warming,
             linetype = "dashed", color = "tan1", size = 0.5) +
  geom_vline(xintercept=(climate %>% 
                           dplyr::filter(run == "LowRegret"))$mean_climate_warming,
             linetype = "dashed", color = "tan1", size = 0.5) +
  geom_vline(xintercept=(climate %>% 
                           dplyr::filter(run == "Single"))$mean_climate_warming,
             linetype = "dashed", color = "tan1", size = 0.5) +
  theme_classic()
ggsave(filename = "ClimateWarmingDist-LayerTheme-Percentile-tos-126.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot

# ----- SSP 2-4.5 -----
solution_list <- list(s38, s39, s2, cbind(sLR, roc_tos_SSP245), cbind(sSINGLE, roc_tos_SSP245))
names <- c("Multiple_SSP126", "Multiple_SSP245", "Multiple_SSP585", "LowRegret", "Single")
climateLayer_list <- list(roc_tos_SSP126, roc_tos_SSP245, roc_tos_SSP585, roc_tos_SSP245, roc_tos_SSP245)
scenario_list <- c("126", "245", "585", "all-245", "all-245")

df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}
climate <- get_ClimateSummary(solution_list, climateLayer_list, "tos", col_scenario = scenario_list, col_approach = "percentile", col_run = names)

summary <- left_join(climate, df, by = "run")
write.csv(summary, paste0(output_summary, "LayerTheme_tos_Summary_245.csv")) # save

list <- list() # empty list
names <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5", "LowRegret", "Single")
group_name = "layer"
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list)

ggRidge <- ggplot(data = df %>% dplyr::filter(!layer %in% c("SSP 5-8.5", "SSP 1-2.6")), aes(x = transformed, y = layer, group = layer, fill = stat(x))) +
  geom_density_ridges_gradient(scale = 3) +
  scale_fill_viridis_c(name = expression('Δ'^"o"*'C yr'^"-1"*''), option = "C") +
  geom_vline(xintercept=(climate %>% 
                           dplyr::filter(run == "Multiple_SSP245"))$mean_climate_warming,
             linetype = "dashed", color = "orchid3", size = 0.5) +
  geom_vline(xintercept=(climate %>% 
                           dplyr::filter(run == "LowRegret"))$mean_climate_warming,
             linetype = "dashed", color = "orchid3", size = 0.5) +
  geom_vline(xintercept=(climate %>% 
                           dplyr::filter(run == "Single"))$mean_climate_warming,
             linetype = "dashed", color = "orchid3", size = 0.5) +
  theme_classic()
ggsave(filename = "ClimateWarmingDist-LayerTheme-Percentile-tos-245.png",
       plot = ggRidge, width = 10, height = 6, dpi = 300,
       path = "Figures/") # save plot
