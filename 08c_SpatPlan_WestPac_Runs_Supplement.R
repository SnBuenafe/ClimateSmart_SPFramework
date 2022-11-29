# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

#### Preliminaries ####
# "Metric + Approach themes: Supplementary"
# Explores different approaches of incorporating climate metrics into spatial prioritization
# 8c: Climate-priority-area approach

# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R") # climate layers are loaded in the script
# Load climate metrics for different metrics (all SSP5-8.5)
metric_list <- c("tos", "phos", "o2os", "velocity", "MHW", "CombinedMetric")
for(metric_num in 1:length(metric_list)) {
  x <- load_metrics(metric = metric_list[metric_num], model = "ensemble", scenario = "SSP 5-8.5")
  assign(paste0(metric_list[metric_num], "_SSP585"), x)
}
CombinedMetric_SSP585 %<>% dplyr::rename(transformed = combined) # rename column name
total_area = nrow(PUs)

###############################################
###### CLIMATE PRIORITY AREA APPROACH #########
###############################################
# ----- A. Climate Warming -----
# 1. Prepare the climate layers and features
aqua_CPA <- fClimatePriorityArea_CSapproach(featuresDF = aqua_sf,
                                            percentile = 5, # Considering the top 5 percentile of each feature as climate-smart areas
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
  dplyr::mutate(target = 0.3) # this approach needs proportions as targets
targets <- fAssignTargets_CPA(climateSmartDF = aqua_CPA,
                              targetsDF = target_df,
                              refugiaTarget = 1 # 100% protection to the most climate-smart areas
)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_CPA %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                tos_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p34 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 5. Solve the planning problem 
s34 <- solve_SPproblem(p34)
saveRDS(s34, paste0(solutions_dir, "s34-EM-ClimatePriorityArea-tos-585.rds")) # save solution

# 6. Plot the spatial design
s34_plot <- s34 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol34 <- fSpatPlan_PlotSolution(s34_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Climate Priority Area, SSP 5-8.5")
ggsave(filename = "EM-ClimatePriorityArea-tos-585.png",
       plot = ggSol34, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save
rm(list = ls(pattern = "^tos"))

# ----- Ocean acidification -----
LoadClimateMetrics(metric = "phos", model = NA, scenario = "SSP 5-8.5")
# 1. Prepare the climate layers and features
ImptFeat <- create_ImportantFeatureLayer(aqua_sf, metric_name = "phos", colname = "transformed", metric_df = roc_phos_SSP585)
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
out_sf <- cbind(Features, roc_phos_SSP585, UniformCost)
p35 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 5. Solve the planning problem 
s35 <- prioritizr::solve(p35)
saveRDS(s35, paste0(output_solutions, "s35-EM-ClimatePriorityArea-phos-585.rds")) # save solution
# 6. Plot the spatial design
s35_plot <- s35 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol35 <- fSpatPlan_PlotSolution(s35_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Climate Priority Area, SSP 5-8.5")
ggsave(filename = "EM-ClimatePriorityArea-phos-585.png",
       plot = ggSol35, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot
rm(list = ls(pattern = "^roc_phos"))
# ----- Declining Oxygen Concentration -----
LoadClimateMetrics(metric = "o2os", model = NA, scenario = "SSP 5-8.5")
# 1. Prepare the climate layers and features
ImptFeat <- create_ImportantFeatureLayer(aqua_sf, metric_name = "o2os", colname = "transformed", metric_df = roc_o2os_SSP585)
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
out_sf <- cbind(Features, roc_o2os_SSP585, UniformCost)
p36 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 5. Solve the planning problem 
s36 <- prioritizr::solve(p36)
saveRDS(s36, paste0(output_solutions, "s36-EM-ClimatePriorityArea-o2os-585.rds")) # save solution
# 6. Plot the spatial design
s36_plot <- s36 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol36 <- fSpatPlan_PlotSolution(s36_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Climate Priority Area, SSP 5-8.5")
ggsave(filename = "EM-ClimatePriorityArea-o2os-585.png",
       plot = ggSol36, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
rm(list = ls(pattern = "^roc_o2os"))
# ----- Climate Velocity -----
LoadClimateMetrics(metric = "velocity", model = NA, scenario = "SSP 5-8.5")
# 1. Prepare the climate layers and features
ImptFeat <- create_ImportantFeatureLayer(aqua_sf, metric_name = "velocity", colname = "transformed", metric_df = velocity_SSP585)
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
out_sf <- cbind(Features, velocity_SSP585, UniformCost)
p37 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 5. Solve the planning problem 
s37 <- prioritizr::solve(p37)
saveRDS(s37, paste0(output_solutions, "s37-EM-ClimatePriorityArea-velocity-585.rds")) # save solution
# 6. Plot the spatial design
s37_plot <- s37 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol37 <- fSpatPlan_PlotSolution(s37_plot, PUs, land) + 
  ggtitle("Climate-smart design: Climate Velocity", subtitle = "Important Feature, SSP 5-8.5")
ggsave(filename = "EM-ClimatePriorityArea-velocity-585.png",
       plot = ggSol37, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot
rm(list = ls(pattern = "^velocity_"))
# ----- Sum of the cumulative MHW intensity -----
LoadClimateMetrics(metric = "MHW_SumCumInt", model = NA, scenario = "SSP 5-8.5")
# 1. Prepare the climate layers and features
ImptFeat <- create_ImportantFeatureLayer(aqua_sf, metric_name = "MHW_SumCumInt", colname = "transformed", metric_df = MHW_SumCumInt_SSP585)
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
out_sf <- cbind(Features, MHW_SumCumInt_SSP585, UniformCost)
p293 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 5. Solve the planning problem 
s293 <- prioritizr::solve(p293)
saveRDS(s293, paste0(output_solutions, "s293-EM-ClimatePriorityArea-MHW_SumCumInt-585.rds")) # save solution
# 6. Plot the spatial design
s293_plot <- s293 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol293 <- fSpatPlan_PlotSolution(s293_plot, PUs, land) + 
  ggtitle("Climate-smart design: Sum of Cumulative MHW Intensity", subtitle = "Climate Priority Area, SSP 5-8.5")
ggsave(filename = "EM-ClimatePriorityArea-MHW_SumCumInt-585.png",
       plot = ggSol293, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot


#### Summary ####
solution_list <- list(s34, s35, s36, s37, s293)
climateLayer_list <- list(roc_tos_SSP585, roc_phos_SSP585, roc_o2os_SSP585, velocity_SSP585, MHW_SumCumInt_SSP585)
metric_list <- c("tos", "phos", "o2os", "velocity", "MHW_SumCumInt")
# ----- Feature representation -----
names <- c("EM_ClimatePriorityArea_tos_585", "EM_ClimatePriorityArea_phos_585", "EM_ClimatePriorityArea_o2os_585", "EM_ClimatePriorityArea_velocity_585", "EM_ClimatePriorityArea_MHW_SumCumInt_585")
feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- represent_feature(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- left_join(df, feat_rep, by = "feature") %>% drop_na()
}
write.csv(feat_rep, paste0(output_summary, "MetricTheme_ClimatePriorityArea_FeatureRepresentation.csv")) # save

# ----- Kernel distribution plots of targets -----
x <- feat_rep %>% 
  pivot_longer(!feature, names_to = "metric", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = metric, group = metric, fill = metric),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_ClimatePriorityArea_tos_585` = "#289E3D",
                               `EM_ClimatePriorityArea_phos_585` = "#E6C173",
                               `EM_ClimatePriorityArea_o2os_585` = "#81B0CC",
                               `EM_ClimatePriorityArea_velocity_585` = "#855600",
                               `EM_ClimatePriorityArea_MHW_SumCumInt_585` = "#3C6342")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  xlim(c(30, NA)) +
  theme_classic()
ggsave(filename = "TargetDist-MetricApproachThemes-climatepriorityarea.png",
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

write.csv(summary, paste0(output_summary, "MetricTheme_ClimatePriorityArea_Summary.csv")) # save

ggArea <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "metric") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-MetricApproachThemes-ClimatePriorityArea-585.png",
       plot = ggArea, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# ----- Get Kappa Correlation Matrix -----
object_list <- list() # empty list
for (i in 1:length(metric_list)) {
  obj <- select_solution(solution_list[[i]], metric_list[i])
  object_list[[i]] <- obj
}

# Save corrplot
file_path_test = "Figures/MetricTheme_ClimatePriorityArea_CorrelationMatrix.png"
png(height=1200, width=1200, res = 200, file=file_path_test, type = "cairo")

matrix <- create_corrmatrix(object_list) %>% 
  plot_corrplot(., length(object_list))

# Then
dev.off()

# ----- Create selection frequency plot -----
sFreq <- create_LowRegretSf(solution_list, names, PUs)
saveRDS(sFreq, paste0(output_lowregret, "sFreq7-EM-Penalty-585.rds")) # save low-regret solution

ggFreq <- plot_SelectionFrequency(sFreq, land) + 
  ggtitle("Metric Theme", subtitle = "Climate priority area (SSP 5-8.5)") +
  inset_element(plot_inset(sFreq), 0.7, 0.7, 0.99, 0.99)

ggsave(filename = "Freq-ClimatePriorityArea-Ensemble-tos-585.png",
       plot = ggFreq, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Approach Theme: Measuring climate-smart performance & Targets ####
names <- c("Feature", "Percentile", "Penalty", "Climate Priority Area")
group_name = "approach"

# Load summaries
load_summary <- function(metric, column) {
  feature <- read_csv(paste0(output_summary, "MetricTheme_Feature_Summary.csv")) %>% dplyr::filter(grepl(metric, run)) %>% dplyr::select(!!sym(column), run)
  percentile <- read_csv(paste0(output_summary, "MetricTheme_Percentile_Summary.csv")) %>% dplyr::filter(grepl(metric, run)) %>% dplyr::select(!!sym(column), run)
  penalty <- read_csv(paste0(output_summary, "MetricTheme_Penalty_Summary.csv")) %>% dplyr::filter(grepl(metric, run)) %>% dplyr::select(!!sym(column), run)
  climatePriorityArea <- read_csv(paste0(output_summary, "MetricTheme_ClimatePriorityArea_Summary.csv")) %>% dplyr::filter(grepl(metric, run)) %>% dplyr::select(!!sym(column), run)
  climate <- bind_rows(feature, percentile, penalty, climatePriorityArea)
}

# Load feat_rep
load_featrep <- function(metric) {
  feature <- read_csv(paste0(output_summary, "MetricTheme_Feature_FeatureRepresentation.csv")) %>% dplyr::select(feature, contains(metric))
  percentile <- read_csv(paste0(output_summary, "MetricTheme_Percentile_FeatureRepresentation.csv")) %>% dplyr::select(feature, contains(metric))
  penalty <- read_csv(paste0(output_summary, "MetricTheme_Penalty_FeatureRepresentation.csv")) %>% dplyr::select(feature, contains(metric))
  climatePriorityArea <- read_csv(paste0(output_summary, "MetricTheme_ClimatePriorityArea_FeatureRepresentation.csv")) %>% dplyr::select(feature, contains(metric))
  feat_rep <- left_join(feature, percentile) %>% 
    left_join(., penalty) %>% 
    left_join(., climatePriorityArea)
}
# ----- Ocean acidification -----
# Measuring climate-smart performance
names <- c("Feature", "Percentile", "Penalty", "Climate Priority Area")
rev <- c("Penalty", "Climate Priority Area", "Percentile", "Feature")
solution_list <- list(s7, s3, s11, s35)

list <- list() # empty list
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list) %>% 
  dplyr::mutate(approach = fct_relevel(approach, rev))

climate <- load_summary("phos", "mean_ocean_acidification")

ggRidge <- ggplot() +
  geom_density_ridges_gradient(data = df %>% dplyr::filter(solution_1 == 1), aes(x = transformed, y = approach, group = approach, fill = ..x..), scale = 1) +
  scale_fill_viridis_c(name = expression('Δ pH yr'^"-1"*''), option = "A") +
  geom_density_ridges(data = df %>% dplyr::filter(solution_1 == 0), aes(x = transformed, y = approach), alpha = 0.25, linetype = "dotted", scale = 1) +
  geom_vline(xintercept = climate$mean_ocean_acidification,
             linetype = "dashed", color = "tan1", size = 0.5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
  labs(x = expression('Ocean acidification (Δ pH yr'^"-1"*')')) +
  theme_classic() +
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.line = element_line(colour = "black", size = 1),
        axis.text = element_text(color = "black", size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.key.height = unit(1, "inch"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"))

ggsave(filename = "OceanAcidificationDist-ApproachTheme-phos.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# Targets
rev1 <- c("EM_Penalty_phos_585", "EM_ClimatePriorityArea_phos_585", "EM_Percentile_phos_585", "EM_Feature_phos_585")
x <- load_featrep("phos") %>% 
  dplyr::select(feature, contains("phos")) %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature)) %>% 
  dplyr::mutate(approach = fct_relevel(approach, rev1))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = approach, group = approach, fill = approach),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_ClimatePriorityArea_phos_585` = "#E6BA7E",
                               `EM_Feature_phos_585` = "#4D3B2A",
                               `EM_Penalty_phos_585` = "#6984BF",
                               `EM_Percentile_phos_585` = "#2B8142")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
  labs(x = "Protection (%)", y = "selection") +
  theme_classic() +
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.line = element_line(colour = "black", size = 1),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_blank())

ggsave(filename = "TargetDist-ApproachTheme-phos.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot
# ----- Ocean deoxygenation -----
solution_list <- list(s8, s4, s12, s36)

list <- list() # empty list
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list) %>% 
  dplyr::mutate(approach = fct_relevel(approach, rev))

climate <- load_summary("o2os", "mean_oxygen_decline")

ggRidge <- ggplot() +
  geom_density_ridges_gradient(data = df %>% dplyr::filter(solution_1 == 1), aes(x = transformed, y = approach, fill = ..x..), scale = 1) +
  scale_fill_viridis_c(name = expression('Δ mol m'^"-3"*' yr'^"-1"*''), option = "D") +
  geom_density_ridges(data = df %>% dplyr::filter(solution_1 == 0), aes(x = transformed, y = approach), alpha = 0.25, linetype = "dotted", scale = 1) +
  geom_vline(xintercept = climate$mean_oxygen_decline,
             linetype = "dashed", color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
  labs(x = expression('Ocean deoxygenation (Δ mol m'^"-3"*' yr'^"-1"*')')) +
  theme_classic() +
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.line = element_line(colour = "black", size = 1),
        axis.text = element_text(color = "black", size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.key.height = unit(1, "inch"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"))

ggsave(filename = "OxygenDeclineDist-ApproachTheme-o2os.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# Targets
rev1 <- c("EM_Penalty_o2os_585", "EM_ClimatePriorityArea_o2os_585", "EM_Percentile_o2os_585", "EM_Feature_o2os_585")
x <- load_featrep("o2os") %>% 
  dplyr::select(feature, contains("o2os")) %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature)) %>% 
  dplyr::mutate(approach = fct_relevel(approach, rev1))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = approach, group = approach, fill = approach),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_ClimatePriorityArea_o2os_585` = "#E6BA7E",
                               `EM_Feature_o2os_585` = "#4D3B2A",
                               `EM_Penalty_o2os_585` = "#6984BF",
                               `EM_Percentile_o2os_585` = "#2B8142")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
  labs(x = "Protection (%)", y = "selection") +
  theme_classic() +
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.line = element_line(colour = "black", size = 1),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_blank())
ggsave(filename = "TargetDist-ApproachTheme-o2os.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# ----- Climate velocity -----
solution_list <- list(s9, s5, s13, s37)

list <- list() # empty list
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list) %>% 
  dplyr::mutate(approach = fct_relevel(approach, rev))

climate <- load_summary("velocity", "median_velocity")

ggRidge <- ggplot() +
  geom_density_ridges_gradient(data = df %>% dplyr::filter(solution_1 == 1), aes(x = transformed, y = approach, fill = ..x..), scale = 1) +
  scale_fill_distiller(name = expression('km yr'^"-1"*''), palette = "RdYlBu") +
  geom_density_ridges(data = df %>% dplyr::filter(solution_1 == 0), aes(x = transformed, y = approach), alpha = 0.25, linetype = "dotted", scale = 1) +
  geom_vline(xintercept = climate$median_velocity,
             linetype = "dashed", color = "khaki3", size = 0.5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
  labs(x = expression('Climate velocity (km yr'^"-1"*')')) +
  theme_classic() +
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.line = element_line(colour = "black", size = 1),
        axis.text = element_text(color = "black", size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.key.height = unit(1, "inch"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"))

ggsave(filename = "ClimateVelocityDist-ApproachTheme-velocity.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# Targets
rev1 <- c("EM_Penalty_velocity_585", "EM_ClimatePriorityArea_velocity_585", "EM_Percentile_velocity_585", "EM_Feature_velocity_585")
x <- load_featrep("velocity") %>% 
  dplyr::select(feature, contains("velocity")) %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature)) %>% 
  dplyr::mutate(approach = fct_relevel(approach, rev1))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = approach, group = approach, fill = approach),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_ClimatePriorityArea_velocity_585` = "#E6BA7E",
                               `EM_Feature_velocity_585` = "#4D3B2A",
                               `EM_Penalty_velocity_585` = "#6984BF",
                               `EM_Percentile_velocity_585` = "#2B8142")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
  labs(x = "Protection (%)", y = "selection") +
  theme_classic() +
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.line = element_line(colour = "black", size = 1),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_blank())

ggsave(filename = "TargetDist-ApproachTheme-velocity.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot
# ----- MHW intensity -----
solution_list <- list(s291, s290, s292, s293)

list <- list() # empty list
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list) %>% 
  dplyr::mutate(approach = fct_relevel(approach, rev))

climate <- load_summary("MHW", "mean_sum_cumulative_intensity")

ggRidge <- ggplot() +
  geom_density_ridges_gradient(data = df %>% dplyr::filter(solution_1 == 1), aes(x = transformed, y = approach, fill = ..x..), scale = 1) +
  scale_fill_viridis_c(name = expression('total degree days'), option = "G") +
  geom_density_ridges(data = df %>% dplyr::filter(solution_1 == 0), aes(x = transformed, y = approach), alpha = 0.25, linetype = "dotted", scale = 1) +
  geom_vline(xintercept = climate$mean_sum_cumulative_intensity,
             linetype = "dashed", color = "khaki3", size = 0.5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
  labs(x = expression('MHW Intensity (total degree days)')) +
  theme_classic() +
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.line = element_line(colour = "black", size = 1),
        axis.text = element_text(color = "black", size = 20),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.key.height = unit(1, "inch"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"))
ggsave(filename = "MHWSumCumIntDist-ApproachTheme-MHW_SumCumInt.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# Targets
rev1 <- c("EM_Penalty_MHW_SumCumInt_585", "EM_ClimatePriorityArea_MHW_SumCumInt_585", "EM_Percentile_MHW_SumCumInt_585", "EM_Feature_MHW_SumCumInt_585")
x <- load_featrep("MHW_SumCumInt") %>% 
  dplyr::select(feature, contains("MHW_SumCumInt")) %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature)) %>% 
  dplyr::mutate(approach = fct_relevel(approach, rev1))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = approach, group = approach, fill = approach),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_ClimatePriorityArea_MHW_SumCumInt_585` = "#E6BA7E",
                               `EM_Feature_MHW_SumCumInt_585` = "#4D3B2A",
                               `EM_Penalty_MHW_SumCumInt_585` = "#6984BF",
                               `EM_Percentile_MHW_SumCumInt_585` = "#2B8142")) +
  geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
  labs(x = "Protection (%)", y = "selection") +
  theme_classic() +
  theme(axis.ticks = element_line(color = "black", size = 1),
        axis.line = element_line(colour = "black", size = 1),
        axis.text.x = element_text(color = "black", size = 20),
        axis.text.y = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_blank())
ggsave(filename = "TargetDist-ApproachTheme-MHW_SumCumInt.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot
