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

# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R") # climate layers are loaded in the script
roc_tos_SSP585 <- load_metrics(metric = "tos", model = "ensemble", scenario = "SSP 5-8.5") # Load climate metric for ens mean
total_area = nrow(PUs)

#########################
###### PERCENTILE #######
#########################
# 1. Prepare climate layer
aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
                                          percentile = 35,
                                          metricDF = rename_metric(roc_tos_SSP585),
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
  dplyr::mutate(target = 30)
targets <- fAssignTargets_Percentile(featuresDF = aqua_sf,
                                     climateSmartDF = aqua_percentile,
                                     targetsDF = target_df)

# 3. Set up the spatial planning problem
out_sf <- cbind(UniformCost,
                aqua_percentile %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry), 
                roc_tos_SSP585 %>% 
                  tibble::as_tibble() %>% 
                  dplyr::select(-cellID, -geometry)
)
p2 <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_cbc_solver(gap = 0.1, verbose = FALSE)

# 4. Solve the planning problem 
s2 <- solve_SPproblem(p2)
saveRDS(s2, paste0(solutions_dir, "s2-EM-Percentile-tos-585.rds")) # save solution

# 5. Plot the spatial design
s2_plot <- s2 %>% 
  mutate(solution_1 = as.logical(solution_1))
ggSol2 <- fSpatPlan_PlotSolution(s2_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5")
ggsave(filename = "EM-Percentile-tos-585.png",
       plot = ggSol2, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#########################
###### FEATURE #######
#########################
# 1. Prepare climate layer
aqua_feature <- fFeature_CSapproach(featuresDF = aqua_sf, 
                                    percentile = 35, 
                                    metricDF = rename_metric(roc_tos_SSP585),
                                    direction = -1 # lower values are more climate-smart
                                    )
# 2. Get list of features and set targets
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
features <- append(features, "climate_layer") # add "climate_layer" to features

targets <- features %>% as_tibble() %>% 
  setNames(., "Species") %>% 
  add_column(target = 0.3) %>% 
  mutate(target = ifelse(str_detect(Species, pattern = "climate_layer"), 30/35, 0.3))

# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, UniformCost)
p6 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s6 <- prioritizr::solve(p6)
saveRDS(s6, paste0(output_solutions, "s6-EM-Feature-tos-585.rds")) # save solution
# 5. Plot the spatial design
s6_plot <- s6 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol6 <- fSpatPlan_PlotSolution(s6_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Feature, SSP 5-8.5")
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
ggSol10 <- fSpatPlan_PlotSolution(s10_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Penalty, SSP 5-8.5")
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
ggSol34 <- fSpatPlan_PlotSolution(s34_plot, PUs, land) + 
  ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Climate Priority Area, SSP 5-8.5")
ggsave(filename = "EM-ClimatePriorityArea-tos-585.png",
       plot = ggSol34, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

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

solution_list <- list(s6, s2, s34, s10)
climateLayer_list <- roc_tos_SSP585
metric_list <- "tos"
approach_list <- c("feature", "percentile", "ClimatePriorityArea", "penalty")

# ----- Feature representation -----
names <- c("EM_Feature_tos_585", "EM_Percentile_tos_585", "EM_ClimatePriorityArea_tos_585", "EM_Penalty_tos_585")
feat_rep <- tibble(feature = character()) # empty tibble
for(i in 1:length(names)) {
  df <- represent_feature(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- left_join(df, feat_rep, by = "feature")
}
write.csv(feat_rep, paste0(output_summary, "ApproachTheme_tos_FeatureRepresentation.csv")) # save

# ----- Kernel distribution plots of targets -----
rev <- c("EM_Penalty_tos_585", "EM_ClimatePriorityArea_tos_585", "EM_Percentile_tos_585", "EM_Feature_tos_585")
x <- feat_rep %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature)) %>% 
  dplyr::mutate(approach = fct_relevel(approach, rev))

ggRidge <- ggplot(data = x) +
  geom_density_ridges(aes(x = percent, y = approach, group = approach, fill = approach),
                      scale = 2) +
  scale_fill_manual(values = c(`EM_ClimatePriorityArea_tos_585` = "#E6BA7E",
                               `EM_Feature_tos_585` = "#4D3B2A",
                               `EM_Penalty_tos_585` = "#6984BF",
                               `EM_Percentile_tos_585` = "#2B8142")) +
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
ggsave(filename = "TargetDist-ApproachTheme-tos.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
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

# Save corrplot
file_path_test = "Figures/ApproachTheme_CorrelationMatrix.png"
png(height=1200, width=1200, res = 200, file=file_path_test, type = "cairo")

matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list))

# Then
dev.off()

# ----- Create selection frequency plot -----
sFreq <- create_LowRegretSf(solution_list, names, PUs)
saveRDS(sFreq, paste0(output_lowregret, "sFreq4-EM-tos-585.rds")) # save low-regret solution

ggFreq <- plot_SelectionFrequency(sFreq, land) + 
  ggtitle("Approach Theme", subtitle = "Percentile (SSP 5-8.5)") +
    inset_element(plot_inset(sFreq), 0.7, 0.7, 0.99, 0.99)

ggsave(filename = "Freq-Approach-tos-585.png",
       plot = ggFreq, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# ----- Features according to frequency selection -----
PlanUnits <- PUs %>% 
  dplyr::mutate(cellID = row_number())
name <- c("selection_1", "selection_2", "selection_3", "selection_4")

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
                      scale = 2) +
  scale_fill_manual(values = c(selection_1 = "#bdc9e1",
                               selection_2 = "#74a9cf",
                               selection_3 = "#2b8cbe",
                               selection_4 = "#045a8d")) +
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

ggsave(filename = "Freq-Targets-ApproachTheme-tos.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# ----- Measuring how climate-smart solutions are using Kernel Density plots -----
# Kernel Density Plots
list <- list() # empty list
group_name = "approach"
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list) %>% 
  dplyr::mutate(approach = fct_relevel(approach, rev))

ggRidge <- ggplot() +
  geom_density_ridges_gradient(data = df %>% dplyr::filter(solution_1 == 1), aes(x = transformed, y = approach, fill = ..x..), scale = 1) +
  scale_fill_viridis_c(name = expression('Δ'^"o"*'C yr'^"-1"*''), option = "C") +
  geom_density_ridges(data = df %>% dplyr::filter(solution_1 == 0), aes(x = transformed, y = approach), alpha = 0.25, linetype = "dotted", scale = 1) +
  geom_vline(xintercept = climate$mean_climate_warming,
             linetype = "dashed", color = "tan1", size = 0.5) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
  labs(x = expression('Climate warming (Δ'^"o"*'C yr'^"-1"*')')) +
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
ggsave(filename = "ClimateWarmingDist-ApproachTheme-tos.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot
