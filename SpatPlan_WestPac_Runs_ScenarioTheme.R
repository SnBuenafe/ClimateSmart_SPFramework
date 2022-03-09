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

#### SSP 1-2.6 #####
# Parameters:
# Ensemble: Ensemble mean
# Climate metric: Rate of Climate Warming (SSP 1-2.6)
# Approach: "Percentile"
# 1. Prepare climate layer
# Intersect this with climate layer, select only those <= 35th percentile.
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP126, PUs = PUs)
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
# Parameters:
# Ensemble: Ensemble mean
# Climate metric: Rate of Climate Warming (SSP 2-4.5)
# Approach: "Percentile"
# 1. Prepare climate layer
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP245, PUs = PUs)
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
# Parameters:
# Ensemble: Ensemble mean
# Climate metric: Rate of Climate Warming (SSP 5-8.5)
# Approach: "Percentile"
# 1. Prepare climate layer
# Retain only planning units of each of the biodiversity features that in intersect with areas of low exposure (<= 35th percentile)
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP585, PUs = PUs)
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
# Feature representation
problem_list <- list(p38, p39, p2)
solution_list <- list(s38, s39, s2)
names <- c("EM-Percentile-tos-126", "EM-Percentile-tos-245", "EM-Percentile-tos-585")
feat_rep <- tibble(feature = character()) # empty tibble
for (i in 1:length(names)) {
  df <- represent_feature(problem_list[[i]], solution_list[[i]], names[i])
  feat_rep <- left_join(df, feat_rep, by = "feature")
}
write.csv(feat_rep, paste0(output_summary, "ScenarioTheme_tos_FeatureRepresentation.csv")) # save

# Summary
climateLayer_list <- list(roc_tos_SSP126, roc_tos_SSP245, roc_tos_SSP585)
df <- tibble(run = character()) # empty tibble
for(i in 1:length(names)) {
  statistics <- compute_summary(solution_list[[i]], total_area, PU_size, names[i], Cost = "cost")
  df <- rbind(statistics, df)
}
scenario_list <- c("126", "245", "585")
climate <- get_ClimateSummary(solution_list, climateLayer_list, "tos", col_scenario = scenario_list, col_approach = "percentile", col_run = names)

summary <- left_join(climate, df, by = "run")

write.csv(summary, paste0(output_summary, "ScenarioTheme_tos_Summary.csv")) # save

ggLowRegret_Area <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", theme = "scenario") + theme(axis.text = element_text(size = 25))
ggsave(filename = "Area-LR-Percentile-tos.png",
       plot = ggLowRegret_Area, width = 7, height = 5, dpi = 300,
       path = "Figures/") # save plot

# Get Kappa Correlation Matrix
list <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
object_list <- list() # empty list
solution_list <- list(s38, s39, s2)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#### Create low-regret areas ####
solution_list <- list(s38, s39, s2)
col_names <- c("penalty_tos_126", "penalty_tos_245", "penalty_tos_585")
s1_LRplot <- create_LowRegretSf(solution_list, col_names, PUs, scenario = TRUE)

(ggLowRegret1 <- plot_lowregret(s1_LRplot, land) + ggtitle("Low-Regret Areas: Different Scenarios", subtitle = "Rate of Climate Warming, Percentile") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "LR-Scenario-tos.png",
       plot = ggLowRegret1, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Summary: low-regret areas ####
summary <- compute_summary(s1_LRplot, total_area, PU_size, "LR-Percentile-tos", Cost = "cost")
write.csv(summary, paste0(output_summary, "ScenarioTheme_tos_LowRegretSummary.csv")) # save

#### Extras ####
x <- read_csv("Output/summary/ScenarioTheme_tos_FeatureRepresentation.csv") %>% 
  dplyr::select(-1)
plot <- ggplot(data = x) +
  geom_line(aes(x = row_number(feature), y = `EM-Percentile-tos-585`), color = "#855600", size = 0.03) +
  geom_line(aes(x = row_number(feature), y = `EM-Percentile-tos-245`), color = "#E6C173", size = 0.03) +
  geom_line(aes(x = row_number(feature), y = `EM-Percentile-tos-126`), color = "#289E3D", size = 1) +
  ylim(40, 50) +
  xlab("feature") +
  ylab("% target") +
  ggtitle("SSP 1-2.6") +
  theme_classic()
ggsave("Figures/extras/SscenarioTheme_Targets_126.png",
      width = 7, height = 5, dpi = 300,
       plot = plot)

plot <- ggplot(data = x) +
  geom_line(aes(x = row_number(feature), y = `EM-Percentile-tos-585`), color = "#855600", size = 0.03) +
  geom_line(aes(x = row_number(feature), y = `EM-Percentile-tos-126`), color = "#289E3D", size = 0.03) +
  geom_line(aes(x = row_number(feature), y = `EM-Percentile-tos-245`), color = "#E6C173", size = 1) +
  ylim(40, 50) +
  xlab("feature") +
  ylab("% target") +
  ggtitle("SSP 2-4.5") +
  theme_classic()
ggsave("Figures/extras/SscenarioTheme_Targets_245.png",
       width = 7, height = 5, dpi = 300,
       plot = plot)

plot <- ggplot(data = x) +
  geom_line(aes(x = row_number(feature), y = `EM-Percentile-tos-245`), color = "#E6C173", size = 0.03) +
  geom_line(aes(x = row_number(feature), y = `EM-Percentile-tos-126`), color = "#289E3D", size = 0.03) +
  geom_line(aes(x = row_number(feature), y = `EM-Percentile-tos-585`), color = "#855600", size = 1) +
  ylim(40, 50) +
  xlab("feature") +
  ylab("% target") +
  ggtitle("SSP 5-8.5") +
  theme_classic()
ggsave("Figures/extras/SscenarioTheme_Targets_585.png",
       width = 7, height = 5, dpi = 300,
       plot = plot)
