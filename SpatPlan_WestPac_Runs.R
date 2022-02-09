#' ---
#' title: "Running Analyses for Climate-Smart Method Paper on the Western Pacific"
#' author: "Tin Buenafe"
#' affiliation: "UQ"
#' date: "Last compiled on `r format(Sys.time(), '%A %d %B %Y')`"
#' output: html_document
#'---

##+ setup, include=FALSE
knitr::opts_chunk$set(warning=FALSE, cache=FALSE, message=FALSE, error=FALSE)
# knitr::opts_chunk$set(collapse = TRUE, comment = "", warning = "off")

#' ## Description
#' This code creates and analyzes spatial designs using the features and the planning region generated from `SpatPlan_Master_WestPac.R`

#' ## Preliminaries
source("HelperFunctions/SpatPlan_Extras.R") # Load the extras, including functions and libraries
source("HelperFunctions/SpatPlan_HelperFxns_WestPac.R") # Load helper functions written specifically for this spatial planning project

#' ## Load files
save_name <- "WestPacific"
PU_size = 669.9 # km2 (0.25 deg at equator)
Shape <- "Hexagon" # "Shape of PUs

#' ### Planning Region
PUs <- read_rds(file.path("Output", paste(save_name, "PU", paste0(PU_size,"km2"), "Output.rds", sep = "_")))
land <- ne_countries(scale = 'large', returnclass = 'sf') %>% 
  fSpatPlan_Convert2PacificRobinson() # Land masses; needed for plotting

#' ### Climate Metrics
#' 1. Rates of Climate Warming
ClimateLayer_path <- "Data/Climate/ClimateMetrics/RateOfChange/tos/"
ClimateLayer_files <- list.files(ClimateLayer_path)

roc_tos_SSP126 <- readRDS(file.path("Output", 
                                    paste(save_name, "PU", paste0(PU_size, "km2"),
                                          ClimateLayer_files[1], sep = "_")))
roc_tos_SSP245 <- readRDS(file.path("Output", 
                                    paste(save_name, "PU", paste0(PU_size, "km2"),
                                          ClimateLayer_files[2], sep = "_")))
roc_tos_SSP585 <- readRDS(file.path("Output", 
                                    paste(save_name, "PU", paste0(PU_size, "km2"),
                                          ClimateLayer_files[3], sep = "_")))
#' 2. Rates of Ocean Acidification
ClimateLayer_path <- "Data/Climate/ClimateMetrics/RateOfChange/phos/"
ClimateLayer_files <- list.files(ClimateLayer_path)

roc_phos_SSP126 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[1], sep = "_")))
roc_phos_SSP245 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[2], sep = "_")))
roc_phos_SSP585 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[3], sep = "_")))
#' 3. Rates of Declining Oxygen Concentration
ClimateLayer_path <- "Data/Climate/ClimateMetrics/RateOfChange/o2os/"
ClimateLayer_files <- list.files(ClimateLayer_path)

roc_o2os_SSP126 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[1], sep = "_")))
roc_o2os_SSP245 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[2], sep = "_")))
roc_o2os_SSP585 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[3], sep = "_")))
#' 4. Climate Velocity
ClimateLayer_path <- "Data/Climate/ClimateMetrics/ClimateVelocity/"
ClimateLayer_files <- list.files(ClimateLayer_path)

velocity_SSP126 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[1], sep = "_")))
velocity_SSP245 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[2], sep = "_")))
velocity_SSP585 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[3], sep = "_")))
#' 5. Annual marine heatwave intensity
# Conservation Features
aqua_sf <- read_rds(file.path("Output", 
                              paste(save_name, "PU", paste0(PU_size,"km2"), 
                                    "AquaMaps_Output.rds", sep = "_")))
# Changing to 1s and 0s
CutOff = 0.5
subset_aqua_sf <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(Doryrhamphus_excisus.excisus, Padina_sanctae.crucis, Platybelone_argalus.platyura,
                Tylosurus_acus.acus, Tylosurus_acus.melanotus)
aqua_sf <- aqua_sf %>% 
  mutate_at(vars(colnames(subset_aqua_sf)), 
            funs(case_when(. >= CutOff ~ 1,
                           . <= CutOff ~ 0,
                           is.na(.) ~ 0)))
# Cost Layer, Squished
cost <- read_rds(file.path("Output", 
                           paste(save_name, "PU", paste0(PU_size,"km2"), 
                                 "CostLayer_Output.rds", sep = "_"))) %>% 
  mutate(Cost_squish = scales::oob_squish(Cost, quantile(Cost, c(0.01, 0.99))))

#' ## Research Question 1
#' ### How much more would a climate-smart spatial design cost, compared to a climate-uninformed spatial design?

#' ### Climate-uninformed spatial design
#' #' To answer this, we first create the climate-uninformed design.
#' 1. Get the list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
#' 2. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, cost)
p1 <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.4) %>% # using 40% as the target percentage of protection
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
#' 3. Solve the planning problem 
s1 <- prioritizr::solve(p1)
#' 4. Plot the spatial design
s1_plot <- s1 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol1 <- fSpatPlan_PlotSolution(s1_plot, PUs, land) + ggtitle("Uninformed"))
#' 5. Check summary statistics
# Feature Representation
feat_rep <- represent_feature(p1, s1, "uninformed")
head(feat_rep)
total_area = nrow(PUs) * PU_size
print(total_area)
# Summary
summary <- compute_summary(s1, total_area, PU_size, "uninformed", Cost = "Cost_squish")
print(summary)

#' ### Climate-smart design (Rate of Climate Warming, SSP 5-8.5)
#' Then, we create the climate-smart design with the following parameters:
#' 1. Climate metric used: rate of climate warming forced under the highest climate scenario (SSP 5-8.5)
#' 2. Climate-smart approach used: "percentile" approach

#' 1. Prepare climate layer
# Retain only planning units of each of the biodiversity features that in intersect with areas of low exposure (<= 50th percentile)
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP585, PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile, roc_tos_SSP585, cost)
p2 <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.8) %>% # using Effective 40% Protection. Since we only retained planning units that intersect with both biodiversity features and areas <= 50th percentile (0.5), by multiplying this by 0.8 target, we effectively protect only 40%.
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s2 <- prioritizr::solve(p2)

#' 5. Plot the spatial design
s2_plot <- s2 %>% 
  mutate(solution_1 = as.logical(solution_1))
(ggSol2 <- fSpatPlan_PlotSolution(s2_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 5-8.5"))

#' 6. Check summary statistics
# Feature Representation
temp <- represent_feature(p2, s2, "percentile_tos_585")
feat_rep <- left_join(temp, feat_rep)
head(feat_rep)

# Summary
temp <- compute_summary(s2, total_area, PU_size, "percentile_tos_585", Cost = "Cost_squish")
summary <- rbind(temp, summary)
print(summary)

# Intersect climate-uninformed spatial plan with the climate layer to get its mean rate of climate warming
# Getting the mean becaus rate of climate warming seems... normal?
# tmp <- roc_tos_SSP585 %>% as_tibble()
#  dplyr::select(slpTrends)
# qqnorm(tmp$slpTrends)
# qqline(tmp$slpTrends)

df <- s1 %>% as_tibble() %>% 
  dplyr::select(solution_1, geometry) %>% 
  left_join(., roc_tos_SSP585) %>% 
  filter(solution_1 == 1) %>% 
  summarize(mean_climate_warming = mean(slpTrends),
            mean_log_climate_warming = mean(log(slpTrends))) %>% 
  dplyr::mutate(run = "uninformed")

# Get the mean rate of climate warming for climate-smart design
df1 <- s2 %>% as_tibble() %>% 
  dplyr::select(solution_1, geometry, slpTrends) %>% 
  filter(solution_1 == 1) %>% 
  summarize(mean_climate_warming = mean(slpTrends),
            mean_log_climate_warming = mean(log(slpTrends))) %>% 
  dplyr::mutate(run = "percentile_tos_585") %>% 
  add_row(df, .)

summary <- summary %>% left_join(., df1, by = c("run"))

# Cost
(ggSummary_Cost <- plot_statistics(summary, col_name = "log10(total_cost)", y_axis = "log10(cost)", color = 2))
# Area
(ggSummary_Area <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", color = 2))
# Mean Climate Warming
(ggSummary_Warming <- plot_statistics(summary, col_name = "mean_climate_warming", y_axis = expression('Δ'^"o"*'C yr'^"-1"*''), color = 2))
(ggSummary_LogWarming <- plot_statistics(summary, col_name = "mean_log_climate_warming", y_axis = expression('log Δ'^"o"*'C yr'^"-1"*''), color = 2) + scale_y_reverse())

#' 7. Comparing Spatial Plans
(gg <- fSpatPlan_PlotComparison(s1_plot, s2_plot, land))

## Create Kappa Correlation Matrix
list <- c("uninformed", "percentile_tos_585")
object_list <- list() # empty list
solution_list <- list(s1, s2)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#' ## Research Question 2
#' ### How does utilizing climate metrics calculated from different climatic variables change climate-smart spatial designs?
#' We should create climate-smart spatial designs using different climate metrics, still using the "percentile" approach
#' We already did the climate-smart spatial design using rate of climate warming (p2, s2)
#' 
#' ### Climate-smart spatial design (Rate of Ocean Acidification)
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those >= median (50th percentile). Greater, because you don't want a lower pH.
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "phos", colname = "slpTrends", metric_df = roc_phos_SSP585, PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, roc_phos_SSP585, cost)
p3 <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.8) %>% # using Effective 40% Protection. Since we only retained planning units that intersect with both biodiversity features and areas >= 50th percentile (0.5), by multiplying this by 0.8 target, we effectively protect only 40%.
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s3 <- prioritizr::solve(p3)

#' 5. Plot the spatial design
s3_plot <- s3 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol3 <- fSpatPlan_PlotSolution(s3_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Percentile, SSP 5-8.5"))

#' ### Climate-smart spatial design (Rate of Declining Oxygen Concentration)
#' 1. Prepare climate layer
# Intersect this with climate layer, select only those >= median (50th percentile). Greater, because you don't want low oxygen.
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "o2os", colname = "slpTrends", metric_df = roc_o2os_SSP585, PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, roc_o2os_SSP585, cost)
p4 <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.8) %>% # using Effective 40% Protection. Since we only retained planning units that intersect with both biodiversity features and areas >= 50th percentile (0.5), by multiplying this by 0.8 target, we effectively protect only 40%.
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s4 <- prioritizr::solve(p4)

#' 5. Plot the spatial design
s4_plot <- s4 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol4 <- fSpatPlan_PlotSolution(s4_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concetration", subtitle = "Percentile, SSP 5-8.5"))

#' ### Climate-smart spatial design (Climate Velocity)
#' 1. Prepare climate layer
# biodiversity <- filter_biodiversity_features(aqua_sf)

# Intersect this with climate layer, select only those <= median (50th percentile). Lesser, because you don't want a higher velocity.
aqua_percentile <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "velocity", colname = "voccMag", metric_df = velocity_SSP585, PUs = PUs)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
# targets should be the same as the last climate-smart run
# print(targets)
out_sf <- cbind(aqua_percentile, velocity_SSP585, cost)
p5 <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.8) %>% # using Effective 40% Protection. Since we only retained planning units that intersect with both biodiversity features and areas <= 50th percentile (0.5), by multiplying this by 0.8 target, we effectively protect only 40%.
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s5 <- prioritizr::solve(p5)

#' 5. Plot the spatial design
s5_plot <- s5 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol5 <- fSpatPlan_PlotSolution(s5_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Percentile, SSP 5-8.5"))

#' ### Summary of all climate-smart designs
#' 1. Feature Representation
list <- c("percentile_phos_585", "percentile_o2os_585", "percentile_velocity_585")
for (i in 3:5) {
  p <- get(paste0("p",i))
  s <- get(paste0("s", i))
  tmp_df <- represent_feature(p, s, list[i-2])
  feat_rep <- left_join(tmp_df, feat_rep)
}

#' 2. Summary

#' Check normality
check_normality(roc_tos_SSP585, "slpTrends")
# Looks normal. I'll use mean.

check_normality(roc_phos_SSP585, "slpTrends")
# Looks Normal! So I'll use the mean for rate of ocean acidification.

check_normality(roc_o2os_SSP585, "slpTrends")
# Looks normal with a few outliers? Maybe use the mean as well?

check_normality(velocity_SSP585, "voccMag")
# Definitely not normal! Use median.

# Get the summary and the mean rate of climate warming, also the mean log rate? for all designs
run_list <- c("percentile_phos_585", "percentile_o2os_585", "percentile_velocity_585")
solution_list <- list(s3, s4, s5)
emptyList <- list()
for (i in 1:length(run_list)) {
  emptyList[[i]] <- compute_summary(solution_list[[i]], total_area, PU_size, run_list[i], Cost = "Cost_squish")
}
percentileSummary <- do.call(rbind, emptyList)

summary <- summary %>% dplyr::mutate(scenario = "585",
                                     approach = ifelse(run == "uninformed", yes = NA, no = "percentile"))

summary <- get_ClimateSummary(solution_list, climate_layer = roc_tos_SSP585, metric = "tos", col_scenario = "585", col_approach = "percentile", col_run = run_list) %>% 
  left_join(., percentileSummary) %>% 
  rbind(summary)

# Get the mean rate of ocean acidification for all designs (and also the mean of the log), starting from the uninformed
solution_list1 <- list(s1)
run_list1 <- c("uninformed")
tmp <- get_ClimateSummary(solution_list1, climate_layer = roc_phos_SSP585, metric = "phos", col_scenario = "585", col_approach = NA, col_run = run_list1)

solution_list <- list(s2, s3, s4, s5)
run_list <- c("percentile_tos_585", "percentile_phos_585", "percentile_o2os_585", "percentile_velocity_585")

summary <- get_ClimateSummary(solution_list, climate_layer = roc_phos_SSP585, metric = "phos", col_scenario = "585", col_approach = "percentile", col_run = run_list) %>% 
  rbind(., tmp) %>% 
  left_join(., summary, by = c("run", "scenario", "approach"))

# Get mean rate of decline in oxygen concentration (and also the mean of the log) for all designs, starting with uninformed
tmp <- get_ClimateSummary(solution_list1, climate_layer = roc_o2os_SSP585, metric = "o2os", col_scenario = "585", col_approach = NA, col_run = run_list1)

summary <- get_ClimateSummary(solution_list, climate_layer = roc_o2os_SSP585, metric = "o2os", col_scenario = "585", col_approach = "percentile", col_run = run_list) %>% 
  rbind(., tmp) %>%
  left_join(., summary, by = c("run", "scenario", "approach"))

# Get median climate velocity concentrations & also mean of the log! starting from the uninformed
tmp <- get_ClimateSummary(solution_list1, climate_layer = velocity_SSP585, metric = "velocity", col_scenario = "585", col_approach = NA, col_run = run_list1)

summary <- get_ClimateSummary(solution_list, climate_layer = velocity_SSP585, metric = "velocity", col_scenario = "585", col_approach = "percentile", col_run = run_list) %>%
  rbind(., tmp) %>% 
  left_join(., summary, by = c("run", "scenario", "approach"))

#' Graph summary
# Cost
ggSummary_Cost <- plot_statistics(summary, col_name = "log10(total_cost)", y_axis = "log10(cost)", color = 1)
# Area
ggSummary_Area <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area", color = 1)
ggSummary_Cost + ggSummary_Area + plot_layout(guides = "collect")
# Climate Warming
ggSummary_Warming <- plot_statistics(summary, col_name = "mean_climate_warming", y_axis = expression('Δ'^"o"*'C yr'^"-1"*''), color = 1)
ggSummary_log_Warming <- plot_statistics(summary, col_name = "mean_log_climate_warming", y_axis = expression('log Δ'^"o"*'C yr'^"-1"*''), color = 1) + scale_y_reverse()
ggSummary_Warming + ggSummary_log_Warming + plot_layout(guides = "collect")
# Ocean Acidification
ggSummary_Ocean_Acidification <- plot_statistics(summary, col_name = "mean_ocean_acidification", y_axis = expression('Δ pH yr'^"-1"*''), color = 1) + scale_y_reverse()
ggSummary_log_Ocean_Acidification <- plot_statistics(summary, col_name = "mean_log_ocean_acidification", y_axis = expression('log Δ pH yr'^"-1"*''), color = 1) + scale_y_reverse()
ggSummary_Ocean_Acidification + ggSummary_log_Ocean_Acidification + plot_layout(guides = "collect")
# Oxygen Decline
ggSummary_Oxygen_Decline <- plot_statistics(summary, col_name = "mean_oxygen_decline", y_axis = expression('Δ mol m'^"-3"*' yr'^"-1"*''), color = 1) + scale_y_reverse()
ggSummary_log_Oxygen_Decline <- plot_statistics(summary, col_name = "mean_log_oxygen_decline", y_axis = expression('Δ mol m'^"-3"*' yr'^"-1"*''), color = 1) + scale_y_reverse()
ggSummary_Oxygen_Decline + ggSummary_log_Oxygen_Decline + plot_layout(guides = "collect")
# Climate Velocity
ggSummary_Climate_Velocity <- plot_statistics(summary, col_name = "median_velocity", y_axis = expression('km yr'^"-1"*''), color = 1)
ggSummary_Log_Climate_Velocity <- plot_statistics(summary, col_name = "mean_log_velocity", y_axis = expression('log km yr'^"-1"*''), color = 1)
ggSummary_Climate_Velocity + ggSummary_Log_Climate_Velocity + plot_layout(guides = "collect")

#' Get Kappa Correlation Matrix
list <- c("uninformed", "percentile_tos_585", "percentile_phos_585", "percentile_o2os_585", "percentile_velocity_585")
object_list <- list() # empty list
solution_list <- list(s1, s2, s3, s4, s5)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#' ### Low-regret Areas
#' To create low-regret climate-smart design, we should only select areas that have been selected for all climate-smart designs utilizing different climate metrics
# Select solutions for all climate-smart designs
solution_list <- list(s2, s3, s4, s5)
col_names <- c("percentile_tos_585", "percentile_phos_585", "percentile_o2os_585", "percentile_velocity_585")
LowRegret_Percentile <- create_LowRegretSf(solution_list, col_names, PUs)

(gg_LowRegret <- plot_lowregret(LowRegret_Percentile, land))

#' Check low-regret summary
LowRegret_SummaryPercentile <- compute_summary(LowRegret_Percentile, total_area, PU_size, "low_regret", Cost = "Cost_squish") %>%
  mutate(approach = "percentile", scenario = "585")
print(LowRegret_SummaryPercentile)

#' ## Research Question 3
#' ### How can we incorporate climate metrics into a marine reserve design workflow?
#' We explore 3 climate-smart approaches here:
#' 1. "Feature" approach: treating the climate metric as a feature itself, with its own target
#' 2. "Percentile" approach: filtering the planning units for each of the biodiversity feature by only retaining those that have low exposure or high retention (wrt the species' range)
#' 3. "Penalty" approach: treating the climate metric as a linear penalty, ultimately altering the objective function.
#' We already did percentile in RQ2. Here we're going to do "Feature" and "Penalty" approaches forced under the highest climate scenario.
#' 
#' ### "Feature" approach
#' #' ### Climate-smart spatial design (Rate of Climate Warming)
#' 
#' 1. Prepare climate layer
ClimateFeature <- create_FeatureLayer(aqua_sf, metric_name = "tos", colname = "slpTrends", metric_df = roc_tos_SSP585)

#' 2. Get list of features
features <- aqua_percentile %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
features <- append(features, "climate_layer") # add "climate_layer" to features

#' 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, cost)

# using Effective 40% Protection. Since we only considered the climate_layer as 1s if they are under (or above for phos and o2os) the 50th percentile (0.5), we multiply it by 0.8 to get an effective protection of 40%.
targets <- features %>% as_tibble() %>% 
  setNames(., "Species") %>% 
  add_column(target = 0.4) %>% 
  mutate(target = ifelse(Species == "climate_layer", 0.8, 0.4))

p6 <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s6 <- prioritizr::solve(p6)

#' 5. Plot the spatial design
s6_plot <- s6 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol6 <- fSpatPlan_PlotSolution(s6_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Feature, SSP 5-8.5"))
#' 
#' ### Climate-smart spatial design (Rate of Ocean Acidification)
#' 
#' 1. Prepare climate layer
ClimateFeature <- create_FeatureLayer(aqua_sf, metric_name = "phos", colname = "slpTrends", metric_df = roc_phos_SSP585)

#' 2. Get list of features
# features should be the same as above

#' 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, cost)

# targets should be the same as above

p7 <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s7 <- prioritizr::solve(p7)

#' 5. Plot the spatial design
s7_plot <- s7 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol7 <- fSpatPlan_PlotSolution(s7_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Feature, SSP 5-8.5"))

#' ### Climate-smart spatial design (Rate of Oxygen Decline)
#' 
#' 1. Prepare climate layer
ClimateFeature <- create_FeatureLayer(aqua_sf, metric_name = "o2os", colname = "slpTrends", metric_df = roc_o2os_SSP585)

#' 2. Get list of features
# features should be the same as above

#' 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, cost)

# targets should be the same as above

p8 <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s8 <- prioritizr::solve(p8)

#' 5. Plot the spatial design
s8_plot <- s8 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol8 <- fSpatPlan_PlotSolution(s8_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Feature, SSP 5-8.5"))

#' ### Climate-smart spatial design (Climate Velocity)
#' 
#' 1. Prepare climate layer
ClimateFeature <- create_FeatureLayer(aqua_sf, metric_name = "velocity", colname = "voccMag", metric_df = velocity_SSP585)

#' 2. Get list of features
# features should be the same as above

#' 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, ClimateFeature, cost)

# targets should be the same as above

p9 <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(targets$target) %>% 
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' 4. Solve the planning problem 
s9 <- prioritizr::solve(p9)

#' 5. Plot the spatial design
s9_plot <- s9 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol9 <- fSpatPlan_PlotSolution(s9_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Feature, SSP 5-8.5"))

# Feature representation
list <- c("feature_tos_585", "feature_phos_585", "feature_o2os_585", "feature_velocity_585")
problem_list <- list(p6, p7, p8, p9)
solution_list <- list(s6, s7, s8, s9)
for (i in 1:length(list)) {
  tmp_df <- represent_feature(problem_list[[i]], solution_list[[i]], list[i])
  feat_rep <- left_join(tmp_df, feat_rep)
}

#' ### Summary of Feature

# Computing summaries for all "feature" approach designs
run_list <- c("feature_tos_585", "feature_phos_585", "feature_o2os_585",
              "feature_velocity_585")
solution_list <- list(s6, s7, s8, s9)
emptyList <- list()
for (i in 1:length(run_list)) {
  emptyList[[i]] <- compute_summary(solution_list[[i]], total_area, PU_size, run_list[i], Cost = "Cost_squish")
}
featureSummary <- do.call(rbind, emptyList)

# Get the mean rate of climate warming for all designs
warming <- get_ClimateSummary(solution_list, climate_layer = roc_tos_SSP585, metric = "tos", col_scenario = "585", col_approach = "feature", col_run = run_list) %>% 
  left_join(., featureSummary)

# Get the mean rate of ocean acidification for all designs
acidification <- get_ClimateSummary(solution_list, climate_layer = roc_phos_SSP585, metric = "phos", col_scenario = "585", col_approach = "feature", col_run = run_list) %>% 
  left_join(., warming, by = c("run", "scenario", "approach"))

# Get the mean rate of declining oxygen concentration for all designs
oxygen <- get_ClimateSummary(solution_list, climate_layer = roc_o2os_SSP585, metric = "o2os", col_scenario = "585", col_approach = "feature", col_run = run_list) %>% 
  left_join(., acidification, by = c("run", "scenario", "approach"))

# Get the mean climate velocity for all designs, then bind it with the summary
summary <- get_ClimateSummary(solution_list, climate_layer = velocity_SSP585, metric = "velocity", col_scenario = "585", col_approach = "feature", col_run = run_list) %>% 
  left_join(., oxygen, by = c("run", "scenario", "approach")) %>% 
  rbind(., summary)
head(summary)

#' Get Kappa Correlation Matrix
list <- c("uninformed", "feature_tos_585", "feature_phos_585", "feature_o2os_585", "feature_velocity_585")
object_list <- list() # empty list
solution_list <- list(s1, s6, s7, s8, s9)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#' ### Low-regret Areas
#' To create low-regret climate-smart design, we should only select areas that have been selected for all climate-smart designs utilizing different climate metrics
# Select solutions for all climate-smart designs
solution_list <- list(s6, s7, s8, s9)
col_names <- c("feature_tos_585", "feature_phos_585", "feature_o2os_585", "feature_velocity_585")
LowRegret_Feature <- create_LowRegretSf(solution_list, col_names, PUs)

(gg_LowRegret <- plot_lowregret(LowRegret_Feature, land))

#' Check low-regret summary
LowRegret_SummaryFeature <- compute_summary(LowRegret_Feature, total_area, PU_size, "low_regret", Cost = "Cost_squish") %>% 
  mutate(approach = "feature", scenario = "585")
print(LowRegret_SummaryFeature)

#' ### "Penalty" approach
#' 
#' ### Climate-smart spatial design (Rate of Climate Warming)
#' 
#' 1. Prepare climate layer
#' First, we should get the scaling. Scaling would be different for each of the climate metrics.
#' To get the scaling, we need to know at what percentage do we want to scale the climate metric compared to the cost.
#' In this scenario, I'm going to look into scaling from 20-70% (increments of 10%), but will report 30% (for now).
#' I calculated scaling using this equation:
#' scaling$_ClimateMetric$ $= \frac{(Cost_{Max} - Cost_{Min})}{(ClimateMetric_{Max} - ClimateMetric_{Min})} \cdot (Scaling_{percent})$

scaling_PenaltyWarming <- create_Scaling(cost$Cost_squish, roc_tos_SSP585$slpTrends, "tos")

#' 2. Get list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, roc_tos_SSP585, cost)
scaling <- scaling_PenaltyWarming %>% filter(scaling == 30) %>% pull() # get scaling for 30%

p10 <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.4) %>% # target is 40% for all features.
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "slpTrends")

#' 4. Solve the planning problem 
s10 <- prioritizr::solve(p10)

#' 5. Plot the spatial design
s10_plot <- s10 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol10 <- fSpatPlan_PlotSolution(s10_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Penalty, SSP 5-8.5"))

#' ### Climate-smart spatial design (Rate of Ocean Acidification)
#' 1. Prepare climate layer
scaling_PenaltyAcidification <- create_Scaling(cost$Cost_squish, roc_phos_SSP585$slpTrends, "phos")
#' 2. Get list of features
#' Feature list should be the same as above.
#' 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, roc_phos_SSP585, cost)
scaling <- scaling_PenaltyAcidification %>% filter(scaling == 30) %>% pull() # get scaling for 30%

p11 <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.4) %>% # target is 40% for all features.
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "slpTrends")

#' 4. Solve the planning problem 
s11 <- prioritizr::solve(p11)

#' 5. Plot the spatial design
s11_plot <- s11 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol11 <- fSpatPlan_PlotSolution(s11_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Ocean Acidification", subtitle = "Penalty, SSP 5-8.5"))

#' ### Climate-smart spatial design (Rate of Declining Oxygen Concentration)
#' 1. Prepare climate layer
scaling_PenaltyOxygen <- create_Scaling(cost$Cost_squish, roc_o2os_SSP585$slpTrends, "o2os")
#' 2. Get list of features
#' Feature list should be the same as above.
#' 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, roc_o2os_SSP585, cost)
scaling <- scaling_PenaltyOxygen %>% filter(scaling == 30) %>% pull() # get scaling for 30%

p12 <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.4) %>% # target is 40% for all features.
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "slpTrends")

#' 4. Solve the planning problem
s12 <- prioritizr::solve(p12)

#' 5. Plot the spatial design
s12_plot <- s12 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol12 <- fSpatPlan_PlotSolution(s12_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Declining Oxygen Concentration", subtitle = "Penalty, SSP 5-8.5"))

#' ### Climate-smart spatial design (Climate Velocity)
#' 1. Prepare climate layer
scaling_PenaltyVelocity <- create_Scaling(cost$Cost_squish, velocity_SSP585$voccMag, "velocity")
#' 2. Get list of features
#' Feature list should be the same as above.
#' 3. Set up the spatial planning problem
out_sf <- cbind(aqua_sf, velocity_SSP585, cost)
scaling <- scaling_PenaltyVelocity %>% filter(scaling == 30) %>% pull() # get scaling for 30%

p13 <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.4) %>% # target is 40% for all features.
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(scaling, data = "voccMag")

#' 4. Solve the planning problem
s13 <- prioritizr::solve(p13)

#' 5. Plot the spatial design
s13_plot <- s13 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol13 <- fSpatPlan_PlotSolution(s13_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Penalty, SSP 5-8.5"))

#' ### Summary of Feature

# Feature representation
list <- c("penalty_tos_585", "penalty_phos_585", "penalty_o2os_585", "penalty_velocity_585")
problem_list <- list(p10, p11, p12, p13)
solution_list <- list(s10, s11, s12, s13)
for (i in 1:length(list)) {
  tmp_df <- represent_feature(problem_list[[i]], solution_list[[i]], list[i])
  feat_rep <- left_join(tmp_df, feat_rep)
}

# Computing summaries for all "penalty" approach designs
run_list <- c("penalty_tos_585", "penalty_phos_585", "penalty_o2os_585",
              "penalty_velocity_585")
solution_list <- list(s10, s11, s12, s13)
emptyList <- list()
for (i in 1:length(run_list)) {
  emptyList[[i]] <- compute_summary(solution_list[[i]], total_area, PU_size, run_list[i], Cost = "Cost_squish")
}
penaltySummary <- do.call(rbind, emptyList)

# Get the mean rate of climate warming for all designs
warming <- get_ClimateSummary(solution_list, climate_layer = roc_tos_SSP585, metric = "tos", col_scenario = "585", col_approach = "penalty", col_run = run_list) %>% 
  left_join(., penaltySummary)

# Get the mean rate of ocean acidification for all designs
acidification <- get_ClimateSummary(solution_list, climate_layer = roc_phos_SSP585, metric = "phos", col_scenario = "585", col_approach = "penalty", col_run = run_list) %>% 
  left_join(., warming, by = c("run", "scenario", "approach"))

# Get the mean rate of declining oxygen concentration for all designs
oxygen <- get_ClimateSummary(solution_list, climate_layer = roc_o2os_SSP585, metric = "o2os", col_scenario = "585", col_approach = "penalty", col_run = run_list) %>% 
  left_join(., acidification, by = c("run", "scenario", "approach"))

# Get the mean climate velocity for all designs, then bind it with the summary
summary <- get_ClimateSummary(solution_list, climate_layer = velocity_SSP585, metric = "velocity", col_scenario = "585", col_approach = "penalty", col_run = run_list) %>% 
  left_join(., oxygen, by = c("run", "scenario", "approach")) %>% 
  rbind(., summary)
head(summary)

#' Get Kappa Correlation Matrix
list <- c("uninformed", "penalty_tos_585", "penalty_phos_585", "penalty_o2os_585", "penalty_velocity_585")
object_list <- list() # empty list
solution_list <- list(s1, s10, s11, s12, s13)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

#' ### Low-regret Areas
#' To create low-regret climate-smart design, we should only select areas that have been selected for all climate-smart designs utilizing different climate metrics
# Select solutions for all climate-smart designs
solution_list <- list(s10, s11, s12, s13)
col_names <- c("penalty_tos_585", "penalty_phos_585", "penalty_o2os_585", "penalty_velocity_585")
LowRegret_Penalty <- create_LowRegretSf(solution_list, col_names, PUs)

(gg_LowRegret <- plot_lowregret(LowRegret_Penalty, land))

#' Check low-regret summary
LowRegret_SummaryPenalty <- compute_summary(low_regret, total_area, PU_size, "low_regret", Cost = "Cost_squish") %>% 
  mutate(approach = "penalty", scenario = "585")
print(LowRegret_SummaryPenalty)

#### RQ4 ####
#' ## Research Question 4
#' ### What are the pros and cons of the different climate-smart marine reserve design approaches explored here?

#' ### Graph summary of plots
df <- summary %>% 
  dplyr::mutate(metric = case_when(str_detect(run, pattern = "tos") ~ "tos",
                                   str_detect(run, pattern = "phos") ~ "phos",
                                   str_detect(run, pattern = "o2os") ~ "o2os",
                                   str_detect(run, pattern = "velocity") ~ "velocity",
                                   str_detect(run, pattern = "uninformed") ~ "uninformed")) %>% 
  dplyr::mutate(approach = ifelse(str_detect(run, pattern = "uninformed"), yes = "uninformed", no = approach))

# Cost + Area
ggComparison_Cost <- plot_ComparisonStatistics(df, col_name = "log10(total_cost)", y_axis = "log10(cost)")
ggComparison_Area <- plot_ComparisonStatistics(df, col_name = "percent_area", y_axis = "% area")
ggComparison_Cost + ggComparison_Area + plot_layout(guides = "collect")
# Climate warming
ggComparison_Warming <- plot_ComparisonStatistics(df, col_name = "mean_climate_warming", y_axis = expression('Δ'^"o"*'C yr'^"-1"*''))
ggComparison_LogWarming <- plot_ComparisonStatistics(df, col_name = "mean_log_climate_warming", y_axis = expression('log Δ'^"o"*'C yr'^"-1"*'')) + scale_y_reverse()
ggComparison_Warming + ggComparison_LogWarming + plot_layout(guides = "collect")
# Ocean Acidification
ggComparison_OceanAcidification <- plot_ComparisonStatistics(df, col_name = "mean_ocean_acidification", y_axis = expression('Δ pH yr'^"-1"*'')) + scale_y_reverse()
ggComparison_LogOceanAcidification <- plot_ComparisonStatistics(df, col_name = "mean_log_ocean_acidification", y_axis = expression('log Δ pH yr'^"-1"*'')) + scale_y_reverse()
ggComparison_OceanAcidification + ggComparison_LogOceanAcidification + plot_layout(guides = "collect")
# Oxygen Decline
ggComparison_OxygenDecline <- plot_ComparisonStatistics(df, col_name = "mean_oxygen_decline", y_axis = expression('Δ mol m'^"-3"*' yr'^"-1"*'')) + scale_y_reverse()
ggComparison_LogOxygenDecline <- plot_ComparisonStatistics(df, col_name = "mean_log_oxygen_decline", y_axis = expression('Δ mol m'^"-3"*' yr'^"-1"*'')) + scale_y_reverse()
ggComparison_OxygenDecline + ggComparison_LogOxygenDecline + plot_layout(guides = "collect")
# Climate Velocity
ggComparison_ClimateVelocity <- plot_ComparisonStatistics(df, col_name = "median_velocity", y_axis = expression('km yr'^"-1"*''))
ggComparison_LogClimateVelocity <- plot_ComparisonStatistics(df, col_name = "mean_log_velocity", y_axis = expression('log km yr'^"-1"*''))
ggComparison_ClimateVelocity + ggComparison_LogClimateVelocity + plot_layout(guides = "collect")

#' ### Compare Low Regret Areas
ggLowRegret_Feature <- plot_lowregret(LowRegret_Feature, land) + labs(subtitle = "Feature, SSP 5-8.5")
ggLowRegret_Percentile <- plot_lowregret(LowRegret_Percentile, land) + labs(subtitle = "Percentile, SSP 5-8.5")
ggLowRegret_Penalty <- plot_lowregret(LowRegret_Penalty, land) + labs(subtitle = "Penalty, SSP 5-8.5")

ggLowRegret_Feature + ggLowRegret_Percentile + ggLowRegret_Penalty + plot_layout(guides = "collect")

LowRegret_SummaryAll <- LowRegret_SummaryFeature %>% 
  rbind(., LowRegret_SummaryPercentile) %>% 
  rbind(., LowRegret_SummaryPenalty)

# Kappa
list <- c("LowRegret_Feature_585", "LowRegret_Percentile_585", "LowRegret_Penalty_585")
object_list <- list() # empty list
solution_list <- list(LowRegret_Feature, LowRegret_Percentile, LowRegret_Penalty)
for (i in 1:length(list)) {
  obj <- select_solution(solution_list[[i]], list[i])
  object_list[[i]] <- obj
}

(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

# Check the Climate Metrics of the Low-Regret Areas
LowRegretFeature_df <- LowRegret_Feature %>% 
  as_tibble() %>% 
  dplyr::select(-feature_velocity_585, -feature_o2os_585, -feature_phos_585, -feature_tos_585, -Cost, -Cost_squish) %>%
  dplyr::rename(SelectionFeature = selection) %>% 
  left_join(., roc_tos_SSP585 %>% as_tibble() %>% dplyr::select(slpTrends, geometry), by = "geometry") %>% 
  dplyr::rename(tos = slpTrends, Feature_Solution = solution_1) %>% 
  left_join(., roc_phos_SSP585 %>% as_tibble() %>% dplyr::select(slpTrends, geometry), by = "geometry") %>% 
  dplyr::rename(phos = slpTrends) %>% 
  left_join(., roc_o2os_SSP585 %>% as_tibble() %>% dplyr::select(slpTrends, geometry), by = "geometry") %>% 
  dplyr::rename(o2os = slpTrends) %>% 
  left_join(., velocity_SSP585 %>% as_tibble() %>% dplyr::select(voccMag, geometry), by = "geometry") %>% 
  dplyr::rename(velocity = voccMag)
  
LowRegretPercentile_df <- LowRegret_Percentile %>% 
  as_tibble() %>% 
  dplyr::select(-percentile_velocity_585, -percentile_o2os_585, -percentile_phos_585, -percentile_tos_585, -Cost, -Cost_squish) %>% 
  dplyr::rename(SelectionPercentile = selection, Percentile_Solution = solution_1)

LowRegretPenalty_df <- LowRegret_Penalty %>% 
  as_tibble() %>% 
  dplyr::select(-penalty_velocity_585, -penalty_o2os_585, -penalty_phos_585, -penalty_tos_585, -Cost, -Cost_squish) %>%
  dplyr::rename(SelectionPenalty = selection, Penalty_Solution = solution_1)

LowRegretAll_sf <- left_join(LowRegretFeature_df, LowRegretPercentile_df, by = c("cellID", "geometry")) %>% 
  left_join(., LowRegretPenalty_df, by = c("cellID", "geometry")) %>% 
  dplyr::mutate(SelectionAll = case_when((SelectionFeature == 4 & SelectionPercentile == 4 & SelectionPenalty == 4) ~ 1,
                                         TRUE ~ 0)) %>% 
  st_as_sf(sf_column_name = "geometry")

LowRegret_SummaryAll <- lowRegret_ClimateSummary(df = LowRegretAll_sf, approach_column = "Feature_Solution") %>% 
  rbind(., lowRegret_ClimateSummary(df = LowRegretAll_sf, approach_column = "Percentile_Solution")) %>% 
  rbind(., lowRegret_ClimateSummary(df = LowRegretAll_sf, approach_column = "Penalty_Solution")) %>% 
  as_tibble() %>% 
  left_join(., LowRegret_SummaryAll, by = "approach")

# Cost + Area
ggComparison_Cost_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "log10(total_cost)", y_axis = "log10(cost)")
ggComparison_Area_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "percent_area", y_axis = "% area")
ggComparison_Cost_LowRegret + ggComparison_Area_LowRegret + plot_layout(guides = "collect")
# Climate warming
ggComparison_Warming_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_climate_warming", y_axis = expression('Δ'^"o"*'C yr'^"-1"*''))
ggComparison_LogWarming_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_log_climate_warming", y_axis = expression('log Δ'^"o"*'C yr'^"-1"*'')) + scale_y_reverse()
ggComparison_Warming_LowRegret + ggComparison_LogWarming_LowRegret + plot_layout(guides = "collect")
# Ocean Acidification
ggComparison_OceanAcidification_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_ocean_acidification", y_axis = expression('Δ pH yr'^"-1"*'')) + scale_y_reverse()
ggComparison_LogOceanAcidification_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_log_ocean_acidification", y_axis = expression('log Δ pH yr'^"-1"*'')) + scale_y_reverse()
ggComparison_OceanAcidification_LowRegret + ggComparison_LogOceanAcidification_LowRegret + plot_layout(guides = "collect")
# Oxygen Decline
ggComparison_OxygenDecline_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_oxygen_decline", y_axis = expression('Δ mol m'^"-3"*' yr'^"-1"*'')) + scale_y_reverse()
ggComparison_LogOxygenDecline_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_log_oxygen_decline", y_axis = expression('Δ mol m'^"-3"*' yr'^"-1"*'')) + scale_y_reverse()
ggComparison_OxygenDecline_LowRegret + ggComparison_LogOxygenDecline_LowRegret + plot_layout(guides = "collect")
# Climate Velocity
ggComparison_ClimateVelocity_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "median_velocity", y_axis = expression('km yr'^"-1"*''))
ggComparison_LogClimateVelocity_LowRegret <- plot_LowRegretStatistics(LowRegret_SummaryAll, col_name = "mean_log_velocity", y_axis = expression('log km yr'^"-1"*''))
ggComparison_ClimateVelocity_LowRegret + ggComparison_LogClimateVelocity_LowRegret + plot_layout(guides = "collect")

#' Create Low-Regret Areas across all Low-Regret Spatial Plans for the three approaches
LowRegret_sf <- LowRegretAll_sf %>% 
  dplyr::rename(solution_1 = SelectionAll) %>% 
  dplyr::mutate(solution_1 = as.logical(solution_1))

#' Plot the spatial design
(ggLowRegret_All <- fSpatPlan_PlotSolution(LowRegret_sf, PUs, land) + ggtitle("Low-Regret Areas", subtitle = "Across all approaches"))

#' Check the summary
df <- cbind(LowRegret_sf, cost)
summary_lr <- compute_summary(df, total_area, PU_size, run_name = "LowRegret_All", Cost = "Cost_squish")
print(summary_lr)