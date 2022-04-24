# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

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
scenario_list = c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
for(scenario_num in 1:length(scenario_list)) {
  LoadClimateMetrics(metric = "tos", model = NA, scenario = scenario_list[scenario_num])
}
s38 <- readRDS(paste0(output_solutions, "s38-EM-Percentile-tos-126.rds")) # SSP1-2.6 solution
s39 <- readRDS(paste0(output_solutions, "s39-EM-Percentile-tos-245.rds")) # SSP2-4.5 solution
s2 <- readRDS(paste0(output_solutions, "s2-EM-Percentile-tos-585.rds")) # SSP5-8.5 solution
sLR <- readRDS(paste0(output_lowregret, "sFreq1-EM-Percentile-tos.rds")) # SelectionFrequency solution (has low-regret data)

#### Low-regret solution ####
# ----- Plot low-regret solution -----
ggLR <- plot_lowregret(sLR, land) + 
  ggtitle("Layer Theme: Low-Regret", subtitle = "Climate Warming, Percentile") + theme(axis.text = element_text(size = 25))
ggsave(filename = "LR-EM-Percentile-Scenario-tos.png",
       plot = ggLR, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

#### Single solution ####
# 1. Prepare climate layers
# SSP1-2.6
aqua_percentile_SSP126 <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP126, PUs = PUs) %>% 
  dplyr::rename_with(~paste0(., "_SSP126"), -c("geometry"))
# SSP2-4.5
aqua_percentile_SSP245 <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP245, PUs = PUs) %>% 
  dplyr::rename_with(~paste0(., "_SSP245"), -c("geometry"))
# SSP5-8.5
aqua_percentile_SSP585 <- create_PercentileLayer(aqua_sf = aqua_sf, metric_name = "tos", colname = "transformed", metric_df = roc_tos_SSP585, PUs = PUs) %>% 
  dplyr::rename_with(~paste0(., "_SSP585"), -c("geometry"))

# 2. Get list of features
scenario <- c("SSP126", "SSP245", "SSP585")
features <- list()
for(i in 1:length(scenario)) {
  x <- get(paste0("aqua_percentile_", scenario[i]))
  features[[i]] <- x %>% as_tibble() %>% 
    dplyr::select(-geometry)
}
features <- do.call(bind_cols, features) %>% names()

# 3. Set up the spatial planning problem
out_sf <- cbind(aqua_percentile_SSP126, aqua_percentile_SSP245, aqua_percentile_SSP585, roc_tos_SSP126, UniformCost)
p <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(30/35) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
# 4. Solve the planning problem 
s <- prioritizr::solve(p)
saveRDS(s, paste0(output_solutions, "temp_single.rds")) # TODO: save solution
# 5. Plot the spatial design
splot <- s %>% 
  mutate(solution_1 = as.logical(solution_1)) 
ggSol <- fSpatPlan_PlotSolution(splot, PUs, land)# + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Percentile, SSP 1-2.6") + theme(axis.text = element_text(size = 25)))
ggsave(filename = "EM-Percentile-tos-126.png",
       plot = ggSol38, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save plot

# Feature representation
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
  
df <- represent_feature(dummy_problem, s, "Single")

# try the "OR"

### try to make a loop ###
spp <- aqua_sf %>% as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

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

  # Commence Parallel Loop.
  
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
  
  # then for the targets, we need to compare it with the original distribution. But select all instances where the PU is selected as a climate refugia (sel freq > 0)


df_SSP126 <- aqua_percentile_SSP126 %>% 
  dplyr::mutate(cellID = row_number()) %>% 
  as_tibble() %>% 
  dplyr::select(-geometry)

# Commence Parallel Loop.

ncores <- detectCores(logical = FALSE) - 1 
cl <- makeCluster(ncores)
registerDoParallel(cl)

list <- vector("list", length = length(spp)) # create empty list

aq_tmp <- foreach(i = 1:length(spp), .packages = c('tidyverse', 'sf', 'magrittr')) %dopar% {
  
  # Select 1 species at a time
  df <- aqua_sf %>% 
    as_tibble() %>% 
    dplyr::select(!!sym(spp[i])) %>% 
    dplyr::mutate(cellID = row_number())
  
  df2 <- impt_df %>% 
    dplyr::select(!!sym(spp[i]), cellID) %>% 
    dplyr::rename(V1 := !!sym(spp[i]))
  
  CombinedDf <- left_join(df, df2, by = "cellID") %>% 
    dplyr::mutate(!!sym(spp[i]) := ifelse(V1 == 1, yes = 0, no = .data[[ spp[i] ]])) %>% 
    dplyr::select(-V1, -cellID)
}
stopCluster(cl)

aqua_df <- do.call(cbind, aq_tmp) %>% 
  cbind(., PUs) %>% st_as_sf(sf_column_name = "geometry")



# -----