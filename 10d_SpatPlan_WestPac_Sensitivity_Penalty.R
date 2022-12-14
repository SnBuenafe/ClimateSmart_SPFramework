# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

#### Preliminaries ####
# "Sensitivity analysis"
# Explores using different thresholds
# Note: We can't have thresholds <
# 10d: Penalty approach

# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R") # climate layers are loaded in the script
tos_SSP585 <- load_metrics(metric = "tos", model = "ensemble", scenario = "SSP 5-8.5") # Load climate metric for ens mean

# Load cost layer and squish
FisheriesCost <- read_rds(file.path("Output", paste(save_name, paste0("Cost.rds"), sep = "_"))) %>%
  mutate(Cost_squish = scales::oob_squish(Cost, quantile(Cost, c(0.01, 0.99))))

# 1. Determine scaling
# Dealing with an actual cost layer
median(tos_SSP585$transformed) # 0.03898989
median(UniformCost$cost) # 669.9

scaling <- 17200

create_sensitivity_penaltySols <- function(vec, metric, direction) {
  
  # empty list
  list <- list()
  for(i in 1:length(vec)) {
    scaling <- vec[i] * direction
    
    # 2. Get list of features
    features <- aqua_sf %>% 
      dplyr::as_tibble() %>% 
      dplyr::select(-geometry, -cellID) %>% 
      names()
    
    # 3. Set up the spatial planning problem
    out_sf <- cbind(UniformCost,
                    aqua_sf %>% 
                      tibble::as_tibble() %>% 
                      dplyr::select(-geometry), 
                    metric %>% 
                      tibble::as_tibble() %>% 
                      dplyr::select(-cellID, -geometry)
    )
    p <- prioritizr::problem(out_sf, features, "cost") %>% # Using an actual cost layer
      add_min_set_objective() %>%
      add_relative_targets(0.3) %>% # using 30% targets
      add_binary_decisions() %>%
      add_cbc_solver(gap = 0.2, verbose = FALSE) %>%  # 20% optimality gap
      add_linear_penalties(scaling, data = "transformed")
    
    # 4. Solve the planning problem 
    list[[i]] <- prioritizr::solve(p) %>% 
      sf::st_drop_geometry() %>% 
      dplyr::select(cellID, solution_1) %>% 
      dplyr::rename(!!sym(paste0("sol_", vec[i])) := solution_1)
    
    gc() # Free up space
    print(paste0("Finished: ", vec[i]))
  }
  
  sol_df <- plyr::join_all(list, by = "cellID", type = "left")
  
  return(sol_df)
}

vec <- 10 * 10^(seq(0, 5, 1)) # using 5 as multiplier
feat_df <- create_sensitivity_penaltySols(vec,
                                          tos_SSP585,
                                          1 # Penalize higher values
                                          )

#### Prepare the data.frame for plots ####
# Merge the data.frame with the metric data
df <- feat_df %>%  
  dplyr::left_join(., tos_SSP585 %>% 
                     dplyr::select(transformed, cellID)) %>% 
  sf::st_as_sf(sf_column_name = "geometry") %>% 
  dplyr::bind_cols(., UniformCost %>% 
                     sf::st_drop_geometry() %>% 
                     dplyr::select(cost))

area <- df %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(starts_with("sol_")) %>% 
  colSums() %>% 
  unname()

warm <- list()
for(i in 1:length(vec)) {
  tmp <- df %>% 
    sf::st_drop_geometry() %>% 
    dplyr::filter(!!sym(paste0("sol_", vec[i])) == 1) %>% 
    dplyr::select(transformed) %>% 
    colSums()
  
  warm[i] <- tmp[["transformed"]]
}
warm %<>% unlist()

cost <- list()
for(i in 1:length(vec)) {
  tmp <- df %>% 
    sf::st_drop_geometry() %>% 
    dplyr::filter(!!sym(paste0("sol_", vec[i])) == 1) %>% 
    dplyr::select(cost) %>% 
    colSums()
  
  cost[i] <- tmp[["cost"]]
}
cost %<>% unlist()

tmp_df <- cbind(vec, area = area*100/nrow(PUs), warm, cost) %>% 
  tibble::as_tibble()

# Plot total penalty vs total cost
ggSens <- fPlot_SensitivityPenalty(tmp_df)
ggsave(filename = "Sensitivity-Penalty.png",
       plot = ggSens, 
       width = 20, height = 12, dpi = 300,
       path = "Figures/") # save plot

#### Plotting Kernel Density Plots of small, medium, and large thresholds ####
# Plotting the KD plots of percentile thresholds = 30, 50, 70
sol_10 <- df %>% 
  dplyr::select(cellID, sol_10, transformed, geometry) %>% 
  dplyr::rename(solution_1 = sol_10)
sol_1000 <- df %>% 
  dplyr::select(cellID, sol_1000, transformed, geometry) %>% 
  dplyr::rename(solution_1 = sol_1000)
sol_1000000 <- df %>% 
  dplyr::select(cellID, `sol_1e+06`, transformed, geometry) %>% 
  dplyr::rename(solution_1 = `sol_1e+06`)

solution_list <- list(sol_10, sol_1000, sol_1000000)

list <- list() # empty list
group_name = "threshold"
threshold_list = c("1", "2", "3")
for(i in 1:length(threshold_list)) {
  list[[i]] <- make_kernel(solution_list[[i]], threshold_list[i], group_name)
}
df <- do.call(rbind, list) %>% 
  dplyr::rename(approach = threshold)

climate <- fGetClimateSummary(solution_list, 
                              tos_SSP585, 
                              "tos", 
                              col_scenario = "585", 
                              col_approach = threshold_list, 
                              col_run = threshold_list, 
                              climateLayer = "single") %>% 
  dplyr::select(approach, mean_tos) %>% 
  dplyr::mutate(approach = row_number())

ggRidge <- fPlot_RidgeClimateSensitivity(df, climate)
ggsave(filename = "ClimateSmartRidge-Sensitivity-Penalty.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# Calculate the mean of the non-selected-planning units
notSelectedClimate <- calculate_meanClimateNotSelected(solution_list, threshold_list) %>% 
  dplyr::rename(mean_tos = mean)

ggRidge <- fPlot_RidgeClimateSensitivity(df, notSelectedClimate)
ggsave(filename = "ClimateSmartRidge-Sensitivity-Penalty-NotSelected.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot



