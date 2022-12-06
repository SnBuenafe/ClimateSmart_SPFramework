# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

#### Preliminaries ####
# "Sensitivity analysis"
# Explores using different thresholds
# Note: We can't have thresholds <
# 10a: Feature approach

# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R") # climate layers are loaded in the script
tos_SSP585 <- load_metrics(metric = "tos", model = "ensemble", scenario = "SSP 5-8.5") # Load climate metric for ens mean

# TODO: Move this to a helper function once done. Not yet done because it takes up memory.
create_sensitivity_featureSols <- function(vec, metric, direction) {
  
  list <- list() # empty list
  for(i in 1:length(vec)) {
    # 1. Prepare climate layer
    aqua_feature <- fFeature_CSapproach(featuresDF = aqua_sf, 
                                        percentile = vec[i], 
                                        metricDF = rename_metric(metric),
                                        direction = direction
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
    targets <- fAssignTargets_Feature(climateSmartDF = aqua_feature,
                                      refugiaTarget = 30,
                                      targetsDF = target_df)
    
    # 3. Set up the spatial planning problem
    out_sf <- cbind(UniformCost,
                    aqua_feature %>% 
                      tibble::as_tibble() %>% 
                      dplyr::select(-cellID, -geometry), 
                    metric %>% 
                      tibble::as_tibble() %>% 
                      dplyr::select(-cellID, -geometry)
    )
    p <- prioritizr::problem(out_sf, targets$feature, "cost") %>%
      add_min_set_objective() %>%
      add_relative_targets(targets$target) %>% 
      add_binary_decisions() %>%
      add_cbc_solver(gap = 0.2, verbose = FALSE) # Using 20% 
    
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

vec <- seq(30, 70, 5)
feat_df <- create_sensitivity_featureSols(vec,
                                          tos_SSP585,
                                          -1)

# Merge the data.frame with the metric data
df <- feat_df %>%  
  dplyr::left_join(., tos_SSP585 %>% 
                     dplyr::select(transformed, cellID)) %>% 
  sf::st_as_sf(sf_column_name = "geometry")

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
    colMeans()
  
  warm[i] <- tmp[["transformed"]]
}
warm %<>% unlist()

tmp_df <- cbind(vec, area = area*100/nrow(PUs), warm) %>% 
  tibble::as_tibble()

# Plot warming x area
coeff = 10e2
ggplot(tmp_df, aes(x=vec)) +
  
  geom_line(aes(y = area), size = 1, color = "#081d58") + 
  geom_line( aes(y = warm * coeff), size = 1, color = "#ec7014") +

  scale_y_continuous(name = "% of planning region selected",
                     sec.axis = sec_axis(~./coeff, name = expression('Warming (Δ'^"o"*'C yr'^"-1"*')'))
  ) +
  scale_x_continuous(name = "Percentile threshold") +
    geom_point(aes(y = area), size = 3, color = "#081d58", shape = 18) +
  geom_point( aes(y = warm * coeff), size = 3, color = "#ec7014", shape = 18) +
  theme_bw() +
  theme(panel.grid.major = element_line(color = "grey70"),
        panel.grid.minor = element_line(color = "grey80"),
        panel.border = element_rect(colour = "black", fill=NA, size=5),
        axis.ticks = element_line(color = "black", linewidth = 2),
        axis.text = element_text(color = "black", size = 25),
        axis.title.x = element_text(color = "black", size = 30),
        axis.title.y = element_text(color = "#081d58", size = 30),
        axis.title.y.right = element_text(color = "#ec7014", size = 30)
        )

ggsave(filename = "Sensitivity-Feature.png",
      # plot = ggRidge, 
       width = 20, height = 12, dpi = 300,
       path = "Figures/") # save plot

# Plotting the KD plots of percentile thresholds = 30, 50, 70
sol_30 <- df %>% 
  dplyr::select(cellID, sol_30, transformed, geometry) %>% 
  dplyr::rename(solution_1 = sol_30)
sol_50 <- df %>% 
  dplyr::select(cellID, sol_50, transformed, geometry) %>% 
  dplyr::rename(solution_1 = sol_50)
sol_70 <- df %>% 
  dplyr::select(cellID, sol_70, transformed, geometry) %>% 
  dplyr::rename(solution_1 = sol_70)

solution_list <- list(sol_30, sol_50, sol_70)
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
                              col_run = names, 
                              climateLayer = "single") %>% 
  dplyr::select(approach, mean_tos) %>% 
  dplyr::mutate(approach = row_number())

ggRidge <- fPlot_RidgeClimateScenario(df, climate)
ggsave(filename = "ClimateSmartRidge-Sensitivity-Feature-NoInt.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# Calculate the mean of the non-selected-planning units
notSelectedClimate <- calculate_meanClimateNotSelected(solution_list, threshold_list) %>% 
  dplyr::rename(mean_tos = mean)

ggRidge <- fPlot_RidgeClimateScenario(df, notSelectedClimate)
ggsave(filename = "ClimateSmartRidge-Sensitivity-Feature-NotSelected.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot
