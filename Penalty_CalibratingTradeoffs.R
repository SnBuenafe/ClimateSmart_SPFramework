roc_tos_SSP585 <- readRDS("Output/WestPacific_ClimateLayer_roc_tos_SSP 5-8.5.rds")

# Define problem without boundary penalties
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

out_sf <- cbind(aqua_sf, UniformCost, roc_tos_SSP585)
p0 <- prioritizr::problem(out_sf, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>% # using 30% as the target percentage of protection
  add_binary_decisions()

# Using the blended approach
prelim_lower <- min(roc_tos_SSP585$transformed)
prelim_upper <- max(roc_tos_SSP585$transformed)
prelim_penalty <- round(10^seq(prelim_lower, prelim_upper, length.out = 9), 5)

# relaxed gap and time limit for the solver
prelim_blended_results <- lapply(prelim_penalty, function(x){
  s <-
    p0 %>% 
    add_linear_penalties(penalty = prelim_penalty[9], data = "transformed") %>% 
    add_gurobi_solver(gap = 0.2, time_limit = 5 * 60) %>% 
    solve()
  s <- data.frame(s = s$solution_1)
  names(s) <- withr::with_options(list(scipen = 30), paste0("penalty_", prelim_penalty[9]))
  s
})

# format results as a single spatial object
df <- cbind(aqua_sf, UniformCost, prelim_blended_results[[1]], prelim_blended_results[[2]],
            prelim_blended_results[[3]], prelim_blended_results[[4]], prelim_blended_results[[5]],
            prelim_blended_results[[6]], prelim_blended_results[[7]], prelim_blended_results[[8]],
            prelim_blended_results[[9]])

# preview results
print(df)

# plot series of prioritizations
df_mutate <- df %>% 
  dplyr::select(starts_with("penalty_")) %>% 
  mutate_if(is.numeric, function(x){
    case_when(x > 0.5 ~ "priority",
              TRUE ~ "other")
  })

columns <- colnames(df_mutate) %>% 
  head(., -1)

for(i in 1:length(columns)) {
  plot <- ggplot() + 
    geom_sf(data = df_mutate %>% dplyr::select(!!sym(columns[i])), aes_string(fill = columns[i]), size = 0.01) +
    scale_fill_manual(values = c("other" = "grey90", "priority" = "darkgreen")) +
    theme_classic()
  
  assign(x = paste0("plot", i), value = plot)
}

(plot1 + plot2 + plot3) / (plot4 + plot5 + plot6) / (plot7 + plot8 + plot9) +
  plot_layout(guides = "collect")

#? Same solutions

# Use the scalings 20 to 100 from the penalty approach
# Get scaling
scaling_PenaltyWarming <- create_Scaling(UniformCost$cost, roc_tos_SSP585$transformed, "tos")

features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

out_sf <- cbind(aqua_sf, roc_tos_SSP585, UniformCost)

fScale <- function(x) {
  scaling <- scaling_PenaltyWarming %>% filter(scaling = x) %>% pull()
  
  p <- prioritizr::problem(out_sf, features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.3) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 0.1, verbose = FALSE) %>% 
    add_linear_penalties(scaling, data = "transformed")
  
  s <- prioritizr::solve(p) %>% as_tibble() %>% dplyr::select(solution_1)
  
  return(s)
}



penalty200_plot <- solP200 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol200 <- fSpatPlan_PlotSolution(penalty200_plot, PUs, land) + ggtitle("Climate-smart design: Rate of Climate Warming", subtitle = "Penalty, SSP 5-8.5 (200% of Cost)") + theme(axis.text = element_text(size = 25)))

compute_summary(solP200, total_area, PU_size, run_name = "Penalty200-tos-585", Cost = "cost")

get_ClimateSummary(solution_list = list(solP200), climate_layer = roc_tos_SSP585, metric = "tos", col_scenario = "585", col_approach = "penalty", col_run = "Penalty200-tos-585", climateLayer = "single")
ggsave(filename = "EM-Penalty-Scaling200-tos-585.png",
       plot = ggSol200, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save