output_calibration <- "Output/calibration/"

scaling_PenaltyWarming <- create_Scaling(UniformCost$cost, roc_tos_SSP585$transformed, "tos")

prelim_lower <- -1.2
prelim_upper <- 4.2
prelim_penalty <- round(10^seq(prelim_lower, prelim_upper, length.out = 9), 5)

# print penalty value
print(prelim_penalty)

# Prepare problem
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()
out_sf <- cbind(aqua_sf, roc_tos_SSP585, cost)

list <- list() # empty list
for(i in 1:length(prelim_penalty)){
  scaling <- prelim_penalty[i]
  
  p <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.3) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 0.1, verbose = FALSE) %>% 
    add_linear_penalties(scaling, "transformed")
  
  s <- solve(p)
  list[[i]] <- s %>% dplyr::select(solution_1)
  saveRDS(s, paste0(output_calibration, "s", i, "-PrelimPenalty.rds")) # save solution
}

# Load solutions
solutions <- lapply(seq(from = 1, to = length(prelim_penalty), by = 1), function(x){
  sol <- readRDS(paste0(output_calibration, "s", x, "-PrelimPenalty.rds")) # read solution
})

df <- list()
for(i in 1:length(prelim_penalty)) {
  
  df[[i]] <- st_join(solutions[[i]], cost) %>% 
    st_join(., roc_tos_SSP585) %>% 
    as_tibble() %>% 
    dplyr::filter(solution_1 == 1) %>% 
    dplyr::summarize(total_cost = sum(Cost_squish), total_penalty = sum(transformed)) %>% 
    dplyr::mutate(run = paste0("Scaling ", prelim_penalty[i]))
  
}

summary <- do.call(rbind, df)
write_csv(summary, paste0(output_calibration, "preliminary_summary.csv"))

ggplot(data = summary)+
  geom_line(aes(x = total_penalty, y = total_cost))

# make the upper limit when the spatial plan looks just like a plan with the penalties as the cost?
p <- prioritizr::problem(out_sf, features, "transformed") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

sol_Cost <- prioritizr::solve(p)
saveRDS(sol_Cost, paste0(output_calibration, "sCost-PrelimPenalty.rds"))

ggCost <- ggplot() + geom_sf(data = sol_Cost, aes(fill = as.logical(solution_1)), size = 0.01)
ggCost
ggPlot9 <- ggplot() + geom_sf(data = solutions[[9]], aes(fill = as.logical(solution_1)), size = 0.01)
ggPlot8 <- ggplot() + geom_sf(data = solutions[[8]], aes(fill = as.logical(solution_1)), size = 0.01)
ggPlot7 <- ggplot() + geom_sf(data = solutions[[7]], aes(fill = as.logical(solution_1)), size = 0.01)
ggPlot7 + ggPlot8 + ggCost + ggPlot9 + plot_layout(guides = "collect")

solution_list <- list(solutions[[5]], solutions[[6]], solutions[[7]], solutions[[8]], solutions[[9]], sol_Cost)
names <- c("Scaling_5", "Scaling_6", "Scaling_7", "Scaling_8", "Scaling_9", "Cost")
object_list <- list() # empty list
for (i in 1:length(names)) {
  obj <- select_solution(solution_list[[i]], names[i])
  object_list[[i]] <- obj
}

# manually save corrplot
(matrix <- create_corrmatrix(object_list) %>% 
    plot_corrplot(., length(object_list)))

# define a new set of penalty values
## note that we use a linear scale to explore both low and high penalty values
penalty <- round(seq(1e-5, prelim_penalty[8], length.out = 9), 5)

list <- list() # empty list
for(i in 1:length(penalty)){
  scaling <- penalty[i]
  
  p <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.3) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
    add_linear_penalties(scaling, "transformed")
  
  s <- solve(p)
  list[[i]] <- s %>% dplyr::select(solution_1)
  saveRDS(s, paste0(output_calibration, "s", i, "-Penalty.rds")) # save solution
}

# Load solutions
solutions <- lapply(seq(from = 1, to = length(penalty), by = 1), function(x){
  sol <- readRDS(paste0(output_calibration, "s", x, "-Penalty.rds")) # read solution
})

df <- list()
for(i in 1:length(penalty)) {
  
  df[[i]] <- st_join(solutions[[i]], cost) %>% 
    st_join(., roc_tos_SSP585) %>% 
    as_tibble() %>% 
    dplyr::filter(solution_1 == 1) %>% 
    dplyr::summarize(total_cost = sum(Cost_squish), total_penalty = sum(transformed)) %>% 
    dplyr::mutate(run = paste0("Scaling ", penalty[i]))
  
}

summary <- do.call(rbind, df)
write_csv(summary, paste0(output_calibration, "penalty_summary.csv"))

# visual 
ggplot(data = summary, aes(x = total_penalty, y = total_cost, label = run))+
  geom_line() +
  geom_point(size = 3) +
  geom_text(size = 3, vjust = -1)

summary %<>% dplyr::mutate(method = case_when(run == "Scaling 837.4136" ~ "visual",
                                              TRUE ~ "none"))

# TOPSIS
topsis_results <- topsis(
  decision =
    summary %>%
    dplyr::select(total_cost, total_penalty) %>%
    as.matrix(),
  weights = c(1, 1),
  impacts = c("-", "-")
)

# add column indicating prioritization selected by TOPSIS method
summary$method[which.max(topsis_results$score)] <- "TOPSIS"

# Cohon method
# generate ideal prioritization based on cost criteria
p <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
sCost <- solve(p)
saveRDS(sCost, paste0(output_calibration, "solCost.rds"))
# generate ideal prioritization based on penalties, cost should be zeroes
cost %<>% dplyr::mutate(zeros = 0)
out_sf <- cbind(aqua_sf, roc_tos_SSP585, cost)

p <- prioritizr::problem(out_sf, features, "zeros") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(1, "transformed")

sPenalty <- solve(p)
saveRDS(sPenalty, paste0(output_calibration, "solPenalty.rds"))

# calculate performance metrics for ideal cost prioritization
s_Cost <- tibble(
  total_cost = sum(sCost %>% as_tibble() %>% dplyr::filter(solution_1 == 1) %>% dplyr::select(Cost_squish)),
  total_penalty = sum(sCost %>% as_tibble() %>% dplyr::filter(solution_1 == 1) %>% dplyr::select(transformed))
)
# calculate performance metrics for ideal penalty prioritization
s_Penalty <- tibble(
  total_cost = sum(sPenalty %>% as_tibble() %>% dplyr::filter(solution_1 == 1) %>% dplyr::select(Cost_squish)),
  total_penalty = sum(sPenalty %>% as_tibble() %>% dplyr::filter(solution_1 == 1) %>% dplyr::select(transformed))
)

# calculate penalty value based on Cohon et al. 1979
cohon_penalty <- abs(
  (s_Cost$total_cost - s_Penalty$total_cost) /
    (s_Cost$total_penalty - s_Penalty$total_penalty)
)

# round to 5 decimal places to avoid numerical issues during optimization
cohon_penalty <- round(cohon_penalty, 5)

# print penalty value
print(cohon_penalty)

# Create solution using cohon penalty
p <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.3) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE) %>% 
  add_linear_penalties(cohon_penalty, "transformed")

sCohon <- solve(p)
saveRDS(sCohon, paste0(output_calibration, "solCohon.rds"))

summary <- bind_rows(
  summary,
  tibble(
    total_cost = sum(sCohon %>% as_tibble() %>% dplyr::filter(solution_1 == 1) %>% dplyr::select(Cost_squish)),
    total_penalty = sum(sCohon %>% as_tibble() %>% dplyr::filter(solution_1 == 1) %>% dplyr::select(transformed)),
    run = paste0("Scaling ", cohon_penalty),
    method = "Cohon"
  )
)

# removing cohon because it seems unreasonable to have it
summary %<>% dplyr::filter(method != "Cohon")

calibrating_plot <- ggplot(data = summary, aes(x = total_penalty, y = total_cost, label = run))+
  geom_line() +
  geom_point(aes(color = method), size = 3) +
  geom_text(aes(color = method), size = 3, vjust = -1) +
  scale_color_manual(
    name = "Method",
    values = c(
      "visual" = "#984ea3",
      "none" = "#000000",
      "TOPSIS" = "#e41a1c",
      "Cohon" = "#377eb8"
    )
  )
ggsave(filename = paste0(output_calibration, "ScalingPlot.png"), plot = calibrating_plot,
       width = 10, height = 10, dpi = 300)
