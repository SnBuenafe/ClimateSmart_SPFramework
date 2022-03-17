Limits <- c(xmin = 60, xmax = 70, ymin = 0, ymax = 10)
Boundary <- fSpatPlan_Get_Boundary(Limits, longlat) %>% 
  st_make_grid(., 
               square = TRUE,
               cellsize = c(1, 1),
               what = "polygons") %>% 
  st_sf() %>% 
  dplyr::mutate(cellID = row_number(), cost = 1)

ggplot(data = Boundary) +
  geom_sf(aes(fill = cellID)) +
  theme_classic()

#### Two species ####
#### All pixels are filled ####
test_FullArea <- function(OverlappingPixels) {
  
  df <- list()
  
  for(i in 1:length(OverlappingPixels)) {
    x = 50 - OverlappingPixels[i]
    Test <- Boundary %>% dplyr::mutate(sp1 = case_when(cellID <= 50 ~ 1,
                                                       cellID > 50 ~ 0),
                                       sp2 = case_when(cellID <= x ~ 0,
                                                       cellID > x ~ 1))
    features <- c("sp1", "sp2")
    
    problem <- prioritizr::problem(Test, features, "cost") %>%
      add_min_set_objective() %>%
      add_relative_targets(0.5) %>%
      add_binary_decisions() %>%
      add_gurobi_solver(gap = 0, verbose = FALSE)
    solution <- solve(problem)
    
    area <- eval_cost_summary(problem, solution %>% dplyr::select(solution_1))$cost /100
    #print(paste0("Area Solution: ", area*100))
    df[[i]] <- eval_feature_representation_summary(problem, solution[, 'solution_1']) %>% 
      dplyr::mutate(area_solution = area*100) %>% 
      dplyr::mutate(overlapping = OverlappingPixels[i])
  }
  
  final_df <- do.call(rbind, df)
  
  return(final_df)
}

sequence <- seq(from = 0, to = 50, by = 5)
Test <- test_FullArea(sequence)

features <- c("sp1", "sp2")

x = 50 - 25
Test_df <- Boundary %>% dplyr::mutate(sp1 = case_when(cellID <= 50 ~ 1,
                                                   cellID > 50 ~ 0),
                                   sp2 = case_when(cellID <= x ~ 0,
                                                   cellID > x ~ 1))
sp1 <- ggplot() + geom_sf(data = Test_df, aes(fill = as.factor(sp1))) + 
  scale_fill_manual(values = c("0" = "skyblue1", "1" = "skyblue4")) +
  ggtitle("Species # 1")
sp2 <- ggplot() + geom_sf(data = Test_df, aes(fill = as.factor(sp2))) + 
  scale_fill_manual(values = c("0" = "skyblue1", "1" = "skyblue4")) +
  ggtitle("Species # 2")

problem <- prioritizr::problem(Test_df, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.5) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
solution <- solve(problem) %>% 
  dplyr::mutate(solution_1 = as.logical(solution_1))

solution_plot <- ggplot() + geom_sf(data = solution, aes(fill = solution_1)) +
  scale_fill_manual(values = c("FALSE" = "skyblue1", "TRUE" = "skyblue4"))

(sp1 + sp2) / (solution_plot + plot_spacer())

plot1 <- ggplot(data = Test %>% filter(feature == "sp2"), aes(x = overlapping)) +
  geom_line(aes(y = area_solution), color = "indianred4", size = 1) +
  ylim(0, 100) +
  theme_classic()
plot2 <- ggplot(data = Test %>% filter(feature == "sp2"), aes(x = overlapping)) +
  geom_line(aes(y = total_amount), color = "turquoise3", size = 1) +
  ylim(0, 100) +
  theme_classic()
plot1 + plot2

test_FullAreaNew <- function(OverlappingPixels) {
  
  df <- list()
  
  for(i in 1:length(OverlappingPixels)) {
  Test <- Boundary %>% dplyr::mutate(sp1 = case_when(cellID <= 50 ~ 1,
                                                     cellID > 50 ~ 0),
                                     sp2 = case_when(cellID <= OverlappingPixels[i] ~ 1,
                                                     cellID > OverlappingPixels[i] ~ 0))
  features <- c("sp1", "sp2")
  
  problem <- prioritizr::problem(Test, features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.5) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 0, verbose = FALSE)
  solution <- solve(problem)
  
  area <- eval_cost_summary(problem, solution %>% dplyr::select(solution_1))$cost /100
  #print(paste0("Area Solution: ", area*100))
  df[[i]] <- eval_feature_representation_summary(problem, solution[, 'solution_1']) %>% 
    dplyr::mutate(area_solution = area*100) %>% 
    dplyr::mutate(overlapping = OverlappingPixels[i])
  }
  
  final_df <- do.call(rbind, df)
  
  return(final_df)
}

sequence <- seq(from = 50, to = 5, by = -5)
Test1 <- test_FullAreaNew(sequence)

plot1 <- ggplot(data = Test1 %>% filter(feature == "sp2"), aes(x = overlapping)) +
  geom_line(aes(y = area_solution), color = "indianred4", size = 1) +
  ylim(0, 100) +
  theme_classic()

plot2 <- ggplot(data = Test1 %>% filter(feature == "sp2"), aes(x = overlapping)) +
  geom_line(aes(y = total_amount), color = "turquoise3", size = 1) +
  ylim(0, 100) +
  theme_classic()
  
# Get line plots
plot1 + plot2

Test_df <- Boundary %>% dplyr::mutate(sp1 = case_when(cellID <= 50 ~ 1,
                                                   cellID > 50 ~ 0),
                                   sp2 = case_when(cellID <= 30 ~ 1,
                                                   cellID > 30 ~ 0))
features <- c("sp1", "sp2")
sp1 <- ggplot() + geom_sf(data = Test_df, aes(fill = as.factor(sp1))) + 
  scale_fill_manual(values = c("0" = "skyblue1", "1" = "skyblue4")) +
  ggtitle("Species # 1")
sp2 <- ggplot() + geom_sf(data = Test_df, aes(fill = as.factor(sp2))) + 
  scale_fill_manual(values = c("0" = "skyblue1", "1" = "skyblue4")) +
  ggtitle("Species # 2")

problem <- prioritizr::problem(Test_df, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.5) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)
solution <- solve(problem) %>% 
  dplyr::mutate(solution_1 = as.logical(solution_1))

solution_plot <- ggplot() + geom_sf(data = solution, aes(fill = solution_1)) +
  scale_fill_manual(values = c("FALSE" = "skyblue1", "TRUE" = "skyblue4"))

(sp1 + sp2) / (solution_plot + plot_spacer())
