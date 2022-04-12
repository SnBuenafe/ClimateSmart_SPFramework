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

#### Both species have 50 cells each ####

species2 <- tibble(
  from = seq(from = 1, to = 51, by = 1),
  to = seq(from = 50, to = 100, by = 1)
)

test_problems <- function(x){
  sequence <- seq(from = species2$from[x], to = species2$to[x], by = 1)
  
  df <- Boundary %>% dplyr::mutate(sp1 = case_when(cellID <= 50 ~ 1,
                                                   cellID > 50 ~ 0)) %>% 
    dplyr::mutate(sp2 = ifelse(cellID %in% sequence, yes = 1, no = 0))
  
  features <- c("sp1", "sp2")
  
  p <- prioritizr::problem(df, features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.4) %>%
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 0, verbose = FALSE)
  
  s <- solve(p) %>% 
    dplyr::mutate(solution_1 = as.logical(solution_1))
  
  summary <- tibble(
    total_cost = sum(s %>% as_tibble() %>% dplyr::filter(solution_1 == 1) %>% dplyr::select(cost)),
    total_sp1 = sum(s %>% as_tibble() %>% dplyr::filter(solution_1 == 1) %>% dplyr::select(sp1)),
    total_sp2 = sum(s %>% as_tibble() %>% dplyr::filter(solution_1 == 1) %>% dplyr::select(sp2)),
    overlapping_cells = 50-(x-1)
  )
}

summary <- lapply(seq(from = 1, to = nrow(species2), by = 1), test_problems) %>% 
  do.call(bind_rows, .)

test50 <- ggplot(data = summary, aes(x = overlapping_cells, y = total_cost)) +
  geom_line() +
  geom_point()

test50x <- ggplot(data = summary, aes(x = total_sp2, y = total_cost)) +
  geom_line() +
  geom_point()

ggsave(plot = test50, filename = "Output/temp/test_results.png", height = 10, width = 10, dpi = 600)

#### Percentile approach ####

# Remove 50% all the time.
climateSmart <- seq(from = 25, by = 1, length = 51)

species2 <- cbind(species2, climateSmart)

test_problems <- function(x){
  sequence <- seq(from = species2$from[x], to = species2$to[x], by = 1)
  
  df <- Boundary %>% dplyr::mutate(sp1 = case_when(cellID <= 50 ~ 1,
                                                   cellID > 50 ~ 0)) %>% 
    dplyr::mutate(sp1_ClimateSmart = case_when(cellID <= 25 ~ 1,
                                               cellID > 25 ~ 0)) %>% 
    dplyr::mutate(sp2 = ifelse(cellID %in% sequence, yes = 1, no = 0)) %>% 
    dplyr::mutate(sp2_ClimateSmart = case_when((sp2 == 1 & cellID <= species2$climateSmart[x]) ~ 1,
                                               TRUE ~ 0))
  
  features <- c("sp1_ClimateSmart", "sp2_ClimateSmart")
  
  p <- prioritizr::problem(df, features, "cost") %>%
    add_min_set_objective() %>%
    add_relative_targets(0.8) %>% # 40% protection, 50th percentile
    add_binary_decisions() %>%
    add_gurobi_solver(gap = 0, verbose = FALSE)
  
  s <- solve(p) #%>% 
   # dplyr::mutate(solution_1 = as.logical(solution_1))
  
  summary <- tibble(
    total_cost = sum(s %>% as_tibble() %>% dplyr::filter(solution_1 == 1) %>% dplyr::select(cost)),
    total_sp1 = sum(s %>% as_tibble() %>% dplyr::filter(solution_1 == 1) %>% dplyr::select(sp1)),
    total_sp2 = sum(s %>% as_tibble() %>% dplyr::filter(solution_1 == 1) %>% dplyr::select(sp2)),
    overlapping_cells = 25 - species2$from[x] + 1
  ) %>% dplyr::mutate(overlapping_cells = ifelse(overlapping_cells < 0, yes = 0, no = overlapping_cells))
}

summary <- lapply(seq(from = 1, to = nrow(species2), by = 1), test_problems) %>% 
  do.call(bind_rows, .)

testPercentile <- ggplot(data = summary, aes(x = overlapping_cells, y = total_cost)) +
  geom_line() +
  geom_point()
testPercentilex <- ggplot(data = summary, aes(x = total_sp2, y = total_cost)) +
  geom_line() +
  geom_point()

test <- test50 + testPercentile
test50x + testPercentilex
ggsave(plot = test, filename = "Output/temp/test_results.png", height = 7, width = 12, dpi = 600)

# Example
x = 16
sequence <- seq(from = species2$from[x], to = species2$to[x], by = 1)

df <- Boundary %>% dplyr::mutate(sp1 = case_when(cellID <= 50 ~ 1,
                                                 cellID > 50 ~ 0)) %>% 
  dplyr::mutate(sp1_ClimateSmart = case_when(cellID <= 25 ~ 1,
                                             cellID > 25 ~ 0)) %>% 
  dplyr::mutate(sp2 = ifelse(cellID %in% sequence, yes = 1, no = 0)) %>% 
  dplyr::mutate(sp2_ClimateSmart = case_when((sp2 == 1 & cellID <= species2$climateSmart[x]) ~ 1,
                                             TRUE ~ 0))

sp1 <- ggplot() + geom_sf(data = df, aes(fill = as.logical(sp1_ClimateSmart)), size = 0.01, show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "black",
                               "FALSE" = "grey80")) +
  ggtitle("Species 1")

sp2 <- ggplot() + geom_sf(data = df, aes(fill = as.logical(sp2_ClimateSmart)), size = 0.01, show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "black",
                               "FALSE" = "grey80")) +
  ggtitle("Species 2")

features <- c("sp1_ClimateSmart", "sp2_ClimateSmart")

p <- prioritizr::problem(df, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.8) %>% # 40% protection, 50th percentile
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

s <- solve(p) %>% 
  dplyr::mutate(solution_1 = as.logical(solution_1))

solution <- ggplot() + geom_sf(data = s, aes(fill = as.logical(solution_1)), size = 0.01, show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "black",
                               "FALSE" = "grey80")) +
  ggtitle("Solution")

testPlot <- sp1 + sp2 + solution + plot_spacer() + plot_annotation(title = "10 Overlapping Cells")
ggsave(plot = testPlot, filename = "Output/temp/testPlot.png", height = 10, width = 10, dpi = 600)

summary <- tibble(
  total_cost = sum(s %>% as_tibble() %>% dplyr::filter(solution_1 == 1) %>% dplyr::select(cost)),
  total_sp1 = sum(s %>% as_tibble() %>% dplyr::filter(solution_1 == 1) %>% dplyr::select(sp1_ClimateSmart)),
  total_sp2 = sum(s %>% as_tibble() %>% dplyr::filter(solution_1 == 1) %>% dplyr::select(sp2_ClimateSmart)),
  overlapping_cells = 25 - species2$from[x] + 1
) %>% dplyr::mutate(overlapping_cells = ifelse(overlapping_cells < 0, yes = 0, no = overlapping_cells))

df <- Boundary %>% dplyr::mutate(sp1 = case_when(cellID <= 100 ~ 1,
                                                 cellID > 100 ~ 0))

features <- c("sp1")

p <- prioritizr::problem(df, features, "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.01) %>% # 40% protection, 50th percentile
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0.2, verbose = FALSE)

s <- solve(p) %>% 
  dplyr::mutate(solution_1 = as.logical(solution_1))
ggplot() + geom_sf(data = s, aes(fill = as.logical(solution_1)), size = 0.01, show.legend = FALSE) +
  scale_fill_manual(values = c("TRUE" = "black",
                               "FALSE" = "grey80")) +
  ggtitle("Solution")
