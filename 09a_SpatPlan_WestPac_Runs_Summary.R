# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

#### Preliminaries ####
# Explores performance of different approaches across metrics
# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R") # climate layers are loaded in the script

######################################
###### CLIMATE-SMART PERFORMANCE #####
######################################
names <-  c("Penalty", "Climate Priority Area", "Feature", "Percentile")
group_name = "approach"

# ----- A. Ocean acidification -----
# Load the solutions for phos
s3 <- readRDS(paste0(solutions_dir, "s3-EM-Percentile-phos-585.rds"))
s7 <- readRDS(paste0(solutions_dir, "s7-EM-Feature-phos-585.rds"))
s11 <- readRDS(paste0(solutions_dir, "s11-EM-Penalty-phos-585.rds"))
s35 <- readRDS(paste0(solutions_dir, "s35-EM-ClimatePriorityArea-phos-585.rds"))
solution_list <- list(s11, s35, s7, s3)

list <- list() # empty list
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list) %>% 
  dplyr::mutate(approach = fct_relevel(approach, names))

climate <- load_summary("phos", "mean_phos")

ggRidge <- fPlot_RidgeAcidificationApproach(df, climate)

ggsave(filename = "ClimateSmartRidge-OceanAcidification.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# Calculate the mean of the non-selected-planning units
notSelectedClimate <- calculate_meanClimateNotSelected(solution_list, names) %>% 
  dplyr::rename(mean_phos = mean)

ggRidge <- fPlot_RidgeAcidificationApproach(df, notSelectedClimate)

ggsave(filename = "ClimateSmartRidge-OceanAcidification-NotSelected.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# ----- B. Ocean Deoxygenation -----
# Load solutions
s4 <- readRDS(paste0(solutions_dir, "s4-EM-Percentile-o2os-585.rds"))
s8 <- readRDS(paste0(solutions_dir, "s8-EM-Feature-o2os-585.rds"))
s12 <- readRDS(paste0(solutions_dir, "s12-EM-Penalty-o2os-585.rds"))
s36 <- readRDS(paste0(solutions_dir, "s36-EM-ClimatePriorityArea-o2os-585.rds"))
solution_list <- list(s12, s36, s8, s4)

list <- list() # empty list
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list) %>% 
  dplyr::mutate(approach = fct_relevel(approach, rev))

climate <- load_summary("o2os", "mean_o2os")

ggRidge <- fPlot_RidgeDeoxygenationApproach(df, climate)

ggsave(filename = "ClimateSmartRidge-OceanDeoxygenation.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# Calculate the mean of the non-selected-planning units
notSelectedClimate <- calculate_meanClimateNotSelected(solution_list, names) %>% 
  dplyr::rename(mean_o2os = mean)

ggRidge <- fPlot_RidgeDeoxygenationApproach(df, notSelectedClimate)

ggsave(filename = "ClimateSmartRidge-OceanDeoxygenation-NotSelected.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# ----- C. Climate velocity -----
# Load solutions
s5 <- readRDS(paste0(solutions_dir, "s5-EM-Percentile-velocity-585.rds"))
s9 <- readRDS(paste0(solutions_dir, "s9-EM-Feature-velocity-585.rds"))
s13 <- readRDS(paste0(solutions_dir, "s13-EM-Penalty-velocity-585.rds"))
s37 <- readRDS(paste0(solutions_dir, "s37-EM-ClimatePriorityArea-velocity-585.rds"))
solution_list <- list(s13, s37, s9, s5)

list <- list() # empty list
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list) %>% 
  dplyr::mutate(approach = fct_relevel(approach, rev))

climate <- load_summary("velocity", "median_velocity")

ggRidge <- fPlot_RidgeVelocityApproach(df, climate)

ggsave(filename = "ClimateSmartRidge-Velocity.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# ----- D. MHW intensity -----
# Load solutions
s290 <- readRDS(paste0(solutions_dir, "s290-EM-Percentile-MHW-585.rds"))
s291 <- readRDS(paste0(solutions_dir, "s291-EM-Feature-MHW-585.rds"))
s292 <- readRDS(paste0(solutions_dir, "s292-EM-Penalty-MHW-585.rds"))
s293 <- readRDS(paste0(solutions_dir, "s293-EM-ClimatePriorityArea-MHW-585.rds"))
solution_list <- list(s292, s293, s291, s290)

list <- list() # empty list
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list) %>% 
  dplyr::mutate(approach = fct_relevel(approach, names))

climate <- load_summary("MHW", "mean_MHW")

ggRidge <- fPlot_RidgeMHWApproach(df, climate)
ggsave(filename = "ClimateSmartRidge-MHW.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# ----- E. Combined Metric -----
# Load solutions
s362 <- readRDS(paste0(solutions_dir, "s362-EM-Percentile-CombinedMetric-585.rds"))
s363 <- readRDS(paste0(solutions_dir, "s363-EM-Feature-CombinedMetric-585.rds"))
s364 <- readRDS(paste0(solutions_dir, "s364-EM-Penalty-CombinedMetric-585.rds"))
s365 <- readRDS(paste0(solutions_dir, "s365-EM-ClimatePriorityArea-CombinedMetric-585.rds"))
solution_list <- list(s364, s365, s363, s362)

list <- list() # empty list
for(i in 1:length(names)) {
  list[[i]] <- make_kernel(solution_list[[i]], names[i], group_name)
}
df <- do.call(rbind, list) %>% 
  dplyr::mutate(approach = fct_relevel(approach, names))

climate <- load_summary("CombinedMetric", "mean_CombinedMetric")

ggRidge <- fPlot_RidgeCombinedMetricApproach(df, climate)
ggsave(filename = "ClimateSmartRidge-CombinedMetric.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

##########################################
###### EFFICIENCY IN MEETING TARGETS #####
##########################################
# ----- A. Ocean acidification -----
x <- load_featrep("phos") %>% 
  dplyr::select(feature, contains("phos")) %>% 
  tidyr::pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- fPlot_RidgeTargetApproach(x)

ggsave(filename = "TargetRidge-OceanAcidification.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# ----- B. Ocean deoxygenation -----
x <- load_featrep("o2os") %>% 
  dplyr::select(feature, contains("o2os")) %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- fPlot_RidgeTargetApproach(x)

ggsave(filename = "TargetRidge-OceanDeoxygenation.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# ----- C. Climate velocity -----
x <- load_featrep("velocity") %>% 
  dplyr::select(feature, contains("velocity")) %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- fPlot_RidgeTargetApproach(x)

ggsave(filename = "TargetRidge-Velocity.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# ----- D. MHW intensity -----
x <- load_featrep("MHW") %>% 
  dplyr::select(feature, contains("MHW")) %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- fPlot_RidgeTargetApproach(x)

ggsave(filename = "TargetRidge-MHW.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot

# ----- E. Combined Metric -----
x <- load_featrep("CombinedMetric") %>% 
  dplyr::select(feature, contains("CombinedMetric")) %>% 
  pivot_longer(!feature, names_to = "approach", values_to = "percent") %>% 
  dplyr::mutate(row_number = row_number(feature))

ggRidge <- fPlot_RidgeTargetApproach(x)

ggsave(filename = "TargetRidge-CombinedMetric.png",
       plot = ggRidge, width = 12, height = 8, dpi = 300,
       path = "Figures/") # save plot
