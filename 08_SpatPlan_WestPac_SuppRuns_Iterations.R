# title: "Running all the different iterations exploring all the different options for all themes"
# author: "Tin Buenafe and Sandra Neubert"

#### Preliminaries ####
# Description
# This code builds upon what has already been done in scripts `04-06_SpatPlan_WestPacific_Runs_` where all the different themes are explored under a microscope.
# Here we run different iterations using the same base approaches.
# Because runs use different iterations, we decided to create loops in this script as opposed to what has been done in `04-06_SpatPlan_WestPacific_Runs_` where we looked to show the exact ways the spatial planning problems were set up and how they were solved
# WARNING: Loops take up a significant amount of memory, so it is not advisable to run them all at the same time (especially the "Climate Priority Area" approach). For weaker machines (< 32gb RAM), it may be best to run each iteration separately.

# Load functions
source("HelperFunctions/SpatPlan_Extras.R") # Load the extras, including functions and libraries
source("HelperFunctions/SpatPlan_HelperFxns_WestPac.R") # Load helper functions written specifically for this spatial planning project
source("HelperFunctions/SpatPlan_IterationFxns_WestPac.R") # Load loop functions written specifically for producing iterations
output_solutions <- "Output/solutions/"
output_summary <- "Output/summary/"
output_lowregret <- "Output/lowregret/"

# Load files
source("03_SpatPlan_Master_Preliminaries.R")
total_area = nrow(PUs) * PU_size

#### Ensemble mean approach ####
# ----- Percentile approach -----
# loop through the metric and scenario lists
metric_list <- c("phos", "o2os", "velocity")
scenario_list <- c("126", "245")

solution <- c("s40", "s41", "s42", "s43", "s44", "s45") # solution names, check metadata
loopthrough_EM_Percentile(solution, metric_list, scenario_list)

# ----- Feature approach -----
metric_list <- c("tos", "phos", "o2os", "velocity")
scenario_list <- c("126", "245")

solution <- c("s46", "s47", "s48", "s49", "s50", "s51", "s52", "s53") # solution names, check metadata
loopthrough_EM_Feature(solution, metric_list, scenario_list)

# ----- Penalty approach -----
metric_list <- c("tos", "phos", "o2os", "velocity")
scenario_list <- c("126", "245")

solution <- c("s54", "s55", "s56", "s57", "s58", "s59", "s60", "s61") # solution names, check metadata
loopthrough_EM_Penalty(solution, metric_list, scenario_list)

# ----- Climate priority area approach -----
metric_list <- c("tos", "phos", "o2os", "velocity")
scenario_list <- c("126", "245")

solution <- c("s222", "s223", "s224", "s225", "s226", "s227", "s228", "s228")
loopthrough_EM_ClimatePriorityArea(solution, metric_list, scenario_list)


#### Multi-model ensemble approach ####
# ----- Percentile approach -----
# loop through the metric, scenario, and model lists
metric_list <- c("tos", "phos", "o2os", "velocity")
scenario_list <- c("126", "245")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

solution <- seq(from = 62, to = 101, by = 1) # solution names, check metadata
solution <-lapply(solution, function(x) {
  y <- paste0("s", x)
}) %>% unlist()

loopthrough_MM_Percentile(solution, metric_list, scenario_list, model_list)

# ----- Penalty approach runs -----
# loop through the metric, scenario, and model lists
metric_list <- c("tos", "phos", "o2os", "velocity")
scenario_list <- c("126", "245")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

solution <- seq(from = 102, to = 141, by = 1) # solution names, check metadata
solution <-lapply(solution, function(x) {
  y <- paste0("s", x)
}) %>% unlist()

loopthrough_MM_Penalty(solution, metric_list, scenario_list, model_list)

scenario_list <- c("585")

solution <- seq(from = 182, to = 201, by = 1) # solution names, check metadata
solution <-lapply(solution, function(x) {
  y <- paste0("s", x)
}) %>% unlist()

loopthrough_MM_Penalty(solution, metric_list, scenario_list, model_list)
# ----- Feature approach runs -----
# loop through the metric, scenario, and model lists
metric_list <- c("tos", "phos", "o2os", "velocity")
scenario_list <- c("126", "245")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

solution <- seq(from = 142, to = 181, by = 1) # solution names, check metadata
solution <-lapply(solution, function(x) {
  y <- paste0("s", x)
}) %>% unlist()

loopthrough_MM_Feature(solution, metric_list, scenario_list, model_list)

scenario_list <- c("585")

solution <- seq(from = 202, to = 221, by = 1) # solution names, check metadata
solution <-lapply(solution, function(x) {
  y <- paste0("s", x)
}) %>% unlist()

loopthrough_MM_Feature(solution, metric_list, scenario_list, model_list)
# ----- Climate priority area approach runs -----
# loop through the metric, scenario, and model lists
metric_list <- c("tos", "phos", "o2os", "velocity")
scenario_list <- c("126", "245", "585")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

solution <- seq(from = 222, to = 289, by = 1) # solution names, check metadata
solution <-lapply(solution, function(x) {
  y <- paste0("s", x)
}) %>% unlist()

loopthrough_MM_ClimatePriorityArea(solution, metric_list, scenario_list, model_list)
