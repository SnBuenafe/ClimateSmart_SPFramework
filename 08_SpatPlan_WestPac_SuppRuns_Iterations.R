# title: "Running all the different iterations exploring all the different options for all themes"
# author: "Tin Buenafe and Sandra Neubert"

#### Preliminaries ####
# Description
# This code builds upon what has already been done in scripts `04-06_SpatPlan_WestPacific_Runs_` where all the different themes are explored under a microscope.
# Here we run different iterations using the same base approaches.
# Because runs use different iterations, we decided to create loops in this script as opposed to what has been done in `04-06_SpatPlan_WestPacific_Runs_` where we looked to show the exact ways the spatial planning problems were set up and how they were solved
# WARNING: Loops take up a significant amount of memory, so it is not advisable to run them all at the same time (especially the "Climate Priority Area" approach). For weaker machines (< 32gb RAM), it may be best to run each iteration separately.

# Possible values for the arguments of loop functions
# metric_list:
# 1. "tos" (climate warming)
# 2. "phos" (ocean acidification)
# 3. "o2os" (deoxygenation)
# 4. "velocity" (climate velocity)
# 5. "MHW_SumCumInt" (sum of annual cumulative MHW intensity)
# scenario_list
# 1. "126" (SSP1-2.6)
# 2. "245" (SSP2-4.5)
# 3. "585" (SSP5-8.5)
# model_list
# 1. CanESM5, 2. CMCC-ESM2, 3. GFDL-ESM4, 4. IPSL-CM6A-LR, 5. NorESM2-MM
# solution: "s" + a number (please check metadata file: Output/nmds/df_groups.csv)
# setting up multiple solutions
# e.g., solution <- seq(from = 317, to = 322, by = 1)
# solution <- lapply(solution, function(x) {
# y <- paste0("s", x)
# }) %>% unlist()

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
metric_list <- c("MHW_SumCumInt")
scenario_list <- c("126", "245")

solution <- c("s299", "s303") # solution names, check metadata
loopthrough_EM_Percentile(solution, metric_list, scenario_list)

# ----- Feature approach -----
metric_list <- c("phos")
scenario_list <- c("126", "245")

solution <- c("s48", "s49") # solution names, check metadata
loopthrough_EM_Feature(solution, metric_list, scenario_list)

# ----- Penalty approach -----
metric_list <- c("MHW_SumCumInt")
scenario_list <- c("126", "245")

solution <- c("s301", "s305") # solution names, check metadata
loopthrough_EM_Penalty(solution, metric_list, scenario_list)

# ----- Climate priority area approach -----
metric_list <- c("MHW_SumCumInt")
scenario_list <- c("126", "245")

solution <- c("s302", "s306") # solution names, check metadata
loopthrough_EM_ClimatePriorityArea(solution, metric_list, scenario_list)

#### Multi-model ensemble approach ####
# ----- Percentile approach -----
# loop through the metric, scenario, and model lists
metric_list <- c("MHW_SumCumInt")
scenario_list <- c("126", "245")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

solution <- c(seq(from = 322, to = 326, by = 1), seq(from = 342, to = 346, by = 1)) # solution names, check metadata
solution <-lapply(solution, function(x) {
  y <- paste0("s", x)
}) %>% unlist()

loopthrough_MM_Percentile(solution, metric_list, scenario_list, model_list)

# ----- Penalty approach runs -----
# loop through the metric, scenario, and model lists
metric_list <- c("MHW_SumCumInt")
scenario_list <- c("585", "126", "245")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

solution <- c(seq(from = 312, to = 316, by = 1), seq(from = 332, to = 336, by = 1), seq(from = 352, to = 356, by = 1)) # solution names, check metadata
solution <-lapply(solution, function(x) {
  y <- paste0("s", x)
}) %>% unlist()

loopthrough_MM_Penalty(solution, metric_list, scenario_list, model_list)

# ----- Feature approach runs -----
# loop through the metric, scenario, and model lists
metric_list <- c("tos")
scenario_list <- c("126", "245" ,"585")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

solution <- c(seq(from = 142, to = 146, by = 1), seq(from = 162, to = 166, by = 1), seq(from = 202, to = 206, by = 1)) # solution names, check metadata
solution <-lapply(solution, function(x) {
  y <- paste0("s", x)
}) %>% unlist()

loopthrough_MM_Feature(solution, metric_list, scenario_list, model_list)


# ----- Climate priority area approach runs -----
# loop through the metric, scenario, and model lists
metric_list <- c("MHW_SumCumInt")
scenario_list <- c("245")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

solution <- seq(from = 357, to = 361, by = 1) # solution names, check metadata
solution <-lapply(solution, function(x) {
  y <- paste0("s", x)
}) %>% unlist()

loopthrough_MM_ClimatePriorityArea(solution, metric_list, scenario_list, model_list)
