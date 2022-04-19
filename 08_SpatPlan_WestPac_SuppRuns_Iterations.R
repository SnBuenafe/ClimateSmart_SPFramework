# title: "Running all the different iterations exploring all the different options for all themes"
# author: "Tin Buenafe and Sandra Neubert"

#### Preliminaries ####
# Description
# This code builds upon what has already been done in scripts `04-06_SpatPlan_WestPacific_Runs_` where all the different themes are explored under a microscope.
# Here we run different iterations using the same base approaches.
# Because runs use different iterations, we decided to create loops in this script as opposed to what has been done in `04-06_SpatPlan_WestPacific_Runs_` where we looked to show the exact ways the spatial planning problems were set up and how they were solved
# WARNING: Loops take up a significant amount of memory, so it is not advisable to run them all at the same time (especially the "Climate Priority Area" approach). For weaker machines (< 32gb RAM), it may be best to run each iteration separately.

# NOTE: Change the ff. parameters before running the loop functions below. Here are their possible values.
# metric_list: tos (climate warming), phos (ocean acidification), o2os (deoxygenation), velocity (climate velocity),
# MHW_num (annual # of MHWs), MHW_CumInt (annual cumulative intensity of MHWs), MHW_CumDur (annual cumulative duration of MHWs)
# scenario_list: 126 (SSP 1-2.6), 245 (SSP 2-4.5), 585 (SSP 5-8.5)
# model_list: CanESM5, CMCC-ESM2, GFDL-ESM4, IPSL-CM6A-LR, NorESM2-MM
# solution: "s" + a number (please check metadata file)


# Load functions
source("HelperFunctions/SpatPlan_Extras.R") # Load the extras, including functions and libraries
source("HelperFunctions/SpatPlan_HelperFxns_WestPac.R") # Load helper functions written specifically for this spatial planning project
source("HelperFunctions/SpatPlan_IterationFxns_WestPac.R") # Load loop functions written specifically for producing iterations
output_solutions <- "Output/solutions/"
output_summary <- "Output/summary/"
output_lowregret <- "Output/lowregret/"

#library(rlang)

#solution <- seq(from = 317, to = 322, by = 1) # solution names, check metadata
#solution <-lapply(solution, function(x) {
#  y <- paste0("s", x)
#}) %>% unlist()

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
metric_list <- c("MHW_SumCumInt")
scenario_list <- c("126", "245")

solution <- c("s300", "s304") # solution names, check metadata
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
metric_list <- c("MHW_SumCumInt")
scenario_list <- c("585", "126", "245")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

solution <- c(seq(from = 307, to = 311, by = 1), seq(from = 327, to = 331, by = 1), seq(from = 347, to = 351, by = 1)) # solution names, check metadata
solution <-lapply(solution, function(x) {
  y <- paste0("s", x)
}) %>% unlist()

loopthrough_MM_Feature(solution, metric_list, scenario_list, model_list)

# ----- Climate priority area approach runs -----
# loop through the metric, scenario, and model lists
metric_list <- c("MHW_num", "MHW_CumInt", "MHW_CumDur")
scenario_list <- c("126", "245", "585")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

solution <- seq(from = 525, to = 526, by = 1) # solution names, check metadata
solution <-lapply(solution, function(x) {
  y <- paste0("s", x)
}) %>% unlist()

loopthrough_MM_ClimatePriorityArea(solution, metric_list, scenario_list, model_list)
