# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

#### Preliminaries ####
# "Iterations for nMDS"
# Creating the rest of the solutions

# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R") # climate layers are loaded in the script

# Loop through percentile solutions
solution_list <- c("s299", "s303")
metric_list <- c("MHW")
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5")
model_list <- c("ensemble")

loopthrough_Percentile(solution_list,
                       metric_list,
                       scenario_list,
                       model_list)

# Loop through feature solutions
solution_list <- c("s366", "s367")
metric_list <- c("CombinedMetric")
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5")
model_list <- c("ensemble")

loopthrough_Feature(solution_list,
                    metric_list,
                    scenario_list,
                    model_list)

solution_list <- paste0("s", seq(368, 382))
metric_list <- c("CombinedMetric")
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

system.time({loopthrough_Feature(solution_list,
                    metric_list,
                    scenario_list,
                    model_list)})

# Loop through penalty solutions
solution_list <- paste0("s", seq(54, 61, 1))
metric_list <- c("tos", "phos", "o2os", "velocity")
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5")
model_list <- c("ensemble")

system.time({loopthrough_Penalty(solution_list,
                                 metric_list,
                                 scenario_list,
                                 model_list)})
