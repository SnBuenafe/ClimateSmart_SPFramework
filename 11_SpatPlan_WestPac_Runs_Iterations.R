# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

#### Preliminaries ####
# "Iterations for nMDS"
# Creating the rest of the solutions

# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R") # climate layers are loaded in the script

# Loop through climate priority area
solution_list <- paste0("s", seq(285, 289, 1))
metric_list <- c("velocity")
scenario_list <- c("SSP 5-8.5")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

system.time({loopthrough_CPA(solution_list, 
                             metric_list, 
                             scenario_list, 
                             model_list)})

solution_list <- paste0("s", seq(419, 433, 1))
metric_list <- c("CombinedMetric")
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

system.time({loopthrough_CPA(solution_list, 
                             metric_list, 
                             scenario_list, 
                             model_list)})