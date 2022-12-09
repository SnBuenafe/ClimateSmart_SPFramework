# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"

#### Preliminaries ####
# "Iterations for nMDS"
# Creating the rest of the solutions

# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R") # climate layers are loaded in the script

# Loop through the all percentile runs
solution_list <- paste0("s", seq(82, 101, 1))
metric_list <- c("tos", "phos", "o2os", "velocity")
scenario_list <- c("SSP 2-4.5")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

system.time({loopthrough_Percentile(solution_list,
                                    metric_list,
                                    scenario_list,
                                    model_list)})

solution_list <- c("s299", "s303")
metric_list <- c("MHW")
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5")
model_list <- c("ensemble")

system.time({loopthrough_Percentile(solution_list,
                                    metric_list,
                                    scenario_list,
                                    model_list)})

solution_list <- paste0("s", c(seq(322, 326, 1), seq(342, 346, 1), seq(294, 298, 1)))
metric_list <- c("MHW")
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
model_list <-  c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

system.time({loopthrough_Percentile(solution_list,
                                    metric_list,
                                    scenario_list,
                                    model_list)})

solution_list <- c("s383", "s384")
metric_list <- c("CombinedMetric")
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5")
model_list <- c("ensemble")

system.time({loopthrough_Percentile(solution_list,
                                    metric_list,
                                    scenario_list,
                                    model_list)})

solution_list <- paste0("s", seq(385, 399, 1))
metric_list <- c("CombinedMetric")
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
model_list <-  c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

system.time({loopthrough_Percentile(solution_list,
                                    metric_list,
                                    scenario_list,
                                    model_list)})

# Loop through feature approach
solution_list <- paste0("s", seq(46, 53, 1))
metric_list <- c("tos", "phos", "o2os", "velocity")
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5")
model_list <- c("ensemble")

system.time({loopthrough_Feature(solution_list,
                                    metric_list,
                                    scenario_list,
                                    model_list)})

solution_list <- paste0("s", seq(142, 221, 1))
metric_list <- c("tos", "phos", "o2os", "velocity")
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

system.time({loopthrough_Feature(solution_list,
                                 metric_list,
                                 scenario_list,
                                 model_list)})

solution_list <- c("s300", "s304")
metric_list <- c("MHW")
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5")
model_list <- c("ensemble")

system.time({loopthrough_Feature(solution_list,
                                 metric_list,
                                 scenario_list,
                                 model_list)})

solution_list <- paste0("s", c(seq(307, 311, 1), seq(327, 331, 1), seq(347, 351, 1)))
metric_list <- c("MHW")
scenario_list <- c("SSP 5-8.5", "SSP 1-2.6", "SSP 2-4.5")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

system.time({loopthrough_Feature(solution_list,
                                 metric_list,
                                 scenario_list,
                                 model_list)})

solution_list <- c("s366", "s367")
metric_list <- c("CombinedMetric")
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5")
model_list <- c("ensemble")

system.time({loopthrough_Feature(solution_list,
                                 metric_list,
                                 scenario_list,
                                 model_list)})

solution_list <- paste0("s", seq(368, 382, 1))
metric_list <- c("CombinedMetric")
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

system.time({loopthrough_Feature(solution_list,
                                 metric_list,
                                 scenario_list,
                                 model_list)})