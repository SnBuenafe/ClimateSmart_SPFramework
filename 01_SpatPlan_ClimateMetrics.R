# title: "Calculating climate metrics"
# author: "Isaac Brito-Morales and Tin Buenafe"

#### Preliminaries ####
# Load packages
#install.packages("pacman")
#devtools::install_github("JorGarMol/VoCC", dependencies = TRUE, build_vignettes = TRUE)
pacman::p_load(BiocManager, ncdf4, PCICt, ncdf4.helpers, raster, tidyverse, terra, doParallel, VoCC)

# Define some lists
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5") # list of climate scenarios
variable_list <- c("tos", "phos", "o2os") # list of variables sens "velocity"
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM") # list of models

# Load functions
source("HelperFunctions/SpatPlan_ClimateMetricFxns_WestPac.R")

#### Create RasterStack for each model ####
# Create list of year_bounds, with the index the same as the model_list
year_bounds <- list()
for(i in 1:length(model_list)) {
  if(i %in% c(1, 2, 4)) {
    from = c(2015) %>% as_tibble_col(column_name = "from")
    to = c(2100) %>% as_tiblbe_col(column_name = "to")
    year_bounds[[i]] <- cbind(from, to)
  }
  else if(i == 3) {
    from = c(2015, 2035, 2055, 2075, 2095) %>% as_tibble_col(column_name = "from")
    to = c(2034, 2054, 2074, 2094, 2100) %>% as_tibble_col(column_name = "to")
    year_bounds[[i]] <- cbind(from, to)
  }
  else if(i == 5) {
    from = c(2015, 2021, 2031, 2041, 2051, 2061, 2071, 2081, 2091) %>% as_tibble_col(column_name = "from")
    to =  c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100) %>% as_tibble_col(column_name = "to")
    year_bounds[[i]] <- cbind(from, to)
  }
}

# Run loop
for(i in 1:length(model_list)) {
  create_raster(mode_list[i], year_bounds[[i]])
}


#### Ensemble mean approach ####
# ----- Create ensemble mean (yearly) for all variables -----
for(scenario_num in 1:length(scenario_list)) {
  for(variable_num in 1:length(variable_list)) {
    generate_ensemble(variable = variable_list[variable_num],
                      scenario = scenario_list[scenario_num])
  }
}

# ----- Calculate rate of change metrics -----
# Use tempTrend() function from VoCC
for(scenario_num in 1:length(scenario_list)) {
  for(variable_num in 1:length(variable_list)) {
    calculate_rate(variable = variable_list[variable_num],
                   scenario = scenario_list[scenario_num])
  }
}

# ----- Calculate climate velocity using temperature -----
# Use gVoCC to calculate for velocity across the scenarios
for(scenario_num in 1:length(scenario_list)) {
  generate_velocity(scenario = scenario_list[scenario_num])
}

#### Multi-model ensemble approach ####
# ----- Create climate layers using the multi-model ensemble approach -----
for(scenario_num in 1:length(scenario_list)) {
  for(variable_num in 1:length(variable_list)) {
    generate_MultiModelEnsemble(variable = variable_list[variable_num], scenario = scenario_list[scenario_num])
  }
}

# ----- Calculate rate of change climate metrics using the multi-model ensemble approach -----
for(scenario_num in 1:length(scenario_list)) {
  for(variable_num in 1:length(variable_list)) {
    calculate_MultiModelEnsembleRate(variable = variable_list[variable_num], scenario = scenario_list[scenario_num], metric = "roc")
  }
}

# ----- Calculate climate velocity using the multi-model ensemble approach -----
for(scenario_num in 1:length(scenario_list)) {
  generate_MultiModelEnsembleVelocity(scenario_list[scenario_num])
}
