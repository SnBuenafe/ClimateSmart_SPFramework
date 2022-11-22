# title: "Create layers for Climate-Smart Methods paper"
# author: "Tin Buenafe and Jason Everett"

#### Overview ####
# This code follows the workflow of _SpatialPlanning_ repository to create a climate-smart Marine Spatial Plan for the Western Pacific.
# The code depends on `sf`, `terra`, `tidyverse`, `rnaturalearth`, `prioritizr`, `VoCC`, `stars`, `patchwork`.

# This code creates the layers necessary for answering the research questions of this project. Analyses are ran in `SpatPlan_WestPac_Runs_XX.R`

# To reproduce this code for the Western Pacific, you will need to have the raw data. Contact Tin for more details.
# However, it's not recommended. The output files (in Output/) are restricted to just the Western Pacific.
# So, the user is advised to start all rerunning scripts starting from 04.

#### Preliminaries ####
# Load the extras, including functions and libraries
source("HelperFunctions/SpatPlan_Extras.R")
# Load helper functions written specifically for this spatial planning project
source("HelperFunctions/SpatPlan_HelperFxns_WestPac.R")

if (!file.exists("Data")) {
  stop("The Data folder does not exist at SpatialPlanning/Data. Please download from the RDM and then try again.")
}

reprocess <- FALSE # Do we want to reprocess the PUs/AquaMaps data or use saved versions


#### Setting user parameters ####
# Calling the region
Region <- "WestPacific"
save_name <- "WestPacific"

PU_size = 669.9 # km2 (0.25 deg at equator)
Shape <- "Hexagon" # "Shape of PUs
MinDepth <- 0
MaxDepth <- 200
CO <- 0.5

# Choose CRS for analysis (Robinson: Pacific-centered)
cCRS <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

#### Creating planning region ####
# If Region = WestPacific, demarcating boundary and removing land areas would be different
# The object "world" here represents the planning region (inverse = FALSE)
world <- fSpatPlan_Get_Boundary(Region, cCRS) %>% 
  fSpatPlan_Convert2PacificRobinson() # project to Pacific Robinson
inverse = FALSE

# Set limits of the boundary
Limits = c(xmin = -120, xmax = 130, ymax = 30, ymin = -55)
Bndry <- fSpatPlan_Get_Boundary(Limits, cCRS)

# Create the planning units
if(reprocess){
  PUs <- fSpatPlan_Get_PlanningUnits(Bndry, world, PU_size, Shape, inverse) %>% 
    dplyr::mutate(cellID = row_number())
  saveRDS(PUs, file.path("Output", paste(save_name, paste0("PlanningRegion.rds"), sep = "_")))
} else {
  PUs <- read_rds(file.path("Output", paste(save_name, paste0("PlanningRegion.rds"), sep = "_")))
}

land <- ne_countries(scale = 'large', returnclass = 'sf') %>% 
  fSpatPlan_Convert2PacificRobinson() 
# Plotting the planning region
(ggPU <- fSpatPlan_PlotPUs(PUs, land))
ggsave("Layer_PlanningRegion.png",
       plot = ggPU, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")


#### Creating AquaMaps Layers (Conservation Features) ####
# AquaMaps Conservation Features
# Pacific-centered projection is tricky, so we defined western and eastern limits
west_limit = -120 # use negative numbers for western limit
east_limit = 130 # use positive numbers for eastern limit
if(reprocess){
  aqua_sf <- fSpatPlan_Get_AquaMaps(PUs, cCRS, MinDepth, MaxDepth, CutOff = CO, limits = c(west_limit, east_limit))
  
  saveRDS(aqua_sf, file.path("Output", paste(save_name, paste0("AquaMaps.rds"), sep = "_"))) # Save rds so you don't have to reprocess everytime.
} else {
  aqua_sf <- read_rds(file.path("Output", paste(save_name, paste0("AquaMaps.rds"), sep = "_")))
}

# Plotting the # of features in AquaMaps
ggFeatureNo <- fSpatPlan_FeatureNo(aqua_sf, land)
ggsave("Layer_AquaMaps.png",
       plot = ggFeatureNo, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Creating Cost Layer ####
# if(reprocess){
#   Cost <- fSpatPlan_Get_Cost(PUs, cCRS, group = "all") # set group = all, if including all functional groups
#   saveRDS(Cost, file.path("Output", paste(save_name, paste0("Cost.rds"), sep = "_"))) # Save rds so you don't have to reprocess everytime
# } else {
#   Cost <- read_rds(file.path("Output", paste(save_name, paste0("Cost.rds"), sep = "_")))
# }
# 
# # Squish the cost layers
# Cost_squish <- Cost %>% 
#   mutate(Cost_squish = scales::oob_squish(Cost, quantile(Cost, c(0.01, 0.99))))

# Plotting the cost layer
# ggCost <- fSpatPlan_PlotCost(Cost, land)

#### Creating climate layers ####
# Climate models considered here are: 1) CanESM5, 2) CMCC-ESM2, 3) GFDL-ESM4, 4) IPSL-CM6A-LR, and 5) NorESM2-MM
# Climate variables considered: 1) tos, 2) o2os, and 3) phos from 2015-2100
# Climate scenarios: 1) SSP 1-2.6, 2) SSP 2-4.5, 3) SSP 5-8.5

scenario_path <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
scenario_input <- c("ssp126", "ssp245", "ssp585")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

#### Ensemble Mean: Rate of Change of Temperature ####
var = "tos"
path <- "Data/EnsembleMean/"
list <- list.files(path)

for(i in 1:length(scenario_input)) {
  if(reprocess) {
    param <- c("ensemble", scenario_input[i], var)
    
    file <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
    file <- which(file == 1)
    
    df <- readRDS(paste0(path, list[file]))
    layer <- fSpatPlan_Get_ClimateLayer(PUs, df, cCRS, metric = "roc_tos", colname = "slpTrends")
    
    saveRDS(layer, file.path("Output", 
                             paste(save_name, "ClimateLayer", "roc_tos", scenario_path[i], "ensemble.rds", sep = "_")))
    
    print(paste0("Saved EM", ": ", scenario_path[i]))
    
  } else {
      df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer", "roc_tos", scenario_path[i], "ensemble.rds", sep = "_")))
      
      assign(x = paste0("roc_tos_", toupper(scenario_input[i])), value = df, envir = .GlobalEnv)
  }
}

# Plot Climate Warming Rates
gg_roc_tos_SSP126 <- fPlot_ClimateWarming(roc_tos_SSP126, world = land)
ggsave("Layer_RateofClimateWarming_SSP126.png",
       plot = gg_roc_tos_SSP126, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

gg_roc_tos_SSP245 <- fPlot_ClimateWarming(roc_tos_SSP245, world = land)
ggsave("Layer_RateofClimateWarming_SSP245.png",
       plot = gg_roc_tos_SSP245, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

gg_roc_tos_SSP585 <- fPlot_ClimateWarming(roc_tos_SSP585, world = land)
ggsave("Layer_RateofClimateWarming_SSP585.png",
       plot = gg_roc_tos_SSP585, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Multi-Model Ensemble: Rate of Change of Temperature ####
path <- "Data/MultiModelEnsemble/raster/"
list <- list.files(path)

for(m in 1:length(model_list)) {
  for(i in 1:length(scenario_input)) {
    if(reprocess) {
      param <- c(model_list[m], scenario_input[i], var)
      
      file <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
      file <- which(file == 1)
      
      df <- readRDS(paste0(path, list[file]))
      layer <- fSpatPlan_Get_ClimateLayer(PUs, df, cCRS, metric = "roc_tos", colname = "slpTrends")
      
      saveRDS(layer, file.path("Output", 
                               paste(save_name, "ClimateLayer", "roc_tos", scenario_path[i], paste0(model_list[m], ".rds"), sep = "_")))
      
      print(paste0("Saved EM", ": ", model_list[m], ";", scenario_path[i]))
      
    } else {
      df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer", "roc_tos", scenario_path[i], paste0(model_list[m], ".rds"), sep = "_")))
      
      assign(x = paste0("roc_tos_", model_list[m], "_", toupper(scenario_input[i])), value = df, envir = .GlobalEnv)
    }
  }
}

# Plot Climate Warming Rates per model (SSP 5-8.5)
gg_tos_CanESM5 <- fPlot_ClimateWarming(roc_tos_CanESM5_SSP585, world = land)
ggsave("Layer_RateofClimateWarming_CanESM5_SSP585.png",
       plot = gg_tos_CanESM5, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_tos_CMCC-ESM2` <- fPlot_ClimateWarming(`roc_tos_CMCC-ESM2_SSP585`, world = land)
ggsave("Layer_RateofClimateWarming_CMCC-ESM2_SSP585.png",
       plot = `gg_tos_CMCC-ESM2`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_tos_GFDL-ESM4` <- fPlot_ClimateWarming(`roc_tos_GFDL-ESM4_SSP585`, world = land)
ggsave("Layer_RateofClimateWarming_GFDL-ESM4_SSP585.png",
       plot = `gg_tos_GFDL-ESM4`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_tos_IPSL-CM6A-LR` <- fPlot_ClimateWarming(`roc_tos_IPSL-CM6A-LR_SSP585`, world = land)
ggsave("Layer_RateofClimateWarming_IPSL-CM6A-LR_SSP585.png",
       plot = `gg_tos_IPSL-CM6A-LR`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_tos_NorESM2-MM` <- fPlot_ClimateWarming(`roc_tos_NorESM2-MM_SSP585`, world = land)
ggsave("Layer_RateofClimateWarming_NorESM2-MM_SSP585.png",
       plot = `gg_tos_NorESM2-MM`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Ensemble Mean: Rate of Change of pH ####
var = "phos"
path <- "Data/EnsembleMean/"
list <- list.files(path)

for(i in 1:length(scenario_input)) {
  if(reprocess) {
    param <- c("ensemble", scenario_input[i], var)
    
    file <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
    file <- which(file == 1)
    
    df <- readRDS(paste0(path, list[file]))
    layer <- fSpatPlan_Get_ClimateLayer(PUs, df, cCRS, metric = "roc_phos", colname = "slpTrends")
    
    saveRDS(layer, file.path("Output", 
                             paste(save_name, "ClimateLayer", "roc_phos", scenario_path[i], "ensemble.rds", sep = "_")))
    
    print(paste0("Saved EM", ": ", scenario_path[i]))
    
  } else {
    df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer", "roc_phos", scenario_path[i], "ensemble.rds", sep = "_")))
    
    assign(x = paste0("roc_phos_", toupper(scenario_input[i])), value = df, envir = .GlobalEnv)
  }
}

# Plot Rates of Change in pH
gg_roc_phos_SSP126 <- fPlot_OceanAcidification(roc_phos_SSP126, world = land)
ggsave("Layer_RateofOceanAcidification_SSP126.png",
       plot = gg_roc_phos_SSP126, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

gg_roc_phos_SSP245 <- fPlot_OceanAcidification(roc_phos_SSP245, world = land)
ggsave("Layer_RateofOceanAcidification_SSP245.png",
       plot = gg_roc_phos_SSP245, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

gg_roc_phos_SSP585 <- fPlot_OceanAcidification(roc_phos_SSP585, world = land)
ggsave("Layer_RateofOceanAcidification_SSP585.png",
       plot = gg_roc_phos_SSP585, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Multi-Model Ensemble: Rate of Change of pH ####
path <- "Data/MultiModelEnsemble/raster/"
list <- list.files(path)

for(m in 1:length(model_list)) {
  for(i in 1:length(scenario_input)) {
    if(reprocess) {
      param <- c(model_list[m], scenario_input[i], var)
      
      file <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
      file <- which(file == 1)
      
      df <- readRDS(paste0(path, list[file]))
      layer <- fSpatPlan_Get_ClimateLayer(PUs, df, cCRS, metric = "roc_phos", colname = "slpTrends")
      
      saveRDS(layer, file.path("Output", 
                               paste(save_name, "ClimateLayer", "roc_phos", scenario_path[i], paste0(model_list[m], ".rds"), sep = "_")))
      
      print(paste0("Saved EM", ": ", model_list[m], ";", scenario_path[i]))
      
    } else {
      df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer", "roc_phos", scenario_path[i], paste0(model_list[m], ".rds"), sep = "_")))
      
      assign(x = paste0("roc_phos_", model_list[m], "_", toupper(scenario_input[i])), value = df, envir = .GlobalEnv)
    }
  }
}

# Plot Ocean Acidification Rates per model
gg_phos_CanESM5 <- fPlot_OceanAcidification(roc_phos_CanESM5_SSP585, world = land)
ggsave("Layer_RateofOceanAcidification_CanESM5_SSP585.png",
         plot = gg_phos_CanESM5, width = 21, height = 29.7, dpi = 300,
         path = "Figures/")
  
`gg_phos_CMCC-ESM2` <- fPlot_OceanAcidification(`roc_phos_CMCC-ESM2_SSP585`, world = land)
ggsave("Layer_RateofOceanAcidification_CMCC-ESM2_SSP585.png",
       plot = `gg_phos_CMCC-ESM2`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_phos_GFDL-ESM4` <- fPlot_OceanAcidification(`roc_phos_GFDL-ESM4_SSP585`, world = land)
ggsave("Layer_RateofOceanAcidification_GFDL-ESM4_SSP585.png",
       plot = `gg_phos_GFDL-ESM4`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_phos_IPSL-CM6A-LR` <- fPlot_OceanAcidification(`roc_phos_IPSL-CM6A-LR_SSP585`, world = land)
ggsave("Layer_RateofOceanAcidification_IPSL-CM6A-LR_SSP585.png",
       plot = `gg_phos_IPSL-CM6A-LR`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_phos_NorESM2-MM` <- fPlot_OceanAcidification(`roc_phos_NorESM2-MM_SSP585`, world = land)
ggsave("Layer_RateofOceanAcidification_NorESM2-MM_SSP585.png",
       plot = `gg_phos_NorESM2-MM`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Ensemble Mean: Rate of Change of O2 ####
var = "o2os"
path <- "Data/EnsembleMean/"
list <- list.files(path)

for(i in 1:length(scenario_input)) {
  if(reprocess) {
    param <- c("ensemble", scenario_input[i], var)
    
    file <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
    file <- which(file == 1)
    
    df <- readRDS(paste0(path, list[file]))
    layer <- fSpatPlan_Get_ClimateLayer(PUs, df, cCRS, metric = "roc_o2os", colname = "slpTrends")
    
    saveRDS(layer, file.path("Output", 
                             paste(save_name, "ClimateLayer", "roc_o2os", scenario_path[i], "ensemble.rds", sep = "_")))
    
    print(paste0("Saved EM", ": ", scenario_path[i]))
    
  } else {
    df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer", "roc_o2os", scenario_path[i], "ensemble.rds", sep = "_")))
    
    assign(x = paste0("roc_o2os_", toupper(scenario_input[i])), value = df, envir = .GlobalEnv)
  }
}

# Plot Rates of Declining Oxygen Concentration
gg_roc_o2os_SSP126 <- fPlot_OceanDeoxygenation(roc_o2os_SSP126, world = land)
ggsave("Layer_RateofDecliningOxygenConcentration_SSP126.png",
       plot = gg_roc_o2os_SSP126, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

gg_roc_o2os_SSP245 <- fPlot_OceanDeoxygenation(roc_o2os_SSP245, world = land)
ggsave("Layer_RateofDecliningOxygenConcentration_SSP245.png",
       plot = gg_roc_o2os_SSP245, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

gg_roc_o2os_SSP585 <- fPlot_OceanDeoxygenation(roc_o2os_SSP585, world = land)
ggsave("Layer_RateofDecliningOxygenConcentration_SSP585.png",
       plot = gg_roc_o2os_SSP585, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Multi-Model Ensemble: Rate of Change of Oxygen Concentration ####
path <- "Data/MultiModelEnsemble/raster/"
list <- list.files(path)

for(m in 1:length(model_list)) {
  for(i in 1:length(scenario_input)) {
    if(reprocess) {
      param <- c(model_list[m], scenario_input[i], var)
      
      file <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
      file <- which(file == 1)
      
      df <- readRDS(paste0(path, list[file]))
      layer <- fSpatPlan_Get_ClimateLayer(PUs, df, cCRS, metric = "roc_o2os", colname = "slpTrends")
      
      saveRDS(layer, file.path("Output", 
                               paste(save_name, "ClimateLayer", "roc_o2os", scenario_path[i], paste0(model_list[m], ".rds"), sep = "_")))
      
      print(paste0("Saved EM", ": ", model_list[m], ";", scenario_path[i]))
      
    } else {
      df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer", "roc_o2os", scenario_path[i], paste0(model_list[m], ".rds"), sep = "_")))
      
      assign(x = paste0("roc_o2os_", model_list[m], "_", toupper(scenario_input[i])), value = df, envir = .GlobalEnv)
    }
  }
}

# Plot Declining Oxygen Concentration Rates per model
gg_o2os_CanESM5 <- fPlot_OceanDeoxygenation(roc_o2os_CanESM5_SSP585, world = land)
ggsave("Layer_RateofDecliningOxygenConcentration_CanESM5_SSP585.png",
       plot = gg_o2os_CanESM5, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_o2os_CMCC-ESM2` <- fPlot_OceanDeoxygenation(`roc_o2os_CMCC-ESM2_SSP585`, world = land)
ggsave("Layer_RateofDecliningOxygenConcentration_CMCC-ESM2_SSP585.png",
       plot = `gg_o2os_CMCC-ESM2`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_o2os_GFDL-ESM4` <- fPlot_OceanDeoxygenation(`roc_o2os_GFDL-ESM4_SSP585`, world = land)
ggsave("Layer_RateofDecliningOxygenConcentration_GFDL-ESM4_SSP585.png",
       plot = `gg_o2os_GFDL-ESM4`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_o2os_IPSL-CM6A-LR` <- fPlot_OceanDeoxygenation(`roc_o2os_IPSL-CM6A-LR_SSP585`, world = land)
ggsave("Layer_RateofDecliningOxygenConcentration_IPSL-CM6A-LR_SSP585.png",
       plot = `gg_o2os_IPSL-CM6A-LR`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_o2os_NorESM2-MM` <- fPlot_OceanDeoxygenation(`roc_o2os_NorESM2-MM_SSP585`, world = land)
ggsave("Layer_RateofDecliningOxygenConcentration_NorESM2-MM_SSP585.png",
       plot = `gg_o2os_NorESM2-MM`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Ensemble Mean: Climate Velocity ####
var = "velocity"
path <- "Data/EnsembleMean/"
list <- list.files(path)

for(i in 1:length(scenario_input)) {
  if(reprocess) {
    param <- c("ensemble", scenario_input[i], var)
    
    file <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
    file <- which(file == 1)
    
    df <- readRDS(paste0(path, list[file]))
    layer <- fSpatPlan_Get_ClimateLayer(PUs, df, cCRS, metric = "velocity", colname = "voccMag")
    
    saveRDS(layer, file.path("Output", 
                             paste(save_name, "ClimateLayer", "velocity", scenario_path[i], "ensemble.rds", sep = "_")))
    
    print(paste0("Saved EM", ": ", scenario_path[i]))
    
  } else {
    df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer", "velocity", scenario_path[i], "ensemble.rds", sep = "_")))
    
    assign(x = paste0("velocity_", toupper(scenario_input[i])), value = df, envir = .GlobalEnv)
  }
}

# Plot Climate velocity
gg_velocity_SSP126 <- fPlot_ClimateVelocity(velocity_SSP126, world = land)
ggsave("Layer_ClimateVelocity_SSP126.png",
       plot = gg_velocity_SSP126, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

gg_velocity_SSP245 <- fPlot_ClimateVelocity(velocity_SSP245, world = land)
ggsave("Layer_ClimateVelocity_SSP245.png",
       plot = gg_velocity_SSP245, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

gg_velocity_SSP585 <- fPlot_ClimateVelocity(velocity_SSP585, world = land)
ggsave("Layer_ClimateVelocity_SSP585.png",
       plot = gg_velocity_SSP585, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Multi-Model Ensemble: Climate Velocity ####
path <- "Data/MultiModelEnsemble/raster/"
list <- list.files(path)

for(m in 1:length(model_list)) {
  for(i in 1:length(scenario_input)) {
    if(reprocess) {
      param <- c(model_list[m], scenario_input[i], var)
      
      file <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
      file <- which(file == 1)
      
      df <- readRDS(paste0(path, list[file]))
      layer <- fSpatPlan_Get_ClimateLayer(PUs, df, cCRS, metric = "velocity", colname = "voccMag")
      
      saveRDS(layer, file.path("Output", 
                               paste(save_name, "ClimateLayer", "velocity", scenario_path[i], paste0(model_list[m], ".rds"), sep = "_")))
      
      print(paste0("Saved EM", ": ", model_list[m], ";", scenario_path[i]))
      
    } else {
      df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer", "velocity", scenario_path[i], paste0(model_list[m], ".rds"), sep = "_")))
      
      assign(x = paste0("velocity_", model_list[m], "_", toupper(scenario_input[i])), value = df, envir = .GlobalEnv)
    }
  }
}

# Plot Climate Velocity per model
gg_velocity_CanESM5 <- fPlot_ClimateVelocity(velocity_CanESM5_SSP585, world = land)
ggsave("Layer_ClimateVelocity_CanESM5_SSP585.png",
       plot = gg_velocity_CanESM5, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_velocity_CMCC-ESM2` <- fPlot_ClimateVelocity(`velocity_CMCC-ESM2_SSP585`, world = land)
ggsave("Layer_ClimateVelocity_CMCC-ESM2_SSP585.png",
       plot = `gg_velocity_CMCC-ESM2`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_velocity_GFDL-ESM4` <- fPlot_ClimateVelocity(`velocity_GFDL-ESM4_SSP585`, world = land)
ggsave("Layer_ClimateVelocity_GFDL-ESM4_SSP585.png",
       plot = `gg_velocity_GFDL-ESM4`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_velocity_IPSL-CM6A-LR` <- fPlot_ClimateVelocity(`velocity_IPSL-CM6A-LR_SSP585`, world = land)
ggsave("Layer_ClimateVelocity_IPSL-CM6A-LR_SSP585.png",
       plot = `gg_velocity_IPSL-CM6A-LR`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_velocity_NorESM2-MM` <- fPlot_ClimateVelocity(`velocity_NorESM2-MM_SSP585`, world = land)
ggsave("Layer_ClimateVelocity_NorESM2-MM_SSP585.png",
       plot = `gg_velocity_NorESM2-MM`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")


#### Ensemble Mean: MHW ####
var = "MHW"
path <- "Data/EnsembleMean/"
list <- list.files(path)

for(i in 1:length(scenario_input)) {
  if(reprocess) {
    param <- c("ensemble", scenario_input[i], var)
    
    file <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
    file <- which(file == 1)
    
    df <- readRDS(paste0(path, list[file]))
    layer <- fSpatPlan_Get_ClimateLayer(PUs, df, cCRS, metric = "MHW", colname = "sum_cum_int")
    
    saveRDS(layer, file.path("Output", 
                             paste(save_name, "ClimateLayer", "MHW", scenario_path[i], "ensemble.rds", sep = "_")))
    
    print(paste0("Saved EM", ": ", scenario_path[i]))
    
  } else {
    df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer", "MHW", scenario_path[i], "ensemble.rds", sep = "_")))
    
    assign(x = paste0("MHW_", toupper(scenario_input[i])), value = df, envir = .GlobalEnv)
  }
}

# Plot MHW metrics
gg_MHW_SSP126 <- fPlot_MHW(MHW_SSP126, world = land)
ggsave("Layer_MHW_SSP126.png",
       plot = gg_MHW_SSP126, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

gg_MHW_SSP245 <- fPlot_MHW(MHW_SSP245, world = land)
ggsave("Layer_MHW_SSP245.png",
       plot = gg_MHW_SSP245, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

gg_MHW_SSP585 <- fPlot_MHW(MHW_SSP585, world = land)
ggsave("Layer_MHW_SSP585.png",
       plot = gg_MHW_SSP585, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Multi-Model Ensemble: MHW metrics ####
path <- "Data/MultiModelEnsemble/raster/"
list <- list.files(path)

for(m in 1:length(model_list)) {
  for(i in 1:length(scenario_input)) {
    if(reprocess) {
      param <- c(model_list[m], scenario_input[i], var)
      
      file <- apply(outer(list, param, stringr::str_detect), 1, all) %>% as.numeric()
      file <- which(file == 1)
      
      df <- readRDS(paste0(path, list[file]))
      layer <- fSpatPlan_Get_ClimateLayer(PUs, df, cCRS, metric = "MHW", colname = "sum_cum_int")
      
      saveRDS(layer, file.path("Output", 
                               paste(save_name, "ClimateLayer", "MHW", scenario_path[i], paste0(model_list[m], ".rds"), sep = "_")))
      
      print(paste0("Saved EM", ": ", model_list[m], ";", scenario_path[i]))
      
    } else {
      df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer", "MHW", scenario_path[i], paste0(model_list[m], ".rds"), sep = "_")))
      
      assign(x = paste0("MHW_", model_list[m], "_", toupper(scenario_input[i])), value = df, envir = .GlobalEnv)
    }
  }
}

# Plot MHW metrics
gg_MHW_CanESM5 <- fPlot_MHW(MHW_CanESM5_SSP585, world = land)
ggsave("Layer_MHW_CanESM5_SSP585.png",
       plot = gg_MHW_CanESM5, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_MHW_CMCC-ESM2` <- fPlot_MHW(`MHW_CMCC-ESM2_SSP585`, world = land)
ggsave("Layer_MHW_CMCC-ESM2_SSP585.png",
       plot = `gg_MHW_CMCC-ESM2`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_MHW_GFDL-ESM4` <- fPlot_MHW(`MHW_GFDL-ESM4_SSP585`, world = land)
ggsave("Layer_MHW_GFDL-ESM4_SSP585.png",
       plot = `gg_MHW_GFDL-ESM4`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_MHW_IPSL-CM6A-LR` <- fPlot_MHW(`MHW_IPSL-CM6A-LR_SSP585`, world = land)
ggsave("Layer_MHW_IPSL-CM6A-LR_SSP585.png",
       plot = `gg_MHW_IPSL-CM6A-LR`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_MHW_NorESM2-MM` <- fPlot_MHW(`MHW_NorESM2-MM_SSP585`, world = land)
ggsave("Layer_MHW_NorESM2-MM_SSP585.png",
       plot = `gg_MHW_NorESM2-MM`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")


#### Ensemble Mean: Combined metric ####
for(i in 1:length(scenario_path)) {
  if(reprocess) {
    combined_df <- combineMetric(scenario = scenario_path[i], model = "ensemble")
    saveRDS(combined_df, file.path("Output", 
                                   paste(save_name, "ClimateLayer", "CombinedMetric", scenario_path[i], "ensemble.rds", sep = "_")))
  } else {
    df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer", "CombinedMetric", scenario_path[i], "ensemble.rds", sep = "_")))
    
    assign(x = paste0("CombinedMetric_", toupper(scenario_input[i])), value = df, envir = .GlobalEnv)
  }
}

# Plot Combined metric
gg_Combined_SSP126 <- fSpatPlan_PlotCombinedClimate(CombinedMetric_SSP126, world = land)
ggsave("Layer_Combined_SSP126.png",
       plot = gg_Combined_SSP126, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

gg_Combined_SSP245 <- fSpatPlan_PlotCombinedClimate(CombinedMetric_SSP245, world = land)
ggsave("Layer_Combined_SSP245.png",
       plot = gg_Combined_SSP245, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

gg_Combined_SSP585 <- fSpatPlan_PlotCombinedClimate(CombinedMetric_SSP585, world = land)
ggsave("Layer_Combined_SSP585.png",
       plot = gg_Combined_SSP585, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Multi-model Ensemble Mean: Combined metric ####
for(i in 1:length(scenario_path)) {
  for(j in 1:length(model_list)) {
    if(reprocess) {
      combined_df <- combineMetric(scenario = scenario_path[i], model = model_list[j])
      saveRDS(combined_df, file.path("Output", 
                                     paste(save_name, "ClimateLayer", "CombinedMetric", scenario_path[i], paste0(model_list[j], ".rds"), sep = "_")))
    } else {
      df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer", "CombinedMetric", scenario_path[i], paste0(model_list[j], ".rds"), sep = "_")))
      
      assign(x = paste0("CombinedMetric_", model_list[j], "_", toupper(scenario_input[i])), value = df, envir = .GlobalEnv)
    }
  }
}

# Plot Combined Metrics
gg_CombinedMetric_CanESM5 <- fSpatPlan_PlotCombinedClimate(CombinedMetric_CanESM5_SSP585, world = land)
ggsave("Layer_CombinedMetric_CanESM5_SSP585.png",
       plot = gg_CombinedMetric_CanESM5, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_CombinedMetric_CMCC-ESM2` <- fSpatPlan_PlotCombinedClimate(`CombinedMetric_CMCC-ESM2_SSP585`, world = land)
ggsave("Layer_CombinedMetric_CMCC-ESM2_SSP585.png",
       plot = `gg_CombinedMetric_CMCC-ESM2`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_CombinedMetric_GFDL-ESM4` <- fSpatPlan_PlotCombinedClimate(`CombinedMetric_GFDL-ESM4_SSP585`, world = land)
ggsave("Layer_CombinedMetric_GFDL-ESM4_SSP585.png",
       plot = `gg_CombinedMetric_GFDL-ESM4`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_CombinedMetric_IPSL-CM6A-LR` <- fSpatPlan_PlotCombinedClimate(`CombinedMetric_IPSL-CM6A-LR_SSP585`, world = land)
ggsave("Layer_CombinedMetric_IPSL-CM6A-LR_SSP585.png",
       plot = `gg_CombinedMetric_IPSL-CM6A-LR`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

`gg_CombinedMetric_NorESM2-MM` <- fSpatPlan_PlotCombinedClimate(`CombinedMetric_NorESM2-MM_SSP585`, world = land)
ggsave("Layer_CombinedMetric_NorESM2-MM_SSP585.png",
       plot = `gg_CombinedMetric_NorESM2-MM`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")