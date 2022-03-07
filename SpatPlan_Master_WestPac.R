# title: "Create layers for Climate-Smart Methods paper"
# author: "Tin Buenafe and Jason Everett"

#### Overview ####
# This code follows the workflow of _SpatialPlanning_ repository to create a climate-smart Marine Spatial Plan for the Western Pacific.
# The code depends on `sf`, `terra`, `tidyverse`, `rnaturalearth`, `prioritizr`, `VoCC`, `stars`, `patchwork`.

# This code creates the layers necessary for answering the research questions of this project. Analyses are ran in `SpatPlan_WestPac_Runs_XX.R`

# To use this code, you will need to download and expand `MME1DATA-Q1215/SpatialPlanning/Data.zip` to the directory `GitHub/SpatialPlanning/Data/`. Note that the download is only 2GB, but the expanded data is 35 GB in size. If you need help subsetting the data to your region due to memory or HD space constraints, contact Jason.

# To use this code, you will need to download and expand `MME1DATA-Q1215/SpatialPlanning/Data.zip` to the directory `GitHub/SpatialPlanning/Data/`. Note that the download is only 2GB, but the expanded data is 35 GB in size. If you need help subsetting the data to your region due to memory or HD space constraints, contact Jason.

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
PUs <- fSpatPlan_Get_PlanningUnits(Bndry, world, PU_size, Shape, inverse)
saveRDS(PUs, file.path("Output", paste(save_name, "PU", paste0(PU_size,"km2"), "Output.rds", sep = "_")))
} else {
  PUs <- read_rds(file.path("Output", paste(save_name, "PU", paste0(PU_size,"km2"), "Output.rds", sep = "_")))
}

land <- ne_countries(scale = 'large', returnclass = 'sf') %>% 
  fSpatPlan_Convert2PacificRobinson() 
# Plotting the planning region
(ggPU <- fSpatPlan_PlotPUs(PUs, land) + theme(axis.text = element_text(size = 25)))

#### Creating AquaMaps Layers (Conservation Features) ####
# AquaMaps Conservation Features
# If using Pacific-centered projection, define western and eastern limits
west_limit = -120 # use negative numbers for western limit
east_limit = 130 # use positive numbers for eastern limit
if(reprocess){
  aqua_sf <- fSpatPlan_Get_AquaMaps(PUs, cCRS, MinDepth, MaxDepth, CutOff = CO, limits = c(west_limit, east_limit))
  
  saveRDS(aqua_sf, file.path("Output", 
                             paste(save_name, "PU", paste0(PU_size,"km2"), 
                                   "AquaMaps_Output.rds", sep = "_"))) # Save rds so you don't have to reprocess everytime.
} else {
  aqua_sf <- read_rds(file.path("Output", 
                                paste(save_name, "PU", paste0(PU_size,"km2"), 
                                      "AquaMaps_Output.rds", sep = "_")))
}

# Plotting the # of features in AquaMaps
(ggFeatureNo <- fSpatPlan_FeatureNo(aqua_sf, land) + theme(axis.text = element_text(size = 25)))

#### Creating Cost Layer ####
if(reprocess){
  Cost <- fSpatPlan_Get_Cost(PUs, cCRS, group = "all") # set group = all, if including all functional groups
  saveRDS(Cost, file.path("Output",
                          paste(save_name, "PU", paste0(PU_size, "km2"),
                                "CostLayer_Output.rds", sep = "_"))) # Save rds so you don't have to reprocess everytime
} else {
  Cost <- read_rds(file.path("Output", 
                                paste(save_name, "PU", paste0(PU_size,"km2"), 
                                      "CostLayer_Output.rds", sep = "_")))
}

# Squish the cost layers
Cost_squish <- Cost %>% 
  mutate(Cost_squish = scales::oob_squish(Cost, quantile(Cost, c(0.01, 0.99))))

# Plotting the cost layer
(ggCost <- fSpatPlan_PlotCost(Cost, land))

#### Creating climate layers ####
# Climate models considered here are: 1) CanESM5, 2) CMCC-ESM2, 3) GFDL-ESM4, 4) IPSL-CM6A-LR, and 5) NorESM2-MM
# Climate variables considered: 1) tos, 2) o2os, and 3) phos from 2015-2100
# Climate scenarios: 1) SSP 1-2.6, 2) SSP 2-4.5, 3) SSP 5-8.5

#### Ensemble Mean: Rate of Change of Temperature ####
if(reprocess) {
  ClimateLayer_path <- "Data/Climate/ClimateMetrics/RateOfChange/tos/"
  ClimateLayer_files <- list.files(ClimateLayer_path)
  
  for (i in 1:length(ClimateLayer_files)){
    climate_layer <- readRDS(paste0(ClimateLayer_path, ClimateLayer_files[i]))
    layer <- fSpatPlan_Get_ClimateLayer(PUs, climate_layer, cCRS, metric = "roc_tos")
    
    saveRDS(layer, file.path("Output",
                            paste(save_name, "PU", paste0(PU_size, "km2"),
                                  ClimateLayer_files[i], sep = "_")))
  }

} else {
  ClimateLayer_path <- "Data/Climate/ClimateMetrics/RateOfChange/tos/"
  ClimateLayer_files <- list.files(ClimateLayer_path)
  
  roc_tos_SSP126 <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[1], sep = "_")))
  roc_tos_SSP245 <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[2], sep = "_")))
  roc_tos_SSP585 <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[3], sep = "_")))
}

# Plot Climate Warming Rates
(gg_roc_tos_SSP126 <- fSpatPlan_PlotClimate(ClimateLayer = roc_tos_SSP126, world = land, metric = "roc_tos") + theme(axis.text = element_text(size = 25)))
(gg_roc_tos_SSP245 <- fSpatPlan_PlotClimate(ClimateLayer = roc_tos_SSP245, world = land, metric = "roc_tos") + theme(axis.text = element_text(size = 25)))
(gg_roc_tos_SSP585 <- fSpatPlan_PlotClimate(ClimateLayer = roc_tos_SSP585, world = land, metric = "roc_tos") + theme(axis.text = element_text(size = 25)))

#### Multi-Model Ensemble: Rate of Change of Temperature ####
if(reprocess) {
  print("Open SpatPlan_ClimateMetrics.R")
  }

} else {
  ClimateLayer_path <- "Data/Climate/ClimateMetrics_Ensemble/tos/SSP 5-8.5/"
  ClimateLayer_files <- list.files(ClimateLayer_path)
  
  tos_CanESM5 <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[1], sep = "_")))
  `tos_CMCC-ESM2` <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[2], sep = "_")))
  `tos_GFDL-ESM4` <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[3], sep = "_")))
  `tos_IPSL-CM6A-LR` <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[4], sep = "_")))

  `tos_NorESM2-MM` <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[5], sep = "_")))
}

# Plot Climate Warming Rates per model
(gg_tos_CanESM5 <- fSpatPlan_PlotClimate(ClimateLayer = tos_CanESM5, world = land, metric = "roc_tos") + theme(axis.text = element_text(size = 25)))
(`gg_tos_CMCC-ESM2` <- fSpatPlan_PlotClimate(ClimateLayer = `tos_CMCC-ESM2`, world = land, metric = "roc_tos") + theme(axis.text = element_text(size = 25)))
(`gg_tos_GFDL-ESM4` <- fSpatPlan_PlotClimate(ClimateLayer = `tos_GFDL-ESM4`, world = land, metric = "roc_tos") + theme(axis.text = element_text(size = 25)))
(`gg_tos_IPSL-CM6A-LR` <- fSpatPlan_PlotClimate(ClimateLayer = `tos_IPSL-CM6A-LR`, world = land, metric = "roc_tos") + theme(axis.text = element_text(size = 25)))
(`gg_tos_NorESM2-MM` <- fSpatPlan_PlotClimate(ClimateLayer = `tos_NorESM2-MM`, world = land, metric = "roc_tos") + theme(axis.text = element_text(size = 25)))

#### Ensemble Mean: Rate of Change of pH ####
if(reprocess) {
  ClimateLayer_path <- "Data/Climate/ClimateMetrics/RateOfChange/phos/"
  ClimateLayer_files <- list.files(ClimateLayer_path)
  
  for (i in 1:length(ClimateLayer_files)){
    climate_layer <- readRDS(paste0(ClimateLayer_path, ClimateLayer_files[i]))
    layer <- fSpatPlan_Get_ClimateLayer(PUs, climate_layer, cCRS, metric = "roc_phos")
    
    saveRDS(layer, file.path("Output",
                             paste(save_name, "PU", paste0(PU_size, "km2"),
                                   ClimateLayer_files[i], sep = "_")))
  }
} else {
  ClimateLayer_path <- "Data/Climate/ClimateMetrics/RateOfChange/phos/"
  ClimateLayer_files <- list.files(ClimateLayer_path)
  
  roc_phos_SSP126 <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[1], sep = "_")))
  roc_phos_SSP245 <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[2], sep = "_")))
  roc_phos_SSP585 <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[3], sep = "_")))
}

# Plot Rates of Change in pH
(gg_roc_phos_SSP126 <- fSpatPlan_PlotClimate(ClimateLayer = roc_phos_SSP126, world = land, metric = "roc_phos") + theme(axis.text = element_text(size = 25)))
(gg_roc_phos_SSP245 <- fSpatPlan_PlotClimate(ClimateLayer = roc_phos_SSP245, world = land, metric = "roc_phos") + theme(axis.text = element_text(size = 25)))
(gg_roc_phos_SSP585 <- fSpatPlan_PlotClimate(ClimateLayer = roc_phos_SSP585, world = land, metric = "roc_phos") + theme(axis.text = element_text(size = 25)))

#### Multi-Model Ensemble: Rate of Change of pH ####
if(reprocess) {
  print("Open SpatPlan_ClimateMetrics.R")
  }

} else {
  ClimateLayer_path <- "Data/Climate/ClimateMetrics_Ensemble/phos/SSP 5-8.5/"
  ClimateLayer_files <- list.files(ClimateLayer_path)
  
  phos_CanESM5 <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[1], sep = "_")))
  `phos_CMCC-ESM2` <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[2], sep = "_")))
  `phos_GFDL-ESM4` <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[3], sep = "_")))
  `phos_IPSL-CM6A-LR` <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[4], sep = "_")))

  `phos_NorESM2-MM` <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[5], sep = "_")))
}

# Plot Ocean Acidification Rates per model
(gg_phos_CanESM5 <- fSpatPlan_PlotClimate(ClimateLayer = phos_CanESM5, world = land, metric = "roc_phos") + theme(axis.text = element_text(size = 25)))
(`gg_phos_CMCC-ESM2` <- fSpatPlan_PlotClimate(ClimateLayer = `phos_CMCC-ESM2`, world = land, metric = "roc_phos") + theme(axis.text = element_text(size = 25)))
(`gg_phos_GFDL-ESM4` <- fSpatPlan_PlotClimate(ClimateLayer = `phos_GFDL-ESM4`, world = land, metric = "roc_phos") + theme(axis.text = element_text(size = 25)))
(`gg_phos_IPSL-CM6A-LR` <- fSpatPlan_PlotClimate(ClimateLayer = `phos_IPSL-CM6A-LR`, world = land, metric = "roc_phos") + theme(axis.text = element_text(size = 25)))
(`gg_phos_NorESM2-MM` <- fSpatPlan_PlotClimate(ClimateLayer = `phos_NorESM2-MM`, world = land, metric = "roc_phos") + theme(axis.text = element_text(size = 25)))

#### Ensemble Mean: Rate of Change of O2 ####
if(reprocess) {
  ClimateLayer_path <- "Data/Climate/ClimateMetrics/RateOfChange/o2os/"
  ClimateLayer_files <- list.files(ClimateLayer_path)
  
  for (i in 1:length(ClimateLayer_files)){
    climate_layer <- readRDS(paste0(ClimateLayer_path, ClimateLayer_files[i]))
    layer <- fSpatPlan_Get_ClimateLayer(PUs, climate_layer, cCRS, metric = "roc_o2os")
    
    saveRDS(layer, file.path("Output",
                             paste(save_name, "PU", paste0(PU_size, "km2"),
                                   ClimateLayer_files[i], sep = "_")))
  }
} else {
  ClimateLayer_path <- "Data/Climate/ClimateMetrics/RateOfChange/o2os/"
  ClimateLayer_files <- list.files(ClimateLayer_path)
  
  roc_o2os_SSP126 <- readRDS(file.path("Output", 
                                       paste(save_name, "PU", paste0(PU_size, "km2"),
                                             ClimateLayer_files[1], sep = "_")))
  roc_o2os_SSP245 <- readRDS(file.path("Output", 
                                       paste(save_name, "PU", paste0(PU_size, "km2"),
                                             ClimateLayer_files[2], sep = "_")))
  roc_o2os_SSP585 <- readRDS(file.path("Output", 
                                       paste(save_name, "PU", paste0(PU_size, "km2"),
                                             ClimateLayer_files[3], sep = "_")))
}

# Plot Rates of Declining Oxygen Concentration
(gg_roc_o2os_SSP126 <- fSpatPlan_PlotClimate(ClimateLayer = roc_o2os_SSP126, world = land, metric = "roc_o2os") + theme(axis.text = element_text(size = 25)))
(gg_roc_o2os_SSP245 <- fSpatPlan_PlotClimate(ClimateLayer = roc_o2os_SSP245, world = land, metric = "roc_o2os") + theme(axis.text = element_text(size = 25)))
(gg_roc_o2os_SSP585 <- fSpatPlan_PlotClimate(ClimateLayer = roc_o2os_SSP585, world = land, metric = "roc_o2os") + theme(axis.text = element_text(size = 25)))

#### Multi-Model Ensemble: Rate of Change of Oxygen Concentration ####
if(reprocess) {
  print("Open SpatPlan_ClimateMetrics.R")
  }

} else {
  ClimateLayer_path <- "Data/Climate/ClimateMetrics_Ensemble/o2os/SSP 5-8.5/"
  ClimateLayer_files <- list.files(ClimateLayer_path)
  
  o2os_CanESM5 <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[1], sep = "_")))
  `o2os_CMCC-ESM2` <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[2], sep = "_")))
  `o2os_GFDL-ESM4` <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[3], sep = "_")))
  `o2os_IPSL-CM6A-LR` <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[4], sep = "_")))

  `o2os_NorESM2-MM` <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[5], sep = "_")))
}

# Plot Declining Oxygen Concentration Rates per model
(gg_o2os_CanESM5 <- fSpatPlan_PlotClimate(ClimateLayer = o2os_CanESM5, world = land, metric = "roc_o2os") + theme(axis.text = element_text(size = 25)))
(`gg_o2os_CMCC-ESM2` <- fSpatPlan_PlotClimate(ClimateLayer = `o2os_CMCC-ESM2`, world = land, metric = "roc_o2os") + theme(axis.text = element_text(size = 25)))
(`gg_o2os_GFDL-ESM4` <- fSpatPlan_PlotClimate(ClimateLayer = `o2os_GFDL-ESM4`, world = land, metric = "roc_o2os") + theme(axis.text = element_text(size = 25)))
(`gg_o2os_IPSL-CM6A-LR` <- fSpatPlan_PlotClimate(ClimateLayer = `o2os_IPSL-CM6A-LR`, world = land, metric = "roc_o2os") + theme(axis.text = element_text(size = 25)))
(`gg_o2os_NorESM2-MM` <- fSpatPlan_PlotClimate(ClimateLayer = `o2os_NorESM2-MM`, world = land, metric = "roc_o2os") + theme(axis.text = element_text(size = 25)))

#### Ensemble Mean: Climate Velocity ####
# Climate Velocity
if(reprocess) {
  ClimateLayer_path <- "Data/Climate/ClimateMetrics/ClimateVelocity/"
  ClimateLayer_files <- list.files(ClimateLayer_path)
  
  for (i in 1:length(ClimateLayer_files)){
    climate_layer <- readRDS(paste0(ClimateLayer_path, ClimateLayer_files[i]))
    layer <- fSpatPlan_Get_ClimateLayer(PUs, climate_layer, cCRS, metric = "velocity")
    
    saveRDS(layer, file.path("Output",
                             paste(save_name, "PU", paste0(PU_size, "km2"),
                                   ClimateLayer_files[i], sep = "_")))
  }
} else {
  ClimateLayer_path <- "Data/Climate/ClimateMetrics/ClimateVelocity/"
  ClimateLayer_files <- list.files(ClimateLayer_path)
  
  velocity_SSP126 <- readRDS(file.path("Output", 
                                       paste(save_name, "PU", paste0(PU_size, "km2"),
                                             ClimateLayer_files[1], sep = "_")))
  velocity_SSP245 <- readRDS(file.path("Output", 
                                       paste(save_name, "PU", paste0(PU_size, "km2"),
                                             ClimateLayer_files[2], sep = "_")))
  velocity_SSP585 <- readRDS(file.path("Output", 
                                       paste(save_name, "PU", paste0(PU_size, "km2"),
                                             ClimateLayer_files[3], sep = "_")))
}

# Plot Climate velocity
(gg_velocity_SSP126 <- fSpatPlan_PlotClimate(ClimateLayer = velocity_SSP126, world = land, metric = "velocity") + theme(axis.text = element_text(size = 25)))
(gg_velocity_SSP245 <- fSpatPlan_PlotClimate(ClimateLayer = velocity_SSP245, world = land, metric = "velocity") + theme(axis.text = element_text(size = 25)))
(gg_velocity_SSP585 <- fSpatPlan_PlotClimate(ClimateLayer = velocity_SSP585, world = land, metric = "velocity") + theme(axis.text = element_text(size = 25)))

#### Multi-Model Ensemble: Climate Velocity ####
if(reprocess) {
  print("Open SpatPlan_ClimateMetrics.R")
  }

} else {
  ClimateLayer_path <- "Data/Climate/ClimateMetrics_Ensemble/velocity/SSP 5-8.5/"
  ClimateLayer_files <- list.files(ClimateLayer_path)
  
  velocity_CanESM5 <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[1], sep = "_")))
  `velocity_CMCC-ESM2` <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[2], sep = "_")))
  `velocity_GFDL-ESM4` <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[3], sep = "_")))
  `velocity_IPSL-CM6A-LR` <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[4], sep = "_")))

  `velocity_NorESM2-MM` <- readRDS(file.path("Output", 
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[5], sep = "_")))
}

# Plot Climate Velocity per model
(gg_velocity_CanESM5 <- fSpatPlan_PlotClimate(ClimateLayer = velocity_CanESM5, world = land, metric = "velocity") + theme(axis.text = element_text(size = 25)))
(`gg_velocity_CMCC-ESM2` <- fSpatPlan_PlotClimate(ClimateLayer = `velocity_CMCC-ESM2`, world = land, metric = "velocity") + theme(axis.text = element_text(size = 25)))
(`gg_velocity_GFDL-ESM4` <- fSpatPlan_PlotClimate(ClimateLayer = `velocity_GFDL-ESM4`, world = land, metric = "velocity") + theme(axis.text = element_text(size = 25)))
(`gg_velocity_IPSL-CM6A-LR` <- fSpatPlan_PlotClimate(ClimateLayer = `velocity_IPSL-CM6A-LR`, world = land, metric = "velocity") + theme(axis.text = element_text(size = 25)))
(`gg_velocity_NorESM2-MM` <- fSpatPlan_PlotClimate(ClimateLayer = `velocity_NorESM2-MM`, world = land, metric = "velocity") + theme(axis.text = element_text(size = 25)))
