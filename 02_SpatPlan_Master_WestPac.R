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
saveRDS(PUs, file.path("Output", paste(save_name, paste0("PlanningRegion.rds"), sep = "_")))
} else {
  PUs <- read_rds(file.path("Output", paste(save_name, paste0("PlanningRegion.rds"), sep = "_")))
}

land <- ne_countries(scale = 'large', returnclass = 'sf') %>% 
  fSpatPlan_Convert2PacificRobinson() 
# Plotting the planning region
(ggPU <- fSpatPlan_PlotPUs(PUs, land) + theme(axis.text = element_text(size = 25)))
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
(ggFeatureNo <- fSpatPlan_FeatureNo(aqua_sf, land) + theme(axis.text = element_text(size = 25)))
ggsave("Layer_AquaMaps.png",
       plot = ggFeatureNo, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Creating Cost Layer ####
if(reprocess){
  Cost <- fSpatPlan_Get_Cost(PUs, cCRS, group = "all") # set group = all, if including all functional groups
  saveRDS(Cost, file.path("Output", paste(save_name, paste0("Cost.rds"), sep = "_"))) # Save rds so you don't have to reprocess everytime
} else {
  Cost <- read_rds(file.path("Output", paste(save_name, paste0("Cost.rds"), sep = "_")))
}

# Squish the cost layers
Cost_squish <- Cost %>% 
  mutate(Cost_squish = scales::oob_squish(Cost, quantile(Cost, c(0.01, 0.99))))

# Plotting the cost layer
ggCost <- fSpatPlan_PlotCost(Cost, land)

#### Creating climate layers ####
# Climate models considered here are: 1) CanESM5, 2) CMCC-ESM2, 3) GFDL-ESM4, 4) IPSL-CM6A-LR, and 5) NorESM2-MM
# Climate variables considered: 1) tos, 2) o2os, and 3) phos from 2015-2100
# Climate scenarios: 1) SSP 1-2.6, 2) SSP 2-4.5, 3) SSP 5-8.5

scenario_path <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
scenario_object <- c("SSP126", "SSP245", "SSP585")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

#### Ensemble Mean: Rate of Change of Temperature ####
ClimateLayer_path <- "Data/Climate/ClimateMetrics/RateOfChange/tos/"
ClimateLayer_files <- list.files(ClimateLayer_path)

if(reprocess) {
  for (i in 1:length(ClimateLayer_files)){
    scenario <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
    climate_layer <- readRDS(paste0(ClimateLayer_path, ClimateLayer_files[i]))
    layer <- fSpatPlan_Get_ClimateLayer(PUs, climate_layer, cCRS, metric = "roc_tos", colname = "slpTrends")
    
    saveRDS(layer, file.path("Output", 
                             paste(save_name, "ClimateLayer", ClimateLayer_files[i], sep = "_")))
    
    print(paste0("Saved EM", ": ", scenario[i]))
  }

} else {
  scenario_object <- c("SSP126", "SSP245", "SSP585")
  
  for(i in 1:length(scenario_object)) {
    df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer", ClimateLayer_files[i], sep = "_")))
    
    assign(x = paste0("roc_tos_", scenario_object[i]), value = df, envir = .GlobalEnv)
  }

}

# Plot Climate Warming Rates
(gg_roc_tos_SSP126 <- fSpatPlan_PlotClimate(ClimateLayer = roc_tos_SSP126, world = land, metric = "roc_tos") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofClimateWarming_SSP126.png",
       plot = gg_roc_tos_SSP126, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(gg_roc_tos_SSP245 <- fSpatPlan_PlotClimate(ClimateLayer = roc_tos_SSP245, world = land, metric = "roc_tos") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofClimateWarming_SSP245.png",
       plot = gg_roc_tos_SSP245, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(gg_roc_tos_SSP585 <- fSpatPlan_PlotClimate(ClimateLayer = roc_tos_SSP585, world = land, metric = "roc_tos") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofClimateWarming_SSP585.png",
       plot = gg_roc_tos_SSP585, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Multi-Model Ensemble: Rate of Change of Temperature ####
path <- "Data/Climate/ClimateMetrics_Ensemble/tos"

if(reprocess) {
  for (i in 1:length(scenario_path)) {
    ClimateLayer_files <- list.files(file.path(path, scenario_path[i]))
    
    for (j in 1:length(ClimateLayer_files)) {
      climate <- readRDS(file.path(path, scenario_path[i], ClimateLayer_files[j]))
      
      layer <- fSpatPlan_Get_ClimateLayer(PUs, climate, cCRS, metric = "roc_tos_ensemble", colname = "slpTrends")
      
      saveRDS(layer, file.path("Output", 
                               paste(save_name, "ClimateLayer", ClimateLayer_files[j], sep = "_")))
      print(paste0("Saved ", scenario_path[i], ": ", model_list[j]))
    }
  }

} else {
  for (i in 1:length(scenario_path)) {
    ClimateLayer_files <- list.files(file.path(path, scenario_path[i]))
    
    for (j in 1:length(ClimateLayer_files)) {
      df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer",
                                              ClimateLayer_files[j], sep = "_")))
      assign(x = paste(metric = "tos", model_list[j], scenario_obj[i], sep = "_"), value = df, envir=.GlobalEnv)
    }
  }
}

# Plot Climate Warming Rates per model (SSP 5-8.5)
(gg_tos_CanESM5 <- fSpatPlan_PlotClimate(ClimateLayer = tos_CanESM5_SSP585, world = land, metric = "roc_tos") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofClimateWarming_CanESM5_SSP585.png",
       plot = gg_tos_CanESM5, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(`gg_tos_CMCC-ESM2` <- fSpatPlan_PlotClimate(ClimateLayer = `tos_CMCC-ESM2_SSP585`, world = land, metric = "roc_tos") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofClimateWarming_CMCC-ESM2_SSP585.png",
       plot = `gg_tos_CMCC-ESM2`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(`gg_tos_GFDL-ESM4` <- fSpatPlan_PlotClimate(ClimateLayer = `tos_GFDL-ESM4_SSP585`, world = land, metric = "roc_tos") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofClimateWarming_GFDL-ESM4_SSP585.png",
       plot = `gg_tos_GFDL-ESM4`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(`gg_tos_IPSL-CM6A-LR` <- fSpatPlan_PlotClimate(ClimateLayer = `tos_IPSL-CM6A-LR_SSP585`, world = land, metric = "roc_tos") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofClimateWarming_IPSL-CM6A-LR_SSP585.png",
       plot = `gg_tos_IPSL-CM6A-LR`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(`gg_tos_NorESM2-MM` <- fSpatPlan_PlotClimate(ClimateLayer = `tos_NorESM2-MM_SSP585`, world = land, metric = "roc_tos") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofClimateWarming_NorESM2-MM_SSP585.png",
       plot = `gg_tos_NorESM2-MM`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Ensemble Mean: Rate of Change of pH ####
ClimateLayer_path <- "Data/Climate/ClimateMetrics/RateOfChange/phos/"
ClimateLayer_files <- list.files(ClimateLayer_path)
if(reprocess) {
  for (i in 1:length(ClimateLayer_files)){
    scenario <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
    climate_layer <- readRDS(paste0(ClimateLayer_path, ClimateLayer_files[i]))
    layer <- fSpatPlan_Get_ClimateLayer(PUs, climate_layer, cCRS, metric = "roc_phos", colname = "slpTrends")
    
    saveRDS(layer, file.path("Output", 
                             paste(save_name, "ClimateLayer", ClimateLayer_files[i], sep = "_")))
    print(paste0("Saved EM", ": ", scenario[i]))
  }
} else {
  scenario_object <- c("SSP126", "SSP245", "SSP585")
  
  for(i in 1:length(scenario_object)) {
    df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer", ClimateLayer_files[i], sep = "_")))
    
    assign(x = paste0("roc_phos_", scenario_object[i]), value = df, envir = .GlobalEnv)
  }
}

# Plot Rates of Change in pH
(gg_roc_phos_SSP126 <- fSpatPlan_PlotClimate(ClimateLayer = roc_phos_SSP126, world = land, metric = "roc_phos") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofOceanAcidification_SSP126.png",
       plot = gg_roc_phos_SSP126, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(gg_roc_phos_SSP245 <- fSpatPlan_PlotClimate(ClimateLayer = roc_phos_SSP245, world = land, metric = "roc_phos") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofOceanAcidification_SSP245.png",
       plot = gg_roc_phos_SSP245, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(gg_roc_phos_SSP585 <- fSpatPlan_PlotClimate(ClimateLayer = roc_phos_SSP585, world = land, metric = "roc_phos") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofOceanAcidification_SSP585.png",
       plot = gg_roc_phos_SSP585, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Multi-Model Ensemble: Rate of Change of pH ####
path <- "Data/Climate/ClimateMetrics_Ensemble/phos"

if(reprocess) {
  for (i in 1:length(scenario_path)) {
    ClimateLayer_files <- list.files(file.path(path, scenario_path[i]))
    
    for (j in 1:length(ClimateLayer_files)) {
      climate <- readRDS(file.path(path, scenario_path[i], ClimateLayer_files[j]))
      
      layer <- fSpatPlan_Get_ClimateLayer(PUs, climate, cCRS, metric = "roc_phos_ensemble", colname = "slpTrends")
      
      saveRDS(layer, file.path("Output", 
                               paste(save_name, "ClimateLayer", ClimateLayer_files[j], sep = "_")))
      
      print(paste0("Saved ", scenario_path[i], ": ", model_list[j]))
    }
  }
  
} else {
  for (i in 1:length(scenario_path)) {
    ClimateLayer_files <- list.files(file.path(path, scenario_path[i]))
    
    for (j in 1:length(ClimateLayer_files)) {
      df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer",
                                              ClimateLayer_files[j], sep = "_")))
      assign(x = paste(metric = "phos", model_list[j], scenario_object[i], sep = "_"), value = df, envir=.GlobalEnv)
    }
  }
}

# Plot Ocean Acidification Rates per model
(gg_phos_CanESM5 <- fSpatPlan_PlotClimate(ClimateLayer = phos_CanESM5_SSP585, world = land, metric = "roc_phos") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofOceanAcidification_CanESM5_SSP585.png",
         plot = gg_phos_CanESM5, width = 21, height = 29.7, dpi = 300,
         path = "Figures/")
  
(`gg_phos_CMCC-ESM2` <- fSpatPlan_PlotClimate(ClimateLayer = `phos_CMCC-ESM2_SSP585`, world = land, metric = "roc_phos") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofOceanAcidification_CMCC-ESM2_SSP585.png",
       plot = `gg_phos_CMCC-ESM2`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(`gg_phos_GFDL-ESM4` <- fSpatPlan_PlotClimate(ClimateLayer = `phos_GFDL-ESM4_SSP585`, world = land, metric = "roc_phos") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofOceanAcidification_GFDL-ESM4_SSP585.png",
       plot = `gg_phos_GFDL-ESM4`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(`gg_phos_IPSL-CM6A-LR` <- fSpatPlan_PlotClimate(ClimateLayer = `phos_IPSL-CM6A-LR_SSP585`, world = land, metric = "roc_phos") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofOceanAcidification_IPSL-CM6A-LR_SSP585.png",
       plot = `gg_phos_IPSL-CM6A-LR`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(`gg_phos_NorESM2-MM` <- fSpatPlan_PlotClimate(ClimateLayer = `phos_NorESM2-MM_SSP585`, world = land, metric = "roc_phos") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofOceanAcidification_NorESM2-MM_SSP585.png",
       plot = `gg_phos_NorESM2-MM`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Ensemble Mean: Rate of Change of O2 ####
ClimateLayer_path <- "Data/Climate/ClimateMetrics/RateOfChange/o2os/"
ClimateLayer_files <- list.files(ClimateLayer_path)
if(reprocess) {
  for (i in 1:length(ClimateLayer_files)){
    scenario <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
    climate_layer <- readRDS(paste0(ClimateLayer_path, ClimateLayer_files[i]))
    layer <- fSpatPlan_Get_ClimateLayer(PUs, climate_layer, cCRS, metric = "roc_o2os", colname = "slpTrends")
    
    saveRDS(layer, file.path("Output", 
                             paste(save_name, "ClimateLayer", ClimateLayer_files[i], sep = "_")))
    print(paste0("Saved EM", ": ", scenario[i]))
  }
} else {
  scenario_object <- c("SSP126", "SSP245", "SSP585")
  
  for(i in 1:length(scenario_object)) {
    df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer", ClimateLayer_files[i], sep = "_")))
    
    assign(x = paste0("roc_o2os_", scenario_object[i]), value = df, envir = .GlobalEnv)
  }
}

# Plot Rates of Declining Oxygen Concentration
(gg_roc_o2os_SSP126 <- fSpatPlan_PlotClimate(ClimateLayer = roc_o2os_SSP126, world = land, metric = "roc_o2os") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofDecliningOxygenConcentration_SSP126.png",
       plot = gg_roc_o2os_SSP126, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(gg_roc_o2os_SSP245 <- fSpatPlan_PlotClimate(ClimateLayer = roc_o2os_SSP245, world = land, metric = "roc_o2os") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofDecliningOxygenConcentration_SSP245.png",
       plot = gg_roc_o2os_SSP245, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(gg_roc_o2os_SSP585 <- fSpatPlan_PlotClimate(ClimateLayer = roc_o2os_SSP585, world = land, metric = "roc_o2os") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofDecliningOxygenConcentration_SSP585.png",
       plot = gg_roc_o2os_SSP585, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Multi-Model Ensemble: Rate of Change of Oxygen Concentration ####
path <- "Data/Climate/ClimateMetrics_Ensemble/o2os"

if(reprocess) {
  for (i in 1:length(scenario_path)) {
    ClimateLayer_files <- list.files(file.path(path, scenario_path[i]))
    
    for (j in 1:length(ClimateLayer_files)) {
      climate <- readRDS(file.path(path, scenario_path[i], ClimateLayer_files[j]))
      
      layer <- fSpatPlan_Get_ClimateLayer(PUs, climate, cCRS, metric = "roc_o2os_ensemble", colname = "slpTrends")
      
      saveRDS(layer, file.path("Output", 
                               paste(save_name, "ClimateLayer", ClimateLayer_files[j], sep = "_")))
      
      print(paste0("Saved ", scenario_path[i], ": ", model_list[j]))
    }
  }
  
} else {
  for (i in 1:length(scenario_path)) {
    ClimateLayer_files <- list.files(file.path(path, scenario_path[i]))
    
    for (j in 1:length(ClimateLayer_files)) {
      df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer",
                                              ClimateLayer_files[j], sep = "_")))
      assign(x = paste(metric = "o2os", model_list[j], scenario_obj[i], sep = "_"), value = df, envir=.GlobalEnv)
    }
  }
}

# Plot Declining Oxygen Concentration Rates per model
(gg_o2os_CanESM5 <- fSpatPlan_PlotClimate(ClimateLayer = o2os_CanESM5_SSP585, world = land, metric = "roc_o2os") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofDecliningOxygenConcentration_CanESM5_SSP585.png",
       plot = gg_o2os_CanESM5, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(`gg_o2os_CMCC-ESM2` <- fSpatPlan_PlotClimate(ClimateLayer = `o2os_CMCC-ESM2_SSP585`, world = land, metric = "roc_o2os") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofDecliningOxygenConcentration_CMCC-ESM2_SSP585.png",
       plot = `gg_o2os_CMCC-ESM2`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(`gg_o2os_GFDL-ESM4` <- fSpatPlan_PlotClimate(ClimateLayer = `o2os_GFDL-ESM4_SSP585`, world = land, metric = "roc_o2os") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofDecliningOxygenConcentration_GFDL-ESM4_SSP585.png",
       plot = `gg_o2os_GFDL-ESM4`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(`gg_o2os_IPSL-CM6A-LR` <- fSpatPlan_PlotClimate(ClimateLayer = `o2os_IPSL-CM6A-LR_SSP585`, world = land, metric = "roc_o2os") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofDecliningOxygenConcentration_IPSL-CM6A-LR_SSP585.png",
       plot = `gg_o2os_IPSL-CM6A-LR`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(`gg_o2os_NorESM2-MM` <- fSpatPlan_PlotClimate(ClimateLayer = `o2os_NorESM2-MM_SSP585`, world = land, metric = "roc_o2os") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_RateofDecliningOxygenConcentration_NorESM2-MM_SSP585.png",
       plot = `gg_o2os_NorESM2-MM`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Ensemble Mean: Climate Velocity ####
ClimateLayer_path <- "Data/Climate/ClimateMetrics/ClimateVelocity/"
ClimateLayer_files <- list.files(ClimateLayer_path)
if(reprocess) {
  scenario <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
  for (i in 1:length(ClimateLayer_files)){
    climate_layer <- readRDS(paste0(ClimateLayer_path, ClimateLayer_files[i]))
    layer <- fSpatPlan_Get_ClimateLayer(PUs, climate_layer, cCRS, metric = "velocity", colname = "voccMag")
    
    saveRDS(layer, file.path("Output", 
                             paste(save_name, "ClimateLayer", ClimateLayer_files[i], sep = "_")))
    
    print(paste0("Saved EM", ": ", scenario[i]))
  }
} else {
  scenario_object <- c("SSP126", "SSP245", "SSP585")
  
  for(i in 1:length(scenario_object)) {
    df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer", ClimateLayer_files[i], sep = "_")))
    
    assign(x = paste0("velocity_", scenario_object[i]), value = df, envir = .GlobalEnv)
  }
}

# Plot Climate velocity
(gg_velocity_SSP126 <- fSpatPlan_PlotClimate(ClimateLayer = velocity_SSP126, world = land, metric = "velocity") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_ClimateVelocity_SSP126.png",
       plot = gg_velocity_SSP126, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(gg_velocity_SSP245 <- fSpatPlan_PlotClimate(ClimateLayer = velocity_SSP245, world = land, metric = "velocity") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_ClimateVelocity_SSP245.png",
       plot = gg_velocity_SSP245, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(gg_velocity_SSP585 <- fSpatPlan_PlotClimate(ClimateLayer = velocity_SSP585, world = land, metric = "velocity") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_ClimateVelocity_SSP585.png",
       plot = gg_velocity_SSP585, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Multi-Model Ensemble: Climate Velocity ####
path <- "Data/Climate/ClimateMetrics_Ensemble/velocity"

if(reprocess) {
  for (i in 1:length(scenario_path)) {
    ClimateLayer_files <- list.files(file.path(path, scenario_path[i]))
    
    for (j in 1:length(ClimateLayer_files)) {
      climate <- readRDS(file.path(path, scenario_path[i], ClimateLayer_files[j]))
      
      layer <- fSpatPlan_Get_ClimateLayer(PUs, climate, cCRS, metric = "velocity_ensemble", colname = "voccMag")
      
      saveRDS(layer, file.path("Output", 
                               paste(save_name, "ClimateLayer", ClimateLayer_files[j], sep = "_")))
      
      print(paste0("Saved ", scenario_path[i], ": ", model_list[j]))
    }
  }
  
} else {
  for (i in 1:length(scenario_path)) {
    ClimateLayer_files <- list.files(file.path(path, scenario_path[i]))
    
    for (j in 1:length(ClimateLayer_files)) {
      df <- readRDS(file.path("Output", paste(save_name, "ClimateLayer",
                                              ClimateLayer_files[j], sep = "_")))
      assign(x = paste(metric = "velocity", model_list[j], scenario_obj[i], sep = "_"), value = df, envir=.GlobalEnv)
    }
  }
}

# Plot Climate Velocity per model
(gg_velocity_CanESM5 <- fSpatPlan_PlotClimate(ClimateLayer = velocity_CanESM5_SSP585, world = land, metric = "velocity") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_ClimateVelocity_CanESM5_SSP585.png",
       plot = gg_velocity_CanESM5, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(`gg_velocity_CMCC-ESM2` <- fSpatPlan_PlotClimate(ClimateLayer = `velocity_CMCC-ESM2_SSP585`, world = land, metric = "velocity") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_ClimateVelocity_CMCC-ESM2_SSP585.png",
       plot = `gg_velocity_CMCC-ESM2`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(`gg_velocity_GFDL-ESM4` <- fSpatPlan_PlotClimate(ClimateLayer = `velocity_GFDL-ESM4_SSP585`, world = land, metric = "velocity") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_ClimateVelocity_GFDL-ESM4_SSP585.png",
       plot = `gg_velocity_GFDL-ESM4`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(`gg_velocity_IPSL-CM6A-LR` <- fSpatPlan_PlotClimate(ClimateLayer = `velocity_IPSL-CM6A-LR_SSP585`, world = land, metric = "velocity") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_ClimateVelocity_IPSL-CM6A-LR_SSP585.png",
       plot = `gg_velocity_IPSL-CM6A-LR`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

(`gg_velocity_NorESM2-MM` <- fSpatPlan_PlotClimate(ClimateLayer = `velocity_NorESM2-MM_SSP585`, world = land, metric = "velocity") + theme(axis.text = element_text(size = 25)))
ggsave("Layer_ClimateVelocity_NorESM2-MM_SSP585.png",
       plot = `gg_velocity_NorESM2-MM`, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")


#### Ensemble Mean: MHW ####
ClimateLayer_path <- "Data/Climate/ClimateMetrics/MHW/"
ClimateLayer_files <- list.files(ClimateLayer_path)

ID <- tibble(column = character(),
             metric = character(),
             save = character())
ID %<>% add_row(column = "trend_n", metric = "MHW_num", save = "(Trend Num)") %>% 
  add_row(column = "trend_peak_int", metric = "MHW_PeakInt", save = "(Peak Intensity)") %>% 
  add_row(column = "trend_cum_int", metric = "MHW_CumInt", save = "(Cumulative Intensity") %>% 
  add_row(column = "trend_dur", metric = "MHW_Dur", save = "(Mean Duration)") %>% 
  add_row(column = "trend_cum_dur", metric = "MHW_CumDur", save = "(Cumulative Duration)") %>% 
  add_row(column = "sum_cum_int", metric = "MHW_SumCumInt", save = "(Sum Cumulative Intensity")

if(reprocess) {
  scenario <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
  
  for (i in 1:length(ClimateLayer_files)){
    for (j in 1:nrow(ID)) {
      climate_layer <- readRDS(paste0(ClimateLayer_path, ClimateLayer_files[i]))
      layer <- fSpatPlan_Get_ClimateLayer(PUs, climate_layer, cCRS, metric = "MHW", colname = ID$column[j])
      
      saveRDS(layer, file.path("Output", 
                               paste(save_name, "ClimateLayer", ID$metric[j], paste0(scenario[i], ".rds"), sep = "_")))
      
      print(paste0("Saved EM", ": ", scenario[i], ID$save[j]))
    }
    
  }
} else {
  scenario_object <- c("SSP126", "SSP245", "SSP585")
  
  for(i in 1:length(scenario_object)) {
    for(j in 1:nrow(ID)) {
      
      df <- readRDS(file.path("Output", 
                              paste(save_name, "ClimateLayer", ID$metric[j], paste0(scenario[i], ".rds"), sep = "_")))
      
      assign(x = paste0(ID$metric[j], "_", scenario_object[i]), value = df, envir = .GlobalEnv)
      
    }
  }
}

# Plot MHW metrics
scenario_object <- c("SSP126", "SSP245", "SSP585")

for(j in 1:length(scenario_object)) {
  for(i in 1:nrow(ID)) {
    x <- get(paste0(ID$metric[i], "_", scenario_object[j]))
    gg_MHW <- fSpatPlan_PlotClimate(ClimateLayer = x, 
                                    world = land,
                                    metric = ID$metric[i]) +
      theme(axis.text = element_text(size = 25))
    ggsave(paste0(paste("Layer", ID$metric[i], scenario_object[j], sep="_"), ".png"),
           plot = gg_MHW, width = 21, height = 29.7, dpi = 300,
           path = "Figures/")
  }
}

#### Multi-Model Ensemble: Climate Velocity ####
path <- "Data/Climate/ClimateMetrics_Ensemble/"
model <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

if(reprocess) {
  for(i in 1:length(scenario_path)) {
    ClimateLayer_files <- list.files(file.path(path, "MHW",scenario_path[i]))
    
    for (j in 1:length(ClimateLayer_files)){
      for (k in 1:nrow(ID)) {
        climate_layer <- readRDS(paste0(path, "MHW/", scenario_path[i], "/", ClimateLayer_files[j]))
        layer <- fSpatPlan_Get_ClimateLayer(PUs, climate_layer, cCRS, metric = "MHW", colname = ID$column[k])
        
        saveRDS(layer, file.path("Output", 
                                 paste(save_name, "ClimateLayer", ID$metric[k], model[j], paste0(scenario[i], ".rds"), sep = "_")))
        
        print(paste0("Saved MM: ", scenario_path[i], ": ", model_list[j], ID$save[k]))
      }
    }
  }

} else {
  for(i in 1:length(scenario_path)) {
    ClimateLayer_files <- list.files(file.path(path, "MHW", scenario_path[i]))
    
    for (j in 1:length(ClimateLayer_files)){
      for (k in 1:nrow(ID)) {
        
        df <- readRDS(file.path("Output", 
                                paste(save_name, "ClimateLayer", ID$metric[k], model[j], paste0(scenario[i], ".rds"), sep = "_")))
        assign(x = paste(ID$metric[k], model[j], scenario_object[i], sep = "_"), value = df, envir=.GlobalEnv)
        
      }
    }
  }
      
}