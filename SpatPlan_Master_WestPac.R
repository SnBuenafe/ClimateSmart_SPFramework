#' ---
#' title: "Create a Spatial Plan for the Western Pacific"
#' author: "Tin Buenafe"
#' affiliation: "UQ"
#' date: "Last compiled on `r format(Sys.time(), '%A %d %B %Y')`"
#' output: 
#'   html_document
#' ---

##+ setup, include=FALSE
knitr::opts_chunk$set(warning=FALSE, cache=FALSE, message=FALSE, error=FALSE)
# knitr::opts_chunk$set(collapse = TRUE, comment = "", warning = "off")

#' ## Overview
#' #' This code follows the workflow of _SpatialPlanning_ repository to create a climate-smart Marine Spatial Plan for the Western Pacific.
#' The code depends on `sf`, `terra`, `tidyverse`, `rnaturalearth`, `prioritizr`, `VoCC`, `stars`, `patchwork`.
#' 
#' This code creates the layers necessary for answering the research questions of this project. Analyses are ran in `SpatPlan_WestPac_Runs.R`
#' 
#' To use this code, you will need to download and expand `MME1DATA-Q1215/SpatialPlanning/Data.zip` to the directory `GitHub/SpatialPlanning/Data/`. Note that the download is only 2GB, but the expanded data is 35 GB in size. If you need help subsetting the data to your region due to memory or HD space constraints, contact Jason.
#' 
#' To use this code, you will need to download and expand `MME1DATA-Q1215/SpatialPlanning/Data.zip` to the directory `GitHub/SpatialPlanning/Data/`. Note that the download is only 2GB, but the expanded data is 35 GB in size. If you need help subsetting the data to your region due to memory or HD space constraints, contact Jason.
#' 
#' ## Preliminaries 
source("HelperFunctions/SpatPlan_Extras.R") # Load the extras, including functions and libraries
source("HelperFunctions/SpatPlan_HelperFxns_WestPac.R") # Load helper functions written specifically for this spatial planning project

if (!file.exists("Data")) {
  stop("The Data folder does not exist at SpatialPlanning/Data. Please download from the RDM and then try again.")
}

reprocess <- FALSE # Do we want to reprocess the PUs/AquaMaps data or use saved versions

#' ### Setting user parameters 

# Calling the region
Region <- "WestPacific"
save_name <- "WestPacific"

PU_size = 669.9 # km2 (0.25 deg at equator)
Shape <- "Hexagon" # "Shape of PUs
MinDepth <- 0
MaxDepth <- 200
CO <- 0.5

#' Choose CRS for analysis
cCRS <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" # Robinson: Pacific-centred

#' ## Creating planning region
# If Region = WestPacific, demarcating boundary and removing land areas would be different
# The object "world" here represents the planning region (inverse = FALSE)
world <- fSpatPlan_Get_Boundary(Region, cCRS) %>% 
  fSpatPlan_Convert2PacificRobinson() # project to Pacific Robinson
inverse = FALSE

# Set limits of the boundary
Limits = c(xmin = -120, xmax = 130, ymax = 30, ymin = -55)
Bndry <- fSpatPlan_Get_Boundary(Limits, cCRS)

#' ### Create planning units
if(reprocess){
PUs <- fSpatPlan_Get_PlanningUnits(Bndry, world, PU_size, Shape, inverse)
saveRDS(PUs, file.path("Output", paste(save_name, "PU", paste0(PU_size,"km2"), "Output.rds", sep = "_")))
} else {
  PUs <- read_rds(file.path("Output", paste(save_name, "PU", paste0(PU_size,"km2"), "Output.rds", sep = "_")))
}

# source("fSpatPlan_Convert2PacificRobinson.R")
land <- ne_countries(scale = 'large', returnclass = 'sf') %>% 
  fSpatPlan_Convert2PacificRobinson() 
#' ### Plotting the planning region
(ggPU <- fSpatPlan_PlotPUs(PUs, land))

#' ## AquaMaps Conservation Features
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
#' ### Plotting the # of features in AquaMaps
(ggFeatureNo <- fSpatPlan_FeatureNo(aqua_sf, land))

#' ## Cost Layer

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

#' ### Plotting the cost layer
(ggCost <- fSpatPlan_PlotCost(Cost, land))

#' ## Climate Layers
# Rate of Change of Temperature
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

# Rate of Change of pH
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

# Rate of Change of O2
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

#' ### Plot the climate layers
# from and to represent the range of values across the scenarios
#' Rates of Change in Temperature
(gg_roc_tos_SSP126 <- fSpatPlan_PlotClimate(roc_tos_SSP126, land, metric = "roc_tos", from = 0, to = 0.05))
(gg_roc_tos_SSP245 <- fSpatPlan_PlotClimate(roc_tos_SSP245, land, metric = "roc_tos", from = 0, to = 0.05))
(gg_roc_tos_SSP585 <- fSpatPlan_PlotClimate(roc_tos_SSP585, land, metric = "roc_tos", from = 0, to = 0.05))

#' Rates of Change in pH
(gg_roc_phos_SSP126 <- fSpatPlan_PlotClimate(roc_phos_SSP126, land, metric = "roc_phos", from = -0.005, to = 0))
(gg_roc_phos_SSP245 <- fSpatPlan_PlotClimate(roc_phos_SSP245, land, metric = "roc_phos", from = -0.005, to = 0))
(gg_roc_phos_SSP585 <- fSpatPlan_PlotClimate(roc_phos_SSP585, land, metric = "roc_phos", from = -0.005, to = 0))

#' Rates of Change in Oxygen
(gg_roc_o2os_SSP126 <- fSpatPlan_PlotClimate(roc_o2os_SSP126, land, metric = "roc_o2os", from = -0.0002, to = 9.08e-05))
(gg_roc_o2os_SSP245 <- fSpatPlan_PlotClimate(roc_o2os_SSP245, land, metric = "roc_o2os", from = -0.0002, to = 9.08e-05))
(gg_roc_o2os_SSP585 <- fSpatPlan_PlotClimate(roc_o2os_SSP585, land, metric = "roc_o2os", from = -0.0002, to = 9.08e-05))

#' Climate velocity
(gg_velocity_SSP126 <- fSpatPlan_PlotClimate(velocity_SSP126, land, metric = "velocity", from = 0, to = 200))
(gg_velocity_SSP245 <- fSpatPlan_PlotClimate(velocity_SSP245, land, metric = "velocity", from = 0, to = 200))
(gg_velocity_SSP585 <- fSpatPlan_PlotClimate(velocity_SSP585, land, metric = "velocity", from = 0, to = 200))