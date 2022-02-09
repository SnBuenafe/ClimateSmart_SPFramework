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
#' To use this code, you will need to download and expand `MME1DATA-Q1215/SpatialPlanning/Data.zip` to the directory `GitHub/SpatialPlanning/Data/`. Note that the download is only 2GB, but the expanded data is 35 GB in size. If you need help subsetting the data to your region due to memory or HD space constraints, contact Jason.
#' 
#' To use this code, you will need to download and expand `MME1DATA-Q1215/SpatialPlanning/Data.zip` to the directory `GitHub/SpatialPlanning/Data/`. Note that the download is only 2GB, but the expanded data is 35 GB in size. If you need help subsetting the data to your region due to memory or HD space constraints, contact Jason.
#' 
#' ## Preliminaries 
source("SpatPlan_Extras.R") # Load the extras, including functions and libraries
source("SpatPlan_HelperFxns_WestPac.R") # Load helper functions written specifically for this spatial planning project
# source("SpatPlan_Process_AquaMaps.R") # This script reprocesses AquaMaps. WARNING: Lots of time (10s hrs) and memory (10s GB)
# source("SpatPlan_Process_MPAs.R") # Only run if you need to reprocess the MPA data. You will need the WDPA files

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
#cCRS <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

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

#' ## Assigning targets
minTarget = 0.2
maxTarget = 0.8

#' ### Inverse area targets 
Targets <- fSpatPlan_Get_TargetsIA(aqua_sf, minTarget, maxTarget)
# apparently there are some NAs? very weird
Targets %>% filter(is.na(Targets$Target)) # getting the list of the species
# changing NAs to zeros?
temp_aqua_sf <- aqua_sf
subset_aqua_sf <- aqua_sf %>% select(Doryrhamphus_excisus.excisus, Padina_sanctae.crucis, Platybelone_argalus.platyura,
                   Tylosurus_acus.acus, Tylosurus_acus.melanotus) %>% 
  as_tibble %>% 
  select(-geometry)
aqua_sf <- temp_aqua_sf %>% 
  mutate_at(vars(colnames(subset_aqua_sf)), 
            funs(case_when(. >= CutOff ~ 1,
                           . <= CutOff ~ 0,
                           is.na(.) ~ 0)))
Targets <- fSpatPlan_Get_TargetsIA(temp_aqua_sf1, minTarget, maxTarget)

#' ### Add different targets for IUCN RedList categories 
speciesCol <- "Species"
Targets <- Targets %>% 
   fSpatPlan_Match_IUCNRedList(speciesCol) %>% # Add RL data to the df
   mutate(Target = case_when(IUCN_Category %in% c("EX","EW","CR","EN","VU") ~ 1,
                                                          TRUE ~ Targets$Target))

#' ## PRIORITIZR RUN: Uninformed
#' ### Get the list of features
features <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(-geometry) %>% 
  names()

#' ### Set up the planning problem 
out_sf <- cbind(Cost_squish, aqua_sf)

p1 <- prioritizr::problem(out_sf, features, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(Targets$Target * 0.4) %>% # using 40% as the target percentage of protection
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' ### Solve the planning problem 
s1 <- prioritizr::solve(p1)

## Representation of features
feat_rep <- represent_feature(p1, s1, "uninformed")
head(feat_rep)

## Summary statistics
total_area = nrow(PUs) * PU_size
print(total_area)
summary <- compute_summary(s1, total_area, PU_size, "uninformed", Cost = "Cost_squish")
print(summary)

#' ### Plotting the solution
s1_plot <- s1 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol1 <- fSpatPlan_PlotSolution(s1_plot, PUs, land))

#' ## PRIORITIZR RUN: Climate-smart "percentile" approach (SSP 2-4.5)
#' ## Climate velocity
#' ### Get list of features
## add 50th percentile of Velocity as a feature
climate_layer <- velocity_SSP245 %>% 
  as_tibble() %>% 
  mutate(climate_layer = case_when(voccMag <= quantile(voccMag, 0.5) ~ 1,# we want to conserve those of low velocity
                                   TRUE ~ 0))
features_append <- append(features, "climate_layer")
Targets_append <- Targets %>% 
  mutate(Transformed_Target = Target * 0.40) %>% # using 40% as the target percentage of protection
  add_row(Species = "climate_layer",
          Area_km2 = nrow(velocity_SSP245 %>% as_tibble %>% filter(voccMag <= quantile(voccMag, 0.5))) * PU_size,
          Target = NA,
          Transformed_Target = 0.8, # Using 0.8 here because 50th percentile * 0.8 = 40% protection
          IUCN_Category = NA) 

#' ### Set up the planning problem 
out_sf <- cbind(Cost_squish, aqua_sf, climate_layer)

p2 <- prioritizr::problem(out_sf, features_append, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(Targets_append$Transformed_Target) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' ### Solve the planning problem 
s2 <- prioritizr::solve(p2)

#' ### Plotting the solution
s2_plot <- s2 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol2 <- fSpatPlan_PlotSolution(s2_plot, PUs, land))

## Representation of features
temp <- represent_feature(p2, s2, "velocity_245") %>% 
  mutate(velocity_245 = case_when(feature == "climate_layer" ~ velocity_245*0.5, # calculating the effective protection allotted for the climate layer (40%)
                                   TRUE ~ velocity_245))
feat_rep <- left_join(temp, feat_rep)
head(feat_rep)

## Summary statistics
temp <- compute_summary(s2, total_area, PU_size, "velocity_245", Cost = "Cost_squish")
summary <- rbind(temp, summary)
print(summary)

#' ## Exposure: Temperature
#' ### Get list of features
## add 50th percentile of temp exposure as a feature
climate_layer <- roc_tos_SSP245 %>% 
  as_tibble() %>% 
  mutate(climate_layer = case_when(slpTrends <= quantile(slpTrends, 0.5) ~ 1, # we want to conserve those of low exposure
                                   TRUE ~ 0))
features_append <- append(features, "climate_layer")

Targets_append <- Targets %>% 
  mutate(Transformed_Target = Target * 0.40) %>% # using 40% as the target percentage of protection
  add_row(Species = "climate_layer",
          Area_km2 = nrow(roc_tos_SSP245 %>% as_tibble %>% filter(slpTrends <= quantile(slpTrends, 0.5))) * PU_size,
          Target = NA,
          Transformed_Target = 0.8, # Using 0.8 here because 50th percentile * 0.8 = 40% protection
          IUCN_Category = NA) 

#' ### Set up the planning problem 
out_sf <- cbind(Cost_squish, aqua_sf, climate_layer)

p3 <- prioritizr::problem(out_sf, features_append, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(Targets_append$Transformed_Target) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' ### Solve the planning problem 
s3 <- prioritizr::solve(p3)

#' ### Plotting the solution
s3_plot <- s3 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol3 <- fSpatPlan_PlotSolution(s3_plot, PUs, land))
ggSol2

## Representation of features
temp <- represent_feature(p3, s3, "roc_tos_245") %>% 
  mutate(roc_tos_245 = case_when(feature == "climate_layer" ~ roc_tos_245*0.5, # calculating the effective protection allotted for the climate layer (40%)
                                  TRUE ~ roc_tos_245))
feat_rep <- left_join(temp, feat_rep)
head(feat_rep)

## Summary statistics
temp <- compute_summary(s3, total_area, PU_size, "roc_tos_245", Cost = "Cost_squish")
summary <- rbind(temp, summary)
print(summary)

#' ## Exposure: pH
#' ### Get list of features
## add 50th percentile of pH exposure as a feature
climate_layer <- roc_phos_SSP245 %>% 
  as_tibble() %>% 
  mutate(climate_layer = case_when(slpTrends >= quantile(slpTrends, 0.5) ~ 1, # we want to conserve those of less negative pH
                                   TRUE ~ 0))
features_append <- append(features, "climate_layer")

Targets_append <- Targets %>% 
  mutate(Transformed_Target = Target * 0.40) %>% # using 40% as the target percentage of protection
  add_row(Species = "climate_layer",
          Area_km2 = nrow(roc_phos_SSP245 %>% as_tibble %>% filter(slpTrends >= quantile(slpTrends, 0.5))) * PU_size,
          Target = NA,
          Transformed_Target = 0.8, # Using 0.8 here because 50th percentile * 0.8 = 40% protection
          IUCN_Category = NA) 

#' ### Set up the planning problem 
out_sf <- cbind(Cost_squish, aqua_sf, climate_layer)

p4 <- prioritizr::problem(out_sf, features_append, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(Targets_append$Transformed_Target) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' ### Solve the planning problem 
s4 <- prioritizr::solve(p4)

#' ### Plotting the solution
s4_plot <- s4 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol4 <- fSpatPlan_PlotSolution(s4_plot, PUs, land))

## Representation of features
temp <- represent_feature(p4, s4, "roc_phos_245") %>% 
  mutate(roc_phos_245 = case_when(feature == "climate_layer" ~ roc_phos_245*0.5, # calculating the effective protection allotted for the climate layer (40%)
                                 TRUE ~ roc_phos_245))
feat_rep <- left_join(temp, feat_rep)
head(feat_rep)

## Summary statistics
temp <- compute_summary(s4, total_area, PU_size, "roc_phos_245", Cost = "Cost_squish")
summary <- rbind(temp, summary)
print(summary)

#' ## Exposure: Oxygen
#' ### Get list of features
## add 50th percentile of Oxygen exposure as a feature
climate_layer <- roc_o2os_SSP245 %>% 
  as_tibble() %>% 
  mutate(climate_layer = case_when(slpTrends >= quantile(slpTrends, 0.5) ~ 1, # we want to conserve those of less negative changes in o2
                                   TRUE ~ 0))
features_append <- append(features, "climate_layer")

Targets_append <- Targets %>% 
  mutate(Transformed_Target = Target * 0.40) %>% # using 40% as the target percentage of protection
  add_row(Species = "climate_layer",
          Area_km2 = nrow(roc_o2os_SSP245 %>% as_tibble %>% filter(slpTrends >= quantile(slpTrends, 0.5))) * PU_size,
          Target = NA,
          Transformed_Target = 0.8, # Using 0.8 here because 50th percentile * 0.8 = 40% protection
          IUCN_Category = NA) 

#' ### Set up the planning problem 
out_sf <- cbind(Cost_squish, aqua_sf, climate_layer)

p5 <- prioritizr::problem(out_sf, features_append, "Cost_squish") %>%
  add_min_set_objective() %>%
  add_relative_targets(Targets_append$Transformed_Target) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0, verbose = FALSE)

#' ### Solve the planning problem 
s5 <- prioritizr::solve(p5)

#' ### Plotting the solution
s5_plot <- s5 %>% 
  mutate(solution_1 = as.logical(solution_1)) 
(ggSol5 <- fSpatPlan_PlotSolution(s5_plot, PUs, land))

## Representation of features
temp <- represent_feature(p5, s5, "roc_o2os_245") %>% 
  mutate(roc_o2os_245 = case_when(feature == "climate_layer" ~ roc_o2os_245*0.5, # calculating the effective protection allotted for the climate layer (40%)
                                  TRUE ~ roc_o2os_245))
feat_rep <- left_join(temp, feat_rep)
head(feat_rep)

## Summary statistics
temp <- compute_summary(s5, total_area, PU_size, "roc_o2os_245", Cost = "Cost_squish")
summary <- rbind(temp, summary)
print(summary)

#' ## Compare all spatial plans
## Create Kappa Correlation Matrix
list <- c("uninformed", "velocity_245", "roc_tos_245", "roc_phos_245", "roc_o2os_245")
object_list <- list() # empty list
for (i in 1:length(list)) {
  obj <- select_solution(paste0("s",i), list[i])
  object_list[[i]] <- obj
}

(matrix <- create_corrmatrix(object_list) %>% 
  plot_corrplot(., length(object_list)))

## Compare summary statistics
# Cost
(ggSummary_Cost <- plot_statistics(summary, col_name = "log10(total_cost)", y_axis = "log10(cost)"))
# Area
(ggSummary_Area <- plot_statistics(summary, col_name = "percent_area", y_axis = "% area"))


# (gg <- fSpatPlan_PlotComparison(s3_plot, s2_plot, land)) # TODO: change colors?
