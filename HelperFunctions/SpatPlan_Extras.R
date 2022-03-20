#install.packages("pacman")
# devtools::install_github("JorGarMol/VoCC", dependencies = TRUE, build_vignettes = TRUE)

pacman::p_load(sf, terra, tidyverse, rnaturalearth, prioritizr, stars, patchwork, proj4, magrittr, doParallel, ggridges, viridis,
               BiocManager, ncdf4, PCICt, ncdf4.helpers, VoCC, RColorBrewer)

longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

source("HelperFunctions/fSpatPlan_Get_PlanningUnits.R")
source("HelperFunctions/fSpatPlan_Get_Boundary.R")
source("HelperFunctions/fSpatPlan_Get_AquaMaps.R")
source("HelperFunctions/fSpatPlan_Match_AquaMapsFishBase.R")
source("HelperFunctions/fSpatPlan_Get_Cost.R")
source("HelperFunctions/fSpatPlan_Get_MaskedPolygon.R")
source("HelperFunctions/fSpatPlan_Get_ClimateLayer.R")
source("HelperFunctions/fSpatPlan_Convert2PacificRobinson.R")

source("HelperFunctions/SpatPlan_Plotting.R")

# A function to fix incorrect column types from fishbase and sealifebase
fix_species_type <- function(df, server = "fishbase"){
  
  if(server == "fishbase"){ # Need to convert type of different columns depending on database
    nm <- c("SpecCode", "DepthRangeShallow", "CommonLength", "CommonLengthF", "LongevityWildRef", "MaxLengthRef", "DangerousRef")
  } else if(server == "sealifebase"){
    nm <- c("SpecCode", "SpeciesRefNo", "GenCode", "DepthRangeRef", "LongevityWildRef", "Weight")
  }
  
  df <- df %>% 
    mutate(across(any_of(nm), as.numeric)) # Convert `nm` variables to numeric
}


# Code to loop through and try and validate names with fishbase and sealifebase
#
# Written by Jason D. Everett
# UQ/CSIRO/UNSW
# Last edited 8th Sept 2021

fSpatPlan_Validate_FBNAs <- function(spp, datab){
  spp <- spp %>% 
    mutate(ValidSpecies = NA, valid = FALSE)
  # For some reason we are getting multiple names back for some species. Check which ones....
  for (a in 1:dim(spp)[1]){
    out <- validate_names(spp$Species[a], server = datab)
    if (length(out) == 1 & is.na(out)){ # Maintain original name
      spp$ValidSpecies[a] <- spp$Species[a]
    } else if (length(out) == 1){
      spp$ValidSpecies[a] <- out[1]
      spp$valid[a] <- TRUE
    } else if (length(out) > 1) {
      # First check if any match the original name
      # Sometimes a species comes back as valid, with an alternative. 
      # Here we check if the name was in fact in the output
      out2 <- out[which(str_detect(out,spp$Species[a]))] 
      spp$valid[a] <- TRUE
      if (length(out2) == 1){ # Name existed in the original
        spp$ValidSpecies[a] <- out2    
      } else {
        spp$ValidSpecies[a] <- out[1] # Guess at the first one.
      }
    }
  }
  
  spp <- spp %>% 
    rename(OrigSpecies = Species)
}


# Create ONE BIG polygon that we can use to populate with PUs

# Written by Jason D. Everett
# UQ/CSIRO/UNSW
# Last edited 8th Sept 2021
fSpatPlan_CreateSinglePolygon <- function (df, res){
  
  library(raster)
  library(fasterize)
  
  # Creating a empty raster
  rs <- raster(ncol = 360*(1/res), nrow = 180*(1/res)) 
  rs[] <- 1:ncell(rs)
  crs(rs) <- CRS(longlat)
  
  # Fasterize the land object
  df_rs <- fasterize(df, rs)
  
  pol <- as(df_rs,  "SpatialPolygonsDataFrame")
  pol$layer <- seq(1, length(pol))
  
  # Now to a sf object and create ONE BIG polygon that we can use to populate with PUs
  pol_sf <- st_as_sf(pol) %>% 
    dplyr::select(layer) %>% 
    summarise(total_layer = sum(layer, do_union = TRUE))
}


# Ensure all features are in the same order.
#
# Written by Jason D. Everett
# UQ/CSIRO/UNSW
# Last edited 8th Sept 2021
fSpatPlan_ArrangeFeatures <- function(df){
  
  # Sort rows to ensure all features are in the same order.
  xy <- st_coordinates(st_centroid(df))
  df <- df[order(xy[,"X"], xy[,"Y"]),]
  
  df <- df %>%
    mutate(cellID = row_number())
  
}

