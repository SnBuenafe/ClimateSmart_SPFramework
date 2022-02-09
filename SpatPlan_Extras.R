#install.packages("pacman")

pacman::p_load(sf, terra, tidyverse, rnaturalearth, prioritizr, stars, patchwork, proj4, magrittr, doParallel,
               BiocManager, ncdf4, PCICt, ncdf4.helpers, VoCC, RColorBrewer)
# devtools::install_github("JorGarMol/VoCC", dependencies = TRUE, build_vignettes = TRUE)
# pacman::p_load(VoCC)

longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

source("fSpatPlan_Get_PlanningUnits.R")
source("fSpatPlan_Get_Boundary.R")
source("fSpatPlan_Get_AquaMaps.R")
source("fSpatPlan_Match_AquaMapsFishBase.R")
source("fSpatPlan_Get_MPAs.R")
source("fSpatPlan_Get_Cost.R")
source("fSpatPlan_Get_TargetsIA.R")
source("fSpatPlan_Match_IUCNRedList.R")
source("fSpatPlan_Get_MaskedPolygon.R")
source("fSpatPlan_Get_ClimateLayer.R")

source("SpatPlan_Plotting.R")


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

# Convert a world Robinson sf object to a Pacific-centred one
#
# Written by Jason D. Everett
# UQ/CSIRO/UNSW
# Last edited 8th Sept 2021
fSpatPlan_Convert2PacificRobinson <- function(df, buff = 0){
  # Define a long & slim polygon that overlaps the meridian line & set its CRS to match 
  # that of world
  # Adapted from here:
  # https://stackoverflow.com/questions/56146735/visual-bug-when-changing-robinson-projections-central-meridian-with-ggplot2
  
  library(magrittr)
  rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                       c(0, 90),
                                       c(0, -90),
                                       c(-0.0001, -90),
                                       c(-0.0001, 90)))) %>%
    st_sfc() %>%
    st_set_crs(longlat)
  
  # Modify world dataset to remove overlapping portions with world's polygons
  df_robinson <- df %>% 
    st_make_valid() %>% # Just in case....
    st_difference(polygon) %>% 
    st_transform(crs = rob_pacific) # Perform transformation on modified version of world dataset
  rm(polygon)
  
  # # notice that there is a line in the middle of Antarctica. This is because we have
  # # split the map after reprojection. We need to fix this:
  bbox <-  st_bbox(df_robinson)
  bbox[c(1,3)] <- c(-1e-5, 1e-5)
  polygon_rob <- st_as_sfc(bbox)
  
  crosses <- df_robinson %>%
    st_intersects(polygon_rob) %>%
    sapply(length) %>%
    as.logical %>%
    which
  
  # # Adding buffer 0
  df_robinson[crosses,] %<>%
    st_buffer(buff)
  
}

