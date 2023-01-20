# Functions are written by Jason D. Everett
# UQ/CSIRO/UNSW

#install.packages("pacman")
#devtools::install_github("JorGarMol/VoCC", dependencies = TRUE, build_vignettes = TRUE)

#install.packages("remotes")
#remotes::install_github("jfq3/ggordiplots", force = TRUE)

#install.packages("/Library/gurobi911/mac64/R/gurobi_9.1-1_R_4.0.2.tgz", repos=NULL)

# devtools::install_github("ropensci/rfishbase")

pacman::p_load(sf, terra, tidyverse, rnaturalearth, prioritizr, stars, patchwork, proj4, magrittr, doParallel, ggridges, viridis, vegan, irr, corrplot, vegan, BiocManager, ncdf4, PCICt, ncdf4.helpers, VoCC, RColorBrewer, ggordiplots, gurobi, rfishbase)

longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

source("HelperFunctions/SpatPlan_Plotting.R")

#### Fucntions for FishBase and SealifeBase ####
# ----- A function to fix incorrect column types from fishbase and sealifebase -----
fix_species_type <- function(df, server = "fishbase"){
  
  if(server == "fishbase"){ # Need to convert type of different columns depending on database
    nm <- c("SpecCode", "DepthRangeShallow", "CommonLength", "CommonLengthF", "LongevityWildRef", "MaxLengthRef", "DangerousRef")
  } else if(server == "sealifebase"){
    nm <- c("SpecCode", "SpeciesRefNo", "GenCode", "DepthRangeRef", "LongevityWildRef", "Weight")
  }
  
  df <- df %>% 
    mutate(across(any_of(nm), as.numeric)) # Convert `nm` variables to numeric
}

# ----- Code to loop through and try and validate names with fishbase and sealifebase -----
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

# ----- Create ONE BIG polygon that we can use to populate with PUs -----
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

# ----- Ensure all features are in the same order -----
# Last edited 8th Sept 2021
fSpatPlan_ArrangeFeatures <- function(df){
  
  # Sort rows to ensure all features are in the same order.
  xy <- st_coordinates(st_centroid(df))
  df <- df[order(xy[,"X"], xy[,"Y"]),]
  
  df <- df %>%
    mutate(cellID = row_number())
  
}

# ----- Create planning units -----

#
# Written: 15 December 2020
# Updated: 11th February 2021


# ----- Code to match aquamaps with fishbase data -----
# Last edited 19 November 2021

# Habitat definitions
# "bathydemersal": Living and feeding on the bottom below 200 m.
# "bathypelagic": Region of the oceanic zone between 1,000 m to 4,000 m; between the mesopelagic layer above and the abyssopelagic layer below. Living or feeding in open waters at depths between 1,000 and 4,000 m. In FishBase this term is used to include the depth range from 200 m to the bottom and thus the zones mesopelagic, bathypelagic and abyssopelagic. 
# "benthic": Dwelling on, or relating to, the bottom of a body of water; living on the bottom of the ocean and feeding on benthic organisms. 
# "benthopelagic": Living and feeding near the bottom as well as in midwaters or near the surface. Feeding on benthic as well as free swimming organisms. Many freshwater fish are opportunistic feeders that forage on the bottom as well as in midwater and near the surface, also pertaining to forms which hover or swim just over the floor of the sea, e.g. Halosauridae, Macrouridae, Moridae, Brotulidae; the depth zone about 100 metres off the bottom at all depths below the edge of the continental shelf.
# "demersal": Sinking to or lying on the bottom; living on or near the bottom and feeding on benthic organisms
# "epipelagic": The uppermost, normally photic layer of the ocean between the ocean surface and the thermocline, usually between depths of 0-200 m; living or feeding on surface waters or at midwater to depths of 200 m.
# "epiphytic":
# "host"
# "others"
# "pelagic": Living and feeding in the open sea; associated with the surface or middle depths of a body of water; free swimming in the seas, oceans or open waters; not in association with the bottom. Many pelagic fish feed on plankton. In FishBase, referring to surface or mid water from 0 to 200 m depth.
# "pelagic-neritic": Inhabiting shallow coastal waters over the continental shelf.
# "pelagic-oceanic": Pertaining to the open ocean beyond the continental shelf; living in the open ocean, offshore. Opposite of neritic.
# "reef-associated": Living and feeding on or near coral reefs
# "sessile": Organism being attached to a substrate.

fSpatPlan_Match_AquaMapsFishBase <- function(spp, fld = c("SpecCode", "Species", "DemersPelag", "DepthRangeShallow", "DepthRangeDeep")){
  
  library(rfishbase)
  
  spp <- spp %>% 
    mutate(longnames2 = str_replace_all(longnames,"_", " ")) %>% 
    relocate(longnames2, .after = longnames)
  
  fish_class <- c("Actinopterygii", "Chondrichthyes", "Elasmobranchii", "Holocephali", "Myxini", "Cephalaspidomorphi", "Sarcopterygii")
  
  # Get the species that are fish
  finfish <- spp %>% 
    filter(class %in% fish_class)
  
  #Get all 
  fspec <- rfishbase::species(finfish$longnames2, 
                              fields = fld,
                              server = "fishbase") %>% 
    fix_species_type(server = "fishbase") %>% # Warnings are for converting "NA" to NA
    distinct()
  
  
  # Get the species that are not fish
  sealife <- spp %>% 
    filter(!class %in% fish_class) %>% 
    mutate(longnames2 = str_replace_all(longnames2,"_", " "))
  
  sspec <- rfishbase::species(sealife$longnames2,
                              fields = fld,
                              server = "sealifebase") %>% 
    fix_species_type(server = "sealifebase") %>% # Warnings are for converting "NA" to NA
    distinct()
  
  
  # Check that dim(finfish)[1] + dim(sealife)[1] == dim(spp)[1]
  
  ## Now fix up NAs
  
  # First FishBase
  fspecNA <- fspec %>% 
    filter(is.na(SpecCode)) # Check NAs
  
  valid <- fSpatPlan_Validate_FBNAs(fspecNA, "fishbase") # Validate all the names
  
  valid <- rfishbase::species(pull(valid, ValidSpecies), # Get the species info for valid names
                              fields = fld,
                              server = "fishbase") %>% 
    fix_species_type(server = "fishbase") # Warnings are for converting "NA" to NA
  
  valid <- valid %>% 
    filter(!is.na(SpecCode)) # Remove unmatched species
  
  fspec <- fspec %>% 
    filter(!is.na(SpecCode)) %>% # Remove NAs
    rbind(valid) %>% # Add valid back in
    rename(Habitat = DemersPelag, FB_DepthRangeShallow = DepthRangeShallow, FB_DepthRangeDeep = DepthRangeDeep)
  rm(valid, fspecNA)
  
  # Now SeaLifeBase
  sspecNA <- sspec %>% 
    filter(is.na(SpecCode))  # Check NAs
  
  valid <- fSpatPlan_Validate_FBNAs(sspecNA, "sealifebase") # Validate all the names
  valid <- rfishbase::species(pull(valid, ValidSpecies), # Get the species info for valid names
                              fields = fld,
                              server = "sealifebase") %>% 
    fix_species_type(server = "sealifebase") # Warnings are for converting "NA" to NA
  
  valid <- valid %>%
    filter(!is.na(SpecCode)) # Remove unmatched species # 1 missing as at 14 Nov 2021
  
  sspec <- sspec %>% 
    filter(!is.na(SpecCode)) %>% # Check NAs
    rbind(valid) %>% 
    rename(Habitat = DemersPelag, FB_DepthRangeShallow = DepthRangeShallow, FB_DepthRangeDeep = DepthRangeDeep)
  rm(valid, sspecNA)
  
  ## Now merge with the original species list
  spp_out <- spp %>% 
    left_join(bind_rows(fspec, sspec), by = c("longnames2" = "Species")) %>%
    # left_join(bind_rows(fspec, sspec), by = c("speccode" = "SpecCode")) %>%
    relocate("Habitat", .after = MaxLon) %>%
    mutate_at("Habitat", str_replace, "Sessile", "sessile") %>%
    mutate_at("Habitat", str_replace, "Demersal", "demersal") %>%
    mutate_at("Habitat", replace_na, "benthic") %>% # I checked NA online and they are all benthic or demersal. Generalising here
    mutate(HabitatCat = case_when(Habitat %in% c("sessile", "demersal", "benthic", "reef-associated", "benthopelagic", "bathydemersal") ~ "Benthic",
                                  Habitat %in% c("pelagic-neritic", "pelagic", "bathypelagic", "pelagic-oceanic", "epipelagic") ~ "Pelagic")) %>% 
    relocate(HabitatCat, .after = Habitat) %>% 
    distinct() # TODO For some reason this adds a few extra rows, but they are identical so we just remove them.
  
  write_rds(spp_out, file.path("Data", "AquaMaps", "AquaMaps_SpeciesInfoFB.rds"))
  
  rm(fspec, sspec)
  
  return(spp_out)
}


