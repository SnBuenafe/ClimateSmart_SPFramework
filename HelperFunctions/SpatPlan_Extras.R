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

# ----- Reproject sf to Robinson -----
# Function written by J.D. Everett and I. Brito-Morales
fSpatPlan_Convert2PacificRobinson <- function(df, buff = 0){
  # Define a long & slim polygon that overlaps the meridian line & set its CRS to match 
  # that of world
  
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
    st_difference(polygon) %>% 
    st_transform(crs = rob_pacific) # Perform transformation on modified version of world dataset
  
  # notice that there is a line in the middle of Antarctica. This is because we have
  # split the map after reprojection. We need to fix this:
  bbox <-  st_bbox(df_robinson)
  bbox[c(1,3)]  <-  c(-1e-5, 1e-5)
  polygon_rob <- st_as_sfc(bbox)
  
  crosses <- df_robinson %>%
    st_intersects(polygon_rob) %>%
    sapply(length) %>%
    as.logical %>%
    which
  
  # Adding buffer 0
  df_robinson[crosses,] %<>%
    st_buffer(buff)
  
  return(df_robinson)
  
}

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
# Function to create square or heaxagonal planning units for your area of interest.
# Inputs needed are:
#   Bndry: An sf polygon object which outlines the limits of the study area.
#       The code takes the bbbox so the limits are the most important.
#       The output inherits the crs from this sf object so ensure it is in the correct projection for your needs
#   LandMass: An sf multipolygon object which contains all the areas (ie land) that you wish to remove from the grid.
#       The code assumes that any Planning Unit whose centroids is over land will be removed. This approximates > 50% of the PU is landward.
#   CellArea: The area in km you wish your resultant Planning Units to be.
#   Shape: Hexagon or Square
#
# Written: 15 December 2020
# Updated: 11th February 2021

fSpatPlan_Get_PlanningUnits <- function(Bndry, LandMass, CellArea, Shape, inverse = TRUE){
  
  if(Shape %in% c("hexagon", "Hexagon")){
    sq <- FALSE
    diameter <- 2 * sqrt((CellArea*1e6)/((3*sqrt(3)/2))) * sqrt(3)/2 # Diameter in m's
  }
  
  if(Shape %in% c("square", "Square")){
    sq <- TRUE
    diameter <- sqrt(CellArea*1e6) # Diameter in m's
  }
  
  # First create planning units for the whole region
  PUs <- st_make_grid(Bndry,
                      square = sq,
                      cellsize = c(diameter, diameter),
                      what = "polygons") %>%
    st_sf()
  
  # Check cell size worked ok.
  print(paste0("Range of cellsize are ",
               round(as.numeric(range(units::set_units(st_area(PUs), "km^2")))[1])," km2 to ",
               round(as.numeric(range(units::set_units(st_area(PUs), "km^2")))[2])," km2")) # Check area
  
  # First get all the PUs partially/wholly within the planning region
  logi_Reg <- st_centroid(PUs) %>%
    st_intersects(Bndry) %>%
    lengths > 0 # Get logical vector instead of sparse geometry binary
  
  PUs <- PUs[logi_Reg, ] # Get TRUE
  
  # Second, get all the pu's with < 50 % area on land (approximated from the centroid)
  # logi_Ocean <- st_centroid(PUs) %>%
  #   st_within(st_union(LandMass)) %>%
  #   lengths > 0 # Get logical vector instead of sparse geometry binary
  
  logi_Ocean <- st_centroid(PUs) %>%
    st_intersects(LandMass) %>%
    lengths > 0 # Get logical vector instead of sparse geometry binary
  
  if(inverse == TRUE){
    PUs <- PUs[!logi_Ocean, ] # Get FALSE
  }
  else{
    PUs <- PUs[logi_Ocean==TRUE, ] # Get TRUE
  }
  return(PUs)
}

# ----- Get boundary of an sf -----
fSpatPlan_Get_Boundary <- function(Limits, cCRS){
  
  # Create function for creating polygon
  polygon <- function(x){
    x <- x %>% 
      as.matrix() %>%
      list() %>%
      st_polygon() %>%
      st_sfc(crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
      st_transform(crs = cCRS)
  }
  
  # if Pacific-centered, two polygons then union them:
  if (is.numeric(Limits) && cCRS == "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"){
    
    Bndry1 <- tibble(x = seq(-180, Limits["xmin"], by = 1), y = Limits["ymin"]) %>%
      bind_rows(tibble(x = Limits["xmin"], y = seq(Limits["ymin"], Limits["ymax"], by = 1))) %>%
      bind_rows(tibble(x = seq(Limits["xmin"], -180, by = -1), y = Limits["ymax"])) %>%
      bind_rows(tibble(x = -180, y = seq(Limits["ymax"], Limits["ymin"], by = -1))) %>% 
      polygon()
    
    Bndry2 <- tibble(x = seq(180, Limits["xmax"], by = -1), y = Limits["ymin"]) %>%
      bind_rows(tibble(x = Limits["xmax"], y = seq(Limits["ymin"], Limits["ymax"], by = 1))) %>%
      bind_rows(tibble(x = seq(Limits["xmax"], 180, by = 1), y = Limits["ymax"])) %>%
      bind_rows(tibble(x = 180, y = seq(Limits["ymax"], Limits["ymin"], by = -1))) %>% 
      polygon()
    
    #sf_use_s2(FALSE)
    Bndry <- st_union(Bndry1, Bndry2)
    
    return(Bndry)
  }
  
  if (is.numeric(Limits)){
    Bndry <- tibble(x = seq(Limits["xmin"], Limits["xmax"], by = 1), y = Limits["ymin"]) %>%
      bind_rows(tibble(x = Limits["xmax"], y = seq(Limits["ymin"], Limits["ymax"], by = 1))) %>%
      bind_rows(tibble(x = seq(Limits["xmax"], Limits["xmin"], by = -1), y = Limits["ymax"])) %>%
      bind_rows(tibble(x = Limits["xmin"], y = seq(Limits["ymax"], Limits["ymin"], by = -1))) %>%
      polygon()
    
    return(Bndry)
  }
  
  if (Limits == "Global"){
    Bndry <- tibble(x = seq(-180, 180, by = 1), y = -90) %>%
      bind_rows(tibble(x = 180, y = seq(-90, 90, by = 1))) %>%
      bind_rows(tibble(x = seq(180, -180, by = -1), y = 90)) %>%
      bind_rows(tibble(x = -180, y = seq(90, -90, by = -1))) %>%
      polygon()
    
    return(Bndry)
  }
  
  # Added Western Pacific
  if (Limits == "WestPacific"){
    ocean_sf <- ne_download(scale = "large", category = "physical", type = "geography_marine_polys", returnclass = "sf") 
    
    # Check the seas listed in ocean_sf
    # list <- unique(ocean_sf$label) %>% 
    #  as_tibble()
    # list[order(list$value),] %>% print(n = Inf) # print everything
    
    # Create the list of water bodies to be included
    ocean_list <- c("NORTH PACIFIC OCEAN", "SOUTH PACIFIC OCEAN", "Philippine Sea", 
                    "Coral Sea", "Tasman Sea", "Bay of Plenty", "Cook Strait", 
                    "Bismarck Sea", "Solomon Sea", "Bass Strait", "Gulf of Papua", 
                    "Banda Sea", "Java Sea", "Celebes Sea", "Makassar Strait",
                    "Molucca Sea", "Halmahera Sea", "Ceram Sea", "Flores Sea")
    # Filter out ocean_list from ocean_sf
    ocean_temp <- ocean_sf %>% 
      dplyr::filter(label %in% ocean_list) %>% 
      dplyr::select(label, ne_id, geometry)
    
    projected_ocean <- fSpatPlan_Convert2PacificRobinson(ocean_temp) # Project it to Pacific Robinson
    
    # Call and filter the EEZs
    eez <- st_read("Data/World_EEZ_v11/eez_v11.shp") %>% 
      filter(SOVEREIGN1 != "Antarctica") %>% 
      dplyr::select(MRGID, GEONAME, TERRITORY1, X_1, Y_1, geometry)
    
    # Create the list of EEZs
    eez_list <- c("Phoenix Group", "Papua New Guinea", "Guam",
                  "Micronesia", "Nauru", "Solomon Islands", "Vanuatu", "New Caledonia", "Fiji",
                  "Marshall Islands", "Tonga", "Niue", "American Samoa", "Samoa",
                  "Tokelau", "Cook Islands", "French Polynesia", "Northern Mariana Islands", "Gilbert Islands",
                  "Tuvalu", "Wallis and Futuna", "Line Group", "Palau", "Indonesia")
    eez_filtered <- eez %>% 
      dplyr::filter(TERRITORY1 %in% eez_list)
    
    # Create polygons that will be used to include ABNJ (+: east/north; -: west/south)
    
    polygon_crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    polygon1 <- fSpatPlan_Get_Boundary(c(xmin = 155, xmax = 176, ymax = 5, ymin = -10), polygon_crs) %>% 
      st_as_sf() %>% 
      st_make_valid()
    polygon2 <- fSpatPlan_Get_Boundary(c(xmin = -161, xmax = -153, ymax = -10, ymin = -19), polygon_crs) %>% 
      st_as_sf() %>% 
      st_make_valid()
    polygon3 <- fSpatPlan_Get_Boundary(c(xmin = 135, xmax = 152, ymax = 6, ymin = 0), polygon_crs) %>% 
      st_as_sf() %>% 
      st_make_valid()
    polygon4 <- fSpatPlan_Get_Boundary(c(xmin = 170, xmax = 176, ymax = -10, ymin = -20), polygon_crs) %>% 
      st_as_sf() %>% 
      st_make_valid()
    
    # Include areas in ocean_temp that are within eez_temp & merge ABNJ
    # Note, you might want to increase memory limit here
    # memory.limit(size = 56000) # for Windows
    merged <- fSpatPlan_Get_MaskedPolygon(projected_ocean, 0.25, eez_filtered, FALSE)
    merged <- merged %>%  # change to inverse = TRUE, if you want the inverse of the intersection of ocean_temp and eez_temp
      st_union(., polygon1) %>% # merge with all the individual polygons
      st_union(., polygon2) %>% 
      st_union(., polygon3) %>% 
      st_union(., polygon4)
    
    # Filter out land masses found within the boundaries of the EEZs of interest
    land_list <- c("Palau", "Guam", "Indonesia", "Northern Mariana Islands", "Federated States of Micronesia", "Papua New Guinea",
                   "Solomon Islands", "New Caledonia", "Vanuatu", "Fiji", "Marshall Islands", "Nauru", "Kiribati", "Tuvalu",
                   "Wallis and Futuna", "Tonga", "Samoa", "American Samoa", "Niue", "Cook Islands", 
                   "French Polynesia", "United States Minor Outlying Islands", "Pitcairn Islands")
    land <- ne_countries(scale = "large", returnclass = "sf") %>% 
      dplyr::filter(name_en %in% land_list)
    indonesia <- eez %>% 
      filter(TERRITORY1 %in% c("Indonesia")) # remove Indonesia EEZ
    land_mask <- st_union(land, indonesia) # we also want to remove Indonesia EEZs at this point
    
    # Finally, remove land_mask from merged to end up with desired planning region
    Bndry <- fSpatPlan_Get_MaskedPolygon(merged, 0.25, land_mask, TRUE) 
    
    return(Bndry)
  }
  
  # Add Pacific
  
  
  # Add Indian
  
  
  # Add Atlantic
  
  
  # Add EEZs
  
  if (Limits == "Australia"){
    # Get Australia's EEZ
    Bndry <- st_read("Data/World_EEZ_v11/eez_v11.shp", quiet = TRUE) %>% 
      filter(TERRITORY1 == "Australia") %>% 
      st_transform(cCRS)
    return(Bndry)
  }
  
  
}

# ----- Code to reduce AquaMaps to our requested species and area. -----
fSpatPlan_Get_AquaMaps <- function(PlanUnits, 
                                   cCRS = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", 
                                   MinD = 0, 
                                   MaxD = 200, # Do epipelagic by default
                                   CutOff = 0.5,
                                   limits = NA){ # No longitundinal limits by default
  
  Sys.setenv(GDAL_MAX_BAND_COUNT = 120000)
  
  species <- read_rds("Data/AquaMaps/AquaMaps_SpeciesInfoFB.rds")# The species info
  
  longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  temp <- PlanUnits %>% # Get the extent for AquaMaps from the Bndry extent
    st_transform(crs = longlat) %>% # Must be long/lat for AquaMaps
    st_bbox() # get sf extent
  
  # If Pacific-centered, change the way of species filtering species and defining extent
  if ((cCRS == "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") && !is.na(extent)){
    ex_sf <- temp + c(1, -1, -1, 1) # Pad the limits by 1 degree
    # ex_sf <- temp + c(5, -5, -5, 5) # Pad the limits by 5 degrees
    # ex_sf <- temp + c(10, -10, -10, 10) # Pad the limits by 10 degrees
    
    # Filter species
    spp <- species %>% 
      dplyr::filter((MaxLon >= ex_sf$xmax & MaxLon <= 180) & (MinLon <= ex_sf$xmin & MinLon >= -180)) %>% 
      dplyr::filter(MinLat <= ex_sf$ymax & MaxLat >= ex_sf$ymin) %>% 
      dplyr::filter(DepthMin <= MaxD & DepthMax >= MinD)
    
    # Create two extents from ex_sf
    ex_sf1 <- ex_sf
    ex_sf1[3] = limits[1] # change to western limit
    ex_sf1[1] = -180
    
    ex_sf2 <- ex_sf
    ex_sf2[1] = limits[2] # change to eastern limit
    ex_sf2[3] = 180
  }
  else{
    ex_sf <- temp + c(-1, -1, 1, 1) # Pad the limits by 1 degree
    # ex_sf <- ex_sf + c(-10, -10, 10, 10) # Pad the limits by 10 degrees
    print(ex_sf)
    
    # Filter the species list to only include those layers with data in the range of our PUs
    
    spp <- species %>%
      dplyr::filter(MinLon <= ex_sf$xmax & MaxLon >= ex_sf$xmin) %>% 
      dplyr::filter(MinLat <= ex_sf$ymax & MaxLat >= ex_sf$ymin) %>% 
      dplyr::filter(DepthMin <= MaxD & DepthMax >= MinD)
    
  }
  
  # stars code to subset by data by our species list and crop area to the region of PlanUnits
  AquaMaps_sf <- read_stars("Data/AquaMaps/AquaMaps.tif", proxy = TRUE) # Load 
  
  # Check that the species list and the tif species match up
  if (all.equal(st_get_dimension_values(AquaMaps_sf, "band"), species$SpeciesID) != TRUE){
    stop("Species lists don't match up in fSpatPlan_Get_AquaMaps.R")
  }
  
  # Create function for cropping
  crop <- function(extent) {
    cropped <- AquaMaps_sf %>%  
      st_crop(extent, crop = TRUE) %>%  # TODO replace ex_sf with a polygon to deal with EEZ or coastal areas
      slice(along = "band", index = spp$SpeciesIDNum) %>% # indexes rows based on SpeciesIDNum
      st_as_stars() %>% # loads it into memory
      st_set_dimensions("band", values = spp$longnames) %>% 
      st_as_sf(na.rm = FALSE, as_points = FALSE, merge = FALSE) %>%
      st_transform(cCRS) # Transform to robinson
  }
  
  # If Pacific-centered, change the way of cropping.
  if ((cCRS == "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") && !is.na(limits)){
    AquaMaps_temp1 <- crop(ex_sf1) # Crops western half of the planning region
    AquaMaps_temp2 <- crop(ex_sf2) # Crops eastern half of the planning region
    
    AquaMaps_sf <- bind_rows(AquaMaps_temp1, AquaMaps_temp2) 
    
    AquaMaps_sf <- AquaMaps_sf %>% # Intersects with cropped aquamaps data with planning region
      st_interpolate_aw(PlanUnits, extensive = FALSE) %>% 
      mutate_at(vars(any_of(spp$longnames)), 
                ~ case_when(. >= CutOff ~ 1,
                            . <= CutOff ~ 0,
                            is.na(.) ~ 0))
  }
  # Else, the default way of cropping
  else {
    AquaMaps_sf <- crop(ex_sf) %>% 
      st_interpolate_aw(PlanUnits, extensive = FALSE) %>% 
      mutate_at(vars(any_of(spp$longnames)), 
                ~ case_when(. >= CutOff ~ 1,
                            . <= CutOff ~ 0,
                            is.na(.) ~ 0))
  }
  # Get names of columns with data in them
  nm <- AquaMaps_sf %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    dplyr::select(which(!colSums(., na.rm=TRUE) %in% 0)) %>% 
    names()
  
  # Remove zero columns now the cutoff has been applied
  AquaMaps_sf <- AquaMaps_sf %>% 
    dplyr::select(all_of(nm))
  
  return(AquaMaps_sf)
}

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

# ----- Get Cost -----
fSpatPlan_Get_Cost <- function(PUs, cCRS, group = "all"){
  
  # TODO Add some filtering ability to get the cost only for specific groups
  # Calling the cost layer
  if(group == "all"){
    call_cost <- terra::rast("Data/Cost/Cost_Raster_Sum.grd") %>% 
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to polygon data
      st_as_sf() # Convert to sf
  }
  
  else if(group == "pelagic"){
    call_cost <- terra::rast("Data/Cost/Cost_RasterStack_byFunctionalGroup.grd") %>% # data by functional group
      terra::subset(., c(14, 16, 19)) %>% # small, medium, and large pelagics
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to polygon data
      st_as_sf() # Convert to sf
    
    # Replace all NAs with the smallest value
    small_value <- list() #empty list
    for(i in 2:ncol(call_cost)-1) {
      small_value[i] <- (min(call_cost[[i]], na.rm = T))/2
    }
    small_value <- unlist(small_value)
    
    call_cost <- call_cost %>% 
      dplyr::mutate(medium = ifelse(is.na(MediumPelagics30_89Cm), small_value[1], MediumPelagics30_89Cm), 
                    small = ifelse(is.na(SmallPelagics30Cm), small_value[2], SmallPelagics30Cm),
                    large = ifelse(is.na(LargePelagics90Cm), small_value[3], LargePelagics90Cm)) %>% 
      dplyr::select(medium, small, large, geometry) %>% 
      dplyr::mutate(layer = medium + small + large) %>% # get the sum
      dplyr::select(layer, geometry) # only select sum and geometry
  }
  
  # If Pacific-centered:
  if(cCRS == "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"){
    temp_cost <- call_cost %>% 
      st_make_valid() %>% 
      st_transform(longlat)
    
    temp_cost <- temp_cost %>% 
      fSpatPlan_Convert2PacificRobinson()
    
    Cost <- temp_cost %>% 
      st_interpolate_aw(PUs, extensive = FALSE) %>% 
      rename(Cost = layer)
    
    return(Cost)
  }
  
  else{
    Cost <- call_cost %>% 
      st_transform(cCRS) %>% # Transform to robinson
      st_interpolate_aw(PUs, extensive = FALSE) %>% ## intersect with PUs
      rename(Cost = layer)
    
    return(Cost)
  }
}

# ----- Mask a polygon -----
fSpatPlan_Get_MaskedPolygon <- function(df, res, mask, inverse){
  
  library(terra)
  library(raster)
  library(fasterize)
  library(sf)
  
  rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  # Creating a empty raster
  rs <- raster::raster(ncol = 360*(1/res), nrow = 180*(1/res)) 
  rs[] <- 1:ncell(rs)
  crs(rs) <- CRS(longlat)
  
  # Fasterize the land object
  df_rs <- fasterize(df, rs)
  
  if(is.na(mask)) { 
    masked_df <- df_rs
  }else{
    mask_sp <- as(mask, "Spatial")
    masked_df <- terra::mask(df_rs, mask_sp, inverse = inverse)
  }
  # Remove land pixels that are still present / delete certain aggrupation of pixels.
  masked_clump <- clump(masked_df, directions = 8) 
  df_clump <- freq(masked_clump) %>% 
    as.data.frame()
  str(which(df_clump$count <= 9)) # which rows of the data.frame are only represented by clumps under 9 pixels?
  str(df_clump$value[which(df_clump$count <= 9)]) # which values do these correspond to?
  excludeID <- df_clump$value[which(df_clump$count <= 9)] # put these into a vector of clump ID's to be removed
  df_rs2 <- masked_clump # make a new raster to be sieved
  df_rs2[df_rs2 %in% excludeID] <- NA # assign NA to all clumps whose IDs are found in excludeID
  
  # Convert from Raster to Polygon
  df_pol <- as(df_rs2,  "SpatialPolygonsDataFrame")
  df_pol$layer <- seq(1, length(df_pol))
  df_pol <- spTransform(df_pol, CRS(longlat))
  # Now to a sf object and create ONE BIG polygon that we can use to populate with PUs
  df_pol_sf <- st_as_sf(df_pol) %>% 
    dplyr::select(layer) %>% 
    dplyr::summarise(total_layer = sum(layer, do_union = TRUE))
}

# ----- Interpolate climate layer to planning units -----
fSpatPlan_Get_ClimateLayer <- function(PlanUnits,
                                       ClimateLayer,
                                       cCRS = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
                                       metric,
                                       colname){
  
  PUs <- PlanUnits %>% 
    dplyr::mutate(cellID = row_number())
  
  if (str_detect(metric, "MHW")) {
    
    temp_ClimateLayer <- ClimateLayer %>% 
      sf::st_as_sf(coords = c("X", "Y"), crs = longlat) %>% 
      fSpatPlan_Convert2PacificRobinson() %>% 
      dplyr::select(sum_cum_int)
    
    ClimateLayer_sf <- PUs %>% 
      st_join(temp_ClimateLayer, join = st_contains_properly, left = TRUE, largest = TRUE)
    
  } else {
    
    temp_ClimateLayer <- ClimateLayer %>% 
      terra::rast() %>% # Convert from RasterStack to SpatRaster
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to polygon data
      st_as_sf(crs = longlat) %>% 
      fSpatPlan_Convert2PacificRobinson()
    
    ClimateLayer_sf <- temp_ClimateLayer %>% 
      st_interpolate_aw(PUs, extensive = FALSE) %>% 
      dplyr::mutate(cellID = row_number())
    
  }
  
  vector <- as_vector(st_nearest_feature(PUs, ClimateLayer_sf %>% filter(!is.na(!!sym(colname)))))
  filtered <- ClimateLayer_sf %>% filter(!is.na(!!sym(colname)))
  
  mutatedClimateLayer <- ClimateLayer_sf %>% 
    dplyr::mutate(transformed = ifelse(is.na(!!sym(colname)), 
                                       yes = (filtered[[ colname ]])[vector[cellID]], 
                                       no = !!sym(colname)))
  
  return(mutatedClimateLayer)
  
}
