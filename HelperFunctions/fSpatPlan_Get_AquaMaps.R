# ----- Code to reduce AquaMaps to our requested species and area. -----
fSpatPlan_Get_AquaMaps <- function(PlanUnits, 
                                   cCRS, 
                                   MinD = 0, 
                                   MaxD = 200, # Do epipelagic by default
                                   CutOff = 0.5,
                                   limits = NA,
                                   PacificCentered = FALSE){ # No longitundinal limits by default
  
  Sys.setenv(GDAL_MAX_BAND_COUNT = 120000)
  
  species <- read_rds("Data/AquaMaps/AquaMaps_SpeciesInfoFB.rds")# The species info
  
  longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  temp <- PlanUnits %>% # Get the extent for AquaMaps from the Bndry extent
    st_transform(crs = longlat) %>% # Must be long/lat for AquaMaps
    st_bbox() # get sf extent
  
  # If Pacific-centered, change the way of species filtering species and defining extent
  if (isTRUE(PacificCentered)) {
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
  } else{
    
    ex_sf <- temp + c(-1, -1, 1, 1) # Pad the limits by 1 degree
    # ex_sf <- ex_sf + c(-10, -10, 10, 10) # Pad the limits by 10 degrees
    
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
  crop <- function(df, extent) {
    cropped <- df %>%
      sf::st_crop(extent, crop = TRUE) %>%  # TODO replace ex_sf with a polygon to deal with EEZ or coastal areas
      stars:::slice.stars(along = "band", index = spp$SpeciesIDNum) %>% # indexes rows based on SpeciesIDNum
      stars::st_as_stars() %>% # loads it into memory
      stars::st_set_dimensions("band", values = spp$longnames) %>%
      sf::st_as_sf(na.rm = FALSE, as_points = FALSE, merge = FALSE)
    
    rs <- AquaMaps_temp1 %>%
      sf::st_drop_geometry() %>%
      is.na() %>%
      rowSums()
    
    nc <- ncol(cropped) - 1 # Number of cols not including geometry
    
    crop_df <- cropped %>%
      dplyr::filter({rs == nc} == FALSE) # Remove Rows with all NAs (except geometry)
    
    return(crop_df)
  }
  
  # If Pacific-centered, change the way of cropping.
  if (isTRUE(PacificCentered)){
    AquaMaps_temp1 <- crop(AquaMaps_sf, ex_sf1) # Crops western half of the planning region
    AquaMaps_temp2 <- crop(AquaMaps_sf, ex_sf2) # Crops eastern half of the planning region

    AquaMaps_sf <- dplyr::bind_rows(AquaMaps_temp1, AquaMaps_temp2) %>% 
      sf::st_transform(crs = longlat) %>% 
      fSpatPlan_Convert2PacificCentered(cCRS = cCRS) %>% 
      sf::st_set_crs(., st_crs(PlanUnits))
    
    aqm <- AquaMaps_sf %>% # Intersects with cropped aquamaps data with planning region
      sf::st_interpolate_aw(PlanUnits,
                            extensive = FALSE) %>% 
      dplyr::mutate_at(vars(any_of(spp$longnames)), 
                       ~ case_when(. >= CutOff ~ 1,
                                   . <= CutOff ~ 0,
                                   is.na(.) ~ 0))
  } else {
    aqm <- crop(ex_sf) %>% 
      sf::st_transform(cCRS) %>% # Transform to desired CRS
      st_interpolate_aw(PlanUnits, extensive = FALSE) %>% 
      mutate_at(vars(any_of(spp$longnames)), 
                ~ case_when(. >= CutOff ~ 1,
                            . <= CutOff ~ 0,
                            is.na(.) ~ 0))
  }
  
  # Get names of columns with data in them
  nm <- aqm %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    dplyr::select(which(!colSums(., na.rm=TRUE) %in% 0)) %>% 
    names()
  
  # Remove zero columns now the cutoff has been applied
  aqm %<>%
    dplyr::select(all_of(nm))
  
  return(aqm)
}