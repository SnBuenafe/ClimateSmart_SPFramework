# ----- Get Cost -----
fSpatPlan_Get_Cost <- function(PUs, cCRS, PacificCentered = TRUE, group = "all"){
  
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
  if(isTRUE(PacificCentered)){
    temp_cost <- call_cost %>% 
      sf::st_make_valid() %>% 
      sf::st_transform(longlat) %>% 
      fSpatPlan_Convert2PacificCentered(cCRS = cCRS)
    
    Cost <- temp_cost %>% 
      sf::st_interpolate_aw(PUs, extensive = FALSE) %>% 
      dplyr::rename(Cost = layer)
    
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