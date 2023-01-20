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
      fSpatPlan_Convert2PacificCentered(cCRS = cCRS) %>% 
      dplyr::select(sum_cum_int)
    
    ClimateLayer_sf <- PUs %>% 
      st_join(temp_ClimateLayer, join = st_contains_properly, left = TRUE, largest = TRUE)
    
  } else {
    
    temp_ClimateLayer <- ClimateLayer %>% 
      terra::rast() %>% # Convert from RasterStack to SpatRaster
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to polygon data
      st_as_sf(crs = longlat) %>% 
      fSpatPlan_Convert2PacificCentered(cCRS = cCRS)
    
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