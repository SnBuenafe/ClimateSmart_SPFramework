
fSpatPlan_Get_ClimateLayer <- function(PlanUnits,
                                       ClimateLayer,
                                       cCRS = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
                                       metric){
  
  source("HelperFunctions/fSpatPlan_Convert2PacificRobinson.R")
  
  PUs <- PlanUnits %>% 
    dplyr::mutate(cellID = row_number())
  # If Pacific-Centered
  if(cCRS == "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"){
    
    temp_ClimateLayer <- ClimateLayer %>% 
      terra::rast() %>% # Convert from RasterStack to SpatRaster
      as.data.frame(xy = TRUE) %>% 
      as_tibble() %>% 
      dplyr::mutate(x = ifelse(x < 0, yes = x + 180, no = x - 180)) %>% 
      as.data.frame() %>% 
      terra::rast(type = "xyz") %>% 
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to polygon data
      st_as_sf() %>% 
      st_set_crs(longlat)
    
    temp_ClimateLayer <- temp_ClimateLayer %>% 
      fSpatPlan_Convert2PacificRobinson()

  } else {
    
    temp_ClimateLayer <- ClimateLayer %>% 
      terra::rast() %>% # Convert from RasterStack to SpatRaster
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to polygon data
      st_as_sf() %>% 
      st_transform(longlat)
    
    } 
    
  ClimateLayer_sf <- temp_ClimateLayer %>% 
    st_interpolate_aw(PUs, extensive = FALSE) %>% 
    dplyr::mutate(cellID = row_number())
  
  if (str_detect(metric, "velocity")) {
    vector <- as_vector(st_nearest_feature(PUs, ClimateLayer_sf %>% filter(!is.na(voccMag))))
    
    # mutate climate layer; replace NAs with values from nearest neighbor
    mutatedClimateLayer <- ClimateLayer_sf %>% 
      dplyr::mutate(transformed = ifelse(is.na(voccMag), 
                                         yes = voccMag[vector[cellID]], 
                                         no = voccMag))
  } else if (str_detect(metric, "roc")) {
    vector <- as_vector(st_nearest_feature(PUs, ClimateLayer_sf %>% filter(!is.na(slpTrends))))
    
    # mutate climate layer; replace NAs with values from nearest neighbor
    mutatedClimateLayer <- ClimateLayer_sf %>% 
      dplyr::mutate(transformed = ifelse(is.na(slpTrends), 
                                         yes = slpTrends[vector[cellID]],
                                         no = slpTrends))
  } 
  
  return(mutatedClimateLayer)

}
