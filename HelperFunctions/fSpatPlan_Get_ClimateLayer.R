
fSpatPlan_Get_ClimateLayer <- function(PlanUnits,
                                       ClimateLayer,
                                       cCRS = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
                                       metric,
                                       colname){
  
  source("HelperFunctions/fSpatPlan_Convert2PacificRobinson.R")
  
  PUs <- PlanUnits %>% 
    dplyr::mutate(cellID = row_number())
  # If Pacific-Centered
  if(cCRS == "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"){
    
    if (str_detect(metric, "MHW")) {
      
      temp_ClimateLayer <- ClimateLayer %>% 
        st_as_sf(coords = c("X", "Y"), crs = longlat) %>% 
        fSpatPlan_Convert2PacificRobinson() 
      
      ClimateLayer_sf <- PUs %>% 
        st_join(temp_ClimateLayer, join = st_intersects, left = TRUE, largest = TRUE)
      
    } else {
      
      temp_ClimateLayer <- ClimateLayer %>% 
        terra::rast() %>% # Convert from RasterStack to SpatRaster
        as.data.frame(xy = TRUE) %>% 
        as_tibble() %>% 
        dplyr::mutate(x = ifelse(x < 0, yes = x + 180, no = x - 180)) %>% 
        as.data.frame() %>% 
        terra::rast(type = "xyz") %>% 
        terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to polygon data
        st_as_sf() %>% 
        st_set_crs(longlat) %>% 
        fSpatPlan_Convert2PacificRobinson()
      
      ClimateLayer_sf <- temp_ClimateLayer %>% 
        st_interpolate_aw(PUs, extensive = FALSE) %>% 
        dplyr::mutate(cellID = row_number())

  }} else {
    
    temp_ClimateLayer <- ClimateLayer %>% 
      terra::rast() %>% # Convert from RasterStack to SpatRaster
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to polygon data
      st_as_sf() %>% 
      st_transform(longlat)
    
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
