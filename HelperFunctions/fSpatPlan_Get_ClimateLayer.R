
fSpatPlan_Get_ClimateLayer <- function(PlanUnits,
                                       ClimateLayer,
                                       cCRS = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
                                       metric){
  
  source("HelperFunctions/fSpatPlan_Convert2PacificRobinson.R")

  # If Pacific-Centered
  if(cCRS == "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"){
    temp_ClimateLayer <- ClimateLayer %>% 
      terra::rast() %>% # Convert from RasterStack to SpatRaster
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to polygon data
      st_as_sf() %>% 
      st_transform(longlat)
    
    temp_ClimateLayer <- temp_ClimateLayer %>% 
      fSpatPlan_Convert2PacificRobinson()
    
    ClimateLayer_sf <- temp_ClimateLayer %>% 
      st_interpolate_aw(PUs, extensive = FALSE) 
    
    if (metric == "velocity") {
      ClimateLayer_sf <- ClimateLayer_sf %>% 
        dplyr::mutate(voccMag = ifelse(is.na(voccMag), median(filter(ClimateLayer_sf, ClimateLayer_sf$voccMag!=0)$voccMag), voccMag)) %>% # replacing NAs with the median
        dplyr::mutate(voccAng = ifelse(is.na(voccAng), median(filter(ClimateLayer_sf, ClimateLayer_sf$voccAng!=0)$voccAng), voccAng)) 
    } else if (metric %in% c("roc_tos", "roc_phos", "roc_o2os", "roc_tos_ensemble", "roc_phos_ensemble", "roc_o2os_ensemble")) {
      ClimateLayer_sf <- ClimateLayer_sf %>% 
        dplyr::mutate(slpTrends = ifelse(is.na(slpTrends), median(filter(ClimateLayer_sf, ClimateLayer_sf$slpTrends!=0)$slpTrends), slpTrends)) %>% # replacing NAs with the median
        dplyr::mutate(seTrends = ifelse(is.na(seTrends), median(filter(ClimateLayer_sf, ClimateLayer_sf$seTrends!=0)$seTrends), seTrends)) %>% 
        dplyr::mutate(sigTrends = ifelse(is.na(sigTrends), median(filter(ClimateLayer_sf, ClimateLayer_sf$sigTrends!=0)$sigTrends), sigTrends))
    } 

  } else {
    
    temp_ClimateLayer <- ClimateLayer %>% 
      terra::rast() %>% # Convert from RasterStack to SpatRaster
      terra::as.polygons(trunc = FALSE, dissolve = FALSE, na.rm=FALSE) %>% # Convert to polygon data
      st_as_sf() %>% 
      st_transform(longlat)
    
    ClimateLayer_sf <- temp_ClimateLayer %>% 
      st_interpolate_aw(PUs, extensive = FALSE) 
    
  }
  
  return(ClimateLayer_sf)

}
