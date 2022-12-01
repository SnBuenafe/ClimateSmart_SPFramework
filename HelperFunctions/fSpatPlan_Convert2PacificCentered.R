# MAKING PACIFIC-CENTERED ROBINSON PROJECTION

# Description:
# This function reprojects a Pacific-centered sf object from a lon-lat to a Robinson projection.
# Function written by J.D. Everett and I. Brito-Morales
# moll_pacific <- "+proj=moll +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs"
# rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
fSpatPlan_Convert2PacificCentered <- function(df, cCRS, buff = 0){
  # Define a long & slim polygon that overlaps the meridian line & set its CRS to match 
  # that of world
  
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
    st_transform(crs = cCRS) # Perform transformation on modified version of world dataset
  
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