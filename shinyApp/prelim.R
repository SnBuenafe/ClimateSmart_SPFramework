
### Preliminaries ####
library(shiny)
library(shinydashboard)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(magrittr)

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


land <- ne_countries(scale = 'large', returnclass = 'sf') %>% 
  fSpatPlan_Convert2PacificRobinson() # Land masses; needed for plotting
save_name <- "WestPacific"
PUs <- read_rds(file.path("Output", paste(save_name, paste0("PlanningRegion.rds"), sep = "_")))




fSpatPlan_PlotComparison <- function(data, land){
  ggplot() +
    geom_sf(data = data, aes(fill = Compare), colour = NA, size = 0.0001) +
    geom_sf(data = land, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(data)$xlim, ylim = st_bbox(data)$ylim) +
    theme_bw() +
    scale_fill_manual(name=" ", values = c("Added (+)" = "Red", "Same" = "ivory3", "Removed (-)" = "Blue"), drop = FALSE) +
    theme(legend.position = "bottom") #optional to make more space for plot
}





fSpatPlan_PlotSolution <- function(s1, PlanUnits, world){
  ggplot() + 
    geom_sf(data = s1, aes(fill = solution_1), colour = NA, size = 0.1, show.legend = TRUE) +
    #    geom_sf(data = PlanUnits, colour = "lightblue", fill = NA, size = 0.1, show.legend = FALSE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PlanUnits)$xlim, ylim = st_bbox(PlanUnits)$ylim) +
    theme(axis.text = element_text(size = 20), 
          legend.text = element_text(size = 13),
          legend.title = element_text(size = 16)) +
    scale_fill_manual(name="Planning Units",
                      values = c("TRUE" = "steelblue4",
                                 "FALSE" = "lightsteelblue2"),
                      labels=c("selected", "not selected")) +
    theme_bw() #+
  #labs(subtitle = "Solution")
  
}


compute_summary <- function(s) {
  
  summary <- s %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    filter(solution_1 == 1) %>% 
    summarize(sum_area = nrow(.))
  
  summary %<>% mutate(percent_area = sum_area*100/35389)
  
  return(summary)
}
create_corrmatrix <- function(list_plans) {
  pacman::p_load(irr)
  
  y = 1
  s_matrix <- list() # empty list
  for(i in 1:length(list_plans)){
    for(j in 1:length(list_plans)){
      kappa_temp <- irr::kappa2(bind_cols(list_plans[[i]], list_plans[[j]]))
      kappa_corrvalue <- kappa_temp$value
      kappa_pvalue <- kappa_temp$p.value
      s_matrix[[y]] <- cbind(colnames(list_plans[[i]]), colnames(list_plans[[j]]), kappa_corrvalue, kappa_pvalue)
      y = y+1
    }
  }
  
  s_matrix_all <- do.call(rbind, s_matrix) %>% 
    as_tibble()
  colnames(s_matrix_all)[1:2] <- c('plan1','plan2')
  
  matrix <- s_matrix_all %>% 
    as_tibble() %>% 
    dplyr::select(-kappa_pvalue) %>% 
    pivot_wider(names_from = plan2, values_from = kappa_corrvalue) %>% 
    as.matrix()
  
  return(matrix)
}
