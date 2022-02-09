

fSpatPlan_Get_TargetsIA <- function(df, target_min, target_max){
  
  PU_area_km2 <- as.numeric(st_area(df[1,1])/1e+06) # Area of each planning unit
  total_PU_area <- nrow(df) * PU_area_km2 # Total area of the study region
  
  feature_area <- df %>% 
    as_tibble() %>% 
    dplyr::select(-geometry) %>% 
    summarise(across(everything(), ~ sum(., is.na(.), 0))) %>% 
    pivot_longer(everything(.), names_to = "Species", values_to = "Area_km2") %>% 
    mutate(Species = str_replace_all(Species, pattern = "_", replacement = " "),
           Area_km2 = Area_km2 * PU_area_km2,
           Target = target_max-((Area_km2/total_PU_area)*(target_max-target_min)))
  
}

