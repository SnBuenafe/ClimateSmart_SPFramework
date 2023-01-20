# Create object for selection frequency across a set of solutions
fGetSelFrequency <- function(solution_list, 
                             col_names, 
                             PUs
                             ) {
  
  df <- list() # empty list
  for (i in 1:length(col_names)) {
    df[[i]] <- solution_list[[i]] %>% 
      dplyr::select(solution_1) %>% # Select solution column
      dplyr::rename(!!sym(col_names[i]) := solution_1) %>%  # Rename solution column to the column name\
      tibble::as_tibble()
      
  }
  
  tmp <- purrr::reduce(df, dplyr::left_join) %>% # join all the solutions
    dplyr::select(-geometry) %>% 
    dplyr::mutate(selection = rowSums(., na.rm = TRUE)) %>% # Calculate selection frequency for all PUs
    dplyr::mutate(cellID = row_number())
  
  # Create the selection frequency sf object
  low_regret <- dplyr::full_join(tmp, PUs, 
                                 by = "cellID") %>% 
    sf::st_as_sf(sf_column_name = "geometry") %>% 
    dplyr::left_join(., 
                     UniformCost %>% 
                       tibble::as_tibble(), 
                     by = c("geometry", "cellID")) %>% 
    sf::st_as_sf(sf_column_name = "geometry") 
  
  return(low_regret)
}
