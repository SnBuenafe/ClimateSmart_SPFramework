# PERCENTILE APPROACH

# ----- Description -----
# This function filters the species' distributions to just their climate-smart areas.
# Inputs:
# featuresDF: feature data.frame which should have a column for cellID
# percentile: cut-off threshold for determining whether an area is a climate priority area or not. Note that the percentile here is the lower limit of the threshold.
# metricDF: climate metric data.frame with 'metric' as the column name of the metric values per planning unit. This should also have a column for the cellID
# direction: If direction = 1, metric values are from low (least climate-smart) to high (most climate-smart). If direction = -1, metric values are from high (least climate-smart) to low (most climate-smart).
# targetsDF: data.frame that has the list of features under "feature" column and their corresponding targets under "target" column

fPercentile_CSapproach <- function(featuresDF,
                                   percentile,
                                   metricDF,
                                   direction
) {
  
  if(any(apply(metricDF, 2, is.na)[, "metric"])){
    print("There are some NAs in the metric data. Please check.")
  }
  
  spp <- featuresDF %>% # Get the list of features
    as_tibble() %>%
    dplyr::select(-geometry,-cellID) %>%
    names()
  
  metric <- metricDF %>% # Make sure metric df is a data.frame
    dplyr::mutate(cellID = row_number()) %>%
    sf::st_drop_geometry() %>%
    tibble::as_tibble()
  
  percentileList <- list()
  for(i in 1:length(spp)) {
    df <- featuresDF %>%
      as_tibble() %>%
      dplyr::select(!!sym(spp[i]), cellID) %>% # Select 1 feature at a time
      left_join(., metric, by = "cellID") %>% # Join with the metric layer
      dplyr::select(-cellID)
    
    filteredDF <- df %>%
      dplyr::filter(!!sym(spp[i]) == 1) # Select only areas with presences
    
    prct <- percentile/100 # Convert percentiles to proportions
    qntl <- quantile(filteredDF$metric, prct)[[1]] # Get the percentile
    
    if(direction == 1) {
      if(i == 1) {
        print("Higher values mean more climate-smart areas.") # Sanity check
      }
      df1 <- df %>%
        dplyr::mutate(V1 = ifelse(metric >= qntl, yes = 1, no = 0), # Filter areas of the highest xth percentile within feat's distribution
                      V2 = ifelse(!!sym(spp[i]) == 1, yes = 1, no = 0)) # Filter areas with the feat present in it
    } else if(direction == -1) {
      if(i == 1) {
        print("Lower values mean more climate-smart areas.") # Sanity check
      }
      df1 <- df %>%
        dplyr::mutate(V1 = ifelse(metric <= qntl, yes = 1, no = 0), # Filter areas of the highest xth percentile within feat's distribution
                      V2 = ifelse(!!sym(spp[i]) == 1, yes = 1, no = 0)) # Filter areas with the feat present in it
    } else {
      if(i == 1) {
        print("Please enter a valid direction: either 1 or -1.") # Sanity check
      }
    }
    
    percentileList[[i]] <- df1 %>%
      dplyr::mutate(!!sym(paste0(spp[i], "_filtered")) := V1*V2) %>% # V1*V2 will be 1 if area is within the xth percentile and if a feat is present in it
      dplyr::select(!!sym(paste0(spp[i], "_filtered")))
  }
  
  resultDF <- percentileList %>% # Create sf object as output
    do.call(bind_cols, .) %>%
    dplyr::bind_cols(., featuresDF %>% dplyr::select(cellID, geometry)) %>%
    dplyr::select(cellID, everything()) %>%
    sf::st_as_sf(sf_column_name = "geometry")
  
  return(resultDF)
}

# Assign targets for the percentile approach
fAssignTargets_Percentile <- function(featuresDF,
                                      climateSmartDF,
                                      targetsDF){
  
  spp <- featuresDF %>% # Get the list of features
    tibble::as_tibble() %>%
    dplyr::select(-cellID, -geometry) %>%
    names()
  
  suppressMessages({
    df <- featuresDF %>%
      tibble::as_tibble() %>%
      dplyr::select(-cellID, -geometry) %>%
      dplyr::summarize(across(everything(), sum)) %>% # Get the # of planning units where feature is present
      tidyr::pivot_longer(everything(), names_to = "feature", values_to = "original") %>%
      dplyr::left_join(., targetsDF) %>%
      dplyr::mutate(feature = paste0(feature, "_filtered")) # Change names
    
    df1 <- climateSmartDF %>%
      tibble::as_tibble() %>%
      dplyr::select(-cellID, -geometry) %>%
      dplyr::summarize(across(everything(), sum)) %>% # Get the # of planning units selected using the climate-smart approach
      pivot_longer(everything(), names_to = "feature", values_to = "filtered")
    
    df %<>% dplyr::left_join(., df1) %>%
      dplyr::mutate(proportion = filtered/original) %>% # Calculating proportion of climate-smart areas over areas where feat is present
      dplyr::mutate(target = target/proportion) %>% # Calculate target based on the set target per feature and the proportion
      dplyr::select(feature, target) %>% 
      dplyr::mutate(target = target/100) # Convert target to proportions
  })
  
  return(df)
  
}
