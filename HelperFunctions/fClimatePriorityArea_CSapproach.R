# CLIMATE-PRIORITY-AREA APPROACH

# ----- Description -----
# This function splits the feature data into climate-smart (CS) and non-climate-smart (NCS) areas depending on the percentile chosen by the user.
# Inputs:
# featuresDF: feature sf object which should have a column for cellID
# percentile: cut-off threshold for determining whether an area is a climate priority area or not. Note that the percentile here is the lower limit of the threshold.
# metricDF: climate metric data.frame with 'metric' as the column name of the metric values per planning unit. This should also have a column for the cellID
# direction: If direction = 1, metric values are from low (least climate-smart) to high (most climate-smart). If direction = -1, metric values are from high (least climate-smart) to low (most climate-smart).
# targetsDF: data.frame that has the list of features under "feature" column and their corresponding targets under "target" column
# refugiaTarget: target assigned to climate-smart areas

fClimatePriorityArea_CSapproach <- function(featuresDF,
                                            percentile,
                                            metricDF,
                                            direction
                                            
) {
  
  spp <- featuresDF %>%
    tibble::as_tibble() %>%
    dplyr::select(-geometry,-cellID) %>%
    names()
  
  metric <- metricDF %>%
    dplyr::mutate(cellID = row_number()) %>%
    sf::st_drop_geometry() %>%
    tibble::as_tibble()
  
  imptList <- list() # empty list to fill in with the important features
  for(i in 1:length(spp)) {
    
    df <- featuresDF %>%
      tibble::as_tibble() %>%
      dplyr::select(!!sym(spp[i]), cellID) %>% # Select 1 species at a time
      dplyr::left_join(., metricDF, by = "cellID") %>%
      dplyr::select(-cellID)
    
    if(any((apply(df, 2, is.na))[,2])) {
      print("There are some NAs in the metric data. Please check.")
    }
    
    # Get the most climate-smart areas
    filteredDF <- df %>%
      dplyr::filter(!!sym(spp[i]) == 1) # Select rows that have biodiversity values (= 1)
    prct <- percentile/100
    qntl <- quantile(metricDF$metric, prct)[[1]] # Get the percentile
    if(direction == 1) {
      if(i == 1) {
        print("Higher values refer to more climate-resilient areas.")
      }
      df %<>%
        dplyr::mutate(V1 = ifelse(metric >= qntl, yes = 1, no = 0),
                      V2 = ifelse(!!sym(spp[i]) == 1, yes = 1, no = 0))
    } else if(direction == -1) {
      if(i == 1) {
        print("Lower values refer to more climate-resilient areas.")
      }
      df %<>%
        dplyr::mutate(V1 = ifelse(metric <= qntl, yes = 1, no = 0),
                      V2 = ifelse(!!sym(spp[i]) == 1, yes = 1, no = 0))
    } else {
      if(i == 1) {
        print("Please enter a valid direction: either 1 or -1.")
      }
    }
    
    imptList[[i]] <- df %>%
      dplyr::mutate(!!sym(paste0(spp[i], "_CS")) := V1*V2) %>%  # CS = climate-smart areas
      dplyr::select(!!sym(paste0(spp[i], "_CS")))
    
  }
  
  imptList %<>% do.call(bind_cols, .) %>%
    dplyr::mutate(cellID = row_number())
  
  repList <- list()
  for(i in 1:length(spp)) {
    
    df1 <- featuresDF %>%
      tibble::as_tibble() %>%
      dplyr::select(!!sym(spp[i]), cellID) %>% # Select 1 species at a time
      dplyr::left_join(., metricDF, by = "cellID")
    
    df2 <- imptList %>%
      dplyr::select(!!sym(paste0(spp[i], "_CS")), cellID)
    
    repList[[i]] <- left_join(df1, df2, by = "cellID") %>%
      dplyr::mutate(!!sym(paste0(spp[i], "_NCS")) := ifelse(!!sym(paste0(spp[i], "_CS")) == 1,
                                                            yes = 0,
                                                            no = .data [[ spp[i] ]])) %>%
      dplyr::select(matches('_NCS|_CS'))
  }
  
  repList %<>% do.call(bind_cols, .) %>%
    dplyr::bind_cols(., featuresDF %>% dplyr::select(cellID, geometry)) %>%
    dplyr::select(cellID, everything()) %>%
    sf::st_as_sf(sf_column_name = "geometry")
  
  return(repList) # Return df with the climate-smart areas and non-climate-smart areas
  
}

# Assign targets for climate-priority-area approach
fAssignTargets_CPA <- function(climateSmartDF,
                               targetsDF,
                               refugiaTarget) {
  spp <- targetsDF %>%
    dplyr::select(feature) %>%
    pull()
  
  featDF <- climateSmartDF %>%
    st_drop_geometry() %>%
    dplyr::select(-cellID) %>%
    dplyr::summarize(across(everything(), sum)) %>%
    pivot_longer(everything(), names_to = "feature", values_to = "planunit")
  
  finalList <- list() # empty list
  for(i in 1:length(spp)) {
    filteredTarget <- targetsDF %>% # Get the set target per feature
      dplyr::filter(feature == spp[i])
    
    trgt <- filteredTarget$target # Extracting the target set for each feature
    
    vars <- c(stringr::str_c(spp[i], "_CS"),
              stringr::str_c(spp[i], "_NCS"))
    
    suppressMessages({
      assignTarget <- featDF %>%
        dplyr::filter(.data$feature %in% vars) %>%
        dplyr::full_join(., filteredTarget)
    })
    
    sumUnits <- sum(assignTarget$planunit, na.rm = TRUE) # getting the total of the feature
    
    assignTarget1 <- assignTarget %>%
      dplyr::mutate(planunit = ifelse(str_ends(feature, paste0(spp[i])), yes = sumUnits, no = planunit)) %>%
      dplyr::mutate(proportion = planunit/sumUnits)
    
    reltargetCS <- assignTarget1[assignTarget1$feature == paste0(spp[i], "_CS"), "proportion"] %>% dplyr::pull()  # get the relative target for the climate-smart areas
    
    if(reltargetCS > assignTarget1[assignTarget1$feature == spp[i], "target"] %>% dplyr::pull()) { # Do this check; is the percentile greater than the assigned target for that feature?
      targetCS <- (assignTarget1[assignTarget1$feature == spp[i], "target"] %>% as.numeric())/(assignTarget1[assignTarget1$feature == paste0(spp[i], "_CS"), "proportion"] %>% as.numeric())
      
      targetNCS <- 0
    } else {
      targetCS <- refugiaTarget
      targetNCS <- ((assignTarget1[assignTarget1$feature == spp[i], "target"] %>% as.numeric()) - reltargetCS)/(assignTarget1[assignTarget1$feature == paste0(spp[i], "_NCS"), "proportion"] %>% as.numeric())
    }
    
    finalList[[i]] <- assignTarget1 %>%
      dplyr::mutate(target = case_when(str_ends(feature, "_CS") ~ targetCS,
                                       str_ends(feature, "_NCS") ~ targetNCS)) %>%
      dplyr::filter(feature != spp[i]) %>%
      dplyr::select(feature, target)
  }
  
  finalDF <- do.call(bind_rows, finalList)
  
  return(finalDF)
}
