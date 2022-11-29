# ----- Description -----
# This function creates a climate layer by selecting the most climate-smart areas in the entire planning region
# Inputs:
# featuresDF: feature sf object which should have a column for cellID
# percentile: cut-off threshold for determining whether an area is a climate priority area or not. Note that the percentile here is the lower limit of the threshold.
# metricDF: climate metric data.frame with 'metric' as the column name of the metric values per planning unit. This should also have a column for the cellID
# direction: If direction = 1, metric values are from low (least climate-smart) to high (most climate-smart). If direction = -1, metric values are from high (least climate-smart) to low (most climate-smart).
# targetsDF: data.frame that has the list of features under "feature" column and their corresponding targets under "target" column
# refugiaTarget: target assigned to climate layer

fFeature_CSapproach <- function(featuresDF,
                                percentile,
                                metricDF,
                                direction
) {
  
  if(any(apply(metricDF, 2, is.na)[, "metric"])){
    print("There are some NAs in the metric data. Please check.")
  }
  prct <- percentile/100
  qntl <- quantile(metricDF$metric, prct)[[1]] # Get the percentile
  if(direction == 1) {
    print("Higher values mean more climate-smart areas.")
    df <- metricDF %>%
      dplyr::mutate(climate_layer = ifelse(metric >= qntl, yes = 1, no = 0)) # Create the climate layer
  } else if(direction == -1) {
    print("Lower values mean more climate-smart areas.")
    df <- metricDF %>%
      dplyr::mutate(climate_layer = ifelse(metric <= qntl, yes = 1, no = 0)) # Create the climate layer
  } else {
    if(i == 1) {
      print("Please enter a valid direction: either 1 or -1.")
    }
  }
  
  # Get the most climate-smart areas
  climateSmartDF <- df %>%
    tibble::as_tibble() %>%
    dplyr::select(cellID, climate_layer)
  
  # Attach "climate_layer" to the features df and have this as the output
  featuresDF %<>% 
    dplyr::left_join(., climateSmartDF, by = "cellID")

  return(featuresDF)
}

# Assign targets for the feature approach
fAssignTargets_Feature <- function(climateSmartDF,
                                   refugiaTarget,
                                   targetsDF){
  
  # Calculate the target depending on the # of PUs deemed as "climate-smart"
  trgt <- refugiaTarget/(sum(climateSmartDF$climate_layer)/nrow(climateSmartDF)) 
  
  climate_layerDF <- tibble::tribble(
    ~feature, ~target,
    "climate_layer", trgt
  )
  
  finalDF <- targetsDF %>%
    dplyr::bind_rows(., climate_layerDF) %>% 
    dplyr::mutate(target = target/100) # Convert target to proportions
  
  return(finalDF)
}