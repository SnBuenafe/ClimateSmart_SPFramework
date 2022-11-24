# PENALTY APPROACH

# ----- Description -----
# Create penalty scaling values for the "penalty" approach
# Inputs:
# cost: vector of cost for all PUs
# climate_metric: vector of metric score for all PUs
# direction: 1 or -1; if former- higher values are more climate-smart; if latter, lower values are more climate-smart
fPenalty_CSapproach <- function(cost,
                                climate_metric, 
                                direction) {
  # I calculated scaling using this equation:
  # scaling$_ClimateMetric$ $= \frac{(Cost_{Max} - Cost_{Min})}{(ClimateMetric_{Max} - ClimateMetric_{Min})} \cdot (Scaling_{percent})$
  #x = (max(cost) - min(cost)) / (max(climate_metric) - min(climate_metric))
  
  percentage <- c(seq(from  = 20, to = 100, by = 10), seq(from = 120, to = 200, by = 20), 400)

  x = (max(cost)) / (max(climate_metric) - min(climate_metric)) # Used max cost instead of range of cost because we're using a uniform cost layer
  
  scaling <- tibble(scaling = numeric(), penalty_value = numeric())
  
  if(direction == 1) {
    print("Higher values are more climate-smart")
    for (i in 1:length(percentage)) {
      scaling %<>% add_row(scaling = percentage[i], penalty_value = -x*percentage[i]/100)
    }
  } else if(direction == -1) {
    print("Lower values are more climate-smart")
    for (i in 1:length(percentage)) {
      scaling %<>% add_row(scaling = percentage[i], penalty_value = x*percentage[i]/100)
    }
  }
  
  return(scaling)
  
}