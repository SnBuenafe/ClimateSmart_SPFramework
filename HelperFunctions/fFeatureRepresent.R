# Description:
# This function calculates the selection percentage of each feature

fFeatureRepresent <- function(p, s, col_name) {
  feat_rep <- eval_feature_representation_summary(p, s[, 'solution_1']) %>% 
    dplyr::select(feature, relative_held) %>% 
    mutate(relative_held = relative_held*100) %>% 
    rename(!!sym(col_name) := relative_held)
  
  if(grepl(pattern = "penalty|feature", x = col_name, ignore.case = TRUE)) {
    feat_rep %<>% add_row(feature = "climate_layer", !!sym(col_name) := NA)
  }
  
  return(feat_rep)
}