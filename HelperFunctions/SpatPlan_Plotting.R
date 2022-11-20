
# Plot solutions
fSpatPlan_PlotSolution <- function(s1, PlanUnits, world){
  gg <- ggplot() + 
    geom_sf(data = s1, aes(fill = solution_1), colour = NA, size = 0.1, show.legend = TRUE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    geom_sf(data = boundary, color = "black", fill = NA, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PlanUnits)$xlim, ylim = st_bbox(PlanUnits)$ylim) +
    scale_colour_manual(values = c("TRUE" = "#7A7196",
                                   "FALSE" = "#EDEBFF"),
                        aesthetics = "fill") + 
    theme_bw() +
    theme(axis.ticks = element_line(color = "black", size = 2),
          panel.border = element_rect(colour = "black", fill=NA, size=5),
          axis.text = element_text(color = "black", size = 50))
  
}

# Plot planning units
fSpatPlan_PlotPUs <- function(PlanUnits, world){
  gg <- ggplot() +
    geom_sf(data = PlanUnits, fill = "lightsteelblue2", color = "grey64", size = 0.05, show.legend = FALSE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PlanUnits)$xlim, ylim = st_bbox(PlanUnits)$ylim) +
    theme_bw() +
    labs(subtitle = "Planning Units")
  
}

# Plot cost
fSpatPlan_PlotCost <- function(Cost, world){
  gg <- ggplot() + 
    geom_sf(data = Cost, aes(fill = Cost), colour = "grey80", size = 0.1, show.legend = TRUE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(Cost)$xlim, ylim = st_bbox(Cost)$ylim) +
    cmocean::scale_fill_cmocean(name = "deep", 
                                aesthetics = c("colour", "fill"),
                                limits = c(0,
                                           as.numeric(quantile(Cost$Cost, 0.99))),
                                oob = scales::squish) +
    theme_bw() +
    labs(subtitle = "Cost (USD)")
  
}

# Plot climate layers
fSpatPlan_PlotClimate <- function(ClimateLayer, world, metric){
  
  if (metric == "velocity") {
    palette = rev(brewer.pal(11, "RdYlBu"))
    expression = expression('km yr'^"-1"*'')
    subtitle = "Climate Velocity"
  } else if (metric == "roc_tos") {
      palette = brewer.pal(9, "YlGn")
      expression = expression('Δ'^"o"*'C yr'^"-1"*'')
      subtitle = "Rate of Change in Temperature"
  } else if (metric == "roc_phos") {
      palette = rev(brewer.pal(9, "YlOrBr"))
      expression = expression('Δ pH yr'^"-1"*'')
      subtitle = "Rate of Change in pH"
  } else if (metric == "roc_o2os") {
      palette = rev(brewer.pal(9, "YlGnBu"))
      expression = expression('Δ mol m'^"-3"*' yr'^"-1"*'')
      subtitle = "Rate of Change in Oxygen"
  } else if (str_detect(metric, "MHW")) {
      palette = brewer.pal(9, "Purples")
      expression = expression('total degree days')
      subtitle = "MHW metrics"
  } 
  
  gg <- ggplot() +
      geom_sf(data = ClimateLayer, aes(fill = transformed), color = NA, size = 0.1, show.legend = TRUE) +
      geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) + 
      coord_sf(xlim = st_bbox(ClimateLayer)$xlim, ylim = st_bbox(ClimateLayer)$ylim) +
    scale_fill_gradientn(name = expression,
                         colors = palette,
                         limits = c(as.numeric(quantile(ClimateLayer$transformed, 0.05)), as.numeric(quantile(ClimateLayer$transformed, 0.95))),
                         oob = scales::squish)+
      labs(fill = expression,
           subtitle = subtitle) +
      theme_bw()
      
}

fSpatPlan_PlotCombinedClimate <- function(ClimateLayer, world) {
  
  expression = expression('Metric score')
  subtitle = "Combined climate-smart metric"
  
  gg <- ggplot() +
    geom_sf(data = ClimateLayer, aes(fill = combined), color = NA, size = 0.1, show.legend = TRUE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) + 
    coord_sf(xlim = st_bbox(ClimateLayer)$xlim, ylim = st_bbox(ClimateLayer)$ylim) +
    scale_fill_viridis_c(name = expression,
                         option = "viridis",
                         direction = -1,
                         oob = scales::squish,
                         na.value = "grey64") +
    labs(fill = expression,
         subtitle = subtitle) +
    theme_bw()

}

fSpatPlan_PlotComparison <- function(soln1, soln2, world){
  
  soln <- soln1 %>%
    dplyr::select(solution_1) %>%
    bind_cols(soln2 %>% as_tibble() %>% dplyr::select(solution_1) %>% rename(solution_2 = solution_1)) %>%
    mutate(Combined = solution_1 + solution_2) %>%
    mutate(Compare = case_when(Combined == 2 ~ "Same",
                               solution_1 == 1 & solution_2 == 0 ~ "Removed (-)",
                               solution_1 == 0 & solution_2 == 1 ~ "Added (+)"),
           Compare = factor(Compare, levels = c("Added (+)", "Same", "Removed (-)"))) %>%
    filter(!is.na(Compare))
  
  gg <- ggplot() +
    geom_sf(data = soln, aes(fill = Compare), colour = NA, size = 0.0001) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(soln)$xlim, ylim = st_bbox(soln)$ylim) +
    theme_bw() +
    scale_fill_manual(values = c("Added (+)" = "Red", "Same" = "ivory3", "Removed (-)" = "Blue"), drop = FALSE)
  
}


fSpatPlan_FeatureNo <- function(df, world){
  
  df <- df %>% 
    as_tibble() %>% 
    mutate(FeatureSum = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>% 
    st_as_sf(sf_column_name = "geometry") %>% 
    dplyr::select(FeatureSum)
  
  gg <- ggplot() + 
    geom_sf(data = df, aes(fill = FeatureSum), colour = "grey64", size = 0.05, show.legend = TRUE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(df)$xlim, ylim = st_bbox(df)$ylim) +
    cmocean::scale_fill_cmocean(name = "deep", 
                                aesthetics = c("fill"),
                                # limits = c(0,
                                           # as.numeric(quantile(Cost$Cost, 0.99))),
                                oob = scales::squish, 
                                trans = "log10") +
    theme_bw() +
    labs(subtitle = "Number of Features")
  
}
