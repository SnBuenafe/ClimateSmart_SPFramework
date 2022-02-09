
fSpatPlan_PlotSolution <- function(s1, PlanUnits, world){
  gg <- ggplot() + 
    geom_sf(data = s1, aes(fill = solution_1), colour = NA, size = 0.1, show.legend = FALSE) +
    geom_sf(data = PlanUnits, colour = "lightblue", fill = NA, size = 0.1, show.legend = FALSE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PlanUnits)$xlim, ylim = st_bbox(PlanUnits)$ylim) +
    scale_colour_manual(values = c("TRUE" = "steelblue4",
                                   "FALSE" = "lightsteelblue2"),
                        aesthetics = c("colour", "fill")) + 
    theme_bw() +
    labs(subtitle = "Solution")
  
}


fSpatPlan_PlotPUs <- function(PlanUnits, world){
  gg <- ggplot() +
    geom_sf(data = PlanUnits, colour = "lightblue", fill = NA, size = 0.1, show.legend = FALSE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PlanUnits)$xlim, ylim = st_bbox(PlanUnits)$ylim) +
    theme_bw() +
    labs(subtitle = "Planning Units")
  
}


fSpatPlan_PlotMPAs <- function(LockedIn, world){
  gg <- ggplot() +
    geom_sf(data = LockedIn, aes(fill = locked_in), colour = "lightblue", size = 0.1, show.legend = FALSE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    scale_colour_manual(values = c("TRUE" = "blue",
                                   "FALSE" = "grey50")) + 
    scale_fill_manual(values = c("TRUE" = "blue",
                                 "FALSE" = "white")) +
    theme_bw() +
    coord_sf(
      xlim = st_bbox(LockedIn)$xlim,
      ylim = st_bbox(LockedIn)$ylim) +
    labs(subtitle = "Locked In Areas")
  
}


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

fSpatPlan_PlotClimate <- function(ClimateLayer, world, metric, from, to){
  if (metric == "velocity") {
    fill = "voccMag"
    palette = rev(brewer.pal(5, "RdYlBu"))
    quantile = ClimateLayer$voccMag
    expression = expression('km yr'^"-1"*'')
    subtitle = "Climate Velocity"
  } else if (metric %in% c("roc_tos", "roc_phos", "roc_o2os")) {
    fill = "slpTrends"
    quantile = ClimateLayer$slpTrends
    if (metric == "roc_tos") {
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
    }
  }
  
  gg <- ggplot() +
      geom_sf(data = ClimateLayer, aes(fill = !!sym(fill)), color = NA, size = 0.1, show.legend = TRUE) +
      geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) + 
      coord_sf(xlim = st_bbox(ClimateLayer)$xlim, ylim = st_bbox(ClimateLayer)$ylim) +
      scale_fill_gradientn(colors = palette, aesthetics = c("colour", "fill"),
                             limits = c(from, to),
                             oob = scales::squish) +
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
    geom_sf(data = df, aes(fill = FeatureSum), colour = "grey80", size = 0.1, show.legend = TRUE) +
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
