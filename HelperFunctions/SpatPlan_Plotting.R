
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

# TODO: Make each function for each metric? Plot climate layers
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

# Plot combined metric scores
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

# Plot Cohen's Correlation Matrix
fPlot_CorrPlot <- function(matrix, num) {
  # creating corrplot
  rownames(matrix) <- matrix[,1]
  n <- num + 1 # num represents the number of inputted spatial plans
  matrix_f <- matrix[,2:n]
  class(matrix_f) <- "numeric"
  
  #col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  plot <- corrplot(matrix_f, method = "shade", cl.lim = c(-0.2,1), tl.col = "black", addCoef.col = "black",
                   col = COL2('BrBG', 200), tl.srt=45)
  return(plot)
}

# Create selection frequency plot
fPlot_SelFrequency <- function(data, land) {
  gg <- ggplot() + geom_sf(data = data, aes(fill = as.factor(selection)), color = NA, size = 0.01) +
    geom_sf(data = land, color = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    geom_sf(data = boundary, color = "black", fill = NA, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(data)$xlim, ylim = st_bbox(data)$ylim) +
    scale_fill_brewer(name = "Selection Frequency",
                      palette = "PuBu", aesthetics = "fill") +
    theme_bw() +
    theme(axis.ticks = element_line(color = "black", size = 2),
          panel.border = element_rect(colour = "black", fill=NA, size=5),
          axis.text = element_text(size = 50, color = "black"))
  return(gg)
}

#### Plots for scenario Theme ####
# Plot ridge plot for Scenario Theme (i.e., comparing climate warming across the three scenarios)
fPlot_RidgeClimateScenario <- function(df) {
  gg <- ggplot() +
    geom_density_ridges_gradient(data = df %>% dplyr::filter(solution_1 == 1), aes(x = transformed, y = scenario, fill = ..x..), scale = 3) +
    scale_fill_viridis_c(name = expression('Δ'^"o"*'C yr'^"-1"*''), option = "C") +
    geom_density_ridges(data = df %>% dplyr::filter(solution_1 == 0), aes(x = transformed, y = scenario), alpha = 0.25, linetype = "dotted", scale = 3) +
    geom_vline(xintercept=(climate %>% 
                             dplyr::filter(scenario == 126))$mean_tos,
               linetype = "dashed", color = "tan1", size = 0.5) +
    geom_vline(xintercept=(climate %>% 
                             dplyr::filter(scenario == 245))$mean_tos,
               linetype = "dashed", color = "orchid3", size = 0.5) +
    geom_vline(xintercept=(climate %>% 
                             dplyr::filter(scenario == 585))$mean_tos,
               linetype = "dashed", color = "orchid4", size = 0.5) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
    labs(x = expression('Climate warming (Δ'^"o"*'C yr'^"-1"*')')) +
    theme_classic() +
    theme(axis.ticks = element_line(color = "black", size = 1),
          axis.line = element_line(colour = "black", size = 1),
          axis.text = element_text(color = "black", size = 20),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          legend.key.height = unit(1, "inch"),
          legend.text = element_text(size = 15, color = "black"),
          legend.title = element_text(size = 15, color = "black"))
  return(gg)
}

# Plot ridge plot for Scenario Theme: Selection Frequency
fPlot_RidgeSelectionScenario <- function(df) {
  gg <- ggplot(data = df) +
    geom_density_ridges(aes(x = percent, y = selection, group = selection, fill = selection),
                        scale = 5) +
    scale_fill_manual(values = c(selection_1 = "#bdc9e1",
                                 selection_2 = "#74a9cf",
                                 selection_3 = "#0570b0")) +
    geom_vline(xintercept=c(30), linetype="dashed", color = "red", size = 1) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
    labs(x = "Protection (%)", y = "selection") +
    theme_classic() +
    theme(axis.ticks = element_line(color = "black", size = 1),
          axis.line = element_line(colour = "black", size = 1),
          axis.text.x = element_text(color = "black", size = 20),
          axis.text.y = element_blank(),
          axis.title.x = element_text(color = "black", size = 20),
          axis.title.y = element_blank())
  return(gg)
}

