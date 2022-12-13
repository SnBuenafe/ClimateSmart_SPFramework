# title: "Helper functions"
# author: "Tin Buenafe"

# Created Helper Functions to make running prioritizr easier.

# Plot statistics
plot_statistics <- function(summary, col_name, y_axis, theme) {
  
  # Dictate palettes and what to plot
  if (theme == "ensemble"){
    color_legend <- c("#FAF7B7", "#E6C173", "#855600", "#5075BA", "#81B0CC", "#5A9E67")
    string <- "as.factor(run)"
  } 
  else if (theme == "metric") {
    color_legend <- c("#3C6342", "#289E3D", "#E6C173", "#81B0CC", "#855600")
    string <- "as.factor(run)"
  } 
  else if (theme == "LR-approach"){
    color_legend = c("#E6BA7E", "#4D3B2A", "#6984BF", "#2B8142")
    string <- "as.factor(run)"
  } 
  else if (theme == "layer"){
    color_legend = c("LowRegret" = "#4C90F5", "Single" = "#A9C2EB", "Multiple_SSP126" = "#289E3D", "Multiple_SSP245" = "#E6C173", "Multiple_SSP585" = "#855600")
    string <- "as.factor(run)"
  }
  
  plot <- ggplot(data = summary, aes_string(x = string)) +
    geom_bar(aes_string(y = col_name, fill = string), stat = 'identity', position = position_dodge()) +
    scale_fill_manual(name = 'Run',
                      values = color_legend) +
    xlab("Run") +
    ylab(y_axis) +
    theme(legend.position = "bottom") +
    theme_classic()
  
  return(plot)
  
}

# Plot features for the workflow figure
plot_AQMFeatures <- function(s1, PlanUnits, world, column){
  gg <- ggplot() + 
    geom_sf(data = s1, aes_string(fill = column), colour = NA, size = 0.1, show.legend = TRUE) +
    #    geom_sf(data = PlanUnits, colour = "lightblue", fill = NA, size = 0.1, show.legend = FALSE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PlanUnits)$xlim, ylim = st_bbox(PlanUnits)$ylim) +
    scale_colour_manual(values = c("TRUE" = "#8856a7",
                                   "FALSE" = "#e0ecf4"),
                        aesthetics = "fill") + 
    theme_bw()
  
}
