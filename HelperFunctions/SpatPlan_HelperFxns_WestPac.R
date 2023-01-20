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


