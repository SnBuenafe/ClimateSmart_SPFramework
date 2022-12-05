
# Plot solutions
fSpatPlan_PlotSolution <- function(s, PlanUnits, world){
  gg <- ggplot() + 
    geom_sf(data = s, aes(fill = solution_1), colour = NA, size = 0.1, show.legend = TRUE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    geom_sf(data = boundary, color = "black", fill = NA, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PlanUnits)$xlim, ylim = st_bbox(PlanUnits)$ylim) +
    scale_colour_manual(values = c("TRUE" = "#7A7196",
                                   "FALSE" = "#EDEBFF"),
                        aesthetics = "fill") + 
    theme_bw() +
    theme(axis.ticks = element_line(color = "black", linewidth = 2),
          panel.border = element_rect(colour = "black", fill=NA, linewidth = 5),
          axis.text = element_text(color = "black", size = 50))
  
}

# Plot planning units
fSpatPlan_PlotPUs <- function(PlanUnits, world){
  gg <- ggplot() +
    geom_sf(data = PlanUnits, fill = "#EDEBFF", color = "grey64", size = 0.05, show.legend = FALSE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(PlanUnits)$xlim, ylim = st_bbox(PlanUnits)$ylim) +
    theme_bw() +
    theme(axis.ticks = element_line(color = "black", size = 2),
          panel.border = element_rect(colour = "black", fill=NA, size=5),
          axis.text = element_text(size = 50)) +
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

# Plot rate of climate warming
fPlot_ClimateWarming <- function(df, world) {
  
  gg <- ggplot() +
    geom_sf(data = df, 
            aes(fill = transformed), 
            color = NA, 
            size = 0.1, 
            show.legend = TRUE) +
    geom_sf(data = world, 
            colour = "grey20", 
            fill = "grey20", 
            alpha = 0.9, 
            size = 0.1, 
            show.legend = FALSE) + 
    coord_sf(xlim = st_bbox(df)$xlim, 
             ylim = st_bbox(df)$ylim) +
    scale_fill_gradientn(name = expression('Δ'^"o"*'C yr'^"-1"*''),
                         colors = brewer.pal(9, "YlGn"),
                         limits = c(as.numeric(quantile(df$transformed, 0.05)), 
                                    as.numeric(quantile(df$transformed, 0.95))),
                         oob = scales::squish) +
    labs(fill = expression('Δ'^"o"*'C yr'^"-1"*''),
         subtitle = "Rate of Change in Temperature") +
    theme_bw() +
    theme(axis.ticks = element_line(color = "black", linewidth = 2),
          panel.border = element_rect(colour = "black", fill=NA, size=5),
          axis.text = element_text(size = 50)) 
    
}

# Plot rate of ocean acidification
fPlot_OceanAcidification <- function(df, world) {
  
  gg <- ggplot() +
    geom_sf(data = df, aes(fill = transformed), color = NA, size = 0.1, show.legend = TRUE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) + 
    coord_sf(xlim = st_bbox(df)$xlim, ylim = st_bbox(df)$ylim) +
    scale_fill_gradientn(name = expression('Δ pH yr'^"-1"*''),
                         colors = rev(brewer.pal(9, "YlOrBr")),
                         limits = c(as.numeric(quantile(df$transformed, 0.05)), as.numeric(quantile(df$transformed, 0.95))),
                         oob = scales::squish) +
    labs(fill = expression('Δ pH yr'^"-1"*''),
         subtitle = "Rate of Change in pH") +
    theme_bw() +
    theme(axis.ticks = element_line(color = "black", linewidth = 2),
          panel.border = element_rect(colour = "black", fill=NA, size=5),
          axis.text = element_text(size = 50)) 
  
}

# Plot rate of ocean deoxygenation
fPlot_OceanDeoxygenation <- function(df, world) {
  
  gg <- ggplot() +
    geom_sf(data = df, aes(fill = transformed), color = NA, size = 0.1, show.legend = TRUE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) + 
    coord_sf(xlim = st_bbox(df)$xlim, ylim = st_bbox(df)$ylim) +
    scale_fill_gradientn(name = expression('Δ mol m'^"-3"*' yr'^"-1"*''),
                         colors = rev(brewer.pal(9, "YlGnBu")),
                         limits = c(as.numeric(quantile(df$transformed, 0.05)), as.numeric(quantile(df$transformed, 0.95))),
                         oob = scales::squish) +
    labs(fill = expression('Δ mol m'^"-3"*' yr'^"-1"*''),
         subtitle = "Rate of Change in Oxygen") +
    theme_bw() +
    theme(axis.ticks = element_line(color = "black", linewidth = 2),
          panel.border = element_rect(colour = "black", fill=NA, size=5),
          axis.text = element_text(size = 50)) 
  
}

# Plot climate velocity
fPlot_ClimateVelocity <- function(df, world) {
  
  gg <- ggplot() +
    geom_sf(data = df, aes(fill = transformed), color = NA, size = 0.1, show.legend = TRUE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) + 
    coord_sf(xlim = st_bbox(df)$xlim, ylim = st_bbox(df)$ylim) +
    scale_fill_gradientn(name = expression('km yr'^"-1"*''),
                         colors = rev(brewer.pal(11, "RdYlBu")),
                         limits = c(as.numeric(quantile(df$transformed, 0.05)), as.numeric(quantile(df$transformed, 0.95))),
                         oob = scales::squish) +
    labs(fill = expression('km yr'^"-1"*''),
         subtitle = "Climate Velocity") +
    theme_bw() +
    theme(axis.ticks = element_line(color = "black", linewidth = 2),
          panel.border = element_rect(colour = "black", fill=NA, size=5),
          axis.text = element_text(size = 50)) 
  
}
# Plot MHW layer
fPlot_MHW <- function(df, world){
  
  gg <- ggplot() +
    geom_sf(data = df, aes(fill = transformed), color = NA, size = 0.1, show.legend = TRUE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) + 
    coord_sf(xlim = st_bbox(df)$xlim, ylim = st_bbox(df)$ylim) +
    scale_fill_gradientn(name = "total degree days",
                         colors = brewer.pal(9, "Purples"),
                         limits = c(as.numeric(quantile(df$transformed, 0.05)), as.numeric(quantile(df$transformed, 0.95))),
                         oob = scales::squish) +
    labs(fill = "total degree days",
         subtitle = "Sum of Cumulative MHW Intensity") +
    theme_bw() +
    theme(axis.ticks = element_line(color = "black", size = 2),
          panel.border = element_rect(colour = "black", fill=NA, size=5),
          axis.text = element_text(size = 50)) 
      
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
    theme_bw() +
    theme(axis.ticks = element_line(color = "black", size = 2),
          panel.border = element_rect(colour = "black", fill=NA, size=5),
          axis.text = element_text(size = 50)) 

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

# Plotting feature richness per planning unit
fSpatPlan_FeatureNo <- function(df, world){
  
  df <- df %>% 
    as_tibble() %>% 
    mutate(FeatureSum = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>% 
    st_as_sf(sf_column_name = "geometry") %>% 
    dplyr::select(FeatureSum)
  
  gg <- ggplot() + 
    geom_sf(data = df, aes(fill = FeatureSum, color = FeatureSum), size = 0.05, show.legend = TRUE) +
    geom_sf(data = world, colour = "grey20", fill = "grey20", alpha = 0.9, size = 0.1, show.legend = FALSE) +
    coord_sf(xlim = st_bbox(df)$xlim, ylim = st_bbox(df)$ylim) +
    cmocean::scale_fill_cmocean(name = "deep", 
                                aesthetics = c("fill", "color"),
                                # limits = c(0,
                                           # as.numeric(quantile(Cost$Cost, 0.99))),
                                oob = scales::squish, 
                                trans = "log10") +
    theme_bw() +
    theme(axis.ticks = element_line(color = "black", size = 2),
          panel.border = element_rect(colour = "black", fill=NA, size=5),
          axis.text = element_text(size = 50)) +
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
    theme(axis.ticks = element_line(color = "black", linewidth = 2),
          panel.border = element_rect(colour = "black", 
                                      fill=NA, 
                                      linewidth=5),
          axis.text = element_text(size = 50, color = "black"))
  return(gg)
}

#### Plots for scenario Theme ####
# Plot statistics
fPlot_StatisticsScenario <- function(summary, col_name, y_axis) {
  string <- "as.factor(scenario)"
  gg <- ggplot(data = summary, aes_string(x = string)) +
    geom_bar(aes_string(y = col_name, fill = string), stat = 'identity', position = position_dodge()) +
    scale_fill_manual(name = 'Run',
                      values = c("126" = "#289E3D", "245" = "#E6C173", "585" = "#855600")
                      ) +
    xlab("Run") +
    ylab(y_axis) +
    theme(legend.position = "bottom") +
    theme_classic() + 
    theme(axis.text = element_text(size = 25))
}

# Plot ridge plot for Scenario Theme's features
fPlot_RidgeTargetScenario <- function(df) {
  gg <- ggplot(data = df) +
    geom_density_ridges(aes(x = percent, 
                            y = scenario, 
                            group = scenario, 
                            fill = scenario),
                        scale = 2) +
    scale_fill_manual(values = c(`EM-Percentile-tos-126` = "#289E3D",
                                 `EM-Percentile-tos-245` = "#E6C173",
                                 `EM-Percentile-tos-585` = "#855600")) +
    geom_vline(xintercept=c(30), linetype="dashed", color = "red", linewidth = 1) +
    theme_classic()
}

# Plot ridge plot for Scenario Theme (i.e., comparing climate warming across the three scenarios)
fPlot_RidgeClimateScenario <- function(df, climate) {
  gg <- ggplot() +
    geom_density_ridges_gradient(data = df %>% 
                                   dplyr::filter(solution_1 == 1), 
                                 aes(x = transformed, y = scenario, fill = ..x..), 
                                 scale = 3) +
    scale_fill_viridis_c(name = expression('Δ'^"o"*'C yr'^"-1"*''), 
                         option = "C") +
    geom_density_ridges(data = df %>% 
                          dplyr::filter(solution_1 == 0), 
                        aes(x = transformed, y = scenario), 
                        alpha = 0.25, 
                        linetype = "dotted", 
                        scale = 3) +
    geom_vline(xintercept=(climate %>% 
                             dplyr::filter(scenario == 126))$mean_tos,
               linetype = "dashed", 
               color = "tan1", 
               linewidth = 0.5) +
    geom_vline(xintercept=(climate %>% 
                             dplyr::filter(scenario == 245))$mean_tos,
               linetype = "dashed", 
               color = "orchid3", 
               linewidth = 0.5) +
    geom_vline(xintercept=(climate %>% 
                             dplyr::filter(scenario == 585))$mean_tos,
               linetype = "dashed", 
               color = "orchid4", 
               linewidth = 0.5) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
    labs(x = expression('Climate warming (Δ'^"o"*'C yr'^"-1"*')')) +
    theme_classic() +
    theme(axis.ticks = element_line(color = "black", linewidth = 1),
          axis.line = element_line(colour = "black", linewidth = 1),
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

#### Plots for ensemble theme ####
# Plot statistics
fPlot_StatisticsEnsemble <- function(summary, col_name, y_axis) {
  string <- "as.factor(run)"
  plot <- ggplot(data = summary, aes_string(x = string)) +
    geom_bar(aes_string(y = col_name, fill = string), stat = 'identity', position = position_dodge()) +
    scale_fill_manual(name = 'Run',
                      values = c("#FAF7B7", "#E6C173", "#855600", "#5075BA", "#81B0CC", "#5A9E67")) +
    xlab("Run") +
    ylab(y_axis) +
    theme(legend.position = "bottom") +
    theme_classic() +
    theme(axis.text = element_text(size = 25),
          axis.text.x = element_blank())
}
# Plot ridge plot for Ensemble Theme's features
fPlot_RidgeTargetEnsemble <- function(df) {
  gg <- ggplot(data = df) +
    geom_density_ridges(aes(x = percent, y = ensemble, group = ensemble, fill = ensemble),
                        scale = 2) +
    scale_fill_manual(values = c(`EM_Percentile_tos_585` = "#FAF7B7",
                                 `MM-CanESM5_Percentile_tos_585` = "#E6C173",
                                 `MM-CMCC-ESM2_Percentile_tos_585` = "#855600",
                                 `MM-GFDL-ESM4_Percentile_tos_585` = "#5075BA",
                                 `MM-IPSL-CM6A-LR_Percentile_tos_585` = "#81B0CC",
                                 `MM-NorESM2-MM_Percentile_tos_585` = "#5A9E67")) +
    geom_vline(xintercept=c(30), linetype="dashed", color = "red", linewidth = 1) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
    labs(x = "Protection (%)", y = "selection") +
    theme_classic() +
    theme(axis.ticks = element_line(color = "black", size = 1),
          axis.line = element_line(colour = "black", linewidth = 1),
          axis.text.x = element_text(color = "black", size = 20),
          axis.text.y = element_blank(),
          axis.title.x = element_text(color = "black", size = 20),
          axis.title.y = element_blank())
}
# Plot ridge plot for Ensemble Theme (i.e., comparing climate warming across the five models + EM approach)
fPlot_RidgeClimateEnsemble <- function(df, climate) {
  intercept1 <- (climate %>% 
                   dplyr::filter(grepl("NorESM2|GFDL", run)))$mean_tos
  intercept2 <- (climate %>% 
                   dplyr::filter(grepl("EM|CMCC", run)))$mean_tos
  intercept3 <- (climate %>% 
                   dplyr::filter(grepl("IPSL|CanESM5", run)))$mean_tos
  
  gg <- ggplot() +
    geom_density_ridges_gradient(data = df %>% 
                                   dplyr::filter(solution_1 == 1),
                                 aes(x = transformed, 
                                     y = ensemble, 
                                     fill = ..x..), 
                                 scale = 1) +
    scale_fill_viridis_c(name = expression('Δ'^"o"*'C yr'^"-1"*''), 
                         option = "C") +
    geom_density_ridges(data = df %>% 
                          dplyr::filter(solution_1 == 0), 
                        aes(x = transformed, 
                            y = ensemble), 
                        alpha = 0.25, 
                        linetype = "dotted", 
                        scale = 1) +
    geom_vline(xintercept = intercept1,
               linetype = "dashed", 
               color = "tan1", 
               linewidth = 0.5) +
    geom_vline(xintercept = intercept2,
               linetype = "dashed", 
               color = "orchid3", 
               linewidth = 0.5) +
    geom_vline(xintercept = intercept3,
               linetype = "dashed", 
               color = "orchid4", 
               linewidth = 0.5) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
    labs(x = expression('Climate warming (Δ'^"o"*'C yr'^"-1"*')')) +
    theme_classic() +
    theme(axis.ticks = element_line(color = "black", linewidth = 1),
          axis.line = element_line(colour = "black", linewidth = 1),
          axis.text = element_text(color = "black", size = 20),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          legend.key.height = unit(1, "inch"),
          legend.text = element_text(size = 15, color = "black"),
          legend.title = element_text(size = 15, color = "black"))
  
  return(gg)
}
# Plot ridge plot for Ensemble Theme: Selection Frequency
fPlot_RidgeSelectionEnsemble <- function(df) {
  gg <- ggplot(data = x) +
    geom_density_ridges(aes(x = percent, y = selection, group = selection, fill = selection),
                        scale = 5) +
    scale_fill_manual(values = c(`selection_1` = "#d0d1e6",
                                 `selection_2` = "#a6bddb",
                                 `selection_3` = "#74a9cf",
                                 `selection_4` = "#2b8cbe",
                                 `selection_5` = "#045a8d")) +
    geom_vline(xintercept=c(30), linetype="dashed", color = "red", linewidth = 1) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
    labs(x = "Protection (%)", y = "selection") +
    theme_classic() +
    theme(axis.ticks = element_line(color = "black", linewidth = 1),
          axis.line = element_line(colour = "black", linewidth = 1),
          axis.text.x = element_text(color = "black", size = 20),
          axis.text.y = element_blank(),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank())
}

#### Plots for metric theme ####
# Plot statistics
fPlot_StatisticsMetric <- function(summary, col_name, y_axis) {
  string <- "as.factor(metric)"
  summary %<>% 
    dplyr::mutate(metric = case_when(str_detect(run, "tos") ~ "tos",
                                     str_detect(run, "phos") ~ "phos",
                                     str_detect(run, "o2os") ~ "o2os",
                                     str_detect(run, "velocity") ~ "velocity",
                                     str_detect(run, "MHW") ~ "MHW",
                                     str_detect(run, "CombinedMetric") ~ "CombinedMetric")) # mutating metric
  
  gg <- ggplot(data = summary, aes_string(x = string)) +
    geom_bar(aes_string(y = col_name, fill = string), stat = 'identity', position = position_dodge()) +
    scale_fill_manual(name = 'Run',
                      values = c(tos = "#289E3D",
                                 phos = "#E6C173",
                                 o2os = "#81B0CC",
                                 velocity = "#855600",
                                 MHW = "#3C6342",
                                 CombinedMetric = "#BFA1BD")
    ) +
    xlab("Run") +
    ylab(y_axis) +
    theme(legend.position = "bottom") +
    theme_classic() + 
    theme(axis.text = element_text(size = 25))
}
# Plot ridge plot for Metric Theme's features
fPlot_RidgeTargetMetric <- function(df) {
  df %<>% 
    dplyr::mutate(met = case_when(str_detect(metric, "tos") ~ "tos",
                                     str_detect(metric, "phos") ~ "phos",
                                     str_detect(metric, "o2os") ~ "o2os",
                                     str_detect(metric, "velocity") ~ "velocity",
                                     str_detect(metric, "MHW") ~ "MHW",
                                     str_detect(metric, "CombinedMetric") ~ "CombinedMetric")) # mutating metric
  
  gg <- ggplot(data = df) +
    geom_density_ridges(aes(x = percent, y = met, group = met, fill = met),
                        scale = 2) +
    scale_fill_manual(values = c(tos = "#289E3D",
                                 phos = "#E6C173",
                                 o2os = "#81B0CC",
                                 velocity = "#855600",
                                 MHW = "#3C6342",
                                 CombinedMetric = "#BFA1BD")) +
    geom_vline(xintercept=c(30), linetype="dashed", color = "red", linewidth = 1) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
    labs(x = "Protection (%)", y = "selection") +
    theme_classic() +
    theme(axis.ticks = element_line(color = "black", linewidth = 1),
          axis.line = element_line(colour = "black", linewidth = 1),
          axis.text.x = element_text(color = "black", size = 20),
          axis.text.y = element_blank(),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank())
}
# Plot ridge plot for Metric Theme: Selection Frequency
fPlot_RidgeSelectionMetric <- function(df) {
  ggplot(data = df) +
    geom_density_ridges(aes(x = percent, y = selection, group = selection, fill = selection),
                        scale = 5) +
    scale_fill_manual(values = c(selection_1 = "#d0d1e6",
                                 selection_2 = "#a6bddb",
                                 selection_3 = "#74a9cf",
                                 selection_4 = "#2b8cbe",
                                 selection_5 = "#045a8d",
                                 selection_6 = "#023858")) +
    geom_vline(xintercept=c(30), linetype="dashed", color = "red", linewidth = 1) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
    labs(x = "Protection (%)", y = "selection") +
    theme_classic() +
    theme(axis.ticks = element_line(color = "black", linewidth = 1),
          axis.line = element_line(colour = "black", linewidth = 1),
          axis.text.x = element_text(color = "black", size = 20),
          axis.text.y = element_blank(),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank())
}

#### Plotting for approach theme ####
# Plot statistics
fPlot_StatisticsApproach <- function(summary, col_name, y_axis) {
  string <- "as.factor(run)"
  gg <- ggplot(data = summary, aes_string(x = string)) +
    geom_bar(aes_string(y = col_name, fill = string), stat = 'identity', position = position_dodge()) +
    scale_fill_manual(name = 'Run',
                      values = c("#E6BA7E", "#4D3B2A", "#6984BF", "#2B8142")
    ) +
    xlab("Run") +
    ylab(y_axis) +
    theme(legend.position = "bottom") +
    theme_classic() + 
    theme(axis.text = element_text(size = 25))
}
# Plot ridge plot for Approach Theme's features
fPlot_RidgeTargetApproach <- function(df) {
  names <-  c("Penalty", "Climate Priority Area", "Feature", "Percentile")
  df %<>%
    dplyr::mutate(approach = case_when(str_detect(approach, "Feature") ~ "Feature",
                                       str_detect(approach, "Percentile") ~ "Percentile",
                                       str_detect(approach, "Penalty") ~ "Penalty",
                                       str_detect(approach, "ClimatePriorityArea") ~ "Climate Priority Area")) %>%
    dplyr::mutate(approach = fct_relevel(approach, names))
  gg <- ggplot(data = df) +
    geom_density_ridges(aes(x = percent, y = approach, group = approach, fill = approach),
                        scale = 2) +
    scale_fill_manual(values = c(`Climate Priority Area` = "#E6BA7E",
                                 `Feature` = "#4D3B2A",
                                 `Penalty` = "#6984BF",
                                 `Percentile` = "#2B8142")) +
    geom_vline(xintercept=c(30), linetype="dashed", color = "red", linewidth = 1) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
    labs(x = "Protection (%)", y = "selection") +
    theme_classic() +
    theme(axis.ticks = element_line(color = "black", linewidth = 1),
          axis.line = element_line(colour = "black", linewidth = 1),
          axis.text.x = element_text(color = "black", size = 20),
          axis.text.y = element_blank(),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank())
}
# Plot ridge plot for Metric Theme: Selection Frequency
fPlot_RidgeSelectionApproach <- function(df) {
  ggplot(data = df) +
    geom_density_ridges(aes(x = percent, y = selection, group = selection, fill = selection),
                        scale = 2) +
    scale_fill_manual(values = c(selection_1 = "#bdc9e1",
                                 selection_2 = "#74a9cf",
                                 selection_3 = "#2b8cbe",
                                 selection_4 = "#045a8d")) +
    geom_vline(xintercept=c(30), linetype="dashed", color = "red", linewidth = 1) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
    labs(x = "Protection (%)", y = "selection") +
    theme_classic() +
    theme(axis.ticks = element_line(color = "black", linewidth = 1),
          axis.line = element_line(colour = "black", linewidth = 1),
          axis.text.x = element_text(color = "black", size = 20),
          axis.text.y = element_blank(),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank())
}
# Plot ridge plot for Approach Theme (i.e., comparing climate warming across the four CS approaches)
fPlot_RidgeClimateApproach <- function(df, climate) {
  names <-  c("Penalty", "Climate Priority Area", "Feature", "Percentile")
  df %<>%
    dplyr::mutate(approach = case_when(str_detect(approach, "Feature") ~ "Feature",
                                       str_detect(approach, "Percentile") ~ "Percentile",
                                       str_detect(approach, "Penalty") ~ "Penalty",
                                       str_detect(approach, "ClimatePriorityArea") ~ "Climate Priority Area")) %>%
    dplyr::mutate(approach = fct_relevel(approach, names))
  
  gg <- ggplot() +
    geom_density_ridges_gradient(data = df %>% 
                                   dplyr::filter(solution_1 == 1), 
                                 aes(x = transformed, 
                                     y = approach, 
                                     fill = ..x..), 
                                 scale = 1) +
    scale_fill_viridis_c(name = expression('Δ'^"o"*'C yr'^"-1"*''), option = "C") +
    geom_density_ridges(data = df %>% 
                          dplyr::filter(solution_1 == 0), 
                        aes(x = transformed, y = approach), 
                        alpha = 0.25, 
                        linetype = "dotted", 
                        scale = 1) +
    geom_vline(xintercept = climate$mean_tos,
               linetype = "dashed", 
               color = "tan1", 
               linewidth = 0.5) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
    labs(x = expression('Climate warming (Δ'^"o"*'C yr'^"-1"*')')) +
    theme_classic() +
    theme(axis.ticks = element_line(color = "black", linewidth = 1),
          axis.line = element_line(colour = "black", linewidth = 1),
          axis.text = element_text(color = "black", size = 20),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.key.height = unit(1, "inch"),
          legend.text = element_text(size = 15, color = "black"),
          legend.title = element_text(size = 15, color = "black"))
}
# Plot ridge plot for Approach Theme (i.e., comparing ocean acidification across the four CS approaches)
fPlot_RidgeAcidificationApproach <- function(df, climate) {
  names <-  c("Penalty", "Climate Priority Area", "Feature", "Percentile")
  df %<>%
    dplyr::mutate(approach = case_when(str_detect(approach, "Feature") ~ "Feature",
                                       str_detect(approach, "Percentile") ~ "Percentile",
                                       str_detect(approach, "Penalty") ~ "Penalty",
                                       str_detect(approach, "Climate Priority Area") ~ "Climate Priority Area")) %>%
    dplyr::mutate(approach = fct_relevel(approach, names))
  
  gg <- ggplot() +
    geom_density_ridges_gradient(data = df %>% 
                                   dplyr::filter(solution_1 == 1), 
                                 aes(x = transformed, 
                                     y = approach, 
                                     group = approach, 
                                     fill = ..x..), 
                                 scale = 1) +
    scale_fill_viridis_c(name = expression('Δ pH yr'^"-1"*''), 
                         option = "A") +
    geom_density_ridges(data = df %>% 
                          dplyr::filter(solution_1 == 0),
                        aes(x = transformed, y = approach), 
                        alpha = 0.25, 
                        linetype = "dotted", 
                        scale = 1) +
    geom_vline(xintercept = climate$mean_phos,
               linetype = "dashed", 
               color = "tan1", 
               linewidth = 0.5) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
    labs(x = expression('Ocean acidification (Δ pH yr'^"-1"*')')) +
    theme_classic() +
    theme(axis.ticks = element_line(color = "black", size = 1),
          axis.line = element_line(colour = "black", linewidth = 1),
          axis.text = element_text(color = "black", size = 20),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.key.height = unit(1, "inch"),
          legend.text = element_text(size = 15, color = "black"),
          legend.title = element_text(size = 15, color = "black"))
}

# Plot ridge plot for Approach Theme (i.e., comparing ocean deoxygenation across the four CS approaches)
fPlot_RidgeDeoxygenationApproach <- function(df, climate) {
  names <-  c("Penalty", "Climate Priority Area", "Feature", "Percentile")
  df %<>%
    dplyr::mutate(approach = case_when(str_detect(approach, "Feature") ~ "Feature",
                                       str_detect(approach, "Percentile") ~ "Percentile",
                                       str_detect(approach, "Penalty") ~ "Penalty",
                                       str_detect(approach, "Climate Priority Area") ~ "Climate Priority Area")) %>%
    dplyr::mutate(approach = fct_relevel(approach, names))
  
  gg <- ggplot() +
    geom_density_ridges_gradient(data = df %>% 
                                   dplyr::filter(solution_1 == 1), 
                                 aes(x = transformed, 
                                     y = approach, 
                                     fill = ..x..), 
                                 scale = 1) +
    scale_fill_viridis_c(name = expression('Δ mol m'^"-3"*' yr'^"-1"*''), 
                         option = "D") +
    geom_density_ridges(data = df %>% 
                          dplyr::filter(solution_1 == 0), 
                        aes(x = transformed, 
                            y = approach), 
                        alpha = 0.25, 
                        linetype = "dotted", 
                        scale = 1) +
    geom_vline(xintercept = climate$mean_o2os,
               linetype = "dashed", 
               color = "black", 
               linewidth = 0.5) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
    labs(x = expression('Ocean deoxygenation (Δ mol m'^"-3"*' yr'^"-1"*')')) +
    theme_classic() +
    theme(axis.ticks = element_line(color = "black", size = 1),
          axis.line = element_line(colour = "black", linewidth = 1),
          axis.text = element_text(color = "black", size = 20),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.key.height = unit(1, "inch"),
          legend.text = element_text(size = 15, color = "black"),
          legend.title = element_text(size = 15, color = "black"))
}
# Plot ridge plot for Approach Theme (i.e., comparing climate velocity across the four CS approaches)
fPlot_RidgeVelocityApproach <- function(df, climate) {
  names <-  c("Penalty", "Climate Priority Area", "Feature", "Percentile")
  df %<>%
    dplyr::mutate(approach = case_when(str_detect(approach, "Feature") ~ "Feature",
                                       str_detect(approach, "Percentile") ~ "Percentile",
                                       str_detect(approach, "Penalty") ~ "Penalty",
                                       str_detect(approach, "Climate Priority Area") ~ "Climate Priority Area")) %>%
    dplyr::mutate(approach = fct_relevel(approach, names))
  
  gg <- ggplot() +
    geom_density_ridges_gradient(data = df %>% 
                                   dplyr::filter(solution_1 == 1), 
                                 aes(x = transformed, 
                                     y = approach, 
                                     fill = ..x..), 
                                 scale = 1) +
    scale_fill_distiller(name = expression('km yr'^"-1"*''), 
                         palette = "RdYlBu") +
    geom_density_ridges(data = df %>% 
                          dplyr::filter(solution_1 == 0), 
                        aes(x = transformed, 
                            y = approach), 
                        alpha = 0.25, 
                        linetype = "dotted", 
                        scale = 1) +
    geom_vline(xintercept = climate$median_velocity,
               linetype = "dashed", 
               color = "khaki3", 
               linewidth = 0.5) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
    labs(x = expression('Climate velocity (km yr'^"-1"*')')) +
    theme_classic() +
    theme(axis.ticks = element_line(color = "black", linewidth = 1),
          axis.line = element_line(colour = "black", linewidth = 1),
          axis.text = element_text(color = "black", size = 20),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.key.height = unit(1, "inch"),
          legend.text = element_text(size = 15, color = "black"),
          legend.title = element_text(size = 15, color = "black"))
}
# Plot ridge plot for Approach Theme (i.e., comparing MHW intensity across the four CS approaches)
fPlot_RidgeMHWApproach <- function(df, climate) {
  names <-  c("Penalty", "Climate Priority Area", "Feature", "Percentile")
  df %<>%
    dplyr::mutate(approach = case_when(str_detect(approach, "Feature") ~ "Feature",
                                       str_detect(approach, "Percentile") ~ "Percentile",
                                       str_detect(approach, "Penalty") ~ "Penalty",
                                       str_detect(approach, "Climate Priority Area") ~ "Climate Priority Area")) %>%
    dplyr::mutate(approach = fct_relevel(approach, names))
  
  gg <- ggplot() +
    geom_density_ridges_gradient(data = df %>% 
                                   dplyr::filter(solution_1 == 1), 
                                 aes(x = transformed, 
                                     y = approach, 
                                     fill = ..x..), 
                                 scale = 1) +
    
    scale_fill_viridis_c(name = expression('total degree days'), 
                         option = "G") +
    geom_density_ridges(data = df %>% 
                          dplyr::filter(solution_1 == 0), 
                        aes(x = transformed, 
                            y = approach), 
                        alpha = 0.25, 
                        linetype = "dotted", 
                        scale = 1) +
    geom_vline(xintercept = climate$mean_MHW,
               linetype = "dashed", 
               color = "khaki3", 
               linewidth = 0.5) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
    labs(x = expression('MHW Intensity (total degree days)')) +
    theme_classic() +
    theme(axis.ticks = element_line(color = "black", linewidth = 1),
          axis.line = element_line(colour = "black", linewidth = 1),
          axis.text = element_text(color = "black", size = 20),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.key.height = unit(1, "inch"),
          legend.text = element_text(size = 15, color = "black"),
          legend.title = element_text(size = 15, color = "black"))
}
# Plot ridge plot for Approach Theme (i.e., comparing combined metric across the four CS approaches)
fPlot_RidgeCombinedMetricApproach <- function(df, climate) {
  names <-  c("Penalty", "Climate Priority Area", "Feature", "Percentile")
  df %<>%
    dplyr::mutate(approach = case_when(str_detect(approach, "Feature") ~ "Feature",
                                       str_detect(approach, "Percentile") ~ "Percentile",
                                       str_detect(approach, "Penalty") ~ "Penalty",
                                       str_detect(approach, "Climate Priority Area") ~ "Climate Priority Area")) %>%
    dplyr::mutate(approach = fct_relevel(approach, names))
  
  gg <- ggplot() +
    geom_density_ridges_gradient(data = df %>% 
                                   dplyr::filter(solution_1 == 1), 
                                 aes(x = transformed, 
                                     y = approach, 
                                     fill = ..x..), 
                                 scale = 1) +
    scale_fill_gradientn(name = "Metric Score",
                         colors = brewer.pal(9, "YlGnBu")) +
    geom_density_ridges(data = df %>% 
                          dplyr::filter(solution_1 == 0), 
                        aes(x = transformed, 
                            y = approach), 
                        alpha = 0.25, 
                        linetype = "dotted", 
                        scale = 1) +
    geom_vline(xintercept = climate$mean_CombinedMetric,
               linetype = "dashed", 
               color = "khaki3", 
               linewidth = 0.5) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_discrete(expand = expansion(mult = c(0.01, 0))) +
    labs(x = expression('Combined metric score')) +
    theme_classic() +
    theme(axis.ticks = element_line(color = "black", linewidth = 1),
          axis.line = element_line(colour = "black", linewidth = 1),
          axis.text = element_text(color = "black", size = 20),
          axis.title.x = element_text(size = 20),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          legend.key.height = unit(1, "inch"),
          legend.text = element_text(size = 15, color = "black"),
          legend.title = element_text(size = 15, color = "black"))
}
