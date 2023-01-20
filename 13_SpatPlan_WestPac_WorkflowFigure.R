# title: "Creating nMDS plot for each of the themes"
# author: "Tin Buenafe"

#### Preliminaries ####
# Description
# This script prepares the plots needed to assemble the figure that shows how each approach is done

# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R")
tos_SSP585 <- load_metrics(metric = "tos", model = "ensemble", scenario = "SSP 5-8.5") # Load warming
phos_SSP585 <- load_metrics(metric = "phos", model = "ensemble", scenario = "SSP 5-8.5") # Load acidification

# Plot features for the workflow figure
plot_AQMFeatures <- function(s, PlanUnits, world){
  gg <- ggplot() + 
    geom_sf(data = s, aes(fill = as.logical(solution)), colour = NA, size = 0.1, show.legend = TRUE) +
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

#### ORIGINAL DISTRIBUTION OF AN EXAMPLE ####
# Using Katsuwonus pelamis as the example for warming
plot <- plot_AQMFeatures(aqua_feature %>% 
                           dplyr::rename(solution = Katsuwonus_pelamis), PUs, land)
ggsave(filename = "Workflow-OriginalDistribution-Kpelamis.png",
       plot = plot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

# Using Stenella coeruleoalba as the example for acidification
plot <- plot_AQMFeatures(aqua_feature %>% 
                           dplyr::rename(solution = Stenella_coeruleoalba), PUs, land)
ggsave(filename = "Workflow-OriginalDistribution-Scoeruleoalba.png",
       plot = plot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

#### FEATURE APPROACH ####
# Create feature object (warming)
aqua_feature <- fFeature_CSapproach(featuresDF = aqua_sf, 
                                    percentile = 35, 
                                    metricDF = rename_metric(tos_SSP585),
                                    direction = -1 # lower values are more climate-smart
)
# Check the targets
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()
target_df <- tibble::as_tibble(features) %>% 
  dplyr::rename(feature = value) %>% 
  dplyr::mutate(target = 30) # Fixed target of 30
targets <- fAssignTargets_Feature(climateSmartDF = aqua_feature,
                                  refugiaTarget = 30,
                                  targetsDF = target_df)

plot <- plot_AQMFeatures(aqua_feature %>% 
                           dplyr::rename(solution = climate_layer), PUs, land)
ggsave(filename = "Workflow-Feature-tos.png",
       plot = plot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

# Create feature object (acidification)
aqua_feature <- fFeature_CSapproach(featuresDF = aqua_sf, 
                                    percentile = 65, 
                                    metricDF = rename_metric(phos_SSP585),
                                    direction = 1 # higher values are more climate-smart
)
# Check the targets
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()
target_df <- tibble::as_tibble(features) %>% 
  dplyr::rename(feature = value) %>% 
  dplyr::mutate(target = 30) # Fixed targets of 30
targets <- fAssignTargets_Feature(climateSmartDF = aqua_feature,
                                  refugiaTarget = 30,
                                  targetsDF = target_df)

plot <- plot_AQMFeatures(aqua_feature %>% 
                           dplyr::rename(solution = climate_layer), PUs, land)
ggsave(filename = "Workflow-Feature-phos.png",
       plot = plot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

#### PERCENTILE APPROACH ####
# Create percentile object (warming - Katsuwonus pelamis)
aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
                                          percentile = 35,
                                          metricDF = rename_metric(tos_SSP585),
                                          direction = -1 # lower values are more climate-smart
)
# Check the targets
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()
target_df <- tibble::as_tibble(features) %>% 
  dplyr::rename(feature = value) %>% 
  dplyr::mutate(target = 30) # Fixed targets of 30
targets <- fAssignTargets_Percentile(featuresDF = aqua_sf,
                                     climateSmartDF = aqua_percentile,
                                     targetsDF = target_df)

plot <- plot_AQMFeatures(aqua_percentile %>% 
                           dplyr::rename(solution = Katsuwonus_pelamis_filtered), PUs, land)
ggsave(filename = "Workflow-Percentile-tos.png",
       plot = plot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

# Create percentile object (acidification - Stenella coeruleoalba)
aqua_percentile <- fPercentile_CSapproach(featuresDF = aqua_sf, 
                                          percentile = 65,
                                          metricDF = rename_metric(phos_SSP585),
                                          direction = 1 # higher values are more climate-smart
)
# Check the targets
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()
target_df <- tibble::as_tibble(features) %>% 
  dplyr::rename(feature = value) %>% 
  dplyr::mutate(target = 30) # Fixed targets of 30
targets <- fAssignTargets_Percentile(featuresDF = aqua_sf,
                                     climateSmartDF = aqua_percentile,
                                     targetsDF = target_df)

plot <- plot_AQMFeatures(aqua_percentile %>% 
                           dplyr::rename(solution = Stenella_coeruleoalba_filtered), PUs, land)
ggsave(filename = "Workflow-Percentile-phos.png",
       plot = plot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

#### CLIMATE PRIORITY AREA APPROACH ####
# Create CPA object (warming - Katsuwonus pelamis)
aqua_CPA <- fClimatePriorityArea_CSapproach(featuresDF = aqua_sf,
                                            percentile = 5, # Considering the top 5 percentile of each feature as climate-smart areas
                                            metricDF = rename_metric(tos_SSP585),
                                            direction = -1 # lower values are more climate-smart
)
# Check the targets
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()
target_df <- tibble::as_tibble(features) %>% 
  dplyr::rename(feature = value) %>% 
  dplyr::mutate(target = 0.3) # Fixed targets of 30
targets <- fAssignTargets_CPA(climateSmartDF = aqua_CPA,
                              targetsDF = target_df,
                              refugiaTarget = 1 # 100% protection to the most climate-smart areas
)

plot <- plot_AQMFeatures(aqua_CPA %>% 
                           dplyr::rename(solution = Katsuwonus_pelamis_CS), PUs, land)
ggsave(filename = "Workflow-ClimatePriorityArea-tos-CS.png",
       plot = plot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

plot <- plot_AQMFeatures(aqua_CPA %>% 
                           dplyr::rename(solution = Katsuwonus_pelamis_NCS), PUs, land)
ggsave(filename = "Workflow-ClimatePriorityArea-tos-NCS.png",
       plot = plot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

# Create CPA object (acidification - Stenella coeruleoalba)
aqua_CPA <- fClimatePriorityArea_CSapproach(featuresDF = aqua_sf,
                                            percentile = 95, # Considering the top 5 percentile of each feature as climate-smart areas
                                            metricDF = rename_metric(phos_SSP585),
                                            direction = 1 # higher values are more climate-smart
)
# Check the targets
features <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(-geometry, -cellID) %>% 
  names()
target_df <- tibble::as_tibble(features) %>% 
  dplyr::rename(feature = value) %>% 
  dplyr::mutate(target = 0.3) # Fixed targets of 30
targets <- fAssignTargets_CPA(climateSmartDF = aqua_CPA,
                              targetsDF = target_df,
                              refugiaTarget = 1 # 100% protection to the most climate-smart areas
)

plot <- plot_AQMFeatures(aqua_CPA %>% 
                           dplyr::rename(solution = Stenella_coeruleoalba_CS), PUs, land)
ggsave(filename = "Workflow-ClimatePriorityArea-phos-CS.png",
       plot = plot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save

plot <- plot_AQMFeatures(aqua_CPA %>% 
                           dplyr::rename(solution = Stenella_coeruleoalba_NCS), PUs, land)
ggsave(filename = "Workflow-ClimatePriorityArea-phos-NCS.png",
       plot = plot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/") # save
