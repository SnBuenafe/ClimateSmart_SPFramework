#### Saving solutions ####
# Saving solutions
output_solutions <- "Output/solutions/"
# list of solutions to be saved
solution_list <- list(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13)
fileLabel_list <- c("s1-uninformed.rds", "s2-percentile-tos-585.rds", "s3-percentile-phos-585.rds", "s4-percentile-o2os-585.rds", "s5-percentile-velocity-585.rds", "s6-feature-tos-585.rds", "s7-feature-phos-585.rds", "s8-feature-o2os-585.rds", "s9-feature-velocity-585.rds", "s10-penalty-tos-585.rds", "s11-penalty-phos-585.rds", "s12-penalty-o2os-585.rds", "s13-penalty-velocity-585.rds")

for(i in 1:length(solution_list)) {
  saveRDS(solution_list[[i]], paste0(output_solutions, fileLabel_list[i]))
}

# Saving feature representation
summary_directory <- "Output/summary/"
write.csv(feat_rep, paste0(summary_directory, "feature_representation.csv"))

# Saving summaries
write.csv(summary, paste0(summary_directory, "summary_statistics.csv"))
write.csv(LowRegret_SummaryAll, paste0(summary_directory, "LowRegret_summary_statistics.csv"))

# Saving more solutions
output_solutions
# list of solutions to be saved (Ensemble: rate of climate warming)
solution_list <- list(s14, s15, s16, s17, s18)
fileLabel_list <- c("s14-percentile-tos-585-CanESM5-ensemble.rds", "s15-percentile-tos-585-CMCC-ESM2-ensemble.rds", "s16-percentile-tos-585-GFDL-ESM4-ensemble.rds", "s17-percentile-tos-585-IPSL-CM6A-LR-ensemble.rds", "s18-percentile-tos-585-NorESM2-MM-ensemble.rds")

for(i in 1:length(solution_list)) {
  saveRDS(solution_list[[i]], paste0(output_solutions, fileLabel_list[i]))
}

# Ensemble: rate of ocean acidification
solution_list <- list(s19, s20, s21, s22, s23)
fileLabel_list <- c("s19-percentile-phos-585-CanESM5-ensemble.rds", "s20-percentile-phos-585-CMCC-ESM2-ensemble.rds", "s21-percentile-phos-585-GFDL-ESM4-ensemble.rds", "s22-percentile-phos-585-IPSL-CM6A-LR-ensemble.rds", "s23-percentile-phos-585-NorESM2-MM-ensemble.rds")

for(i in 1:length(solution_list)) {
  saveRDS(solution_list[[i]], paste0(output_solutions, fileLabel_list[i]))
}

# Ensemble: rate of declining oxygen concentration
solution_list <- list(s24, s25, s26, s27, s28)
fileLabel_list <- c("s24-percentile-o2os-585-CanESM5-ensemble.rds", "s25-percentile-o2os-585-CMCC-ESM2-ensemble.rds", "s26-percentile-o2os-585-GFDL-ESM4-ensemble.rds", "s27-percentile-o2os-585-IPSL-CM6A-LR-ensemble.rds", "s28-percentile-o2os-585-NorESM2-MM-ensemble.rds")

for(i in 1:length(solution_list)) {
  saveRDS(solution_list[[i]], paste0(output_solutions, fileLabel_list[i]))
}

# Ensemble: velocity
solution_list <- list(s29, s30, s31, s32, s33)
fileLabel_list <- c("s29-percentile-velocity-585-CanESM5-ensemble.rds", "s30-percentile-velocity-585-CMCC-ESM2-ensemble.rds", "s31-percentile-velocity-585-GFDL-ESM4-ensemble.rds", "s32-percentile-velocity-585-IPSL-CM6A-LR-ensemble.rds", "s33-percentile-velocity-585-NorESM2-MM-ensemble.rds")

for(i in 1:length(solution_list)) {
  saveRDS(solution_list[[i]], paste0(output_solutions, fileLabel_list[i]))
}

# Important feature approach
solution_list <- list(s34, s35, s36, s37)
fileLabel_list <- c("s34-imptfeature-tos-585.rds", "s35-imptfeature-phos-585.rds", "s36-imptfeature-o2os-585.rds", "s37-imptfeature-velocity-585.rds")

for(i in 1:length(solution_list)) { 
  saveRDS(solution_list[[i]], paste0(output_solutions, fileLabel_list[i]))
}

# Different scenarios
solution_list <- list(s38, s39)
fileLabel_list <- c("s38-percentile-tos-126.rds", "s39-percentile-tos-245.rds")

for(i in 1:length(solution_list)) { 
  saveRDS(solution_list[[i]], paste0(output_solutions, fileLabel_list[i]))
}

#### Multi Model Ensemble Selection Frequency (Percentile) ####
ggsave(filename = 'MultiModelEnsemble_Percentile_tos_585.png',
       plot = gg_Selection_tosEnsemble_Frequency, width = 21, height = 29.7, dpi = 300,
       path = 'Figures/')
ggsave(filename = 'MultiModelEnsemble_Percentile_phos_585.png',
       plot = gg_Selection_phosEnsemble_Frequency, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = 'MultiModelEnsemble_Percentile_o2os_585.png',
       plot = gg_Selection_o2osEnsemble_Frequency, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = 'MultiModelEnsemble_Percentile_velocity_585.png',
       plot = gg_Selection_velocityEnsemble_Frequency, width = 21, height = 29.7, dpi = 300,
       path = 'Figures/')
#### Percentile approach ####
ggsave(filename = "EnsembleMean_Percentile_tos_585.png",
       plot = ggSol2, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "EnsembleMean_Percentile_phos_585.png",
       plot = ggSol3, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "EnsembleMean_Percentile_o2os_585.png",
       plot = ggSol4, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "EnsembleMean_Percentile_velocity_585.png",
       plot = ggSol5, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "PercentileArea_585.png",
       plot = ggSummary_Area, width = 7, height = 5, dpi = 300,
       path = "Figures/")
png("Figures/PercentileKappa_585.png", width = 8, height = 8)
ggsave(filename = "LowRegret_Percentile_585.png",
       plot = gg_LowRegretPercentile, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
# Save Low-regret areas and summaries for multiple cliamte scenarios
ggsave(filename = "LowRegret_ClimateScenario.png",
       plot = gg_LowRegretClimateScenario, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "LowRegret_ClimateScenario_Area.png",
       plot = ggSummary_Area, width = 7, height = 5, dpi = 300,
       path = "Figures/")

#### Feature approach ####
ggsave(filename = "EnsembleMean_Feature_tos_585.png",
       plot = ggSol6, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "EnsembleMean_Feature_phos_585.png",
       plot = ggSol7, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "EnsembleMean_Feature_o2os_585.png",
       plot = ggSol8, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "EnsembleMean_Feature_velocity_585.png",
       plot = ggSol9, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "LowRegret_Feature_585.png",
       plot = gg_LowRegretFeature, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "FeatureArea_585.png",
       plot = ggSummary_Area, width = 7, height = 5, dpi = 300,
       path = "Figures/")
#### Penalty approach ####
ggsave(filename = "EnsembleMean_Penalty_tos_585.png",
       plot = ggSol10, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "EnsembleMean_Penalty_phos_585.png",
       plot = ggSol11, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "EnsembleMean_Penalty_o2os_585.png",
       plot = ggSol12, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "EnsembleMean_Penalty_velocity_585.png",
       plot = ggSol13, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "LowRegret_Penalty_585.png",
       plot = gg_LowRegretPenalty, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "PenaltyArea_585.png",
       plot = ggSummary_Area, width = 7, height = 5, dpi = 300,
       path = "Figures/")
#### Important feature approach ####
ggsave(filename = "EnsembleMean_ImptFeature_tos_585.png",
      plot = ggSol34, width = 21, height = 29.7, dpi = 300,
      path = "Figures/")
ggsave(filename = "EnsembleMean_ImptFeature_phos_585.png",
       plot = ggSol35, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "EnsembleMean_ImptFeature_o2os_585.png",
       plot = ggSol36, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "EnsembleMean_ImptFeature_velocity_585.png",
       plot = ggSol37, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "LowRegret_ImptFeature_585.png",
       plot = gg_LowRegretImptFeature, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "ImportantFeatureArea_585.png",
       plot = ggSummary_Area, width = 7, height = 5, dpi = 300,
       path = "Figures/")
#### Low-regret areas ####
ggsave(filename = "LowRegretArea_585.png",
       plot = ggComparison_Area_LowRegret, width = 7, height = 5, dpi = 300,
       path = "Figures/")
#### Different climate scenarios ####
ggsave(filename = "EnsembleMean_Percentile_tos_126.png",
       plot = ggSol38, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "EnsembleMean_Percentile_tos_245.png",
       plot = ggSol39, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
#### Percentile workflow figures ####
ggsave(filename = "01a_Percentile_AquaMapsSubset.png",
       plot = aqm1_Plot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "01b_Percentile_AquaMapsSubset.png",
       plot = aqm1_PercentilePlot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "02a_Percentile_AquaMapsSubset.png",
       plot = aqm2_Plot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "02b_Percentile_AquaMapsSubset.png",
       plot = aqm2_PercentilePlot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Feature workflow figures ####
ggsave(filename = "01_Feature_Climate.png",
       plot = gg_roc_tos_SSP585, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "02_Feature_Climate.png",
       plot = featsub, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Important Feature figures ####
ggsave(filename = "01a_ImportantFeature_5thPercentile.png",
       plot = aqm1_5thPercentile, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "01b_ImportantFeature_95thPercentile.png",
       plot = aqm1_95thPercentile, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "02a_ImportantFeature_5thPercentile.png",
       plot = aqm2_5thPercentile, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "02b_ImportantFeature_95thPercentile.png",
       plot = aqm2_95thPercentile, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

#### Prioritizr layers ####
ggsave("Layer_PlanningRegion.png",
       plot = ggPU, width = 7, height = 5, dpi = 300,
       path = "Figures/")
ggsave("Layer_AquaMaps.png",
       plot = ggFeatureNo, width = 7, height = 5, dpi = 300,
       path = "Figures/")
ggsave("Layer_RateOfClimateWarming_SSP126.png",
       plot = gg_roc_tos_SSP126, width = 7, height = 5, dpi = 300,
       path = "Figures/")
ggsave("Layer_RateOfClimateWarming_SSP245.png",
       plot = gg_roc_tos_SSP245, width = 7, height = 5, dpi = 300,
       path = "Figures/")
ggsave("Layer_RateOfClimateWarming_SSP585.png",
       plot = gg_roc_tos_SSP585, width = 7, height = 5, dpi = 300,
       path = "Figures/")
ggsave("Layer_RateOfOceanAcidification_SSP126.png",
       plot = gg_roc_phos_SSP126, width = 7, height = 5, dpi = 300,
       path = "Figures/")
ggsave("Layer_RateOfOceanAcidification_SSP245.png",
       plot = gg_roc_phos_SSP245, width = 7, height = 5, dpi = 300,
       path = "Figures/")
ggsave("Layer_RateOfOceanAcidification_SSP585.png",
       plot = gg_roc_phos_SSP585, width = 7, height = 5, dpi = 300,
       path = "Figures/")
ggsave("Layer_RateOfDecliningOxygen_SSP126.png",
       plot = gg_roc_o2os_SSP126, width = 7, height = 5, dpi = 300,
       path = "Figures/")
ggsave("Layer_RateOfDecliningOxygen_SSP245.png",
       plot = gg_roc_o2os_SSP245, width = 7, height = 5, dpi = 300,
       path = "Figures/")
ggsave("Layer_RateOfDecliningOxygen_SSP585.png",
       plot = gg_roc_o2os_SSP585, width = 7, height = 5, dpi = 300,
       path = "Figures/")
ggsave("Layer_ClimateVelocity_SSP126.png",
       plot = gg_velocity_SSP126, width = 7, height = 5, dpi = 300,
       path = "Figures/")
ggsave("Layer_ClimateVelocity_SSP245.png",
       plot = gg_velocity_SSP245, width = 7, height = 5, dpi = 300,
       path = "Figures/")
ggsave("Layer_ClimateVelocity_SSP585.png",
       plot = gg_velocity_SSP585, width = 7, height = 5, dpi = 300,
       path = "Figures/")
