# Saving files.

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

## Saving plots
# Multi Model Ensemble Selection Frequency
ggsave(filename = 'MultiModelEnsemble_Frequency.png',
       plot = gg_Selection_Ensemble_Frequency, width = 21, height = 29.7, dpi = 300,
       path = 'Figures/')
# Climate warming using ensemble mean (percentile)
ggsave(filename = "EnsembleMean_Solution.png",
       plot = ggSol2, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
# Ocean acidification using ensemble mean (percentile)
ggsave(filename = "Percentile_ph_585.png",
       plot = ggSol3, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
# Declining oxygen rate (percentile)
ggsave(filename = "Percentile_o2_585.png",
       plot = ggSol4, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
# Velocity (percentile)
ggsave(filename = "Percentile_velocity_585.png",
       plot = ggSol5, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
# Cost of Percentile approaches
ggsave(filename = "PercentileCost_585.png",
       plot = ggSummary_Cost, width = 7, height = 5, dpi = 300,
       path = "Figures/")
# Area of Percentile approaches
ggsave(filename = "PercentileArea_585.png",
       plot = ggSummary_Area, width = 7, height = 5, dpi = 300,
       path = "Figures/")
# Kappa
png("Figures/PercentileKappa_585.png", width = 8, height = 8)


# Low-regret areas
ggsave(filename = "PercentileLowRegret_585.png",
       plot = gg_LowRegretPercentile, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "FeatureLowRegret_585.png",
       plot = gg_LowRegretFeature, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "PenaltyLowRegret_585.png",
       plot = gg_LowRegretPenalty, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "LowRegretCost_585.png",
       plot = ggComparison_Cost_LowRegret, width = 7, height = 5, dpi = 300,
       path = "Figures/")
ggsave(filename = "LowRegretArea_585.png",
       plot = ggComparison_Area_LowRegret, width = 7, height = 5, dpi = 300,
       path = "Figures/")
ggsave(filename = "LowRegretAll_585.png",
       plot = ggLowRegret_All, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

# Percentile workflow figures
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
ggsave(filename = "03a_Percentile_AquaMapsSubset.png",
       plot = aqm3_Plot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "03b_Percentile_AquaMapsSubset.png",
       plot = aqm3_PercentilePlot, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")

# Feature workflow figures
ggsave(filename = "01_Feature_Climate.png",
       plot = gg_roc_tos_SSP585, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
ggsave(filename = "02_Feature_Climate.png",
       plot = featsub, width = 21, height = 29.7, dpi = 300,
       path = "Figures/")
