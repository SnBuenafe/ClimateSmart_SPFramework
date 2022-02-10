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
