# title: "Creating nMDS plot for each of the themes"
# author: "Tin Buenafe and Sandra Neubert"

#### Preliminaries ####
# Description
# This script creates nMDS ordination plots for all 360 spatial plans, for each of the 4 climate-smart aspects explored in the paper:
# 1. Emission scenarios (SSP1-2.6, SSP2-4.5, and SSP5-8.5)
# 2. Models (Ensemble mean, and all 5 models in the ensemble)
# 3. Metrics (Warming, ocean acidification, ocean deoxygenation, climate velocity, MHW intensity)
# 4. Approaches (feature, percentile, climate-priority-area, penalty)

# Load functions
source("HelperFunctions/SpatPlan_Extras.R") # Load the extras, including functions and libraries
source("HelperFunctions/SpatPlan_HelperFxns_WestPac.R") # Load helper functions written specifically for this spatial planning project
source("HelperFunctions/SpatPlan_IterationFxns_WestPac.R") # Load loop functions written specifically for producing iterations
output_solutions <- "Output/solutions/"
output_summary <- "Output/summary/"
output_lowregret <- "Output/lowregret/"

# ----- Preprocessing -----

solution <- seq(from = 2, to = 361, by = 1) # solution names, check metadata
solution <-lapply(solution, function(x) {
  y <- paste0("s", x)
}) %>% unlist()

# Extracting selected/not-selected data for all solutions
pivot <- list() # empty list
for(i in (1:length(solution))) {
  path <- "Output/solutions/"
  pattern <- paste0(solution[i], "-")
  file <- list.files(path = path, pattern = paste0("^", pattern))
  
  s <- readRDS(paste0(path, file)) %>% as_tibble() %>% 
    dplyr::select(solution_1) %>% 
    dplyr::mutate(cellID = paste0("PU_", row_number())) %>% 
    dplyr::mutate(solution = solution[i])
  
  pivot[[i]] <- s %>% 
    pivot_wider(names_from = cellID, values_from = solution_1)
  
  print(solution[i])
}
pivot_combined <- do.call(bind_rows, pivot)
write.csv(pivot_combined, "Output/nmds/nmds_df.csv") # save pivot table

matrix <- data.matrix(pivot_combined[,-1], rownames.force = TRUE) # make df into a matrix
rownames(matrix) <- solution

# ----- Create nMDS ordination -----
# Create nMDS 
solution.mds <- metaMDS(matrix, distance = "jaccard", autotransform = FALSE, try = 1000)
# using Jaccard dissimilarity matrix because data is "presence/absence"

# Load groupings data
df_groups <- read.csv("Output/nmds/df_groups.csv") %>% as_tibble()

# ----- Plot ordinations -----
plotOrdination <- function(x, palette) {
  Ordi_Obj <- gg_ordiplot(solution.mds, groups = df_groups[[ x ]], ellipse = TRUE, kind = "sd")
  p <- Ordi_Obj$plot +
    scale_color_manual(values = palette) +
    theme_bw()
  return(p)
}

# Scenario Theme
palette <- c("SSP1-2.6" = "#289E3D", "SSP2-4.5" = "#E6C173", "SSP5-8.5" = "#855600")
(plot <- plotOrdination("scenario", palette))
ggsave("Figures/nMDS_scenario.png", plot = plot, height = 10, width = 10, dpi = 600)

# Ensemble Theme
palette <- c("EM" = "#FAF7B7", "CanESM5" = "#E6C173", "CMCC-ESM2" = "#855600", "GFDL-ESM4" = "#5075BA", "IPSL-CM6A-LR" = "#81B0CC", "NorESM2-MM" = "#5A9E67")
(plot <- plotOrdination("model", palette))
ggsave("Figures/nMDS_ensemble.png", plot = plot, height = 10, width = 10, dpi = 600)

# Metrics theme
palette <- c("MHW_SumCumInt" = "#3C6342", "o2os" = "#289E3D", "phos" = "#E6C173", "tos" = "#81B0CC", "velocity" = "#855600")
(plot <- plotOrdination("metric", palette))
ggsave("Figures/nmds_metric.png", plot = plot, height = 10, width = 10, dpi = 600)

# Approach Theme
palette <- c("climate priority area" = "#E6BA7E", "feature" = "#4D3B2A", "penalty" = "#6984BF", "percentile" = "#2B8142")
(plot <- plotOrdination("approach", palette))
ggsave("Figures/nmds_approach.png", plot = plot, height = 10, width = 10, dpi = 600)


# ----- Plot ordination with different colors (metrics) and shapes (approach) -----
long <- scores(solution.mds, display = "sites") %>% bind_cols(., df_groups)
palette <- c("climate priority area" = "#E6BA7E", "feature" = "#4D3B2A", "penalty" = "#6984BF", "percentile" = "#2B8142")

long_plot <- ggplot(data = long, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = approach, shape = metric), size = 3) +
  scale_color_manual(values = palette) +
  theme_bw()
ggsave("Figures/nmds_longPlot.png", plot = long_plot, height = 10, width = 10, dpi = 600)
