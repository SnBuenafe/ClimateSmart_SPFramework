# title: "Creating nMDS plot for each of the themes"
# author: "Tin Buenafe and Sandra Neubert"

#### Preliminaries ####
# Description
# This script creates nMDS ordination plots for all 360 spatial plans, for each of the 4 climate-smart aspects explored in the paper:
# 1. Emission scenarios (SSP1-2.6, SSP2-4.5, and SSP5-8.5)
# 2. Models (Ensemble mean and all 5 models in the ensemble)
# 3. Metrics (Warming, ocean acidification, ocean deoxygenation, climate velocity, MHW intensity, and the combined climate-smart metric)
# 4. Approaches (feature, percentile, climate-priority-area, and penalty)

# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R")

#### PREPROCESSING ####
solution <- paste0("s", seq(from = 2, to = 433, by = 1)) # solution names; make sure that this checks out with metadata

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

# ----- nMDS ORDINATION -----
reps <- 1000 
stressTest <- vegan::oecosimu(comm = round(matrix), method = "quasiswap_count",
                              nestfun = metaMDS, autotransform = FALSE, k = 2,
                              distance = "jaccard", nsimul = reps, parallel = 2, statistic = "stress",
                              alternative = "less", trace = TRUE, maxit = 1000,
                              trymax = 200, sratmax = 0.9999999)

# Create nMDS 
solution.mds <- metaMDS(matrix, distance = "jaccard", autotransform = FALSE, try = 1000)
# using Jaccard dissimilarity matrix because data is "presence/absence"

# Load groupings data
df_groups <- read.csv("Output/nmds/df_groups1.csv") %>% as_tibble()

# ----- Plot ordinations -----
plotOrdination <- function(x, palette) {
  Ordi_Obj <- ggordiplots::gg_ordiplot(solution.mds, groups = df_groups[[ x ]], ellipse = TRUE, kind = "sd")
  p <- Ordi_Obj$plot +
    scale_color_manual(values = palette) +
    theme_bw()
  return(p)
}

# Scenario Theme
palette <- c("SSP1-2.6" = "#289E3D", "SSP2-4.5" = "#E6C173", "SSP5-8.5" = "#855600")
plot <- plotOrdination("scenario", palette)
ggsave("Figures/nMDS-ScenarioTheme.png", plot = plot, height = 10, width = 10, dpi = 600)

# Ensemble Theme
palette <- c("ensemble mean" = "#F59145", "CanESM5" = "#E6C173", "CMCC-ESM2" = "#855600", "GFDL-ESM4" = "#5075BA", "IPSL-CM6A-LR" = "#81B0CC", "NorESM2-MM" = "#5A9E67")
plot <- plotOrdination("model", palette)
ggsave("Figures/nMDS-EnsembleTheme.png", plot = plot, height = 10, width = 10, dpi = 600)

# Metrics theme
palette <- c("MHW" = "#3C6342", "o2os" = "#289E3D", "phos" = "#E6C173", "tos" = "#81B0CC", "velocity" = "#855600", "combined" = "#BFA1BD")
plot <- plotOrdination("metric", palette)
ggsave("Figures/nMDS-MetricTheme.png", plot = plot, height = 10, width = 10, dpi = 600)

# Approach Theme
palette <- c("climate priority area" = "#E6BA7E", "feature" = "#4D3B2A", "penalty" = "#6984BF", "percentile" = "#2B8142")
plot <- plotOrdination("approach", palette)
ggsave("Figures/nMDS-ApproachTheme.png", plot = plot, height = 10, width = 10, dpi = 600)


# ----- Plot ordination with different colors (metrics) and shapes (approach) -----
long <- scores(solution.mds, display = "sites") %>% bind_cols(., df_groups)
palette <- c("climate priority area" = "#E6BA7E", "feature" = "#4D3B2A", "penalty" = "#6984BF", "percentile" = "#2B8142")

long_plot <- ggplot(data = long, aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(color = approach, shape = metric), size = 3) +
  scale_color_manual(values = palette) +
  theme_bw()
ggsave("Figures/nMDS-Supplementary.png", plot = long_plot, height = 10, width = 10, dpi = 600)
