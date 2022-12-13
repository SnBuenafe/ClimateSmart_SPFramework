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
# source("03_SpatPlan_Master_Preliminaries.R")
library(tidyverse)
library(vegan)

#### PREPROCESSING ####
solution <- paste0("s", seq(from = 2, to = 433, by = 1)) # solution names; make sure that this checks out with metadata

# Extracting selected/not-selected data for all solutions
# pivot <- list() # empty list
# for(i in (1:length(solution))) {
#   path <- "Output/solutions/"
#   pattern <- paste0(solution[i], "-")
#   file <- list.files(path = path, pattern = paste0("^", pattern))
#   
#   s <- readRDS(paste0(path, file)) %>% as_tibble() %>% 
#     dplyr::select(solution_1) %>% 
#     dplyr::mutate(cellID = paste0("PU_", row_number())) %>% 
#     dplyr::mutate(solution = solution[i])
#   
#   pivot[[i]] <- s %>% 
#     pivot_wider(names_from = cellID, values_from = solution_1)
#   
#   print(solution[i])
# }
# pivot_combined <- do.call(bind_rows, pivot)
# write.csv(pivot_combined, "Output/nmds/nmds_df.csv") # save pivot table

pivot_combined <- read_csv("Output/nmds/nmds_df.csv") %>% # read file
  dplyr::select(-1) # remove first column if needed

matrix <- data.matrix(pivot_combined[,-1], rownames.force = TRUE) # make df into a matrix
rownames(matrix) <- solution

#### PERMUTATION-BASED ORDINATION ####
reps <- 1000
stressTest <- vegan::oecosimu(comm = matrix, method = "c0", # this method preserves species frequencies
                              nestfun = metaMDS, autotransform = FALSE, k = 2,
                              distance = "jaccard", nsimul = reps, statistic = "stress",
                              alternative = "less", trace = TRUE,
                              trymax = 100)
saveRDS(stressTest, "Output/nmds/stresstest1000.rds")
#stressTest <- readRDS("Output/nmds/stresstest1000.rds")

# TODO: Save output from this function?
simVector <- as.vector(stressTest$oecosimu$simulated) %>% 
  tibble::as_tibble() %>% 
  bind_cols(., row_n = seq(1:reps))
hist(stressTest$oecosimu$simulated, xlim = c(0.39, max(stressTest$oecosimu$simulated)+0.001))
ggplot(data = simVector) +
  geom_histogram(aes(x = value), color = "black", binwidth =  0.0005) +
  scale_x_continuous(c(0.27, 0.405)) +
  geom_vline(xintercept = stressTest$oecosimu$statistic %>% 
               as.numeric(), linetype = "dashed", color = "red")

### PLOT THE NMDS #### 
solution.mds <- vegan::metaMDS(matrix, distance = "jaccard", autotransform = FALSE, try = 500, trymax = 1000)
# using Jaccard dissimilarity matrix because data is "presence/absence"
saveRDS(solution.mds, "Output/nmds/nmds.rds") # save the nMDS
solution.mds <- readRDS("Output/nmds/nmds.rds")

# Load groupings data
df_groups <- read.csv("Output/nmds/df_groups1.csv") %>% 
  tibble::as_tibble()

# Plot scenario theme
plot <- fPlot_Ordination("scenario", c("SSP1-2.6" = "#289E3D", 
                                       "SSP2-4.5" = "#E6C173", 
                                       "SSP5-8.5" = "#855600"))
ggsave("Figures/nMDS-ScenarioTheme.png", plot = plot, height = 10, width = 10, dpi = 600)

# Plot ensemble theme
plot <- fPlot_Ordination("model", c("ensemble mean" = "#F59145", 
                                  "CanESM5" = "#E6C173", 
                                  "CMCC-ESM2" = "#855600", 
                                  "GFDL-ESM4" = "#5075BA", 
                                  "IPSL-CM6A-LR" = "#81B0CC", 
                                  "NorESM2-MM" = "#5A9E67"))
ggsave("Figures/nMDS-EnsembleTheme.png", plot = plot, height = 10, width = 10, dpi = 600)

# Plot metric theme
plot <- fPlot_Ordination("metric", c("MHW" = "#3C6342", 
                                     "o2os" = "#289E3D", 
                                     "phos" = "#E6C173", 
                                     "tos" = "#81B0CC", 
                                     "velocity" = "#855600", 
                                     "combined" = "#BFA1BD"))
ggsave("Figures/nMDS-MetricTheme.png", plot = plot, height = 10, width = 10, dpi = 600)

# Plot approach Theme
plot <- fPlot_Ordination("approach", c("climate priority area" = "#F59145",
                                     "feature" = "#4D3B2A", 
                                     "penalty" = "#6984BF", 
                                     "percentile" = "#2B8142"))
ggsave("Figures/nMDS-ApproachTheme.png", plot = plot, height = 10, width = 10, dpi = 600)

# Plot metric x approaches (SUpplementary)
long <- scores(solution.mds, display = "sites") %>% bind_cols(., df_groups)
plot <- fPlot_SuppOrdination(long, c("climate priority area" = "#F59145", 
                             "feature" = "#4D3B2A", 
                             "penalty" = "#6984BF", 
                             "percentile" = "#2B8142"))
ggsave("Figures/nMDS-Supplementary.png", plot = plot, height = 10, width = 10, dpi = 600)
