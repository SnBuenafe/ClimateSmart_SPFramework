tos_SSP126 <- load_metrics(metric = "tos", model = "ensemble", scenario = "SSP 1-2.6")
tos_SSP245 <- load_metrics(metric = "tos", model = "ensemble", scenario = "SSP 2-4.5")
tos_SSP585 <- load_metrics(metric = "tos", model = "ensemble", scenario = "SSP 5-8.5")

tos <- list(tos_SSP126$transformed, tos_SSP245$transformed, tos_SSP585$transformed)
boxplot(tos)
abline(h = 0.0066, col = "darkgreen")
abline(h = 0.017, col = "orange")
abline(h = 0.0373, col = "red")
quantile(tos_SSP245$transformed, 0.35)

png(filename = "Figures/SSPs.png", width = 1200, height = 500, units = "px")
par(mfrow = c(1,3))
hist(tos_SSP126$transformed, breaks = 400, ylim=c(0,1200))
abline(v = 0.0066, col = "darkgreen")
hist(tos_SSP245$transformed, breaks = 400, ylim=c(0,1200))
abline(v = 0.017, col = "orange")
hist(tos_SSP585$transformed, breaks = 400, ylim=c(0,1200))
abline(v = 0.0373, col = "red")

dev.off()
length(!is.na(tos_SSP245$transformed))

# Numbers for the ensemble theme
ens <- readRDS("Output/lowregret/sFreq2-EM-Percentile-tos.rds")
nrow(ens %>% dplyr::filter(selection == 5))/35389 #24.2% are common to all

dummy <- call_dummy() 
sol <- ens %>% 
  dplyr::mutate(solution_1 = ifelse(selection == 5, yes = 1, no = 0))
df <- fFeatureRepresent(dummy, sol, "selection_5")
nrow(df %>% dplyr::filter(selection_5 < 30))/8711 #20.3% unmet targets

# Numbers for the scenario theme
scen <- readRDS("Output/lowregret/sFreq1-EM-Percentile-tos.rds")
nrow(scen %>% dplyr::filter(selection == 3))/35389 #34.2% are common to all

sol <- scen %>% 
  dplyr::mutate(solution_1 = ifelse(selection == 3, yes = 1, no = 0))
df <- fFeatureRepresent(dummy, sol, "selection_3")
nrow(df %>% dplyr::filter(selection_3 < 30))/8711 #2.9% unmet targets

# Numbers for the metric theme
met <- readRDS("Output/lowregret/sFreq3-EM-Percentile-585.rds")
nrow(met %>% dplyr::filter(selection == 6))/35389 #9.8% are common to all

sol <- met %>% 
  dplyr::mutate(solution_1 = ifelse(selection == 6, yes = 1, no = 0))
df <- fFeatureRepresent(dummy, sol, "selection_6")
nrow(df %>% dplyr::filter(selection_6 < 30))/8711 #48.4% unmet targets

# Numbers for the approach theme
app <- readRDS("Output/lowregret/sFreq4-EM-tos-585.rds")
nrow(app %>% dplyr::filter(selection == 4))/35389 #9.1% are common to all

sol <- app %>% 
  dplyr::mutate(solution_1 = ifelse(selection == 4, yes = 1, no = 0))
df <- fFeatureRepresent(dummy, sol, "selection_4")
nrow(df %>% dplyr::filter(selection_4 < 30))/8711 #98.7% unmet targets

# Assessing approaches
# 1. Climate warming
tos_SSP585 <- load_metrics(metric = "tos", model = "ensemble", scenario = "SSP 5-8.5") # load metric
# Load solutions
s2 <- readRDS("Output/solutions/s2-EM-Percentile-tos-585.rds")
s6 <- readRDS("Output/solutions/s6-EM-Feature-tos-585.rds")
s10 <- readRDS("Output/solutions/s10-EM-Penalty-tos-585.rds")
s34 <- readRDS("Output/solutions/s34-EM-ClimatePriorityArea-tos-585.rds")

thres <- load_summary("tos", "mean_tos")
mean_tos_585 <- 0.03906677
sel <- s2 %>% # planning units selected
  dplyr::filter(solution_1 == 1)
nsel <- s2 %>% # planning units that are not selected
  dplyr::filter(solution_1 == 0)

nrow(sel %>% dplyr::filter(transformed <= mean_tos_585))/nrow(sel) # of planning units that are selected that are less than the mean tos?

sel <- s6 %>% # planning units selected
  dplyr::filter(solution_1 == 1)
nsel <- s6 %>% # planning units that are not selected
  dplyr::filter(solution_1 == 0)
nrow(sel %>% dplyr::filter(transformed <= mean_tos_585))/nrow(sel) # of planning units that are selected that are less than the mean tos?

sel <- s10 %>% # planning units selected
  dplyr::filter(solution_1 == 1)
nsel <- s10 %>% # planning units that are not selected
  dplyr::filter(solution_1 == 0)
nrow(sel %>% dplyr::filter(transformed <= mean_tos_585))/nrow(sel) # of planning units that are selected that are less than the mean tos?

sel <- s34 %>% # planning units selected
  dplyr::filter(solution_1 == 1)
nsel <- s34 %>% # planning units that are not selected
  dplyr::filter(solution_1 == 0)
nrow(sel %>% dplyr::filter(transformed <= mean_tos_585))/nrow(sel) # of planning units that are selected that are less than the mean tos?

# 2. Ocean acidification
phos_SSP585 <- load_metrics(metric = "phos", model = "ensemble", scenario = "SSP 5-8.5") # load metric
# Load solutions
s3 <- readRDS("Output/solutions/s3-EM-Percentile-phos-585.rds")
s7 <- readRDS("Output/solutions/s7-EM-Feature-phos-585.rds")