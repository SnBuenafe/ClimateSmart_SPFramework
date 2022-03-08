# Loads all layers that have been created in `SpatPlan_Master_WestPac.R`

save_name <- "WestPacific"
PU_size = 669.9 # km2 (0.25 deg at equator)
Shape <- "Hexagon" # "Shape of PUs

#### Planning region ####

PUs <- read_rds(file.path("Output", paste(save_name, "PU", paste0(PU_size,"km2"), "Output.rds", sep = "_")))
land <- ne_countries(scale = 'large', returnclass = 'sf') %>% 
  fSpatPlan_Convert2PacificRobinson() # Land masses; needed for plotting

### Climate Metrics ####
# 1. Rates of Climate Warming
ClimateLayer_path <- "Data/Climate/ClimateMetrics/RateOfChange/tos/"
ClimateLayer_files <- list.files(ClimateLayer_path)

roc_tos_SSP126 <- readRDS(file.path("Output", 
                                    paste(save_name, "PU", paste0(PU_size, "km2"),
                                          ClimateLayer_files[1], sep = "_")))
roc_tos_SSP245 <- readRDS(file.path("Output", 
                                    paste(save_name, "PU", paste0(PU_size, "km2"),
                                          ClimateLayer_files[2], sep = "_")))
roc_tos_SSP585 <- readRDS(file.path("Output", 
                                    paste(save_name, "PU", paste0(PU_size, "km2"),
                                          ClimateLayer_files[3], sep = "_")))
ClimateLayer_path <- "Data/Climate/ClimateMetrics_Ensemble/tos/SSP 5-8.5/"
ClimateLayer_files <- list.files(ClimateLayer_path)

tos_CanESM5 <- readRDS(file.path("Output",
                                 paste(save_name, "PU", paste0(PU_size, "km2"),
                                      ClimateLayer_files[1], sep = "_")))
`tos_CMCC-ESM2` <- readRDS(file.path("Output",
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[2], sep = "_")))
`tos_GFDL-ESM4` <- readRDS(file.path("Output",
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[3], sep = "_")))
`tos_IPSL-CM6A-LR` <- readRDS(file.path("Output",
                                        paste(save_name, "PU", paste0(PU_size, "km2"),
                                              ClimateLayer_files[4], sep = "_")))
`tos_NorESM2-MM` <- readRDS(file.path("Output",
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[5], sep = "_")))

# 2. Rates of Ocean Acidification
ClimateLayer_path <- "Data/Climate/ClimateMetrics/RateOfChange/phos/"
ClimateLayer_files <- list.files(ClimateLayer_path)

roc_phos_SSP126 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[1], sep = "_")))
roc_phos_SSP245 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[2], sep = "_")))
roc_phos_SSP585 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[3], sep = "_")))
ClimateLayer_path <- "Data/Climate/ClimateMetrics_Ensemble/phos/SSP 5-8.5/"
ClimateLayer_files <- list.files(ClimateLayer_path)

phos_CanESM5 <- readRDS(file.path("Output",
                                 paste(save_name, "PU", paste0(PU_size, "km2"),
                                      ClimateLayer_files[1], sep = "_")))
`phos_CMCC-ESM2` <- readRDS(file.path("Output",
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[2], sep = "_")))
`phos_GFDL-ESM4` <- readRDS(file.path("Output",
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[3], sep = "_")))
`phos_IPSL-CM6A-LR` <- readRDS(file.path("Output",
                                        paste(save_name, "PU", paste0(PU_size, "km2"),
                                              ClimateLayer_files[4], sep = "_")))
`phos_NorESM2-MM` <- readRDS(file.path("Output",
                                      paste(save_name, "PU", paste0(PU_size, "km2"),
                                            ClimateLayer_files[5], sep = "_")))
# 3. Rates of Declining Oxygen Concentration
ClimateLayer_path <- "Data/Climate/ClimateMetrics/RateOfChange/o2os/"
ClimateLayer_files <- list.files(ClimateLayer_path)

roc_o2os_SSP126 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[1], sep = "_")))
roc_o2os_SSP245 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[2], sep = "_")))
roc_o2os_SSP585 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[3], sep = "_")))
# 4. Climate Velocity
ClimateLayer_path <- "Data/Climate/ClimateMetrics/ClimateVelocity/"
ClimateLayer_files <- list.files(ClimateLayer_path)

velocity_SSP126 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[1], sep = "_")))
velocity_SSP245 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[2], sep = "_")))
velocity_SSP585 <- readRDS(file.path("Output", 
                                     paste(save_name, "PU", paste0(PU_size, "km2"),
                                           ClimateLayer_files[3], sep = "_")))
# 5. Annual marine heatwave intensity

#### Conservation Features ####
aqua_sf <- read_rds(file.path("Output", 
                              paste(save_name, "PU", paste0(PU_size,"km2"), 
                                    "AquaMaps_Output.rds", sep = "_")))
# Changing to 1s and 0s
CutOff = 0.5
subset_aqua_sf <- aqua_sf %>% 
  as_tibble() %>% 
  dplyr::select(Doryrhamphus_excisus.excisus, Padina_sanctae.crucis, Platybelone_argalus.platyura,
                Tylosurus_acus.acus, Tylosurus_acus.melanotus)
aqua_sf <- aqua_sf %>% 
  mutate_at(vars(colnames(subset_aqua_sf)), 
            funs(case_when(. >= CutOff ~ 1,
                           . <= CutOff ~ 0,
                           is.na(.) ~ 0)))

#### Cost layer ####
# Cost Layer, Squished
cost <- read_rds(file.path("Output", 
                           paste(save_name, "PU", paste0(PU_size,"km2"), 
                                 "CostLayer_Output.rds", sep = "_"))) %>% 
  mutate(Cost_squish = scales::oob_squish(Cost, quantile(Cost, c(0.01, 0.99))))

# Uniform Cost (Using the Area)
UniformCost <- PUs %>% 
  dplyr::mutate(cost = PU_size)
