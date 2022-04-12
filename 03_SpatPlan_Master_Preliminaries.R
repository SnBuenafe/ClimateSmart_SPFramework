# title: "Loading all layers"
# author: "Tin Buenafe and Jason Everett"

# This code loads all layers limited to the planning region created in `02_SpatPlan_Master_WestPac.R`

save_name <- "WestPacific"
PU_size = 669.9 # km2 (0.25 deg at equator)
Shape <- "Hexagon" # "Shape of PUs

#### Planning region ####

PUs <- read_rds(file.path("Output", paste(save_name, paste0("PlanningRegion.rds"), sep = "_")))
land <- ne_countries(scale = 'large', returnclass = 'sf') %>% 
  fSpatPlan_Convert2PacificRobinson() # Land masses; needed for plotting

### Climate Metrics ####

#metric_list <- c("tos", "phos", "o2os", "velocity")
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

# Call function for each metric
fcallMetrics <- function(metric, 
                         model = NA, # if model = NA, approach is ensemble mean
                         path # path with / at the end
                         ) {
  
  scenario_obj <- c("SSP126", "SSP245", "SSP585")
  scenario_path <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
  
  if(is.na(model)) {
    if (metric == "velocity") {
      files <- list.files(file.path(path))
    } else {
      files <- list.files(file.path(path, metric))
    }
    
    for(i in 1:length(files)) {
      df <- readRDS(file.path("Output",
                              paste(save_name, "ClimateLayer", files[i], sep = "_")))
      
      if (metric == "velocity") {
        assign(x = paste(metric, scenario_obj[i], sep = "_"), value = df, envir=.GlobalEnv)
      } else {
        assign(x = paste("roc", metric, scenario_obj[i], sep = "_"), value = df, envir=.GlobalEnv)
      }
      
    }
  }
  else{
    for (i in 1:length(scenario_path))
    {
      files <- list.files(file.path(path, metric, scenario_path[i]))
      for(j in 1:length(files)) {
        df <- readRDS(file.path("Output",
                                paste(save_name, "ClimateLayer", files[j], sep = "_")))
        assign(x = paste(metric, model[j], scenario_obj[i], sep = "_"), value = df, envir=.GlobalEnv)
      }
    }
  }

}

# 1. Rates of Climate warming
fcallMetrics(metric = "tos", path = "Data/Climate/ClimateMetrics/RateOfChange") # ensemble mean
fcallMetrics(metric = "tos", path = "Data/Climate/ClimateMetrics_Ensemble", model = model_list) # multimodel approach

# 2. Rates of Ocean Acidification
fcallMetrics(metric = "phos", path = "Data/Climate/ClimateMetrics/RateOfChange") # ensemble mean
fcallMetrics(metric = "phos", path = "Data/Climate/ClimateMetrics_Ensemble", model = model_list) # multimodel approach

# 3. Rates of Declining Oxygen Concentration
fcallMetrics(metric = "o2os", path = "Data/Climate/ClimateMetrics/RateOfChange") # ensemble mean
fcallMetrics(metric = "o2os", path = "Data/Climate/ClimateMetrics_Ensemble", model = model_list) # multimodel approach
               
# 4. Climate Velocity
fcallMetrics(metric = "velocity", path = "Data/Climate/ClimateMetrics/ClimateVelocity") # ensemble mean
fcallMetrics(metric = "velocity", path = "Data/Climate/ClimateMetrics_Ensemble", model = model_list) # multimodel

# 5. Annual marine heatwave intensity

#### Conservation Features ####
aqua_sf <- read_rds(file.path("Output", paste(save_name, paste0("AquaMaps.rds"), sep = "_")))
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
# cost <- read_rds(file.path("Output", paste(save_name, paste0("Cost.rds"), sep = "_"))) %>% 
#  mutate(Cost_squish = scales::oob_squish(Cost, quantile(Cost, c(0.01, 0.99))))

# Uniform Cost (Using the Area)
UniformCost <- PUs %>% 
  dplyr::mutate(cost = PU_size)
