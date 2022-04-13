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
  
  ### Climate Metrics ####
  # Call function for each metric
  LoadClimateMetrics <- function(metric, 
                           model = NA, # if model = NA, approach is ensemble mean
                           scenario
  ) {
    
    if (scenario == "SSP 1-2.6") {
      scenario_obj = "SSP126"
    } else if (scenario == "SSP 2-4.5") {
        scenario_obj = "SSP245"
    } else if (scenario == "SSP 5-8.5") {
          scenario_obj = "SSP585"
    }
    
    # ----- Set path -----
    if(is.na(model)) { # Ensemble mean approach
      if (metric == "velocity") {
        path = "Data/Climate/ClimateMetrics/ClimateVelocity"
      } else if (str_detect(metric, pattern = "MHW")) {
        path = "Data/Climate/ClimateMetrics/MHW"
      } else {
        path = file.path("Data/Climate/ClimateMetrics/RateOfChange", metric)
      }
    } else { # Multi-model ensemble approach
      if (str_detect(metric, pattern = "MHW")) {
        path = file.path("Data/Climate/ClimateMetrics_Ensemble/MHW", scenario)
      } else{
        path = file.path("Data/Climate/ClimateMetrics_Ensemble", metric, scenario)
      }

    }
    
    # ----- Read RDS file -----
    if(is.na(model)) {
      output <- paste0("Output/", save_name, "_ClimateLayer")
      
      if (metric == "velocity") {
        metric_tmp = "velocity_tos"
        
        df <- readRDS(paste(output, metric_tmp, paste0(scenario, ".rds"), sep = "_"))
        assign(x = paste(metric, scenario_obj, sep = "_"), value = df, envir=.GlobalEnv)
      } else if (metric %in% c("tos", "phos", "o2os")) {
        metric_tmp = paste0("roc_", metric)
        
        df <- readRDS(paste(output, metric_tmp, paste0(scenario, ".rds"), sep = "_"))
        assign(x = paste(metric_tmp, scenario_obj, sep = "_"), value = df, envir=.GlobalEnv)
      } else if (str_detect(metric, pattern = "MHW")) {
        df <- readRDS(paste(output, metric, paste0(scenario, ".rds"), sep = "_"))
        assign(x = paste(metric, scenario_obj, sep = "_"), value = df, envir=.GlobalEnv)
      }
      
    } else { # Multi-model ensemble approach
      output <- paste0("Output/", save_name, "_ClimateLayer")
      
      if (str_detect(metric, pattern = "velocity")) {
        df <- readRDS(paste(output, metric, model, scenario, "ensemble.rds", sep = "_"))
        assign(x = paste(metric, model, scenario_obj, sep = "_"), value = df, envir = .GlobalEnv)
      } else if (str_detect(metric, pattern = "MHW")) {
        df <- readRDS(paste(output, metric, model, paste0(scenario, ".rds"), sep = "_"))
        assign(x = paste(metric, model, scenario_obj, sep = "_"), value = df, envir = .GlobalEnv)
      } else {
        metric_tmp = paste0("roc_", metric)
        
        df <- readRDS(paste(output, metric_tmp, model, scenario, "ensemble.rds", sep = "_"))
        assign(x = paste(metric, model, scenario_obj, sep = "_"), value = df, envir = .GlobalEnv)
      }
      
}

}
