source("HelperFunctions/SpatPlan_Extras.R") # Load the extras, including functions and libraries
source("HelperFunctions/SpatPlan_HelperFxns_WestPac.R") # Load helper functions written specifically for this spatial planning project
output_solutions <- "Output/test/solutions/"
output_summary <- "Output/summary/"
output_lowregret <- "Output/lowregret/"


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

### Conservation Features ####
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

# Uniform Cost (Using the Area)
UniformCost <- PUs %>% 
  dplyr::mutate(cost = PU_size)


#clean environment
rm(subset_aqua_sf, land, CutOff)

#
theme_names <- c("climate_priority_area") #"feature", "penalty", "percentile"
scenario_names <- c("SSP126")#, "SSP245", "SSP585")
model_names <- c("CanESM5")#, "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
metric_names <- c("tos")#, "phos", "o2os", "velocity")
i <- 190
gc()

for (theme_num in 1:length(theme_names)){
  for (scenario_num in 1:length(scenario_names)){
    for (model_num in 1:length(model_names)){
      for (metric_num in 1:length(metric_names)){
        # 1. Rates of Climate warming
          fcallMetrics(metric = metric_names[metric_num],path = "Data/Climate/ClimateMetrics_Ensemble", model = model_names) # ensemble mean
          metric_dat <- paste(metric_names[metric_num], model_names[model_num], scenario_names[scenario_num], sep = "_") #string at the moment: could be problem (https://stackoverflow.com/questions/6034655/convert-string-to-a-variable-name)
          metric_dat <- eval(parse(text = metric_dat))
          ImptFeat <- create_ImportantFeatureLayer(aqua_sf, metric_name = metric_names[metric_num], colname = "transformed", 
                                                   metric_df =  metric_dat)
          gc()#clear up space
          RepFeat <- create_RepresentationFeature(ImptFeat, aqua_sf)
          #rm(metric_dat)
          
      }
    }
  }
}


metric_dat <- paste(metric_names[1], model_names[1], scenario_names[1], sep = "_") #string at the moment: could be problem (https://stackoverflow.com/questions/6034655/convert-string-to-a-variable-name)
ImptFeat <- create_ImportantFeatureLayer(aqua_sf, metric_name = metric_names[metric_num], colname = "transformed", 
                                         metric_df =  metric_dat)



for (theme_num in 1:length(theme_names)){
  for (scenario_num in 1:length(scenario_names)){
    for (model_num in 1:length(model_names)){
      for (metric_num in 1:length(metric_names)){
        # 1. Rates of Climate warming
        if (metric_num == 1){ #load temperature data
          fcallMetrics(metric = metric_names[metric_num],path = "Data/Climate/ClimateMetrics_Ensemble", model = model_names) # ensemble mean
          metric_dat <- paste(metric_names[metric_num], model_names[model_num], scenario_names[scenario_num], sep = "_") #string at the moment: could be problem (https://stackoverflow.com/questions/6034655/convert-string-to-a-variable-name)
          
        } else if (metric_num == 2){ #load pH data
          fcallMetrics(metric = metric_names[metric_num], path = "Data/Climate/ClimateMetrics/ClimateVelocity") # ensemble mean
          
        } else if (metric_num == 3){ #load oxygen data
          fcallMetrics(metric = metric_names[metric_num], path = "Data/Climate/ClimateMetrics/ClimateVelocity") # ensemble mean
          
        } else if (metric_num == 4){ #load velocity data
          fcallMetrics(metric = metric_names[metric_num], path = "Data/Climate/ClimateMetrics/ClimateVelocity") # ensemble mean
          
        }
        
      }
    }
  }
}


for (theme_num in 1:length(theme_names)){
  for (scenario_num in 1:length(scenario_names)){
    for (model_num in 1:length(model_names)){
      for (metric_num in 1:length(metric_names)){
        # 1. Rates of Climate warming
        if (metric_num == 1){ #load temperature data
          fcallMetrics(metric = metric_names[metric_num], path = "Data/Climate/ClimateMetrics/TempSlope", model = model_names) # ensemble mean
         
          } else if (metric_num == 2){ #load pH data
              fcallMetrics(metric = metric_names[metric_num], path = "Data/Climate/ClimateMetrics/ClimateVelocity") # ensemble mean
          
          } else if (metric_num == 3){ #load oxygen data
              fcallMetrics(metric = metric_names[metric_num], path = "Data/Climate/ClimateMetrics/ClimateVelocity") # ensemble mean
            
          } else if (metric_num == 4){ #load velocity data
             fcallMetrics(metric = metric_names[metric_num], path = "Data/Climate/ClimateMetrics/ClimateVelocity") # ensemble mean
            
        }
        
      }
    }
  }
}


for (theme_num in 1:length(theme_names)){
  for (scenario_num in 1:length(scenario_names)){
    for (model_num in 1:length(model_names)){
      for (metric_num in 1:length(metric_names)){
        
        metric_dat <- paste(metric_names[metric_num], model_names[model_num], scenario_names[scenario_num], sep = "_") #string at the moment: could be problem (https://stackoverflow.com/questions/6034655/convert-string-to-a-variable-name)
        ImptFeat <- create_ImportantFeatureLayer(aqua_sf, metric_name = metric_names[metric_num], colname = "transformed", 
                                                 metric_df =  metric_dat)
        RepFeat <- create_RepresentationFeature(ImptFeat, aqua_sf)
        Features <- cbind(ImptFeat, RepFeat) %>% 
          dplyr::select(-geometry.1)
        # 2. Get list of features
        features <- Features %>% 
          as_tibble() %>% 
          dplyr::select(-geometry) %>% 
          names()
        # 3. Differentiate targets for important features and representative features
        targets <- features %>% as_tibble() %>% 
          setNames(., "Species") %>% 
          add_column(target = 1) %>% 
          mutate(target = ifelse(str_detect(Species, pattern = ".1"), 25/95, 1))
        # 4. Set up the spatial planning problem
        out_sf <- cbind(Features, metric_dat, UniformCost)
        p <- prioritizr::problem(out_sf, features, "cost") %>%
          add_min_set_objective() %>%
          add_relative_targets(targets$target) %>%
          add_binary_decisions() %>%
          add_gurobi_solver(gap = 0, verbose = FALSE)
        # 5. Solve the planning problem 
        s <- prioritizr::solve(p)
        ID <- paste("s", i, sep= "")
        print(ID)
        ID_long <- paste(ID,model_names[model_num], theme_names[theme_num], metric_names[metric_num], scenario_names[scenario_num], sep = "-")
        ID_save <- paste(ID_long, ".rds", sep = "")
        saveRDS(s, paste0(output_solutions, ID_save)) # save solution
        
        # 6. Plot the spatial design
        s37_plot <- s37 %>% 
          mutate(solution_1 = as.logical(solution_1)) 
        (ggSol37 <- fSpatPlan_PlotSolution(s37_plot, PUs, land) + ggtitle("Climate-smart design: Climate Velocity", subtitle = "Important Feature, SSP 5-8.5") + theme(axis.text = element_text(size = 25)))
        ggsave(filename = "EM-ClimatePriorityArea-velocity-585.png",
               plot = ggSol37, width = 21, height = 29.7, dpi = 300,
               path = "Figures/") # save plot
        i = i+1
      }
    }
  }
  climate_layer <- readRDS(paste0(ClimateLayer_path, ClimateLayer_files[i]))
  layer <- fSpatPlan_Get_ClimateLayer(PUs, climate_layer, cCRS, metric = "roc_tos")
  
  saveRDS(layer, file.path("Output", 
                           paste(save_name, "ClimateLayer", ClimateLayer_files[i], sep = "_")))
}