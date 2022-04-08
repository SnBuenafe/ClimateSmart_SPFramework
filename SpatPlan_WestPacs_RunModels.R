# title: "Climate-smart methods paper runs"
# author: "Sandra Neubert and Tin Buenafe"

#### Run Models ####
# Description
# This script runs the models based on the climate priority area approach 
#for each combination of metrics (4), climate model (5) and climate scenario (3).
#To allow for keep the RStudio environment as empty as possible for parallel processing, 
#necessary functions were extracted from the SpatPlan_Master_Preliminaries script.


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

model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")

# Call function for each metric (adapted from Preliminaries script)
fcallMetrics2 <- function(metric, 
                          model = NA, # if model = NA, approach is ensemble mean
                          path, # path with / at the end
                          scenario = NA
) {
  
  scenario_obj <- c("SSP126", "SSP245", "SSP585")
  scenario_path <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5")
  
  if(is.na(model)) {
    if(is.na(scenario)){
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
  }
  else{
    if (scenario == "SSP126"){
      scenario_path <- scenario_path[1]
    } else if (scenario == "SSP245") {
      scenario_path <- scenario_path[2]
    } else if (scenario == "SSP585"){
      scenario_path <- scenario_path[3]
    }
    
    files <- list.files(file.path(path, metric, scenario_path))
    for(j in 1:length(files)) {
      df <- readRDS(file.path("Output",
                              paste(save_name, "ClimateLayer", files[j], sep = "_")))
      assign(x = paste(metric, model[j], scenario, sep = "_"), value = df, envir=.GlobalEnv)
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
rm(subset_aqua_sf, CutOff, model_list)


# Initialise variables for loop
theme_names <- c("ClimatePriorityArea") #"feature", "penalty", "percentile"
scenario_names <- c("SSP126", "SSP245", "SSP585") 
model_names <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM")
metric_names <- c("tos", "phos", "o2os", "velocity")
i <- 250 #ID starting location of CPA in Meta data file (excluding EM)
gc()

library(rlang)

for (theme_num in 1:length(theme_names)){ #not really necessary anymore: too much computer power needed if all approaches in one loop
  for (scenario_num in 2:length(scenario_names)){
    for (metric_num in 1:length(metric_names)){
      for (model_num in 1:length(model_names)){
        # 1. Rates of Climate warming
          fcallMetrics2(metric = metric_names[metric_num],path = "Data/Climate/ClimateMetrics_Ensemble", model = model_names, scenario = scenario_names[scenario_num]) # ensemble mean
          metric_dat <- paste(metric_names[metric_num], model_names[model_num], scenario_names[scenario_num], sep = "_") #string at the moment: could be problem (https://stackoverflow.com/questions/6034655/convert-string-to-a-variable-name)
          metric_dat <- eval_tidy(quo(!! sym(metric_dat)))
          ImptFeat <- create_ImportantFeatureLayer(aqua_sf, metric_name = metric_names[metric_num], colname = "transformed", 
                                                   metric_df =  metric_dat)
          gc()#clear up space
          RepFeat <- create_RepresentationFeature(ImptFeat, aqua_sf)
          gc()
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
          print(paste(ID, metric_names[metric_num], model_names[model_num], scenario_names[scenario_num], sep = ","))#to double-check 
          ID_long <- paste(ID,model_names[model_num], theme_names[theme_num], metric_names[metric_num], scenario_names[scenario_num], sep = "-")
          ID_save <- paste(ID_long, ".rds", sep = "")
          saveRDS(s, paste0(output_solutions, ID_save)) # save solution
          
          # 6. Plot the spatial design
          ID_plot <- paste(ID_long, ".png", sep = "")
          s_plot <- s %>% 
            mutate(solution_1 = as.logical(solution_1)) 
          (ggSol <- fSpatPlan_PlotSolution(s_plot, PUs, land) + theme(axis.text = element_text(size = 25)))
          ggsave(filename = ID_plot,
                 plot = ggSol, width = 21, height = 29.7, dpi = 300,
                 path = "Figures/") # save plot
          
          #clean up environment
          rm(ImptFeat, RepFeat, Features, features, targets)
          rm(list=ls(pattern=paste0(metric_names[metric_num], ".*")))
          gc()
          
          i <- i+1 #set counter to new ID number
          
      }
    }
  }
  #if  ( i == 290 ) break;
}
