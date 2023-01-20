# title: "Calculating climate metrics"
# author: "Isaac Brito-Morales and Tin Buenafe"

#### Preliminaries ####
# Load packages
#install.packages("pacman")
#devtools::install_github("JorGarMol/VoCC", dependencies = TRUE, build_vignettes = TRUE)
pacman::p_load(BiocManager, ncdf4, PCICt, ncdf4.helpers, raster, tidyverse, terra, VoCC)

# Define some lists
scenario_list <- c("ssp126", "ssp245", "ssp585") # list of climate scenarios
variable_list <- c("tos", "phos", "o2os") # list of variables sans "velocity"

# Load landmass
land <- ne_countries(scale = 'large', returnclass = 'sf') %>% 
  fSpatPlan_Convert2PacificRobinson() 

#### Multi-model ensemble approach: metrics ####
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM") # list of models

for(v in 1:length(variable_list)) {
  for (m in 1:length(model_list)) {
    for(s in 1:length(scenario_list)) {
      # Create the raster stack
      rs <- raster::stack(paste0("Data/Climatology/newfreq/", paste(variable_list[v], model_list[m], scenario_list[s], 2015, 2100, "annual", sep = "_"), ".nc"))
      
      # Clean up names
      names(rs) <- names(rs) %>%
        substr(., 1, 5)
      
      # Create rates of changes
      slp <- tempTrend(rs, th = 10)
      
      # Save raster files of climate metrics
      saveRDS(slp, paste0("Data/MultiModelEnsemble/raster/", paste(variable_list[v], model_list[m], scenario_list[s], sep = "_"), ".rds"))
      
      # Calculate velocity if the variable is tos
      if(variable_list[v] == "tos") {
        grad <- spatGrad(rs, th = 0.0001, projected = FALSE) # spatial gradient
        
        # VoCC local gradient
        vocc <- gVoCC(slp, grad)
        vocc$voccMag[] <- ifelse(is.infinite(vocc$voccMag[]), NA, vocc$voccMag[]) # replace inf with NAs
        
        saveRDS(vocc, paste0("Data/MultiModelEnsemble/raster/velocity_", paste(model_list[m], scenario_list[s], sep = "_"), ".rds"))
      }
      
      print(paste0("Variable: ", variable_list[v], "; model: ", model_list[m], "; scenario: ", scenario_list[s])) # Sanity test
    }
  }
}

#### Ensemble mean approach ####
for(v in 1:length(variable_list)) {
  for(s in 1:length(scenario_list)) {
    # Create the raster stack
    rs <- raster::stack(paste0("Data/Climatology/ensemble/", paste(variable_list[v], scenario_list[s], 2015, 2100, "annual", "ensemble", sep = "_"), ".nc"))
    
    # Clean up names
    names(rs) <- names(rs) %>%
      substr(., 1, 5)
    
    # Create rates of changes
    slp <- tempTrend(rs, th = 10)
    
    # Save raster files of climate metrics
    saveRDS(slp, paste0("Data/EnsembleMean/", paste(variable_list[v], "ensemble", scenario_list[s], sep = "_"), ".rds"))
    
    # Calculate velocity if the variable is tos
    if(variable_list[v] == "tos") {
      grad <- spatGrad(rs, th = 0.0001, projected = FALSE) # spatial gradient
      
      # VoCC local gradient
      vocc <- gVoCC(slp, grad)
      vocc$voccMag[] <- ifelse(is.infinite(vocc$voccMag[]), NA, vocc$voccMag[]) # replace inf with NAs
      
      saveRDS(vocc, paste0("Data/EnsembleMean/velocity_", paste("ensemble", scenario_list[s], sep = "_"), ".rds"))
    }
    
    print(paste0("Variable: ", variable_list[v], "; scenario: ", scenario_list[s])) # Sanity test
  }
}
