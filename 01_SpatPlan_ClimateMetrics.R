# title: "Calculating climate metrics"
# author: "Isaac Brito-Morales and Tin Buenafe"

#### Preliminaries ####
# Load packages
#install.packages("pacman")
#devtools::install_github("JorGarMol/VoCC", dependencies = TRUE, build_vignettes = TRUE)
pacman::p_load(BiocManager, ncdf4, PCICt, ncdf4.helpers, raster, tidyverse, terra, doParallel, VoCC)

# Define some lists
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5") # list of climate scenarios
variable_list <- c("tos", "phos", "o2os") # list of variables sens "velocity"
model_list <- c("CanESM5", "CMCC-ESM2", "GFDL-ESM4", "IPSL-CM6A-LR", "NorESM2-MM") # list of models

#### Load functions ####
# Converting .nc files to raster files
ncdf_2D_rs <- function(nc, from, to, v = "tos", x = "lon", y = "lat") {
  # Extract data from the netCDF file  
  nc <- nc_open(nc)
  dat <- ncvar_get(nc, v) # x, y, year 
  dat[] <- dat
  X <- dim(dat)[1]
  Y <- dim(dat)[2]
  tt <- nc.get.time.series(nc, v = "time", time.dim.name = "time")
  tt <- as.POSIXct(tt)
  tt <- as.Date(tt)
  nc_close(nc)
  rs <- raster(nrow = Y, ncol = X) # Make a raster with the right dims
  # Fix orientation of original data
  drs <- data.frame(coordinates(rs))
  # Create Rasters Stack
  rs_list <- list() # empty list to allocate results
  st <- stack()
  for (i in 1:length(tt)) {
    dt1 <- rasterFromXYZ(cbind(drs, as.vector(dat[,, i])))
    dt1[]<- ifelse(dt1[] <= -2, NA, dt1[]) # for some models that have weird temperatures
    dt1[]<- ifelse(dt1[] >= 40, NA, dt1[]) # for some models that have weird temperatures
    st <- addLayer(st, flip(dt1, 2))
    print(paste0(i, " of ", length(tt)))
  }
  names(st) <- seq(as.Date(paste(from, "1", "1", sep = "/")), 
                   as.Date(paste(to, "12", "1", sep = "/")), by = "month")
  crs(st) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  return(st)
}

# Function to create RasterStacks for each model
# Make sure that the following folders exist in the directory.
# Input folder: "Data/GCM/regrid", "Data/GCM/modified/regrid/" (for IPSL-CM6A-LR)
# Int folder: "Data/Climate/RawRaster"
# Output folder: "Data/Climate/RasterStack/"
create_raster <- function(model, bounds) {
  
  scenario_list <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5") # list of climate scenarios
  variable_list <- c("tos", "phos", "o2os") # list of variables
  
  for (k in 1:length(variable_list)) {
    for (i in 1:length(scenario_list)){
      if(model == "IPSL-CM6A-LR") {
        path = paste0("Data/GCM/", model, "/", variable_list[k], "/", scenario_list[i], "/modified/regrid/")
      }
      else {
        path = paste0("Data/GCM/", model, "/", variable_list[k], "/", scenario_list[i], "/regrid/")
      }
      fileList <- list.files(path)
      
      for (j in 1:length(fileList)){
        # convert netCDF to raster
        raster <- ncdf_2D_rs(paste0(path, fileList[j]), v = variable_list[k], from = bounds$from[j], to = bounds$to[j], x = "lon", y = "lat")
        
        # save raster as .rds to conserve space
        saveRDS(raster, paste0("Data/Climate/RawRaster/", variable_list[k], "/", scenario_list[i], "/", model, "/", fileList[j], ".rds"))
        rm(raster)
        gc()
      }
      
      rm(path, fileList)
    }
  }
  
  for (k in 1:length(variable_list)) {
    for (j in 1:length(scenario_list)){
      path = paste0("Data/Climate/RawRaster/", variable_list[k], "/", scenario_list[j], "/", model, "/")
      temporaryList <- list.files(path)
      
      x <- stack()
      
      for (i in 1:length(temporaryList)){
        raster <- readRDS(paste0(path, temporaryList[i]))
        x <- stack(x, raster)
      }
      
      path_rs = paste0("Data/Climate/RasterStack/", variable_list[k], "/", scenario_list[j], "/")
      saveRDS(x, paste0(path_rs, variable_list[k], "_", model, "_", scenario_list[j], ".rds"))
    }
  }
  
}

# Function to create ensemble mean (monthly & yearly) 
# Make sure that the following folders exist in the directory.
# Input folder: "Data/Climate/RasterStack/"
# Output folders: "Data/Climate/Ensemble/" (monthly)
# "Data/Climate/YearlyEnsemble/" (yearly)
generate_ensemble <- function(variable, scenario) {
  input_path = paste0("Data/Climate/RasterStack/", variable, "/", scenario, "/")
  output_path = paste0("Data/Climate/Ensemble/", variable, "/")
  yr_path = paste0("Data/Climate/YearlyEnsemble/", variable, "/")
  
  list_files <- list.files(input_path)
  
  stack <- stack() # empty stack
  for (i in 1:length(list_files)) {
    x <- readRDS(paste0(input_path, list_files[i]))
    stack <- stack(x, stack)
  }
  
  # Getting mean temperatures per month to make ensemble
  index <- rep(1:1032, times = 5)  # 5 complete replications
  mean <- stackApply(x = stack, indices = index, fun = base::mean)
  
  from = 2015
  to = 2100
  names(mean) <- seq(as.Date(paste(from, "1", "1", sep = "/")), 
                     as.Date(paste(to, "12", "1", sep = "/")), by = "month")
  
  # Should end up with a raster that has 1032 layers (monthly)
  saveRDS(mean, paste0(output_path, variable, "_", scenario, ".rds"))
  
  # Saving yearly data as well
  yr_file <- sumSeries(mean, p = "2015-01/2100-12", yr0 = "2015-01-01", l = nlayers(mean),
                       fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")
  
  saveRDS(yr_file, paste0(yr_path, variable, "_", scenario, ".rds"))
  
}

# Function to calculate rates of change (warming, acidification, deoxygenation)
# Make sure that the following folders exist in the directory:
# Input folder: "Data/Climate/YearlyEnsemble"
# Output folder: "Data/Climate/ClimateMetrics/RateOfChange/
calculate_rate <- function(variable, scenario) {
  file = paste0("Data/Climate/YearlyEnsemble/", variable, "/", variable, "_", scenario, ".rds")
  rs <- readRDS(file)
  
  slp <- tempTrend(rs, th = 10)
  saveRDS(slp, paste0("Data/Climate/ClimateMetrics/RateOfChange/", variable, "/roc_", variable, "_", scenario, ".rds"))
}

# Function to calculate climate velocity
# Make sure that the following folders exist in the directory:
# Input folders: "Data/Climate/YearlyEnsemble/" (should contain the annual ensemble mean)
# "Data/Climate/ClimateMetrics/RateOfChange/" (should contain the rate of change in temperature -- temporal gradient in climate velocity)
# Output folder: "Data/Climate/ClimateMetrics/ClimateVelocity/"
generate_velocity <- function(scenario) {
  file <- paste0("Data/Climate/YearlyEnsemble/tos/tos_", scenario, ".rds")
  
  input_path <- paste0("Data/Climate/ClimateMetrics/RateOfChange/tos/roc_tos_", scenario, ".rds")
  out_path <- "Data/Climate/ClimateMetrics/ClimateVelocity/"
  
  rs <- readRDS(file)
  slp <- readRDS(input_path) # temporal gradient
  grad <- spatGrad(rs, th = 0.0001, projected = FALSE) # spatial gradient
  
  # VoCC local gradient
  vocc <- gVoCC(slp, grad)
  vocc$voccMag[] <- ifelse(is.infinite(vocc$voccMag[]), NA, vocc$voccMag[]) # replace inf with NAs
  
  saveRDS(vocc, paste0(out_path, "velocity_tos_", scenario, ".rds")) # save velocity raster
}

# Function to create ensemble (yearly) using the multi-model ensemble approach (5 layers, one for each model)
# Make sure that the following folders exist in the directory:
# Input folder: "Data/Climate/RasterStack/"
# Output folder: "Data/Climate/MultiModelEnsemble/"
generate_MultiModelEnsemble <- function(variable, scenario) {
  inpdir <- "Data/Climate/RasterStack/"
  outdir <- "Data/Climate/MultiModelEnsemble/"
  
  file_list <- list.files(paste0(inpdir, variable, "/", scenario, "/"))
  
  for(i in 1:length(file_list)) {
    df <- readRDS(paste0(inpdir, variable, "/", scenario, "/", file_list[i]))
    
    yr_file <- sumSeries(df, p = "2015-01/2100-12", yr0 = "2015-01-01", l = nlayers(df),
                         fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")
    save_name <- unlist(str_split(file_list[i], ".rds"))[1]
    saveRDS(yr_file, paste0(outdir, variable, "/", scenario, "/", save_name, "_ensemble.rds"))
  }
}

# Function to calculate rates of change using the multi-model ensemble approach
# Make sure that the following folders exist in the directory:
# Input folder: "Data/Climate/MultiModelEnsemble/"
# Output folder: "Data/Climate/ClimateMetrics_Ensemble/"
calculate_MultiModelEnsembleRate <- function(variable, scenario, metric) {
  inpdir <- "Data/Climate/MultiModelEnsemble/"
  outdir <- "Data/Climate/ClimateMetrics_Ensemble/"
  
  file_list <- list.files(paste0(inpdir, variable, "/", scenario, "/"))
  
  for(i in 1:length(file_list)) {
    rs <- readRDS(paste0(inpdir, variable, "/", scenario, "/", file_list[i]))
    
    slp <- tempTrend(rs, th = 10)
    
    save_name <- unlist(str_split(file_list[i], pattern = "_"))[2]
    
    saveRDS(slp, paste0(outdir, variable, "/", scenario, "/", metric, "_", variable, "_", save_name, "_", scenario, "_ensemble.rds"))
  }
  
}

# Function to calculate climate velocity using the multi-model ensemble approach
# Make sure that the following folders exist in the directory:
# Input folder: "Data/Climate/MultiModelEnsemble/"
# Int folder: "Data/Climate/ClimateMetrics_Ensemble/tos/" where the rate of climate warming is saved (temporal gradient)
# Output folder: "Data/CLimate/ClimateMetrics_Ensemble/velocity/"
generate_MultiModelEnsembleVelocity <- function(scenario) {
  inpdir = paste0("Data/Climate/MultiModelEnsemble/tos/", scenario, "/")
  file_list <- list.files(inpdir)
  tempdir = paste0("Data/Climate/ClimateMetrics_Ensemble/tos/", scenario, "/")
  temp_list <- list.files(tempdir)
  
  outdir = paste0("Data/Climate/ClimateMetrics_Ensemble/velocity/", scenario, "/")
  
  for (i in 1:length(file_list)) {
    rs <- readRDS(paste0(inpdir, file_list[i]))
    slp <- readRDS(paste0(tempdir, temp_list[i])) # temporal gradient
    
    grad <- spatGrad(rs, th = 0.0001, projected = FALSE) # spatial gradient
    
    # VoCC local gradient
    vocc <- gVoCC(slp, grad)
    vocc$voccMag[] <- ifelse(is.infinite(vocc$voccMag[]), NA, vocc$voccMag[]) # replace inf with NAs
    
    save_name <- unlist(str_split(temp_list[i], pattern = "_"))[3]
    saveRDS(vocc, paste0(outdir, "velocity_", save_name, "_", scenario, "_ensemble.rds"))
    
    rm(rs, slp, grad, vocc)
  }
}

#### Create RasterStack for each model ####
# Create list of year_bounds, with the index the same as the model_list
year_bounds <- list()
for(i in 1:length(model_list)) {
  if(i %in% c(1, 2, 4)) {
    from = c(2015) %>% as_tibble_col(column_name = "from")
    to = c(2100) %>% as_tiblbe_col(column_name = "to")
    year_bounds[[i]] <- cbind(from, to)
  }
  else if(i == 3) {
    from = c(2015, 2035, 2055, 2075, 2095) %>% as_tibble_col(column_name = "from")
    to = c(2034, 2054, 2074, 2094, 2100) %>% as_tibble_col(column_name = "to")
    year_bounds[[i]] <- cbind(from, to)
  }
  else if(i == 5) {
    from = c(2015, 2021, 2031, 2041, 2051, 2061, 2071, 2081, 2091) %>% as_tibble_col(column_name = "from")
    to =  c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100) %>% as_tibble_col(column_name = "to")
    year_bounds[[i]] <- cbind(from, to)
  }
}

# Run loop
for(i in 1:length(model_list)) {
  create_raster(mode_list[i], year_bounds[[i]])
}



#### Ensemble mean approach ####
# ----- Create ensemble mean (yearly) for all variables -----
for(scenario_num in 1:length(scenario_list)) {
  for(variable_num in 1:length(variable_list)) {
    generate_ensemble(variable = variable_list[variable_num],
                      scenario = scenario_list[scenario_num])
  }
}

# ----- Calculate rate of change metrics -----
# Use tempTrend() function from VoCC
for(scenario_num in 1:length(scenario_list)) {
  for(variable_num in 1:length(variable_list)) {
    calculate_rate(variable = variable_list[variable_num],
                   scenario = scenario_list[scenario_num])
  }
}

# ----- Calculate climate velocity using temperature -----
# Use gVoCC to calculate for velocity across the scenarios
for(scenario_num in 1:length(scenario_list)) {
  generate_velocity(scenario = scenario_list[scenario_num])
}

#### Multi-model ensemble approach ####
# ----- Create climate layers using the multi-model ensemble approach -----
for(scenario_num in 1:length(scenario_list)) {
  for(variable_num in 1:length(variable_list)) {
    generate_MultiModelEnsemble(variable = variable_list[variable_num], scenario = scenario_list[scenario_num])
  }
}

# ----- Calculate rate of change climate metrics using the multi-model ensemble approach -----
for(scenario_num in 1:length(scenario_list)) {
  for(variable_num in 1:length(variable_list)) {
    calculate_MultiModelEnsembleRate(variable = variable_list[variable_num], scenario = scenario_list[scenario_num], metric = "roc")
  }
}

# ----- Calculate climate velocity using the multi-model ensemble approach -----
for(scenario_num in 1:length(scenario_list)) {
  generate_MultiModelEnsembleVelocity(scenario_list[scenario_num])
}
