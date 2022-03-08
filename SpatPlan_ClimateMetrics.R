install.packages("pacman")

pacman::p_load(BiocManager, ncdf4, PCICt, ncdf4.helpers, raster, tidyverse, terra, doParallel)

# loading packages
devtools::install_github("JorGarMol/VoCC", dependencies = TRUE, build_vignettes = TRUE)
pacman::p_load(VoCC)

#### Raster Stacks ####

# IBM's function for converting .nc files to raster files
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

# define some lists
scenario_list <- c("SSP 1-2.6", "SSP 2-4.5", "SSP 5-8.5") # list of climate scenarios
variable_list <- c("tos", "phos", "o2os") # list of variables
model_list <- c("NorESM2-MM", "IPSL-CM6A-LR", "GFDL-ESM4", "CMCC-ESM2", "CanESM5") # list of models

# create function with the following inputs:
# 1. model
# 2. year bounds

# Tin's function to create a RasterStack
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

# NorESM2-MM
# setting yearly bounds of the files
from <- c(2015, 2021, 2031, 2041, 2051, 2061, 2071, 2081, 2091) %>% as_tibble_col(column_name = "from")
to <-  c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100) %>% as_tibble_col(column_name = "to")
year_bounds <- cbind(from, to)
create_raster("NorESM2-MM", year_bounds)

# GFDL-ESM4
# setting yearly bounds of the files
from <- c(2015, 2035, 2055, 2075, 2095) %>% as_tibble_col(column_name = "from")
to <- c(2034, 2054, 2074, 2094, 2100) %>% as_tibble_col(column_name = "to")
year_bounds <- cbind(from, to)
create_raster("GFDL-ESM4", year_bounds)

# IPSL-CM6A-LR; has /modified/ in file
# setting yearly bounds of the files
from <- c(2015) %>% as_tibble_col(column_name = "from")
to <- c(2100) %>% as_tibble_col(column_name = "to")
year_bounds <- cbind(from, to)
create_raster("IPSL-CM6A-LR", year_bounds)

# CMCC-ESM2
# same bounds as above
create_raster("CMCC-ESM2", year_bounds)

# CanESM5
# same bounds as above
create_raster("CanESM5", year_bounds)

#### Climate metrics ####

#### Mean annual change in temperature from 2015 to 2100 (using slp()) ####
## Creating a function to calculate this across variables and scenarios:
temp_slope <- function(variable, scenario, folder_name) {
  path = paste0("Data/Climate/RasterStack/", variable, "/", scenario, "/")
  list_files <- list.files(path)
  
  
  for (i in 1:length(list_files)) {
    rs <- readRDS(paste0("Data/Climate/RasterStack/", variable, "/", scenario, "/", list_files[i]))
    
    slp <- tempTrend(rs, th = 10) # Get temporal trend (slope)
    saveRDS(slp, paste0("Data/Climate/ClimateMetrics/", folder_name, "/", scenario, "/", list_files[i]))
    rm(rs, slp)
  }
  
  stack <- stack() # empty stack
  for (i in 2:length(list_files)) { # starts with 2 as not to include the "ensemble" folder
    x <- readRDS(paste0("Data/Climate/ClimateMetrics/", folder_name, "/", scenario, "/", list_files[i]))
    stack <- stack(x$slpTrends, stack)
  }
  
  meanR <- calc(stack, base::mean) # calculate a mean raster
  sdR <- calc(stack, stats::sd) # calculate sd raster
  file <- stack(meanR, sdR)
  names(file) <- c("slpTrends", "seTrends")
  # save raster stack
  saveRDS(file, paste0("Data/Climate/ClimateMetrics/", folder_name, "/", scenario, "/ensemble/", variable, "_", scenario, ".rds"))
  
}
folder_list <- c("TempSlope", "pHSlope", "OxygenSlope")
## SSP 1-2.6
temp_slope(variable = variable_list[1], scenario = scenario_list[1], folder_name = folder_list[1])
## SSP 2-4.5
temp_slope(variable = variable_list[1], scenario = scenario_list[2], folder_name = folder_list[1])
## SSP 5-8.5
temp_slope(variable = variable_list[1], scenario = scenario_list[3], folder_name = folder_list[1])

#### Mean annual change in pH from 2015 to 2100 (using slp()) ####
## SSP 1-2.6
temp_slope(variable = variable_list[2], scenario = scenario_list[1], folder_name = folder_list[2])
## SSP 2-4.5
temp_slope(variable = variable_list[2], scenario = scenario_list[2], folder_name = folder_list[2])
## SSP 5-8.5
temp_slope(variable = variable_list[2], scenario = scenario_list[3], folder_name = folder_list[2])

####  Mean annual change in oxygen from 2015 to 2100 (using slp()) ####
## SSP 1-2.6
temp_slope(variable = variable_list[3], scenario = scenario_list[1], folder_name = folder_list[3])
## SSP 2-4.5
temp_slope(variable = variable_list[3], scenario = scenario_list[2], folder_name = folder_list[3])
## SSP 5-8.5
temp_slope(variable = variable_list[3], scenario = scenario_list[3], folder_name = folder_list[3])

#### Making multi-model ensembles ####
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

# temperature
generate_ensemble(variable = variable_list[1], scenario = scenario_list[1]) # SSP 1-2.6
generate_ensemble(variable = variable_list[1], scenario = scenario_list[2]) # SSP 2-4.5
generate_ensemble(variable = variable_list[1], scenario = scenario_list[3]) # SSP 5-8.5

# ph
generate_ensemble(variable = variable_list[2], scenario = scenario_list[1]) # SSP 1-2.6
generate_ensemble(variable = variable_list[2], scenario = scenario_list[2]) # SSP 2-4.5
generate_ensemble(variable = variable_list[2], scenario = scenario_list[3]) # SSP 5-8.5

# oxygen
generate_ensemble(variable = variable_list[3], scenario = scenario_list[1]) # SSP 1-2.6
generate_ensemble(variable = variable_list[3], scenario = scenario_list[2]) # SSP 2-4.5
generate_ensemble(variable = variable_list[3], scenario = scenario_list[3]) # SSP 5-8.5

#### Calculate for rates of change (using tempTrend) ####
calculate_rate <- function(variable, scenario) {
  file = paste0("Data/Climate/YearlyEnsemble/", variable, "/", variable, "_", scenario, ".rds")
  rs <- readRDS(file)
  
  slp <- tempTrend(rs, th = 10)
  saveRDS(slp, paste0("Data/Climate/ClimateMetrics/RateOfChange/", variable, "/roc_", variable, "_", scenario, ".rds"))
}
# rates of temp change
for (i in 1:3) {
  calculate_rate(variable = variable_list[1], scenario = scenario_list[i])
}
# rates of ph change
for (i in 1:3) {
  calculate_rate(variable = variable_list[2], scenario = scenario_list[i])
}
# rates of oxygen change
for (i in 1:3) {
  calculate_rate(variable = variable_list[3], scenario = scenario_list[i])
}

#### Climate velocity (temperature) ####

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

generate_velocity(scenario = scenario_list[1]) # SSP 1-2.6
generate_velocity(scenario = scenario_list[2]) # SSP 2-4.5
generate_velocity(scenario = scenario_list[3]) # SSP 5-8.5

#### Creating multi-model ensemble -- Jase's suggestion ####

scenario = "SSP 5-8.5"
variable = "o2os"

# make this into a function with inputs: variable, scenario
inpdir <- "Data/Climate/RasterStack/"
outdir <- "Data/Climate/MultiModelEnsemble/"


ncores <- detectCores() - 1 
cl <- makeCluster(ncores)
registerDoParallel(cl)

file_list <- list.files(paste0(inpdir, variable, "/", scenario, "/"))

parallelStructure <- foreach(i = 2:length(file_list), .packages = c('tidyverse', 'VoCC', 'raster')) %dopar% {
  df <- readRDS(paste0(inpdir, variable, "/", scenario, "/", file_list[5]))
  
  yr_file <- sumSeries(df, p = "2015-01/2100-12", yr0 = "2015-01-01", l = nlayers(df),
                       fun = function(x) colMeans(x, na.rm = TRUE), freqin = "months", freqout = "years")
  save_name <- unlist(str_split(file_list[5], ".rds"))[1]
  saveRDS(yr_file, paste0(outdir, variable, "/", scenario, "/", save_name, "_ensemble.rds"))
  
  rm(yr_file)
}

#### Create climate metrics for the MultiModelEnsemble ####
variable = "tos"
scenario = "SSP 5-8.5"
metric = "roc"

calculate_rate <- function(variable, scenario, metric) {
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
# rates of temp change
calculate_rate(variable = "tos", scenario = "SSP 5-8.5", metric = "roc")

# rates of ocean acidification
calculate_rate(variable = "phos", scenario = "SSP 5-8.5", metric = "roc")

# rates of declining oxygen concentration
calculate_rate(variable = "o2os", scenario = "SSP 5-8.5", metric = "roc")

# velocity

scenario = "SSP 5-8.5"
generate_velocity <- function(scenario) {
  inpdir = paste0("Data/Climate/MultiModelEnsemble/tos/", scenario, "/")
  file_list <- list.files(inpdir)
  tempdir = paste0("Data/Climate/ClimateMetrics_Ensemble/tos/", scenario, "/")
  temp_list <- list.files(tempdir)
  
  outdir = paste0("Data/Climate/ClimateMetrics_Ensemble/velocity/", scenario, "/")
  
  for (i in 3:length(file_list)) {
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
generate_velocity(scenario = "SSP 5-8.5")


### TODO: Adding save as outputs as well

# Temperature
#' Call climate layers to be used here (different for each model)
inpdir <- "Data/Climate/ClimateMetrics_Ensemble/tos/SSP 5-8.5/"
file_list <- list.files(inpdir)

for (i in 1:length(file_list)) {
  cCRS <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  save_name <- unlist(str_split(file_list[i], pattern = "_"))[3]
  
  file <- readRDS(paste0(inpdir, file_list[i]))
  climate_layer <- fSpatPlan_Get_ClimateLayer(ClimateLayer = file, PUs, cCRS, metric = "roc_tos_ensemble")
  
  assign(x = paste0("tos_", save_name), value = climate_layer)
}

#' Save all of them as climate layers
list <- list(tos_CanESM5, `tos_CMCC-ESM2`, `tos_GFDL-ESM4`, `tos_IPSL-CM6A-LR`, `tos_NorESM2-MM`)
name_list <- c("roc_tos_SSP 5-8.5_CanESM5_ensemble.rds", "roc_tos_SSP 5-8.5_CMCC-ESM2_ensemble.rds",
               "roc_tos_SSP 5-8.5_GFDL-ESM4_ensemble.rds", "roc_tos_SSP 5-8.5_IPSL-CM6A-LR_ensemble.rds",
               "roc_tos_SSP 5-8.5_NorESM2-MM_ensemble.rds")

for (i in 1:length(list)){
  saveRDS(list[[i]], file.path("Output",
                           paste(save_name, "PU", paste0(PU_size, "km2"),
                                 name_list[i], sep = "_")))
}

#' If already done the saving above, just call the files
tos_CanESM5 <- readRDS("Output/WestPacific_PU_669.9km2_roc_tos_SSP 5-8.5_CanESM5_ensemble.rds")
`tos_CMCC-ESM2`<- readRDS("Output/WestPacific_PU_669.9km2_roc_tos_SSP 5-8.5_CMCC-ESM2_ensemble.rds")
`tos_GFDL-ESM4` <- readRDS("Output/WestPacific_PU_669.9km2_roc_tos_SSP 5-8.5_GFDL-ESM4_ensemble.rds")
`tos_IPSL-CM6A-LR` <- readRDS("Output/WestPacific_PU_669.9km2_roc_tos_SSP 5-8.5_IPSL-CM6A-LR_ensemble.rds")
`tos_NorESM2-MM` <- readRDS("Output/WestPacific_PU_669.9km2_roc_tos_SSP 5-8.5_NorESM2-MM_ensemble.rds")

# pH
## ph (Rate of ocean acidification)

#' Call climate layers to be used here (different for each model)
inpdir <- "Data/Climate/ClimateMetrics_Ensemble/phos/SSP 5-8.5/"
file_list <- list.files(inpdir)

for (i in 1:length(file_list)) {
  cCRS <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  save_name <- unlist(str_split(file_list[i], pattern = "_"))[3]
  
  file <- readRDS(paste0(inpdir, file_list[i]))
  climate_layer <- fSpatPlan_Get_ClimateLayer(ClimateLayer = file, PUs, cCRS, metric = "roc_phos_ensemble")
  
  assign(x = paste0("phos_", save_name), value = climate_layer)
}

#' Save all of them as climate layers
list <- list(phos_CanESM5, `phos_CMCC-ESM2`, `phos_GFDL-ESM4`, `phos_IPSL-CM6A-LR`, `phos_NorESM2-MM`)
name_list <- c("roc_phos_SSP 5-8.5_CanESM5_ensemble.rds", "roc_phos_SSP 5-8.5_CMCC-ESM2_ensemble.rds",
               "roc_phos_SSP 5-8.5_GFDL-ESM4_ensemble.rds", "roc_phos_SSP 5-8.5_IPSL-CM6A-LR_ensemble.rds",
               "roc_phos_SSP 5-8.5_NorESM2-MM_ensemble.rds")

for (i in 1:length(list)){
  saveRDS(list[[i]], file.path("Output",
                               paste(save_name, "PU", paste0(PU_size, "km2"),
                                     name_list[i], sep = "_")))
}

#' If already done the saving above, just call the files
phos_CanESM5 <- readRDS("Output/WestPacific_PU_669.9km2_roc_phos_SSP 5-8.5_CanESM5_ensemble.rds")
`phos_CMCC-ESM2`<- readRDS("Output/WestPacific_PU_669.9km2_roc_phos_SSP 5-8.5_CMCC-ESM2_ensemble.rds")
`phos_GFDL-ESM4` <- readRDS("Output/WestPacific_PU_669.9km2_roc_phos_SSP 5-8.5_GFDL-ESM4_ensemble.rds")
`phos_IPSL-CM6A-LR` <- readRDS("Output/WestPacific_PU_669.9km2_roc_phos_SSP 5-8.5_IPSL-CM6A-LR_ensemble.rds")
`phos_NorESM2-MM` <- readRDS("Output/WestPacific_PU_669.9km2_roc_phos_SSP 5-8.5_NorESM2-MM_ensemble.rds")

## o2 (Rate of Declining Oxygen Concentration)

#' Call climate layers to be used here (different for each model)
inpdir <- "Data/Climate/ClimateMetrics_Ensemble/o2os/SSP 5-8.5/"
file_list <- list.files(inpdir)

for (i in 1:length(file_list)) {
  cCRS <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  save_name <- unlist(str_split(file_list[i], pattern = "_"))[3]
  
  file <- readRDS(paste0(inpdir, file_list[i]))
  climate_layer <- fSpatPlan_Get_ClimateLayer(ClimateLayer = file, PUs, cCRS, metric = "roc_o2os_ensemble")
  
  assign(x = paste0("o2os_", save_name), value = climate_layer)
}

#' Save all of them as climate layers
list <- list(o2os_CanESM5, `o2os_CMCC-ESM2`, `o2os_GFDL-ESM4`, `o2os_IPSL-CM6A-LR`, `o2os_NorESM2-MM`)
name_list <- c("roc_o2os_SSP 5-8.5_CanESM5_ensemble.rds", "roc_o2os_SSP 5-8.5_CMCC-ESM2_ensemble.rds",
               "roc_o2os_SSP 5-8.5_GFDL-ESM4_ensemble.rds", "roc_o2os_SSP 5-8.5_IPSL-CM6A-LR_ensemble.rds",
               "roc_o2os_SSP 5-8.5_NorESM2-MM_ensemble.rds")

for (i in 1:length(list)){
  save_name = "WestPacific"
  saveRDS(list[[i]], file.path("Output",
                               paste(save_name, "PU", paste0(PU_size, "km2"),
                                     name_list[i], sep = "_")))
}

#' If already done the saving above, just call the files
o2os_CanESM5 <- readRDS("Output/WestPacific_PU_669.9km2_roc_o2os_SSP 5-8.5_CanESM5_ensemble.rds")
`o2os_CMCC-ESM2`<- readRDS("Output/WestPacific_PU_669.9km2_roc_o2os_SSP 5-8.5_CMCC-ESM2_ensemble.rds")
`o2os_GFDL-ESM4` <- readRDS("Output/WestPacific_PU_669.9km2_roc_o2os_SSP 5-8.5_GFDL-ESM4_ensemble.rds")
`o2os_IPSL-CM6A-LR` <- readRDS("Output/WestPacific_PU_669.9km2_roc_o2os_SSP 5-8.5_IPSL-CM6A-LR_ensemble.rds")
`o2os_NorESM2-MM` <- readRDS("Output/WestPacific_PU_669.9km2_roc_o2os_SSP 5-8.5_NorESM2-MM_ensemble.rds")

## velocity

#' Call climate layers to be used here (different for each model)
inpdir <- "Data/Climate/ClimateMetrics_Ensemble/velocity/SSP 5-8.5/"
file_list <- list.files(inpdir)

for (i in 1:length(file_list)) {
  cCRS <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  
  save_name <- unlist(str_split(file_list[i], pattern = "_"))[2]
  
  file <- readRDS(paste0(inpdir, file_list[i]))
  climate_layer <- fSpatPlan_Get_ClimateLayer(ClimateLayer = file, PUs, cCRS, metric = "velocity_ensemble")
  
  assign(x = paste0("velocity_", save_name), value = climate_layer)
}

#' Save all of them as climate layers
list <- list(velocity_CanESM5, `velocity_CMCC-ESM2`, `velocity_GFDL-ESM4`, `velocity_IPSL-CM6A-LR`, `velocity_NorESM2-MM`)
name_list <- c("velocity_SSP 5-8.5_CanESM5_ensemble.rds", "velocity_SSP 5-8.5_CMCC-ESM2_ensemble.rds",
               "velocity_SSP 5-8.5_GFDL-ESM4_ensemble.rds", "velocity_SSP 5-8.5_IPSL-CM6A-LR_ensemble.rds",
               "velocity_SSP 5-8.5_NorESM2-MM_ensemble.rds")

for (i in 1:length(list)){
  saveRDS(list[[i]], file.path("Output",
                               paste(save_name, "PU", paste0(PU_size, "km2"),
                                     name_list[i], sep = "_")))
}

#' If already done the saving above, just call the files
velocity_CanESM5 <- readRDS("Output/WestPacific_PU_669.9km2_velocity_SSP 5-8.5_CanESM5_ensemble.rds")
`velocity_CMCC-ESM2`<- readRDS("Output/WestPacific_PU_669.9km2_velocity_SSP 5-8.5_CMCC-ESM2_ensemble.rds")
`velocity_GFDL-ESM4` <- readRDS("Output/WestPacific_PU_669.9km2_velocity_SSP 5-8.5_GFDL-ESM4_ensemble.rds")
`velocity_IPSL-CM6A-LR` <- readRDS("Output/WestPacific_PU_669.9km2_velocity_SSP 5-8.5_IPSL-CM6A-LR_ensemble.rds")
`velocity_NorESM2-MM` <- readRDS("Output/WestPacific_PU_669.9km2_velocity_SSP 5-8.5_NorESM2-MM_ensemble.rds")
