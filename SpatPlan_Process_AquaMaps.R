
# TODO Look at switching to data.table 
# Intersect the data with PUs

library(tidyverse)
library(terra)
library(stars)
library(foreach)
library(doParallel)

source("SpatPlan_Extras.R")

# Load and process layers
aqua <- vroom::vroom("~/Nextcloud/MME1Data/AquaMaps/v2019/hcaf_species_native.csv") %>% # ~ 5 mins
  relocate(CenterLat, .after = CenterLong) %>% 
  mutate(SpeciesID = str_replace_all(SpeciesID, pattern = "-", replacement = "_"))

## A missing species at the moment
# # A tibble: 1 × 5
# SpecCode Species           DemersPelag DepthRangeShallow DepthRangeDeep
# <dbl> <chr>             <chr>                   <dbl>          <dbl>
#   1       NA Marshallena lepta NA                         NA             NA

# Load and save species info
species <- vroom::vroom("~/Nextcloud/MME1Data/AquaMaps/v2019/speciesoccursum.csv") %>% 
  left_join(vroom::vroom("~/Nextcloud/MME1Data/AquaMaps/v2019/hspen.csv"), by = "SpeciesID") %>% 
  mutate(SpeciesID = str_replace_all(SpeciesID, pattern = "-", replacement = "_"), 
         longnames = str_c(genus, species, sep = "_")) %>% 
  relocate(longnames, .after = SpeciesID) %>% 
  arrange(longnames) %>% 
  mutate(SpeciesIDNum = row_number()) %>% 
  relocate(SpeciesIDNum , .after = SpeciesID)

species <- aqua %>% 
  group_by(SpeciesID) %>% 
  summarise(MinLat = min(CenterLat, na.rm = TRUE),
            MaxLat = max(CenterLat, na.rm = TRUE),
            MinLon = min(CenterLong, na.rm = TRUE),
            MaxLon = max(CenterLong, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(species, ., by = "SpeciesID") %>% 
  relocate(all_of(c("MinLat", "MaxLat", "MinLon", "MaxLon")) , .after = longnames)

saveRDS(species, file.path("Data", "AquaMaps", "AquaMaps_SpeciesInfo.rds"))

# This code attempts to improve the AM data by getting depth (and other) data from FishBase/SealifeBase
# Also checks validity of names from FB
species <- fSpatPlan_Match_AquaMapsFishBase(species) # 68 minutes @ 3 Sept 2021

geo <- aqua %>% # Use to ensure all rasters are the same extent
  distinct(CsquareCode, .keep_all = TRUE) %>%
  dplyr::select(-c("SpeciesID", "Probability"))

## Process data in parallel

parallel::detectCores() # See how many cores are available on the CPU
n.cores <- parallel::detectCores() - 2 # Define number of cores

#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores,
  type = "FORK"
)

print(my.cluster) #check cluster definition (optional)
doParallel::registerDoParallel(cl = my.cluster) #register it to be used by %dopar%
foreach::getDoParRegistered() #check if it is registered (optional)
foreach::getDoParWorkers() # how many workers are available? (optional)


foreach(
  c = 1:length(species$SpeciesID)
) %dopar% {
  fi <- file.path("Data", "AquaMaps", "Species", paste0("AquaMaps_", species$longnames[c],".grd"))
  
  aqua %>%
    filter(SpeciesID %in% species$SpeciesID[c]) %>%
    pivot_wider(names_from = SpeciesID, values_from = Probability, values_fill = NA) %>%
    left_join(geo, ., by = c("CenterLong", "CenterLat", "CsquareCode")) %>%
    dplyr::select(-CsquareCode) %>%
    as.matrix() %>%
    terra::rast(type = "xyz", crs = "+proj=longlat +datum=WGS84") %>%
    terra::writeRaster(fi, overwrite = TRUE)
  rm(fi)
}

parallel::stopCluster(cl = my.cluster)

# End individual file processing

# Load each raster file and create nc file of 1000 rasters
# Build the file list from species so they are in the correct order
fil <- crossing(a = "Data", b = .Platform$file.sep, c = "AquaMaps", d = .Platform$file.sep, e = "Species", f = .Platform$file.sep, g = "AquaMaps_", h = species$longnames, i = ".grd") %>% 
  unite(col = filename, sep = "") %>% 
  pull(filename)

fils <- seq(1, length(fil), by = 1000) # Create sequence of 1000 element groups
fils = c(fils, (length(fil)+1)) # Add the end length

for (f in 1:(length(fils)-1)){
  
  r <- rast(fil[fils[f]:(fils[f+1]-1)])
  crs(r) <- "epsg:4326" #"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  fname <- file.path("Data", "AquaMaps","Split1GB", paste0("AquaMaps_", str_pad(f, width = 2, side = "left", pad = "0"),".tif"))
  writeRaster(r, fname, overwrite=TRUE, progress = TRUE, memfrac = 0.8, gdal = c("COMPRESS=NONE"), tempdir = tempdir(), todisk = TRUE, verbose = TRUE)
  
  rm(r)
}


# Having a lot of trouble with seperate nc files in stars. Now trying to merge into 1 nc file
r <- list.files(file.path("Data", "AquaMaps", "Split1GB"), pattern = ".tif.aux.xml", full.names = TRUE) %>% # Get the list of nc files
  str_replace_all(".aux.xml", "") %>% # List of o
  rast() 

writeRaster(r, "Data/AquaMaps/AquaMaps.tif", overwrite=TRUE, progress = TRUE, memfrac = 0.8, gdal = c("COMPRESS=NONE", "projwin_srs=EPSG:4326"), tempdir = tempdir(), todisk = TRUE, verbose = TRUE)



