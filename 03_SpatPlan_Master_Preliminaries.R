# Loading preliminary layers
# DESCRIPTION: This code loads all layers limited to the planning region created in `02_SpatPlan_Master_WestPac.R` and all code needed to run scripts

# Load all helper functions
helpfxns <- list.files(path = "HelperFunctions/", pattern = "*.R")
sapply(paste0("HelperFunctions/", helpfxns), source, .GlobalEnv)

# Declare directories
solutions_dir <- "Output/solutions/"
summary_dir <- "Output/summary/"
lowregret_dir <- "Output/lowregret/"
  
save_name <- "WestPacific"
PU_size = 669.9 # km2 (0.25 deg at equator)
Shape <- "Hexagon" # "Shape of PUs
cCRS <- "+proj=moll +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m no_defs" # using equal-area

#### Planning region ####
PUs <- read_rds(file.path("Output", paste(save_name, paste0("PlanningRegion.rds"), sep = "_")))
land <- rnaturalearth::ne_countries(scale = 'large', returnclass = 'sf') %>% 
  fSpatPlan_Convert2PacificCentered(cCRS = cCRS) # Land masses; needed for plotting

# Make boundary
boundary <- PUs %>% 
  sf::st_make_valid() %>% 
  sf::st_union()

# Total area
total_area = nrow(PUs)*PU_size

#### Conservation Features ####
aqua_sf <- read_rds(file.path("Output", paste(save_name, paste0("AquaMaps.rds"), sep = "_")))

# Changing to 1s and 0s
CutOff = 0.5
subset_aqua_sf <- aqua_sf %>% 
  tibble::as_tibble() %>% 
  dplyr::select(Doryrhamphus_excisus.excisus, Padina_sanctae.crucis, Platybelone_argalus.platyura,
                Tylosurus_acus.acus, Tylosurus_acus.melanotus)
aqua_sf <- aqua_sf %>% 
  dplyr::mutate_at(vars(colnames(subset_aqua_sf)), 
                   funs(case_when(. >= CutOff ~ 1,
                                  . <= CutOff ~ 0,
                                  is.na(.) ~ 0))) %>% 
  dplyr::mutate(cellID = row_number())

#### Cost layer ####
# Cost Layer, Squished
# cost <- read_rds(file.path("Output", paste(save_name, paste0("Cost.rds"), sep = "_"))) %>% 
#  mutate(Cost_squish = scales::oob_squish(Cost, quantile(Cost, c(0.01, 0.99))))

# Uniform Cost (using area of the planning units)
UniformCost <- PUs %>% 
  dplyr::mutate(cost = PU_size)
