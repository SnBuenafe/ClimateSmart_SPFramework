# Code to match aquamaps with fishbase data
#
# Written by Jason D. Everett
# UQ/CSIRO/UNSW
# Last edited 19 November 2021

# devtools::install_github("ropensci/rfishbase")

# Habitat definitions
# "bathydemersal": Living and feeding on the bottom below 200 m.
# "bathypelagic": Region of the oceanic zone between 1,000 m to 4,000 m; between the mesopelagic layer above and the abyssopelagic layer below. Living or feeding in open waters at depths between 1,000 and 4,000 m. In FishBase this term is used to include the depth range from 200 m to the bottom and thus the zones mesopelagic, bathypelagic and abyssopelagic. 
# "benthic": Dwelling on, or relating to, the bottom of a body of water; living on the bottom of the ocean and feeding on benthic organisms. 
# "benthopelagic": Living and feeding near the bottom as well as in midwaters or near the surface. Feeding on benthic as well as free swimming organisms. Many freshwater fish are opportunistic feeders that forage on the bottom as well as in midwater and near the surface, also pertaining to forms which hover or swim just over the floor of the sea, e.g. Halosauridae, Macrouridae, Moridae, Brotulidae; the depth zone about 100 metres off the bottom at all depths below the edge of the continental shelf.
# "demersal": Sinking to or lying on the bottom; living on or near the bottom and feeding on benthic organisms
# "epipelagic": The uppermost, normally photic layer of the ocean between the ocean surface and the thermocline, usually between depths of 0-200 m; living or feeding on surface waters or at midwater to depths of 200 m.
# "epiphytic":
# "host"
# "others"
# "pelagic": Living and feeding in the open sea; associated with the surface or middle depths of a body of water; free swimming in the seas, oceans or open waters; not in association with the bottom. Many pelagic fish feed on plankton. In FishBase, referring to surface or mid water from 0 to 200 m depth.
# "pelagic-neritic": Inhabiting shallow coastal waters over the continental shelf.
# "pelagic-oceanic": Pertaining to the open ocean beyond the continental shelf; living in the open ocean, offshore. Opposite of neritic.
# "reef-associated": Living and feeding on or near coral reefs
# "sessile": Organism being attached to a substrate.

fSpatPlan_Match_AquaMapsFishBase <- function(spp, fld = c("SpecCode", "Species", "DemersPelag", "DepthRangeShallow", "DepthRangeDeep")){
  
  library(rfishbase)
  
  spp <- spp %>% 
    mutate(longnames2 = str_replace_all(longnames,"_", " ")) %>% 
    relocate(longnames2, .after = longnames)
  
  fish_class <- c("Actinopterygii", "Chondrichthyes", "Elasmobranchii", "Holocephali", "Myxini", "Cephalaspidomorphi", "Sarcopterygii")
  
  # Get the species that are fish
  finfish <- spp %>% 
    filter(class %in% fish_class)
  
  #Get all 
  fspec <- rfishbase::species(finfish$longnames2, 
                              fields = fld,
                              server = "fishbase") %>% 
    fix_species_type(server = "fishbase") %>% # Warnings are for converting "NA" to NA
    distinct()
  

  # Get the species that are not fish
  sealife <- spp %>% 
    filter(!class %in% fish_class) %>% 
    mutate(longnames2 = str_replace_all(longnames2,"_", " "))
  
  sspec <- rfishbase::species(sealife$longnames2,
                              fields = fld,
                              server = "sealifebase") %>% 
    fix_species_type(server = "sealifebase") %>% # Warnings are for converting "NA" to NA
    distinct()
  
  
  # Check that dim(finfish)[1] + dim(sealife)[1] == dim(spp)[1]
  
  ## Now fix up NAs
  
  # First FishBase
  fspecNA <- fspec %>% 
    filter(is.na(SpecCode)) # Check NAs
  
  valid <- fSpatPlan_Validate_FBNAs(fspecNA, "fishbase") # Validate all the names
  
  valid <- rfishbase::species(pull(valid, ValidSpecies), # Get the species info for valid names
                              fields = fld,
                              server = "fishbase") %>% 
    fix_species_type(server = "fishbase") # Warnings are for converting "NA" to NA
  
  valid <- valid %>% 
    filter(!is.na(SpecCode)) # Remove unmatched species
  
  fspec <- fspec %>% 
    filter(!is.na(SpecCode)) %>% # Remove NAs
    rbind(valid) %>% # Add valid back in
    rename(Habitat = DemersPelag, FB_DepthRangeShallow = DepthRangeShallow, FB_DepthRangeDeep = DepthRangeDeep)
  rm(valid, fspecNA)
  
  # Now SeaLifeBase
  sspecNA <- sspec %>% 
    filter(is.na(SpecCode))  # Check NAs
  
  valid <- fSpatPlan_Validate_FBNAs(sspecNA, "sealifebase") # Validate all the names
  valid <- rfishbase::species(pull(valid, ValidSpecies), # Get the species info for valid names
                              fields = fld,
                              server = "sealifebase") %>% 
    fix_species_type(server = "sealifebase") # Warnings are for converting "NA" to NA
  
  valid <- valid %>%
    filter(!is.na(SpecCode)) # Remove unmatched species # 1 missing as at 14 Nov 2021

  sspec <- sspec %>% 
    filter(!is.na(SpecCode)) %>% # Check NAs
    rbind(valid) %>% 
    rename(Habitat = DemersPelag, FB_DepthRangeShallow = DepthRangeShallow, FB_DepthRangeDeep = DepthRangeDeep)
  rm(valid, sspecNA)
  
  ## Now merge with the original species list
  spp_out <- spp %>% 
    left_join(bind_rows(fspec, sspec), by = c("longnames2" = "Species")) %>%
    # left_join(bind_rows(fspec, sspec), by = c("speccode" = "SpecCode")) %>%
    relocate("Habitat", .after = MaxLon) %>%
    mutate_at("Habitat", str_replace, "Sessile", "sessile") %>%
    mutate_at("Habitat", str_replace, "Demersal", "demersal") %>%
    mutate_at("Habitat", replace_na, "benthic") %>% # I checked NA online and they are all benthic or demersal. Generalising here
    mutate(HabitatCat = case_when(Habitat %in% c("sessile", "demersal", "benthic", "reef-associated", "benthopelagic", "bathydemersal") ~ "Benthic",
                                  Habitat %in% c("pelagic-neritic", "pelagic", "bathypelagic", "pelagic-oceanic", "epipelagic") ~ "Pelagic")) %>% 
  relocate(HabitatCat, .after = Habitat) %>% 
    distinct() # TODO For some reason this adds a few extra rows, but they are identical so we just remove them.
  
  write_rds(spp_out, file.path("Data", "AquaMaps", "AquaMaps_SpeciesInfoFB.rds"))
  
  rm(fspec, sspec)
  
  return(spp_out)
}
