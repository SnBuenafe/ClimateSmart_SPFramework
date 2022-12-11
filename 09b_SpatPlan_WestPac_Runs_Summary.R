# title: "Exploring different aspects of climate-smart reserve design"
# author: "Tin Buenafe"
# Getting the appropriate figures (i.e., numbers/values) for the manuscript

#### Preliminaries ####
# Load preliminaries
source("03_SpatPlan_Master_Preliminaries.R") # climate layers are loaded in the script

#### ENSEMBLE THEME ####
ens <- readRDS("Output/lowregret/sFreq2-EM-Percentile-tos.rds")
nrow(ens %>% 
       dplyr::filter(selection == 5))/nrow(PUs) #23.0% are common to all

dummy <- call_dummy() 
sol <- ens %>% 
  dplyr::mutate(solution_1 = ifelse(selection == 5, yes = 1, no = 0))
df <- fFeatureRepresent(dummy, sol, "selection_5")
nrow(df %>% 
       dplyr::filter(selection_5 < 30))/nrow(df) #19.9% unmet targets

#### SCENARIO THEME ####
scen <- readRDS("Output/lowregret/sFreq1-EM-Percentile-tos.rds")
nrow(scen %>% 
       dplyr::filter(selection == 3))/nrow(PUs) #33.5% are common to all

sol <- scen %>% 
  dplyr::mutate(solution_1 = ifelse(selection == 3, yes = 1, no = 0))
df <- fFeatureRepresent(dummy, sol, "selection_3")
nrow(df %>% 
       dplyr::filter(selection_3 < 30))/nrow(df) #2.9% unmet targets

#### METRIC THEME ####
met <- readRDS("Output/lowregret/sFreq3-EM-Percentile-585.rds")
nrow(met %>% dplyr::filter(selection == 6))/nrow(PUs) #9.6% are common to all

sol <- met %>% 
  dplyr::mutate(solution_1 = ifelse(selection == 6, yes = 1, no = 0))
df <- fFeatureRepresent(dummy, sol, "selection_6")
nrow(df %>% dplyr::filter(selection_6 < 30))/nrow(df) #36.0% unmet targets

#### APPROACH THEME ####
app <- readRDS("Output/lowregret/sFreq4-EM-tos-585.rds")
nrow(app %>% dplyr::filter(selection == 4))/nrow(PUs) #5.9% are common to all

sol <- app %>% 
  dplyr::mutate(solution_1 = ifelse(selection == 4, yes = 1, no = 0))
df <- fFeatureRepresent(dummy, sol, "selection_4")
nrow(df %>% dplyr::filter(selection_4 < 30))/nrow(df) #98.5% unmet targets
