###########################################################################.####
###########################################################################.####
# Script for Cleaning Survey Data for Data in brief (Wheat part)
###########################################################################.####
# Authour: Charles Rees (FiBL and ETH Zuerich) and Lorin Ineichen 
###########################################################################.####
###########################################################################.####


###########################################################################.####
# Library                                                                   ####
###########################################################################.####
library(tidyr)
library(dplyr)
library(writexl)
library(sjlabelled)

rm(list = ls())
setwd("../_Data_in_brief")

###########################################################################.####
###########################################################################.####
# Load Wheat Data                                                           ####
###########################################################################.####
###########################################################################.####

### File paths are blinded 
# Load pre-cleaned data file (from Data_Cleaning_Nature_Scientific_Data)
crop_data <- readRDS("RAW_FILE_PATH_REDACTED")
# Load data of other infos as schemes
schemes <- readRDS("RAW_FILE_PATH_REDACTED") 

###########################################################################.####
###########################################################################.####
# Select and Reclassify Variables                                           ####
###########################################################################.####
###########################################################################.####

# Reclassify
names(crop_data)
crop_data$id<-as.character(crop_data$id)

# add schemes from original data
relevant_vars <- schemes %>%
  dplyr::select(
    id, organic_scheme, soil_cover_scheme, tillage_scheme, herbicide_scheme, 
    pesticide_scheme, fertiliser_scheme, wider_row_scheme, protection_strip_scheme,
    prec_application_schem, kantonal_scheme_soil, kantonal_scheme_inputs, 
    kantonal_scheme_investment, kantonal_scheme_none)


# Merge the relevant variables into crop_data using the id variable
crop_data_schemes <- crop_data %>%
  left_join(relevant_vars, by = "id")

# Rename correctly
crop_data_schemes <- crop_data_schemes %>%
  dplyr::rename(
    organic_scheme = organic_scheme.y,
    fertiliser_scheme = fertiliser_scheme.y
  )

#Remove ids that do not have secondary data 
missing_ids <- c(84, 134, 267, 562, 878, 1572, 1577, 1690, 1928, 1944, 3587)
crop_data_schemes <- crop_data_schemes %>% filter(!(id %in% missing_ids))

# Rename id to survey_id (everywhere accross all datasets same key_variable)
crop_data_schemes$survey_id <- crop_data_schemes$id

##########################################################################.####
##########################################################################.####
### Finalize dataset                                                       ####
##########################################################################.####
##########################################################################.####

# Create wheat price variabel
crop_data_schemes <- crop_data_schemes %>% 
  dplyr::mutate(milling_wheat_pice_oln_23 = 58.5, # harvest price according to Swiss granum (CHF / dt)
                milling_wheat_price_bio_23 = 107.5, # harvest price according to SBV (CHF / dt)
  )

##########################################################################.####
## Select variables                                                        ####
##########################################################################.####
# creat dataframe
wheat_data <- crop_data_schemes %>%
  dplyr::select(
    survey_id,
    milling_wheat_production_standard,
    milling_wheat_area_reported,
    milling_wheat_yield, 
    milling_wheat_average_yield, 
    milling_wheat_synthetic_fert, 
    milling_wheat_organic_fert, 
    milling_wheat_sowing_density, 
    milling_wheat_biostimulator_treats, 
    milling_wheat_herbicide_treats, 
    milling_wheat_fungicide_treats, 
    milling_wheat_insecticide_treats, 
    milling_wheat_pgr_treats,
    milling_wheat_production_standard)

##########################################################################.####
### Add Labels                                                             ####
##########################################################################.####
wheat_data <- labelled::set_variable_labels(
  wheat_data,
  survey_id = "Individual survey ID",
  milling_wheat_production_standard = "Farm grew milling wheat in 2022/23 growing season [1 = yes]",
  milling_wheat_area_reported = "Area of milling wheat grown - reported by respondant [ha]",
  milling_wheat_yield = "Milling wheat yield in 2022/23 [dt/ha]",
  milling_wheat_average_yield = "Milling wheat yield in preceding five growing seasons [dt/ha]",
  milling_wheat_synthetic_fert = "Quantity of synthetic fertiliser applied [kg N/ha]",
  milling_wheat_organic_fert = "Quantity of organic fertiliser applied [kg N/ha]",
  milling_wheat_sowing_density = "Sowing density of wheat [kg seed/ha]",
  milling_wheat_biostimulator_treats = "Number of treatments with biostimulators [Average number/ha]",
  milling_wheat_herbicide_treats = "Number of treatments with herbicides [Average number/ha]",
  milling_wheat_fungicide_treats = "Number of treatments with fungicides [Average number/ha]",
  milling_wheat_insecticide_treats = "Number of treatments with insecticides [Average number/ha]",
  milling_wheat_pgr_treats = "Number of treatments with plant growth regulators [Average number/ha]"
)

###########################################################################.####
# Save Dataset                                                              ####
###########################################################################.####
saveRDS(wheat_data, file = "Q:/Dep_FSS/2_Projects/2023_EU-InBestSoil_35245/02_ProjectWork/_Data_in_brief/Dataset_survey_wheat_subset.rds")

# Save as excel
write_xlsx(wheat_data, 
           path = "Q:/Dep_FSS/2_Projects/2023_EU-InBestSoil_35245/02_ProjectWork/_Data_in_brief/Dataset_survey_wheat_subset.xlsx")