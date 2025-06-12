###########################################################################.####
###########################################################################.####
# Script for Cleaning Survey Data for Data article
###########################################################################.####
# Authour: Charles Rees (FiBL and ETH Zuerich) and Lorin Ineichen (FiBL)
###########################################################################.####
###########################################################################.####

# This R Script loads the raw un-cleaned data and then cleans all the variables 
# that required a text or numerical input from the respondents. The variables 
# that exist as coded id numbers are changed to more descriptive names on the 
# basis of the "codebook" excel document which further describes the meaning 
# of each variable.

# The variables that are cleaned are associated with:
# 1. Other measures to reduce compaction not listed in the original survey
# 2. Other primary cultivation methods not listed in the original survey
# 3. The number of plant protection and crop input treatments that were answered non-numerically
# 4. Milling wheat seed sowing density 
# 5. Milling wheat yields recorded in the wrong units or implausible amounts
# 6. Crop input "true zeros" for non use
# 7. Milling wheat fertiliser application recorded in the wrong units or amounts
# 8. Birth year, management start written in YY format instead of YYYY 
# 9. SAK/FTE (Full-time equivalent) variables 

# The code then does the following:
# 1. Removes "protest" answers and keeps only observations that filled out survey after page 8 (Farm characteristics)
# 2. Fills NAs that are "true zeros" with 0 answers
# 3. Defines class of variables (factor, numerical)
# 4. Grouping birth year, management start and arable land areas for anonymity reasons
# 5. Translates variables into English
# 6. Condenses the dataset to include only specific variables

###########################################################################.####
# Library                                                                   ####
###########################################################################.####
library(exactextractr)   # For efficiently extracting raster values over polygons.
library(tidyverse)       # A collection of packages (ggplot2, dplyr, tidyr, etc.) for data manipulation and visualization.
library(raster)          # For working with raster datasets (reading, writing, analyzing).
library(stringr)         # For working with and manipulating strings.
library(plyr)            # For splitting, applying, and combining data (similar to dplyr but older).
library(sf)              # For handling spatial vector data (simple features).
library(tmap)            # For creating thematic maps (static and interactive).
library(terra)           # For working with spatial data (raster and vector, a modern alternative to raster).
library(ggplot2)         # For data visualization (part of tidyverse, loaded separately here).
library(fasterize)       # For converting spatial vector data to raster data quickly.
library(readr)           # For reading tabular data like CSV files (part of tidyverse).
library(openxlsx)        # For reading, writing, and manipulating Excel files without Java dependency.
library(tidyr)           # For data tidying and reshaping (part of tidyverse).
library(reshape2)        # For reshaping data between wide and long formats (older than tidyr).
library(fs)              # For working with files and directories (cross-platform).
library(arsenal)         # For statistical reporting, including comparisons of datasets.
library(dplyr)           # For data manipulation (part of tidyverse).
library(spatialreg)      # For spatial regression analysis.
library(spldv)           # For panel data econometrics with spatial dependence.
library(spatstat.geom)   # For working with spatial point patterns (part of the spatstat package suite).
library(stargazer)       # For creating publication-quality regression tables and summaries.
library(spdep)           # For spatial dependence and spatial regression modeling.
library(ProbitSpatial)   # For probit models with spatial dependence.
library(spmodel)         # For spatial linear mixed models and geostatistics.
library(MODISTools)      # For downloading and processing MODIS remote sensing data.
library(sphet)           # For spatial econometrics with heteroskedasticity and additional features.
library(estimatr)        # For fast and robust linear model estimation.
library(nngeo)           # For nearest-neighbor-based operations on spatial data.
library(visdat)          # For visualizing data structures and missingness.
library(excel.link)      # For direct communication between R and Excel.
library(sjlabelled)      # For working with variable labels (common in survey datasets).
library(tidyselect)      # For selecting Variables 
library(tibble)          # For data-frames.
library(writexl)         # For creating Excel datasets.
library(sjlabelled)      # For creating labels that describe the variable names.

rm(list = ls())

###########################################################################.####
###########################################################################.####
# Load Primary Raw Data                                                     ####
###########################################################################.####
###########################################################################.####

### File paths are blinded 

# Raw data file
raw_data <- xl.read.file("RAW_FILE_PATH_REDACTED")
raw_data<-as.data.frame(raw_data)

# Codebook
data_codes <- read.xlsx("RAW_FILE_PATH_REDACTED")

###########################################################################.####
###########################################################################.####
# Re-naming Variables                                                       ####
###########################################################################.####
###########################################################################.####
# Raw data file
clean_data <- raw_data

# Matching question codes and new names
for (j in 1:ncol(raw_data)) { # for all variable names in the raw data
  for (i in 1:nrow(data_codes)) { # for all names of variables in code book
    
    names(clean_data)[j] = ifelse(names(clean_data)[j] == data_codes$Original_Question_Code_LimeSurvey[i], data_codes$Variable_Name[i], names(clean_data)[j] )
    # rename variable if code matches code
  }
}

###########################################################################.####
###########################################################################.####
# Cleaning Cultivation Measures                                             ####
###########################################################################.####
###########################################################################.####

###########################################################################.####
## Categorizing Pressure Reduction Measures "Other"                         ####
###########################################################################.####

### Other compaction measures data file
compaction_measures <- read.xlsx("RAW_FILE_PATH_REDACTED") # explains how other compaction measure responses are grouped together

### Matching text responses for compaction reduction practices to new codes
clean_data <- merge(clean_data, compaction_measures, by.x = "compact_other_measure", by.y = "compact_other_measure", all = TRUE) %>%
  relocate(compact_other_measure, .after = compact_no_measure)

clean_data$compaction_oth_reduced_machienery_size <- as.character(grepl("small machines", clean_data$compact_other_measure_text))
clean_data$compaction_oth_tacked_tractor <- as.character(grepl("tracks", clean_data$compact_other_measure_text))
clean_data$compaction_oth_manual_pressure_adjust <- as.character(grepl("manual pressure", clean_data$compact_other_measure_text))
clean_data$compaction_oth_low_pressure_tyres <- as.character(grepl("low pressure", clean_data$compact_other_measure_text))
clean_data$compaction_oth_pressure_control_tyres <- as.character(grepl("pressure control", clean_data$compact_other_measure_text))
clean_data$compaction_oth_wide_tyres <- as.character(grepl("wide tyres", clean_data$compact_other_measure_text))

clean_data <- clean_data %>%
  dplyr::mutate(compact_wide_double_tyre = ifelse(compact_double_wheels == "Ja" | compaction_oth_wide_tyres == TRUE, "Wide or double tyres", "Not used"),
                compact_lower_pressure_tyre = ifelse(compact_low_pressure_tire == "Ja" |
                                                       compact_tire_pressure_control == "Ja" |
                                                       compaction_oth_manual_pressure_adjust == TRUE |
                                                       compaction_oth_low_pressure_tyres == TRUE |
                                                       compaction_oth_pressure_control_tyres == TRUE, "Tyre pressures lowered", "Not used"),
                compact_tracks = ifelse(compact_traced_vehicle == "Ja" | compaction_oth_tacked_tractor == TRUE, "Tracked vehicles", "Not used"),
                compact_smaller_machienery = ifelse(compaction_oth_reduced_machienery_size == TRUE, "Light machienes", "Not used")
  )

clean_data$compact_wide_double_tyre <- factor(clean_data$compact_wide_double_tyre, levels = c("Not used", "Wide or double tyres"))

clean_data$compact_lower_pressure_tyre <- factor(clean_data$compact_lower_pressure_tyre, levels = c("Not used", "Tyre pressures lowered"))

clean_data$compact_tracks <- factor(clean_data$compact_tracks, levels = c("Not used", "Tracked vehicles"))

clean_data$compact_smaller_machienery <- factor(clean_data$compact_smaller_machienery, levels = c("Not used", "Light machienes"))

compaction_clean_data <- clean_data %>%
  subset(select = c(id, compact_double_wheels, compact_low_pressure_tire, compact_tire_pressure_control,compact_traced_vehicle, compact_no_measure, compact_other_measure, compact_wide_double_tyre, compact_lower_pressure_tyre, compact_tracks, compact_smaller_machienery))

### Triple checked

###########################################################################.####
## Categorizing Primary Cultivation Methods "Other"                         ####
###########################################################################.####

### Other cultivation measures data files by type
cultivation_measures_pulled <- read.xlsx("RAW_FILE_PATH_REDACTED") # explains how other pulled cultivation measure responses are grouped together
cultivation_measures_pto <- read.xlsx("RAW_FILE_PATH_REDACTED") # explains how other pto cultivation measure  responses are grouped together

### Removing duplicate answers
cultivation_measures_pulled <- unique(cultivation_measures_pulled[ , 1:4 ])
cultivation_measures_pto <- unique(cultivation_measures_pto[ , 1:4 ])

cultivation_measures_pulled <- cultivation_measures_pulled %>%
  subset(select = c("culti_machine_pulled_text_name", "culti_machine_pulled_class_name_new"))

cultivation_measures_pto <- cultivation_measures_pto %>%
  subset(select = c("culti_machine_pto_text_name", "culti_machine_pto_class_name_new"))


### Matching text responses for compaction reduction practices to new codes in clean data and putting variable back in order listed in codebook
clean_data <- merge(clean_data, cultivation_measures_pulled,  by.x = "prem_cultivation_pulled_tool_other" , by.y = "culti_machine_pulled_text_name", all = TRUE) %>%
  relocate(prem_cultivation_pulled_tool_other, .after = prem_cultivation_pulled_tool)

clean_data <- merge(clean_data, cultivation_measures_pto, by.x = "prem_cultivation_pto_tool_other", by.y = "culti_machine_pto_text_name", all = TRUE) %>%
  relocate(prem_cultivation_pto_tool_other, .after = prem_cultivation_pto_tool)

### Creating new columns for re-grouped and anglicised names of preliminary cultivation methods
clean_data <- clean_data %>%
  dplyr::mutate(prem_cultivation_pto_tool_new = prem_cultivation_pto_tool,
                prem_cultivation_pulled_tool_new = prem_cultivation_pulled_tool,
                
                prem_cultivation_pto_tool_new = ifelse(prem_cultivation_pto_tool_new == "Sonstiges", NA, 
                                                       ifelse(prem_cultivation_pto_tool_new == "Schälfräse", "Rotary Harrow (PTO)", 
                                                              ifelse(prem_cultivation_pto_tool_new == "Spatenmaschine", "Spader (PTO)", NA))),
                
                prem_cultivation_pulled_tool_new = ifelse(prem_cultivation_pulled_tool_new == "Sonstiges", NA, 
                                                          ifelse(prem_cultivation_pulled_tool_new == "Pflug", "Plough", 
                                                                 ifelse(prem_cultivation_pulled_tool_new == "Grubber", "Cultivator", NA))),
                
                prem_cultivation_direct = pem_cultivation_method,
                prem_cultivation_direct = ifelse(prem_cultivation_direct == "Ich führe keine Grundbodenbearbeitung durch (z.B. Direktsaat)", "Direct Drilling", NA)) %>%
  arrange(id)

### merging preliminary cultivation methods into one
clean_data <- clean_data %>%
  dplyr::mutate(preliminary_cultivation_tool_combined = pmap_chr(list(culti_machine_pulled_class_name_new, culti_machine_pto_class_name_new, prem_cultivation_pto_tool_new, prem_cultivation_pulled_tool_new, prem_cultivation_direct), ~ paste(na.omit(c(...)), collapse = " ")),
                preliminary_cultivation_tool_combined = ifelse(preliminary_cultivation_tool_combined == "", NA, preliminary_cultivation_tool_combined))

table(clean_data$preliminary_cultivation_tool_combined)

clean_data <- clean_data %>%
  dplyr::mutate(preliminary_cultivation_tool_grouped = ifelse(preliminary_cultivation_tool_combined == "Cultivator" | preliminary_cultivation_tool_combined == "Disc Harrow", "Disc or Tined Cultivator", 
                                                              ifelse(preliminary_cultivation_tool_combined == "Power Harrow (PTO)" | preliminary_cultivation_tool_combined == "Rotary Harrow (PTO)" | preliminary_cultivation_tool_combined == "Spader (PTO)", "Rotary Cultivator Type - PTO Driven", 
                                                                     ifelse(preliminary_cultivation_tool_combined == "Plough", "Plough", 
                                                                            ifelse(preliminary_cultivation_tool_combined == "Half plough, half cultivator", "Plough/Cultivator", 
                                                                                   ifelse(preliminary_cultivation_tool_combined == "Direct Drilling", "Direct Drilling", NA))))))

clean_data$preliminary_cultivation_tool_grouped <- factor(clean_data$preliminary_cultivation_tool_grouped, levels = c("Direct Drilling", "Disc or Tined Cultivator", "Plough/Cultivator", "Plough", "Rotary Cultivator Type - PTO Driven"))

table(clean_data$preliminary_cultivation_tool_grouped)

### merging preliminary cultivation methods into one
clean_data <- clean_data %>%
  dplyr::mutate(preliminary_cultivation_tool_class = pem_cultivation_method,
                preliminary_cultivation_tool_class = ifelse(preliminary_cultivation_tool_class == "Ich führe keine Grundbodenbearbeitung durch (z.B. Direktsaat)", "No Primary Cultivation - Direct Drilling", 
                                                            ifelse(preliminary_cultivation_tool_class == "Zapfwellenbetrieben (z.B. Schälfräse, Spatenmaschine)", "Primary Cultivation - PTO Driven", 
                                                                   ifelse(preliminary_cultivation_tool_class == "Gezogen (z.B. Pflug, Grubber)", "Primary Cultivation - Trailed Implement", NA))),
                
                preliminary_cultivation_tool_class = ifelse(preliminary_cultivation_tool_grouped == "Direct Drilling", "No Primary Cultivation - Direct Drilling",
                                                            ifelse(preliminary_cultivation_tool_grouped == "Disc or Tined Cultivator", "Primary Cultivation - Trailed Implement (Cultivator)",
                                                                   ifelse(preliminary_cultivation_tool_grouped == "Plough", "Primary Cultivation - Trailed Implement (Plough)",
                                                                          ifelse(preliminary_cultivation_tool_grouped == "Plough/Cultivator", "Primary Cultivation - Trailed Implement (Cultivator)",
                                                                                 ifelse(preliminary_cultivation_tool_grouped == "Rotary Cultivator Type - PTO Driven", "Primary Cultivation - PTO Driven", preliminary_cultivation_tool_class))))),
                
                preliminary_cultivation_tool_class = ifelse(is.na(preliminary_cultivation_tool_class) & pem_cultivation_method == "Gezogen (z.B. Pflug, Grubber)", "Primary Cultivation - Trailed Implement (Cultivator)", preliminary_cultivation_tool_class),
                
                preliminary_cultivation_tool_class = ifelse(is.na(preliminary_cultivation_tool_class) & pem_cultivation_method == "Zapfwellenbetrieben (z.B. Schälfräse, Spatenmaschine)", "Primary Cultivation - PTO Driven", preliminary_cultivation_tool_class))



clean_data$preliminary_cultivation_tool_class <- factor(clean_data$preliminary_cultivation_tool_class, levels = c("No Primary Cultivation - Direct Drilling", "Primary Cultivation - Trailed Implement (Cultivator)", "Primary Cultivation - Trailed Implement (Plough)", "Primary Cultivation - PTO Driven"))

table(clean_data$preliminary_cultivation_tool_class)

clean_data_prem_cult <- clean_data %>%
  subset(select = c(id, pem_cultivation_method, preliminary_cultivation_tool_grouped, preliminary_cultivation_tool_class))

###########################################################################.####
## Categorizing Seedbed Preparation Cultivation Methods                     ####
###########################################################################.####

### Creating new columns for re-grouped and Anglicised names of seedbed cultivation methods
clean_data <- clean_data %>%
  dplyr::mutate(seedbed_cultivation_tool_class = seedbed_method,
                seedbed_cultivation_tool_class = ifelse(seedbed_cultivation_tool_class == "Zapfwellenbetriebenz.B. Schälfräse, Kreiselegge, etc.", "Seedbed Preparation - PTO Driven", 
                                                        ifelse(seedbed_cultivation_tool_class == "Gezogenz.B. Grubber etc.", "Seedbed Preparation - Trailed Implement", 
                                                               ifelse(seedbed_cultivation_tool_class == "Ich führe keine (weitere) Saatbettbereitung durch", "No (further) Seedbed Preparation - Direct Drilling or Single Pass Cultivation", NA))),
                ### Changing Answer for ID 1673 (TG24214) for MA003S2 --> Sch?lpflug & for MA004 --> Zapfwellenbetriebenz.B. Sch?lfr?se, Kreiselegge, etc.becuase it was classified wrong
                seedbed_cultivation_tool_class = ifelse(id == 1673, "Seedbed Preparation - PTO Driven", seedbed_cultivation_tool_class))

clean_data$seedbed_cultivation_tool_class <- factor(clean_data$seedbed_cultivation_tool_class, levels = c("No (further) Seedbed Preparation - Direct Drilling or Single Pass Cultivation", "Seedbed Preparation - Trailed Implement", "Seedbed Preparation - PTO Driven"))

table(clean_data$seedbed_cultivation_tool_class)

### Creating new columns for Anglicised names of drilling methods
clean_data <- clean_data %>%
  dplyr::mutate(drilling_method_class = drilling_method,
                drilling_method_class = ifelse(drilling_method_class == "SolosaatTraktor mit Egge gefolgt von Traktor mit Sämaschine", "Crop Establishment - Multi-pass", 
                                               ifelse(drilling_method_class == "KombiTraktor mit Säkombination", "Crop Establishment - Single-pass",  NA)))

clean_data$drilling_method_class <- factor(clean_data$drilling_method_class, levels = c("Crop Establishment - Single-pass", "Crop Establishment - Multi-pass"))

table(clean_data$drilling_method_class)
### Triple checked

rm(compaction_clean_data, compaction_measures, cultivation_measures_pto, cultivation_measures_pulled, clean_data_prem_cult)

##########################################################################.####
##########################################################################.####
# Cleaning Crop Input Data                                                 ####
##########################################################################.####
##########################################################################.####

##########################################################################.####
## Categorizing Pesticide Treatment Frequencies Not Specified Correctly    ####
##########################################################################.####
### Cleaning biostimulants 

table(clean_data$milling_wheat_biostimulator_treats)

clean_data <- clean_data %>%
  dplyr::mutate(milling_wheat_biostimulator_treats = ifelse(milling_wheat_biostimulator_treats %in% c("-", "--", "/", "0x", "ke", "Ke", "n", "no", "O", "X"), 0,
                                                            ifelse(milling_wheat_biostimulator_treats %in% c("em", "EM", "Mg"), 1, 
                                                                   ifelse(milling_wheat_biostimulator_treats %in% c("2x"), 2, milling_wheat_biostimulator_treats))))
table(clean_data$milling_wheat_biostimulator_treats)

ggplot(data = clean_data, aes(x = milling_wheat_biostimulator_treats)) +
  geom_bar() +
  theme_classic() +
  labs(x = "Biostimulator Treatments", y = "Count")

##########################################################################.####
### Cleaning herbicides 

table(clean_data$milling_wheat_herbicide_treats)

clean_data <- clean_data %>%
  dplyr::mutate(milling_wheat_herbicide_treats = ifelse(milling_wheat_herbicide_treats %in% c("-", "--", "/", "0x", "ke", "Ke", "n", "no", "O", "X"), 0,
                                                        ifelse(milling_wheat_herbicide_treats %in% c("01", "1,", "1.", "1x", "j"), 1, 
                                                               ifelse(milling_wheat_herbicide_treats %in% c("2x"), 2, milling_wheat_herbicide_treats))))
table(clean_data$milling_wheat_herbicide_treats)

ggplot(data = clean_data, aes(x = milling_wheat_herbicide_treats)) +
  geom_bar() +
  theme_classic() +
  labs(x = "Herbicide Treatments", y = "Count")

##########################################################################.####
### Cleaning fungicides 

table(clean_data$milling_wheat_fungicide_treats)

clean_data <- clean_data %>%
  dplyr::mutate(milling_wheat_fungicide_treats = ifelse(milling_wheat_fungicide_treats %in% c("-", "--", "/", "0x", "ke", "Ke", "n", "no", "O", "X", "o"), 0,
                                                        ifelse(milling_wheat_fungicide_treats %in% c("1-"), 1, 
                                                               ifelse(milling_wheat_fungicide_treats %in% c("2x"), 2, milling_wheat_fungicide_treats))))
table(clean_data$milling_wheat_fungicide_treats)

ggplot(data = clean_data, aes(x = milling_wheat_fungicide_treats)) +
  geom_bar() +
  theme_classic() +
  labs(x = "Fungicide Treatments", y = "Count")

##########################################################################.####
### Cleaning insecticides 

table(clean_data$milling_wheat_insecticide_treats)

clean_data <- clean_data %>%
  dplyr::mutate(milling_wheat_insecticide_treats = ifelse(milling_wheat_insecticide_treats %in% c("-", "--", "05", "0x", "?", "Ke", "n", "no", "O", "X", "o"), 0,
                                                          ifelse(milling_wheat_insecticide_treats %in% c("JA"), 1, milling_wheat_insecticide_treats)),
                milling_wheat_insecticide_treats = ifelse(id == 2258, 0, milling_wheat_insecticide_treats)) # the "?" symbol was not recognised in the ifelse statement so manually adjusting it using respondent id

table(clean_data$milling_wheat_insecticide_treats)

ggplot(data = clean_data, aes(x = milling_wheat_insecticide_treats)) +
  geom_bar() +
  theme_classic() +
  labs(x = "Insecticide Treatments", y = "Count")

##########################################################################.####
### Cleaning pgr 

table(clean_data$milling_wheat_pgr_treats)

clean_data <- clean_data %>%
  dplyr::mutate(milling_wheat_pgr_treats = ifelse(milling_wheat_pgr_treats %in% c("-", "0x", "?", "Ke", "n", "5d", "O", "X", "o"), 0,
                                                  ifelse(milling_wheat_pgr_treats %in% c("1,", "1x"), 1, 
                                                         ifelse(milling_wheat_pgr_treats %in% c("2x"), 2, milling_wheat_pgr_treats))))

table(clean_data$milling_wheat_pgr_treats)

ggplot(data = clean_data, aes(x = milling_wheat_pgr_treats)) +
  geom_bar() +
  theme_classic() +
  labs(x = "PGR Treatments", y = "Count")


##########################################################################.####
## Filling true zeros in crop inputs                                       ####
##########################################################################.####

### Changing NA to 0 for those who grew wheat as this is a "true zero" application as the question was not asked depending on previous answers regarding production system e.g. organic = yes
if (TRUE) {
  ### Synthetic fertiliser non-application (0 kg N)
  table(clean_data$milling_wheat_synthetic_fert)
  sum(is.na(clean_data$milling_wheat_synthetic_fert)) 
  clean_data$milling_wheat_synthetic_fert <- ifelse(clean_data$milling_wheat_grow == "Ja", ifelse(is.na(clean_data$milling_wheat_synthetic_fert), 0, clean_data$milling_wheat_synthetic_fert), NA)
  table(clean_data$milling_wheat_synthetic_fert)
  sum(is.na(clean_data$milling_wheat_synthetic_fert))
  
  ### Organic fertiliser non-application (0 kg N)
  table(clean_data$milling_wheat_organic_fert)
  sum(is.na(clean_data$milling_wheat_organic_fert))
  clean_data$milling_wheat_organic_fert <- ifelse(clean_data$milling_wheat_grow == "Ja", ifelse(is.na(clean_data$milling_wheat_organic_fert), 0, clean_data$milling_wheat_organic_fert), NA)
  table(clean_data$milling_wheat_organic_fert)
  sum(is.na(clean_data$milling_wheat_organic_fert))
  
  ### Biostimulator non-use (0 treatments)
  table(clean_data$milling_wheat_biostimulator_treats)
  clean_data$milling_wheat_biostimulator_treats <- ifelse(clean_data$milling_wheat_grow == "Ja", ifelse(is.na(clean_data$milling_wheat_biostimulator_treats), 0, clean_data$milling_wheat_biostimulator_treats), NA)
  table(clean_data$milling_wheat_biostimulator_treats)
  
  ### Herbicide non-use (0 treatments)
  table(clean_data$milling_wheat_herbicide_treats)
  clean_data$milling_wheat_herbicide_treats <- ifelse(clean_data$milling_wheat_grow == "Ja", ifelse(is.na(clean_data$milling_wheat_herbicide_treats), 0, clean_data$milling_wheat_herbicide_treats), NA)
  table(clean_data$milling_wheat_herbicide_treats)
  
  ### Fungicide non-use (0 treatments)
  table(clean_data$milling_wheat_fungicide_treats)
  clean_data$milling_wheat_fungicide_treats <- ifelse(clean_data$milling_wheat_grow == "Ja", ifelse(is.na(clean_data$milling_wheat_fungicide_treats), 0, clean_data$milling_wheat_fungicide_treats), NA)
  table(clean_data$milling_wheat_fungicide_treats)
  
  ### Insecticide non-use (0 treatments)
  table(clean_data$milling_wheat_insecticide_treats)
  clean_data$milling_wheat_insecticide_treats <- ifelse(clean_data$milling_wheat_grow == "Ja", ifelse(is.na(clean_data$milling_wheat_insecticide_treats), 0, clean_data$milling_wheat_insecticide_treats), NA)
  table(clean_data$milling_wheat_insecticide_treats)
  
  ### PGR non-use (0 treatments)
  table(clean_data$milling_wheat_pgr_treats)
  clean_data$milling_wheat_pgr_treats <- ifelse(clean_data$milling_wheat_grow == "Ja", ifelse(is.na(clean_data$milling_wheat_pgr_treats), 0, clean_data$milling_wheat_pgr_treats), NA)
  table(clean_data$milling_wheat_pgr_treats)
} 

##########################################################################.####
##########################################################################.####
# Editing magnitude of crop yields and inputs with obvious errors          ####
##########################################################################.####
##########################################################################.####

crop_data <- clean_data %>%
  subset(select = c(id, token, lastpage, organic_scheme, fertiliser_scheme, milling_wheat_grow, milling_wheat_area, milling_wheat_ip_suisse, milling_wheat_demeter, milling_wheat_yield, milling_wheat_average_yield, milling_wheat_synthetic_fert, milling_wheat_organic_fert, milling_wheat_sowing_density, milling_wheat_biostimulator_treats, milling_wheat_herbicide_treats, milling_wheat_fungicide_treats, milling_wheat_insecticide_treats, milling_wheat_pgr_treats)) %>%
  filter(milling_wheat_grow == "Ja") %>%
  filter(lastpage > 8) %>%
  dplyr::mutate(use_synthetic_fert = ifelse(milling_wheat_synthetic_fert >0, 1, 0),
                use_organic_fert = ifelse(milling_wheat_organic_fert >0, 1, 0)) %>%
  dplyr::rename(milling_wheat_area_reported = milling_wheat_area)


spurious_entries_yield <- c(2715, 2394) # wrote 0 but looks like did not mean to fill out wheat data

crop_data <- crop_data %>%
  filter(!id %in% spurious_entries_yield)

##########################################################################.####
## Editing magnitude where yield units are incorrect (t/ha rather than dt/are)  ####
##########################################################################.####

crop_data[crop_data$id == 1870, "milling_wheat_yield"] <- 14 # wrote 1.4 but looks like wrong units

crop_data[crop_data$id == 3093, "milling_wheat_yield"] <- 17 # wrote 1.7 but looks like wrong units
crop_data[crop_data$id == 3093, "milling_wheat_average_yield"] <- 17 # wrote 1.7 but looks like wrong units

crop_data[crop_data$id == 2983, "milling_wheat_yield"] <- 20 # wrote 2.0 but looks like wrong units
crop_data[crop_data$id == 2983, "milling_wheat_average_yield"] <- 20 # wrote 2.5 but looks like wrong units

crop_data[crop_data$id == 1694, "milling_wheat_yield"] <- 30 # wrote 3.0 but looks like wrong units
crop_data[crop_data$id == 1694, "milling_wheat_average_yield"] <- 50 # wrote 5.0 but looks like wrong units

crop_data[crop_data$id == 3536, "milling_wheat_yield"] <- 35 # wrote 3.5 but looks like wrong units
crop_data[crop_data$id == 3536, "milling_wheat_average_yield"] <- 45 # wrote 4.5 but looks like wrong units

crop_data[crop_data$id == 1066, "milling_wheat_yield"] <- 46 # wrote 4.6 but looks like wrong units
crop_data[crop_data$id == 1066, "milling_wheat_average_yield"] <- 50 # wrote 5.0 but looks like wrong units

crop_data[crop_data$id == 1652, "milling_wheat_yield"] <- 48 # wrote 4.8 but looks like wrong units
crop_data[crop_data$id == 1652, "milling_wheat_average_yield"] <- 48 # wrote 4.8 but looks like wrong units

crop_data[crop_data$id == 610, "milling_wheat_yield"] <- 50 # wrote 5.0 but looks like wrong units
crop_data[crop_data$id == 610, "milling_wheat_average_yield"] <- 80 # wrote 8.0 but looks like wrong units

crop_data[crop_data$id == 2837, "milling_wheat_yield"] <- 50 # wrote 5.0 but looks like wrong units
crop_data[crop_data$id == 2837, "milling_wheat_average_yield"] <- 60 # wrote 6.0 but looks like wrong units

crop_data[crop_data$id == 3207, "milling_wheat_yield"] <- 50 # wrote 5.0 but looks like wrong units
crop_data[crop_data$id == 3207, "milling_wheat_average_yield"] <- 70 # wrote 7.0 but looks like wrong units

crop_data[crop_data$id == 3590, "milling_wheat_yield"] <- 50 # wrote 5.0 but looks like wrong units
crop_data[crop_data$id == 3590, "milling_wheat_average_yield"] <- 50 # wrote 5.0 but looks like wrong units

crop_data[crop_data$id == 1012, "milling_wheat_yield"] <- 51 # wrote 5.1 but looks like wrong units
crop_data[crop_data$id == 1012, "milling_wheat_average_yield"] <- 55 # wrote 5.5 but looks like wrong units

crop_data[crop_data$id == 1214, "milling_wheat_yield"] <- 52 # wrote 5.2 but looks like wrong units
crop_data[crop_data$id == 1214, "milling_wheat_average_yield"] <- 48 # wrote 4.8 but looks like wrong units

crop_data[crop_data$id == 1989, "milling_wheat_yield"] <- 57 # wrote 5.757 but looks like wrong units and double typed
crop_data[crop_data$id == 1989, "milling_wheat_average_yield"] <- 60 # wrote 6.0 but looks like wrong units

crop_data[crop_data$id == 1700, "milling_wheat_yield"] <- 58 # wrote 5.8 but looks like wrong units
crop_data[crop_data$id == 1700, "milling_wheat_average_yield"] <- 62 # wrote 6.2 but looks like wrong units

crop_data[crop_data$id == 137, "milling_wheat_yield"] <- 60 # wrote 6.0 but looks like wrong units 
crop_data[crop_data$id == 137, "milling_wheat_average_yield"] <- 55 # wrote 5.5 but looks like wrong units

crop_data[crop_data$id == 843, "milling_wheat_yield"] <- 60 # wrote 6.0 but looks like wrong units 
crop_data[crop_data$id == 843, "milling_wheat_average_yield"] <- 60 # wrote 6.0 but looks like wrong units

crop_data[crop_data$id == 932, "milling_wheat_yield"] <- 60 # wrote 6.0 but looks like wrong units 
crop_data[crop_data$id == 932, "milling_wheat_average_yield"] <- 60 # wrote 6.0 but looks like wrong units

crop_data[crop_data$id == 933, "milling_wheat_yield"] <- 60 # wrote 6.0 but looks like wrong units 
crop_data[crop_data$id == 933, "milling_wheat_average_yield"] <- 70 # wrote 7.0 but looks like wrong units

crop_data[crop_data$id == 1132, "milling_wheat_yield"] <- 60 # wrote 6.0 but looks like wrong units 
crop_data[crop_data$id == 1132, "milling_wheat_average_yield"] <- 65 # wrote 6.5 but looks like wrong units

crop_data[crop_data$id == 1578, "milling_wheat_yield"] <- 60 # wrote 6.0 but looks like wrong units 
crop_data[crop_data$id == 1578, "milling_wheat_average_yield"] <- 80 # wrote 8.0 but looks like wrong units

crop_data[crop_data$id == 1769, "milling_wheat_yield"] <- 60 # wrote 6.0 but looks like wrong units 
crop_data[crop_data$id == 1769, "milling_wheat_average_yield"] <- 60 # wrote 6.0 but looks like wrong units

crop_data[crop_data$id == 1830, "milling_wheat_yield"] <- 60 # wrote 6.0 but looks like wrong units 
crop_data[crop_data$id == 1830, "milling_wheat_average_yield"] <- 57 # wrote 5.7 but looks like wrong units

crop_data[crop_data$id == 2661, "milling_wheat_yield"] <- 60 # wrote 6.0 but looks like wrong units 
crop_data[crop_data$id == 2661, "milling_wheat_average_yield"] <- 60 # wrote 6.0 but looks like wrong units

crop_data[crop_data$id == 2789, "milling_wheat_yield"] <- 60 # wrote 6.0 but looks like wrong units 

crop_data[crop_data$id == 2891, "milling_wheat_yield"] <- 60 # wrote 6.0 but looks like wrong units 
crop_data[crop_data$id == 2891, "milling_wheat_average_yield"] <- 70 # wrote 7.0 but looks like wrong units

crop_data[crop_data$id == 3085, "milling_wheat_yield"] <- 60 # wrote 6.0 but looks like wrong units 
crop_data[crop_data$id == 3085, "milling_wheat_average_yield"] <- 60 # wrote 6.0 but looks like wrong units

crop_data[crop_data$id == 3190, "milling_wheat_yield"] <- 60 # wrote 6.0 but looks like wrong units 
crop_data[crop_data$id == 3190, "milling_wheat_average_yield"] <- 65 # wrote 6.5 but looks like wrong units

crop_data[crop_data$id == 2408, "milling_wheat_yield"] <- 62 # wrote 6.2 but looks like wrong units 
crop_data[crop_data$id == 2408, "milling_wheat_average_yield"] <- 64 # wrote 6.4 but looks like wrong units

crop_data[crop_data$id == 374, "milling_wheat_yield"] <- 65 # wrote 6.5 but looks like wrong units 
crop_data[crop_data$id == 374, "milling_wheat_average_yield"] <- 70 # wrote 7.0 but looks like wrong units

crop_data[crop_data$id == 2543, "milling_wheat_yield"] <- 65 # wrote 6.5 but looks like wrong units 
crop_data[crop_data$id == 2543, "milling_wheat_average_yield"] <- 60 # wrote 6.0 but looks like wrong units

crop_data[crop_data$id == 3405, "milling_wheat_yield"] <- 65 # wrote 6.5 but looks like wrong units 
crop_data[crop_data$id == 3405, "milling_wheat_average_yield"] <- 75 # wrote 7.5 but looks like wrong units

crop_data[crop_data$id == 427, "milling_wheat_yield"] <- 68 # wrote 6.8 but looks like wrong units 
crop_data[crop_data$id == 427, "milling_wheat_average_yield"] <- 72 # wrote 7.2 but looks like wrong units

crop_data[crop_data$id == 2967, "milling_wheat_yield"] <- 70 # wrote 7.0 but looks like wrong units 
crop_data[crop_data$id == 2967, "milling_wheat_average_yield"] <- 70 # wrote 7.0 but looks like wrong units

crop_data[crop_data$id == 1288, "milling_wheat_yield"] <- 75 # wrote 7.5 but looks like wrong units 
crop_data[crop_data$id == 1288, "milling_wheat_average_yield"] <- 80 # wrote 8.0 but looks like wrong units

crop_data[crop_data$id == 2947, "milling_wheat_yield"] <- 78 # wrote 7.8 but looks like wrong units 
crop_data[crop_data$id == 2947, "milling_wheat_average_yield"] <- 77 # wrote 7.7 but looks like wrong units

crop_data[crop_data$id == 1510, "milling_wheat_yield"] <- 80 # wrote 8.0 but looks like wrong units 
crop_data[crop_data$id == 1510, "milling_wheat_average_yield"] <- 70 # wrote 7.0 but looks like wrong units

crop_data[crop_data$id == 1910, "milling_wheat_yield"] <- 80 # wrote 8.0 but looks like wrong units 
crop_data[crop_data$id == 1910, "milling_wheat_average_yield"] <- 75 # wrote 7.5 but looks like wrong units

crop_data[crop_data$id == 1065, "milling_wheat_yield"] <- 90 # wrote 9.0 but looks like wrong units 
crop_data[crop_data$id == 1065, "milling_wheat_average_yield"] <- 80 # wrote 8.0 but looks like wrong units

crop_data[crop_data$id == 1065, "milling_wheat_yield"] <- 90 # wrote 9.0 but looks like wrong units 
crop_data[crop_data$id == 1065, "milling_wheat_average_yield"] <- 80 # wrote 8.0 but looks like wrong units

crop_data[crop_data$id == 2506, "milling_wheat_yield"] <- 100 # wrote 10.0 but looks like wrong units 
crop_data[crop_data$id == 2506, "milling_wheat_average_yield"] <- 100 # wrote 10.0 but looks like wrong units

crop_data[crop_data$id == 2140, "milling_wheat_yield"] <- 20 # wrote 200.0 but looks like wrong units 
crop_data[crop_data$id == 2140, "milling_wheat_average_yield"] <- 18 # wrote 180.0 but looks like wrong units

crop_data[crop_data$id == 2452, "milling_wheat_average_yield"] <- 20 # wrote 200.0 but looks implausible given previous answer for current yield

crop_data[crop_data$id == 692, "milling_wheat_average_yield"] <- 48 # wrote 4.8 but looks like wrong units

##########################################################################.####
## Editing magnitude of fertiliser where the numerical unit scale was misread####
##########################################################################.####

crop_data[crop_data$id == 1066, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 2506, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 301, "milling_wheat_synthetic_fert"] <- NA # wrote 500 but looks like wrote kg fertiliser (or max allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 675, "milling_wheat_synthetic_fert"] <- NA # wrote 500 but looks like wrote kg fertiliser (or max allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 765, "milling_wheat_synthetic_fert"] <- NA # wrote 500 but looks like wrote kg fertiliser (or max allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 948, "milling_wheat_synthetic_fert"] <- NA # wrote 500 but looks like wrote kg fertiliser (or max allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 949, "milling_wheat_synthetic_fert"] <- NA # wrote 500 but looks like wrote kg fertiliser (or max allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 1182, "milling_wheat_synthetic_fert"] <- NA # wrote 500 but looks like wrote kg fertiliser (or max allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 1219, "milling_wheat_synthetic_fert"] <- NA # wrote 500 but looks like wrote kg fertiliser (or max allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 2712, "milling_wheat_synthetic_fert"] <- NA # wrote 500 but looks like wrote kg fertiliser (or max allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 2765, "milling_wheat_synthetic_fert"] <- NA # wrote 500 but looks like wrote kg fertiliser (or max allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 2874, "milling_wheat_synthetic_fert"] <- NA # wrote 500 but looks like wrote kg fertiliser (or max allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 3332, "milling_wheat_synthetic_fert"] <- NA # wrote 500 but looks like wrote kg fertiliser (or max allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 3552, "milling_wheat_synthetic_fert"] <- NA # wrote 500 but looks like wrote kg fertiliser (or max allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 3206, "milling_wheat_synthetic_fert"] <- NA # wrote 485 but looks like wrote kg fertiliser (or just under max (97%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 1524, "milling_wheat_synthetic_fert"] <- NA # wrote 460 but looks like wrote kg fertiliser (or just under max (92%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 129, "milling_wheat_synthetic_fert"] <- NA # wrote 450 but looks like wrote kg fertiliser (or just under max (90%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 1030, "milling_wheat_synthetic_fert"] <- NA # wrote 450 but looks like wrote kg fertiliser (or just under max (90%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 1160, "milling_wheat_synthetic_fert"] <- NA # wrote 450 but looks like wrote kg fertiliser (or just under max (90%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 1755, "milling_wheat_synthetic_fert"] <- NA # wrote 450 but looks like wrote kg fertiliser (or just under max (90%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 2038, "milling_wheat_synthetic_fert"] <- NA # wrote 450 but looks like wrote kg fertiliser (or just under max (90%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 2592, "milling_wheat_synthetic_fert"] <- NA # wrote 450 but looks like wrote kg fertiliser (or just under max (90%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 2942, "milling_wheat_synthetic_fert"] <- NA # wrote 450 but looks like wrote kg fertiliser (or just under max (90%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 1558, "milling_wheat_synthetic_fert"] <- NA # wrote 420 but looks like wrote kg fertiliser (or just under max (84%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 3374, "milling_wheat_synthetic_fert"] <- NA # wrote 420 but looks like wrote kg fertiliser (or just under max (84%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 156, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 225, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 829, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)
crop_data[crop_data$id == 829, "milling_wheat_organic_fert"] <- NA # wrote 100 but looks like responded based on above by with a ratio (max 500 (400 + 100))

crop_data[crop_data$id == 834, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 1012, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)
crop_data[crop_data$id == 1012, "milling_wheat_organic_fert"] <- NA # wrote 100 but looks like responded based on above by with a ratio (max 500 (400 + 100))

crop_data[crop_data$id == 1123, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 1772, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 1811, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 1938, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 2109, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 2180, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 2385, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 2538, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 223, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 3232, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 3287, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 3313, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 3331, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)
crop_data[crop_data$id == 3331, "milling_wheat_organic_fert"] <- NA # wrote 100 but looks like responded based on above by with a ratio (max 500 (400 + 100))

crop_data[crop_data$id == 3508, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)
crop_data[crop_data$id == 3508, "milling_wheat_organic_fert"] <- NA # wrote 50 but looks like responded based on above by with a ratio of 90% as does reduced fertiliser scheme with 90% max the target (max 500 (400 + 50))

crop_data[crop_data$id == 3122, "milling_wheat_synthetic_fert"] <- NA # wrote 375 but looks like wrote kg fertiliser (or just under max (75%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)
crop_data[crop_data$id == 3122, "milling_wheat_organic_fert"] <- NA # wrote 125 but looks like responded based on above by with a ratio (max 500 (375 + 125))

crop_data[crop_data$id == 3258, "milling_wheat_synthetic_fert"] <- NA # wrote 350 but looks like wrote kg fertiliser (or just under max (70%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 3258, "milling_wheat_synthetic_fert"] <- NA # wrote 350 but looks like wrote kg fertiliser (or just under max (70%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

###########################################################################.####

crop_data[crop_data$id == 955, "milling_wheat_synthetic_fert"] <- 135 # wrote 1.35 but looks like wrote wrong magnitude
crop_data[crop_data$id == 955, "milling_wheat_organic_fert"] <- 120 # wrote 1.2 but looks like wrote wrong magnitude

crop_data[crop_data$id == 2555, "milling_wheat_synthetic_fert"] <- 150 # wrote 1.5 but looks like wrote wrong magnitude
crop_data[crop_data$id == 2555, "milling_wheat_organic_fert"] <- 100 # wrote 1.0 but looks like wrote wrong magnitude

crop_data[crop_data$id == 2191, "milling_wheat_synthetic_fert"] <- 50 # wrote 5.0 but looks like got magnitude wrong 

crop_data[crop_data$id == 2561, "milling_wheat_synthetic_fert"] <- 50 # wrote 5.0 but looks like wrote wrong magnitude 
crop_data[crop_data$id == 2561, "milling_wheat_organic_fert"] <- 100 # wrote 10.0 but looks like wrote wrong magnitude 

crop_data[crop_data$id == 281, "milling_wheat_synthetic_fert"] <- 55 # wrote 5.525 but looks like tried to calculate something which went wrong and duplicated answer from next box , this amount would make sense based on other answers

crop_data[crop_data$id == 797, "milling_wheat_synthetic_fert"] <- 60 # wrote 6.0 but looks like wrote wrong magnitude 
crop_data[crop_data$id == 797, "milling_wheat_organic_fert"] <- 30 # wrote 3.0 but looks like wrote wrong magnitude

###########################################################################.####

crop_data[crop_data$id == 3008, "milling_wheat_synthetic_fert"] <- NA # wrote 350 but looks like wrote kg fertiliser 
crop_data[crop_data$id == 3008, "milling_wheat_organic_fert"] <- NA # wrote 350 but looks like responded based on above by with a ratio (max 1000 (350 + 350))
# probably tried a ratio of 70% but also implausible answer for sowing density - drop this observation or impute

crop_data[crop_data$id == 3296, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (60%) allowable dose as 1000 was max on scale) and not N (ammonium nitrate = 27.5% N)
crop_data[crop_data$id == 3296, "milling_wheat_organic_fert"] <- NA # wrote 200 but looks like responded based on above by with a ratio (max 1000 (400 + 200))
# not certain of the error here - drop this observation or impute

crop_data[crop_data$id == 2943, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (60%) allowable dose as 1000 was max on scale) and not N (ammonium nitrate = 27.5% N)
crop_data[crop_data$id == 2943, "milling_wheat_organic_fert"] <- NA # wrote 200 but looks like responded based on above by with a ratio (max 1000 (400 + 200))
# not certain of the error here - drop this observation or impute

crop_data[crop_data$id == 2312, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (60%) allowable dose as 1000 was max on scale) and not N (ammonium nitrate = 27.5% N)
crop_data[crop_data$id == 2312, "milling_wheat_organic_fert"] <- NA # wrote 500 but looks like responded based on above by with a ratio (max 1000 (400 + 500))
# not certain of the error here - drop this observation or impute

crop_data[crop_data$id == 1943, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (60%) allowable dose as 1000 was max on scale) and not N (ammonium nitrate = 27.5% N)
crop_data[crop_data$id == 1943, "milling_wheat_organic_fert"] <- NA # wrote 200 but looks like responded based on above by with a ratio (max 1000 (400 + 200))
# not certain of the error here - drop this observation or impute

crop_data[crop_data$id == 1534, "milling_wheat_synthetic_fert"] <- NA # wrote 400 but looks like wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)
crop_data[crop_data$id == 1534, "milling_wheat_organic_fert"] <- NA # wrote 140 but looks like responded based on above by with a ratio (max 540 (400 + 140))

crop_data[crop_data$id == 3207, "milling_wheat_synthetic_fert"] <- NA # wrote 3 but looks like number of treatments

crop_data[crop_data$id == 1578, "milling_wheat_synthetic_fert"] <- NA # wrote 1 but looks like number of treatments
crop_data[crop_data$id == 1578, "milling_wheat_organic_fert"] <- NA # wrote 2 but looks like number of treatments

crop_data[crop_data$id == 3230, "milling_wheat_synthetic_fert"] <- NA # wrote 1 but looks like wrote wrong magnitude
crop_data[crop_data$id == 3230, "milling_wheat_organic_fert"] <- NA # wrote 1 but looks like wrote wrong magnitude

crop_data[crop_data$id == 2245, "milling_wheat_synthetic_fert"] <- NA # wrote 1 but looks like wrote wrong magnitude

crop_data[crop_data$id == 1990, "milling_wheat_synthetic_fert"] <- NA # wrote 1 but looks like wrote wrong magnitude
crop_data[crop_data$id == 1990, "milling_wheat_organic_fert"] <- NA # wrote 2 but looks like wrote wrong magnitude

crop_data[crop_data$id == 534, "milling_wheat_synthetic_fert"] <- NA # wrote 1.5 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 534, "milling_wheat_organic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here - drop this observation or impute

crop_data[crop_data$id == 2861, "milling_wheat_synthetic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude

crop_data[crop_data$id == 2774, "milling_wheat_synthetic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 2774, "milling_wheat_organic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here - drop this observation or impute

crop_data[crop_data$id == 554, "milling_wheat_synthetic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 554, "milling_wheat_organic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here - drop this observation or impute

crop_data[crop_data$id == 260, "milling_wheat_synthetic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 260, "milling_wheat_organic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here and sowing density looks implausible at 70 - drop this observation or impute

crop_data[crop_data$id == 151, "milling_wheat_synthetic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 151, "milling_wheat_organic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here  - drop this observation or impute

crop_data[crop_data$id == 3142, "milling_wheat_synthetic_fert"] <- NA # wrote 2.1 but looks like wrote wrong magnitude 

crop_data[crop_data$id == 2718, "milling_wheat_synthetic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 2718, "milling_wheat_organic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here  - drop this observation or impute

crop_data[crop_data$id == 2439, "milling_wheat_synthetic_fert"] <- NA # wrote 2.5 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 2439, "milling_wheat_organic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here  - drop this observation or impute

crop_data[crop_data$id == 2439, "milling_wheat_synthetic_fert"] <- NA # wrote 2.5 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 2439, "milling_wheat_organic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here  - drop this observation or impute

crop_data[crop_data$id == 577, "milling_wheat_synthetic_fert"] <- NA # wrote 3.0 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 577, "milling_wheat_organic_fert"] <- NA # wrote 32.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here but implausible either way - drop this observation or impute

crop_data[crop_data$id == 1818, "milling_wheat_synthetic_fert"] <- NA # wrote 3.0 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 1818, "milling_wheat_organic_fert"] <- NA # wrote 1.5 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here maybe a ratio - drop this observation or impute

crop_data[crop_data$id == 2201, "milling_wheat_synthetic_fert"] <- NA # wrote 3.0 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 2201, "milling_wheat_organic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here maybe a ratio - drop this observation or impute

crop_data[crop_data$id == 3268, "milling_wheat_synthetic_fert"] <- NA # wrote 3.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here maybe a ratio or fertiliser kg and not N - drop this observation or impute

crop_data[crop_data$id == 1114, "milling_wheat_synthetic_fert"] <- NA # wrote 4.0 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 1114, "milling_wheat_organic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here maybe a ratio - drop this observation or impute

crop_data[crop_data$id == 1114, "milling_wheat_synthetic_fert"] <- NA # wrote 4.0 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 1114, "milling_wheat_organic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here maybe a ratio - drop this observation or impute

crop_data[crop_data$id == 1730, "milling_wheat_synthetic_fert"] <- NA # wrote 4.0 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 1730, "milling_wheat_organic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here, maybe a ratio, sowing density also implausible - drop this observation or impute

crop_data[crop_data$id == 1927, "milling_wheat_synthetic_fert"] <- NA # wrote 4.0 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 1927, "milling_wheat_organic_fert"] <- NA # wrote 3.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here, maybe a ratio - drop this observation or impute

crop_data[crop_data$id == 2054, "milling_wheat_synthetic_fert"] <- NA # wrote 4 but looks like wrote  got units wrong and wrote kg fertiliser (or just under max (80%) allowable dose as 500 was max on scale) and not N (ammonium nitrate = 27.5% N)

crop_data[crop_data$id == 3174, "milling_wheat_synthetic_fert"] <- NA # wrote 4.0 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 3174, "milling_wheat_organic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here, maybe a ratio, sowing density also implausible - drop this observation or impute

crop_data[crop_data$id == 3260, "milling_wheat_synthetic_fert"] <- NA # wrote 4.0 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 3260, "milling_wheat_organic_fert"] <- NA # wrote 1.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here, maybe a ratio - drop this observation or impute

crop_data[crop_data$id == 1444, "milling_wheat_synthetic_fert"] <- NA # wrote 5.0 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 1444, "milling_wheat_organic_fert"] <- NA # wrote 3.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here, maybe a ratio - drop this observation or impute

crop_data[crop_data$id == 3428, "milling_wheat_synthetic_fert"] <- NA # wrote 5.0 but looks like wrote wrong magnitude or number of treatments
crop_data[crop_data$id == 3428, "milling_wheat_organic_fert"] <- NA # wrote 2.0 but looks like wrote wrong magnitude or number of treatments
# not certain of the error here, maybe a ratio - drop this observation or impute

crop_data[crop_data$id == 570, "milling_wheat_organic_fert"] <- NA # wrote 500 but not plausible 
# not certain of the error here - drop this observation or impute

crop_data[crop_data$id == 2655, "milling_wheat_synthetic_fert"] <- NA # wrote 300 but looks like wrote wrong magnitude or miscalculation
crop_data[crop_data$id == 2655, "milling_wheat_organic_fert"] <- NA # wrote 500 but looks like wrote wrong magnitude or miscalculation
# not certain of the error here, maybe a ratio, sowing density also implausible - drop this observation or impute

crop_data[crop_data$id == 583, "milling_wheat_synthetic_fert"] <- NA # wrote 300 but looks like wrote wrong magnitude or miscalculation
crop_data[crop_data$id == 583, "milling_wheat_organic_fert"] <- NA # wrote 400 but looks like wrote wrong magnitude or miscalculation
# not certain of the error here, maybe a ratio - drop this observation or impute

crop_data[crop_data$id == 605, "milling_wheat_organic_fert"] <- NA # wrote 350 but looks like wrote wrong magnitude or miscalculation
# not certain of the error here, maybe a ratio - drop this observation or impute

crop_data[crop_data$id == 2697, "milling_wheat_organic_fert"] <- NA # wrote 350 but looks like wrote wrong magnitude or miscalculation
# not certain of the error here, maybe a ratio - drop this observation or impute

crop_data[crop_data$id == 701, "milling_wheat_synthetic_fert"] <- NA # wrote 200 but looks like wrote wrong magnitude or miscalculation
crop_data[crop_data$id == 701, "milling_wheat_organic_fert"] <- NA # wrote 300 but looks like wrote wrong magnitude or miscalculation
# not certain of the error here, maybe a ratio - drop this observation or impute

crop_data[crop_data$id == 3001, "milling_wheat_organic_fert"] <- NA # wrote 300 but looks like wrote wrong magnitude given other answers

crop_data[crop_data$id == 1255, "milling_wheat_organic_fert"] <- NA # wrote 250 but looks like wrote wrong magnitude given other answers

crop_data[crop_data$id == 1646, "milling_wheat_organic_fert"] <- NA # wrote 1 but looks like wrote wrong magnitude or used number of treatments
# not certain of the error here - drop this observation or impute

crop_data[crop_data$id == 1922, "milling_wheat_organic_fert"] <- NA # wrote 1 but looks like wrote wrong magnitude or used number of treatments
# not certain of the error here - drop this observation or impute

crop_data[crop_data$id == 2776, "milling_wheat_organic_fert"] <- NA # wrote 1 but looks like wrote wrong magnitude or used number of treatments
# not certain of the error here - drop this observation or impute

##########################################################################.####
## Editing magnitude of fertiliser (numerical unit scale was misread)      ####
##########################################################################.####

crop_data[crop_data$id == 910, "milling_wheat_sowing_density"] <- NA # wrote 0 but not possible

crop_data[crop_data$id == 1141, "milling_wheat_sowing_density"] <- NA # wrote 0 but not possible

crop_data <- crop_data %>% # respondees who put 1 or 2 most likely put dt/ha rather than kg/ha
  dplyr::mutate(milling_wheat_sowing_density = ifelse(milling_wheat_sowing_density == 1 | milling_wheat_sowing_density == 2, milling_wheat_sowing_density*100, milling_wheat_sowing_density))

crop_data <- crop_data %>% # respondees who put above 280 used seed density / m2 rather than kg/ha - re calculated assuming TSW 50g (half of seeds/m2 number)
  dplyr::mutate(milling_wheat_sowing_density = ifelse(milling_wheat_sowing_density >= 300, milling_wheat_sowing_density/2, milling_wheat_sowing_density))

crop_data[crop_data$id == 3001, "milling_wheat_sowing_density"] <- 180 # wrote 1.8 but looks like wrong magnitude

crop_data[crop_data$id == 1670, "milling_wheat_sowing_density"] <- 160 # wrote 16.0 but looks like wrong magnitude

crop_data[crop_data$id == 1365, "milling_wheat_sowing_density"] <- 220 # wrote 2206060, looks like the answers for the previous 2 boxes also typed in this one

crop_data[crop_data$id == 3567, "milling_wheat_sowing_density"] <- 190 # wrote 190350, looks like the equivalent seeds/m2 also typed in this one

crop_data[crop_data$id == 3520, "milling_wheat_sowing_density"] <- 180 # wrote 180120, looks like the answers for the next box also typed in this one

crop_data[crop_data$id == 3222, "milling_wheat_sowing_density"] <- 155 # wrote 155120, looks like the answers for the next box also typed in this one

crop_data[crop_data$id == 2556, "milling_wheat_sowing_density"] <- 180 # wrote 18090, looks like the answers for the next box also typed in this one

crop_data[crop_data$id == 1248, "milling_wheat_sowing_density"] <- 180 # wrote 1801, looks like the answers for the next box also typed in this one

crop_data[crop_data$id == 916, "milling_wheat_sowing_density"] <- 180 # wrote 1800, looks like the answers for the next box also typed in this one

##########################################################################.####
## Checking if total fertiliser amounts still look spurious                ####
##########################################################################.####

crop_data <- crop_data %>% 
  dplyr::mutate(milling_wheat_total_fert = milling_wheat_synthetic_fert + milling_wheat_organic_fert)

### If total fertiliser usage is still way too high to be correct (>= 300) setting both to NA
crop_data <- crop_data %>% 
  dplyr::mutate(milling_wheat_synthetic_fert = ifelse(milling_wheat_total_fert >= 300, NA, milling_wheat_synthetic_fert),
                milling_wheat_organic_fert = ifelse(milling_wheat_total_fert >= 300, NA, milling_wheat_organic_fert),
                milling_wheat_total_fert_recalc = milling_wheat_synthetic_fert + milling_wheat_organic_fert
  )

### Removing wheat data (there will be two separate datasets)
clean_data <- clean_data %>%
  subset(select = -c(milling_wheat_yield, milling_wheat_average_yield, milling_wheat_synthetic_fert, milling_wheat_organic_fert, milling_wheat_sowing_density, milling_wheat_biostimulator_treats, milling_wheat_herbicide_treats, milling_wheat_fungicide_treats, milling_wheat_insecticide_treats, milling_wheat_pgr_treats)) 

##########################################################################.####
##########################################################################.####
# Cleaning Wheat Production Dataset - Merging                              ####
##########################################################################.####
##########################################################################.####

### merging preliminary cultivation methods into one
crop_data <- crop_data %>%
  dplyr::mutate(milling_wheat_ip_suisse = ifelse(milling_wheat_ip_suisse == "Ja", "IP-Suisse", 
                                                 ifelse(milling_wheat_ip_suisse == "Nein", "ÖLN", 
                                                        ifelse(is.na(milling_wheat_ip_suisse), NA, NA))),
                
                milling_wheat_demeter = ifelse(milling_wheat_demeter == "Ja", "Demeter", 
                                               ifelse(milling_wheat_demeter == "Nein", "Bio-Suisse", 
                                                      ifelse(is.na(milling_wheat_demeter), NA, NA))),
                
                milling_wheat_production_standard = pmap_chr(list(milling_wheat_ip_suisse, milling_wheat_demeter), ~ paste(na.omit(c(...)), collapse = " ")),
                milling_wheat_production_standard = ifelse(milling_wheat_production_standard == "", NA, milling_wheat_production_standard)) %>%
  
  # Above block creates a single variable that merges the information about production standards for wheat production to enable the creation of a factor variable below
  
  # Dropping superfluous variables to make clear which is the cleaned variable
  subset(select = -c(milling_wheat_ip_suisse, milling_wheat_demeter, milling_wheat_total_fert)) %>%
  
  # Converting character variables to numeric to enable their inclusion in the models
  dplyr::mutate(milling_wheat_biostimulator_treats = as.numeric(milling_wheat_biostimulator_treats),
                milling_wheat_herbicide_treats = as.numeric(milling_wheat_herbicide_treats),
                milling_wheat_fungicide_treats = as.numeric(milling_wheat_fungicide_treats),
                milling_wheat_insecticide_treats = as.numeric(milling_wheat_insecticide_treats),
                milling_wheat_pgr_treats = as.numeric(milling_wheat_pgr_treats)
  )

# Making the factor variable for the production standards of wheat production
crop_data$milling_wheat_production_standard <- factor(crop_data$milling_wheat_production_standard, levels = c("ÖLN", "IP-Suisse", "Bio-Suisse", "Demeter")) 

saveRDS(crop_data, file = "../crop_data.rds")

##########################################################################.####
##########################################################################.####
# Cleaning Farmer Characteristics                                          ####
##########################################################################.####
##########################################################################.####
## Changing ages and experience when wrong (not in YYYY format)            ####

##########################################################################.####
### Age to birth year                                                      ####
# RULE APPLIED: 65 years of age maximum year allowed to still manage farm (recieve direct subsidies in switzerland)
# If greater than 65 written safe to assume that the fomat written was YY rather than YYYY
# Cross check this with year took over farm and whether also in YY or YYYY format

clean_data[clean_data$id == 88, "born"] <- 1974 # from 1074
clean_data[clean_data$id == 172, "born"] <- 1983 # from 1083
clean_data[clean_data$id == 274, "born"] <- 1984 # from 84 (wrote YY)
clean_data[clean_data$id == 439, "born"] <- 1976 # from 1876
clean_data[clean_data$id == 584, "born"] <- 1964 # from 64 (wrote YY)
clean_data[clean_data$id == 659, "born"] <- 1972 # from 72 (wrote YY)
clean_data[clean_data$id == 926, "born"] <- 1964 # from 64 (wrote YY)
clean_data[clean_data$id == 1056, "born"] <- 1979 # from 79 (wrote YY)
clean_data[clean_data$id == 1084, "born"] <- 1977 # from 77 (wrote YY)
clean_data[clean_data$id == 1550, "born"] <- 1983 # from 83 (wrote YY)

clean_data[clean_data$id == 1646, "born"] <- 1973 # from 73 (wrote YY)
clean_data[clean_data$id == 1757, "born"] <- 1972 # from 72 (wrote YY)
clean_data[clean_data$id == 1785, "born"] <- 1967 # from 67 (wrote YY)
clean_data[clean_data$id == 1786, "born"] <- 1968 # from 68 (wrote YY)
clean_data[clean_data$id == 1910, "born"] <- 1977 # from 1377
clean_data[clean_data$id == 2278, "born"] <- 1986 # from 86 (wrote YY)
clean_data[clean_data$id == 2298, "born"] <- 1975 # from 75 (wrote YY)
clean_data[clean_data$id == 2338, "born"] <- 1991 # from 91 (wrote YY)
clean_data[clean_data$id == 2613, "born"] <- 1970 # from 70 (wrote YY)

clean_data[clean_data$id == 2634, "born"] <- 1969 # from 69 (wrote YY)
clean_data[clean_data$id == 2737, "born"] <- 1968 # from 68 (wrote YY)
clean_data[clean_data$id == 2951, "born"] <- 1975 # from 75 (wrote YY)
clean_data[clean_data$id == 3042, "born"] <- 1982 # from 82 (wrote YY)
clean_data[clean_data$id == 3077, "born"] <- 1968 # from 68 (wrote YY)
clean_data[clean_data$id == 3078, "born"] <- 1975 # from 75 (wrote YY)
clean_data[clean_data$id == 3222, "born"] <- 1967 # from 67 (wrote YY)
clean_data[clean_data$id == 3325, "born"] <- 1967 # from 67 (wrote YY)
clean_data[clean_data$id == 3445, "born"] <- 1967 # from 67 (wrote YY)

clean_data[clean_data$id == 3467, "born"] <- 1981 # from 81 (wrote YY)

clean_data[clean_data$id == 3419, "born"] <- NA # from 1900 
clean_data[clean_data$id == 2530, "born"] <- NA # from 0 - did not write

##########################################################################.####
### Experience years to farm take over year                                ####

clean_data[clean_data$id == 2634, "management_start"] <- 1996 # from 96 (wrote YY)
clean_data[clean_data$id == 2737, "management_start"] <- 1998 # from 98 (wrote YY)
clean_data[clean_data$id == 3222, "management_start"] <- 1994 # from 94 (wrote YY)

clean_data[clean_data$id == 1912, "management_start"] <- NA # from 2026 (potentially meant 2006 but not certain, respondent born 1980)
clean_data[clean_data$id == 3077, "management_start"] <- NA # from 2099 (most likely meant 2009, respondent born 1968)


##########################################################################.####
# Report SAK correctly                                                     ####                                  

clean_data[clean_data$id == 1950, "sak"] <- 2.03 # wrote 203 but looks like wrong magnitude as would be 6.1 sak/ha
clean_data[clean_data$id == 3329, "sak"] <- 4 # wrote 40 but looks like wrong magnitude as would be 3.9 sak/ha
clean_data[clean_data$id == 1788, "sak"] <- 4.8 # wrote 48 but looks like wrong magnitude as would be 3.7 sak/ha
clean_data[clean_data$id == 3137, "sak"] <- 9.2 # wrote 92 but looks like wrong magnitude as would be 2.67 sak/ha
clean_data[clean_data$id == 711, "sak"] <- 7 # wrote 70 but looks like wrong magnitude as would be 2.6 sak/ha
clean_data[clean_data$id == 1621, "sak"] <- 5 # wrote 50 but looks like wrong magnitude as would be 2.46 sak/ha

clean_data[clean_data$id == 3273, "sak"] <- NA # wrote 0 but not possible
clean_data[clean_data$id == 3258, "sak"] <- NA # wrote 0 but not possible
clean_data[clean_data$id == 726, "sak"] <- NA # wrote 0 but not possible
clean_data[clean_data$id == 2458, "sak"] <- NA # wrote 0 but not possible

# Farmer id 2162 has a lot of wrong values, write wrong values as NA
clean_data[clean_data$id == 3258, c("organic_scheme", "tillage_scheme", "wider_row_scheme", "protection_strip_scheme", "kantonal_scheme_soil")] <- ""

##########################################################################.####
## Make new variables (for grouping or correction)                         ####
##########################################################################.####

cleaned_pre_data <- clean_data %>%
  dplyr::mutate(age = 2024 - born, # age respondent is at survey time
                management_exper = 2024 - management_start, # years of management experience at time of survey
                management_exper = ifelse(is.na(management_exper), 0, ifelse(management_exper < 0, 0, management_exper)), # cannot have minus 1 years of experience (for those few who took over 2024)
                cultivation_missing = ifelse(is.na(cultivation_work), 1, 0),
                seedbed_missing = ifelse(is.na(seedbed_work), 1, 0),
                psm_missing = ifelse(is.na(plant_protection_work), 1, 0),
                org_fert_missing = ifelse(is.na(organic_fertiliser_work), 1, 0),
                synth_fert_missing = ifelse(is.na(synthetic_fertiliser_work), 1, 0),
                count_missing_field_work = cultivation_missing + seedbed_missing + psm_missing + org_fert_missing + synth_fert_missing)


###########################################################################.####
###########################################################################.####
# Regroup Variables (Anonymity reasons)                                     ####
###########################################################################.####
###########################################################################.####

###########################################################################.####
### Group ages                                                              ####
cleaned_pre_data <- cleaned_pre_data %>%
  mutate(age_group = case_when(
    age >= 23 & age <= 32 ~ "23 to 32", # Number of farmers: 246
    age >= 33 & age <= 42 ~ "33 to 42", # Number of farmers: 662
    age >= 43 & age <= 52 ~ "43 to 52", # Number of farmers: 807
    age >= 53 & age <= 62 ~ "53 to 62", # Number of farmers: 874
    age >= 63 & age <= 84 ~ "63 to 84", # Number of farmers: 148
    TRUE ~ NA_character_  
  ))

cleaned_pre_data$age_group<-as.factor(cleaned_pre_data$age_group)

###########################################################################.####
### Group experience                                                        ####
cleaned_pre_data <- cleaned_pre_data %>%
  mutate(years_experience_group = case_when(
    management_exper >= 0 & management_exper <= 5 ~ "0 to 5", # Number of farmers:
    management_exper >= 6 & management_exper <= 15 ~ "6 to 15", # Number of farmers:
    management_exper >= 16 & management_exper <= 25 ~ "16 to 25", # Number of farmers:
    management_exper >= 26 & management_exper <= 35 ~ "26 to 35", # Number of farmers:
    management_exper >= 36 & management_exper <= 53 ~ "36 to 53", # Number of farmers:
    TRUE ~ NA_character_  
  )) 

cleaned_pre_data$years_experience_group<-as.factor(cleaned_pre_data$years_experience_group)
str(cleaned_pre_data$years_experience_group)

###########################################################################.####
### Arable land areas (less specific)                                       ####
cleaned_pre_data <- cleaned_pre_data %>%
  mutate(arable_area_group = case_when(
    arable_area < 1 ~ "Less than 1 ha",                      
    arable_area >= 1 & arable_area < 3 ~ "1 to less than 3 ha",  
    arable_area >= 3 & arable_area < 5 ~ "3 to less than 5 ha",  
    arable_area >= 5 & arable_area < 10 ~ "5 to less than 10 ha", 
    arable_area >= 10 & arable_area < 20 ~ "10 to less than 20 ha", 
    arable_area >= 20 & arable_area < 30 ~ "20 to less than 30 ha", 
    arable_area >= 30 & arable_area < 50 ~ "30 to less than 50 ha", 
    arable_area >= 50 ~ "50 ha and more",  # 50 ha and more
    TRUE ~ NA_character_  # In case of any missing or invalid data
  ))

cleaned_pre_data$arable_area_group<-as.factor(cleaned_pre_data$arable_area_group)
# str(cleaned_pre_data$arable_area_group)

##########################################################################.####
##########################################################################.####
# Drop "protest" answers and answers not completed before page 8       ####
##########################################################################.####
##########################################################################.####
cleaned_pre_data <- cleaned_pre_data %>%
  filter(id != 2394) %>% # All answers the same 
  filter(lastpage > 8) # keeping only answers that filled out all pages up until page 8 (Q. about farm structure)

##########################################################################.####
##########################################################################.####
# Cleaning Practice Data                                                   ####
##########################################################################.####
##########################################################################.####

### Filling NAs as zeros for practice usage where the question asking if ever used was No - i.e. true zeros
practices <- c("strip_till", "mulch_till", "zero_till", "contour_till", "subsoiling", "ctf", "mulching", "undersow", "cover_crop", "biochar", "compost", "soil_condition_test")

for (p in practices) {
  x <- paste0(p,"_use")
  print(x)
  
  cleaned_data[[paste(x)]] <- ifelse(is.na(cleaned_data[[paste(x)]]), 0,  cleaned_data[[paste(x)]])
  
  x <- paste0(p,"_freq_used")
  print(x)
  
  cleaned_data[[paste(x)]] <- ifelse(is.na(cleaned_data[[paste(x)]]), 0,  cleaned_data[[paste(x)]])
  
}

##########################################################################.####
##########################################################################.####
# Class definition (only several variables, rest follows below)            ####
##########################################################################.####
##########################################################################.####
# Save previous data of data cleaning in new df 
cleaned_data_raw<-cleaned_data

value_mapping_yes_no <- c(
  "Ja" = "Yes", 
  "Nein" = "No"
)

# Ersetze die Werte in den angegebenen Variablen und wandle sie in Faktoren um
employ_vars <- c("employ_family", "employ_non_family", "employ_seasonal", 
                 "employ_trainee", "employ_no_other")


# Ersetze die Werte und wandle sie in Faktoren um
library(dplyr)
for (var in employ_vars) {
  cleaned_data_raw[[var]] <- factor(
    dplyr::recode(cleaned_data_raw[[var]], "1" = "Yes", "0" = "No"),
    levels = c("Yes", "No")
  )
}

##########################################################################.####
##########################################################################.####
# Correct values or rename                                                 ####
##########################################################################.####
##########################################################################.####

table(cleaned_data_raw$ag_educ)

cleaned_data_raw <- cleaned_data_raw %>%
  dplyr::mutate(ag_educ = ifelse(ag_educ == "Keine" | ag_educ == "Direktzahlungskurs", "Minimum Required Qualifications", 
                                 ifelse(ag_educ == "Agrarpraktiker*in EBA" | ag_educ == "Landwirt*in EFZ" , "Agricultural Apprenticeship",
                                        ifelse(ag_educ == "Landwirtschaftliche Handelsschule" | ag_educ == "Höhere Fachhochschule (Agrartechniker)", "Agricultural Technical Diploma",
                                               ifelse(ag_educ == "Landwirtschaftliche Betriebsleiterschule / Meisterprüfung" , "Agricultural Managerial Qualification", 
                                                      ifelse(ag_educ == "Fachhochschule (BSc/MSc)" | ag_educ == "Universität oder ETH/EPF (BSc/MSc/PhD)", "Agricultural Academic Qualification", "Missing"))))))

table(cleaned_data_raw$ag_educ)


###########################################################################.####
###########################################################################.####
# Translations to English                                                   ####
###########################################################################.####
###########################################################################.####
cleaned_engl_data<-cleaned_pre_data

cleaned_engl_data <- cleaned_engl_data %>%
  dplyr::mutate(across(everything(), as.character)) %>%
  
  # Translate Ja/Nein to English
  mutate(across(everything(), ~ ifelse(. == "Nein", "No", 
                                       ifelse(. == "Ja", "Yes", 
                                              ifelse(. == "N/A", "Not Answered", .))))) %>%
  
  # "De" to English
  mutate(startlanguage = ifelse(startlanguage == "de", "ge", startlanguage)) %>%
  
  # Gender
  mutate(gender = ifelse(gender == "männlich", "Male", 
                         ifelse(gender == "Weiblich", "Female", gender))) %>%
  
  # Scales
  mutate(across(everything(), ~ ifelse(. == "Sehr niedrig", "Very low", 
                                       ifelse(. == "Niedrig", "Low", 
                                              ifelse(. == "Mittel", "Medium", 
                                                     ifelse(. == "Hoch", "High", 
                                                            ifelse(. == "Umfassend", "Very high", .))))))) %>%
  
  # Problems
  mutate(across(everything(), ~ ifelse(. == "Kein Problem", "No problem",
                                       ifelse(. == "Geringes Problem", "Low problem",
                                              ifelse(. == "Mässiges Problem", "Medium problem",
                                                     ifelse(. == "Ziemliches Problem", "Somewhat large problem",
                                                            ifelse(. == "Grosses Problem", "Large Problem", .))))))) %>%
  
  # Priorities
  mutate(across(everything(), ~ ifelse(. == "Keine Priorität", "Not a priority",
                                       ifelse(. == "Geringe Priorität", "Low priority",
                                              ifelse(. == "Mittlere Priorität", "Moderate priority",
                                                     ifelse(. == "Hohe Priorität", "High priority",
                                                            ifelse(. == "Oberste Priorität", "Top priority", .))))))) %>%
  
  # Soilprofile
  mutate(soil_assessment = ifelse(tolower(trimws(soil_assessment)) == "keine bewertung des boden mittels bodenprofil und kein bodenbewirtschaftungsplan vorhanden", 
                                  "No soilprofile and soil management plan",
                                  ifelse(tolower(trimws(soil_assessment)) == "boden mittels bodenprofil bewertet", 
                                         "Soilprofile", 
                                         ifelse(tolower(trimws(soil_assessment)) == "boden mittels bodenprofil bewertet und bodenbewirtschaftungsplan vorhanden", 
                                                "Soilprofile and soil management plan", 
                                                soil_assessment))))%>%
  
  # Haupterwerb and Nebenerwerb
  mutate(farm_business_category = ifelse(farm_business_category == "Haupterwerb", "Main_income",
                                         ifelse(farm_business_category == "Nebenerwerb", "Side_income", farm_business_category)))%>%
  
  # Succession
  mutate(succession = ifelse(succession == "Noch nicht relevant", "Not relevant yet", succession)) %>%
  
  # Translation of frequency
  mutate(across(everything(), ~ ifelse(. == "Nie", "Never",
                                       ifelse(. == "1 oder 2 Mal", "Once or twice",
                                              ifelse(. == "3 Mal", "Three times",
                                                     ifelse(. == "4 oder 5 Mal", "Four or five times",
                                                            ifelse(. == "Mehr als 5 Mal", "More than five times", .))))))) %>%
  
  # Agreement levels
  mutate(across(everything(), ~ ifelse(. == "Stimmt überhaupt nicht zu", "Strongly disagree",
                                       ifelse(. == "Stimme nicht zu", "Disagree",
                                              ifelse(. == "Stimme weder zu noch ab", "Neither agree/disagree",
                                                     ifelse(. == "Stimme zu", "Agree",
                                                            ifelse(. == "Stimme voll und ganz zu", "Strongly agree", .))))))) %>%
  
  # Field work
  mutate(across(everything(), ~ ifelse(. == "Die Arbeiten werden von einem landwirtschaftlichen Angestellten oder von mir selbst ausgeführt", "Self",
                                       ifelse(. == "Ich beauftrage Lohnunternehmen, die diese Aufgabe übernehmen", "Contractor",
                                              ifelse(. == "", "Not Answered", .)))))


##########################################################################.####
### Rename cantons English                                                 ####
cleaned_engl_data <- cleaned_engl_data %>%
  mutate(
    canton = case_when(
      kanton == "Aargau" ~ "Aargau",
      kanton == "Appenzell Ausserrhoden" ~ "Appenzell Outer Rhodes",
      kanton == "Appenzell Innerrhoden" ~ "Appenzell Inner Rhodes",
      kanton == "Basel-Landschaft" ~ "Basel-Landschaft",
      kanton == "Basel-Stadt" ~ "Basel City",
      kanton == "Bern" ~ "Bern",
      kanton == "Freiburg" ~ "Fribourg",
      kanton == "Genf" ~ "Geneva",
      kanton == "Glarus" ~ "Glarus",
      kanton == "Graubünden" ~ "Grisons",
      kanton == "Jura" ~ "Jura",
      kanton == "Luzern" ~ "Lucerne",
      kanton == "Neuenburg" ~ "Neuchâtel",
      kanton == "Nidwalden" ~ "Nidwalden",
      kanton == "Obwalden" ~ "Obwalden",
      kanton == "Schaffhausen" ~ "Schaffhausen",
      kanton == "Solothurn" ~ "Solothurn",
      kanton == "St. Gallen" ~ "St. Gallen",
      kanton == "Tessin" ~ "Ticino",
      kanton == "Thurgau" ~ "Thurgau",
      kanton == "Uri" ~ "Uri",
      kanton == "Waadt" ~ "Vaud",
      kanton == "Wallis" ~ "Valais",
      kanton == "Zug" ~ "Zug",
      kanton == "Zürich" ~ "Zurich",
      TRUE ~ kanton  # Falls kein Kanton passt, behält er den ursprünglichen Namen
    )
  )

cleaned_engl_data$canton<-as.factor(cleaned_engl_data$canton)

##########################################################################.####
##########################################################################.####
# Define order and class                                                   ####
##########################################################################.####
##########################################################################.####

## Correct the levels of factors                                          #### 
# (eg. 1= very low / 5=high)
## Correct the levels of factors ####

table(cleaned_engl_data$biochar_know)

prefix_levels <- list(
  # Knowledge
  `_know`   = c("Very low", "Low", "Medium", "Very high", "High"),
  
  # Problems
  `problem_`  = c("No problem", "Low problem", "Medium problem",
                  "Somewhat large problem", "Large problem"),
  
  # Priorities
  `priority_` = c("Not a priority", "Low priority", "Moderate priority", "High priority", "Top priority"),
  
  # Soilprofile
  `profile`  = c("No soilprofile and soil management plan",
                 "Soilprofile",
                 "Soilprofile and soil management plan"),
  
  # Succession
  `succession` = c("Not relevant yet", 
                   "No",
                   "Yes"),
  
  # Training & Advice
  `advice_` = c("Never",
                "Once or twice",
                "Three times",
                "Four or five times",
                "More than five times"),
  
  # Training & Advice
  `consult_` = c("Never",
                 "Once or twice",
                 "Three times",
                 "Four or five times",
                 "More than five times"),
  
  # Training & Advice
  `participate_` = c("Never",
                     "Once or twice",
                     "Three times",
                     "Four or five times",
                     "More than five times"),
  
  #Attitude
  `belief_` = c ("Strongly disagree",
                 "Disagree",
                 "Neither agree/disagree",
                 "Agree",
                 "Strongly agree"),
  
  #Soil assessment
  `soil_assessment` = c ("No soilprofile and soil management plan",
                         "Soilprofile",
                         "Soilprofile and soil management plan"),
  
  # Education
  `ag_educ` = c ("Minimum Required Qualifications", "Agricultural Apprenticeship", 
                 "Agricultural Technical Diploma", "Agricultural Managerial Qualification", 
                 "Agricultural Academic Qualification"),
  
  #scheme
  `_scheme` = c ("Not Answered",
                 "No",
                 "Yes"),
  
  #succession
  `succession` = c ("Not relevant yet",
                    "No",
                    "Yes"),
  
  #Gender
  `gender` = c ("Not Answered",
                "Male",
                "Female"))

# Apply factor ordering dynamically
cleaned_engl_data <- cleaned_engl_data %>%
  mutate(across(
    where(is.character),
    ~ {
      colname <- cur_column()
      matched_key <- names(prefix_levels)[which(sapply(names(prefix_levels), function(p) str_detect(colname, fixed(p))))]
      if (length(matched_key) > 0) {
        factor(.x, levels = prefix_levels[[matched_key[1]]], ordered = TRUE)
      } else {
        .x
      }
    }
  ))

table(cleaned_engl_data$biochar_know)
table(cleaned_engl_data$advice_private_service)


##########################################################################.####
## Correct order of focus                                                  ####
# Make numeric and make correct order
cleaned_engl_data$focus_arable <- as.numeric(as.character(cleaned_engl_data$focus_arable))
table(sort(cleaned_engl_data$focus_arable))

cleaned_engl_data$focus_livestock <- as.numeric(as.character(cleaned_engl_data$focus_livestock))
table(sort(cleaned_engl_data$focus_livestock))

cleaned_engl_data$focus_permanent_crops <- as.numeric(as.character(cleaned_engl_data$focus_permanent_crops))
table(sort(cleaned_engl_data$focus_permanent_crops))

cleaned_engl_data$focus_other_enterprises <- as.numeric(as.character(cleaned_engl_data$focus_other_enterprises))
table(sort(cleaned_engl_data$focus_other_enterprises))

##########################################################################.####
## Correct order of straw kept                                             ####
# Make numeric and make correct order
cleaned_engl_data$prop_straw_kept <- as.numeric(as.character(cleaned_engl_data$prop_straw_kept))
table(sort(cleaned_engl_data$prop_straw_kept))


##########################################################################.####
## correct order of years_exp._group                                       ####
cleaned_engl_data$years_experience_group <- factor(
  cleaned_engl_data$years_experience_group,
  levels = c("0 to 5", "6 to 15", "16 to 25", "26 to 35", "36 to 53"),
  ordered = TRUE
)

table(cleaned_engl_data$years_experience_group)

##########################################################################.####
### Define correct class                                                   ####                                                 
columns_as_factors <- c(
  "age_group", "years_experience_group", "canton", "arable_area_group",
  "startlanguage", "farm_business_category", "focus_arable", "focus_livestock",
  "focus_permanent_crops", "focus_other_enterprises", "prop_straw_kept",
  "employ_family", "succession", "soil_assessment"
)

columns_as_binary <- c(
  "none_scheme","gender", "kantonal_scheme_none", "strip_till_in_network",
  "mulch_till_in_network", "zero_till_in_network", "contour_till_in_network",
  "subsoiling_in_network", "ctf_in_network", "mulching_in_network",
  "undersow_in_network", "cover_crop_in_network", "biochar_in_network",
  "compost_in_network", "soil_condition_test_in_network", "strip_till_ever_used",
  "mulch_till_ever_used", "zero_till_ever_used", "contour_till_ever_used",
  "subsoiling_ever_used", "ctf_ever_used", "mulching_ever_used",
  "undersow_ever_used", "cover_crop_ever_used", "biochar_ever_used",
  "compost_ever_used", "soil_condition_test_ever_used"
)

columns_as_numeric <- c(
  "strip_till_freq_used","sak", "mulch_till_freq_used", "zero_till_freq_used",
  "contour_till_freq_used", "subsoiling_freq_used", "ctf_freq_used",
  "mulching_freq_used", "undersow_freq_used", "cover_crop_freq_used",
  "biochar_freq_used", "compost_freq_used", "soil_condition_test_freq_used",
  "arable_area_share_rent"
)

# Convert specified columns
# Convert to factors
for (col in columns_as_factors) {
  if (col %in% colnames(cleaned_engl_data)) {
    cleaned_engl_data[[col]] <- as.factor(cleaned_engl_data[[col]])
  }
}

# Convert to binary factors (keep "Yes"/"No")
for (col in columns_as_binary) {
  if (col %in% colnames(cleaned_engl_data)) {
    cleaned_engl_data[[col]] <- as.factor(cleaned_engl_data[[col]]) # Retain original "Yes"/"No" as factors
  }
}

# Convert to numeric
for (col in columns_as_numeric) {
  if (col %in% colnames(cleaned_engl_data)) {
    cleaned_engl_data[[col]] <- as.numeric(cleaned_engl_data[[col]])
  }
} 

str(cleaned_engl_data$biochar_know)

##########################################################################.####
### Reclassify Knowledge of Practices                                      ####
cleaned_data_reclassify<-cleaned_engl_data

knowlegde <- c(
  "strip_till_know", 
  "mulch_till_know", 
  "zero_till_know", 
  "contour_till_know", 
  "subsoiling_know", 
  "ctf_know", 
  "mulching_know", 
  "undersow_know", 
  "cover_crop_know", 
  "biochar_know", 
  "compost_know", 
  "soil_condition_test_know"
)

for (col in knowlegde) {
  cleaned_data_reclassify[[col]] <- as.factor(cleaned_data_reclassify[[col]])
}

levels(cleaned_data_reclassify$biochar_know)

##########################################################################.####
### Reclassify fieldwork carried out by contractors                        ####
fieldwork <- c("cultivation_work", "seedbed_work", "plant_protection_work", "organic_fertiliser_work", "synthetic_fertiliser_work")

for (col in fieldwork) {
  cleaned_data_reclassify[[col]] <- as.factor(cleaned_data_reclassify[[col]])
}


# str(cleaned_data_reclassify$plant_protection_work)

##########################################################################.####
### Reclassify schemes                                                     ####                                         

scheme <- c(
  "organic_scheme", 
  "soil_cover_scheme", 
  "tillage_scheme", 
  "herbicide_scheme", 
  "pesticide_scheme", 
  "fertiliser_scheme", 
  "wider_row_scheme", 
  "protection_strip_scheme", 
  "prec_application_schem"
)

# Apply as.factor() to each column in the list
for (col in scheme) {
  cleaned_data_reclassify[[col]] <- as.factor(cleaned_data_reclassify[[col]])
}

# Test if worked or still OK
# str(cleaned_data_reclassify$tillage_scheme) # should be factor

##########################################################################.####
### Reclassify cantonal schemes                                            ####

kantonal_scheme<-c(
  "kantonal_scheme_soil",
  "kantonal_scheme_inputs",
  "kantonal_scheme_investment"
)

# Make factors
for (col in kantonal_scheme) {
  cleaned_data_reclassify[[col]] <- as.factor(cleaned_data_reclassify[[col]])
}

# Test if worked
# str(cleaned_data_reclassify$tillage_scheme)

##########################################################################.####
### Reclassify problems                                                    ####

problem<- c(
  "problem_soil_erosion",
  "problem_standing_water",
  "problem_soil_condition",
  "problem_rut_formation",
  "problem_machine_availability",
  "problem_input_constraints",
  "problem_uneven_establishment",
  "problem_arable_diseases",
  "problem_arable_weeds",
  "problem_arable_pests",
  "problem_stressed_crops",
  "problem_lodged_crops",
  "problem_low_yield",
  "problem_crop_quality"
)

for (col in problem) {
  cleaned_data_reclassify[[col]] <- as.factor(cleaned_data_reclassify[[col]])
}

# Test if worked
# str(cleaned_data_reclassify$problem_stressed_crops)

##########################################################################.####
### Reclassify advice & consultation                                       ####
advice <- c("advice_private_service", "advice_farm_trader", "advice_public_extension", "consult_farmers", "consult_social_media", "consult_press", "participate_equip_demo", "participate_discuss_group", "participate_farm_demo", "participate_course")

for (col in advice) {
  cleaned_data_reclassify[[col]] <- as.factor(cleaned_data_reclassify[[col]])
}

# Test if worked
# str(cleaned_data_reclassify$advice_private_service)

##########################################################################.####
### Reclassify risks                                                       ####

pref_risk <- c("pref_risk_ag_production", "pref_risk_invest_tech", "pref_risk_crop_input","pref_time_invest")

for (col in pref_risk) {
  cleaned_data_reclassify[[col]] <- as.factor(cleaned_data_reclassify[[col]])
}

# Recode the values
cleaned_data_reclassify <- cleaned_data_reclassify %>%
  mutate(across(all_of(pref_risk), ~ ifelse(. == "0(Überhaupt nicht bereit)", "0",
                                            ifelse(. == "10(Sehr bereitwillig)", "10", .))))


# make numeric
for (col in pref_risk) {
  cleaned_data_reclassify[[col]] <- as.numeric(cleaned_data_reclassify[[col]])
}

#Test
summary(cleaned_data_reclassify$pref_risk_ag_production)

##########################################################################.####
### Reclassify Practices                                                   ####
usage <- c(
  "strip_till_use",
  "mulch_till_use",
  "zero_till_use",
  "contour_till_use",
  "subsoiling_use",
  "ctf_use",
  "mulching_use",
  "undersow_use",
  "cover_crop_use",
  "biochar_use",
  "compost_use",
  "soil_condition_test_use")

for (var in usage) {
  cleaned_data_reclassify[[var]] <- as.numeric(as.character(cleaned_data_reclassify[[var]]))
}

str(cleaned_data_reclassify$ctf_use)

##########################################################################.####
### Reclassify Belief                                                      ####
belief <- c("belief_solution", "belief_goal_achievement", "belief_efficacy", "belief_goal_setting")

for (col in belief) {
  cleaned_data_reclassify[[col]] <- as.factor(cleaned_data_reclassify[[col]])
}

# str(cleaned_data_reclassify$belief_goal_achievement)

##########################################################################.####
### Reclassify Wheat                                                       ####

# milling_wheat_grow as factor
cleaned_data_reclassify$milling_wheat_grow<-as.factor(cleaned_data_reclassify$milling_wheat_grow)

# milling_wheat_ip_suisse
cleaned_data_reclassify$milling_wheat_ip_suisse<-as.factor(cleaned_data_reclassify$milling_wheat_ip_suisse)

# milling_wheat_demeter
cleaned_data_reclassify$milling_wheat_demeter<-as.factor(cleaned_data_reclassify$milling_wheat_demeter)

# Milling wheat area
cleaned_data_reclassify$milling_wheat_area<-as.numeric(cleaned_data_reclassify$milling_wheat_area)

##########################################################################.####
### Reclassify Priority                                                    ####
priority <- c("priority_max_yields", "priority_min_costs", "priority_min_labour_req", 
              "priority_min_risk", "priority_min_weed_pest_disease", "priority_adapt_to_weather", 
              "priority_adapt_to_farmland", "priority_improve_soil", "priority_improve_biodiversity", 
              "priority_reduce_enviro_impact", "priority_expand", "priority_adapt_to_market", 
              "priority_adapt_to_legislation", "priority_seek_professional_advice", 
              "priority_seek_casual_advice", "priority_seek_peer_approval")

# Create loop to change to factors
for (col in priority) {
  cleaned_data_reclassify[[col]] <- as.factor(cleaned_data_reclassify[[col]])
}

###########################################################################.####
###########################################################################.####
# Remove Variables (Anonimity reasons or relevance)                         ####
###########################################################################.####
###########################################################################.####
cleaned_rm_data<-cleaned_data_reclassify

# Remove information that must be less detailed or were grouped into other variable
cleaned_rm_data <- cleaned_rm_data %>%
  dplyr::select(-born,
                -age,
                -management_start,
                -management_exper,
                -ag_educ,
                -kanton,
                -arable_area)

# Remove private information
cleaned_rm_data <- cleaned_rm_data %>%
  dplyr::select(-farm_id,
                -surname,
                -email,
                -form_address,
                -form_address_name)


# Remove info that allow tracking or are not important
cleaned_rm_data <- cleaned_rm_data %>%
  dplyr::select(-seed,
                -token,
                -startdate,
                -datestamp,
                -ipaddr,
                -refurl,
                -plz)

# Remove all Time information as well as participation benchmark etc
cleaned_rm_data <- cleaned_rm_data %>%
  dplyr::select(-contains("Time"), 
                -submitdate, 
                -lastpage, 
                -blocked_email,
                -sendout_batch)

# Remove regional projects
cleaned_rm_data <- cleaned_rm_data %>%
  dplyr::select(-starts_with("regional_project"))

# Remove info about gezogen/zapfwellbetrieben
cleaned_rm_data <- cleaned_rm_data %>%
  dplyr::select(-pem_cultivation_method,
                -prem_cultivation_pto_tool,
                -prem_cultivation_pto_tool_other,
                -prem_cultivation_pulled_tool,
                -prem_cultivation_pulled_tool_other,
                -seedbed_method,
                -drilling_method)

# Remove extra info management (want to continue with survey)
cleaned_rm_data <- cleaned_rm_data %>%
  dplyr::select(-management_involved)%>%
  dplyr::select(-no_management)


##########################################################################.####
##########################################################################.####
# Finalize dataset                                                         ####
##########################################################################.####
##########################################################################.####

##########################################################################.####
## Select variables                                                        ####
##########################################################################.####

names(cleaned_rm_data)
# These Survey IDs do not have secondary data, therefore remove from dataset
missing_ids <- c(84, 134, 267, 562, 878, 1572, 1577, 1690, 1928, 1944, 3587)
cleaned_rm_data <- cleaned_rm_data %>% filter(!(id %in% missing_ids))

cleaned_rm_data$survey_id <- cleaned_rm_data$id

data_finished <- cleaned_rm_data %>%
  dplyr::select(survey_id,
                startlanguage,
                age_group,
                gender,
                years_experience_group,
                sak,
                
                # Schemes
                organic_scheme,
                soil_cover_scheme,
                tillage_scheme,
                herbicide_scheme,
                pesticide_scheme,
                fertiliser_scheme,
                wider_row_scheme,
                protection_strip_scheme,
                prec_application_schem,
                none_scheme,
                kantonal_scheme_soil,
                kantonal_scheme_inputs,
                kantonal_scheme_investment,
                kantonal_scheme_none,
                
                # Knowledge 
                strip_till_know,
                mulch_till_know,
                zero_till_know,
                contour_till_know,
                subsoiling_know,
                ctf_know,
                mulching_know,
                undersow_know,
                cover_crop_know,
                biochar_know,
                compost_know,
                soil_condition_test_know,
                
                # Network
                strip_till_in_network,
                mulch_till_in_network,
                zero_till_in_network,
                contour_till_in_network,
                subsoiling_in_network,
                ctf_in_network,
                mulching_in_network,
                undersow_in_network,
                cover_crop_in_network,
                biochar_in_network,
                compost_in_network,
                soil_condition_test_in_network,
                
                # Usage Y/N
                strip_till_ever_used,
                mulch_till_ever_used,
                zero_till_ever_used,
                contour_till_ever_used,
                subsoiling_ever_used,
                ctf_ever_used,
                mulching_ever_used,
                undersow_ever_used,
                cover_crop_ever_used,
                biochar_ever_used,
                compost_ever_used,
                soil_condition_test_ever_used,
                
                # Usage on parcel
                strip_till_use,
                mulch_till_use,
                zero_till_use,
                contour_till_use,
                subsoiling_use,
                ctf_use,
                mulching_use,
                undersow_use,
                cover_crop_use,
                biochar_use,
                compost_use,
                soil_condition_test_use,
                
                # Usage freq.
                strip_till_freq_used,
                mulch_till_freq_used,
                zero_till_freq_used,
                contour_till_freq_used,
                subsoiling_freq_used,
                ctf_freq_used,
                mulching_freq_used,
                undersow_freq_used,
                cover_crop_freq_used,
                biochar_freq_used,
                compost_freq_used,
                soil_condition_test_freq_used,
                
                # Wheat Business and Focus
                milling_wheat_grow,
                milling_wheat_area,
                arable_area_share_rent,
                farm_business_category,
                focus_arable,
                focus_livestock,
                focus_permanent_crops,
                focus_other_enterprises,
                prop_straw_kept,
                employ_family,
                employ_non_family,
                employ_seasonal,
                employ_trainee,
                succession,
                soil_assessment,
                
                # Advice and Consultation
                advice_private_service,
                advice_farm_trader,
                advice_public_extension,
                consult_farmers,
                consult_social_media,
                consult_press,
                participate_equip_demo,
                participate_discuss_group,
                participate_farm_demo,
                participate_course,
                
                # Preferences and Beliefs
                pref_risk_ag_production,
                pref_risk_invest_tech,
                pref_risk_crop_input,
                belief_solution,
                belief_goal_achievement,
                belief_efficacy,
                belief_goal_setting,
                
                # Problems
                problem_soil_erosion,
                problem_standing_water,
                problem_soil_condition,
                problem_rut_formation,
                problem_machine_availability,
                problem_input_constraints,
                problem_uneven_establishment,
                problem_arable_diseases,
                problem_arable_weeds,
                problem_arable_pests,
                problem_stressed_crops,
                problem_lodged_crops,
                problem_low_yield,
                problem_crop_quality,
                
                
                # Priorities
                priority_max_yields,
                priority_min_costs,
                priority_min_labour_req,
                priority_min_risk,
                priority_min_weed_pest_disease,
                priority_adapt_to_weather,
                priority_adapt_to_farmland,
                priority_improve_soil,
                priority_improve_biodiversity,
                priority_reduce_enviro_impact,
                priority_expand,
                priority_adapt_to_market,
                priority_adapt_to_legislation,
                priority_seek_professional_advice,
                priority_seek_casual_advice,
                priority_seek_peer_approval)




# Check if worked
variable_i<-tibble(variable = names(data_finished), type = sapply(data_finished, class))
print(variable_i, n=400)

##########################################################################.####
## Add Labels                                                              ####
##########################################################################.####
data_finished <- data_finished %>%
  labelled::set_variable_labels(
    survey_id = "Individual survey ID",
    startlanguage = "Language",
    age_group = "Age [Group]",
    gender = "Gender",
    years_experience_group = "Years of farming experience [Group]",
    sak = "Full-time equivalents [FTE]",
    ag_edu = "Level of agricultural education [Factor]",
    
    # Schemes
    organic_scheme = "Participates in federal subsidy scheme for organic farming [Y/N]",
    soil_cover_scheme = " Participates in federal subsidy scheme for soil coverage [Y/N]",
    tillage_scheme = "Participates in federal subsidy scheme for reduced tillage [Y/N]",
    herbicide_scheme = "Participates in federal subsidy scheme for no herbicides [Y/N]",
    pesticide_scheme = "Participates in federal subsidy scheme for no pesticides [Y/N]",
    fertiliser_scheme = "Participates in federal subsidy scheme for efficient nitrogen usage [Y/N]",
    wider_row_scheme = "Participates in federal subsidy scheme for wider rows in cereals [Y/N]",
    protection_strip_scheme = "Participates in federal subsidy scheme for beneficial protection strips [Y/N]",
    prec_application_schem = "Participates in federal subsidy scheme for precision application technologies [Y/N]",
    none_scheme = "No federal subsidy", # Can we remove this one from the dataset as it's not definitely true - just not participates in the ones we asked
    kantonal_scheme_soil = "Participates in cantonal subsidy scheme targeting improved soil [Y/N]",
    kantonal_scheme_inputs = "Participates in Cantonal subsidy scheme targeting reduced inputs [Y/N]",
    kantonal_scheme_investment = "Participates in Cantonal subsidy scheme targeting investment in machinery [Y/N]",
    kantonal_scheme_none = "No cantonal subsidy", # Can we remove this one from the dataset too as it's not definitely true - just not participates in the ones we asked
    
    # Knowledge
    strip_till_know = "Percieved practical knowledge of strip tillage [Factor]",
    mulch_till_know = "Percieved practical knowledge of mulch tillage [Factor]",
    zero_till_know = "Percieved practical knowledge of zero tillage [Factor]",
    contour_till_know = "Percieved practical knowledge of contour farming [Factor]",
    subsoiling_know = "Percieved practical knowledge of deep non-inversion tillage [Factor]",
    ctf_know = "Percieved practical knowledge of controlled traffic farming [Factor]",
    mulching_know = "Percieved practical knowledge of mechanical mulching [Factor]",
    undersow_know = "Percieved practical knowledge of undersowing [Factor]",
    cover_crop_know = "Percieved practical knowledge of cover crops [Factor]",
    biochar_know = "Percieved practical knowledge of biochar application [Factor]",
    compost_know = "Percieved practical knowledge of compost application [Factor]",
    soil_condition_test_know = "Percieved practical knowledge of soil condition testing [Factor]",
    
    # Network
    strip_till_in_network = "Network adoption of strip tillage [Y/N]",
    mulch_till_in_network = "Network adoption of mulch tillage [Y/N]",
    zero_till_in_network = "Network adoption of zero tillage [Y/N]",
    contour_till_in_network = "Network adoption of contour farming [Y/N]",
    subsoiling_in_network = "Network adoption of deep non-inversion tillage [Y/N]",
    ctf_in_network = "Network adoption of controlled traffic farming [Y/N]",
    mulching_in_network = "Network adoption of mechanical mulching [Y/N]",
    undersow_in_network = "Network adoption of undersowing [Y/N]",
    cover_crop_in_network = "Network adoption of cover crops [Y/N]",
    biochar_in_network = "Network adoption of biochar application [Y/N]",
    compost_in_network = "Network adoption of compost application [Y/N]",
    soil_condition_test_in_network = "Network adoption of soil condition testing [Y/N]",
    
    # Ever used
    strip_till_ever_used = "Ever adopted strip tillage [Y/N]",
    mulch_till_ever_used = "Ever adopted mulch tillage [Y/N]",
    zero_till_ever_used = "Ever adopted zero tillage [Y/N]",
    contour_till_ever_used = "Ever adopted contour farming [Y/N]",
    subsoiling_ever_used = "Ever adopted deep non-inversion tillage [Y/N]",
    ctf_ever_used = "Ever adopted controlled traffic farming [Y/N]",
    mulching_ever_used = "Ever adopted mechanical mulching [Y/N]",
    undersow_ever_used = "Ever adopted undersowing [Y/N]",
    cover_crop_ever_used = "Ever adopted cover crops [Y/N]",
    biochar_ever_used = "Ever adopted biochar application [Y/N]",
    compost_ever_used = "Ever adopted compost application [Y/N]",
    soil_condition_test_ever_used = "Ever adopted soil condition testing [Y/N]",
    
    # Usage on parcel
    strip_till_use = "Extent of strip tillage usage on arable land in 22/23 [%]",
    mulch_till_use = "Extent of mulch tillage usage on arable land in 22/23 [%]",
    zero_till_use = "Extent of zero tillage usage on arable land in 22/23 [%]",
    contour_till_use = "Extent of contour farming usage on arable land in 22/23 [%]",
    subsoiling_use = "Extent of deep non-inversion tillage usage on arable land in 22/23 [%]",
    ctf_use = "Extent of controlled traffic farming usage on arable land in 22/23 [%]",
    mulching_use = "Extent of mechanical mulching usage on arable land in 22/23 [%]",
    undersow_use = "Extent of undersowing usage on arable land in 22/23 [%]",
    cover_crop_use = "Extent of cover crops usage on arable land in 22/23 [%]",
    biochar_use = "Extent of biochar application usage on arable land in 22/23 [%]",
    compost_use = "Extent of compost application usage on arable land in 22/23 [%]",
    soil_condition_test_use = "Extent of soil condition testing usage on arable land in 22/23 [%]",
    
    # Usage frequency
    strip_till_freq_used = "Number of years strip tillage used in last 10 [No./10 years]",
    mulch_till_freq_used = "Number of years mulch tillage used in last 10 [No./10 years]",
    zero_till_freq_used = "Number of years zero tillage used in last 10 [No./10 years]",
    contour_till_freq_used = "Number of years contour farming used in last 10 [No./10 years]",
    subsoiling_freq_used = "Number of years deep non-inversion tillage used in last 10 [No./10 years]",
    ctf_freq_used = "Number of years controlled traffic farming used in last 10 [No./10 years]",
    mulching_freq_used = "Number of years mechanical mulching used in last 10 [No./10 years]",
    undersow_freq_used = "Number of years undersowing used in last 10 [No./10 years]",
    cover_crop_freq_used = "Number of years cover crops used in last 10 [No./10 years]",
    biochar_freq_used = "Number of years biochar application used in last 10 [No./10 years]",
    compost_freq_used = "Number of years compost application used in last 10 [No./10 years]",
    soil_condition_test_freq_used = "Number of years soil condition testing  used in last 10 [No./10 years]",
    
    # Farm characteristics
    milling_wheat_grow = "Grows milling wheat [Y/N]",
    milling_wheat_area = "Self-reported area under milling wheat [ha]",
    arable_area_share_rent = "Share of arable area rented [%]",
    farm_business_category = "Farm business category [Factor]",
    focus_arable = "Farm focus on arable crops [%]",
    focus_livestock = "Farm focus on livestock [%]",
    focus_permanent_crops = "Farm focus on permanent crops [%]",
    focus_other_enterprises = "Farm focus on other enterprises [%]",
    prop_straw_kept = "Proportion of straw kept on farm [%]",
    employ_family = "Employs family members [Y/N]",
    employ_non_family = "Employs non-family members [Y/N]",
    employ_seasonal = "Employs seasonal workers [Y/N]",
    employ_trainee = "Employs trainees [Y/N]",
    succession = "Succession plan in place [Factor]",
    soil_assessment = "Performs soil assessments [Factor]",
    
    # Advice and Consultation
    advice_private_service = "Advice frequency private service providers [Factor]",
    advice_farm_trader = "Advice frequency farm traders [Factor]",
    advice_public_extension = "Advice frequency public extension services [Factor]",
    consult_farmers = "Consult frequency other farmers for farming advice [Factor]",
    consult_social_media = "Consult frequency social media for farming advice [Factor]",
    consult_press = "Consult frequency farming press for farming advice [Factor]",
    participate_equip_demo = "Participation frequency equipment demonstrations [Factor]",
    participate_discuss_group = "Participation frequency discussion groups [Factor]",
    participate_farm_demo = "Participation frequency farm demonstration events [Factor]",
    participate_course = "Participation frequency agricultural courses [Factor]",
    
    # Preferences and Beliefs
    pref_risk_ag_production = "Willingness to take risks in agricultural production [0-10]",
    pref_risk_invest_tech = "Willingness to take risks with technology investments [0-10]",
    pref_risk_crop_input = "Willingness to take risks with crop input use [0-10]",
    belief_solution = "Confidence in finding a solution when difficulties encountered [Factor]",
    belief_goal_achievement = "Confidence in reaching production goals [Factor]",
    belief_efficacy = "Conviction that weather has most influence on farming results [Factor]",
    belief_goal_setting = "Conviction that self-set production goals are ambitious [Factor]",
    
    # Problems
    problem_soil_erosion = "Experienced loss or erosion of topsoil [Factor]",
    problem_standing_water ="Experienced standing water on fields long after rain [Factor]",
    problem_soil_condition = "Experienced sub-optimal soil conditions for the upcoming work in the field [Factor]",
    problem_rut_formation = "Experienced formation of deep wheel ruts in field during fieldwork [Factor]",
    problem_machine_availability = "Experienced lack of availability of required machines at the right time [Factor]",
    problem_input_constraints = "Experienced time or labour constraints needed to perform additional measures [Factor]",
    problem_uneven_establishment = "Experienced uneven crop establishment in early growing season [Factor]", 
    problem_arable_diseases = "Experienced infestation with crop diseases or soil born diseases [Factor]", 
    problem_arable_weeds = "Experienced infestation of arable weeds [Factor]", 
    problem_arable_pests = "Experienced infestation of arable pests [Factor]", 
    problem_stressed_crops ="Experienced stressed or stunted crops following dry periods [Factor]", 
    problem_lodged_crops = "Experienced lodging of crops [Factor]", 
    problem_low_yield = "Experienced lower crop yield than desirable [Factor]", 
    problem_crop_quality = "Experienced crops being downgraded at sale due to not meeting quality requirements [Factor]", 
    
    # Priorities
    priority_max_yields = "Priority level attributed to achieving maximum yields [Factor]",
    priority_min_costs = "Priority level attributed to minimising costs [Factor]",
    priority_min_labour_req = "Priority level attributed to minimising labour requirements [Factor]",
    priority_min_risk = "Priority level attributed to reducing risks [Factor]",
    priority_min_weed_pest_disease = "Priority level attributed to minimising weeds, pests, and diseases [Factor]",
    priority_adapt_to_weather = "Priority level attributed to adapting to weather variability [Factor]",
    priority_adapt_to_farmland = "Priority level attributed to adapting practices to farmland conditions [Factor]",
    priority_improve_soil = "Priority level attributed to improving soil quality [Factor]",
    priority_improve_biodiversity = "Priority level attributed to improving biodiversity [Factor]",
    priority_reduce_enviro_impact = "Priority level attributed to reducing environmental impact [Factor]",
    priority_expand = "Priority level attributed to expanding the farm business [Factor]",
    priority_adapt_to_market = "Priority level attributed to adapting to market developments [Factor]",
    priority_adapt_to_legislation = "Priority level attributed to adapting to legal and policy changes [Factor]",
    priority_seek_professional_advice = "Priority level attributed to seeking professional advice [Factor]",
    priority_seek_casual_advice = "Priority level attributed to seeking informal advice [Factor]",
    priority_seek_peer_approval = "Priority level attributed to aligning with peers’ expectations [Factor]"
  )

##########################################################################.####
## Save Dataset                                                            ####
##########################################################################.####

saveRDS(data_finished, file = "../Dataset_survey.rds")

write_xlsx(data_finished, path = "../Dataset_survey.xlsx")
.