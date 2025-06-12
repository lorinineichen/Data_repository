###########################################################################.####
###########################################################################.####
# Script for Cleaning Secondary Data for Data article 
###########################################################################.####
# Authour: Charles Rees (FiBL and ETH Zuerich) and Lorin Ineichen (FiBL)
###########################################################################.####
###########################################################################.####


###########################################################################.####
# Library                                                                   ####
###########################################################################.####
# install.packages("devtools")
# devtools::install_github("brad-cannell/codebookr")

library(codebookr)
library(dplyr, warn.conflicts = FALSE)
library(haven)
library(plyr)
library(writexl)
library(sjlabelled)

rm(list = ls())

###########################################################################.####
###########################################################################.####
# Loading Secondary Data                                                    ####
###########################################################################.####
###########################################################################.####

### File paths are blinded 

field_data <- readRDS("RAW_FILE_PATH_REDACTED")

weather_data <- readRDS("RAW_FILE_PATH_REDACTED")

secondary_data <- merge(field_data, weather_data, by.x = c("survey_id", "lastpage"), by.y = c("survey_id", "lastpage"), all = TRUE)

rm(field_data, weather_data)

###########################################################################.####
###########################################################################.####
# Reducing Data Granularity By Manually Rounding                            ####
###########################################################################.####
###########################################################################.####

secondary_data <- secondary_data %>%
  filter(survey_id != 2394) %>% # All answers the same 
    filter(lastpage > 8) %>%
  dplyr::select(-lastpage)

secondary_data_rounded <- secondary_data %>%
  dplyr::mutate(total_land_area = round_any(total_land_area, 5, f = ceiling),
                total_arable_area = round_any(total_arable_area, 5, f = ceiling),
                total_perm_grass_area = round_any(total_perm_grass_area, 5, f = ceiling),
                
                total_land_area = ifelse(total_land_area >= 100, 100, total_land_area),
                total_arable_area = ifelse(total_land_area >= 100, 100, total_arable_area),
                total_perm_grass_area = ifelse(total_land_area >= 100, 100, total_perm_grass_area),
                
                perc_arable_beets = round_any(perc_arable_beets, 5),
                perc_arable_potatoes = round_any(perc_arable_potatoes, 5),
                perc_arable_maize = round_any(perc_arable_maize, 5),
                perc_arable_temp_grass = round_any(perc_arable_temp_grass, 5),
                perc_arable_bff = round_any(perc_arable_bff, 5),
                perc_arable_barley = round_any(perc_arable_barley, 5),
                perc_arable_wheat = round_any(perc_arable_wheat, 5),
                
                perc_arable_other_cereals = round_any(perc_arable_other_cereals, 5),
                perc_arable_osr = round_any(perc_arable_osr, 5),
                perc_arable_other_oilseeds = round_any(perc_arable_other_oilseeds, 5),
                perc_arable_legumes = round_any(perc_arable_legumes, 5),
                perc_arable_veg_fruits = round_any(perc_arable_veg_fruits, 5),
                
                total_wheat_area = round_any(total_wheat_area, 1, f = ceiling),
                total_wheat_area = ifelse(total_wheat_area >= 30, 30, total_wheat_area),
                avreage_clay_content_wheat = round_any(avreage_clay_content_wheat, 5),
                avreage_sand_content_wheat = round_any(avreage_sand_content_wheat, 5),
                avreage_silt_content_wheat = round_any(avreage_silt_content_wheat, 5),
                average_altitude_wheat = round_any(average_altitude_wheat, 50),
                average_slope_angle_wheat = round_any(average_slope_angle_wheat, 1)
                
                )
  

### Looping Temperature Variables  ####
temp_vars <- c("average_temp_2022_09", "average_temp_2022_10", "average_temp_2022_11", "average_temp_2022_12", "average_temp_2023_01", "average_temp_2023_02", "average_temp_2023_03", "average_temp_2023_04", "average_temp_2023_05", "average_temp_2023_06", "average_temp_2023_07", "average_temp_2023_08")   

for (t in temp_vars) {
 
  secondary_data_rounded[[paste(t)]] <- round_any(secondary_data_rounded[[paste(t)]], 1)
  
  }


### Looping Precipitation Variables  ####
prec_vars <- c("total_prec_2022_09", "total_prec_2022_10", "total_prec_2022_11", "total_prec_2022_12", "total_prec_2023_01", "total_prec_2023_02", "total_prec_2023_03", "total_prec_2023_04", "total_prec_2023_05", "total_prec_2023_06", "total_prec_2023_07", "total_prec_2023_08")

for (p in prec_vars) {
  
  secondary_data_rounded[[paste(p)]] <- round_any(secondary_data_rounded[[paste(p)]], 10)
  
}


###########################################################################.####
###########################################################################.####
# Adding Variable Labels                                                    ####
###########################################################################.####
###########################################################################.####
secondary_data_rounded <- secondary_data_rounded %>%
  
  labelled::set_variable_labels(
    
    kanton_regisered_farm_site = "Canton of farm registration [Canton]",
    agricultural_zone_farm_site = "Agricultural zone of farm [Factor]",
    
    number_plots_total = "Number of plots registered to farm [No.]",
    total_land_area = "Total land registered to farm [ha]",
    number_plots_arable = "Number of arable plots registered to farm [No.]",
    total_arable_area = "Total arable land registered to farm [ha]",
    number_plots_perm_grass = "Number of permanent grassland plots registered to farm [No.]",
    total_perm_grass_area = "Total permanent grassland registered to farm [ha]",
    
    perc_arable_beets = "Share of arable land producing beets [%]",
    perc_arable_potatoes = "Share of arable land producing potatoes [%]",
    perc_arable_maize = "Share of arable land producing maize [%]",
    perc_arable_temp_grass = "Share of arable land under temporary grass [%]",
    perc_arable_bff = "Share of arable land under biodiversity protection [%]",
    perc_arable_barley = "Share of arable land producing barley [%]",
    perc_arable_wheat = "Share of arable land producing wheat [%]",
    perc_arable_other_cereals = "Share of arable land producing other cereals [%]",
    perc_arable_osr = "Share of arable land producing oilseed rape [%]",
    perc_arable_other_oilseeds = "Share of arable land producing other oilseeds [%]",
    perc_arable_legumes = "Share of arable land producing legumes [%]",
    perc_arable_veg_fruits = "Share of arable land producing field veg/berries [%]",
    
    number_wheat_plots_total = "Number of milling wheat plots registered to farm [No.]",
    total_wheat_area = "Total milling wheat land registered to farm [ha]",
    avreage_clay_content_wheat = "Average soil clay content - milling wheat fields - 5-15cm depth [%]",
    avreage_sand_content_wheat = "Average soil sand content - milling wheat fields - 5-15cm depth [%]",
    avreage_silt_content_wheat = "Average soil silt content - milling wheat fields - 5-15cm depth [%]",
    average_altitude_wheat = "Average altitude - milling wheat fields [metre a.s.l.]",
    average_slope_angle_wheat = "Average slope angle - milling wheat fields [%]",
    
    average_temp_2022_09 = "Average temperature - milling wheat fields - September 2022 [°C]", 
    average_temp_2022_10 = "Average temperature - milling wheat fields - October 2022 [°C]",
    average_temp_2022_11 = "Average temperature - milling wheat fields - November 2022 [°C]",
    average_temp_2022_12 = "Average temperature - milling wheat fields - December 2022 [°C]",
    average_temp_2023_01 = "Average temperature - milling wheat fields - January 2023 [°C]",
    average_temp_2023_02 = "Average temperature - milling wheat fields - February 2023 [°C]",
    average_temp_2023_03 = "Average temperature - milling wheat fields - March 2023 [°C]",
    average_temp_2023_04 = "Average temperature - milling wheat fields - April 2023 [°C]",
    average_temp_2023_05 = "Average temperature - milling wheat fields - May 2023 [°C]",
    average_temp_2023_06 = "Average temperature - milling wheat fields - June 2023 [°C]",
    average_temp_2023_07 = "Average temperature - milling wheat fields - July 2023 [°C]",
    average_temp_2023_08 = "Average temperature - milling wheat fields - August 2023 [°C]",
    
    total_prec_2022_09 = "Total precipitation - milling wheat fields - September 2022 [mm]", 
    total_prec_2022_10 = "Total precipitation - milling wheat fields - October 2022 [mm]", 
    total_prec_2022_11 = "Total precipitation - milling wheat fields - November 2022 [mm]", 
    total_prec_2022_12 = "Total precipitation - milling wheat fields - December 2022 [mm]", 
    total_prec_2023_01 = "Total precipitation - milling wheat fields - January 2023 [mm]", 
    total_prec_2023_02 = "Total precipitation - milling wheat fields - February 2023 [mm]",
    total_prec_2023_03 = "Total precipitation - milling wheat fields - March 2023 [mm]", 
    total_prec_2023_04 = "Total precipitation - milling wheat fields - April 2023 [mm]", 
    total_prec_2023_05 = "Total precipitation - milling wheat fields - May 2023 [mm]", 
    total_prec_2023_06 = "Total precipitation - milling wheat fields - June 2023 [mm]", 
    total_prec_2023_07 = "Total precipitation - milling wheat fields - July 2023 [mm]", 
    total_prec_2023_08 = "Total precipitation - milling wheat fields - August 2023 [mm]", 
    
    survey_id = "Unique InBestSoil survey id",
    
  )

###########################################################################.####
###########################################################################.####
# Adding Other Atributes as Necessary                                       ####
###########################################################################.####
###########################################################################.####

secondary_data_rounded <- secondary_data_rounded %>%
  
  cb_add_col_attributes(survey_id,
                        source = "InBestSoil farm survey",
                        col_type = "Character") %>%
  
  ###########################################################################.####

  cb_add_col_attributes(kanton_regisered_farm_site,
                        source = "Extracted variable from the bewirtschaftungseinheiten map (2023) provided by geodienste.ch") %>%
  
  ###########################################################################.####

  cb_add_col_attributes(agricultural_zone_farm_site,
                        source = "Extracted variable from the boundaries of agricultural zones in Switzerland map provided by the Federal Office for Agriculture (FOAG)") %>%
  
  ###########################################################################.####

  cb_add_col_attributes(number_plots_total,
                        source = "Extracted variable from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(total_land_area,
                        source = "Extracted variable from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(number_plots_arable,
                        source = "Extracted variable from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(total_arable_area,
                        source = "Extracted variable from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(number_plots_perm_grass,
                        source = "Extracted variable from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(total_perm_grass_area,
                        source = "Extracted variable from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(perc_arable_beets,
                        source = "Calculated variable derived from information extracted from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(perc_arable_potatoes,
                        source = "Calculated variable derived from information extracted from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(perc_arable_maize,
                        source = "Calculated variable derived from information extracted from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(perc_arable_temp_grass,
                        source = "Calculated variable derived from information extracted from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(perc_arable_bff,
                        source = "Calculated variable derived from information extracted from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(perc_arable_barley,
                        source = "Calculated variable derived from information extracted from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(perc_arable_wheat,
                        source = "Calculated variable derived from information extracted from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(perc_arable_other_cereals,
                        source = "Calculated variable derived from information extracted from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(perc_arable_osr,
                        source = "Calculated variable derived from information extracted from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(perc_arable_other_oilseeds,
                        source = "Calculated variable derived from information extracted from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(perc_arable_legumes,
                        source = "Calculated variable derived from information extracted from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(perc_arable_veg_fruits,
                        source = "Calculated variable derived from information extracted from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(number_wheat_plots_total,
                        source = "Extracted variable from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  cb_add_col_attributes(total_wheat_area,
                        source = "Extracted variable from the nutzungsflächen map (2023) provided by geodienste.ch") %>%
  
  ###########################################################################.####

  cb_add_col_attributes(avreage_clay_content_wheat,
                        source = "Extracted variable from the soilgrids provided by the International Soil Reference and Information Centre (ISRIC)") %>%
  
  cb_add_col_attributes(avreage_sand_content_wheat,
                        source = "Extracted variable from the soilgrids provided by the International Soil Reference and Information Centre (ISRIC)") %>%
  
  cb_add_col_attributes(avreage_silt_content_wheat,
                        source = "Extracted variable from the soilgrids provided by the International Soil Reference and Information Centre (ISRIC)") %>%
  
  ###########################################################################.####

  cb_add_col_attributes(average_altitude_wheat,
                        source = "Extracted variable from the digital height model DHM25 provided by the Federal Office of Topography (swisstopo)") %>%
  
  cb_add_col_attributes(average_slope_angle_wheat,
                        source = "Calculated variable derived from information extracted from the digital height model DHM25 provided by the Federal Office of Topography (swisstopo)") %>%
  
  ###########################################################################.####

  cb_add_col_attributes(average_temp_2022_09,
                        source = "Extracted variable from the gridded monthly average temperature data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(average_temp_2022_10,
                        source = "Extracted variable from the gridded monthly average temperature data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(average_temp_2022_11,
                        source = "Extracted variable from the gridded monthly average temperature data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(average_temp_2022_12,
                        source = "Extracted variable from the gridded monthly average temperature data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(average_temp_2023_01,
                        source = "Extracted variable from the gridded monthly average temperature data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(average_temp_2023_02,
                        source = "Extracted variable from the gridded monthly average temperature data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(average_temp_2023_03,
                        source = "Extracted variable from the gridded monthly average temperature data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(average_temp_2023_04,
                        source = "Extracted variable from the gridded monthly average temperature data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(average_temp_2023_05,
                        source = "Extracted variable from the gridded monthly average temperature data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(average_temp_2023_06,
                        source = "Extracted variable from the gridded monthly average temperature data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(average_temp_2023_07,
                        source = "Extracted variable from the gridded monthly average temperature data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(average_temp_2023_08,
                        source = "Extracted variable from the gridded monthly average temperature data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  ###########################################################################.####

  cb_add_col_attributes(total_prec_2022_09,
                        source = "Extracted variable from the gridded monthly average precipitation data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(total_prec_2022_10,
                        source = "Extracted variable from the gridded monthly average precipitation data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(total_prec_2022_11,
                        source = "Extracted variable from the gridded monthly average precipitation data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(total_prec_2022_12,
                        source = "Extracted variable from the gridded monthly average precipitation data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(total_prec_2023_01,
                        source = "Extracted variable from the gridded monthly average precipitation data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(total_prec_2023_02,
                        source = "Extracted variable from the gridded monthly average precipitation data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(total_prec_2023_03,
                        source = "Extracted variable from the gridded monthly average precipitation data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(total_prec_2023_04,
                        source = "Extracted variable from the gridded monthly average precipitation data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(total_prec_2023_05,
                        source = "Extracted variable from the gridded monthly average precipitation data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(total_prec_2023_06,
                        source = "Extracted variable from the gridded monthly average precipitation data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(total_prec_2023_07,
                        source = "Extracted variable from the gridded monthly average precipitation data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)") %>%
  
  cb_add_col_attributes(total_prec_2023_08,
                        source = "Extracted variable from the gridded monthly average precipitation data provided by the Federal Office of Meteorology and Climatology (MeteoSwiss)")
  

###########################################################################.####
###########################################################################.####
# Generating Codebook                                                       ####
###########################################################################.####
###########################################################################.####

secondary_data_blinded <- secondary_data_rounded %>%
  dplyr::select(-number_plots_total,
                -number_plots_arable,
                -number_plots_perm_grass,
                -kanton_regisered_farm_site)
  

secondary_data_labels <- data.frame(variable_name = colnames(secondary_data_blinded), variable_label = get_label(secondary_data_blinded))

secondary_data_codebook <- codebook(df = secondary_data_blinded,
                                    title = "Codebook for Secondary Data",
                                    subtitle = "A Collection of Variables from Publicly Available and Free-to-access Sources",
                                    
                                    description = "As a supplement to the data that we collected ourselves via the InBestSoil farm survey, we also extracted additional variables from publicly available sources to supplement the primary data. All data we present herewith has been reduced in granularity via a rounding procedure in order to protect the anonymity of the farmers who answered the survey as per our data protection agreement. For certain very large farms we set the maximum land area to 100 hectares to further protect the identity of these farmers as farms of such scale are rare in Switzerland.
                                    
                                    The data can be linked to the full survey data set and the subset of data covering wheat production via the 'surveyid' variable. We include general farm structural information regarding cropped areas and the percentages of crop categories grown in rotation by a given farm. For the farms that grew milling wheat in the 2022/2023 production season we also include more detailed information relevant for wheat production aggregated across wheat plots including the average soil characteristics, geomorphology and meteorology. As all data presented within this document is secondary data that we did not collect ourselves we direct you to the original sources - which are outlined by variable below - for specific details. In using this data we also ask that you to respect and abide by the data usage regulations of the original sources.
                                    
                                    For the variables extracted from geodienste.ch we would like to cite and thank the following cantonal administrations who have made the data publicly available via this webservice: Aargau (AG), Appenzell Ausserhoden (AR), Appenzell Innerrhoden (AI), Bern (BE), Basel-Landschaft (BL), Basel-Stadt (BS), Fribourg (FR), Genève (GE), Glarus (GL), Graubünden (GR), Jura (JU), Luzern (LU), Neuchâtel (NE), Nidwalden (NW), Obwalden (OW), St. Gallen (SG), Schaffhausen (SH), Solothurn (SO), Schwyz (SZ), Thurgau (TG), Ticino (TI), Uri (UR), Vaud (VD), Valais (VS), Zug (ZG) and Zürich (ZH). Please consult the included README file for the contact information for each cantonal geographic information system administration involved in providing the geodata via gieodienste.ch.")

##########################################################################.####
##########################################################################.####
### Finalize dataset                                                       ####
##########################################################################.####
##########################################################################.####

saveRDS(secondary_data_blinded, file = "OUTPUT_FILE_PATH_REDACTED")

write_xlsx(secondary_data_blinded, path = "OUTPUT_FILE_PATH_REDACTED")

write_xlsx(secondary_data_labels, path = "OUTPUT_FILE_PATH_REDACTED")

print(secondary_data_codebook, "OUTPUT_FILE_PATH_REDACTED")