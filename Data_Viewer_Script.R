#///////////////////////////////////////////////////////////////////////////####
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\####
# Script for browsing and reviewing combined dataset ####
#///////////////////////////////////////////////////////////////////////////####
# Authours: Charles Rees (FiBL and ETH Zürich) and Lorin Ineichen (FiBL)    ####
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\####

#///////////////////////////////////////////////////////////////////////////####
# SET UP PRIOR TO EXECUTION ####
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\####


# install.packages("sjlabelled")
library(sjlabelled) # to add variable labels in R (as in stata)

rm(list = ls())

# Paste the path to where you downloaded and saved the data repository file
your_file_path <- "***YOUR_FILE_PATH***/Data_Repository" 

setwd(your_file_path)


#///////////////////////////////////////////////////////////////////////////####
# PRIMARY DATASET ####
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\####

# Load labelled primary dataset
primary_data <- readRDS("01_Open_Data/01_Primary_Data/Primary_Data.rds")

View(primary_data)


#///////////////////////////////////////////////////////////////////////////####
# WHEAT DATA SUBSET ####
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\####

# Load labelled wheat dataset
wheat_data <- readRDS("01_Open_Data/02_Wheat_Data/Wheat_Data.rds")

View(wheat_data)


#///////////////////////////////////////////////////////////////////////////####
# SECONDARY DATA ####
#\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\####

# Load labelled secondary dataset
secondary_data <- readRDS("01_Open_Data/03_Linked_Secondary_Data/Linked_Secondary_Data.rds")

View(secondary_data)
