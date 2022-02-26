library(tidyverse)
experiment_name = "20220111_P_conjugation_heat_shock_ratios"
save(list="experiment_name", file = "experiment_info.Rdata")

dir.create(str_c("01_clean_formatted_data/", experiment_name))
dir.create(str_c("02_transfer_data_and_info/", experiment_name))
dir.create(str_c("03_results/", experiment_name))

source("01_data_cleaning_and_formatting.R")
source("02_data_augmentation.R")

# Ideas for code improvements
### Allow for reservoir inputs of strains
### Allow for bi-parental setups
### More elegant solution for controls?
### Create a new project that takes the colony counts and the information
###   from this document to create beautiful charts
###   and statistics based on the information being stratified
###   have it be adaptive, such that sheet names specify the variable
###   names in the data frames
###   Make an EXCLUDED_ prefix for sheets to be excluded