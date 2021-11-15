source("99_parameters.R")

dir.create(str_c("1_data_clean_up/", experiment_name))
dir.create(str_c("2_data_augmentation/", experiment_name))
dir.create(str_c("3_data_visualisation/", experiment_name))
dir.create(str_c("X_error_report/", experiment_name))

source("01_SpectraMax loader.R")
source("02_data_augmentation.R")

# Ideas for code improvements
### Allow for reservoir inputs of strains
### Allow for bi-parental setups
### More elegant solution for controls?
### 