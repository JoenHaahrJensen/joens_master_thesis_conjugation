library(tidyverse)

source("99_parameters.R")
folder_path = "0_raw_data"


# Loading plate reader data -----------------------------------------------


# Given an experiment/folder name in 99_parameters
# the file paths for the data files are defined here
helper_file_path = str_c(folder_path,"/",experiment_name,"/helper.txt")
donor_file_path = str_c(folder_path,"/",experiment_name,"/donor.txt")
receiver_file_path = str_c(folder_path,"/",experiment_name,"/receiver.txt")

# these vectors will be used to index the plate
col_ID <- c("1","2","3","4","5","6","7","8","9","10","11","12")
row_ID <- c("A","B","C","D","E","F","G","H")

# We want to load and convert the data into a manage long format
# with row and column indexing
load_plate_reader_data <- function(file_path){
  #file_path = helper_file_path
  tbl <- read_tsv(file_path, skip = 3)
  tbl <- tbl[2:9,3:14]
  colnames(tbl) = col_ID
  tbl <- cbind(tbl,row_ID)
  tbl <-  
    tbl %>% 
    pivot_longer(cols = 1:12,
                 names_to = "col_ID",
                 values_to = "OD",
                 values_transform = list(OD = as.character))
  return(tbl)
}

# We want to reduce duplicated code
# so we create a tibble with file_paths for some purrr::map action
all_data <- tribble(
  ~strain, ~filename,
  "helper", helper_file_path,
  "donor", donor_file_path,
  "receiver", receiver_file_path
)

# We can to create a new column with the filenames
all_data <- 
  all_data %>% 
  mutate(data = purrr::map(filename, load_plate_reader_data))

# We want to check whether the OD readings from the experiment
# is in a wrong format or is outside allowed OD limits
data_criteria_sorting = function(tbl) {
  # tbl <- OD_reads_helper
  tbl <- 
    tbl %>% 
    mutate(OD = replace(tbl$OD, str_detect(tbl$OD, "[a-zA-Z]"), 0),
           OD = as.numeric(OD),
           OD = OD - OD_blank,
      format_check = OD != 0,
      limit_check = case_when(
        !is.numeric(OD) ~ TRUE,
        OD < lower_OD_limit ~ FALSE,
        lower_OD_limit <= OD & OD <= higher_OD_limit ~ TRUE,
        OD > higher_OD_limit ~ FALSE
      )
    )
  return(tbl)
}

# We can overwrite "data" to add the data sorting criteria checks
all_data2 <- 
  all_data %>% 
  mutate(data = purrr::map(data, data_criteria_sorting)) %>% 
  select(strain,data) %>% 
  unnest(data)

# We want to sort out all data that does not meet the given
# criteria for the future code to run smoothly
all_data3 <- 
  all_data2 %>% 
  select(strain, row_ID, col_ID, OD, limit_check, format_check) 
  # pivot_wider(names_from = strain, 
  #             values_from = c(OD),
  #             names_prefix = "OD_")

# tibble with identified errors is generated
error_report <-
  all_data2 %>%
  filter(format_check == FALSE | limit_check == FALSE)

# We want to script to say if any datapoints have been dropped
# based on the criteria
if(nrow(error_report) > 0){
  n_limit_errors = filter(error_report, limit_check == FALSE) %>% summarise(n())
  n_format_errors = filter(error_report, format_check == FALSE) %>% summarise(n())
  print(str_c("WARNING: ", n_limit_errors, " limit errors and ", n_format_errors, " format errors"))
}


# Loading setup data ------------------------------------------------------

# The setup data is an excel sheet that specifies the desired
# concentrations of the different strains.
setup_file_path = str_c(folder_path,"/",experiment_name,"/setup.xlsx")

# we want the setup data indexed with col_ID and row_ID and in
# a long format for easy manipulation
load_setup_data <- function(file_path, sheet_name){
  tbl <- readxl::read_xlsx(setup_file_path, col_names = FALSE, sheet = sheet_name)
  colnames(tbl) = col_ID
  tbl <- cbind(tbl,row_ID)
  tbl <-  
    tbl %>% 
    pivot_longer(cols = 1:12, names_to = "col_ID", values_to = "target_OD")
  return(tbl)
}

# Again, we can be efficient with some purrr::map action
all_setup_data <- tribble(
  ~strain, ~filename,
  "helper", setup_file_path,
  "donor", setup_file_path,
  "receiver", setup_file_path
)

all_setup_data <- 
  all_setup_data %>% 
  mutate(setup_data = purrr::map2(.x = filename, .y = strain, .f = load_setup_data))

# The data is pivoted longer to be merged with the other data
all_setup_data2 <-
  all_setup_data %>% 
  unnest(setup_data) %>% 
  # pivot_wider(names_from = "strain", values_from = "target_OD", names_prefix = "target_OD_") %>% 
  select(everything(),-filename)

clean_data <- full_join(all_data3, all_setup_data2)

write_tsv(x = clean_data, 
          file = str_c("1_data_clean_up/",experiment_name,"/01_clean_data.tsv"))
