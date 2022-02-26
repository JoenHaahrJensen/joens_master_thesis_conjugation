# The subtracion of the blank value is wrong and needs fixing
# Better yet, add an excel sheet specifying strain (inluding blank as a strain option)
#   this will allows proper subtraction of background OD
#   To this add an escape value, in case all wells are contaminated

library(tidyverse)

load("experiment_info.Rdata")
raw_data_folder_path = "00_raw_data"

# Loading setup data ------------------------------------------------------

# The setup data is an excel sheet that specifies the desired
# concentrations of the different strains.
setup_file_path <- str_c(raw_data_folder_path,"/",experiment_name,"/setup.xlsx")
parameters <- readxl::read_xlsx(setup_file_path, sheet = "parameters")
list2env(parameters,globalenv())
save(list = c(names(parameters),"experiment_name"),file = "experiment_info.Rdata")

if(conjugation_mode == "bi-parental"){
  parental_strains <- c("donor", "receiver")
} else if(conjugation_mode == "tri-parental"){
  parental_strains <- c("helper","donor","receiver")
}

all_strains <- tribble(
  ~strain,
  parental_strains
) %>% 
  unnest(strain)

# these vectors will be used to index the plate
col_ID <- c("1","2","3","4","5","6","7","8","9","10","11","12")
row_ID <- c("A","B","C","D","E","F","G","H")


# we want the setup data indexed with col_ID and row_ID and in
# a long format for easy manipulation
load_setup_data <- function(setup_file_path, sheet_name){
  tbl <- readxl::read_xlsx(setup_file_path, col_names = FALSE, sheet = sheet_name)
  colnames(tbl) = col_ID
  tbl <- cbind(tbl,row_ID)
  tbl <-  
    tbl %>% 
    pivot_longer(cols = 1:12, names_to = "col_ID", values_to = "target_OD")
  return(tbl)
}


# Again, we can be efficient with some purrr::map action
all_setup_data <- 
  all_strains %>% 
  mutate(filename = setup_file_path) 

all_setup_data <- 
  all_setup_data %>% 
  mutate(setup_data = purrr::map2(.x = filename, .y = strain, .f = load_setup_data))

# The data is pivoted longer to be merged with the other data
all_setup_data2 <-
  all_setup_data %>% 
  unnest(setup_data) %>% 
  # pivot_wider(names_from = "strain", values_from = "target_OD", names_prefix = "target_OD_") %>% 
  select(everything(),-filename)

# Loading plate reader data -----------------------------------------------


# We want to load and convert the data into a manage long format
# with row and column indexing
# file_path = "00_raw_data/ratios_of_helper_donor_receiver/plater_raw.txt"

load_plate_reader_data <- function(file_path){
  tbl <- read_tsv(file_path,
                  col_names = FALSE,
                  skip = 5)
  tbl <- tbl[1:8,3:14]
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

all_data <- 
  all_strains %>% 
  mutate(file_path = str_c(raw_data_folder_path,experiment_name,strain, sep = "/"), 
           file_path = str_c(file_path,".txt"))

# We can to create a new column with the filenames
all_data <- 
  all_data %>% 
  mutate(data = purrr::map(file_path, load_plate_reader_data))

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

clean_data <- full_join(all_data3, all_setup_data2)

write_tsv(x = clean_data, 
          file = str_c("01_clean_formatted_data/",experiment_name,"/01_clean_data.tsv"))

