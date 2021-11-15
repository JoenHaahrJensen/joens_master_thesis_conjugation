#install.packages("tidyverse")
library(tidyverse)
source("99_parameters.R")

target_volume <- 500
target_reciever_donor_ratio <- 10

target_receiver_OD <- 0.2
target_donor_OD <- target_receiver_OD/target_reciever_donor_ratio
row_ID <- c("A","B","C","D","E","F","G","H")


# Generate and load dummy data --------------------------------------------


load_plate_reader_data <- function(filename){
  # filename = "Dummy_plate_reader_data.csv"
  tbl <- read_csv2(filename,
            col_names = FALSE)
  colnames(tbl) = 1:12
  tbl <- cbind(tbl,row_ID)
  tbl <- 
    pivot_longer(data = tbl,
                 cols = matches(match = "\\d+"),
                 names_to = "col_ID",
                 values_to = "OD600")
  return(tbl)
}

all_data <- tribble(
  ~strain, ~filename,
  "helper", "Dummy_plate_reader_data_ET.csv",
  "donor", "Dummy_plate_reader_data_DH.csv",
  "receiver", "Dummy_plate_reader_data_ATCC.csv"
)

all_data <- 
  all_data %>% 
    mutate(data = purrr::map(filename, load_plate_reader_data))

data_criteria_sorting = function(tbl) {
  # tbl <- OD_reads_helper
  tbl <- 
    tbl %>% 
    mutate(
      format_check = is.numeric(OD600),
      limit_check = case_when(
        !is.numeric(OD600) ~ TRUE,
        OD600 < lower_OD_limit ~ FALSE,
        lower_OD_limit <= OD600 & OD600 <= higher_OD_limit ~ TRUE,
        OD600 > higher_OD_limit ~ FALSE
      )
    )
  return(tbl)
  }


all_data <- 
  all_data %>% 
  mutate(criteria_errors = purrr::map(data, data_criteria_sorting))
# Merge loaded data -------------------------------------------------------

error_report <- 
  all_data %>% 
  select(strain, criteria_errors) %>% 
  unnest(criteria_errors)

all_data <- 
  all_data %>% 
  select(strain, data) %>% 
  unnest(data) %>% 
  pivot_wider(names_from = strain, 
              values_from = c(OD600),
              names_prefix = "OD_")

# the following should be replaced with a an excel sheet that is loaded in the
# start for generalizability

all_data <- 
  all_data %>% 
  mutate(target_OD_helper = 0.05,
         target_OD_donor = 0.05,
         target_OD_receiver = 0.5)


all_data <-  
  all_data %>% 
  mutate(transfer_volume_receiver = target_OD_receiver * target_volume / OD_receiver,
         transfer_volume_helper = target_OD_helper * target_volume / OD_helper,
         transfer_volume_donor = target_OD_donor * target_volume / OD_donor,
         sum_of_volumes = transfer_volume_receiver + transfer_volume_helper + transfer_volume_donor)

all_volumes <- 
  all_data %>% 
  mutate(total_volume_check = sum_of_volumes <= 1000) %>% 
  pivot_longer(cols = matches("^OD_"),
               names_to = "strain",
               values_to = "OD",
               names_prefix = "OD_") %>% 
  mutate(transfer_volume = case_when(
    strain == "helper" ~ transfer_volume_helper,
    strain == "donor" ~ transfer_volume_donor,
    strain == "receiver" ~ transfer_volume_receiver
  )) %>% 
  select(everything(), -matches(c("target_OD", "transfer_volume_")))


all_volumes <- 
  all_volumes %>% 
  mutate(transfer_volume_corrected = case_when(
    sum_of_volumes > 1000 ~ 1000 / sum_of_volumes * transfer_volume,
    sum_of_volumes <= 1000 ~ transfer_volume),
    mimimum_transfer_volume_check = transfer_volume_corrected >= 10)

error_report2 <- 
  error_report %>% 
  full_join(all_volumes, by = c("strain","col_ID","row_ID"))
 
error_report3 <- 
  error_report2 %>% 
    group_by(col_ID,row_ID) %>% 
    summarise(limit_check = any(limit_check == FALSE),
              format_check = any(format_check == FALSE),
              total_volume_check = any(total_volume_check == FALSE),
              mimimum_transfer_volume_check = any(mimimum_transfer_volume_check == FALSE)
              ) %>% 
    mutate(error_present = limit_check | format_check | total_volume_check | mimimum_transfer_volume_check)

error_report3 %>% 
  ggplot(mapping = aes(x = col_ID, y = row_ID)) +
    geom_point(aes(fill = error_present),
               shape = 21,
               size = 10)+
    theme_void() +
    # scale_fill_gradient(low="#56B4E9", high="#E69F00") +
    theme(legend.position = "none")
