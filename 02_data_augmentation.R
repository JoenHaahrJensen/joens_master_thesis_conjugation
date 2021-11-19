# install.packages("tidyverse")
# Add 350 uL reciever limit to hit-pick calculator
# Add 200 uL pipetting limit ot hit-pick steps
# load col_ID as factor and refactor such that 1 is first and 12 is last
library(tidyverse)
source("99_parameters.R")

row_ID <- c("A","B","C","D","E","F","G","H")

all_data <- read_tsv(str_c("1_data_clean_up/",experiment_name,"/01_clean_data.tsv"),
                     lazy = FALSE)
                     # col_types = cols(col_ID = col_character()))

all_data <-  
  all_data %>% 
  group_by(row_ID, col_ID) %>% 
  mutate(transfer_volume_original = target_OD * target_volume/OD,
         transfer_max_volume_check = case_when(
            any(transfer_volume_original > 350) ~ FALSE,
            transfer_volume_original <= 350 ~ TRUE))

all_data2 <- 
  all_data %>%
  group_by(col_ID,row_ID) %>% 
  mutate(correction_factor_volume = case_when(
      !transfer_max_volume_check ~ 350 / max(transfer_volume_original),
      transfer_max_volume_check ~ 1)) %>% 
  mutate(transfer_volume = transfer_volume_original * correction_factor_volume)
  
all_data3 <-  
  all_data2 %>% 
  group_by(col_ID,row_ID) %>% 
  mutate(sum_of_volumes = sum(transfer_volume),
         total_volume_check = case_when(
           any(sum_of_volumes > 1000) ~ FALSE,
           sum_of_volumes <= 1000 ~ TRUE),
         total_volume_correction_factor = case_when(
           !total_volume_check ~ 1000/sum_of_volumes,
           total_volume_check ~ 1))

all_volumes <- 
  all_data3 %>% 
  group_by(col_ID,row_ID) %>% 
  mutate(transfer_volume = transfer_volume * total_volume_correction_factor,
    mimimum_transfer_volume_check = transfer_volume >= 10 | transfer_volume == 0)

# Error check
error_report <- 
  all_volumes %>% 
  group_by(col_ID,row_ID) %>% 
  mutate(correction_factor_combined = total_volume_correction_factor * correction_factor_volume) %>% 
  summarise(total_volume_fail = any(total_volume_check == FALSE, na.rm = TRUE),
            mimimum_transfer_volume_fail = any(mimimum_transfer_volume_check == FALSE, na.rm = TRUE),
            format_fail = any(format_check == FALSE, na.rm = TRUE),
            limit_fail = any(limit_check == FALSE, na.rm = TRUE),
            transfer_volume_fail = any(transfer_max_volume_check == FALSE),
            correction_factor_combined = min(correction_factor_combined),
            correction_factor_combined = round(correction_factor_combined, 2)) %>% 
  mutate(error_type = case_when(
    format_fail == TRUE ~ "format_fail",
    limit_fail == TRUE ~ "limit_fail",
    mimimum_transfer_volume_fail == TRUE ~ "mimimum_transfer_volume_fail",
    total_volume_fail == TRUE ~ "total_volume_fail",
    transfer_volume_fail == TRUE ~ "transfer_volume_fail",
    TRUE ~ "correct")) %>%
  ungroup()

visual_error_report <- 
  error_report %>% 
  ggplot(mapping = aes(x = col_ID, y = row_ID)) +
    geom_point(aes(fill = error_type),
               shape = 21,
               size = 10) +
    geom_label(aes(label = correction_factor_combined)) + 
    theme_minimal() +
    scale_y_discrete(limits=rev) +
    theme(legend.position = "bottom")

all_volumes2 <- all_volumes %>%
  full_join(error_report, by = c("col_ID","row_ID")) %>% 
  filter(error_type == "correct" | error_type == "total_volume_fail" | error_type == "transfer_volume_fail") %>% 
  select(col_ID, row_ID, strain, transfer_volume)

export_volumes <-
  all_volumes2 %>% 
  mutate(row_num = case_when(
    row_ID == "A" ~ 1,
    row_ID == "B" ~ 2,
    row_ID == "C" ~ 3,
    row_ID == "D" ~ 4,
    row_ID == "E" ~ 5,
    row_ID == "F" ~ 6,
    row_ID == "G" ~ 7,
    row_ID == "H" ~ 8,
  ),
  position = (row_num-1) * 12 + col_ID) %>% 
  select(strain, position, transfer_volume) %>% 
  filter(transfer_volume != 0) %>% 
  mutate(dest_plate = "dest_plate",
         dest_position = position)

export_volumes <- 
  export_volumes %>% 
  mutate(transfer_replicates = ceiling(transfer_volume/200)) %>%
  group_by(strain,position, transfer_volume, dest_plate, dest_position) %>% 
  expand(count = seq(1:transfer_replicates)) %>% 
  mutate(transfer_volume = case_when(
    count == 1 ~ transfer_volume %% 200,
    count != 1 ~ 200
  ),
  transfer_volume = round(transfer_volume, digits = 0))

export_volumes_helper <- 
  filter(export_volumes, strain == "helper")
export_volumes_donor <- 
  filter(export_volumes, strain == "donor")
export_volumes_receiver <- 
  filter(export_volumes, strain == "receiver")

write_csv(x = export_volumes_helper, file = str_c("3_data_visualisation/",experiment_name,"/export_volumes_helper.csv"))
write_csv(x = export_volumes_donor, file = str_c("3_data_visualisation/",experiment_name,"/export_volumes_donor.csv"))
write_csv(x = export_volumes_receiver, file = str_c("3_data_visualisation/",experiment_name,"/export_volumes_receiver.csv"))


ggsave(filename = str_c("X_error_report/",experiment_name,"/error_report_visual.png"), plot = visual_error_report)
