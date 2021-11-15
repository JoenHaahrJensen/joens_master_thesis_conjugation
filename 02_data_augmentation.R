#install.packages("tidyverse")
library(tidyverse)
source("99_parameters.R")

row_ID <- c("A","B","C","D","E","F","G","H")

all_data <- read_tsv(str_c("1_data_clean_up/",experiment_name,"/01_clean_data.tsv"),
                     lazy = FALSE)

all_data <-  
  all_data %>% 
  mutate(transfer_volume = target_OD * target_volume/OD) %>% 
  group_by(row_ID, col_ID) %>% 
  mutate(sum_of_volumes = sum(transfer_volume),
         total_volume_check = sum_of_volumes <= 1000) %>% 
  ungroup()


all_volumes <- 
  all_data %>% 
  mutate(transfer_volume_corrected = case_when(
    sum_of_volumes > 1000 ~ 1000 / sum_of_volumes * transfer_volume,
    sum_of_volumes <= 1000 ~ transfer_volume),
    mimimum_transfer_volume_check = transfer_volume_corrected >= 10 | transfer_volume_corrected == 0)

# Error check
error_report <- 
  all_volumes %>% 
  group_by(col_ID,row_ID) %>% 
  summarise(total_volume_fail = any(total_volume_check == FALSE, na.rm = TRUE),
            mimimum_transfer_volume_fail = any(mimimum_transfer_volume_check == FALSE, na.rm = TRUE),
            format_fail = any(format_check == FALSE, na.rm = TRUE),
            limit_fail = any(limit_check == FALSE, na.rm = TRUE)) %>% 
  mutate(error_type = case_when(
    format_fail == TRUE ~ "format_fail",
    limit_fail == TRUE ~ "limit_fail",
    mimimum_transfer_volume_fail == TRUE ~ "mimimum_transfer_volume_fail",
    total_volume_fail == TRUE ~ "total_volume_fail",
    TRUE ~ "correct"
  )) %>%
  ungroup()

visual_error_report <- 
  error_report %>% 
  ggplot(mapping = aes(x = col_ID, y = row_ID)) +
    geom_point(aes(fill = error_type),
               shape = 21,
               size = 10)+
    theme_minimal() +
    scale_y_discrete(limits=rev) +
    theme(legend.position = "bottom")

all_volumes2 <- all_volumes %>%
  full_join(error_report, by = c("col_ID","row_ID")) %>% 
  filter(error_type == "correct" | error_type == "total_volume_fail") %>% 
  select(col_ID, row_ID, strain, transfer_volume_corrected)

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
  select(strain, position, transfer_volume_corrected) %>% 
  filter(transfer_volume_corrected != 0) %>% 
  mutate(dest_plate = "dest_plate",
         dest_position = position)

write_csv(x = export_volumes, file = str_c("3_data_visualisation/",experiment_name,"/export_volumes.csv"))
ggsave(filename = str_c("X_error_report/",experiment_name,"/error_report_visual.png"), plot = visual_error_report)
