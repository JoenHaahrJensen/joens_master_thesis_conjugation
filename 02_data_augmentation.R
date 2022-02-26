# install.packages("tidyverse")
# install.packages("formattable")

# Remove all variables
rm(list = ls(all.names = TRUE))

# Add 350 uL reciever limit to hit-pick calculator
# Add 200 uL pipetting limit ot hit-pick steps
# load col_ID as factor and refactor such that 1 is first and 12 is last
library(tidyverse)
library(formattable)
load("experiment_info.Rdata")

row_ID <- c("A","B","C","D","E","F","G","H")

all_data <- read_tsv(str_c("01_clean_formatted_data/",experiment_name,"/01_clean_data.tsv"),
                     lazy = FALSE)
                     # col_types = cols(col_ID = col_character()))

all_data <-  
  all_data %>% 
  group_by(row_ID, col_ID) %>% 
  mutate(transfer_volume_required = target_OD * target_volume_uL/OD,
         transfer_max_volume_check = case_when(
            any(transfer_volume_required > 350) ~ FALSE,
            transfer_volume_required <= 350 ~ TRUE))

all_data2 <- 
  all_data %>%
  group_by(col_ID,row_ID) %>% 
  mutate(correction_factor_volume = case_when(
      !transfer_max_volume_check ~ 350 / max(transfer_volume_required),
      transfer_max_volume_check ~ 1)) %>% 
  mutate(transfer_volume = transfer_volume_required * correction_factor_volume)
  
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
  ggplot(mapping = aes(x = as.factor(col_ID), y = row_ID)) +
    geom_point(aes(fill = error_type),
               shape = 21,
               size = 10) +
    geom_label(aes(label = correction_factor_combined)) + 
    theme_minimal() +
    scale_y_discrete(limits=rev) +
    theme(legend.position = "bottom") +
    labs(x = "Column",
         y = "ROw",
         title = str_c("error report for experiment:",experiment_name))

all_volumes2 <- all_volumes %>%
  full_join(error_report, by = c("col_ID","row_ID"))

export_volumes <-
  all_volumes2 %>% 
  filter(error_type == "correct" | error_type == "total_volume_fail" | error_type == "transfer_volume_fail") %>%  
  ungroup(col_ID,row_ID) %>%
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

tip_volume = 200

export_volumes2 <- 
  export_volumes %>% 
  mutate(transfer_replicates = ceiling(transfer_volume/tip_volume)) %>%
  group_by(strain, position, transfer_volume, dest_plate, dest_position) %>% 
  expand(count = seq(1:transfer_replicates)) %>% 
  mutate(transfer_volume = case_when(
    count * tip_volume == transfer_volume ~ tip_volume,
    count == 1 ~ transfer_volume %% tip_volume,
    count != 1 ~ tip_volume
  ),
  transfer_volume = round(transfer_volume, digits = 0),
  source_plate = strain) %>% 
  rename("source_position" = "position") %>% 
  select(strain, source_plate, source_position, dest_plate, dest_position, transfer_volume)


# Saving data -------------------------------------------------------------

export_volumes2 %>% 
  ungroup(everything()) %>% 
  group_by(strain) %>% 
  nest() %>% 
  mutate(filename = str_c("02_transfer_data_and_info/",experiment_name,"/export_volumes_",strain,".csv"),
          export_file = purrr::walk2(.x = data, .y = filename, .f = write_csv))

all_volumes2 %>% 
  write_tsv(str_c("02_transfer_data_and_info/",experiment_name,"/transfer_volumes_and_checks.tsv"))  

error_report %>% 
  select(row_ID, col_ID, correction_factor_combined) %>% 
  write_tsv(str_c("02_transfer_data_and_info/",experiment_name,"/correction_matrix.tsv"))  

ggsave(filename = str_c("02_transfer_data_and_info/",experiment_name,"/error_report_visual.png"), plot = visual_error_report)


# Formattable table -------------------------------------------------------


all_volumes2 %>% 
  select(row_ID, col_ID, 
         strain, 
         target_OD, 
         OD, 
         transfer_volume_required, 
         transfer_volume, 
         correction_factor_volume) %>%
  arrange(row_ID, col_ID) %>% 
  head(15) %>% 
  mutate(target_OD                = round(target_OD,                digits = 2),
         OD                       = round(OD,                       digits = 2),
         transfer_volume_required = round(transfer_volume_required, digits = 2),
         transfer_volume          = round(transfer_volume,          digits = 2),
         correction_factor_volume = round(correction_factor_volume, digits = 2)) %>% 
  formattable(list(
    OD = color_tile("transparent", "Orange"),
    transfer_volume = color_bar("lightblue")))

