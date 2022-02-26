# Remove all variables
rm(list = ls(all.names = TRUE))

library(tidyverse)

load("experiment_info.Rdata")

all_data <- read_tsv(str_c("02_transfer_data_and_info/",experiment_name,"/transfer_volumes_and_checks.tsv"),
                     lazy = FALSE)

all_results <- readxl::read_xlsx(path = str_c("00_raw_data/",experiment_name,"/results.xlsx"))

### load setup_data and find the strain ratios.
receiver_donor_ratio_data <- 
  all_data %>% 
  select(row_ID,col_ID,strain,target_OD) %>% 
  pivot_wider(names_from = strain, values_from = target_OD) %>% 
  mutate(receiver_donor_ratio = receiver/donor)

merged_results <- 
  left_join(all_results,all_data, by =c("row_ID","col_ID")) %>% 
  left_join(receiver_donor_ratio_data, by =c("row_ID","col_ID"))

merged_results <- 
  merged_results %>% 
  mutate(count_corrected = (count * correction_factor_combined)/dilution) 

theme_set(
  theme_minimal() +
  theme(text = element_text(face = "bold"),
        axis.title = element_text(size = 10, face = "bold"),
        plot.title = element_text(size = 10, face = "bold"))
)



# Plot for conjugation efficiency -----------------------------------------

conjugation_efficiency_plot <- merged_results %>% 
  filter(
    log_vs_lag == "lag"
  ) %>% 
  group_by(selection) %>% 
  summarise(
    count_average = mean(count_corrected),
    count_std = sd(count_corrected),
    n = n()
  ) %>%
  mutate(sem = count_std/sqrt(n)) %>% 
  ggplot(aes(y = count_average, x = selection, fill = selection)) +
  geom_col(position = "dodge2") +
  geom_errorbar(aes(ymin = count_average - sem,
                    ymax = count_average + sem)) +
  scale_y_log10() +
  scale_fill_manual(values = c("#454545","#53257d")) +
  theme(legend.position = "none") +
  labs(y = "CFU",
       x = " ",
       title = "Bi-parental conjugation efficiency")

ggsave(filename = str_c("03_results/",experiment_name,"/conjugation_efficiency_plot.png"), 
       plot = conjugation_efficiency_plot,
       width = 3,
       height = 3,
       dpi = 300)


# Plot for log vs lag -----------------------------------------------------

log_vs_lag_plot <- 
  merged_results %>% 
  filter(
    selection == "PIA + gent30"
  ) %>% 
  group_by(log_vs_lag) %>% 
  summarise(
    count_average = mean(count_corrected),
    count_std = sd(count_corrected),
    n = n()) %>%
  mutate(sem = count_std/sqrt(n)) %>% 
  ggplot(aes(y = count_average, x = log_vs_lag, fill = log_vs_lag)) +
  geom_col(position = "dodge2") +
  geom_errorbar(aes(ymin = count_average - sem,
                    ymax = count_average + sem)) +
  scale_y_log10() +
  scale_fill_manual(values = c("#53257d","#53257d")) +
  theme(legend.position = "none") +
  labs(y = "CFU",
       x = " ",
       title = "exponential vs stationary reciever")

ggsave(filename = str_c("03_results/",experiment_name,"/log_vs_lag_plot.png"), 
       plot = log_vs_lag_plot,
       width = 3,
       height = 3,
       dpi = 300)


# plot for ratios ---------------------------------------------------------

ratio_plot <- 
  merged_results %>% 
  filter(
    heat_shock == "no"
  ) %>% 
  ggplot(aes(y = count_corrected, x = receiver_donor_ratio)) +
  geom_point() +
  scale_y_log10() +
  xlim(0,5) +
  scale_color_manual(values = c("#53257d")) +
  labs(y = "CFU",
       x = "Reciever/donor ratio",
       title = "Receiver donor ratio")


ggsave(filename = str_c("03_results/",experiment_name,"/ratio_plot_plot.png"),
       plot = ratio_plot,
       width = 3,
       height = 3,
       dpi = 300)

# plot for heat shock -----------------------------------------------------

heat_shock_plot <- 
  merged_results %>% 
    group_by(heat_shock) %>% 
    summarise(
      count_average = mean(count_corrected),
      count_std = sd(count_corrected),
      n = n()) %>%
    mutate(sem = count_std/sqrt(n)) %>% 
    ggplot(aes(y = count_average, x = heat_shock, fill = heat_shock)) +
    geom_col(position = "dodge2") +
    geom_errorbar(aes(ymin = count_average - sem,
                      ymax = count_average + sem)) +
    scale_y_log10() +
    scale_fill_manual(values = c("#53257d","#53257d")) +
    theme(legend.position = "none") +
    labs(y = "CFU",
         x = " ",
         title = "Effect of heat shock")

ggsave(filename = str_c("03_results/",experiment_name,"/heat_shock_plot.png"), 
       plot = heat_shock_plot,
       width = 3,
       height = 3,
       dpi = 300)

