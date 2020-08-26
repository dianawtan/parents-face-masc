# script to clean raw data files
# written by: Diana Tan 26/08/20

#### libraries ----

library(pacman)
p_load(tidyverse, readxl, haven)


#### data import and wrangling: facial masculinity data ----

masc_data_header <- read_excel("01_raw_data/Gender_Results.xlsx", sheet = "Features") %>%
  slice(1)

masc_data <- read_excel("01_raw_data/Gender_Results.xlsx", sheet = "Features") 
colnames(masc_data) <- masc_data_header
masc_data <- masc_data %>%
  slice(-1) %>%
  rename("PARENT_ID" = "File Name",
         "group" = "Class") %>%
  select(-c("Sno", "group")) %>%
  mutate_at(c("n_sto","ex_ex", "ft_ft", "sto_pg", "ex_ch_L", "ex_ch_R", "sbal_sn_sbal", "sn_prn", "sn_sto", "n_prn", "gender_score", "facial_area"), as.numeric)

masc_data$PARENT_ID <- as.character(masc_data$PARENT_ID)

behav_data <- read_csv("01_raw_data/01_clean_data.csv") %>%
  select(-c("abs_hor":"da_mean"))

full_data <- full_join(behav_data, masc_data, by = "PARENT_ID")

rm(masc_data_header)
rm(behav_data)
rm(masc_data)

#### data wrangling: face measure ----

# reverse score from gender score (0 = masculine; 1 = feminine) to masculinity score (0 = feminine; 1 = masculine)

full_data$masc_score <- 1 - full_data$gender_score

#### export data ----

write_csv(full_data, "02_clean_data/01_clean_data.csv")


