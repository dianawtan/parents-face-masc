# script to clean raw data files
# written by: Diana Tan 26/08/20

#### libraries ----

library(pacman)
p_load(tidyverse, readxl, haven)


#### data import and wrangling: facial masculinity data ----

masc_data_header <- read_excel("01_raw_data/Gender_Results.xlsx", sheet = "Features") %>%
  slice(1)

masc_data <- read_excel("01_raw_data/Gender_Results.xlsx", sheet = "Features") 

norm_dist <- read_excel("02_clean_data/normdist_calc.xlsx") %>%
  dplyr::select(PARENT_ID, normdist)

colnames(masc_data) <- masc_data_header

masc_data <- masc_data %>%
  slice(-1) %>%
  rename("PARENT_ID" = "File Name",
         "group" = "Class") %>%
 dplyr:: select(-c("Sno", "group")) %>%
  mutate_at(c("n_sto","ex_ex", "ft_ft", "sto_pg", "ex_ch_L", "ex_ch_R", "sbal_sn_sbal", "sn_prn", "sn_sto", "n_prn", "gender_score", "facial_area"), as.numeric)

masc_data$PARENT_ID <- as.character(masc_data$PARENT_ID)

behav_data <- read_csv("01_raw_data/01_clean_data.csv") %>%
  dplyr::select(-c("abs_hor":"da_mean"))

full_data <- full_join(behav_data, masc_data, by = "PARENT_ID")  %>%
  distinct(PARENT_ID, .keep_all = TRUE)

full_data <- full_data %>%
  filter(PARENT_ID != "2011125MO")

rm(masc_data_header)
rm(behav_data)
rm(masc_data)

#### data wrangling: face measure ----

# reverse score from gender score (0 = masculine; 1 = feminine) to masculinity score (0 = feminine; 1 = masculine)

full_data$masc_score <- 1 - full_data$gender_score

#### data selection ----

# add anonymous ID

full_data$anon_id <- 1:nrow(full_data)

# add norm dist

full_data <- right_join(full_data, norm_dist, by = "PARENT_ID")

# selection of data used in manuscript

manuscript_data <- full_data %>%
  dplyr::select(anon_id, sex, group, ageAtScan, facial_area, masc_score, normdist, parent_bmi, parent_head)

#### export data ----

write_csv(manuscript_data, "02_clean_data/01_clean_data.csv")


#### random selection ----
## of observations for reliability analyses


fathers <- full_data %>%
  filter(group == "Parent" & sex == "male")

selectedFa <- fathers[sample(nrow(fathers), 7), ]

mothers <- full_data %>%
  filter(group == "Parent")

selectedMo <- mothers[sample(nrow(mothers), 8), ]

males <- full_data %>%
  filter(group == "Control")

selectedMales <- males[sample(nrow(males), 7), ]

females <- full_data %>%
  filter(group == "Control")

selectedFemales <- females[sample(nrow(females), 8), ]

randSelection <- bind_rows(selectedFemales, selectedMales)
randSelection <- bind_rows(randSelection, selectedFa)
randSelection <- bind_rows(randSelection, selectedMo)

randSelection <- randSelection %>%
  select(PARENT_ID, anon_id, sex, group, ageAtScan, facial_area, masc_score, normdist, parent_bmi, parent_head)

write_csv(randSelection, "02_clean_data/randSelect.csv")

