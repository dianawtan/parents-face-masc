

pacman::p_load(ggplot2, readr, tidyverse)

data <- read_csv("02_clean_data/02_clean_transf_data.csv") %>%
  filter(PARENT_ID != "2011125MO")

data$group_new <- paste(data$group, data$sex)

ggplot(data) +
  geom_density(aes(x = masc_score, fill = group_new))
