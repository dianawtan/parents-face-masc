# preliminary data analyses script
# written by Diana Tan 26/08/20

#### libraries ----

library(pacman)
p_load(readr, ggpubr, ggplot2, MASS, dplyr, purrr)


#### data import ----

clean_data <- read_csv("02_clean_data/01_clean_data.csv")


#### assumption checks ----

# density plots

ggdensity(clean_data$masc_score) # bimodel because sexual dimorphism

# no transformation needed