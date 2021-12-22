# preliminary data analyses script
# written by Diana Tan 26/08/20

#### libraries ----

library(pacman)
p_load(readr, ggpubr, ggplot2, MASS, dplyr, purrr)


#### data import ----

clean_data <- read_csv("02_clean_data/01_clean_data.csv")


#### assumption checks ----

# density plots

ggdensity(clean_data$masc_score) # bimodel because sexual dimorphism, no transformation needed

ggdensity(clean_data$facial_area) #normally distributed
ggdensity(clean_data$g_n_sto) #normally distributed
ggdensity(clean_data$g_ex_ex) #normally distributed
ggdensity(clean_data$g_ft_ft) #right skew
ggdensity(clean_data$g_sto_pg) #right skew
ggdensity(clean_data$g_ex_ch_L) #normally distributed
ggdensity(clean_data$g_ex_ch_R) #normally distributed
ggdensity(clean_data$l_sbal_sn_sbal) #normally distributed
ggdensity(clean_data$l_sn_prn) #normally distributed
ggdensity(clean_data$l_sn_sto) #normally distributed
ggdensity(clean_data$l_n_prn) #normally distributed


#### data transformation ----

# log transformation

clean_data$g_ft_ft_log <- log(clean_data$g_ft_ft)

clean_data$g_sto_pg_log <- log(clean_data$g_sto_pg)


#### data export ----

write_csv(clean_data, "02_clean_data/02_clean_transf_data.csv")
