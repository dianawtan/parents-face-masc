## data analysis script
## written by Diana Tan 26/08/20
## last updated on: 21/12/21

## These analyses compared parents' faces versus controls' faces. 

#### libraries ----
library(pacman)
pacman::p_load(psych, readr, tidyverse, sjstats, pwr, DescTools)


#### data import ----

data <- read_csv("02_clean_data/02_clean_transf_data.csv")


#### descriptive statistics ----
describeBy(x = data, group = c("group", "sex_parent"), digits = 3)


#### check for confounding variables ----

dataM <- data %>%
  filter(sex_parent == "male") 

dataF <- data %>%
  filter(sex_parent == "female") 

# combined sexes

corr.test(data$masc_score, data$ageAtScan) #r = 0.26, p < .001, n = 355
corr.test(data$masc_score, data$facial_area) #r = 0.55, p <.001. n = 355

# males only

corr.test(dataM$masc_score, dataM$ageAtScan) #r = 0.26, p < .001, n = 108
corr.test(dataM$masc_score, dataM$facial_area) #r = 0.24, p = .01. n = 108

# females only

corr.test(dataF$masc_score, dataF$ageAtScan) #r = 0.20, p < .001, n = 247
corr.test(dataF$masc_score, dataF$facial_area) #r = 0.38, p < .001, n = 247


#### main analyses ----

### masculinity score

## before controlling for age and facial area

summary(aov(masc_score ~ group*sex_parent, data = data))
eta_sq(aov(masc_score ~ group*sex_parent, data = data), partial = TRUE, ci.lvl = 0.95)

## after controlling for age and facial area

summary(aov(masc_score ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(masc_score ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = TRUE, ci.lvl = 0.95)


### facial distances

## before controlling for age and facial area

summary(aov(g_n_sto ~ group*sex_parent, data = data))
eta_sq(aov(g_n_sto ~ group*sex_parent, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(g_ex_ex ~ group*sex_parent, data = data))
eta_sq(aov(g_ex_ex ~ group*sex_parent, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(g_ft_ft_box ~ group*sex_parent, data = data))
eta_sq(aov(g_ft_ft_box ~ group*sex_parent, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(g_sto_pg_box ~ group*sex_parent, data = data))
eta_sq(aov(g_sto_pg_box ~ group*sex_parent, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(g_ex_ch_L ~ group*sex_parent, data = data))
eta_sq(aov(g_ex_ch_L ~ group*sex_parent, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(g_ex_ch_R ~ group*sex_parent, data = data))
eta_sq(aov(g_ex_ch_R ~ group*sex_parent, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(l_sbal_sn_sbal ~ group*sex_parent, data = data))
eta_sq(aov(l_sbal_sn_sbal ~ group*sex_parent, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(l_sn_prn ~ group*sex_parent, data = data))
eta_sq(aov(l_sn_prn ~ group*sex_parent, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(l_sn_sto ~ group*sex_parent, data = data))
eta_sq(aov(l_sn_sto ~ group*sex_parent, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(l_n_prn ~ group*sex_parent, data = data))
eta_sq(aov(l_n_prn ~ group*sex_parent, data = data), partial = TRUE, ci.lvl = 0.95)


## after controlling for age and facial area

summary(aov(g_n_sto ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(g_n_sto ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(g_ex_ex ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(g_ex_ex ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(g_ft_ft_box ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(g_ft_ft_box ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(g_sto_pg_box ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(g_sto_pg_box ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(g_ex_ch_L ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(g_ex_ch_L ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(g_ex_ch_R ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(g_ex_ch_R ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(l_sbal_sn_sbal ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(l_sbal_sn_sbal ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(l_sn_prn ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(l_sn_prn ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(l_sn_sto ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(l_sn_sto ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = TRUE, ci.lvl = 0.95)

summary(aov(l_n_prn ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(l_n_prn ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = TRUE, ci.lvl = 0.95)


#### additional analyses re: sex of proband children ----

# do parents of autistic boys show more masculine faces than those of autistic girls?
summary(aov(masc_score ~ sex_child, data = dataF)) # no
summary(aov(masc_score ~ sex_child, data = dataM)) # no

summary(aov(masc_score ~ sex_child + ageAtScan + facial_area, data = dataF)) # no even after controlling
summary(aov(masc_score ~ sex_child + ageAtScan + facial_area, data = dataM)) # no even after controlling

