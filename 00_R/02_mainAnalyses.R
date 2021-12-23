## data analysis script
## written by Diana Tan 26/08/20
## last updated on: 21/12/21

## These analyses compared parents' faces versus controls' faces. 

#### libraries ----
library(pacman)
pacman::p_load(psych, readr, tidyverse, sjstats, pwr, DescTools, caret, MASS)


#### data import ----

data <- read_csv("02_clean_data/02_clean_transf_data.csv")


#### descriptive statistics ----
describeBy(x = data, group = c("group"), digits = 3)

describeBy(x = data, group = c("group", "sex_parent"), digits = 3)


#### check for confounding variables ----

# combined sexes

corr.test(data$masc_score, data$ageAtScan) #r = 0.26, p < .001, n = 355
corr.test(data$masc_score, data$facial_area) #r = 0.55, p <.001, n = 355

corr.test(data$g_n_sto, data$ageAtScan) # r = 0.26, p < .001, n = 355
corr.test(data$g_n_sto, data$facial_area)# r = 0.44, p < .001, n = 355

corr.test(data$g_ex_ex, data$ageAtScan) # r = 0.15, p < .01, n = 355
corr.test(data$g_ex_ex, data$facial_area) # r = 0.24, p < .001, n = 355

corr.test(data$g_ft_ft_log, data$ageAtScan) # r = 0.13, p < .01, n = 355
corr.test(data$g_ft_ft_log, data$facial_area) # r = 0.45, p < .001, n = 355

corr.test(data$g_sto_pg_log, data$ageAtScan) # r = 0.25, p < .001, n = 355
corr.test(data$g_sto_pg_log, data$facial_area) # r = 0.55, p < .001, n = 355

corr.test(data$g_ex_ch_L, data$ageAtScan) # r = 0.26, p < .001, n = 355
corr.test(data$g_ex_ch_L, data$facial_area) # r = 0.56, p < .001, n = 355

corr.test(data$g_ex_ch_R, data$ageAtScan) # r = 0.25, p < .001, n = 355
corr.test(data$g_ex_ch_R, data$facial_area) # r = 0.49, p < .001, n = 355

corr.test(data$l_sbal_sn_sbal, data$ageAtScan) # r = 0.12, p = .02, n = 355
corr.test(data$l_sbal_sn_sbal, data$facial_area) # r = 0.21, p < .001, n = 355

corr.test(data$l_sn_prn, data$ageAtScan) # r = 0.27, p < .001, n = 355
corr.test(data$l_sn_prn, data$facial_area) # r = 0.38, p < .001, n = 355

corr.test(data$l_sn_sto, data$ageAtScan) # r = 0.17, p < .001, n = 355
corr.test(data$l_sn_sto, data$facial_area) # r = 0.35, p < .001, n = 355

corr.test(data$l_n_prn, data$ageAtScan) # r = 0.20, p < .001, n = 355
corr.test(data$l_n_prn, data$facial_area) # r = 0.41, p < .001, n = 355


#### main analyses ----

### masculinity score

## before controlling for age and facial area

summary(aov(masc_score ~ group*sex_parent, data = data))
eta_sq(aov(masc_score ~ group*sex_parent, data = data), partial = TRUE, ci.lvl = 0.95)

## after controlling for age and facial area

summary(aov(masc_score ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(masc_score ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = FALSE, ci.lvl = 0.95)
# d = .1554, 95% CI[.01, .11]

### facial distances

## before controlling for age and facial area

summary(aov(g_n_sto ~ group*sex_parent, data = data))
eta_sq(aov(g_n_sto ~ group*sex_parent, data = data), partial = FALSE, ci.lvl = 0.95)

summary(aov(g_ex_ex ~ group*sex_parent, data = data))
eta_sq(aov(g_ex_ex ~ group*sex_parent, data = data), partial = FALSE, ci.lvl = 0.95)

summary(aov(g_ft_ft_log ~ group*sex_parent, data = data))
eta_sq(aov(g_ft_ft_log ~ group*sex_parent, data = data), partial = FALSE, ci.lvl = 0.95)

summary(aov(g_sto_pg_log ~ group*sex_parent, data = data))
eta_sq(aov(g_sto_pg_log ~ group*sex_parent, data = data), partial = FALSE, ci.lvl = 0.95)

summary(aov(g_ex_ch_L ~ group*sex_parent, data = data))
eta_sq(aov(g_ex_ch_L ~ group*sex_parent, data = data), partial = FALSE, ci.lvl = 0.95)

summary(aov(g_ex_ch_R ~ group*sex_parent, data = data))
eta_sq(aov(g_ex_ch_R ~ group*sex_parent, data = data), partial = FALSE, ci.lvl = 0.95)

summary(aov(l_sbal_sn_sbal ~ group*sex_parent, data = data))
eta_sq(aov(l_sbal_sn_sbal ~ group*sex_parent, data = data), partial = FALSE, ci.lvl = 0.95)

summary(aov(l_sn_prn ~ group*sex_parent, data = data))
eta_sq(aov(l_sn_prn ~ group*sex_parent, data = data), partial = FALSE, ci.lvl = 0.95)

summary(aov(l_sn_sto ~ group*sex_parent, data = data))
eta_sq(aov(l_sn_sto ~ group*sex_parent, data = data), partial = FALSE, ci.lvl = 0.95)

summary(aov(l_n_prn ~ group*sex_parent, data = data))
eta_sq(aov(l_n_prn ~ group*sex_parent, data = data), partial = FALSE, ci.lvl = 0.95)


## after controlling for age and facial area

summary(aov(g_n_sto ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(g_n_sto ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = FALSE, ci.lvl = 0.95)
# d = .3517, 95% CI[.14, .56]

summary(aov(g_ex_ex ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(g_ex_ex ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = FALSE, ci.lvl = 0.95)
# d = .3136, 95% CI[.11, .52]

summary(aov(g_ft_ft_log ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(g_ft_ft_log ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = FALSE, ci.lvl = 0.95)
# d = .3809, 95% CI[.17, .59]

summary(aov(g_sto_pg_log ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(g_sto_pg_log ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = FALSE, ci.lvl = 0.95)
# d = .1097, 95% CI[0, .31]

summary(aov(g_ex_ch_L ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(g_ex_ch_L ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = FALSE, ci.lvl = 0.95)
# d = .2295, 95% CI[.1, .44]

summary(aov(g_ex_ch_R ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(g_ex_ch_R ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = FALSE, ci.lvl = 0.95)
# d = .2708, 95% CI[.06, .48]

summary(aov(l_sbal_sn_sbal ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(l_sbal_sn_sbal ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = FALSE, ci.lvl = 0.95)
# d = .2708, 95% CI[.06, .48]

summary(aov(l_sn_prn ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(l_sn_prn ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = FALSE, ci.lvl = 0.95)
# d = .4135, 95% CI[.20, .63]

summary(aov(l_sn_sto ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(l_sn_sto ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = FALSE, ci.lvl = 0.95)
# d = .1679, 95% CI[.01, .38]

summary(aov(l_n_prn ~ group*sex_parent + ageAtScan + facial_area, data = data))
eta_sq(aov(l_n_prn ~ group*sex_parent + ageAtScan + facial_area, data = data), partial = FALSE, ci.lvl = 0.95)
# d = .2783, 95% CI[.06, .48]

#### additional analyses re: sex of proband children ----

### do parents of autistic boys show more masculine faces than those of autistic girls?
summary(aov(masc_score ~ sex_child, data = dataF)) # no
summary(aov(masc_score ~ sex_child, data = dataM)) # no

summary(aov(masc_score ~ sex_child + ageAtScan + facial_area, data = dataF)) # no even after controlling
summary(aov(masc_score ~ sex_child + ageAtScan + facial_area, data = dataM)) # no even after controlling

### predictive analysis - do parents/controls' masculinity score predict autism status of children? (followed steps on http://www.sthda.com/english/articles/36-classification-methods-essentials/146-discriminant-analysis-essentials-in-r/)

## Split the data into training (90%) and test set (10%)
set.seed(1234)

training.samples <- data$group %>%
  createDataPartition(p = 0.9, list = FALSE)

train.data <- data[training.samples, ]

test.data <- data[-training.samples, ]

## Select predictors to be entered into model

train.data <- train.data %>%
  dplyr::select(group, masc_score, g_n_sto, g_ex_ex, g_ft_ft,  g_ex_ch_R, l_sn_prn, l_n_prn) # variables that significantly differed between proband and control parents based on corrected alpha of .005

test.data <- test.data %>%
  dplyr::select(group, masc_score, g_n_sto, g_ex_ex, g_ft_ft, g_ex_ch_R, l_sn_prn, l_n_prn) # variables that significantly differed between proband and control parents based on corrected alpha of .005


## Normalise the data

# Estimate preprocessing parameters
preproc.param <- train.data %>%
  preProcess(method = c("center", "scale"))

# transform the data using the estimated parameters
train.transformed <- preproc.param %>%
  predict(train.data)

test.transformed <- preproc.param %>%
  predict(test.data)

## run Linear Discriminant Analysis

# fit the model
model <- lda(group ~ ., data = train.transformed, cv = TRUE)

# make predictions
predictions <- model %>%
  predict(test.transformed)

# model accuracy
mean(predictions$class == test.transformed$group)

