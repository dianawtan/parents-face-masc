## data analysis script
## written by Diana Tan 26/08/20
## last updated on: 04/02/21

## These analyses compared parents' faces versus controls' faces. 

#### libraries ----
library(pacman)
p_load(psych, readr, tidyverse, sjstats, pwr, effsize)


#### data import ----

data <- read_csv("02_clean_data/01_clean_data.csv")


#### descriptive statistics ----
describeBy(x = data, group = c("group", "sex"), digits = 3)


#### check for confounding variables ----

dataM <- data %>%
  filter(sex == "male") 

dataF <- data %>%
  filter(sex == "female") 

corr.test(dataM$masc_score, dataM$ageAtScan) #r = 0.26, p < .001, n = 108
corr.test(dataM$masc_score, dataM$facial_area) #r = 0.24, p = .01. n = 108

corr.test(dataF$masc_score, dataF$ageAtScan) #r = 0.20, p < .001, n = 247
corr.test(dataF$masc_score, dataF$facial_area) #r = 0.38, p = < .001, n = 247


#### main analyses ----

## before controlling for age and facial area
summary(aov(masc_score ~ group*sex, data = data))
summary(aov(masc_score ~ group, data = dataM))
summary(aov(masc_score ~ group, data = dataF))

## after controlling for age and facial area

summary(aov(masc_score ~ group*sex + ageAtScan + facial_area, data = data))
summary(aov(masc_score ~ group + ageAtScan + facial_area, data = dataM))
summary(aov(masc_score ~ group + ageAtScan + facial_area, data = dataF))


