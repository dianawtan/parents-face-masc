## data analysis script
## written by Diana Tan 26/08/20
## last updated on: 13/01/21

## These analyses compared parents' faces versus controls' faces. 

#### NOTE TO SELF ----

## Select variables that are included in final paper 



#### libraries ----
library(pacman)
p_load(psych, readr, tidyverse, sjstats, pwr)


#### data import ----

data <- read_csv("02_clean_data/02_clean_transf_data.csv")


#### descriptive statistics ----
describeBy(x = data, group = c("group", "sex"), digits = 3)


#### main analyses ----

## comparing between fathers and controls

dataM <- data %>%
  filter(sex == "male") 

t.test(data = dataM, masc_score ~ group)
cohen.d(dataM$masc_score ~ dataM$group)
cohen.d.ci()
## comparing between mothers and controls

dataF <- data %>%
  filter(sex == "female") 

t.test(data = dataF, masc_score ~ group)

