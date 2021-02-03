## data analysis script
## written by Diana Tan 26/08/20
## last updated on: 13/01/21

## These analyses compared parents' faces versus controls' faces. 

#### NOTE TO SELF ----

## Select variables that are included in final paper 



#### libraries ----
library(pacman)
p_load(psych, readr, tidyverse, sjstats, pwr, effsize)


#### data import ----

data <- read_csv("02_clean_data/02_clean_transf_data.csv")


#### descriptive statistics ----
describeBy(x = data, group = c("group", "sex"), digits = 3)


#### main analyses ----

## comparing between fathers (n=58) and controls (n=50)

dataM <- data %>%
  filter(sex == "male") 

dataM$masc_score <- as.numeric(dataM$masc_score)

t.test(data = dataM, masc_score ~ group)
cohen.d(dataM$masc_score ~ dataM$group)
# d = 0.6052191
cohen.d.ci(d = 0.6052191, n1 = 58, n2 = 50)


## comparing between mothers (n=134) and controls (n=113)

dataF <- data %>%
  filter(sex == "female") 

t.test(data = dataF, masc_score ~ group)
cohen.d(dataF$masc_score ~ dataF$group)
# d = 0.3754097
cohen.d.ci(d = 0.3754097, n1 = 134, n2 = 113)

## correlations between masc scores and BAPQ total

corr.test(x = dataM$masc_score, y = dataM$bapq_total, method = "pearson")

corr.test(x = dataF$masc_score, y = dataF$bapq_total, method = "pearson")
