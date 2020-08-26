## data analysis script
## written by Diana Tan 26/08/20

## These analyses compared parents' faces versus controls' faces. 

#### libraries ----
library(pacman)
p_load(psych, readr, tidyverse, effsize, wBoot)


#### data import ----

data <- read_csv("02_clean_data/02_clean_transf_data.csv")

#### data analysis ----

# subsetting data frames
dataF <- data %>%
  filter(sex == "female")
dataM <- data %>%
  filter(sex == "male")

# check for confounding variables

t.test(data = dataF, ageAtScan ~ group)
t.test(data = dataM, ageAtScan ~ group)

t.test(data = dataF, parent_bmi ~ group)
cohen.d(dataF$parent_bmi ~ dataF$group)

t.test(data = dataM, parent_bmi ~ group)
cohen.d(dataM$parent_bmi ~ dataM$group)

t.test(data = dataF, parent_head ~ group)
cohen.d(dataF$parent_head ~ dataF$group)

t.test(data = dataM, parent_head ~ group)
cohen.d(dataM$parent_head ~ dataM$group)

# mothers vs controls

t.test(data = dataF, masc_score ~ group)
cohen.d(dataF$masc_score ~ dataF$group)

summary(aov(data = dataF, masc_score ~ group + parent_head))

# fathers vs controls

t.test(data = dataM, masc_score ~ group)
cohen.d(dataM$masc_score ~ dataM$group)

summary(aov(data = dataM, masc_score ~ group + parent_head))


#### correlations ----

# BAPQ (overall)
corr.test(x = data$masc_score, y = data$bapq_total, method = "pearson")
corr.test(x = data$masc_score, y = data$bapq_aloof, method = "pearson")
corr.test(x = data$masc_score, y = data$bapq_prag, method = "pearson")
corr.test(x = data$masc_score, y = data$bapq_rigid, method = "pearson")
# CC-A (overall)
corr.test(x = data$masc_score, y = data$cca_total_raw_box, method = "pearson")
corr.test(x = data$masc_score, y = data$cca_lang_raw_box, method = "pearson")
corr.test(x = data$masc_score, y = data$cca_prag_raw_box, method = "pearson")
corr.test(x = data$masc_score, y = data$cca_soceng_raw_box, method = "pearson")


# BAPQ (father)
corr.test(x = dataM$masc_score, y = dataM$bapq_total, method = "pearson")
corr.test(x = dataM$masc_score, y = dataM$bapq_aloof, method = "pearson")
corr.test(x = dataM$masc_score, y = dataM$bapq_prag, method = "pearson")
corr.test(x = dataM$masc_score, y = dataM$bapq_rigid, method = "pearson")
# CC-A (father)
corr.test(x = dataM$masc_score, y = dataM$cca_total_raw_box, method = "pearson")
corr.test(x = dataM$masc_score, y = dataM$cca_lang_raw_box, method = "pearson")
corr.test(x = dataM$masc_score, y = dataM$cca_prag_raw_box, method = "pearson")
corr.test(x = dataM$masc_score, y = dataM$cca_soceng_raw_box, method = "pearson")



# BAPQ (mother)
corr.test(x = dataF$masc_score, y = dataF$bapq_total, method = "pearson")
corr.test(x = dataF$masc_score, y = dataF$bapq_aloof, method = "pearson")
corr.test(x = dataF$masc_score, y = dataF$bapq_prag, method = "pearson")
corr.test(x = dataF$masc_score, y = dataF$bapq_rigid, method = "pearson")

# CC-A (mother)
corr.test(x = dataF$masc_score, y = dataF$cca_total_raw_box, method = "pearson")
corr.test(x = dataF$masc_score, y = dataF$cca_lang_raw_box, method = "pearson")
corr.test(x = dataF$masc_score, y = dataF$cca_prag_raw_box, method = "pearson")
corr.test(x = dataF$masc_score, y = dataF$cca_soceng_raw_box, method = "pearson")

# comparing masc between number of BAP features
summary(aov(data = data, masc_score ~ bap_features))
summary(aov(data = dataM, masc_score ~ bap_features))
summary(aov(data = dataF, masc_score ~ bap_features))


# father masc and child ADOS
corr.test(x = dataM$masc_score, y = dataM$CSS_OVERALL_TOTAL, method = "pearson")
corr.test(x = dataM$masc_score, y = dataM$CSS_SA_TOTAL, method = "pearson")
corr.test(x = dataM$masc_score, y = dataM$CSS_RRB_TOTAL, method = "pearson")

# mother masc and child ADOS
corr.test(x = dataF$masc_score, y = dataF$CSS_OVERALL_TOTAL, method = "pearson")
corr.test(x = dataF$masc_score, y = dataF$CSS_SA_TOTAL, method = "pearson")
corr.test(x = dataF$masc_score, y = dataF$CSS_RRB_TOTAL, method = "pearson")



#### descriptive statistics ----
describeBy(x = data, group = c("group", "sex"), digits = 3)
