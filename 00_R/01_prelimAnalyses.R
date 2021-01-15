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
ggdensity(clean_data$n_sto) # normal
ggdensity(clean_data$ex_ex) # normal
ggdensity(clean_data$ft_ft) # slight positive skew
ggdensity(clean_data$sto_pg) # slight positive skew
ggdensity(clean_data$ex_ch_L) # normal
ggdensity(clean_data$ex_ch_R) # normal
ggdensity(clean_data$sbal_sn_sbal) # normal
ggdensity(clean_data$sn_prn) # normal
ggdensity(clean_data$sn_sto) # normal
ggdensity(clean_data$n_prn) # normal

ggdensity(clean_data$cca_lang_raw) # positive skew
ggdensity(clean_data$cca_prag_raw) # positive skew
ggdensity(clean_data$cca_soceng_raw) # positive skew
ggdensity(clean_data$cca_total_raw) # positive skew


#### data transformation ----


# Since distributions are skewed, variables are transformed using Box-Cox transformation
# create a function for transforming variables using Box Cox procedures

boxCoxTrans <- function(measure, group) {
  model <- boxcox(measure ~ group,
                lambda = seq(-6, 6, 0.1)) # try values -6 to 6 by 0.1 to identify appropriate exponent
  modelDf <- data.frame(model$x, model$y) # Create a data frame with the results
  modelDfOrdered <- modelDf[with(modelDf, order(-modelDf$model.y)), ] # Order the new data frame by decreasing y
  modelDfOrdered[1, ] # Display the lambda with the greatest log likelihood
  lambda <- modelDfOrdered[1, "model.x"]
  
  if(lambda == 0) log10(measure)
  else (measure ^ lambda - 1) / lambda
  
}

# A function to add 1 to all CCA variables so values are non-zero 

addZero <- function(measure) {
  absMeas <- measure + 1
}

ccaVar <- clean_data[ , c("cca_total_raw", "cca_lang_raw", "cca_prag_raw", "cca_soceng_raw")]
transCcaVar <- map(.x = ccaVar, .f = addZero) %>%
  as.data.frame 

transCcaVar <- map(.x = transCcaVar, .f = boxCoxTrans, group = clean_data$sex) %>%
  as.data.frame() %>%
  rename("cca_total_raw_box" = "cca_total_raw",
         "cca_lang_raw_box" = "cca_lang_raw",
         "cca_prag_raw_box" = "cca_prag_raw",
         "cca_soceng_raw_box" = "cca_soceng_raw")

ggdensity(transCcaVar$cca_lang_raw_box) 
ggdensity(transCcaVar$cca_prag_raw_box) 
ggdensity(transCcaVar$cca_soceng_raw_box) 
ggdensity(transCcaVar$cca_total_raw_box) 

clean_data <- bind_cols(clean_data, transCcaVar)

# transform the two face measures that are positively skewed

faceVar <- clean_data %>%
  select("ft_ft", "sto_pg") %>%
  rename("ft_ft_box" = "ft_ft",
         "sto_pg_box" = "sto_pg")

transfaceVar <- map(.x = faceVar, .f = boxCoxTrans, group = clean_data$sex) %>%
  as_tibble()

ggdensity(transfaceVar$ft_ft_box) 
ggdensity(transfaceVar$sto_pg_box) 

clean_data <- bind_cols(clean_data, transfaceVar)

#### data export ----

write_csv(clean_data, "02_clean_data/02_clean_transf_data.csv")
