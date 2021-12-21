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

# Since distributions are skewed, variables are transformed using Box-Cox transformation
# create a function for transforming variables using Box Cox procedures
# https://rcompanion.org/handbook/I_12.html

boxCoxTrans <- function(measure) {
  model <- boxcox(measure ~ 1,
                  lambda = seq(-6, 6, 0.1)) # try values -6 to 6 by 0.1 to identify appropriate exponent
  modelDf <- data.frame(model$x, model$y) # Create a data frame with the results
  modelDfOrdered <- modelDf[with(modelDf, order(-modelDf$model.y)), ] # Order the new data frame by decreasing y
  modelDfOrdered[1, ] # Display the lambda with the greatest log likelihood
  lambda <- modelDfOrdered[1, "model.x"]
  
  if(lambda == 0) log10(measure)
  else (measure ^ lambda - 1) / lambda
  
}

# create a dataframe of measures to be transformed

faceVar <- clean_data %>%
  select(g_ft_ft, g_sto_pg)

transFaceVar <- map(.x = faceVar, .f = boxCoxTrans) %>%
  as.data.frame() %>%
  rename("g_ft_ft_box" = "g_ft_ft",
         "g_sto_pg_box" = "g_sto_pg")

# check distribution of transformed variables 

ggdensity(transFaceVar$g_ft_ft_box) # normally distributed
ggdensity(transFaceVar$g_sto_pg_box) # normally distributed 

clean_data <- bind_cols(clean_data, transFaceVar)


#### data export ----

write_csv(clean_data, "02_clean_data/02_clean_transf_data.csv")
