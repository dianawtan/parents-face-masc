## data visualisation script
## written by Diana Tan 04/02/21
## last updated on: 04/02/21

#### libraries ---- 
library(pacman)
p_load(ggplot2, readr, tidyverse)


#### data import ----
data <- read_csv("02_clean_data/01_clean_data.csv") 

data$group_new <- paste(data$group, data$sex_parent)
data$group_new <- gsub("proband male", "Fathers of Autistic Children", data$group_new)
data$group_new <- gsub("proband female", "Mothers of Autistic Children", data$group_new)
data$group_new <- gsub("control male", "Fathers of Non-Autistic Children", data$group_new)
data$group_new <- gsub("control female", "Mothers of Non-Autistic Children", data$group_new)

data$normdist <- as.numeric(data$normdist)

meanscore <- tibble(group = c("Fathers of Autistic Children", "Fathers of Non-Autistic Children", "Mothers of Autistic Children", "Mothers of Non-Autistic Children"), meanscore = c(0.85, 0.80, 0.32, 0.28))
  



#### probability density graph ----
probden <- ggplot(data = data, aes(x = masc_score, y = normdist, colour = group_new)) +
  geom_point(aes(shape = group_new), size = 2) +
  scale_shape_manual(values = c(3, 1, 2, 8)) +
  geom_line(size = 0.8) +
  geom_vline(data = meanscore, aes(xintercept = meanscore, colour = group), size = 1) +
  scale_color_brewer(palette = "Dark2") +
  theme_classic() +
  theme(legend.title = element_blank()) +
  labs(x = "Facial Masculinity Score", y = "Probability Density") +
  annotate("text", x = 0.19, y = 5, label = "mean = 0.28") +
  annotate("text", x = 0.41, y = 5, label = "mean = 0.32") +
  annotate("text", x = 0.71, y = 5, label = "mean = 0.80") +
  annotate("text", x = 0.95, y = 5, label = "mean = 0.85")

print(probden)


ggsave("probden.tiff", 
       plot = probden,
       units = "cm",
       dpi = 1000,
       width = 25,
       height = 12)

