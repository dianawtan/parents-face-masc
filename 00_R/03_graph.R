## data visualisation script
## written by Diana Tan 04/02/21
## last updated on: 04/02/21

#### libraries ---- 
library(pacman)
p_load(ggplot2, readr, tidyverse)


#### data import ----
data <- read_csv("02_clean_data/01_clean_data.csv") 

data$group_new <- paste(data$group, data$sex)
data$group_new <- factor(data$group_new, levels = c("Parent male", "Control male", "Parent female", "Control female"))
data$normdist <- as.numeric(data$normdist)


#### probability density graph ----
probden <- ggplot(data, aes(x = masc_score, y = normdist, colour = group_new)) +
  geom_point(aes(shape = group_new), size = 2) +
  scale_shape_manual(values = c(3, 1, 2, 8)) +
  geom_line(size = 0.8) +
  scale_color_brewer(palette = "Dark2") +
  theme_classic() +
  theme(legend.title = element_blank()) +
  labs(x = "Facial Masculinity Score", y = "Probability Density")

print(probden)

ggsave("probden.png", 
       plot = probden,
       units = "cm",
       dpi = 1000,
       width = 21,
       height = 10)

#### violin plot ----
violinplot <- ggplot(data, aes(x = group_new, y = masc_score)) +
  geom_violin() + 
  geom_boxplot() +
  geom_point(position = "jitter")
  

print(violinplot)
