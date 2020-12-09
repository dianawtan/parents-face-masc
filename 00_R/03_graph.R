

pacman::p_load(ggplot2, readr, tidyverse)

data <- read_csv("02_clean_data/02_clean_transf_data.csv") %>%
  filter(PARENT_ID != "2011125MO")

data$group_new <- paste(data$group, data$sex)
data$normdist <- as.numeric(data$normdist)

asfarParents <- ggplot(data, aes(x = masc_score, y = normdist, colour = group_new)) +
  geom_point(aes(shape = group_new), size = 2) +
  scale_shape_manual(values = c(3, 1, 2, 8)) +
  geom_line(size = 0.8) +
  scale_color_brewer(palette = "Dark2") +
  theme_classic() +
  theme(legend.title = element_blank()) +
  labs(x = "Facial Masculinity Score", y = "Probability Density")

print(asfarParents)


ggsave("asfarParentsMasc.png", 
       plot = asfarParents,
       units = "cm",
       dpi = 1000,
       width = 21,
       height = 10)
