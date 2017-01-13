library(tidyverse)
options("scipen"=10) 

offres <- read.csv2("offres.csv",stringsAsFactors = FALSE)

offres <-tbl_df(offres)

offres$Periode <- as.Date(offres$Periode)

offres <- filter(offres, !is.na(OEE_A))

offres_long <- offres %>%
  gather(categorie, nombre, -Periode)
offres_long$categorie <- as.factor(offres_long$categorie)

ggplot(data = offres, aes(Periode, Total_France))+
  geom_line()

offres_OEE <- offres_long %>% 
  filter(substr(as.character(.$categorie), 1, 3) == "OEE") %>%
  mutate(nombre = nombre / 1000)
 
ggplot(data = offres_OEE, aes(Periode, nombre, color = categorie)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "Nombre d'offres d'emplois par mois", 
       subtitle = "De 1996 à 2015, par catégorie\nA est une offre d'emploi de plus de 6 mois, B entre 1 et 6 mois, et C inférieur à 1 mois",
       caption = "Source : pôle emploi",
       x = "Année",
       y = "Nombre d'offres (milliers)",
       color = "Catégorie") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_discrete(breaks = c("OEE_A", "OEE_B", "OEE_C"),
                       labels = LETTERS[1:3]) +
  scale_y_continuous(breaks = seq(0, 150, by = 25)) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")

library(lubridate)
ggplot(data = offres_OEE %>% filter(year(Periode) >= 2007), aes(Periode, nombre, color = categorie)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "Nombre d'offres d'emplois par mois", 
       subtitle = "De 1996 à 2015, par catégorie\nA est une offre d'emploi de plus de 6 mois, B entre 1 et 6 mois, et C inférieur à 1 mois",
       caption = "Source : pôle emploi",
       x = "Année",
       y = "Nombre d'offres (milliers)",
       color = "Catégorie") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_discrete(breaks = c("OEE_A", "OEE_B", "OEE_C"),
                       labels = LETTERS[1:3]) +
  scale_y_continuous(breaks = seq(0, 150, by = 25)) +
  theme(axis.text.x = element_text(angle = 0),
        legend.position = "bottom")
