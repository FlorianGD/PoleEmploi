library(tidyverse)
library(lubridate)
library(broom)
options("scipen"=10, digits = 5) 

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

ggplot(data = offres_OEE %>% filter(year(Periode) >= 2005,
                                    categorie %in% c("OEE_A", "OEE_B")), 
       aes(Periode, nombre, color = categorie)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess") +
  labs(title = "Nombre d'offres d'emplois par mois", 
       subtitle = "De 2005 à 2015, par catégorie\nA est une offre d'emploi de plus de 6 mois, B entre 1 et 6 mois",
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

ggplot(offres_long %>% filter(substr(categorie, 1, 2) == "To"),
       aes(Periode, nombre)) +
  geom_line(color = "tomato") +
  geom_smooth(method = "loess") +
  facet_wrap(~ categorie, scales = "free_y", ncol = 1) +
  theme_minimal()

# Plus grande variabilité dans les DOM (avec un volume plus faible), tendances
# semblables
# Vérifions à partir d'un modèle linéaire simple

offres_totales <- offres %>% 
  select(Periode, starts_with("Tot"))

mod_lm_France <- lm(Total_France ~ Periode, offres_totales)
mod_lm_France_df <- tidy(mod_lm_France)
mod_lm_DOM <- lm(Total_DOM ~ Periode, offres_totales)
mod_lm_DOM_df <- tidy(mod_lm_DOM)

models <- bind_rows("France" = mod_lm_France_df,"DOM" = mod_lm_DOM_df, .id = "Perimetre")
models
# Sur la période, nous ne voyons pas la même tendance : positive et relativement
# bonne (p.value ~5e-3) pour la France, mais légèrement négative pour les DOM

offres_tot_groupe <- offres_long %>% 
  filter(substr(categorie, 1, 3) == "Tot") %>%
  mutate(groupe = factor(ifelse(Periode < as.Date("2008-01-01"),
                                   "Avant 2008", "Après 2008"),
                             levels = c("Avant 2008", "Après 2008")))
           
ggplot(offres_tot_groupe, aes(Periode, nombre, color = categorie)) +
  geom_line() +
  facet_grid(categorie ~ groupe, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

# Avant et après la crise de 2008, les tendances sont nettement différentes

offres_nest <- offres_tot_groupe %>% 
  nest(-groupe, - categorie) %>% 
  mutate(models = map(data, ~ lm(nombre ~ Periode, data = .))) %>% 
  mutate(tidied = map(models, tidy)) %>% 
  unnest(tidied)

offres_tot_groupe %>% 
  nest(-groupe, - categorie) %>% 
  mutate(models = map(data, ~ lm(nombre ~ Periode, data = .))) %>% 
  `$`(models) %>% 
  map(summary)

offres_nest %>% 
  group_by(term) %>%
  mutate(p.adjusted = p.adjust(p.value)) %>% 
  select(-statistic, -p.value)
