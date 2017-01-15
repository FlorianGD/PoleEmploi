#' ---
#' title: "Analyse des données ouvertes de pôle emploi"
#' author: "Florian Gaudin-Delrieu"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'

#' Des données de pôle emploi sont accessibles en open data. En particulier, les
#' données relatives aux offres d'emploi sont [disponibles
#' ici.](http://www.pole-emploi.org/informations/open-data-pole-emploi-@/25799/view-category-25799.html?)
#' 
#' Les données sont sous la forme d'un fichier excel avec 2 feuilles, dont une correspond à la description des variables.
#' Pour pouvoir lire les données dans R, j'ai dû modifier dans OpenOffice le fichier et l'exporter au format csv (certaines cellules étaient fusionnées, et le formattage des nombres amenait des problèmes).
#' Le fichier `offres.csv` fourni provient directement du fichier Excel.
#' Les intitulés de colonnes ont été modifiés pour être plus facilement lisibles : Periode (sans accent), puis OEE_A, OEE_B et OEE_C (au lieu d'un nom plus long et avec espace), et enfin Total_France et Total_DOM.

#' Chargeons les libraries nécessaires
library(tidyverse)
library(lubridate)
library(broom)
options("scipen"=10, digits = 5) # pour ne pas avoir trop de notation scientifique

#' Grâce à `readr::read_csv2` les colonnes ont directement le bon format (date et numérique), et sont sous forme de tibble.
offres <- read_csv2("offres.csv")

#' Regardons le `summary`
summary(offres)

#' Où sont les NA's ?
offres[is.na(offres$OEE_A), ]

#' Ils sont sur la même ligne, décembre 2015 et correspondent à des données non encore disponibles, nous pouvons supprimer la ligne sereinement.
offres <- filter(offres, !is.na(OEE_A))

#' Passons maintenant le tibble sous forme longue, plus pratique pour les graphiques, à l'aide de `tidyr::gather`.
offres_long <- offres %>%
  gather(categorie, nombre, -Periode)
offres_long$categorie <- as.factor(offres_long$categorie)

#' Premier graphe rapide
#+ Graphe Total France
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
