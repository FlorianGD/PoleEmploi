#' ---
#' title: "Analyse des données ouvertes de pôle emploi"
#' author: "Florian Gaudin-Delrieu"
#' date: "`r format(Sys.Date())`"
#' output: github_document
#' ---
#'

#' # Offres d'emploi 
#' ## Récupération des données 
#' Des données de pôle emploi sont
#' accessibles en open data. En particulier, les données relatives aux offres
#' d'emploi enregristrées sur pôle emploi sont [disponibles 
#' ici.](http://www.pole-emploi.org/informations/open-data-pole-emploi-@/25799/view-category-25799.html?)
#' 
#' Les données sont sous la forme d'un fichier excel avec 2 feuilles, dont une
#' correspond à la description des variables. Pour pouvoir lire les données dans
#' R, j'ai dû modifier dans OpenOffice le fichier et l'exporter au format csv
#' (certaines cellules étaient fusionnées, et le formattage des nombres amenait
#' des problèmes). Le fichier `offres.csv` fourni provient directement du
#' fichier Excel. Les intitulés de colonnes ont été modifiés pour être plus
#' facilement lisibles : 
#' 
#' * Periode (sans accent) ;
#' * OEE_A, OEE_B et OEE_C ( OEE signifie "Offres d'emploi enregistrées", pour
#' les catégories A, B, et C) ;
#' * Total_France et Total_DOM.  
 

#' Chargeons les libraries nécessaires
library(tidyverse)
library(lubridate)    # Pour la gestion des dates
library(broom)        # Pour la gestion "tidy" des modèles
library(RColorBrewer) # Pour les palettes des graphes
library(GGally)       # Pour la mise à l'échelle (rescale11)
options("scipen"=10, digits = 5) # pour ne pas avoir trop de notation scientifique

#' Grâce à `readr::read_csv2` les colonnes ont directement le bon format (date
#' et numérique), et sont sous forme de tibble.
offres <- read_csv2("offres.csv")

#' ## Nettoyage des données
#' Regardons les données
offres  # Le tibble gère l'affichage pour ne pas remplir la console
summary(offres)

#' Nous voyons qu'il y a des données manquantes (NA), où sont-elles ?
offres[is.na(offres$OEE_A), ]

#' Elles sont sur la même ligne, décembre 2015 et correspondent à des données non
#' encore disponibles, nous pouvons supprimer la ligne sereinement.
offres <- filter(offres, !is.na(OEE_A))

#' Passons maintenant le tibble sous forme longue, plus pratique pour les
#' graphiques, à l'aide de `tidyr::gather`.
offres_long <- offres %>%
  gather(categorie, nombre, -Periode)
offres_long$categorie <- as.factor(offres_long$categorie)
glimpse(offres_long)
head(offres_long)

#' Séparons des jeux de données pour les offres par catégorie, et les offres
#' totales. Nous transformons le nombre d'annonces en milliers d'annonces

offres_OEE <- offres_long %>%
  filter(substr(as.character(.$categorie), 1, 3) == "OEE") %>%
  droplevels() %>% 
  mutate(nombre = nombre / 1000)


offres_totales <- offres_long %>%
  filter(substr(as.character(.$categorie), 1, 3) == "Tot") %>%
  droplevels() %>% 
  mutate(nombre = nombre / 1000)

#' J'aime bien le thème minimal pour les graphes.
theme_set(theme_minimal())

#' ## Premières analyses
#' Regardons d'abord les données globales sur le périmètre France
#+ Graphe Total 

ggplot(offres, aes(Periode, Total_France /1000)) +
  geom_line(color = "tomato") +
  geom_smooth(method = "loess", se = FALSE, span = 0.2) +
  labs(title = "Offres d'emplois en France métropolitaine",
       subtitle = "De 1996 à 2015",
       caption= "Source : pôle emploi",
       x = NULL,
       y = "Nombre d'offres d'emploi (en milliers)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 90))

#' Il y a une croissance des offres jusqu'en 2000, puis une baisse jusqu'en 
#' 2003. Cela correspond à l'éclatement de la bulle internet, suivi du krach 
#' boursier des années 2001-2002. S'ensuit une phase de reprise jusqu'en 2008 et
#' un effondrement très fort correspondant à la crise financière de 2008 (crise des subprimes).
#' Une reprise s'opère dès 2009, jusqu'à mi 2011 où une nouvelle chute importante 
#' s'opère jusqu'en 2013. Cela correspond à la crise de la dette de la zone euro.
#' Depuis une timide reprise semble apparaître.  

#' Regardons maintenant les offres par catégorie. D'après la notice du fichier
#' pôle emploi, voilà à quoi correspondent les catégories :  
#' 
#' * Catégorie A : offre d'emploi durable, pour des contrats de plus de 6 mois ;
#' * Catégorie B : offre d'emploi temporaire, pour des contrats entre 1 et 6 mois ;
#' * Catégorie C : offre d'emploi occasionnel, pour des contrats inférieurs à 1 mois.
#+ GrapheCategories

ggplot(data = offres_OEE, aes(Periode, nombre, color = categorie)) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", span = 0.2) +
  labs(
    title = "Nombre d'offres d'emplois par mois",
    subtitle = "De 1996 à 2015, par catégorie\nA est une offre d'emploi de plus de 6 mois, B entre 1 et 6 mois, et C inférieur à 1 mois",
    caption = "Source : pôle emploi",
    x = NULL,
    y = "Nombre d'offres (milliers)",
    color = "Catégorie"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_brewer(palette = "Set1",
                     breaks = c("OEE_A", "OEE_B", "OEE_C"),
                     labels = LETTERS[1:3]) +
  scale_y_continuous(breaks = seq(0, 150, by = 25)) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")

#' Nous voyons que les offres d'emplois occasionnels (C) représentent un volume
#' bien moindre que les offres d'emplois dits temporaires ou durables. Ils
#' semblent moins affectés par les variations décrites dans les tendances
#' globales (crises de 2000-2002, 2008 et 2011). La tendance de ces offres est 
#' orientée à la baisse depuis 2008.  
#' Si en 1996 les volumes d'offres entre les catégories A et B sont sensiblement
#' différents (2 fois plus d'offres d'emploi de catégorie A que B), à partir des années 2000
#' les volumes sont du même ordre de grandeur. Les offres d'emploi de catégorie B
#' dépassent même celles des catégories A en 2003, avec un chassé croisé depuis.
#' Entre 2005 et 2008 les offres d'emploi de catégorie A progressent le plus, mais
#' seront plus touchées par la crise de 2008. Les offres de catégorie B diminuent 
#' aussi assez fortement, et sont au même niveau début 2009. On constate sur les deux 
#' séries une reprise jusqu'à mi 2011, puis une baisse. La baisse est plus forte
#' pour les emplois de catégorie B, et stagne depuis 2013, alors que les offres 
#' de catégorie A repartent à la hausse depuis 2013.  
#'   

#' Regardons maintenant un peu plus précisément les offres des catégroies A et B
#' depuis 1999 (pour que les offres soient sur une échelle comparable, après
#' la progression forte entre 1996 et 1999 pour les catégories B).
#+ GrapheAetB
ggplot(
  data = offres_OEE %>% filter(categorie %in% c("OEE_A", "OEE_B"),
                               year(Periode) >= 1999),
  aes(Periode, nombre, color = categorie)
) +
  geom_line() +
  geom_smooth(se = FALSE, method = "loess", span = 0.3) +
  labs(
    title = "Nombre d'offres d'emplois par mois",
    subtitle = "De 1999 à 2015, par catégorie\nA est une offre d'emploi de plus de 6 mois, B entre 1 et 6 mois",
    caption = "Source : pôle emploi",
    x = NULL,
    y = "Nombre d'offres (milliers)",
    color = "Catégorie"
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_color_brewer(palette = "Set1",
                     breaks = c("OEE_A", "OEE_B", "OEE_C"),
                     labels = LETTERS[1:3]) +
  scale_y_continuous(breaks = seq(0, 150, by = 25)) +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")

#' Nous voyons mieux les variations, les catégories A subissent plus fortement 
#' la crise de 2008, mais récupèrent mieux depuis 2013. Les catégories B quant à
#' elles stagnent depuis 2013, avec une variabilité qui augmente.

#' ## Modélisation des OEE 
#' Y a-t-il la même tendance, année par année pour toutes les catégories ? Et
#' quelles sont les années/catégories qui se distinguent des autres ?  
#' Pour cela, nous allons  créer des régressions linéaires pour chaque année
#' et chaque catégorie. Cela nécessitera d'utiliser : 
#' 
#' 1. `tidyr::nest` pour créer un tibble avec colonnes de listes, pour regrouper
#'  les données autres que annee et catégorie ;
#' 2. `purrr::map` pour appliquer la régression linéaire `lm` pour chaque année
#' et catégorie ;
#' 3. encore `purrr::map` pour appliquer `broom::tidy` qui met sous forme de tibble
#' chaque modèle (mais toujours dans une colonne de listes) ;
#' 4. et enfin `tidyr::unnest` pour remettre le modèle sous forme de tibble.

offres_modeles <- offres_long %>%
  mutate(annee = year(Periode)) %>%
  nest(-annee, -categorie) %>% 
  mutate(models = map(data, ~ lm(nombre ~ Periode, data = .))) %>%
  mutate(tidied = map(models, tidy)) %>%
  unnest(tidied)

offres_modeles

#' Ce qui nous intéresse ici est le coefficient de la régression (ce qui est dans
#' la colonne term et avec la valeur `Periode`) pour savoir quelle
#' est la tendance (positive, négative, plus ou moins importante)

tendances <- offres_modeles  %>% 
  filter(term == "Periode") %>% 
  select(annee, categorie, tendance = estimate)

#' Comme toutes les courbes ne sont pas sur les mêmes échelles de valeurs, nous
#' allons les remettre à l'échelle entre -1 et 1 pour chaque catégorie avec la
#'fonction `GGally::rescale11`.

tendance_echelle <- tendances %>% 
  group_by(categorie) %>% 
  mutate(tendance = rescale11(tendance))

#' Nous pouvons maintenant faire une "heatmap" pour voir ces évolutions par 
#' année et par catégorie.
#+ Heatmap
ggplot(tendance_echelle, 
       aes(categorie, annee, fill = tendance)) +
  geom_raster() +
  scale_fill_gradientn(colors = brewer.pal(7, "RdYlBu"))+
  scale_y_continuous(breaks = 1996:2015, labels = 1996:2015) +
  scale_x_discrete(breaks = levels(tendance_echelle$categorie),
                   labels = c("A", "B", "C", "DOM", "Total France"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = NULL,
       y = NULL,
       fill = "Evolution\nannuelle\nrelative",
       title = "Evolution annuelle des offres enregitrées d'emploi",
       subtitle = "Calculée relativement pour chaque catégorie",
       caption = "Source : pôle emploi")

#' Nous voyons que les catégories C sont en baisse continue (globalement)
#' depuis 2005 où il y a eu la chute la plus importante.
