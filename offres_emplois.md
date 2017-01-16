Analyse des données ouvertes de pôle emploi
================
Florian Gaudin-Delrieu
2017-01-16

Offres d'emploi
===============

Récupération des données
------------------------

Des données de pôle emploi sont accessibles en open data. En particulier, les données relatives aux offres d'emploi enregristrées sur pôle emploi sont [disponibles ici.](http://www.pole-emploi.org/informations/open-data-pole-emploi-@/25799/view-category-25799.html?)

Les données sont sous la forme d'un fichier excel avec 2 feuilles, dont une correspond à la description des variables. Pour pouvoir lire les données dans R, j'ai dû modifier dans OpenOffice le fichier et l'exporter au format csv (certaines cellules étaient fusionnées, et le formattage des nombres amenait des problèmes). Le fichier `offres.csv` fourni provient directement du fichier Excel. Les intitulés de colonnes ont été modifiés pour être plus facilement lisibles :

-   Periode (sans accent) ;
-   OEE\_A, OEE\_B et OEE\_C ( OEE signifie "Offres d'emploi enregistrées", pour les catégories A, B, et C) ;
-   Total\_France et Total\_DOM.
    Chargeons les libraries nécessaires

``` r
library(tidyverse)
```

    ## Loading tidyverse: ggplot2
    ## Loading tidyverse: tibble
    ## Loading tidyverse: tidyr
    ## Loading tidyverse: readr
    ## Loading tidyverse: purrr
    ## Loading tidyverse: dplyr

    ## Conflicts with tidy packages ----------------------------------------------

    ## filter(): dplyr, stats
    ## lag():    dplyr, stats

``` r
library(lubridate)    # Pour la gestion des dates
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(broom)        # Pour la gestion "tidy" des modèles
library(RColorBrewer) # Pour les palettes des graphes
options("scipen"=10, digits = 5) # pour ne pas avoir trop de notation scientifique
```

Grâce à `readr::read_csv2` les colonnes ont directement le bon format (date et numérique), et sont sous forme de tibble.

``` r
offres <- read_csv2("offres.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   Periode = col_date(format = ""),
    ##   OEE_A = col_integer(),
    ##   OEE_B = col_integer(),
    ##   OEE_C = col_integer(),
    ##   Total_France = col_integer(),
    ##   Total_DOM = col_integer()
    ## )

Nettoyage des données
---------------------

Regardons les données

``` r
offres  # Le tibble gère l'affichage pour ne pas remplir la console
```

    ## # A tibble: 240 × 6
    ##       Periode  OEE_A OEE_B OEE_C Total_France Total_DOM
    ##        <date>  <int> <int> <int>        <int>     <int>
    ## 1  1996-01-01 101200 50800 19800       171800      3900
    ## 2  1996-02-01 103100 50000 21100       174200      4300
    ## 3  1996-03-01 102800 49900 23000       175700      5500
    ## 4  1996-04-01 103000 52400 22900       178300      5100
    ## 5  1996-05-01 100900 55900 23900       180700      4800
    ## 6  1996-06-01 104600 60200 23700       188500      4900
    ## 7  1996-07-01 102000 60100 25800       187900      5200
    ## 8  1996-08-01 104000 64200 25700       193900      5600
    ## 9  1996-09-01  99800 65700 28100       193600      5100
    ## 10 1996-10-01  97100 64800 35100       197000      5200
    ## # ... with 230 more rows

``` r
summary(offres)
```

    ##     Periode               OEE_A            OEE_B            OEE_C      
    ##  Min.   :1996-01-01   Min.   : 93200   Min.   : 49900   Min.   :18800  
    ##  1st Qu.:2000-12-24   1st Qu.:103450   1st Qu.: 87750   1st Qu.:32450  
    ##  Median :2005-12-16   Median :109400   Median :102300   Median :37800  
    ##  Mean   :2005-12-15   Mean   :111849   Mean   :100356   Mean   :36017  
    ##  3rd Qu.:2010-12-08   3rd Qu.:118950   3rd Qu.:113650   3rd Qu.:40700  
    ##  Max.   :2015-12-01   Max.   :141600   Max.   :129600   Max.   :50900  
    ##                       NA's   :1        NA's   :1        NA's   :1      
    ##   Total_France      Total_DOM   
    ##  Min.   :171800   Min.   :3700  
    ##  1st Qu.:227100   1st Qu.:5000  
    ##  Median :249900   Median :5600  
    ##  Mean   :248221   Mean   :5598  
    ##  3rd Qu.:269900   3rd Qu.:6200  
    ##  Max.   :311500   Max.   :7900  
    ##  NA's   :1        NA's   :1

Nous voyons qu'il y a des données manquantes (NA), où sont-elles ?

``` r
offres[is.na(offres$OEE_A), ]
```

    ## # A tibble: 1 × 6
    ##      Periode OEE_A OEE_B OEE_C Total_France Total_DOM
    ##       <date> <int> <int> <int>        <int>     <int>
    ## 1 2015-12-01    NA    NA    NA           NA        NA

Elles sont sur la même ligne, décembre 2015 et correspondent à des données non encore disponibles, nous pouvons supprimer la ligne sereinement.

``` r
offres <- filter(offres, !is.na(OEE_A))
```

Passons maintenant le tibble sous forme longue, plus pratique pour les graphiques, à l'aide de `tidyr::gather`.

``` r
offres_long <- offres %>%
  gather(categorie, nombre, -Periode)
offres_long$categorie <- as.factor(offres_long$categorie)
glimpse(offres_long)
```

    ## Observations: 1,195
    ## Variables: 3
    ## $ Periode   <date> 1996-01-01, 1996-02-01, 1996-03-01, 1996-04-01, 199...
    ## $ categorie <fctr> OEE_A, OEE_A, OEE_A, OEE_A, OEE_A, OEE_A, OEE_A, OE...
    ## $ nombre    <int> 101200, 103100, 102800, 103000, 100900, 104600, 1020...

``` r
head(offres_long)
```

    ## # A tibble: 6 × 3
    ##      Periode categorie nombre
    ##       <date>    <fctr>  <int>
    ## 1 1996-01-01     OEE_A 101200
    ## 2 1996-02-01     OEE_A 103100
    ## 3 1996-03-01     OEE_A 102800
    ## 4 1996-04-01     OEE_A 103000
    ## 5 1996-05-01     OEE_A 100900
    ## 6 1996-06-01     OEE_A 104600

Séparons des jeux de données pour les offres par catégorie, et les offres totales. Nous transformons le nombre d'annonces en milliers d'annonces

``` r
offres_OEE <- offres_long %>%
  filter(substr(as.character(.$categorie), 1, 3) == "OEE") %>%
  droplevels() %>% 
  mutate(nombre = nombre / 1000)


offres_totales <- offres_long %>%
  filter(substr(as.character(.$categorie), 1, 3) == "Tot") %>%
  droplevels() %>% 
  mutate(nombre = nombre / 1000)
```

J'aime bien le thème minimal pour les graphes.

``` r
theme_set(theme_minimal())
```

Premières analyses
------------------

Regardons d'abord les données globales sur le périmètre France

``` r
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
```

![](offres_emplois_files/figure-markdown_github/Graphe%20Total-1.png)

Il y a une croissance des offres jusqu'en 2000, puis une baisse jusqu'en 2003. Cela correspond à l'éclatement de la bulle internet, suivi du krach boursier des années 2001-2002. S'ensuit une phase de reprise jusqu'en 2008 et un effondrement très fort correspondant à la crise financière de 2008 (crise des subprimes). Une reprise s'opère dès 2009, jusqu'à mi 2011 où une nouvelle chute importante s'opère jusqu'en 2013. Cela correspond à la crise de la dette de la zone euro. Depuis une timide reprise semble apparaître.
Regardons maintenant les offres par catégorie. D'après la notice du fichier pôle emploi, voilà à quoi correspondent les catégories :

-   Catégorie A : offre d'emploi durable, pour des contrats de plus de 6 mois ;
-   Catégorie B : offre d'emploi temporaire, pour des contrats entre 1 et 6 mois ;
-   Catégorie C : offre d'emploi occasionnel, pour des contrats inférieurs à 1 mois.

``` r
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
```

![](offres_emplois_files/figure-markdown_github/GrapheCategories-1.png)

Nous voyons que les offres d'emplois occasionnels (C) représentent un volume bien moindre que les offres d'emplois dits temporaires ou durables. Ils semblent moins affectés par les variations décrites dans les tendances globales (crises de 2000-2002, 2008 et 2011). La tendance de ces offres est orientée à la baisse depuis 2008.
Si en 1996 les volumes d'offres entre les catégories A et B sont sensiblement différents (2 fois plus d'offres d'emploi de catégorie A que B), à partir des années 2000 les volumes sont du même ordre de grandeur. Les offres d'emploi de catégorie B dépassent même celles des catégories A en 2003, avec un chassé croisé depuis. Entre 2005 et 2008 les offres d'emploi de catégorie A progressent le plus, mais seront plus touchées par la crise de 2008. Les offres de catégorie B diminuent aussi assez fortement, et sont au même niveau début 2009. On constate sur les deux séries une reprise jusqu'à mi 2011, puis une baisse. La baisse est plus forte pour les emplois de catégorie B, et stagne depuis 2013, alors que les offres de catégorie A repartent à la hausse depuis 2013.

Regardons maintenant un peu plus précisément les offres des catégroies A et B depuis 1999 (pour que les offres soient sur une échelle comparable, après la progression forte entre 1996 et 1999 pour les catégories B).

``` r
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
```

![](offres_emplois_files/figure-markdown_github/GrapheAetB-1.png)

Nous voyons mieux les variations, les catégories A subissent plus fortement la crise de 2008, mais récupèrent mieux depuis 2013. Les catégories B quant à elles stagnent depuis 2013, avec une variabilité qui augmente. \#\# Modélisation des OEE Essayons de faire un modèle simple pour les OEE par catégorie. \#\#\# Modèle linéaire simple : France entière

``` r
modele_france <- lm(nombre ~ Periode,
                    data = filter(offres_totales, categorie == "Total_France"))
summary(modele_france)
```

    ## 
    ## Call:
    ## lm(formula = nombre ~ Periode, data = filter(offres_totales, 
    ##     categorie == "Total_France"))
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -66.96 -21.96   3.89  21.71  61.13 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 213.958469  12.182348   17.56   <2e-16 ***
    ## Periode       0.002612   0.000917    2.85   0.0048 ** 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 29.8 on 237 degrees of freedom
    ## Multiple R-squared:  0.0331, Adjusted R-squared:  0.029 
    ## F-statistic: 8.11 on 1 and 237 DF,  p-value: 0.00478

Le modèle n'est pas très bon (avec un R<sup>2</sup> de 0.029), mais la tendance qui ressort est à la hausse (le coefficient est légèrement positif : 0.003).

``` r
annees_changement <- c(1996, 2000, 2003, 2008, 2009, 2011, 2013, 2015)

dates_changement <- offres %>%
  select(Periode, Total_France) %>%
  mutate(annee = year(Periode)) %>%
  filter(annee %in% annees_changement) %>% 
  group_by(annee) %>%
  summarise(mois_max = which.max(Total_France),
            mois_min = which.min(Total_France),
            val      = mean(Total_France)) %>% 
  mutate(evol = sign(lead(val) - val),
         mois = ifelse(evol == 1, mois_min, mois_max),
         Periode = as_date(paste(annee, mois, "01", sep ="-"))) %>% 
  filter(!is.na(Periode)) %>% 
  `$`(Periode) #pour avoir un vecteur

dates_changement <- c(dates_changement, as_date("2015-12-01"))


offres_totales$intervalle <- cut(offres$Periode, dates_changement)

ggplot(offres_totales %>% filter(categorie == "Total_France"), 
       aes(Periode, nombre, color = intervalle)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE)
```

![](offres_emplois_files/figure-markdown_github/unnamed-chunk-10-1.png)

France et DOM
-------------

Y a-t-il les mêmes variations entre la France et les DOM ? Nous ne disposons que des données totales et non par catégorie.

``` r
# Plus grande variabilité dans les DOM (avec un volume plus faible), tendances
# semblables
# Vérifions à partir d'un modèle linéaire simple


mod_lm_France <- lm(Total_France ~ Periode, offres)
mod_lm_France_df <- tidy(mod_lm_France)
mod_lm_DOM <- lm(Total_DOM ~ Periode, offres)
mod_lm_DOM_df <- tidy(mod_lm_DOM)

models <- bind_rows("France" = mod_lm_France_df,"DOM" = mod_lm_DOM_df, .id = "Perimetre")
models
```

    ##   Perimetre        term       estimate    std.error statistic    p.value
    ## 1    France (Intercept) 213958.4693822 12182.347509  17.56299 8.6677e-45
    ## 2    France     Periode      2.6119171     0.917017   2.84828 4.7818e-03
    ## 3       DOM (Intercept)   5680.6452620   340.133194  16.70124 6.4813e-42
    ## 4       DOM     Periode     -0.0063392     0.025603  -0.24759 8.0466e-01

``` r
# Sur la période, nous ne voyons pas la même tendance : positive et relativement
# bonne (p.value ~5e-3) pour la France, mais légèrement négative pour les DOM

offres_tot_groupe <- offres_totales %>%
  mutate(groupe = factor(ifelse(Periode < as.Date("2008-01-01"),
                                   "Avant 2008", "Après 2008"),
                             levels = c("Avant 2008", "Après 2008")))

ggplot(offres_tot_groupe, aes(Periode, nombre, color = categorie)) +
  geom_line() +
  facet_grid(categorie ~ groupe, scales = "free") +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()
```

![](offres_emplois_files/figure-markdown_github/unnamed-chunk-11-1.png)

``` r
# Avant et après la crise de 2008, les tendances sont nettement différentes

offres_nest <- offres_tot_groupe %>%
  nest(-groupe, - categorie) %>%
  mutate(models = map(data, ~ lm(nombre ~ Periode, data = .))) %>%
  mutate(tidied = map(models, tidy)) %>%
  unnest(tidied)

offres_nest %>%
  group_by(term) %>%
  mutate(p.adjusted = p.adjust(p.value)) %>%
  select(-statistic, -p.value)
```

    ## Source: local data frame [8 x 6]
    ## Groups: term [2]
    ## 
    ##      categorie     groupe        term     estimate    std.error p.adjusted
    ##         <fctr>     <fctr>       <chr>        <dbl>        <dbl>      <dbl>
    ## 1 Total_France Avant 2008 (Intercept)   0.63772311 11.385183029 9.5541e-01
    ## 2 Total_France Avant 2008     Periode   0.02127599  0.000969750 7.3947e-47
    ## 3 Total_France Après 2008 (Intercept) 654.00853612 36.651673348 2.6237e-31
    ## 4 Total_France Après 2008     Periode  -0.02658024  0.002390539 9.0873e-19
    ## 5    Total_DOM Avant 2008 (Intercept)   0.42455590  0.421521506 6.3111e-01
    ## 6    Total_DOM Avant 2008     Periode   0.00045245  0.000035904 1.9606e-24
    ## 7    Total_DOM Après 2008 (Intercept)  18.01213155  0.987048920 6.8771e-32
    ## 8    Total_DOM Après 2008     Periode  -0.00082162  0.000064378 7.4801e-22
