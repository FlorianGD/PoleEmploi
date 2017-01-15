Analyse des données ouvertes de pôle emploi
================
Florian Gaudin-Delrieu
2017-01-15

Des données de pôle emploi sont accessibles en open data. En particulier, les données relatives aux offres d'emploi sont [disponibles ici.](http://www.pole-emploi.org/informations/open-data-pole-emploi-@/25799/view-category-25799.html?)

Les données sont sous la forme d'un fichier excel avec 2 feuilles, dont une correspond à la description des variables. Pour pouvoir lire les données dans R, j'ai dû modifier dans OpenOffice le fichier et l'exporter au format csv (certaines cellules étaient fusionnées, et le formattage des nombres amenait des problèmes). Le fichier `offres.csv` fourni provient directement du fichier Excel. Les intitulés de colonnes ont été modifiés pour être plus facilement lisibles : Periode (sans accent), puis OEE\_A, OEE\_B et OEE\_C (au lieu d'un nom plus long et avec espace), et enfin Total\_France et Total\_DOM. Chargeons les libraries nécessaires

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
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(broom)
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

Regardons le `summary`

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

Où sont les NA's ?

``` r
offres[is.na(offres$OEE_A), ]
```

    ## # A tibble: 1 × 6
    ##      Periode OEE_A OEE_B OEE_C Total_France Total_DOM
    ##       <date> <int> <int> <int>        <int>     <int>
    ## 1 2015-12-01    NA    NA    NA           NA        NA

Ils sont sur la même ligne, décembre 2015 et correspondent à des données non encore disponibles, nous pouvons supprimer la ligne sereinement.

``` r
offres <- filter(offres, !is.na(OEE_A))
```

Passons maintenant le tibble sous forme longue, plus pratique pour les graphiques, à l'aide de `tidyr::gather`.

``` r
offres_long <- offres %>%
  gather(categorie, nombre, -Periode)
offres_long$categorie <- as.factor(offres_long$categorie)
```

Premier graphe rapide

``` r
ggplot(data = offres, aes(Periode, Total_France))+
  geom_line()
```

![](offres_emplois_files/figure-markdown_github/Graphe%20Total%20France-1.png)

``` r
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
```

![](offres_emplois_files/figure-markdown_github/Graphe%20Total%20France-2.png)

``` r
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
```

![](offres_emplois_files/figure-markdown_github/Graphe%20Total%20France-3.png)

``` r
ggplot(offres_long %>% filter(substr(categorie, 1, 2) == "To"),
       aes(Periode, nombre)) +
  geom_line(color = "tomato") +
  geom_smooth(method = "loess") +
  facet_wrap(~ categorie, scales = "free_y", ncol = 1) +
  theme_minimal()
```

![](offres_emplois_files/figure-markdown_github/Graphe%20Total%20France-4.png)

``` r
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
```

    ##   Perimetre        term       estimate    std.error statistic    p.value
    ## 1    France (Intercept) 213958.4693822 12182.347509  17.56299 8.6677e-45
    ## 2    France     Periode      2.6119171     0.917017   2.84828 4.7818e-03
    ## 3       DOM (Intercept)   5680.6452620   340.133194  16.70124 6.4813e-42
    ## 4       DOM     Periode     -0.0063392     0.025603  -0.24759 8.0466e-01

``` r
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
```

![](offres_emplois_files/figure-markdown_github/Graphe%20Total%20France-5.png)

``` r
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
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = nombre ~ Periode, data = .)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -30874 -10565     13   9358  40941 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   637.72   11385.18    0.06     0.96    
    ## Periode        21.28       0.97   21.94   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 14700 on 142 degrees of freedom
    ## Multiple R-squared:  0.772,  Adjusted R-squared:  0.771 
    ## F-statistic:  481 on 1 and 142 DF,  p-value: <2e-16
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = nombre ~ Periode, data = .)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -45749 -11436    115  14211  37166 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 654008.54   36651.67    17.8   <2e-16 ***
    ## Periode        -26.58       2.39   -11.1   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19400 on 93 degrees of freedom
    ## Multiple R-squared:  0.571,  Adjusted R-squared:  0.566 
    ## F-statistic:  124 on 1 and 93 DF,  p-value: <2e-16
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = nombre ~ Periode, data = .)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1212.8  -339.2    -1.7   281.1  2741.0 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 424.5559   421.5215    1.01     0.32    
    ## Periode       0.4525     0.0359   12.60   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 545 on 142 degrees of freedom
    ## Multiple R-squared:  0.528,  Adjusted R-squared:  0.525 
    ## F-statistic:  159 on 1 and 142 DF,  p-value: <2e-16
    ## 
    ## 
    ## [[4]]
    ## 
    ## Call:
    ## lm(formula = nombre ~ Periode, data = .)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ##  -1883   -333     -8    304   1217 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 18012.1316   987.0489    18.2   <2e-16 ***
    ## Periode        -0.8216     0.0644   -12.8   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 524 on 93 degrees of freedom
    ## Multiple R-squared:  0.637,  Adjusted R-squared:  0.633 
    ## F-statistic:  163 on 1 and 93 DF,  p-value: <2e-16

``` r
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
    ## 1 Total_France Avant 2008 (Intercept)    637.72311 11385.183029 9.5541e-01
    ## 2 Total_France Avant 2008     Periode     21.27599     0.969750 7.3947e-47
    ## 3 Total_France Après 2008 (Intercept) 654008.53612 36651.673348 2.6237e-31
    ## 4 Total_France Après 2008     Periode    -26.58024     2.390539 9.0873e-19
    ## 5    Total_DOM Avant 2008 (Intercept)    424.55590   421.521506 6.3111e-01
    ## 6    Total_DOM Avant 2008     Periode      0.45245     0.035904 1.9606e-24
    ## 7    Total_DOM Après 2008 (Intercept)  18012.13155   987.048920 6.8771e-32
    ## 8    Total_DOM Après 2008     Periode     -0.82162     0.064378 7.4801e-22
