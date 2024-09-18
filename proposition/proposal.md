Proposition de projet
================
Erika Audet, Anne-Marie Groulx et Allisson Picard

## 1. Introduction

À l’aide d’une base de données qui recense 11200 éruptions volcaniques
sur 13 000 ans, nous aimerions créer une application qui permet
d’évaluer les probabilités d’activités volcaniques futures et de
visualiser la répartition géographique de celles-ci.

Le jeu de données provient du Smithsonian Institution. Considérant
l’énorme intervalle de temps sur lequel se déroulent les événements
volcaniques recensés, les données ont été collectées de différentes
manières. On trouve par exemple des données collectées et datées par
observations historiques, par radiocarbone, par téphrochronologie et par
17 autres méthodes différentes. Cette information est disponible pour
chaque observation dans le jeu de données sous la variable
Evidence_method_dating.

Nous avons pris les données sur Kaggle et il s’agit d’une version de
2020. L’avantage de travailler avec ces données plutôt que celles
provenant directement du Smithsonian est qu’elles sont déjà *tidy*. Le
jeu de données contient 5 tables, mais nous pensons nous en tenir à 3
d’entre elles, soit:

- eruption.csv : Recensement des éruptions volcaniques.
- event.csv : Recensement de chaque type d’événement par éruption. Par
  exemple, l’éruption 10011 contient 6 événements différents: Explosion,
  coulée de lave, etc…
- volcano.csv : Information géographique et géologique sur les volcans.

## 2. Données

Les jeux de données sont dans le dossier ‘data’.  
Voir le fichier README du dossier data pour voir le détail sur les
variables.

|                                                  |           |
|:-------------------------------------------------|:----------|
| Name                                             | eruptions |
| Number of rows                                   | 11178     |
| Number of columns                                | 15        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |           |
| Column type frequency:                           |           |
| character                                        | 4         |
| numeric                                          | 11        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |           |
| Group variables                                  | None      |

Data summary

**Variable type: character**

| skim_variable          | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:-----------------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| volcano_name           |         0 |          1.00 |   3 |  37 |     0 |      921 |          0 |
| eruption_category      |         0 |          1.00 |  18 |  20 |     0 |        3 |          0 |
| area_of_activity       |      6484 |          0.42 |   3 |  60 |     0 |     2592 |          0 |
| evidence_method_dating |      1280 |          0.89 |   5 |  25 |     0 |       20 |          0 |

**Variable type: numeric**

| skim_variable   | n_missing | complete_rate |      mean |       sd |        p0 |       p25 |       p50 |       p75 |      p100 | hist  |
|:----------------|----------:|--------------:|----------:|---------:|----------:|----------:|----------:|----------:|----------:|:------|
| volcano_number  |         0 |          1.00 | 300284.37 | 52321.19 | 210010.00 | 263310.00 | 290050.00 | 343030.00 | 600000.00 | ▇▇▁▁▁ |
| eruption_number |         0 |          1.00 |  15666.91 |  3297.61 |  10001.00 |  12817.25 |  15650.50 |  18463.75 |  22355.00 | ▇▇▇▇▅ |
| vei             |      2906 |          0.74 |      1.95 |     1.16 |      0.00 |      1.00 |      2.00 |      2.00 |      7.00 | ▅▇▃▁▁ |
| start_year      |         1 |          1.00 |    622.85 |  2482.17 | -11345.00 |    680.00 |   1847.00 |   1950.00 |   2020.00 | ▁▁▁▁▇ |
| start_month     |       193 |          0.98 |      3.45 |     4.07 |      0.00 |      0.00 |      1.00 |      7.00 |     12.00 | ▇▁▂▁▂ |
| start_day       |       196 |          0.98 |      7.02 |     9.65 |      0.00 |      0.00 |      0.00 |     15.00 |     31.00 | ▇▁▂▁▁ |
| end_year        |      6846 |          0.39 |   1917.33 |   157.65 |   -475.00 |   1895.00 |   1957.00 |   1992.00 |   2020.00 | ▁▁▁▁▇ |
| end_month       |      6849 |          0.39 |      6.22 |     3.69 |      0.00 |      3.00 |      6.00 |      9.00 |     12.00 | ▇▅▇▆▇ |
| end_day         |      6852 |          0.39 |     13.32 |     9.83 |      0.00 |      4.00 |     15.00 |     21.00 |     31.00 | ▇▃▆▃▅ |
| latitude        |         0 |          1.00 |     16.87 |    30.76 |    -77.53 |     -6.10 |     17.60 |     40.82 |     85.61 | ▁▃▇▆▃ |
| longitude       |         0 |          1.00 |     31.57 |   115.25 |   -179.97 |    -77.66 |     55.71 |    139.39 |    179.58 | ▂▃▂▁▇ |

Data summary

|                                                  |        |
|:-------------------------------------------------|:-------|
| Name                                             | events |
| Number of rows                                   | 41322  |
| Number of columns                                | 10     |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |        |
| Column type frequency:                           |        |
| character                                        | 3      |
| numeric                                          | 7      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |        |
| Group variables                                  | None   |

**Variable type: character**

| skim_variable | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:--------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| volcano_name  |         0 |          1.00 |   3 |  37 |     0 |      899 |          0 |
| event_type    |         0 |          1.00 |   3 |  35 |     0 |       56 |          0 |
| event_remarks |     36442 |          0.12 |   2 | 593 |     0 |      342 |          0 |

**Variable type: numeric**

| skim_variable       | n_missing | complete_rate |      mean |       sd |     p0 |       p25 |      p50 |       p75 |   p100 | hist  |
|:--------------------|----------:|--------------:|----------:|---------:|-------:|----------:|---------:|----------:|-------:|:------|
| volcano_number      |         0 |          1.00 | 296428.86 | 48713.19 | 210010 | 263250.00 | 284210.0 | 342090.00 | 600000 | ▇▆▁▁▁ |
| eruption_number     |         0 |          1.00 |  15428.93 |  3190.71 |  10001 |  12693.25 |  15334.5 |  18081.75 |  22352 | ▇▇▇▇▃ |
| eruption_start_year |         0 |          1.00 |    850.53 |  2298.53 | -11345 |   1257.00 |   1884.0 |   1968.00 |   2020 | ▁▁▁▁▇ |
| event_number        |         0 |          1.00 | 127402.19 | 15453.72 | 100001 | 113978.25 | 127832.5 | 141701.75 | 153202 | ▆▇▆▇▇ |
| event_date_year     |     31315 |          0.24 |   1365.65 |  1707.33 |  -9650 |   1773.00 |   1912.0 |   1977.00 |   2020 | ▁▁▁▁▇ |
| event_date_month    |     34190 |          0.17 |      6.38 |     3.52 |      1 |      3.00 |      6.0 |      9.00 |     12 | ▇▅▅▅▇ |
| event_date_day      |     35399 |          0.14 |     15.46 |     8.96 |      1 |      8.00 |     15.0 |     23.00 |     31 | ▇▆▆▆▆ |

Data summary

|                                                  |         |
|:-------------------------------------------------|:--------|
| Name                                             | volcano |
| Number of rows                                   | 958     |
| Number of columns                                | 26      |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |         |
| Column type frequency:                           |         |
| character                                        | 18      |
| numeric                                          | 8       |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |         |
| Group variables                                  | None    |

**Variable type: character**

| skim_variable        | n_missing | complete_rate | min | max | empty | n_unique | whitespace |
|:---------------------|----------:|--------------:|----:|----:|------:|---------:|-----------:|
| volcano_name         |         0 |             1 |   3 |  34 |     0 |      954 |          0 |
| primary_volcano_type |         0 |             1 |   6 |  19 |     0 |       26 |          0 |
| last_eruption_year   |         0 |             1 |   1 |   7 |     0 |      342 |          0 |
| country              |         0 |             1 |   4 |  32 |     0 |       89 |          0 |
| region               |         0 |             1 |   6 |  30 |     0 |       19 |          0 |
| subregion            |         0 |             1 |   4 |  38 |     0 |       98 |          0 |
| tectonic_settings    |         0 |             1 |   7 |  47 |     0 |       11 |          0 |
| evidence_category    |         0 |             1 |  14 |  18 |     0 |        5 |          0 |
| major_rock_1         |         0 |             1 |   6 |  40 |     0 |       10 |          0 |
| major_rock_2         |         0 |             1 |   1 |  40 |     0 |       11 |          0 |
| major_rock_3         |         0 |             1 |   1 |  40 |     0 |       11 |          0 |
| major_rock_4         |         0 |             1 |   1 |  40 |     0 |       11 |          0 |
| major_rock_5         |         0 |             1 |   1 |  40 |     0 |       11 |          0 |
| minor_rock_1         |         0 |             1 |   1 |  40 |     0 |       11 |          0 |
| minor_rock_2         |         0 |             1 |   1 |  40 |     0 |       11 |          0 |
| minor_rock_3         |         0 |             1 |   1 |  40 |     0 |        9 |          0 |
| minor_rock_4         |         0 |             1 |   1 |  23 |     0 |        3 |          0 |
| minor_rock_5         |         0 |             1 |   1 |   1 |     0 |        1 |          0 |

**Variable type: numeric**

| skim_variable            | n_missing | complete_rate |       mean |         sd |        p0 |       p25 |       p50 |        p75 |        p100 | hist  |
|:-------------------------|----------:|--------------:|-----------:|-----------:|----------:|----------:|----------:|-----------:|------------:|:------|
| volcano_number           |         0 |             1 |  298585.33 |   49792.66 | 210010.00 | 263025.00 | 300055.50 |  343088.00 |   390829.00 | ▆▇▇▇▆ |
| latitude                 |         0 |             1 |      14.98 |      31.58 |    -78.50 |     -5.40 |     14.51 |      40.80 |       71.08 | ▁▂▆▇▆ |
| longitude                |         0 |             1 |      23.54 |     109.85 |   -179.97 |    -78.28 |     36.39 |     131.05 |      179.58 | ▃▅▂▃▇ |
| elevation                |         0 |             1 |    1867.03 |    1401.55 |  -2500.00 |    881.00 |   1622.50 |    2548.25 |     6879.00 | ▁▆▇▂▁ |
| population_within_5_km   |         0 |             1 |   47860.46 |  298668.99 |      0.00 |      0.00 |    295.00 |    4642.00 |  5783287.00 | ▇▁▁▁▁ |
| population_within_10_km  |         0 |             1 |   61217.75 |  302385.96 |      0.00 |     23.25 |   1633.50 |   20730.25 |  5783287.00 | ▇▁▁▁▁ |
| population_within_30_km  |         0 |             1 |  304044.35 |  735422.11 |      0.00 |    408.00 |  13918.00 |  256521.00 |  7073814.00 | ▇▁▁▁▁ |
| population_within_100_km |         0 |             1 | 2730174.00 | 5690980.66 |      0.00 |  11397.75 | 354646.00 | 2981102.25 | 40640105.00 | ▇▁▁▁▁ |

## 3. Plan de travail

### Objectif

L’objectif principal de notre application web en lien avec notre jeu de
données est de prédire les années des prochaines éruptions volcaniques
de chaque volcan ainsi que leur force à l’aide d’un modèle prédictif.
L’utilisateur de l’application pourrait choisir un volcan précis ou
certains critères géographiques pour que des probabilités d’événements
et une prévision d’intensité lui soit retournées.

Si le temps le permet, nous aimerions aussi faire un volet
cartographique. L’utilisateur pourrait indiquer un endroit pour que
l’application lui retourne le volcan le plus près et ses probabilités
d’activité volcanique. Il serait aussi possible de cartographier
l’intensité de l’activité volcanique (par force et par fréquence) à
l’aide d’un code de couleurs. L’utilisateur pourrait filtrer par années,
par altitude, par longitude, etc… et voir cette carte se mettre à jour
en fonction des critères sélectionnés. Il pourrait aussi être
intéressant de faire une animation qui présente l’évolution de
l’intensité et de la fréquence d’activité volcanique dans le temps.

### Statistiques descriptives et exploration de données

Pour analyser les données de façon très préliminaire, nous pensons faire
des diagrammes en boîte pour les variables numériques suivantes: Année
d’éruption et indice de force. On pourrait aussi faire des diagrammes à
barres qui présentent la fréquence de chaque type d’éruption. Des
histogrammes de la fréquences des éruptions dans le temps pourrait aussi
être pertinents. Toutes ces analyses pourraient être faites sur
l’ensemble des données et marginalement par continent, par volcan ou
selon d’autres variable catégorielle.

### Calculs sur les données

Il pourrait être pratique de créer une nouvelle variable qui calcule le
temps écoulé entre 2 événements consécutifs pour chaque volcan (voir la
section [Processus de dénombrement](#processus-de-dénombrement) à ce
sujet). Il pourrait aussi être pratique de fusionner les variables
année, mois et jour pour créer une variable de type date.

### Méthodes anticipées pour la création d’un modèle prédictif

Ce n’est pas encore clair pour nous qu’elle sera la meilleure méthode
pour faire un modèle prédictif, mais voici ce à quoi nous avons pensé
jusqu’à maintenant.

#### Processus de dénombrement

Il s’agirait ici de modéliser la loi que suit la variable T, soit le
temps écoulé entre 2 événements. À partir de cette loi, on pourrait
calculer les probabilités qu’un nouvel événement survienne dans un
intervalle donné. Pour trouver les paramètres de la loi de T, on
pourrait faire une interface dans laquelle l’utilisateur peut comparer
la répartition des temps écoulés entre 2 éruptions pour un volcan donné
et les fonctions de densité des lois théoriques. Une fois la loi choisie
pour T, l’utilisateur peut calculer des probabilités.

**Exemple où un utilisateur visualise la distribution de T et y ajuste
une exponentielle(1).**
![](proposal_files/figure-gfm/loi%20T-1.png)<!-- -->

Il peut ensuite voir le processus de dénombrement associée à la loi de T
choisie:
![](proposal_files/figure-gfm/processus%20dénombrement-1.png)<!-- -->

Et finalement calculer des probabilités:

Si $T\sim Exp(a)$, alors le nombre d’événements au temps t
$N(t)\sim Poisson(at)$. Ici, on aurait que $T\sim Exp(1)$.  
Quelle est la probabilité que le nombre d’événements entre les temps 100
et 150 \> 60?  
$N(t)-N(s)\sim Poisson(a(t-s))$ -\> $N(150)-N(100)\sim Poisson(50))$  
P(N(150)-N(100)) \> 60) = 0.07216018  

``` r
ppois(60, lambda=50, lower = FALSE)
```

    ## [1] 0.07216018

#### Modèle linéaire généralisé

Nous pourrions tenter d’adapter un modèle linéaire généralisé pour
prédire l’indice de force d’une éruption. Nous ferons les tests
appropriés sur les variables explicatives pour trouver celles qui sont
les plus significatives.
