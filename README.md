
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wikidataESR

<!-- badges: start -->

<!-- badges: end -->

The goal of wikidataESR is to …

## Installation

<!--
You can install the released version of **openESR** from [CRAN](https://CRAN.R-project.org) with:


```r
install.packages("openESR")
```

Or-->

You can install the development version from GitHub with:

``` r
# install.packages("devtools")
remotes::install_github("nfrerebeau/DataESR")
```

## Usage

``` r
# Load package
library(WikidataESR)
```

### Get Data

Pour récupérer les données, vous devez disposer d’un identifiant
wikidata racine, et savoir quelles
[propriétés](https://github.com/juliengossa/DataESR/tree/master/etablissements.esr#liste-proprietes)
vous souhaitez explorer.

L’exploration se fait avec la fonction `wdesr_get_graph`.

Par exemple, il est possible d’explorer les établissements du Site
Alsace :

  - L’*identifiant wikidata* est
    [Q61716176](https://www.wikidata.org/wiki/Q61716176) ;
  - Les *propriétés* sont “composante” et “associé” ;
  - Il est possible de fixer une *profondeur* pour l’exploration.

<!-- end list -->

``` r
alsace <- wdesr_get_graph("Q61716176", c('composante','associé'), 1)
```

### Tracer des représentations

Il est possible de tracer directement le graphe retourné par
`wdesr_get_graph` :

``` r
plot(alsace)
```

![](man/figures/README-plot-1.png)<!-- -->

### Plus de profondeur

Il est souvent utile d’aller plus en profondeur pour explorer plus de
relations. Cela se fait en modifiant l’argument `depth`
:

``` r
bordeaux <- wdesr_get_graph("Q16541346", c('composante', 'associé'), depth = 2)
plot(bordeaux)
```

![](man/figures/README-wdesr-depth-1.png)<!-- -->

### Gestion du cache

Afin de ne pas avoir à retélécharger systématiquement les données,
`wikidataESR` utilise un cache local. Il est possible de sauvegarder ce
cache pour une utilisation future avec `wdesr_save_cache()`.

Il suffira ensuite de recharger le cache à la prochaine session pour ne
pas retélécharger les données avec `wdesr_load_cache()`. Attention : en
cas de modification des données sur wikidata, les données du cache local
peuvent être périmées.

Il est enfin possible d’accéder directement au cache avec
`wdesr_get_cache()`.
