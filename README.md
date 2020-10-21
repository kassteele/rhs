
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rhs

<!-- badges: start -->

<!-- badges: end -->

## Rijkshuisstijlkleuren

rhs is een eenvoudig R package om de rijkshuisstijlkleuren toegankelijk
te maken in R. Uit de 17 communicatiekleuren van het Rijkshuisstijl kun
je een palet kiezen met een extra palet van 7 grijstinten. Meer
informatie vind je op de
[website](https://www.rijkshuisstijl.nl/basiselementen/basiselementen-online/online-kleuren)
van de Rijksoverheid.

## Installatie

Je installeert rhs van [GitHub](https://github.com/) via:

``` r
# install.packages("devtools")
devtools::install_github("kassteele/rhs")
```

## Voorbeeld

Hieronder een eenvoudig voorbeeld:

``` r
# Koppel het rhs package aan
library(rhs)

# Geef de kleurcodes van twee rijkshuisstijlkleuren
rhs("paars", "groen")
#>     paars     groen 
#> "#42145f" "#39870c"
```
