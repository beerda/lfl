
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/beerda/lfl?branch=master&svg=true)](https://ci.appveyor.com/project/beerda/lfl)
[![codecov](https://codecov.io/gh/beerda/lfl/branch/master/graph/badge.svg)](https://app.codecov.io/gh/beerda/lfl)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/lfl)](https://cran.r-project.org/package=lfl)

# lfl

The lfl package provides various algorithms related to linguistic fuzzy
logic: mining for linguistic fuzzy association rules, composition of
fuzzy relations, performing perception-based logical deduction (PbLD),
and forecasting time-series using fuzzy rule-based ensemble (FRBE). The
package also contains basic fuzzy-related algebraic functions capable of
handling missing values in different styles (Bochvar, Sobocinski, Kleene
etc.), computation of Sugeno integrals and fuzzy transform.

## Installation

To install the stable version from CRAN, simply issue the following
command within your R session:

``` r
install.packages("lfl")
```

If you want to install the development version instead, type:

``` r
install.packages("devtools")
devtools::install_github("beerda/lfl")
```
