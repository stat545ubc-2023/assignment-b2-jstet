
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pRevalence

pRevalence provides tools for operations (calculating, filtering) based
on the prevalence of one categorical variable related to another.

## Installation

You can install the development version of pRevalence from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("stat545ubc-2023/categorical_prevalence")
```

## Example

To filter a dataset of trees by the number of neighborhoods a tree
species occurs in, you can use the `filter_by_prevalence` function from
the pRevalence package. The function requires two parameters: a limit
and a boolean value indicating whether the prevalence should be at least
the limit (TRUE) or below it (FALSE).

The limit should be specified as a percentage represented by a number
between 0 and 1.

``` r
library(pRevalence)
library(tidyverse)
library(datateachr)
filtered_data <- vancouver_trees %>% filter_by_prevalence(neighbourhood_name, genus_name, limit=0.2, at_least = TRUE)
```

## Dev Setup

Check package with

    devtools::check()

Run tests with

    testthat::test()
