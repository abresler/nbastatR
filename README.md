nbastatR
================

## Installation

``` r
devtools::install_github("abresler/nbastatR")
```

# nbastatR <img src="https://asbcllc.com/logos/nbastatR.png" align="right" />

A package to help you master the NBA data universe in R.

## Wrappers

  - NBA Stats API
  - Basketball-Reference
  - HoopsHype
  - nbadraft.net
  - RealGM
  - Basketball Insiders

## Parallel Computing

This package now supports parallel computing for all iterative
functions. In order to utilize this, just run \``future::plan` with your
selected method.

For example, to use the multiprocess function:

``` r
library(nbastatR)
library(future)
plan(multiprocess) 
game_logs(seasons = 2010:2019)
```
