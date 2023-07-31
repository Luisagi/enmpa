ENMPA: An R package for Ecological Niche Modeling using Presence-Absence
Data
================
Luis F. Arias-Giraldo, Marlon E. Cobos, A. Town Peterson.

- [Installation](#installation)
- [Example](#example)
  - [Running analyses](#running-analyses)

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->
<hr>

The package `enmpa` comprises a set of tools to perform Ecological Niche
Modeling analysis, including data partitioning, model selection,
calibration, fitting and evaluation.

<br>

## Installation

You can install the development version of enmpa from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Luisagi/enmpa")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(enmpa)
library(terra)

# Load species occurrences and environmental data
pa_data <- read.csv(system.file("extdata", "pa_data.csv", package = "enmpa"))
env_vars <- terra::rast(system.file("extdata", "raster_vars.tif", package = "enmpa"))
```

``` r
head(pa_data)
#>   Pres_abs     bio_1 bio_12
#> 1        0  4.222687    403
#> 2        0  6.006802    738
#> 3        0  4.079385    786
#> 4        1  8.418489    453
#> 5        0  8.573750    553
#> 6        1 16.934618    319
```

``` r
terra::plot(env_vars)
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

### Running analyses
