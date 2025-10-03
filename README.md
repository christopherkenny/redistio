
<!-- README.md is generated from README.Rmd. Please edit that file -->

# redistio <a href="http://www.christophertkenny.com/redistio/"><img src="man/figures/logo.png" align="right" height="114" alt="redistio website" /></a>

<!-- badges: start -->

[![r-universe status
badge](https://christopherkenny.r-universe.dev/badges/redistio)](https://christopherkenny.r-universe.dev/ui#package:redistio)
[![](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/christopherkenny/redistio/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/christopherkenny/redistio/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`redistio` provides a point-and-click districting interface powered by
[Shiny](https://shiny.posit.co/) and
[mapgl](https://github.com/walkerke/mapgl). For regular `sf` objects, it
can be used to draw districts and export assignment files. For
`redist_map` objects, algorithmic assistance is enabled for map drawing.

## Installation

You can install the development version of `redistio` from
[GitHub](https://github.com/) with:

``` r
pak::pak('christopherkenny/redistio')
```

## Example

The most basic application of `redistio` starts with an `sf` tibble and
a column of district assignments.

``` r
library(redistio)

draw(dc, dc$ward)
```

![](man/figures/example_interactive.png)
