# Guess which columns contain election data

Guess which columns contain election data

## Usage

``` r
guess_elections(shp)
```

## Arguments

- shp:

  an `sf` tibble that you want to draw with

## Value

a named `list` of columns

## Examples

``` r
guess_elections(dc)
#> $pre_16
#> $pre_16$dem
#> [1] "pre_16_dem_cli"
#> 
#> $pre_16$rep
#> [1] "pre_16_rep_tru"
#> 
#> 
#> $pre_20
#> $pre_20$dem
#> [1] "pre_20_dem_bid"
#> 
#> $pre_20$rep
#> [1] "pre_20_rep_tru"
#> 
#> 
#> $uss_20
#> $uss_20$dem
#> [1] "uss_20_dem_str"
#> 
#> $uss_20$rep
#> [1] "uss_20_rep_wei"
#> 
#> 
```
