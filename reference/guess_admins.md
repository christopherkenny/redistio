# Guess which columns contain administrative units

Guess which columns contain administrative units

## Usage

``` r
guess_admins(shp)
```

## Arguments

- shp:

  an `sf` tibble that you want to draw with

## Value

a named `list` of types

## Examples

``` r
guess_admins(dc)
#> $admin
#> [1] "vtd"
#> 
#> $subadmin
#> character(0)
#> 
#> $multi
#> [1] "vtd"
#> 
#> $total
#> [1] "vtd"
#> 
```
