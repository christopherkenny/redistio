# Guess and estimate which columns contain demographic data

Guess and estimate which columns contain demographic data

## Usage

``` r
guesstimate_demographics(shp)
```

## Arguments

- shp:

  an `sf` tibble that you want to draw with

## Value

a named `list` of columns

## Examples

``` r
guesstimate_demographics(dc)
#> # A tibble: 143 × 16
#>    pct_pop_hisp pct_pop_white pct_pop_black pct_pop_aian pct_pop_asian
#>           <dbl>         <dbl>         <dbl>        <dbl>         <dbl>
#>  1       0.0845         0.421        0.344      0.00121         0.103 
#>  2       0.104          0.592        0.0932     0.00232         0.156 
#>  3       0.0936         0.607        0.0578     0.00252         0.197 
#>  4       0.0922         0.670        0.0438     0.000459        0.146 
#>  5       0.0945         0.781        0.0214     0.000829        0.0552
#>  6       0.0943         0.682        0.0533     0.000921        0.110 
#>  7       0.0992         0.655        0.0625     0               0.116 
#>  8       0.0702         0.755        0.0445     0.000971        0.0659
#>  9       0.118          0.636        0.0842     0.000344        0.127 
#> 10       0.105          0.682        0.0675     0.000173        0.0909
#> # ℹ 133 more rows
#> # ℹ 11 more variables: pct_pop_nhpi <dbl>, pct_pop_other <dbl>,
#> #   pct_pop_two <dbl>, pct_vap_hisp <dbl>, pct_vap_white <dbl>,
#> #   pct_vap_black <dbl>, pct_vap_aian <dbl>, pct_vap_asian <dbl>,
#> #   pct_vap_nhpi <dbl>, pct_vap_other <dbl>, pct_vap_two <dbl>
```
