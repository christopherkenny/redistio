# Create a `tibble` of precinct stats

Create a `tibble` of precinct stats

## Usage

``` r
hover_precinct(shp, ...)
```

## Arguments

- shp:

  a [tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)
  with precinct stats

- ...:

  named tidyselections

## Value

A [tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html)

## Examples

``` r
hover_precinct(dc, 1, pop = dplyr::starts_with('pop'), vap = dplyr::starts_with('vap'))
#> [[1]]
#> # A tibble: 1 × 144
#>   rowname V1         V2    V3    V4    V5    V6    V7    V8    V9    V10   V11  
#>   <chr>   <chr>      <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr> <chr>
#> 1 GEOID20 1100102-0… 1100… 1100… 1100… 1100… 1100… 1100… 1100… 1100… 1100… 1100…
#> # ℹ 132 more variables: V12 <chr>, V13 <chr>, V14 <chr>, V15 <chr>, V16 <chr>,
#> #   V17 <chr>, V18 <chr>, V19 <chr>, V20 <chr>, V21 <chr>, V22 <chr>,
#> #   V23 <chr>, V24 <chr>, V25 <chr>, V26 <chr>, V27 <chr>, V28 <chr>,
#> #   V29 <chr>, V30 <chr>, V31 <chr>, V32 <chr>, V33 <chr>, V34 <chr>,
#> #   V35 <chr>, V36 <chr>, V37 <chr>, V38 <chr>, V39 <chr>, V40 <chr>,
#> #   V41 <chr>, V42 <chr>, V43 <chr>, V44 <chr>, V45 <chr>, V46 <chr>,
#> #   V47 <chr>, V48 <chr>, V49 <chr>, V50 <chr>, V51 <chr>, V52 <chr>, …
#> 
#> $pop
#> # A tibble: 9 × 144
#>   rowname      V1    V2    V3    V4    V5    V6    V7    V8    V9   V10   V11
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 pop        9944  6908  6354  6532  4823 10853  3327  6178  5809  5795  7794
#> 2 pop_hisp    840   715   595   602   456  1023   330   434   684   608   836
#> 3 pop_white  4185  4090  3855  4375  3765  7405  2178  4662  3692  3952  5275
#> 4 pop_black  3418   644   367   286   103   579   208   275   489   391   473
#> 5 pop_aian     12    16    16     3     4    10     0     6     2     1    12
#> 6 pop_asian  1020  1076  1251   954   266  1191   386   407   739   527   732
#> 7 pop_nhpi     11    14     0     2     2     9     3     3     0     0     3
#> 8 pop_other    39    31    44    27    25    60    25    55    15    21    53
#> 9 pop_two     419   322   226   283   202   576   197   336   188   295   410
#> # ℹ 132 more variables: V12 <dbl>, V13 <dbl>, V14 <dbl>, V15 <dbl>, V16 <dbl>,
#> #   V17 <dbl>, V18 <dbl>, V19 <dbl>, V20 <dbl>, V21 <dbl>, V22 <dbl>,
#> #   V23 <dbl>, V24 <dbl>, V25 <dbl>, V26 <dbl>, V27 <dbl>, V28 <dbl>,
#> #   V29 <dbl>, V30 <dbl>, V31 <dbl>, V32 <dbl>, V33 <dbl>, V34 <dbl>,
#> #   V35 <dbl>, V36 <dbl>, V37 <dbl>, V38 <dbl>, V39 <dbl>, V40 <dbl>,
#> #   V41 <dbl>, V42 <dbl>, V43 <dbl>, V44 <dbl>, V45 <dbl>, V46 <dbl>,
#> #   V47 <dbl>, V48 <dbl>, V49 <dbl>, V50 <dbl>, V51 <dbl>, V52 <dbl>, …
#> 
#> $vap
#> # A tibble: 9 × 144
#>   rowname      V1    V2    V3    V4    V5    V6    V7    V8    V9   V10   V11
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 vap        8803  6804  6170  6225  4217 10004  2669  4780  5216  5018  6749
#> 2 vap_hisp    778   701   572   566   371   929   237   322   624   509   689
#> 3 vap_white  4122  4045  3766  4202  3342  6818  1790  3616  3247  3455  4634
#> 4 vap_black  2509   630   365   263    92   556   176   228   480   351   418
#> 5 vap_aian     11    16    14     3     2    10     0     6     2     0    12
#> 6 vap_asian   979  1059  1220   918   249  1156   321   378   718   475   664
#> 7 vap_nhpi     11    11     0     2     1     9     3     3     0     0     2
#> 8 vap_other    32    30    35    20    23    49    24    39    13    19    35
#> 9 vap_two     361   312   198   251   137   477   118   188   132   209   295
#> # ℹ 132 more variables: V12 <dbl>, V13 <dbl>, V14 <dbl>, V15 <dbl>, V16 <dbl>,
#> #   V17 <dbl>, V18 <dbl>, V19 <dbl>, V20 <dbl>, V21 <dbl>, V22 <dbl>,
#> #   V23 <dbl>, V24 <dbl>, V25 <dbl>, V26 <dbl>, V27 <dbl>, V28 <dbl>,
#> #   V29 <dbl>, V30 <dbl>, V31 <dbl>, V32 <dbl>, V33 <dbl>, V34 <dbl>,
#> #   V35 <dbl>, V36 <dbl>, V37 <dbl>, V38 <dbl>, V39 <dbl>, V40 <dbl>,
#> #   V41 <dbl>, V42 <dbl>, V43 <dbl>, V44 <dbl>, V45 <dbl>, V46 <dbl>,
#> #   V47 <dbl>, V48 <dbl>, V49 <dbl>, V50 <dbl>, V51 <dbl>, V52 <dbl>, …
#> 
```
