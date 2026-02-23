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
with columns `group`, `rowname`, and one column per precinct (`V1`,
`V2`, ...). Groups are labeled with human-readable names (e.g.
`"Total Population"`, `"Voting Age Population"`).

## Examples

``` r
hover_precinct(dc, pop = dplyr::starts_with('pop'), vap = dplyr::starts_with('vap'))
#> # A tibble: 18 × 145
#>    group     rowname    V1    V2    V3    V4    V5    V6    V7    V8    V9   V10
#>    <chr>     <chr>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 Total Po… Total    9944  6908  6354  6532  4823 10853  3327  6178  5809  5795
#>  2 Total Po… Hispan…   840   715   595   602   456  1023   330   434   684   608
#>  3 Total Po… White    4185  4090  3855  4375  3765  7405  2178  4662  3692  3952
#>  4 Total Po… Black    3418   644   367   286   103   579   208   275   489   391
#>  5 Total Po… Native     12    16    16     3     4    10     0     6     2     1
#>  6 Total Po… Asian    1020  1076  1251   954   266  1191   386   407   739   527
#>  7 Total Po… NH/PI      11    14     0     2     2     9     3     3     0     0
#>  8 Total Po… Other      39    31    44    27    25    60    25    55    15    21
#>  9 Total Po… Two+      419   322   226   283   202   576   197   336   188   295
#> 10 Voting A… Total    8803  6804  6170  6225  4217 10004  2669  4780  5216  5018
#> 11 Voting A… Hispan…   778   701   572   566   371   929   237   322   624   509
#> 12 Voting A… White    4122  4045  3766  4202  3342  6818  1790  3616  3247  3455
#> 13 Voting A… Black    2509   630   365   263    92   556   176   228   480   351
#> 14 Voting A… Native     11    16    14     3     2    10     0     6     2     0
#> 15 Voting A… Asian     979  1059  1220   918   249  1156   321   378   718   475
#> 16 Voting A… NH/PI      11    11     0     2     1     9     3     3     0     0
#> 17 Voting A… Other      32    30    35    20    23    49    24    39    13    19
#> 18 Voting A… Two+      361   312   198   251   137   477   118   188   132   209
#> # ℹ 133 more variables: V11 <dbl>, V12 <dbl>, V13 <dbl>, V14 <dbl>, V15 <dbl>,
#> #   V16 <dbl>, V17 <dbl>, V18 <dbl>, V19 <dbl>, V20 <dbl>, V21 <dbl>,
#> #   V22 <dbl>, V23 <dbl>, V24 <dbl>, V25 <dbl>, V26 <dbl>, V27 <dbl>,
#> #   V28 <dbl>, V29 <dbl>, V30 <dbl>, V31 <dbl>, V32 <dbl>, V33 <dbl>,
#> #   V34 <dbl>, V35 <dbl>, V36 <dbl>, V37 <dbl>, V38 <dbl>, V39 <dbl>,
#> #   V40 <dbl>, V41 <dbl>, V42 <dbl>, V43 <dbl>, V44 <dbl>, V45 <dbl>,
#> #   V46 <dbl>, V47 <dbl>, V48 <dbl>, V49 <dbl>, V50 <dbl>, V51 <dbl>, …
```
