# Add summary statistics to plans

Add summary statistics to plans

## Usage

``` r
add_plan_stats(plans, ref_plan, map = NULL, name = NULL, ...)
```

## Arguments

- plans:

  a `redist_plans` object

- ref_plan:

  an integer vector of district assignments

- map:

  a `redist_map` object. Required if `plans` contains summary
  statistics.

- name:

  name for the reference plan. Defaults to `"ref"`.

- ...:

  additional arguments (currently ignored)

## Value

a `redist_plans` object with the reference plan added

## Examples

``` r
dc_map <- redist::redist_map(dc, existing_plan = ward)
#> Projecting to CRS 3857
plans <- redist::redist_smc(dc_map, nsims = 10)
#> Loading required package: redistmetrics
#> 
#> Attaching package: ‘redist’
#> The following object is masked from ‘package:stats’:
#> 
#>     filter
#> SEQUENTIAL MONTE CARLO
#> Sampling 10 143-unit maps with 8 districts and population between 80790 and 91596.
#> Split [0/7] ■                                | ETA?
#> Split [7/7] ■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■■  | ETA 0s
#> 
plans <- add_plan_stats(plans, dc_map$ward, map = dc_map, name = 'example')
```
