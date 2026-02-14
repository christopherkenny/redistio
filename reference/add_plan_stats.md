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
plans <- redist_smc(dc_map, nsims = 10)
#> Error in redist_smc(dc_map, nsims = 10): could not find function "redist_smc"
plans <- add_plan_stats(plans, dc_map$ward, map = dc_map, name = 'example')
#> Error: object 'plans' not found
```
