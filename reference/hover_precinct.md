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
#> Error in dplyr::bind_rows(lapply(rlang::enquos(...), function(ooo) {    tibble::as_tibble(tibble::rownames_to_column(as.data.frame(t(dplyr::select(shp,         !!ooo)))))}), .id = "group"): Can't combine `1$V1` <character> and `pop$V1` <double>.
```
