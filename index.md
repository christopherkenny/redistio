# redistio

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

Or from [R-universe](https://christopherkenny.r-universe.dev/) with:

``` r
install.packages('redistio', repos = c('https://christopherkenny.r-universe.dev', 'https://cloud.r-project.org'))
```

## Example

The most basic application of `redistio` starts with an `sf` tibble and
a column of district assignments.

``` r
library(redistio)

draw(dc, dc$ward)
```

![](reference/figures/example_interactive.png)

# Configuration

## Editor options

The editor can be configured primarily through
[`redistio_options()`](http://www.christophertkenny.com/redistio/reference/redistio_options.md).
Some configurable options:

- `theme`: the [bslib](https://rstudio.github.io/bslib/) theme to use
- `panels`: which panels to include. Allows for removing panels like
  `elections`, if drawing districts in a party-blind manner
- `palette_party`: the colors to use for partisan maps
- `palette_pop`: the colors to use for population maps
- `palette_pct`: the colors to use for percentage maps. Diverging
  palettes are recommended.
- `map_tiles`: the base map style function to use from
  [mapgl](https://walker-data.com/mapgl/reference/index.html#styling-helpers)
- `use_alogrithms`: whether to enable algorithmic assistance for map
  drawing. Requires a `redist_map` object.
- `alg_max_districts`: the maximum number of districts allowed in
  algorithmic simulations
- `alg_max_sims`: the maximum number of simulations allowed in
  algorithmic simulations
- `use_planscore`: whether to use the [PlanScore
  API](https://planscore.org/) to evaluate plans. Requires internet
  access and a PlanScore key.

## Data Dependent Options

Several data-based options may be configured inside
[`draw()`](http://www.christophertkenny.com/redistio/reference/draw.md):

- `layers`: Columns to use as toggle layers, where you can show things
  like county lines above the shapes
- `elect_cols`: Specify election columns directly. This use a guessing
  approach which follows the ALARM Project column naming schema by
  default.
- `demog_cols`: Specify demographic columns directly. This use a
  guessing approach which follows the ALARM Project column naming schema
  by default.
- `split_cols`: Specify columns which contain administrative units to
  check splits for.
- `hover_fn`: A function to display precinct-data based on the row of
  `shp` that the mouse is over.

## Demo on Bluesky

For a short video demo of `redistio`, see [this post on Bluesky
Social](https://bsky.app/profile/chriskenny.bsky.social/post/3m2c3yw2agk2c).
