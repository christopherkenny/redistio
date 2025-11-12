# Interactive Adjacency Graph Editing

Interactive Adjacency Graph Editing

## Usage

``` r
adj_editor(
  shp,
  adj = geomander::adjacency(shp),
  init_plan,
  palette = NULL,
  layers = NULL,
  hover_fn = hover_precinct,
  opts = redistio_options()
)
```

## Arguments

- shp:

  an `sf` tibble that you want to draw with

- adj:

  a zero-indexed adjacency graph

- init_plan:

  plan to initialize coloring

- palette:

  Color palette to fill shapes with. Default is `Polychrome 36` or, if
  installed, `crayons::crayons$no_48`.

- layers:

  Named list of `sf` objects to overlay. Also takes column names in
  `shp` to group by.

- hover_fn:

  Function to generate tables for mouse hovering. Default is
  [`hover_precinct()`](http://www.christophertkenny.com/redistio/reference/hover_precinct.md).

- opts:

  list of options. Default is
  [`redistio_options()`](http://www.christophertkenny.com/redistio/reference/redistio_options.md)

## Value

Shiny app

## Examples

``` r
if (interactive()) {
  adj_editor(dc, init_plan = dc$ward)
  adj_editor(dc, init_plan = dc$ward, layers = list(neighborhoods = 'adv_nbr'))
}
```
