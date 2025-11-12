# Interactive Plan Drawing

Interactive Plan Drawing

## Usage

``` r
draw(
  shp,
  init_plan,
  ndists,
  palette = NULL,
  layers = NULL,
  pop_tol = 0.05,
  pop_col = "pop",
  adj_col = "adj",
  split_cols = guess_admins,
  elect_cols = guess_elections,
  demog_cols = guesstimate_demographics,
  hover_fn = hover_precinct,
  opts = redistio_options()
)
```

## Arguments

- shp:

  an `sf` tibble that you want to draw with

- init_plan:

  Plan to initialize with.

- ndists:

  Number of districts to draw if `init_plan` is not supplied.

- palette:

  Color palette to fill shapes with. Default is `Polychrome 36` or, if
  installed, `crayons::crayons$no_48`.

- layers:

  Named list of `sf` objects to overlay. Also takes column names in
  `shp` to group by.

- pop_tol:

  the population tolerance.

- pop_col:

  Name of column in `shp` that contains population data.

- adj_col:

  Name of column in `shp` that contains adjacency information.

- split_cols:

  Names of column in `shp` that contain administrative units

- elect_cols:

  Names of column in `shp` that contain election data

- demog_cols:

  Names of column in `shp` that contain demographic data

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
  draw(dc, dc$ward)
  draw(dc, dc$ward, layers = list(neighborhoods = 'adv_nbr'))
}
```
