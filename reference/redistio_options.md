# Set options for `redistio`

Set options for `redistio`

## Usage

``` r
redistio_options(
  theme = "flatly",
  panels = c("elections", "demographics", "integrity", "algorithms"),
  select_color = "purple",
  border_color = "#000000",
  palette_pop = "Purples",
  palette_pct = "PuOr",
  palette_party = ggredist::ggredist$partisan,
  map_tiles = mapgl::carto_style("voyager"),
  leaflet_height = "91vh",
  crs = 4326,
  na_color = "#0000",
  layer_weight = 1.5,
  layer_color = "#000000",
  locked_districts = NULL,
  use_algorithms = TRUE,
  alg_max_districts = 3,
  alg_max_sims = 100,
  use_planscore = TRUE,
  save_assignment_path = "redistio.csv",
  save_shape_path = "redistio.geojson",
  debounce = 150,
  projection = "mercator",
  ...
)
```

## Arguments

- theme:

  a name of a bootswatch preset theme or other
  [`bslib::bs_theme()`](https://rstudio.github.io/bslib/reference/bs_theme.html)
  object

- panels:

  which panels to display in the app, `'draw'` is always shown.

- select_color:

  a color to use for highlighting selected districts

- border_color:

  a color to use for precinct borders in the map

- palette_pop:

  a color palette to use for whole people. Defaults to
  `RColorBrewer::brewer.pal(n = 3, name = 'Purples')`.

- palette_pct:

  a color palette to use for percentages of people. Defaults to
  `RColorBrewer::brewer.pal(n = 3, name = 'PuOr')`.

- palette_party:

  a color palette to use for parties

- map_tiles:

  a tileset to use for the map background, from `leaflet::providers`

- leaflet_height:

  height to pass to `leaflet::leafletOutput()`

- crs:

  a coordinate reference system to use in `leaflet::leaflet()`

- na_color:

  a color to use for unassigned precincts

- layer_weight:

  a stroke width to use for layers in `leaflet::leaflet()`

- layer_color:

  colors to use for layers in
  [`mapgl::maplibre()`](https://walker-data.com/mapgl/reference/maplibre.html)

- locked_districts:

  districts to lock on app start to stop edits

- use_algorithms:

  whether to use redistricting simulation algorithms

- alg_max_districts:

  maximum number of districts to use in algorithms

- alg_max_sims:

  maximum number of simulations to use in algorithms

- use_planscore:

  whether to use PlanScore to evaluate plans

- save_assignment_path:

  Output path to save assignment file to.

- save_shape_path:

  Output path to save shapefile to.

- debounce:

  Number of milliseconds to debounce map hover events. Defaults to 150.

- projection:

  Maplibre projection to use. Default is `'mercator'`.

- ...:

  additional arguments (currently ignored)

## Value

a `list`

## Examples

``` r
redistio_options()
#> $theme
#> [1] "flatly"
#> 
#> $panels
#> [1] "elections"    "demographics" "integrity"    "algorithms"  
#> 
#> $select_color
#> [1] "purple"
#> 
#> $border_color
#> [1] "#000000"
#> 
#> $palette_pop
#> [1] "#EFEDF5" "#BCBDDC" "#756BB1"
#> 
#> $palette_pct
#> [1] "#F1A340" "#F7F7F7" "#998EC3"
#> 
#> $palette_party
#> <palette[15]>
#>  [1]  #A0442C   #B25D4C   #C27568   #D18E84   #DFA8A0   #EBC2BC  
#>  [2]  #F6DCD9   #F9F9F9   #DAE2F4   #BDCCEA   #9FB6DE   #82A0D2  
#>  [3]  #638BC6   #3D77BB   #0063B1  
#> $map_tiles
#> [1] "https://basemaps.cartocdn.com/gl/voyager-gl-style/style.json"
#> 
#> $leaflet_height
#> [1] "91vh"
#> 
#> $crs
#> [1] 4326
#> 
#> $na_color
#> [1] "#0000"
#> 
#> $layer_weight
#> [1] 1.5
#> 
#> $layer_color
#> [1] "#000000"
#> 
#> $locked_districts
#> NULL
#> 
#> $use_algorithms
#> [1] TRUE
#> 
#> $alg_max_districts
#> [1] 3
#> 
#> $alg_max_sims
#> [1] 100
#> 
#> $use_planscore
#> [1] TRUE
#> 
#> $save_assignment_path
#> [1] "redistio.csv"
#> 
#> $save_shape_path
#> [1] "redistio.geojson"
#> 
#> $debounce
#> [1] 150
#> 
#> $projection
#> [1] "mercator"
#> 
```
