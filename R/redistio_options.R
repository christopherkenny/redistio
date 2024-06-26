#' Set options for `redistio`
#'
#' @param theme a name of a bootswatch preset theme or other `bslib::bs_theme()` object
#' @param panels which panels to display in the app, `'draw'` is always shown.
#' @param select_color a color to use for highlighting selected districts
#' @param palette_pop a color palette to use for whole people
#' @param palette_pct a color palette to use for percentages of people
#' @param palette_party a color palette to use for parties
#' @param map_tiles a tileset to use for the map background, from `leaflet::providers`
#' @param leaflet_height height to pass to `leaflet::leafletOutput()`
#' @param crs a coordinate reference system to use in `leaflet::leaflet()`
#' @param na_color a color to use for unassigned precincts
#' @param layer_weight a stroke width to use for layers in `leaflet::leaflet()`
#' @param layer_color colors to use for layers in `leaflet::leaflet()`
#' @param locked_districts districts to lock on app start to stop edits
#' @param use_algorithms whether to use redistricting simulation algorithms
#' @param alg_max_districts maximum number of districts to use in algorithms
#' @param alg_max_sims maximum number of simulations to use in algorithms
#' @param use_planscore whether to use PlanScore to evaluate plans
#' @param save_assignment_path Output path to save assignment file to.
#' @param save_shape_path Output path to save shapefile to.
#' @param ... additional arguments (currently ignored)
#'
#' @return a `list`
#' @export
#'
#' @examples
#' redistio_options()
redistio_options <- function(theme = 'flatly',
                             panels = c('elections', 'demographics', 'integrity', 'algorithms'),
                             select_color = 'purple',
                             palette_pop = 'Purples',
                             palette_pct = 'PuOr',
                             palette_party = ggredist::ggredist$partisan,
                             map_tiles = 'CartoDB.Positron',
                             leaflet_height = '91vh', crs = 4326,
                             na_color = '#0000',
                             layer_weight = 1.5, layer_color = '#000000',
                             locked_districts = NULL,
                             use_algorithms = TRUE, alg_max_districts = 3,
                             alg_max_sims = 100,
                             use_planscore = TRUE,
                             save_assignment_path = 'redistio.csv',
                             save_shape_path = 'redistio.geojson',
                             ...) {
  if (!rlang::is_closure(map_tiles)) {
    mt <- map_tiles
    map_tiles <- function(map) leaflet::addProviderTiles(map, provider = mt)
  }
  list(
    theme = theme,
    panels = panels,
    select_color = select_color,
    palette_pop = palette_pop,
    palette_pct = palette_pct,
    palette_party = palette_party,
    map_tiles = map_tiles,
    leaflet_height = leaflet_height,
    crs = crs,
    na_color = na_color,
    layer_weight = layer_weight,
    layer_color = layer_color,
    locked_districts = locked_districts,
    use_algorithms = use_algorithms,
    alg_max_districts = alg_max_districts,
    alg_max_sims = alg_max_sims,
    use_planscore = use_planscore,
    save_assignment_path = save_assignment_path,
    save_shape_path = save_shape_path
  )
}
