#' Set options for `redistio`
#'
#' @param theme a name of a bootswatch preset theme or other `bslib::bs_theme()` object
#' @param panels which panels to display in the app, `'draw'` is always shown.
#' @param select_color a color to use for highlighting selected districts
#' @param leaflet_height height to pass to `leaflet::leafletOutput()`
#' @param crs a coordinate reference system to use in `leaflet::leaflet()`
#' @param layer_weight a stroke width to use for layers in `leaflet::leaflet()`
#' @param layer_color a color to use for layers in `leaflet::leaflet()`
#' @param use_algorithms whether to use redistricting simulation algorithms
#' @param alg_max_districts maximum number of districts to use in algorithms
#' @param alg_max_sims maximum number of simulations to use in algorithms
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
                             leaflet_height = '91vh', crs = 4326,
                             layer_weight = 1.5, layer_color = '#000000',
                             use_algorithms = TRUE, alg_max_districts = 3,
                             alg_max_sims = 100,
                             ...) {
  list(
    theme = theme,
    panels = panels,
    select_color = select_color,
    leaflet_height = leaflet_height,
    crs = crs,
    layer_weight = layer_weight,
    layer_color = layer_color,
    use_algorithms = use_algorithms,
    alg_max_districts = alg_max_districts,
    alg_max_sims = alg_max_sims
  )
}
