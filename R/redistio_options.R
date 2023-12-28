#' Set options for `redistio`
#'
#' @param theme a name of a bootswatch preset theme or other `bslib::bs_theme()` object
#' @param select_color a color to use for highlighting selected districts
#' @param leaflet_height height to pass to `leaflet::leafletOutput()`
#' @param crs a coordinate reference system to use in `leaflet::leaflet()`
#' @param use_algorithms whether to use redistricting simulation algorithms
#' @param alg_max_districts maximum number of districts to use in algorithms
#' @param ... additional arguments (currently ignored)
#'
#' @return a `list`
#' @export
#'
#' @examples
#' redistio_options()
redistio_options <- function(theme = 'flatly', select_color = 'purple',
                             leaflet_height = '91vh', crs = 4326,
                             use_algorithms = TRUE, alg_max_districts = 3,
                             ...) {
  list(
    theme = theme,
    select_color = select_color,
    leaflet_height = leaflet_height,
    crs = crs,
    use_algorithms = use_algorithms,
    alg_max_districts = alg_max_districts
  )
}
