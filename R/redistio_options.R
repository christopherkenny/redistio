#' Set options for `redistio`
#'
#' @param theme a name of a bootswatch preset theme or other `bslib::bs_theme()` object
#' @param select_color a color to use for highlighting selected districts
#' @param leaflet_height height to pass to `leaflet::leafletOutput()`
#' @param crs a coordinate reference system to use in `leaflet::leaflet()`
#' @param ... additional arguments (currently ignored)
#'
#' @return a `list`
#' @export
#'
#' @examples
#' redistio_options()
redistio_options <- function(theme = 'flatly', select_color = 'purple',
                             leaflet_height = '91vh', crs = 4326,
                             ...) {
  list(
    theme = theme,
    select_color = select_color,
    leaflet_height = leaflet_height,
    crs = crs
  )
}
