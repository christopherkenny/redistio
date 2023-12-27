#' Set options for `redistio`
#'
#' @param theme a name of a bootswatch preset theme or other `bslib::bs_theme()` object
#' @param select_color a color to use for highlighting selected districts
#' @param ... additional arguments (currently ignored)
#'
#' @return a `list`
#' @export
#'
#' @examples
#' redistio_options()
redistio_options <- function(theme = 'flatly', select_color = 'purple', ...) {
  list(
    theme = theme,
    select_color = select_color
  )
}
