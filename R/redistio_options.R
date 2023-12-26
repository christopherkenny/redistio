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
#' redistio_options(theme = 'minty')
redistio_options <- function(theme = 'darkly', select_color = 'purple', ...) {
  list(
    theme = ifelse(is.character(theme), bslib::bs_theme(preset = theme), theme),
    select_color = select_color
  )
}
