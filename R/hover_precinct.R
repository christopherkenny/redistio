#' Create a `tibble` of precinct stats
#'
#' @param shp a [tibble::tibble] with precinct stats
#' @param ... named tidyselections
#'
#' @return A [tibble::tibble]
#' @export
#'
#' @examples
#' hover_precinct(dc, 1, pop = dplyr::starts_with('pop'), vap = dplyr::starts_with('vap'))
hover_precinct <- function(shp, ...) {
  if (inherits(shp, 'sf')) {
    shp <- sf::st_drop_geometry(shp)
  }

  # shp <- shp |>
  #   dplyr::slice(id)

  lapply(
    rlang::enquos(...),
    function(ooo) {
      shp |>
        dplyr::select(!!ooo) |>
        t() |>
        as.data.frame() |>
        tibble::rownames_to_column() |>
        tibble::as_tibble()
    }
  ) #|>
  # dplyr::bind_rows(.id = 'group')
}
