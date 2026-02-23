#' Create a `tibble` of precinct stats
#'
#' @param shp a [tibble::tibble] with precinct stats
#' @param ... named tidyselections
#'
#' @return A [tibble::tibble] with columns `group`, `rowname`, and one column
#'   per precinct (`V1`, `V2`, ...). Groups are labeled with human-readable
#'   names (e.g. `"Total Population"`, `"Voting Age Population"`).
#' @export
#'
#' @examples
#' hover_precinct(dc, pop = dplyr::starts_with('pop'), vap = dplyr::starts_with('vap'))
hover_precinct <- function(shp, ...) {
  if (inherits(shp, 'sf')) {
    shp <- sf::st_drop_geometry(shp)
  }

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
  ) |>
    dplyr::bind_rows(.id = 'group') |>
    format_alarm_names() |>
    dplyr::ungroup()
}
