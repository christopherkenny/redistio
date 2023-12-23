#' Create a tibble of precinct stats
#'
#' @param shp `sf` tibble
#' @param id integer row number
#' @param ... named tidyselections
#'
#' @return A [tibble::tibble]
#' @export
#'
#' @examples
#' hover_precinct(dc, 1, pop = dplyr::starts_with('pop'), vap = dplyr::starts_with('vap'))
hover_precinct <- function(shp, id, ...) {
  z <- shp %>%
    sf::st_drop_geometry() %>%
    dplyr::slice(id)

  lapply(rlang::enquos(...),
         function(ooo) {
           z %>%
             dplyr::select(!!ooo) %>%
             t() %>%
             as.data.frame() %>%
             tibble::rownames_to_column() %>%
             tibble::as_tibble()
         }) %>%
    dplyr::bind_rows(.id = 'group')
}

