planscoreUI <- function(id) {

}

planscoreServer <- function(id, plan, shp) {
  stopifnot(shiny::is.reactivevalues(plan))
  stopifnot(!shiny::is.reactive(shp))

  shiny::moduleServer(id, function(input, output, session) {

    f <- tempfile(fileext = '.geojson')

    shp |>
      tibble::as_tibble() |>
      sf::st_as_sf() |>
      dplyr::mutate(
        District = plan$pl
      ) |>
      dplyr::group_by(.data$District) |>
      dplyr::summarise() |>
      sf::st_write(f)

    planscorer::ps_upload_file(file = f)
  })
}
