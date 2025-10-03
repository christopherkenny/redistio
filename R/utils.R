prep_palette <- function(palette = NULL, ndists) {
  if (is.null(palette)) {
    if (rlang::is_installed('crayons')) {
      palette <- crayons::crayons$no_48
    } else {
      palette <- suppressWarnings(grDevices::palette.colors(
        n = ndists,
        'Polychrome 36'
      ))
    }
  }
  palette <- unname(as.character(palette))
  if (length(palette) != ndists) {
    if (length(palette) > ndists) {
      palette <- palette[seq_len(ndists)]
    } else {
      if (length(palette) < 1) {
        stop('`palette` must have at least one color.')
      }
      palette <- rep(palette, ceiling(ndists / length(palette)))[seq_len(
        ndists
      )]
    }
  }
  palette
}

prep_shp <- function(shp, crs) {
  # process shp components ----
  if (!sf::st_is_longlat(shp)) {
    shp <- sf::st_transform(shp, crs)
  }

  shp <- shp |>
    sf::st_make_valid()

  shp_in <- shp
  shp <- shp |>
    tibble::as_tibble() |>
    sf::st_as_sf() |>
    dplyr::select(-where(is.list))

  list(
    all_cols = shp_in,
    no_list_cols = shp
  )
}
