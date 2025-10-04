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

prep_layers <- function(layers, shp) {
  if (!is.null(layers)) {
    sf_entries <- which(vapply(
      layers,
      function(x) inherits(x, 'sf'),
      logical(1)
    ))
    char_entries <- which(vapply(layers, is.character, logical(1)))
    if ((length(sf_entries) + length(char_entries)) != length(layers)) {
      stop('`layers` must be a list of `sf` objects and character vectors.')
    }
    for (i in seq_along(char_entries)) {
      nom <- layers[[char_entries[i]]]
      layers[[char_entries[i]]] <- shp |>
        dplyr::select(-where(is.list)) |>
        dplyr::group_by(!!rlang::sym(layers[[char_entries[i]]])) |>
        dplyr::summarize()
      if (is.null(names(layers)) || names(layers)[char_entries[i]] == '') {
        names(layers)[char_entries[i]] <- nom
      }
    }
    if (is.null(names(layers))) {
      names(layers) <- paste0('layer_', seq_along(layers))
    }
    if (any(names(layers) == '')) {
      names(layers)[names(layers) == ''] <- paste0('layer_', seq_along(layers))
    }
  }
  layers
}
