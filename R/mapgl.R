discrete_palette <- function(palette, rcp, column = 'redistio_id', base = 0) {
  idx <- seq_len(length(rcp)) + base
  p <- palette[rcp]
  idx <- idx[!is.na(p)]
  p <- p[!is.na(p)]

  mapgl::match_expr(
    column = column,
    values = as.character(idx),
    stops = p,
    default = '#000000'
  )
}

district_palette <- function(
  palette,
  column = 'redistio_district',
  state = 'district'
) {
  palette <- unname(as.character(palette))
  selector <- list(
    'coalesce',
    list('feature-state', state),
    list('get', column)
  )
  stops <- purrr::map2(seq_along(palette), palette, list) |>
    unlist(recursive = FALSE)

  c(list('match', selector), stops, list('#000000'))
}

set_district_state <- function(map, districts, feature_ids = NULL) {
  if (!inherits(map, 'maplibre_proxy')) {
    rlang::abort('`map` must be a MapLibre proxy.')
  }
  if (is.null(feature_ids)) {
    feature_ids <- seq_along(districts) - 1L
  }
  if (length(feature_ids) != length(districts)) {
    rlang::abort('`feature_ids` and `districts` must have the same length.')
  }

  districts <- as.integer(districts)
  districts[is.na(districts)] <- 0L
  map$session$sendCustomMessage(
    'redistio-set-district-state',
    list(
      id = map$id,
      source = 'redistio',
      feature_ids = unname(feature_ids),
      districts = unname(districts)
    )
  )

  invisible(map)
}

enable_map_hover <- function(session, id, layer_id, delay) {
  session$sendCustomMessage(
    'redistio-enable-hover',
    list(
      id = session$ns(id),
      layer_id = layer_id,
      delay = delay
    )
  )

  invisible(NULL)
}

percent_palette <- function(palette, na_color = '#CCCCCC', column = '') {
  palette <- palette |>
    unname() |>
    as.character()

  na_case <- list(
    'case',
    list(
      '==',
      list('get', column),
      NULL
    ),
    na_color
  )

  brks <- seq(0, 1, length.out = length(palette))

  stops <- purrr::map2(brks, palette, list) |>
    unlist(recursive = FALSE)

  interp <- list(
    'interpolate',
    list('linear'),
    list('get', column)
  ) |>
    append(stops)

  out <- list(
    expression = na_case |>
      append(list(interp)),
    breaks = brks,
    colors = palette,
    method = 'interpolate_equal',
    n_breaks = length(palette)
  )
  class(out) <- 'mapgl_continuous_scale'
  out
}

get_simple_legend_colors <- function(p, n_max = 3) {
  full <- p |>
    mapgl::get_legend_colors()

  if (length(full) > n_max) {
    idx <- c(1, round(length(full) / 2), length(full))
    full[idx]
  } else {
    full
  }
}

get_simple_legend_labels <- function(p, n_max = 3) {
  full <- p |>
    mapgl::get_legend_labels()

  if (length(full) > n_max) {
    idx <- c(1, round(length(full) / 2), length(full))
    full[idx]
  } else {
    full
  }
}
