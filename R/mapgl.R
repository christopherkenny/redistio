discrete_palette <- function(palette, rcp, column = 'redistio_id', base = 0) {
  by_idx <- list(
    'match',
    list('get', column)
  )

  idx <- seq_len(length(rcp)) + base
  p <- palette[rcp]
  idx <- idx[!is.na(p)]
  p <- p[!is.na(p)]

  cols <- purrr::map2(idx, p, function(x, y) {
    if (!is.na(y)) {
      list(x, y)
    }
  }) |>
    unlist()

  by_idx |>
    append(cols) |>
    append('#000000')
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
