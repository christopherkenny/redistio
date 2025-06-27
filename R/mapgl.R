discrete_palette <- function(palette, rcp) {
  by_idx <- list(
    'match',
    list('get', 'redistio_id')
  )

  idx <- seq_len(length(rcp))
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
    append('#FFFFFFAA')
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
