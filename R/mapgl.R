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
