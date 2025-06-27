discrete_palette <- function(palette, rcp) {

  by_idx <- list(
    'match',
    list('get', 'redistio_id')
  )

  cols <- purrr::map2(seq_len(length(rcp)), palette[rcp], list) |>
    unlist()

  by_idx |>
    append(cols) |>
    append('#FFFFFF')
}
