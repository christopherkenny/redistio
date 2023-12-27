table_pop <- function(shp, tgt_pop) {
  shp |>
    dplyr::as_tibble() |>
    dplyr::group_by(.data$redistio_curr_plan) |>
    dplyr::summarize(pop = as.integer(sum(.data$pop))) |>
    dplyr::mutate(dev = as.integer(.data$pop - round(tgt_pop))) |>
    dplyr::arrange(as.integer(.data$redistio_curr_plan))
}
