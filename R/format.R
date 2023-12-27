format_demog_string <- function(x) {
  rplc <- c(
    'white' = 'White',
    'black' = 'Black',
    'hisp' = 'Hispanic',
    'aian' = 'Native',
    'asian' = 'Asian',
    'nhpi' = 'Pacific Islander',
    'other' = 'Other',
    'two' = 'Two+'
  )

  for (i in seq_along(rplc)) {
    x <- stringr::str_replace(x, pattern = names(rplc)[i], replacement = rplc[i])
  }
  x
}

format_alarm_names <- function(x) {
  x |>
    dplyr::mutate(rowname = vapply(stringr::str_split(.data$rowname, '_', n = 2), function(y) purrr::pluck(y, 2, .default = ''), FUN.VALUE = '')) |>
    dplyr::mutate(rowname = dplyr::if_else(.data$rowname == '', 'Total', .data$rowname)) |>
    dplyr::mutate(rowname = format_demog_string(.data$rowname)) |>
    dplyr::mutate(group = dplyr::case_when(
      group == 'pop' ~ 'Total Population',
      group == 'vap' ~ 'Voting Age Population',
      group == 'cvap' ~ 'Citizen Voting Age Population',
      TRUE ~ group
    )
    ) |>
    dplyr::group_by(.data$group)
}
