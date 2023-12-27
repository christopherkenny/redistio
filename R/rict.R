# borrowed from rict until public
rict_population <- function(map, plan, as_gt = TRUE) {

  map$District <- plan
  tgt_pop <- round(sum(map$pop) / length(unique(plan)))

  df <- map |>
    tibble::as_tibble() |>
    dplyr::group_by(District) |>
    dplyr::summarize(
      Population = sum(pop),
      deviation = Population - tgt_pop,
      pct_deviation = deviation / tgt_pop,
      .groups = 'drop'
    )
  if (as_gt) {
    df |>
      gt::gt() |>
      gt::fmt_number(columns = c('Population', 'deviation'), decimals = 0) |>
      gt::fmt_percent(columns = 'pct_deviation', decimals = 1) |>
      gt::tab_spanner(label = 'Deviation', columns = c('deviation', 'pct_deviation')) |>
      gt::cols_label(
        deviation = 'People',
        pct_deviation = '%'
      )
  } else {
    df
  }
}

# tallyiers ----
tally_pop <- function(map, plan, pop_cols = dplyr::starts_with('pop_'), pop = 'pop',
                      normalize = FALSE) {

  pop_cols <- map |>
    tibble::as_tibble() |>
    dplyr::select(pop_cols) |>
    names()
  map <- map |>
    tibble::as_tibble() |>
    dplyr::mutate(District = plan) |>
    dplyr::group_by(District) |>
    dplyr::summarize(
      dplyr::across(c(dplyr::all_of(.env$pop), dplyr::all_of(pop_cols)), sum),
      .groups = 'drop'
    )

  if (normalize) {
    .pop <-rlang::eval_tidy(rlang::ensym(pop), map)
    map <- map |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(pop_cols), .fns = function(x) x / !!rlang::ensym(pop))
      )
  }

  map
}
tally_vap <- function(map, plan, vap_cols = dplyr::starts_with('vap_'), vap = 'vap',
                      normalize = FALSE) {
  tally_pop(map, plan, pop_cols = vap_cols, pop = vap, normalize = normalize)
}
