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

rict_contiguity <- function(map, plan, as_gt = TRUE) {

  map$District <- plan
  df <- geomander::check_contiguity(adj = map$adj, group = plan) |>
    dplyr::group_by(District = group) |>
    dplyr::summarise(Pieces = max(.data$component)) |>
    dplyr::mutate(
      District = as.integer(.data$District)
    )

  if (as_gt) {
    df |>
      gt::gt()
  } else {
    df
  }
}

rict_compactness <- function(map, plan, measures = list(
  'comp_polsby' = redistmetrics::comp_polsby,
  'comp_schwartz' = redistmetrics::comp_schwartz,
  'comp_reock' = redistmetrics::comp_reock,
  'comp_ch' = redistmetrics::comp_ch),
  as_gt = TRUE) {

  meas <- lapply(measures, function(x) {
    x(plan, map)
  }) |>
    setNames(names(measures)) |>
    tibble::as_tibble()

  out <- dplyr::bind_cols(
    tibble::tibble(
      District = sort(unique(plan))
    ),
    meas
  )

  if (as_gt) {
    out |>
      gt::gt() |>
      gt::fmt_percent(columns = dplyr::starts_with('comp_'), decimals = 1) |>
      gt::cols_label_with(
        columns = dplyr::starts_with('comp_'),
        fn = function(x) format_compactness(x)
      )
  } else {
    out
  }
}

rict_splits <- function(map, plan, admin = NULL, subadmin = NULL, total = NULL,
                        multi = NULL, as_gt = TRUE) {

  admin_out <- lapply(seq_along(admin), function(i) {
    redistmetrics::splits_admin(plan, map, !!rlang::sym(admin[i]))
  })
  if (length(admin_out) > 0) {
    admin_out <- admin_out |>
      setNames(paste0('admin_', admin)) |>
      tibble::as_tibble()
  }

  subadmin_out <- lapply(seq_along(subadmin), function(i) {
    redistmetrics::splits_subadmin(plan, map, !!rlang::sym(subadmin[i]))
  })
  if (length(subadmin_out) > 0) {
    subadmin_out <- subadmin_out |>
      setNames(paste0('subadmin_', subadmin)) |>
      tibble::as_tibble()
  }

  total_out <- lapply(seq_along(total), function(i) {
    redistmetrics::splits_total(plan, map, !!rlang::sym(total[i]))
  })
  if (length(total_out) > 0) {
    total_out <- total_out |>
      setNames(paste0('total_', total)) |>
      tibble::as_tibble()
  }

  multi_out <- lapply(seq_along(multi), function(i) {
    redistmetrics::splits_multi(plan, map, !!rlang::sym(multi[i]))
  })
  if (length(multi_out) > 0) {
    multi_out <- multi_out |>
      setNames(paste0('multi_', multi)) |>
      tibble::as_tibble()
  }

  out <- list(
    admin_out,
    subadmin_out,
    total_out,
    multi_out
  ) |>
    purrr::discard(.p = function(d) length(d) == 0) |>
    purrr::list_cbind() |>
    dplyr::mutate(
      District = sort(unique(plan)),
      .before = dplyr::everything()
    )

  if (as_gt) {
    out |>
      gt::gt()
  } else {
    out
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
