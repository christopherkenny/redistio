# borrowed from rict until public
rict_population <- function(map, plan, as_gt = TRUE) {
  map$District <- plan
  tgt_pop <- round(sum(map$pop) / length(unique(plan)))

  df <- map |>
    tibble::as_tibble() |>
    dplyr::group_by(.data$District) |>
    dplyr::summarize(
      Population = sum(.data$pop),
      deviation = .data$Population - tgt_pop,
      pct_deviation = .data$deviation / tgt_pop,
      .groups = 'drop'
    )
  if (as_gt) {
    df |>
      gt::gt() |>
      gt::fmt_number(columns = c('Population', 'deviation'), decimals = 0) |>
      gt::fmt_percent(columns = 'pct_deviation', decimals = 1) |>
      gt::tab_spanner(label = 'Deviation', columns = c('deviation', 'pct_deviation')) |>
      gt::cols_label(
        dplyr::any_of('deviation') ~ 'People',
        dplyr::any_of('pct_deviation') ~ '%'
      )
  } else {
    df
  }
}

rict_contiguity <- function(map, plan, adj, as_gt = TRUE) {
  plan[is.na(plan)] <- max(plan, na.rm = TRUE) + 1L
  df <- geomander::check_contiguity(adj = adj, group = plan) |>
    dplyr::group_by(District = .data$group) |>
    dplyr::summarise(Pieces = max(.data$component)) |>
    dplyr::mutate(
      District = as.integer(.data$District),
      District = ifelse(.data$District == max(plan, na.rm = TRUE) + 1L, NA_integer_, .data$District)
    )

  if (as_gt) {
    df |>
      gt::gt()
  } else {
    df
  }
}

rict_compactness <- function(
  map, plan, measures = list(
    'comp_polsby' = redistmetrics::comp_polsby,
    'comp_schwartz' = redistmetrics::comp_schwartz,
    'comp_reock' = redistmetrics::comp_reock,
    'comp_ch' = redistmetrics::comp_ch
  ),
  as_gt = TRUE
) {
  meas <- lapply(measures, function(x) {
    x(plan, map)
  }) |>
    stats::setNames(names(measures)) |>
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
      stats::setNames(paste0('admin_', admin)) |>
      tibble::as_tibble()
  }

  subadmin_out <- lapply(seq_along(subadmin), function(i) {
    redistmetrics::splits_sub_admin(plan, map, !!rlang::sym(subadmin[i]))
  })
  if (length(subadmin_out) > 0) {
    subadmin_out <- subadmin_out |>
      stats::setNames(paste0('subadmin_', subadmin)) |>
      tibble::as_tibble()
  }

  total_out <- lapply(seq_along(total), function(i) {
    redistmetrics::splits_total(plan, map, !!rlang::sym(total[i]))
  })
  if (length(total_out) > 0) {
    total_out <- total_out |>
      stats::setNames(paste0('total_', total)) |>
      tibble::as_tibble()
  }

  multi_out <- lapply(seq_along(multi), function(i) {
    redistmetrics::splits_multi(plan, map, !!rlang::sym(multi[i]))
  })
  if (length(multi_out) > 0) {
    multi_out <- multi_out |>
      stats::setNames(paste0('multi_', multi)) |>
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

rict_elections <- function(map, plan, as_gt = TRUE) {
  elecs <- map |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::contains('_dem_'), dplyr::ends_with('_dem')) |>
    names() |>
    stringr::word(end = 2, sep = '_') |>
    unique()

  elect_tb <- lapply(elecs, function(el) {
    vote_d <- map |>
      dplyr::as_tibble() |>
      dplyr::select(
        dplyr::starts_with(paste0(el, '_dem')),
        dplyr::starts_with(paste0(el, '_rep'))
      )
    if (ncol(vote_d) != 2) {
      return(NULL)
    }
    dvote <- vote_d |> dplyr::pull(1)
    rvote <- vote_d |> dplyr::pull(2)

    tibble::tibble(
      District = plan,
      dvote = dvote,
      rvote = rvote
    ) |>
      dplyr::group_by(.data$District) |>
      dplyr::summarise(
        {{ el }} := sum(dvote, na.rm = TRUE) / (sum(dvote, na.rm = TRUE) + sum(rvote, na.rm = TRUE))
      )
  }) |>
    purrr::discard(.p = function(d) is.null(d))

  if (length(elect_tb) > 1) {
    elect_tb <- elect_tb |>
      purrr::reduce(dplyr::left_join, by = 'District') |>
      dplyr::rowwise() |>
      dplyr::mutate(
        e_dvs = mean(dplyr::c_across(-'District'), na.rm = TRUE),
      ) |>
      dplyr::ungroup()
  } else if (length(elect_tb) == 1) {
    elect_tb <- elect_tb[[1]]
  } else {
    elect_tb <- tibble::tibble(District = sort(unique(plan)))
  }

  cycles <- map |>
    tibble::as_tibble() |>
    dplyr::select(dplyr::starts_with('adv_')) |>
    names() |>
    stringr::word(start = 2, end = 2, sep = '_') |>
    unique()

  cycle_tb <- lapply(cycles, function(el) {
    vote_d <- map |>
      dplyr::as_tibble() |>
      dplyr::select(
        dplyr::starts_with(paste0('adv_', el)),
        dplyr::starts_with(paste0('arv_', el))
      )
    if (ncol(vote_d) != 2) {
      return(NULL)
    }
    dvote <- vote_d |> dplyr::pull(1)
    rvote <- vote_d |> dplyr::pull(2)

    # if (all(is.na(dvote)) | all(is.na(rvote))) {
    #   return(NULL)
    # }

    el <- paste0('avg_', el)

    tibble::tibble(
      District = plan,
      dvote = dvote,
      rvote = rvote
    ) |>
      dplyr::group_by(.data$District) |>
      dplyr::summarise(
        {{ el }} := sum(dvote, na.rm = TRUE) / (sum(dvote, na.rm = TRUE) + sum(rvote, na.rm = TRUE))
      )
  }) |>
    purrr::discard(.p = function(d) is.null(d))

  if (length(cycle_tb) > 1) {
    cycle_tb <- cycle_tb |>
      purrr::reduce(dplyr::left_join, by = 'District') |>
      dplyr::rowwise() |>
      dplyr::mutate(
        avg_cycle = mean(dplyr::c_across(dplyr::starts_with('avg_')), na.rm = TRUE)
      ) |>
      dplyr::ungroup()
  } else if (length(cycle_tb) == 1) {
    cycle_tb <- cycle_tb[[1]]
  } else {
    cycle_tb <- tibble::tibble(District = sort(unique(plan)))
  }

  if ('ndv' %in% names(map) && 'nrv' %in% names(map)) {
    ndv <- map |> dplyr::pull('ndv')
    nrv <- map |> dplyr::pull('nrv')
    ndshare_tb <- tibble::tibble(
      District = plan,
      ndv = ndv,
      nrv = nrv
    ) |>
      dplyr::group_by(.data$District) |>
      dplyr::summarise(
        ndshare = sum(.data$ndv, na.rm = TRUE) / (sum(.data$ndv, na.rm = TRUE) + sum(.data$nrv, na.rm = TRUE))
      )
  } else {
    ndshare_tb <- tibble::tibble(District = sort(unique(plan)))
  }

  out <- list(
    elect_tb,
    cycle_tb,
    ndshare_tb
  ) |>
    purrr::reduce(dplyr::left_join, by = 'District') |>
    dplyr::relocate(dplyr::any_of(c('District', 'e_dvs', 'avg_cycle', 'ndshare')), .before = 1)

  if (as_gt) {
    out |>
      gt::gt() |>
      gt::tab_spanner(
        label = 'Contests Dem. Vote Share',
        columns = dplyr::any_of(setdiff(names(elect_tb), c('District', 'e_dvs')))
      ) |>
      gt::tab_spanner(
        label = 'Cycle Dem. Vote Share',
        columns = dplyr::any_of(setdiff(names(cycle_tb), c('District', 'avg_cycle')))
      ) |>
      gt::tab_spanner(
        label = 'Average Dem. Vote Share',
        columns = dplyr::any_of(c('e_dvs', 'avg_cycle', 'ndshare'))
      ) |>
      gt::cols_label(
        dplyr::any_of('e_dvs') ~ 'Contest',
        dplyr::any_of('avg_cycle') ~ 'Cycle',
        dplyr::any_of('ndshare') ~ 'Pre-Average'
      ) |>
      gt::cols_label_with(
        columns = dplyr::starts_with('avg_'),
        fn = function(x) stringr::str_replace(x, 'avg_', '20')
      ) |>
      gt::cols_label_with(
        columns = dplyr::any_of(setdiff(names(elect_tb), c('District', 'e_dvs'))),
        fn = function(x) {
          paste0(
            x |> stringr::word(sep = '_') |> format_election_names(),
            paste0(' 20', x |> stringr::word(start = 2, end = 2, sep = '_'))
          )
        }
      ) |>
      data_color_party(-1) |>
      gt::fmt_percent(
        columns = -1,
        decimals = 1
      )
  } else {
    out
  }
}

# tallyiers ----
tally_pop <- function(map, plan, pop_cols = dplyr::starts_with('pop_'), pop = 'pop',
                      normalize = FALSE) {
  pop_cols <- map |>
    tibble::as_tibble() |>
    dplyr::select({{ pop_cols }}) |>
    names()
  map <- map |>
    tibble::as_tibble() |>
    dplyr::mutate(District = plan) |>
    dplyr::group_by(.data$District) |>
    dplyr::summarize(
      dplyr::across(c(dplyr::all_of(.env$pop), dplyr::all_of(pop_cols)), sum),
      .groups = 'drop'
    )

  if (normalize) {
    .pop <- rlang::eval_tidy(rlang::ensym(pop), map)
    map <- map |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(pop_cols), .fns = function(x) x / !!rlang::ensym(pop))
      )
  }

  map
}
tally_vap <- function(map, plan, vap_cols = dplyr::starts_with('vap_'), vap = 'vap',
                      normalize = FALSE) {
  vap_cols <- map |>
    tibble::as_tibble() |>
    dplyr::select({{ vap_cols }}) |>
    names()
  tally_pop(map, plan, pop_cols = dplyr::all_of(vap_cols), pop = vap, normalize = normalize)
}

# data colors ====
data_color_party <- function(tab, columns = gt::everything(), ...) {
  tab |>
    gt::data_color(
      columns = !!rlang::enquo(columns),
      palette = partisan,
      domain = c(0, 1),
      ...
    )
}

partisan <- structure(c(
  '#A0442C', '#B25D4C', '#C27568', '#D18E84', '#DFA8A0',
  '#EBC2BC', '#F6DCD9', '#F9F9F9', '#DAE2F4', '#BDCCEA', '#9FB6DE',
  '#82A0D2', '#638BC6', '#3D77BB', '#0063B1'
), class = c(
  'palette',
  'character'
))
